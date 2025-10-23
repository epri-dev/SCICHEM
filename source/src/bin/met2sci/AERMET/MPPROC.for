        SUBROUTINE MPPROC( ISTAT,ISTAGE )
C=====================================================================**
C     Module MPPROC of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine reads the header records from the merged
C               meteorological data file.  If $, %, or @ is in second
C               column of header record, then the record is processed
C               as if it had been included in the runstream.
C
C     Arguments:
C        ISTAT    Status returned from this subroutine
C                 -1  =  errors
C                  0  =  no errors
C
C     Revision history:
C        10/07/96   Logic added to check for existence of the merged met
C                   data file; restructured some of the other logic;
C                   changed the way the header record 'history trace'
C                   is determined.
C
C         1/10/97   removed the 'history trace'
C-----------------------------------------------------------------------

C------ Variable Declaration

      IMPLICIT NONE
      
      INTEGER   NUMBER, DEVICE, ISTAT, ISAVEPATH, ISTAGE
      CHARACTER PREP*3, CARD*132
      LOGICAL   XUNIT, HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        NUMBER = Counter to keep track of header count
C        DEVICE = Device number of input file for headers
C        PREP   = 1st three characters from header record
C        CARD   = Control card 'image' as read for header file
C
C.......................................................................

C---- Data Initialization

      PATH   = PATHWD(6)
      LOC    = 'MPPROC'
      PATHID = 6
      ISTAT  = 0
      NUMBER = 0

      HDR = .TRUE.
      UADATA = .FALSE.
      SFDATA = .FALSE.

C---- Set the generic DEVICE to the unit attached to the merge file
      DEVICE = DEV40

C-----At this point in the processing, the status array has not been
C     checked to determine if there is sufficient information to
C     process the data, including the existence of the merged met
C     data.  Check for its existence here and abort if the file
C     was not defined (either through omission or an error decoding
C     the runstream record).

      INQUIRE( UNIT=DEVICE, OPENED=XUNIT )
      IF( XUNIT )THEN

C------- Rewind device;
C        Loop on a read that checks for '*' in first column; if
C        not '*' or EOH ('***'), stop processing, otherwise continue.

         REWIND DEVICE

   10    NUMBER = NUMBER + 1
         READ( DEVICE,400,IOSTAT=IOFLAG,END=100 ) PREP,CARD
  400    FORMAT(A3,A132)


C------- Check read status
         IF( IOFLAG .NE. 0 )THEN
            MESS =  BLNK80
            ECODE = 'E20'
            WRITE( MESS,2500 ) IOFLAG,NUMBER
 2500       FORMAT( ' ERROR READING MERGE FILE HEADER # ',I3)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            GO TO 10
         ENDIF

C------- Look for '*' or '***' to signify a comment or end of headers

         IF( PREP .EQ. '***' )THEN
C---------- End of headers marker encountered - return
            ISTAT = 2
            RETURN

         ELSEIF( PREP(1:1) .EQ. '*' )THEN
C---------- This record is a header - define the fields on this record
            CALL DEFINE( 132,CARD )

            IF ( PREP(2:2) .EQ. '%' )THEN
C------------- This record defines a pathway - process accordingly
               CALL FDPATH ( NUMBER,CARD,PATHID )

               IF( ( PATHID .LT. 1 )  .OR.  ( PATHID .GT. 5 ) )THEN
C---------------- Only the upperair, surface, onsite and merge pathways
C                 should be present in these header records.
                  MESS =  BLNK80
                  ECODE = 'E02'
                  WRITE( MESS,2100 )
 2100             FORMAT(' INVALID PATHWAY IN MERGE FILE HEADER',
     &                   ' RECORDS')
                  CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
                  ISTAT = 1

               ELSE
C---------------- Write the record to the temporary file and process
C                 the next record
                  CALL WRTCRD ( NUMBER,PREP,CARD,DEV70 )
C---------------- Set the flags to indicate if data type is processed;
C                 the check for site-specific data is made in MPTEST
                  IF( PATHID .EQ. 2 )THEN
                     UADATA = .TRUE.
                  ELSEIF (PATHID .EQ. 3 )THEN
                     SFDATA = .TRUE.
                  ENDIF
C---------------- Branch to statement 10 to read next record
                  GO TO 10
               ENDIF

            ENDIF

            CALL WRTCRD( NUMBER,PREP,CARD,DEV70 )

C---------- Check to see if this header should be actively processed

            IF( ( PREP(2:2) .EQ. '$' ) .OR.
     &          ( PREP(2:2) .EQ. '@' ) )THEN
C------------- Process this record: it is a LOCATION keyword or a
C              keyword associated with the on-site pathway

               CALL FDKEY( NUMBER,CARD,KEYID )
               ISAVEPATH = IRD1
               IF( PATHID.EQ.2 )THEN
                  IRD1 = 2
                  CALL UACARD( NUMBER,CARD,ISTAGE,HDR )
                  IRD1 = ISAVEPATH
                  ISAVEPATH = 0
               ELSEIF( PATHID.EQ.3 )THEN
                  IRD1 = 3
                  CALL SFCARD( NUMBER,CARD,ISTAGE,HDR )
                  IRD1 = ISAVEPATH
                  ISAVEPATH = 0
               ELSEIF( PATHID.EQ.4 )THEN
                  IRD1 = 4
                  CALL OSCARD( NUMBER,CARD,ISTAGE )
                  IRD1 = ISAVEPATH
                  ISAVEPATH = 0
               ENDIF
C               IF( PATHID.EQ.2 ) CALL UACARD( NUMBER,CARD )
C               IF( PATHID.EQ.3 ) CALL SFCARD( NUMBER,CARD )
C               IF( PATHID.EQ.4 ) CALL OSCARD( NUMBER,CARD )
            ENDIF
C---------- Branch to statement 10 to read next record
            GO TO 10

         ELSE
C---------- Somehow the end of the merge file headers was missed -
C           return with a fatal status because we do not know
C           where the pointer is in the data file.
            MESS =  BLNK80
            ECODE = 'E05'
            WRITE( MESS,2200 )
 2200       FORMAT(' EOH (''***'') NOT FOUND IN MERGE FILE ')
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            MESS =  BLNK80
            ECODE = '   '
            WRITE( MESS,2201 ) NUMBER
 2201       FORMAT(' - LOCATION IN FILE LOST AT RECORD ', I3)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

C------- An end-of-file encountered while reading the merge file headers
  100    MESS =  BLNK80
         ECODE = 'E05'
         WRITE( MESS,2000 )
 2000    FORMAT(' E-O-F READING MERGE FILE HEADERS')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         ISTAT = 1

      ELSE

C------- The merge met data file is not open (XUNIT is FALSE);
C        fatal error - write a message and set the status flag
         MESS =  BLNK80
         ECODE = 'E08'
         WRITE( MESS,2300 )
 2300    FORMAT(' MERGED MET DATA FILE NOT OPENED')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      RETURN
      END

