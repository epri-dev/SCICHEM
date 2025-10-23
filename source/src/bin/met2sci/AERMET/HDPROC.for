      SUBROUTINE HDPROC( ISTAGE )
C=====================================================================**
C          HDPROC Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Reads the header records for the initial input files,
C               if any, for the current path.
C               If appropriate (ie. $ is given in second column
C               of header card), we process this card as if
C               it were part of the current input stream
C
C               If the 'EXTRACT' record is present but the 'DATA'
C               record is not present, the file is read for headers.
C
C               If there are no EXTRACT & DATA records, the status of
C               the 'QAOUT' record is checked.  If it is present, the
C               QA file is read for header records.
C
C     Called by:  SETUP
C
C     Initial Release: December 15, 1992
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------

C---- Data Declarations


      IMPLICIT NONE
      
      INTEGER   NUMBER,DEVICE, IPATH, N, ISTAGE
      CHARACTER PREP*3,CARD*132, ALPHAB*26
      
      LOGICAL HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C      NUMBER = Counter keeps track of header count
C      DEVICE = Device number of input file for headers
C      PREP   = First three characters from header record
C      CARD   = Record as read for header file


C---- Data Initialization
C
      LOC  = 'HDPROC'
      DATA ALPHAB /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      NUMBER = 0

      HDR = .TRUE.
      
C---- Reprocess input file header records if the only processes are:

C     quality assurance for UPPERAIR and SURFACE pathways only, as
C     determined by the presence of EXTRACT (keyword # 7) and QAOUT
C     (keyword # 8) and absence of DATA (keyword # 4)

C     OR
C     merge data for UPPERAIR, SURFACE and/or ONSITE pathways, as
C     determined by the presence of the QAOUT keyword(s) and absence
C     of EXTRACT and DATA keywords

C     DEV12 = upperair EXTRACT output
C     DEV21 = surface EXTRACT output
C     DEV31 = onsite input to QA
C     DEV13 = upperair QAOUT output
C     DEV22 = surface QAOUT output
C     DEV32 = on-site QAOUT output

      DO IPATH = 2,4
         PATH = PATHWD(IPATH)
         DEVICE = 0
         IF( STATUS(IPATH,4).EQ.0 .AND. STATUS(IPATH,7).GT.1 .AND.
     &       STATUS(IPATH,8).GT.1 )THEN
C---------- EXTRACT and QAOUT keywords present in runstream but the
C           DATA keyword is not - must be QAing the data
C           (note: onsite data QA uses DATA keyword for file input
C                  and should not have any header records to process)
            IF( IPATH.EQ.2 ) DEVICE = DEV12
            IF( IPATH.EQ.3 ) DEVICE = DEV21

         ELSEIF( STATUS(IPATH,4).EQ.0 .AND. STATUS(IPATH,7).EQ.0 .AND.
     &           STATUS(IPATH,8).GT.1 )THEN
C---------- QAOUT is the only file keyword present in runstream -
C           must be merging data
            IF( IPATH.EQ.2 ) DEVICE = DEV13
            IF( IPATH.EQ.3 ) DEVICE = DEV22
            IF( IPATH.EQ.4 ) DEVICE = DEV32
         ENDIF

C------- No data to process for path; go to next path
         IF( DEVICE .NE. 0 )THEN

C---------- Process file headers: Check for '*' in first column; if there
C           is no '*', stop processing this file

            REWIND DEVICE

  10        READ( DEVICE,1000,IOSTAT=IOFLAG,END=5000 ) PREP,CARD
1000        FORMAT( A3,A132 )
            NUMBER = NUMBER + 1

C---------- Check read status
            IF( IOFLAG.NE.0 )THEN
               MESS =  BLNK80
               ECODE = 'E20'
               WRITE( MESS,2000 ) PATHWD(IPATH), DEVICE
2000           FORMAT(' ERROR READING ',A10,' HEADER ON UNIT ',I3)
               CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
               RETURN
            ENDIF

C---------- Look for '*'

            IF( PREP(1:1).NE.'*' .OR. PREP.EQ.'***') GO TO 5000

C---------- A header record - process it

C           Provide a 'trace' on the history of this control record:
C                the third character of the header card prefix will
C                contain a letter (A-Z), denoting when it was introduced
C                to the run.  BLNK = current, A = next to last, B = prev.
C                one to A, etc.

            IF( PREP .NE. '***' )THEN
               IF( PREP(3:3) .EQ. ' ' )THEN
                  PREP(3:3) = 'A'

               ELSE
                  N = INDEX( ALPHAB, PREP(3:3) )
                  IF( N .LT. 26 )THEN
                     PREP(3:3) = ALPHAB(N+1:N+1)
                  ENDIF
               ENDIF
            ENDIF

C---------- Define the fields on this record               ---- CALL DEFINE
            CALL DEFINE( 132,CARD )

            IF( PREP(2:2).EQ.'%' )THEN
C------------- This record defines a pathway               ---- CALL FDPATH
               CALL FDPATH (NUMBER,CARD,PATHID)

            ELSEIF( PREP(2:2).EQ.'$' .OR. PREP(2:2).EQ.'@'  )THEN
C------------- This record defines a keyword to reprocess  ---- CALL FDKEY
               CALL FDKEY( NUMBER,CARD,KEYID )
               IF( KEYID.GT.0 )THEN
                  IF( IPATH.EQ.2 ) CALL UACARD( NUMBER,CARD,ISTAGE,HDR )
                  IF( IPATH.EQ.3 ) CALL SFCARD( NUMBER,CARD,ISTAGE,HDR )
                  IF( IPATH.EQ.4 ) CALL OSCARD( NUMBER,CARD,ISTAGE )
               ENDIF
            ENDIF

C---------- Write the record to the temporary file         ---- CALL WRTCRD
            CALL WRTCRD( NUMBER,PREP,CARD,DEV70 )


C---------- Process the next record
            GO TO 10

         ENDIF                         ! DEVICE ne 0

5000     CONTINUE

      ENDDO                            ! DO loop

      RETURN
      END

