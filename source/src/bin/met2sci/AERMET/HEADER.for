      SUBROUTINE HEADER( ISTAGE )
C=======================================================================
C          HEADER Module of the AERMET Meteorological Preprocessor
C
C     Purpose: This routine writes, if appropriate, header(s) to
C              output files.  It also reprocesses some of the current
C              program stmts so that they override any header stmts.
C
C     Called by:  Main program (AERMET)
C
C     Initial Release: December 1992
C
C     Revision History:
C        <none>
C
C     Developed by: Pacific Environmental Services, Inc. (PES)
C                   Research Triangle Park, NC
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      

      INTEGER   NUMBER,ITEST,FILE1,FILE2, I, ISTAGE
      CHARACTER CARD*132, OPATH*2
      
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

C      NUMBER  Tracks number of header records,
C      ITEST   Status of an operation
C              1 = failed
C              2 = succeeded
C      FILE1   Output device unit to which the headers are written
C      FILE2   QA-file's device unit (required when FILE1 is the
C              EXTRACT/DATA-File's device unit.
C      OPATH   Pathway of last written header record to merge output
C              file.  Use this to key when to write a sub-header record
C              in the merge output file.
C
C.......................................................................

C---- Data Initializations

      PATH  = PATHWD(PATHID)
      LOC   = 'HEADER'
      OPATH = '  '
      HDR   = .TRUE.

C---- Check all the pathway's STATUS'
C      If any are < 0          -       no file
C      If any are = 4          -       prep merge file

      IF( JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0
     &                                 .OR. OSSTAT.LT.0 )THEN
         RETURN
      ENDIF


      IF( UASTAT.EQ.4 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.4 )THEN
         CONTINUE
      ELSEIF( UASTAT.EQ.4 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.0 )THEN
         CONTINUE
      ELSEIF( UASTAT.EQ.4 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.4 )THEN
         CONTINUE
      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.4 )THEN
         CONTINUE
      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.0 .AND. OSSTAT.EQ.4 )THEN
         CONTINUE
      ELSEIF( UASTAT.EQ.0 .AND. SFSTAT.EQ.4 .AND. OSSTAT.EQ.0 )THEN
         CONTINUE
      ELSE
         GO TO 25
      ENDIF

C---- Confirm that the merge file has been defined

      IF( STATUS(5,22).LE.1 ) RETURN

C---- Initialize header counter

      NUMBER = 0

      REWIND DEV40
      REWIND DEV70
  10  READ( DEV70,1000,END=20 ) BUF08(1),CARD
1000  FORMAT( A8,A132 )
      BUF02 = BUF08(1)(1:2)
      CALL DEFINE( 132,CARD )

      IF( BUF02.EQ.'JB' ) GO TO  10

      IF( CARD(14:19).EQ.'HEADER' ) GO TO 10

      NUMBER = NUMBER + 1

C---- Determine if a sub-header is required

      IF( NUMBER.GT.0 )THEN
         IF( BUF02.NE.OPATH )THEN
            BUF80(10) = BLN132
            BUF03     = '*T '
            WRITE( BUF80(10),1500 ) PATHWD(5)
1500        FORMAT( 14X,'HEADER RECORDS FOR ',A10)
            WRITE( DEV40,803 ) BUF03,BUF80(10)
 803        FORMAT(A3,A132)
            NUMBER = NUMBER + 1
            OPATH = BUF02
         ENDIF
      ENDIF

C---- Write header to merge data file

      BUF03 = '   '
      WRITE( BUF03,2000 ) BUF08(1)(3:3),BUF08(1)(8:8)
2000  FORMAT( '*',2A1 )
      WRITE( DEV40,803 ) BUF03,CARD
      GO TO 10

 20   CONTINUE

C---- Now add the current stmts on DEV75
      REWIND DEV75
21    READ( DEV75,1000,END=22 ) BUF08(1),CARD
      BUF03 = '   '
      WRITE( BUF03,2000 ) BUF08(1)(3:3),BUF08(1)(8:8)
      WRITE( DEV40,803 ) BUF03,CARD
      GO TO 21

  22  CONTINUE
      RETURN

C---- We do not fit into either of the special cases;
C     loop through pathways and perform necessary
C     operations.

  25  CONTINUE
  
      DO I=2,3

         IF( I.EQ.2 )THEN
            ITEST = UASTAT
         ELSE
            ITEST = SFSTAT
         ENDIF

C---- See if this is a null pathway or we slipped through the check to
C     merge data

         IF( ITEST.EQ.0 .OR. ITEST.EQ.4 ) CYCLE

C---- Initialize counter

         NUMBER = 0

         FILE2 = 0
         BUF08(3) = BLNK08
         BUF08(4) = BLNK08
         BUF08(5) = 'BUF03'
         IF( ITEST.EQ.2 )THEN
C---------- Header needed for QA-file only
            BUF08(3)  = 'QA'
            IF( I.EQ.2 )THEN
               FILE1 = DEV13
            ELSE
               FILE1 = DEV22
            ENDIF

         ELSE
C---------- Header needed for extract-file and possible QA-file
            BUF08(3) = 'EXTRACT '
            IF( I.EQ.2 )THEN
               FILE1 = DEV12
            ELSE
               FILE1 = DEV21
            ENDIF
            IF( ITEST.EQ.3 .OR. ITEST.EQ.5 .OR. ITEST.EQ.7 )THEN
               BUF08(4) = 'QA'
               IF( I.EQ.2 )THEN
                  FILE2 = DEV13
               ELSE
                  FILE2 = DEV22
               ENDIF
            ENDIF
         ENDIF

C------- Identify records for the file's headers

         REWIND FILE1
         IF( FILE2.NE.0 )REWIND FILE2
         REWIND DEV70
  30     READ( DEV70,1000,END=40 ) BUF08(1),CARD

         BUF02 = BUF08(1)(1:2)
         CALL DEFINE( 132,CARD )

         IF( CARD(14:19).EQ.'HEADER' ) GO TO 30

         IF( (I.EQ.2 .AND. BUF02.NE.'UA') .OR.
     &       (I.EQ.3 .AND. BUF02.NE.'SF') ) GO TO 30

C------- Check counter.  If equal to zero, write special
C        header before writing rest of headers.
C
         IF( NUMBER.EQ.0 )THEN
            BUF80(10) = BLN132
            BUF03     = '*T '
            WRITE( BUF80(10),1500 ) PATHWD(I)
            WRITE( FILE1,803 ) BUF03,BUF80(10)
            NUMBER = NUMBER + 1
         ENDIF

C------- Write header to file

         WRITE( FILE1,5000 ) BUF08(1)(3:3),
     &          BUF08(1)(8:8),CARD
5000     FORMAT('*',2A1,A132 )

         IF( FILE2.NE.0 )THEN
            WRITE( FILE2,5000 ) BUF08(1)(3:3), BUF08(1)(8:8),CARD
         ENDIF
         GO TO 30

  40     CONTINUE
C------- Now add the current stmts on DEV75
         REWIND DEV75

C------- Write AERMET Version number to header
         WRITE( FILE1,4444 ) VERSNO
4444     FORMAT('*    AERMET Version ',a6)
         IF( FILE2.NE.0 )THEN
            WRITE( FILE2,4444 ) VERSNO
         ENDIF

  41     READ( DEV75,1000,END=50 ) BUF08(1),CARD

         BUF02 = BUF08(1)(1:2)
         CALL DEFINE( 132,CARD )

         IF( (I.EQ.2 .AND. BUF02.NE.'UA') .OR.
     &       (I.EQ.3 .AND. BUF02.NE.'SF') ) GO TO 41

         BUF03 = '   '
         WRITE( BUF03,2000 ) BUF08(1)(3:3),BUF08(1)(8:8)
         WRITE( FILE1,803 ) BUF03,CARD

         IF( FILE2.NE.0 )THEN
            WRITE( FILE2,803 ) BUF03,CARD
         ENDIF

         GO TO 41

  50     CONTINUE

      ENDDO

C---- Process ONSITE pathway: is there a file to process?
      IF( OSSTAT.EQ.0 ) GO TO 80

C---- Initialize counter

      NUMBER = 0

      BUF08(3) = 'QA'
      REWIND DEV32
      REWIND DEV70
      WRITE( DEV32,4444 ) VERSNO
  60  READ( DEV70,1000,END=70 ) BUF08(1),CARD

      BUF02 = BUF08(1)(1:2)
      CALL DEFINE( 132,CARD )

      IF( BUF02.NE.'OS' ) GO TO  60

      IF( CARD(14:19).EQ.'HEADER' ) GO TO 60

C      CHECK COUNTER.  IF EQUAL TO ZERO, WRITE SPECIAL
C      HEADER BEFORE WRITING REST OF HEADERS.

      IF( NUMBER.EQ.0 )THEN
         BUF03 = '*T '
         BUF80(10) = BLN132
         WRITE( BUF80(10),1500 ) PATHWD(4)
         WRITE( DEV32,803 ) BUF03,BUF80(10)
         NUMBER = NUMBER + 1
      ENDIF

C      WRITE HEADER TO FILE

      WRITE( DEV32,5000) BUF08(1)(3:3),BUF08(1)(8:8),CARD
      GO TO 60

  70  CONTINUE
C---- Now add the current statements from DEV75
      REWIND DEV75
  71  READ( DEV75,1000,END=72 ) BUF08(1),CARD

      BUF03 = '   '
      WRITE( BUF03,2000 ) BUF08(1)(3:3),BUF08(1)(8:8)

C*    Only write the ONSITE pathway header records to the on-site
C     output file (as is done for SURFACE (SF) and UPPERAIR (UA) )
      IF( BUF08(1)(1:2) .NE. 'OS' ) GO TO 71

      WRITE( DEV32,803 ) BUF03,CARD
      GO TO 71

  72  CONTINUE
  80  CONTINUE

C---- Now  reprocess some of the current control stmts on DEV75

      REWIND DEV75
      NUMBER = 0
  81  READ( DEV75,1000,END=82,IOSTAT=IOFLAG ) BUF08(1),CARD

      NUMBER = NUMBER + 1
      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = 'E20'
         WRITE( MESS,2020 )
2020     FORMAT(' ERROR READING FROM TEMPORARY FILE ON DEV75')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C---- CHECK TO SEE IF THIS STATEMENT SHOULD BE PROCESSED by looking
C     for the special characters %, $, or @

      IF( BUF08(1)(8:8).EQ.'%' )THEN
C------- A pathway record
         CALL DEFINE( 132,CARD )
         CALL FDPATH (NUMBER,CARD,PATHID)
      ENDIF

      IF( BUF08(1)(8:8).EQ.'$' .OR. BUF08(1)(8:8).EQ.'@' )THEN
C------- A keyword record
         CALL DEFINE( 132,CARD )
         CALL FDKEY( NUMBER,CARD,KEYID )
         IF( KEYID.GT.0 )THEN
            IF( PATHID.EQ.2 ) CALL UACARD( NUMBER,CARD,ISTAGE,HDR )
            IF( PATHID.EQ.3 ) CALL SFCARD( NUMBER,CARD,ISTAGE,HDR )
            IF( PATHID.EQ.4 ) CALL OSCARD( NUMBER,CARD,ISTAGE )
         ENDIF
      ENDIF

      GO TO 81
  82  CONTINUE


      RETURN
      END

