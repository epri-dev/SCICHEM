      SUBROUTINE MRHDR(PATH,LOC,NUM,IDEV2,IDEV3,IDEV4,OUDEV,IDEV1,ISTAT)
C=====================================================================**
C
C     PURPOSE:    Processes the header records for merge
C
C     CALLED BY:
C
C=======================================================================

C

      IMPLICIT NONE
      
      INTEGER IDEV(4),IL,OUDEV,NUM,IOST, I, ISTAT
      INTEGER IDEV1,IDEV2,IDEV3,IDEV4
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C---  INPUT DEVICES USED
C  IDEV(2)     = INPUT FILE (FOR MERGE,  RAWINSONDE DATA)
C  IDEV(3)     = FOR MERGE, SURFACE OBS DATA
C  IDEV(4)     = FOR MERGE, ONSITE DATA
C
C---  OUTPUT DEVICES USED
C  OUDEV     = OUTPUT FILE (UNFORMATTED FOR MERGED DATA)
C  IDEV(1)   = SUMMARY FILE
C
C
C  PATH      = 10-CHARACTER IDENTIFIER FOR THE PATH
C  LOC       = 8-CHARACTER IDENTIFIER OF PROGRAM CALLING THIS ROUTINE
C  IDEV(2)   = INPUT LOGICAL UNIT NUMBER (UPPER AIR FOR MERGE)
C  IDEV(3)   = INPUT LOGICAL UNIT NUMBER OF SF DATA - FOR MERGE ONLY
C  IDEV(4)   = INPUT LOGICAL UNIT NUMBER OF OS DATA - FOR MERGE ONLY
C  OUDEV     = OUTPUT DEVICE LOGICAL UNIT NUMBER
C  NUM       = PATH NUMBER IN THE ARRAY "STATUS(NUM,-)"
C  KOUNT     = HEADER COUNTER
C  IWORK1( ) = HEADER COUNTER FOR THE INPUT FILES TO MERGE


C--- FORMAT STATEMENTS USED

  200 FORMAT(A3,A80)
  217 FORMAT(10X,I5,' HEADERS PROCESSED FROM INPUT FILES')
  404 FORMAT(' ERROR READING ',A10,' FILE HEADER #',I3)
  444 FORMAT(' E-O-F READING ',A10,' FILE HEADER')
C=======================================================================

      ISTAT = 0
      IDEV(2) = IDEV2
      IDEV(3) = IDEV3
      IDEV(4) = IDEV4
      KOUNT   = 0
      REWIND OUDEV

C---- Read the first 3 characters of each record in OUDEV and test the
C     first character to check for a valid end of header records

    1 CONTINUE
    
      BUF03 = '   '
      READ(OUDEV,200,END=400,ERR=120,IOSTAT=IOST) BUF03,BUF80(1)
      IF (BUF03.EQ.'***') GO TO 400
      IF(BUF03(1:1) .EQ. '*') THEN
         KOUNT = KOUNT + 1
         GO TO 1
      ELSE
         GO TO 400
      ENDIF

400   REWIND OUDEV

      DO I=1,KOUNT
         READ(OUDEV,200) BUF03,BUF80(1)
      ENDDO


C---- Position input files on first data record.
      DO IL=2,4
         IWORK1(950) = 0
         IF(STATUS(IL,8) .GE. 2) THEN
            REWIND IDEV(IL)
  131       BUF03 = '   '
            READ(IDEV(IL),200,END=1301,ERR=1302,IOSTAT=IOST) BUF03,
     *                                                     BUF80(1)
            IF (BUF03.EQ.'***') THEN
               IWORK1(950) = IWORK1(950) + 1
               CYCLE
            ENDIF
            IF(BUF03(1:1) .EQ. '*') THEN
               IWORK1(950) = IWORK1(950) + 1
               GO TO 131
            ENDIF
         ENDIF
         CYCLE

C------- Processing continues here for errors and end-of-files
 1301    MESS =  BLNK80
         WRITE(MESS,444) PATHWD(IL)
         CALL ERRHDL(0,PATH,'E22',LOC,MESS)
         ISTAT = 1
         STATUS(IL,8) = 0
         CYCLE

 1302    MESS =  BLNK80
         WRITE(MESS,404) PATHWD(IL), IWORK1(950)
         CALL ERRHDL(0,PATH,'E20',LOC,MESS)
         ISTAT = 1

      ENDDO

      WRITE(IDEV1,217) KOUNT
      RETURN

C---- Error reading headers that were written to output file
  120 MESS =  BLNK80
      WRITE(MESS,404) PATHWD(5), KOUNT+1
      CALL ERRHDL(0,PATH,'E23',LOC,MESS)
      ISTAT = 1
      RETURN

      END

