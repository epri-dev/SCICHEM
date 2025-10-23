      SUBROUTINE OTHHDR(PATH, LOC, NUM, INDEV, OUDEV, ISTAT)
C=====================================================================**
C
C     PURPOSE:  Process header records for pathways other than
C               merge.
C
C     CALLED BY:     Any subroutine that writes file header records
C
C=======================================================================


      IMPLICIT NONE
      
      INTEGER INDEV, OUDEV, NUM, IOST, NHDRS, ISTAT

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C  PATH      = 10-CHARACTER IDENTIFIER FOR THE PATH
C  LOC       = 6-CHARACTER IDENTIFIER OF PROGRAM CALLING THIS ROUTINE
c  INDEV     = INPUT DEVICE LOGICAL UNIT NUMBER
C  OUDEV     = OUTPUT DEVICE LOGICAL UNIT NUMBER
C  NUM       = PATH NUMBER IN THE ARRAY "STATUS(NUM,-)"
C  KOUNT     = COUNTER FOR NUMBER OF HEADER RECORDS


C=======================================================================

C     Read the headers from the output file (OUDEV) to obtain a count
C     of the number of headers, then backspace to position the pointer
C     just before the '***' record.

      KOUNT =  0
      REWIND OUDEV
   2  BUF03 = '   '
      READ(OUDEV,200,END=210,ERR=215,IOSTAT=IOST) BUF03,BUF80(1)

      IF( BUF03.EQ.'***' ) GO TO 210
      IF( BUF03(1:1) .EQ. '*' )THEN
         KOUNT = KOUNT + 1
         GO TO 2
      ENDIF

 210  BACKSPACE OUDEV

C---- Position the input file at the beinning of the data
      REWIND INDEV
      NHDRS = 0
   3  BUF03 = '   '
      READ(INDEV,200,END=220,ERR=225,IOSTAT=IOST) BUF03,BUF80(1)
      IF( BUF03.EQ.'***' ) RETURN
      IF( BUF03(1:1) .EQ. '*' ) GO TO 3

C---- Likely that there are no headers - maybe user developed the
C     input file without using Stage 1
      REWIND INDEV
      RETURN

C---- Error and EOF conditions
  215 MESS =  BLNK80
      WRITE(MESS,404) PATHWD(NUM), KOUNT
      CALL ERRHDL(0,PATH,'E20',LOC,MESS)
      ISTAT = 1
      RETURN

  220 MESS =  BLNK80
      WRITE(MESS,444) PATHWD(NUM)
      CALL ERRHDL(0,PATH,'E20',LOC,MESS)
      ISTAT = 1
      RETURN

  225 MESS =  BLNK80
      WRITE(MESS,404) PATHWD(NUM),KOUNT
      CALL ERRHDL(0,PATH,'E20',LOC,MESS)
      ISTAT = 1
      RETURN

C--- FORMAT STATEMENTS

  200 FORMAT (A3,A132)
  217 FORMAT (10X,I5,' HEADERS PROCESSED FROM INPUT FILES')
  404 FORMAT (' ERROR READING ',A10,' FILE HEADER #',I3)
  444 FORMAT (' E-O-F READING ',A10,' FILE HEADER')

      END

