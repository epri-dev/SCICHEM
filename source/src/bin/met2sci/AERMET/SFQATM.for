      SUBROUTINE SFQATM(GMT2LST)
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C     PURPOSE
C     THIS ROUTINE IS USED TO CONVERT FROM GRENWICH MEAN TIME (GMT)
C     TO LOCAL STANDARD TIME (LST)

C     CALLED BY: SFEXT

C     Revisions:
C         12/11/09: Added code to read the ASOS flag at the end of the
C                   hour's second record
C
C=======================================================================


      IMPLICIT NONE
      

      INTEGER    KREC, NDAYS(0:12), LDAYS(0:12)
      INTEGER    GMT2LST, JULIAN
      INTEGER    SF2YR, SF4YR, SFJDY
      INTEGER    ISTAT, JDYHR, JULDAY

      CHARACTER*60  BUF60

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

      DATA PATH/'SURFACE'/,LOC/'SFQATM'/
      DATA NDAYS/31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      DATA LDAYS/31, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

c     ------------------------------------------------------------------
c     VARIABLE DESCRIPTIONS

C     KREC        = COUNTER FOR DATA RECORDS

C     ISTAT       = HEADER STATUS

c     ------------------------------------------------------------------

      KREC  = 0
      ISTAT = 0

C     Position pointer to end of headers on device 21

      CALL OTHHDR(PATH,LOC, 3, DEV21, 70, ISTAT)
      IF(ISTAT .LT. 0) THEN
         MESS =  BLNK80
         WRITE(MESS,135)
         CALL ERRHDL(0,PATH,'E48',LOC,MESS)
         SFSTAT = -1
         RETURN
      ENDIF

c     Copy data records to temporary file on unit 70
      REWIND 70

   10 READ(DEV21,2112,END=12,ERR=410)SF2YR,SFGMO,SFGDY,SFGHR,BUF60,
     &     BUF80(1),BUF03

      WRITE(70,2112)SF2YR,SFGMO,SFGDY,SFGHR,BUF60,BUF80(1),BUF03
      KREC = KREC + 1
      GOTO 10

   12 CONTINUE               !  EOF on DEV21
      REWIND 70              !  Reposition pointer to beginning of unit 70

c     Reposition pointer to EOH on DEV21
      CALL OTHHDR(PATH,LOC, 3, DEV21, 70, ISTAT)
      IF(ISTAT .LT. 0) THEN
         MESS =  BLNK80
         WRITE(MESS,135)
         CALL ERRHDL(0,PATH,'E48',LOC,MESS)
         SFSTAT = -1
         RETURN
      ENDIF

C    WRITE(DEV21,2200) VERSNO

      WRITE(*,609)

      KREC = 0
   20 CONTINUE

      SF4YR = 0
      SFGMO = 0
      SFGDY = 0
      SFGHR = 0

C --- Initialize SURFACE data arrays to missing
      CALL FLSFC(1)

C --- Read SURFACE data record from extracted file
      READ(70,2112,END=400,ERR=410)SF2YR,SFGMO,SFGDY,SFGHR,BUF60,
     &     BUF80(1),BUF03


      IF(SF2YR .GE. 50)THEN
         SF4YR = 1900 + SF2YR
      ELSE
         SF4YR = 2000 + SF2YR
      ENDIF

   30 KREC = KREC + 1


C --- Get Julian day (day number of the year)
      SFJDY = JULIAN(SF2YR, SFGMO, SFGDY)
      JDYHR = SFJDY*100 + SFGHR

C --- Apply time zone adjustment to convert to local time
      SFGHR = SFGHR - GMT2LST

      IF( SFGHR .LE. 0 )THEN             !  Add 24 hours

         SFGHR = SFGHR + 24

         SFGDY = SFGDY - 1             !  Reduce day of month by 1

         IF( SFGDY .EQ. 0 )THEN          !  Reset day of month

            SFGMO = SFGMO - 1          !  Reduce month by 1
            IF( MOD(SF4YR,4).EQ.0 )THEN  !  Leap year
               SFGDY = LDAYS(SFGMO)
            ELSE
               SFGDY = NDAYS(SFGMO)
            ENDIF

            IF( SFGMO .EQ. 0 )THEN
               SFGMO = 12              !  Reset month
               SF4YR = SF4YR - 1       !  Reduce year by 1

               IF( SF2YR .EQ. 0 )THEN                                    ! dtb132 02151
                  SF2YR = 99                                             ! dtb132 02151
               ELSE                                                      ! dtb132 02151
                  SF2YR = SF2YR - 1       !
               ENDIF                                                     ! dtb132 02151

            ENDIF

         ENDIF

      ELSEIF( SFGHR .GT. 24 )THEN        !  Subtract 24 hours

         SFGHR = SFGHR - 24
         JULDAY = SFJDY + 1
         
         IF( JULDAY .EQ. 367 )THEN
            SF4YR = SF4YR + 1
            JULDAY = 1
           
         ELSEIF( JULDAY .EQ. 366 )THEN
            IF( MOD(SF4YR,4) .NE. 0 )THEN
               SF4YR = SF4YR + 1
               JULDAY = 1
            ENDIF
         ENDIF

         IF( SF4YR .GE. 2000 )THEN
            SF2YR = SF4YR - 2000
         ELSE
            SF2YR = SF4YR - 1900
         ENDIF

         CALL GREG( SF2YR, JULDAY, SFGMO, SFGDY )
         
      ENDIF


      WRITE(DEV21,2112)SF2YR,SFGMO,SFGDY,SFGHR,BUF60,BUF80(1),BUF03

      GO TO 20                         !  Get next record

C-----------------------------------------------------------------------
C *** PROCESSING CONTINUES HERE ON AN EOF

400   MESS=BLNK40
      WRITE(MESS,800) KREC
      CALL ERRHDL(0,PATH,'I49',LOC,MESS)
      RETURN

C-----------------------------------------------------------------------
C *** PROCESSING CONTINUES HERE ON A READ ERROR

410   MESS=BLNK40
      WRITE(MESS,810) KREC + 1
      CALL ERRHDL(KREC,PATH,'E42',LOC,MESS)
      RETURN

C-----------------------------------------------------------------------
  135 FORMAT(' Error condition processing headers ')
c             ....+....1....+....2....+....3....+....4....+....5
  140 FORMAT(' Data record ', I5,' in IQA file is a missed observation')
  300 FORMAT(80A1)
  301 FORMAT(1X,I2,I2,I2,I2,4(1X,I5),6(1X,I5.5))
  307 FORMAT(8X,5(1X,I5.5),7(1X,I5),1x,I6)

  800 FORMAT(' End of file after data record:             ', I5)
  810 FORMAT(' Error reading data record:                 ', I5)


  609 FORMAT( ' ' )
  610 FORMAT('+  Stage 1: Mapping GMT to LST (Local Standard Time) for',
     &             ' month-day-year ', 2(I2.2,:'-'),I4)

 2112 FORMAT (1X, 4I2.2, A60/ A80,A3 )

 2200 FORMAT('*  SF   SURFACE DATA REMAP  AERMET DATED ', I5)

      END

