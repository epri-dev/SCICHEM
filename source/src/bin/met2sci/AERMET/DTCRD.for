      SUBROUTINE DTCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          DTCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of the
C               delta-T measurement heights.
C
C-----------------------------------------------------------------------

C------ Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*)
      INTEGER  ISTAT,NN,ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT     PROCESS STATUS 1 = ERROR IN PROCESSING
C                               2 = PROCESSING OK
C      CARD      Runstream record
C      NN        DELTA-T MEASUREMENT PAIR (E.G. 1,2 OR 3)

C-----Data Initializations

      PATH = 'ONSITE'
      LOC  = ' DTCRD'
      ISTAT = 0

C-----Check the number of fields on this record: must be 4

      IF( NWORDS.LT.4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 ) NWORDS
1000     FORMAT(' Too few fields for ''DELTA_TEMP'' keyword: ',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS .GT. 4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields for ''DELTA_TEMP'' keyword: ',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C-----Read level index
      BUF08(1) = ' D-TEMP '
      BUF01(1) = ' '
      CALL GETWRD(2,KOUNT,CARD,1,1,1,BUF08(1),BUF01(1),ITEST)
      ISTAT = ITEST
      IF( ITEST .EQ. 2 )THEN
         IOFLAG = 0
         READ(BUF01(1),3000,IOSTAT=IOFLAG) NN
3000     FORMAT(I1)
         IF (IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3100 ) BUF01(1)
3100        FORMAT( ' Error decoding level index on DELTA_TEMP ',
     &               'keyword: ', A1)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3200 )
3200     FORMAT( ' ERROR FROM S.GETWRD: DELTA_TEMP LEVEL')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C-----Read height 1
      BUF08(3) = BLNK08
      CALL GETWRD(3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST)
      IF( ITEST .EQ. 2 )THEN
         IF (ISTAT.NE.1) ISTAT=ITEST
         IOFLAG = 0
         CALL READRL(BUF08(3),8,OSLL(NN),IOFLAG)
         IF (IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,4000 ) BUF08(3), NN
4000        FORMAT(' Error decodeing DELTA_TEMP height #1 (',
     &              A8,') for level index: ',I2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,4100 )
4100     FORMAT(' ERROR FROM S.GETWRD: DELTA_T 1st HEIGHT')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

C-----Read height 2
      BUF08(4) = BLNK08
      CALL GETWRD(4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST)
      IF( ITEST .EQ. 2 )THEN
         IF (ISTAT.NE.1) ISTAT=ITEST
         IOFLAG = 0
         CALL READRL(BUF08(4),8,OSUL(NN),IOFLAG)
         IF (IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) BUF08(4), NN
5000        FORMAT(' Error decodeing DELTA_TEMP height #2 (',
     &              A8,') for level index: ',I2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,5100 )
5100     FORMAT(' ERROR FROM S.GETWRD: DELTA_T 2nd HEIGHT')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C-----Read both levels; make sure upper level is above lower level
C     and that both are above the surface

      IF( ISTAT .NE. 1 )THEN
         IF( OSLL(NN).GE.OSUL(NN) .OR. OSLL(NN).LE.0 .OR.
     &       OSUL(NN).LE.0 .OR. ABS(OSUL(NN)-OSLL(NN)).LT.0.1 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,6000 ) NN,OSLL(NN),OSUL(NN)
6000        FORMAT(' DELTA_TEMP ',I1,' height errors: ',2F8.3 )
            CALL ERRHDL ( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
C-----   All seems ok; increase counter for number of DELTA-TEMP values
         OSNDT = OSNDT + 1
         ISTAT = 2
      ENDIF

      RETURN
      END

