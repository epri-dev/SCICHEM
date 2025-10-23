      SUBROUTINE AVGCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          AVGCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway defintion of the number
C               of observations reported during each hour.  Also defines
C               the minimum number of observations required to compute
C               an hourly average (OSMIN).
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations

      CHARACTER CARD*(*)
      INTEGER   ISTAT,ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C        ISTAT       Process status 1 = error in processing
C                                   2 = processing ok
C        CARD        Runstream record

C---- Data Initializations

      PATH = 'ONSITE'
      LOC  = 'AVGCRD'
      ISTAT = 0


C---- Check the number of fields on this record: must be 2

      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''OBS/HOUR'' keyword!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''OBS/HOUR'' keyword!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Fetch the second field on the card: # observation periods/hour
      BUF08(1) = 'OBS/HOUR'
      BUF02 = '  '
      CALL GETWRD( 2,KOUNT,CARD,1,2,1,BUF08(1),BUF02,ITEST )
      ISTAT = ITEST
      IF( ITEST .EQ. 2 )THEN
         READ( BUF02,2000,IOSTAT=IOFLAG ) OSAVG
2000     FORMAT( I2 )
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF02
3000        FORMAT( ' Error dedcoding OBS/HOUR field:', A2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3100 )
3100     FORMAT( ' ERROR FROM S.GETWRD: OBS/HOUR')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      IF( ISTAT.NE. 1 )THEN
         IF( OSAVG.LE.0 .OR. OSAVG. GT. 12 )THEN
C---------- The value is outside the valid range of 1 to 12
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) OSAVG
5000        FORMAT( ' Invalid number of OBS/HOUR: ',I3,
     &              '; must be between 1 & 12, inclusive')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSEIF( MOD(60,OSAVG) .NE. 0 )THEN
C---------- The value is not evenly divisible into 60, 
C           so does not represent a valid observation
C           interval
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5100 ) OSAVG
5100        FORMAT( ' Invalid number of OBS/HOUR: ',I3,
     &              '; must be evenly divisible into 60.')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE

C---------- Define the minumum number of observations periods required
C           to compute an hourly average.
            OSMIN = (OSAVG + 1) / 2
         ENDIF
      ENDIF

      IF( ISTAT .NE. 1 )THEN
         ISTAT = 2
      ENDIF

      RETURN
      END

