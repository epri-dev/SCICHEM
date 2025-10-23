      SUBROUTINE VALCRD(KOUNT,CARD,FREQ,SECT,ALBED,BOWENR,ROUGH,ISTAT)
C=====================================================================**
C          VALCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Process values for albedo, bowen ratio, and
C               surface roughness length.
C
C     Initial Release:  December 1992
C
C     Revision History:
C          1/27/97  moved from ONSITE pathway
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      

      CHARACTER CARD*(*)
      REAL      ALBED,BOWENR,ROUGH
      INTEGER   ISTAT,FREQ,SECT,ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT          PROCESS STATUS 1 = ERROR IN PROCESSING
C                                    2 = PROCESSING OK
C      CARD           'IMAGE' SFC CHARACTERISTICS DATA
C      ALBEDO         VALUE READ FOR ALBEDO
C      BOWENR         VALUE READ FOR BOWEN RATIO
C      ROUGH          VALUE READ FOR SURFACE ROUGHNESS LENGTH (M)
C      FREQ           FREQUENCY INDEX,
C                      IF MONTHLY, THEN FREQ IS 1 THROUGH 12
C                      IF SEASONAL, THEN FREQ IS 1 THROUGH 4
C                      (1 = WINTER MONTHS 12,1,2 AND
C                      2 = SPRING MONTHS 3,4,5 ETC)
C                      IF ANNUAL, FREQ IS IGNORED
C      SECT           WIND DIRECTION SECTOR, MUST BE LE. OSNWDS VALUE

C---- Data Initialization

      PATH = 'METPREP'
      LOC  = 'VALCRD'
      ISTAT = 0

C---- Frequency identifier
      BUF08(1) = ' FREQ-ID'
      BUF08(2) = BLNK08
      CALL GETWRD( 2,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      ISTAT = ITEST
      IF( ITEST .EQ. 2 )THEN
         READ( BUF08(2),1000,IOSTAT=IOFLAG ) FREQ
1000     FORMAT( I8 )
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,1500 ) BUF08(2)
1500        FORMAT(' Error decoding frequency index: ', A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,1600 )
1600     FORMAT(' ERROR FROM S.GETWRD: FREQUENCY INDEX')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Wind direction sector
      BUF08(1) = ' SECT-ID'
      BUF08(3) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST )
      IF( ITEST.EQ.2 )THEN
         IF( ISTAT .NE. 1) ISTAT = ITEST
         READ( BUF08(3),1000,IOSTAT=IOFLAG ) SECT
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,2500 ) BUF08(3)
2500        FORMAT(' Error decoding sector: ', A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,2600 )
2600     FORMAT(' ERROR FROM S.GETWRD: SECTOR INDEX')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Albedo
      BUF08(1) = '  ALBEDO'
      BUF08(4) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )
      IF( ITEST.EQ.2 )THEN
         IF( ISTAT .NE. 1) ISTAT = ITEST
         CALL READRL(BUF08(4),8,ALBED,IOFLAG)
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3500 ) BUF08(4)
3500        FORMAT(' Error decoding albedo: ', A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3600 )
3600     FORMAT(' ERROR FROM S.GETWRD: ALBEDO')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Bowen ratio
      BUF08(1) = '   BOWEN'
      BUF08(5) = BLNK08
      CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(5),ITEST)
      IF( ITEST.NE.1 )THEN
         IF( ISTAT .NE. 1) ISTAT = ITEST
         CALL READRL(BUF08(5),8,BOWENR,IOFLAG)
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,4500 ) BUF08(5)
4500        FORMAT(' Error decoding bowen ratio: ', A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,4600 )
4600     FORMAT(' ERROR FROM S.GETWRD: BOWEN RATIO')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Surface roughness length
      BUF08(1) = '      Z0'
      BUF08(6) = BLNK08
      CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(6),ITEST )
      IF( ITEST.EQ.2 )THEN
         IF( ISTAT .NE. 1 ) ISTAT = ITEST
         CALL READRL(BUF08(6),8,ROUGH,IOFLAG)
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5500 ) BUF08(6)
5500        FORMAT(' Error decoding surface roughness: ', A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSE
            IF( ROUGH.LE.1.0E-8 )THEN
c              If the entered roughness length is less than or equal to zero
c              (using value of 1.0E-8 to account for truncation error), then
C              issue error message for invalid roughness value.
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,5510 ) ROUGH 
 5510          FORMAT(' Roughness value (',F11.7,'m) is NOT valid; ',
     &                 'less than or equal to zero!')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ELSEIF( ROUGH .LT. 0.0001 )THEN
c              If the entered roughness length is less that 0.0001m,
c              reset it to 0.0001m and issue a warning.
               ECODE = 'W06'
               MESS =  BLNK80
               WRITE( MESS,5520 ) ROUGH 
 5520          FORMAT(' Roughness value (', F8.5,') less than 0.0001m',
     &                 '; value may be suspect and has been reset to ',
     &                 '0.0001m.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ROUGH = 0.0001
            ELSEIF( ROUGH .LT. 0.001 )THEN
c              If the entered roughness length is less that 0.001m,
c              issue a warning but do not adjust the roughness value
               ECODE = 'W06'
               MESS =  BLNK80
               WRITE( MESS,5530 ) ROUGH
 5530          FORMAT(' Roughness value (', F8.5,') less than 0.001m',
     &                 '; value may be suspect.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ENDIF
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,5600 )
5600     FORMAT(' ERROR FROM S.GETWRD: SURFACE ROUGHNESS LENGTH')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      IF( ISTAT.NE.1 ) ISTAT = 2
      RETURN
      END

