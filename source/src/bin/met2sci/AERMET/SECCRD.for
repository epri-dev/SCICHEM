      SUBROUTINE SECCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          SECCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  processes the wind sector definitions for the surface
C               characteristics
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
      INTEGER   ISTAT,SECT,ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'                      
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT    Process status 1 = error in processing
C                              2 = PROCESSING OK
C      CARD     Runstream record 
C      SECT     Wind direction sector, must be <= OSNWDS

C---- Data Initializations

      PATH = 'METPREP'
      LOC  = 'SECCRD' 
      ISTAT = 0


C---- Check the number of fields on record: must be 4

      IF( NWORDS.LT.4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''SECTOR'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ELSEIF( NWORDS.GT.4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''SECTOR'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF 


C---- Check to be sure a valid FREQ_SECT keyword is defined
      IF( SFCFRQ.EQ.0 )THEN

C------- FREQ_SECT not defined
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,2000 ) 
2000     FORMAT(' Missing or misplaced ''FREQ_SECT'' keyword; ',
     &          '''FREQ_SECT'' must appear before ''SECTOR''.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         OSNWDS = 0
         RETURN

      ELSEIF( SFCFRQ.EQ.1 )THEN
C------- FREQ_SECT in error
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,3600 ) 
3600     FORMAT(' Error on ''FREQ_SECT'' keyword')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1 
         OSNWDS = 0
         RETURN
      ENDIF

C---- Fetch second data field (should be SECTOR index)
      BUF08(1) = ' SECT-ID'
      BUF02 = '  '
      CALL GETWRD( 2,KOUNT,CARD,1,2,1,BUF08(1),BUF02,ITEST ) 
      ISTAT = ITEST

      IF( ITEST.EQ.2 )THEN
         READ( BUF02,5500,IOSTAT=IOFLAG ) SECT
5500     FORMAT( I2 )
         IF( IOFLAG.EQ.0 )THEN
C---------- Check sector index value

            IF( SECT.LE.0 .OR. SECT.GT.OSNWDS )THEN
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,6000 ) SECT,OSNWDS  
6000           FORMAT(' SECTOR index (',I3,') > number of',
     &                ' primary SECTORs allowed (',I3,')')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF

         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,6100 ) BUF02
6100        FORMAT(' Error decoding primary SECTOR index: ',A2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,6200 )
6200     FORMAT(' ERROR FROM S.GETWRD: PRIMARY WIND SECTOR INDEX')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Fetch the third field - the sector starting direction
      BUF08(1) = ' WDSTART'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST ) 

      IF( ITEST.EQ.2 )THEN
         IF( ISTAT.NE.1 ) ISTAT = ITEST
         CALL READRL(BUF08(2),8,XRD1,IOFLAG)
         IF( IOFLAG.NE.0 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,7500 ) SECT,BUF08(2)
7500        FORMAT(' Error reading primary SECTOR # ',I2,
     &             ' lower bound: ',A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         MESS =  BLNK80
         ECODE = 'E07'
         WRITE( MESS,7600 )
7600     FORMAT(' ERROR FROM S.GETWRD:',
     &          ' PRIMARY WIND SECTOR LOWER BOUND')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Fetch the fourth field - the sector ending direction
      BUF08(1) = ' WDSTOP'
      BUF08(3) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST ) 

      IF( ITEST.EQ.2 )THEN
         IF( ISTAT.NE.1 ) ISTAT = ITEST
         CALL READRL(BUF08(3),8,XRD2,IOFLAG)
         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,8500 ) SECT,BUF08(3)
8500        FORMAT(' Error reading primary SECTOR # ',I2,
     &             ' upper bound: ',A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         MESS =  BLNK80
         ECODE = 'E07'
         WRITE( MESS,8600 ) 
8600     FORMAT(' ERROR FROM S.GETWRD:',
     &          ' PRIMARY WIND SECTOR UPPER BOUND')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

C---- Check the sector boundaries
      IF( ISTAT .NE. 1 )THEN
         IF( XRD1.LT.0.0 .OR. XRD1.GT.360.0 .OR.
     &       XRD2.LT.0.0 .OR. XRD2.GT.360.0 .OR.
     &       XRD1.EQ.XRD2 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,9000 ) SECT,BUF08(2),BUF08(3)
9000        FORMAT(' Check primary SECTOR # ',I2,' bounds: ',A8,1X,A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE
C---------- Store values in OSWDS array
            OSWDS(SECT,1) = XRD1
            OSWDS(SECT,2) = XRD2
            ISTAT = 2
         ENDIF
      ENDIF

C---- Increment counter for number of sector keywords
      NUMSEC = NUMSEC + 1

      RETURN
      END

