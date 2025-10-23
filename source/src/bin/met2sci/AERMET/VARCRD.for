      SUBROUTINE VARCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          VARCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of the list of
C               variable names on the READ keyword.
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      

      CHARACTER CARD*(*)
      INTEGER  ISTAT,ITEST,CRDNO

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT        PROCESS STATUS 1 = ERROR IN PROCESSING
C                                  2 = PROCESSING OK
C      CARD         'IMAGE' WITH VARIABLE NAMES ('MAPS' INPUT DATA LIST(S))

C---- Data Initializations

      PATH = 'ONSITE'
      LOC  = 'VARCRD'
      ISTAT = 0

C---- Check the number of fields with this keyword

      IF( NWORDS .LT. 3 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''READ'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Get the index for READ

      buf02 = '  '
      BUF08(1) = 'READINDX'
      CALL GETWRD( 2,KOUNT,CARD,1,2,1,BUF08(1),buf02,ITEST )
      ISTAT = ITEST
      IF( ITEST .NE. 1 )THEN
         READ( buf02,1001,IOSTAT=IOFLAG ) CRDNO
1001     FORMAT( I2 )

         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2000 ) buf02
2000        FORMAT( ' Error reading index on ''READ'' keyword: ',A2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

      ELSE
         MESS =  BLNK80
         ECODE = 'E07'
         WRITE( MESS,2100 )
2100     FORMAT(' ERROR FROM S.GETWRD: ''READ'' INDEX')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Check that the number of READ keywords does not exceed
C     the maximum allowable

      IF( CRDNO .GT. OSMRDS )THEN
         MESS =  BLNK80
         ECODE = 'E06'
         WRITE( MESS,3000 ) BUF02,OSMRDS
3000     FORMAT( ' Too many ''READ'' keywords: ',A2, '; MAX =',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Decipher variable list on remainder of keyword
      CALL OSDTCD( KOUNT,CARD,CRDNO,ITEST )
      IF( ISTAT.NE.1 ) ISTAT = ITEST

      RETURN
      END

