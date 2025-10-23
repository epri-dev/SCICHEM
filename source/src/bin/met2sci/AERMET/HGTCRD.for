      SUBROUTINE HGTCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          HGTCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of the tower
C               measurement heights.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations

      CHARACTER CARD*(*)
      INTEGER  I, ISTAT, ITEST, NCardHgts

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT           Process status 1 = error in processing
C                                     2 = processing OK
C      CARD            Record with tower height data

C---- Data Initializations

      PATH = 'ONSITE'
      LOC  = 'HGTCRD'
      ISTAT = 0

C---- Check the number of fields on this keyword: must be greater than 2

      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''OSHEIGHTS'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      NCardHgts = NWORDS - 1
      OSNL = OSNL + NCardHgts

C---- Check the number of heights; cannot exceed OSML (defined in
C     OS1.INC)
      IF( OSNL .GT. OSML )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,1500 ) OSNL,OSML
1500     FORMAT(' Number of OSHEIGHTS (',I2,') exceeds MAX allowed:',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Read heights
      BUF08(1) = 'OSHEIGHT'
      DO I=1,NCardHgts
         BUF08(2) = BLNK08
         CALL GETWRD(I+1,KOUNT,CARD,1,8,2,BUF08(1),BUF08(2),ITEST)
         IF( ITEST .EQ. 2 )THEN
            IF (ISTAT.NE.1) ISTAT=ITEST
            IOFLAG = 0
            CALL READRL(BUF08(2),8,OSHT(OSNL-(NCardHgts-I)),IOFLAG)
            IF( IOFLAG.NE.0 )THEN
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,2000 ) I,BUF08(2)
2000           FORMAT(' ERROR DECODING HEIGHT # ',I2,': ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF
         ELSE
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,2100 ) I
2100        FORMAT(' ERROR FROM S.GETWRD: ONSITE HEIGHT # ',I2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ENDDO

      IF( ISTAT.NE.1 ) ISTAT = 2

      RETURN
      END

