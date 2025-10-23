      SUBROUTINE THRESH1MIN( KOUNT,CARD,ISTAT )
C=======================================================================
C          THRESH1MIN Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine processes the user's input for the
C               threshold wind speed for the hourly averaged 1-minute
C               ASOS wind speed.
C
C     Arguments:
C        KOUNT      Card image number
C        CARD       Image of processor control information
C        ISTAT      Status of processing image
C                      1 = error occurred
C                      2 = all ok
C
C     Programmed by:  AMEC
C
C     Revision History
C          March 2012   Original release
C
C
C-----------------------------------------------------------------------

C---- Variables Declarations

      IMPLICIT NONE

      INTEGER       I, ISTAT, ITEST
      CHARACTER     CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Intialization

      PATH = PATHWD(6)
      LOC  = 'THRESH1M'
      ISTAT = 0

C------ Check number of fields on record: must be 2

        IF( NWORDS.LT.2 )THEN
           ECODE = 'E04'
           MESS =  BLNK80
           WRITE( MESS,1000 )
1000       FORMAT(' Too few fields on ''THRESH_1MIN'' keyword')
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN
        ELSEIF( NWORDS .GT. 2 )THEN
           ECODE = 'E04'
           MESS =  BLNK80
           WRITE( MESS,1100 )
1100       FORMAT(
     &           ' Too many fields on ''THRESH_1MIN'' keyword')
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN
        ENDIF

C------ Get data from the threshold parameter field
        BUF08(1) = 'THRESH1M'
        BUF08(3) = BLNK08
        CALL GETWRD(2,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST)
        ISTAT=ITEST
        IF (ITEST.EQ.2) THEN
C          GETWRD processed without error
C           IOFLAG = 0
           CALL STONUM(BUF08(3),8,THRESH1SPD,IOFLAG)
C           CALL READRL(BUF08(3),8,THRESH1SPD,IOFLAG)
           IF (IOFLAG.NE.1 )THEN
              ECODE = 'E06'
              MESS =  BLNK80
              WRITE( MESS,3000 ) BUF08(3)
3000          FORMAT( ' ERROR:'
     &                ' READING ASOS 1-MIN THRESHOLD WIND SPEED ',A8)
              CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
           ENDIF
        ELSE
           ECODE = 'E07'
           MESS =  BLNK80
           WRITE( MESS,3100 )
3100       FORMAT( ' ERROR FROM S.GETWRD:'
     &             ' ASOS 1-MIN THRESHOLD WIND SPEED')
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
        ENDIF

C------ Check THRESH1SPD, the variable name with the threshold wind
        IF( THRESH1SPD .LT. 0.0 )THEN
           MESS =  BLNK80
           ECODE = 'E06'
           WRITE( MESS,5000 ) THRESH1SPD
5000       FORMAT(' ERROR:'
     &            ' NEGATIVE ASOS 1-MIN THRESHOLD WIND SPEED ',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1

        ELSEIF( THRESH1SPD .GT. 1.0 )THEN
           MESS =  BLNK80
           ECODE = 'E06'
           WRITE( MESS,5010 ) THRESH1SPD
5010       FORMAT(' ERROR:'
     &            ' ASOS 1-MIN THRESHOLD WIND EXCEEDS 1.0 m/s: ',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1

        ELSEIF( THRESH1SPD .GT. 0.5 )THEN
           MESS =  BLNK80
           ECODE = 'W06'
           WRITE( MESS,5020 ) THRESH1SPD
5020       FORMAT(' WARNING:'
     &            ' ASOS 1-MIN THRESHOLD WIND EXCEEDS 0.5 m/s: ',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

        ENDIF

        IF( ISTAT.NE.1 ) ISTAT = 2
        RETURN

        END

