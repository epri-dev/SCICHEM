        SUBROUTINE CLMCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          CLMCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of the
C               threshold wind speed defined by MIN_WIND
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

        IMPLICIT NONE

C------ Variable Declarations

        CHARACTER CARD*(*)
        INTEGER  ISTAT,ITEST

        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'

C        ISTAT     PROCESS STATUS 1 = ERROR IN PROCESSING
C                                 2 = PROCESSING OK
C        CARD      Runstream record

C------ Data Initialization

        PATH = 'ONSITE'
        LOC  = 'CLMCRD'
        ISTAT = 0

C------ Check number of fields on record: must be 2

        IF( NWORDS.LT.2 )THEN
           ECODE = 'E04'
           MESS =  BLNK80
           WRITE( MESS,1000 )
1000       FORMAT(' Too few fields on ''ONSITE THRESHOLD'' keyword')
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN
        ELSEIF( NWORDS .GT. 2 )THEN
           ECODE = 'E04'
           MESS =  BLNK80
           WRITE( MESS,1100 )
1100       FORMAT(' Too many fields on ''ONSITE THRESHOLD'' keyword')
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN
        ENDIF

C------ Get data from the threshold parameter field
        BUF08(3) = BLNK08
        CALL GETWRD(2,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST)
        ISTAT=ITEST
        IF (ITEST.EQ.2) THEN
           IOFLAG = 0
           CALL READRL(BUF08(3),8,OSCALM,IOFLAG)
           IF (IOFLAG.NE.0 )THEN
              ECODE = 'E06'
              MESS =  BLNK80
              WRITE( MESS,3000 ) BUF08(3)
3000          FORMAT( ' ERROR:'
     &                ' READING ONSITE THRESHOLD WIND SPEED ',A8)
              CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
           ENDIF
        ELSE
              ECODE = 'E07'
              MESS =  BLNK80
              WRITE( MESS,3100 )
3100          FORMAT( ' ERROR FROM S.GETWRD:'
     &                ' ONSITE THRESHOLD WIND SPEED')
              CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1
        ENDIF

C------ Check OSCALM, the variable name with the threshold wind
        IF( OSCALM .LT. 0.0 )THEN
           MESS =  BLNK80
           ECODE = 'E06'
           WRITE( MESS,5000 ) OSCALM
5000       FORMAT(' ERROR:'
     &            ' NEGATIVE ONSITE THRESHOLD WIND SPEED ',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1

        ELSEIF( OSCALM .GT. 1.0 )THEN
           MESS =  BLNK80
           ECODE = 'E06'
           WRITE( MESS,5010 ) OSCALM
5010       FORMAT(' ERROR:'
     &            'ONSITE THRESHOLD WIND EXCEEDS 1.0 m/s:',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1

        ELSEIF( OSCALM .GT. 0.5 )THEN
           MESS =  BLNK80
           ECODE = 'W06'
           WRITE( MESS,5020 ) OSCALM
5020       FORMAT(' WARNING:'
     &            ' ONSITE THRESHOLD WIND EXCEEDS 0.5 m/s:',F6.2)
           CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

        ENDIF

        IF( ISTAT.NE.1 ) ISTAT = 2
        RETURN
        END

