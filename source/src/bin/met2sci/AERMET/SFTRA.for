      SUBROUTINE SFTRA( KOUNT,CARD,ISTAT )
C=====================================================================**
C          SFTRA Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the SURFACE pathway definitions of variables
C               not to track in the message file (NO_MISSING keyword)
C               during QA
C
C-----------------------------------------------------------------------

C-----Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*8
      INTEGER  ISTAT,ITEST,N, I

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT       PROCESS STATUS 1 = ERROR IN PROCESSING
C                                 2 = PROCESSING OK
C      CARD        RECORD WITH NO_MISSING VARIABLES LISTED
C      NAME        VARIABLE NAME ON RECORD

C-----Data Initialization

      PATH = 'SURFACE'
      LOC  = ' SFTRA'

C-----Check the number of fields

      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''NO_MISSING'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C-----Identify which variable(s) are listed

      LOOP_WRDS: DO N=2,NWORDS
C------- Fetch variable name (note all variable names are supposed
C        to be 4-characters in length).

         NAME =  'FIELD   '
         POSINX = N-1
         WRITE( NAME(7:8),2000 ) POSINX
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,4,4,1,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.1 )THEN

C---------- Search for match within SURFACE pathway variable list
            LOOP_VARS: DO I=30,51
               IF( BUF04(1).EQ.VNAMES(I) )THEN
                  SFSTRA(I-29) = 1
                  SFSAUD(I-29) = 1
                  IF( ISTAT .NE. 1) ISTAT = 2
                  CYCLE LOOP_WRDS
               ENDIF
            ENDDO LOOP_VARS

C---------- This point is reached if there was no match
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF04(1)
3000        FORMAT(1X,A4,' DOES NOT MATCH SURFACE VARIABLE NAMES')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,3500 ) N
3500        FORMAT(' ERROR FROM S.GETWRD: VARIABLE NAME IN FIELD ',I2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ENDDO LOOP_WRDS

      RETURN
      END

