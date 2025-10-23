      SUBROUTINE UATRA( KOUNT,CARD,ISTAT )
C=====================================================================**
C          UATRA Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the UPPERAIR pathway definitions of variables
C               not to track in the message file (NO_MISSING keyword)
C               during QA
C
C-----------------------------------------------------------------------

C---- Variable Declarations
C

      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*8
      INTEGER  ISTAT,ITEST,N, I

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT     Process status 1 = error in processing
C                               2 = processing ok
C      CARD      Record with NO_MISSING variables listed
C      NAME      Variable name on record

C---- Data Initialization

      PATH = 'UPPERAIR'
      LOC  = ' UATRA'

C---- Check number of fields on the record

      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''NO_MISSING'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Identify which variable(s) are listed

      LOOP_WRDS: DO N=2,NWORDS

C     Fetch variable name (note all variable names are supposed
C     to be 4-characters in length).

         NAME =  'FIELD   '
         POSINX = N-1
         WRITE( NAME(7:8),2000 ) POSINX
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,4,4,1,NAME,BUF04(1),ITEST )

         IF( ITEST.NE.1 )THEN
C---------- Search for match within UPPERAIR pathway variable list;
C           the last two variables are skipped because they pertain
C           to mixing heights, which are not supported in AERMET

            LOOP_VARS: DO I=1,UAVR-2
               IF( BUF04(1).EQ.UAVAR(I) )THEN
                  UAVTRA(I) = 1
                  UAVAUD(I) = 1
                  IF( ISTAT .NE. 1) ISTAT = 2
                  CYCLE LOOP_WRDS
               ENDIF
            ENDDO LOOP_VARS

C---------- This point is reached if there was no match
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF04(1)
3000        FORMAT(1X,A4,' DOES NOT MATCH UPPERAIR VARIABLE NAMES')
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

