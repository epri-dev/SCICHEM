      SUBROUTINE SAUDIT( KOUNT,CARD,ISTAT )
C=====================================================================**
C          SAUDIT Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the SURFACE pathway definitions of variables
C               to be summarized in the final audit/QA report.
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

C---- Variable Declaration


      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*8
      INTEGER  ISTAT,ITEST,N, I

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT       Processing status
C                      1 = failed
C                      2 = successful
C      CARD        Runstream record with variables to report for QA
C      NAME        A variable name on the runstream record


C---- Data Initialization

      PATH = 'SURFACE'
      LOC  = 'SAUDIT'


C---- Check the number of fields on the record - cannot be less than 2

      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''AUDIT'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Identify which variable(s) is(are) listed

      LOOP_WRDS: DO N=2,NWORDS

C------- Fetch variable name                               ---- CALL GETWRD
C        NOTE: All variable names are supposed to be
C              4-characters in length

         NAME =  'FIELD   '
         POSINX = N-1
         WRITE( NAME(7:8),2000 ) POSINX
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,4,4,1,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.1 )THEN
C---------- Search for match within SURFACE pathway variable list
            LOOP_VARS: DO I= 30, 51
               IF( BUF04(1).EQ.VNAMES(I) )THEN
                  SFSAUD(I-29) = 1
                  ISTAT = 2
                  CYCLE LOOP_WRDS
               ENDIF
            ENDDO LOOP_VARS

C---------- This point is reached if there was no match on the names
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF04(1)
3000        FORMAT(1X,A4,': NO MATCH WITH SURFACE VARIABLE NAMES')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,3100 ) N
3100        FORMAT(' ERROR FROM S.GETWRD: VARIABLE NAME IN FIELD ',I2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ENDDO LOOP_WRDS

      RETURN
      END

