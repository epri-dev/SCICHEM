      SUBROUTINE OAUDIT( KOUNT,CARD,ISTAT )
C=====================================================================**
C          OAUDIT Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of variables
C               to be summarized in the final audit/QA report.
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*8
      INTEGER  ISTAT,ITEST, I, N

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C     ISTAT       Processing status
C                    1 = failed
C                    2 = successful
C     CARD        Runstream record with variables to report for QA
C     NAME        Variable name on the runstream record


C---- Data Initialization

      PATH = 'ONSITE'
      LOC  = 'OAUDIT'


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

      MAINLOOP: DO N=2,NWORDS

C---- Fetch variable name                                  ---- CALL GETWRD

         NAME =  'FIELD   '
         POSINX = N-2
         WRITE( NAME(7:8),2000 ) POSINX
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,1,4,2,NAME,BUF04(1),ITEST )
         IF( ITEST .NE. 1 )THEN
C---------- Search for match within SURFACE pathway variable list
            DO I=1,34
               IF( BUF04(1).EQ.VNAMES(I) )THEN
                  OSSAUD(I) = 1
                  ISTAT = 2
                  CYCLE MAINLOOP
               ENDIF
            ENDDO

C---------- No match on the 4-character variable names;
C           search for a match on first two letters of BUF04(1)
C           in vector variable names -- the user may have included
C           height indicator in the vector variable's name, e.g.,
C           TT01, WS02, and the previous search did not work.

            BUF04(2) = BLNK04
            BUF04(2)(1:2) = BUF04(1)(1:2)
            DO I=15,29
               IF( BUF04(2).EQ.VNAMES(I) )THEN
                   OSSAUD(I) = 1
                   ISTAT = 2
                   CYCLE MAINLOOP
               ENDIF
            ENDDO

C---------- This point is reached if there was no match on the names
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF04(1)
3000        FORMAT(1X,A4,': NO MATCH WITH ONSITE VARIABLE NAMES')
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

      ENDDO MAINLOOP

      RETURN
      END

