      SUBROUTINE OSTRA( KOUNT,CARD,ISTAT )
C=====================================================================**
C          OSTRA Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the ONSITE pathway definitions of variables
C               not to track in the message file (NO_MISSING keyword)
C
C-----------------------------------------------------------------------

C---- Variable Decalrations


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

C      ISTAT     Process status 1 = error in processing
C                               2 = processing ok
C      CARD      Record with NO_MISSING trace variables listed
C      NAME      Variable name on record

C---- Data Initialization
C
      PATH = 'ONSITE'
      LOC  = ' OSTRA'

C---- Check number of fields

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
C------- Fetch variable name

         NAME =  'FIELD   '
         POSINX = N-1
         WRITE( NAME(7:8),2000 ) POSINX
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,2,4,2,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.1 )THEN
C---------- Search for match within ONSITE pathway variable list

            LOOP_VARS: DO I=1,34
               IF( BUF04(1).EQ.VNAMES(I) )THEN
                  OSSTRA(I) = 1
                  OSSAUD(I) = 1
                  IF( ISTAT .NE. 1) ISTAT = 2
                  CYCLE LOOP_WRDS
               ENDIF
            ENDDO LOOP_VARS

C---------- The user may have included height indicator in the vector
C           variable's name, which is not supported.  Search for a
C           match of first two letters of buf04(1) in vector
C           variable names.

            BUF04(2) = BLNK04
            BUF04(2)(1:2) = BUF04(1)(1:2)
            LOOP_VARS2: DO I=15,29
               IF( BUF04(2).EQ.VNAMES(I) )THEN
                  OSSTRA(I) = 1
                  OSSAUD(I) = 1
                  IF( ISTAT .NE. 1) ISTAT = 2
                  CYCLE LOOP_WRDS
               ENDIF
            ENDDO LOOP_VARS2

C---------- If this point is reached, then there was no match
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) BUF04(1)
3000        FORMAT(1X,A4,' DOES NOT MATCH ONSITE VARIABLE NAMES')
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

