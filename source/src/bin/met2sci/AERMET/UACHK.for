      SUBROUTINE UACHK( KOUNT,CARD,ISTAT )
C=====================================================================**
C          UACHK Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the UPPERAIR pathway redefinition of QA
C               parameters.
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*), NAME*4
      INTEGER  ISTAT,KEY,MISS,LBOUND,UBOUND,ITEST, I

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' WITH NEW QA RANGE CHECK DATA
C        NAME            VNAME ON IMAGE
C        KEY             TYPE OF RANGE CHECK
C        MISS            NEW MISSING VALUE
C        LBOUND          NEW LOWER BOUND
C        UBOUND          NEW UPPER BOIUND

C---- Data Initialization

      PATH = 'UPPERAIR'
      LOC  = ' UACHK'
      ISTAT = 0

C---- Check the number of fields

      IF( NWORDS.NE.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         IF( NWORDS.LT.6 )THEN
            WRITE( MESS,1000 )
1000        FORMAT(' Too few fields on ''RANGE'' keyword.')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(' Too many fields on ''RANGE'' keyword.')
         ENDIF

         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Decipher the fields on this record                   ---- CALL RNGCRD
      CALL RNGCRD( KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ITEST )
      ISTAT = ITEST

C---- Identify which variable has new QA data

      IF( ISTAT .EQ. 2 )THEN
         DO I=1,12
            IF( NAME.EQ.UAVAR(I) )THEN
C------------- Redefine QA values
               UAQA(I,1) = KEY
               UAQA(I,2) = MISS
               UAQA(I,3) = LBOUND
               UAQA(I,4) = UBOUND
               ISTAT = 2
               RETURN
            ENDIF
         ENDDO

      ELSE
         ISTAT = 1
         RETURN
      ENDIF

C---- This point is reached if there was no match on a variable name
      ECODE = 'E06'
      MESS =  BLNK80
      WRITE( MESS,6000 ) NAME
6000  FORMAT(1X,A4,' DOES NOT MATCH UPPERAIR VARIABLE NAMES')
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1

      RETURN
      END

