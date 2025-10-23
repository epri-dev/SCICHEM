      SUBROUTINE FDKEY( KOUNT,CARD,KEYNUM )
C=====================================================================**
C          FDKEY Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE: Identifies the keyword on an input runstream record (CARD)
C              through the variable KEYNUM.
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER ISTAT, KEYNUM, I
      CHARACTER CARD*(*)
      CHARACTER KEY*12

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations

      PATH = PATHWD(PATHID)
      LOC  = ' FDKEY'
      KEY  = '            '

C     PATHID is the pathway number (1=JOB, 2=UPPERAIR, etc)
C     KEYNUM is the position in the keyword in the KEYWRD array

      KEYNUM = 0

      CALL GETWRD(1,KOUNT,CARD,2,12,2,'KEYWORD ',KEY,ISTAT)

      IF (ISTAT .EQ. 1) GO TO 30
      DO I = 1,NKEYWD
         IF( KEY .EQ. KEYWRD(I) )THEN
            KEYNUM = I
            RETURN
         ENDIF
      ENDDO

C---- No match found

  30  KEYNUM = 0

      RETURN
      END

