      SUBROUTINE CLHT (P1, ICHT, IABSNT, MISS)
C=======================================================================
C        CLHT Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Compute the ceiling height in kilometers; a value of
C               990 = 990000 feet = 300 km = unlimited ceil. ht.)
C
C     Called by: D144LV, D028LV
C
C     Arguments:
C       P1      Input   First overpunch character in the ceiling field
C       ICHT    Output  Ceiling height in kilometers*10
C       IABSNT  Input   Missing value indicator
C       MISS    Input   Missing value flag
C
C     Revision history:
C       11/30/94 - Made into separate subroutine from an ENTRY point
C                  (PES, Inc.)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations
      INTEGER :: P1, ICHT, ICL, MISS, IABSNT
      
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      ICHT = MISS

C---- Interpret only expected overpunches

      IF(SFOVR(P1).EQ.-8888) THEN
         ICHT = 300
      ELSEIF(SFOVR(P1) .GE. 0 .AND. SFOVR(P1) .LE. 9 .AND.
     1        SFOVR(P1+1) .GE. 0 .AND. SFOVR(P1+1) .LE. 9 .AND.
     2        SFOVR(P1+2) .GE. 0 .AND. SFOVR(P1+2) .LE. 9) THEN
         ICL = SFOVR(P1)*100 + SFOVR(P1+1)*10 + SFOVR(P1+2)
         IF(ICL .EQ. 888) THEN
            ICHT = MISS
         ELSE
            ICL = ICL * 100
            CALL P2MMTR(ICL,ICHT)
            ICHT = NINT((FLOAT(ICHT)/1000.0) * 10.0)
         ENDIF
      ENDIF

      RETURN
      END

