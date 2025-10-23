      SUBROUTINE CVG (P2,ICVG,IABSNT,MISS)
C=====================================================================**
C        CVG Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Compute the sky coverage in tenths
C
C     Called by: D144LV, D028LV
C
C     Arguments:
C       P2      Input   First overpunch character in the coverage field
C       ICVG    Output  Sky coverage in tenths
C       IABSNT  Input   Missing value indicator
C       MISS    Input   Missing value flag
C
C     Revision history:
C       11/30/94 - Made into separate subroutine from an ENTRY point
C                  (PES, Inc.)
C------------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations
      INTEGER, intent(in)  :: P2, MISS, IABSNT
      INTEGER, intent(out) :: ICVG
      
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      IF(SFOVR(P2) .EQ. IABSNT .OR.
     &   SFOVR(P2) .EQ. -99999 .OR.
     &   SFOVR(P2) .GE. 10)     THEN
         ICVG = MISS

      ELSEIF (SFOVR(P2) .EQ. -8888) THEN
         ICVG = 10

      ELSE
         ICVG= SFOVR(P2)
      ENDIF

      RETURN
      END

