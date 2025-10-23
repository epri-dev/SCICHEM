      SUBROUTINE CLOUDS (P3,ICLTYP,IABSNT,MISS)
C=====================================================================**
C         CLOUDS Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Interpret the cloud type, mapped to the TD3280 codes
C
C     Called by: D144LV, D028LV
C
C     Arguments:
C       P3      Input   First overpunch character in the coverage field
C       ICLTYP  Output  Cloud type
C       IABSNT  Input   Missing value indicator
C       MISS    Input   Missing value flag
C
C     Revision history:
C       11/30/94 - Made into separate subroutine from an ENTRY point
C                 (PES, Inc.)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations
      INTEGER P3, ICLTYP, MISS, IABSNT
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      IF(SFOVR(P3) .EQ. IABSNT) THEN
         ICLTYP = MISS
      ELSEIF (SFOVR(P3) .EQ. -8888) THEN
         ICLTYP = 98
      ELSE
         ICLTYP= SFOVR(P3)
         IF( ICLTYP .EQ. 0) THEN
            CONTINUE
         ELSEIF(ICLTYP .EQ. 1) THEN
            ICLTYP = 45
         ELSEIF(ICLTYP .EQ. 2) THEN
            ICLTYP = 16
         ELSEIF(ICLTYP .EQ. 3) THEN
            ICLTYP = 15
         ELSEIF(ICLTYP .EQ. 4) THEN
            ICLTYP = 11
         ELSEIF(ICLTYP .EQ. 5) THEN
            ICLTYP = 18
         ELSEIF(ICLTYP .EQ. 6) THEN
            ICLTYP = 21
         ELSEIF(ICLTYP .EQ. 7) THEN
            ICLTYP = 23
         ELSEIF(ICLTYP .EQ. 8) THEN
            ICLTYP = 32
         ELSEIF(ICLTYP .EQ. 9) THEN
            ICLTYP = 37
         ELSEIF(ICLTYP .EQ. 12) THEN
            ICLTYP = 13
         ELSEIF(ICLTYP .EQ. 14) THEN
            ICLTYP = 17
         ELSEIF(ICLTYP .EQ. 15) THEN
            ICLTYP = 19
         ELSEIF(ICLTYP .EQ. 16) THEN
            ICLTYP = 22
         ELSEIF(ICLTYP .EQ. 17) THEN
            ICLTYP = 28
         ELSEIF(ICLTYP .EQ. 19) THEN
            ICLTYP = 39
         ELSE
            ICLTYP = MISS
         ENDIF
      ENDIF

      RETURN
      END

