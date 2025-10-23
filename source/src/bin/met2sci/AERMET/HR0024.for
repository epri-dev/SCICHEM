      SUBROUTINE HR0024(IYR,JULDAY,IHR)
C=====================================================================**
C        HR0024 Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To convert a date/time group from hour=0 to hour=24 of
C               the previous day.
C
C     Called by:     GETSDG, MERGE
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER IYR, JULDAY, IHR
C
C     IYR    = YEAR
C     JULDAY = JULIAN DAY
C     IHR    = IHR
C
      IHR = 24
      IF(JULDAY .EQ. 1) THEN
         IYR = IYR - 1

C*       Account for Y2K
         IF( IYR .LT. 0 )THEN
            IYR = 99
         ENDIF

         IF( MOD(IYR,4).EQ.0 )THEN
            JULDAY = 366
         ELSE
            JULDAY = 365
         ENDIF
      ELSE
         JULDAY = JULDAY - 1
      ENDIF
C
      RETURN
      END

