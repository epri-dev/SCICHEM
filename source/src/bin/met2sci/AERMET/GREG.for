      SUBROUTINE GREG( YEAR,JDAY,MNTH,MDAY )
C=====================================================================**
C        GREG Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the Gregorian month and day given the year
C               and Julian day
C
C     Called by:  Several routines that needs to convert Julian to
C                 Gregorian day
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER*4 YEAR,JDAY,MNTH,MDAY,L,J
C
C      YEAR = CALENDAR YEAR
C      JDAY = DAY OF YEAR -- 1, 365, OR 366
C      MNTH = MONTH OF YEAR -- 1, 12
C      MDAY = DAY OF MONTH -- 1, 31
C
C-----------------------------------------------------------------------
      L=365

      IF(MOD(YEAR,4).EQ.  0) L=366
      IF(MOD(YEAR,100).EQ.0) L=365
      IF(MOD(YEAR,400).EQ.0) L=366

      J=MOD(JDAY+305,L)
      J=MOD(J,153)/61+(J/153)*2+J
      MNTH=MOD(J/31+2,12)+1
      MDAY=MOD(J,31)+1

      RETURN
      END

      FUNCTION JULIAN( YR1,MO1,DY1 )
C=====================================================================**
C        JULIAN Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To convert Gregorian month and day to Julian day for the
C               given year.
C
C     Limitations:  Works only for years 1901 - 2099.
C
C     Called by:  Several routines that needs to convert Gregorian to
C                 Julian day
C
C     Calls to:      -NONE-
C
C=======================================================================
C
C    LOCAL VARIABLES
C
        INTEGER YR1,MO1,DY1,JULIAN
C
        INCLUDE 'WORK1.INC'
C
C        YR1        LAST 2-DIGITS CALENDAR YEAR
C        MO1        MONTH OF YEAR (1-12)
C        DY1        DAY OF MONTH (1-31)
C-----------------------------------------------------------------------

      IF( YR1.LT.0 .OR. MO1.LE.0 .OR. DY1.LE.0 )THEN
C ---    Year is out-of-range
         JULIAN = 0
      ELSEIF( MO1.GT.12 .OR. DY1.GT.31 )THEN
C ---    Month and/or day exceeds upper bounds
         JULIAN = 999
      ELSE

         IWORK1(1) = MOD( (MO1+9),12 )
         IWORK1(2) = (IWORK1(1)*153+2)/5 + DY1 + 58

         IF( MOD(YR1,4).EQ.0 )THEN
            IWORK1(3) = 366
            IWORK1(2) = IWORK1(2) + 1
         ELSE
            IWORK1(3) = 365
         ENDIF

         JULIAN = 1 + MOD( IWORK1(2),IWORK1(3) )

      ENDIF

      RETURN
      END

