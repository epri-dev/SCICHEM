      SUBROUTINE ICHRND( PATH,IYDY,IYR,IJD )
C=====================================================================**
C          ICHRND Module of the AERMET Meteorological Preprocessor
C
C  Purpose:  Converts a common sequential day to a date between 1900 and
C            1999.  The zero point (day 1) is the first day of 1900.
C
C  Argument list:
C          Input:   IYDY - common sequential day
C          Output:  IYR  - year (2 digits)
C                   IJD  - julian day (3 digits)
C
C  CALLED BY:     MERGE
C
C  CALLS TO:      ERROR
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INTEGER IYDY,IYR,IJD
      INTEGER IYDYO, IYDYN, NDAY
      INCLUDE 'WORK1.INC'

      LOC = '  ICHRND'
      IYDYO = 0

C      IYDYO   = Old sequential day
C      IYDYN   = New sequential day
C-----------------------------------------------------------------------
C *** Check for invalid data

      IF( IYDY.LT.1 )THEN
         MESS =  BLNK80
         WRITE(MESS,1000) IYDY
         CALL ERRHDL(IYDY,PATH,'E19',LOC,MESS)
         IYR = -99
         IJD = -99
         RETURN
      ENDIF

C *** Convert common sequential day to 2-digit year and julian day
C     First, determine if the chronological day in the 1900's
C     (chronological day <=36525) or 2000's (chronological day > 36525)

      IF( IYDY .LE. 36525 )THEN
         IYDYO = IYDY
      ELSEIF( IYDY .GT. 36525 )THEN
         IYDYO = IYDY - 36525
      ENDIF

      IYR = 0

 10   CONTINUE
 
      IF( MOD(IYR,4) .EQ. 0 )THEN
         NDAY = 366
      ELSE
         NDAY = 365
      ENDIF

      IYDYN = IYDYO - NDAY

C *** When the chronological day becomes negative, the year and julian
C      day have been determined

      IF( IYDYN .GT. 0 )THEN
         IYR = IYR + 1
         IYDYO = IYDYN
         GO TO 10
      ELSEIF( IYDYN .LE. 0 )THEN
         IJD = IYDYO
      ENDIF

C *** Check for valid year and julian day

      IF( IYR .GT. 99 )THEN
         GO TO 999
      ELSE
         IF( MOD(IYR,4) .EQ. 0 )THEN
            NDAY = 366
         ELSE
            NDAY = 365
         ENDIF

         IF( (IJD .LT. 0) .OR. (IJD.GT.NDAY) )THEN
            GO TO 999
         ENDIF
      ENDIF

      RETURN

  999 MESS =  BLNK80
      WRITE(MESS,2000)
      CALL ERRHDL(IYDY,PATH,'E19',LOC,MESS)
      IYR = -99
      IJD = -99
      RETURN

C-----------------------------------------------------------------------
C *** Format statements.

 1000 FORMAT(' CHRONOLOGICAL DAY NOT VALID ',I7)
 2000 FORMAT(' YEAR/JULIAN DAY NOT VALID; SET TO -99' )

      END

