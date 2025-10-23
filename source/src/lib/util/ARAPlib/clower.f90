SUBROUTINE clower(line)

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) < 'A' )CYCLE
  IF( line(i:i) > 'Z' )CYCLE
  line(i:i) = CHAR(ICHAR(line(i:i)) + 32)
END DO

RETURN
END
