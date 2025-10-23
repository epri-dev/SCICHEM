SUBROUTINE cupper( line )

CHARACTER(*), INTENT( INOUT ) :: line

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) < 'a' )CYCLE
  IF( line(i:i) > 'z' )CYCLE
  line(i:i) = CHAR(ICHAR(line(i:i)) - 32)
END DO

RETURN
END
