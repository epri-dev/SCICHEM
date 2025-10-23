SUBROUTINE untabify( line )

CHARACTER(*), INTENT( INOUT ) :: line

CHARACTER(1), PARAMETER :: TAB = CHAR(9)

INTEGER i

DO i = 1,LEN(line)
  IF( line(i:i) == TAB )line(i:i) = ' '
END DO

RETURN
END

