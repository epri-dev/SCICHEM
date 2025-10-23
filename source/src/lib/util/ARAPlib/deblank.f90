SUBROUTINE deblank( line )

CHARACTER(*), INTENT( INOUT ) :: line

CHARACTER(1), PARAMETER :: SP   = CHAR(32)

INTEGER i, j, n

line = ADJUSTL(TRIM(line))

n = LEN(line)
IF( n == 0 )THEN
  line = ''
  RETURN
END IF

j = 0
DO i = 1,n
  IF( line(i:i) == SP )CYCLE
  j = j + 1
  line(j:j) = line(i:i)
END DO

line = line(1:j)

RETURN
END

