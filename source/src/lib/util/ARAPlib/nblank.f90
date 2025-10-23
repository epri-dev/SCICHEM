INTEGER FUNCTION nblank(line)

IMPLICIT NONE

CHARACTER(*) line

INTEGER i

DO i = LEN(line),1,-1
   nblank = i
   IF( line(i:i)/=' ' .AND. line(i:i)/=CHAR(0) )RETURN
END DO

nblank = 1

RETURN
END
