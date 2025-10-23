SUBROUTINE interp( xi,ni,vi,xo,no,vo )

IMPLICIT NONE

!------ 1-d linear interpolation routine - no extrapolation

REAL, DIMENSION(*) :: xi, vi, xo, vo
INTEGER ni, no

INTEGER i, ii, j, jj
REAL    rate

!------ points below xi(1)

DO i = 1,no
  IF( xo(i) >= xi(1) )GOTO 10
END DO

DO i = 1,no
  vo(i) = vi(1)
END DO

RETURN

10 CONTINUE
IF( i > 1 )THEN
  ii = i - 1
  DO i = 1,ii
    vo(i) = vi(1)
  END DO
ELSE
  ii = 0
END IF

!------ set internal points

jj = 2

DO i = ii+1,no

  DO j = jj,ni
    IF( xi(j) >= xo(i) )GOTO 20
  END DO

  GOTO 30

20 CONTINUE
  rate = (xo(i) - xi(j-1))/(xi(j) - xi(j-1))
  vo(i) = vi(j-1) + rate*(vi(j)- vi(j-1))
  jj = j

END DO

RETURN

!------ points above xi(ni)

30 CONTINUE
ii = i
DO i = ii,no
  vo(i) = vi(ni)
END DO

RETURN
END
