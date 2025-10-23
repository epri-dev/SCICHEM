SUBROUTINE plot_resolution(dx,dy,nn)

IMPLICIT NONE

REAL dx,dy
INTEGER nn

REAL x,y,ratx,raty

CALL GetNCARRates(ratx,raty)

x =  dx*ratx/20.
y = -dy*raty/20.

x = MAX(x,y)

y = LOG(x)/LOG(2.)

nn = MAX(INT(y) + 1 , 0)

RETURN
END
