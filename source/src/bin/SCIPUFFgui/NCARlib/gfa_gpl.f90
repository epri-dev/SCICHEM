SUBROUTINE gfa( n,x,y )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: n
REAL, DIMENSION(n), INTENT( IN ) :: x,y

TYPE( T_POINT ) pl(MAXPNT)

INTEGER i,j,imax,np,iorop
INTEGER irv
INTEGER(POINTER_LEN)ipn,iopn,jjj

CALL SetPen()
CALL SetBrush()

DO i = 1,n,MAXPNT-1
  imax = MIN(i+MAXPNT-1,n)
  np   = imax - i + 1
  DO j = 1,np
    pl(j)%x = ixorg + NINT(ratx*(x(i+j-1)-xorg))
    pl(j)%y = iyorg + NINT(raty*(y(i+j-1)-yorg))
  END DO
  IF( ibmode == SPECIAL )THEN
    irv   = Polyline( ihdcnc,pl(1),np )
    ipn   = GetStockObject( NULL_PEN )
    iopn  = SelectObject( ihdcnc,ipn )
    iorop = SetROP2( ihdcnc,R2_MASKPEN )
    irv   = Polygon( ihdcnc,pl(1),np )
    jjj   = SelectObject( ihdcnc,iopn )
    IF( iorop /= 0 )jjj = SetROP2( ihdcnc,iorop )
    irv = DeleteObject( ipn )
  ELSE
    irv = Polygon( ihdcnc,pl(1),np )
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE gpl( n,x,y )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: n
REAL, DIMENSION(n), INTENT( IN ) :: x,y

INTEGER nn,i,j,imax,np
LOGICAL irv

TYPE( T_POINT ) pl(MAXPNT)

nn = n
CALL checkpoly( nn,x,y )
IF( nn <= 0 )RETURN

CALL SetPen()

DO i = 1,n,MAXPNT-1
  imax = MIN(i+MAXPNT-1,n)
  np   = imax - i + 1
  DO j = 1,np
    pl(j)%x = ixorg + NINT(ratx*(x(i+j-1)-xorg))
    pl(j)%y = iyorg + NINT(raty*(y(i+j-1)-yorg))
  END DO
  irv = Polyline( ihdcnc,pl(1),np )
END DO

RETURN
END

!==============================================================================

SUBROUTINE gfellips( x,y )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( IN ) :: x,y

INTEGER xl,xr,yt,yb
LOGICAL irv

CALL SetPen()
CALL SetBrush()

xl = ixorg + NINT(ratx*(x(1)-xorg))
xr = ixorg + NINT(ratx*(x(2)-xorg))
yt = iyorg + NINT(raty*(y(3)-yorg))
yb = iyorg + NINT(raty*(y(1)-yorg))

irv = Ellipse( ihdcnc,xl,yt,xr,yb )

RETURN
END

!==============================================================================

SUBROUTINE gellips(x,y)

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( IN ) :: x,y

INTEGER xl,xr,yt,yb
INTEGER(POINTER_LEN)i,ibr,iobr
INTEGER irv

CALL SetPen()

ibr  = GetStockObject( NULL_BRUSH )
iobr = SelectObject( ihdcnc,ibr )

xl = ixorg + NINT(ratx*(x(1)-xorg))
xr = ixorg + NINT(ratx*(x(2)-xorg))
yt = iyorg + NINT(raty*(y(3)-yorg))
yb = iyorg + NINT(raty*(y(1)-yorg))

irv = Ellipse( ihdcnc,xl,yt,xr,yb )

i   = SelectObject( ihdcnc,iobr )
irv = DeleteObject( ibr )

RETURN
END

!==============================================================================

SUBROUTINE checkpoly( n,x,y )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER,            INTENT( INOUT ) :: n
REAL, DIMENSION(n), INTENT( IN    ) :: x,y

TYPE( T_RECT  ) rec
TYPE( T_POINT ), DIMENSION(2) :: pt

LOGICAL lok
INTEGER dxp,dyp,ixmax,ixmin,iymax,iymin,i,ix,iy
INTEGER jxmax,jxmin,jymax,jymin
REAL    xx,yy,xtst

ixmax = -10000000
ixmin =  10000000
iymax = -10000000
iymin =  10000000
xtst  = FLOAT(HUGE(i)/100)
DO i = 1,n
  xx  = ratx*(x(i)-xorg)
  yy  = raty*(y(i)-yorg)
  IF( ABS(xx) > xtst )xx = SIGN(xtst,xx)
  IF( ABS(yy) > xtst )yy = SIGN(xtst,yy)
  ix  = ixorg + NINT(xx)
  iy  = iyorg + NINT(yy)
  ixmax = MAX(ixmax,ix)
  iymax = MAX(iymax,iy)
  ixmin = MIN(ixmin,ix)
  iymin = MIN(iymin,iy)
END DO

pt(1)%x = 0
pt(1)%y = 0
pt(2)%x = 2
pt(2)%y = 2

lok = DPtoLP(  ihdcnc,pt(1),2 )

dxp = pt(2)%x - pt(1)%x
dyp = pt(2)%y - pt(1)%y

jxmax = MIN(ixmax,32766)
jymax = MIN(iymax,32766)
jxmin = MIN(ixmin,jxmax-dxp)
jymin = MIN(iymin,jymax-dyp)
jxmin = MAX(jxmin,-32766)
jymin = MAX(jymin,-32766)
jxmax = MAX(jxmax,jxmin+dxp)
jymax = MAX(jymax,jymin+dyp)

rec%left   = jxmin
rec%right  = jxmax
rec%top    = jymax
rec%bottom = jymin


IF( .NOT.RectVisible(ihdcnc,rec) )n = 0

RETURN
END

!==============================================================================

SUBROUTINE gaa(xo,yo,rx,ry,as,ae)

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

REAL, INTENT( IN ) :: xo,yo,rx,ry,as,ae

INTEGER n,itop,ilft,ibot,irgt,isx,iex,isy,iey
LOGICAL irv

REAL, DIMENSION(4) :: x,y

CALL SetPen()

n = 4
x(1) = xo - rx
x(2) = xo - rx
x(3) = xo + rx
x(4) = xo + rx
y(1) = yo - ry
y(2) = yo + ry
y(3) = yo + ry
y(4) = yo - ry

CALL checkpoly( n,x,y )
IF (n <= 0 )RETURN

ilft = ixorg + NINT(ratx*((xo-rx)-xorg))
irgt = ixorg + NINT(ratx*((xo+rx)-xorg))
itop = iyorg + NINT(raty*((yo+ry)-yorg))
ibot = iyorg + NINT(raty*((yo-ry)-yorg))

x(1) = xo + rx*COSD(as)
y(1) = yo + ry*SIND(as)
x(2) = xo + rx*COSD(ae)
y(2) = yo + ry*SIND(ae)

isx = ixorg + NINT(ratx*(x(1)-xorg))
iex = ixorg + NINT(ratx*(x(2)-xorg))
isy = iyorg + NINT(raty*(y(1)-yorg))
iey = iyorg + NINT(raty*(y(2)-yorg))

irv = Arc( ihdcnc,ilft,itop,irgt,ibot,isx,isy,iex,iey )

RETURN
END
