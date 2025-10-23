SUBROUTINE set_plot( lwnd )

USE contri_fi
USE testprt

IMPLICIT NONE

LOGICAL lwnd

REAL rat,xcnt,ycnt,plen,ratx,xlen,ylen,xlat

CALL set_plot_axes( lwnd,xmnd,xmxd,ymnd,ymxd )

xcnt = xcb
ycnt = ycb
IF( boxl > 0 )THEN
  plen = boxl
  xlat = 1.0
ELSE
  IF( llc )THEN
    xlat = 1.0
  ELSE
    xlat = COSD((ymxd+ymnd)/2.)
  END IF
  plen = -boxl*AMAX1((xmxd-xmnd)*xlat,(ymxd-ymnd))
END IF
rat = (ymxd-ymnd)/((xmxd-xmnd)*xlat)
IF( str /= default )THEN
  ratx = str
ELSE
  ratx = rat
  IF( lvl )ratx = ratx/(SQRT((xev-xov)**2 + (yev-yov)**2)/(FLOAT(m0)*dxsrf))
END IF

ratx = MAX(0.2,ratx)
ratx = MIN(5.0,ratx)
IF( ratx <= 1. )THEN
  xlen = plen
  ylen = plen*ratx
ELSE
  ylen = plen
  xlen = plen/ratx
END IF
IF( plen <= 1.)THEN
  IF( (xcnt-0.5*xlen) < 0. .OR. (xcnt+0.5*xlen) > 1.0 )THEN
    xcnt = 0.5
  END IF
  IF( (ycnt-0.5*ylen) < 0. .OR. (ycnt+0.5*ylen) > 1.0 )THEN
    ycnt = 0.5
  END IF
  pxmn = xcnt - 0.5*xlen
  pymn = ycnt - 0.5*ylen
ELSE
  pxmn = 0.5*(FLOAT(nxpage) - xlen) - FLOAT(ixpage-1)
  pymn = 0.5*(FLOAT(nypage) - ylen) - FLOAT(iypage-1)
  xcnt = pxmn + 0.5*xlen
  ycnt = pymn + 0.5*ylen
END IF

pxmx = pxmn + xlen
pymx = pymn + ylen

CALL set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,1 )

RETURN
END

!===============================================================================

SUBROUTINE set_plot_axes( lwnd,xmn,xmx,ymn,ymx )

USE contri_fi

IMPLICIT NONE

LOGICAL lwnd
REAL    xmn,xmx,ymn,ymx

xminp  = (xmin_def - xod)*xsdp + xop
yminp  = (ymin_def - yod)*ysdp + yop
dxsrf  = dxsrf_def*xsdp
dysrf  = dysrf_def*ysdp

xminp  = scx*xminp + shx
yminp  = scy*yminp + shy
dxsrf = scx*dxsrf
dysrf = scy*dysrf

lwnd = .false.
IF( xmnb == default )THEN
  xmn = xminp
ELSE
  xmn = xmnb
  lwnd = .true.
END IF
IF( xmxb == default )THEN
  xmx = xminp + FLOAT(m0)*dxsrf
ELSE
  xmx = xmxb
  lwnd = .true.
END IF
IF( ymnb == default )THEN
  ymn = yminp
ELSE
  ymn = ymnb
  lwnd = .true.
END IF
IF( ymxb == default )THEN
  ymx = yminp + FLOAT(n0)*dysrf
ELSE
  ymx = ymxb
  lwnd = .true.
END IF

RETURN
END

!===============================================================================

SUBROUTINE set_plot_pages( size,AR,Cart,nxpages,nypages )

USE contri_fi

IMPLICIT NONE

REAL    size,AR
INTEGER nxpages,nypages
LOGICAL Cart

REAL    rat,plen,ratx,xlen,ylen,xlat
LOGICAL lwnd

CALL set_plot_axes( lwnd,xmnd,xmxd,ymnd,ymxd )

IF( size > 0 )THEN
  plen = size
  xlat = 1.0
ELSE
  IF( Cart )THEN
    xlat = 1.0
  ELSE
    xlat = COSD((ymxd+ymnd)/2.)
  END IF
  plen = -size*MAX((xmxd-xmnd)*xlat,(ymxd-ymnd))
END IF
rat = (ymxd-ymnd)/((xmxd-xmnd)*xlat)
IF( AR /= default )THEN
  ratx = AR
ELSE
  ratx = rat
END IF
ratx = MAX(0.2,ratx)
ratx = MIN(5.0,ratx)
IF( ratx <= 1. )THEN
  xlen = plen
  ylen = plen*ratx
ELSE
  ylen = plen
  xlen = plen/ratx
END IF

nxpages = INT(xlen) + 1
nypages = INT(ylen) + 1

RETURN
END

