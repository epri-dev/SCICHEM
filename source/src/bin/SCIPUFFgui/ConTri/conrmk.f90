SUBROUTINE draw_registration()

USE files_fi
USE contri_fi
USE testprt

IMPLICIT NONE

INTEGER i, n, nx, ny, nc
REAL    x0, y0, x1, y1, ratx, raty, xx, dx, yy, dy, sz, tx, ty

REAL, DIMENSION(2) :: x, y

CHARACTER(8) string

ratx = (xmxd-xmnd)/(pxmx-pxmn)
raty = (ymxd-ymnd)/(pymx-pymn)

x0 = xmnd - ratx*pxmn
x1 = xmnd + ratx*(1.-pxmn)
y0 = ymnd - raty*pymn
y1 = ymnd + raty*(1.-pymn)

CALL set( 0.,1.,0.,1.,x0,x1,y0,y1,1 )

CALL gsplci( ipalln+10 )
CALL gslwsc( 0.01 )
CALL gsln( 3 )

CALL get_marks( x0,x1,xx,dx,nx,llc )
CALL get_marks( y0,y1,yy,dy,ny,llc )

sz     = 0.5
string = 'Mg'
CALL GetNCARFontSize( 0.,0.,string(1:2),sz,0.,0.,tx,ty )
tx = 0.25*tx

n = 2
x(1) = x0
x(2) = x1
DO i = 1,ny
  y(1) = yy + FLOAT(i-1)*dy
  y(2) = y(1)
  CALL gpl( n,x,y )
  IF( ixpage == 1  )THEN
  IF( llc  )THEN
    CALL c_format( y(1),nc,string )
    IF( string(nc-1:nc) =='.0' )nc = nc - 2
  ELSE
    CALL d_format( y(1),nc,string )
    IF( string(nc-2:nc) ==':00' )nc = nc - 3
    IF( string(nc-2:nc) ==':00' )nc = nc - 3
  END IF
  CALL plchhq( x(1)+tx,y(1)+tx,string(1:nc),sz,0.,-1. )
  END IF
END DO

y(1) = y0
y(2) = y1
DO i = 1,nx
  x(1) = xx + FLOAT(i-1)*dx
  x(2) = x(1)
  CALL gpl( n,x,y )
  IF( iypage == 1 )THEN
  IF( llc )THEN
    CALL c_format( x(1),nc,string )
    IF( string(nc-1:nc) =='.0' )nc = nc - 2
  ELSE
    CALL d_format( x(1),nc,string )
    IF( string(nc-2:nc) ==':00' )nc = nc - 3
    IF( string(nc-2:nc) ==':00' )nc = nc - 3
  END IF
  CALL plchhq( x(1)-tx,y(1)+tx,string(1:nc),sz,90.,-1. )
  END IF
END DO

CALL gsln( 6 )
CALL gsplci( 1 )

RETURN
END

!==============================================================================

SUBROUTINE get_marks( x0,x1,x,dx,nx,llc )

IMPLICIT NONE

REAL    x0, x1, x, dx
INTEGER nx
LOGICAL llc

REAL    ax,y,z
INTEGER i

ax = ALOG10(x1 - x0)
i  = INT(ax)
IF( ax < 0 )i = i-1
y = ax - FLOAT(i)
z = 10.**y

2000 CONTINUE
IF( llc )THEN
  IF( z <= 3. )THEN
    dx = 1.*10.**i
  ELSE IF( z <= 6. )THEN
    dx = 2.*10.**i
  ELSE
    dx = 5.*10.**i
  END IF
ELSE
  IF( i >= 0 )THEN
    IF( z <= 3. )THEN
      dx = 1.*10.**i
    ELSE IF( z <= 6. )THEN
      dx = 2.*10.**i
    ELSE
      dx = 5.*10.**i
    END IF
  ELSE IF( i == -1 )THEN
    IF( z <= 3. )THEN
      dx = 1.6666667*10.**i
    ELSE IF( z <= 6.  )THEN
      dx = 3.3333333*10.**i
    ELSE
      dx = 5.*10.**i
    END IF
  ELSE if (i == -2 )THEN
    IF( z <= 3. )THEN
      dx = 1.6666667*10.**i
    ELSE IF( z <= 6. )THEN
      dx = 3.3333333*10.**i
    ELSE
      dx = 8.3333333*10.**i
    END IF
  ELSE
    IF( z <= 3. )THEN
      dx = 2.7777777*10.**i
    ELSE IF( z <= 6. )THEN
      dx = 5.5555555*10.**i
    ELSE
      dx = 8.3333333*10.**i
    END IF
  END IF
END IF
x = AINT(x0/dx-0.1)*dx
DO WHILE( x <= x0 )
  x = x + dx
END DO
nx = 1
DO WHILE( x+FLOAT(nx-1)*dx < x1 )
  nx = nx + 1
END DO
nx = nx - 1
IF( nx <= 0 )THEN
  z = z - 3.
  IF( z <= 0. )THEN
    z = z + 10.
    i = i - 1
  END IF
  GOTO 2000
END IF

RETURN
END
