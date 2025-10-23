SUBROUTINE gridal( mtx,ndx,mty,ndy,ilx,ily,fsz )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mtx
INTEGER, INTENT( IN ) :: ndx
INTEGER, INTENT( IN ) :: mty
INTEGER, INTENT( IN ) :: ndy
INTEGER, INTENT( IN ) :: ilx
INTEGER, INTENT( IN ) :: ily
REAL,    INTENT( IN ) :: fsz

CHARACTER(20) cfmt
REAL, DIMENSION(8) :: x,y

INTEGER i,ii,nch,ios
LOGICAL irv
REAL    xhgt,xlen,dx,dy,dxx,dyy

TYPE( T_RECT ) winrec
TYPE( T_POINT) vorg
TYPE( T_SIZE)  vext

irv = GetViewportOrgEx( ihdcnc,vorg )
irv = GetViewportExtEx( ihdcnc,vext )

winrec%top    = vorg%y
winrec%left   = vorg%x
winrec%bottom = vorg%y + vext%cy
winrec%right  = vorg%x + vext%cx

hrgn = CreateRectRgnIndirect( winrec )
ii   = SelectClipRgn( ihdcnc,hrgn )
irv  = DeleteObject( hrgn )

x(1) = xorg
y(1) = yorg
x(2) = xend
y(2) = y(1)
x(3) = x(2)
y(3) = yend
x(4) = xorg
y(4) = y(3)
x(5) = x(1)
y(5) = y(1)

CALL gsplci( 1 )
CALL gsln( 0 )
CALL gslwsc( 0.333 )
CALL gpl( 5,x,y )

y(1) = yorg
y(2) = yorg + 0.015*(yend-yorg)
y(3) = yend
y(4) = yend - 0.015*(yend-yorg)
y(5) = yorg
y(6) = yorg + 0.01*(yend-yorg)
y(7) = yend
y(8) = yend - 0.01*(yend-yorg)

cfmt ='0'//CHAR(0)
nch  = 1
CALL plchhq( 0,0,cfmt(1:nch),-1.,0.,0. )
xhgt = sszhnc
xlen = sszwnc
dy = 1.2*xhgt*fsz
dx = (xend-xorg)/FLOAT(MAX(1,mtx))
dxx = dx/FLOAT(MAX(1,ndx))
DO i = 1,mtx+1
  x(1) = xorg + FLOAT(i-1)*dx
  x(2) = x(1)
  x(3) = x(1)
  x(4) = x(1)
  CALL gpl( 2,x(1),y(1) )
  CALL gpl( 2,x(3),y(3) )
  IF( ilx > 0 )THEN
    WRITE( cfmt,cfmtx(1:nchfx),IOSTAT=ios )x(1)  !label tick mark
    IF( ios /= 0 )cfmt ='Err'
    nch = LEN_TRIM(cfmt)
    CALL plchhq( x(1),y(1)-dy,cfmt(1:nch),fsz,0.,0. )
  END IF
  IF( ndx > 1 .AND. i <= mtx )THEN
    DO ii = 2,ndx
      x(5) = x(1) + FLOAT(ii-1)*dxx
      x(6) = x(5)
      x(7) = x(5)
      x(8) = x(5)
      CALL gpl( 2,x(5),y(5) )
      CALL gpl (2,x(7),y(7) )
    END DO
  END IF
END DO

x(1) = xorg
x(2) = xorg + 0.015*(xend-xorg)
x(3) = xend
x(4) = xend - 0.015*(xend-xorg)
x(5) = xorg
x(6) = xorg + 0.01*(xend-xorg)
x(7) = xend
x(8) = xend - 0.01*(xend-xorg)

dx = 0.5*xlen*fsz
dy = (yend-yorg)/FLOAT(MAX(1,mty))
dyy = dy/FLOAT(MAX(1,ndy))
do i = 1,mty+1
  y(1) = yorg + FLOAT(i-1)*dy
  y(2) = y(1)
  y(3) = y(1)
  y(4) = y(1)
  CALL gpl( 2,x(1),y(1) )
  CALL gpl( 2,x(3),y(3) )
  IF( ily > 0 )then
    WRITE(cfmt,cfmty(1:nchfy),IOSTAT=ios)y(1)   !label tick mark
    IF( ios /= 0 )cfmt ='Err'
    nch = LEN_TRIM(cfmt)
    CALL plchhq( x(1)-dx,y(1)-0.4*xhgt*fsz,cfmt(1:nch),fsz,0.,1. )
  END IF
  IF( ndy > 1 .AND. i <= mty )THEN
    DO ii = 2,ndy
      y(5) = y(1) + FLOAT(ii-1)*dyy
      y(6) = y(5)
      y(7) = y(5)
      y(8) = y(5)
      CALL gpl( 2,x(5),y(5) )
      CALL gpl( 2,x(7),y(7) )
    END DO
  END IF
END DO

RETURN
END
