SUBROUTINE set( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,iflg )

USE ncarlib_fi
USE lastset_fi
USE SwinAPI

IMPLICIT NONE

REAL,    INTENT( IN ) :: pxmn
REAL,    INTENT( IN ) :: pxmx
REAL,    INTENT( IN ) :: pymn
REAL,    INTENT( IN ) :: pymx
REAL,    INTENT( IN ) :: xmnd
REAL,    INTENT( IN ) :: xmxd
REAL,    INTENT( IN ) :: ymnd
REAL,    INTENT( IN ) :: ymxd
INTEGER, INTENT( IN ) :: iflg

TYPE( T_RECT  ) winrec
TYPE( T_POINT ) winpt(2)

INTEGER npxxw,npxyw,npxw,ii
INTEGER irv
INTEGER ixend,iyend
INTEGER jxorg,jyorg,jxend,jyend

pxmn0 = pxmn
pxmx0 = pxmx
pymn0 = pymn
pymx0 = pymx
xmnd0 = xmnd
xmxd0 = xmxd
ymnd0 = ymnd
ymxd0 = ymxd
iflg0 = iflg

IF( ncres <= 0 )THEN

  irv = GetWindowRect( ihwndnc,winrec )
  winpt(1)%x = winrec%left
  winpt(1)%y = winrec%top
  winpt(2)%x = winrec%right
  winpt(2)%y = winrec%bottom
  irv = DPtoLP( ihdcnc,winpt(1),2 )

  npxxw = winpt(1)%x - winpt(2)%x
  npxyw = winpt(1)%y - winpt(2)%y
  npxw  = MIN(npxxw,npxyw)

ELSE

  npxw = ncres

END IF

ixorg = NINT(MAX(0.,pxmn     )*FLOAT(npxw))
iyorg = NINT(MIN(1.,(1.-pymn))*FLOAT(npxw))
ixend = NINT(MIN(1.,pxmx     )*FLOAT(npxw))
iyend = NINT(MAX(0.,(1.-pymx))*FLOAT(npxw))
winpt(1)%x = ixorg
winpt(2)%x = ixend
winpt(1)%y = iyorg
winpt(2)%y = iyend
irv   = LPtoDP(  ihdcnc,winpt(1),2 )
jxorg = winpt(1)%x
jxend = winpt(2)%x + 1
jyorg = winpt(1)%y + 1
jyend = winpt(2)%y

hrgn = CreateRectRgn( jxorg,jyend,jxend,jyorg )
ii   = SelectClipRgn( ihdcnc,hrgn )
irv  = DeleteObject( hrgn )

ixorg = NINT(pxmn     *FLOAT(npxw))
iyorg = NINT((1.-pymn)*FLOAT(npxw))
ixend = NINT(pxmx     *FLOAT(npxw))
iyend = NINT((1.-pymx)*FLOAT(npxw))

npxx = ixend - ixorg
npxy = iyorg - iyend

xorg  = xmnd
yorg  = ymnd
xend  = xmxd
yend  = ymxd

ratx  =  FLOAT(npxx)/(xmxd-xmnd)
raty  = -FLOAT(npxy)/(ymxd-ymnd)

RETURN
END

!==============================================================================

SUBROUTINE getset( pxmn,pxmx,pymn,pymx,xmnd,xmxd,ymnd,ymxd,iflg )

USE ncarlib_fi
USE lastset_fi

IMPLICIT NONE

REAL,    INTENT( OUT ) :: pxmn
REAL,    INTENT( OUT ) :: pxmx
REAL,    INTENT( OUT ) :: pymn
REAL,    INTENT( OUT ) :: pymx
REAL,    INTENT( OUT ) :: xmnd
REAL,    INTENT( OUT ) :: xmxd
REAL,    INTENT( OUT ) :: ymnd
REAL,    INTENT( OUT ) :: ymxd
INTEGER, INTENT( OUT ) :: iflg

pxmn = pxmn0
pxmx = pxmx0
pymn = pymn0
pymx = pymx0
xmnd = xmnd0
xmxd = xmxd0
ymnd = ymnd0
ymxd = ymxd0
iflg = iflg0

RETURN
END
