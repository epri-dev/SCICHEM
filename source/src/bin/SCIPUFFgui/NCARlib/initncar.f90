SUBROUTINE InitNCARGraphics( instance )

USE ncarlib_fi
USE lastset_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: instance

INTEGER iflg
REAL    pxmn
REAL    pxmx
REAL    pymn
REAL    pymx
REAL    xmnd
REAL    xmxd
REAL    ymnd
REAL    ymxd
INTEGER ired,igreen,iblue,i
INTEGER irv

pxmn0 = 0.
pxmx0 = 1.
pymn0 = 0.
pymx0 = 1.
xmnd0 = 0.
xmxd0 = 1.
ymnd0 = 0.
ymxd0 = 1.
iflg0 = -1
pxmn = 0.
pxmx = 1.
pymn = 0.
pymx = 1.
xmnd = 0.
xmxd = 1.
ymnd = 0.
ymxd = 1.
iflg = -1

instnc  = instance
ihwndnc  = -999
ihdcnc  = -999
ihdcget = -999
ihbitnc = 0
ihpnc = 0
ihbnc = 0
ihpalnc = 0
ibstyle = 0
ibmode  = OPAQUE
ibcolor = 1
ipstyle = 6
ipcolor = 1
ipwidth = 5
ibstylec = 0
ibmodec  = OPAQUE
ibcolorc = 1
ipstylec = 6
ipcolorc = 1
ipwidthc = 1
itcolor = 1
itbcolor = 0
itbmode = TRANSPARENT
itstyle = 1
newbrush = .true.
newpen   = .true.
ncres    = 0
idevnc   = 0
fontnc   = 0.025
npalnc   = MAXCOLOR+1
NCARfont = 'Arial'

ncpal%palVersion    = '0300'X
ncpal%palNumEntries = npalnc

brushnc%lbHatch = HS_DIAGCROSS

pstylenc = IOR(PS_GEOMETRIC,IOR(PS_ENDCAP_SQUARE,PS_JOIN_BEVEL))

lprt = .false.

DO i = 0,MAXCOLOR
  ired   = ((MAXCOLOR - i)*255)/MAXCOLOR
  igreen = ((           i)*255)/MAXCOLOR
  iblue  = MIN(ired,igreen)
  ncpal%palPalEntry(i+1)%peRed   = INT1(ired)
  ncpal%palPalEntry(i+1)%peGreen = INT1(igreen)
  ncpal%palPalEntry(i+1)%peBlue  = INT1(iblue)
  ncpal%palPalEntry(i+1)%peFlags = PC_NOCOLLAPSE
END DO
irv     = DeleteObject( ihpalnc )
ihpalnc = CreatePalette( MSWIN$ncpal )

RETURN
END
