SUBROUTINE plchhq( xx,yy,clab,fsz,rot,cntr )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

REAL,         INTENT( IN ) :: xx,yy
CHARACTER(*), INTENT( IN ) :: clab
REAL,         INTENT( IN ) :: fsz
REAL,         INTENT( IN ) :: rot
REAL,         INTENT( IN ) :: cntr

TYPE( T_POINT ) ipos
TYPE( T_SIZE  ) TSZ

CHARACTER(120) ctem

INTEGER icol,jcol,itcol,ibcol,irop,imode,ialign,i
INTEGER irot,ifsz,ix,iy,nch,jtcol,jbcol,jrop,jmode,jalign
INTEGER(POINTER_LEN) iold,inew
INTEGER irv
REAL    xtst,x,y

!     Prepare to write

IF( lprt )THEN
  CALL setRGBcolor( itcolor,icol )
  CALL setRGBcolor( itbcolor,jcol )
ELSE
  icol = PALETTEINDEX(INT2(itcolor))
  jcol = PALETTEINDEX(INT2(itbcolor))
END IF
itcol = SetTextColor( ihdcnc,icol )
ibcol = SetBkColor(   ihdcnc,jcol )
irop  = SetROP2(      ihdcnc,R2_COPYPEN )
imode = SetBkMode(    ihdcnc,itbmode )

!     Set Alignment

IF( cntr == 0. )THEN
  i = IOR(TA_BASELINE,IOR(TA_UPDATECP,TA_CENTER))
ELSE IF( cntr < 0. )THEN
  i = IOR(TA_BASELINE,IOR(TA_UPDATECP,TA_LEFT))
ELSE
  i = IOR(TA_BASELINE,IOR(TA_UPDATECP,TA_RIGHT))
END IF
ialign = SetTextAlign( ihdcnc,i )

!     Set Rotation

irot = NINT(rot*10.)
DO WHILE( irot < 0 )
 irot = irot + 3600
END DO
DO WHILE( irot > 3600 )
 irot = irot - 3600
END DO

!     Create Font

ifsz = NINT(FLOAT(ncres)*fontnc*ABS(fsz))
CALL SetFont( ihdcnc,NCARfont,ifsz,irot,400,inew,iold )

!     Set Text

nch = LEN_TRIM(clab)
ctem = clab(1:nch)//CHAR(0)

!     Set Position

IF( fsz > 0.0 )THEN
  xtst = FLOAT(HUGE(ix)/100)
  x    = ratx*(xx-xorg)
  y    = raty*(yy-yorg)
  IF( ABS(x) > xtst )x = SIGN(xtst,x)
  IF( ABS(y) > xtst )y = SIGN(xtst,y)
  ix  = ixorg + NINT(x)
  iy  = iyorg + NINT(y)
  irv = MoveToEx( ihdcnc,ix,iy,ipos )

!     Write the string

  irv = TextOut( ihdcnc,0,0,ctem,nch )
END IF


!     Save String Size

irv = GetTextExtentPoint( ihdcget,ctem,nch,TSZ )
sszwnc =  FLOAT(TSZ%cx)/ratx
sszhnc = -FLOAT(TSZ%cy)/raty

!     Restore Device Context

jtcol  = SetTextColor( ihdcnc,itcol )
jbcol  = SetBkColor(   ihdcnc,ibcol )
IF( irop /= 0  )jrop  = SetROP2( ihdcnc,irop )
IF( imode /= 0 )jmode = SetBkMode( ihdcnc,imode )

jalign = SetTextAlign( ihdcnc,ialign )

CALL RestoreFont( ihdcnc,inew,iold )

IF(fsz > 0.0 )irv = MoveToEx( ihdcnc,ipos%x,ipos%y,ipos )

RETURN
END
!*******************************************************************************
!         Set Font
!*******************************************************************************

SUBROUTINE SetFont( ihdldc,font,ifsz,irot,iwgt,inew,iold )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN),      INTENT( IN  ) :: ihdldc
CHARACTER(*),              INTENT( IN  ) :: font
INTEGER,                   INTENT( IN  ) :: ifsz
INTEGER,                   INTENT( IN  ) :: irot
INTEGER(POINTER_LEN),      INTENT( OUT ) :: inew
INTEGER(POINTER_LEN),      INTENT( OUT ) :: iold
INTEGER,                   INTENT( IN  ) :: iwgt

CHARACTER(16) cfnt

cfnt  = TRIM(font)//CHAR(0)
inew  = CreateFont( -ifsz,0,irot,irot,iwgt,0,0,0,0,0,0,1,0,cfnt )

iold  = SelectObject(ihdldc,inew)

RETURN
END
!*******************************************************************************
!         Restore Font
!*******************************************************************************

SUBROUTINE RestoreFont( ihdldc,inew,iold )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: ihdldc
INTEGER(POINTER_LEN), INTENT( IN ) :: inew
INTEGER(POINTER_LEN), INTENT( IN ) :: iold

INTEGER i
LOGICAL irv

i   = SelectObject( ihdldc,iold )
irv = DeleteObject( inew )

RETURN
END
!*******************************************************************************
!         Set Default Font
!*******************************************************************************

SUBROUTINE SetNCARFont( font )

USE ncarlib_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: font

NCARfont = TRIM(font)

RETURN
END
