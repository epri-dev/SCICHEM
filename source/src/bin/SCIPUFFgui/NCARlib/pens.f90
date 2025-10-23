SUBROUTINE SetBrush()

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

CHARACTER(16) cbrush
INTEGER icol,imode
INTEGER(POINTER_LEN) irv,iobr,ibrold

IF( newbrush )THEN
  ibrold = ihbnc
  IF( ibmode == SPECIAL )THEN
    cbrush  ='NCARBrush'C
    irv = DeleteObject( ihbitnc )
    ihbitnc = LoadBitmap( instnc,cbrush )
    IF( ihbitnc /= 0 )THEN
      ihbnc = CreatePatternBrush( ihbitnc )
    ELSE
      ihbnc = 0
    END IF
  ELSE
    irv = DeleteObject( ihbitnc )
    ihbitnc = 0
    IF( lprt )THEN
      CALL setRGBcolor( ibcolor,brushnc%lbColor )
    ELSE
      icol = PALETTEINDEX(INT2(ibcolor))
      brushnc%lbColor = icol
    END IF
    brushnc%lbStyle = ibstyle
    ihbnc = CreateBrushIndirect( brushnc )
  END IF
  IF( ihbnc /= 0 )THEN
    iobr = SelectObject( ihdcnc,ihbnc )
    irv = DeleteObject( iobr )
    IF( ibrold /= iobr )irv = DeleteObject( ibrold )
  ELSE
    ihbnc = ibrold
  END IF
  IF( ibmode == SPECIAL )THEN
    imode = SetBkMode( ihdcnc,TRANSPARENT )
  ELSE
    imode = SetBkMode( ihdcnc,ibmode )
  END IF
  ibstylec = ibstyle
  ibmodec  = ibmode
  ibcolorc = ibcolor
  newbrush = .false.
END IF

RETURN
END

!===============================================================================

SUBROUTINE SetPen()

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER icol,istyle,iwid
INTEGER(POINTER_LEN)iopn,ipold
LOGICAL irv

IF( newpen )THEN
  IF( lprt )THEN
    CALL setRGBcolor( ipcolor,brushnc%lbColor )
  ELSE
    icol = PALETTEINDEX(INT2(ipcolor))
    brushnc%lbColor = icol
  END IF
  brushnc%lbStyle = BS_SOLID
  istyle = IOR(pstylenc,ipstyle)
  ipold = ihpnc
  ihpnc  = ExtCreatePen( istyle,ipwidth,brushnc,0,0 )
  IF( ihpnc == 0 )THEN
    istyle = ipstyle
    iwid   = ipwidth
    IF( lprt )iwid = MAX(iwid,6)
    ihpnc = CreatePen( istyle,iwid,brushnc%lbColor )
  END IF
  IF( ihpnc /= 0 )THEN
    iopn = SelectObject( ihdcnc,ihpnc )
    irv = DeleteObject( iopn )
    IF( ipold /= iopn )irv = DeleteObject( ipold )
  ELSE
    ihpnc = ipold
  END IF
  ipstylec = ipstyle
  ipcolorc = ipcolor
  ipwidthc = ipwidth
  newpen   = .false.
END IF

RETURN
END

!===============================================================================

SUBROUTINE ClearBrush()

USE ncarlib_fi

IMPLICIT NONE

newbrush = .true.
ibstyle = 0
ibcolor = 1

RETURN
END

!===============================================================================

SUBROUTINE ClearPen()

USE ncarlib_fi

IMPLICIT NONE

newpen = .TRUE.
ipstyle = 6
ipcolor = 1
ipwidth = 5

RETURN
END

!===============================================================================

SUBROUTINE setRGBcolor( indx,icol )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: indx
INTEGER, INTENT( OUT ) :: icol

IF( indx > npalnc .OR. indx < 0 )THEN
  icol = RGB( INT1(0),INT1(0),INT1(0) )
ELSE
  icol = RGB( ncpal%palPalEntry(indx+1)%peRed,   &
              ncpal%palPalEntry(indx+1)%peGreen, &
		          ncpal%palPalEntry(indx+1)%peBlue )
END IF

RETURN
END

