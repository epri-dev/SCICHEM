SUBROUTINE gscr( indx,r,g,b,lerr )

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: indx
REAL,    INTENT( IN  ) :: r,g,b
LOGICAL, INTENT( OUT ) :: lerr

INTEGER i

IF( indx > npalnc .OR. indx < 0 .OR. ihpalnc == 0 )RETURN

ncpal%palPalEntry(indx+1)%peRed   = NINT(r*255.)
ncpal%palPalEntry(indx+1)%peGreen = NINT(g*255.)
ncpal%palPalEntry(indx+1)%peBlue  = NINT(b*255.)
ncpal%palPalEntry(indx+1)%peFlags = PC_NOCOLLAPSE

i = SetPaletteEntries( ihpalnc,indx,1,ncpal%palPalEntry(indx+1) )

lerr = i /= 1

newpen   = .true.
newbrush = .true.

RETURN
END

!==============================================================================

SUBROUTINE checkpalette()

USE ncarlib_fi
USE SwinAPI

IMPLICIT NONE

INTEGER i
LOGICAL lerr

TYPE( T_LOGPALETTE ) chkpal

i = GetPaletteEntries( ihpalnc,0,npalnc,chkpal%palPalEntry(1) )

lerr = i /= npalnc
IF( lerr )THEN
  WRITE(11,*)'Error in CheckPalette'
  WRITE(11,*)'Wrong number of palette entries retrieved'
  WRITE(11,*)i,npalnc
END IF

DO i = 0,npalnc-1
  lerr = ncpal%palPalEntry(i+1)%peRed /= chkpal%palPalEntry(i+1)%peRed
  lerr = lerr .OR. ncpal%palPalEntry(i+1)%peGreen /= chkpal%palPalEntry(i+1)%peGreen
  lerr = lerr .OR. ncpal%palPalEntry(i+1)%peBlue /= chkpal%palPalEntry(i+1)%peBlue
  IF( lerr )THEN
    WRITE(11,*)'Error in CheckPalette'
    WRITE(11,*)'Wrong palette entries retrieved'
    WRITE(11,*)'Entry = ',i
    WRITE(11,*)'Red  =',ncpal%palPalEntry(i+1)%peRed,chkpal%palPalEntry(i+1)%peRed
    WRITE(11,*)'Green=',ncpal%palPalEntry(i+1)%peGreen,chkpal%palPalEntry(i+1)%peGreen
    WRITE(11,*)'Blue =',ncpal%palPalEntry(i+1)%peBlue,chkpal%palPalEntry(i+1)%peBlue
  END IF
END DO

RETURN
END
