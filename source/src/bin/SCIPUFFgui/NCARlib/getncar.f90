!===============================================================================

SUBROUTINE GetNCARWindow( iwind )

USE ncarlib_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( OUT ) :: iwind

iwind = ihwndnc

RETURN
END

!===============================================================================

SUBROUTINE GetNCARDC( ihdc,jhdc )

USE ncarlib_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( OUT ) :: ihdc
INTEGER(POINTER_LEN), INTENT( OUT ) :: jhdc

ihdc = ihdcnc
jhdc = ihdcget

RETURN
END

!===============================================================================

SUBROUTINE GetNCARPalette( ihpal )

USE ncarlib_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( OUT ) :: ihpal

ihpal = ihpalnc

RETURN
END

!===============================================================================

SUBROUTINE GetNCARRates( xrat,yrat )

USE ncarlib_fi

IMPLICIT NONE

REAL,    INTENT( OUT ) :: xrat
REAL,    INTENT( OUT ) :: yrat

xrat = ratx
yrat = raty

RETURN
END

!===============================================================================

SUBROUTINE GetNCARTransformX( xo,xe,xrat,ixo )

USE ncarlib_fi

IMPLICIT NONE

REAL,    INTENT( OUT ) :: xo
REAL,    INTENT( OUT ) :: xe
REAL,    INTENT( OUT ) :: xrat
INTEGER, INTENT( OUT ) :: ixo

xo   = xorg
xe   = xend
xrat = ratx
ixo  = ixorg

RETURN
END

!===============================================================================

SUBROUTINE GetNCARTransformY( yo,ye,yrat,iyo )

USE ncarlib_fi

IMPLICIT NONE

REAL,    INTENT( OUT ) :: yo
REAL,    INTENT( OUT ) :: ye
REAL,    INTENT( OUT ) :: yrat
INTEGER, INTENT( OUT ) :: iyo

yo   = yorg
ye   = yend
yrat = raty
iyo  = iyorg

RETURN
END

!===============================================================================

SUBROUTINE NCARTransform( x,y,ix,iy )

USE ncarlib_fi

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x
REAL,    INTENT( IN  ) :: y
INTEGER, INTENT( OUT ) :: ix
INTEGER, INTENT( OUT ) :: iy

ix = ixorg + NINT(ratx*(x-xorg))
iy = iyorg + NINT(raty*(y-yorg))

RETURN
END

!===============================================================================

SUBROUTINE NCARTransformI(ix,iy,x,y)

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: ix
INTEGER, INTENT( IN  ) :: iy
REAL,    INTENT( OUT ) :: x
REAL,    INTENT( OUT ) :: y

x = xorg + FLOAT(ix-ixorg)/ratx
y = yorg + FLOAT(iy-iyorg)/raty

RETURN
END

!===============================================================================

SUBROUTINE GetNCARFontSize( x,y,string,fnt,rot,cntr,sizh,sizv )

USE ncarlib_fi

IMPLICIT NONE

REAL,         INTENT( IN  ) :: x
REAL,         INTENT( IN  ) :: y
CHARACTER(*), INTENT( IN ) :: string
REAL,         INTENT( IN  ) :: fnt
REAL,         INTENT( IN  ) :: rot
REAL,         INTENT( IN  ) :: cntr
REAL,         INTENT( OUT ) :: sizv
REAL,         INTENT( OUT ) :: sizh

CALL plchhq( x,y,string,-fnt,rot,cntr )

sizh = sszwnc
sizv = sszhnc

RETURN
END

!===============================================================================

SUBROUTINE GetNCARResolution( nres )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: nres

nres = ncres

RETURN
END

!===============================================================================

LOGICAL FUNCTION NCARPrint()

USE ncarlib_fi

IMPLICIT NONE

NCARPrint = lprt

RETURN
END


