SUBROUTINE gsfaci( icolor )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icolor

ibcolor = icolor
newbrush = newbrush .or. ibcolor /= ibcolorc

RETURN
END

!==============================================================================

SUBROUTINE gsplci( icolor )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icolor

ipcolor = icolor
newpen = newpen .OR. ipcolor /= ipcolorc

RETURN
END

!==============================================================================

SUBROUTINE gstxci( icolor )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icolor

itcolor = icolor

RETURN
END

!==============================================================================

SUBROUTINE gstbci( icolor )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icolor

itbcolor = icolor

RETURN
END

!==============================================================================

SUBROUTINE gstxst( istyle )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: istyle

itstyle = IABS(istyle)

RETURN
END

!==============================================================================

SUBROUTINE gstxmd( istyle )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: istyle

IF( istyle >= 0 )THEN
  itbmode = TRANSPARENT
ELSE
  itbmode = OPAQUE
END IF

RETURN
END

!==============================================================================

SUBROUTINE gsfais( istyl )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: istyl

INTEGER jstyl

jstyl   = IABS(istyl)
ibstyle = (jstyl/2)*2

IF( istyl < 0 )THEN
  ibmode = SPECIAL
ELSE
  ibmode = 2 - MOD(jstyl,2)
END IF

newbrush = newbrush .OR. ibstyle /= ibstylec .OR. ibmode  /= ibmodec

RETURN
END

!==============================================================================

SUBROUTINE gsln( istyl )

USE ncarlib_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: istyl

IF( istyl <= 0 )THEN
  ipstyle = 6
ELSE
  ipstyle = istyl
END IF
newpen = newpen .OR. ipstyle /= ipstylec

RETURN
END

!==============================================================================

SUBROUTINE gslwsc( width )

USE ncarlib_fi

REAL, INTENT( IN ) :: width

INTEGER iwid

iwid = NINT( width*1000./72. )

ipwidth = iwid
newpen = newpen .OR. ipwidth /= ipwidthc

RETURN
END
