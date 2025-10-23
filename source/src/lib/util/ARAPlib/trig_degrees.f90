MODULE TrigConst
  SAVE
  REAL, PARAMETER :: DEG2RAD = 3.141592653 / 180.
  REAL, PARAMETER :: RAD2DEG = 180. / 3.141592653
END MODULE TrigConst

!==============================================================================

REAL FUNCTION sind( x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: x

sind = SIN( x*DEG2RAD )

RETURN
END

!==============================================================================

REAL FUNCTION cosd( x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: x

cosd = COS( x*DEG2RAD )

RETURN
END

!==============================================================================

REAL FUNCTION tand( x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: x

tand = TAN( x*DEG2RAD )

RETURN
END

!==============================================================================

REAL FUNCTION asind( x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: x

asind = ASIN(x)*RAD2DEG

RETURN
END

!==============================================================================

REAL FUNCTION acosd( x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: x

acosd = ACOS(x)*RAD2DEG

RETURN
END


!==============================================================================

REAL FUNCTION atan2d( y,x )

USE TrigConst

IMPLICIT NONE

REAL, INTENT( IN ) :: y, x

atan2d = ATAN2(y,x)*RAD2DEG

RETURN
END

