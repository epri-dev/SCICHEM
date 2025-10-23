REAL FUNCTION heavi(x)

IMPLICIT NONE

REAL, INTENT( IN ) :: x

IF( x <= 0. )THEN
  heavi = 0.
ELSE
  heavi = 1.
END IF

RETURN
END
