MODULE reallocate

IMPLICIT NONE

CONTAINS

!==============================================================================

INTEGER FUNCTION reallocate_real1d( a,inc ) RESULT( irv )

INTEGER,       INTENT( IN ) :: inc
REAL, DIMENSION(:), POINTER :: a

REAL, DIMENSION(:), POINTER :: b

INTEGER n, ios, i

IF( inc > 0 )THEN

  n = SIZE(a); b => a; NULLIFY(a)

  ALLOCATE( a(n+inc),STAT=ios )
  IF( ios /= 0 )THEN
    irv = -1; RETURN
  END IF

  n = MIN(n,n+inc)

  DO i = 1,n
    a(i) = b(i)
  END DO

  DEALLOCATE( b,STAT=ios )

ELSE IF( inc < 0 )THEN

  DEALLOCATE( a,STAT=ios )
  ALLOCATE( a(ABS(inc)),STAT=ios )
  IF( ios /= 0 )THEN
    irv = -1; RETURN
  END IF

END IF

irv = 0

RETURN

END FUNCTION reallocate_real1d

!==============================================================================

INTEGER FUNCTION reallocate_integer1d( a,inc ) RESULT( irv )

INTEGER,          INTENT( IN ) :: inc
INTEGER, DIMENSION(:), POINTER :: a

INTEGER, DIMENSION(:), POINTER :: b

INTEGER n, ios, i

n = SIZE(a); b => a; NULLIFY(a)

ALLOCATE( a(n+inc),STAT=ios )
IF( ios /= 0 )THEN
  irv = 1; RETURN
END IF

n = MIN(n,n+inc)

DO i = 1,n
  a(i) = b(i)
END DO

DEALLOCATE( b,STAT=ios )

irv = 0

RETURN

END FUNCTION reallocate_integer1d

!==============================================================================

INTEGER FUNCTION reallocate_real2d( a,inc1,inc2 ) RESULT( irv )

INTEGER,         INTENT( IN ) :: inc1, inc2
REAL, DIMENSION(:,:), POINTER :: a

REAL, DIMENSION(:,:), POINTER :: b

INTEGER n1, n2, ios, i, j

n1 = SIZE(a,1); n2 = SIZE(a,2); b => a; NULLIFY(a)

ALLOCATE( a(n1+inc1,n2+inc2),STAT=ios )
IF( ios /= 0 )THEN
  irv = 1; RETURN
END IF

DO j = 1,MIN(n2,n2+inc2)
  DO i = 1,MIN(n1,n1+inc1)
    a(i,j) = b(i,j)
  END DO
END DO

DEALLOCATE( b,STAT=ios )

irv = 0

RETURN

END FUNCTION reallocate_real2d

!==============================================================================

INTEGER FUNCTION reallocate_dble1d( a,inc ) RESULT( irv )

INTEGER,       INTENT( IN ) :: inc
DOUBLE PRECISION, DIMENSION(:), POINTER :: a

DOUBLE PRECISION, DIMENSION(:), POINTER :: b

INTEGER n, ios, i

IF( inc > 0 )THEN

  n = SIZE(a); b => a; NULLIFY(a)

  ALLOCATE( a(n+inc),STAT=ios )
  IF( ios /= 0 )THEN
    irv = -1; RETURN
  END IF

  n = MIN(n,n+inc)

  DO i = 1,n
    a(i) = b(i)
  END DO

  DEALLOCATE( b,STAT=ios )

ELSE IF( inc < 0 )THEN

  DEALLOCATE( a,STAT=ios )
  ALLOCATE( a(ABS(inc)),STAT=ios )
  IF( ios /= 0 )THEN
    irv = -1; RETURN
  END IF

END IF

irv = 0

RETURN

END FUNCTION reallocate_dble1d

END MODULE reallocate
