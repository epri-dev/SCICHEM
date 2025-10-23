MODULE SWIMutilArrayPtr

IMPLICIT NONE

CONTAINS

!==============================================================================

SUBROUTINE CopyArray( a,b,n )

INTEGER,      INTENT( IN  ) :: n
REAL, DIMENSION(:), POINTER :: b
REAL, DIMENSION(:), POINTER :: a

INTEGER i

!$OMP PARALLEL DO PRIVATE(i) SHARED(a,b)
DO i = 1,n
  a(i) = b(i)
END DO
!$OMP END PARALLEL DO

RETURN
END SUBROUTINE CopyArray

!==============================================================================

SUBROUTINE MultArray( n,a,b,c )

INTEGER,      INTENT( IN  ) :: n
REAL, DIMENSION(:), POINTER :: b, c
REAL, DIMENSION(:), POINTER :: a

INTEGER i

!$OMP PARALLEL DO PRIVATE(i) SHARED(a,b,c)
DO i = 1,n
  a(i) = b(i) * c(i)
END DO
!$OMP END PARALLEL DO

RETURN
END SUBROUTINE MultArray

!==============================================================================

SUBROUTINE ScaleArray( fac,b,n )

INTEGER,       INTENT( IN ) :: n
REAL,          INTENT( IN ) :: fac
REAL, DIMENSION(:), POINTER :: b

INTEGER i

IF( fac == 1.0 )RETURN

!$OMP PARALLEL DO PRIVATE(i) SHARED(fac,b)
DO i = 1,n
  b(i) = fac*b(i)
END DO
!$OMP END PARALLEL DO

RETURN
END SUBROUTINE ScaleArray

!==============================================================================

SUBROUTINE UpdateArray( bFac,b,cFac,c,n )

!   Update array-b by interpolating toward array-c
!   Assumes bFac + cFac = 1.0

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: n
REAL,               INTENT( IN ) :: bFac, cFac
REAL, DIMENSION(:), POINTER      :: b, c

INTEGER i

IF( bFac == 0.0 )THEN

!$OMP PARALLEL DO PRIVATE(i) SHARED(n,b,c)
  DO i = 1,n
    b(i) = c(i)
  END DO
!$OMP END PARALLEL DO

ELSE IF( cFac == 0.0 )THEN

  RETURN

ELSE

!$OMP PARALLEL DO PRIVATE(i) SHARED(n,b,c,bFac,cFac)
  DO i = 1,n
    b(i) = bFac*b(i) + cFac*c(i)
  END DO
!$OMP END PARALLEL DO

END IF

RETURN
END SUBROUTINE UpdateArray

END MODULE SWIMutilArrayPtr
