SUBROUTINE xmatmul( a,b,c )

!------ Matrix multiplication
!       N.B. first index is ROW; second is COLUMN

IMPLICIT NONE

REAL, DIMENSION(3,3), INTENT( IN  ) :: a, b
REAL, DIMENSION(3,3), INTENT( OUT ) :: c

REAL(8) sum

INTEGER i, j, k

DO j = 1,3
  DO i = 1,3
    sum = 0.D0
    DO k = 1,3
      sum = sum + DBLE(a(i,k))*DBLE(b(k,j))
    END DO
    c(i,j) = SNGL(sum)
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE trnsps( a,b )

!------ Matrix transpose

IMPLICIT NONE

REAL, DIMENSION(3,3), INTENT( IN  ) :: a
REAL, DIMENSION(3,3), INTENT( OUT ) :: b

INTEGER i, j

DO j = 1,3
  DO i = 1,3
    b(i,j) = a(j,i)
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE matvmul( a,x,y )

!------ Matrix-vector multiplication intended for rotating coordinate system
!       N.B. first index of A is ROW; second is COLUMN (x,y are column vectors)

IMPLICIT NONE

REAL, DIMENSION(3,3), INTENT( IN  ) :: a
REAL, DIMENSION(3),   INTENT( IN  ) :: x
REAL, DIMENSION(3),   INTENT( OUT ) :: y

INTEGER i, j
REAL(8)    sum

DO i = 1,3
  sum = 0.D0
  DO j = 1,3
    sum = sum + DBLE(a(i,j))*DBLE(x(j))
  END DO
  y(i) = SNGL(sum)
END DO

RETURN
END

!==============================================================================

SUBROUTINE xmatmul2( a,b,c )

!------ Matrix multiplication - 2x2 version
!       N.B. first index is ROW; second is COLUMN

IMPLICIT NONE

REAL, DIMENSION(2,2), INTENT( IN  ) :: a, b
REAL, DIMENSION(2,2), INTENT( OUT ) :: c

REAL(8) sum

INTEGER i, j, k

DO j = 1,2
  DO i = 1,2
    sum = 0.
    DO k = 1,2
      sum = sum + DBLE(a(i,k))*DBLE(b(k,j))
    END DO
    c(i,j) = SNGL(sum)
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE trnsps2( a,b )

!------ Matrix transpose - 2x2 version

IMPLICIT NONE

REAL, DIMENSION(2,2), INTENT( IN  ) :: a
REAL, DIMENSION(2,2), INTENT( OUT ) :: b

INTEGER i, j

DO j = 1,2
  DO i = 1,2
    b(i,j) = a(j,i)
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE matvmul2( a,x,y )

!------ Matrix-vector multiplication intended for rotating coordinate system - 2x2 version
!       N.B. first index of A is ROW; second is COLUMN (x,y are column vectors)

IMPLICIT NONE

REAL, DIMENSION(2,2), INTENT( IN  ) :: a
REAL, DIMENSION(2),   INTENT( IN  ) :: x
REAL, DIMENSION(2),   INTENT( OUT ) :: y

INTEGER i, j
REAL(8)    sum

DO i = 1,2
  sum = 0.
  DO j = 1,2
    sum = sum + DBLE(a(i,j))*DBLE(x(j))
  END DO
  y(i) = SNGL(sum)
END DO

RETURN
END
