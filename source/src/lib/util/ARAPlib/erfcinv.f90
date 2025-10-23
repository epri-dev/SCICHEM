MODULE erfcinv_fi

  SAVE

  INTEGER         , PARAMETER :: MAX  = 1001
  DOUBLE PRECISION, PARAMETER :: DLIM = 4.8D0

  LOGICAL erfcinv_init
  DOUBLE PRECISION, DIMENSION(MAX) ::  xx,yy

END MODULE erfcinv_fi

!==============================================================================

REAL FUNCTION erfcinv( arg )

USE erfcinv_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: arg

REAL(8)  y,dconv

IF( .NOT. erfcinv_init )CALL init_erfcinv()

y = DBLE(arg)

IF( y < 0.0D0 .OR. y > 2.0D0 )THEN

  erfcinv = 1.E+36

ELSE IF( y >= yy(1) )THEN

  erfcinv = -SNGL(DLIM)

ELSE IF( y <= yy(MAX) )THEN

  erfcinv = SNGL(DLIM)

ELSE

  CALL dlocate( yy,MAX,y,xx,dconv )

  erfcinv = SNGL(dconv)

END IF

RETURN
END

!==============================================================================

SUBROUTINE init_erfcinv()

USE erfcinv_fi

IMPLICIT NONE

REAL(8)  dxx
INTEGER  i

REAL(8), EXTERNAL :: derfc

erfcinv_init = .TRUE.
dxx = 2.D0*DLIM/FLOAT(MAX-1)

DO i = 1,MAX
  xx(i) = -DLIM + dxx*FLOAT(i-1)
  yy(i) = derfc( xx(i) )
END DO


RETURN
END

!==============================================================================

SUBROUTINE dlocate( x,n,xx,y,yy )

!--- Finds j such that x(j) < xx < x(j+1). x is monotonic. j = 0 or j =n+1
!--- if out of range

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: n
REAL(8),               INTENT( IN  ) :: xx
REAL(8), DIMENSION(n), INTENT( IN  ) :: x,y
REAL(8),               INTENT( OUT ) :: yy

INTEGER j,jl,jm,ju

IF( xx >= x(1) )THEN

  yy = y(1)

!=== xx greater than last point - hold constant

ELSE IF( xx <= x(n) )THEN

  yy = y(n)

ELSE

  jl = 0                            ! lower limit
  ju = n + 1                        ! upper limit

  DO WHILE( ju-jl > 1 )
    jm = (ju+jl)/2                 ! midpoint
    IF( xx < x(jm) )THEN
      jl = jm                      ! replace lower limit
    ELSE
      ju = jm                      ! replace upper limit
    END IF
  END DO

  j = jl+1

!=== Interpolate

  yy = y(jl) + (y(j)-y(jl))*((xx-x(jl))/(x(j)-x(jl)))

END IF

RETURN
END
