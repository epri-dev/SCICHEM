REAL FUNCTION InterpVal( n,x,xx,y )

! Locate the value of yy at xx from a monotonic {x(j+1)>x(j)},  x vs y table

IMPLICIT NONE

!=== Inputs and Outputs =======================================================

INTEGER            ,INTENT( IN ) :: n    !Number of points in array
REAL               ,INTENT( IN ) :: xx   !Value to locate
REAL, DIMENSION(n) ,INTENT( IN ) :: x,y  !Table values

!=== Locals ===================================================================

INTEGER   :: j,j1,jm,ju
REAL      :: yy

!==============================================================================
!=== Initialize return

yy = 0.0

!=== xx less than first point - extrapolate

IF( xx <= x(1) )THEN

  yy = (xx/x(1))*y(1)

!=== xx greater than last point - hold constant

ELSE IF( xx >= x(n) )THEN

  yy = y(n)

ELSE

!=== xx between first and last points - interpolate
!    Locate interpolation points - bisection method

  j1 = 0                            ! lower limit
  ju = n+1                          ! upper limit
  DO WHILE( (ju-j1) > 1 )
    jm = (ju+j1)/2                  ! midpoint
    IF( xx > x(jm) )THEN
       j1 = jm                      ! replace lower limit
    ELSE
       ju = jm                      ! replace upper limit
    END IF
  END DO
  j = j1+1

!=== Interpolate

  yy = y(j1) + (y(j)-y(j1))*((xx-x(j1))/(x(j)-x(j1)))

END IF

!=== Set return value and exit

InterpVal = yy

RETURN
END
