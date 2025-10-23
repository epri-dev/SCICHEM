SUBROUTINE clnpar( rat,gbar,sigg )

IMPLICIT NONE

REAL, INTENT( IN  ) :: rat
REAL, INTENT( OUT ) :: gbar, sigg

!  This Subroutine determines the Gaussian mean and sigma given
!  the sigma/mean ratio (rat) for a clipped normal distribution.
!  The parameters returned are Gaussian-mean/Clipped-mean (gbar)
!  and Gaussian-sigma/Clipped-mean (sigg). The parameters are
!  interpolated from the following table.

!    ------------------ Table of RAT, GBAR, SIGG ------------------

INTEGER, PARAMETER :: N1 = 19, N2 = 21, N10 = 17, N100 = 19

REAL, SAVE, DIMENSION(3,N1) :: X1 = RESHAPE( (/ &
    0.1000000    ,   1.000000    ,  0.9999996E-01, &
    0.1500000    ,   1.000000    ,  0.1500000    , &
    0.2000000    ,   1.000000    ,  0.2000000    , &
    0.2500000    ,  0.9999981    ,  0.2500072    , &
    0.3000000    ,  0.9999660    ,  0.3001260    , &
    0.3500000    ,  0.9997762    ,  0.3507082    , &
    0.4000000    ,  0.9991474    ,  0.4023888    , &
    0.4500000    ,  0.9976677    ,  0.4558905    , &
    0.5000001    ,  0.9948961    ,  0.5118033    , &
    0.5500001    ,  0.9904375    ,  0.5705312    , &
    0.6000001    ,  0.9835079    ,  0.6329482    , &
    0.6500001    ,  0.9738972    ,  0.6989896    , &
    0.7000001    ,  0.9609293    ,  0.7692201    , &
    0.7500001    ,  0.9440483    ,  0.8439555    , &
    0.8000001    ,  0.9230663    ,  0.9231166    , &
    0.8500001    ,  0.8968502    ,   1.007575    , &
    0.9000002    ,  0.8654099    ,   1.097018    , &
    0.9500002    ,  0.8281893    ,   1.191711    , &
     1.000000    ,  0.7846111    ,   1.291921    /) , (/ 3,N1 /) )

REAL, SAVE, DIMENSION(3,N2) :: X2 = RESHAPE( (/ &
     1.000000    ,  0.7846111    ,   1.291921    , &
     1.050000    ,  0.7341762    ,   1.397825    , &
     1.100000    ,  0.6765100    ,   1.509505    , &
     1.150000    ,  0.6111955    ,   1.627077    , &
     1.200000    ,  0.5377188    ,   1.750731    , &
     1.250000    ,  0.4556106    ,   1.880616    , &
     1.300000    ,  0.3646297    ,   2.016724    , &
     1.350000    ,  0.2642729    ,   2.159233    , &
     1.400000    ,  0.1542022    ,   2.308208    , &
     1.450000    ,  0.3041815E-01,   2.466037    , &
     1.500000    , -0.9799373E-01,   2.626817    , &
     1.550000    , -0.2381313    ,   2.794926    , &
     1.600000    , -0.3907962    ,   2.970713    , &
     1.649999    , -0.5549173    ,   3.153371    , &
     1.699999    , -0.7307782    ,   3.342945    , &
     1.749999    , -0.9190733    ,   3.539723    , &
     1.799999    ,  -1.119630    ,   3.743493    , &
     1.849999    ,  -1.332759    ,   3.954329    , &
     1.899999    ,  -1.559048    ,   4.172450    , &
     1.949999    ,  -1.797908    ,   4.397448    , &
     1.999999    ,  -2.051569    ,   4.630424    /) , (/ 3,N2 /) )

REAL, SAVE, DIMENSION(3,N10) :: X10 = RESHAPE( (/ &
     1.999999    ,  -2.051569    ,   4.630424    , &
     2.499999    ,  -5.387887    ,   7.371889    , &
     2.999999    ,  -10.33678    ,   10.89142    , &
     3.499999    ,  -17.10986    ,   15.23050    , &
     3.999999    ,  -25.86062    ,   20.41066    , &
     4.500000    ,  -36.68866    ,   26.43834    , &
     5.000000    ,  -49.73885    ,   33.34196    , &
     5.500000    ,  -65.23964    ,   41.18114    , &
     6.000000    ,  -83.07764    ,   49.88799    , &
     6.500000    ,  -102.7420    ,   59.26623    , &
     7.000000    ,  -126.4875    ,   70.10172    , &
     7.500000    ,  -150.7448    ,   81.11997    , &
     8.000000    ,  -180.4022    ,   94.00233    , &
     8.500000    ,  -210.8741    ,   107.1658    , &
     9.000000    ,  -243.3536    ,   120.9987    , &
     9.500000    ,  -282.4449    ,   137.0364    , &
     10.00000    ,  -321.5363    ,   153.0740    /) , (/ 3,N10 /) )

REAL, SAVE, DIMENSION(3,N100) :: X100 = RESHAPE( (/ &
     10.00000    ,  -321.5363    ,   153.0740    , &
     15.00000    ,  -889.7404    ,   369.6515    , &
     20.00000    ,  -1775.106    ,   682.2498    , &
     25.00000    ,  -3045.141    ,   1105.600    , &
     30.00000    ,  -4594.355    ,   1606.689    , &
     35.00000    ,  -6666.756    ,   2250.804    , &
     40.00000    ,  -8739.157    ,   2894.920    , &
     45.00000    ,  -11847.44    ,   3809.671    , &
     50.00000    ,  -15083.71    ,   4757.863    , &
     55.00000    ,  -18319.99    ,   5706.056    , &
     60.00000    ,  -21556.27    ,   6654.248    , &
     65.00000    ,  -26853.44    ,   8116.241    , &
     70.00000    ,  -32176.01    ,   9584.565    , &
     75.00000    ,  -37498.57    ,   11052.89    , &
     80.00000    ,  -42821.14    ,   12521.21    , &
     85.00000    ,  -48143.71    ,   13989.54    , &
     90.00000    ,  -53466.27    ,   15457.86    , &
     95.00000    ,  -59305.60    ,   17048.81    , &
     100.0000    ,  -68524.72    ,   19441.78    /) , (/ 3,N100 /) )

! Values of rat less than 0.1 - These values are outside of the table

IF( rat < 0.1 )THEN
  gbar = 1.0
  sigg = rat

! Values of rat between 0.1 and 1.0 - Fine resolution section - DEL = 0.05

ELSE IF( rat <= 1.0 )THEN
  CALL interp_clnpar( N1,X1,rat,gbar,sigg )

! Values of rat between 1.0 and 2.0 - Fine resolution section - DEL = 0.05

ELSE IF( rat <= 2.0 )THEN
  CALL interp_clnpar( N2,X2,rat,gbar,sigg )

! Values of rat between 2.0 and 10.0 - Medium resolution - DEL = 0.5

ELSE IF( rat <= 10.0 )THEN
  CALL interp_clnpar( N10,X10,rat,gbar,sigg )

! Values of rat between 10.0 and 100.0 - Coarse resolution - DEL = 5.0
! Values of rat greater than 100.0 - Outside Table - Extrapolate

ELSE
  CALL interp_clnpar( N100,X100,rat,gbar,sigg )

END IF

RETURN
END

!===============================================================================

SUBROUTINE interp_clnpar( nx,x,rat,gbar,sigg )

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: nx
REAL,                  INTENT( IN  ) :: rat
REAL, DIMENSION(3,nx), INTENT( IN  ) :: x
REAL,                  INTENT( OUT ) :: gbar, sigg

INTEGER i
REAL    del, beta, betm

del = x(1,2) - x(1,1)  !Assumed to be uniformly spaced

IF( rat >= x(1,nx-1) )THEN
  i = nx-1
ELSE
  i = MIN( INT((rat-x(1,1))/del)+1, nx-1 )
END IF

beta = (rat - x(1,i)) / del ; betm = 1. - beta

gbar = betm*x(2,i) + beta*x(2,i+1)
sigg = betm*x(3,i) + beta*x(3,i+1)

RETURN
END

!==============================================================================

REAL FUNCTION CNexceed( meanCN, sigCN, val )

IMPLICIT NONE

REAL, PARAMETER :: RT2R = 0.7071068             ! Reciprocal of square root of 2.0

REAL, INTENT( IN ) :: meanCN, sigCN, val

REAL arg

REAL,EXTERNAL :: erfc

arg = (val - meanCN)/sigCN
CNexceed = 0.5*erfc( arg*RT2R )

RETURN
END

