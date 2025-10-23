REAL FUNCTION erfc(arg)

IMPLICIT NONE

REAL, INTENT( IN ) :: arg

REAL, PARAMETER :: p  =  0.3275911
REAL, PARAMETER :: a1 =  0.254829592
REAL, PARAMETER :: a2 = -0.284496736
REAL, PARAMETER :: a3 =  1.421413741
REAL, PARAMETER :: a4 = -1.453152027
REAL, PARAMETER :: a5 =  1.061405429

REAL t

t = 1./(1.+p*ABS(arg))

erfc = t*(a1+t*(a2+t*(a3+t*(a4+t*a5))))*EXP(-arg**2)

IF( arg < 0.0 )erfc = 2. - erfc

RETURN
END

!==============================================================================

REAL(8) FUNCTION derfc(arg)

IMPLICIT NONE

REAL(8), INTENT( IN ) :: arg

REAL(8), PARAMETER :: p  =  0.3275911D0
REAL(8), PARAMETER :: a1 =  0.254829592D0
REAL(8), PARAMETER :: a2 = -0.284496736D0
REAL(8), PARAMETER :: a3 =  1.421413741D0
REAL(8), PARAMETER :: a4 = -1.453152027D0
REAL(8), PARAMETER :: a5 =  1.061405429D0

REAL(8) t

t = 1.D0/(1.D0+p*ABS(arg))

derfc = t*(a1+t*(a2+t*(a3+t*(a4+t*a5))))*DEXP(-arg**2)

IF( arg < 0.0D0 )derfc = 2.D0 - derfc

RETURN
END
