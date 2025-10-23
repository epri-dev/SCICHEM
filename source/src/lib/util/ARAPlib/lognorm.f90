SUBROUTINE logn_bin( bin,nb,MMD,sigma,weight )

IMPLICIT NONE

REAL,    DIMENSION(*), INTENT( IN  ) :: bin
INTEGER,               INTENT( IN  ) :: nb
REAL,                  INTENT( IN  ) :: MMD
REAL,                  INTENT( IN  ) :: sigma
REAL,    DIMENSION(*), INTENT( OUT ) :: weight

REAL    sig, arg, fracl, fracu
INTEGER i

REAL, EXTERNAL :: erfc

sig = SQRT(2.)*LOG(sigma)

fracl = 0.
fracu = 0.

DO i = 1,nb-1
  arg   = LOG(bin(i+1)/MMD)/sig
  fracu = 1. - 0.5*erfc(arg)
  weight(i) = fracu - fracl
  fracl = fracu
END DO

weight(nb) = 1. - fracu

RETURN
END

!===============================================================================

SUBROUTINE check_logn( binl,binu,MMD,sigma,frac )

IMPLICIT NONE

REAL, INTENT( IN  ) :: binl
REAL, INTENT( IN  ) :: binu
REAL, INTENT( IN  ) :: MMD
REAL, INTENT( IN  ) :: sigma
REAL, INTENT( OUT ) :: frac  !Mass fraction outside bins

REAL sig, argu, argl

REAL, EXTERNAL :: erfc

IF( binu <= binl )THEN
  frac = 1.
  RETURN
END IF

IF( sigma <= 1.0 )THEN

  IF( MMD >= binl .AND. MMD <= binu )THEN
    frac = 0.
  ELSE
    frac = 1.
  END IF

ELSE

  sig = SQRT(2.)*LOG(sigma)

  IF( binl > 0. )THEN
    argl = LOG(binl/MMD)/sig
    argl = erfc(argl)
  ELSE
    argl = 2.
  END IF

  argu = LOG(binu/MMD)/sig

  frac = 1. - 0.5*( argl - erfc(argu) )

END IF

RETURN
END

