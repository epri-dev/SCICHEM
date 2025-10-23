RECURSIVE SUBROUTINE SetSimpleBL( t,zi,hflux )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: t         !Local time of day in hours
REAL, INTENT( OUT ) :: zi, hflux

REAL, EXTERNAL :: SimpleZI, SimpleHFLX

zi    = SimpleZI( Prj%BL%ZImin,Prj%BL%ZImax,t )
hflux = SimpleHFLX( Prj%BL%HFLXmin,Prj%BL%HFLXmax,t )

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION SimpleZI( zimin,zimax,t )

IMPLICIT NONE

REAL, INTENT( IN ) :: zimin, zimax, t

REAL frac

frac = t / 24.

IF( frac < 0.25 .OR. frac > 0.75 )THEN
  SimpleZI = zimin
ELSE IF( frac < 0.50 )THEN
  SimpleZI = zimin + (zimax-zimin)*4.*(frac-0.25)
ELSE
  SimpleZI = zimax
END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION SimpleHFLX( HFLXmin,HFLXmax,t )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: HFLXmin, HFLXmax, t

REAL frac

frac = t / 24.

IF( frac < 0.25 .OR. frac > 0.75 )THEN
  SimpleHFLX = HFLXmin
ELSE
  frac = frac - 0.25
  SimpleHFLX = HFLXmin + (HFLXmax-HFLXmin)*SIN(frac*PI2)
END IF

RETURN
END

