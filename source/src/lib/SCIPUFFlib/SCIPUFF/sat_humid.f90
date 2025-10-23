RECURSIVE SUBROUTINE SatVapPress( tabs,p,ew )

!------ WMO formula for saturation vapor pressure over water
!       Stated temperature range is -45C to 60C
!       N.B. Use correction for moist air (vs. pure vapor) from
!       Appendix A4.2 of A. Gill (1982), Atmosphere-Ocean Dynamics, Academic Press, NY
!       which is a reasonable fit to Table 4.10 of WMO (1966)

!------ Input: tabs = absolute temp. (K)
!              p    = pressure (mb)

!------ Output: ew = staturation vapor pressure (over a plane of water)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) ::  tabs, p
REAL, INTENT( OUT ) ::  ew

REAL t, f

!------ Temperature in Celsius

t = tabs + ABSZERO
t = MIN(MAX(t,-45.),60.)  !Limit range

!------ Saturation vapor pressure

ew = 6.112 * EXP(17.62*t/(243.12+t))
f  = 1. + (4.5 + 0.0006*t*t)*p*1.E-6  !Correction for moist air
ew = f*ew

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE RelHumid( r,tabs,p,rh )

!------ Compute relative humidity based on WMO definition

!------ Input: r    = mixing ratio (g/g)
!              tabs = absolute temperature (K)
!              p    = pressure (mb)

!------ Output: rh = relative humidity in percent

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) ::  r, tabs, p
REAL, INTENT( OUT ) ::  rh

REAL ep, ew

!------ Get saturation pressure

CALL SatVapPress( tabs,p,ew )

!------ Vapor pressure

ep = r / (MR+r) * P

!------ Relative humidty

rh = 100. * ep / ew
rh = MIN(MAX(rh,0.),100.)

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE HumidMixRatio( rh,tabs,p,r )

!------ Compute humidity mixing ratio from relative humidity
!       based on WMO definition

!------ Input: rh   = relative humidity in percent
!              tabs = absolute temperature (K)
!              p    = pressure (mb)

!------ Output: r = mixing ratio (g/g)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) ::  rh, tabs, p
REAL, INTENT( OUT ) ::  r

REAL ep, ew, xv

!------ Get saturation pressure

CALL SatVapPress( tabs,p,ew )

!------ Vapor pressure & mole fraction

ep = rh/100. * ew
xv = ep / p
xv = MAX(MIN(xv,0.999),0.)

!------ Humidty mixing ratio

r = MR*xv / (1.-xv)

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE sat_humid( tabs,p,hs )

!------ From fits to Smithsonian Meteorological Tables 89 and 94 given in
!       Appendix A4.2 of A. Gill (1982), Atmosphere-Ocean Dynamics, Academic Press, NY

!------ Input: tabs = absolute temp. (K)
!              p    = pressure (mb)

!------ Output: hs = staturation mixing ratio (g/g)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) ::  tabs
REAL, INTENT( IN  ) ::  p
REAL, INTENT( OUT ) ::  hs

REAL, PARAMETER :: EMAX = 1./(1.+MR)

REAL t, e, f

!------ Temperature in Celsius

t = tabs + ABSZERO
t = MIN(MAX(t,-100.),100.)  !Limit range to [-100C,100C]

!------ Saturation vapor pressure

e = (0.7859 + 0.03477*t) / (1. + 0.00412*t)
IF( t <= 0. )THEN
  e = e + 0.00422*t
END IF
f = 1. + (4.5 + 0.0006*t*t)*p*1.E-6
e = f*10.**e

!------ Saturation mixing ratio (from equ. A4.3)

e  = MIN( e/p,EMAX )  !Limit vapor pressure ratio so max(hs) = 1. (Arbitrary)
hs = MR*e/(1.-e)

RETURN
END
