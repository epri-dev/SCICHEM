SUBROUTINE vdep_dry( ustar,h_cnp,zruf,drat,dm,dbpx,vdry )

USE scipuff_fi

IMPLICIT NONE

!------ Slinn (1982) parameters for Chamberlain's (1966,67) grass data

REAL, PARAMETER :: AHAT   = 500.      !microns
REAL, PARAMETER :: ACARAT = 10.       !microns
REAL, PARAMETER :: FCARAT = .01
REAL, PARAMETER :: CVCD   = 0.333333
REAL, PARAMETER :: EFAC   = 0.16      !for fit to Slinn model

REAL, INTENT( IN  ) :: ustar !Friction velocity (m/s)
REAL, INTENT( IN  ) :: h_cnp !Canopy height (m)
REAL, INTENT( IN  ) :: zruf  !Roughness height (m)
REAL, INTENT( IN  ) :: drat  !Gravitational settling velocity (m/s)
REAL, INTENT( IN  ) :: dm    !Mean particle diameter (m)
REAL, INTENT( IN  ) :: dbpx  !Particle diffusivity in air (m/s/s)
REAL, INTENT( OUT ) :: vdry  !Deposition velocity (m/s)

REAL rpx, zref, zdispl, eb, tvpx, st, fac, aim, eim, ein
REAL eff, phi_r, udep, z0

!------ no deposition if ustar=0

vdry = 0.

IF( ustar == 0.0 )GOTO 9999

!-----  No canopy - use Lewellen & Sheng flat plate model

IF( h_cnp <= 0.0 )THEN

  zref   = 10.
  zdispl = 0.
  z0     = zruf

  eb   = 0.8*(dbpx/rnu)**0.7
  tvpx = drat/g
  st   = ustar*ustar*tvpx/rnu
  fac  = (1.+3.66*ustar*tvpx/zref)**2
  aim  = 0.08*st*(1.-EXP(-0.424*st))/fac
  eim  = aim/(1.+aim)
  eff  = 1. - (1.-eb)*(1.-eim)

ELSE

!------ Canopy - use parameterized Slinn model

  rpx    = 0.5*dm*1.E6
  zref   = 2.0*h_cnp
  zdispl = 0.5*h_cnp
  z0     = 0.1*h_cnp

  tvpx = drat/g
  eb   = CVCD*(dbpx/rnu)**0.7
  st   = tvpx*ustar/(AHAT*1.E-6)
  ein  = CVCD*(FCARAT*rpx/(rpx+ACARAT) &
       + (1.0-FCARAT)*rpx/(rpx+AHAT))
  eim  = st*st/(1.+st*st)
  eff  = 1. - (1.-eim)*(1.-ein)*(1.-eb)
  eff  = eff / (EFAC + (1.-EFAC)*eff)

END IF

phi_r = LOG(1.0 + (zref-zdispl)/z0)
udep  = ustar*VONK/phi_r ! = ustar**2/u_ref

vdry = udep*eff

9999 CONTINUE

RETURN
END
