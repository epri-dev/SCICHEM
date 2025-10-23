!==============================================================================
!
!  SCIPUFF turbulence profile routines
!
!==============================================================================
SUBROUTINE BLturb( zarg,zi,ustar2,wstar2,hflux,uu,vv,ww,qq,wwz,sy,sz, &
                               amet,bmet,hcnp,alpc,zruf2d )
USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: zarg,zi
REAL, INTENT( IN  ) :: ustar2,wstar2
REAL, INTENT( OUT ) :: uu,vv,ww,qq,sy,sz
REAL, INTENT( OUT ) :: wwz
REAL, INTENT( OUT ) :: amet,bmet
REAL, INTENT( IN  ) :: hflux,hcnp,alpc,zruf2d

REAL, PARAMETER :: FacSigU = 0.5

REAL z, wt, rat, soq, uux, zr, fac, uu_min

REAL, EXTERNAL :: f22, f33, g22, g33
REAL, EXTERNAL :: f33z, g33z

!------ Set profiles as a function z / zi

z = zarg*zi

vv = wstar2*g22( zarg )
wt = MAX(hflux*(1.0-zarg),0.)

uu_min = Prj%BL%UUcalm

!------ Scale turbulence inside canopy

IF( z < hcnp )THEN
  fac  = EXP(2.*FacSigU*alpc*(z/hcnp-1.))
  uu   = fac*MAX(ustar2*f22( 0. ) , Prj%BL%WWtrop)
  ww   = fac*MAX(ustar2*f33( 0. ) + wstar2*g33( hcnp/zi ),Prj%BL%WWtrop)
  wwz  = fac*(ustar2*f33z( 0. ) + wstar2*g33z( hcnp/zi )) / zi
  vv   = fac*vv
  wt   = fac*wt
  zr   = MAX(0.01,z/hcnp)*zruf2d
  uu_min = fac*uu_min
ELSE
  uu  = MAX(ustar2*f22( zarg-hcnp/zi ) , Prj%BL%WWtrop)
  ww  = MAX(ustar2*f33( zarg-hcnp/zi ) + wstar2*g33( zarg ),Prj%BL%WWtrop)
  IF( zarg > 0.0 )THEN
    wwz = ( ustar2*f33z( zarg-hcnp/zi ) + wstar2*g33z( zarg ) ) / zi
  ELSE
    wwz = ( ustar2*f33z( zarg-hcnp/zi ) + wstar2*g33z( 0.001 ) ) / zi
  END IF
  zr  = zruf2d
END IF

sy = 0.3*zi
sz = 0.65*MIN(z+zr,MAX(zi-z,Prj%BL%SLtrop/0.65))
sz = sz*sy/SQRT(sz*sz+sy*sy)

uux = uu_min - uu
IF( vv < uux )THEN
  rat = vv/uux
  sy  = 1./(rat**1.5/sy + (1.-rat)**1.5/Prj%BL%SLcalm)
  vv  = uux
END IF

qq  = 2.*(uu + vv*(sz/sy)**TwoThirds) + ww

soq  = sz/(SQRT(qq)+1.E-6)
bmet = EQF*soq**2
amet = soq/A*ww                                        !Limit buoyancy enhancement
IF( wt > 0.0 )amet = amet + MIN(bmet*wt,2.0*B*amet/BS) !assuming buoyancy production
                                                       !cannot exceed dissipation
bmet = 2.*bmet !Equilibrium heat flux

RETURN
END

!==============================================================================
! Velocity variance profiles
!==============================================================================

REAL FUNCTION f22( z ) ! shear-driven vv (transverse)

IMPLICIT NONE

REAL, INTENT( IN ) :: z

f22 = 2.5*(1.0-z)

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION f33( z ) ! shear-driven ww (vertical)

IMPLICIT NONE

REAL, INTENT( IN ) :: z

f33 = 1.5*(1.0-z)

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION g22( z ) ! buoyancy-driven vv (horizontal)

IMPLICIT NONE

REAL, INTENT( IN ) :: z

g22 = 0.13*(1.0+1.5*EXP(-z))

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION g33( z ) ! buoyancy-driven ww (vertical)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: z

g33 = 1.1*(1.05-z)*z**TwoThirds

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION f33z( z ) ! shear-driven ww (vertical)

IMPLICIT NONE

REAL, INTENT( IN ) :: z

f33z = -1.5

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION g33z( z ) ! buoyancy-driven ww (vertical)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: z

g33z = 1.1 * (z**TwoThirds) * (TwoThirds*(1.05-z)/z - 1.0)

RETURN
END


