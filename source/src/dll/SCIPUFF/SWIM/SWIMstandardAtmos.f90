
!------ US Standard Atmosphere routines

MODULE StndAtmos_fi

  SAVE

  INTEGER :: nz
  REAL    :: dz

  REAL, DIMENSION(:), ALLOCATABLE :: tStnd
  REAL, DIMENSION(:), ALLOCATABLE :: tzStnd
  REAL, DIMENSION(:), ALLOCATABLE :: prStnd
  REAL, DIMENSION(:), ALLOCATABLE :: prLogStnd
  REAL, DIMENSION(:), ALLOCATABLE :: facStnd

  REAL, PARAMETER :: ZMAX_ATMOS = 500.E3

END MODULE StndAtmos_fi

!==============================================================================

INTEGER FUNCTION InitStndAtmos( zmax,nzo )

!----- Generate interpolation arrays up to zmax
!      (Analytical expressions used above zmax)


USE SWIM_fi
USE SWIMparam_fd
USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL,    INTENT( INOUT ) :: zmax
INTEGER, INTENT( OUT   ) :: nzo

INTEGER alloc_stat, k
REAL    z, t, tz, pr

zmax = MIN(zmax,85.E3)       !Standard atmosphere defined up to 85 km
nz   = INT(zmax/100.) + 1    !100m vertical grid spacing
nzo  = nz

!------ Allocate vertical grid arrays

ALLOCATE( tStnd(nz),tzStnd(nz),prStnd(nz),prLogStnd(nz),facStnd(nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  InitStndAtmos = SWIMfailure
  error%Number  = UK_ERROR
  error%Routine = 'InitStndAtmos'
  error%Message = 'Error allocating arrays'
  GOTO 9999
END IF

dz = zmax / FLOAT(nz-1)

!------ Loop over grid

DO k = 1,nz

  z = FLOAT(k-1)*dz

  CALL StandardAtmosphere( z,t,tz,pr )

  facStnd(k)   = (1./pr)**KAPPA
  tStnd(k)     = t
  tzStnd(k)    = facStnd(k)*(tz + GAMMA0)
  prStnd(k)    = pr
  prLogStnd(k) = LOG(pr)

END DO

InitStndAtmos = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE stnd_atmos( ht,pr,t,tz,iflag )

!------ Numerical approximation to 1976 U.S. Standard Atmosphere
!       Accurate to a height of 85 km.

!------ Input : ht = height in meters (MSL)
!       Output: pr = p/p0;  t = pot. temp (iflag=0) or temp (iflag=1);
!               tz = d(tpot)/dz  (gradient is always for potential temperature)
!       N.B. Use parameter SA_TPOT = 0 to check iflag

USE StndAtmos_fi
USE constants_fd
USE SWIMparam_fd

IMPLICIT NONE

REAL,    INTENT( IN  )  :: ht
REAL,    INTENT( OUT )  :: pr, t, tz
INTEGER, INTENT( IN  )  :: iflag

INTEGER k
REAL    z, rate, ratm

!------ Make sure height is above sea level and below top of atmosphere

z = MAX(ht,0.)
z = MIN(z,ZMAX_ATMOS)

!------ Get index

IF( z >= FLOAT(nz-1)*dz )THEN        !Use semi-analytical functions above top interpolation level

  CALL StandardAtmosphere( z,t,tz,pr )

  rate = (1./pr)**KAPPA
  tz   = rate*(tz + GAMMA0)

  IF( iflag == SA_TPOT )t = t * rate

ELSE                                 !Interpolate below zmax

  k    = MIN(INT(z/dz)+1,nz-1)
  rate = z/dz - FLOAT(k-1)
  ratm = 1. - rate

  t  = rate*tStnd(k+1)  + ratm*tStnd(k)
  tz = rate*tzStnd(k+1) + ratm*tzStnd(k)
  pr = rate*prStnd(k+1) + ratm*prStnd(k)

  IF( iflag == SA_TPOT )t = (rate*facStnd(k+1) + ratm*facStnd(k)) * t

END IF

tz = MAX(tz,GAMMA_EPA)

RETURN
END

!==============================================================================

REAL FUNCTION StndTempFunc( ht,iflag ) RESULT( t )

!------ Numerical approximation to 1976 U.S. Standard Atmosphere

!------ Input : ht = height in meters (MSL)
!       Output: t  = temp (actual or potential, depending on iflag)

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL,    INTENT( IN ) :: ht
INTEGER, INTENT( IN ) :: iflag      !0 -> t=pot. temp; 1 -> t=actual temp

INTEGER k
REAL    z, rate, ratm, pr, tz

!------ Make sure height is above sea level and below top of atmosphere

z = MAX(ht,0.)
z = MIN(z,ZMAX_ATMOS)

!------ Get index

IF( z >= FLOAT(nz-1)*dz )THEN        !Use semi-analytical functions above top interpolation level

  CALL StandardAtmosphere( z,t,tz,pr )
  IF( iflag == 0 )t = t * (1./pr)**KAPPA


ELSE                                 !Interpolate below zmax

  k    = MIN(INT(z/dz)+1,nz-1)
  rate = z/dz - FLOAT(k-1)
  ratm = 1. - rate

  IF( iflag == 0 )THEN
    t = rate*tStnd(k+1)  + ratm*tStnd(k)
    t = (rate*facStnd(k+1) + ratm*facStnd(k)) * t
  ELSE
    t = rate*tStnd(k+1)  + ratm*tStnd(k)
  END IF

END IF

RETURN
END

!==============================================================================

REAL FUNCTION StndTemp( ht ) RESULT( t )

!DEC$ ATTRIBUTES DLLEXPORT :: StndTemp

!------ Numerical approximation to 1976 U.S. Standard Atmosphere

!------ Input : ht = height in meters (MSL)
!       Output: t  = actual temp

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: ht

REAL, EXTERNAL :: StndTempFunc

t = StndTempFunc( ht,1 )

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION StndTpot( ht ) RESULT( t )

!------ Numerical approximation to 1976 U.S. Standard Atmosphere

!------ Input : ht = height in meters (MSL)
!       Output: t = pot. temp

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: ht

REAL, EXTERNAL :: StndTempFunc

t = StndTempFunc( ht,0 )

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION StndLogP( ht ) RESULT( pr )

!DEC$ ATTRIBUTES DLLEXPORT :: StndLogP

!------ Numerical approximation to 1976 U.S. Standard Atmosphere

!------ Input : ht = height in meters (MSL)
!       Output: pr = log(p)

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: ht

INTEGER k
REAL    z, rate, ratm, t, tz

!------ Make sure height is above sea level and below top of atmosphere

z = MAX(ht,0.)
z = MIN(z,ZMAX_ATMOS)

!------ Get index

IF( z >= FLOAT(nz-1)*dz )THEN        !Use semi-analytical functions above top interpolation level

  CALL StandardAtmosphere( z,t,tz,pr )
  pr = LOG(pr)

ELSE                                 !Interpolate below zmax

  k    = MIN(INT(z/dz)+1,nz-1)
  rate = z/dz - FLOAT(k-1)
  ratm = 1. - rate

  pr = rate*prLogStnd(k+1) + ratm*prLogStnd(k)

END IF

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION StndHumid( p,t ) RESULT( h )

!------ Humidity profile based on linear relative humidity profile of Manabe & Wetherald (1967)

!------ Input : p = pressure (mb)
!               t = absolute temperature (K)
!       Output: h = humidity mixing ratio

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: p, t

REAL rh

IF( p > 10.0 )THEN

  rh = RHSRF + RHSLOPE*MIN(p-1000,0.) !Percent
  CALL HumidMixRatio( rh,t,p,h )
  h = MAX(h,HSTRAT)

ELSE

  h = HSTRAT*p/10.  !Make h go to zero at very hight altitudes

END IF

RETURN
END

!==============================================================================

RECURSIVE REAL FUNCTION StndRelativeHumid( p ) RESULT( rh )

!------ Relative humidity profile based on linear profile of Manabe & Wetherald (1967)

!------ Input : p = pressure (mb)

!       Output: rh = relative humidity in percent

USE StndAtmos_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: p

IF( p > 10.0 )THEN
  rh = RHSRF + RHSLOPE*MIN(p-1000,0.)
ELSE
  rh = RHSTRAT
END IF

RETURN
END

!==============================================================================

SUBROUTINE StandardAtmosphere( z,t,tz,pr )

!------ 1976 US Standard Atmosphere
!       Defined up to 85 km

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: z           !Geometric height (m)
REAL, INTENT( OUT ) :: t, tz, pr   !Temperature (K), gradient (K/m), pressure ratio

REAL,   PARAMETER :: GMR = 34.163195  !Hydrostatic constant (g/Rgas)
INTEGER,PARAMETER :: NZT = 8          !Number of entries in the defining tables

REAL,DIMENSION(NZT),PARAMETER:: HTAB= &
                        (/0.0, 11.0, 20.0, 32.0, 47.0, 51.0, 71.0, 84.852/)
REAL,DIMENSION(NZT),PARAMETER:: TTAB= &
        (/288.15, 216.65, 216.65, 228.65, 270.65, 270.65, 214.65, 186.946/)
REAL,DIMENSION(NZT),PARAMETER:: PTAB= &
             (/1.0, 2.233611E-1, 5.403295E-2, 8.5666784E-3, 1.0945601E-3, &
                                   6.6063531E-4, 3.9046834E-5, 3.68501E-6/)
REAL,DIMENSION(NZT),PARAMETER:: GTAB= &
                              (/-6.5, 0.0, 1.0, 2.8, 0.0, -2.8, -2.0, 0.0/)

INTEGER i
REAL    h, t0, dz, fac

!------ Convert to geopotential height (km)

fac = REARTH / (z+REARTH)
h   = z*fac * 1.E-3

!------ Find section based on geopotential height

IF( h < HTAB(NZT) )THEN
  i = 1
  DO WHILE( i < NZT )
    IF( h <= HTAB(i+1) )EXIT
    i = i + 1
  END DO
ELSE
  i = NZT
END IF

!------ Define temperature in sections of constant gradient

tz = GTAB(i)
t0 = TTAB(i)
dz = h - HTAB(i)
t  = t0 + tz*dz

IF( tz == 0.0 )THEN                                     !Pressure ratio
  pr = PTAB(i)*EXP(-GMR*dz/t0)
ELSE
  pr = PTAB(i)*(t0/t)**(GMR/tz)
END IF

!------ Convert gradient to geometric height (per meter)

tz = tz*fac**2 * 1.E-3

RETURN
END

!==============================================================================

SUBROUTINE StandardAtmosphere1966( z,t,tz,pr )

!------ 1966 Version (original implementation in SCIPUFF)

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: z
REAL, INTENT( OUT ) :: t, tz, pr

REAL, PARAMETER :: PFAC = -G0 / RGAS

!------ Define temperature in sections of constant gradient

IF( z <= 11000. )THEN
  tz = -6.4891E-3
  t  = 288.15 + z*tz
  pr = (t/288.15)**(PFAC/tz)

ELSE IF( z <= 11500. )THEN
  tz = -2.400E-4
  t  = 216.77 + (z-11000.)*tz
  pr = 0.22332*(t/216.77)**(PFAC/tz)

ELSE IF( z <= 20000. )THEN
  tz = 0.
  t  = 216.65
  pr = 0.20639*EXP(PFAC*(z-11500.)/t)

ELSE IF( z <= 20500. )THEN
  tz = 8.600E-4
  t  = 216.65 + (z-20000.)*tz
  pr = 5.3995E-2*(t/216.65)**(PFAC/tz)

ELSE IF( z <= 32000. )THEN
  tz = 1.0061E-3
  t  = 217.08 + (z-20500.)*tz
  pr = 4.9904E-2*(t/217.087)**(PFAC/tz)

ELSE IF( z <= 47000. )THEN
  tz = 2.8000E-3
  t  = 228.48 + (z-32000.)*tz
  pr = 8.5614E-3*(t/228.48)**(PFAC/tz)

ELSE IF( z <= 52000. )THEN
  tz = 0.
  t  = 270.65
  pr = 1.0915E-3*EXP(PFAC*(z-47000.)/t)

ELSE IF( z <= 61000. )THEN
  tz = -2.000E-3
  t  = 270.65 + (z-52000.)*tz
  pr = 5.8051E-4*(t/270.65)**(PFAC/tz)

ELSE IF( z <= 69000. )THEN
  tz = -4.000E-3
  t  = 252.65 + (z-61000.)*tz
  pr = 1.7909E-4*(t/252.65)**(PFAC/tz)

ELSE IF( z <= 79000. )THEN
  tz = -3.000E-3
  t  = 220.65 + (z-69000.)*tz
  pr = 5.6304E-5*(t/220.65)**(PFAC/tz)

ELSE
  tz = 0.
  t  = 190.65
  pr = 1.0654E-5*EXP(PFAC*(z-79000.)/t)

END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION ClearStndAtmos()

USE SWIMparam_fd
USE StndAtmos_fi

IMPLICIT NONE

INTEGER alloc_stat

IF( ALLOCATED(tStnd)     )DEALLOCATE( tStnd,STAT=alloc_stat )
IF( ALLOCATED(tzStnd)    )DEALLOCATE( tzStnd,STAT=alloc_stat )
IF( ALLOCATED(prStnd)    )DEALLOCATE( prStnd,STAT=alloc_stat )
IF( ALLOCATED(prLogStnd) )DEALLOCATE( prLogStnd,STAT=alloc_stat )
IF( ALLOCATED(facStnd)   )DEALLOCATE( facStnd,STAT=alloc_stat )

ClearStndAtmos = SWIMsuccess

RETURN
END


