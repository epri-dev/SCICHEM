REAL FUNCTION ufall( rhoa,rhop,rmuair,d )

! ----- Calculates particle fall speed in m/s

!       Uses Cd = 24/Re * (1 + 0.158 * Re**2/3 ) for Re<1000
!            Cd = 0.403                          for Re>1000
!       where   Re = U * d / nu

!       Also includes low density correction factor
!            1 + Kn * ( 1 + 0.522*EXP(-0.656/Kn) )
!       which reduces Cd for Kn>0.01

USE constants_fd
IMPLICIT NONE

REAL, INTENT( IN ):: rhoa       ! Air density (kg/m3)
REAL, INTENT( IN ):: rhop       ! Particle density (kg/m3)
REAL, INTENT( IN ):: rmuair     ! Air viscosity (kg/m/s)
REAL, INTENT( IN ):: d          ! Particle diameter (m)

REAL, PARAMETER :: SIGMOL = 3.72E-10  !Air molecule diam (m)
REAL, PARAMETER :: AVAGAD = 6.021E26  !Avagadro's # - molecules/kg-mole
REAL, PARAMETER :: LAMFAC = MWAIR/(PI*SQRT2*AVAGAD*SIGMOL*SIGMOL)

REAL r, alp, u, re, dnu, fac, dua, fdash
REAL fval, un, vnu, lambda, knud

!------ Check for zero density or diameter

IF( rhop == 0.0 .OR. d == 0.0 )THEN
  ufall = 0.
  GOTO 9999
END IF

!------ Set air dynamic viscosity and particle radius

vnu = rmuair/rhoa
r   = 0.5*d

!------ Check for high altitude correction

lambda = LAMFAC/rhoa
knud   = MIN(lambda/d,0.5)
IF( knud > 1.E-2 )THEN
  fac = 1.0 + knud*(1.644+0.522*EXP(-0.656/knud))
ELSE
  fac = 1.0
END IF

alp = SQRT(fac*2.666667*rhop*r*G0/rhoa)

!------ High-Re limit - u=alp/sqrt(cd) where cd=0.403

u  = alp*1.5752427
re = u*d/vnu

IF( re >= 1000. )THEN
  ufall = u
  GOTO 9999
END IF

dnu  = d/vnu
fac  = alp*alp*dnu/24.

IterationLoop: DO

  dua   = 0.158*(u*dnu)**TwoThirds
  fdash = 1.0 + 1.6666667*dua
  fval  = u*(1.0+dua) - fac
  un    = u - fval/fdash

  IF( ABS(un-u) <= 1.E-4*u )THEN
    ufall = un
    EXIT
  END IF

  u = un

END DO IterationLoop

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION SetParticleDiff( dm,rnu )

IMPLICIT NONE

REAL, INTENT( IN ) :: dm
REAL, INTENT( IN ) :: rnu

LOGICAL off_end, less_than
INTEGER j
REAL    rate, dbpx

!--- Set particle diffusivivity in air - from Smoke, Dust and Haze
!    Fundamentals of Aerosol Behavior by S. K. Friedlander (Wiley, 1977)

REAL, PARAMETER, DIMENSION(13) :: RP = &       ! Particle diameter (m)
     (/5.0E-10,1.0E-9,2.5E-9,5.0E-9,1.0E-8,2.5E-8,5.0E-8,2.5E-7,5.0E-7, &
                                           2.5E-6,5.0E-6,2.5E-5,5.0E-5 /)

REAL, PARAMETER, DIMENSION(13) :: DP = &        ! Particle diffusivity (m2/s)
     (/5.1E-6,1.3E-6,2.1E-7,5.2E-8,1.3E-8,2.4E-9,6.7E-10,6.3E-11,2.8E-11, &
                                          4.9E-12,2.4E-12,1.0E-20,1.0E-20 /)

off_end   = .FALSE.
less_than = .TRUE.

j = 2

DO WHILE( less_than )
  IF( 2.0*RP(j) >= dm )THEN
    less_than = .FALSE.
  ELSE
    j = j + 1
    IF( j > 13 )THEN
      off_end   = .TRUE.
      less_than = .FALSE.
    END IF
  END IF
END DO

IF( off_end )THEN
  SetParticleDiff = 1.E-20
ELSE
  rate = (0.5*dm-RP(j-1))/(RP(j)-RP(j-1))
  dbpx = LOG(DP(j-1)) + rate*(LOG(DP(j))-LOG(DP(j-1)))
  SetParticleDiff = MIN(rnu,EXP(dbpx))
END IF

RETURN
END
