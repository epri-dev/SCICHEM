SUBROUTINE init_wash()

USE scipuff_fi
USE chem_fi

IMPLICIT NONE

REAL, PARAMETER :: A_RAIN = 7.317E-04
REAL, PARAMETER :: B_RAIN = 0.21
REAL, PARAMETER :: A_SNOW = 5.215E-04
REAL, PARAMETER :: B_SNOW = 0.25
REAL, PARAMETER :: RHOW   = 1.00E+03
REAL, PARAMETER :: V_SNOW = 1.1

TYPE( puff_material )pmatl

INTEGER i, j, icls, alloc_stat

REAL,    EXTERNAL :: ufall
LOGICAL, EXTERNAL :: IsGas,IsParticle
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle

!----- Allocate arrays

ALLOCATE( twash(0:NWASH,ntypp),vwash(0:NWASH),dwash(0:NWASH),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'init_wash'
  eMessage = 'Error allocating washout arrays'
  GOTO 9999
END IF

ALLOCATE( pratebl(0:NRAIN),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'init_wash'
  eMessage = 'Error allocating pratebl array'
  GOTO 9999
END IF
pratebl(0) = 0.
DO i = 1,NRAIN
  pratebl(i) = PWASH(i)
END DO

!-----  Set Precipitation fall velocity (m/s) and mean drop diam (m)

vwash(0) = 0.0 !No precipitation
dwash(0) = 0.0
DO i = 1,NRAIN
  dwash(i) = A_RAIN*PWASH(i)**B_RAIN
  vwash(i) = ufall( rhoair,RHOW,rmuair,dwash(i) )
END DO
DO i = NRAIN+1,NWASH
  dwash(i) = A_SNOW*PWASH(i)**B_SNOW
  vwash(i) = V_SNOW
END DO

!-----  Calculate Scavenging Coefficients (s)

DO j = 1,ntypp

  icls = typeID(j)%icls

  CALL get_puff_material( j,pmatl )

  twash(0,j) = 0. !No precipitation

  IF( IsGas(icls) )THEN

    DO i = 1,NWASH
      CALL get_wash_gas( i,icls,pmatl,PWASH(i),dwash(i),vwash(i),twash(i,j) )
    END DO

!-----  Note: Wet particles only washout at the rate of the dry particle size
!             (but should be calculated on the fly)
  ELSE IF( IsLiquid(icls) .OR. IsParticle(icls) .OR. IsWetParticle(icls) )THEN

    DO i = 1,NWASH
      CALL get_wash_part( i,icls,pmatl,PWASH(i),dwash(i),vwash(i),twash(i,j) )
    END DO

  END IF

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_wash_part( ityppr,icls,pmatl,pr,dr,vr,tauwo )

USE scipuff_fi

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: ityppr, icls
TYPE( puff_material ), INTENT( IN  ) :: pmatl
REAL,                  INTENT( IN  ) :: pr, dr, vr
REAL,                  INTENT( OUT ) :: tauwo

REAL dif, rhop
REAL dp, vp

REAL,    EXTERNAL :: SetWashScale
LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsWetParticle

!-----  Get puff parameters

IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN !Particle

  CALL part_wash_param( pmatl,dp,rhop,vp,dif )

ELSE IF( IsLiquid(icls) )THEN !Liquid

  CALL liq_wash_param( pmatl,dp,rhop,vp,dif )

END IF

!-----  Return if particle fall speed is greater than precip

IF( vp > vr )THEN

  tauwo = 0.0

ELSE

  tauwo = SetWashScale( vp,dp,ityppr,pr,vr,dr,dif )

END IF

RETURN
END

!===============================================================================

REAL FUNCTION SetWashScale( vp,dp,ip,pr,vr,dr,dif )

USE scipuff_fi

IMPLICIT NONE

REAL,    INTENT( IN ) :: vp, dp      !Particle fall speed and diameter
INTEGER, INTENT( IN ) :: ip          !Precip type ID
REAL,    INTENT( IN ) :: pr, vr, dr  !Precip rate, fall speed and diameter
REAL,    INTENT( IN ) :: dif         !Particle diffusivity

REAL, PARAMETER :: MUW    = 1.00E-03
REAL, PARAMETER :: CNVFAC = 4.167E-07

REAL sc3, tau
REAL re, re2, sc, st, wi, h, e1, e2, e3, sc2

!--- Set washout time scale for particle size

!-----  Calculate dimensionless groups

tau = vp/g
re  = (0.5*dr*vr*rhoair)/rmuair
sc  = rmuair/(rhoair*dif)
st  = 2.*tau*(vr-vp)/dr
h   = dp/dr

IF( ip <= NRAIN )THEN
  wi = rmuair/MUW !RAIN
ELSE
  wi = 0.         !SNOW
END IF

re2 = SQRT(re)
sc2 = SQRT(sc)
sc3 = sc**(1./3.)

!-----  Seinfeld model uses St* (sts) but gives unreasonably sensitive
!       response; full model is:
!rsg       sts  = (1.2+(1./12.)*LOG(1.+re))/(1.+LOG(1.+re))
!rsg       IF( st > sts )THEN
!rsg         e2 = ((st-sts)/(st-sts+0.666667))**1.5
!rsg       ELSE
!rsg         e2 = 1.0E-09
!rsg       END IF

  e1   = 4.*h*(wi + (1.+2.*re2)*h)
  e2   = (st/(st+TwoThirds))**1.5
  e3   = 4.*(1. + re2*(0.4*sc3 + 0.16*sc2))/(re*sc)

!----- Also reduce efficiency by factor of 3 to match Horn et al. (1988)
!      and Sparmacher (1993) data

SetWashScale = CNVFAC*pr*(e1 + e2 + e3)/(3.0*dr)

RETURN
END

!===============================================================================

SUBROUTINE get_wash_gas( ityppr,icls,pmatl,pr,dr,vr,tauwo )

USE mauxstruct_fd

IMPLICIT NONE

INTEGER,               INTENT( IN  ) :: ityppr, icls
TYPE( puff_material ), INTENT( IN  ) :: pmatl
REAL,                  INTENT( IN  ) :: pr, dr, vr
REAL,                  INTENT( OUT ) :: tauwo

tauwo = 0.

RETURN
END

!===============================================================================

SUBROUTINE part_wash_param( pmatlIn,dp,rhop,vp,dif )

USE mauxstruct_fd

IMPLICIT NONE

TYPE( puff_material ), INTENT( IN  ) :: pmatlIn
REAL,                  INTENT( OUT ) :: dp, rhop, vp, dif

TYPE( part_material ) pmatl

pmatl = TRANSFER(pmatlIn,pmatl)

dp   = pmatl%dbar
rhop = pmatl%rho
vp   = pmatl%vd
dif  = pmatl%diff

RETURN
END

!===============================================================================

REAL FUNCTION SetPrate( zbar,zinv,hp,prbl )
!-----  Set Rain Precipitation Groups (mm/hr): Rain + Snow

USE precip_fd

IMPLICIT NONE

REAL, INTENT( IN ):: zbar,zinv,hp,prbl

REAL, PARAMETER                           :: CLDMIN = 500.
REAL, DIMENSION(0:NRAIN+NSNOW), PARAMETER :: PR = (/ 0.,0.5,3.5,25.0,5.,20.,100. /)

INTEGER ityppr,iprtyp

SetPrate = 0.

IF( zbar <= MAX(zinv,hp+CLDMIN) )THEN
  ityppr = NINT(prbl)
ELSE
  ityppr = 0
END IF

IF ( ityppr <= NRAIN ) THEN
  iprtyp = ityppr
ELSE
  iprtyp = 0
END IF
SetPrate = PR(iprtyp)

RETURN
END

!===============================================================================

SUBROUTINE liq_wash_param( pmatlIn,dp,rhop,vp,dif )

USE scipuff_fi

IMPLICIT NONE

REAL, PARAMETER :: TREF = 293.

TYPE( puff_material ), INTENT( IN  ) :: pmatlIn
REAL,                  INTENT( OUT ) :: dp, rhop, vp, dif

TYPE( liquid_material ) pmatl

REAL cs, hvd, difd, aspct, tem

REAL, EXTERNAL :: ufall

pmatl = TRANSFER(pmatlIn,pmatl)

dp  = pmatl%dbar

CALL lqd( pmatl,TREF,rhop,cs,hvd,difd )

CALL drop_deform( rhop,rhoair,dp,pmatl%st,aspct )

tem = rhop/(aspct*aspct)
vp  = ufall( rhoair,tem,rmuair,dp )
dp  = aspct*dp
dif = pmatl%diff

RETURN
END
