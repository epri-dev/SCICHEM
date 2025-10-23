MODULE pool_fi

  USE mauxstruct_fd
  USE param_fd

  IMPLICIT NONE

  SAVE

  INTEGER, PARAMETER                   :: NTMAX = (SCIPUFF_POOLSRCPARAM-7)/2
  REAL, PARAMETER                      :: KAPPA      = 0.4       ! Von Karman's constant
  REAL, PARAMETER                      :: SIGMA      = 0.85      ! Turbulent Schmidt-Prandtl number
  REAL, PARAMETER                      :: SBCON      = 5.6697E-8 ! Stefan-Boltzman constant
  REAL, PARAMETER                      :: Ea         = 0.75      ! Emissivity of the atm
  REAL, PARAMETER                      :: Ep         = 0.95      ! Emissivity of liquid pool
  REAL, PARAMETER                      :: kgrnd      = 1.4       ! Thermal conductivity for concrete
  REAL, PARAMETER                      :: alpha_grnd = 7.E-7     ! Thermal diffusivity for concrete
  REAL, PARAMETER                      :: z0_pool    = 1.e-3     ! Roughness for pool

  TYPE  hpool
    SEQUENCE
    REAL                               :: tm     !Time
    REAL                               :: Th     !Pool temperature
  END TYPE  hpool

  INTEGER                              :: nt
  REAL                                 :: tm, dt
  REAL                                 :: cpv, cpl, hvd, rhod, cs, pvap, rhoa, rhov
  REAL                                 :: difd, Qg, Qa, mflx ,jm, jh, Qnet
  REAL                                 :: apool, tp, ta, pb
  REAL                                 :: tboil, ckel,ustar
  REAL                                 :: Kconst, nu, chi, eta
  REAL                                 :: xfrac, xlg
  REAL                                 :: tgrnd           !Ground temperature in K (constant)
  REAL                                 :: taold           !Air temperature in K from previous dtstep
  REAL                                 :: mass_remaining  !Mass left in the pool
  REAL                                 :: mass_epsilon    !Mass evaporated but too small to subtract
  REAL                                 :: pool_age
  REAL                                 :: pool_offset

  TYPE( liquid_material )              :: pmatl
  TYPE( hpool ), DIMENSION(:), POINTER :: poolh

END MODULE

!-----------------------------------------------------------------------

SUBROUTINE InitPoolAux( imat,xmass,age,offset,rel )

USE cont_rel_fd
USE scipuff_fi
USE met_fi, ONLY: pbx => pb
USE srfevap_fi, ONLY: tsrf
USE pool_fi
USE UtilMtlAux
USE cont_rel_functions

IMPLICIT NONE

INTEGER,                  INTENT( IN )    :: imat
REAL,                     INTENT( IN )    :: xmass
REAL,                     INTENT( IN )    :: age
REAL,                     INTENT( IN )    :: offset
TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

INTEGER ios

rel%naux = SCIPUFF_POOLSRCPARAM
CALL allocate_contRel_aux( rel )

mass_remaining = xmass
mass_epsilon   = 0.
pool_age       = age
pool_offset    = offset

CALL SetPoolMaterial( imat )
IF( nError /= NO_ERROR )GOTO 9999

!---- Set constant

Kconst = kgrnd/SQRT(PI*alpha_grnd)

!---- Set ground temperature

tgrnd = tsrf

!---- Set air temperature

taold = tsrf

!---- calculate boiling point for liquid

tboil = pmatl%b/(pmatl%a - LOG10(pbx*0.7500616)) - pmatl%c - ABSZERO

!---- Initialize aux space

nt = 1
ALLOCATE( poolh(nt),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitPoolAux'
  eMessage = 'Error allocating local arrays'
  WRITE(eInform,'(A,I10)') 'Requested size =',nt
  GOTO 9999
END IF

poolh(1)%tm = 0.
poolh(1)%Th = MIN(tboil,tsrf)

CALL PutPoolAux( rel )

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE GetPoolAux( rel )

USE cont_rel_fd
USE scipuff_fi
USE pool_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

INTEGER ios, i, iaux

iaux = 1
mass_remaining = rel%saux(iaux)
IF( mass_remaining > 0.0 )THEN
  iaux  = iaux + 1
  mass_epsilon = rel%saux(iaux)
  iaux  = iaux + 1
  tgrnd = rel%saux(iaux)
  iaux  = iaux + 1
  taold = rel%saux(iaux)
  iaux  = iaux + 1
  pool_age = rel%saux(iaux)
  iaux  = iaux + 1
  pool_offset = rel%saux(iaux)
  iaux  = iaux + 1
  nt = NINT(rel%saux(iaux))
  IF( nt > 0 )THEN
    ALLOCATE( poolh(NTMAX),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'GetPoolAux'
      eMessage = 'Error allocating local arrays'
      WRITE(eInform,'(A,I10)') 'Requested size =',nt
      GOTO 9999
    END IF
    DO i = 1,nt
      iaux = iaux + 1
      poolh(i)%tm = rel%saux(iaux)
      iaux = iaux + 1
      poolh(i)%Th = rel%saux(iaux)
    END DO
    IF ( nt < NTMAX )THEN
      DO i = nt+1,NTMAX
        poolh(i)%tm = 0.0
        poolh(i)%Tm = 0.0
      END DO
    END IF
  END IF
END IF

9999 CONTINUE

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE PutPoolAux( rel )

USE cont_rel_fd
USE scipuff_fi
USE pool_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

INTEGER ios,i,iaux

iaux = 1
rel%saux(iaux) = mass_remaining
IF( mass_remaining > 0.0 )THEN
  iaux = iaux + 1
  rel%saux(iaux) = mass_epsilon
  iaux = iaux + 1
  rel%saux(iaux) = tgrnd
  iaux = iaux + 1
  rel%saux(iaux) = taold
  iaux = iaux + 1
  rel%saux(iaux) = pool_age
  iaux = iaux + 1
  rel%saux(iaux) = pool_offset
  iaux = iaux + 1
  rel%saux(iaux) = FLOAT(nt)
  IF( nt > 0 )THEN
    DO i = 1,nt
      iaux = iaux + 1
      rel%saux(iaux) = poolh(i)%tm
      iaux = iaux + 1
      rel%saux(iaux) = poolh(i)%th
    END DO
    IF ( nt < NTMAX )THEN
      DO i = nt+1,NTMAX
        iaux = iaux + 1
        rel%saux(iaux) = 0.0
        iaux = iaux + 1
        rel%saux(iaux) = 0.0
      END DO
    END IF
  END IF
END IF

DEALLOCATE( poolh,STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'PutPoolAux'
  eMessage = 'Error deallocating local arrays'
  WRITE(eInform,'(A)') 'Requested array = poolh'
END IF

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE StepPool( rate,rhop,dp,tair,ustr,zruf,patm,mass0,dtstep )

! --- Routine for stepping an evaporating (boiling/non-boiling) pool,
!     using internal time steps. Returns average evaporation rate (Kg/s),
!     for user time step dt, at time tx

USE scipuff_fi
USE pool_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: rate   !mass evaporation rate (kg/sec)
REAL, INTENT( OUT ) :: rhop   !vapor density
REAL, INTENT( IN  ) :: dp     !pool diameter (m)
REAL, INTENT( IN  ) :: tair   !air temp (K)
REAL, INTENT( IN  ) :: ustr   !ustar (m/s)
REAL, INTENT( IN  ) :: zruf   !surface roughness (m)
REAL, INTENT( IN  ) :: patm   !atmospheric pressure
REAL, INTENT( IN  ) :: mass0  !initial pool mass
REAL, INTENT( IN  ) :: dtstep !time step (s)

REAL  tm0, tfinal
REAL  dtp, dtt, dtm
REAL  denom, zm
REAL  mrate, mass_evap, mass, eps
REAL, EXTERNAL :: fun_rhoa

INTERFACE
  INTEGER FUNCTION reduce_poolh( a ) RESULT( irv )
    USE pool_fi
    TYPE( hpool ), DIMENSION(:), POINTER  :: a
  END FUNCTION
END INTERFACE

pb = patm

IF( mass_remaining <= 0 )THEN
  rate = 0.0
  GOTO 9999
END IF

tp    = poolh(nt)%Th

zm = 20.0*MAX(zruf,z0_pool)
ustar = ustr*LOG(zm/zruf+1.0)/LOG(zm/z0_pool+1.0)   ! Pool ustar

!--- Calculate pool area

apool = 0.25*PI*dp*dp

!---- Find boiling point (Kelvin)

ckel  = pmatl%c + ABSZERO
tboil = pmatl%b/(pmatl%a - LOG10(pb*0.7500616)) - ckel
IF( tp > tboil )tp = tboil

!--- Save user time step

tm     = poolh(nt)%tm
tfinal = tm + dtstep
dt     = 1.E36
dtt    = 1.E36
dtm    = 1.E36

mass      = mass_remaining - mass_epsilon
mass_evap = 0.0

IF( tair /= taold )dtt = 0.1*dtstep

DO WHILE( tm < tfinal )

  dt = MIN(0.9*dtm,0.9*dtt,2.*dt,tfinal-tm)

!--- Reduce, if required, the time levels in pool history

  IF( nt == NTMAX-1 )THEN
    IF( reduce_poolh(poolh) /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'StepPool'
      eMessage = 'Error reducing storage space'
      GOTO 9999
    END IF
  END IF

!-- call routine to calculate mass flux and heat flux (ground and air)

  tm0 = tm
  tm  = tm + dt
  ta  = taold + (tm - tfinal + dtstep)*(tair - taold)/dtstep

!--- calculate vapor density

  rhoa = fun_rhoa( ta,pb  )
  rhov = rhoa*pmatl%w/MWAIR
  rhop = rhov

  CALL get_mflux( dp )

!--- Find change in pool temperature

  denom = cpl*mass/(dt*apool) + nu + chi + eta
  dtp   = Qnet/denom
  mrate = mflx*apool                            ! kg/s

!--- Check for boiling pool

  IF( tp + dtp >= tboil )THEN
    mflx = mflx + MAX((tp + dtp - tboil),0.0)*cpl*mass/(hvd*apool*dt)
    dtp  = tboil - tp
  END IF

!--- Time step size for 1% of mass to evaporate

  dtm = 0.01*mass0/mrate
  dtt = 1.E36

!---- Reset time, if time step dt is greater than dtm
!     and redo calculations.

  IF( dt > 1.01*dtm )THEN
    tm = tm0
    CYCLE
  END IF

!---- calculate mass evaporated with the time step size of dt

  IF( mass - mrate*dt > 0.0 )THEN
    mass_evap = mass_evap + mrate*dt
    mass      = mass_remaining - (mass_epsilon + mass_evap)
  ELSE
    mass_evap = mass_evap + mass
    EXIT
  END IF

  tp           = tp + dtp
  nt           = nt + 1
  poolh(nt)%tm = tm
  poolh(nt)%Th = tp

END DO

taold = tair

!--- Calculate overall evaporation rate

rate = mass_evap/dtstep          ! kg/s

!--- Calculate final mass

mass_epsilon = mass_epsilon + mass_evap
eps = mass_remaining*EPSILON(mass_remaining)
IF( mass_epsilon > eps )THEN
  mass_remaining = MAX(0.0,mass_remaining - mass_epsilon)
  mass_epsilon   = 0.0
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION reduce_poolh( a ) RESULT( irv )

USE pool_fi

IMPLICIT NONE

TYPE( hpool ), DIMENSION(:), POINTER :: a

INTEGER n2, i, i0

n2 = INT(NTMAX/2)
i0 = INT(n2/2)

!--- Reduce the first half of time history by half

DO i = 1,n2-1,2
  a((i+1)/2) = a(i)
END DO

!--- Move the second half up

DO i = 1,NTMAX-n2
  a(i0+i) = a(n2-1+i)
END DO

nt  = i0 + NTMAX - n2

irv = 0

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE get_mflux( dp )

! ----- calculate mass flux and heat flux (ground and air)

USE pool_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: dp

!--- Calculate heat flux from the atmosphere

CALL lqd( pmatl,tp,rhod,cs,hvd,difd )
pvap  = cs*tp*8.31436E1/pmatl%w               ! mb
xfrac = MIN(0.9999,pvap/pb)
IF( xfrac < 0.0005 )THEN          !Asymptotic solution for small x
  xlg = 1.0 + (0.5 + 0.3333333*xfrac)*xfrac
ELSE
  xlg = -LOG(1.0-xfrac)/xfrac
END IF

!---- Calculate dimensionless mass and heat flux number

CALL JFlux( dp )

chi = rhov*cpv*ustar*jh*(1.-xfrac)*xlg
eta = 4.*SBCON*Ep*tp**3
Qa  = chi*(ta-tp) + SBCON*(Ea*ta**4 - Ep*tp**4)

!--- Calculate heat flux from the ground

nu = Kconst*2./SQRT(dt)

IF( nt == 1 )THEN
  Qg = nu*(tgrnd-tp)
ELSE
  CALL GetQg()
END IF

!---- Calculate mass flux and adjust for flux at boiling point

mflx = ustar*cs*jm*xlg                       ! kg/(m2.s)
Qnet = Qg  + Qa  - mflx*hvd

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE GetQg()

! ----- Find the ground heat flux using the pool temperature history

USE pool_fi

IMPLICIT NONE

INTEGER i
REAL    dtm, dtp, dt0, dt1
REAL    Qs

Qs = 0.0

DO i = 2,nt
  dtm  = poolh(i)%tm - poolh(i-1)%tm
  dtp  = poolh(i)%Th - poolh(i-1)%Th
  dt0  = 1./SQRT(tm - poolh(i-1)%tm)
  dt1  = 1./SQRT(tm - poolh(i)%tm)
  Qs   = Qs + ((poolh(i)%Th*poolh(i-1)%tm - poolh(i-1)%Th*poolh(i)%tm)*(dt1 - dt0) &
            - dtp*(1./dt1 + tm*dt1 - 1./dt0 - tm*dt0))/dtm
END DO

Qg = Kconst*(tgrnd/SQRT(tm) - Qs - poolh(nt)%Th/SQRT(dt))

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE JFlux( dp )

! --- Calculate dimensionless mass/heat flux for evaporating pool
!   ( Based on Kunsch J. P., Jrnl. of Haz. Matrl., 59, 1998, 167-187)

USE basic_fi
USE pool_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: dp

REAL, PARAMETER :: PR = 0.85

INTEGER i
REAL    rnua, jflx
REAL    re, sc, dm, Ld, rnvold, c1, c2
REAL    rnv, beta

!-----  Compute the Schmidt number, Sc and Reynolds number, Re.

rnua = rmuair/rhoair               ! Kinematic viscosity
re   = ustar*z0_pool/rnua          ! Flat plate Re
sc   = rnua/difd
ld   = dp/z0_pool

rnvold  = 1.
rnv     = 1./7.

DO WHILE( ABS(rnv-rnvold)/rnvold > 0.01 )
  rnvold = rnv
  C1  = KAPPA*KAPPA*rnv*(1.+ rnv)*(1.+ 2.*rnv)/(SIGMA*EXP(1./rnv))
  C2  = 1. + 0.5*rnv/(1.+ rnv)
  rnv = 1./(1./rnv +  1./(1.+ 2.*rnv)*LOG(C1*C2*Ld))
END DO

DO i = 1,2

  IF( i == 1 )THEN
    dm = sc
  ELSE
    dm = PR
  END IF

  IF( re < 0.13 )THEN
    beta = (3.85*dm**0.3333 - 1.3)**2 + SIGMA/KAPPA*LOG(0.13*dm)
  ELSE
    beta = 7.3*re**0.25*SQRT(dm) - 5.0*SIGMA
  END IF

  jflx = C2/(beta + SIGMA/(KAPPA*rnv)*(C1*C2*Ld)**(rnv/(1.+ 2.*rnv)))

  IF( i == 1 )THEN
    jm = jflx
  ELSE
    jh = jflx
  END IF

END DO

RETURN
END

!-----------------------------------------------------------------------

SUBROUTINE SetPoolMaterial( imat )

USE scipuff_fi
USE pool_fi
USE UtilMtlAux

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imat

INTEGER iaux, icls

TYPE( aerosol_material ) pmatlAer

LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsLiquid

iaux = material(imat)%iaux
icls = material(imat)%icls

IF( IsLiquid(icls) )THEN

  CALL GetLiquidParam( pmatl,iaux,2,mat_aux )
  cpv = pmatl%cpV
  cpl = pmatl%cpL

ELSE IF( IsAerosol(icls) )THEN

  CALL GetAerosolParam( pmatlAer,iaux,mat_aux )
  pmatl%rho  = pmatlAer%rho
  pmatl%rhob = 0.0
  pmatl%a    = pmatlAer%a
  pmatl%b    = pmatlAer%b
  pmatl%c    = pmatlAer%c
  pmatl%w    = pmatlAer%w
  cpv        = pmatlAer%cpV
  cpl        = pmatlAer%cpL

ELSE

  nError   = IV_ERROR
  eMessage = 'Invalid Material Class'
  WRITE(eInform,*)'Class =',icls
  eRoutine = 'SetPoolMaterial'
  GO TO 9999

END IF

9999 CONTINUE

RETURN
END
