SUBROUTINE InitCalcBL( fld,t )

!------ Initialize calculated boundary layer

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld
REAL,             INTENT( IN    ) :: t

INTEGER i, j, nstep
REAL    dt, dts, ts, tUTC
LOGICAL tflag_sav

REAL, DIMENSION(2) :: tsr, tss, t_local

REAL, EXTERNAL :: SolarTime, UTCtime

!------ Get saved BL depth if beyond start of run; compute aux BL fields

IF( t > 0. )THEN

  CALL StepCalcBL( fld,t,0. )  !Timestep = 0 so zi is not advanced
  IF( error%Number /= NO_ERROR )GOTO 9999

ELSE

!------ Otherwise compute ZI from stable BL at sunrise

  IF( .NOT.BTEST(fld%type,FTB_ZI) )CALL InitStableBL( fld )

  tUTC = UTCtime( t )

  t_local(1) = SolarTime( tUTC,fld%grid%lon(1) )
  t_local(2) = SolarTime( tUTC,fld%grid%lon(fld%grid%nXY) )
  tsr(1)     = fld%grid%sunrise(1)
  tsr(2)     = fld%grid%sunrise(fld%grid%nXY)
  tss(1)     = fld%grid%sunset(1)
  tss(2)     = fld%grid%sunset(fld%grid%nXY)

!------ Check if BL must be advanced from dawn to current time

  dt    = 0.2  !12 min timestep
  nstep = 0

  DO i = 1,2
    DO j = 1,2
      IF( t_local(i) > tsr(j) .AND. t_local(i) < tss(j) )THEN
        nstep = MAX(nstep,NINT((t_local(i)-tsr(j))/dt))
      END IF
    END DO
  END DO

  tflag_sav = BTEST(fld%type,FTB_T)
  IF( nstep > 0 )fld%type = IBCLR(fld%type,FTB_T) !Use standard atmosphere profile for calculation of BL entrainment
                                                  !through daytime
  dts = dt*3600.
  ts  = t - FLOAT(nstep)*dts
  DO i = 1,nstep + 1
    CALL StepCalcBL( fld,ts,dts )
    IF( error%Number /= NO_ERROR )GOTO 9999
    ts  = ts + dts
  END DO

  IF( tflag_sav )fld%type = IBSET(fld%type,FTB_T)

END IF
!  stop
!------ Clear initialization bit

fld%status = IBCLR(fld%status,FSB_DOINITBL)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE InitStableBL( fld )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ),   INTENT( INOUT ) :: fld

INTEGER i0, i, j, ix, ik
REAL    rlat, tref, pref, zruf, hc, alpha, bowen, albedo
REAL    wspd, ustar, rn, zbl, cc, L
LOGICAL ComputeZi, FixedHeatFlux, tref_flag, pref_flag

REAL, EXTERNAL :: InvLfromL

tref = TSURF
pref = PSURF

tref_flag = ASSOCIATED(fld%field%Tpot)  !Set according to met input for consistency in initializing BL
pref_flag = ASSOCIATED(fld%field%Press) !when standard atmosphere is used (and tflag=F)

FixedHeatFlux = BTEST(fld%type,FTB_HFLX) !Heat flux fixed if available
ComputeZi     = .TRUE.

!$OMP PARALLEL SHARED(fld,tref_flag,pref_flag,ComputeZi,FixedHeatFlux)
!$OMP DO PRIVATE(j,i0,ix,i,wspd,zruf,hc,alpha,bowen,albedo,cc,rn,ik,zbl,rlat,ustar,L,tref,pref)
DO j = 1,fld%grid%nY
  i0 = (j-1)*fld%grid%nX

  DO ix = 1,fld%grid%nX
    i = i0 + ix

    CALL SetSurfRef( tref_flag,pref_flag,fld%BLaux%tbl(i),fld%BLaux%pbl(i), &
                       fld%grid%terrain%H(i)+fld%grid%Hmin,tref,pref )

    wspd   = SQRT(fld%BLaux%wspd2(i))
    zruf   = fld%grid%landcover%roughness(i)
    hc     = fld%grid%landcover%canopyHt(i)
    alpha  = fld%grid%landcover%alpha(i)
    bowen  = fld%grid%landcover%Bowen(i)
    albedo = fld%grid%landcover%albedo(i)
    cc     = fld%BL%cc(i)

    rn           = -1.  !Set for stable conditions
    fld%BL%zi(i) = 100. !Initial value

!------ Compute night-time depth; iterate on surface layer depth and L

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nXY + i
      zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
    ELSE
      zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%D(i)
    END IF
    rlat = fld%grid%lat(i)
    ustar = NOT_SET_R
    CALL SurfIter( zbl,wspd,tref,pref,zruf,hc,alpha,bowen, &
                   cc,rn,rlat,ComputeZi,FixedHeatFlux, &
                   fld%BL%HeatFlux(i),fld%BL%zi(i), &
                   fld%BLaux%zsl(i),L,ustar,.FALSE. )
    fld%BLaux%ustr2(i) = ustar**2

!------ Set Zsl

    CALL set_zsl( fld%BLaux%zsl(i),fld%BL%zi(i),L, &
                fld%grid%landcover%roughness(i),fld%grid%landcover%canopyHt(i) )

    fld%BL%invMOL(i) = InvLfromL( L )

  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END

!==============================================================================

SUBROUTINE StepCalcBL( fld,t,dt )

!------ Advance calculated boundary layer

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ),   INTENT( INOUT ) :: fld
REAL,               INTENT( IN    ) :: t, dt

REAL, PARAMETER :: TAU_ZI = 1800.  !Relaxation timescale for stable BL

INTEGER i0, i, j, k, kp, ix, ik, ikp, alloc_stat
INTEGER nxb, nyb, nzb, nxy
REAL    tsr, tss,  t_local, tUTC
REAL    rlat, rlon, tref, pref, wspd, sun_ang, qr, rn, cc
REAL    zruf, hc, alpha, albedo, bowen, L
REAL    rate, tx, dzmax, zbl, dt_adv, dtx, dts

LOGICAL tref_flag, pref_flag, FixedHeatFlux, ComputeZi

REAL, DIMENSION(:), ALLOCATABLE :: zi_eq, ziOld, ustar, us, vs, ws, dxr, dyr
REAL, DIMENSION(:), ALLOCATABLE :: hflx, facL, zsl_eq

REAL, DIMENSION(:), POINTER :: Zptr

REAL, EXTERNAL :: SolarTime, UTCTime, zi_rate, InvLfromL, LfromInvL

!------ UTC time

tUTC = UTCTime( t )

!------ Set reference temperature & pressure flags

tref_flag = ASSOCIATED(fld%field%Tpot)  !Set according to met input for consistency in initializing BL
pref_flag = ASSOCIATED(fld%field%Press) !when standard atmosphere is used (and tflag=F)

FixedHeatFlux =  BTEST(fld%type,FTB_HFLX)   !Heat flux is fixed if available
ComputeZi     = .NOT.BTEST(fld%type,FTB_ZI) !Compute zi if not available
ComputeZi     = ComputeZi .AND. dt > 0.     !and dt > 0.

nxb = fld%grid%nX
nyb = fld%grid%nY
nzb = fld%grid%nZ
nxy = nxb*nyb

!------ Allocate arrays for dynamic calculation

ALLOCATE( zi_eq(nxy),ziOld(nxy),ustar(nxy),us(nxy),vs(nxy),ws(nxy), &
                       hflx(nxy),facL(nxy),zsl_eq(nxy), &
                       dxr(nxy),dyr(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'StepCalcBL'
  error%Message = 'Error allocating work arrays'
  GOTO 9999
END IF

IF( FixedHeatFlux )THEN
  DO i = 1,nxy
    hflx(i) = fld%BL%HeatFlux(i)
  END DO
END IF

!------ Loop over all met grid points

!$OMP PARALLEL SHARED(fld,nxb,nyb,tref_flag,pref_flag,zi_eq,ustar,ComputeZi,FixedHeatFlux,hflx,zsl_eq,facL)
!$OMP DO PRIVATE(j,i0,ix,i,wspd,zruf,hc,alpha,bowen,albedo,cc,rlat,rlon,tsr,tss,t_local,sun_ang,qr,rn,ik,zbl,L,tref,pref)
DO j = 1,nyb
  i0 = (j-1)*nxb

  DO ix = 1,nxb
    i = i0 + ix

    CALL SetSurfRef( tref_flag,pref_flag,fld%BLaux%tbl(i),fld%BLaux%pbl(i), &
                       fld%grid%terrain%H(i)+fld%grid%Hmin,tref,pref )

    wspd     = SQRT(fld%BLaux%wspd2(i))
    zi_eq(i) = fld%BL%zi(i)
    zruf     = fld%grid%landcover%roughness(i)
    hc       = fld%grid%landcover%canopyHt(i)
    alpha    = fld%grid%landcover%alpha(i)
    bowen    = fld%grid%landcover%Bowen(i)
    albedo   = fld%grid%landcover%albedo(i)
    cc       = fld%BL%cc(i)

    rlat = fld%grid%lat(i)
    rlon = fld%grid%lon(i)
    tsr  = fld%grid%sunrise(i)
    tss  = fld%grid%sunset(i)
    t_local = SolarTime( tUTC,rlon )

    IF( t_local > tsr .AND. t_local < tss )THEN
      CALL sun( rlat,fld%BLaux%JulianDay,t_local,sun_ang )
      CALL total( sun_ang,cc,qr )
    ELSE
      sun_ang = 0.
      qr      = 0.
    END IF

    CALL hvnet( sun_ang,albedo,cc,qr,tref,rn )

!------ iterate on z_sl and xml

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nXY + i
      zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
    ELSE
      zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%D(i)
    END IF
    ustar(i) = NOT_SET_R
    CALL SurfIter( zbl,wspd,tref,pref,zruf,hc,alpha,bowen, &
                   cc,rn,rlat,ComputeZi,FixedHeatFlux, &
                   hflx(i),zi_eq(i),zsl_eq(i),L,ustar(i),.FALSE. )
    IF( hflx(i) > 0.  .OR. .NOT.ComputeZi )THEN
      fld%BLaux%ustr2(i) = ustar(i)**2
      fld%BL%invMOL(i)   = InvLfromL( L )
      fld%BLaux%zsl(i)   = zsl_eq(i)
      IF( .NOT.FixedHeatFlux )fld%BL%HeatFlux(i) = hflx(i)
    ELSE
      facL(i) = tref/(VONK*G0)
    END IF

  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

IF( ComputeZi )THEN

!------ Time integration

  tx     = dt
  dt_adv = dt

  txLoop : DO WHILE( tx > 0. )

    DO i = 1,nxy
      ziOld(i) = fld%BL%zi(i)
    END DO

!------ Set velocity for advection

    CALL SetAdvZi( fld,us,vs,ws,dxr,dyr,dt_adv )
    dt_adv = MIN(dt_adv,tx)

!$OMP PARALLEL SHARED(fld,nxb,nyb,nzb,nxy,dt_adv,ustar,ziOld,us,vs,ws, &
!$OMP dxr,dyr,FixedHeatFlux,hflx,zi_eq,zsl_eq,facL)
!$OMP DO PRIVATE(j,i0,ix,i,dtx,k,rate,kp,ik,ikp,dzmax,dts)
    DO j  = 1,nyb
      i0 = (j-1)*nxb

      DO ix = 1,nxb
        i = i0 + ix

!------ Find growth rate for positive heat flux

        IF( hflx(i) > 0. )THEN

          dtx = dt_adv

          DO WHILE( dtx > 0. )

            IF( BTEST(fld%type,FTB_T) .AND. nzb > 1 )THEN
              k = 1
            ELSE
              k = 0
            END IF

            rate = zi_rate( fld,i,ustar(i) )

!------ Limit vertical growth

            IF( k /= 0 )THEN

              IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
                Zptr => fld%grid%sigma%Z(i:nxy*nzb:nxy)
              ELSE
                Zptr => fld%grid%Z
              END IF
              DO WHILE( Zptr(k)*fld%grid%terrain%D(i) <= fld%BL%zi(i) )
                k = k + 1
                IF( k > nzb )EXIT
              END DO
              IF( k > nzb )k = 0
            END IF

            IF( k > 0 )THEN
              IF( k == nzb )THEN
                kp = k - 1
              ELSE
                kp = k + 1
              END IF
              IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
                ik  = (k-1)*nxy + i
                ikp = (kp-1)*nxy + i
                dzmax = 0.01*ABS(fld%grid%sigma%Z(ikp)-fld%grid%sigma%Z(ik))
                dzmax = MAX(fld%grid%sigma%Z(ik) - fld%BL%zi(i),dzmax)
              ELSE
                dzmax = 0.01*ABS(fld%grid%Z(kp)-fld%grid%Z(k))
                dzmax = MAX(fld%grid%Z(k)*fld%grid%terrain%D(i) - fld%BL%zi(i),dzmax)
              END IF
            ELSE
              dzmax = 1.E+10
            END IF

            dzmax = MIN(dzmax,MAX(10.,0.1*fld%BL%zi(i)))
            dts   = MIN(dtx,dzmax/(rate+SMALL))

            fld%BL%zi(i) = fld%BL%zi(i) + rate*dts

            CALL set_zsl( fld%BLaux%zsl(i),fld%BL%zi(i),LfromInvL(fld%BL%invMOL(i)), &
                          fld%grid%landcover%roughness(i),fld%grid%landcover%canopyHt(i) )
            dtx = dtx - dts

          END DO

        ELSE

          rate = dt_adv / TAU_ZI
          fld%BLaux%ustr2(i) = (fld%BLaux%ustr2(i) + rate*ustar(i)**2) / (1.+rate)
          fld%BL%zi(i)       = (fld%BL%zi(i)       + rate*zi_eq(i)   ) / (1.+rate)
          fld%BLaux%zsl(i)   = (fld%BLaux%zsl(i)   + rate*zsl_eq(i)  ) / (1.+rate)
          fld%BLaux%zsl(i)   = MIN(fld%BLaux%zsl(i),fld%BL%zi(i))
          IF( .NOT.FixedHeatFlux )fld%BL%HeatFlux(i) = (fld%BL%HeatFlux(i) + rate*hflx(i) ) / (1.+rate)
          fld%BL%invMOL(i)   = -fld%BL%HeatFlux(i)/fld%BLaux%ustr2(i)**1.5 / facL(i)

        END IF

        CALL AdvZi( ix,j,fld,ziOld,us(i),vs(i),ws(i),dxr(i),dyr(i),dt_adv )

      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

    tx = tx - dt_adv

  END DO txLoop

END IF

9999 CONTINUE

IF( ALLOCATED(zi_eq)  )DEALLOCATE( zi_eq,STAT=alloc_stat )
IF( ALLOCATED(ustar)  )DEALLOCATE( ustar,STAT=alloc_stat )
IF( ALLOCATED(hflx)   )DEALLOCATE( hflx, STAT=alloc_stat )
IF( ALLOCATED(facL)   )DEALLOCATE( facL, STAT=alloc_stat )
IF( ALLOCATED(zsl_eq) )DEALLOCATE( facL, STAT=alloc_stat )
IF( ALLOCATED(ziOld)  )DEALLOCATE( ziOld,STAT=alloc_stat )
IF( ALLOCATED(us)     )DEALLOCATE( us,   STAT=alloc_stat )
IF( ALLOCATED(vs)     )DEALLOCATE( vs,   STAT=alloc_stat )
IF( ALLOCATED(ws)     )DEALLOCATE( ws,   STAT=alloc_stat )
IF( ALLOCATED(dxr)    )DEALLOCATE( dxr,  STAT=alloc_stat )
IF( ALLOCATED(dyr)    )DEALLOCATE( dyr,  STAT=alloc_stat )
IF( ALLOCATED(dyr)    )DEALLOCATE( dyr,  STAT=alloc_stat )

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION SolarTime( tz,lon )

IMPLICIT NONE

REAL, INTENT( IN ) :: tz, lon

REAL, PARAMETER :: DEGHR = 15.
REAL, PARAMETER :: HR24  = 24.

SolarTime = tz + lon/DEGHR

IF( SolarTime >= HR24 )THEN
  SolarTime = SolarTime - HR24
ELSE IF( SolarTime < 0. )THEN
  SolarTime = SolarTime + HR24
END IF

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION zi_rate( fld,i,ustar ) RESULT( rate )

USE SWIMmetField_fd
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ),   INTENT( IN ) :: fld
INTEGER,            INTENT( IN ) :: i
REAL,               INTENT( IN ) :: ustar

REAL, PARAMETER :: B_KP  =  2.5
REAL, PARAMETER :: A_ENT =  1.4

REAL    gamma, dzi_mech, dzi_conv, zi, ziMSL
REAL    tem, pr, d
INTEGER k, ip, nz, nxy

REAL, DIMENSION(:), POINTER :: z, Tpot

zi = fld%BL%zi(i)

IF( BTEST(fld%type,FTB_T) )THEN
  d  =  fld%grid%terrain%D(i)
  z  => fld%grid%Z
  nz  = fld%grid%nZ
  nxy = fld%grid%nXY
  IF( BTEST(fld%grid%type,GTB_Z3D) )z => fld%grid%sigma%Z(i:nxy*nz:nxy)
  IF( zi < z(nz)*d .AND. zi > z(1)*d )THEN
    Tpot => fld%field%Tpot
    k = 1
    DO WHILE( z(k)*d < zi )
      k = k + 1
    END DO
    k = MIN(k+1,nz)                     !Set to give gradient above zi
    ip = (k-1)*fld%grid%nXY + i
    IF( BTEST(fld%grid%type,GTB_STAGGER) )ip = ip + fld%grid%nXY
    gamma = (Tpot(ip)-Tpot(ip-nxy))/(d*(z(k)-z(k-1)))
  ELSE
    ziMSL = zi + fld%grid%terrain%H(i) + fld%grid%hmin
    CALL stnd_atmos( ziMSL,pr,tem,gamma,SA_TA )
  END IF
ELSE
  ziMSL = zi + fld%grid%terrain%H(i) + fld%grid%hmin
  CALL stnd_atmos( ziMSL,pr,tem,gamma,SA_TA )
END IF

gamma    = MAX(gamma,0.001)
dzi_mech = 2.*B_KP*ustar**3/(gamma*G0/300.*zi**2)
dzi_conv = A_ENT*fld%BL%HeatFlux(i)/(gamma*zi)
rate     = MAX(dzi_mech,dzi_conv)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE SetAdvZi( fld,ui,vi,wi,dxr,dyr,dt_adv )

!------ Set velocities for advecting boundary layer depth

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterp_fd
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( MetField ),               INTENT( IN    ) :: fld
REAL, DIMENSION(fld%grid%nXY ), INTENT(   OUT ) :: ui, vi, wi
REAL, DIMENSION(fld%grid%nXY ), INTENT(   OUT ) :: dxr, dyr
REAL,                           INTENT( INOUT ) :: dt_adv

TYPE( metv ) mv

INTEGER i, j, i0, ip, ipm, is, km, jm, im, i0m
INTEGER kp, izp, izm
INTEGER nxb, nyb, nzb, nxyb
REAL    zu, xfac, yfac, xb, yb, wp
REAL    dtx, dty, umax, vmax, hx, hy

REAL, DIMENSION(:), ALLOCATABLE :: dt_advx

REAL, DIMENSION(:), POINTER :: u,  v, w, zi
REAL, DIMENSION(:), POINTER :: zb, zbw

IF( fld%grid%nXY == 1 )THEN
  ui = 0.; vi = 0.; wi = 0.
  RETURN
END IF

nxb  = fld%grid%nX
nyb  = fld%grid%nY
nzb  = fld%grid%nZ
nxyb = fld%grid%nXY

zi => fld%BL%zi
u  => fld%field%U
v  => fld%field%V
w  => fld%field%W

zb => fld%grid%Z

umax = 0.
vmax = 0.

ALLOCATE( dt_advx(nyb),STAT=i )

!------ Build 2d arrays of u, v, & w

IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    zbw => fld%grid%Zw
  ELSE
    zbw => fld%grid%Z
  END IF

!$OMP PARALLEL SHARED(fld,nxb,nyb,nxyb,nzb,zi,ui,vi,wi)
!$OMP DO PRIVATE(i,j,jm,i0,i0m,is,zu,km,kp,im,ip,ipm,izp,izm,wp,zb,zbw,mv)
  DO j = 1,nyb
    jm  = MAX(j-1,1)
    i0  = (j-1)*nxb
    i0m = (jm-1)*nxb

    DO i = 1,nxb
      is = i0 + i

      IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
        CALL trans_z( zi(is),zu,i,j,fld%grid )
      ELSE
        zu = zi(is)
      END IF

      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        zb => fld%grid%sigma%Z(is:nxyb*nzb:nxyb)
      ELSE
        zb => fld%grid%Z
      END IF
      CALL SetZfac( zu,zb,nzb,mv,0.,0. )

      km = MAX(1,ABS(mv%km)) + 1
      kp = MIN( km+1,nzb+1 )

      im  = i0 + MAX(i-1,1)
      ip  = (km-1)*nxyb + is
      ipm = (km-1)*nxyb + im
      izp = (kp-1)*nxyb + is
      izm = (kp-1)*nxyb + im
      ui(is) = 0.5*(mv%rzm1*(u(ip)  + u(ipm)) + &
                    mv%ratz*(u(izp) + u(izm)) )

      im  = i0m + i
      ipm = (km-1)*nxyb + im
      izm = (kp-1)*nxyb + im
      vi(is) = 0.5*(mv%rzm1*(v(ip)  + v(ipm)) + &
                    mv%ratz*(v(izp) + v(izm)) )

      IF( BTEST(fld%type,FTB_W) )THEN

        IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
          IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
            zbw => fld%grid%sigma%Zw(is:nxyb*nzb:nxyb)
          ELSE
            zbw => fld%grid%sigma%Z(is:nxyb*nzb:nxyb)
          END IF
        ELSE
          zbw => fld%grid%Zw
        END IF

        CALL SetZfac( zu,zbw,nzb,mv,0.,0. )
        km = ABS(mv%km)
        ip = (km-1)*nxyb + is

        IF( km <= nzb )THEN
          wp = w(ip+nxyb)
        ELSE
          wp = 0.
        END IF

        wi(is) = mv%rzm1*w(ip) + mv%ratz*wp

      ELSE

        wi(is) = 0.

      END IF

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  DO is = 1,nxyb
    umax = MAX(ABS(ui(is)),umax)
    vmax = MAX(ABS(vi(is)),vmax)
  END DO

ELSE

!$OMP PARALLEL SHARED(fld,nxb,nyb,nxyb,nzb,zi,ui,vi,wi)
!$OMP DO PRIVATE(i,j,i0,is,zu,km,kp,ip,wp,mv,zb)
  DO j = 1,nyb
    i0 = (j-1)*nxb

    DO i = 1,nxb
      is = i0 + i

      IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
        CALL trans_z( zi(is),zu,i,j,fld%grid )
      ELSE
        zu = zi(is)
      END IF

      IF( nzb > 1 )THEN

        IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
          zb => fld%grid%sigma%Z(is:nxyb*nzb:nxyb)
        ELSE
          zb => fld%grid%Z
        END IF
        CALL SetZfac( zu,zb,nzb,mv,0.,0. )
        km = ABS(mv%km)
        ip = (km-1)*nxyb + is

        IF( km < nzb )THEN

          ui(is) = mv%rzm1*u(ip) + mv%ratz*u(ip+nxyb)
          vi(is) = mv%rzm1*v(ip) + mv%ratz*v(ip+nxyb)

          IF( BTEST(fld%type,FTB_W) )THEN
            wi(is) = mv%rzm1*w(ip) + mv%ratz*w(ip+nxyb)
          ELSE
            wi(is) = 0.
          END IF

        ELSE

          ui(is) = u(ip)
          vi(is) = v(ip)
          wi(is) = 0.

        END IF

      ELSE

        ui(is) = u(is)
        vi(is) = v(is)
        wi(is) = 0.

      END IF


    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  DO is = 1,nxyb
    umax = MAX(ABS(ui(is)),umax)
    vmax = MAX(ABS(vi(is)),vmax)
  END DO

END IF

!------ Ensure parallel flow near the ground

IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN

!$OMP PARALLEL SHARED(fld,nxb,nyb,nxyb,nzb,zi,ui,vi,wi)
!$OMP DO PRIVATE(i,j,jm,i0,i0m,is,im,zu,zbw,xfac,hx,hy,mv,wp)
  DO j = 1,nyb
    jm  = MAX(j-1,1)
    i0  = (j-1)*nxb
    i0m = (jm-1)*nxb

    DO i = 1,nxb
      is = i0 + i
      im = i0 + MAX(i-1,1)

      CALL trans_z( zi(is),zu,i,j,fld%grid )

      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
          zbw => fld%grid%sigma%Zw(is:nxyb*nzb:nxyb)
        ELSE
          zbw => fld%grid%sigma%Z(is:nxyb*nzb:nxyb)
        END IF
      ELSE
        IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
          zbw => fld%grid%Zw
        ELSE
          zbw  => fld%grid%Z
        END IF
      END IF

      CALL SetZfac( zu,zbw,nzb,mv,0.,0. )

      IF( mv%km < 0 )THEN

        xfac = MIN(zu/zbw(-mv%km),1.0)

        IF( ui(is) >= 0. )THEN
          hx = fld%grid%terrain%Hx(im)
        ELSE
          hx = fld%grid%terrain%Hx(is)
        END IF

        IF( vi(is) >= 0. )THEN
          hy = fld%grid%terrain%Hy(i0m+i)
        ELSE
          hy = fld%grid%terrain%Hy(is)
        END IF

        IF( BTEST(fld%type,FTB_W) )THEN
          wp = xfac*w(is)
        ELSE
          wp = 0.
        END IF
        wi(is)= wp + (1.-xfac)*(hx*ui(is) + hy*vi(is))

      END IF

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

END IF

dt_advx = dt_adv

!$OMP PARALLEL SHARED(fld,nxb,nyb,dxr,dyr,dt_advx,umax,vmax)
!$OMP DO PRIVATE(i,j,i0,is,xb,yb,dtx,dty,xfac,yfac)
DO j = 1,nyb
  i0 = (j-1)*nxb
  yb  = fld%grid%Ymin + FLOAT(j-1)*fld%grid%dY
  DO i = 1,nxb
    is = i0 + i
    xb = fld%grid%Xmin + FLOAT(i-1)*fld%grid%dX
    CALL SWIMmapfac( fld%grid%coord,xb,yb,xfac,yfac )
    dxr(is) = xfac/fld%grid%dX
    dyr(is) = yfac/fld%grid%dY
    dtx     = 0.25/(dxr(is)*umax+1.E-10)
    dty     = 0.25/(dyr(is)*vmax+1.E-10)
    dt_advx(j)  = MIN(dtx,dty,dt_advx(j))
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

dt_adv = MINVAL(dt_advx)

IF( ALLOCATED(dt_advx) )DEALLOCATE( dt_advx,STAT=i )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE AdvZi( i,j,fld,ziOld,ui,vi,wi,dxr,dyr,dt )

USE SWIM_fi

IMPLICIT NONE

INTEGER,                       INTENT( IN    ) :: i, j
TYPE( MetField ),              INTENT( INOUT ) :: fld
REAL, DIMENSION(fld%grid%nXY), INTENT( IN    ) :: ziOld
REAL,                          INTENT( IN    ) :: ui,vi, wi
REAL,                          INTENT( IN    ) :: dxr, dyr
REAL,                          INTENT( IN    ) :: dt

INTEGER nx, ny, i0, i0p, ip, jp, is, jm, im, i0m
REAL    cx, cy

REAL, PARAMETER :: ZI_LIM = 10.

REAL, DIMENSION(:), POINTER :: zi, H

IF( fld%grid%nXY == 1 )RETURN

nx = fld%grid%nX
ny = fld%grid%nY

zi => fld%BL%zi
H  => fld%grid%terrain%H

i0 = (j-1)*nx
is = i0 + i

cx = ABS(ui)*dt*dxr
cy = ABS(vi)*dt*dyr

IF( ui > 0. )THEN
  im = MAX(i-1,1)
  zi(is) = zi(is) - cx*(ziOld(is)-ziOld(i0+im)+H(is)-H(i0+im))
ELSE
  ip = MIN(i+1,nx)
  zi(is) = zi(is) - cx*(ziOld(is)-ziOld(i0+ip)+H(is)-H(i0+ip))
END IF

IF( vi > 0. )THEN
  jm  = MAX(j-1,1)
  i0m = (jm-1)*nx
  zi(is) = zi(is) - cy*(ziOld(is)-ziOld(i0m+i)+H(is)-H(i0m+i))
ELSE
  jp  = MIN(j+1,ny)
  i0p = (jp-1)*nx
  zi(is) = zi(is) - cy*(ziOld(is)-ziOld(i0p+i)+H(is)-H(i0p+i))
END IF

zi(is) = MAX(zi(is) + wi*dt,ZI_LIM)

!------ Prevent zi from going below twice canopy height (or equivalent roughness)

zi(is) = MAX(zi(is),2.*fld%grid%landcover%canopyHt(is),20.*fld%grid%landcover%roughness(is) )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE trans_z( z,zu,i,j,grid )

USE SWIMmetField_fd
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN  ) :: grid
REAL,            INTENT( IN  ) :: z
INTEGER,         INTENT( IN  ) :: i, j
REAL,            INTENT( OUT ) :: zu

INTEGER ip

ip = (j-1)*grid%nX + i
IF( BTEST(grid%type,GTB_Z3D) )THEN
  zu = z
ELSE
  zu = z*grid%Ztop/(grid%Ztop-grid%terrain%H(ip)) !z is relative to local terrain height
END IF

RETURN
END
