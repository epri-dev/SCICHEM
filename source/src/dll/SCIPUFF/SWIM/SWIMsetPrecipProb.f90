MODULE PrecipConstants_fd

  USE constants_fd

  REAL, PARAMETER :: SCLHT    = RGAS*256./G0 ! 256 K is avg. trop. temp. from USSA.
  REAL, PARAMETER :: THTECON1 = 3376. ! K
  REAL, PARAMETER :: THTECON2 = 2.54
  REAL, PARAMETER :: THTECON3 = 0.81
  REAL, PARAMETER :: TLCLC1   = 2840.
  REAL, PARAMETER :: TLCLC2   = 3.5
  REAL, PARAMETER :: TLCLC3   = 4.805
  REAL, PARAMETER :: TLCLC4   = 55.
  REAL, PARAMETER :: GAMMA    = RGAS/CP
  REAL, PARAMETER :: CPMD     =.887          !Cp_moist=cp*(1.+cpmd*q)
  REAL, PARAMETER :: RGASMD   = 0.608        !Rgas_moist=rgas*(1.+rgasmd*qvp)
  REAL, PARAMETER :: GAMMAMD  = RGASMD-CPMD  !Gamma_moist=gamma*(1.+gammamd*qvp)
  REAL, PARAMETER :: EPS      = 0.622
  REAL, PARAMETER :: EZERO    = 6.112
  REAL, PARAMETER :: XLHC     = 2.5e6
  REAL, PARAMETER :: ESLCON1  = 17.67
  REAL, PARAMETER :: ESLCON2  = 29.65

END MODULE PrecipConstants_fd

!==============================================================================

INTEGER FUNCTION SWIMsetPrecipProb( fld )

!------ Compute probability of rain, snow or frozen precipitation
!       Based on routines provided by Penn State

!------ N.B. works on "next" fields on the assumption that precip. rate, etc. have just
!       been read in

USE SWIM_fi
USE SWIMparam_fd
USE PrecipConstants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER, PARAMETER :: MKZ = 87, NUOR = 12
REAL,    PARAMETER :: BL = 20.19, BF = 63.35, BI = 16.46

REAL, PARAMETER, DIMENSION(NUOR) :: CX = (/ -1.,-6.,150.,300.,750.,300.,550.,1200.,50.,50.,800.,4000. /)
REAL, PARAMETER, DIMENSION(NUOR) :: AL = (/ -17.93,  -5.83, -28.48, -30.27, -16.04, +34.90, &
                                            -6.38,  +6.55, +23.83, +20.95,  -5.88,  +5.04 /)
REAL, PARAMETER, DIMENSION(NUOR) :: AF = (/ +7.99,  -6.82,  +4.88,  -4.56,  -0.13, +18.72, &
                                           -22.35, -13.84, -27.23, -25.82, -15.60, +22.96 /)
REAL, PARAMETER, DIMENSION(NUOR) :: AI = (/ +9.94, +12.65, +23.60, +34.83, +16.17, -53.62, &
                                            +28.73,  +7.29,  +3.40,  +4.87, +21.49, -28.01/)

REAL, PARAMETER, DIMENSION(MKZ) :: zz = (/   0., 10., 20., 30., 40., 50., 60., 70., 80., 90., &
                                           100.,110.,120.,130.,140.,150.,160.,170.,180.,190., &
                                           200.,250.,300.,350.,400.,450.,500.,550.,600.,650., &
                                           700.,750.,800.,850.,900.,950.,                     &
                                          1000.,1110.,1200.,1300.,1400.,1500.,1600.,1700.,1800.,1900., &
                                          2000.,2100.,2200.,2300.,2400.,2500.,2600.,2700.,2800.,2900., &
                                          3000.,3100.,3200.,3300.,3400.,3500.,3600.,3700.,3800.,3900., &
                                          4000.,4100.,4200.,4300.,4400.,4500.,4600.,4700.,4800.,4900., &
                                          5000.,5100.,5200.,5300.,5400.,5500.,5600.,5700.,5800.,5900.,6000.  /)

INTEGER nx, ny, nxy, nz, alloc_stat
INTEGER i, j, k, i0, is, ip, kz, ks, lwarmw
REAL    pr, press, tb, ztmp, pr1, dtdz, q, hsx, e, tlcl, eth
REAL    zlev, zlevx, vcp0, vcp1, valp0, valp1
REAL    dtdzlow, dtwbdzlow, del_tmpz, del_twbz
REAL    tsrfto10, t5to25, dzwarm, dawarm, dzcoldw, dacoldw, zr, prbtot
LOGICAL lstagger

REAL, DIMENSION(3)    :: prcprb
REAL, DIMENSION(NUOR) :: lfi, cxr

REAL, DIMENSION(:), ALLOCATABLE :: ghts, ghxs, tmps, twbs
REAL, DIMENSION(:), ALLOCATABLE :: zzh, dz, tmpz, twbz

REAL, DIMENSION(:), POINTER :: Tpot, P, H, Rain

REAL, EXTERNAL :: SWIMrlimit, tonpsadiabat
REAL, EXTERNAL :: StndHumid

SWIMsetPrecipProb = SWIMfailure

Tpot => fld%NextField%Tpot
P    => fld%NextField%Press
H    => fld%NextField%Humid
Rain => fld%NextBL%rainprb

nx  = fld%grid%nX
ny  = fld%grid%nY
nz  = fld%grid%nZ
nxy = fld%grid%nXY

ALLOCATE( ghts(nz),ghxs(nz),tmps(nz),twbs(nz), &
          zzh(MKZ-1),dz(MKZ-1),tmpz(MKZ-1),twbz(MKZ-1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMsetPrecipProb'
  error%Message = 'Error allocating profile arrays for precipitation calculation'
  GOTO 9999
END IF

DO k = 1,MKZ-1
  zzh(k) = (zz(k+1)+zz(k))/2.
  dz(k)  = zz(k+1)-zz(k)
END DO

lstagger = BTEST( fld%grid%type,GTB_STAGGERZ )

!------ Loop over horizontal grid

DO j = 1,ny
  i0 = (j-1)*nx

  DO i = 1,nx
    is = i0 + i

!------ Construct vertical profiles from gridded fields

    DO k = 1,nz
      ip  = (k-1)*nxy + is

!------ Height

      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        ghts(k) = fld%grid%sigma%Z(ip)
      ELSE
        ghts(k) = fld%grid%Z(k)*fld%grid%terrain%D(is)
      END IF
      ghts(k) = ghts(k) + fld%grid%terrain%H(is) + fld%grid%Hmin
      ghxs(k) = EXP(-ghts(k)/SCLHT)

!------ Pressure

      IF( BTEST(fld%type,FTB_P) )THEN
        IF( lstagger )THEN
          pr = P(ip+nxy)
        ELSE
          pr = P(ip)
        END IF
        pr = EXP(pr)
      ELSE
        CALL stnd_atmos( ghts(k),pr,tb,dtdz,SA_TA )
      END IF
      press = pr * PSURF

!------ Temperature

      IF( BTEST(fld%type,FTB_T) )THEN
        IF( lstagger )THEN
          tb = Tpot(ip+nxy)
        ELSE
          tb = Tpot(ip)
        END IF
        tb = tb * pr**KAPPA
      ELSE IF( BTEST(fld%type,FTB_P) )THEN   !Otherwise, already called stnd_atmos
        CALL stnd_atmos( ztmp,pr1,tb,dtdz,SA_TA )
      END IF

      tmps(k) = tb !Absolute temperature (K)

!------ Humidity mixing ratio (g/g)

      IF( BTEST(fld%type,FTB_H) )THEN
        IF( lstagger )THEN
          q = H(ip+nxy)
        ELSE
          q = H(ip)
        END IF
        CALL HumidMixRatio( q,tb,press,hsx )
        q = hsx
      ELSE
        q = StndHumid( press,tb )
      END IF
      q = MAX(q,SMALL)

!------ Wetbulb temperature (C)

      e       = q*press / (EPS+q)
      tlcl    = TLCLC1/(LOG(tmps(k)**TLCLC2/e)-TLCLC3)+TLCLC4
      eth     = tmps(k)*(1000./press)**(GAMMA*(1.+GAMMAMD*q))*EXP((THTECON1/tlcl-THTECON2)*q*(1.+THTECON3*q))
      twbs(k) = tonpsadiabat( eth,press ) + ABSZERO  !Celsius
      tmps(k) = tmps(k) + ABSZERO !Convert to Celsius

    END DO

!------ Calculate low-level lapse rates

    dtdzlow   = (tmps(2)-tmps(1))/(ghts(2)-ghts(1))
    dtwbdzlow = (twbs(2)-twbs(1))/(ghts(2)-ghts(1))

!------ Set for extrapolating above top level

    CALL stnd_atmos( ghts(nz),pr1,tb,dtdz,SA_TA )
    del_tmpz = tmps(nz) - tb
    press    = pr1*PSURF
    e        = q*press / (EPS+q)
    tlcl     = TLCLC1/(LOG(tb**TLCLC2/e)-TLCLC3)+TLCLC4
    eth      = tb*(1000./press)**(GAMMA*(1.+GAMMAMD*q))*EXP((THTECON1/TLCL-THTECON2)*q*(1.+THTECON3*q))
    del_twbz = twbs(nz) - tonpsadiabat( eth,press )

!------ Interpolate tmp & twb to pre-defined height levels.

    ks = 1

    kzLoop : DO kz = 1,MKZ-1
      zlev  = zzh(kz) + fld%grid%terrain%H(is) + fld%grid%Hmin
      zlevx = EXP(-(zlev)/SCLHT)

      IF( zlev <= ghts(1) )THEN         !Extrapolate below 1st level using local gradients
        tmpz(kz) = tmps(1) + dtdzlow*(ghts(1)-zlev)
        twbz(kz) = MIN( twbs(1)+dtwbdzlow*(ghts(1)-zlev), tmpz(kz) )
        CYCLE kzLoop

      ELSE IF( zlev >= ghts(nz) )THEN   !Extrapolate above top level with standard atmosphere
        CALL stnd_atmos( zlev,pr1,tb,dtdz,SA_TA )
        tmpz(kz) = del_tmpz + tb
        press    = pr1*PSURF
        e        = q*press / (eps+q)
        tlcl     = TLCLC1/(LOG(tb**TLCLC2/e)-TLCLC3)+TLCLC4
        eth      = tb*(1000./press)**(GAMMA*(1.+GAMMAMD*q))*EXP((THTECON1/TLCL-THTECON2)*q*(1.+THTECON3*q))
        twbz(kz) = del_twbz + tonpsadiabat( eth,press )
        CYCLE kzLoop

      END IF

      DO k = ks,nz-1
        vcp0 = ghxs(k)
        vcp1 = ghxs(k+1)
        IF( zlevx <= vcp0 .AND. zlevx >= vcp1 )THEN
          valp0    = tmps(k)
          valp1    = tmps(k+1)
          tmpz(kz) = (zlevx-vcp0)*(tmps(k+1)-tmps(k))/(vcp1-vcp0) + tmps(k)
          valp0    = twbs(k)
          valp1    = twbs(k+1)
          twbz(kz) = (zlevx-vcp0)*(twbs(k+1)-twbs(k))/(vcp1-vcp0) + twbs(k)
          ks = k
          CYCLE kzLoop
        END IF
      END DO

    END DO kzLoop

!------ Determine layer-average temperatures, warm-layer depth, warm-layer
!       area, cold-wet-bulb-layer depth, and cold-wet-bulb-layer area.

    tsrfto10 = 0.
    t5to25   = 0.
    dzwarm   = 0.
    dawarm   = 0.
    dzcoldw  = 0.
    dacoldw  = 0.
    lwarmw   = 0

    DO k = MKZ-1,1,-1
      IF( zzh(k) < 1000.)tsrfto10 = tsrfto10 + tmpz(k)*dz(k)
      IF( zzh(k) > 500. .AND. zzh(k) < 2500. )t5to25 = t5to25 + tmpz(k)*dz(k)
      IF( tmpz(k) > 0. )THEN
        dzwarm = dzwarm + dz(k)
        dawarm = dawarm + dz(k)*tmpz(k)
      END IF
      IF( twbz(k) <= 0. )THEN
        IF( lwarmw == 0 )THEN
          dzcoldw = dzcoldw + dz(k)
          dacoldw = dacoldw - dz(k)*twbz(k)
        END IF
      ELSE
        lwarmw = 1
      END IF
    END DO

    tsrfto10 =.0010*tsrfto10
    t5to25   =.0005*t5to25

    zr = 0
    IF( tmpz(MKZ-1) < 0. .AND. dzwarm > 0. )zr=1
    dzcoldw = dzcoldw*lwarmw
    dacoldw = dacoldw*lwarmw

!------ Assign values to predictors

    cxr(1)  = tsrfto10
    cxr(2)  = t5to25
    cxr(3)  = dzwarm
    cxr(4)  = dzwarm
    cxr(5)  = dawarm
    cxr(6)  = zr*dzwarm
    cxr(7)  = zr*dzwarm
    cxr(8)  = zr*dzwarm
    cxr(9)  = zr*dawarm
    cxr(10) = dzcoldw
    cxr(11) = dzcoldw
    cxr(12) = dacoldw

!------ Determine the lfi based on criteria for each predictor

    DO k = 1,NUOR
      lfi(k) = 0.
      IF( cxr(k) <= CX(k) )lfi(k) = 1.
    END DO

!------ Calculate the probabilities from the regression equations

    prcprb(1) = BL
    prcprb(2) = BF
    prcprb(3) = BI
    DO k = 1,nuor
      prcprb(1) = prcprb(1) + AL(k)*lfi(k)
      prcprb(2) = prcprb(2) + AF(k)*lfi(k)
      prcprb(3) = prcprb(3) + AI(k)*lfi(k)
    END DO

    prbtot = 0.
    DO k = 1,3
      prcprb(k) = SWIMrlimit(prcprb(k),0.,100.)
      prbtot    = prbtot + prcprb(k)
    END DO

    prcprb = 100./prbtot * prcprb

    Rain(is) = prcprb(1) + prcprb(2)  !Combine prob of rain and frozen

  END DO
END DO

SWIMsetPrecipProb = SWIMresult

9999 CONTINUE

IF( ALLOCATED(ghts) )DEALLOCATE(ghts,STAT=alloc_stat)
IF( ALLOCATED(ghxs) )DEALLOCATE(ghxs,STAT=alloc_stat)
IF( ALLOCATED(tmps) )DEALLOCATE(tmps,STAT=alloc_stat)
IF( ALLOCATED(twbs) )DEALLOCATE(twbs,STAT=alloc_stat)
IF( ALLOCATED(zzh)  )DEALLOCATE(zzh, STAT=alloc_stat)
IF( ALLOCATED(dz)   )DEALLOCATE(dz,  STAT=alloc_stat)
IF( ALLOCATED(tmpz) )DEALLOCATE(tmpz,STAT=alloc_stat)
IF( ALLOCATED(twbz) )DEALLOCATE(twbz,STAT=alloc_stat)

RETURN
END

!==============================================================================

REAL FUNCTION tonpsadiabat( thtei,prsi )

!   This function gives the temperature (in K) on a moist adiabat
!   (specified by thtei in K) given pressure in hPa.
!   Based on Bolton (1980) formula for theta_e.

USE SWIM_fi
USE PrecipConstants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: thtei, prsi

INTEGER nit
REAL    tdried, tup, tdn, tcheck, tnew, escheck, wscheck


REAL, EXTERNAL :: SWIMrlimit

tdried = thtei*(.001*prsi)**GAMMA
tup    = tdried

tdn = 100.
nit = 0

DO
  nit = nit+1
  tcheck  = 0.5*(tup+tdn)
  escheck = EZERO*EXP(eslcon1*(tcheck+ABSZERO)/(tcheck-eslcon2))
  wscheck = EPS*escheck/(prsi-escheck)
  tnew    = tdried*EXP(-XLHC*wscheck/(CP*tcheck))
  IF( ABS(tnew-tcheck) < 0.01)EXIT
  IF( nit == 100 )EXIT
  IF( tnew < tcheck )THEN
    tup = tcheck
    tdn = MAX(tdn,tnew)
  ELSE
    tdn = tcheck
    tup = MIN(tup,tnew)
  END IF
END DO

tonpsadiabat =.5*(tnew+tcheck)

RETURN
END
