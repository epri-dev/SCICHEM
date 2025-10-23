INTEGER FUNCTION SWIMobsSrfLayer( First,fld )

!------ Set or compute surface layer parameters for Obs

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( IN ) :: First
TYPE( MetField ), INTENT( IN ) :: fld

TYPE( ObsMet ), POINTER :: Obs

INTEGER jul, k, nz, alloc_stat, i
REAL    tLocal, tSolar, tUTC, zi, hflux, ziSBL, hfluxSBL, unit_vel, lat, lon, xlon
REAL    hobs, zruf, hc, alpha, albedo, bowen, cc, rn, mol, ufac, wt, fsl, f
REAL    zsl, usl, vsl, zref, wspd, tref, pref, ustar, tz_us, ztem
REAL    zm, tm, pm, dtdz
LOGICAL lSetZi, FixedHeatFlux, FixedZi, GotLL, lustar

INTEGER, EXTERNAL :: JulianPrj, SWIMgetLL
REAL,    EXTERNAL :: ulog, LocalTime, UTCTime, SolarTime, ComputeNetRadiation
REAL,    EXTERNAL :: StndLogP

SWIMobsSrfLayer = SWIMfailure

!------ Check if any new obs

IF( First%numObs == 0 )THEN
  SWIMobsSrfLayer = SWIMsuccess
  GOTO 9999
END IF

!------ Check for No Boundary Layer

IF( fld%BLtype == BLP_NONE )THEN  !If no BL, set zsl = 0 for all obs

  Obs => First%obs
  DO WHILE( ASSOCIATED(Obs) )
    Obs%BL%zsl = 0.
    Obs => Obs%NextObs
  END DO

  SWIMobsSrfLayer = SWIMsuccess
  GOTO 9999

END IF

!------ Set locat, UTC time and Julian day

tLocal = LocalTime( First%time )
tUTC   = UTCtime( First%time )
jul    = JulianPrj( First%time )

!------ Set depth and heat flux for Simple BL

IF( fld%BLtype == BLP_SBL )THEN
  IF( tLocal == NOT_SET_R )THEN
    IF( ASSOCIATED(fld%grid%lon) )THEN
      xlon = 0.5*(fld%grid%lon(1)+fld%grid%lon(fld%grid%nXY))
    ELSE
      xlon = 0.
    END IF
    tLocal = SolarTime( tUTC,xlon )
  END IF
  CALL SetSimpleBL( tLocal,ziSBL,hfluxSBL )
END IF

!------ Loop over linked-list

Obs => First%obs

DO WHILE( ASSOCIATED(Obs) )

  NumObsGTzero : IF( Obs%Vel%nz > 0 )THEN

!------ Set local variables

    hobs   = Obs%BL%h
    zruf   = Obs%BL%surface%zruf
    hc     = Obs%BL%surface%hc
    alpha  = Obs%BL%surface%alpha
    albedo = Obs%BL%surface%albedo
    bowen  = Obs%BL%surface%bowen

!------ Define reference height, speed, etc. from lowest Obs level

    zref = Obs%Vel%z(1) - hobs
    usl  = Obs%Vel%U(1)
    vsl  = Obs%Vel%V(1)

    wspd = MAX( usl**2+vsl**2,Prj%BL%WWtrop,1.E-6 )

    IF( zref > MAX(2.*hc,20.*zruf) )THEN
      ufac = 1.0
    ELSE
      ufac = ulog(hc,alpha,zref,zruf,0.) / ulog(hc,alpha,MAX(2.*hc,20.*zruf),zruf,0.)
    END IF

    wspd = SQRT(MAX(wspd,Prj%BL%UUcalm*ufac**2))

    ztem = hobs+fld%grid%Hmin
    CALL stnd_atmos( ztem,pref,tref,tz_us,SA_TPOT )

    IF( BTEST(First%type,OTB_T) )THEN
      IF( Obs%Tpot%nz > 0 )THEN
        IF( Obs%Tpot%z(1) - hobs < ZLIM2 )THEN
          tref = Obs%Tpot%obs(1)
        END IF
      END IF
    END IF

    IF( BTEST(First%type,OTB_P) )THEN
      IF( Obs%Press%nz > 0 )THEN
        IF( Obs%Press%z(1) - hobs < ZLIM2 )THEN
          pref = EXP(Obs%Press%obs(1))
        END IF
      END IF
    END IF

    tref = tref*pref**KAPPA  !Convert to actual temperature
    pref = pref*PSURF        !Convert to millibars

    IF( Obs%varSrf%cloudcover /= OBP_NODATA )THEN
      cc = Obs%varSrf%cloudcover
    ELSE
      cc = Prj%BL%cc
    END IF

!------ Surface layer is completely defined if given M-O Length

    IF( Obs%varSrf%invL /= OBP_NODATA )THEN

      IF(  Obs%varSrf%zi == OBP_NODATA )THEN !Set Zi based on L if no obs
        lSetZI = .TRUE.
      ELSE
        lSetZI = .FALSE.
        zi = Obs%varSrf%zi
      END IF

      ustar = wspd**2
      CALL SetBLfromMOL( Obs%varSrf%invL,zruf,hc,alpha,lSetZi,mol,zi,zsl,zref,ustar )

      ustar = SQRT(ustar)
      hflux = -ustar**3*tref/(VONK*mol*G0)

    ELSE

!------ Otherwise, set parameters based on obs or calculation

!------ Get lat/lon if required for heat flux or BL depth

      lat = -999.
      IF( (Obs%varSrf%hflux == OBP_NODATA .OR. &
           Obs%varSrf%zi    == OBP_NODATA) .AND. &
           fld%BLtype /= BLP_SBL )THEN
        IF( SWIMgetLL(Obs%x,Obs%y,lon,lat) == SWIMsuccess )THEN
          GotLL = .TRUE.
        ELSE
          GotLL = .FALSE.
        END IF
      END IF

!------ Set heat flux

      rn = -1.
      IF( Obs%varSrf%hflux == OBP_NODATA )THEN
        IF( fld%BLtype == BLP_SBL )THEN
          hflux = hfluxSBL
          FixedHeatFlux = .TRUE.
        ELSE IF( GotLL )THEN
          IF( tUTC == NOT_SET_R )THEN
            tSolar = tLocal
          ELSE
            tSolar = SolarTime( tUTC,lon )
          END IF
          rn = ComputeNetRadiation( tSolar,jul,lat,tref,cc,albedo )
          hflux = rn / RHOCP
          FixedHeatFlux = .FALSE. !Heat flux will be computed in SurfIter
        ELSE
          hflux = 0.
          FixedHeatFlux = .FALSE. !Heat flux will be computed in SurfIter
        END IF
      ELSE
        hflux = Obs%varSrf%hflux
        FixedHeatFlux = .TRUE.
      END IF

!------ Set BL depth

      IF( Obs%varSrf%zi == OBP_NODATA )THEN
        IF( fld%BLtype == BLP_SBL )THEN
          zi      = ziSBL
          FixedZi = .TRUE.
        ELSE
         IF( hflux > 0.)THEN
            zi = 1000.
          ELSE
            zi = 100.
          END IF
          FixedZi = .NOT.GotLL  !Compute zi only if latitude given
        END IF
      ELSE
        zi      = Obs%varSrf%zi
        FixedZi = .TRUE.
      END IF

      IF( Obs%varSrf%ustr /= OBP_NODATA .AND. FixedHeatFlux )THEN
        lustar = .TRUE.
        ustar  = Obs%varSrf%ustr
      ELSE
        lustar = .FALSE.
        ustar  = NOT_SET_R
      END IF

!------ Compute surface layer depth (and heat flux & zi if appropriate)

      CALL SurfIter( zref,wspd,tref,pref,zruf,hc,alpha,bowen, &
                     cc,rn,lat,.NOT.FixedZi,FixedHeatFlux, &
                     hflux,zi,zsl,mol,ustar,lustar )

      IF( BTEST(First%type,OTB_UST) .AND. .NOT.lustar )THEN
        Obs%varSrf%ustr = ustar
      END IF

    END IF

!------ Extrapolate velocity to zsl if greater than zref; blend between
!       bracketing observations

    IF( zsl > zref )THEN

      k = 1               !Find heighest obs level below zsl
      DO
        IF( k+1 > Obs%Vel%nz )EXIT
        IF( Obs%Vel%z(k+1) - hobs > zsl )EXIT
        k = k + 1
      END DO

      zref = Obs%Vel%z(k) - hobs

      unit_vel = 1.
      CALL get_ubl( zref,unit_vel,mol,zruf,hc,alpha,zsl,ufac )

      usl = Obs%Vel%U(k)*ufac
      vsl = Obs%Vel%V(k)*ufac

      IF( k+1 <= Obs%Vel%nz )THEN
        wt  = (zsl+hobs-Obs%Vel%z(k)) / (Obs%Vel%z(k+1)-Obs%Vel%z(k)+SMALL)
        usl = (1.-wt)*usl + wt*Obs%Vel%U(k+1)
        vsl = (1.-wt)*vsl + wt*Obs%Vel%V(k+1)
      END IF

    END IF

!------ Set surface layer parameters in Obs structure

    Obs%BL%zsl   = zsl
    Obs%BL%usl   = usl
    Obs%BL%vsl   = vsl
    Obs%BL%L     = mol
    Obs%BL%hflux = hflux
    Obs%BL%zi    = zi
    Obs%BL%tref  = tref
    Obs%BL%pref  = pref
    Obs%BL%cc    = cc
    Obs%BL%uslx  = usl
    Obs%BL%vslx  = vsl

!------ Construct array of surface layer factors

    IF( ASSOCIATED(obs%SrfPrf%z) )THEN
      DEALLOCATE( obs%SrfPrf%z,obs%SrfPrf%fsl,STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMobsSrfLayer'
        error%Message = 'Error deallocating obs surface layer arrays'
        GOTO 9999
      END IF
    END IF

    nz = Obs%Vel%nz
    ALLOCATE( Obs%SrfPrf%z(nz),Obs%SrfPrf%fsl(nz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMobsSrfLayer'
      error%Message = 'Error allocating obs surface layer arrays'
      GOTO 9999
    END IF

    DO i = 1,nz
      Obs%SrfPrf%z(i) = Obs%Vel%z(i) - Obs%BL%h
    END DO

    fsl = 0.
    CALL set_fsl( zsl,zsl,hc,alpha,zruf,mol,f,fsl )
    Obs%SrfPrf%fsl = fsl

    k = 1
    DO
      IF( k > nz )EXIT
      IF( Obs%SrfPrf%z(k) > zsl )EXIT
      CALL set_fsl( Obs%SrfPrf%z(k),zsl,hc,alpha,zruf,mol,Obs%SrfPrf%fsl(k),fsl )
      k = k + 1
    END DO

  END IF NumObsGTzero

!------ Set standard atmosphere temperature and pressure
!       at top/bottom levels (used for extrapolation)

  IF( Obs%Tpot%nz > 0 )THEN
    zm = Obs%Tpot%z(1) + fld%grid%Hmin
    CALL stnd_atmos( zm,pm,tm,dtdz,SA_TPOT )
    Obs%Stnd%Tbot = tm
    zm = Obs%Tpot%z(Obs%Tpot%nz) + fld%grid%Hmin
    CALL stnd_atmos( zm,pm,tm,dtdz,SA_TPOT )
    Obs%Stnd%Ttop = tm
  END IF

  IF( Obs%Press%nz > 0 )THEN
    zm = Obs%Press%z(1) + fld%grid%Hmin
    Obs%Stnd%LogPbot = StndLogP( zm )
    zm = Obs%Press%z(Obs%Press%nz) + fld%grid%Hmin
    Obs%Stnd%LogPtop = StndLogP( zm )
  END IF

!------ Point to next Obs

  Obs => Obs%NextObs

END DO

SWIMobsSrfLayer = SWIMsuccess

9999 CONTINUE

RETURN
END



