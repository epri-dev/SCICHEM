INTEGER FUNCTION SWIMinterpObs( fld )

!------ Interpolate onto 3d grid using multiple obs sources

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMObsSort
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

REAL,    PARAMETER :: CORE_FAC2     = 0.5**2
REAL,    PARAMETER :: T_PERSISTENCE = 6.*3600.  !6 hour persistence time-scale

INTEGER iProgress, ixProgress, jyProgress, jObsProgress, jProgress, iProgress_last
INTEGER irv, jObs, iObs, jrv
INTEGER ix, jy, i0, ip, ip0, i, n
INTEGER nx, ny, nxy
REAL    xi, yi, xu, yv, xfac, yfac, hp, hu, hv
REAL    dx, dy, fac, zmax, ntot
REAL    x1, x2, y1, y2, area
LOGICAL lProf, lSort
LOGICAL lerr
LOGICAL lAssm, lter, lstagger, lPrevObs
REAL    u, v, Tpot, du2, obsTime
REAL    x0, y0, dp, zm, mapfac, gxp, gyp
REAL    rate, pr, dum, dum1, dum2, ztmp, pr1, pr2, t0, dtdz, rh
REAL    qc
INTEGER iter, nz, j, k, nzb, iList

TYPE( met1dh ) mx, mxu, mxv, my, myu, myv
TYPE( metv   ) mv

REAL, DIMENSION(:), POINTER :: zb, Z
REAL, DIMENSION(:), POINTER :: Tptr

TYPE( MetField ), POINTER :: fldp

TYPE( ObsInterp ), DIMENSION(:), ALLOCATABLE :: InterpPrf

TYPE( ObsMet ), POINTER :: Obs

TYPE( NearestObs ), DIMENSION(:,:), ALLOCATABLE :: NearObs

REAL, DIMENSION(:), ALLOCATABLE :: uPrev, vPrev, du2Max, u2Sum

REAL, DIMENSION(fld%nObsSource) :: PrevTimeWt

INTERFACE
  SUBROUTINE SetObsVertScale( obs,NearObs )
    USE SWIMobs_fd
    USE SWIMObsSort
    TYPE( ObsMet    ), POINTER      :: Obs
    TYPE( NearestObs), INTENT( IN ) :: NearObs
  END SUBROUTINE SetObsVertScale
END INTERFACE

INTEGER, EXTERNAL :: SWIMallocInterpPrf, SWIMdeallocInterpPrf, SWIMzeroInterpPrf, SWIMnullifyInterpPrf
INTEGER, EXTERNAL :: SWIMobsSrfLayer, SWIMsetObsWt, SWIMcombObsField
INTEGER, EXTERNAL :: NearestObsParam
INTEGER, EXTERNAL :: PostCheckMessage
INTEGER, EXTERNAL :: PostProgressMessage
INTEGER, EXTERNAL :: PostProgressBarMessage
REAL,    EXTERNAL :: SWIMrlimit
LOGICAL, EXTERNAL :: CheckInDomain
INTEGER, EXTERNAL :: SWIMinterpTimeAssm, SWIMcnvCoord, getFieldIndex
REAL,    EXTERNAL :: SetRate

SWIMinterpObs = SWIMfailure

jrv = PostCheckMessage()

!------ Set assimilation flag

lAssm = .NOT.BTEST(fld%type,FTB_OBS)

!------ Loop over obs and set surface layer parameters at obs locations

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  irv = SWIMobsSrfLayer( ObsSrc(jobs),fld )
  IF( irv /= SWIMsuccess )GOTO 9999
  jrv = PostCheckMessage()
END DO

!------ Define time based on earliest obs source

IF( lAssm )THEN
  fld%tNext = fld%t2
ELSE
  fld%tNext = HUGE(1.)
END IF
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( ObsSrc(jObs)%numObs > 0 )fld%tNext = MIN(fld%tNext,ObsSrc(jObs)%time)
END DO

IF( lAssm )THEN
  message%bString  = 'Assimilating observations into background meteorology'
ELSE
  message%bString  = 'Interpolating observation meteorology'
END IF
CALL SWIMtimeMessage( message%cString,fld%tNext )

jrv = PostProgressMessage( message )

!------ Interpolate background fields to next obs time and
!       initialize output fields with background

IF( lAssm )THEN

  rate = SetRate( fld%tNext,fld%t2,fld%t1 )
  irv = SWIMinterpTimeAssm( rate,fld )
  fld%t1 = fld%tNext

  CALL CopyFieldAssm( fld )

!------ Check for no obs; done if that's the case

  iObs = 0
  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    iObs = iObs + ObsSrc(jObs)%numObs + ObsSrc(jObs)%PrevNumObs
  END DO

  IF( iObs == 0 )THEN
    SWIMinterpObs = SWIMresult
    GOTO 9999
  END IF

END IF

!------ Use domain center to define overall map factors

xi = 0.5*(fld%grid%Xmin+fld%grid%Xmax)
yi = 0.5*(fld%grid%Ymin+fld%grid%Ymax)
CALL SWIMmapfac( fld%grid%coord,xi,yi,xfac,yfac )

!------ Set local grid variables

nx = fld%grid%nX;   ny = fld%grid%nY; nxy = fld%grid%nXY
dx = fld%grid%dX;   dy = fld%grid%dY
x0 = fld%grid%Xmin; y0 = fld%grid%Ymin

!------ Allocate interpolation profile structure

ALLOCATE( InterpPrf(ny),STAT=irv )

DO i = 1,ny

  irv = SWIMnullifyInterpPrf( InterpPrf(i) )
  IF( irv /= SWIMsuccess )GOTO 9999

  irv = SWIMallocInterpPrf( fld,InterpPrf(i) )
  IF( irv /= SWIMsuccess )GOTO 9999

  DO ix = 1,fld%grid%nZ
    InterpPrf(i)%z(ix) = fld%grid%z(ix)
  END DO
  InterpPrf(i)%Hmin = fld%grid%Hmin

  InterpPrf(i)%type = IBSET(0,OIB_UV)
  IF( BTEST(fld%type,FTB_LSV  ) )InterpPrf(i)%type = IBSET(InterpPrf(i)%type,OIB_LSV)
  IF( BTEST(fld%type,FTB_UU   ) )InterpPrf(i)%type = IBSET(InterpPrf(i)%type,OIB_UU)

  IF( BTEST(fld%grid%type,GTB_STAGGER) )InterpPrf(i)%type = IBSET(InterpPrf(i)%type,OIB_STAGGER)

END DO

!------ Determine number of nearest obs used for interpolation

ALLOCATE( NearObs(numObsSrc,ny),STAT=irv )
IF( irv /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'SWIMinterpObs'
  error%Message = 'Error allocating nearest obs array'
  GOTO 9999
END IF

jrv = PostCheckMessage()

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  NearObs(jObs,1)%numInterp = NearestObsParam( ObsSrc(jObs),Prj%maxObs )
  DO jy = 1,ny
    NearObs(jObs,jy)%numInterp = NearObs(jObs,1)%numInterp
    NULLIFY(NearObs(jObs,jy)%obs)
  END DO
  IF( BTEST(ObsSrc(jObs)%type,OTB_RIFL) )InterpPrf(1)%type = IBSET(InterpPrf(1)%type,OIB_RIFL)
END DO

IF( BTEST(InterpPrf(1)%type,OIB_RIFL) )THEN
  DO jy = 1,ny
    InterpPrf(jy)%type = IBSET(InterpPrf(jy)%type,OIB_RIFL)
    DO iObs = 1,numObsSrc
      NULLIFY(NearObs(iObs,jy)%obs)
    END DO
  END DO
END IF

DO j = 1,fld%nObsSource
  jObs = fld%iObsSource(j)
  IF( ASSOCIATED(ObsSrc(jObs)%GridList)) THEN
    IF( ObsSrc(jObs)%numObs > 0 .OR. ObsSrc(jObs)%PrevNumObs > 0 )THEN
       CALL SWIMsetObsInterpVar( InterpPrf(1),ObsSrc(jObs) )
    END IF
  END IF
END DO

!------ Set time weight for previous Obs lists; set max obs level and weighted total obs

zmax  = -999.
lProf = .FALSE.
ntot  = 0.

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)

  jrv = PostCheckMessage()

  zmax  = MAX(zmax,ObsSrc(jObs)%Zmax)

  IF( BTEST(ObsSrc(jObs)%type,OTB_PRF) )lProf = .TRUE.  !Used only in SWIMcombObsField for temperature

  IF( ObsSrc(jObs)%PrevNumObs > 0 )THEN

    IF( ObsSrc(jObs)%numObs > 0 )THEN
      IF( ObsSrc(jObs)%time-fld%tNext >  0.05*(ObsSrc(jObs)%time-ObsSrc(jObs)%PrevTime) )THEN
        fac = (ObsSrc(jObs)%time-fld%tNext)/(ObsSrc(jObs)%time-ObsSrc(jObs)%PrevTime)
        PrevTimeWt(i) = SWIMrlimit(fac,0.,1.)
      ELSE
        PrevTimeWt(i) = 0.
      END IF
    ELSE
      PrevTimeWt(i) = EXP(-ABS(fld%tNext-ObsSrc(jObs)%PrevTime)/T_PERSISTENCE)
    END IF

    zmax = MAX(zmax,ObsSrc(jObs)%PrevZmax)

  ELSE

    PrevTimeWt(i) = 0.

  END IF

  ntot = ntot + (1.- PrevTimeWt(i))*ObsSrc(jObs)%numObsDom &
                   + PrevTimeWt(i) *ObsSrc(jObs)%PrevNumObsDom

END DO

ntot = MAX(ntot,1.) !If there are no velocity obs from current or previous time,
                    !influence area will be entire domain
DO i = 1,ny
  InterpPrf(i)%zmax = zmax
END DO

!------ Save original observations for assimilation scheme
!       and setup interpolation factors

IF( lAssm )THEN

!------ Allocate arrays for convergence test

  ALLOCATE( uPrev(fld%grid%nZ),vPrev(fld%grid%nZ),du2Max(fld%grid%nZ),u2Sum(fld%grid%nZ),STAT=irv )
  IF( irv /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinterpObs'
    error%Message = 'Error allocating convergence test arrays'
    GOTO 9999
  END IF

!------ Preliminary interpolation setup

  lter = BTEST(fld%grid%type,GTB_TERRAIN)

  nzb = fld%grid%nZ
  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
    NULLIFY( zb )
    ALLOCATE( zb(nzb),STAT=irv )
    IF( irv /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinterpObs'
      error%Message = 'Error allocating height array for sigma coordinates'
      GOTO 9999
    END IF
  ELSE
    zb  => fld%grid%Z
  END IF

  mapfac = 1. !Not used

!------ Loop over obs sources

  ObsSrc1 : DO i = 1,fld%nObsSource
    iObs = fld%iObsSource(i)

    DO iList = 1,2  !Loop over current and previous (if any) linked-lists

      SELECT CASE( iList )
        CASE( 1 )
          IF( ObsSrc(iObs)%numObs == 0 )CYCLE
          Obs => ObsSrc(iObs)%obs
        CASE( 2 )
          IF( PrevTimeWt(i) <= 0. )CYCLE ObsSrc1
          Obs => ObsSrc(iObs)%PrevObs
      END SELECT

      DO WHILE( ASSOCIATED(Obs) )

!------ Horizontal interpolation factors

        xi = Obs%x
        yi = Obs%y

        CALL get_1dfac( xi,x0,dx,nx,mapfac,mx )
        CALL get_1dfac( yi,y0,dy,ny,mapfac,my )
        IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
          CALL get_1dfac( xi,x0+dx/2.,dx,nx,mapfac,mxu )
          CALL get_1dfac( yi,y0+dy/2.,dy,ny,mapfac,myv )
          IF( BTEST(fld%grid%type,GTB_STAGGERB) )THEN
            mxv = mxu; myu = myv
          ELSE
            mxv = mx; myu = my
          END IF
        ELSE
          mxu = mx; mxv = mx
          myu = my; myv = my
        END IF

        CALL SetMXY( mx, my, Obs%mxy, nx,nxy,lter )
        CALL SetMXY( mxu,myu,Obs%mxyu,nx,nxy,lter )
        CALL SetMXY( mxv,myv,Obs%mxyv,nx,nxy,lter )

!------ Setup vertical grid levels

!------ Interpolate vertical grid for sigma coordinate

        IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
          DO k = 1,nzb
            ip =  (k-1)*nxy + 1
            Z => fld%grid%sigma%Z(ip:); CALL IntXY( Obs%mxy,Z,zb(k) )
          END DO
        END IF

        IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
          dp = 1.
        ELSE
          dp = 1. - Obs%BL%h/fld%grid%Ztop
        END IF

!------ Loop over vertical levels for each type of measurement (assimilation fields only)

        nz = Obs%Vel%nz
        Obs%VelObs%nz = nz
        IF( nz > 0 )THEN
          ALLOCATE( Obs%VelObs%z(nz),Obs%VelObs%u(nz),Obs%VelObs%v(nz),STAT=irv )
          ALLOCATE( Obs%mzVel(nz),STAT=irv )
          DO k = 1,nz
            Obs%VelObs%z(k) = Obs%Vel%z(k)
            Obs%VelObs%u(k) = Obs%Vel%u(k)
            Obs%VelObs%v(k) = Obs%Vel%v(k)
            zm = (Obs%Vel%z(k) - Obs%BL%h)/dp
            CALL SetZfac( zm,zb,nzb,Obs%mzVel(k),0.,0. )
          END DO
        ELSE
          NULLIFY( Obs%VelObs%z,Obs%VelObs%u,Obs%VelObs%v )
        END IF

        IF( BTEST(fld%type,FTB_P) )THEN
          nz = Obs%Press%nz
          Obs%PressObs%nz = nz
          IF( nz > 0 )THEN
            ALLOCATE( Obs%PressObs%z(nz),Obs%PressObs%obs(nz),STAT=irv )
            ALLOCATE( Obs%mzPress(nz),STAT=irv )
            DO k = 1,nz
              Obs%PressObs%z(k)   = Obs%Press%z(k)
              Obs%PressObs%obs(k) = Obs%Press%obs(k)
              zm = (Obs%Press%z(k) - Obs%BL%h)/dp
              CALL SetZfac( zm,zb,nzb,Obs%mzPress(k),0.,0. )
            END DO
          ELSE
            NULLIFY( Obs%PressObs%z,Obs%PressObs%obs )
          END IF
        END IF

        IF( BTEST(fld%type,FTB_T) )THEN
          nz = Obs%Tpot%nz
          Obs%TpotObs%nz = nz
          IF( nz > 0 )THEN
            ALLOCATE( Obs%TpotObs%z(nz),Obs%TpotObs%obs(nz),STAT=irv )
            ALLOCATE( Obs%mzTpot(nz),STAT=irv )
            IF( .NOT.BTEST(fld%type,FTB_P) )THEN
              Tptr => Obs%Tpot%obs; Obs%Tpot%obs => Obs%TpotStA%obs; Obs%TpotStA%obs => Tptr
            END IF
            DO k = 1,nz
              Obs%TpotObs%z(k)   = Obs%Tpot%z(k)
              Obs%TpotObs%obs(k) = Obs%Tpot%obs(k)
              zm = (Obs%Tpot%z(k) - Obs%BL%h)/dp
              CALL SetZfac( zm,zb,nzb,Obs%mzTpot(k),0.,0. )
            END DO
          ELSE
            NULLIFY( Obs%TpotObs%z,Obs%TpotObs%obs )
          END IF
        END IF

        IF( BTEST(fld%type,FTB_H) )THEN
          nz = Obs%Humid%nz
          Obs%HumidObs%nz = nz
          IF( nz > 0 )THEN
            ALLOCATE( Obs%HumidObs%z(nz),Obs%HumidObs%obs(nz),STAT=irv )
            ALLOCATE( Obs%mzHumid(nz),STAT=irv )
            DO k = 1,nz
              Obs%HumidObs%z(k)   = Obs%Humid%z(k)
              Obs%HumidObs%obs(k) = Obs%Humid%obs(k)
              zm  = (Obs%Humid%z(k) - Obs%BL%h)/dp
              CALL SetZfac( zm,zb,nzb,Obs%mzHumid(k),0.,0. )
            END DO
          ELSE
            NULLIFY( Obs%HumidObs%z,Obs%HumidObs%obs )
          END IF
        END IF

        IF( BTEST(fld%type,FTB_QCLD) )THEN
          nz = Obs%Qcloud%nz
          Obs%QcloudObs%nz = nz
          IF( nz > 0 )THEN
            ALLOCATE( Obs%QcloudObs%z(nz),Obs%QcloudObs%obs(nz),STAT=irv )
            ALLOCATE( Obs%mzQcloud(nz),STAT=irv )
            DO k = 1,nz
              Obs%QcloudObs%z(k)   = Obs%Qcloud%z(k)
              Obs%QcloudObs%obs(k) = Obs%Qcloud%obs(k)
              zm  = (Obs%Qcloud%z(k) - Obs%BL%h)/dp
              CALL SetZfac( zm,zb,nzb,Obs%mzQcloud(k),0.,0. )
            END DO
          ELSE
            NULLIFY( Obs%QcloudObs%z,Obs%QcloudObs%obs )
          END IF
        END IF

        Obs => Obs%NextObs

      END DO

    END DO
  END DO ObsSrc1

END IF

!------ Estimate ratio of background error variance to obs appropriate to assimilation grid size

IF( lAssm )THEN

  area = dx*dy/(xfac*yfac)                       !Asssimilation grid cell area

  IF( BTEST(fld%gridSource%type,GSB_NEST) )THEN
    fldp => field(fld%gridSource%unit)           !Background field
  ELSE
    fldp => field(getFieldIndex(fld%index))      !Point to self if not a nest
  END IF
  xi = 0.5*(fldp%grid%Xmin+fldp%grid%Xmax)
  yi = 0.5*(fldp%grid%Ymin+fldp%grid%Ymax)
  CALL SWIMmapfac( fldp%grid%coord,xi,yi,x2,y2 )
  fac  = (fldp%grid%dX*fldp%grid%dY)/(x2*y2)     !Background field grid cell area
  y1   = SQRT(MIN(area/fac, 1.))                 !Background/Assimilation grid size

  fac  = R_OBS**2 * PI                           !Obs influence area (m^2)  *** hard-coded ***
  x1   = SQRT(MIN(area/fac,1.))                  !Obs/Assimilation grid size

  fac  = 0.5*(1.+x1-y1)
!  E2BO = (1.-fac)*0.1 + fac*9.                  !Empirical function
!  E2BO = 9.                                      !"Standard value" from Kovalets, et al. (2004)

END IF

!------ Define "core" radius (to avoid singularities at obs locations)
!       based on grid size and total number of observations

IF( BTEST(InterpPrf(1)%type,OIB_RIFL) )THEN

  InterpPrf(1)%a2 = 0.

ELSE

  IF( lAssm )THEN
    area = MAX(4.*area,PI*R_OBS**2)  !Max of 4x assimilation cell area and assumed obs influence area
    InterpPrf(1)%a2 = 1./(CORE_FAC2*area)
  ELSE
    InterpPrf(1)%a2 = ntot*xfac*yfac/(CORE_FAC2*FLOAT(nx*ny)*dx*dy)
  END IF

  IF( ntot >= 2. .AND. .NOT.lAssm )THEN

    x1 = HUGE(0.); x2 = -HUGE(0.)
    y1 = HUGE(0.); y2 = -HUGE(0.)

    DO i = 1,fld%nObsSource
      jObs = fld%iObsSource(i)
      Obs => ObsSrc(jobs)%obs
      DO WHILE( ASSOCIATED(Obs) )
        IF( Obs%Vel%nz > 0 )THEN
          xi = Obs%x; yi = Obs%y
          IF( CheckInDomain(xi,yi,fld%grid) )THEN
            x1 = MIN(x1,xi); x2 = MAX(x2,xi)
            y1 = MIN(y1,yi); y2 = MAX(y2,yi)
          END IF
        END IF
        Obs => Obs%NextObs
      END DO
    END DO

    area = MIN( MAX((x2-x1)/xfac,(y2-y1)/yfac)**2,FLOAT(nx*ny)*dx*dy/(xfac*yfac) )

    IF( area > 0. )THEN

    fac  = dx*dy/(xfac*yfac)            !Grid cell area
    area = MAX(area,fac*0.02**2)        !Core radius at least 2% of grid length (roughly speaking)
    area = MIN( area,FLOAT(nx*ny)*fac ) !Limit to domain area
    InterpPrf(1)%a2 = ntot/(CORE_FAC2*area)
    ENDIF

  END IF

  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    Obs => ObsSrc(jobs)%obs
    DO WHILE( ASSOCIATED(Obs) )
      IF( .NOT.BTEST(InterpPrf(1)%type,OIB_RIFL) )Obs%a2 = InterpPrf(1)%a2
      Obs => Obs%NextObs
    END DO
  END DO

END IF

DO i = 1,ny
  InterpPrf(i)%a2 = InterpPrf(1)%a2
END DO

!------ Adjust vertical scale for extrapolation if other obs are nearby
!       Search for close neighbors using nearest obs algorithm

IF( fld%nObsSource > 1 .OR. .NOT.ANY(BTEST(ObsSrc(1)%type,(/OTB_FCST,OTB_ANLY/))) )THEN

  DO i = 1,fld%nObsSource
    iObs = fld%iObsSource(i)
    n = MAX(ObsSrc(iObs)%numObs,ObsSrc(iObs)%PrevNumObs)
    ALLOCATE( NearObs(i,1)%Obs(n),STAT=irv )
    IF( irv /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinterpObs'
      error%Message = 'Error allocating nearest obs list'
      GOTO 9999
    END IF
  END DO

  DO j = 1,fld%nObsSource
    jObs = fld%iObsSource(j)

    lPrevObs = .FALSE.
    obsTime  = ObsSrc(jobs)%time

    Obs => ObsSrc(jobs)%obs

    DO WHILE( ASSOCIATED(Obs) )

      IF( Obs%Vel%nz > 0 )THEN

        xi = Obs%x
        yi = Obs%y

        DO i = 1,fld%nObsSource
          iObs = fld%iObsSource(i)

          IF( ABS(ObsSrc(iObs)%time-obsTime) <=  0.5*ObsSrc(jobs)%timeBin )THEN
            lSort = ObsSrc(iObs)%numObs > 1
            irv = FindNearestList( xi,yi,Obs%xfac,Obs%yfac,1.0,ObsSrc(iObs)%obs, &
                                   NearObs(i,1),ObsSrc(iObs)%numObs,lSort )
            CALL SetObsVertScale( Obs,NearObs(i,1) )
          END IF

          IF( PrevTimeWt(i)*ObsSrc(iObs)%PrevNumObs > 0.2 .AND. &
              ABS(ObsSrc(iObs)%PrevTime-obsTime) <=  0.5*ObsSrc(jobs)%timeBin )THEN
            lSort = ObsSrc(iObs)%PrevNumObs > 1
            irv = FindNearestList( xi,yi,Obs%xfac,Obs%yfac,1.0,ObsSrc(iObs)%PrevObs, &
                                   NearObs(i,1),ObsSrc(iObs)%PrevNumObs,lSort )
            CALL SetObsVertScale( Obs,NearObs(i,1) )
          END IF

        END DO

      END IF

      Obs => Obs%NextObs

      IF( .NOT.ASSOCIATED(Obs) .AND. .NOT.lPrevObs )THEN
        IF( ObsSrc(jObs)%PrevNumObs > 0 )Obs => ObsSrc(jObs)%PrevObs
        lPrevObs = .TRUE.
        obsTime = ObsSrc(jObs)%PrevTime
      END IF

    END DO
  END DO

  DO iObs = 1,fld%nObsSource
    DEALLOCATE( NearObs(iObs,1)%Obs,STAT=irv ); NULLIFY( NearObs(iObs,1)%Obs )
  END DO

END IF

DO i = 1,ny
  DO iObs = 1,numObsSrc
    NearObs(iObs,i)%numObs = NearObs(iObs,1)%numObs
  END DO
END DO

!------ Assimilation iteration (exit after single iterations for non-assimilation)

SCMiteration : DO iter = 1,SCM_MAXITER

!------ Loop over horizontal grid and update fields
!       Skip first iteration if assimilating

  IF( lAssm )THEN
    IF( iter == 1 )GOTO 1000
    DO k = 1,fld%grid%nz
      u2Sum(k)  = 0.
      du2Max(k) = 0.
    END DO
  END IF

  jProgress = nx*ny*fld%nObsSource
  iProgress_last = 0
  lerr = .FALSE.

!$OMP PARALLEL SHARED(ny,nx,dx,dy,xfac,yfac,fld,InterpPrf,numObsSrc,ObsSrc,NearObs,PrevTimeWt,lProf,lAssm,u2Sum,du2Max,lerr)
!$OMP DO PRIVATE(jy,i0,yi,yv,ip0,ix,i,xi,hp,ip,hu,hv,xu,irv,j,jobs,k,xfac,yfac,uPrev,vPrev,du2)  REDUCTION(.OR.:lerr) REDUCTION( MAX:du2Max) REDUCTION(+:u2Sum)
jLoop : DO jy = 1,ny
  i0 = (jy-1)*nx
  yi = fld%grid%Ymin + FLOAT(jy-1)*dy

  IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
    yv  = yi + 0.5*dy
    ip0 = (MIN(jy+1,ny)-1)*nx
  ELSE
    yv = yi
  END IF

  iLoop : DO ix = 1,nx
    i  = i0 + ix

    xi = fld%grid%Xmin + FLOAT(ix-1)*dx

    hp = fld%grid%terrain%H(i)

    IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
      ip = i0 + MIN(ix+1,nx)
      hu = 0.5*(hp+fld%grid%terrain%H(ip))
      ip = ip0 + ix
      hv = 0.5*(hp+fld%grid%terrain%H(ip))
      xu = xi + 0.5*dx
    ELSE
      hu = hp
      hv = hp
      xu = xi
    END IF

!------ Set interpolation profile structure

    InterpPrf(jy)%x  = xi; InterpPrf(jy)%y  = yi
    InterpPrf(jy)%xu = xu; InterpPrf(jy)%yv = yv

    CALL SWIMmapfac( fld%grid%coord,xi,yi,xfac,yfac )

    InterpPrf(jy)%lExtrap = .NOT.lAssm

    IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
      DO k = 1,fld%grid%nZ
        ip = i + (k-1)*nxy
        InterpPrf(jy)%z(k) = fld%grid%sigma%Z(ip)
      END DO
    END IF

    InterpPrf(jy)%xfac = xfac; InterpPrf(jy)%yfac = yfac

    InterpPrf(jy)%h = hp; InterpPrf(jy)%hu = hu; InterpPrf(jy)%hv = hv

    InterpPrf(jy)%d  = fld%grid%terrain%D(i)
    InterpPrf(jy)%du = fld%grid%terrain%Du(i)
    InterpPrf(jy)%dv = fld%grid%terrain%Dv(i)

    InterpPrf(jy)%zruf  = fld%grid%landcover%roughness(i)
    InterpPrf(jy)%hc    = fld%grid%landcover%canopyHt(i)
    InterpPrf(jy)%alpha = fld%grid%landcover%alpha(i)

!------ Zero sums

    irv = SWIMzeroInterpPrf( fld,InterpPrf(jy) )

!------ Loop over obs sources, interpolate and sum

     ObsSrcLoop : DO j = 1,fld%nObsSource
       jObs = fld%iObsSource(j)

      IF( ObsSrc(jObs)%numObs > 0 .OR. ObsSrc(jObs)%PrevNumObs > 0 )THEN
        irv = SWIMsetObsWt( InterpPrf(jy),NearObs(j,jy),ObsSrc(jObs),PrevTimeWt(j) )
        lerr = lerr .OR. irv /= SWIMsuccess
        IF( lerr )CYCLE
      END IF

     END DO ObsSrcLoop

!------ Save previous u,v fields for convergence test

    IF( lAssm )THEN

      ip = i
      IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
        ip = i + fld%grid%nXY
      ELSE
        ip = i
      END IF

      DO k = 1,fld%grid%nz
        uPrev(k) = fld%NextField%U(ip+(k-1)*fld%grid%nXY)
        vPrev(k) = fld%NextField%V(ip+(k-1)*fld%grid%nXY)
        u2Sum(k) = u2Sum(k) + uPrev(k)**2 + vPrev(k)**2
      END DO

    END IF

!------ Combine interpolated observations

    irv = SWIMcombObsField( i,fld,InterpPrf(jy),lProf,lAssm )

!------ Save max velocity changes

    IF( lAssm )THEN
      DO k = 1,fld%grid%nz
        du2 = (uPrev(k) - fld%NextField%U(ip+(k-1)*fld%grid%nXY))**2 + &
              (vPrev(k) - fld%NextField%V(ip+(k-1)*fld%grid%nXY))**2
        du2Max(k) = MAX(du2Max(k),du2)
      END DO
    END IF

  END DO iLoop
END DO jLoop
!$OMP END DO
!$OMP END PARALLEL

IF( lerr )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinterObs'
  error%Message = 'Observation interpolation error'
  error%Inform  = 'Exact error unknown in multi-processor environment'
  GOTO 9999
END IF

!------ Check for convergence based on maximum normalized change in squared velocity

IF( lAssm )THEN

  du2 = 0.
  DO k = 1,fld%grid%nz
    u2Sum(k) = u2Sum(k) / FLOAT(fld%grid%nXY)
    du2 = MAX(du2Max(k)/u2Sum(k),du2)
  END DO

  IF( du2 < DU2EPS )EXIT SCMiteration

END IF

!------ Jump to this point for first assimilation iteration

1000 CONTINUE

!------ Done if not assimilating

IF( .NOT.lAssm )EXIT SCMiteration

lstagger = BTEST( fld%grid%type,GTB_STAGGERZ ) !Used in IntXYZ

!------ Interpolate updated fields to obs locations

ObsSrc2 : DO j = 1,fld%nObsSource        !Loop over obs sources
  iObs = fld%iObsSource(j)

  DO iList = 1,2  !Loop over current and previous (if any) linked-lists

    SELECT CASE( iList )
      CASE( 1 )
        IF( ObsSrc(iObs)%numObs == 0 )CYCLE
        Obs => ObsSrc(iObs)%obs
      CASE( 2 )
        IF( PrevTimeWt(j) <= 0. )CYCLE ObsSrc2
        Obs => ObsSrc(iObs)%PrevObs
    END SELECT

    DO WHILE( ASSOCIATED(Obs) )  !Linked-list of observations

      hp = Obs%BL%h
      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        DO k = 1,nzb
          ip =  (k-1)*nxy + 1
          Z => fld%grid%sigma%Z(ip:); CALL IntXY( Obs%mxy,Z,zb(k) )
        END DO
        dp = 1.
      ELSE
        dp = 1. - Obs%BL%h/fld%grid%Ztop
      END IF

!------ Velocity interpolation

      IF( Obs%vel%nz > 0 )THEN   !Only do obs with at least one velocity measurement

        DO i = 1,Obs%vel%nz

!------ Account for surface layer in vertical interpolation

          zm  = Obs%Vel%z(i) - Obs%BL%h
          CALL set_mv( zb,nzb,Obs%mzVel(i),mv,zm,dp,Obs%BL%zsl, &
                                                    Obs%BL%surface%hc, &
                                                    Obs%BL%surface%alpha, &
                                                    Obs%BL%surface%zruf, &
                                                    Obs%BL%L )

!------ Interpolate velocities

          CALL IntXYZ( Obs%mxyu,mv,fld%NextField%U, &
                       u,dum,dum1,dum2,dp,gxp,gyp,1,lstagger )
          CALL IntXYZ( Obs%mxyv,mv,fld%NextField%V, &
                       v,dum,dum1,dum2,dp,gxp,gyp,2,lstagger )

!------ Compute difference between observations and updated field

          Obs%Vel%u(i) = Obs%VelObs%u(i) - u
          Obs%Vel%v(i) = Obs%VelObs%v(i) - v

!------ Get surface layer velocity difference at obs locations

          IF( i == 1 )THEN

            zm  = Obs%BL%zsl
            CALL set_mv( zb,nzb,Obs%mzVel(i),mv,zm,dp,Obs%BL%zsl, &
                                                      Obs%BL%surface%hc, &
                                                      Obs%BL%surface%alpha, &
                                                      Obs%BL%surface%zruf, &
                                                      Obs%BL%L )
            CALL IntXYZ( Obs%mxyu,mv,fld%NextField%U, &
                       u,dum,dum1,dum2,dp,gxp,gyp,1,lstagger )
            CALL IntXYZ( Obs%mxyv,mv,fld%NextField%V, &
                       v,dum,dum1,dum2,dp,gxp,gyp,2,lstagger )

            Obs%BL%uslx = Obs%BL%usl - u
            Obs%BL%vslx = Obs%BL%vsl - v

          END IF

        END DO

      END IF

!------ Pressure interpolation - Log(P/Psurf)

      IF( BTEST(fld%type,FTB_P) .AND. Obs%Press%nz > 0 )THEN

        DO i = 1,Obs%Press%nz

          CALL IntXYZ( Obs%mxy,Obs%mzPress(i),fld%NextField%Press, &
                       pr,dum,dum1,dum2,dp,gxp,gyp,0,lstagger )
          zm = (Obs%PressObs%z(i)-hp)/dp
          IF( zm < zb(1)*dp )THEN
            ztmp = zb(1)
          ELSE IF( zm > zb(nzb)*dp )THEN
            ztmp = zb(nzb)
          ELSE
            ztmp = NOT_SET_R
          END IF
          IF( ztmp /= NOT_SET_R )THEN              !Extrapolate with standard atmosphere
            ztmp = ztmp*dp + hp + Prj%Hmin
            CALL stnd_atmos( ztmp,pr1,t0,dtdz,SA_TA )
            ztmp = zm*dp + Prj%Hmin
            CALL stnd_atmos( ztmp,pr2,t0,dtdz,SA_TA )
            pr = EXP(pr) + pr2 - pr1
            pr = LOG(pr)
          END IF

          Obs%Press%obs(i) = Obs%PressObs%obs(i) - pr !Probably not a good idea

        END DO

      END IF

!------ Temperature

      IF( BTEST(fld%type,FTB_T) .AND. Obs%Tpot%nz > 0 )THEN

        DO i = 1,Obs%Tpot%nz

          CALL IntXYZ( Obs%mxy,Obs%mzTpot(i),fld%NextField%Tpot, &
                       Tpot,dum,dum1,dtdz, &
                       dp,gxp,gyp,0,lstagger )

          zm = (Obs%TpotObs%z(i)-hp)/dp
          IF( zm < zb(1)*dp )THEN
            ztmp = zb(1)
          ELSE IF( zm > zb(nzb)*dp )THEN
            ztmp = zb(nzb)
          ELSE
            ztmp = NOT_SET_R
          END IF
          IF( ztmp /= NOT_SET_R )THEN              !Extrapolate with standard atmosphere
            ztmp = ztmp*dp + hp + Prj%Hmin
            CALL stnd_atmos( ztmp,pr1,t0,dtdz,SA_TPOT )
            Tpot = Tpot - t0
            ztmp = zm*dp + Prj%Hmin
            CALL stnd_atmos( ztmp,dum,t0,dtdz,SA_TPOT )
            Tpot = Tpot + t0
          END IF

          Obs%Tpot%obs(i) = Obs%TpotObs%obs(i) - Tpot

        END DO

      END IF

!------ Humidity - RH

      IF( BTEST(fld%type,FTB_H) .AND. Obs%Humid%nz > 0 )THEN

        DO i = 1,Obs%Humid%nz

          CALL IntXYZ( Obs%mxy,Obs%mzHumid(i),fld%NextField%Humid, &
                       rh,dum,dum1,dum2, &
                       dp,gxp,gyp,0,lstagger )

          zm = (Obs%HumidObs%z(i)-hp)/dp
          IF( zm > zb(nzb)*dp )THEN
            ztmp = zm + hp + Prj%Hmin
            CALL stnd_atmos( ztmp,pr1,dum,dtdz,SA_TA )
            pr1 = pr1*PSURF
            ztmp = zb(nzb)*dp + hp + Prj%Hmin
            CALL stnd_atmos( ztmp,pr,dum,dtdz,SA_TA )
            pr = pr*PSURF
            IF( pr > PSTRAT )THEN         !Only extrapolate if top met level is below stratosphere
              rh = rh + (RHSTRAT-rh)/(PSTRAT-pr) * (pr1-pr)
            END IF
          END IF

          Obs%Humid%obs(i) = Obs%HumidObs%obs(i) - rh

        END DO

      END IF

!------ Cloud liquid water (g/m^3)

      IF( BTEST(fld%type,FTB_QCLD) .AND. Obs%Qcloud%nz > 0 )THEN

        DO i = 1,Obs%Qcloud%nz

          CALL IntXYZ( Obs%mxy,Obs%mzQcloud(i),fld%NextField%Qcloud, &
                       qc,dum,dum1,dum2, &
                       dp,gxp,gyp,0,lstagger )

          zm = (Obs%QcloudObs%z(i)-hp)/dp
          IF( zm > zb(nzb)*dp )qc = 0.

          Obs%Qcloud%obs(i) = Obs%QcloudObs%obs(i) - qc

        END DO

      END IF

!------ Go to next obs

      Obs => Obs%NextObs

    END DO

  END DO

END DO ObsSrc2

END DO SCMiteration

jrv = PostProgressBarMessage( 0 )
SWIMinterpObs = SWIMresult

!------ Set bottom slice for staggered grid

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN

  IF( BTEST(InterpPrf(1)%type,OIB_UV) )THEN
    DO i = 1,nxy
      fld%NextField%U(i) = fld%NextField%U(i+nxy)
      fld%NextField%V(i) = fld%NextField%V(i+nxy)
    END DO
  END IF

  IF( BTEST(InterpPrf(1)%type,OIB_LSV) )THEN
    DO i = 1,nxy
      fld%NextLSV%UU(i) = fld%NextLSV%UU(i+nxy)
      fld%NextLSV%UV(i) = fld%NextLSV%UV(i+nxy)
      fld%NextLSV%VV(i) = fld%NextLSV%VV(i+nxy)
      IF( BTEST(fld%type,FTB_LSVL) )fld%NextLSV%SL(i) = fld%NextLSV%SL(i+nxy)
    END DO
  END IF

  IF( BTEST(InterpPrf(1)%type,OIB_UU) )THEN
    DO i = 1,nxy
      fld%NextBLprof%UU(i) = fld%NextBLprof%UU(i+nxy)
      fld%NextBLprof%VV(i) = fld%NextBLprof%VV(i+nxy)
      fld%NextBLprof%WW(i) = fld%NextBLprof%WW(i+nxy)
      fld%NextBLprof%WT(i) = fld%NextBLprof%WT(i+nxy)
      fld%NextBLprof%SL(i) = fld%NextBLprof%SL(i+nxy)
      fld%NextBLprof%SZ(i) = fld%NextBLprof%SZ(i+nxy)
    END DO
  END IF

  IF( ASSOCIATED(InterpPrf(1)%Tpot%obs) )THEN
    DO i = 1,nxy
      fld%NextField%Tpot(i) = fld%NextField%Tpot(i+nxy)
    END DO
  END IF

  IF( ASSOCIATED(InterpPrf(1)%Press%obs) )THEN
    DO i = 1,nxy
      fld%NextField%Press(i) = fld%NextField%Press(i+nxy)
    END DO
  END IF

  IF( ASSOCIATED(InterpPrf(1)%Humid%obs) )THEN
    DO i = 1,nxy
      fld%NextField%Humid(i) = fld%NextField%Humid(i+nxy)
    END DO
  END IF

  IF( ASSOCIATED(InterpPrf(1)%Qcloud%obs) )THEN
    DO i = 1,nxy
      fld%NextField%Qcloud(i) = fld%NextField%Qcloud(i+nxy)
    END DO
  END IF

END IF

9999 CONTINUE

IF( ALLOCATED(NearObs) )DEALLOCATE( NearObs,STAT=irv )
IF( ALLOCATED(InterpPrf) )THEN
  DO i = 1,ny
    irv = SWIMdeallocInterpPrf( fld,InterpPrf(i) )
  END DO
  DEALLOCATE( InterpPrf,STAT=irv )
END IF

!------ Clean-up pointers for saved observations

IF( lAssm )THEN

  ObsSrc3 : DO i = 1,fld%nObsSource
    iObs = fld%iObsSource(i)

    DO iList = 1,2
      SELECT CASE( iList )
        CASE( 1 )
          IF( ObsSrc(iObs)%numObs == 0 )CYCLE
          Obs => ObsSrc(iObs)%obs
        CASE( 2 )
          IF( PrevTimeWt(i) <= 0. )CYCLE ObsSrc3
          Obs => ObsSrc(iObs)%PrevObs
      END SELECT

      DO WHILE( ASSOCIATED(Obs) )
        IF( Obs%Vel%nz   > 0 )THEN
          DO k = 1,Obs%Vel%nz
            Obs%Vel%u(k) = Obs%VelObs%u(k)
            Obs%Vel%v(k) = Obs%VelObs%v(k)
          END DO
          DEALLOCATE( Obs%VelObs%z,Obs%VelObs%u,Obs%VelObs%v,Obs%mzVel,STAT=irv )
        END IF
        IF( Obs%Press%nz > 0 .AND. BTEST(fld%type,FTB_P) )THEN
          DO k = 1,Obs%Press%nz
            Obs%Press%obs(k) = Obs%PressObs%obs(k)
          END DO
          DEALLOCATE( Obs%PressObs%z,Obs%PressObs%obs,Obs%mzPress,STAT=irv )
        END IF
        IF( Obs%Tpot%nz  > 0 .AND. BTEST(fld%type,FTB_T) )THEN
          IF( .NOT.BTEST(fld%type,FTB_P) )THEN
            Tptr => Obs%Tpot%obs; Obs%Tpot%obs => Obs%TpotStA%obs; Obs%TpotStA%obs => Tptr
          ELSE
            Tptr => Obs%Tpot%obs
          END IF
          DO k = 1,Obs%Tpot%nz
            Tptr(k) = Obs%TpotObs%obs(k)
          END DO
          DEALLOCATE( Obs%TpotObs%z,Obs%TpotObs%obs,Obs%mzTpot,STAT=irv )
        END IF
        IF( Obs%Humid%nz > 0 .AND. BTEST(fld%type,FTB_H) )THEN
          DO k = 1,Obs%Humid%nz
            Obs%Humid%obs(k) = Obs%HumidObs%obs(k)
          END DO
          DEALLOCATE( Obs%HumidObs%z,Obs%HumidObs%obs,Obs%mzHumid,STAT=irv )
        END IF
        IF( Obs%Qcloud%nz > 0 .AND. BTEST(fld%type,FTB_QCLD) )THEN
          DO k = 1,Obs%Qcloud%nz
            Obs%Qcloud%obs(k) = Obs%QcloudObs%obs(k)
          END DO
          DEALLOCATE( Obs%QcloudObs%z,Obs%QcloudObs%obs,Obs%mzQcloud,STAT=irv )
        END IF

        Obs => Obs%NextObs
      END DO

    END DO
  END DO ObsSrc3

  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
    DEALLOCATE( zb,STAT=irv ); NULLIFY( zb )
  END IF

END IF

IF( ALLOCATED(uPrev)  )DEALLOCATE(uPrev, STAT=irv)
IF( ALLOCATED(vPrev)  )DEALLOCATE(vPrev, STAT=irv)
IF( ALLOCATED(du2Max) )DEALLOCATE(du2Max,STAT=irv)
IF( ALLOCATED(u2Sum)  )DEALLOCATE(u2Sum, STAT=irv)

RETURN
END

!===============================================================================

INTEGER FUNCTION NearestObsParam( First,n_max )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( IN ) :: First
INTEGER,          INTENT( IN ) :: n_max

INTEGER, PARAMETER :: N_ANALYSIS = 4

IF( ANY(BTEST(First%type,(/OTB_FCST,OTB_ANLY/))) )THEN
  NearestObsParam = N_ANALYSIS
ELSE IF( n_max <= 0 )THEN
  NearestObsParam = MAX(First%numObs,First%PrevNumObs)
ELSE
  NearestObsParam = MIN(n_max,MAX(First%numObs,First%PrevNumObs))
END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMsetObsInterpVar( InterpPrf,First )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMObsSort

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( FirstObs  ),  INTENT( INOUT ) :: First

  First%GridList%lInterpVel  = .FALSE.
  First%GridList%lInterpP    = .FALSE.
  First%GridList%lInterpT    = .FALSE.
  First%GridList%lInterpH    = .FALSE.
  First%GridList%lInterpQcld = .FALSE.
  First%GridList%lInterpZi   = .FALSE.
  First%GridList%lInterpHf   = .FALSE.
  First%GridList%lInterpUs   = .FALSE.
  First%GridList%lInterpL    = .FALSE.
  First%GridList%lInterpCC   = .FALSE.
  First%GridList%lInterpPr   = .FALSE.

  IF( BTEST(InterpPrf%type,OIB_UV ) .OR. &
      BTEST(InterpPrf%type,OIB_LSV) .OR. &
      BTEST(InterpPrf%type,OIB_UU ) )THEN
      First%GridList%lInterpVel = .TRUE.
  END IF

  IF( BTEST(First%type,OTB_T) .AND. ASSOCIATED(InterpPrf%Tpot%obs) )THEN
      First%GridList%lInterpT = .TRUE.
  END IF

  IF( BTEST(First%type,OTB_P) .AND. ASSOCIATED(InterpPrf%Press%obs) )THEN
      First%GridList%lInterpP = .TRUE.
  END IF

!------ Humidity

  IF( BTEST(First%type,OTB_H) .AND. ASSOCIATED(InterpPrf%Humid%obs) )THEN
      First%GridList%lInterpH = .TRUE.
  END IF

  IF( BTEST(First%type,OTB_QCLD) .AND. ASSOCIATED(InterpPrf%Qcloud%obs) )THEN
      First%GridList%lInterpQcld = .TRUE.
  END IF

  IF( BTEST(First%type,OTB_BLV) )THEN
    First%GridList%lInterpZi = BTEST(First%type,OTB_ZI)
    First%GridList%lInterpHf = BTEST(First%type,OTB_HFLX)
    First%GridList%lInterpUs = BTEST(First%type,OTB_UST)
    First%GridList%lInterpL  = BTEST(First%type,OTB_MOL)
    First%GridList%lInterpCC = BTEST(First%type,OTB_CC)
    First%GridList%lInterpPr = BTEST(First%type,OTB_PRCP) .OR. BTEST(First%type,OTB_PRATE)
  END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMallocInterpPrf( fld,InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( MetField ),  INTENT( IN  ) :: fld
TYPE( ObsInterp ), INTENT( OUT ) :: InterpPrf

INTEGER alloc_stat, n

SWIMallocInterpPrf = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMallocInterpPrf'
error%Message = 'Error allocating interpolation profiles'

n = fld%grid%nZ
InterpPrf%nz = n

ALLOCATE( InterpPrf%z(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999


  ALLOCATE( InterpPrf%U%obs(n),InterpPrf%U%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999

  ALLOCATE( InterpPrf%V%obs(n),InterpPrf%V%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999


IF( BTEST(fld%type,FTB_T) )THEN
  ALLOCATE( InterpPrf%Tpot%obs(n),InterpPrf%Tpot%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  ALLOCATE( InterpPrf%Humid%obs(n),InterpPrf%Humid%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  ALLOCATE( InterpPrf%Press%obs(n),InterpPrf%Press%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  ALLOCATE( InterpPrf%Qcloud%obs(n),InterpPrf%Qcloud%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
  ALLOCATE( InterpPrf%UUL%obs(n),InterpPrf%UUL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%VVL%obs(n),InterpPrf%VVL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%UVL%obs(n),InterpPrf%UVL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  IF( BTEST(fld%type,FTB_LSVL) )THEN
    ALLOCATE( InterpPrf%SHL%obs(n),InterpPrf%SHL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  END IF
END IF

IF( BTEST(fld%type,FTB_UU) )THEN
  ALLOCATE( InterpPrf%UU%obs(n),InterpPrf%UU%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%VV%obs(n),InterpPrf%VV%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%WW%obs(n),InterpPrf%WW%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%WT%obs(n),InterpPrf%WT%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%SL%obs(n),InterpPrf%SL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%SZ%obs(n),InterpPrf%SZ%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
END IF

SWIMallocInterpPrf = SWIMresult

error%Number  = NO_ERROR
error%Routine = ' '
error%Message = ' '

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMnullifyInterpPrf( InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( ObsInterp ), INTENT( OUT ) :: InterpPrf

INTEGER irv

INTEGER, EXTERNAL :: SWIMnullifyObsInterpPrf

SWIMnullifyInterpPrf = SWIMfailure

InterpPrf%nz = 0

NULLIFY( InterpPrf%z )
irv = SWIMnullifyObsInterpPrf( InterpPrf%U )
irv = SWIMnullifyObsInterpPrf( InterpPrf%V )
irv = SWIMnullifyObsInterpPrf( InterpPrf%Tpot )
irv = SWIMnullifyObsInterpPrf( InterpPrf%Humid )
irv = SWIMnullifyObsInterpPrf( InterpPrf%Press )
irv = SWIMnullifyObsInterpPrf( InterpPrf%Qcloud )
irv = SWIMnullifyObsInterpPrf( InterpPrf%UUL )
irv = SWIMnullifyObsInterpPrf( InterpPrf%VVL )
irv = SWIMnullifyObsInterpPrf( InterpPrf%UVL )
irv = SWIMnullifyObsInterpPrf( InterpPrf%SHL )
irv = SWIMnullifyObsInterpPrf( InterpPrf%UU )
irv = SWIMnullifyObsInterpPrf( InterpPrf%VV )
irv = SWIMnullifyObsInterpPrf( InterpPrf%WW )
irv = SWIMnullifyObsInterpPrf( InterpPrf%WT )
irv = SWIMnullifyObsInterpPrf( InterpPrf%SL )
irv = SWIMnullifyObsInterpPrf( InterpPrf%SZ )

SWIMnullifyInterpPrf = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMnullifyObsInterpPrf( obs )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( ObsInterpPrf ), INTENT( INOUT ) :: obs

NULLIFY( obs%obs )
NULLIFY( obs%wt  )

SWIMnullifyObsInterpPrf = SWIMresult

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMdeallocInterpPrf( fld,InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( MetField ),  INTENT( IN  )   :: fld
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf

INTEGER alloc_stat, irv
INTEGER, EXTERNAL :: SWIMdeallocObsInterpPrf

IF( ASSOCIATED(InterpPrf%z) )DEALLOCATE( InterpPrf%z,STAT=alloc_stat )

irv = SWIMdeallocObsInterpPrf( InterpPrf%U )
irv = SWIMdeallocObsInterpPrf( InterpPrf%V )

IF( BTEST(fld%type,FTB_T) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Tpot )
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Humid )
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Press )
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Qcloud )
END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UUL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%VVL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UVL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SHL )
END IF

IF( BTEST(fld%type,FTB_UU) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UU )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%VV )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%WW )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%WT )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SZ )
END IF

SWIMdeallocInterpPrf = SWIMresult

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMdeallocObsInterpPrf( prf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( ObsInterpPrf ), INTENT( INOUT ) :: prf

INTEGER alloc_stat

IF( ASSOCIATED(prf%obs) )DEALLOCATE( prf%obs,STAT=alloc_stat )
IF( ASSOCIATED(prf%wt ) )DEALLOCATE( prf%wt,STAT=alloc_stat )

SWIMdeallocObsInterpPrf = SWIMresult

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMzeroInterpPrf( fld,InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( MetField ),  INTENT( IN    ) :: fld
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf

INTEGER i

  DO i = 1,InterpPrf%nz
    InterpPrf%U%obs(i) = 0.; InterpPrf%U%wt(i) = 0.
    InterpPrf%V%obs(i) = 0.; InterpPrf%V%wt(i) = 0.
  END DO

IF( BTEST(fld%type,FTB_T) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Tpot%obs(i) = 0.; InterpPrf%Tpot%wt(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Humid%obs(i) = 0.; InterpPrf%Humid%wt(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Press%obs(i) = 0.; InterpPrf%Press%wt(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Qcloud%obs(i) = 0.; InterpPrf%Qcloud%wt(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%UUL%obs(i) = 0.; InterpPrf%UUL%wt(i) = 0.
    InterpPrf%VVL%obs(i) = 0.; InterpPrf%VVL%wt(i) = 0.
    InterpPrf%UVL%obs(i) = 0.; InterpPrf%UVL%wt(i) = 0.
    IF( BTEST(fld%type,FTB_LSVL) )THEN
      InterpPrf%SHL%obs(i) = 0.; InterpPrf%SHL%wt(i) = 0.
    END IF
  END DO
END IF

IF( BTEST(fld%type,FTB_UU) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%UU%obs(i) = 0.; InterpPrf%UU%wt(i) = 0.
    InterpPrf%VV%obs(i) = 0.; InterpPrf%VV%wt(i) = 0.
    InterpPrf%WW%obs(i) = 0.; InterpPrf%WW%wt(i) = 0.
    InterpPrf%WT%obs(i) = 0.; InterpPrf%WT%wt(i) = 0.
    InterpPrf%SL%obs(i) = 0.; InterpPrf%SL%wt(i) = 0.
    InterpPrf%SZ%obs(i) = 0.; InterpPrf%SZ%wt(i) = 0.
  END DO
END IF

!------ Zero surface quantities

IF( BTEST(fld%type,FTB_ZI) )THEN
  InterpPrf%Zi%obs = 0.; InterpPrf%Zi%wt = 0.
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  InterpPrf%Hflux%obs = 0.; InterpPrf%Hflux%wt = 0.
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  InterpPrf%invL%obs = 0.; InterpPrf%invL%wt = 0.
END IF

IF( BTEST(fld%type,FTB_PRCP) .OR. BTEST(fld%type,FTB_PRATE) )THEN
  InterpPrf%Prcp%obs = 0.; InterpPrf%Prcp%wt = 0.
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  InterpPrf%CldCv%obs = 0.; InterpPrf%CldCv%wt = 0.
END IF

IF( BTEST(fld%type,FTB_UST) )THEN
  InterpPrf%Ust%obs = 0.; InterpPrf%Ust%wt = 0.
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  InterpPrf%LAI%obs = 0.; InterpPrf%LAI%wt = 0.
END IF

SWIMzeroInterpPrf = SWIMresult

RETURN
END

!==============================================================================

SUBROUTINE CopyFieldAssm( fld )

!------ Copy background Field1 into NextField for initializing assimilation
!       N.B. Assumes NO SWIFT, BL profile, 3d Z-field, met unc (file)

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetField), INTENT( INOUT ) :: fld

INTEGER n

!------ 3D arrays

n = SIZE(fld%Field1%U)

CALL CopyArray( fld%NextField%U,fld%Field1%U,n )
CALL CopyArray( fld%NextField%V,fld%Field1%V,n )
IF( BTEST(fld%type,FTB_W  ) )CALL CopyArray( fld%NextField%W,fld%Field1%W,n )
IF( BTEST(fld%type,FTB_DU2) )CALL CopyArray( fld%NextField%dU2,fld%Field1%dU2,n )

IF( BTEST(fld%type,FTB_T) )THEN
  CALL CopyArray( fld%NextField%Tpot,fld%Field1%Tpot,n )
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  CALL CopyArray( fld%NextField%Humid,fld%Field1%Humid,n )
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  CALL CopyArray( fld%NextField%Press,fld%Field1%Press,n )
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  CALL CopyArray( fld%NextField%Qcloud,fld%Field1%Qcloud,n )
END IF

IF( BTEST(fld%type,FTB_Z) )THEN

!ERROR
!  CALL CopyArray( fld%NextField%Z,fld%Field2%Z,n )

!  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
!    fld%grid%sigma%Z => fld%NextField%Z(fld%grid%nXY+1:)
!    CALL SetSigmaZw( fld%grid )
!  ELSE
!    fld%grid%sigma%Z => fld%NextField%Z
!  END IF

END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
!  CALL CopyArray( fld%LSV%UU,fld%NextLSV%UU,n )
!  CALL CopyArray( fld%LSV%VV,fld%NextLSV%VV,n )
!  CALL CopyArray( fld%LSV%UV,fld%NextLSV%UV,n )
!  IF( BTEST(fld%type,FTB_LSVL) )CALL CopyArray( fld%LSV%SL,fld%NextLSV%SL,n )
END IF


!------ 2D (horizontal) arrays

n = fld%grid%nXY

IF( BTEST(fld%type,FTB_ZI) )THEN
  CALL CopyArray( fld%NextBL%zi,fld%BL1%zi,n )
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  CALL CopyArray( fld%NextBL%HeatFlux,fld%BL1%HeatFlux,n )
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  CALL CopyArray( fld%NextBL%invMOL,fld%BL1%invMOL,n )
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  CALL CopyArray( fld%NextBL%cc,fld%BL1%cc,n )
END IF

IF( BTEST(fld%type,FTB_PRCP) )THEN
  CALL CopyArray( fld%NextBL%prcp,fld%BL1%prcp,n )
ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
  CALL CopyArray( fld%NextBL%prcp,fld%BL1%prcp,n )
  IF( ASSOCIATED(fld%NextBL%rainprb) )CALL CopyArray( fld%NextBL%rainprb,fld%BL1%rainprb,n )
END IF

IF( BTEST(fld%type,FTB_ZRUF) )THEN
  CALL CopyArray( fld%NextBL%zruf,fld%BL1%zruf,n )
  CALL CopyArray( fld%grid%landcover%roughness,fld%NextBL%zruf,n )
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  CALL CopyArray( fld%NextBL%LAI,fld%BL1%LAI,n )
END IF

RETURN
END

!==============================================================================

SUBROUTINE CopyNextField2Assm( fld )

!------ Copy Field2 into Field2 for initializing assimilation
!       N.B. Used only when assimiliation occurs on gridded domain, i.e. not a nest
!            Assumes NO SWIFT, BL profile, 3d Z-field, met unc (file)

USE SWIM_fi
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetField), INTENT( INOUT ) :: fld

INTEGER n

!------ 3D arrays

n = SIZE(fld%NextField%U)

CALL CopyArray( fld%Field2%U,fld%NextField%U,n )
CALL CopyArray( fld%Field2%V,fld%NextField%V,n )
IF( BTEST(fld%type,FTB_W  ) )CALL CopyArray( fld%Field2%W,fld%NextField%W,n )
IF( BTEST(fld%type,FTB_DU2) )CALL CopyArray( fld%Field2%dU2,fld%NextField%dU2,n )

IF( BTEST(fld%type,FTB_T) )THEN
  CALL CopyArray( fld%Field2%Tpot,fld%NextField%Tpot,n )
END IF

IF( BTEST(fld%type,FTB_H) )THEN
  CALL CopyArray( fld%Field2%Humid,fld%NextField%Humid,n )
END IF

IF( BTEST(fld%type,FTB_P) )THEN
  CALL CopyArray( fld%Field2%Press,fld%NextField%Press,n )
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  CALL CopyArray( fld%Field2%Qcloud,fld%NextField%Qcloud,n )
END IF

IF( BTEST(fld%type,FTB_Z) )THEN

!ERROR
!  CALL CopyArray( fld%Field2%Z,fld%Field2%Z,n )

!  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
!    fld%grid%sigma%Z => fld%Field2%Z(fld%grid%nXY+1:)
!    CALL SetSigmaZw( fld%grid )
!  ELSE
!    fld%grid%sigma%Z => fld%Field2%Z
!  END IF

END IF

IF( BTEST(fld%type,FTB_LSV) )THEN
!  CALL CopyArray( fld%LSV%UU,fld%NextLSV%UU,n )
!  CALL CopyArray( fld%LSV%VV,fld%NextLSV%VV,n )
!  CALL CopyArray( fld%LSV%UV,fld%NextLSV%UV,n )
!  IF( BTEST(fld%type,FTB_LSVL) )CALL CopyArray( fld%LSV%SL,fld%NextLSV%SL,n )
END IF


!------ 2D (horizontal) arrays

n = fld%grid%nXY

IF( BTEST(fld%type,FTB_ZI) )THEN
  CALL CopyArray( fld%BL2%zi,fld%NextBL%zi,n )
END IF

IF( BTEST(fld%type,FTB_HFLX) )THEN
  CALL CopyArray( fld%BL2%HeatFlux,fld%NextBL%HeatFlux,n )
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  CALL CopyArray( fld%BL2%invMOL,fld%NextBL%invMOL,n )
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  CALL CopyArray( fld%BL2%cc,fld%NextBL%cc,n )
END IF

IF( BTEST(fld%type,FTB_PRCP) )THEN
  CALL CopyArray( fld%BL2%prcp,fld%NextBL%prcp,n )
ELSE IF( BTEST(fld%type,FTB_PRATE) )THEN
  CALL CopyArray( fld%BL2%prcp,fld%NextBL%prcp,n )
  IF( ASSOCIATED(fld%BL2%rainprb) )CALL CopyArray( fld%BL2%rainprb,fld%NextBL%rainprb,n )
END IF

IF( BTEST(fld%type,FTB_ZRUF) )THEN
  CALL CopyArray( fld%BL2%zruf,fld%NextBL%zruf,n )
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  CALL CopyArray( fld%BL2%LAI,fld%NextBL%LAI,n )
END IF

RETURN
END
