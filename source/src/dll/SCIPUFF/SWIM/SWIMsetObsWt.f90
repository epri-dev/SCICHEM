RECURSIVE INTEGER FUNCTION SWIMsetObsWt( InterpPrf,NearObs,First,PrevTimeWt )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMObsSort

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( INOUT ) :: NearObs
TYPE( FirstObs  ),  INTENT( IN    ) :: First
REAL,               INTENT( IN    ) :: PrevTimeWt

INTEGER irv, iList, numObs
REAL    tfac
LOGICAL lsort

TYPE( ObsMet           ), POINTER :: Obs
TYPE( FirstObsGridList ), POINTER :: GridList

INTEGER, EXTERNAL :: SWIMobsInterpVel, SWIMobsInterpT, SWIMobsInterpH, SWIMobsInterpP
INTEGER, EXTERNAL :: SWIMobsInterpQ
INTEGER, EXTERNAL :: SWIMobsInterpSrf

SWIMsetObsWt = SWIMfailure

!------ Loop over current and previous linked-lists
!       Only use previous obs if time factor > 0.

DO iList = 1,2

  SELECT CASE( iList )
    CASE( 1 )
      IF( First%numObs == 0 )CYCLE
      Obs      => First%Obs
      GridList => First%GridList
      numObs   =  First%numObs
      tfac     =  1. - PrevTimeWt
    CASE( 2 )
      IF( PrevTimeWt <= 0. )EXIT
      Obs      => First%PrevObs
      GridList => First%PrevGridList
      numObs   =  First%PrevNumObs
      tfac     =  PrevTimeWt
  END SELECT

  ALLOCATE( NearObs%Obs(numObs),STAT=irv )
  IF( irv /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMsetObsWt'
    error%Message = 'Error allocating nearest obs list'
    GOTO 9999
  END IF

!------ First construct list of nearest obs

  lsort = NearObs%numInterp < numObs !Only order obs if number of nearest is
                                     !less than total number of obs stations
  IF( lsort .AND. ASSOCIATED(GridList) )THEN
    irv = FindNearestGridList( InterpPrf%x,InterpPrf%y,InterpPrf%xfac,InterpPrf%yfac, &
                               InterpPrf%a2,GridList,NearObs,numObs )
  ELSE
     irv = FindNearestList( InterpPrf%x,InterpPrf%y,InterpPrf%xfac,InterpPrf%yfac, &
                            InterpPrf%a2,Obs,NearObs,numObs,lsort )
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Interpolate velocity

  IF( BTEST(InterpPrf%type,OIB_UV ) .OR. &
      BTEST(InterpPrf%type,OIB_LSV) .OR. &
      BTEST(InterpPrf%type,OIB_UU ) )THEN

    irv = SWIMobsInterpVel( InterpPrf,NearObs,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

!------ Temperature

  IF( BTEST(First%type,OTB_T) .AND. ASSOCIATED(InterpPrf%Tpot%obs) )THEN
    irv = SWIMobsInterpT( InterpPrf,NearObs,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Pressure

  IF( BTEST(First%type,OTB_P) .AND. ASSOCIATED(InterpPrf%Press%obs) )THEN
    irv = SWIMobsInterpP( InterpPrf,NearObs,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Humidity

  IF( BTEST(First%type,OTB_H) .AND. ASSOCIATED(InterpPrf%Humid%obs) )THEN
    irv = SWIMobsInterpH( InterpPrf,NearObs,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Cloud liquid water

  IF( BTEST(First%type,OTB_QCLD) .AND. ASSOCIATED(InterpPrf%Qcloud%obs) )THEN
    irv = SWIMobsInterpQ( InterpPrf,NearObs,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Surface variables

  IF( BTEST(First%type,OTB_BLV) )THEN
    irv = SWIMobsInterpSrf( InterpPrf,NearObs,First,tfac )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Cleanup

  DEALLOCATE( NearObs%Obs,STAT=irv ); NULLIFY( NearObs%Obs )

END DO

SWIMsetObsWt = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(NearObs%Obs) )DEALLOCATE( NearObs%Obs,STAT=irv )

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpVel( InterpPrf,NearObs,tfac )

!------ Interpolate obs velocity profiles

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMvertWt

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
REAL,               INTENT( IN    ) :: tfac

INTEGER alloc_stat, i, k, nsta, nz
REAL    HorizWt, zref, wspd, rn, lat, lon, zi, zsl, bowen, mol, ustar, r2u, r2v, facp
REAL    zInfl,  fslObs, fObs, f, fsl, uref
LOGICAL lstagger, lbl, DiffLandChar, ComputeZi, FixedHeatFlux

REAL,    DIMENSION(:), POINTER   :: zObs
REAL,    DIMENSION(InterpPrf%nz) :: zu, zv, z
REAL,    DIMENSION(InterpPrf%nz) :: VertWt, fac, wt, tem
INTEGER, DIMENSION(InterpPrf%nz) :: k1, k2

TYPE( ObsMet ), POINTER :: Obs

INTERFACE
  RECURSIVE SUBROUTINE InterpObsSurfaceLayer( nz,z,u,fac,k1,k2,usl,uObs,Obs )
    USE SWIMobs_fd
    INTEGER,                INTENT( IN )    :: nz
    REAL,    DIMENSION(nz), INTENT( IN )    :: z
    REAL,    DIMENSION(nz), INTENT( INOUT ) :: u
    REAL,    DIMENSION(nz), INTENT( IN    ) :: fac
    INTEGER, DIMENSION(nz), INTENT( IN )    :: k1, k2
    REAL,                   INTENT( IN )    :: usl
    REAL,    DIMENSION(:),  POINTER         :: uObs
    TYPE( ObsMet ),         POINTER         :: Obs
  END SUBROUTINE InterpObsSurfaceLayer
END INTERFACE

REAL,    EXTERNAL :: dist2
INTEGER, EXTERNAL :: SWIMgetLL

SWIMobsInterpVel = SWIMfailure

nsta     = 0
nz       = InterpPrf%nz
lstagger = BTEST(InterpPrf%type,OIB_STAGGER)

!------ Construct height (AGL) arrays

zu = InterpPrf%z*InterpPrf%du
IF( lstagger )THEN
  zv = InterpPrf%z*InterpPrf%dv
  z  = InterpPrf%z*InterpPrf%d
ELSE
  zv = zu
  z  = zu
END IF

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

  nzObsGTzero : IF( Obs%Vel%nz > 0 )THEN

    nsta = nsta + 1

!------ Allocate obs height array

    ALLOCATE( zObs(Obs%Vel%nz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMobsInterpVel'
      error%Message = 'Error obs height array'
      GOTO 9999
    END IF

    zObs = Obs%Vel%z(1:Obs%Vel%nz)

!------ Compute surface layer parameters if land characteristics
!       differ between obs location and interpolation point

    lbl = Obs%BL%zsl > 0.

    DiffLandChar = lbl .AND. &
    (ABS(InterpPrf%zruf -Obs%BL%surface%zruf)  / MAX(InterpPrf%zruf, Obs%BL%surface%zruf,0.1 ) > 0.2 .OR. &
     ABS(InterpPrf%hc   -Obs%BL%surface%hc)    / MAX(InterpPrf%hc,   Obs%BL%surface%hc,1.E-3 ) > 0.2 .OR. &
     ABS(InterpPrf%alpha-Obs%BL%surface%alpha) / MAX(InterpPrf%alpha,Obs%BL%surface%alpha,1. ) > 0.2 )

    IF( DiffLandChar )THEN

      rn     = NOT_SET_R
      bowen  = NOT_SET_R
      zref   = 1000.      !Set large value to insure iteration on zsl, L and ustar (ZrefIter)
      zi     = 1000.
      wspd   = SQRT(Obs%BL%usl**2 + Obs%BL%vsl**2 + 1.E-6)

      IF( Obs%varSrf%invL == OBP_NODATA )THEN

        IF( SWIMgetLL(InterpPrf%x,InterpPrf%y,lon,lat) == SWIMsuccess )THEN
          ComputeZi = .TRUE.
        ELSE
          ComputeZi = .FALSE.
          CALL SWIMclearError()
        END IF

        FixedHeatFlux = .TRUE.

        IF( Obs%varSrf%Zi /= OBP_NODATA )THEN
          zi = Obs%varSrf%Zi
          zref = MIN(zref,zi,Obs%BL%zsl)
          zref = MAX(zref,20.*InterpPrf%zruf,20.*Obs%BL%surface%zruf)
          ComputeZi = .FALSE.
        END IF

        ustar = NOT_SET_R
        CALL SurfIter( zref,wspd,Obs%BL%tref,Obs%BL%pref,InterpPrf%zruf,InterpPrf%hc, &
                       InterpPrf%alpha,bowen,Obs%BL%cc, &
                       rn,lat,ComputeZi,FixedHeatFlux,Obs%BL%hflux, &
                       zi,zsl,mol,ustar,.FALSE. )

      ELSE

        ustar = wspd**2
        CALL SetBLfromMOL( Obs%varSrf%invL,InterpPrf%zruf,InterpPrf%hc, &
                           InterpPrf%alpha,.TRUE.,mol,zi,zsl,zref,ustar )
        ustar = SQRT(ustar)

      END IF

      zInfl = MAX( 20.*Obs%BL%surface%zruf,2.*Obs%BL%surface%hc, &
                   20.*InterpPrf%zruf,     2.*InterpPrf%hc )
      zInfl = MIN( zInfl,MAX(zsl,Obs%BL%zsl))

      fsl    = -1.
      fslObs = -1.

      CALL set_fsl( zInfl,Obs%BL%zsl,Obs%BL%surface%hc,Obs%BL%surface%alpha, &
                            Obs%BL%surface%zruf,Obs%BL%L,fObs,fslObs )

      CALL set_fsl( zInfl,zsl,InterpPrf%hc,InterpPrf%alpha, &
                            InterpPrf%zruf,mol,f,fsl )
      facp = fObs/f

    END IF

!------ Interpolate U & V if required

    IF( BTEST(InterpPrf%type,OIB_UV) )THEN

!------ First compute values for U (then re-compute for V if staggered grid)

!------ Define horizontal weight

      IF( lstagger )THEN
        r2u = dist2( Obs%x,Obs%y,InterpPrf%xu,InterpPrf%y,InterpPrf%xfac,InterpPrf%yfac )
        HorizWt = 1. / ( 1. + Obs%a2*r2u)
      ELSE
        HorizWt = NearObs%obs(i)%wt
      END IF

!------ Vertical weights and interpolation factors

      CALL CalcVertWt( nz,zu,InterpPrf%hu, &
                       Obs%Vel%nz,zObs,Obs%BL%h,Obs%vscaleTop,Obs%vscaleBot, &
                       VertWt,k1,k2,fac,InterpPrf%zmax )

!------ Interpolate U-velocity

      DO k = 1,nz
        wt(k)  = HorizWt * VertWt(k) * tfac
        tem(k) = fac(k)*Obs%Vel%u(k1(k)) + (1.-fac(k))*Obs%Vel%u(k2(k))
      END DO

!------ Apply surface layer

      IF( lbl )THEN

        CALL InterpObsSurfaceLayer( nz,zu,tem,fac,k1,k2,Obs%BL%uslx,Obs%Vel%U,Obs )

        IF( DiffLandChar )THEN

          fsl    = -1.
          fslObs = -1.

          IF( zu(1) <= zInfl )THEN
            k = 1
            DO
              IF( zu(k) >= zInfl )EXIT
              IF( k == nz )EXIT
              k = k + 1
            END DO

            zref = zu(k)
            CALL set_fsl( zref,Obs%BL%zsl,Obs%BL%surface%hc,Obs%BL%surface%alpha, &
                                Obs%BL%surface%zruf,Obs%BL%L,fObs,fslObs )
            uref = tem(k) * facp/fObs
          END IF

          DO k = 1,nz
            IF( zu(k) <= zInfl )THEN
              CALL set_fsl( zu(k),zsl,InterpPrf%hc,InterpPrf%alpha, &
                                  InterpPrf%zruf,mol,f,fsl )
              tem(k) = uref * f
            ELSE
              EXIT
            END IF
          END DO

        END IF

      END IF

!------ Sum weighted (and interpolated) velocity

      DO k = 1,nz
        InterpPrf%U%obs(k) = InterpPrf%U%obs(k) + wt(k)*tem(k)
        InterpPrf%U%wt(k)  = InterpPrf%U%wt(k)  + wt(k)
      END DO

!------ Now do V

      IF( lstagger )THEN

!------ Re-define weights for staggered grid

        r2v = dist2( Obs%x,Obs%y,InterpPrf%x,InterpPrf%yv,InterpPrf%xfac,InterpPrf%yfac )
        HorizWt = 1. / ( 1. + Obs%a2*r2v)

        CALL CalcVertWt( nz,zv,InterpPrf%hv, &
                         Obs%Vel%nz,zObs,Obs%BL%h,Obs%vscaleTop,Obs%vscaleBot, &
                         VertWt,k1,k2,fac,InterpPrf%zmax )

      END IF

!------ Interpolate V-velocity

      DO k = 1,nz
        wt(k)  = HorizWt * VertWt(k) * tfac
        tem(k) = fac(k)*Obs%Vel%v(k1(k)) + (1.-fac(k))*Obs%Vel%v(k2(k))
      END DO

!------ Apply surface layer

      IF( lbl )THEN

        CALL InterpObsSurfaceLayer( nz,zv,tem,fac,k1,k2,Obs%BL%vslx,Obs%Vel%V,Obs )

        IF( DiffLandChar )THEN

          fslObs = -1.
          fsl    = -1.

          IF( zv(1) <= zInfl )THEN
            k = 1
            DO
              IF( zv(k) >= zInfl )EXIT
              IF( k == nz )EXIT
              k = k + 1
            END DO
            zref = zv(k)
            CALL set_fsl( zref,Obs%BL%zsl,Obs%BL%surface%hc,Obs%BL%surface%alpha, &
                                Obs%BL%surface%zruf,Obs%BL%L,fObs,fslObs )
            uref = tem(k) * facp/fObs
          END IF

          DO k = 1,nz
            IF( zv(k) <= zInfl )THEN
              CALL set_fsl( zv(k),zsl,InterpPrf%hc,InterpPrf%alpha, &
                                  InterpPrf%zruf,mol,f,fsl )
              tem(k) = uref * f
            ELSE
              EXIT
            END IF
          END DO

        END IF

      END IF

!------ Sum weighted (and interpolated) velocity

      DO k = 1,nz
        InterpPrf%V%obs(k) = InterpPrf%V%obs(k) + wt(k)*tem(k)
        InterpPrf%V%wt(k)  = InterpPrf%V%wt(k)  + wt(k)
      END DO

    END IF

!------ LSV

    IF( BTEST(InterpPrf%type,OIB_LSV) )THEN

      IF( lstagger .OR. .NOT.BTEST(InterpPrf%type,OIB_UV) )THEN

        HorizWt = 1. / ( 1. + Obs%a2*dist2( Obs%x,Obs%y,InterpPrf%x,InterpPrf%y, &
                                            InterpPrf%xfac,InterpPrf%yfac ))

        CALL CalcVertWt( nz,z,InterpPrf%h, &
                         Obs%Vel%nz,zObs,Obs%BL%h,Obs%vscaleTop,Obs%vscaleBot, &
                         VertWt,k1,k2,fac,InterpPrf%zmax )
        DO k = 1,nz
          wt(k) = HorizWt * VertWt(k) * tfac
        END DO

      END IF

      DO k = 1,nz
        tem(k) = fac(k)*Obs%Vel%LSV%uu(k1(k)) + (1.-fac(k))*Obs%Vel%LSV%uu(k2(k))
        InterpPrf%UUL%obs(k) = InterpPrf%UUL%obs(k) + wt(k)*tem(k)
        InterpPrf%UUL%wt(k)  = InterpPrf%UUL%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%LSV%vv(k1(k)) + (1.-fac(k))*Obs%Vel%LSV%vv(k2(k))
        InterpPrf%VVL%obs(k) = InterpPrf%VVL%obs(k) + wt(k)*tem(k)
        InterpPrf%VVL%wt(k)  = InterpPrf%VVL%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%LSV%uv(k1(k)) + (1.-fac(k))*Obs%Vel%LSV%uv(k2(k))
        InterpPrf%UVL%obs(k) = InterpPrf%UVL%obs(k) + wt(k)*tem(k)
        InterpPrf%UVL%wt(k)  = InterpPrf%UVL%wt(k)  + wt(k)
      END DO

      IF( ASSOCIATED(Obs%Vel%LSV%sl) )THEN
        tem = fac*Obs%Vel%LSV%sl(k1(1:nz)) + (1.-fac)*Obs%Vel%LSV%sl(k2(1:nz))
        InterpPrf%SHL%obs = InterpPrf%SHL%obs + wt*tem
        InterpPrf%SHL%wt  = InterpPrf%SHL%wt  + wt
      END IF

    END IF

!------ Boundary layer turbulence profiles

    IF( BTEST(InterpPrf%type,OIB_UU) )THEN

      IF( lstagger .OR. .NOT.BTEST(InterpPrf%type,OIB_UV) )THEN

        HorizWt = 1. / ( 1. + Obs%a2*dist2( Obs%x,Obs%y,InterpPrf%x,InterpPrf%y, &
                                            InterpPrf%xfac,InterpPrf%yfac ))

        CALL CalcVertWt( nz,z,InterpPrf%h, &
                         Obs%Vel%nz,zObs,Obs%BL%h,Obs%vscaleTop,Obs%vscaleBot, &
                         VertWt,k1,k2,fac,InterpPrf%zmax )
        DO k = 1,nz
          wt(k) = HorizWt * VertWt(k) * tfac
        END DO

      END IF

      DO k = 1,nz
        tem(k) = fac(k)*Obs%Vel%Blprof%uu(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%uu(k2(k))
        InterpPrf%UU%obs(k) = InterpPrf%UU%obs(k) + wt(k)*tem(k)
        InterpPrf%UU%wt(k)  = InterpPrf%UU%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%Blprof%vv(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%vv(k2(k))
        InterpPrf%VV%obs(k) = InterpPrf%VV%obs(k) + wt(k)*tem(k)
        InterpPrf%VV%wt(k)  = InterpPrf%VV%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%Blprof%ww(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%ww(k2(k))
        InterpPrf%WW%obs(k) = InterpPrf%WW%obs(k) + wt(k)*tem(k)
        InterpPrf%WW%wt(k)  = InterpPrf%WW%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%Blprof%wt(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%wt(k2(k))
        InterpPrf%WT%obs(k) = InterpPrf%WT%obs(k) + wt(k)*tem(k)
        InterpPrf%WT%wt(k)  = InterpPrf%WT%wt(k)  + wt(k)

        tem(k) = fac(k)*Obs%Vel%Blprof%sl(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%sl(k2(k))
        InterpPrf%SL%obs(k) = InterpPrf%SL%obs(k) + wt(k)*tem(k)
        InterpPrf%SL%wt(k)  = InterpPrf%SL%wt(k)  + wt(k)

        tem (k)= fac(k)*Obs%Vel%Blprof%sz(k1(k)) + (1.-fac(k))*Obs%Vel%Blprof%sz(k2(k))
        InterpPrf%SZ%obs(k) = InterpPrf%SZ%obs(k) + wt(k)*tem(k)
        InterpPrf%SZ%wt(k)  = InterpPrf%SZ%wt(k)  + wt(k)
      END DO

    END IF

    DEALLOCATE( zObs,STAT=alloc_stat ); NULLIFY( zObs )

!------ Check if number of nearest obs have been used

    IF( nsta == NearObs%numInterp )EXIT StaLoop

  END IF nzObsGTzero

END DO StaLoop

SWIMobsInterpVel = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE InterpObsSurfaceLayer( nz,z,u,fac,k1,k2,usl,uObs,Obs )

USE SWIMobs_fd

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: nz
REAL,    DIMENSION(nz), INTENT( IN    ) :: z
REAL,    DIMENSION(nz), INTENT( INOUT ) :: u
REAL,    DIMENSION(nz), INTENT( IN    ) :: fac
INTEGER, DIMENSION(nz), INTENT( IN    ) :: k1, k2
REAL,                   INTENT( IN    ) :: usl
REAL,    DIMENSION(:),  POINTER         :: uObs
TYPE( ObsMet ),         POINTER         :: Obs

INTEGER k, i1, i2
REAL    zsl, L, zruf, hc, alpha
REAL    fsl, fz, facm, facp

zsl  = Obs%BL%zsl; L = Obs%BL%L
zruf = Obs%BL%surface%zruf; hc = Obs%BL%surface%hc; alpha = Obs%BL%surface%alpha
fsl  = -1.

DO k = 1,nz

  i1 = k1(k); i2 = k2(k)

  IF( i1 == 1 .AND. i2 == 1 )THEN                !Below all obs levels or single obs

    IF( Obs%SrfPrf%z(1) < zsl )THEN
      IF( z(k) > zsl )THEN
        u(k) = usl
      ELSE
        CALL get_ubl( Obs%SrfPrf%z(1),uObs(1),L,zruf,hc,alpha,z(k),u(k) )
      END IF
    ELSE IF( z(k) < Obs%SrfPrf%z(1) )THEN
      CALL vel_prof( z(k),u(k),L,zruf,hc,alpha,zsl,usl )
    END IF

  ELSE IF( Obs%SrfPrf%z(i1) >= zsl )THEN !Both levels above surface layer;

    IF( z(k) < zsl )THEN      !linear interpolation ok, unless in surface layer
      CALL set_fsl( zsl, zsl,hc,alpha,zruf,L,fz,fsl )
      CALL set_fsl( z(k),zsl,hc,alpha,zruf,L,fz,fsl )
      u(k) = u(k) * fz/fsl
    END IF

  ELSE !Lower level (and perhaps upper level) in surface layer; use surface layer interpolation
                                           !
    CALL set_fsl( z(k),zsl,hc,alpha,zruf,L,fz,fsl )
    facm = fac(k)      * fz/Obs%SrfPrf%fsl(i1)
    facp = (1.-fac(k)) * fz/Obs%SrfPrf%fsl(i2)

    u(k) = facm*uObs(i1) + facp*uObs(i2)

  END IF

END DO

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpT( InterpPrf,NearObs,tfac )

!------ Interpolate obs temperature profiles

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMvertWt

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
REAL,               INTENT( IN    ) :: tfac

INTEGER i, k, nsta, nz
REAL    HorizWt, zm

REAL,    DIMENSION(InterpPrf%nz) :: z, zt
REAL,    DIMENSION(InterpPrf%nz) :: VertWt, fac, wt, tem
INTEGER, DIMENSION(InterpPrf%nz) :: k1, k2

TYPE( ObsMet ), POINTER :: Obs

REAL, EXTERNAL :: StndTpot

SWIMobsInterpT = SWIMfailure

nsta = 0
nz   = InterpPrf%nz

!------ Construct vertical grid (AGL)

z = InterpPrf%z * InterpPrf%d

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

  IF( Obs%Tpot%nz > 0 )THEN

    nsta = nsta + 1

!------ Define horizontal weight

    HorizWt = NearObs%obs(i)%wt

!------ Vertical weights and interpolation factors

    CALL CalcVertWt( nz,z,InterpPrf%h, &
                     Obs%Tpot%nz,Obs%Tpot%z,Obs%BL%h,0.,0., &
                     VertWt,k1,k2,fac,InterpPrf%zmax )
    wt = HorizWt * VertWt * tfac

!------ Interpolate or extrapolate using Standard Atmosphere

    zt = z + InterpPrf%h

    DO k = 1,nz

      IF( zt(k) < Obs%Tpot%z(1) )THEN

        tem(k) = Obs%Tpot%obs(1)
        IF( InterpPrf%lExtrap )THEN
          zm = zt(k) + InterpPrf%Hmin
          tem(k) = tem(k) + StndTpot( zm ) - Obs%Stnd%Tbot
        END IF

      ELSE IF( zt(k) > Obs%Tpot%z(Obs%Tpot%nz) )THEN

        tem(k) = Obs%Tpot%obs(Obs%Tpot%nz)
        IF( InterpPrf%lExtrap )THEN
          zm = zt(k) + InterpPrf%Hmin
          tem(k) = tem(k) + StndTpot( zm ) - Obs%Stnd%Ttop
        END IF

      ELSE

        tem(k) = fac(k)*Obs%Tpot%obs(k1(k)) + (1.-fac(k))*Obs%Tpot%obs(k2(k))

      END IF

    END DO

!------ Sum weighted (and interpolated) temperature

    InterpPrf%Tpot%obs = InterpPrf%Tpot%obs + wt*tem
    InterpPrf%Tpot%wt  = InterpPrf%Tpot%wt  + wt

!------ Check if number of nearest obs have been used

    IF( nsta == NearObs%numInterp )EXIT StaLoop

  END IF

END DO StaLoop

SWIMobsInterpT = SWIMresult

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpP( InterpPrf,NearObs,tfac )

!------ Interpolate obs pressure profiles

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMvertWt

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
REAL,               INTENT( IN    ) :: tfac

INTEGER i, k, nsta, nz
REAL    HorizWt, zm

REAL,    DIMENSION(InterpPrf%nz) :: z, zt
REAL,    DIMENSION(InterpPrf%nz) :: VertWt, fac, wt, tem
INTEGER, DIMENSION(InterpPrf%nz) :: k1, k2

TYPE( ObsMet ), POINTER :: Obs

REAL, EXTERNAL :: StndLogP

SWIMobsInterpP = SWIMfailure

nsta = 0
nz   = InterpPrf%nz

!------ Construct vertical grid (AGL)

z = InterpPrf%z * InterpPrf%d

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

  IF( Obs%Press%nz > 0 )THEN

    nsta = nsta + 1

!------ Define horizontal weight

    HorizWt = NearObs%obs(i)%wt

!------ Vertical weights and interpolation factors

    CALL CalcVertWt( nz,z,InterpPrf%h, &
                     Obs%Press%nz,Obs%Press%z,Obs%BL%h,0.,0., &
                     VertWt,k1,k2,fac,InterpPrf%zmax )
    wt = HorizWt * VertWt * tfac

!------ Interpolate or extrapolate using Standard Atmosphere

    zt = z + InterpPrf%h

    DO k = 1,nz

      IF( zt(k) < Obs%Press%z(1) )THEN

        tem(k) = Obs%Press%obs(1)
        IF( InterpPrf%lExtrap )THEN
          zm = zt(k) + InterpPrf%Hmin
          tem(k) = tem(k) + StndLogP( zm ) - Obs%Stnd%LogPbot
        END IF

      ELSE IF( zt(k) > Obs%Press%z(Obs%Press%nz) )THEN

        tem(k) = Obs%Press%obs(Obs%Press%nz)
        IF( InterpPrf%lExtrap )THEN
          zm = zt(k) + InterpPrf%Hmin
          tem(k) = tem(k) + StndLogP( zm ) - Obs%Stnd%LogPtop
        END IF

      ELSE

        tem(k) = fac(k)*Obs%Press%obs(k1(k)) + (1.-fac(k))*Obs%Press%obs(k2(k))

      END IF

    END DO

!------ Sum weighted (and interpolated) pressure

    InterpPrf%Press%obs = InterpPrf%Press%obs + wt*tem
    InterpPrf%Press%wt  = InterpPrf%Press%wt  + wt

!------ Check if number of nearest obs have been used

    IF( nsta == NearObs%numInterp )EXIT StaLoop

  END IF

END DO StaLoop

SWIMobsInterpP = SWIMresult

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpH( InterpPrf,NearObs,tfac )

!------ Interpolate obs humidity profile
!       N.B. Work in relative humidity

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMvertWt
USE constants_fd

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
REAL,               INTENT( IN    ) :: tfac

INTEGER i, nsta, nz, k, kbot, ktop
REAL    HorizWt, zm, Px
LOGICAL lPress

REAL,    DIMENSION(InterpPrf%nz) :: z, zt
REAL,    DIMENSION(InterpPrf%nz) :: VertWt, fac, wt, tem
INTEGER, DIMENSION(InterpPrf%nz) :: k1, k2

TYPE( ObsMet ), POINTER :: Obs

REAL, EXTERNAL :: SWIMrlimit, StndLogP

SWIMobsInterpH = SWIMfailure

nsta = 0
nz   = InterpPrf%nz

!------ Construct vertical grid (AGL)

z = InterpPrf%z * InterpPrf%d

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

  IF( Obs%Humid%nz > 0 )THEN

    nsta = nsta + 1

!------ Define horizontal weight

    HorizWt = NearObs%obs(i)%wt

!------ Vertical weights and interpolation factors

    CALL CalcVertWt( nz,z,InterpPrf%h, &
                     Obs%Humid%nz,Obs%Humid%z,Obs%BL%h,0.,0., &
                     VertWt,k1,k2,fac,InterpPrf%zmax )
    wt = HorizWt * VertWt * tfac

!------ Interpolate; check if extrapolatation required

    kbot = 0
    ktop = 0

    zt = z + InterpPrf%h

    DO k = 1,nz

      IF( zt(k) < Obs%Humid%z(1) )THEN                 !Extrapolation below required
        tem(k) = Obs%Humid%obs(1)
        kbot = k
      ELSE IF( zt(k) > Obs%Humid%z(Obs%Humid%nz) )THEN !Extrapolation above required
        tem(k) = Obs%Humid%obs(Obs%Humid%nz)
        IF( ktop == 0 )ktop = k
      ELSE
        tem(k) = fac(k)*Obs%Humid%obs(k1(k)) + (1.-fac(k))*Obs%Humid%obs(k2(k))
      END IF

    END DO

!------ Extrapolation required; get obs pressure on interpolation levels if possible

    IF( InterpPrf%lExtrap .AND. (kbot > 0 .OR. ktop > 0) )THEN

      IF( ktop == 0 )ktop = nz+1

      lPress = Obs%Press%nz > 0
      IF( lPress )CALL CalcVertWt( nz,z,InterpPrf%h, &
                                   Obs%Press%nz,Obs%Press%z,Obs%BL%h,0.,0., &
                                   VertWt,k1,k2,fac,InterpPrf%zmax )

      DO k = 1,nz

        IF( k > kbot .AND. k < ktop )CYCLE

        zm = zt(k) + InterpPrf%Hmin

        IF( lPress )THEN

          IF( zt(k) < Obs%Press%z(1) )THEN
            Px = Obs%Press%obs(1) + StndLogP( zm ) - Obs%Stnd%LogPbot
          ELSE IF( zt(k) > Obs%Press%z(Obs%Press%nz) )THEN
            Px = Obs%Press%obs(Obs%Press%nz) + StndLogP( zm ) - Obs%Stnd%LogPtop
          ELSE
            Px = fac(k)*Obs%Press%obs(k1(k)) + (1.-fac(k))*Obs%Press%obs(k2(k))
          END IF

        ELSE

          Px = StndLogP( zm )

        END IF

        Px = EXP(Px)*PSURF

!------ Extrapolate using linear RH profile

        IF( k <= kbot )THEN !Extrapolate below using slope from Manabe & Wetherald (1967)

          tem(k) = Obs%Stnd%RHbot + RHSLOPE*(Px-Obs%Stnd%PHbot)
          tem(k) = SWIMrlimit( tem(k),1.,100.)

        ELSE IF(  k >= ktop )THEN !Extrapolate above towards stratosphere value

          IF( Obs%Stnd%PHtop < PSTRAT )THEN
            tem(k) = Obs%Humid%obs(Obs%Humid%nz)
          ELSE
            tem(k) = Obs%Stnd%RHtop + Obs%Stnd%extrapRHslope*(Px-Obs%Stnd%PHtop)
            tem(k) = SWIMrlimit( tem(k),1.,100.)
          END IF

        END IF

      END DO

    END IF

!------ Sum weighted (and interpolated) humidity

    InterpPrf%Humid%obs = InterpPrf%Humid%obs + wt*tem
    InterpPrf%Humid%wt  = InterpPrf%Humid%wt  + wt

!------ Check if number of nearest obs have been used

    IF( nsta == NearObs%numInterp )EXIT StaLoop

  END IF

END DO StaLoop

SWIMobsInterpH = SWIMresult

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpQ( InterpPrf,NearObs,tfac )

!------ Interpolate a cloud water

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE SWIMvertWt
USE constants_fd

IMPLICIT NONE

TYPE( ObsInterp ),  INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
REAL,               INTENT( IN    ) :: tfac

INTEGER i, nsta, nz, k, kbot, ktop
REAL    HorizWt, zm

REAL,    DIMENSION(InterpPrf%nz) :: z, zt
REAL,    DIMENSION(InterpPrf%nz) :: VertWt, fac, wt, tem
INTEGER, DIMENSION(InterpPrf%nz) :: k1, k2

TYPE( ObsMet ), POINTER :: Obs

REAL, EXTERNAL :: SWIMrlimit, StndLogP

SWIMobsInterpQ = SWIMfailure

nsta = 0
nz   = InterpPrf%nz

!------ Construct vertical grid (AGL)

z = InterpPrf%z * InterpPrf%d

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

  IF( Obs%Qcloud%nz > 0 )THEN

    nsta = nsta + 1

!------ Define horizontal weight

    HorizWt = NearObs%obs(i)%wt

!------ Vertical weights and interpolation factors

    CALL CalcVertWt( nz,z,InterpPrf%h, &
                     Obs%Qcloud%nz,Obs%Qcloud%z,Obs%BL%h,0.,0., &
                     VertWt,k1,k2,fac,InterpPrf%zmax )
    wt = HorizWt * VertWt * tfac

!------ Interpolate; set to zero above/below

    kbot = 0
    ktop = 0

    zt = z + InterpPrf%h

    DO k = 1,nz

      IF( zt(k) < Obs%Qcloud%z(1) )THEN                 !Extrapolation below required
        zm = Obs%Qcloud%z(1)-zt(k)
        IF( zm <= 10. )THEN
          tem(k) = Obs%Qcloud%obs(1)
        ELSE
          tem(k) = Obs%Qcloud%obs(1) * EXP(-0.1*(zm-10.))
        END IF
      ELSE IF( zt(k) > Obs%Qcloud%z(Obs%Qcloud%nz) )THEN !Extrapolation above required
        zm = zt(k) - Obs%Qcloud%z(Obs%Qcloud%nz)
        IF( zm <= 10. )THEN
          tem(k) = Obs%Qcloud%obs(Obs%Qcloud%nz)
        ELSE
          tem(k) = Obs%Qcloud%obs(Obs%Qcloud%nz) * EXP(-0.1*(zm-10.))
        END IF
        tem(k) = 0.
      ELSE
        tem(k) = fac(k)*Obs%Qcloud%obs(k1(k)) + (1.-fac(k))*Obs%Qcloud%obs(k2(k))
      END IF

    END DO

!------ Sum weighted (and interpolated) humidity

    InterpPrf%Qcloud%obs = InterpPrf%Qcloud%obs + wt*tem
    InterpPrf%Qcloud%wt  = InterpPrf%Qcloud%wt  + wt

!------ Check if number of nearest obs have been used

    IF( nsta == NearObs%numInterp )EXIT StaLoop

  END IF

END DO StaLoop

SWIMobsInterpQ = SWIMresult

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMobsInterpSrf( InterpPrf,NearObs,First,tfac )

!------ Interpolate surface obs

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( ObsInterp  ), INTENT( INOUT ) :: InterpPrf
TYPE( NearestObs ), INTENT( IN    ) :: NearObs
TYPE( FirstObs   ), INTENT( IN    ) :: First
REAL,               INTENT( IN    ) :: tfac

REAL    wt, ziObs
INTEGER i, nstaZi, nstaHflux, nstaL, nstaCC, nstaPr, nstaUst
LOGICAL DoneZI, DoneHflux, DoneL, DoneCC, DonePrcp, DoneUst

TYPE( ObsMet ), POINTER :: Obs

SWIMobsInterpSrf = SWIMfailure

!------ Initialize depending on which obs are available

nstaZi = 0; nstaHflux = 0; nstaL = 0; nstaCC = 0; nstaPr = 0; nstaUst = 0

DoneZi    = .NOT.BTEST(First%type,OTB_ZI)
DoneHflux = .NOT.BTEST(First%type,OTB_HFLX)
DoneUst   = .NOT.BTEST(First%type,OTB_UST)
DoneL     = .NOT.BTEST(First%type,OTB_MOL)
DoneCC    = .NOT.BTEST(First%type,OTB_CC)
DonePrcp  = .NOT.(BTEST(First%type,OTB_PRCP) .OR. BTEST(First%type,OTB_PRATE))

!------ Loop over nearest obs stations

StaLoop : DO i = 1,NearObs%numObs

  Obs => NearObs%obs(i)%obs

!------ Define horizontal weight

    wt = NearObs%obs(i)%wt * tfac

!------ Sum weighted observations

  IF( .NOT.DoneZi .AND. Obs%varSrf%zi /= OBP_NODATA )THEN
    ziObs = MAX(2.*InterpPrf%hc,Obs%varSrf%zi)
    InterpPrf%Zi%obs = InterpPrf%Zi%obs + wt*ziObs
    InterpPrf%Zi%wt  = InterpPrf%Zi%wt + wt
    nstaZi = nstaZi + 1
    DoneZi = nstaZi == NearObs%numInterp
  END IF

  IF( .NOT.DoneHflux .AND. Obs%varSrf%hflux /= OBP_NODATA )THEN
    InterpPrf%Hflux%obs = InterpPrf%Hflux%obs + wt*Obs%varSrf%hflux
    InterpPrf%Hflux%wt  = InterpPrf%Hflux%wt + wt
    nstaHflux = nstaHflux + 1
    DoneHflux = nstaHflux == NearObs%numInterp
  END IF

  IF( .NOT.DoneUst .AND. Obs%varSrf%ustr /= OBP_NODATA )THEN
    InterpPrf%Ust%obs = InterpPrf%Ust%obs + wt*Obs%varSrf%ustr
    InterpPrf%Ust%wt  = InterpPrf%Ust%wt + wt
    nstaUst = nstaUst + 1
    DoneUst = nstaUst == NearObs%numInterp
  END IF

  IF( .NOT.DoneL .AND. Obs%varSrf%invL /= OBP_NODATA )THEN
    InterpPrf%invL%obs = InterpPrf%invL%obs + wt*Obs%varSrf%invL
    InterpPrf%invL%wt  = InterpPrf%invL%wt + wt
    nstaL = nstaL + 1
    DoneL = nstaL == NearObs%numInterp
  END IF

  IF( .NOT.DoneCC .AND. Obs%varSrf%cloudcover /= OBP_NODATA )THEN
    InterpPrf%CldCv%obs = InterpPrf%CldCv%obs + wt*Obs%varSrf%cloudcover
    InterpPrf%CldCv%wt  = InterpPrf%CldCv%wt + wt
    nstaCC = nstaCC + 1
    DoneCC = nstaCC == NearObs%numInterp
  END IF

  IF( .NOT.DonePrcp .AND. Obs%varSrf%prcp /= OBP_NODATA )THEN
    IF( BTEST(First%type,OTB_PRATE) )THEN
      InterpPrf%Prcp%obs = InterpPrf%Prcp%obs + wt*Obs%varSrf%prcp
      InterpPrf%Prcp%wt  = InterpPrf%Prcp%wt + wt
      nstaPr   = nstaPr + 1
      DonePrcp = nstaPr == NearObs%numInterp
    ELSE
      InterpPrf%Prcp%obs = Obs%varSrf%prcp
      InterpPrf%Prcp%wt  = 1.0
      DonePrcp = .TRUE.
    END IF
  END IF

  IF( DoneZI .AND. DoneHflux .AND. DoneL .AND. DoneCC .AND. DonePrcp )EXIT StaLoop

END DO StaLoop

SWIMobsInterpSrf = SWIMresult

RETURN
END
