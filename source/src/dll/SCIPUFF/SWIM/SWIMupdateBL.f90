INTEGER FUNCTION SWIMupdateBL( t,fld )

!------ Update boundary layer fields to time t

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMinterpPointer

IMPLICIT NONE

REAL,             INTENT( IN    ) :: t
TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, i, ifld, jul, nx, ik
REAL    tLocal, xlon, L, zi, hflx, fac, zref
LOGICAL lSetZi, lMessage

REAL,         DIMENSION(:), POINTER :: pZi, pHf, pUs
TYPE( meth ), DIMENSION(:), POINTER :: mh

INTERFACE

  SUBROUTINE SunRiseSet( jul,lat,n,tsr,tss )
    INTEGER,            INTENT( IN  ) :: jul, n
    REAL, DIMENSION(:), POINTER       :: lat
    REAL, DIMENSION(:), POINTER       :: tsr, tss
  END SUBROUTINE SunRiseSet

  SUBROUTINE Smooth2d( fldp,grid,fld,UorV )
    USE SWIMmetField_fd
    REAL, DIMENSION(:),     POINTER      :: fldp, fld
    TYPE( MetGrid ),        INTENT( IN ) :: grid
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE Smooth2d

END INTERFACE

REAL,    EXTERNAL :: LocalTime, UTCtime, SolarTime
INTEGER, EXTERNAL :: SWIMVertDiffCoeff, JulianPrj, PostProgressMessage

SWIMupdateBL = SWIMfailure

!------- Post message, but not if creating a smooth field

IF( BTEST(fld%type,FTB_SMOOTH) )THEN
  lMessage = BTEST(field(fld%gridSource%unit)%status,FSB_DONESMOOTH)
ELSE
  lMessage = .TRUE.
END IF
IF( lMessage )THEN
  message%cString  = 'Updating Boundary Layer'
  irv = PostProgressMessage( message )
END IF

!------ Only clear second BL initial bit if first is already clear
!       N.B. Allows second call to SWIMupdateMet at start/restart to improve u*, L estimates

IF( .NOT.BTEST(fld%status,FSB_DOINITBL) )fld%status = IBCLR(fld%status,FSB_DOINITBL2)

!------ Set some auxiliary BL parameters

CALL SetUbl( fld )

!------ Compute sunrise/sunset if required

IF( fld%BLtype == BLP_CALC .OR. Prj%decay )THEN
  IF( ASSOCIATED(fld%grid%sunrise) )THEN

    jul = JulianPrj( t )
    IF( BTEST(fld%status,FSB_DOINITBL) )fld%BLaux%JulianDay = -100000
    IF( jul /= fld%BLaux%JulianDay )THEN
      CALL  SunRiseSet( jul,fld%grid%lat,fld%grid%nXY,fld%grid%sunrise,fld%grid%sunset )
      fld%BLaux%JulianDay = jul
    END IF

  END IF
END IF

IF( fld%BLtype /= BLP_CALC )fld%status = IBCLR(fld%status,FSB_DOINITBL)

!------ Define squared-velocity for ustar calculation (including ensemble variance)

CALL SetSqVel( fld )
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Set BL parameters based on type

SELECT CASE( fld%BLtype )
  CASE( BLP_CALC )  !Calculated BL

    IF( BTEST(fld%status,FSB_DOINITBL) )THEN
      CALL InitCalcBL( fld,t )
    ELSE
      CALL StepCalcBL( fld,t,t-fld%tBL )
    END IF
    IF( error%Number /= NO_ERROR )GOTO 9999

    CALL CheckZi( fld )

    CALL SetSL( fld )

  CASE( BLP_MET ) !Met (obs or gridded) input

    IF( BTEST(fld%type,FTB_MOL) )THEN
      lSetZi = .NOT.BTEST(fld%type,FTB_ZI)

!$OMP PARALLEL SHARED(fld,lSetZi)
!$OMP DO PRIVATE(i,ik,zref,L)
      DO i = 1,fld%grid%nxy
        IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
          ik   = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
          zref = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
        ELSE
          zref = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%d(i)
        END IF
        fld%BLaux%ustr2(i) = fld%BLaux%wspd2(i)
        CALL SetBLfromMOL( fld%BL%invMOL(i),fld%grid%landcover%roughness(i),&
                           fld%grid%landcover%canopyHt(i),fld%grid%landcover%alpha(i), &
                           lSetZi,L,fld%BL%zi(i),fld%BLaux%zsl(i),zref,fld%BLaux%ustr2(i) )
      END DO
!$OMP END DO
!$OMP END PARALLEL
    ELSE IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                                BTEST(fld%type,FTB_NEST)) )THEN
      CALL ZslUstr( fld )
    ELSE
      CALL ZslIter( fld )
    END IF

    CALL SetSL( fld )

    IF( BTEST(fld%type,FTB_MOL) )THEN
      DO i = 1,fld%grid%nxy
        fld%BL%HeatFlux(i) = -fld%BL%invMOL(i)*SQRT(fld%BLaux%ustr2(i))**3 / (VONK*G0/300.)
      END DO
    END IF

  CASE( BLP_PROF )   !Detailed profile met input

    CALL ProfileBL( fld )

  CASE( BLP_SBL )   !Simple BL

    tLocal = LocalTime( t )
    IF( tLocal == NOT_SET_R )THEN
      IF( ASSOCIATED(fld%grid%lon) )THEN
        xlon = 0.5*(fld%grid%lon(1)+fld%grid%lon(fld%grid%nX))
      ELSE
        xlon = 0.
      END IF
      tLocal = SolarTime( UTCtime(t),xlon )
    END IF
    CALL SetSimpleBL( tLocal,zi,hflx )

    DO i = 1,fld%grid%nxy
      fld%BL%zi(i)       = zi
      fld%BL%HeatFlux(i) = hflx
    END DO

    CALL ZslIter( fld )
    CALL SetSL( fld )

  CASE( BLP_NEST )  !Nested interpolation or averaging (including polar field)

    ifld = fld%gridSource%unit
    pZi  => field(ifld)%BL%zi
    pHf  => field(ifld)%BL%HeatFlux
    NULLIFY( pUs )

    IF( BTEST(fld%type,FTB_SMOOTH) )THEN

      CALL Smooth2d( pZi,fld%grid,fld%BL%zi )
      CALL Smooth2d( pHf,fld%grid,fld%BL%HeatFlux )

      IF( BTEST(field(ifld)%type,FTB_UST) )THEN
        pUs => field(ifld)%BLaux%ustr2
        CALL Smooth2d( pUs,fld%grid,fld%BLaux%ustr2 )
      END IF

    ELSE IF( BTEST(fld%type,FTB_NEST) )THEN

      mh => fld%GridSource%IntrpFac%mh

      DO i = 1,fld%grid%nxy
        CALL IntXY( mh(i),pZi,fld%BL%zi(i) )
        CALL IntXY( mh(i),pHf,fld%BL%HeatFlux(i) )
      END DO
      IF( BTEST(field(ifld)%type,FTB_UST) )THEN
        pUs => field(ifld)%BL%ustr
        DO i = 1,fld%grid%nxy
          CALL IntXY( mh(i),pUs,fld%BL%ustr(i) ) !Use as initial value for iteration
        END DO
        NULLIFY( pUs ) !So we use ZslIter
      END IF

    ELSE  !Polar field

      nx  = field(ifld)%grid%nX
      fac = 1./FLOAT(nx-1)
      IF( BTEST(fld%type,FTB_NPOLE) )THEN;
        i = (field(ifld)%grid%nY-1)*nx
      ELSE
        i = 0
      END IF

      fld%BL%zi(1)       = SUM(pZi(i+2:i+nx)) * fac
      fld%BL%HeatFlux(1) = SUM(pHf(i+2:i+nx)) * fac

    END IF

    IF( ASSOCIATED(pUs) )THEN
      CALL ZslUstr( fld )       !Use smoothed ustar
    ELSE
      CALL ZslIter( fld )
    END IF

    CALL SetSL( fld )

END SELECT

!------ Compute wstar

!$OMP PARALLEL SHARED(fld)
!$OMP DO PRIVATE(i)
DO i = 1,fld%grid%nXY
  IF( fld%BL%HeatFlux(i) > 0. )THEN
    fld%BLaux%wstr2(i) = (G0/300.*fld%BL%HeatFlux(i)*fld%BL%zi(i))**TwoThirds
  ELSE
    fld%BLaux%wstr2(i) = 0.0
  END IF
END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Compute coefficients for vertical diffusivity gradient (except for profile BL)

IF( fld%BLtype /= BLP_PROF )THEN
  irv = SWIMVertDiffCoeff( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( BTEST(fld%type,FTB_SMOOTH) )THEN
  lMessage = BTEST(field(fld%gridSource%unit)%status,FSB_DONESMOOTH)
ELSE
  lMessage = .TRUE.
END IF
IF( lMessage )THEN
  message%cString  = 'Finished Boundary Layer'
  irv = PostProgressMessage( message )
END IF

SWIMupdateBL = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetUbl( fld )

!------ set bl arrays using lowest level of three-dimensional arrays

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, j, i0
REAL    ztem, dum, pbl, tbl

IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
  i0 = fld%grid%nXY
ELSE
  i0 = 0
END IF

!$OMP PARALLEL DEFAULT (SHARED)
!$OMP DO PRIVATE(i,j,ztem,tbl,pbl,dum)
DO i = 1,fld%grid%nXY
  j = (fld%grid%landcover%kbl(i)-1)*fld%grid%nXY + i0 + i
  fld%BLaux%ubl(i) = fld%Field%U(j)
  fld%BLaux%vbl(i) = fld%Field%V(j)
  IF( .NOT.BTEST(fld%type,FTB_P) .OR. .NOT.BTEST(fld%type,FTB_T) )THEN
    IF( BTEST(fld%grid%type,GTB_SIGMA) )THEN
      ztem = fld%grid%sigma%Z(i) + fld%grid%terrain%H(i) + fld%grid%Hmin
    ELSE
      ztem = fld%grid%Z(1)*fld%grid%terrain%D(i) + fld%grid%terrain%H(i) + fld%grid%Hmin
    END IF
    CALL stnd_atmos( ztem,pbl,tbl,dum,SA_TA )
  END IF
  IF( BTEST(fld%type,FTB_P) )THEN
    fld%BLaux%pbl(i) = EXP(fld%Field%Press(j))  !pressure ratio
  ELSE
    fld%BLaux%pbl(i) = pbl
  END IF
  IF( BTEST(fld%type,FTB_T) )THEN
    fld%BLaux%tbl(i) = fld%Field%Tpot(j)*fld%BLaux%pbl(i)**KAPPA
  ELSE
    fld%BLaux%tbl(i) = tbl
  END IF
  fld%BLaux%pbl(i) = fld%BLaux%pbl(i)*PSURF
END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END

!==============================================================================

SUBROUTINE SetSqVel( fld )

!------ Define squared-velocity used for Ustar calculation

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

CALL SetSqVel0( fld,fld%BL,fld%BLaux )

RETURN
END

!==============================================================================

SUBROUTINE SetSqVel0( fld,BL,BLaux )

!------ Define squared-velocity used for Ustar calculation

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer
USE SWIMinterp_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ),   INTENT( INOUT ) :: fld
TYPE( MetBLparam ), INTENT( INOUT ) :: BL
TYPE( MetBLaux   ), INTENT( INOUT ) :: BLaux

INTEGER alloc_stat
INTEGER i, j, i0, nx, ny, nxy, ik
INTEGER i0m, itl, ibr, ibl, iu, iv
REAL    zbl, L, fac

REAL, DIMENSION(:), ALLOCATABLE :: uuFac, u2
REAL, EXTERNAL :: LfromInvL, ulog

nxy = fld%grid%nXY
nx  = fld%grid%nX
ny  = fld%grid%nY

!------ Use lowest level wind components to compute ustar
!       unless ustar is explicit input

!$OMP PARALLEL DO PRIVATE(i) SHARED(BLaux,nxy)
DO i = 1,nxy
  BLaux%wspd2(i) = BLaux%ubl(i)**2 + BLaux%vbl(i)**2
END DO
!$OMP END PARALLEL DO

IF( BTEST(fld%grid%type,GTB_STAGGERB) )THEN
!$OMP PARALLEL DO PRIVATE(j,i0,i0m,i,ik,itl,ibr,ibl) SHARED(BLaux,nx,ny)
  DO j = 1,ny
    i0  = (j-1)*nx
    i0m = (MAX(j-1,1)-1)*nx
    DO i = 1,nx
      ik  = i0 + i
      itl = i0 + MAX(i-1,1)
      ibr = i0m + i
      ibl = i0m + MAX(i-1,1)
      BLaux%wspd2(ik) = 0.25*( BLaux%wspd2(ik) + BLaux%ubl(itl)**2 + BLaux%vbl(itl)**2   &
                                               + BLaux%ubl(ibr)**2 + BLaux%vbl(ibr)**2  &
                                               + BLaux%ubl(ibl)**2 + BLaux%vbl(ibl)**2 )
    END DO
  END DO
!$OMP END PARALLEL DO
ELSE IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
!$OMP PARALLEL DO PRIVATE(j,i0,i0m,i,ik,iu,iv) SHARED(BLaux,nx,ny)
  DO j = 1,ny
    i0  = (j-1)*nx
    i0m = (MAX(j-1,1)-1)*nx
    DO i = 1,nx
      ik = i0 + i
      iu = i0 + MAX(i-1,1)
      iv = i0m + i
      BLaux%wspd2(ik) = 0.5*( BLaux%wspd2(ik) + BLaux%ubl(iu)**2 + BLaux%vbl(iv)**2 )
    END DO
  END DO
!$OMP END PARALLEL DO
END IF

!------ Compute reduction factor for LSV and/or Met Uncertainty
!       and UUcalm (as minimum wind speed)
!       N.B. Use Zsl and L from previous time step

ALLOCATE( uuFac(nxy),u2(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetSqVel'
  error%Message = 'Error allocating LSV / Met Uncertainty reduction factor array'
  GOTO 9999
END IF

!------ If using ustar as input, save u**2 for later

IF( BTEST(fld%type,FTB_UST) .AND. .NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                        BTEST(fld%type,FTB_NEST)) )THEN
!$OMP PARALLEL DO PRIVATE(i) SHARED(BLaux,u2,nxy)
  DO i = 1,nxy
    u2(i) = BLaux%wspd2(i)
    BLaux%wspd2(i) = 0.
  END DO
!$OMP END PARALLEL DO
ELSE
!$OMP PARALLEL DO PRIVATE(i) SHARED(u2,nxy)
  DO i = 1,nxy
    u2(i) = 0.
  END DO
!$OMP END PARALLEL DO
END IF

!$OMP PARALLEL SHARED(fld,BLaux,BL,uuFac)
!$OMP DO PRIVATE(i,ik,zbl,L)
DO i = 1,nxy
  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
    ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
    zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
  ELSE
    zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%D(i)
  END IF

  IF( BLaux%zsl(i) > zbl )THEN

    L = LfromInvL( BL%invMOL(i) )

    uuFac(i) = ulog( fld%grid%landcover%canopyHt(i), &
                     fld%grid%landcover%alpha(i),    &
                     zbl,                            &
                     fld%grid%landcover%roughness(i),&
                     L )                             &
                         /                          &
               ulog( fld%grid%landcover%canopyHt(i), &
                     fld%grid%landcover%alpha(i),    &
                     BLaux%zsl(i),               &
                     fld%grid%landcover%roughness(i),&
                     L )

    uuFac(i) = uuFac(i)**2

  ELSE

    uuFac(i) = 1.0

  END IF

END DO
!$OMP END DO
!$OMP END PARALLEL

IF( fld%BLtype /= BLP_PROF )THEN

!------ Add large-scale variability

SELECT CASE( fld%LSVtype )

  CASE( LVP_INPUT )
!$OMP PARALLEL SHARED(fld,BLaux,uuFac)
!$OMP DO PRIVATE(i)
    DO i = 1,nxy
      BLaux%wspd2(i) = BLaux%wspd2(i) + (fld%LSV%UU(1) + fld%LSV%VV(1)) * uuFac(i)
    END DO
!$OMP END DO
!$OMP END PARALLEL

  CASE( LVP_MET )
!$OMP PARALLEL SHARED(fld,BLaux)
!$OMP DO PRIVATE(i)
    DO i = 1,nxy
      BLaux%wspd2(i) = BLaux%wspd2(i) + fld%LSV%UU(i) + fld%LSV%VV(i)
    END DO
!$OMP END DO
!$OMP END PARALLEL

  CASE( LVP_MODEL )
    nx = fld%grid%nX
!$OMP PARALLEL SHARED(fld,BLaux,uuFac)
!$OMP DO PRIVATE(j,i0,i)
    DO j = 1,fld%grid%nY
      i0 = (j-1)*nx
      DO i = 1,nx
        BLaux%wspd2(i0+i) = BLaux%wspd2(i0+i) + (fld%LSV%UU(i0+i) + fld%LSV%VV(i0+i)) * uuFac(i0+i)
      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

END SELECT

END IF

!------ Set aux ustar^2 for case where ustar is given on met input
!       N.B. Increase ustar to account for LSV or wind uncertainty

IF( BTEST(fld%type,FTB_UST) .AND..NOT.(BTEST(fld%type,FTB_OBS) .OR. &
                                       BTEST(fld%type,FTB_NEST)) )THEN

!$OMP PARALLEL SHARED(nxy,BLaux,BL,fld,u2,uuFac,Prj)
!$OMP DO PRIVATE(i,ik,zbl,L,fac)
  DO i = 1,nxy

    IF( BLaux%wspd2(i) == 0. )THEN  !If no LSV or uncertainty, use input ustar

      BLaux%ustr2(i) = MAX(BL%ustr(i)**2,1.E-6)

    ELSE

      u2(i) = MAX(u2(i),Prj%BL%UUcalm*uuFac(i),Prj%BL%WWtrop,1.E-6)
      IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
        ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
        zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
      ELSE
        zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%D(i)
      END IF
      zbl = MIN(zbl,BLaux%zsl(i))
      L = LfromInvL( BL%invMOL(i) )
      fac = VONK/ulog( fld%grid%landcover%canopyHt(i), &
                       fld%grid%landcover%alpha(i),    &
                       zbl,                            &
                       fld%grid%landcover%roughness(i),&
                       L )
      BLaux%ustr2(i) = (u2(i)+BLaux%wspd2(i))*MIN(BL%ustr(i)**2/u2(i),fac**2)
      BLaux%ustr2(i) = MAX(BLaux%ustr2(i),BL%ustr(i)**2)
      BLaux%ustr2(i) = MAX(BLaux%ustr2(i),1.E-6)

    END IF

  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

!------ Ensure minimum value

!$OMP PARALLEL SHARED(BLaux,uuFac,Prj)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy
    BLaux%wspd2(i) = MAX(BLaux%wspd2(i),uuFac(i)*MIN(Prj%BL%UUcalm,Prj%BL%WWtrop),1.E-6)
  END DO
!$OMP END DO
!$OMP END PARALLEL

END IF

9999 CONTINUE

IF( ALLOCATED(uuFac) )DEALLOCATE( uuFac,STAT=alloc_stat )
IF( ALLOCATED(u2)    )DEALLOCATE( u2,   STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE ZslIter( fld )

!------ Iterate to set surface layer height since zsl depends on xmol2,
!       which depends on zsl ...

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, ik
REAL    tref, pref, wspd, rn, rlat, ustar, zbl, L
LOGICAL ladv_zi, got_hflx, tflag, pflag

REAL, EXTERNAL :: InvLfromL

ladv_zi  = .FALSE.
got_hflx = .TRUE.
rn       = NOT_SET_R
rlat     = NOT_SET_R
tflag    = BTEST(fld%type,FTB_T)
pflag    = BTEST(fld%type,FTB_P)

!$OMP PARALLEL SHARED(fld,ladv_zi,got_hflx,rn,rlat,tflag,pflag)
!$OMP DO PRIVATE(i,tref,pref,wspd,ik,zbl,ustar,L)
DO i = 1,fld%grid%nxy

  CALL SetSurfRef( tflag,pflag,fld%BLaux%tbl(i),fld%BLaux%pbl(i), &
                     fld%grid%terrain%H(i)+fld%grid%Hmin,tref,pref )

  wspd = SQRT(fld%BLaux%wspd2(i))
  IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
    ik  = (fld%grid%landcover%kbl(i)-1)*fld%grid%nxy + i
    zbl = MAX(fld%grid%sigma%Z(ik),10.*fld%grid%landcover%roughness(i))
  ELSE
    zbl = fld%grid%Z(fld%grid%landcover%kbl(i))*fld%grid%terrain%D(i)
  END IF

  IF( BTEST(fld%type,FTB_UST) )THEN
    ustar = fld%BL%ustr(i)           !Use as initial condition, but not fixed
  ELSE
    ustar = -1.
  END IF
  CALL SurfIter( zbl,wspd,tref,pref,fld%grid%landcover%roughness(i), &
                                    fld%grid%landcover%canopyHt(i),   &
                                    fld%grid%landcover%alpha(i), &
                                    fld%grid%landcover%Bowen(i), &
                                    fld%BL%cc(i),rn,rlat,ladv_zi,got_hflx, &
                                    fld%BL%HeatFlux(i), &
                                    fld%BL%zi(i),fld%BLaux%zsl(i),L,ustar,.FALSE. )
  fld%BLaux%ustr2(i) = ustar**2
  fld%BL%invMOL(i)   = InvLfromL( L )

END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END

!===============================================================================

SUBROUTINE ZslUstr( fld )

!------ Set surface layer height and L for case where ustar and heat flux are
!       provided on met input
!       N.B. Assumes fld%BLaux%wspd2 has been set in SetSqVel.

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i
REAL    tref, pref, L
LOGICAL tflag, pflag

REAL, EXTERNAL :: LfromInvL

tflag = BTEST(fld%type,FTB_T)
pflag = BTEST(fld%type,FTB_P)

!$OMP PARALLEL SHARED(fld,tflag,pflag)
!$OMP DO PRIVATE(i,tref,pref,L)
DO i = 1,fld%grid%nxy

  CALL SetSurfRef( tflag,pflag,fld%BLaux%tbl(i),fld%BLaux%pbl(i), &
                   fld%grid%terrain%H(i)+fld%grid%Hmin,tref,pref )

  fld%BL%invMOL(i) = -VONK*G0*fld%BL%HeatFlux(i)/tref / SQRT(fld%BLaux%ustr2(i))**3

  L = LfromInvL( fld%BL%invMOL(i) )

  CALL set_zsl( fld%BLaux%zsl(i),fld%BL%zi(i),L, &
                fld%grid%landcover%roughness(i),fld%grid%landcover%canopyHt(i) )
END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMVertDiffCoeff( fld )

!------ Set vertical diffusivity coefficients

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER i, is, k
REAL    dzbl, zbl, zi, hc, alpha, zruf
REAL    uutem, vvtem, wwtem, qqtem, sltem, sztem
REAL    wwztem
REAL    zi_min

SWIMVertDiffCoeff = SWIMfailure

dzbl = 1.0/FLOAT(fld%BLaux%nz-1)

zi_min = HUGE(0.)
!$OMP PARALLEL DO PRIVATE(is) SHARED(fld) REDUCTION(MIN:zi_min)
DO is = 1,fld%grid%nxy
  zi_min = MIN(zi_min,fld%BL%zi(is))
END DO
!$OMP END PARALLEL DO
IF( zi_min <= 0. )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMVertDiffCoeff'
  error%Message = 'Invalid mixing depth'
  error%Inform  = 'Must be greater than 0'
  GOTO 9999
END IF

!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(fld,dzbl)
!$OMP DO
DO is = 1,fld%grid%nxy

  zi = fld%BL%zi(is)

    hc    = MAX(fld%grid%landcover%canopyHt(is),0.)
    alpha = fld%grid%landcover%alpha(is)
    zruf  = fld%grid%landcover%roughness(is)

    DO k = 1,fld%BLaux%nz
      i = (k-1)*fld%grid%nxy + is
      zbl = FLOAT(k-1)*dzbl
      CALL blturb( zbl,zi, &
                   fld%BLaux%ustr2(is),fld%BLaux%wstr2(is),fld%BL%HeatFlux(is), &
                   uutem,vvtem,wwtem,qqtem,wwztem,sltem,sztem, &
                   fld%BLaux%aDiff(i),fld%BLaux%bDiff(i), &
                   hc,alpha,zruf )
      IF( k == 1 )THEN
        fld%BLaux%uu_sh(is)   = uutem
        fld%BLaux%uu_buoy(is) = vvtem
        fld%BLaux%sbl(is)     = sltem
      END IF

    END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

SWIMVertDiffCoeff = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMsetSunFac( t,grid )

!------ Set sine of sun angle

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

REAL,            INTENT( IN    ) :: t
TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER i, j, is, jul_met
REAL    tUTC, sun_ang, t_local, frac

INTEGER, EXTERNAL :: JulianPrj
REAL,    EXTERNAL :: UTCTime, SolarTime, LocalTime, sind

SWIMsetSunFac = SWIMfailure

IF( ASSOCIATED(grid%lat) )THEN

  jul_met = JulianPrj( t )
  tUTC    = UTCTime( t )

!$OMP PARALLEL SHARED(grid,jul_met,tUTC)
!$OMP DO PRIVATE(j,i,is,t_local,sun_ang)
  DO j = 1,grid%nY
    DO i = 1,grid%nX
      is = (j-1)*grid%nX + i
      IF( tUTC == NOT_SET_R )THEN
        t_local = LocalTime( t )
      ELSE
        t_local = SolarTime( tUTC,grid%lon(is) )
      END IF
      IF( t_local > grid%sunrise(is) .AND. t_local < grid%sunset(is) )THEN
        CALL sun( grid%lat(is),jul_met,t_local,sun_ang )
        grid%sunfac(is) = sind(sun_ang)
     ELSE
        grid%sunfac(is) = 0.
      END IF
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

ELSE

  IF( Prj%local )THEN
    frac = (t/3600. + Prj%hourStart)/24.0
  ELSE
    frac = (t/3600. + Prj%hourStart + Prj%timeZone)/24.0
  END IF
  frac = frac - INT(frac)
  IF( frac < 0. )frac = frac + 1.
  IF( frac > 0.25 .AND. frac < 0.75 )THEN
    sun_ang = SIN((frac-0.25)*PI2)
  ELSE
    sun_ang = 0.
  END IF
  DO i = 1,grid%nXY
    grid%sunfac(i) = sun_ang
  END DO

END IF

SWIMsetSunFac = SWIMresult

RETURN
END
