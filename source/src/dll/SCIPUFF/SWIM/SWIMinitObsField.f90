INTEGER FUNCTION SWIMinitObsField( fld,metType )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE VertGrid_fd
USE constants_fd
USE message_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld
INTEGER,          INTENT( IN    ) :: metType

INTEGER irv, alloc_stat, nmax, jObs, i, k, i0, k0
INTEGER nxy, nz, nxyz
REAL    zmax, ztem, pr, ta, dum
REAL    InitHflx, InitZi, tLocal, x, y, lat, lon
LOGICAL lRifl

TYPE( VertGrid ) :: vgrid

TYPE ( messageT ) :: caution

REAL,    DIMENSION(:,:), ALLOCATABLE :: zb
INTEGER, DIMENSION(:),   ALLOCATABLE :: nzb

INTERFACE
  INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )
    USE SWIM_fi
    REAL, DIMENSION(:),      POINTER         :: zMC
    INTEGER,                 INTENT( IN    ) :: nzMC
    TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid
  END FUNCTION SWIMinitMcWIF
END INTERFACE

INTEGER, EXTERNAL :: SWIMreadTerrain
INTEGER, EXTERNAL :: SWIMalloc3dField, SWIMallocBLParam, SWIMallocBLaux
INTEGER, EXTERNAL :: SWIMallocObsWt, SWIMallocBLprof
INTEGER, EXTERNAL :: SetBLType, SWIMgetLL
REAL,    EXTERNAL :: LocalTime, UTCtime, SolarTime, StndRelativeHumid
INTEGER, EXTERNAL :: PostCautionMessage

SWIMinitObsField = SWIMfailure

!------ Set times as "not set"

fld%t     = NOT_SET_R
fld%tNext = NOT_SET_R
fld%tBL   = NOT_SET_R

!------ Set initial status

fld%status = 0
fld%status = IBSET(fld%status,FSB_DOINIT)
fld%status = IBSET(fld%status,FSB_UPDATE)

NULLIFY( vgrid%z )

!------ Set field type based on available variables from obs sources

CALL SetFieldTypeObs( fld )

!------ Set field grid type

fld%grid%type = 0
CALL SetFieldGridMapType( fld%grid%type )


!------ Check for consistency of time reference

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( ObsSrc(jObs)%local .NEQV. Prj%localMet )THEN
    caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMinitObsField'
    caution%aString = 'Obs file inconsistent with overall time references'
    IF( ObsSrc(jObs)%local )THEN
      caution%bString = TRIM(ObsSrc(jObs)%Source)//' is LOCAL'
      caution%cString = 'Overall is UTC'
    ELSE
      caution%bString = TRIM(ObsSrc(jObs)%Source)//' is UTC'
      caution%cString = 'Overall is LOCAL'
    END IF
    irv = PostCautionMessage( caution )
  END IF
END DO

!------ Check for use of influence radius

lRifl = .TRUE.
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( .NOT.BTEST(ObsSrc(jObs)%type,OTB_RIFL) )lRifl = .FALSE.
END DO

IF(.NOT.lRifl )THEN
DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
    ObsSrc(jObs)%type = IBCLR(ObsSrc(jObs)%type,OTB_RIFL)
  END DO
END IF

!------ Set surface parameters that may be "not set" based on landuse category

CALL SetPrjBL()
IF( error%Number  /= NO_ERROR )GOTO 9999

!------ Read terrain file if available

IF( BTEST(Prj%MC%type,MCB_TER) .OR. BTEST(Prj%MC%type,MCB_LC) )THEN

  irv = SWIMreadTerrain( fld%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

  fld%type = IBSET(fld%type,FTB_W)  !Set vertical velocity bit

!------ Clear SWIFT if requested; change to McWif

  IF( BTEST(Prj%MC%type,MCB_SWIFT) )THEN

    Prj%MC%type = IBCLR(Prj%MC%type,MCB_SWIFT)
    Prj%MC%type = IBSET(Prj%MC%type,MCB_MCWIF)

  END IF

!------ Initialize McWif (if not using SWIFT)

  IF( BTEST(Prj%MC%type,MCB_MCWIF) )THEN

    irv = SWIMinitMcWIF( Prj%MC%Z,Prj%MC%nZ,fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999

    fld%type = IBSET(fld%type,FTB_MCWIF)

  ELSE

    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitObsField'
    error%Message = 'Invalid mass-consistent model type'
    GOTO 9999

  END IF

  CALL SWIMsetPrjCoord()

ELSE

!------ Check that project grid is defined

  IF( prj%Xmin == NOT_SET_R .OR. prj%Xmin == DEF_VAL_R .OR. &
      prj%Ymin == NOT_SET_R .OR. prj%Ymin == DEF_VAL_R .OR. &
      prj%Xmax == NOT_SET_R .OR. prj%Xmax == DEF_VAL_R .OR. &
      prj%Ymax == NOT_SET_R .OR. prj%Ymax == DEF_VAL_R )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinintObsField'
    error%Message = 'Unable to define a Project domain from Observations.'
    error%Action = 'Options: Provide a domain, Add a terrain file, Switch to gridded meteorology'
    GOTO 9999
  END IF

  fld%grid%coord = PrjCoord

!------ Default horizontal grid is a single point

  fld%grid%nX = 1; fld%grid%nY = 1; fld%grid%nXY = 1

  fld%grid%Xmin = prj%xmin;  fld%grid%Xmax = prj%xmax
  fld%grid%Ymin = prj%ymin;  fld%grid%Ymax = prj%ymax

  fld%grid%dX = prj%xmax - prj%xmin
  fld%grid%dY = prj%ymax - prj%ymin

  ALLOCATE( fld%grid%terrain%H(1),fld%grid%terrain%D(1), &
            fld%grid%terrain%Du(1),fld%grid%terrain%Dv(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinintObsField'
    error%Message = 'Error allocating single point terrain arrays'
    GOTO 9999
  END IF

  fld%grid%terrain%H(1)  = 0.
  fld%grid%terrain%D(1)  = 1.
  fld%grid%terrain%Du(1) = 1.; fld%grid%terrain%Dv(1) = 1.

  fld%grid%Hmin = 0.

  ALLOCATE( fld%grid%landcover%roughness(1),fld%grid%landcover%canopyHt(1), &
            fld%grid%landcover%alpha(1),fld%grid%landcover%Bowen(1), &
            fld%grid%landcover%albedo(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinintObsField'
    error%Message = 'Error allocating single point landuse arrays'
    GOTO 9999
  END IF

    fld%grid%landcover%roughness(1) = Prj%BL%zruf
    fld%grid%landcover%canopyHt(1)  = Prj%BL%hc
    fld%grid%landcover%alpha(1)     = Prj%BL%alpha
    fld%grid%landcover%Bowen(1)     = Prj%BL%Bowen
    fld%grid%landcover%albedo(1)    = Prj%BL%albedo

!------ Build vertical grid by blending multiple obs sources

  nmax = 0
  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    nmax = MAX(nmax,ObsSrc(jObs)%nz)
  END DO

  ALLOCATE( zb(nmax,fld%nObsSource),nzb(fld%nObsSource),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinintObsField'
    error%Message = 'Error allocating vertical grid arrays'
    GOTO 9999
  END IF

  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    nzb(jObs) = ObsSrc(jObs)%nz
    DO k = 1,nzb(jObs)
      zb(k,jObs) = ObsSrc(jObs)%z(k)
    END DO
    IF( nzb(jObs) < nmax ) THEN
      DO k = nzb(jObs)+1,nmax
        zb(k,jObs) = -999.
      END DO
    END IF
  END DO

  zmax = -999.
  DO i = 1,fld%nObsSource
    jObs = fld%iObsSource(i)
    DO k = 1,nzb(jObs)
      zmax = MAX(zb(k,jObs),zmax)
    END DO
  END DO

!(SFP) Combine grids in a way consistent with SCIP 3.2
!      Note:  Calling build_vert_grid here exposes the bug in building height list
!             Its possible to consistently throw out the lower of the two
!             points being checked.  This can result in excessive spacing
!(SFP) Use Project domain for zmax instead of max obs height for consistency with 3.2
!  CALL build_vert_grid( zb,nzb,nmax,zmax,numObsSrc,vgrid )
!  CALL build_vert_grid( zb,nzb,nmax,Prj%Zmax,numObsSrc,vgrid )
!  IF( error%Number /= NO_ERROR )GOTO 9999
  IF( fld%nObsSource == 1 )THEN
    ALLOCATE( vgrid%z(ObsSrc(1)%nz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinintObsField'
      error%Message = 'Error allocating vertical grid work array'
      GOTO 9999
    END IF
    vgrid%nz = ObsSrc(1)%nz
    vgrid%z  = ObsSrc(1)%z
  ELSE
    IF( ObsSrc(1)%nz > ObsSrc(2)%nz )THEN
      IF( ObsSrc(1)%z(1) <= 20.0 )THEN
        ALLOCATE( vgrid%z(ObsSrc(1)%nz),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinintObsField'
          error%Message = 'Error allocating vertical grid work array'
          GOTO 9999
        END IF
        vgrid%nz = ObsSrc(1)%nz
        DO i = 1,vgrid%nz
          vgrid%z(i)  = ObsSrc(1)%z(i)
        END DO
      ELSE
        ALLOCATE( vgrid%z(ObsSrc(1)%nz+1),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinintObsField'
          error%Message = 'Error allocating vertical grid work array'
          GOTO 9999
        END IF
        vgrid%nz = ObsSrc(1)%nz+1
        vgrid%z(1) = 10.0
        DO i = 1,ObsSrc(1)%nz
          vgrid%z(i+1) = ObsSrc(1)%z(i)
        END DO
      END IF
    ELSE
      IF( ObsSrc(2)%z(1) <= 20.0 )THEN
        ALLOCATE( vgrid%z(ObsSrc(2)%nz),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinintObsField'
          error%Message = 'Error allocating vertical grid work array'
          GOTO 9999
        END IF
        vgrid%nz = ObsSrc(2)%nz
        DO i = 1,vgrid%nz
          vgrid%z(i) = ObsSrc(2)%z(i)
        END DO
      ELSE
        ALLOCATE( vgrid%z(ObsSrc(2)%nz+1),STAT=alloc_stat )
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinintObsField'
          error%Message = 'Error allocating vertical grid work array'
          GOTO 9999
        END IF
        vgrid%nz = ObsSrc(2)%nz+1
        vgrid%z(1) = 10.0
        DO i = 1,ObsSrc(2)%nz
          vgrid%z(i+1) = ObsSrc(2)%z(i)
        END DO
      END IF
    END IF
  END IF

  fld%grid%nZ = vgrid%nz
  ALLOCATE( fld%grid%Z(fld%grid%nZ),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinintObsField'
    error%Message = 'Error allocating vertical grid array'
    GOTO 9999
  END IF

  DO i = 1,vgrid%nz
    fld%grid%Z(i) = vgrid%z(i) ! + Prj%BL%hc  !Add uniform canopy height (only for non-terrain projects)
  END DO
  fld%grid%Ztop = fld%grid%Z(fld%grid%nZ)

  DEALLOCATE( zb,nzb,vgrid%z,STAT=alloc_stat )

!  fld%grid%coord = PrjCoord

END IF

!------ Set grid sizes for allocating

nz = fld%grid%nZ;
IF( BTEST(fld%grid%type,GTB_STAGGER) )nz = nz + 1

nxy = fld%grid%nXY; nxyz = nxy*nz

IF(  BTEST(fld%type,FTB_UU) )THEN
  fld%BLaux%nz = nz
ELSE
  fld%BLaux%nz = Prj%BL%nzbl
END IF

!------ Allocate met fields

IF( nxy >= 6 )fld%type = IBSET(fld%type,FTB_DU2)

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,nxyz,fld%NextField )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMallocBLParam( fld%type,nxy,fld%BL,fld%NextBL )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMallocBLaux( nxy,fld%BLaux )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%type,FTB_UU) )THEN

  irv = SWIMallocBLprof( nxyz,fld%BLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

  irv = SWIMallocBLprof( nxyz,fld%NextBLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

irv = SWIMallocObsWt( fld%type,nxy,nxyz,fld%obsWt )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize fields

DO i = 1,nxyz
  fld%Field%U(i) = 0.; fld%NextField%U(i) = 0.
  fld%Field%V(i) = 0.; fld%NextField%V(i) = 0.
END DO

IF( BTEST(fld%type,FTB_W) )THEN
  DO i = 1,nxyz
    fld%Field%W(i) = 0.; fld%NextField%W(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_QCLD) )THEN
  DO i = 1,nxyz
    fld%Field%Qcloud(i) = 0.; fld%NextField%Qcloud(i) = 0.
  END DO
END IF

!------ Initialize Temperature and/or Pressure and/or humidity with standard atmosphere

IF( BTEST(fld%type,FTB_T) .OR. BTEST(fld%type,FTB_P) .OR. BTEST(fld%type,FTB_H) )THEN

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    k0 = nxy
  ELSE
    k0 = 0
  END IF

  DO k = 1,fld%grid%nZ
    i0 = k0 + (k-1)*nxy
    DO i = 1,nxy
      ztem = fld%grid%Hmin + fld%grid%terrain%H(i) + fld%grid%z(k)*fld%grid%terrain%d(i)
      CALL stnd_atmos( ztem,pr,ta,dum,SA_TPOT )
      IF( BTEST(fld%type,FTB_T) )THEN
        fld%Field%Tpot(i0+i)     = ta
        fld%NextField%Tpot(i0+i) = ta
      END IF
      IF( BTEST(fld%type,FTB_P) )THEN
        fld%Field%Press(i0+i)     = LOG(pr)
        fld%NextField%Press(i0+i) = fld%Field%Press(i0+i)
      END IF
      IF( BTEST(fld%type,FTB_H) )THEN
        fld%Field%Humid(i0+i)     = StndRelativeHumid( pr*PSURF )
        fld%NextField%Humid(i0+i) = fld%Field%Humid(i0+i)
      END IF
    END DO
  END DO

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN  !Bottom slice
    DO i = 1,nxy
      IF( BTEST(fld%type,FTB_T) )THEN
        fld%Field%Tpot(i)     = fld%Field%Tpot(nxy+i)
        fld%NextField%Tpot(i) = fld%NextField%Tpot(nxy+i)
      END IF
      IF( BTEST(fld%type,FTB_P) )THEN
        fld%Field%Press(i)     = fld%Field%Press(nxy+i)
        fld%NextField%Press(i) = fld%NextField%Press(nxy+i)
      END IF
      IF( BTEST(fld%type,FTB_H) )THEN
        fld%Field%Humid(i)     = fld%Field%Humid(nxy+i)
        fld%NextField%Humid(i) = fld%NextField%Humid(nxy+i)
      END IF
    END DO
  END IF

END IF

!------ Set grid level for setting surface layer profile (above canopy)

CALL set_kbl( fld%grid )
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Initialize BL parameters (in case first observations are missing)

IF( BTEST(fld%type,FTB_ZI) .OR. BTEST(fld%type,FTB_HFLX) )THEN

  tLocal = LocalTime( Prj%time )
  IF( tLocal == NOT_SET_R )THEN
    x = 0.5*(fld%grid%xMin+fld%grid%xMax); y = 0.5*(fld%grid%yMin+fld%grid%yMax)
    IF( SWIMgetLL( x,y,lat,lon ) == SWIMfailure )lon = 0.
    tLocal = SolarTime( UTCtime(Prj%time),lon )
  END IF

  CALL SetSimpleBL( tLocal,InitZi,InitHflx )

  IF( BTEST(fld%type,FTB_ZI) )THEN
    DO i = 1,nxy
      fld%BL%zi(i) = InitZI; fld%NextBL%zi(i) = InitZI
    END DO
  END IF

  IF( BTEST(fld%type,FTB_HFLX) )THEN
    DO i = 1,nxy
      fld%BL%HeatFlux(i) = InitHflx; fld%NextBL%HeatFlux(i) = InitHflx
    END DO
  END IF

END IF

IF( BTEST(fld%type,FTB_UST) )THEN
  DO i = 1,nxy
    fld%BL%ustr(i) = 1.E-1; fld%NextBL%ustr(i) = 1.E-1
  END DO
END IF

IF( BTEST(fld%type,FTB_MOL) )THEN
  DO i = 1,nxy
    fld%BL%invMOL(i) = 1.E-4; fld%NextBL%invMOL(i) = 1.E-4
  END DO
END IF

IF( BTEST(fld%type,FTB_CLDCV) )THEN
  DO i = 1,nxy
    fld%BL%cc(i) = 0.; fld%NextBL%cc(i) = 0.
  END DO
END IF

IF( BTEST(fld%type,FTB_PRCP) .OR.  BTEST(fld%type,FTB_PRATE) )THEN
  DO i = 1,nxy
    fld%BL%prcp(i) = 0.; fld%NextBL%prcp(i) = 0.
  END DO
  IF( BTEST(fld%type,FTB_PRCP) .AND.  BTEST(fld%type,FTB_PRATE) )fld%type = IBCLR(fld%type,FTB_PRCP)
END IF

IF( BTEST(fld%type,FTB_LAI) )THEN
  DO i = 1,nxy
    fld%BL%LAI(i) = 0.; fld%NextBL%LAI(i) = 0.
  END DO
END IF

!------ Initialize turbulence profiles

IF( BTEST(fld%type,FTB_UU) )THEN

  DO i = 1,SIZE(fld%BLprof%UU)
    fld%BLprof%UU(i) = 0.; fld%NextBLprof%UU(i) = 0.
    fld%BLprof%VV(i) = 0.; fld%NextBLprof%VV(i) = 0.
    fld%BLprof%WW(i) = 0.; fld%NextBLprof%WW(i) = 0.
    fld%BLprof%WT(i) = 0.; fld%NextBLprof%WT(i) = 0.
    fld%BLprof%SL(i) = 0.; fld%NextBLprof%SL(i) = 0.
    fld%BLprof%SZ(i) = 0.; fld%NextBLprof%SZ(i) = 0.
  END DO

END IF

!------ Set/check boundary layer type

irv = SetBLType( fld )
IF( irv /= SWIMsuccess )GOTO 9999

DO i = 1,nxy
  fld%BL%cc(i) = Prj%BL%cc
END DO

SWIMinitObsField = SWIMresult

9999 CONTINUE

IF( ALLOCATED(zb)       )DEALLOCATE( zb,STAT=alloc_stat )
IF( ALLOCATED(nzb)      )DEALLOCATE( nzb,STAT=alloc_stat )
IF( ASSOCIATED(vgrid%z) )DEALLOCATE( vgrid%z,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE SetFieldTypeObs( fld )

!------ Set field type bits based on available observations

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER(8) type
INTEGER jObs
INTEGER i

type = IBSET(0,FTB_OBS)

DO i = 1,fld%nObsSource
  jObs = fld%iObsSource(i)
  IF( BTEST(ObsSrc(jObs)%type,OTB_T   ) )type = IBSET(type,FTB_T)
  IF( BTEST(ObsSrc(jObs)%type,OTB_P   ) )type = IBSET(type,FTB_P)
  IF( BTEST(ObsSrc(jObs)%type,OTB_H   ) )type = IBSET(type,FTB_H)
  IF( BTEST(ObsSrc(jObs)%type,OTB_QCLD) )type = IBSET(type,FTB_QCLD)
  IF( BTEST(ObsSrc(jObs)%type,OTB_ZI  ) )type = IBSET(type,FTB_ZI)
  IF( BTEST(ObsSrc(jObs)%type,OTB_HFLX) )type = IBSET(type,FTB_HFLX)
  IF( BTEST(ObsSrc(jObs)%type,OTB_MOL ) )type = IBSET(type,FTB_MOL)
  IF( BTEST(ObsSrc(jObs)%type,OTB_UST ) )type = IBSET(type,FTB_UST)
  IF( BTEST(ObsSrc(jObs)%type,OTB_PRCP) )type = IBSET(type,FTB_PRCP)
  IF( BTEST(ObsSrc(jObs)%type,OTB_PRATE))type = IBSET(type,FTB_PRATE)
  IF( BTEST(ObsSrc(jObs)%type,OTB_CC  ) )type = IBSET(type,FTB_CLDCV)
  IF( BTEST(ObsSrc(jObs)%type,OTB_LSV ) )type = IBSET(type,FTB_LSV)
  IF( BTEST(ObsSrc(jObs)%type,OTB_LSVL) )type = IBSET(type,FTB_LSVL)
  IF( BTEST(ObsSrc(jObs)%type,OTB_UU  ) )type = IBSET(type,FTB_UU)
  IF( BTEST(ObsSrc(jObs)%type,OTB_LAI) )type = IBSET(type,FTB_LAI)
END DO

fld%type = type

RETURN
END

