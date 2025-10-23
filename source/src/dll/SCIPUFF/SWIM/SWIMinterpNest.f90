INTEGER FUNCTION SWIMinterpNest( fld )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMintrpGrid_fi

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, ifldp
LOGICAL DoIt
INTEGER nzb, k, alloc_stat
LOGICAL lSaveZ
REAL, DIMENSION(:), ALLOCATABLE :: zSave, zwSave

TYPE( MetField ), POINTER :: fldp

TYPE( MetMean3D    ) :: pNextField
TYPE( MetBLparam   ) :: pNextBL
TYPE( MetVariance  ) :: pNextLSV

INTERFACE
  INTEGER FUNCTION SetGridInterpZ( src,grid,height3d )
    USE SWIMmetField_fd
    TYPE( GridSrc ), TARGET,     INTENT( INOUT ) :: src      !Input
    TYPE( MetGrid ),             INTENT( IN    ) :: grid     !Output grid
    REAL, DIMENSION(:), POINTER, OPTIONAL        :: height3d !Time-varying 3d height field
  END FUNCTION SetGridInterpZ
END INTERFACE

INTEGER, EXTERNAL :: SetupNestField, ExpandNestField

SWIMinterpNest = SWIMfailure

!------ Point to parent grid

ifldp = fld%gridSource%unit
fldp => field(ifldp)

lSaveZ = .FALSE.

!------ Check for initialization

IF( BTEST(fld%status,FSB_DOINIT) )THEN

  irv = SetupNestField( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

  IF( BTEST(fldp%status,FSB_UPDATE) .AND. .NOT.BTEST(fldp%status,FSB_FIRST) )THEN
    fld%tNext = fldp%t
    IF( fld%nObsSource > 0 )THEN
      CALL SWIMinterpGridded( fieldType = fld%type,                 &
                              BLType    = fld%BLtype,               &
                              time      = fld%tNext,                &
                              grid      = fld%grid,                 &
                              IntrpFac  = fld%gridSource%IntrpFac,  &
                              nxi       = fld%gridSource%nX,        &
                              nyi       = fld%gridSource%nY,        &
                              nzi       = fld%gridSource%nZ,        &
                              inpFld    = fldp%Field,               &
                              inpBL     = fldp%BL,                  &
                              inpLSV    = fldp%LSV,                 &
                              inpGrid   = fldp%grid,                &
                              nxo       = fld%grid%nX,              &
                              nyo       = fld%grid%nY,              &
                              nzo       = fld%grid%nZ,              &
                              outFld    = fld%Field2,               &
                              outBl     = fld%BL2,                  &
                              outLSV    = fld%NextLSV)
      fld%t2 = fldp%t
    ELSE
      CALL SWIMinterpGridded( fieldType = fld%type,                 &
                              BLType    = fld%BLtype,               &
                              time      = fld%tNext,                &
                              grid      = fld%grid,                 &
                              IntrpFac  = fld%gridSource%IntrpFac,  &
                              nxi       = fld%gridSource%nX,        &
                              nyi       = fld%gridSource%nY,        &
                              nzi       = fld%gridSource%nZ,        &
                              inpFld    = fldp%Field,               &
                              inpBL     = fldp%BL,                  &
                              inpLSV    = fldp%LSV,                 &
                              inpGrid   = fldp%grid,                &
                              nxo       = fld%grid%nX,              &
                              nyo       = fld%grid%nY,              &
                              nzo       = fld%grid%nZ,              &
                              outFld    = fld%NextField,            &
                              outBl     = fld%NextBL,               &
                              outLSV    = fld%NextLSV )
    END IF
    DoIt = .FALSE.
  ELSE
    DoIt = .TRUE.
  END IF

ELSE

  DoIt = BTEST(fldp%status,FSB_UPDATE)
  IF( fld%nObsSource > 0 )DoIt = DoIt .AND. fldp%tNext > fld%t2

END IF

!------ Get next time (if available)

IF( DoIt )THEN

  IF( BTEST(fldp%type,FTB_OBS) )THEN

    IF( fldp%grid%nXY > fld%gridSource%nXY )THEN
      irv = ExpandNestField( fld )
      IF( irv /= SWIMsuccess )GOTO 9999
    END IF

  END IF

  pNextField = fldp%NextField
  pNextBL    = fldp%NextBL
  pNextLSV   = fldp%NextLSV

    fld%tNext  = fldp%tNext



!------ Define new vertical interpolation factors if parent has time-varying height field

  IF( BTEST(fldp%type,FTB_Z) )THEN

    CALL set_kbl( fldp%grid )
    IF( error%Number /= NO_ERROR )GOTO 9999

    irv = SetGridInterpZ( fld%gridSource,fld%grid,pNextField%Z )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE IF( ASSOCIATED(fldp%NextZ) )THEN

    nzb = fldp%grid%nZ
    DO k = 1,nzb
      IF( ABS(fldp%NextZ(k)-fldp%grid%Z(k)) > 0.01 )THEN
        lSaveZ = .TRUE.
        EXIT
      END IF
    END DO

    IF( lSaveZ )THEN

      ALLOCATE( zSave(nzb),STAT=alloc_stat )
      zSave = fldp%grid%Z
      fldp%grid%Z = fldp%NextZ
      IF( ASSOCIATED(fldp%grid%Zw) )THEN
        ALLOCATE( zwSave(nzb),STAT=alloc_stat )
        zwSave = fldp%grid%Zw
        IF( fldp%grid%Zw(1) > 0. )THEN
          fldp%grid%Zw(1) = 0.5*fldp%grid%Z(1)
        END IF
        fldp%grid%Zw(1) = 0.
        DO k = 2,nzb
          fldp%grid%Zw(k) = 0.5*(fldp%grid%Z(k-1)+fldp%grid%Z(k))
        END DO
        fldp%grid%Zw(nzb+1) = 1.5*fldp%grid%Z(nzb) - 0.5*fldp%grid%Z(nzb-1)
      END IF

      irv = SetGridInterpZ( fld%gridSource,fld%grid )
      IF( irv /= SWIMsuccess )GOTO 9999

    END IF

  END IF

    IF( fld%nObsSource > 0 )THEN
      CALL SWIMinterpGridded( fieldType = fld%type,                 &
                              BLType    = fld%BLtype,               &
                              time      = fld%tNext,                &
                              grid      = fld%grid,                 &
                              IntrpFac  = fld%gridSource%IntrpFac,  &
                              nxi       = fld%gridSource%nX,        &
                              nyi       = fld%gridSource%nY,        &
                              nzi       = fld%gridSource%nZ,        &
                              inpFld    = pNextField,               &
                              inpBL     = pNextBL,                  &
                              inpLSV    = pNextLSV,                 &
                              inpGrid   = fldp%grid,                &
                              nxo       = fld%grid%nX,              &
                              nyo       = fld%grid%nY,              &
                              nzo       = fld%grid%nZ,              &
                              outFld    = fld%Field2,               &
                              outBl     = fld%BL2,                  &
                              outLSV    = fld%NextLSV )
      fld%t2 = fldp%tNext
    ELSE
      CALL SWIMinterpGridded( fieldType = fld%type,                 &
                              BLType    = fld%BLtype,               &
                              time      = fld%tNext,                &
                              grid      = fld%grid,                 &
                              IntrpFac  = fld%gridSource%IntrpFac,  &
                              nxi       = fld%gridSource%nX,        &
                              nyi       = fld%gridSource%nY,        &
                              nzi       = fld%gridSource%nZ,        &
                              inpFld    = pNextField,               &
                              inpBL     = pNextBL,                  &
                              inpLSV    = pNextLSV,                 &
                              inpGrid   = fldp%grid,                &
                              nxo       = fld%grid%nX,              &
                              nyo       = fld%grid%nY,              &
                              nzo       = fld%grid%nZ,              &
                              outFld    = fld%NextField,            &
                              outBl     = fld%NextBL,               &
                              outLSV    = fld%NextLSV )
    END IF
  IF( error%Number /= NO_ERROR )GOTO 9999

END IF

IF( lSaveZ )THEN

  fldp%grid%Z = zSave
  DEALLOCATE( zSave,STAT=alloc_stat )
  IF( ASSOCIATED(fldp%grid%Zw) )THEN
    fldp%grid%Zw = zwSave
    DEALLOCATE( zwSave,STAT=alloc_stat )
  END IF

END IF

IF( .NOT.BTEST(fld%status,FSB_DOINIT) )THEN
  IF( BTEST(fldp%status,FSB_UPDATE) )THEN
    fld%status = IBSET(fld%status,FSB_UPDATE)
  ELSE
    fld%status = IBCLR(fld%status,FSB_UPDATE)
  END IF
END IF

SWIMinterpNest = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetupNestField( fld )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMintrpGrid_fi
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER alloc_stat, irv, i, nxy, nz, nxyz, ifld, metType
REAL    Hmax, Htem, Hbar, Hpbar

TYPE( MetField ), POINTER :: fldp

REAL, DIMENSION(:), POINTER :: zNest

INTERFACE

  INTEGER FUNCTION ReadZgrid( VertGridFile,nz,z )
    CHARACTER(*), INTENT( IN  ) :: VertGridFile
    INTEGER,      INTENT( OUT ) :: nz
    REAL, DIMENSION(:), POINTER :: z
  END FUNCTION ReadZgrid

  INTEGER FUNCTION SetNestZgrid( Hmax,nz,zNest )
    REAL,         INTENT( IN    ) :: Hmax
    INTEGER,      INTENT( INOUT ) :: nz
    REAL, DIMENSION(:), POINTER   :: zNest
  END FUNCTION SetNestZgrid

  INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )
    USE SWIM_fi
    REAL, DIMENSION(:),      POINTER         :: zMC
    INTEGER,                 INTENT( IN    ) :: nzMC
    TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid
  END FUNCTION SWIMinitMcWIF

  INTEGER FUNCTION SetGridInterpZ( src,grid,height3d )
    USE SWIMmetField_fd
    TYPE( GridSrc ), TARGET,     INTENT( INOUT ) :: src      !Input
    TYPE( MetGrid ),             INTENT( IN    ) :: grid     !Output grid
    REAL, DIMENSION(:), POINTER, OPTIONAL        :: height3d !Time-varying 3d height field
  END FUNCTION SetGridInterpZ

END INTERFACE

INTEGER, EXTERNAL :: SetNestSource, SetGridInterp2d
INTEGER, EXTERNAL :: SWIMalloc3dField, SWIMallocBLParam, GridLogMsg
INTEGER, EXTERNAL :: SWIMallocBLaux, SetGridLL, InitLSV
INTEGER, EXTERNAL :: SWIMinitObsAssim
REAL,    EXTERNAL :: SWIMsetHmin

SetupNestField = SWIMfailure

NULLIFY( zNest )

!------ Set index of 'parent' field

ifld = fld%gridSource%unit
fldp => field(ifld)

!------ Set grid source parameters

irv = SetNestSource( fld%gridSource,fldp%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set horizontal interpolation factors

irv = SetGridInterp2d( fld%gridSource,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set terrain and landcover based on parent grid (or simple input)
!       if not already set

nxy = fld%grid%nXY

IF( .NOT.BTEST(fld%grid%type,GTB_TERRAIN) )THEN
  IF( BTEST(fldp%grid%type,GTB_TERRAIN) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%terrain%H,fld%grid%terrain%H(i) )
      fld%grid%terrain%H(i) = fld%grid%terrain%H(i) + fldp%grid%Hmin
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_TERRAIN)
  ELSE
    DO i = 1,nxy
      fld%grid%terrain%H(i) = 0.
    END DO
  END IF
ELSE
  IF( BTEST(fldp%grid%type,GTB_TERRAIN) )THEN
    Hpbar = 0.0   ! Adjust terrain heights to match average parent elevation
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%terrain%H,Htem )
      Hpbar = Hpbar + Htem
    END DO
    Hpbar = Hpbar/FLOAT(nxy) + fldp%grid%Hmin
    Hbar = 0.
    DO i = 1,nxy
      Hbar = Hbar + fld%grid%terrain%H(i)
    END DO
    Hbar = Hbar/FLOAT(nxy)
    DO i = 1,nxy
      fld%grid%terrain%H(i) = fld%grid%terrain%H(i) - Hbar + Hpbar
    END DO
  ELSE   ! Adjust terrain heights to ensure Hmin=0.0
    Hbar = HUGE(Hbar)
    DO i = 1,nxy
      Hbar = MIN(fld%grid%terrain%H(i),Hbar)
    END DO
    DO i = 1,nxy
      fld%grid%terrain%H(i) = fld%grid%terrain%H(i) - Hbar
    END DO
  END IF
END IF

IF( .NOT.BTEST(fld%grid%type,GTB_ZRUF) )THEN
  IF( BTEST(fldp%grid%type,GTB_ZRUF) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%landcover%roughness,fld%grid%landcover%roughness(i) )
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_ZRUF)
  ELSE
    DO i = 1,nxy
      fld%grid%landcover%roughness(i) = Prj%BL%zruf
    END DO
  END IF
END IF

IF( .NOT.BTEST(fld%grid%type,GTB_HCNP) )THEN
  IF( BTEST(fldp%grid%type,GTB_HCNP) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%landcover%canopyHt,fld%grid%landcover%canopyHt(i) )
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_HCNP)
  ELSE
    DO i = 1,nxy
      fld%grid%landcover%canopyHt(i) = Prj%BL%hc
    END DO
  END IF
END IF

IF( .NOT.BTEST(fld%grid%type,GTB_ALPHA) )THEN
  IF( BTEST(fldp%grid%type,GTB_ALPHA) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%landcover%alpha,fld%grid%landcover%alpha(i) )
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_ALPHA)
  ELSE
    DO i = 1,nxy
      fld%grid%landcover%alpha(i) = Prj%BL%alpha
    END DO
  END IF
END IF

IF( .NOT.BTEST(fld%grid%type,GTB_ALBEDO) )THEN
  IF( BTEST(fldp%grid%type,GTB_ALBEDO) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%landcover%albedo,fld%grid%landcover%albedo(i) )
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_ALBEDO)
  ELSE
    DO i = 1,nxy
      fld%grid%landcover%albedo(i) = Prj%BL%albedo
    END DO
  END IF
END IF

IF( .NOT.BTEST(fld%grid%type,GTB_BOWEN) )THEN
  IF( BTEST(fldp%grid%type,GTB_BOWEN) )THEN
    DO i = 1,nxy
      CALL IntXY( fld%GridSource%IntrpFac%mh(i),fldp%grid%landcover%Bowen,fld%grid%landcover%Bowen(i) )
    END DO
    fld%grid%type = IBSET(fld%grid%type,GTB_BOWEN)
  ELSE
    DO i = 1,nxy
      fld%grid%landcover%Bowen(i) = Prj%BL%Bowen
    END DO
  END IF
END IF

!------ Subtract minimum terrain height

CALL SetHmin( fld%grid )

Prj%Hmin = SWIMsetHmin()

!------ Initialize appropriate model

IF( BTEST(fld%type,FTB_MCWIF) )THEN

    Hmax = -HUGE(Hbar)
    DO i = 1,nxy
      Hmax = MAX(fld%grid%terrain%H(i),Hmax)
    END DO

    IF( BTEST(Prj%MC%type,MCB_GRIDNEST) .AND. fld%nObsSource == 0 )THEN
      nz = Prj%MC%nz
      ALLOCATE( zNest(nz),STAT=irv )
      IF( irv /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SetupNestField'
        error%Message = 'Error allocating vertical grid for nested field'
        GOTO 9999
      END IF
      zNest(1:nz) = Prj%MC%z(1:nz)
    ELSE
      nz = 0
    END IF

    IF( fld%gridSource%nSource == 2 )THEN
      irv = ReadZgrid( fld%gridSource%Source(2),nz,zNest )
    ELSE
      irv = SetNestZgrid( Hmax,nz,zNest )
    ENDIF
    IF( irv /= SWIMsuccess )GOTO 9999

    Prj%MC%type = IBCLR(Prj%MC%type,MCB_GRIDNEST)  !Clear for any subsequent nests

    irv = SWIMinitMcWIF( zNest,nz,fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999

ELSE

  GOTO 9999 !Initialize other models in the future

END IF

!------ Set grid level for setting surface layer profile (above canopy)

CALL set_kbl( fld%grid)
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Set vertical interpolation factors

irv = SetGridInterpZ( fld%gridSource,fld%grid,fldp%Field%Z )
IF( irv /= SWIMsuccess )GOTO 9999

fld%gridSource%IntrpFac%Stagger = BTEST(fldp%grid%type,GTB_STAGGERZ)

!------ Allocate met fields

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  nxyz = nxy*(fld%grid%nZ+1)
ELSE
  nxyz = nxy*fld%grid%nZ
END IF

IF( nxy >= 6 )fld%type = IBSET(fld%type,FTB_DU2)

irv = SWIMalloc3dField( fld%type,nxyz,fld%Field )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMalloc3dField( fld%type,nxyz,fld%NextField )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMallocBLParam( fld%type,nxy,fld%BL,fld%NextBL )
IF( irv /= SWIMsuccess )GOTO 9999

fld%BLaux%nz = Prj%BL%nzbl

irv = SWIMallocBLaux( nxy,fld%BLaux )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Some final things:

IF( .NOT.BTEST(fld%type,FTB_CLDCV) )THEN
  DO i = 1,fld%grid%nXY
    fld%BL%cc(i) = Prj%BL%cc
  END DO
END IF

IF( fld%nObsSource > 0 )THEN
  irv = SWIMinitObsAssim( fld )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Compute lat/lon arrays if possible

irv = SetGridLL( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize large-scale variability

irv = InitLSV( fld )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Log messages

irv = GridLogMsg( fld )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

SetupNestField = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(zNest) )DEALLOCATE( zNest,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION ExpandNestField( fld )

!------ Re-set interpolation factors when a field is 'expanded' (obs only)

USE SWIM_fi
USE SWIMparam_fd
USE SWIMintrpGrid_fi
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER alloc_stat, irv, ifld

TYPE( MetField), POINTER :: fldp

INTERFACE
  INTEGER FUNCTION SetGridInterpZ( src,grid,height3d )
    USE SWIMmetField_fd
    TYPE( GridSrc ), TARGET,     INTENT( INOUT ) :: src      !Input
    TYPE( MetGrid ),             INTENT( IN    ) :: grid     !Output grid
    REAL, DIMENSION(:), POINTER, OPTIONAL        :: height3d !Time-varying 3d height field
  END FUNCTION SetGridInterpZ
END INTERFACE

INTEGER, EXTERNAL :: SetNestSource, SetGridInterp2d

ExpandNestField = SWIMfailure

!------ Set index of 'parent' field

ifld = fld%gridSource%unit
fldp => field(ifld)

!------ Set grid source parameters

irv = SetNestSource( fld%gridSource,fldp%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Deallocate interpolation factors

DEALLOCATE( fld%gridSource%IntrpFac%mh,fld%gridSource%IntrpFac%mhu,fld%gridSource%IntrpFac%mhv,STAT=alloc_stat )
DEALLOCATE( fld%gridSource%IntrpFac%mz,fld%gridSource%IntrpFac%mzu,fld%gridSource%IntrpFac%mzv,STAT=alloc_stat )
DEALLOCATE( fld%gridSource%IntrpFac%mzw,STAT=alloc_stat )

!------ Set horizontal interpolation factors

irv = SetGridInterp2d( fld%gridSource,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set vertical interpolation factors

irv = SetGridInterpZ( fld%gridSource,fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

ExpandNestField = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetNestSource( src,grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src
TYPE( MetGrid ), INTENT( IN    ) :: grid

INTEGER alloc_stat, i

SetNestSource = SWIMfailure

src%iStart = 1; src%iEnd = grid%nX
src%jStart = 1; src%jEnd = grid%nY

src%X0 = grid%Xmin; src%nX = grid%nX; src%dX = grid%dX
src%Y0 = grid%Ymin; src%nY = grid%nY; src%dY = grid%dY

src%nXY = grid%nXY
src%nZ  = grid%nZ

IF( ASSOCIATED(src%Z) )DEALLOCATE( src%Z,STAT=alloc_stat )
ALLOCATE( src%Z(src%nZ),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'SetNestSource'
  error%Message = 'Error allocating vertical grid for nested field'
  GOTO 9999
END IF

src%Z = grid%Z

!------ Always set w-grid (even if w not set, or grid is not staggered)

IF( ASSOCIATED(src%Zw) )DEALLOCATE( src%Zw,STAT=alloc_stat )
ALLOCATE( src%Zw(src%nZ),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'SetNestSource'
  error%Message = 'Error allocating w-grid for nested field'
  GOTO 9999
END IF

IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
  DO i = 1,src%nZ
    src%Zw(i) = grid%Zw(i)
  END DO
ELSE
  DO i = 1,src%nZ
    src%Zw(i) = grid%Z(i)
  END DO
END IF

!------ Set coordinate type

SELECT CASE( grid%coord%type )
  CASE( I_LATLON )
    src%type = IBSET(src%type,GSB_LATLON)
  CASE( I_UTM )
    src%type = IBSET(src%type,GSB_UTM)
  CASE( I_LAMBERT )
    src%type = IBSET(src%type,GSB_LAMBERT)
  CASE( I_POLAR )
    src%type = IBSET(src%type,GSB_POLAR)
  CASE( I_RPOLAR )
    src%type = IBSET(src%type,GSB_RPOLAR)
  CASE( I_ROTLL )
    src%type = IBSET(src%type,GSB_ROTLL)
  CASE( I_MERCATOR )
    src%type = IBSET(src%type,GSB_MERCATOR)
END SELECT

SetNestSource = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetNestZgrid( Hmax,nz,zNest )

USE SWIM_fi
USE SWIMparam_fd
USE reallocate

IMPLICIT NONE

REAL,         INTENT( IN    ) :: Hmax
INTEGER,      INTENT( INOUT ) :: nz
REAL, DIMENSION(:),   POINTER :: zNest

INTEGER alloc_stat, nExtra, i
REAL    dz

SetNestZgrid = SWIMfailure

IF( nz == 0 )THEN

  IF( ASSOCIATED(zNest) )DEALLOCATE( zNest,STAT=alloc_stat )

  nz = 24

  ALLOCATE( zNest(nz),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    error%Routine = 'SetNestZgrid'
    error%Message = 'Error allocating vertical grid for nested field'
    GOTO 9999
  END IF

  zNest(1:24) = (/ 1.00000E+01, 3.00000E+01, 5.31921E+01, 8.01016E+01, 1.11343E+02, &
                   1.47635E+02, 1.89821E+02, 2.38887E+02, 2.95993E+02, 3.62497E+02, &
                   4.39996E+02, 5.30364E+02, 6.35808E+02, 7.58920E+02, 9.02754E+02, &
                   1.07091E+03, 1.26761E+03, 1.49786E+03, 1.76755E+03, 2.08364E+03, &
                   2.45432E+03, 2.88929E+03, 3.40000E+03, 4.00000E+03 /)

END IF

IF( Hmax > 0.5*zNest(nz) )THEN

  dz = MAX(zNest(nz)-zNest(nz-1),600.)

  nExtra = INT( (2.*Hmax - zNest(nz)) / dz + 1. )

  alloc_stat = reallocate_real1d( zNest,nExtra )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    error%Routine = 'SetNestZgrid'
    error%Message = 'Error allocating vertical grid for nested field'
    GOTO 9999
  END IF

  DO i = 1,nExtra
    zNest(nz+i) = zNest(nz) + FLOAT(i)*dz
  END DO

  nz = nz + nExtra

END IF

SetNestZgrid = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadZgrid( VertGridFile,nz,z )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: VertGridFile
INTEGER,      INTENT( OUT ) :: nz
REAL, DIMENSION(:), POINTER :: z

INTEGER ios, alloc_stat

ReadZgrid = SWIMfailure
error%Routine = 'ReadZgrid'
CALL ReportFileName( error%Inform,'File=',VertGridFile )

OPEN(UNIT=SWIMunit,FILE=TRIM(VertGridFile),STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening vertical grid file'
  GOTO 9999
END IF

READ(SWIMunit,*,IOSTAT=ios) nz
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Message = 'Error reading number of vertical grid levels'
  GOTO 9999
END IF

ALLOCATE(z(nz),STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array for vertical grid levels'
  GOTO 9999
END IF

READ(SWIMunit,*,IOSTAT=ios) z
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Message = 'Error reading vertical grid levels'
  GOTO 9999
END IF

ReadZgrid = SWIMresult
error%Routine = ''
error%Inform  = ''

9999 CONTINUE

CLOSE(UNIT=SWIMunit,IOSTAT=ios)

RETURN
END

