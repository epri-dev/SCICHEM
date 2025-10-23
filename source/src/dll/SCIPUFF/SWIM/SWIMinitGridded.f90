RECURSIVE INTEGER FUNCTION SWIMinitGridded( ifld,unit,metType ) RESULT( FuncVal )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ifld
INTEGER, INTENT( INOUT ) :: unit
INTEGER, INTENT( INOUT ) :: metType

TYPE( MetField ), POINTER :: fld

CHARACTER(PATH_MAXLENGTH) NestSource, file, path, pathx, NestSourcePath
INTEGER irv, alloc_stat, nxy, nxyz, inest, i
INTEGER nzb
LOGICAL lexist, lQQ, lNestTerr
LOGICAL lflag

INTEGER(8) n1, n2

TYPE ( messageT ) caution

INTERFACE
  INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )
    USE SWIM_fi
    REAL, DIMENSION(:),      POINTER         :: zMC
    INTEGER,                 INTENT( IN    ) :: nzMC
    TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid
  END FUNCTION SWIMinitMcWIF
END INTERFACE

INTEGER, EXTERNAL :: SWIMinitMEDOC, SWIMinitSCIP
INTEGER, EXTERNAL :: SWIMreadTerrain, SWIMsetMEDOCterrain
INTEGER, EXTERNAL :: SetGriddedType, SetGridParam, SetGridInterp
INTEGER, EXTERNAL :: SWIMalloc3dField, SWIMallocBLParam, SWIMallocBLaux
INTEGER, EXTERNAL :: SWIMreallocMetField, SWIMwarningMessage, SWIMdeallocGrid
INTEGER, EXTERNAL :: SetBLType, SetFieldTypeGridded
INTEGER, EXTERNAL :: SWIMallocQLprof
REAL,    EXTERNAL :: SWIMtimeOffset
INTEGER, EXTERNAL :: SWIMinitWRF, SWIMsetWRFterrain
INTEGER, EXTERNAL :: SWIMinitMEDOClist
INTEGER, EXTERNAL :: getFieldIndex
INTEGER, EXTERNAL :: MEDOCpositionToBreak
INTEGER, EXTERNAL :: PostCautionMessage

CHARACTER(128),            EXTERNAL :: ArraySizeStr
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

FuncVal = SWIMfailure

NestSource = ' '

fld => field(ifld)

!------ Determine specific file type and format

irv = SetGriddedType( fld%gridSource,metType )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%gridSource%type,GSB_WRF) .OR. &
    BTEST(fld%gridSource%type,GSB_INITWRF) )Prj%localMet = .FALSE.  !WRF times assumed UTC

!------ Set time offset

fld%gridSource%timeOffset = SWIMtimeOffset()
IF( error%Number /= NO_ERROR )GOTO 9999

!------ Set times as "not set"

fld%t     = NOT_SET_R
fld%tNext = NOT_SET_R
fld%tBL   = NOT_SET_R

!------ Initialize grid

fld%grid%type = 0
CALL SWIMinitCoord( fld%grid%coord )

!------ Set initial status

fld%status = 0
fld%status = IBSET(fld%status,FSB_DOINIT)
fld%status = IBSET(fld%status,FSB_UPDATE)
fld%type   = 0
IF( BTEST(fld%gridSource%type,GSB_WRF) .AND. .NOT.BTEST(fld%gridSource%type,GSB_INITWRF) )fld%type = IBSET(fld%type,FTB_WRF)

fld%grid%Xmin = NOT_SET_R !To indicate grid has not been defined

lNestTerr = .FALSE.

!------ Read terrain file for mass-consistent adjustment
!       N.B. McWIF will be used with MEDOC file
!            and 3d climo

IF( BTEST(Prj%MC%type,MCB_TER) .OR. BTEST(Prj%MC%type,MCB_LC) )THEN

  IF( BTEST(fld%gridSource%type,GSB_SCIP) )THEN

    CALL SetFieldGridMapType( fld%grid%type )

    irv = SWIMreadTerrain( fld%grid )
    IF( irv /= SWIMsuccess )GOTO 9999

    fld%type = IBSET(fld%type,FTB_W)
    fld%type = IBSET(fld%type,FTB_INTRP)

  ELSE

    lNestTerr = .TRUE.

  END IF

END IF

!------ Open file, read header and set gridSource

IF( BTEST(fld%gridSource%type,GSB_MEDOC) )THEN

  irv = SWIMinitMEDOC( fld%gridSource,NestSource ) !Rewind file
  IF( irv /= SWIMsuccess )GOTO 9999

  CALL InitGridSrcLimits( fld%gridSource )


ELSE IF( BTEST(fld%gridSource%type,GSB_SCIP) )THEN

  irv = SWIMinitSCIP( fld%gridSource )  !Positioned to read first time
  IF( irv /= SWIMsuccess )GOTO 9999

  CALL InitGridSrcLimits( fld%gridSource )


ELSE IF( BTEST(fld%gridSource%type,GSB_INITWRF) )THEN

  irv = SWIMinitWRF( unit )
  IF( irv /= SWIMsuccess )GOTO 9999

  fld => field(ifld)  !Re-point because reallocating met fields breaks original connection

  CALL InitGridSrcLimits( fld%gridSource )

  metType = SWIMWRF

ELSE IF( BTEST(fld%gridSource%type,GSB_WRF) )THEN !SWIMinitWRF has been called already

  CALL InitGridSrcLimits( fld%gridSource )

ELSE IF( BTEST(fld%gridSource%type,GSB_INITMEDOC) )THEN

  irv = SWIMinitMEDOClist( unit )
  IF( irv /= SWIMsuccess )GOTO 9999

  fld => field(ifld)  !Re-point because reallocating met fields breaks original connection

  CALL InitGridSrcLimits( fld%gridSource )

  metType = SWIMMEDOC

END IF

!------ Setup McWIF with 3d climo or SCIP gridded; McWIF w/ MEDOC will be defined later

IF( BTEST(Prj%MC%type,MCB_TER) .OR. BTEST(Prj%MC%type,MCB_LC) )THEN
  IF( lNestTerr )THEN

    Prj%MC%type = IBSET(Prj%MC%type,MCB_GRIDNEST)  !Create nest within gridded outer domain
    Prj%MC%type = IBCLR(Prj%MC%type,MCB_SWIFT)

  ELSE
    Prj%MC%type = IBSET(Prj%MC%type,MCB_MCWIF)

      CALL SetFieldGridMapType( fld%grid%type )

      irv = SWIMinitMcWIF( Prj%MC%Z,Prj%MC%nZ,fld%grid )
      IF( irv /= SWIMsuccess )GOTO 9999

      fld%type = IBSET(fld%type,FTB_MCWIF)

  END IF

END IF

!------ Set field type based on variables available from gridded source

irv = SetFieldTypeGridded( fld%type,fld%gridSource )
IF( irv /= SWIMsuccess )GOTO 9999


!------ Setup for interpolation N.B. only SCIP gridded w/ terrain)

IF( BTEST(fld%type,FTB_INTRP) )THEN

  fld%gridSource%kStart = 1
  fld%gridSource%iSkip  = 1
  fld%gridSource%jSkip  = 1

  CALL set_kbl( field(ifld)%grid )
  IF( error%Number /= NO_ERROR )GOTO 9999

  irv = SetGridInterp( fld%type,fld%gridSource,fld%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Or setup non-interpolation grid parameters

ELSE

  irv = SetGridParam( fld%gridSource,fld%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

   IF( BTEST(fld%gridSource%type,GSB_SCIP) )THEN
     CALL set_kbl( field(ifld)%grid )                 !Called later for other input
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF

END IF

IF( (BTEST(fld%gridSource%type,GSB_ZMSL) .OR. BTEST(fld%gridSource%type,GSB_ZAGL)) .AND. &
    .NOT.BTEST(fld%type,FTB_Z) )THEN
  error%Number   = IV_ERROR
  error%Routine  = 'SWIMinitGridded'
  error%Message  = 'Height coordinate specified but 3d field not present'
  GOTO 9999
END IF

!------ Set terrain status for MEDOC file
!       Read terrain for 3d climo & set 3d height field

IF( BTEST(fld%gridSource%type,GSB_MEDOC) )THEN
  irv = SWIMsetMEDOCterrain( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

  irv = MEDOCpositionToBreak( fld%gridSource )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE IF( BTEST(fld%gridSource%type,GSB_WRF) )THEN
  irv = SWIMsetWRFterrain( fld )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

n1 = fld%grid%nX
n2 = fld%grid%nY
IF( n1*n2 > HUGE(0) )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitGridded'
  error%Message = 'Grid size too large'
  error%Inform  =  ArraySizeStr( 2,(/fld%grid%nX,fld%grid%nY /) )
  GOTO 9999
END IF

!------ Check landuse fields

IF( BTEST(fld%grid%type,GTB_HCNP) )THEN
  IF( .NOT.BTEST(fld%grid%type,GTB_ZRUF) )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitGridded'
    error%Message = 'Roughness must be given along with canopy height'
    IF( BTEST(fld%gridSource%type,GSB_MEDOC) )THEN
      CALL ReportFileName( error%Inform,'File=',fld%gridSource%Source(1) )
    END IF
    GOTO 9999
  END IF
  IF( .NOT.BTEST(fld%grid%type,GTB_ALPHA) )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitGridded'
    error%Message = 'Canopy velocity parameter must be given along with canopy height'
    IF( BTEST(fld%gridSource%type,GSB_MEDOC) )THEN
      CALL ReportFileName( error%Inform,'File=',fld%gridSource%Source(1) )
    END IF
    GOTO 9999
  END IF
END IF

!------ Allocate met fields

nxy = fld%grid%nXY

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

lQQ = BTEST(fld%gridSource%type,GSB_MEDOC) .AND. ANY(fld%gridSource%Var3dID==GVP_TKE)
lQQ = lQQ .OR. BTEST(fld%gridSource%type,GSB_WRF) .AND. ANY(fld%gridSource%Var3dID==GVP_TKE)

IF( lQQ )THEN

  irv = SWIMallocQLprof( nxyz,fld%QLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

  irv = SWIMallocQLprof( nxyz,fld%NextQLprof )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Allocate vertical levels for time-varying Sigma-z on MEDOC file

IF( BTEST(fld%gridSource%type,GSB_MEDOC) .AND. BTEST(fld%gridSource%type,GSB_SIGMAZ) )THEN

  nzb = field(ifld)%grid%nZ
  ALLOCATE( field(ifld)%NextZ(nzb),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitGridded'
    error%Message = 'Error allocating next vertical grid'
    CALL ReportFileName( error%Action,'File= ',fld%gridSource%Source(1) )
    WRITE(error%Inform,'(A,I8)') 'Size=',nzb
    GOTO 9999
  END IF

  DO i = 1,nzb
    field(ifld)%NextZ(i) = field(ifld)%grid%Z(i)
  END DO

END IF

!------ Set/check boundary layer type

irv = SetBLType( field(ifld) )
IF( irv /= SWIMsuccess )GOTO 9999

DO i = 1,fld%grid%nXY
  fld%BL%cc(i) = Prj%BL%cc
END DO

!------ Check BL and uncertainty for time-reverse calculation

IF( Reverse )THEN

  IF( field(ifld)%BLtype /= BLP_MET )THEN
    error%Number  = RV_ERROR
    error%Routine = 'SWIMinitGridded'
    error%Message = 'Insufficient input for time-reverse calculation'
    error%Inform  = 'Boundary-layer variables must be on MEDOC file'
    GOTO 9999
  END IF

END IF

!------ Check UTM zone, if appropriate

IF( Prj%coord == I_UTM .AND. fld%grid%coord%type == I_UTM )THEN

    IF( Prj%UTMzone /= fld%grid%coord%zone )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMinitGridded'
      error%Message = 'Project UTM zone different from gridded meteorology zone'
      WRITE(error%Inform, &
            FMT="('Project zone = ',I2,2X,'Met zone = ',I2)") &
            Prj%UTMzone, fld%grid%coord%zone
      GOTO 9999
    END IF

END IF

!------ Check for global, periodic domain

IF( BTEST(fld%grid%type,GTB_LATLON) )THEN
  IF( fld%grid%Xmax-fld%grid%Xmin > 359.9 )fld%grid%type = IBSET(fld%grid%type,GTB_PERIODIC)
END IF


!------ Check for nested input; add recursively

file = NestSource; NestSource = TRIM(StripNull(file))

IF( LEN_TRIM(NestSource) > 0 )THEN

  CALL SubPathSep( NestSource )  ! Ignore slash type in path separator
  CALL SplitName( NestSource,file,pathx )
  IF( LEN_TRIM(pathx) == 0 )THEN                          !Add path of Source if NestSource is w/o a path
    CALL SplitName( fld%gridSource%Source(1),file,path )
    CALL AddPath( NestSource,path )
  ELSE                                                    !Add path of Source assuming relative path
    CALL SplitName( fld%gridSource%Source(1),file,path )
    NestSourcePath = NestSource
    CALL AddPath( NestSourcePath,path )
    INQUIRE( FILE=TRIM(NestSourcePath),EXIST=lexist )
    IF( lexist )NestSource = NestSourcePath
  END IF

  INQUIRE( FILE=TRIM(NestSource),EXIST=lexist )
  IF( .NOT.lexist )THEN                                  !If not found, look in Source directory
    IF( LEN_TRIM(pathx) > 0 )THEN
      caution%aString = 'Nested file not found in original location'
      caution%bString = 'File name = '//TRIM(NestSource)
      caution%cString = 'Path of parent MEDOC file will be checked'
      irv = PostCautionMessage( caution )
    END IF

    CALL SplitName( fld%gridSource%Source(1),file,path )
    CALL SplitName( NestSource,file,pathx )
    NestSource = TRIM(file)
    CALL AddPath( NestSource,path )

    INQUIRE( FILE=TRIM(NestSource),EXIST=lexist )
    IF( .NOT.lexist )THEN
      error%Number  = WN_ERROR
      error%Routine = 'SWIMinitGridded'
      error%Message = 'Nested MEDOC file does not exist'
      CALL ReportFileName( error%Inform,'File=',NestSource )
      error%Action  = 'Do you wish to continue without it?'
      irv = SWIMwarningMessage( 'User elected not to continue' )
      IF( irv == SWIMsuccess )FuncVal = SWIMresult
      GOTO 9999
    END IF

  END IF

  irv = SWIMreallocMetField( 1 )    !Increments numField by 1
  IF( irv /= SWIMsuccess )GOTO 9999

  fld => field(ifld)

  unit = unit + 1

  CALL setFieldIndex( field(numField)%index,numField )

  ALLOCATE( field(numField)%gridSource%Source(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitGridded'
    error%Message = 'Error allocating array for gridded file name'
    GOTO 9999
  END IF

  field(numField)%gridSource%nSource   = 1
  field(numField)%gridSource%Source(1) = TRIM(NestSource)
  field(numField)%gridSource%type      = IBSET(0,GSB_MEDOC)
  field(numField)%gridSource%unit      = unit

  inest = numField

  irv = SWIMinitGridded( inest,unit,metType )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE

  lflag = .FALSE.
  IF( BTEST(fld%gridSource%type,GSB_WRF) .AND. getFieldIndex(fld%index) < numField )lflag = .TRUE.

  IF( lflag )THEN

    inest = getFieldIndex( fld%index ) + 1
    CALL setFieldIndex( field(inest)%index,inest )

    irv = SWIMinitGridded( inest,unit,metType )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

END IF

FuncVal = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetGriddedType( Src,metType )

!------ Determine gridded file type and format

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: Src
INTEGER,         INTENT( INOUT ) :: metType

CHARACTER(PATH_MAXLENGTH) line
CHARACTER(PATH_MAXLENGTH) FileName
CHARACTER(8)   fflag

INTEGER unit, ios
REAL    lonx, latx
LOGICAL lflag

INTEGER, EXTERNAL :: getFieldIndex

SetGriddedType = SWIMfailure

!------ Nothing to be done for certain types

lflag = BTEST(src%type,GSB_SCIP)
lflag = lflag .OR. BTEST(src%type,GSB_INITWRF) .OR. BTEST(src%type,GSB_WRF)
lflag = lflag .OR. BTEST(src%type,GSB_INITMEDOC)
IF( lflag )THEN

  SetGriddedType = SWIMresult
  GOTO 9999
END IF

!------ Set locals

unit     = getFieldIndex( Src%unit )
FileName = TRIM(Src%Source(1))

!------ Open file as 'formatted'

OPEN(UNIT=unit,FILE=FileName,STATUS='OLD',FORM='FORMATTED', &
                                          ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SetGriddedType'
  error%Message = 'Error opening GRIDDED MET input file'
  CALL ReportFileName( error%Inform,'File=',FileName )
  GOTO 9999
END IF

!------ Read first record (skip over blank lines )

DO
  READ(unit,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SetGriddedType'
    error%Message = 'Error reading GRIDDED MET input file'
    CALL ReportFileName( error%Inform,'File=',FileName )
    GOTO 9999
  END IF
  IF( LEN_TRIM(line) > 0 )EXIT
END DO

!------ If first record begins with 'F', this is a formatted MEDOC file

IF( TRIM(line(1:1)) == 'F' )THEN

  Src%type = IBSET(Src%type,GSB_MEDOC)
  metType = SWIMMEDOC

ELSE IF(  line(1:3) == 'WRF' )THEN

  Src%type = IBSET(Src%type,GSB_INITWRF)
  metType  = SWIMWRF

ELSE IF(  line(1:5) == 'MEDOC' )THEN

  Src%type = IBSET(Src%type,GSB_INITMEDOC)
  metType  = SWIMMEDOC

!------ If first record is a comment ('# ... '), assume a SCIP gridded file

ELSE IF( line(1:1) == '#' )THEN

  Src%type = IBSET(Src%type,GSB_SCIP)
  metType  = SWIMSCIP

ELSE

!------ Check if first record matches SCIP format

  READ(line,FMT='(2F10.3)',IOSTAT=ios) lonx,latx

!------ If read error, check for binary MEDOC file

  IF( ios /= 0 )THEN

    CLOSE(unit,IOSTAT=ios)
    OPEN(UNIT=unit,FILE=FileName,STATUS='OLD',FORM='UNFORMATTED', &
                                             ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      error%Number  = OP_ERROR
      error%Routine = 'SetGriddedType'
      error%Message = 'Error opening GRIDDED MET input file'
      CALL ReportFileName( error%Inform,'File=',FileName )
      GOTO 9999
    END IF
    READ(unit,IOSTAT=ios) fflag

    IF( fflag(1:1) == 'B' )THEN !This is a binary MEDOC file

      Src%type = IBSET(Src%type,GSB_BINARY)
      Src%type = IBSET(Src%type,GSB_MEDOC)
      metType  = SWIMMEDOC

    ELSE                         !Error if not 'BBBBBBBB'

      error%Number  = IV_ERROR
      error%Routine = 'SetGriddedType'
      error%Message = 'Unrecognized GRIDDED MET input file'
      CALL ReportFileName( error%Inform,'File=',FileName )

    END IF

!------ Check if lat/lon are valid values for SCIP format

  ELSE

    IF( ABS(lonx) <= 360. .OR. ABS(latx) <= 180. )THEN

      Src%type = IBSET(Src%type,GSB_SCIP)
      metType  = SWIMSCIP

    ELSE

      error%Number  = IV_ERROR
      error%Routine = 'SetGriddedType'
      error%Message = 'Unrecognized GRIDDED MET input file'
      CALL ReportFileName( error%Inform,'File=',FileName )

    END IF

  END IF

END IF

SetGriddedType = SWIMresult

CLOSE(unit,IOSTAT=ios)

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetFieldTypeGridded( type,Src )

!------ Set source type (gridded file format) and
!       field type bits based on available fields

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER(8),      INTENT( INOUT ) :: type
TYPE( GridSrc ), INTENT( INOUT ) :: Src

INTEGER i
LOGICAL lu, lv, luv

INTEGER, EXTERNAL :: SWIMaddLogMessage

SetFieldTypeGridded = SWIMfailure

!------ Source type

IF( BTEST(Src%type,GSB_MEDOC) )THEN
  type = IBSET(type,FTB_MEDOC)

ELSE IF( BTEST(Src%type,GSB_SCIP) )THEN
  type = IBSET(type,FTB_SCIP)
  type = IBSET(type,FTB_P)  !Pressure levels available

END IF

lu = .FALSE.; lv = lu; luv = lu;

!------ Available 3d fields

DO i = 1,Src%nVar3d
  SELECT CASE( Src%Var3dID(i) )

    CASE( GVP_W )
      type = IBSET(type,FTB_W)

    CASE( GVP_T )
      type = IBSET(type,FTB_T)

    CASE( GVP_P )
      type = IBSET(type,FTB_P)

    CASE( GVP_H )
      type = IBSET(type,FTB_H)

    CASE( GVP_QCLD )
      type = IBSET(type,FTB_QCLD)

    CASE( GVP_Z )
      type = IBSET(type,FTB_Z)

    CASE( GVP_UUL )
      type = IBSET(type,FTB_LSV)
      lu = .TRUE.

    CASE( GVP_VVL )
      type = IBSET(type,FTB_LSV)
      lv = .TRUE.

    CASE( GVP_UVL )
      type = IBSET(type,FTB_LSV)
      luv = .TRUE.

    CASE( GVP_SHL )
      type = IBSET(type,FTB_LSVL)

  END SELECT
END DO

IF( BTEST(type,FTB_LSV) )THEN
  IF( .NOT.(lu .AND. lv .AND. luv) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetFieldTypeGridded'
    error%Message = 'Missing large-scale variability fields on'
    error%Inform  = 'UUL, VVL and UVL required'
    CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
    GOTO 9999
  END IF
END IF

!------ Available 2d fields

DO i = 1,Src%nVar2d
  SELECT CASE( Src%Var2dID(i) )

    CASE( GVP_ZI )
      type = IBSET(type,FTB_ZI)

    CASE( GVP_HFLX )
      type = IBSET(type,FTB_HFLX)

    CASE( GVP_CC )
      type = IBSET(type,FTB_CLDCV)

    CASE( GVP_ZRUFT )
      type = IBSET(type,FTB_ZRUF)

    CASE( GVP_PRATE )
      type = IBSET(type,FTB_PRATE)

    CASE( GVP_UST )
      type = IBSET(type,FTB_UST)

    CASE( GVP_ACCPR )
      type = IBSET(type,FTB_PRATE)
      type = IBSET(type,FTB_ACCPR)

    CASE( GVP_LAI )
      type = IBSET(type,FTB_LAI)

  END SELECT

END DO

IF( BTEST(type,FTB_UST) )THEN
  IF( .NOT.(BTEST(type,FTB_ZI) .AND. BTEST(type,FTB_HFLX)) )THEN
    type = IBCLR(type,FTB_UST)
    DO i = 1,Src%nVar2d
      IF( Src%Var2dID(i) == GVP_UST )Src%Var2dID(i) = GVP_NONE
    END DO
    i = SWIMaddLogMessage( '*** U* input ignored w/o surface heat flux and boundary layer depth')
  END IF
END IF

SetFieldTypeGridded = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SetGridParam( src,grid )

!------ Compute grid dimensions and skip increments

USE SWIM_fi
USE SWIMparam_fd
USE GridCoord_fd
USE constants_fd
USE landuse_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src
TYPE( MetGrid ), INTENT( INOUT ) :: grid

INTEGER irv, ios, alloc_stat
INTEGER iskip, jskip, nxb, nyb, nxyb, nzb, ntot, i, k
INTEGER iu, iv, iw, iz
REAL    latMax, latMin
REAL    xtem, ytem, tem
REAL    RearthKM
LOGICAL lflag

TYPE( landuse_init ) file_landuse

REAL, DIMENSION(:), ALLOCATABLE :: z

INTEGER, EXTERNAL :: TransGridCoord, SWIMallocTerrain
INTEGER, EXTERNAL :: SetGridSrcLimits
INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: InitLambert, InitPolar, InitMercator, InitRotPolar, InitRotLL
INTEGER, EXTERNAL :: InitLandUse

CHARACTER(128), EXTERNAL :: ArraySizeStr

SetGridParam = SWIMfailure

!------ Set earth radius to be consistent with WRF model

IF( BTEST(src%type,GSB_WRF) .OR. BTEST(src%type,GSB_WRFINP) )THEN
  RearthKM = 6370.
ELSE
  RearthKM = Rearth*1.E-3
END IF

!------ Setup grid structure based on input coordinates; compute map projection factors, if appropriate

IF( BTEST(src%type,GSB_LAMBERT) )THEN

  grid%type       = IBSET(grid%type,GTB_LAMBERT)
  grid%coord%type = I_LAMBERT

  irv = InitLambert( src%Lat0,src%Lat1,src%Lat2,RearthKM, &
                     grid%coord%n,grid%coord%f,grid%coord%m0,grid%coord%y0 )
  IF( irv /= 1 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error defining Lambert conformal projection'
    IF( irv == -1 )THEN
      error%Inform = 'Latitudes cannot exceed 89 degress'
    ELSE IF( irv == -2 )THEN
      error%Inform = 'First standard latitude must be less than second'
    ELSE IF( irv == -3 )THEN
      error%Inform = 'Standard latitudes cannot bracket equator'
    END IF
    GOTO 9999
  END IF

  grid%coord%Lat0 = src%Lat0
  grid%coord%Lon0 = src%Lon0
  grid%coord%Lat1 = src%Lat1
  grid%coord%Lat2 = src%Lat2
  grid%coord%Rearth = RearthKM

ELSE IF( BTEST(src%type,GSB_POLAR) )THEN

  grid%type = IBSET(grid%type,GTB_POLAR)
  grid%coord%type = I_POLAR

  irv = InitPolar( src%Lat0,src%Lat1,RearthKM,grid%coord%n,grid%coord%f,grid%coord%m0,grid%coord%y0 )
  IF( irv /= 1 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error defining Polar stereographic projection'
    IF( irv == -1 )THEN
      error%Inform = 'Latitudes cannot exceed 89 degress'
    END IF
    GOTO 9999
  END IF

  grid%coord%Lat0 = src%Lat0
  grid%coord%Lon0 = src%Lon0
  grid%coord%Lat1 = src%Lat1
  grid%coord%Lat2 = NOT_SET_R
  grid%coord%Rearth = RearthKM

ELSE IF( BTEST(src%type,GSB_RPOLAR) )THEN

  grid%type = IBSET(grid%type,GTB_RPOLAR)
  grid%coord%type = I_RPOLAR

  irv = InitRotPolar( src%Lat0,src%Lon0,grid%coord%sp0,grid%coord%cp0, &
                                        grid%coord%sl0,grid%coord%cl0, &
                                        grid%coord%cc0,grid%coord%sc0, &
                                        grid%coord%cs0,grid%coord%ss0  )
  IF( irv /= 1 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error defining rotated polar stereographic projection'
    IF( irv == -1 )THEN
      error%Inform = 'Latitudes cannot exceed 89 degress'
    END IF
    GOTO 9999
  END IF

  grid%coord%Lat0 = src%Lat0
  grid%coord%Lon0 = src%Lon0
  grid%coord%Lat1 = NOT_SET_R
  grid%coord%Lat2 = NOT_SET_R
  grid%coord%Rearth = 6.367E3  !Use RAMS value (but error is small)

ELSE IF( BTEST(src%type,GSB_ROTLL) )THEN

  grid%type = IBSET(grid%type,GTB_ROTLL)
  grid%coord%type = I_ROTLL

  irv = InitRotLL( src%Lat0,grid%coord%sp0,grid%coord%cp0 )
  IF( irv /= 1 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error initializing rotated lat/lon coordinates'
    IF( irv == -1 )THEN
      error%Inform = 'Latitudes cannot exceed 89 degress'
    END IF
    GOTO 9999
  END IF

  grid%coord%Lat0 = src%Lat0
  grid%coord%Lon0 = src%Lon0
  grid%coord%Lat1 = NOT_SET_R
  grid%coord%Lat2 = NOT_SET_R
  grid%coord%Rearth = RearthKM      !N.B Not needed for this coordinate system

ELSE IF( BTEST(src%type,GSB_MERCATOR) )THEN

  grid%type = IBSET(grid%type,GTB_MERCATOR)
  grid%coord%type = I_MERCATOR

  irv = InitMercator( src%Lat1,RearthKM,grid%coord%f,grid%coord%m0 )
  IF( irv /= 1 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error defining Mercator projection'
    IF( irv == -1 )THEN
      error%Inform = 'Latitudes cannot exceed 89 degress'
    END IF
    GOTO 9999
  END IF

  grid%coord%Lat0 = 0.
  grid%coord%Lon0 = src%Lon0
  grid%coord%Lat1 = src%Lat1
  grid%coord%Lat2 = NOT_SET_R
  grid%coord%n    = 0.
  grid%coord%Rearth = RearthKM

ELSE IF( BTEST(src%type,GSB_LATLON) )THEN

  grid%type       = IBSET(grid%type,GTB_LATLON)
  grid%coord%type = I_LATLON
  grid%coord%Rearth = RearthKM      !N.B Not needed for this coordinate system

ELSE IF( BTEST(src%type,GSB_CARTESIAN) )THEN

  grid%type = IBSET(grid%type,GTB_CARTESIAN)

  grid%coord%type          = I_CARTESIAN
  grid%coord%reference%x   = src%X0
  grid%coord%reference%y   = src%Y0
  grid%coord%reference%lat = src%Lat0
  grid%coord%reference%lon = src%Lon0
  grid%coord%Rearth = RearthKM      !N.B Not needed for this coordinate system

ELSE

  grid%type = IBSET(grid%type,GTB_UTM)
  grid%coord%type = I_UTM
  grid%coord%zone = src%UTMzone
  grid%coord%Rearth = RearthKM      !N.B Not needed for this coordinate system

END IF

!------ Limit grid extents based on project domain.
!       N.B. Cartesian grids in Lat/lon projects are not limited
!       (Also set in SWIMinit3DClimo)

lflag = BTEST(src%type,GSB_MEDOC) .OR. BTEST(src%type,GSB_SCIP)
lflag = lflag .OR. BTEST(src%type,GSB_WRF)
IF( lflag )THEN
  irv = SetGridSrcLimits( src,grid%coord )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Limit ABS(lat) <= POLARLIM_LAT

IF( BTEST(src%type,GSB_LATLON) )THEN

  DO
    latMax = src%y0 + (src%jEnd-1)*src%dY
    IF( latMax > POLARLIM_LAT )THEN
      src%jEnd = src%jEnd - 1
    ELSE
      EXIT
    END IF
    IF( src%jEnd <= src%jStart+1 )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error adjusting grid limits in northern latitude'
      WRITE(error%Inform,'(A,F5.1,A)') 'Limited to',POLARLIM_LAT,' degrees'
      CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
      GOTO 9999
    END IF
  END DO

  DO
    latMin = src%y0 + (src%jStart-1)*src%dY
    IF( latMin < -POLARLIM_LAT )THEN
      src%jStart = src%jStart + 1
    ELSE
      EXIT
    END IF
    IF( src%jStart >= src%jEnd-1 )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error adjusting grid limits in southern latitude'
      WRITE(error%Inform,'(A,F5.1,A)') 'Limited to -',POLARLIM_LAT,' degrees'
      CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
      GOTO 9999
    END IF
  END DO

END IF

!------ Compute skip parameter

IF( src%iSkip <= 0 )THEN

  CALL SetSkip( src%iEnd-src%iStart+1,src%jEnd-src%jStart+1,Prj%MAX1D_MET,iskip,nxb,nyb )

  jskip     = iskip
  src%iSkip = iskip
  src%jSkip = jskip

ELSE

  iskip = src%iSkip
  jskip = src%jSkip
  nxb   = (src%iEnd-src%iStart+iskip)/iskip
  nyb   = (src%jEnd-src%jStart+jskip)/iskip

END IF

!------ Set for periodic domain (SCIP or MEDOC only, not 3d climo)

IF( BTEST(src%type,GSB_PERIODIC) )THEN
  IF( BTEST(src%type,GSB_MEDOC) .OR. BTEST(src%type,GSB_SCIP) )THEN
    nxb = nxb + 1
  END IF
END IF

!------ Check horizontal grid size

nxyb = nxb*nyb

IF( nxyb <= 0                    .OR. &
    (nxb == 1 .AND. src%nX /= 1) .OR. &
    (nyb == 1 .AND. src%nY /= 1) .OR. &
    nxb > Prj%MAX1D_MET .OR. nyb > Prj%MAX1D_MET )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetGridParam'
  error%Message = 'Error setting met read skip parameter'
  CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
  WRITE(error%Inform,'(A,3I8)') 'nx,ny,nxy=',nxb,nyb,nxyb
  GOTO 9999
END IF

!------ Setup grid origin & spacing

grid%Xmin = src%X0 + (src%iStart-1)*src%dX
grid%Ymin = src%Y0 + (src%jStart-1)*src%dY
grid%dX = FLOAT(iskip)*src%dX
grid%dY = FLOAT(jskip)*src%dY

grid%nX = nxb; grid%nY = nyb; grid%nXY = nxyb

IF( BTEST(src%type,GSB_LATLON) )THEN
  grid%coord%reference%lat = src%Y0
  grid%coord%reference%lon = src%X0
ELSE IF( ANY(BTEST(src%type,(/GSB_LAMBERT,GSB_POLAR,GSB_RPOLAR,GSB_MERCATOR,GSB_ROTLL/))) )THEN
  grid%coord%reference%x   = 0.
  grid%coord%reference%y   = 0.
  grid%coord%reference%lat = src%Lat0
  grid%coord%reference%lon = src%Lon0
ELSE IF( BTEST(src%type,GSB_UTM) )THEN  !Already done for Cartesian
  grid%coord%reference%x   = src%X0
  grid%coord%reference%y   = src%Y0
  grid%coord%reference%lat = src%Lat0
  grid%coord%reference%lon = src%Lon0
END IF

!------ Set grid extents

IF( grid%dX <= 0. .OR. grid%dX <= 0. )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetGridParam'
  error%Message = 'Invalid grid spacing on met file'
  error%Inform  = 'Grid spacing must be positive'
  CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
  GOTO 9999
END IF

IF( nxb == 1 .AND. nyb == 1 )THEN !Grid spacing spans domain for single point grid

  grid%Xmax = grid%Xmin + grid%dX
  grid%Ymax = grid%Ymin + grid%dY

ELSE

  grid%Xmax = grid%Xmin + FLOAT(nxb-1)*grid%dX
  grid%Ymax = grid%Ymin + FLOAT(nyb-1)*grid%dY

END IF

!------ Check min/max latitudes (only for lat/lon input)
!       N.B. May exceed POLARCAP_LAT but should not get too close to 90 deg

IF( BTEST(src%type,GSB_LATLON) )THEN
  IF( grid%Ymax > POLARLIM_LAT .OR. grid%Ymin < -POLARLIM_LAT )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Latitude exceeds northern and/or southern limits'
    WRITE(error%Inform,'(A,F5.1,A)') 'Limited to +/-',POLARLIM_LAT,' degrees'
    CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
    GOTO 9999
  END IF
END IF

!------ Check/set project reference location for non-UTM project

IF( Prj%coord /= I_UTM )THEN
  IF( Prj%Lon0 == NOT_SET_R .OR. Prj%Lat0 == NOT_SET_R )THEN
    IF( grid%coord%reference%lon /= NOT_SET_R .AND. grid%coord%reference%lat /= NOT_SET_R )THEN
      Prj%Lon0 = grid%coord%reference%lon
      Prj%Lat0 = grid%coord%reference%lat
      IF( grid%coord%type /= I_LATLON )THEN
        Prj%Xref = grid%coord%reference%x
        Prj%Yref = grid%coord%reference%y
      END IF
    END IF
  END IF
END IF

!------ Reset Project coordinate structure

CALL SWIMsetPrjCoord()

!------ Set project domain (if not already set)

IF( Prj%Xmin == NOT_SET_R .OR. Prj%Xmin == DEF_VAL_R .OR. &
    Prj%Xmax == NOT_SET_R .OR. Prj%Xmax == DEF_VAL_R .OR. &
    Prj%Ymin == NOT_SET_R .OR. Prj%Ymin == DEF_VAL_R .OR. &
    Prj%Ymax == NOT_SET_R .OR. Prj%Ymax == DEF_VAL_R )THEN

  IF( Prj%coord == grid%coord%type )THEN

    Prj%Xmin = grid%Xmin
    Prj%Xmax = grid%Xmax
    Prj%Ymin = grid%Ymin
    Prj%Ymax = grid%Ymax

  ELSE
    Prj%Xmin = -HUGE(0.); Prj%Xmax = HUGE(0.)
    Prj%Ymin = -HUGE(0.); Prj%Ymax = HUGE(0.)

    DO i = 1,grid%nY
      ytem = grid%Ymin + FLOAT(i-1)*grid%dY
      irv = SWIMcnvCoord( grid%Xmin,ytem,grid%coord,xtem,tem,PrjCoord )
      Prj%Xmin = MAX(Prj%Xmin,xtem)
      irv = SWIMcnvCoord( grid%Xmax,ytem,grid%coord,xtem,tem,PrjCoord )
      Prj%Xmax = MIN(Prj%Xmax,xtem)
    END DO
    DO i = 1,grid%nX
      xtem = grid%Xmin + FLOAT(i-1)*grid%dX
      irv = SWIMcnvCoord( xtem,grid%Ymin,grid%coord,tem,ytem,PrjCoord )
      Prj%Ymin = MAX(Prj%Ymin,ytem)
      irv = SWIMcnvCoord( xtem,grid%Ymax,grid%coord,tem,ytem,PrjCoord )
      Prj%Ymax = MIN(Prj%Ymax,ytem)
    END DO
  END IF
END IF

!------ Setup vertical grid; may be Sigma-P(MM5), 3d height field, or Sigma-Z

IF( BTEST(Src%type,GSB_SIGMAF) )THEN

  grid%type  = IBSET(grid%type,GTB_SIGMA)
  grid%type  = IBSET(grid%type,GTB_Z3D)
  src%kStart = 1

ELSE IF( BTEST(Src%type,GSB_ZMSL) .OR. BTEST(Src%type,GSB_ZAGL) )THEN

  grid%type  = IBSET(grid%type,GTB_Z3D)

  src%kStart = 1
  DO k = 1,src%nZ
    src%Z(k) = FLOAT(k)
  END DO

ELSE                                    !Sigma-Z

  k = 0
  DO
    k = k + 1
    IF( src%Z(k) > 0. )THEN !First valid level must be above ground level
      src%kStart = k; EXIT
    ELSE IF( k == src%nZ )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'No valid grid level heights'
      CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
      GOTO 9999
    END IF
  END DO

END IF

nzb     = (src%nZ-src%kStart) + 1
grid%nZ = nzb

ALLOCATE( grid%Z(nzb),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SetGridParam'
  error%Message = 'Error allocating vertical grid'
  CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
  WRITE(error%Inform,'(A,I8)') 'Size=',nzb
  GOTO 9999
END IF

DO k = 1,nzb
  grid%Z(k) = src%Z(src%kStart-1+k)
END DO

!------ Check for valid staggered grid setup

IF( src%nStg2d > 0 )  THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetGridParam'
  error%Message = 'Staggered 2d fields not supported'
  CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
  GOTO 9999
END IF

IF( src%nStg3d > 0 )THEN

  iu = 0; iv = 0; iw = 0; iz = 0

  DO i = 1,src%nStg3D
    SELECT CASE( src%iStg3D(0,i) )

      CASE( GVP_U )
        IF( src%iStg3D(3,i) /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetGridParam'
          error%Message = 'U cannot be offset vertically from cell center'
          CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
          GOTO 9999
        END IF
        IF( src%iStg3D(1,i) /= 0 )iu = iu + 1
        IF( src%iStg3D(2,i) /= 0 )iu = iu + 1

      CASE( GVP_V )
        IF( src%iStg3D(3,i) /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetGridParam'
          error%Message = 'V cannot be offset vertically from cell center'
          CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
          GOTO 9999
        END IF
        IF( src%iStg3D(1,i) /= 0 )iv = iv + 1
        IF( src%iStg3D(2,i) /= 0 )iv = iv + 1

      CASE( GVP_W )
        IF( src%iStg3D(1,i) /= 0 .OR. src%iStg3D(2,i) /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetGridParam'
          error%Message = 'W cannot be offset horizontally from cell center'
          CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
          GOTO 9999
        END IF
        IF( src%iStg3D(3,i) /= 0 )iw = iw + 1

      CASE( GVP_Z )
        IF( src%iStg3D(1,i) /= 0 .OR. src%iStg3D(2,i) /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SetGridParam'
          error%Message = 'Z cannot be offset horizontally from cell center'
          CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
          GOTO 9999
        END IF
        IF( src%iStg3D(3,i) /= 0 )src%type = IBSET(src%type,GSB_ZSTAGGER)

      CASE DEFAULT
        error%Number  = IV_ERROR
        error%Routine = 'SetGridParam'
        error%Message = 'Invalid staggered 3d field: only U, V, W or Z can be offset'
        WRITE(error%Inform,"('Field no. ',I2,' in list of staggered fields')",IOSTAT=ios) i
        GOTO 9999

    END SELECT
  END DO

  IF( iu /= iv  .OR. iu > 2 .OR. iv > 2 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Invalid U and V grid locations'
    error%Inform  = 'Only Arakawa A, B or C grids are supported'
    CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
    GOTO 9999
  END IF

  IF( (iw > 0 .AND. iu == 0) .OR. (iw == 0 .AND. iu > 0) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Invalid or unsupported staggered grid definition'
    error%Inform  = 'U, V and W must all be offset or none (all co-located)'
    CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
    GOTO 9999
  END IF

!------ Set staggered grid parameters (only if U & V are actually offset)

  IF( iu > 0 )THEN

    grid%type = IBSET(grid%type,GTB_STAGGER)
    grid%type = IBSET(grid%type,GTB_STAGGERZ)                !Vertical stagger (W only) required at this point
    IF( BTEST(src%type,GSB_ZSTAGGER) ) &
      grid%type = IBSET(grid%type,GTB_Z3DW)                  !3d heights located at W-points

    IF( iu == 2 )grid%type = IBSET(grid%type,GTB_STAGGERB)   !Arakawa B if any U,V shifted in x and y (C is default)

    src%type = IBSET(src%type,GSB_STAGGER)
    IF( iu == 2 )src%type = IBSET(src%type,GSB_STAGGERB)

    ALLOCATE( grid%Zw(nzb+1),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error allocating w-vertical grid'
      CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
      WRITE(error%Inform,'(A,I8)') 'Size=',nzb+1
      GOTO 9999
    END IF

      IF( BTEST(grid%type,GTB_SIGMA) )THEN
        grid%Zw(1) = 1.                     !Assuming ground level
      ELSE
        grid%Zw(1) = 0.
      END IF
      DO i = 2,nzb
        grid%Zw(i) = 0.5*(grid%Z(i-1)+grid%Z(i))
      END DO
      IF( BTEST(grid%type,GTB_SIGMA) )THEN
        grid%Zw(nzb+1) = 0.                     !Assuming top of domain
      ELSE
        grid%Zw(nzb+1) = 1.5*grid%Z(nzb) - 0.5*grid%Z(nzb-1)
      END IF

!------ Setup shift index arrays for staggered grids

    ALLOCATE( Src%Var3dShift(3,Src%nVar3D),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error allocating stagger index arrays for 3d fields'
      GOTO 9999
    END IF

    Src%Var3dShift = 0

    DO i = 1,Src%nStg3d
      DO k = 1,Src%nVar3D
        IF( Src%Var3dID(k) == Src%iStg3d(0,i) )THEN
          Src%Var3dShift(:,k) = Src%iStg3d(1:3,i)
          EXIT
        END IF
      END DO
    END DO

  ELSE IF( BTEST(src%type,GSB_ZSTAGGER) )THEN

    ALLOCATE( Src%Var3dShift(3,Src%nVar3D),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error allocating stagger index arrays for 3d fields'
      GOTO 9999
    END IF

    Src%Var3dShift = 0

    DO i = 1,Src%nStg3d
      DO k = 1,Src%nVar3D
        IF( Src%Var3dID(k) == Src%iStg3d(0,i) )THEN
          Src%Var3dShift(:,k) = Src%iStg3d(1:3,i)
          EXIT
        END IF
      END DO
    END DO

  END IF

END IF

grid%Ztop = src%Ztop

!------ Check vertical grid

Src%lVertFlip = grid%Z(1) > grid%Z(nzb)

IF( Src%lVertFlip )THEN
  DO k = 1,nzb-1
    IF( grid%Z(k) <= grid%Z(k+1) )THEN
      error%Number   = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Inconsistent vertical grid. First grid level greater than last'
      error%Inform  = 'But levels do not decrease monotonically'
      CALL ReportFileName( error%Action,'File= ',Src%Source(1) )
      GOTO 9999
    END IF
  END DO
ELSE
  DO k = 1,nzb-1
    IF( grid%Z(k) >= grid%Z(k+1) )THEN
      error%Number   = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Vertical grid levels must increase monotonically'
      CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
      GOTO 9999
    END IF
  END DO
END IF

!------ Reverse vertical flip flag for sigma coordinates
!       i.e., assume top level (sigma=0) comes first

IF( BTEST(Src%type,GSB_SIGMAF) )Src%lVertFlip = .NOT.Src%lVertFlip

IF( Src%lVertFlip )THEN

  ALLOCATE( z(nzb),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error allocating vertical grid for sigma coordinates'
    GOTO 9999
  END IF

  z(1:nzb) = grid%Z(1:nzb)

  DO k = 1,nzb
    grid%Z(k) = z(nzb-k+1)
  END DO

  DEALLOCATE( z,STAT=alloc_stat )

  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN

    ALLOCATE( z(nzb+1),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error allocating vertical grid for sigma coordinates'
      GOTO 9999
    END IF

    z(1:nzb+1) = grid%Zw(1:nzb+1)

    DO k = 2,nzb
      grid%Zw(k) = z(nzb+1-k+1)  !N.B. top/bottom already set above
    END DO

    DEALLOCATE( z,STAT=alloc_stat )

  END IF

END IF

!------ Allocate 3d height and 2d surface pressure arrays for sigma coordinates

IF( BTEST(grid%type,GTB_SIGMA) )THEN

  ntot = grid%nXY*grid%nZ
  ALLOCATE( grid%sigma%Z(ntot), &
            grid%sigma%Psrf(grid%nXY), &
            grid%sigma%Px(grid%nXY),grid%sigma%Py(grid%nXY),STAT=alloc_stat)
  IF( alloc_stat /= 0 )THEN
    error%Number   = IV_ERROR
    error%Routine = 'SetGridParam'
    error%Message = 'Error allocating arrays for sigma height levels'
    CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
    GOTO 9999
  END IF
  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
    ntot = grid%nXY*(grid%nZ+1)
    ALLOCATE( grid%sigma%Zw(ntot),STAT=alloc_stat)
    IF( alloc_stat /= 0 )THEN
      error%Number   = IV_ERROR
      error%Routine = 'SetGridParam'
      error%Message = 'Error allocating arrays for sigma height w-levels'
      GOTO 9999
    END IF
  END IF

!------ Allocate 3d height arrays for 3d height coordinates

ELSE IF( BTEST(grid%type,GTB_Z3D) )THEN

  IF( BTEST(grid%type,GTB_STAGGERZ) )THEN
    IF( BTEST(grid%type,GTB_Z3DW) )THEN
      ntot = grid%nXY*grid%nZ
      ALLOCATE( grid%sigma%Z(ntot),STAT=alloc_stat)
      IF( alloc_stat /= 0 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetGridParam'
        error%Message = 'Error allocating arrays for sigma height levels'
        CALL ReportFileName( error%Inform,'File= ',Src%Source(1) )
        GOTO 9999
      END IF
    ELSE
      ntot = grid%nXY*(grid%nZ+1)
      ALLOCATE( grid%sigma%Zw(ntot),STAT=alloc_stat)
      IF( alloc_stat /= 0 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SetGridParam'
        error%Message = 'Error allocating arrays for sigma height w-levels'
        GOTO 9999
      END IF
    END IF
  END IF

END IF

!------ Set terrain/landuse bits

DO i = 1,src%nVar2d
  SELECT CASE( src%Var2dID(i) )

    CASE( GVP_TERRAIN )
      grid%type = IBSET(grid%type,GTB_TERRAIN)

    CASE( GVP_ZRUF,GVP_ZRUFT )
      grid%type = IBSET(grid%type,GTB_ZRUF)

    CASE( GVP_HCNP )
      grid%type = IBSET(grid%type,GTB_HCNP)

    CASE( GVP_ALPHA )
      grid%type = IBSET(grid%type,GTB_ALPHA)

    CASE( GVP_ALBEDO )
      grid%type = IBSET(grid%type,GTB_ALBEDO)

    CASE( GVP_BOWEN )
      grid%type = IBSET(grid%type,GTB_BOWEN)

    CASE( GVP_LANDUSE )
      grid%type = IBSET(grid%type,GTB_LANDUSE)

  END SELECT
END DO

!------ Allocate terrain/landuse arrays

irv = SWIMallocTerrain( grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set surface parameters that may be "not set" based on landuse category

CALL SetPrjBL()
IF( error%Number  /= NO_ERROR )GOTO 9999

!------ Initialize terrain and landcover if not on gridded file

IF( .NOT.BTEST(grid%type,GTB_TERRAIN ) )THEN
  DO i = 1,nxyb
    grid%terrain%H(i)  = 0.
    grid%terrain%Hx(i) = 0.
    grid%terrain%Hy(i) = 0.
    grid%terrain%D(i)  = 1.
    grid%terrain%Du(i) = 1.
    grid%terrain%Dv(i) = 1.
  END DO
  grid%Hmin = 0.
END IF

IF( BTEST(grid%type,GTB_LANDUSE)   )THEN

  file_landuse%lun  = 0
  file_landuse%file = TRIM(Prj%MC%LandUseFile)

  irv = InitLandUse( file_landuse )
  IF( irv /= 1 )THEN
    CALL SWIMgetLandUseError()
    GOTO 9999
  END IF

END IF

IF( BTEST(grid%type,GTB_HCNP)   )THEN
  IF( .NOT.BTEST(grid%type,GTB_ZRUF)   )THEN
    DO i = 1,nxyb
      grid%landcover%roughness(i) = Prj%BL%zruf
    END DO
  END IF
  IF( .NOT.BTEST(grid%type,GTB_ALPHA)   )THEN
    DO i = 1,nxyb
      grid%landcover%roughness(i) = Prj%BL%alpha
    END DO
  END IF
ELSE
  IF( BTEST(grid%type,GTB_ZRUF)   )THEN
    DO i = 1,nxyb
      grid%landcover%canopyHt(i) = 0.   !No canopy if roughness specified (w/o canopy)
      grid%landcover%alpha(i)    = 0.
    END DO
  ELSE
    DO i = 1,nxyb
      grid%landcover%canopyHt(i)  = Prj%BL%hc
      grid%landcover%alpha(i)     = Prj%BL%alpha
      grid%landcover%roughness(i) = Prj%BL%zruf
    END DO
  END IF
END IF

IF( .NOT.BTEST(grid%type,GTB_BOWEN)  )THEN
  DO i = 1,nxyb
    grid%landcover%Bowen(i) = Prj%BL%Bowen
  END DO
END IF
IF( .NOT.BTEST(grid%type,GTB_ALBEDO) )THEN
  DO i = 1,nxyb
    grid%landcover%albedo(i) = Prj%BL%albedo
  END DO
END IF

SetGridParam = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SetGridSrcLimits( src,coord )

!------ Compute limits of gridded source that contain project domain

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src
TYPE( MapCoord ), INTENT( IN    ) :: coord

INTEGER irv
INTEGER i, j, i1, i2, j1, j2
REAL    xtem, ytem, x, y

INTEGER, EXTERNAL :: SWIMcnvCoord

SetGridSrcLimits = SWIMfailure

!------ If project domain not set, use full gridded met domain

IF( Prj%Xmin == NOT_SET_R .OR. Prj%Xmin == DEF_VAL_R .OR. &
    Prj%Xmax == NOT_SET_R .OR. Prj%Xmax == DEF_VAL_R .OR. &
    Prj%Ymin == NOT_SET_R .OR. Prj%Ymin == DEF_VAL_R .OR. &
    Prj%Ymax == NOT_SET_R .OR. Prj%Ymax == DEF_VAL_R )THEN
  src%iStart = 1; src%iEnd = src%nX
  src%jStart = 1; src%jEnd = src%nY
  SetGridSrcLimits = SWIMresult
  GOTO 9999
END IF

!------ Loop over met domain and compute project coordinates;
!       (Don't set limits for global (lat/lon) project or met)
!       Check if cell is within project domain; set grid index limits

i1 = HUGE(0); i2 = -HUGE(0)
j1 = HUGE(0); j2 = -HUGE(0)

IF( Prj%coord == I_LATLON      .AND. Prj%Xmax-Prj%Xmin >= 359.9 .OR. &
    BTEST(Src%type,GSB_LATLON) .AND. src%nX*src%dX     >= 359.9 )THEN

  i1 = 1; i2 = src%nX

ELSE

  i2 = 1; i1 = src%nX
  j2 = 1; j1 = src%nY

  DO i = 1,src%nX
    x = src%X0 + FLOAT(i-1)*src%dX
    DO j = 1,src%nY
      y = src%Y0 + FLOAT(j-1)*src%dY
      irv = SWIMcnvCoord( x,y,coord,xtem,ytem,PrjCoord )
      IF( irv == SWIMfailure )EXIT
      IF( ytem >= Prj%Ymin )THEN
        j1 = MIN(j1,j-1)
        EXIT
      END IF
    END DO
    DO j = src%nY,1,-1
      y = src%Y0 + FLOAT(j-1)*src%dY
      irv = SWIMcnvCoord( x,y,coord,xtem,ytem,PrjCoord )
      IF( irv == SWIMfailure )EXIT
      IF( ytem <= Prj%Ymax )THEN
        j2 = MAX(j2,j+1)
        EXIT
      END IF
    END DO
  END DO

  IF( Prj%coord == I_LATLON .AND. BTEST(Src%type,GSB_LATLON) )THEN
    IF( Prj%Xmin <= src%X0 )THEN
      IF( Prj%Xmin+360. < src%X0 + FLOAT(src%nx-1)*src%dX )THEN
        i1 = 1; i2 = src%nX
      END IF
    ELSE IF( Prj%Xmax > src%X0 + FLOAT(src%nx-1)*src%dX  )THEN
      IF( Prj%Xmax-360. < src%X0 )THEN
        i1 = 1; i2 = src%nX
      END IF
    END IF
  END IF

  IF( i1 == src%nX )THEN
    DO j = 1,src%nY
      y = src%Y0 + FLOAT(j-1)*src%dY
      DO i = 1,src%nX
        x = src%X0 + FLOAT(i-1)*src%dX
        irv = SWIMcnvCoord( x,y,coord,xtem,ytem,PrjCoord )
        IF( irv == SWIMfailure )EXIT
        IF( xtem >= Prj%Xmin )THEN
          i1 = MIN(i1,i-1)
          EXIT
        END IF
      END DO
      DO i = src%nX,1,-1
        x = src%X0 + FLOAT(i-1)*src%dX
        irv = SWIMcnvCoord( x,y,coord,xtem,ytem,PrjCoord )
        IF( irv == SWIMfailure )EXIT
        IF( xtem <= Prj%Xmax )THEN
          i2 = MAX(i2,i+1)
          EXIT
        END IF
      END DO
    END DO
  END IF

END IF

IF( i1 > i2 .OR. j1 > j2 )THEN  !Domain mismatch - don't limit met domain here

  src%iStart = 1
  src%jStart = 1

  src%iEnd = src%nX
  src%jEnd = src%nY

ELSE  !------ Try to go slightly outside project domain

  src%iStart = i1 - 1
  src%jStart = j1 - 1

  src%iEnd = i2 + 1
  src%jEnd = j2 + 1

END IF

src%iStart = MAX(src%iStart,1); src%iEnd = MIN(src%iEnd,src%nX)
src%jStart = MAX(src%jStart,1); src%jEnd = MIN(src%jEnd,src%nY)

SetGridSrcLimits = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE InitGridSrcLimits( src )

USE SWIM_fi

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: src

src%iStart = 1; src%iEnd = src%nX; src%iSkip = 0
src%jStart = 1; src%jEnd = src%nY; src%jSkip = 0

RETURN
END
