INTEGER FUNCTION SWIMinitWRF( unit )

!----- Setup appropriate number of met fields
!      Build WRF file names and store in GridSource structure

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE netcdf_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: unit

CHARACTER(PATH_MAXLENGTH) :: path, root, string, listFile

INTEGER irv, alloc_stat, lun, iskip
INTEGER i, j, nrd, ngrids, ifld, if1
LOGICAL lexist

INTEGER, DIMENSION(99) :: iGrid

INTEGER, EXTERNAL :: SWIMreallocMetField, SWIMaddLogMessage, PostProgressMessage
INTEGER, EXTERNAL :: InitWRFsource
REAL,    EXTERNAL :: GetWRFTime

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: WRFfileName

!------ Initialize failure

SWIMinitWRF  = SWIMfailure

listFile = TRIM(field(numField)%gridSource%Source(1))

error%Routine = 'SWIMinitWRF'
error%Number  = RD_ERROR
CALL ReportFileName( error%Inform,'File=',listFile )

!------ Setup messaging

message%bString = 'Reading WRF list file '//TRIM(listFile)

irv = PostProgressMessage( message )

irv = SWIMaddLogMessage( message%bString )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Open list file

lun = unit
OPEN(UNIT=lun,FILE=TRIM(listFile),STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=irv )
IF( irv /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening WRF list file'
  GOTO 9999
END IF

!------ Initialize path and grid specs

path   = ''
ngrids = 0
iGrid  = 0
iskip  = 1

!------ Read first line: should be 'WRF'

READ(lun,FMT='(A)',IOSTAT=irv) string
IF( irv /= 0 )THEN
  error%Message = 'Error reading first line in WRF list file'
  GOTO 9999
END IF

IF( TRIM(string) /= 'WRF' )THEN
  error%Message = 'First line in WRF list file must be "WRF"'
  GOTO 9999
END IF

!------ Read second line: check for path or grid specs; otherwise it's the file name

READ(lun,FMT='(A)',IOSTAT=irv) root
IF( irv /= 0 )THEN
  error%Message = 'Error reading first line in WRF list file'
  GOTO 9999
END IF

string = root !Copy and make case insensitive
CALL cupper( string )

i = INDEX(string,'PATH=')
IF( i > 0 )THEN
  path = TRIM(root(i+5:))
  iskip = iskip + 1
ELSE
  i = INDEX(string,'GRIDS=')
  IF( i > 0 )THEN
    CALL ParseWRFgridList( string,ngrids,iGrid )
    iskip = iskip + 1
  END IF
END IF

!------ Check second line for path or grid specs

IF( i > 0 )THEN

  READ(lun,FMT='(A)',IOSTAT=irv) root
  IF( irv /= 0 )THEN
    error%Message = 'Error reading second line in WRF list file'
    GOTO 9999
  END IF

  string = root !Copy and make case insensitive
  CALL cupper( string )

  i = INDEX(string,'PATH=')
  IF( i > 0 )THEN
    path  = TRIM(root(i+5:))
    iskip = iskip + 1
  ELSE
    i = INDEX(string,'GRIDS=')
    IF( i > 0 )THEN
      CALL ParseWRFgridList( string,ngrids,iGrid )
      iskip = iskip + 1
    END IF
  END IF

  IF( i > 0 )THEN   !Read first file name (if previous line was special input)
    READ(lun,FMT='(A)',IOSTAT=irv) root
    IF( irv /= 0 )THEN
      error%Message = 'Error reading file name in WRF list file'
      GOTO 9999
    END IF
  END IF

END IF

!------ Check number of grids

IF( ngrids > 99 .OR. MAXVAL(iGrid) > 99 )THEN
  error%Number  = IV_ERROR
  error%Message = 'Too many WRF grids'
  error%Action  = 'Max number is 99'
  GOTO 9999
END IF

IF( ngrids > 0 )THEN
  IF( MINVAL(iGrid(1:ngrids)) < 1 )THEN
    error%Message = 'Invalid grid number specified'
    error%Action = 'Must be > 1'
    GOTO 9999
  END IF
END IF

CALL AddPath( root,path )

!------ Setup for multiple grids

ngrids = MAX(ngrids,1)

if1 = numField
irv = SWIMreallocMetField( ngrids-1 )    !Increments numField (already done for first field)
IF( irv /= SWIMsuccess )GOTO 9999

DO i = if1,numField
  j = i - if1 + 1
  string = WRFfileName( root,iGrid(j) )
  INQUIRE( FILE=TRIM(string),EXIST=lexist )    !Make sure files exist
  IF( .NOT.lexist )THEN
    error%Number  = IV_ERROR
    error%Message = 'WRF file does not exist'
    CALL ReportFileName( error%Inform,'File=',string )
  END IF

  IF( j > 1 )THEN
    ALLOCATE( field(i)%gridSource%Source(1),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Message = 'Error allocating array for WRF file name'
      GOTO 9999
    END IF
  END IF

  field(i)%gridSource%Source(1) = TRIM(string)                       !Save name for initialization

  field(i)%gridSource%type = 0                                       !Resets first field from GSB_INITWRF
  field(i)%gridSource%type = IBSET(field(i)%gridSource%type,GSB_WRF)
  field(i)%gridSource%unit = 1                                       !Used as index into array of "source" files

  field(i)%type = IBSET(field(i)%type,FTB_WRF)

END DO

WRITE(message%bString,FMT='(A,99I3)',IOSTAT=irv) 'Using WRF grids',iGrid(1:ngrids)

irv = PostProgressMessage( message )

irv = SWIMaddLogMessage( message%bString )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Define source parameters for each grid

DO i = if1,numField
  irv = InitWRFsource( field(i)%gridSource )
  IF( irv /= SWIMsuccess )GOTO 9999
END DO

!------ Read list file names and save

REWIND(lun,IOSTAT=irv)
IF( iskip > 0 )THEN
  DO i = 1,iskip
    READ(lun,FMT=*,IOSTAT=irv) !Skip path name and/or grid specs
  END DO
END IF

nrd = 0
DO
  READ(lun,FMT='(A)',IOSTAT=irv) string
  IF( irv < 0 )THEN
    EXIT  !End-of-file
  ELSE IF( irv > 0 )THEN
    error%Message = 'Error reading WRF list file'
    GOTO 9999
  ELSE
    nrd = nrd + 1
  END IF
END DO

REWIND(lun,IOSTAT=irv)
IF( iskip > 0 )THEN
  DO i = 1,iskip
    READ(lun,FMT=*,IOSTAT=irv) !Skip path name and/or grid specs
  END DO
END IF

DEALLOCATE( field(if1)%gridSource%Source,STAT=alloc_stat )

DO i = if1,numField
  field(i)%gridSource%nSource = 0
  ALLOCATE( field(i)%gridSource%Source(nrd),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Message = 'Error allocating space for input file names'
    GOTO 9999
  END IF
END DO

DO i = 1,nrd

  READ(lun,FMT='(A)',IOSTAT=irv) root
  IF( irv > 0 )THEN
    error%Message = 'Error reading WRF list file'
    GOTO 9999
  END IF

  DO j = 1,ngrids
    ifld = if1 + j-1

    string = WRFfileName( root,iGrid(j) )
    CALL ADDPATH( string,path )
    INQUIRE( FILE=TRIM(string),EXIST=lexist )
    IF( lexist )THEN
      field(ifld)%gridSource%nSource = field(ifld)%gridSource%nSource + 1
      field(ifld)%gridSource%Source(field(ifld)%gridSource%nSource) = TRIM(string)
    END IF

  END DO

END DO

!------ Set index to latest file with time equal to or earlier than project start time

DO j = 1,ngrids
  ifld = if1 + j-1

  field(ifld)%gridSource%tFirst = GetWRFTime( field(ifld)%gridSource%Source(1),0,field(ifld)%gridSource%tFcst,1 )
  IF( error%Number /= NO_ERROR )GOTO 9999

  i = 1
  IF( Reverse )THEN
    DO WHILE( GetWRFTime(field(ifld)%gridSource%Source(i),0,field(ifld)%gridSource%tFcst,9999) >= Prj%time )
      i = i + 1
      IF( i > field(ifld)%gridSource%nSource )EXIT
    END DO
    field(ifld)%gridSource%unit = MIN(i,field(ifld)%gridSource%nSource)
  ELSE
    DO WHILE( GetWRFTime(field(ifld)%gridSource%Source(i),0,field(ifld)%gridSource%tFcst,1) <= Prj%time )
      i = i + 1
      IF( i > field(ifld)%gridSource%nSource )EXIT
    END DO
    field(ifld)%gridSource%unit = MAX(i-1,1)
  END IF

END DO

SWIMinitWRF = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

CLOSE(lun,IOSTAT=irv )

RETURN
END

!==============================================================================

SUBROUTINE ParseWRFgridList( string,ngrids,iGrid )

IMPLICIT NONE

CHARACTER(*),          INTENT( IN  ) :: string
INTEGER,               INTENT( OUT ) :: ngrids
INTEGER, DIMENSION(*), INTENT( OUT ) :: iGrid

INTEGER, PARAMETER :: MAXN = 99

CHARACTER(256) kwrd
CHARACTER(32)  c_arg(MAXN)
CHARACTER(256) string2

INTEGER i, nch, irv, n_arg
LOGICAL lerr

ngrids = 0

i = INDEX( string,'GRIDS=')
IF( i == 0 )GOTO 9999

string2 = TRIM(string(7:))
IF( LEN_TRIM(string2) == 0 )GOTO 9999

DO i = 1,LEN_TRIM(string2)
  IF( string2(i:i) == ',' )string2(i:i) = ' '
END DO

CALL get_next_data( 0,string2,nch,kwrd,n_arg,c_arg,99,lerr )
IF( lerr )GOTO 9999

DO i = 1,n_arg
  READ(c_arg(i),FMT=*,IOSTAT=irv) iGrid(i)
  IF( irv /= 0 )GOTO 9999
END DO

ngrids = n_arg

9999 CONTINUE

RETURN
END

!==============================================================================

CHARACTER(*) FUNCTION WRFfileName( root,ig ) RESULT( fname )

!------ Build file name based on root and grid number
!       File name is root if ig=0 or does not begin with 'wrfout-d'

USE DefSize_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: root
INTEGER,      INTENT( IN ) :: ig

INTEGER i

CHARACTER(PATH_MAXLENGTH) str

fname = TRIM(root)

IF( ig > 0 )THEN

  str = TRIM(fname)
  CALL cupper(str)
  i = INDEX(str,'WRFOUT_D')

  IF( i > 0 )THEN
    i = i + 8  !Grid id characters (assume file name ..\wrfout-dxx)
    WRITE(str,'(I2.2)') ig
    fname(i:i+1) = str(1:2)
  END IF

END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION InitWRFsource( Src )

!------ Setup gridded source structure by reading WRF file

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE string_utils
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

TYPE( GridSrc ), INTENT( INOUT ) :: Src

INTEGER fid, irv, alloc_stat
INTEGER i, j, k, id_z
INTEGER nxi, nyi, nzi
INTEGER ivtype, ndims, natts, nvars, unlimdimID
INTEGER coordType, LUnumCat
INTEGER nv2, nv3
INTEGER iyear, imon, iday, ihour, imin, isec
REAL    dx, dy, RearthKM
REAL    rn, x0, y0, f, m0, yy0
REAL    lonLim, xlat, xlon
LOGICAL lZruf, lLU, lQ2, lTH2, lQFX, lHFX

INTEGER(C_INTPTR_T) k_len

INTEGER, DIMENSION(3,3) :: istg3d

INTEGER, DIMENSION(NF_MAX_VAR_DIMS) :: dimids

CHARACTER(NF_MAX_NAME) varnam, string, analysisTime, LUmodel

INTERFACE
  REAL FUNCTION get_real1( fid,var_name,is ) RESULT( x )
    INTEGER,           INTENT( IN ) :: fid
    CHARACTER*(*),     INTENT( IN ) :: var_name
    INTEGER, OPTIONAL, INTENT( IN ) :: is
  END FUNCTION get_real1
END INTERFACE

INTEGER, EXTERNAL :: SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMwarningMessage
INTEGER, EXTERNAL :: InitLambert, InitPolar, InitMercator
INTEGER, EXTERNAL :: LL2Lambert, LL2Polar, LL2Mercator
REAL,    EXTERNAL :: GetTimeMet, SWIMtimeOffset

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull, StripNull

!------ Initialize failure

InitWRFsource = SWIMfailure

error%Routine = 'InitWRFsource'
error%Number  = UK_ERROR
error%Message = 'Error reading WRF file'
CALL ReportFileName( error%Inform,'File=',Src%source(1) )

!------ Open file

irv = nc_open( AddNull(Src%source(1)),NF_NOWRITE,fid )
IF( irv /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening WRF file'
  error%Action = StringCToF( nc_strerror( irv ) )
  fid = 0
  GOTO 9999
END IF

!----- Get general file information

irv = nc_inq( fid,ndims,nvars,natts,unlimdimID ); IF( irv /= 0 )GOTO 9999

!----- Convert analysis time to project time

irv = nc_get_att_text( fid,NC_GLOBAL,'SIMULATION_START_DATE',analysisTime ); IF( irv /= 0 )GOTO 9999

string = StripNull(analysisTime)

READ(string,FMT='(I4,5(1X,I2))',IOSTAT=irv) iyear,imon,iday,ihour,imin,isec
IF( irv /= 0 )THEN
  error%Number  = RD_ERROR
  error%Message = 'Error reading analysis time'
  GOTO 9999
END IF

Src%timeOffset = SWIMtimeOffset()
Src%tFcst      = GetTimeMet( iday,imon,iyear,ihour,imin,isec,Src%timeOffset )

!----- Set grid dimensions

DO i = 0,ndims-1
  irv = nc_inq_dim( fid,i,varnam,k_len );
  IF( irv /= 0 )THEN
    error%Action = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF
  irv = nc_inq_dimid( fid,varnam,j )

  SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )
    CASE( 'west_east' )
      nxi = k_len
    CASE( 'south_north' )
      nyi = k_len
    CASE( 'bottom_top' )
      nzi = k_len
  END SELECT
END DO

!----- Setup model coordinate parameters and land use model

LUmodel  = ''
LUnumCat = 0

DO j = 0,natts-1

  irv = nc_inq_attname( fid,NC_GLOBAL,j,varnam ); IF( irv /= 0 )GOTO 9999
  SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )

    CASE( 'MAP_PROJ' )
      irv = nc_get_att_int( fid,NC_GLOBAL,varnam,k )
      SELECT CASE( k )
        CASE( 1 )
          Src%type  = IBSET(Src%type,GSB_LAMBERT)
          coordType = I_LAMBERT
        CASE( 2 )
          Src%type  = IBSET(Src%type,GSB_POLAR)
          coordType = I_POLAR
        CASE( 3 )
          Src%type  = IBSET(Src%type,GSB_MERCATOR)
          coordType = I_MERCATOR
        CASE DEFAULT
          error%Number  = IV_ERROR
          error%Message = 'Unsupported map projection'
          error%Action = StringCToF( nc_strerror( irv ) )
          GOTO 9999
      END SELECT

    CASE( 'DX' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,dx ); IF( irv /= 0 )GOTO 9999
      dx = dx*0.001

    CASE( 'DY' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,dy ); IF( irv /= 0 )GOTO 9999
      dy = dy*0.001

    CASE( 'TRUELAT1' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,Src%Lat1 ); IF( irv /= 0 )GOTO 9999

    CASE( 'TRUELAT2' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,Src%Lat2 ); IF( irv /= 0 )GOTO 9999

    CASE( 'MOAD_CEN_LAT' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,Src%Lat0 ); IF( irv /= 0 )GOTO 9999

    CASE( 'STAND_LON' )
      irv = nc_get_att_float( fid,NC_GLOBAL,varnam,Src%Lon0 ); IF( irv /= 0 )GOTO 9999

    CASE( 'MMINLU' )
      irv = nc_get_att_text( fid,NC_GLOBAL,varnam,LUmodel); IF( irv /= 0 )GOTO 9999

    CASE( 'NUM_LAND_CAT' )
      irv = nc_get_att_int( fid,NC_GLOBAL,varnam,LUnumCat ); IF( irv /= 0 )GOTO 9999

  END SELECT

END DO

!------ Check land use model and set appropriate parameter

IF( LUmodel(1:6) == 'NLCD40' )THEN
  Src%LUmodel = LU_NLCD40
  IF( LUnumCat > 40 )THEN
    error%Action = 'Too many land use categories for NLCD40 model'
    GOTO 9999
  END IF

ELSE IF( LUmodel(1:4) == 'USGS' )THEN
  IF( LUnumCat > 24 )THEN
    Src%LUmodel = LU_USGS33
    IF( LUnumCat > 33 )THEN
      error%Action = 'Too many land use categories for USGS model'
      GOTO 9999
    END IF
  ELSE
    Src%LUmodel = LU_USGS24
  END IF

ELSE IF( LUmodel(1:4) == 'MODIS' )THEN
  Src%LUmodel = LU_MODIS
  IF( LUnumCat > 33 )THEN
    error%Action = 'Too many land use categories for MODIS model'
    GOTO 9999
  END IF

ELSE IF( LUmodel(1:25) == 'MODIFIED_IGBP_MODIS_NOAH' )THEN
  Src%LUmodel = LU_MODIS_NOAH
  IF( LUnumCat > 33 )THEN
    error%Action = 'Too many land use categories for IGBP_MODIS_NOAH model'
    GOTO 9999
  END IF

ELSE
  error%Number  = WN_ERROR
  error%Message = 'Unsupported WRF land use model '//TRIM(LUmodel)
  error%Inform  = 'Uniform roughness will be assumed'
  error%Action  = 'Do you wish to continue?'
  irv = SWIMwarningMessage( 'User elected not to continue' )
  IF( irv /= SWIMsuccess )GOTO 9999
  Src%LUmodel = LU_NONE

END IF

Src%nX = nxi;  Src%dX = dx
Src%nY = nyi;  Src%dY = dy

Src%nZ  = nzi
Src%nXY = nxi*nyi

!----- Adjust reference longitude to best fit project domain

CALL AdjustLongitudeDom( Src%Lon0,NOT_SET_R )

!----- Vertical grid
!      N.B. Will be defined as sigma-z coordinates in SWIMreadWRF after 3d height is set

ALLOCATE( Src%Z(nzi),Src%Zw(nzi),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Message = 'Error allocating vertical grid arrays for WRF file'
  GOTO 9999
END IF

DO k = 1,nzi
  Src%Z(k)  = FLOAT(k)
  Src%Zw(k) = FLOAT(k)
END DO

!----- Check fields; count 2d & 3d fields for output

nv3 = 0
nv2 = 0

id_z  = -1
lZruf = .FALSE.
lLU   = .FALSE.
lQ2   = .FALSE.
lTH2  = .FALSE.
lQFX  = .FALSE.
lHFX  = .FALSE.

DO i = 0,nvars-1
  varnam = ''
  irv = nc_inq_var( fid,i,varnam,ivtype,ndims,dimids,natts )
  IF( irv /= 0 )CYCLE
  varnam = ADJUSTL(TRIM(StripNull(varnam))); CALL cupper( varnam )
  SELECT CASE( TRIM(varnam) )
    CASE( 'U','V','W','T','P','QVAPOR' )
      nv3 = nv3 + 1
    CASE( 'QCLOUD' )
      nv3 = nv3 + 1
    CASE( 'PH' )
      nv3 = nv3 + 1
      id_z = i
    CASE( 'HGT' )
      nv2 = nv2 + 1
    CASE( 'PBLH','HFX','UST' )
       IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )nv2 = nv2 + 1
    CASE( 'ZNT' )
      lZruf = .TRUE.
      nv2 = nv2 + 1
    CASE( 'LU_INDEX' )
      nv2 = nv2 + 1
      lLU = .TRUE.
    CASE( 'ALBEDO' )
      nv2 = nv2 + 1
    CASE( 'RAINC' )
      IF( Prj%BL%pr_type == -1. )THEN
        nv2 = nv2 + 1
      END IF
    CASE( 'RAINSH' )
      IF( Prj%BL%pr_type == -1. )THEN
        Src%lPrecipAux = .TRUE.
      END IF
    CASE( 'RAINNC' )
      IF( Prj%BL%pr_type == -1. )THEN
        Src%lPrecipAux = .TRUE.
      END IF
    CASE( 'LAI' )
      nv2 = nv2 + 1
  END SELECT

!------ Check for fields used in computing surface buoyancy flux

  IF( TRIM(varnam) == 'Q2' )THEN
    lQ2 = .TRUE.
  ELSE IF( TRIM(varnam) == 'TH2' )THEN
    lTH2 = .TRUE.
  ELSE IF( TRIM(varnam) == 'QFX' )THEN
    lQFX = .TRUE.
  ELSE IF( TRIM(varnam) == 'HFX' )THEN
    lHFX = .TRUE.
  END IF

END DO

IF( .NOT.lZruf .AND. lLU )nv2 = nv2 + 1 !Use landuse type if roughness unavailable

Src%lBuoy = lQ2 .AND. lTH2 .AND. lQFX .AND. lHFX  !Enable buoyancy flux calculation

!------ Check for vertical levels

IF( id_z == -1 )THEN
  error%Number  = IV_ERROR
  error%Message = 'Grid heights unavailable on WRF file'
  GOTO 9999
END IF

Src%type = IBSET(Src%type,GSB_ZAGL)    !3D, time-varying height AGL

!------ Define/initialize parameters for reading

Src%nVar2d = nv2
Src%nVar3d = nv3

!------- Allocate arrays for field names and indices

ALLOCATE( Src%Var2dName(nv2),Src%Var2dID(nv2),Src%Conv2d(nv2), &
          Src%Var3dName(nv3),Src%Var3dID(nv3),Src%Conv3d(nv3),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Message = 'Error allocating arrays for WRF field names and indices'
  GOTO 9999
END IF

Src%Conv2d = 1.0
Src%Conv3d = 1.0


nv3 = 0
nv2 = 0

istg3d = 0

string = AddNull('stagger')

!------ Setup names and id's for 2d & 3d fields

DO i = 0,nvars-1

  varnam = ''
  irv = nc_inq_var( fid,i,varnam,ivtype,ndims,dimids,natts )
  IF( irv /= 0 )CYCLE
  varnam = ADJUSTL(TRIM(StripNull(varnam))); CALL cupper( varnam )

  SELECT CASE( varnam )
    CASE( 'U' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_U
      irv = nc_get_att_text( fid,i,string,varnam )
      IF( INDEX(varnam,'X') > 0 )istg3d(1,1) = 1  !Location at x(i)+dx/2
      IF( INDEX(varnam,'Y') > 0 )istg3d(2,1) = 1

    CASE( 'V' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_V
      irv = nc_get_att_text( fid,i,string,varnam )
      IF( INDEX(varnam,'X') > 0 )istg3d(1,2) = 1
      IF( INDEX(varnam,'Y') > 0 )istg3d(2,2) = 1

    CASE( 'W' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_W
      irv = nc_get_att_text( fid,i,string,varnam )
      IF( INDEX(varnam,'Z') > 0 )istg3d(3,3) = 1

    CASE( 'T' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_T

    CASE( 'P' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_P

    CASE( 'PH' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_Z

    CASE( 'QVAPOR' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_H

    CASE( 'QCLOUD' )
      nv3 = nv3 + 1
      Src%Var3dName(nv3) = varnam
      Src%Var3dID(nv3)   = GVP_QCLD
      Src%Conv3d(nv3)    = 1.E+3

    CASE( 'HGT' )
      nv2 = nv2 + 1
      Src%Var2dName(nv2) = varnam
      Src%Var2dID(nv2)   = GVP_TERRAIN

    CASE( 'PBLH' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        nv2 = nv2 + 1
        Src%Var2dName(nv2) = varnam
        Src%Var2dID(nv2)   = GVP_ZI
      END IF

    CASE( 'ZNT' )
      nv2 = nv2 + 1
      Src%Var2dName(nv2) = varnam
      Src%Var2dID(nv2)   = GVP_ZRUFT

    CASE( 'LU_INDEX' )
      nv2 = nv2 + 1
      Src%Var2dName(nv2) = varnam
      Src%Var2dID(nv2)   = GVP_LANDUSE
      IF( .NOT.lZruf )THEN
        nv2 = nv2 + 1
        Src%Var2dName(nv2) = TRIM(varnam)  !//' (z0)'
        Src%Var2dID(nv2)   = GVP_ZRUFT
!        Src%Var2dID(nv2)   = GVP_ZRUF
      END IF

    CASE( 'HFX' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        nv2 = nv2 + 1
        Src%Var2dName(nv2) = varnam
        Src%Var2dID(nv2)   = GVP_HFLX
      END IF

    CASE( 'UST' )
      IF( Prj%BL%type == BLP_OPER .OR. Prj%BL%type == BLP_MET )THEN
        nv2 = nv2 + 1
        Src%Var2dName(nv2) = varnam
        Src%Var2dID(nv2)   = GVP_UST
      END IF

    CASE( 'ALBEDO' )
      nv2 = nv2 + 1
      Src%Var2dName(nv2) = varnam
      Src%Var2dID(nv2)   = GVP_ALBEDO

    CASE( 'RAINC' )
      IF( Prj%BL%pr_type == -1. )THEN
        nv2 = nv2 + 1
        Src%Var2dName(nv2) = varnam
        Src%Var2dID(nv2)   = GVP_ACCPR
      END IF
    CASE( 'LAI' )
      nv2 = nv2 + 1
      Src%Var2dName(nv2) = varnam
      Src%Var2dID(nv2)   = GVP_LAI


  END SELECT

END DO

DO i = 1,nv3
  irv = SWIMaddLogMessage( 'Recognized WRF 3d variable: '//TRIM(Src%Var3dName(i)) )
END DO

DO i = 1,nv2
  irv = SWIMaddLogMessage( 'Recognized WRF 2d variable: '//TRIM(Src%Var2dName(i)) )
END DO

!------ Setup staggered grid definitions

Src%nStg2D = 0
Src%nStg3D = 0

DO i = 1,3
  IF( SUM(istg3d(:,i)) > 0 )Src%nStg3D = Src%nStg3D + 1
END DO

IF( Src%nStg3D > 0 )THEN

  ALLOCATE( Src%iStg3D(0:3,Src%nStg3D),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Message = 'Error allocating arrays defining staggered grids'
    GOTO 9999
  END IF

  k = 0
  DO i = 1,3
    IF( SUM(istg3d(:,i)) > 0 )THEN
      k = k + 1
      Src%iStg3D(1:3,k) = istg3d(:,i)
      IF( i == 1 )THEN
        Src%iStg3D(0,k) = GVP_U
      ELSE IF( i == 2 )THEN
        Src%iStg3D(0,k) = GVP_V
      ELSE
        Src%iStg3D(0,k) = GVP_W
      END IF
    END IF
  END DO

END IF

!------ Initialize coordinate transformation parameters

RearthKM = 6370. !Value from WRF model

SELECT CASE( coordType  )

  CASE( I_LAMBERT )
    IF( Src%Lat1 > Src%Lat2 )THEN
      x0 = Src%Lat2; Src%Lat2 = Src%Lat1; Src%Lat1 = x0
    END IF
    irv = InitLambert( Src%Lat0,Src%Lat1,Src%Lat2,RearthKM,rn,f,m0,yy0 )
    lonLim = 90./ABS(rn)

  CASE( I_POLAR )
    irv = InitPolar( Src%Lat0,Src%Lat1,RearthKM,rn,f,m0,yy0 )
    lonLim = 90.

  CASE( I_MERCATOR )
    irv = InitMercator( Src%Lat1,RearthKM,f,m0 )
    lonLim = 180.

END SELECT

!----- Get SW lat/lon

xlat = get_real1( fid,AddNull('XLAT') )
xlon = get_real1( fid,AddNull('XLONG') )
IF( xlat == NOT_SET_R .OR. xlon == NOT_SET_R )THEN
  error%Number  = RD_ERROR
  error%Message = 'Error reading XLAT and XLONG on WRF file'
  GOTO 9999
END IF

IF( xlon-Src%Lon0 > lonLim )THEN     !Make sure longitude is within limits of central longitude
  xlon = xlon - 360.
ELSE IF( xlon-Src%Lon0 < -lonLim )THEN
  xlon = xlon + 360.
END IF

!------ Convert SW corner to native coordinates

SELECT CASE( coordType  )
  CASE( I_LAMBERT )
    irv = LL2Lambert( xlat,xlon,Src%Lat0,Src%Lon0,rn,f,yy0,x0,y0 )

  CASE( I_POLAR )
    irv = LL2Polar( xlat,xlon,Src%Lat0,Src%Lon0,rn,f,yy0,x0,y0 )

  CASE( I_MERCATOR )
    irv = LL2Mercator( xlat,xlon,Src%Lon0,f,x0,y0 )

END SELECT

Src%x0 = x0
Src%y0 = y0

InitWRFsource = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

IF( fid > 0 )THEN
  irv = nc_close( fid )
  IF( irv /= 0 )THEN
    error%Message = 'Error closing WRF file'
    error%Action = StringCToF( nc_strerror( irv ) )
  END IF
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetWRFterrain( fld )

!------ Read terrain from WRF netCDF file and set gradients
!       Also sets other fixed 2d fields roughness, albedo & landuse
!       N.B. also set 3d height so set_kbl can be called

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMutilArrayPtr
USE string_utils
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER irv, alloc_stat, fid, ios
INTEGER i, j, k, k0, j0, is, ip, nx, ny, nz, nxy
INTEGER ix, iy, iz, it, iSeason, jul
INTEGER id_var, ivtype, ndims, natts
INTEGER idPH, idPHB, luWRF, luSCIPUFF
LOGICAL lz0, lLU
REAL    xtem, ytem, lat, lon

INTEGER(C_INTPTR_T) k_len
INTEGER(C_INTPTR_T), DIMENSION(5) :: istart, icount

INTEGER, DIMENSION(NF_MAX_VAR_DIMS) :: dimids

CHARACTER(NF_MAX_NAME) varnam, string

REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: wrk3d, aux3d
REAL, DIMENSION(:,:,:),   ALLOCATABLE :: wrk2d
REAL, DIMENSION(:),       POINTER     :: pwrk, pwrk3d, pterr

TYPE( MapCoord ) :: coord

INTERFACE

  INTEGER FUNCTION Read3dVarGridded( src,nx,ny,nz,var,ks0,ishft,jshft,var_src )
    USE SWIM_fi
    TYPE( GridSrc ),              INTENT( IN ) :: src
    INTEGER,                      INTENT( IN ) :: nx, ny, nz
    REAL, DIMENSION(:),           POINTER      :: var
    INTEGER,                      INTENT( IN ) :: ks0
    INTEGER,            OPTIONAL, INTENT( IN ) :: ishft
    INTEGER,            OPTIONAL, INTENT( IN ) :: jshft
    REAL, DIMENSION(:), OPTIONAL, POINTER      :: var_src
  END FUNCTION Read3dVarGridded

  SUBROUTINE Copy2dVar( src,grid,wrk,var )
    USE SWIM_fi
    TYPE( GridSrc ), INTENT( IN ) :: src
    TYPE( MetGrid ), INTENT( IN ) :: grid
    REAL, DIMENSION(:), POINTER   :: wrk
    REAL, DIMENSION(:), POINTER   :: var
  END SUBROUTINE Copy2dVar

  SUBROUTINE AdjustCanopyPrf( fld,is )
    USE SWIM_fi
    TYPE( MetField ), TARGET, INTENT( INOUT ) :: fld
    INTEGER,                  INTENT( IN    ) :: is
  END SUBROUTINE AdjustCanopyPrf

END INTERFACE

INTEGER, EXTERNAL :: SWIMaddLogMessage, PostProgressMessage
INTEGER, EXTERNAL :: SetTerrainGrad
INTEGER, EXTERNAL :: SWIMwarningMessage
INTEGER, EXTERNAL :: JulianPrj, GetWRFSeason, SWIMcnvCoord

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull, StripNull

!------ Nullify local pointers

NULLIFY( pwrk,pwrk3d )

!------ Setup messaging

message%bString  = 'reading terrain on WRF file'

irv = PostProgressMessage( message )

SWIMsetWRFterrain = SWIMfailure

CALL ReportFileName( string,'Reading terrain from file ',fld%gridSource%Source(1) )
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize error structure

error%Number  = RD_ERROR
error%Routine = 'SWIMsetWRFterrain'
error%Message = 'Error reading WRF file'
CALL ReportFileName( error%Inform,'File=',fld%gridSource%Source(1) )

!------ Open WRF netCDF file

irv = nc_open( AddNull(fld%gridSource%source(1)),NF_NOWRITE,fid )
IF( irv /= 0 )THEN
  error%Number  = OP_ERROR
  error%Message = 'Error opening WRF file'
  error%Action = StringCToF( nc_strerror( irv ) )
  GOTO 9999
END IF

!------ Setup for 2d fields

istart = 0; icount = 0

irv = nc_inq_varid( fid,AddNull('HGT'),id_var )
irv = nc_inq_var( fid,id_var,varnam,ivtype,ndims,dimids,natts )
IF( irv /= 0 )GOTO 9999

nx = fld%gridSource%nX
ny = fld%gridSource%nY

DO j = 1,ndims

  irv = nc_inq_dim( fid,dimids(j),varnam,k_len );
  IF( irv /= 0 )THEN
    error%Message = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )
    CASE( 'Time' )
      icount(j) = 1;  it = j
    CASE( 'west_east' )
      icount(j) = k_len; ix = j
    CASE( 'south_north' )
      icount(j) = k_len; iy = j
  END SELECT

END DO

ALLOCATE( wrk2d(icount(ix),icount(iy),icount(it)),pwrk(nx*ny),pterr(nx*ny),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating arrays to read WRF terrain'
  GOTO 9999
END IF

!------ Read terrain

irv = nc_get_vara_float( fid,id_var,istart,icount,wrk2d )
IF( irv /= 0 )THEN
  error%Action = StringCToF( nc_strerror( irv ) )
  GOTO 9999
END IF

DO i = 1,nx
  DO j = 1,ny
    is = (j-1)*nx + i
    pterr(is) = wrk2d(i,j,1)
  END DO
END DO

CALL Copy2dVar( fld%gridSource,fld%grid,pterr,fld%grid%terrain%H )

!------ Read roughness directly or convert from landuse
!       N.B. Will be updated with time-varying fields if appropriate
!       N.B. Assume array order and extents are same as HGT

lz0 = .FALSE.
lLU = .FALSE.

IF( nc_inq_varid(fid,AddNull('ZNT'),id_var) == 0 )THEN
  lz0 = .TRUE.
ELSE IF( nc_inq_varid(fid,AddNull('LU_INDEX'),id_var) == 0 )THEN
  lLU = .TRUE.
END IF

IF( lz0 .OR. lLU )THEN

  irv = nc_get_vara_float( fid,id_var,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Action = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  IF( lLU )THEN
    jul = JulianPrj( 0. )
    xtem = 0.5*(fld%grid%Xmin+fld%grid%Xmax); ytem = 0.5*(fld%grid%Ymin+fld%grid%Ymax)
    coord%type = I_LATLON
    irv = SWIMcnvCoord( xtem,ytem,fld%grid%coord,lon,lat,coord )
    iSeason = GetWRFSeason( lat,jul )
    CALL WRFcat2zruf( fld%gridSource%LUmodel,iSeason,nx,ny,wrk2d )
  END IF

  DO i = 1,nx
    DO j = 1,ny
      is = (j-1)*nx + i
      pwrk(is) = wrk2d(i,j,1)
    END DO
  END DO

  CALL Copy2dVar( fld%gridSource,fld%grid,pwrk,fld%grid%landcover%roughness )

END IF

!------ Set albedo

IF( nc_inq_varid(fid,AddNull('ALBEDO'),id_var) == 0 )THEN

  irv = nc_get_vara_float( fid,id_var,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Action = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  DO i = 1,nx
    DO j = 1,ny
      is = (j-1)*nx + i
      pwrk(is) = wrk2d(i,j,1)
    END DO
  END DO

  CALL Copy2dVar( fld%gridSource,fld%grid,pwrk,fld%grid%landcover%albedo )

END IF

IF( nc_inq_varid(fid,AddNull('LU_INDEX'),id_var) == 0 )THEN

  irv = nc_get_vara_float( fid,id_var,istart,icount,wrk2d )
  IF( irv /= 0 )THEN
    error%Action = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  DO i = 1,nx
    DO j = 1,ny
      is = (j-1)*nx + i
      luWRF = NINT(wrk2d(i,j,1))
      CALL WRFcat2SCIPUFF( fld%gridSource%LUmodel,luWRF,luSCIPUFF )
      pwrk(is) = FLOAT(luSCIPUFF)
    END DO
  END DO

  ALLOCATE( pwrk3d(SIZE(fld%grid%landcover%LandUse)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Message = 'Error allocating land use array'
    GOTO 9999
  END IF

  CALL Copy2dVar( fld%gridSource,fld%grid,pwrk,pwrk3d )

  DO i = 1,SIZE(fld%grid%landcover%LandUse)
    fld%grid%landcover%LandUse(i) = NINT(pwrk3d(i))
  END DO

  DEALLOCATE( pwrk3d,STAT=alloc_stat )

END IF

!------ Setup 3d height for use in set_kbl

irv = nc_inq_varid( fid,AddNull('PH'),idPH )
IF( irv /= 0 )GOTO 9999

irv = nc_inq_varid( fid,AddNull('PHB'),idPHB )
IF( irv /= 0 )GOTO 9999

irv = nc_inq_var( fid,idPH,varnam,ivtype,ndims,dimids,nAtts )
IF( irv /= 0 )GOTO 9999

istart = 0; icount = 0

DO j = 1,ndims

  irv = nc_inq_dim( fid,dimids(j),varnam,k_len );
  IF( irv /= 0 )THEN
    error%Message = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF

  SELECT CASE( ADJUSTL(TRIM(StripNull(varnam))) )
    CASE( 'Time' )
      icount(j) = 1;  it = j
    CASE( 'west_east' )
      icount(j) = k_len; ix = j
    CASE( 'south_north' )
      icount(j) = k_len; iy = j
    CASE( 'bottom_top_stag' )
      icount(j) = k_len-1; iz = j
      istart(j) = istart(j) + 1      !PH is staggered vertically (and level 0 is at the ground)
      nz = k_len-1                   !and therefore has an extra level
  END SELECT

END DO

ALLOCATE( wrk3d(icount(ix),icount(iy),icount(iz),icount(it)), &
          aux3d(icount(ix),icount(iy),icount(iz),icount(it)), &
          pwrk3d(nx*ny*nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating arrays to read WRF grid cell heights'
  GOTO 9999
END IF

!------ Read geopotential fields (base and perturbation)

irv = nc_get_vara_float( fid,idPH,istart,icount,wrk3d )
IF( irv /= 0 )THEN
  error%Action = StringCToF( nc_strerror( irv ) )
  GOTO 9999
END IF

irv = nc_get_vara_float( fid,idPHB,istart,icount,aux3d )
IF( irv /= 0 )THEN
  error%Action = StringCToF( nc_strerror( irv ) )
  GOTO 9999
END IF

!------ Add base-state and perturbation geopotential; divide by "g"; subtract terrain height

nxy = nx*ny

DO k = 1,nz
  k0 = (k-1)*nxy
  DO j = 1,ny
    j0 = (j-1)*nx
    DO i = 1,nx
      is = j0 + i
      ip = k0 + is
      pwrk3d(ip) = (wrk3d(i,j,k,1) + aux3d(i,j,k,1))/G0
      pwrk3d(ip) = pwrk3d(ip) - pterr(is)
    END DO
  END DO
END DO

!------ Average down to cell center

DO k = nz,2,-1
  k0 = (k-1)*nxy
  pwrk3d(k0+1:k0+nxy) = 0.5*(pwrk3d(k0+1:k0+nxy)+pwrk3d(k0-nxy+1:k0))
END DO
pwrk3d(1:nxy) = 0.5*pwrk3d(1:nxy)

!------ Copy into final array, accounting for offsets and skipping
!       N.B. sigma%Z is temporary for use in set_kbl

k = fld%grid%nXY*fld%grid%nZ
ALLOCATE( fld%grid%sigma%Z(k),STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Message = 'Error allocating array for WRF 3d heights'
  GOTO 9999
END IF

irv = Read3dVarGridded( fld%gridSource,fld%grid%nX,fld%grid%nY,fld%grid%nZ, &
                                              fld%grid%sigma%Z,0,0,0,pwrk3d )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Clear error since we've read successfully

CALL SWIMclearError()

!------ Get minimimum terrain elevation

CALL SetHmin( fld%grid )

WRITE(string,'(A,ES12.4)',IOSTAT=ios) 'Minimum terrain elevation (WRF) =',fld%grid%hmin
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

Prj%Hmin = MIN(Prj%Hmin,fld%grid%Hmin)

!------ Set terrain fields and gradients

irv = SetTerrainGrad( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set grid level for setting surface layer profile (above canopy)

CALL set_kbl( fld%grid )
IF( error%Number /= NO_ERROR )GOTO 9999

IF( Prj%BL%hc > 0. .AND. .NOT.BTEST(fld%grid%type,GTB_HCNP) .AND. &
                         .NOT.BTEST(fld%grid%type,GTB_ZRUF) )THEN
  IF( .NOT.BTEST(fld%status,FSB_CANOPYWARN) )THEN
    DO i = 1,fld%grid%nXY
      IF( fld%grid%landcover%kbl(i) > 1 )THEN
        error%Number  = WN_ERROR
        error%Routine = 'SWIMsetWRFterrain'
        error%Message = 'Influence height of user-specified canopy is above first WRF level'
        error%Inform  = 'Wind profile will be modified.'
        error%Action  = 'Do you wish to continue?'
        irv = SWIMwarningMessage( 'User elected not to continue' )
        IF( irv /= SWIMsuccess )GOTO 9999
        EXIT
      END IF
    END DO
    fld%status = IBSET(fld%status,FSB_CANOPYWARN)
  END IF

  DO i = 1,fld%grid%nXY
    IF( fld%grid%landcover%kbl(i) > 1 )THEN
      CALL AdjustCanopyPrf( fld,i )
    END IF
  END DO

END IF

!------ Yet another SWIMming success

SWIMsetWRFterrain = SWIMresult

9999 CONTINUE

IF( ALLOCATED(wrk2d) )DEALLOCATE( wrk2d,STAT=alloc_stat )
IF( ALLOCATED(wrk3d) )DEALLOCATE( wrk3d,STAT=alloc_stat )
IF( ALLOCATED(aux3d) )DEALLOCATE( aux3d,STAT=alloc_stat )

IF( ASSOCIATED(pwrk)   )DEALLOCATE( pwrk,   STAT=alloc_stat )
IF( ASSOCIATED(pterr)  )DEALLOCATE( pterr,  STAT=alloc_stat )
IF( ASSOCIATED(pwrk3d) )DEALLOCATE( pwrk3d, STAT=alloc_stat )

IF( ASSOCIATED(fld%grid%sigma%z) )DEALLOCATE( fld%grid%sigma%z,STAT=alloc_stat )

IF( fid > 0 )THEN
  irv = nc_close( fid )
  IF( irv /= 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMsetWRFterrain'
    error%Message = 'Error closing WRF file'
    error%Action = StringCToF( nc_strerror( irv ) )
  END IF
END IF

RETURN
END

!==============================================================================

REAL FUNCTION GetWRFTime( FileName,gid,tStart,it ) RESULT( tMet )

!------ Read time from WRF file and set project time

USE SWIM_fi
USE SWIMparam_fd
USE string_utils

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: FileName      !WRF netCDF file
INTEGER,      INTENT( IN ) :: gid           !File id (>0 if currently open)
REAL,         INTENT( IN ) :: tStart        !WRF start time relative to in project start
INTEGER,      INTENT( IN ) :: it            !File timebreak (1=first)

INTEGER irv, fid, n_times, i

INTERFACE
  REAL FUNCTION get_real1( fid,var_name,is ) RESULT( x )
    INTEGER,           INTENT( IN ) :: fid
    CHARACTER*(*),     INTENT( IN ) :: var_name
    INTEGER, OPTIONAL, INTENT( IN ) :: is
  END FUNCTION get_real1
END INTERFACE

INTEGER, EXTERNAL :: GetWRFnTimes

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull

!------ Initialize error

error%Number  = IV_ERROR
error%Routine = 'GetWRFTime'
error%Message = 'Error reading time from WRF head file'
CALL ReportFileName( error%Inform,'File=',FileName )

tMet = NOT_SET_R

!------ Open file

IF( gid > 0 )THEN
  fid = gid
ELSE
  irv = nc_open( AddNull(FileName),NF_NOWRITE,fid )
  IF( irv /= 0 )THEN
    error%Number  = OP_ERROR
    error%Message = 'Error opening WRF file'
    error%Action = StringCToF( nc_strerror( irv ) )
    GOTO 9999
  END IF
END IF

n_times = GetWRFnTimes( fid ); IF( n_times < 1 )GOTO 9999

i = MAX(it,1)
i = MIN(i,n_times)

tMet = get_real1( fid,'XTIME',i-1 )
IF( tMet == NOT_SET_R )GOTO 9999
tMet = tMet*60. !Convert to seconds

IF( Reverse )THEN
  tMet = tStart - tMet
ELSE
  tMet = tStart + tMet
END IF

CALL bin_time( tMet,1. )

CALL SWIMclearError()

9999 CONTINUE

IF( gid <= 0 )THEN
  irv = nc_close( fid )
  IF( irv /= 0 )THEN
    error%Message = 'Error closing WRF file'
    error%Action = StringCToF( nc_strerror( irv ) )
  END IF
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION GetWRFnTimes( fid ) RESULT( n_times )

!------ Get number of times on a WRF file

USE SWIM_fi
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

INTEGER, INTENT( IN ) :: fid

INTEGER irv, iv, ndims, natts, I

INTEGER(C_INTPTR_T) k_len

INTEGER, DIMENSION(NF_MAX_VAR_DIMS) :: dimids

CHARACTER(NF_MAX_NAME) varnam

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull, StripNull

n_times = -1

irv = nc_inq_varid( fid,AddNull('Times'),iv )
IF( irv /= 0 )GOTO 9999

irv = nc_inq_var( fid,iv,varnam,iv,ndims,dimids,natts )
IF( irv /= 0 )GOTO 9999

DO i = 1,ndims
  irv = nc_inq_dim( fid,dimids(i),varnam,k_len ); n_times = k_len
  IF( TRIM(StripNull(varnam)) == 'Time' )EXIT
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

REAL FUNCTION get_real1( fid,var_name,is ) RESULT( x )

!------ Get first value of a real array on a WRF file

USE SWIM_fi
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: fid
CHARACTER*(*),     INTENT( IN ) :: var_name
INTEGER, OPTIONAL, INTENT( IN ) :: is

INTEGER irv, id_var, ivtype, ndims, natts

INTEGER, DIMENSION(NF_MAX_VAR_DIMS) :: dimids

INTEGER(C_INTPTR_T), DIMENSION(:), ALLOCATABLE :: istart, icount

REAL,    DIMENSION(1) :: x4
REAL(8), DIMENSION(1) :: x8

CHARACTER(NF_MAX_NAME) varnam

CHARACTER(NF_MAX_NAME), EXTERNAL :: AddNull

irv = nc_inq_varid( fid,AddNull(var_name),id_var )
IF( irv /= 0 )GOTO 9999
irv = nc_inq_var( fid,id_var,varnam,ivtype,ndims,dimids,natts )
IF( irv /= 0 )GOTO 9999

IF( .NOT.(ivtype == NF_REAL .OR. ivtype == NF_DOUBLE) )THEN
  irv = -1
  GOTO 9999
END IF

ALLOCATE( istart(ndims),icount(ndims),STAT=irv )
IF( irv /= 0 )GOTO 9999

istart = 0; icount = 1
IF( PRESENT(is) )istart(1) = is

IF( ivtype == NF_REAL )THEN
  irv = nc_get_vara_float( fid,id_var,istart,icount,x4 )
  IF( irv /= 0 )GOTO 9999
ELSE IF( ivtype == NF_DOUBLE )THEN
  irv = nc_get_vara_double( fid,id_var,istart,icount,x8 )
  IF( irv /= 0 )GOTO 9999
  x4 = x8
END IF

x = x4(1)

9999 CONTINUE

IF( irv /= 0 )x = NOT_SET_R

IF( ALLOCATED(istart) )DEALLOCATE( istart,STAT=irv )
IF( ALLOCATED(icount) )DEALLOCATE( icount,STAT=irv )

RETURN
END

!==============================================================================

SUBROUTINE WRFcat2zruf( LUmodel,iSeason,nx,ny,wrk2d )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: LUmodel
INTEGER,                INTENT( IN    ) :: iSeason
INTEGER,                INTENT( IN    ) :: nx, ny
REAL, DIMENSION(nx,ny), INTENT( INOUT ) :: wrk2d

INTEGER i, j, cat

SELECT CASE( LUmodel )
  CASE( LU_NLCD40 )
    DO j = 1,ny
      DO i = 1,nx
        cat = MAX(1,MIN(NINT(wrk2d(i,j)),40))
        wrk2d(i,j) = ROUGHNESS_NLCD40(cat,iSeason) * 0.01
      END DO
    END DO

  CASE( LU_USGS24 )
    DO j = 1,ny
      DO i = 1,nx
        cat = MAX(1,MIN(NINT(wrk2d(i,j)),24))
        wrk2d(i,j) = ROUGHNESS_USGS24(cat)
      END DO
    END DO

  CASE( LU_USGS33 )
    DO j = 1,ny
      DO i = 1,nx
        cat = MAX(1,MIN(NINT(wrk2d(i,j)),33))
        wrk2d(i,j) = ROUGHNESS_USGS33(cat,iSeason) * 0.01
      END DO
    END DO

  CASE( LU_MODIS )
    DO j = 1,ny
      DO i = 1,nx
        cat = MAX(1,MIN(NINT(wrk2d(i,j)),33))
        wrk2d(i,j) = ROUGHNESS_MODIS(cat,iSeason) * 0.01
      END DO
    END DO

  CASE( LU_MODIS_NOAH )
    DO j = 1,ny
      DO i = 1,nx
        cat = MAX(1,MIN(NINT(wrk2d(i,j)),33))
        wrk2d(i,j) = ROUGHNESS_IGBP_MODIS(cat,iSeason) * 0.01
      END DO
    END DO

  CASE DEFAULT
    DO j = 1,ny
      DO i = 1,nx
        wrk2d(i,j) = 0.1
      END DO
    END DO

END SELECT

RETURN
END

!==============================================================================

SUBROUTINE WRFcat2SCIPUFF( LUmodel,LUin,LUout )

USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: LUmodel  !WRF land use model
INTEGER, INTENT( IN  ) :: LUin     !WRF land use category
INTEGER, INTENT( OUT ) :: LUout    !Corresponding SCIPUFF category

SELECT CASE( LUmodel )
  CASE( LU_NLCD40 )
    LUout = cat_NCLD40( LUin )

  CASE( LU_USGS24 )
    LUout = cat_USGS24( LUin )

  CASE( LU_USGS33 )
    LUout = cat_USGS33( LUin )

  CASE( LU_MODIS )
    LUout = cat_MODIS( LUin )

  CASE( LU_MODIS_NOAH )
    LUout = cat_IGBP_MODIS( LUin )

  CASE DEFAULT
    LUout = cat_USGS24( LUin )

END SELECT

RETURN
END

!===============================================================================

INTEGER FUNCTION GetWRFSeason( lat,jul )

!DEC$ ATTRIBUTES DLLEXPORT :: GetWRFSeason

!------ Set summer or winter based on latitude (kind of) and julian day
!       1=summer, 2=winter
!       N.B. Used only for converting WRF categories to roughness

IMPLICIT NONE

REAL,    INTENT( IN ) :: lat
INTEGER, INTENT( IN ) :: jul

INTEGER is

IF( jul <= 0 .OR. jul > 366 .OR. &
    lat < -90. .OR. lat > 90. )THEN
   GetWRFSeason = 1
   RETURN
END IF

IF( ABS(lat) > 70. )THEN
  is = 2                          !Always winter

ELSE IF( ABS(lat) < 20. )THEN
  is = 1                          !Always summer

ELSE IF( jul >= 105 .AND. jul <= 287 )THEN
  IF( lat > 0. )is = 1            !Summer in Northern Hemisphere
  IF( lat < 0. )is = 2            !Winter in Southern     "

ELSE
  IF( lat > 0. )is = 2            !Winter in Northern Hemisphere
  IF( lat < 0. )is = 1            !Summer in Southern     "

END IF

GetWRFSeason = is

RETURN
END


