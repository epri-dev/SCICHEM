MODULE TerHead_fd

  USE GridCoord_fd

  TYPE TerHeaderStr

    INTEGER :: nskip
    LOGICAL :: lcat
    LOGICAL :: lzruf
    LOGICAL :: lalbed
    LOGICAL :: lbowen
    LOGICAL :: lhcanp
    LOGICAL :: lalpha
    REAL    :: hscale
    INTEGER :: nX, nY
    INTEGER :: Imax,  Jmax
    INTEGER :: Is,    Js
    INTEGER :: Iskip, Jskip
    INTEGER :: nField
    TYPE( GridCoord ) :: grid
    CHARACTER(8), DIMENSION(:), POINTER :: FieldName

  END TYPE TerHeaderStr

END MODULE TerHead_fd

!==============================================================================

INTEGER FUNCTION SWIMreadTerrain( grid )

!------ Read terrain/landuse file

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE TerHead_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( INOUT ) :: grid

TYPE( TerHeaderStr ) :: TerHead

INTEGER irv, ios, lun, nrec
LOGICAL lerr, luse, lcat

CHARACTER(8) ter_map
CHARACTER(8) prj_map

INTEGER, EXTERNAL :: SWIMcheckTerrainHeader, ReadTerrainHeader
INTEGER, EXTERNAL :: CheckTerrainCoord, TransGridCoord, ReadTerrainFields
INTEGER, EXTERNAL :: SetTerrainDomain
INTEGER, EXTERNAL :: PostProgressMessage

SWIMreadTerrain = SWIMfailure

message%bString = 'Reading terrain file'
irv = PostProgressMessage( message )

lun = SWIMunit

CALL ReportFileName( error%Action,'File=',Prj%MC%TerFile )

!------ Clear header structure

CALL ClearTerrainHeader( TerHead )

!------ Check header for map compatibility; determine if skipping is required

irv = SWIMcheckTerrainHeader( Prj%coord,Prj%MC%TerFile,lerr,ter_map,nrec,luse,lcat )
IF( irv /= SWIMsuccess )GOTO 9999

IF( lerr )THEN
  IF( Prj%coord == I_LATLON )THEN
    prj_map = 'LATLON'
  ELSE IF( Prj%coord == I_CARTESIAN )THEN
    prj_map = 'CARTESIAN'
  ELSE IF( Prj%coord == I_UTM )THEN
    prj_map = 'UTM'
  END IF
  error%Number  = IV_ERROR
  error%Routine = 'SWIMreadTerrain'
  error%Message = 'Incompatible project and terrain map types'
  error%Inform  = 'Project is '//TRIM(prj_map)//' terrain is '//TRIM(ter_map)
  GOTO 9999
END IF

!------ Open terrain file

OPEN( lun,FILE=Prj%MC%TerFile,STATUS='OLD',ACTION='READ',IOSTAT=ios )
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMreadTerrain'
  error%Message = 'Error opening terrain file'
  GOTO 9999
END IF

!------ Read header records

TerHead%nskip = nrec

irv = ReadTerrainHeader( lun,TerHead )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Check terrain coordinates

irv = CheckTerrainCoord( TerHead )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Transform origin and grid spacing for map transformation

irv = TransGridCoord( TerHead%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set sub-domain limits

irv = SetTerrainDomain( TerHead,grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Allocate and fill terrain and landuse arrays

irv = ReadTerrainFields( lun,TerHead,grid )
IF( irv /= SWIMsuccess )GOTO 9999

error%Action = '' !Clear since no error occurred

SWIMreadTerrain = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(TerHead%FieldName) )DEALLOCATE( TerHead%FieldName,STAT=ios )

CLOSE( UNIT=lun,IOSTAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE ClearTerrainHeader( TerHead )

!------ Clear header structure

USE TerHead_fd
USE default_fd

IMPLICIT NONE

TYPE( TerHeaderStr ), INTENT( INOUT ) :: TerHead

TerHead%nskip  = 0
TerHead%nX     = 0
TerHead%nY     = 0
TerHead%Imax   = 0
TerHead%Jmax   = 0
TerHead%Is     = 0
TerHead%Js     = 0
TerHead%Iskip  = 0
TerHead%Jskip  = 0
TerHead%Imax   = 0
TerHead%nfield = 0
TerHead%Imax   = 0
TerHead%hscale = 0.

TerHead%lcat   = .FALSE.
TerHead%lzruf  = .FALSE.
TerHead%lalbed = .FALSE.
TerHead%lbowen = .FALSE.
TerHead%lhcanp = .FALSE.
TerHead%lalpha = .FALSE.

TerHead%grid%UTMzone = NOT_SET_I
TerHead%grid%coord   = NOT_SET_I
TerHead%grid%Lat0    = NOT_SET_R
TerHead%grid%Lon0    = NOT_SET_R
TerHead%grid%X0      = NOT_SET_R
TerHead%grid%Y0      = NOT_SET_R
TerHead%grid%dX      = NOT_SET_R
TerHead%grid%dY      = NOT_SET_R

NULLIFY( TerHead%FieldName )

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadTerrainHeader( lun,TerHead )

!------ Read first record (land use keyword or units)

USE SWIM_fi
USE SWIMparam_fd
USE TerHead_fd

IMPLICIT NONE

INTEGER,              INTENT( IN    ) :: lun
TYPE( TerHeaderStr ), INTENT( INOUT ) :: TerHead

INTEGER, PARAMETER :: MAXN = 7  !Max no. of variables in header record

CHARACTER(8)  ter_map
CHARACTER(72) c_origin
CHARACTER(80) line
CHARACTER(64) kwrd
CHARACTER(32) c_arg(MAXN)

INTEGER nch, n_arg, i, ios
LOGICAL lerr

ReadTerrainHeader = SWIMfailure

!------ Read first record

line = ' '
CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
IF( lerr )THEN
  error%Number  = RD_ERROR
  error%Routine = 'ReadTerrainHeader'
  error%Message = 'Error reading 1st record'
  GOTO 9999
END IF

IF( TRIM(kwrd) == 'LAND_USE' )THEN !Land cover available

  IF( BTEST(Prj%MC%type,MCB_LC) )THEN         !Use land cover

    DO i = 1, n_arg
      CALL cupper( c_arg(i) )
      IF( TRIM(c_arg(i)) == 'CATEGORY' )TerHead%lcat   = .TRUE.
      IF( TRIM(c_arg(i)) == 'ZRUF'     )TerHead%lzruf  = .TRUE.
      IF( TRIM(c_arg(i)) == 'ALBED'    )TerHead%lalbed = .TRUE.
      IF( TRIM(c_arg(i)) == 'BOWEN'    )TerHead%lbowen = .TRUE.
      IF( TRIM(c_arg(i)) == 'HCANP'    )TerHead%lhcanp = .TRUE.
      IF( TRIM(c_arg(i)) == 'ALPHC'    )TerHead%lalpha = .TRUE.
    END DO

    IF( TerHead%lhcanp .AND. .NOT.TerHead%lalpha )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainHeader'
      error%Message = 'Canopy height on file but no flow index (alpha)'
      GOTO 9999
    END IF

    IF( TerHead%lcat .AND. (TerHead%lzruf .OR. TerHead%lalbed .OR. TerHead%lbowen .OR. &
                                               TerHead%lhcanp .OR. TerHead%lalpha) )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainHeader'
      error%Message = 'Land use category cannot be given with other parameters'
      GOTO 9999
    END IF

  ELSE

    n_arg = 0 !Ignore land cover variables

  END IF

ELSE

  BACKSPACE( UNIT=lun,IOSTAT=ios )
  n_arg = 0

END IF

IF( n_arg > 0 )THEN
  TerHead%nField = n_arg
  ALLOCATE( TerHead%FieldName(n_arg),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'ReadTerrainHeader'
    error%Message = 'Error allocating array for field names'
    GOTO 9999
  END IF
  DO i = 1,n_arg
    TerHead%FieldName(i) = TRIM(c_arg(i))
  END DO
END IF

!------ Read first record (units)

IF( TerHead%nskip > 0 )THEN
  DO i = 1,TerHead%nskip + 2
    READ(lun,*,IOSTAT=ios)
  END DO
END IF

READ(lun,'(A8,A72)',IOSTAT=ios) ter_map, c_origin
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'ReadTerrainHeader'
  error%Message = 'Error reading terrain file (1st record)'
  GOTO 9999
END IF

CALL cupper( ter_map )
CALL cupper( c_origin )

TerHead%hscale = 1.

IF( LEN_TRIM(c_origin) <= 1 )THEN  !Origin not set

  TerHead%grid%Lon0 = NOT_SET_R
  TerHead%grid%Lat0 = NOT_SET_R

ELSE

  IF( TRIM(ter_map) == 'SCALED' )THEN

    READ(c_origin,*,IOSTAT=ios) TerHead%hscale
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainHeader'
      error%Message = 'Error reading scale (1st record)'
      GOTO 9999
    END IF
    ter_map ='METERS'

  ELSE IF( TRIM(ter_map) == 'UTM' )THEN

    READ(c_origin,*,IOSTAT=ios) TerHead%grid%UTMzone
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainHeader'
      error%Message = 'Error UTM zone on terrain file (1st record)'
      GOTO 9999
    END IF

  ELSE

    READ(c_origin,*,IOSTAT=ios) TerHead%grid%Lon0,TerHead%grid%Lat0
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainHeader'
      error%Message = 'Error lat/lon origin on terrain file (1st record)'
      GOTO 9999
    END IF

  END IF

END IF

!------ Read second record (grid info)

READ(lun,*,IOSTAT=ios) TerHead%grid%X0,TerHead%grid%Y0,TerHead%grid%dX,TerHead%grid%dY, &
                       TerHead%Imax,TerHead%Jmax
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'ReadTerrainHeader'
  error%Message = 'Error reading terrain file (2nd record)'
  GOTO 9999
END IF

!------ Check grid parameters

IF( TerHead%grid%dX <= 0. .OR. TerHead%grid%dY <= 0. )THEN
  error%Number  = RD_ERROR
  error%Routine = 'ReadTerrainHeader'
  error%Message = 'Grid spacing must be > 0'
  GOTO 9999
END IF

IF( TerHead%Imax <= 1 .OR. TerHead%Jmax <= 1 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'ReadTerrainHeader'
  error%Message = 'Error reading terrain file (2nd record)'
  error%Inform  = 'No. of grid points must be > 1'
  GOTO 9999
END IF

!------ Set map parameters

SELECT CASE( TRIM(ter_map) )

  CASE( 'LATLON' )
    CALL AdjustLongitudeDom( TerHead%grid%X0,FLOAT(TerHead%Imax-1)*TerHead%grid%dX )
    TerHead%grid%coord = I_LATLON
    TerHead%grid%Lon0  = TerHead%grid%X0
    TerHead%grid%Lat0  = TerHead%grid%Y0

  CASE( 'M','METERS' )
    TerHead%grid%coord = I_CARTESIAN
    TerHead%grid%dX    = TerHead%grid%dX * 1.E-3
    TerHead%grid%dY    = TerHead%grid%dY * 1.E-3

  CASE( 'KM' )
    TerHead%grid%coord = I_CARTESIAN

  CASE( 'UTM' )
    TerHead%grid%coord   = I_UTM

  CASE DEFAULT
    error%Number  = IV_ERROR
    error%Routine = 'ReadTerrainHeader'
    error%Message = 'Invalid map type on terrain file (1st record)'
    error%Inform  = 'Valid types are LATLON, M, METERS, KM or UTM'
    GOTO 9999
END SELECT

TerHead%grid%nX = TerHead%Imax
TerHead%grid%nY = TerHead%Jmax

ReadTerrainHeader = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION CheckTerrainCoord( TerHead )

!------ Check coordinate system compatibility

USE SWIM_fi
USE SWIMparam_fd
USE TerHead_fd
USE datums

IMPLICIT NONE

TYPE( TerHeaderStr ), INTENT( INOUT ) :: TerHead

INTEGER irv
REAL    lat, lon

LOGICAL, EXTERNAL :: CheckUTMref

CheckTerrainCoord = SWIMfailure

IF( Prj%coord == I_UTM )THEN

  IF( Prj%UTMzone /= DEF_VAL_I )THEN

    IF( TerHead%grid%coord == I_UTM )THEN

      IF( Prj%UTMzone /= TerHead%grid%UTMzone )THEN
        error%Number  = IV_ERROR
        error%Routine = 'CheckTerrainCoord'
        error%Message = 'Project UTM zone different from terrain zone'
        WRITE(error%Inform, &
              FMT="('Project zone = ',I2,2X,'Terrain zone = ',I2)") &
              Prj%UTMzone, TerHead%grid%UTMzone
        error%Action  = 'Update terrain file to project domain or modify project domain to terrain file domain'
        GOTO 9999
      END IF

    ELSE IF( TerHead%grid%coord == I_CARTESIAN )THEN

      IF( TerHead%grid%Lon0 /= NOT_SET_R .AND. TerHead%grid%Lat0 /= NOT_SET_R )THEN
        irv = UTM2LL( Prj%UTMzone,TerHead%grid%X0,TerHead%grid%Y0,lat,lon )
        IF( irv /= 0 )THEN
          CALL SWIMsetUTMerror( 'UTM2LL',irv )
          GOTO 9999
        END IF
        IF( .NOT.CheckUTMref( lat,lon,TerHead%grid%Lat0,TerHead%grid%Lon0) )THEN
          error%Number  = IV_ERROR
          error%Routine = 'CheckTerrainCoord'
          error%Message = 'Grid origin mismatch (wrong zone or non-UTM met input)'
          WRITE(error%Inform,'(A,2F8.2)') 'UTM project lat/lon=',lat,lon
          WRITE(error%Action,'(A,2F8.2)') 'Met/terrain lat/lon=',TerHead%grid%Lat0,TerHead%grid%Lon0
          GOTO 9999
        END IF
      END IF

    END IF

  ELSE

    IF( TerHead%grid%coord == I_UTM )Prj%UTMzone = TerHead%grid%UTMzone

  END IF

ELSE IF( Prj%Lon0 == NOT_SET_R .OR. Prj%Lat0 == NOT_SET_R )THEN

  IF( TerHead%grid%Lon0 /= NOT_SET_R .AND. TerHead%grid%Lat0 /= NOT_SET_R )THEN
    Prj%Lon0 = TerHead%grid%Lon0
    Prj%Lat0 = TerHead%grid%Lat0
    IF( TerHead%grid%coord == I_CARTESIAN )THEN
      Prj%Xref = TerHead%grid%X0
      Prj%Yref = TerHead%grid%Y0
    END IF
  END IF

END IF

CheckTerrainCoord = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION TransGridCoord( grid )

!------ Transform from terrain to project coordinate system and
!       check project reference location

USE SWIM_fi
USE SWIMparam_fd
USE GridCoord_fd
USE datums

IMPLICIT NONE

TYPE( GridCoord ), INTENT( INOUT ) :: grid

TYPE( MapCoord ) :: coordPrj

INTEGER irv
REAL    lat, lon, xmap, ymap, x0, y0

LOGICAL, EXTERNAL :: HasPrjReference
INTEGER, EXTERNAL :: SetRefGrid

TransGridCoord = SWIMfailure

!------ First check/set project reference location

irv = SetRefGrid( grid )
IF( irv /= SWIMsuccess )GOTO 9999

IF( Prj%coord == grid%coord )THEN
  TransGridCoord = SWIMresult
  RETURN
END IF

PrjCoord%type = 0

!------ Convert coordinates

SELECT CASE( Prj%coord )

  CASE( I_LATLON )
    coordPrj%type = I_LATLON

    IF( grid%coord == I_UTM )THEN

      irv = UTM2LL( grid%UTMzone,grid%X0,grid%Y0,lat,lon )
      IF( irv /= 0 )THEN
        CALL SWIMsetUTMerror( 'UTM2LL',irv )
        GOTO 9999
      END IF
      grid%X0 = lon
      grid%Y0 = lat
      CALL SWIMmapfac( coordPrj,lon,lat,xmap,ymap )
      grid%dX = grid%dX * 1.E3 * xmap
      grid%dY = grid%dY * 1.E3 * ymap

    ELSE

      IF( HasPrjReference() )THEN
        CALL SWIMmapfac( coordPrj,Prj%Lon0,Prj%Lat0,xmap,ymap )
        xmap = 1.E3 * xmap
        ymap = 1.E3 * ymap
        grid%X0 = Prj%Lon0 + (grid%X0 - Prj%Xref)*xmap
        grid%Y0 = Prj%Lat0 + (grid%Y0 - Prj%Yref)*ymap
        grid%dX = grid%dX * xmap
        grid%dY = grid%dY * ymap
      ELSE
        error%Number  = IV_ERROR
        error%Routine = 'TransGridCoord'
        error%Message = 'Reference location must be set'
        error%Inform  = 'Lat/lon project, Cartesian terrain/met file'
        GOTO 9999
      END IF

    END IF

  CASE( I_UTM )

    IF( grid%coord == I_LATLON )THEN

      IF( Prj%UTMzone == NOT_SET_I )THEN
        error%Number  = IV_ERROR
        error%Routine = 'TransGridCoord'
        error%Message = 'UTM zone must be set'
        error%Inform  = 'UTM project, Lat/lon terrain file'
        GOTO 9999
      END IF

      irv = LL2UTM( grid%Lat0,grid%Lon0,Prj%UTMzone,x0,y0 )
      IF( irv /= 0 )THEN
        CALL SWIMsetUTMerror( 'LL2UTM',irv )
        GOTO 9999
      END IF
      grid%X0 = x0
      grid%Y0 = y0
      coordPrj%type = I_LATLON
      CALL SWIMmapfac( coordPrj,grid%Lon0,grid%Lat0,xmap,ymap )
      xmap = 1.E-3 / xmap
      ymap = 1.E-3 / ymap
      grid%dX = grid%dX * xmap
      grid%dY = grid%dY * ymap

    END IF

  CASE( I_CARTESIAN,I_METERS )

    IF( grid%coord == I_LATLON )THEN

      IF( HasPrjReference() )THEN
        coordPrj%type = I_LATLON
        CALL SWIMmapfac( coordPrj,Prj%Lon0,Prj%Lat0,xmap,ymap )
        IF( Prj%coord == I_CARTESIAN )THEN
          xmap = 1.E-3 / xmap
          ymap = 1.E-3 / ymap
        ELSE
          xmap = 1. / xmap
          ymap = 1. / ymap
        END IF
        grid%coord = Prj%coord !Set terrain to cartesian coordinates
        grid%dX = grid%dX * xmap
        grid%dY = grid%dY * ymap
        grid%X0 = Prj%Xref + (grid%Lon0-Prj%Lon0)*xmap
        grid%Y0 = Prj%Yref + (grid%Lat0-Prj%Lat0)*ymap
      ELSE
        error%Number  = IV_ERROR
        error%Routine = 'TransGridCoord'
        error%Message = 'Reference location must be set'
        error%Inform  = 'Cartesian project, Lat/lon terrain file'
        GOTO 9999
      END IF

    END IF

  CASE DEFAULT

    error%Number  = IV_ERROR
    error%Routine = 'TransGridCoord'
    error%Message = 'Unrecognized map type'
    GOTO 9999

END SELECT

!------ A SWIMming success

TransGridCoord = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SetTerrainDomain( TerHead,grid )

!------ Set domain based on terrain and project limits

USE SWIM_fi
USE SWIMparam_fd
USE TerHead_fd

IMPLICIT NONE

TYPE( TerHeaderStr ), INTENT( INOUT ) :: TerHead
TYPE( MetGrid ),      INTENT( INOUT ) :: grid

INTEGER i1, i2, j1, j2, ilen, jlen, irv, ios
INTEGER iskip, jskip, nx, ny, nxy
REAL    xmin, xmax, ymin, ymax
REAL    x1, x2, y1, y2, dxfac, dyfac
REAL    xminP, yminP, xmaxP, ymaxP
REAL    xminG, yminG, xmaxG, ymaxG
LOGICAL new_domain

CHARACTER(128) string

INTEGER, EXTERNAL :: SWIMaddLogMessage, SWIMwarningMessage, SWIMcnvCoord

SetTerrainDomain = SWIMfailure

xmin = TerHead%grid%X0
ymin = TerHead%grid%Y0
xmax = TerHead%grid%X0 + FLOAT(TerHead%Imax-1)*TerHead%grid%dX
ymax = TerHead%grid%Y0 + FLOAT(TerHead%Jmax-1)*TerHead%grid%dY

CALL SWIMsetPrjCoord() !N.B. Called again after this routine

grid%coord%type = TerHead%grid%coord
IF( grid%coord%type == I_UTM )THEN
  grid%coord%zone = TerHead%grid%UTMzone
ELSE IF( grid%coord%type == I_CARTESIAN .OR. grid%coord%type == I_METERS )THEN
  grid%coord%reference%lat = Prj%Lat0
  grid%coord%reference%lon = Prj%Lon0
  grid%coord%reference%x   = Prj%Xref
  grid%coord%reference%y   = Prj%Yref
END IF

!------ Domain set by terrain file if project domain not set

IF( Prj%Xmin == DEF_VAL_R .OR. Prj%Xmax == DEF_VAL_R .OR. &
    Prj%Ymin == DEF_VAL_R .OR. Prj%Ymax == DEF_VAL_R )THEN

  new_domain = .TRUE.
  i1 = 1; i2 = TerHead%Imax
  j1 = 1; j2 = TerHead%Jmax

ELSE !- Check terrain/project domain overlap

  new_domain = .FALSE.

  irv = SWIMcnvCoord( Prj%Xmin,Prj%Ymin,PrjCoord,xminP,yminP,grid%coord )
  irv = SWIMcnvCoord( Prj%Xmax,Prj%Ymax,PrjCoord,xmaxP,ymaxP,grid%coord )
  irv = SWIMcnvCoord( xmin,ymin,grid%coord,xminG,yminG,PrjCoord )
  irv = SWIMcnvCoord( xmax,ymax,grid%coord,xmaxG,ymaxG,PrjCoord )

  x1 = MAX(xmin,xminP); x2 = MIN(xmax,xmaxP)
  y1 = MAX(ymin,yminP); y2 = MIN(ymax,ymaxP)

  IF( x2 <= x1 .OR. y2 <= y1 )THEN  !Total mismatch

    error%Number  = DM_ERROR
    error%Routine = 'SetTerrainDomain'
    error%Message = 'Calculation domain is outside the terrain grid'
    WRITE(error%Inform,'(A,4F10.2)') &
         'Terrain Domain  : ',xminG,xmaxG,yminG,ymaxG
    WRITE(error%Action,'(A,4F10.2)') &
          'Project Domain : ',Prj%Xmin,Prj%Xmax,Prj%Ymin,Prj%Ymax
    GOTO 9999

  ELSE IF( .NOT.BTEST(grid%type,GTB_NEST) )THEN
    IF( xminP < xmin-TerHead%grid%dX .OR. &  !Project domain extends
        xmaxP > xmax+TerHead%grid%dX .OR. &  !more than a grid space
        yminP < ymin-TerHead%grid%dY .OR. &  !beyond terrain domain
        ymaxP > ymax+TerHead%grid%dY )THEN

      error%Number  = DM_ERROR
      error%Routine = 'SetTerrainDomain:Domain mismatch'
      error%Message = 'Do you want SCIPUFF to adjust the project domain and continue?'
      WRITE(error%Inform,'(A,4F10.2)')'Terrain Domain : ',xminG,xmaxG,yminG,ymaxG
      WRITE(error%Action,'(A,4F10.2)')'Project Domain : ',Prj%Xmin,Prj%Xmax,Prj%Ymin,Prj%Ymax

      irv = SWIMwarningMessage( 'Adjust project domain and retry' )
      IF( irv /= SWIMsuccess )GOTO 9999

      new_domain = .TRUE.
      Prj%Xmin = MAX(Prj%Xmin,xminG); Prj%Xmax = MIN(Prj%Xmax,xmaxG)
      Prj%Ymin = MAX(Prj%Ymin,yminG); Prj%Ymax = MIN(Prj%Ymax,ymaxG)
      irv = SWIMcnvCoord( Prj%Xmin,Prj%Ymin,PrjCoord,x1,y1,grid%coord )
      irv = SWIMcnvCoord( Prj%Xmax,Prj%Ymax,PrjCoord,x2,y2,grid%coord )

      IF( x2 <= x1 .OR. y2 <= y1 )THEN
        error%Number = DM_ERROR
        error%Action = 'Project domain extends beyond met/terrain domain'
        GOTO 9999
      END IF

    END IF
  END IF

  i1 = INT((x1-xmin)/TerHead%grid%dX) + 1
  j1 = INT((y1-ymin)/TerHead%grid%dY) + 1
  i2 = INT((x2-xmin)/TerHead%grid%dX+0.5) + 1
  j2 = INT((y2-ymin)/TerHead%grid%dY+0.5) + 1

  TerHead%grid%X0 = xmin + (i1-1)*TerHead%grid%dX
  TerHead%grid%Y0 = ymin + (j1-1)*TerHead%grid%dY

END IF

ilen = i2 - i1 + 1
jlen = j2 - j1 + 1

IF( MIN(ilen,jlen) < 6 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetTerrainDomain'
  error%Message = 'Terrain grid too small for mass-consistent calculation'
  error%Inform  = 'Must be at least 6x6'
  GOTO 9999
END IF

!------ Check if field must be averaged based on grid dimensions

CALL SetSkip( ilen,jlen,Prj%MAX1D_MET,iskip,nx,ny )
jskip = iskip

!------ Set grid parameters based on skipping

nxy  = nx*ny
ilen = MIN(nx*iskip,TerHead%Imax-i1+1)
jlen = MIN(ny*jskip,TerHead%Jmax-j1+1)
i2   = i1 + ilen - 1
j2   = j1 + jlen - 1
IF( nxy <= 0 .OR. nx == 1 .OR. ny == 1 .OR.  &
    nx > PRJ%MAX1D_MET .OR. ny > PRJ%MAX1D_MET )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetTerrainDomain'
  error%Message = 'Error setting terrain read skip parameter'
  WRITE(error%Inform,'(A,3I8)')'nx,ny,nxy=',nx,ny,nxy
  GOTO 9999
END IF

IF( iskip > 1 )THEN
  WRITE(string,'(A,I1)',IOSTAT=ios) 'Skipping data in read_ter: ',iskip
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

TerHead%Is = i1; TerHead%Iskip = iskip; TerHead%nX = nx
TerHead%Js = j1; TerHead%Jskip = jskip; TerHead%nY = ny

!------ Adjust grid size for skipping

TerHead%grid%dX = FLOAT(iskip)*TerHead%grid%dX
TerHead%grid%dY = FLOAT(jskip)*TerHead%grid%dY

!------ Set met grid info

grid%coord%type = TerHead%grid%coord

IF( grid%coord%type == I_UTM )grid%coord%zone = TerHead%grid%UTMzone

IF( grid%coord%type == I_CARTESIAN .OR. grid%coord%type == I_METERS )THEN
  grid%coord%reference%lat = Prj%Lat0
  grid%coord%reference%lon = Prj%Lon0
  grid%coord%reference%x   = Prj%Xref
  grid%coord%reference%y   = Prj%Yref
END IF

grid%Xmin = TerHead%grid%X0; grid%Xmax = TerHead%grid%X0 + FLOAT(TerHead%nX-1)*TerHead%grid%dX
grid%dX   = TerHead%grid%dX; grid%nX = TerHead%nX

grid%Ymin = TerHead%grid%Y0; grid%Ymax = TerHead%grid%Y0 + FLOAT(TerHead%nY-1)*TerHead%grid%dY
grid%dY   = TerHead%grid%dY; grid%nY = TerHead%nY

grid%nXY = grid%nX*grid%nY

IF( MIN(grid%nX,grid%nY) < 6 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SetTerrainDomain'
  error%Message = 'Terrain grid too small for mass-consistent calculation'
  error%Inform  = 'Must be at least 6x6'
  GOTO 9999
END IF

!------ Set project domain

IF( new_domain )THEN

  dxfac = 0.5*(iskip-1)*TerHead%grid%dX/FLOAT(iskip)
  dyfac = 0.5*(jskip-1)*TerHead%grid%dY/FLOAT(jskip)

  Prj%Xmin = MAX(TerHead%grid%X0-dxfac,xmin)
  Prj%Ymin = MAX(TerHead%grid%Y0-dyfac,ymin)

  Prj%Xmax = MIN(TerHead%grid%X0+FLOAT(nx-1)*TerHead%grid%dX+dxfac,xmax)
  Prj%Ymax = MIN(TerHead%grid%Y0+FLOAT(ny-1)*TerHead%grid%dY+dyfac,ymax)

  IF( Prj%coord /= grid%coord%type )THEN
    xminP = Prj%Xmin; yminP = Prj%Ymin; xmaxP = Prj%Xmax; ymaxP = Prj%Ymax
    irv = SWIMcnvCoord( xminP,yminP,grid%coord,Prj%Xmin,Prj%Ymin,PrjCoord )
    irv = SWIMcnvCoord( xmaxP,ymaxP,grid%coord,Prj%Xmax,Prj%Ymax,PrjCoord )
  END IF

END IF

SetTerrainDomain = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadTerrainFields( lun,TerHead,grid )

!------ Read terrain and landuse fields

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE TerHead_fd
USE landuse_fd
USE prjstruct_fd

IMPLICIT NONE

INTEGER,              INTENT( IN    ) :: lun
TYPE( TerHeaderStr ), INTENT( IN    ) :: TerHead
TYPE( MetGrid ),      INTENT( INOUT ) :: grid

TYPE( landuse_init ) file_landuse

INTEGER imax, jmax, i1, j1, iskip, jskip, nx, ny, nxy
INTEGER ios, irv, nrec
INTEGER ivar, i, j, is, i0, j0, n, ii, jj, ix
INTEGER imax2, jmax2, jul, iseason, lu_index
REAL    hscale, dum, lat, lon, yp
LOGICAL lcat

CHARACTER(8) ctmp

REAL, DIMENSION(:,:), ALLOCATABLE :: work
REAL, DIMENSION(:),   ALLOCATABLE :: rtmp

REAL, DIMENSION(:), POINTER :: hs, zruf, hc, alpha, albedo, bowen

INTEGER, EXTERNAL :: SWIMallocTerrain
INTEGER, EXTERNAL :: InitLandUse, InitLandUseCat, ExitLandUse
INTEGER, EXTERNAL :: AddLandUseCat, OutLandUseCat, SetLandUse
INTEGER, EXTERNAL :: GetSeason, JulianPrj, SWIMgetLL

CHARACTER(128), EXTERNAL :: ArraySizeStr

ReadTerrainFields  = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'ReadTerrainFields'
error%Message = 'Error reading terrain file'

!------ Copy out header variables into locals

imax  = TerHead%Imax
jmax  = TerHead%Jmax
i1    = TerHead%Is
j1    = TerHead%Js
iskip = TerHead%Iskip
jskip = TerHead%Jskip
nx    = TerHead%nX
ny    = TerHead%nY
nrec  = TerHead%nskip
nxy   = nx*ny

hscale = TerHead%hscale

!------ Allocate terrain/landuse fields

IF( TerHead%lcat )grid%type = IBSET(grid%type,GTB_LANDUSE)

irv = SWIMallocTerrain( grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Assign local pointers to terrain/landuse fields

hs     => grid%terrain%H
zruf   => grid%landcover%roughness
hc     => grid%landcover%canopyHt
alpha  => grid%landcover%alpha
albedo => grid%landcover%albedo
bowen  => grid%landcover%Bowen

!------ Allocate workspace

ALLOCATE( work(imax,jmax),rtmp(nxy),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'ReadTerrainFields'
  error%Message = 'Error allocating array to read terrain file'
  error%Inform  =  ArraySizeStr(2,(/imax,jmax/))
  GOTO 9999
END IF

!----- Initialize landuse dll

IF( TerHead%lcat )THEN

  file_landuse%lun  = 0
  file_landuse%file = TRIM(Prj%MC%LandUseFile)

  irv = InitLandUse( file_landuse )
  IF( irv /= 1 )THEN
    CALL SWIMgetLandUseError()
    GOTO 9999
  END IF

  irv = InitLandUseCat( nxy )         !Category cells
  IF( irv /= 1 )THEN
    CALL SWIMgetLandUseError()
    GOTO 9999
  END IF

END IF

ctmp = TRIM(NOT_SET_C)

!------ Loop over all fields

Fields : DO ivar = 1,TerHead%nField+1

  lcat = ivar > 1 .AND. TerHead%lcat    !Set landuse category flag

!------ Read fields; format depends of variable

  IF( ivar == 1 .OR. lcat )THEN  !Terrain or landuse category
    READ(lun,'(12F6.0) ',IOSTAT=ios) ((work(i,j),i=1,imax),j=1,jmax)
    IF( ios /= 0 )THEN
      IF( lcat )THEN
        error%Action = 'Error reading landuse category'
      ELSE
        error%Action = 'Error reading terrain height field'
      END IF
      GOTO 9999
    END IF
  ELSE                           !Other landuse fields
    READ(lun,'(12F10.0)',IOSTAT=ios) ((work(i,j),i=1,imax),j=1,jmax)
    IF( ios /= 0 )THEN
      error%Action = 'Error reading landuse field '//TRIM(ctmp)
      GOTO 9999
    END IF
  END IF

!------ Average over "skipped" grid points

  jLoop : DO j = 1,ny
    j0 = j1 + (j-1)*jskip - 1     !Row-index offset into full domain

    iLoop : DO i = 1,nx
      is       = (j-1)*nx + i
      rtmp(is) = 0.               !Zero sum

      i0 = i1 + (i-1)*iskip - 1   !Column-index offset into full domain

      IF( .NOT.lcat )THEN         !Sum over cells, except for category

        n = 0
        DO jj = j0+1,MIN(j0+jskip,jmax)
          DO ii = i0+1,MIN(i0+iskip,imax)
            rtmp(is) = rtmp(is) + work(ii,jj)
            n = n + 1
          END DO
        END DO
        rtmp(is) = rtmp(is) / FLOAT(n)

      ELSE                         !Use LandUse function to combine categories

        DO jj = j0+1,MIN(j0+jskip,jmax)
          DO ii = i0+1,MIN(i0+iskip,imax)
            irv = AddLandUseCat( is,NINT(work(ii,jj)) )
            IF( irv /= 1 )THEN
              CALL SWIMgetLandUseError()
              GOTO 9999
            END IF
          END DO
        END DO

      END IF

    END DO iLoop

  END DO jLoop

!------ Set combined landuse category

  IF( lcat )THEN

    DO is = 1,nxy
      rtmp(is) = FLOAT(OutLandUseCat(is))
      IF( rtmp(is) == 0. )THEN
        CALL SWIMgetLandUseError()
        GOTO 9999
      END IF
    END DO

  ELSE IF( ivar == 1 )THEN !IF( TRIM(ctmp) /= 'HCANP' .AND. TRIM(ctmp) /= 'ALPHC' )THEN

!------ Or smooth terrain (and other landuse fields) with 2D 1-2-1 filter

    CALL smooth( rtmp,nx,ny )

  END IF

!------ Fill appropriate fields

  IF( ivar == 1 )THEN   !Terrain is first variable

    IF( BTEST(Prj%MC%type,MCB_TER) )THEN
      DO i = 1,nxy
        hs(i) = hscale*rtmp(i)
      END DO
    ELSE
      DO i = 1,nxy
        hs(i) = 0.
      END DO
    END IF

!------ Set nrec for skipping latlon fields if present

    IF( TerHead%nField > 0 .AND. nrec == 0 )THEN
      READ(lun,'(A8)',IOSTAT=ios) ctmp
      IF( ios /= 0 )THEN
        error%Action = 'Error reading looking for LATLON string'
        GOTO 9999
      END IF
      CALL cupper( ctmp )
      IF( TRIM(ctmp) == 'LATLON' )THEN
        READ(lun,*,IOSTAT=ios) (dum,i=1,4),imax2,jmax2
        IF( ios /= 0 )THEN
          error%Action = 'Error LATLON domain record'
          GOTO 9999
        END IF
        nrec = (imax2*jmax2+11)/12
      END IF
      BACKSPACE( lun,IOSTAT=ios )
    END IF

  ELSE

    IF( ADJUSTL(TRIM(ctmp)) /= ADJUSTL(TRIM(TerHead%FieldName(ivar-1))) )THEN
      error%Number  = RD_ERROR
      error%Routine = 'ReadTerrainFields'
      error%Message = 'Wrong field name : '//TRIM(ctmp)
      error%Inform  = 'Expecting : '//TRIM(TerHead%FieldName(ivar-1))
      GOTO 9999
    END IF

    IF( ADJUSTL(TRIM(ctmp)) == 'ZRUF' )THEN
      DO i = 1,nxy
        zruf(i) = hscale*rtmp(i)
      END DO

    ELSE IF( ADJUSTL(TRIM(ctmp)) == 'ALBED' )THEN
      DO i = 1,nxy
        albedo(i) = rtmp(i)
      END DO

    ELSE IF( ADJUSTL(TRIM(ctmp)) == 'BOWEN' )THEN
      DO i = 1,nxy
        bowen(i) = rtmp(i)
      END DO

    ELSE IF( ADJUSTL(TRIM(ctmp)) == 'HCANP' )THEN
      DO i = 1,nxy
        hc(i) = hscale*rtmp(i)
      END DO

    ELSE IF( ADJUSTL(TRIM(ctmp)) == 'ALPHC' )THEN
      DO i = 1,nxy
        alpha(i) = rtmp(i)
      END DO

    ELSE IF( ADJUSTL(TRIM(ctmp)) == 'CATEGORY' )THEN

      jul   = JulianPrj( 0. ) !Julian day at project start
      grid%landcover%julLU = jul

      DO j = 1,ny
        i0 = (j-1)*nx
        yp = TerHead%grid%Y0 + FLOAT(j-1)*TerHead%grid%dY
        irv = SWIMgetLL( TerHead%grid%X0,yp,lon,lat )
        IF( irv /= SWIMsuccess )GOTO 9999

        iseason = GetSeason( lat,jul )
        IF( iseason <= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'GetSeason'
          error%Message = 'Must have lat/lon and julian day'
          GOTO 9999
        END IF

        DO ix = 1,nx
          i = i0 + ix
          lu_index = NINT(rtmp(i))
          irv = SetLandUse( lu_index,iseason,Prj%BL%i_wet, &
                             zruf(i),hc(i),alpha(i),albedo(i),bowen(i) )
          IF( irv /= 1 )THEN
            error%Number  = UK_ERROR
            error%Routine = 'SetLandUse'
            error%Message = 'Land use index out of range'
            GOTO 9999
          END IF

          grid%landcover%LandUse(i) = lu_index

        END DO
      END DO

    END IF

  END IF

  IF( ivar /= TerHead%nField+1 )THEN
    IF( nrec > 0 )THEN
      DO i = 1,nrec+1
        READ(lun,*,IOSTAT=ios)
        IF( ios /= 0 )THEN
          error%Action = 'Error skipping fields '
          GOTO 9999
        END IF
      END DO
    END IF
    READ(lun,'(A8)',IOSTAT=ios) ctmp
    IF( ios /= 0 )THEN
      error%Action = 'Error reading field name'
      GOTO 9999
    END IF
    CALL cupper( ctmp )
  END IF

END DO Fields

!------- Find minimum terrain height; subtract from height array

CALL SetHmin( grid )

!------- Set constant values for landuse fields not read or set

IF( .NOT.TerHead%lcat )THEN
  IF( .NOT.TerHead%lzruf )THEN
    DO i = 1,nxy
      zruf(i) = Prj%BL%zruf
    END DO
  END IF
  IF( .NOT.TerHead%lhcanp )THEN
    DO i = 1,nxy
      hc(i) = Prj%BL%hc
    END DO
  END IF
  IF( .NOT.TerHead%lalpha )THEN
    DO i = 1,nxy
      alpha(i) = Prj%BL%alpha
    END DO
  END IF
  IF( .NOT.TerHead%lbowen )THEN
    DO i = 1,nxy
      bowen(i) = Prj%BL%Bowen
    END DO
  END IF
  IF( .NOT.TerHead%lalbed )THEN
    DO i = 1,nxy
      albedo(i) = Prj%BL%albedo
    END DO
  END IF
END IF

!------ Set terrain/landuse bits

grid%type = IBSET(grid%type,GTB_TERRAIN)

IF( TerHead%lzruf  .OR. TerHead%lcat )grid%type = IBSET(grid%type,GTB_ZRUF)
IF( TerHead%lhcanp .OR. TerHead%lcat )grid%type = IBSET(grid%type,GTB_HCNP)
IF( TerHead%lalpha .OR. TerHead%lcat )grid%type = IBSET(grid%type,GTB_ALPHA)
IF( TerHead%lbowen .OR. TerHead%lcat )grid%type = IBSET(grid%type,GTB_ALBEDO)
IF( TerHead%lalbed .OR. TerHead%lcat )grid%type = IBSET(grid%type,GTB_BOWEN)

!------ Check for zero or negative roughness and canopy heights

IF( BTEST(grid%type,GTB_ZRUF) )THEN
  DO i = 1,nxy
    IF( zruf(i) <= 0. )zruf(i) = Prj%BL%zruf
  END DO
END IF

IF( BTEST(grid%type,GTB_HCNP) )THEN
  DO i = 1,nxy
    IF( hc(i) < 0. )hc(i) = 0.
  END DO
END IF

!------ Another SWIMming success

ReadTerrainFields = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

IF( ALLOCATED(work) )DEALLOCATE( work,STAT=ios )
IF( ALLOCATED(rtmp) )DEALLOCATE( rtmp,STAT=ios )

RETURN
END
