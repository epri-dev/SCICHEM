INTEGER FUNCTION SWIMaddNest( NestDomain,H,Zruf,Hcnp,Alpha,LandUse )

!------ Add nest to met fields
!       Note that setup is completed after 'parent' field is completely defined

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMaddNest

USE SWIM_fi
USE SWIMparam_fd
USE uniformGridT_fd
USE constants_fd

IMPLICIT NONE

TYPE( uniformGridT ),            INTENT( INOUT ) :: NestDomain
REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: H
REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Zruf
REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Hcnp
REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Alpha
INTEGER, DIMENSION(*), OPTIONAL, INTENT( IN    ) :: LandUse

TYPE( MetField), POINTER :: fld, fldp

INTEGER irv, ifldp, i, nxy
INTEGER ifld

INTEGER, EXTERNAL :: SWIMreallocMetField, SWIMallocTerrain, SWIMwarningMessage, AddOutput
INTEGER, EXTERNAL :: SWIMaddSmoothPotential
LOGICAL, EXTERNAL :: SWIMaddNewNest

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMaddNest = SWIMfailure

ifld = -1

!------ Check if adding a nest is required

IF( NestDomain%mode < 0 )THEN

  ifldp = ABS(NestDomain%mode)

ELSE IF( NestDomain%mode >= 10000 )THEN

  ifld = NestDomain%mode/10000
  NestDomain%mode = NestDomain%mode - ifld*10000

  ifldp = NestDomain%mode/1000
  NestDomain%mode = NestDomain%mode - ifldp*1000

ELSE

  IF( .NOT.SWIMaddNewNest(NestDomain,ifldp) )THEN
    SWIMaddNest = SWIMresult
    GOTO 9999
  END IF

END IF

!------ Add a met field

IF( ifld == -1 )THEN

  irv = SWIMreallocMetField( 1 )
  IF( irv /= SWIMsuccess )GOTO 9999

  ifld = numField

END IF

!------ Point to nested met field; Set index of 'parent' field

fld  => field(ifld)
fldp => field(ifldp)

fld%index = 0
CALL setFieldIndex( fld%index,ifld )

!------ Set times as 'not set'

fld%t     = NOT_SET_R
fld%tNext = NOT_SET_R
fld%tBL   = NOT_SET_R

!------ Set initial status

fld%status = 0
fld%status = IBSET(fld%status,FSB_DOINIT)
fld%status = IBSET(fld%status,FSB_UPDATE)

IF( BTEST(fldp%status,FSB_FIRST) )fld%status = IBSET(fld%status,FSB_FIRST)

!------ Set field type

fld%type = IBSET(0,FTB_NEST)          !Implies grid interpolation
fld%type = IBSET(fld%type,FTB_W)
fld%type = IBSET(fld%type,FTB_MCWIF)

IF( BTEST(fldp%type,FTB_T) )fld%type = IBSET(fld%type,FTB_T)
IF( BTEST(fldp%type,FTB_P) )fld%type = IBSET(fld%type,FTB_P)
IF( BTEST(fldp%type,FTB_H) )fld%type = IBSET(fld%type,FTB_H)
IF( BTEST(fldp%type,FTB_QCLD) )fld%type = IBSET(fld%type,FTB_QCLD)

IF( BTEST(fldp%type,FTB_LSV ) )fld%type = IBSET(fld%type,FTB_LSV)
IF( BTEST(fldp%type,FTB_LSVL) )fld%type = IBSET(fld%type,FTB_LSVL)

!------ Set BL type

SELECT CASE( fldp%BLtype )

  CASE( BLP_SBL,BLP_NONE )
    fld%BLtype = fldp%BLtype

  CASE( BLP_MET )
    IF( BTEST(fldp%type,FTB_MOL) )THEN  !Don't interpolate MOL - too dangerous since Ustar may vary a lot
      fld%BLtype = BLP_NEST
    ELSE
      fld%BLtype = BLP_MET
      IF( BTEST(fldp%type,FTB_ZI)   )fld%type = IBSET(fld%type,FTB_ZI)
      IF( BTEST(fldp%type,FTB_HFLX) )fld%type = IBSET(fld%type,FTB_HFLX)
      IF( BTEST(fldp%type,FTB_UST)  )fld%type = IBSET(fld%type,FTB_UST) !Set for SWIMinterpGridded, but Ustar re-computed on nest grid
    END IF

  CASE DEFAULT
    fld%BLtype = BLP_NEST

END SELECT

IF( BTEST(fldp%type,FTB_CLDCV)  )fld%type = IBSET(fld%type,FTB_CLDCV)

IF( BTEST(fldp%type,FTB_PRCP)  )fld%type = IBSET(fld%type,FTB_PRCP)
IF( BTEST(fldp%type,FTB_PRATE) )fld%type = IBSET(fld%type,FTB_PRATE)
IF( BTEST(fldp%type,FTB_ACCPR) )fld%type = IBSET(fld%type,FTB_ACCPR)

fld%BLaux%nz = 0  !Set later

!------ Set grid source type and save parent field number

fld%gridSource%type = IBSET(fld%gridSource%type,GSB_NEST)
fld%gridSource%unit = ifldp

!------ Set nested grid parameters
!       Staggered grid is Arakawa C; vertical velocity too

fld%grid%type = 0

fld%grid%type = IBSET(fld%grid%type,GTB_STAGGER)
fld%grid%type = IBSET(fld%grid%type,GTB_STAGGERZ)

  IF( NestDomain%mode < 0 )THEN    !TER file with MEDOC outer met

    CALL SetFieldGridMapType( fld%grid%type )

    fld%grid%coord%type = Prj%coord
    fld%grid%coord%zone = Prj%UTMzone

    fld%grid%coord%Reference%x   = Prj%Xref
    fld%grid%coord%Reference%y   = Prj%Yref
    fld%grid%coord%Reference%lon = Prj%Lon0
    fld%grid%coord%Reference%lat = Prj%Lat0

  ELSE

    SELECT CASE( NestDomain%mode )
      CASE( I_LATLON )
        fld%grid%type = IBSET(fld%grid%type,GTB_LATLON)
      CASE( I_UTM )
        fld%grid%type = IBSET(fld%grid%type,GTB_UTM)
      CASE( I_CARTESIAN )
        fld%grid%type = IBSET(fld%grid%type,GTB_CARTESIAN)
      CASE( I_METERS )
        fld%grid%type = IBSET(fld%grid%type,GTB_METERS)
    END SELECT

    fld%grid%coord%type = NestDomain%mode
    fld%grid%coord%zone = NestDomain%UTMzone

  END IF

  fld%grid%Xmin = NestDomain%xmin
  fld%grid%nX   = NestDomain%nx
  fld%grid%dX   = NestDomain%dx

  fld%grid%Ymin = NestDomain%ymin
  fld%grid%nY   = NestDomain%ny
  fld%grid%dY   = NestDomain%dy

fld%grid%Xmax = fld%grid%Xmin + FLOAT(fld%grid%nX-1)*fld%grid%dX
fld%grid%Ymax = fld%grid%Ymin + FLOAT(fld%grid%nY-1)*fld%grid%dY

fld%grid%nXY = fld%grid%nX*fld%grid%nY
nxy          = fld%grid%nXY

IF( PRESENT(LandUse) )fld%grid%type = IBSET(fld%grid%type,GTB_LANDUSE)

irv = SWIMallocTerrain( fld%grid )
IF( irv /= SWIMsuccess )GOTO 9999

fld%grid%nZ = 0

!------ Copy terrain and landcover parameters (if input)

  IF( PRESENT(H) )THEN

    IF( H(1) /= NOT_SET_R )THEN
      DO i = 1,nxy
        fld%grid%terrain%H(i) = H(i)
      END DO
      fld%grid%type = IBSET(fld%grid%type,GTB_TERRAIN)

      IF( PRESENT(Zruf) )THEN
        DO i = 1,nxy
          fld%grid%landcover%roughness(i) = Zruf(i)
        END DO
        fld%grid%type = IBSET(fld%grid%type,GTB_ZRUF)
      END IF

      IF( PRESENT(Hcnp) )THEN
        DO i = 1,nxy
          fld%grid%landcover%canopyHt(i) = Hcnp(i)
        END DO
        fld%grid%type = IBSET(fld%grid%type,GTB_HCNP)
      END IF

      IF( PRESENT(Alpha) )THEN
        DO i = 1,nxy
          fld%grid%landcover%alpha(i) = Alpha(i)
        END DO
        fld%grid%type = IBSET(fld%grid%type,GTB_ALPHA)
      END IF

      IF( PRESENT(LandUse) )THEN
        DO i = 1,nxy
          fld%grid%landcover%LandUse(i) = LandUse(i)
        END DO
      END IF

    END IF
  END IF

!------ Add output file

IF( (Prj%lOut3D .OR. Prj%lOut2D) .AND. lInitOutput )THEN
  irv = AddOutput( ifld,unitMet )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF


!------ Add potential smooth fields

i = numField
irv = SWIMaddSmoothPotential( i )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

SWIMaddNest = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMinitGriddedNest( ifldIn )

!------ Add nested field with gridded parent

USE SWIM_fi
USE SWIMparam_fd
USE prjstruct_fd
USE uniformGridT_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifldIN

INTEGER irv, alloc_stat, i, ifldp, nxy
REAL    xc, yc, xmap, ymap, dssNest, dssMin, dss
LOGICAL lTerr

CHARACTER(512) TerFile

TYPE( MetGrid      ) :: TerGrid
TYPE( uniformGridT ) :: domain

REAL,    DIMENSION(:), ALLOCATABLE :: cnpHt, zruf, alpha, ht
INTEGER, DIMENSION(:), ALLOCATABLE :: LandUse

INTERFACE
  INTEGER FUNCTION SWIMaddNest( NestDomain,H,Zruf,Hcnp,Alpha,LandUse )
    USE uniformGridT_fd
    TYPE( uniformGridT ),            INTENT( INOUT ) :: NestDomain
    REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: H
    REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Zruf
    REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Hcnp
    REAL,    DIMENSION(*), OPTIONAL, INTENT( IN    ) :: Alpha
    INTEGER, DIMENSION(*), OPTIONAL, INTENT( IN    ) :: LandUse
  END FUNCTION SWIMaddNest
END INTERFACE

INTEGER, EXTERNAL :: SWIMallocTerrain, SWIMreadTerrain
INTEGER, EXTERNAL :: SWIMcnvCoord
INTEGER, EXTERNAL :: SWIMwarningMessage
INTEGER, EXTERNAL :: GridLogMsg
INTEGER, EXTERNAL :: SWIMreadTerrainNest, SWIMsetNestDomain, SWIMdeallocGrid
REAL,    EXTERNAL :: RateMyParent

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMinitGriddedNest = SWIMfailure

!------ Read terrain file

CALL nullifyMetGrid( TerGrid )

TerGrid%type = 0
TerGrid%type = IBSET(TerGrid%type,GTB_NEST) !Project domain will not be re-defined
lTerr        = .TRUE.

IF( ifldIn <= 0 )THEN

  TerFile = TRIM(Prj%MC%TerFile)
  irv = SWIMreadTerrain( TerGrid )

ELSE

  IF( field(ifldIN)%gridSource%Source(1)(1:4) == 'NEST' .OR. &
      field(ifldIN)%gridSource%Source(1)(1:9) == 'SUBDOMAIN' )THEN

    irv = SWIMsetNestDomain( field(ifldIN)%gridSource%Source(1), &
                             field(ifldIN)%gridSource%unit,TerGrid )
    lTerr = .FALSE.

  ELSE

    TerFile = TRIM(field(ifldIN)%gridSource%Source(1))
    irv = SWIMreadTerrainNest( TerFile,TerGrid )

  END IF

END IF
IF( irv /= SWIMsuccess )GOTO 9999

!------ Define map factor at center of nested domain

xc = 0.5*(TerGrid%Xmin+TerGrid%Xmax)
yc = 0.5*(TerGrid%Ymin+TerGrid%Ymax)

CALL SWIMmapfac( TerGrid%coord,xc,yc,xmap,ymap )

dssNest = (TerGrid%dX/xmap)**2 + (TerGrid%dY/ymap)**2

!------ Loop over gridded fields; look for best parenting skills

IF( ifldIn <= 0 )THEN

ifldp  = 0
dssMin = HUGE(0.)

DO i = 1,numField
  dss = RateMyParent( TerGrid%Xmin,TerGrid%Xmax,TerGrid%dX, &
                      TerGrid%Ymin,TerGrid%Ymax,TerGrid%dY,TerGrid%coord, &
                      field(i)%grid%Xmin,field(i)%grid%Xmax,field(i)%grid%dX, &
                      field(i)%grid%Ymin,field(i)%grid%Ymax,field(i)%grid%dY,field(i)%grid%coord )
  IF( dss > 0. )THEN
    IF( ifldp == 0 )ifldp = i
    IF( dss < dssMin )THEN
      ifldp  = i
      dssMin = dss
    END IF
  END IF
END DO

IF( ifldp == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMinitGriddedNest'
  error%Message = 'Terrain file domain is not contained within gridded domain'
  CALL ReportFileName( error%Inform,'File=',TerFile )
  error%Action  = 'Modify or eliminate terrain file'
  GOTO 9999
ELSE IF( dssMin < dssNest )THEN
  error%Number  = WN_ERROR
  error%Routine = 'SWIMinitGriddedNest'
  error%Message = 'A nested field already exists with finer resolution than terrain grid'
  error%Inform  = 'Terrain file will be ignored; no mass-consistent calculation'
  error%Action  = 'Do you wish to continue?'
  irv = SWIMwarningMessage( 'User elected not to continue' )
  IF( irv == SWIMsuccess )SWIMinitGriddedNest = SWIMresult
  GOTO 9999
END IF

ELSE

  ifldp = field(ifldIN)%gridSource%unit

END IF

!------ Initialize grid structure for adding nest

domain%xmin = TerGrid%Xmin; domain%dx = TerGrid%dX;; domain%nx = TerGrid%nX
domain%ymin = TerGrid%Ymin; domain%dy = TerGrid%dY;; domain%ny = TerGrid%nY

!------ Encode parent field number

IF( ifldIn > 0 )THEN
  domain%mode = 10000*ifldIN + 1000*ifldp + TerGrid%coord%type
ELSE
  domain%mode = -ifldp
END IF

IF( TerGrid%coord%type == I_UTM )THEN
  domain%UTMZone = TerGrid%coord%zone
ELSE
  domain%UTMZone = NOT_SET_I
END IF

nxy = TerGrid%nXY

!------ Copy terrain and canopy arrays for building nest field

ALLOCATE( ht(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitGriddedNest'
  error%Message = 'Error allocating terrain elevation array'
  error%Inform  =  ArraySizeStr( 2,(/TerGrid%nx,TerGrid%ny/) )
  GOTO 9999
END IF

IF( lTerr )THEN
  DO i = 1,nxy
    ht(i) = TerGrid%terrain%H(i) + TerGrid%Hmin !Add Hmin back since it's subtracted again later
  END DO
ELSE
  DO i = 1,nxy
    ht(i) = NOT_SET_R  !Do not set terrain, etc. in SWIMaddNest
  END DO
END IF

IF( BTEST(TerGrid%type,GTB_ZRUF) .OR. &
    BTEST(TerGrid%type,GTB_HCNP) .OR. &
    BTEST(TerGrid%type,GTB_ALPHA) )THEN

  ALLOCATE( cnpHt(nxy),alpha(nxy),zruf(nxy),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitGriddedNest'
    error%Message = 'Error allocating roughness & canopy arrays'
    error%Inform  =  ArraySizeStr( 2,(/TerGrid%nx,TerGrid%ny/) )
    GOTO 9999
  END IF

  IF( BTEST(TerGrid%type,GTB_HCNP) )THEN
    DO i = 1,nxy
      cnpHt(i) = TerGrid%landcover%canopyHt(i)
    END DO
  ELSE
    DO i = 1,nxy
      cnpHt(i) = Prj%BL%hc
    END DO
  END IF

  IF( BTEST(TerGrid%type,GTB_ZRUF) )THEN
    DO i = 1,nxy
      zruf(i) = TerGrid%landcover%roughness(i)
    END DO
  ELSE
    DO i = 1,nxy
      zruf(i) = Prj%BL%zruf
    END DO
  END IF

  IF( BTEST(TerGrid%type,GTB_ALPHA) )THEN
    DO i = 1,nxy
      alpha(i) = TerGrid%landcover%alpha(i)
    END DO
  ELSE
    DO i = 1,nxy
      alpha(i) = Prj%BL%alpha
    END DO
  END IF

  IF( BTEST(TerGrid%type,GTB_LANDUSE) )THEN
    ALLOCATE( LandUse(nxy),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinitGriddedNest'
      error%Message = 'Error allocating landuse array'
      error%Inform  =  ArraySizeStr( 2,(/TerGrid%nx,TerGrid%ny/) )
      GOTO 9999
    END IF
    DO i = 1,nxy
      LandUse(i) = TerGrid%landcover%LandUse(i)
    END DO
    irv = SWIMaddNest( domain,ht,zruf,cnpHt,alpha,LandUse )
  ELSE
    irv = SWIMaddNest( domain,ht,zruf,cnpHt,alpha )
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE

  irv = SWIMaddNest( domain,ht )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

IF( ifldIN > 0 )THEN
  i = ifldIN
ELSE
  i = numField
END IF

IF( lTerr )THEN
  CALL SetHmin( field(i)%grid )
ELSE
  field(i)%status = IBSET(field(i)%status,FSB_TERRAIN)
END IF

!------ Log messages

irv = GridLogMsg( field(i) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Another SWIMming success

SWIMinitGriddedNest = SWIMresult

9999 CONTINUE

IF( ALLOCATED(ht)      )DEALLOCATE( ht,   STAT=alloc_stat )
IF( ALLOCATED(cnpHt)   )DEALLOCATE( cnpHt,STAT=alloc_stat )
IF( ALLOCATED(zruf)    )DEALLOCATE( zruf, STAT=alloc_stat )
IF( ALLOCATED(alpha)   )DEALLOCATE( alpha,STAT=alloc_stat )
IF( ALLOCATED(LandUse) )DEALLOCATE( LandUse,STAT=alloc_stat )

irv = SWIMdeallocGrid( TerGrid )

RETURN
END
!==============================================================================

REAL FUNCTION RateMyParent( Child_Xmin,Child_Xmax,Child_dX, &
                            Child_Ymin,Child_Ymax,Child_dY,Child_coord, &
                            Parent_Xmin,Parent_Xmax,Parent_dX, &
                            Parent_Ymin,Parent_Ymax,Parent_dY,Parent_coord ) RESULT( dss )

!------ Return measure of grid resolution of a potential parent grid, i.e.,
!       child grid is contained entirely within parent

!DEC$ ATTRIBUTES DLLEXPORT :: RateMyParent

USE SWIM_fi

IMPLICIT NONE

REAL,             INTENT( IN    ) :: Child_Xmin, Child_Xmax, Child_dX, Child_Ymin, Child_Ymax, Child_dY
REAL,             INTENT( IN    ) :: Parent_Xmin,Parent_Xmax,Parent_dX,Parent_Ymin,Parent_Ymax,Parent_dY
TYPE( MapCoord ), INTENT( INOUT ) :: Child_coord, Parent_coord

INTEGER irv
REAL    Xmin, Xmax, Ymin, Ymax, xtem, ytem
REAL    xc, yc, xmap, ymap

INTEGER, EXTERNAL :: SWIMcnvCoord

dss = 0.

IF( Child_coord%type /= Parent_coord%type )THEN
  irv = SWIMcnvCoord( Child_Xmin+Child_dX,Child_Ymin+Child_dY,Child_coord, &
                                                        Xmin,Ymin,Parent_coord )
  Xmax = Xmin; Ymax = Ymin
  irv = SWIMcnvCoord( Child_Xmin+Child_dX,Child_Ymax-Child_dY,Child_coord, &
                                                        xtem,ytem,Parent_coord )
  CALL CheckDomMinMax( xtem,ytem,Xmin,Ymin,Xmax,Ymax )
  irv = SWIMcnvCoord( Child_Xmax-Child_dX,Child_Ymax-Child_dY,Child_coord, &
                                                        xtem,ytem,Parent_coord )
  CALL CheckDomMinMax( xtem,ytem,Xmin,Ymin,Xmax,Ymax )
  irv = SWIMcnvCoord( Child_Xmax-Child_dX,Child_Ymin+Child_dY,Child_coord, &
                                                        xtem,ytem,Parent_coord )
  CALL CheckDomMinMax( xtem,ytem,Xmin,Ymin,Xmax,Ymax )
ELSE
  Xmin = Child_Xmin+Child_dX; Xmax = Child_Xmax-Child_dX
  Ymin = Child_Ymin+Child_dY; Ymax = Child_Ymax-Child_dY
END IF

IF( Xmin >= Parent_Xmin .AND. Xmax <= Parent_Xmax .AND. &
    Ymin >= Parent_Ymin .AND. Ymax <= Parent_Ymax )THEN
  xc = 0.5*(Parent_Xmin+Parent_Xmax)
  yc = 0.5*(Parent_Ymin+Parent_Ymax)
  CALL SWIMmapfac( Parent_coord,xc,yc,xmap,ymap )
  dss = (Parent_dX/xmap)**2 + (Parent_dY/ymap)**2
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetNestDomain( string,ifldp,TerGrid )

!------ Setup nested domains for obs assimilation
!       N.B. Setting terrain, etc. deferred until call to SetupNestField

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer

IMPLICIT NONE

CHARACTER(*),    INTENT( INOUT ) :: string
INTEGER,         INTENT( IN    ) :: ifldp
TYPE( MetGrid ), INTENT( OUT   ) :: TerGrid

INTEGER ios, irv, n_arg, nch, i
REAL    res
LOGICAL lerr, lRes

CHARACTER(4) kwrd

INTEGER, PARAMETER :: MAX_ARG = 10
CHARACTER(256), DIMENSION(MAX_ARG) :: c_arg

TYPE( GridSrc ) :: src

TYPE( MetField ), POINTER :: fldp
TYPE( MetGrid  ), POINTER :: grdp

INTEGER, EXTERNAL :: SWIMallocTerrain

SWIMsetNestDomain = SWIMfailure

!------ Parse string containing nest grid definition

CALL get_next_data( 0,string,nch,kwrd,n_arg,c_arg,MAX_ARG,lerr )
IF( lerr )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMsetNestDomain'
  error%Message = 'Error reading nest grid parameters'
  GOTO 9999
END IF

CALL cupper( c_arg(1) )
IF( TRIM(c_arg(1)) /= 'NEST' .AND. TRIM(c_arg(1)) /= 'SUBDOMAIN' )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMsetNestDomain'
  error%Message = 'Invalid string defining nest grid'
  GOTO 9999
END IF

lRes = .FALSE.

!------ Check coordinates (and set index for next element of c_arg to read)

SELECT CASE( TRIM(c_arg(2)) )
  CASE( 'LATLON','LL','LLA' )
    TerGrid%coord%type = I_LATLON
    i = 2

  CASE( 'UTM' )
    TerGrid%coord%type = I_LATLON
    READ(c_arg(3),*,IOSTAT=ios ) TerGrid%coord%zone
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'SWIMsetNestDomain'
      error%Message = 'Error reading UTM zone for nest grid'
      GOTO 9999
    END IF
    i = 3

  CASE( 'CART','CARTESIAN' )
    IF( Prj%coord /= I_CARTESIAN )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMsetNestDomain'
      error%Message = 'Invalid coordinate type for nest grid'
      error%Inform  = 'Cartesian allowed only with Cartesian project'
      GOTO 9999
    END IF
    i = 2

  CASE( 'RES','RESOLUTION' )
    lRes = .TRUE.
    READ(c_arg(3),*,IOSTAT=ios ) res
    IF( ios /= 0 .OR. res <= 0. )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMsetNestDomain'
      error%Message = 'Invalid resolution for assimilation grid'
      GOTO 9999
    END IF

  CASE DEFAULT
END SELECT

!------ Check arguments for defining nest:
!       Single number defines grid resolution only;
!       Otherwise, 6 numbers (xmin,dx,nx,...)

IF( .NOT.lRes .AND. n_arg < i+6 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMsetNestDomain'
  error%Message = 'Not enough parameters to define nest grid'
  error%Inform  = 'Required input: xmin,dx,nx,ymin,dy,ny'
  GOTO 9999
END IF

!------ Point to parent field

fldp => field(ifldp)

!------ Setup output grid

IF( lRes )THEN

  grdp => fldp%grid

  TerGrid%coord%type = grdp%coord%type

  TerGrid%Xmin = grdp%Xmin;
  TerGrid%dX   = MIN(grdp%dX,res)
  TerGrid%nX   = NINT(1.+(grdp%nX -1)*grdp%dX/TerGrid%dX)

  TerGrid%Ymin = grdp%Ymin;
  TerGrid%dY   = MIN(grdp%dY,res)
  TerGrid%nY   = NINT(1.+(grdp%nY -1)*grdp%dY/TerGrid%dY)

ELSE

  IF( n_arg < i+6 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMsetNestTerrain'
    error%Message = 'Not enough parameters to define nest grid'
    error%Inform  = 'Required input: xmin,dx,nx,ymin,dy,ny'
    GOTO 9999
  END IF

  READ(c_arg(i+1),*,IOSTAT=ios ) TerGrid%Xmin
  READ(c_arg(i+2),*,IOSTAT=ios ) TerGrid%dX
  READ(c_arg(i+3),*,IOSTAT=ios ) TerGrid%nX

  READ(c_arg(i+4),*,IOSTAT=ios ) TerGrid%Ymin
  READ(c_arg(i+5),*,IOSTAT=ios ) TerGrid%dY
  READ(c_arg(i+6),*,IOSTAT=ios ) TerGrid%nY

END IF

TerGrid%nXY = TerGrid%nX*TerGrid%nY

TerGrid%type = 0

!------ Allocate terrain and landuse 2d fields
!       (which will be defined in SetupNestField)

irv = SWIMallocTerrain( TerGrid )
IF( irv /= SWIMsuccess )GOTO 9999

src%type = IBSET(src%type,GSB_NEST)
src%unit = ifldp

!------ Done

SWIMsetNestDomain = SWIMresult

9999 CONTINUE

RETURN
END

