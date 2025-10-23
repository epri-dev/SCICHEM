!*******************************************************************************
!                DrawFieldFunction
!*******************************************************************************
INTEGER FUNCTION DrawFieldFunction( UserID,grdI,Field,PlotType,contourHead,contourList,GUIdraw, &
                                    UserFill,UserDraw )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagstr_fd
USE poparea_fi
USE AreaMode_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                      INTENT( IN )    :: UserID      !User ID tag
INTEGER,                                                      INTENT( IN )    :: grdI        !SAG grid ID
TYPE( SCIPPlotFieldT ),                                       INTENT( IN )    :: Field       !Field definition
TYPE( SCIPPlotTypeT ),                                        INTENT( IN )    :: PlotType    !Plot definition
TYPE( SCIPContourHeaderT ),                                   INTENT( IN )    :: contourHead !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                              INTENT( INOUT ) :: contourList !Contour array
TYPE( ARAPDrawT ),                                            INTENT( IN )    :: GUIdraw     !Draw instructions
INTEGER, EXTERNAL                                                             :: UserFill    !User Fill function
INTEGER, EXTERNAL                                                             :: UserDraw    !User Draw function

!==============================================================================
! Local variables
!==============================================================================
INTEGER                             :: irv
INTEGER                             :: currentState

INTERFACE
  SUBROUTINE DrawField( grdID,Field,PlotType,contourHead,contourList,GUIdraw,UserFill,UserDraw )
    USE field_fd
    USE contourlist_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    TYPE( ARAPDrawT ),                         INTENT( IN    ) :: GUIdraw      !Draw instructions
    INTEGER, EXTERNAL                                          :: UserFill     !Address of User supplied
                                                                               !fill routine passed by value
    INTEGER, EXTERNAL                                          :: UserDraw     !Address of User supplied
                                                                               !draw routine passed by value
  END SUBROUTINE DrawField
END INTERFACE

!==== initialize

DrawFieldFunction = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== draw filed

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL DrawField( grdI,Field,PlotType,contourHead,contourList, &
                       GUIdraw,UserFill,UserDraw )

  IF( nError == NO_ERROR )DrawFieldFunction = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                DrawField
!*******************************************************************************
SUBROUTINE DrawField( grdI,Field,PlotType,contourHead,contourList,GUIdraw, &
                                UserFill,UserDraw )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagstr_fd
USE constants_fd
USE poparea_fi
USE AreaMode_fd
USE abort
USE contour_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                      INTENT( IN )    :: grdI        !SAG grid ID
TYPE( SCIPPlotFieldT ),                                       INTENT( IN )    :: Field       !Field definition
TYPE( SCIPPlotTypeT ),                                        INTENT( IN )    :: PlotType    !Plot definition
TYPE( SCIPContourHeaderT ),                                   INTENT( IN )    :: contourHead !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                              INTENT( INOUT ) :: contourList !Contour array
TYPE( ARAPDrawT ),                                            INTENT( IN )    :: GUIdraw     !Draw instructions
INTEGER, EXTERNAL                                                             :: UserFill    !User Fill function
INTEGER, EXTERNAL                                                             :: UserDraw    !User Draw function

!==============================================================================
! Local variables
!==============================================================================
LOGICAL                             :: lrestore
LOGICAL                             :: lclose
LOGICAL                             :: log_interp
LOGICAL                             :: lPopCell
LOGICAL                             :: lPopTri
LOGICAL                             :: FillContour
LOGICAL                             :: Fill_Lo
LOGICAL                             :: Fill_Hi
LOGICAL                             :: DrawContour
INTEGER                             :: irv
INTEGER                             :: nfld
INTEGER                             :: plotfld
INTEGER                             :: i
INTEGER                             :: j
INTEGER                             :: ios
INTEGER,  DIMENSION(3)              :: ifld
TYPE( SAGgrid_str ), POINTER        :: grd
REAL(8)                             :: swap
TYPE( SCIPContourElementList )      :: contour

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_BottomFunctionID
INTEGER, EXTERNAL :: SAG_DrawFieldID
INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: PlotFunction
INTEGER, EXTERNAL :: CellPopArea
INTEGER, EXTERNAL :: PolygonPopArea
INTEGER, EXTERNAL :: NullPopArea

INTERFACE
  SUBROUTINE SAG_InitDrawField( grdI,        & !SAG grid ID
                                ifld,        & !Field number
                                log_interp,  & !logarithmic interolation
                                nlev,        & !Number of contour levels
                                level,       & !Array of contour levels
                                istart,      & !Start contour level
                                istop,       & !Stop  contour level
                                FillContour, & !Fill flag
                                fill_lo,     & !Fill below lowest level flag
                                fill_hi,     & !Fill above highest level flag
                                DrawContour, & !Draw flag
                                lclose,      & !Close contours flag
                                lPopCell,    & !Pop/Area by cell
                                lPopTri,     & !Pop/Area by triangles
                                lDoPop )       !Pop/Area flag
    INTEGER,                         INTENT( IN ) :: grdI        !SAG grid ID
    INTEGER,                         INTENT( IN ) :: ifld        !Field number
    LOGICAL,                         INTENT( IN ) :: log_interp  !logarithmic interolation
    INTEGER,                         INTENT( IN ) :: nlev        !Number of contour levels
    REAL,   DIMENSION(nlev), TARGET, INTENT( IN ) :: level       !Array of contour levels
    INTEGER,                         INTENT( IN ) :: istart      !Start contour level
    INTEGER,                         INTENT( IN ) :: istop       !Stop  contour level
    LOGICAL,                         INTENT( IN ) :: FillContour !Fill flag
    LOGICAL,                         INTENT( IN ) :: fill_lo     !Fill below lowest level flag
    LOGICAL,                         INTENT( IN ) :: fill_hi     !Fill above highest level flag
    LOGICAL,                         INTENT( IN ) :: DrawContour !Draw flag
    LOGICAL,                         INTENT( IN ) :: lclose      !Close contours flag
    LOGICAL,                         INTENT( IN ) :: lPopCell    !Pop/Area by Cell
    LOGICAL,                         INTENT( IN ) :: lPopTri     !Pop/Area by Tri
    LOGICAL, OPTIONAL,               INTENT( IN ) :: lDoPop      !Pop/Area flag
  END SUBROUTINE SAG_InitDrawField
END INTERFACE

!==============================================================================
! Initialize error
!==============================================================================
irv = SAG_InitError()

contour%listHdr =  contourHead
contour%listPtr => contourList

IF( Aborted() )GOTO 9999

!==============================================================================
! Turn on SAG Special value feature
!==============================================================================
irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

!==============================================================================
! Set number of contour levels
!==============================================================================
nlev = contour%listHdr%number
IF( nlev <= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'DrawField'
  eMessage = 'Error - No contours specified'
  GOTO 9999
END IF

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - DrawField'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,*)'grdI = ',grdI
!CALL DumpField(1,Field,NULL)
!CALL DumpPlotType(PlotType)
!CALL DumpContour(contourHead%number,contourList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF
!==============================================================================
! Allocate space for contour levels
!==============================================================================
ALLOCATE( level(nlev),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'DrawField'
  eMessage = 'Error allocating contour array'
  GOTO 9999
END IF

!==============================================================================
! Set contour levels
!==============================================================================
IF( ASSOCIATED(contour%listPtr) )THEN
  DO i = 1,contour%listHdr%number
    level(i) = contour%listPtr(i)%contour    !/contour%listHdr%scale
  END DO
ELSE
  nError   = UK_ERROR
  eRoutine = 'DrawField'
  eMessage = 'Error - contour array not associated'
  GOTO 9999
END IF

IF( Aborted() )GOTO 9999

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'DrawField'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

plotfld = 3       !Results of PlotFunction are here

IF( grd%type == SAG_GRID_NONE )THEN
  PlotFunc_small  = -1.E36  !set to some parameter?
ELSE
  PlotFunc_small  = 1.E-30  !set to some parameter?
END IF
PlotFunc_spv = HP_SPV

!==============================================================================
! Check if plot field has already been created
!==============================================================================

IF( grd%PlotType /= PlotType%type .OR. grd%PlotData /= PlotType%data )THEN

!==============================================================================
! Set function parameters
!==============================================================================
  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )

      PlotFunc_nfun =  PLOT_FUNCTION_NULL
      PlotFunc_data =  0.0
      log_interp    =  Field%interpType == SCIPoff

    CASE( HP_PROB )

      PlotFunc_nfun =  PLOT_FUNCTION_PROB_CLIP
      PlotFunc_data =  PlotType%data
      log_interp    = .FALSE.

    CASE( HP_EXCEED )

      PlotFunc_nfun =  PLOT_FUNCTION_ICPROB_CLIP
      PlotFunc_data =  PlotType%data
      log_interp    =  Field%interpType == SCIPoff

    CASE( HP_VARIANCE )

      PlotFunc_nfun =  PLOT_FUNCTION_SWITCH
      PlotFunc_data =  0.0
      log_interp    =  Field%interpType == SCIPoff

    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'DrawField'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9999

  END SELECT

  PlotFunc_nComp = grd%pushtype

!==============================================================================
! Set auxiliary plot data
!==============================================================================
  IF( grd%naux <= 0 )THEN
    AuxType = 0
  ELSE
    AuxType = grd%ipblk(1)%type
    AuxData => grd%aux(1)
  END IF

!==============================================================================
! Compute Plot function
!==============================================================================
  nfld     = 3
  ifld(1)  = 1         !mean
  ifld(2)  = 2         !variance
  ifld(3)  = plotfld   !PlotField
  lrestore = .FALSE.

  CALL InitAbortedCount()

  irv = SAG_BottomFunctionID( grdI,PlotFunction,nfld,ifld,lrestore,.TRUE. )
  IF( irv /= SAG_OK )THEN
    IF( nError /= AB_ERROR )THEN
      nError   = UK_ERROR
      eRoutine = 'DrawField'
      eMessage = 'Error computing plot function'
    END IF
    GOTO 9999
  END IF

  IF( Aborted() )GOTO 9999

  CALL CheckSmoothCN2( grdI )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )
      log_interp =  Field%interpType == SCIPoff

    CASE( HP_PROB )
      log_interp = .FALSE.

    CASE( HP_EXCEED )
      log_interp =  Field%interpType == SCIPoff

    CASE( HP_VARIANCE )
      log_interp =  Field%interpType == SCIPoff

  END SELECT

END IF

FillContour = GUIdraw%fillContour == SCIPTrue
Fill_Lo     = GUIdraw%fill_Lo     == SCIPTrue
Fill_Hi     = GUIdraw%fill_Hi     == SCIPTrue
DrawContour = GUIdraw%drawContour == SCIPTrue

IF( Aborted() )GOTO 9999

!==============================================================================
! Check for Pop/Area calculations
!==============================================================================
IF( PlotType%areaMode /= HP_OFF )THEN

!==============================================================================
! Allocate space for Pop/Area results
!==============================================================================
  ALLOCATE( PopArea(nlev),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'DrawField'
    eMessage = 'Error allocating Population/Area array'
    GOTO 9999
  END IF

!==============================================================================
! Initialize Pop/Area results
!==============================================================================
  DO i = 1,nlev
    PopArea(i) = 0.0d0
  END DO

  lPopCell = BTEST(PlotType%areaMode,POP_BEXPECT)
  lPopTri  = .NOT.lPopCell

ELSE

  lPopTri  = .FALSE.
  lPopCell = .FALSE.

END IF

IF( lPopCell .AND. ( PlotType%type /= HP_MEAN .OR. grd%ipnam(2) == 'None' ) )THEN
  nError   = UK_ERROR
  eRoutine = 'DrawField'
  eMessage = 'Invalid Population/PlotType combination'
  eInform  = 'Expected Area/Population only valid with Mean plots with Variance available'
  eAction  = 'Change the Area/Population mode to "Within contour".'
  GOTO 9999
END IF

!==============================================================================
! Initialize POPArea parameters
!==============================================================================

popScale = 100.

griddef%doPop = .NOT.BTEST(PlotType%areaMode,POP_BAREA)

gridDef%xOrigin = DBLE(grd%xmin)
gridDef%yOrigin = DBLE(grd%ymin)
gridDef%dx      = DBLE(grd%dx)
gridDef%dy      = DBLE(grd%dy)

gridDef%coord = Field%coordinate%mode
IF( gridDef%coord == I_UTM )THEN

  gridDef%zone = Field%coordinate%UTMzone

ELSE IF( gridDef%coord == I_CARTESIAN )THEN

  gridDef%reference%x   = DBLE(Field%coordinate%reference%x)
  gridDef%reference%y   = DBLE(Field%coordinate%reference%y)
  gridDef%reference%lon = DBLE(Field%coordinate%reference%lon)
  gridDef%reference%lat = DBLE(Field%coordinate%reference%lat)
  gridDef%yfac = 1000.D0*DBLE(SPHFACR)                      !(Km to Degrees)
  gridDef%xfac = gridDef%yfac/COSD(gridDef%reference%lat)   !(Km to Degrees)

END IF

!==============================================================================
! Initialize SAG Draw Field
!==============================================================================
lclose = .FALSE. !Leave open contours

IF( Aborted() )GOTO 9999

!==============================================================================
! Initialize Draw
!==============================================================================
CALL SAG_InitDrawField( grdI,          & !SAG grid ID
                        plotfld,       & !Field number
                        log_interp,    & !logarithmic interolation
                        nlev,          & !Number of contour levels
                        level,         & !Array of contour levels
                        1,             & !Start contour level
                        nlev,          & !Stop  contour level
                        FillContour,   & !Fill flag
                        Fill_Lo,       & !Fill below lowest level flag
                        Fill_Hi,       & !Fill above highest level flag
                        DrawContour,   & !Draw flag
                        lclose,        & !Close contours flag
                        lPopCell,      & !Pop/area on Cells
                        lPopTri,       & !Pop/Area on Triangles
                 lDoPop=griddef%doPop )          !Pop/Area flag (used to determine thread safety

IF( Aborted() )GOTO 9998

!==============================================================================
! Draw Field
!==============================================================================
IF( lPopTri )THEN
  irv = SAG_DrawFieldID( grdI,UserFill,UserDraw,PolygonPopArea )
ELSE IF( lPopCell )THEN
  irv = SAG_DrawFieldID( grdI,UserFill,UserDraw,CellPopArea )
ELSE
  irv = SAG_DrawFieldID( grdI,UserFill,UserDraw,NullPopArea )
END IF
IF( irv /= SAG_OK )THEN
  IF( nError /= AB_ERROR )THEN
    nError   = UK_ERROR
    eRoutine = 'DrawField'
    eMessage = 'Error drawing SAG field'
  END IF
  GOTO 9998
END IF

IF( Aborted() )GOTO 9998

!==============================================================================
! Move Pop/Area calculations to output contour list
!==============================================================================
IF( PlotType%areaMode /= HP_OFF )THEN

!==============================================================================
! Areal Calculation - Compute totals
!==============================================================================
  IF( lPopTri )THEN
    IF( .NOT.BTEST(contour%listHdr%drawMode,BLOHI_CONTOUR) )THEN
      DO j = 2,nlev
        PopArea(j) = PopArea(j) + PopArea(j-1)
      END DO
    END IF
    DO i = 1,nlev/2
      swap              = PopArea(i)
      PopArea(i)        = PopArea(nlev-i+1)
      PopArea(nlev-i+1) = swap
    END DO
  END IF

!==============================================================================
! Area Calculation
!==============================================================================
  IF( BTEST(PlotType%areaMode,HP_BAREA) )THEN
    DO i = 1,nlev
      contour%listPtr(i)%population = 0.0
      contour%listPtr(i)%area       = SNGL(PopArea(i))
    END DO

!==============================================================================
! Population Calculation
!==============================================================================
  ELSE
    DO i = 1,nlev
      contour%listPtr(i)%population = SNGL(PopArea(i))
      contour%listPtr(i)%area       = 0.0
    END DO
  END IF

ELSE

!==============================================================================
! Clear - No Pop/Area calculations
!==============================================================================
  DO i = 1,contour%listHdr%number
    contour%listPtr(i)%population = 0.0
    contour%listPtr(i)%area       = 0.0
  END DO

END IF

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Output Dump - DrawField'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpContour(contourHead%number,contourList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF
!==============================================================================
! Free the Draw Field
!==============================================================================
9998 CONTINUE
CALL SAG_FreeDrawField()

!==============================================================================
! Clean up - Deallocate contour and PopArea work space
!==============================================================================

9999 CONTINUE

IF( ALLOCATED(level)   )DEALLOCATE( level,  STAT=ios )
IF( ALLOCATED(PopArea) )DEALLOCATE( PopArea,STAT=ios )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                GetFieldMinMaxFunction
!*******************************************************************************
INTEGER FUNCTION GetFieldMinMaxFunction( UserID,grdI,PlotType, &
                                         dmin,dmax,fmin,fmax,dmn,dmx )

USE field_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN ) :: UserID    !User ID tag
INTEGER,               INTENT( IN ) :: grdI      !SAG grid ID
TYPE( SCIPPlotTypeT ), INTENT( IN ) :: PlotType  !Plot Definition
REAL,                  INTENT( OUT ):: dmin,dmax !Mean Min/Max
REAL,                  INTENT( OUT ):: fmin,fmax !Variance Min/Max
REAL,                  INTENT( OUT ):: dmn,dmx   !Plot Field Min/Max

!==============================================================================
! Local Variables
!==============================================================================
INTEGER irv
INTEGER currentState

!==== initialize

GetFieldMinMaxFunction = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Get field min/max values

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL GetFieldMinMax( grdI,PlotType,dmin,dmax,fmin,fmax,dmn,dmx )

  IF( nError == NO_ERROR )GetFieldMinMaxFunction = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                GetFieldMinMax
!*******************************************************************************
SUBROUTINE GetFieldMinMax( grdI,PlotType,dmin,dmax,fmin,fmax,dmn,dmx )

USE field_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN ) :: grdI      !SAG grid ID
TYPE( SCIPPlotTypeT ), INTENT( IN ) :: PlotType  !Plot Definition
REAL,                  INTENT( OUT ):: dmin,dmax !Mean Min/Max
REAL,                  INTENT( OUT ):: fmin,fmax !Variance Min/Max
REAL,                  INTENT( OUT ):: dmn,dmx   !Plot Field Min/Max

!==============================================================================
! Local Variables
!==============================================================================
INTEGER                      :: irv
INTEGER                      :: nfld
INTEGER                      :: jfld
INTEGER, DIMENSION(2)        :: ifld
REAL,    DIMENSION(4)        :: fldmx
LOGICAL                      :: lrestore
TYPE( SAGgrid_str ), POINTER :: grd

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_BottomMinMaxID
INTEGER, EXTERNAL :: SAG_BottomFunctionID
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: PlotFunction

!==============================================================================
! Initialize error
!==============================================================================
irv = SAG_InitError()

IF( Aborted() )GOTO 9999

!==============================================================================
! Turn on SAG Special value feature
!==============================================================================
irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'GetFieldMinMax'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

!==============================================================================
! Mean/Variance Min/Max
!==============================================================================
lrestore = .TRUE.

DO jfld = 1,2
  IF( Aborted() )GOTO 9999
  irv = SAG_BottomMinMaxID( grdI,jfld,lrestore,fldmx(2*jfld-1) )
  IF( irv /= SAG_OK )THEN
    IF( nError /= AB_ERROR )THEN
      nError   = UK_ERROR
      eRoutine = 'GetFieldMinMax'
      eMessage = 'Error computing Mean/Variance Min/Max'
    END IF
    GOTO 9999
  END IF
END DO

dmin = fldmx(1)
dmax = fldmx(2)
fmin = fldmx(3)
fmax = fldmx(4)

IF( Aborted() )GOTO 9999

!==============================================================================
! Check if plot field has already been created
!==============================================================================

IF( grd%PlotType /= PlotType%type .OR. grd%PlotData /= PlotType%data )THEN

!==============================================================================
! Set function parameters
!==============================================================================
  SELECT CASE( PlotType%type )

    CASE( HP_MEAN )

      PlotFunc_nfun = PLOT_FUNCTION_NULL
      PlotFunc_data = 0.0

    CASE( HP_PROB )

      PlotFunc_nfun = PLOT_FUNCTION_PROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_EXCEED )

      PlotFunc_nfun = PLOT_FUNCTION_ICPROB_CLIP
      PlotFunc_data = PlotType%data

    CASE( HP_VARIANCE )

      PlotFunc_nfun =  PLOT_FUNCTION_SWITCH
      PlotFunc_data =  0.0

    CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'GetFieldMinMax'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9999

  END SELECT

  PlotFunc_nComp = grd%pushtype

  IF( grd%type == SAG_GRID_NONE )THEN
    PlotFunc_small = -1.E36  !set to some parameter?
  ELSE
    PlotFunc_small = 1.E-30  !set to some parameter?
  END IF
  PlotFunc_spv = HP_SPV

  IF( Aborted() )GOTO 9999

!==============================================================================
! Set auxiliary plot data
!==============================================================================
  IF( grd%naux <= 0 )THEN
    AuxType = 0
  ELSE
    AuxType = grd%ipblk(1)%type
    AuxData => grd%aux(1)
  END IF

!==============================================================================
! Compute Plot function
!==============================================================================
  nfld     = 2
  ifld(1)  = 1         !mean
  ifld(2)  = 2         !variance

  CALL InitAbortedCount()

  irv = SAG_BottomFunctionID( grdI,PlotFunction,nfld,ifld,lrestore,.TRUE. )
  IF( irv /= SAG_OK )THEN
    IF( nError /= AB_ERROR )THEN
      nError   = UK_ERROR
      eRoutine = 'GetFieldMinMax'
      eMessage = 'Error computing plot function'
    END IF
    GOTO 9999
  END IF

  IF( Aborted() )GOTO 9999

  CALL CheckSmoothCN2( grdI )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( grd%PlotType /= -1 )grd%PlotType = PlotType%type
  grd%PlotData = PlotType%data

END IF

!==============================================================================
! Plot Field Min/Max
!==============================================================================
jfld = 3  !Results of PlotFunction are here

irv = SAG_BottomMinMaxID( grdI,jfld,lrestore,fldmx )
IF( irv /= SAG_OK )THEN
  IF( nError /= AB_ERROR )THEN
    nError   = UK_ERROR
    eRoutine = 'GetFieldMinMax'
    eMessage = 'Error computing plot function min/max'
  END IF
  GOTO 9999
END IF

dmn = fldmx(1)
dmx = fldmx(2)

IF( grd%PlotType == -1 )dmx = 10.0*dmn


9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!               PlotFunction
!*******************************************************************************
INTEGER FUNCTION PlotFunction( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
REAL, POINTER, DIMENSION(:) :: dat   !Pointer to Grid data
INTEGER                     :: mxgrd !Data field size
TYPE ( SAGcell_str )        :: p0    !Cell descriptor

!==============================================================================
! Local variables
!==============================================================================
INTEGER  im
INTEGER  iv
INTEGER  io
INTEGER  im2, iv2
REAL     m1, m2, v1, v2

!==============================================================================
! Function calls
!==============================================================================
REAL, EXTERNAL :: xfun
REAL, EXTERNAL :: xfun2

!==============================================================================
! Initialize return value
!==============================================================================
PlotFunction = SAG_ERROR

IF( AbortedCount() )GOTO 9999

!==============================================================================
! Define data field pointers
!==============================================================================
im = p0%id            !index of mean field at cell id
iv = im + mxgrd       !variance
io = im + 2*mxgrd     !output

!==============================================================================
! Compute output
!==============================================================================
IF( AuxType == 0 )THEN
  IF( PlotFunc_nComp == 2 )THEN
    im2 = im + 3*mxgrd
    iv2 = iv + 3*mxgrd
    IF( dat(im2) > 0.0 )THEN
      m1 = dat(im)
      v1 = dat(iv)
      m2 = dat(im2)
      v2 = dat(iv2)
      dat(io) = xfun2( m1,m2,        & !Mean
                       v1,v2,        & !Variance
                       PlotFunc_nfun,  & !Functin ID
                       PlotFunc_data,  & !Function input
                       PlotFunc_small, & !Small values
                       PlotFunc_spv    ) !Special value
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      dat(io) = xfun( dat(im),        & !Mean
                      dat(iv),        & !Variance
                      PlotFunc_nfun,  & !Functin ID
                      PlotFunc_data,  & !Function input
                      PlotFunc_small, & !Small values
                      PlotFunc_spv    ) !Special value
    END IF
  ELSE
    dat(io) = xfun( dat(im),        & !Mean
                    dat(iv),        & !Variance
                    PlotFunc_nfun,  & !Functin ID
                    PlotFunc_data,  & !Function input
                    PlotFunc_small, & !Small values
                    PlotFunc_spv    ) !Special value
  END IF
END IF
!==============================================================================
! Set return value
!==============================================================================
PlotFunction = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!               PlotFunction
!*******************************************************************************
INTEGER FUNCTION PlotFunctionPointValue( dat,mxgrd,p0 )

USE sagdef_fd
USE cellstr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi

IMPLICIT NONE

!==============================================================================
! This function was added to addres the GetPointValue need to have multiple
! values at a point.  Using the normal PlotFunction but just simply using a
! different INTERFACE (SAGcell_str vs. getp_cell_str) seemed to work but didn't
! seem like a good practice to me. (SFP)
!==============================================================================
! Function Arguments
!==============================================================================
REAL, POINTER, DIMENSION(:) :: dat   !Pointer to Grid data
INTEGER                     :: mxgrd !Data field size
TYPE ( getp_cell_str )      :: p0    !Cell descriptor

!==============================================================================
! Local variables
!==============================================================================
INTEGER  im, iv, im2, iv2, io
REAL     m1, m2, v1, v2

!==============================================================================
! Function calls
!==============================================================================
REAL, EXTERNAL :: xfun, xfun2

!==============================================================================
! Initialize return value
!==============================================================================
PlotFunctionPointValue = SAG_ERROR

!==============================================================================
! Define data field pointers
!==============================================================================
im = p0%id            !index of mean field at cell id
iv = im + mxgrd       !variance
io = im + 2*mxgrd     !output

!==============================================================================
! Compute output
!==============================================================================
IF( AuxType == 0 )THEN
  IF( PlotFunc_nComp == 2 )THEN
    im2 = im + 3*mxgrd
    iv2 = iv + 3*mxgrd
    IF( dat(im2) > 0.0 )THEN
      m1 = dat(im)
      v1 = dat(iv)
      m2 = dat(im2)
      v2 = dat(iv2)
      dat(io) = xfun2( m1,m2,        & !Mean
                       v1,v2,        & !Variance
                       PlotFunc_nfun,  & !Functin ID
                       PlotFunc_data,  & !Function input
                       PlotFunc_small, & !Small values
                       PlotFunc_spv    ) !Special value
      IF( nError /= NO_ERROR )GOTO 9999
    ELSE
      dat(io) = xfun( dat(im),        & !Mean
                      dat(iv),        & !Variance
                      PlotFunc_nfun,  & !Functin ID
                      PlotFunc_data,  & !Function input
                      PlotFunc_small, & !Small values
                      PlotFunc_spv    ) !Special value
    END IF
  ELSE
    dat(io) = xfun( dat(im),        & !Mean
                    dat(iv),        & !Variance
                    PlotFunc_nfun,  & !Functin ID
                    PlotFunc_data,  & !Function input
                    PlotFunc_small, & !Small values
                    PlotFunc_spv    ) !Special value
  END IF
END IF
!==============================================================================
! Set return value
!==============================================================================
p0%f(3) = dat(io)
PlotFunctionPointValue = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!             xfun
!*******************************************************************************
REAL FUNCTION xfun( v1,v2,ifun,val,small,spv )

USE PlotFunc_fd

IMPLICIT NONE

REAL, PARAMETER :: GAMMA_MIN = 1.E-4
REAL, PARAMETER :: SQR2      = 1.4142136

!==============================================================================
! Function Arguments
!==============================================================================
REAL,    INTENT( IN ) :: v1     !mean
REAL,    INTENT( IN ) :: v2     !variance
INTEGER, INTENT( IN ) :: ifun   !function id
REAL,    INTENT( IN ) :: val    !numerical input value
REAL,    INTENT( IN ) :: small  !small number
REAL,    INTENT( IN ) :: spv    !special value

!==============================================================================
! Local variables
!==============================================================================
REAL sig
REAL soc
REAL prb
REAL arg
REAL gbar
REAL sigg
REAL gamma
REAL exv

REAL, EXTERNAL :: erfc
REAL, EXTERNAL :: erfci

!==============================================================================
! Check for Special Values
!==============================================================================
IF( v1 == spv .OR. v2 == spv )THEN

  xfun = spv

ELSE

!==============================================================================
! Compute function
!==============================================================================
  SELECT CASE( ifun )

!------ No function

    CASE( PLOT_FUNCTION_NULL )

      xfun = MAX(v1,small)

!------ Add

    CASE( PLOT_FUNCTION_ADD )

      xfun = MAX( v1 + v2 , small )

!------ Multiply

    CASE( PLOT_FUNCTION_MULT )

      xfun = MAX( v1 * v2 , small )

!------ Divide

    CASE( PLOT_FUNCTION_DIV )

      IF( v2 > small )THEN
        xfun = MAX( v1/v2 , small )
      ELSE
        xfun = spv
      END IF

!------ Invert

    CASE( PLOT_FUNCTION_INVERT )

      IF( v1 <= small )THEN
        xfun = spv
      ELSE
        xfun = MAX( 1./v1 , small )
      END IF

!------ Square root

    CASE( PLOT_FUNCTION_SQRT )

      IF( v1 >= 0. )THEN
        xfun = MAX( SQRT(v1),small )
      ELSE
        xfun = spv
      END IF

!------ Sigma/mean

    CASE( PLOT_FUNCTION_CCOC )

      sig = SQRT( MAX(v2,0.0) )
      IF( ABS(v1) > small )THEN
        xfun = MAX( 0.0,sig/v1 )
      ELSE
        xfun = spv
      END IF

!------ Probability of Exceeding - Clipped-normal

    CASE( PLOT_FUNCTION_PROB_CLIP )

      sig = SQRT( MAX(v2,0.0) )
      IF( v1 > small )THEN
        soc = MAX( 0.001,sig/v1 )
        CALL clnpar( soc,gbar,sigg )
        arg = (val/v1 - gbar)/sigg
        IF( arg > 1.E6 )THEN
          prb = 0.
        ELSE IF( arg < -1.E6 )THEN
          prb = 1.
        ELSE
          prb = 0.5*erfc( arg/SQR2 )
        END IF
      ELSE
        prb = 0.0
      END IF
      xfun = 100.*MAX( prb,small )

!------ Probability of Exceeding - lognormal

    CASE( PLOT_FUNCTION_PROB_LOGN )

      sig = SQRT( MAX(v2,0.0) )
      IF( v1 > small )THEN
        soc = sig/v1
        gbar = -0.5*LOG(1.+soc**2)
        sigg = SQRT(-2.*gbar)
        arg = (LOG(val/v1) - gbar)/sigg
        IF( arg > 1.E6 )THEN
          prb = 0.
        ELSE IF( arg < -1.E6 )THEN
          prb = 1.
        ELSE
          prb = 0.5*erfc( arg/SQR2 )
        END IF
      ELSE
        prb = 0.0
      END IF
      xfun = 100.*MAX( prb,small )

!------ Conditional Probability of Exceeding - Clipped-normal

    CASE( PLOT_FUNCTION_CPROB_CLIP )

      sig = SQRT( MAX(v2,0.0) )
      IF( v1 > small )THEN
        soc = MAX( 0.001,sig/v1 )
        CALL clnpar( soc,gbar,sigg )
        arg = (val/v1 - gbar)/sigg
        IF( arg > 1.E6 )THEN
          prb = 0.
        ELSE IF( arg < -1.E6 )THEN
          prb = 1.
        ELSE
          prb = 0.5*erfc( arg/SQR2 )
          gamma = MAX( 0.5*erfc( -(gbar/sigg)/SQR2),GAMMA_MIN )
          IF( gamma > 0.0 )prb = prb/gamma
        END IF
      ELSE
        prb = 0.0
      END IF
      xfun = 100.*MAX( prb,small )

!------ Inverse Conditional Probability of Exceeding - Clipped Normal

    CASE( PLOT_FUNCTION_ICPROB_CLIP )

      sig = SQRT( MAX(v2,0.0) )
      IF( v1 > small )THEN
        soc = MAX( 0.001,sig/v1 )
        CALL clnpar( soc,gbar,sigg )
        gamma = MAX( 0.5*erfc(-(gbar/sigg)/SQR2),GAMMA_MIN )
        exv   = v1*(erfci( 0.01*val*gamma )*sigg + gbar)
      ELSE
        exv = 0.0
      END IF
      xfun = MAX( exv,small )

!------ Switch function

    CASE( PLOT_FUNCTION_SWITCH )

      xfun = MAX(v2,small)

!------ Switch function

    CASE( PLOT_FUNCTION_SPEED )

      xfun = SQRT(v1*v1+v2*v2)

!------ Invalid function

    CASE DEFAULT

      xfun = spv

  END SELECT

END IF

RETURN
END
!*******************************************************************************
!             xfun2
!*******************************************************************************
REAL FUNCTION xfun2( m1,m2,v1,v2,ifun,val,small,spv )

USE PlotFunc_fd
USE error_fi

IMPLICIT NONE

REAL,   PARAMETER :: GAMMA_MIN     = 1.E-4
REAL,   PARAMETER :: SQR2          = 1.4142136
LOGICAL,PARAMETER :: INVERT        = .TRUE.
LOGICAL,PARAMETER :: NO_INVERT     = .FALSE.
LOGICAL,PARAMETER :: CONDITIONAL   = .TRUE.
LOGICAL,PARAMETER :: NOT_CONDITION = .FALSE.

!==============================================================================
! Function Arguments
!==============================================================================
REAL,    INTENT( IN    ) :: m1, m2     !mean
REAL,    INTENT( IN    ) :: v1, v2     !variance
INTEGER, INTENT( IN    ) :: ifun       !function id
REAL,    INTENT( INOUT ) :: val        !numerical input value
REAL,    INTENT( IN    ) :: small      !small number
REAL,    INTENT( IN    ) :: spv        !special value

!==============================================================================
! Local variables
!==============================================================================
REAL    exv, prb, x1, x2, sig, soc, gbar, sigg, arg
REAL    ratPDF, test, p1, p2, test1, test2, gamma
LOGICAL PDF1, PDF2

REAL, DIMENSION(2) :: xmean, xsig, qq

REAL, EXTERNAL :: erfc, erfci, ProbExceed2

!==============================================================================
! Initialize return value in case of error
!==============================================================================
xfun2 = 0.0

!==============================================================================
! Check for Special Values
!==============================================================================
IF( m1 == spv .OR. v1 == spv )THEN

  xfun2 = spv

ELSE

!==============================================================================
! Compute function
!==============================================================================
  x1 = m1 + m2
  x2 = v1 + v2

  SELECT CASE( ifun )

!------ No function

    CASE( PLOT_FUNCTION_NULL )

      xfun2 = MAX( x1,small )

!------ Add

    CASE( PLOT_FUNCTION_ADD )

      xfun2 = MAX( x1 + x2,small )

!------ Multiply

    CASE( PLOT_FUNCTION_MULT )

      xfun2 = MAX( x1 * x2,small )

!------ Divide

    CASE( PLOT_FUNCTION_DIV )

      IF( x2 > small )THEN
        xfun2 = MAX( x1/x2,small )
      ELSE
        xfun2 = spv
      END IF

!------ Invert

    CASE( PLOT_FUNCTION_INVERT )

      IF( x1 <= small )THEN
        xfun2 = spv
      ELSE
        xfun2 = MAX( 1.0/x1,small )
      END IF

!------ Square root

    CASE( PLOT_FUNCTION_SQRT )

      IF( x1 >= 0.0 )THEN
        xfun2 = MAX( SQRT(x1),small )
      ELSE
        xfun2 = spv
      END IF

!------ Sigma/mean

    CASE( PLOT_FUNCTION_CCOC )

      sig = SQRT( MAX(x2,0.0) )
      IF( ABS(x1) > small )THEN
        xfun2 = MAX( 0.0,sig/x1 )
      ELSE
        xfun2 = spv
      END IF

!------ Probability of Exceeding - lognormal

    CASE( PLOT_FUNCTION_PROB_LOGN )

      sig = SQRT( MAX(x2,0.0) )
      IF( x1 > small )THEN
        soc = sig/x1
        gbar = -0.5*LOG(1.+soc**2)
        sigg = SQRT(-2.*gbar)
        arg = (LOG(val/x1) - gbar)/sigg
        IF( arg > 1.E6 )THEN
          prb = 0.0
        ELSE IF( arg < -1.E6 )THEN
          prb = 1.0
        ELSE
          prb = 0.5*erfc( arg/SQR2 )
        END IF
      ELSE
        prb = 0.0
      END IF
      xfun2 = 100.*MAX( prb,small )

!------ Probability of Exceeding - Clipped-normal

    CASE( PLOT_FUNCTION_PROB_CLIP )

      xmean(1) = m1
      xsig(1)  = SQRT(v1)
      qq(1)    = 1.0
      xmean(2) = m2
      xsig(2)  = SQRT(v2)
      qq(2)    = 1.0

      test = ABS(LOG(xmean(1)*xsig(2)/(xmean(2)*xsig(1))))
      IF( test <= 0.05 )THEN
        PDF1 = .TRUE.
        PDF2 = .FALSE.
      ELSE IF( test >= 0.5 )THEN
        PDF2 = .TRUE.
        PDF1 = .FALSE.
      ELSE
        PDF1 = .TRUE.
        PDF2 = .TRUE.
        ratPDF = (test-0.05)/0.45
      END IF

      IF( PDF1 )THEN
        sig = SQRT( MAX(x2,0.0) )
        IF( x1 > small )THEN
          soc = MAX( 0.001,sig/x1 )
          CALL clnpar( soc,gbar,sigg )
          arg = (val/x1 - gbar)/sigg
          IF( arg > 1.E6 )THEN
            p1 = 0.0
          ELSE IF( arg < -1.E6 )THEN
            p1 = 1.0
          ELSE
            p1 = 0.5*erfc( arg/SQR2 )
          END IF
        ELSE
          p1 = 0.0
        END IF
      END IF

      IF( PDF2 )THEN
        p2 = ProbExceed2( xmean(1),xsig(1),xmean(2),xsig(2),val )
        IF( nError /= NO_ERROR )GOTO 9999
        IF( PDF1 )THEN
          xfun2 = 100.*MAX( (1.0-ratPDF)*p1 + ratPDF*p2,small )
        ELSE
          xfun2 = 100.*MAX( p2,small )
        END IF
      ELSE
        xfun2 = 100.*MAX( p1,small )
      END IF

!------ Conditional Probability of Exceeding - Clipped-normal

    CASE( PLOT_FUNCTION_CPROB_CLIP )

      xmean(1) = m1
      xsig(1)  = SQRT(v1)
      qq(1)    = 1.0
      xmean(2) = m2
      xsig(2)  = SQRT(v2)
      qq(2)    = 1.0
      exv      = val

      CALL ptClipNormSet( 2,xmean,xsig,qq,NO_INVERT,CONDITIONAL,2,exv,prb )
      IF( nError /= NO_ERROR )GOTO 9999
      xfun2 = 100.*MAX( prb,small )

!------ Inverse Conditional Probability of Exceeding - Clipped Normal

    CASE( PLOT_FUNCTION_ICPROB_CLIP )

      sig = SQRT(v1)
      soc = MAX( 0.001,sig/m1 )
      CALL clnpar( soc,gbar,sigg )
      gamma = MAX( 0.5*erfc(-(gbar/sigg)/SQR2),GAMMA_MIN )
      exv   = m1*(erfci( 0.01*val*gamma )*sigg + gbar)
      test1 = MAX( exv,small )

      sig = SQRT(v2)
      soc = MAX( 0.001,sig/m2 )
      CALL clnpar( soc,gbar,sigg )
      gamma = MAX( 0.5*erfc(-(gbar/sigg)/SQR2),GAMMA_MIN )
      exv   = m2*(erfci( 0.01*val*gamma )*sigg + gbar)
      test2 = MAX( exv,small )

      test1 = test1 + test2

      xmean(1) = m1
      xsig(1)  = SQRT(v1)
      qq(1)    = 1.0
      xmean(2) = m2
      xsig(2)  = SQRT(v2)
      qq(2)    = 1.0
      prb = 0.01*val
      CALL ptClipNormSet( 2,xmean,xsig,qq,INVERT,CONDITIONAL,2,exv,prb )
      IF( nError /= NO_ERROR )GOTO 9999
      xfun2 = MAX( exv,small )

      xfun2 = MAX( xfun2,test1 )

!------ Switch function

    CASE( PLOT_FUNCTION_SWITCH )

      xfun2 = MAX( x2,small )

!------ Switch function

    CASE( PLOT_FUNCTION_SPEED )

      xfun2 = SQRT(x1*x1+x2*x2)

!------ Invalid function

    CASE DEFAULT

      xfun2 = spv

  END SELECT

END IF

9999 CONTINUE

RETURN
END
!******************************************************************************
! ProbExceed2
!   Purpose : Calculate probability of exceedance for sum of two clipped normals
!   Input   : Means,std devs, and exceedance value
!   Return  : Probability
!******************************************************************************
REAL FUNCTION ProbExceed2( mean1,sig1,mean2,sig2,val )

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: mean1, sig1
REAL, INTENT( IN ) :: mean2, sig2
REAL, INTENT( IN ) :: val

REAL, PARAMETER :: GAMMA_MIN = 1.E-4

REAL xmean1, xsig1, gamma1, xmean2, xsig2, gamma2
REAL soc, zX, fX
REAL x, delX, delX1, delX2, delXt, prob
REAL xmean, xsig, zX1, zX2, delXo

REAL, EXTERNAL :: erfc, pdf2cn

!---- Set up Gaussian parameters

soc  = MAX(0.001,sig1/mean1)
CALL clnpar( soc,xmean1,xsig1 )
gamma1 = 1.0 - MAX(0.5*erfc( -xmean1/(SQRT2*xsig1) ),GAMMA_MIN)
xmean1 = xmean1 * mean1
xsig1  = xsig1  * mean1

soc  = MAX(0.001,sig2/mean2)
CALL clnpar( soc,xmean2,xsig2 )
gamma2 = 1.0 - MAX(0.5*erfc( -xmean2/(SQRT2*xsig2) ),GAMMA_MIN)
xmean2 = xmean2 * mean2
xsig2  = xsig2  * mean2

xmean = xmean1 + xmean2
xsig  = SQRT(xsig1*xsig1 + xsig2*xsig2)

ProbExceed2 = 0.0

!--- First check for extreme conditions

IF( xmean - val + 6.*xsig < 0.0 )RETURN

IF( xmean - val > 6.*xsig )THEN
  ProbExceed2 = 1.0
  RETURN
END IF

!----- Set integration step based on both pdf's

IF( xmean1 >= 0.0 )THEN
  delX1 = 0.1*xsig1
ELSE
  delX1 = 0.1*xsig1*xsig1/MAX(xsig1,-xmean1)
END IF

IF( xmean2 >= 0.0 )THEN
  delX2 = 0.1*xsig2
ELSE
  delX2 = 0.1*xsig2*xsig2/MAX(xsig2,-xmean2)
END IF

IF( xmean >= 0.0 )THEN
  delXt = 0.1*xsig
ELSE
  delXt = 0.1*xsig*xsig/MAX(xsig,-xmean)
END IF

delX = MIN(delX1,delX2)

prob = 0.0

zX1 = (val-xmean1)/xsig1
zX2 = (val-xmean2)/xsig2
IF( zX1 < 5.0 )THEN
  delX = delX1
ELSE
  delX = delXt
END IF
IF( zX2 < 5.0 )delX = MIN(delX,delX2)

x    = val
zX   = (x-xmean)/xsig

delXo = delX

DO WHILE( zX < 5.0 )

  zX1 = (x-xmean1)/xsig1
  zX2 = (x-xmean2)/xsig2
  IF( zX1 < 5.0 )THEN
    delX = delX1
  ELSE
    delX = delXt
  END IF
  IF( zX2 < 5.0 )delX = MIN(delX,delX2)

  IF( delX > delXo )x = x + 0.5*(delX-delXo)

  fX = pdf2cn( xmean1,xsig1,gamma1,xmean2,xsig2,gamma2,x )

  prob = prob + fX*delX

  x  = x + delX
  zX = (x-xmean)/xsig

  delXo = delX

END DO

ProbExceed2 = MIN(1.0,prob)

RETURN
END
!******************************************************************************
! pdf2cn
!   Purpose : Calculate pdf for sum of 2 clipped normals
!******************************************************************************
REAL FUNCTION pdf2cn( mux,sigx,gammax,muy,sigy,gammay,z )

USE constants_fd, ONLY: PI2, SQRT2

IMPLICIT NONE

REAL, INTENT( IN ) :: mux, sigx, gammax
REAL, INTENT( IN ) :: muy, sigy, gammay
REAL, INTENT( IN ) :: z

REAL, PARAMETER :: RT2PI = 2.510574695     !SQRT(PI2)

REAL b, c, s, sigx2, sigy2, sig2

REAL, EXTERNAL :: erfc

sigx2 = sigx*sigx
sigy2 = sigy*sigy
sig2  = sigx2 + sigy2

b = (mux*sigy2 + (z-muy)*sigx2)/sig2
s = SQRT2*sigx*sigy/SQRT(sig2)
c = 0.5*((mux+muy-z)**2/sig2)

pdf2cn = EXP(-c)/(2.0*SQRT(PI2*sig2)) * (erfc(-b/s) - erfc((z-b)/s)) &
       + gammay * EXP(-0.5*(z-mux)**2/sigx2)/(RT2PI*sigx) &
       + gammax * EXP(-0.5*(z-muy)**2/sigy2)/(RT2PI*sigy)

RETURN
END
