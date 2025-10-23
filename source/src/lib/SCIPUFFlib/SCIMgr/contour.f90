!*******************************************************************************
!                ContourCountF
!*******************************************************************************
INTEGER FUNCTION ContourCountF( UserID,grdI,Field,PlotType,contourHead, &
                                contourList,Mode,nLine,nPoint )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagstr_fd
USE contour_fd
USE sagtri_fd
USE sagcnt_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN  ) :: UserID       !USER ID tag
INTEGER,                                                            INTENT( IN  ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN  ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN  ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN  ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( IN  ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN  ) :: Mode         !Contour Mode
INTEGER,                                                            INTENT( OUT ) :: nLine        !Total number of lines
INTEGER,                                                            INTENT( OUT ) :: nPoint       !Total number of points

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

INTERFACE
  SUBROUTINE ContourCount( grdID,Field,PlotType,contourHead,contourList,Mode,nLine,nPoint )
    USE tooluser_fd
    INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                  INTENT( IN  ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( IN  ) :: contourList  !Contour array
    INTEGER,                                     INTENT( IN  ) :: Mode         !Contour Mode
    INTEGER,                                     INTENT( OUT ) :: nLine        !Total number of lines
    INTEGER,                                     INTENT( OUT ) :: nPoint       !Total number of points
  END SUBROUTINE ContourCount
END INTERFACE

!==== initialize

ContourCountF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== count contour lines and points

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL ContourCount( grdI,Field,PlotType,contourHead,contourList,Mode,nLine,nPoint )

  IF( nError == NO_ERROR )ContourCountF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
!*******************************************************************************
!                ContourCount
!*******************************************************************************
SUBROUTINE ContourCount( grdI,Field,PlotType,contourHead, &
                         contourList,Mode,nLine,nPoint )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE sagdef_fd
USE PtrGrdStrItf
USE sagstr_fd
USE contour_fd
USE sagtri_fd
USE sagcnt_fd
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN  ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN  ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN  ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN  ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET,&
                                                                    INTENT( IN  ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN  ) :: Mode         !Contour Mode
INTEGER,                                                            INTENT( OUT ) :: nLine        !Total number of lines
INTEGER,                                                            INTENT( OUT ) :: nPoint       !Total number of points

!==============================================================================
! Local variables
!==============================================================================
LOGICAL                             :: lrestore
LOGICAL                             :: lclose
LOGICAL                             :: log_interp
INTEGER                             :: irv
INTEGER                             :: nfld
INTEGER                             :: plotfld
INTEGER                             :: i
INTEGER                             :: nlev
INTEGER,  DIMENSION(3)              :: ifld
TYPE( SAGgrid_str ), POINTER        :: grd
TYPE( SAGTriangleT_str ), POINTER   :: triT
TYPE( SAGcontour_str )              :: current
TYPE( SCIPContourElementList )      :: contour

REAL                                :: dmin,dmax,fmin,fmax,dmn,dmx
INTEGER                             :: imin,imax

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_SetSpecialValue
INTEGER, EXTERNAL :: SAG_BottomFunctionID
INTEGER, EXTERNAL :: SAG_CountContourID
INTEGER, EXTERNAL :: SAG_InitError
INTEGER, EXTERNAL :: SAG_TrianglesID
INTEGER, EXTERNAL :: SAG_NodesID
INTEGER, EXTERNAL :: SAG_LastError
INTEGER, EXTERNAL :: PlotFunction

!==============================================================================
! Initialize error
!==============================================================================

irv = SAG_InitError()

contour%listHdr =  contourHead
contour%listPtr => contourList

NULLIFY(current%ippts)
NULLIFY(current%iplns)

IF( Aborted() )GOTO 9999

!==============================================================================
! Set number of contour levels
!==============================================================================
nlev = contour%listHdr%number
IF( nlev <= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ContourCount'
  eMessage = 'Error - No contours specified'
  GOTO 9999
END IF

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - ContourCount'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpField(1,Field,NULL)
!CALL DumpPlotType(PlotType)
!WRITE(lun_dbg,'(A,I8,B34.32)')' ContourMode',Mode,Mode
!CALL DumpContour(contourHead%number,contourList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)

!!DEC$ ENDIF
CALL GetFieldMinMax( grdI,PlotType,dmin,dmax,fmin,fmax,dmn,dmx )
IF( nError /= NO_ERROR )GOTO 9999

imin = 1
imax = 0
DO i = 1,nlev
  IF( contour%listPtr(i)%contour <= dmn )imin = i + 1
  IF( contour%listPtr(i)%contour <= dmx )imax = i
END DO
IF( imin > imax .AND. imax > 0 )THEN
  nLine  = 1
  nPoint = 4*DOMAIN_POINTS_PER_SIDE+1
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'ContourCount'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG triangle structure
!==============================================================================
triT => SAG_PtrTriStr( grdI )

!==============================================================================
! Initialize parameters for SAG triangle/contour structure
!==============================================================================
lclose        = BTEST(Mode,BCLOSE_CONTOUR)
plotfld       = 3                           !Results of PlotFunction are here
current%mxpts = 0
current%mxlns = 0

!==============================================================================
! Turn on/off SAG Special value feature as needed
!==============================================================================
IF( BTEST(Mode,BCLOSE_CONTOUR) )THEN
  irv = SAG_SetSpecialValue( .FALSE.,HP_SPV )
ELSE
  irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )
END IF

!==============================================================================
! Initialize counters
!==============================================================================
nLine  = 0
nPoint = 0

IF( Aborted() )GOTO 9999

IF( grd%type == SAG_GRID_NONE )THEN
  PlotFunc_small = -1.E36  !set to some parameter?
ELSE
  PlotFunc_small = 1.E-30  !set to some parameter?
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
      eRoutine = 'ContourCount'
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
! Set plot function values
!==============================================================================
  nfld     = 3
  ifld(1)  = 1         !mean
  ifld(2)  = 2         !variance
  ifld(3)  = plotfld   !plot variable
  lrestore = .FALSE.
  CALL InitAbortedCount()
  irv = SAG_BottomFunctionID( grdI,PlotFunction,nfld,ifld,lrestore,.TRUE. )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourCount'
    eMessage = 'Error computing plot function'
    GOTO 9999
  END IF

  IF( Aborted() )GOTO 9999

  CALL CheckSmoothCN2( grdI )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( grd%PlotType /= -1 )grd%PlotType = PlotType%type
  grd%PlotData = PlotType%data

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

!==============================================================================
! Triangulate
!==============================================================================
nfld    = 1
ifld(1) = plotfld
IF( .NOT.ASSOCIATED(triT%iptri) )THEN

  IF( Aborted() )GOTO 9999

  irv = SAG_TrianglesID( grdI,nfld,ifld,.TRUE. )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourCount'
    eMessage = 'Error computing field triangles'
    GOTO 9998
  END IF

ELSE

  IF( Aborted() )GOTO 9999

  irv = SAG_NodesID( grdI,nfld,ifld )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourCount'
    eMessage = 'Error computing field triangles node values'
    GOTO 9998
  END IF

END IF

IF( Aborted() )GOTO 9999

!==============================================================================
! Loop over contour levels
!==============================================================================
DO i = 1,nlev

!==============================================================================
! Initialize the contour structure
!==============================================================================
  CALL SAG_InitContour( current,1,contour%listPtr(i)%contour,log_interp,lclose )

!==============================================================================
! Count
!==============================================================================
  irv = SAG_CountContourID( grdI,current )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourCount'
    eMessage = 'Error counting contour points/lines'
    GOTO 9998
  END IF

!==============================================================================
! Add this contours count to the total
!==============================================================================
  nLine  = nLine  + current%nlns
  nPoint = nPoint + current%npts

  IF( Aborted() )GOTO 9999

END DO

!==============================================================================
! Free the Contour structure
!==============================================================================
9998 CONTINUE
CALL SAG_FreeContour( current )

9999 CONTINUE

NULLIFY( contour%listPtr )

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                ContourFieldF
!*******************************************************************************
INTEGER FUNCTION ContourFieldF( UserID,grdI,Field,PlotType,contourHead, &
                                contourList,Mode,Line,Point )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE sagtri_fd
USE sagcnt_fd
USE default_fd
USE sagstr_fd
USE sagdef_fd
USE contour_fd
USE AreaMode_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE SCIMgrState
USE PtrGrdStrItf
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN    ) :: UserID       !USER ID tag
INTEGER,                                                            INTENT( IN    ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN    ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN    ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN    ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( INOUT ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN    ) :: Mode         !Contour Mode
TYPE( SCIPLineT  ), DIMENSION(*),                                   INTENT( OUT   ) :: Line         !Lines output array
TYPE( SCIPPointT ), DIMENSION(*),                                   INTENT( OUT   ) :: Point        !Points output array

!==============================================================================
! Local variables
!==============================================================================
INTEGER irv
INTEGER currentState

INTERFACE
  SUBROUTINE ContourField( grdID,Field,PlotType,contourHead,contourList,Mode,Line,Point )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    INTEGER,                                   INTENT( IN    ) :: Mode         !Contour Mode
    TYPE( SCIPLineT  ), DIMENSION(*),          INTENT( OUT   ) :: Line         !Lines output array
    TYPE( SCIPPointT ), DIMENSION(*),          INTENT( OUT   ) :: Point        !Points output array
  END SUBROUTINE ContourField
END INTERFACE

!==== initialize

ContourFieldF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== get contour points and lines

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL ContourField( grdI,Field,PlotType,contourHead,contourList,Mode,Line,Point )

  IF( nError == NO_ERROR )ContourFieldF = SCIPsuccess

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
!                ContourField
!*******************************************************************************
SUBROUTINE ContourField( grdI,Field,PlotType,contourHead,contourList,Mode,Line,Point )

USE field_fd
USE contourlist_fd
USE SCIMgr_fd
USE sagtri_fd
USE sagcnt_fd
USE default_fd
USE sagstr_fd
USE sagdef_fd
USE contour_fd
USE AreaMode_fd
USE PlotFunc_fi
USE PlotAux_fi
USE error_fi
USE PtrGrdStrItf
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                            INTENT( IN    ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN    ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN    ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN    ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( INOUT ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN    ) :: Mode         !Contour Mode
TYPE( SCIPLineT  ), DIMENSION(*),                                   INTENT( OUT   ) :: Line         !Lines output array
TYPE( SCIPPointT ), DIMENSION(*),                                   INTENT( OUT   ) :: Point        !Points output array

!==============================================================================
! Local variables
!==============================================================================
LOGICAL                             :: lrestore
LOGICAL                             :: lclose
LOGICAL                             :: log_interp
INTEGER                             :: irv
INTEGER                             :: nlev
INTEGER                             :: nfld
INTEGER                             :: plotfld
INTEGER                             :: i
INTEGER                             :: j
INTEGER                             :: k
INTEGER,  DIMENSION(3)              :: ifld
TYPE( SAGgrid_str ), POINTER        :: grd
TYPE( SAGTriangleT_str ), POINTER   :: triT
TYPE( SAGcontour_str )              :: current
INTEGER                             :: nLine
INTEGER                             :: nPoint
INTEGER                             :: nPts
TYPE ( SAGpoint_str ), POINTER, DIMENSION(:) :: c_points
INTEGER,               POINTER, DIMENSION(:) :: c_lines
TYPE( SCIPFieldCoordinateT )        :: LLACoordinate
TYPE( ARAPDrawT )                   :: PopDraw
TYPE( SCIPContourElementList )      :: contour
TYPE( SCIPContourHeaderT )          :: drawHead

REAL                                :: dmin,dmax,fmin,fmax,dmn,dmx
REAL                                :: xmn,ymn,xmx,ymx,dx,dy
INTEGER                             :: m0,n0
INTEGER                             :: imin,imax,is,handedness
!==============================================================================
! Function calls
!==============================================================================
INTEGER,EXTERNAL :: SAG_SetSpecialValue
INTEGER,EXTERNAL :: SAG_BottomFunctionID
INTEGER,EXTERNAL :: SAG_BuildContourID
INTEGER,EXTERNAL :: SAG_InitError
INTEGER,EXTERNAL :: SAG_TrianglesID
INTEGER,EXTERNAL :: SAG_NodesID
INTEGER,EXTERNAL :: SAG_LastError
LOGICAL,EXTERNAL :: ClosedContour
INTEGER,EXTERNAL :: RightHandedContour
LOGICAL,EXTERNAL :: SamePoint
INTEGER,EXTERNAL :: PlotFunction
INTEGER,EXTERNAL :: NullFill
INTEGER,EXTERNAL :: NullDraw

INTERFACE
  SUBROUTINE DrawField(grdID,Field,PlotType,contourHead,contourList,GUIdraw,UserFill,UserDraw)
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

!==============================================================================
! Initialize error
!==============================================================================
irv = SAG_InitError()

contour%listHdr =  contourHead
contour%listPtr => contourList

NULLIFY(current%ippts)
NULLIFY(current%iplns)

IF( Aborted() )GOTO 9999

!==============================================================================
! Set number of contour levels
!==============================================================================
nlev = contour%listHdr%number
IF( nlev <= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ContourField'
  eMessage = 'Error - No contours specified'
  GOTO 9999
END IF

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - ContourField'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,*)'grdI = ',grdI
!CALL DumpField(1,Field,NULL)
!CALL DumpPlotType(PlotType)
!WRITE(lun_dbg,'(A,I8,B34.32)')' ContourMode',Mode,Mode
!CALL DumpContour(contourHead%number,contourList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)

!!DEC$ ENDIF
CALL GetFieldMinMax( grdI,PlotType,dmin,dmax,fmin,fmax,dmn,dmx )
IF( nError /= NO_ERROR )GOTO 9999

imin = 1
imax = 0
DO i = 1,nlev
  IF( contour%listPtr(i)%contour <= dmn )imin = i + 1
  IF( contour%listPtr(i)%contour <= dmx )imax = i
END DO
IF( imin > imax .AND. imax > 0 )THEN
  nLine  = 1
  nPoint = 4*DOMAIN_POINTS_PER_SIDE+1
  Line(1)%index  = imax
  Line(1)%start  = 1
  Line(1)%number = nPoint
  Line(1)%mode   = HP_LEFTHAND
  CALL GetFieldDomain( grdI,m0,n0,xmn,ymn,dx,dy )
  IF( nError /= NO_ERROR )GOTO 9999
  xmx = xmn + FLOAT(m0)*dx
  ymx = ymn + FLOAT(n0)*dy
  dx = (xmx-xmn)/FLOAT(DOMAIN_POINTS_PER_SIDE)
  dy = (ymx-ymn)/FLOAT(DOMAIN_POINTS_PER_SIDE)
  DO i = 1,DOMAIN_POINTS_PER_SIDE
    Point(i)%x = xmn + dx*(i-1)
    Point(i)%y = ymn
  END DO
  DO i = 1,DOMAIN_POINTS_PER_SIDE
    Point(i+DOMAIN_POINTS_PER_SIDE)%x = xmx
    Point(i+DOMAIN_POINTS_PER_SIDE)%y = ymn + dy*(i-1)
  END DO
  DO i = 1,DOMAIN_POINTS_PER_SIDE
    Point(i+2*DOMAIN_POINTS_PER_SIDE)%x = xmx - dx*(i-1)
    Point(i+2*DOMAIN_POINTS_PER_SIDE)%y = ymx
  END DO
  DO i = 1,DOMAIN_POINTS_PER_SIDE
    Point(i+3*DOMAIN_POINTS_PER_SIDE)%x = xmn
    Point(i+3*DOMAIN_POINTS_PER_SIDE)%y = ymx - dy*(i-1)
  END DO
  Point(nPoint)%x = xmn
  Point(nPoint)%y = ymn
  GOTO 1000
END IF

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  nError   = UK_ERROR
  eRoutine = 'ContourField'
  eMessage = 'Error getting grid structure from grid ID'
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG triangle structure
!==============================================================================
triT => SAG_PtrTriStr( grdI )

!==============================================================================
! Initialize parameters for SAG triangle/contour structure
!==============================================================================
lclose      = BTEST(Mode,BCLOSE_CONTOUR)
plotfld     = 3                      !Results of PlotFunction are here
current%mxpts = 0
current%mxlns = 0
NULLIFY( c_points )
NULLIFY( c_lines )

!==============================================================================
! Turn on/off SAG Special value feature as needed
!==============================================================================
IF( lclose )THEN
  irv = SAG_SetSpecialValue( .FALSE.,HP_SPV )
ELSE
  irv = SAG_SetSpecialValue( .TRUE.,HP_SPV )
END IF

!==============================================================================
! Initialize counters
!==============================================================================
nLine  = 0
nPoint = 0

IF( Aborted() )GOTO 9999

IF( grd%type == SAG_GRID_NONE )THEN
  PlotFunc_small = -1.E36  !set to some parameter?
ELSE
  PlotFunc_small = 1.E-30  !set to some parameter?
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
      eRoutine = 'ContourField'
      eMessage = 'Error - Invalid Plot type'
      WRITE(eInform,*)'PlotType=',PlotType%type
      GOTO 9999

  END SELECT

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
! Set plot function values
!==============================================================================
  nfld     = 3
  ifld(1)  = 1         !mean
  ifld(2)  = 2         !variance
  ifld(3)  = plotfld   !plot variable
  lrestore = .FALSE.
  CALL InitAbortedCount()
  irv = SAG_BottomFunctionID( grdI,PlotFunction,nfld,ifld,lrestore,.TRUE. )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourField'
    eMessage = 'Error computing plot function'
    GOTO 9999
  END IF

  IF( Aborted() )GOTO 9999

  CALL CheckSmoothCN2( grdI )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( grd%PlotType /= -1 )grd%PlotType = PlotType%type
  grd%PlotData = PlotType%data

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

!==============================================================================
! Triangulate - Keep Nodes in grid coordinates for accuracy
!==============================================================================
nfld    = 1
ifld(1) = plotfld
IF( .NOT.ASSOCIATED(triT%iptri) )THEN

  IF( Aborted() )GOTO 9999

  irv = SAG_TrianglesID( grdI,nfld,ifld,.TRUE.)
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourField'
    eMessage = 'Error computing field triangles'
    GOTO 9998
  END IF

ELSE

  IF( Aborted() )GOTO 9998

  irv = SAG_NodesID( grdI,nfld,ifld )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourField'
    eMessage = 'Error computing field triangles node values'
    GOTO 9998
  END IF

END IF

IF( Aborted() )GOTO 9998

!==============================================================================
! Loop over contour levels
!==============================================================================
DO i = 1,nlev

!==============================================================================
! Initialize the contour structure
!==============================================================================
  CALL SAG_InitContour( current,1,contour%listPtr(i)%contour,log_interp,lclose )

!==============================================================================
! Count
!==============================================================================
  irv = SAG_BuildContourID( grdI,current )
  IF( irv /= SAG_OK )THEN
    nError = SAG_LastError( eInform,eAction )
    IF( nError == NO_ERROR )nError = UK_ERROR
    eRoutine = 'ContourField'
    eMessage = 'Error counting contour points/lines'
    GOTO 9998
  END IF

!==============================================================================
! Add this contours count to the total
!==============================================================================
  c_points => current%ippts
  c_lines  => current%iplns

  DO j = 1,current%nlns
    nLine = nLine + 1
    Line(nLine)%index = i
    Line(nLine)%start = nPoint + 1
    IF( j == current%nlns )THEN
      npts = current%npts - c_lines(j) + 1
    ELSE
      npts = c_lines(j+1) - c_lines(j)
    END IF
    Line(nLine)%number = npts
    DO k = 1,npts
      nPoint = nPoint + 1
      Point(nPoint)%x = c_points(c_lines(j) + k - 1)%x
      Point(nPoint)%y = c_points(c_lines(j) + k - 1)%y
    END DO
    is = Line(nLine)%start
    DO WHILE( SamePoint(Point(is),Point(is+1)) .AND. is < (Line(nLine)%start + npts - 2) )
      is = is + 1
    END DO
    IF( ClosedContour(Point(Line(nLine)%start),Point(nPoint)) )THEN
      handedness = RightHandedContour( grdI,plotfld,contour%listPtr(i)%contour,Point(is),Point(is+1),log_interp )
      Line(nLine)%mode = NOT_SET_I
      DO WHILE( (handedness == 0) .AND. (is < (Line(nLine)%start + npts - 2)) )
        is = is + 1
        DO WHILE( SamePoint(Point(is),Point(is+1)) .AND. is < (Line(nLine)%start + npts - 2) )
          is = is + 1
        END DO
        handedness = RightHandedContour( grdI,plotfld,contour%listPtr(i)%contour,Point(is),Point(is+1),log_interp )
      END DO
      IF( handedness > 0 )THEN
        Line(nLine)%mode = HP_RIGHTHAND
      ELSE IF( handedness < 0 )THEN
        Line(nLine)%mode = HP_LEFTHAND
      ELSE
        IF( Line(nLine)%mode == NOT_SET_I )THEN
          Line(nLine)%mode = HP_OPEN
          eRoutine = 'contour'
          eMessage = 'Unable to determine handedness of a closed contour'
          WRITE(eInform,'(A,G13.4)')'Contour level',contour%listPtr(i)%contour
          eAction = 'Contour handedness set to Open Contour'
          CALL InfoMessage( )
        END IF
      END IF
    ELSE
      Line(nLine)%mode = HP_OPEN
    END IF
  END DO

  IF( Aborted() )GOTO 9998

END DO

!==============================================================================
! Convert to Project Coordinates if necessary
!==============================================================================
DO i = 1,nPoint
  Point(i)%x = grd%xmin + grd%dx*Point(i)%x
  Point(i)%y = grd%ymin + grd%dy*Point(i)%y
END DO

IF( Aborted() )GOTO 9998

!==============================================================================
! Convert to LatLon Output
!==============================================================================
1000 CONTINUE

IF( BTEST(Mode,BLATLON_OUTPUT) )THEN
  LLACoordinate%mode          = HD_LATLON
  LLACoordinate%UTMZone       = NOT_SET_I
  LLACoordinate%reference%x   = NOT_SET_R
  LLACoordinate%reference%y   = NOT_SET_R
  LLACoordinate%reference%lat = NOT_SET_R
  LLACoordinate%reference%lon = NOT_SET_R
  LLACoordinate%vertSlice%resolution = NOT_SET_I
  LLACoordinate%vertSlice%startPt%x  = NOT_SET_R
  LLACoordinate%vertSlice%startPt%y  = NOT_SET_R
  LLACoordinate%vertSlice%endPt      = LLACoordinate%vertSlice%startPt
  LLACoordinate%horzSlice%height     = NOT_SET_R
  LLACoordinate%horzSlice%mode       = NOT_SET_I
  CALL TransformPt( Field%coordinate,LLACoordinate,nPoint,Point )
  IF( nError /= NO_ERROR )THEN
    !Keep origianl message but change routine
    eRoutine = 'ContourField'
    GOTO 9998
  END IF
END IF

IF( Aborted() )GOTO 9998

!==============================================================================
! Check for Population request
!==============================================================================
IF( PlotType%areaMode /= HP_OFF )THEN
  PopDraw%fillContour = SCIPfalse
  PopDraw%drawContour = SCIPfalse
  PopDraw%fill_Lo     = SCIPfalse
  PopDraw%fill_Hi     = SCIPtrue
  drawHead = contourHead
  drawHead%drawMode = Mode  !To acount for Lo/Hi contours (instead of normal Hi/Lo) like Time
  CALL DrawField( grdI,Field,PlotType,drawHead,contourList, &
                       PopDraw,NullFill,NullDraw )
  IF( nError /= NO_ERROR )THEN
    !Keep origianl message but change routine
    eRoutine = 'ContourField'
    GOTO 9998
  END IF
END IF

IF( Aborted() )GOTO 9998

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Output Dump - ContourField'
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpContour(contourHead%number,contourList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF
!==============================================================================
! Free the Contour structure
!==============================================================================
9998 CONTINUE

NULLIFY( c_points )
NULLIFY( c_lines )

CALL SAG_FreeContour( current )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                NullFill
!*******************************************************************************
INTEGER FUNCTION NullFill( np,xp,yp,nlev )

USE sagdef_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN ) :: xp     !Input  X array
REAL,   DIMENSION(np), INTENT( IN ) :: yp     !Output Y array
INTEGER,               INTENT( IN ) :: nlev   !Current level (pen color)

!==============================================================================
! Set Return Value
!==============================================================================
NullFill = SAG_OK

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                NullDraw
!*******************************************************************************
INTEGER FUNCTION NullDraw( np,xp,yp,nlev )

USE sagdef_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN ) :: np     !number of points
REAL,   DIMENSION(np), INTENT( IN ) :: xp     !Input  X array
REAL,   DIMENSION(np), INTENT( IN ) :: yp     !Output Y array
INTEGER,               INTENT( IN ) :: nlev   !Current level (pen color)

!==============================================================================
! Set Return Value
!==============================================================================
NullDraw = SAG_OK

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                ClosedContour
!*******************************************************************************
LOGICAL FUNCTION ClosedContour( Spt,Ept )

USE field_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPPointT ), INTENT( IN ) :: Spt        !Starting Point
TYPE( SCIPPointT ), INTENT( IN ) :: Ept        !Ending Point

!==============================================================================
! Local variables
!==============================================================================
REAL dx, dxx, dy, dyy

!==============================================================================
! Initialize Return Value
!==============================================================================
ClosedContour = .FALSE.

!==============================================================================
! Check nearness of end point to start point
!==============================================================================
dx  = ABS(Spt%x-Ept%x)
dy  = ABS(Spt%y-Ept%y)
dxx = ABS(Spt%x-NEAREST(Spt%x,-1.0))
dyy = ABS(Spt%y-NEAREST(Spt%y,-1.0))

IF( dx <= dxx .AND. dy <= dyy )ClosedContour = .TRUE.

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                RightHandedContour
!*******************************************************************************
INTEGER FUNCTION RightHandedContour( grdI,plotfld,cValue,Spt,Ept,log_interp )

USE field_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,            INTENT( IN ) :: grdI
INTEGER,            INTENT( IN ) :: plotfld
REAL,               INTENT( IN ) :: cValue
TYPE( SCIPPointT ), INTENT( IN ) :: Spt        !Starting Point
TYPE( SCIPPointT ), INTENT( IN ) :: Ept        !Next Point
LOGICAL,            INTENT( IN ) :: log_interp !Interpolation flag true=log false=linear

!==============================================================================
! Local variables
!==============================================================================
REAL    x0, y0, alfa, beta, dx, dy, dxx, dyy, dl, xx, yy, dm
INTEGER nfld

REAL,    DIMENSION(1) :: rValue
REAL,    DIMENSION(1) :: lValue
INTEGER, DIMENSION(1) :: ifld

REAL, PARAMETER :: FACTOR_NEAREST = 2.0
REAL, PARAMETER :: FACTOR_ACTUAL  = 0.04

REAL, EXTERNAL :: cosd, sind, atan2d

!==============================================================================
! Initialize Return Value
!==============================================================================
RightHandedContour = 0

!==============================================================================
! Compute MidPoint
!==============================================================================
x0 = 0.5*(Spt%x + Ept%x)
y0 = 0.5*(Spt%y + Ept%y)

!==============================================================================
! Compute angle
!==============================================================================
dx = Ept%x-Spt%x
dy = Ept%y-Spt%y

!==============================================================================
! Compute offset
!==============================================================================
dl   = SQRT( dx**2 + dy**2 )    !Full distance between points

dxx  = x0 - NEAREST(x0,-1.0)
dyy  = y0 - NEAREST(y0,-1.0)
dm   = MAX( dxx,dyy )           !Smallest calculatable offset

beta = MIN(dl,MAX(FACTOR_ACTUAL*dl,FACTOR_NEAREST*dm))

!==============================================================================
! Compute checkpoint to the right
!==============================================================================
alfa = atan2d( dy,dx ) - 90.0

xx = x0 + beta*cosd( alfa )
yy = y0 + beta*sind( alfa )

!==============================================================================
! Check
!==============================================================================
nfld = 1
ifld = plotfld
CALL GetPointValGC( xx,yy,rValue,grdI,nfld,ifld,log_interp )

!==============================================================================
! Compute checkpoint to the left
!==============================================================================
alfa = atan2d( dy,dx ) + 90.0

xx = x0 + beta*cosd( alfa )
yy = y0 + beta*sind( alfa )

!==============================================================================
! Check
!==============================================================================
nfld = 1
ifld = plotfld
CALL GetPointValGC( xx,yy,lValue,grdI,nfld,ifld,log_interp )

!==============================================================================
! Set return value
!==============================================================================
IF( rValue(1) >= cValue .AND. lValue(1) < cValue )THEN
  RightHandedContour = 1
ELSE IF( rValue(1) < cValue .AND. lValue(1) >= cValue )THEN
  RightHandedContour = -1
ELSE
  RightHandedContour = 0
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SamePoint
!*******************************************************************************
LOGICAL FUNCTION SamePoint( Spt,Ept )

USE field_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE( SCIPPointT ), INTENT( IN ) :: Spt        !Starting Point
TYPE( SCIPPointT ), INTENT( IN ) :: Ept        !Next Point

!==============================================================================
! Local variables
!==============================================================================
REAL x0, y0, dx, dy, dxx, dyy

REAL, PARAMETER :: FACTOR_NEAREST = 2.0
REAL, PARAMETER :: FACTOR_ACTUAL  = 0.04

!==============================================================================
! Initialize Return Value
!==============================================================================
SamePoint = .FALSE.

!==============================================================================
! Compute MidPoint
!==============================================================================
x0 = 0.5*(Spt%x + Ept%x)
y0 = 0.5*(Spt%y + Ept%y)

!==============================================================================
! Compute angle
!==============================================================================
dx = Ept%x - Spt%x
dy = Ept%y - Spt%y

!==============================================================================
! Compute offset
!==============================================================================
dx   = SQRT( dx**2 + dy**2 )    !Full distance between points

dxx  = x0 - NEAREST(x0,-1.0)
dyy  = y0 - NEAREST(y0,-1.0)
dy   = MAX( dxx,dyy )           !Smallest calculatable offset

SamePoint = dx <= FACTOR_NEAREST*dy

RETURN
END
