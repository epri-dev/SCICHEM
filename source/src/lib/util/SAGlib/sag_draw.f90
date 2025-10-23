!*******************************************************************************
!                SAG_InitDrawField
!*******************************************************************************
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
                              lDoPop       ) !Pop/Area flag

USE sagdrw_usr

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
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

!==============================================================================
! Local Variables
!==============================================================================
!==============================================================================
! Functions
!==============================================================================

!==============================================================================
!==============================================================================
! Clear internal SAG draw structure in sagdrw_usr module
!==============================================================================
CALL SAG_FreeDrawField()

!==============================================================================
! Load draw instructions into internal SAG draw structure in sagdrw_usr module
!==============================================================================
draw%ifld        = ifld
draw%log_interp  = log_interp

draw%FillContour = FillContour
draw%fill_lo     = fill_lo
draw%fill_hi     = fill_hi

draw%DrawContour = DrawContour
draw%lclose      = lclose

draw%nlev        =  nlev
draw%iplev       => level

draw%start       =  istart
draw%stop        =  istop

draw%lAreaCell   = lPopCell
draw%lAreaTri    = lPopTri

IF( PRESENT( lDoPop ) )THEN
  draw%lDoThreaded = .NOT.lDoPop
ELSE
  draw%lDoThreaded = .FALSE.
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SAG_FreeDrawField
!*******************************************************************************
SUBROUTINE SAG_FreeDrawField()

USE sagdrw_usr

IMPLICIT NONE

!==============================================================================
! Disassociate pointers to contour levels and Pop/Area results arrays
!    NOTE - Does not Deallocate the space - calling functions responsibility
!==============================================================================
IF( ASSOCIATED(draw%iplev) )NULLIFY( draw%iplev )

!==============================================================================
! Zero number of levels
!==============================================================================
draw%nlev = 0

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SAG_DrawFieldID
!*******************************************************************************
INTEGER FUNCTION SAG_DrawFieldID( grdI,UserFill,UserDraw,UserArea )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER, INTENT( IN ) :: grdI      !SAG Grid ID
INTEGER, EXTERNAL     :: UserFill  !User Fill function
INTEGER, EXTERNAL     :: UserDraw  !User Draw function
INTEGER, EXTERNAL     :: UserArea  !User Area function

!==============================================================================
! Local variables
!==============================================================================
TYPE ( SAGgrid_str )     , POINTER :: grd
TYPE ( SAGTriangleT_str ), POINTER :: triT

!==============================================================================
! Function calls
!==============================================================================
INTERFACE
  INTEGER FUNCTION SAG_DrawField( grd,triT,UserFill,UserDraw,UserArea )
    USE sagstr_fd
    USE sagtri_fd
    TYPE ( SAGgrid_str ),      POINTER :: grd      !SAG grid structure
    TYPE ( SAGtriangleT_str ), POINTER :: triT     !SAG triangle structure
    INTEGER, EXTERNAL                  :: UserFill !User Fill function
    INTEGER, EXTERNAL                  :: UserDraw !User Draw function
    INTEGER, EXTERNAL                  :: UserArea !User Area function
  END FUNCTION SAG_DrawField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_DrawFieldID = SAG_ERROR

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG triangle structure
!==============================================================================
triT => SAG_PtrTriStr( grdI )

!==============================================================================
! Call SAG_DrawField
!==============================================================================
SAG_DrawFieldID = SAG_DrawField( grd,triT,UserFill,UserDraw,UserArea )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SAG_DrawCellID
!*******************************************************************************
INTEGER FUNCTION SAG_DrawCellID( grdI,UserDraw )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER, INTENT( IN ) :: grdI      !SAG Grid ID
INTEGER, EXTERNAL     :: UserDraw  !User draw function

!==============================================================================
! Local variables
!==============================================================================
TYPE ( SAGgrid_str )     , POINTER :: grd

!==============================================================================
! Function calls
!==============================================================================
INTERFACE
  INTEGER FUNCTION SAG_DrawCell( grd,UserDraw )
    USE sagstr_fd
    TYPE ( SAGgrid_str ), POINTER :: grd      !SAG grid structure
    INTEGER, EXTERNAL             :: UserDraw !User Draw function
  END FUNCTION SAG_DrawCell
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_DrawCellID = SAG_ERROR

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Call SAG_DrawCell
!==============================================================================
SAG_DrawCellID = SAG_DrawCell( grd,UserDraw )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SAG_DrawTriangleID
!*******************************************************************************
INTEGER FUNCTION SAG_DrawTriangleID( grdI,UserDraw )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER, INTENT( IN ) :: grdI      !SAG Grid ID
INTEGER, EXTERNAL     :: UserDraw  !User draw function

!==============================================================================
! Local variables
!==============================================================================
INTEGER                            :: irv
INTEGER                            :: nfld
INTEGER, DIMENSION(2)              :: ifld
TYPE ( SAGgrid_str )     , POINTER :: grd
TYPE ( SAGTriangleT_str ), POINTER :: triT

!==============================================================================
! Function calls
!==============================================================================
INTEGER,EXTERNAL :: SAG_TrianglesID
INTERFACE
  INTEGER FUNCTION SAG_DrawTriangle( triT,UserDraw,grd )
    USE sagstr_fd
    USE sagtri_fd
    TYPE ( SAGtriangleT_str ), POINTER :: triT     !SAG triangle structure
    INTEGER, EXTERNAL                  :: UserDraw !User Draw function
    TYPE ( SAGgrid_str )     , POINTER :: grd
  END FUNCTION SAG_DrawTriangle
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_DrawTriangleID = SAG_ERROR

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG triangle structure
!==============================================================================
triT => SAG_PtrTriStr( grdI )
IF( .NOT.ASSOCIATED(triT) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Build triangles if necessary
!==============================================================================
IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  nfld    = 1
  ifld(1) = 1
  irv = SAG_TrianglesID( grdI,nfld,ifld,.TRUE. )
  IF( irv /= SAG_OK )THEN
    GOTO 9999
  END IF
END IF

!==============================================================================
! Call SAG_DrawTriangle
!==============================================================================
SAG_DrawTriangleID = SAG_DrawTriangle( triT,UserDraw,grd )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                SAG_DrawField
!*******************************************************************************
INTEGER FUNCTION SAG_DrawField( grd,triT,UserFill,UserDraw,UserArea )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagcnt_fd
USE sagdrw_fd
USE sagfil_fd
USE sagerr_fi
USE sagdrw_usr
USE sagpop_fi
USE omp_lib

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE ( SAGgrid_str ),      POINTER :: grd      !SAG grid structure
TYPE ( SAGtriangleT_str ), POINTER :: triT     !SAG triangle structure
INTEGER, EXTERNAL                  :: UserFill !User Fill function
INTEGER, EXTERNAL                  :: UserDraw !User Draw function
INTEGER, EXTERNAL                  :: UserArea !User Area function

!==============================================================================
! Local vaiables
!==============================================================================
INTEGER                :: irv
INTEGER                :: ic
INTEGER                :: nfld
INTEGER, DIMENSION(2)  :: ifld
LOGICAL                :: needTriangles
TYPE( SAGcontour_str ) :: contour

TYPE( SAGnodeT_str ), POINTER :: node

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_TriangleFill

INTERFACE
  INTEGER FUNCTION SAG_BottomFunction( grd,UserFunction,nfld,ifld,restore,lThreadSafe )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
    INTEGER,               EXTERNAL     :: UserFunction
    INTEGER,               INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
    LOGICAL,               INTENT( IN ) :: restore
    LOGICAL,               INTENT( IN ) :: lThreadSafe !Should be FALSE if UserFunction is not Threadsafe
  END FUNCTION SAG_BottomFunction

  INTEGER FUNCTION SAG_Nodes( grd,nfld,ifld,nodeI )
    USE sagstr_fd
    USE sagnod_fd
    TYPE( SAGgrid_str ),  POINTER :: grd
    INTEGER                       :: nfld
    INTEGER, DIMENSION(*), TARGET :: ifld
    TYPE( SAGnodeT_str ),  TARGET :: nodeI
  END FUNCTION SAG_Nodes

  INTEGER FUNCTION SAG_Triangles( grd,nfld,ifld,connect,triT )
    USE sagstr_fd
    USE sagtri_fd
    TYPE ( SAGgrid_str ),     POINTER      :: grd
    INTEGER,                  INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*),    INTENT( IN ) :: ifld
    LOGICAL,                  INTENT( IN ) :: connect
    TYPE( SAGtriangleT_str ), POINTER      :: triT
  END FUNCTION SAG_Triangles

  SUBROUTINE SAG_FreeTriangle( triT )
    USE sagtri_fd
    TYPE( SAGTriangleT_str ), POINTER :: triT
  END SUBROUTINE SAG_FreeTriangle

  INTEGER FUNCTION SAG_BuildContour( triT,contourI )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ),       POINTER         :: triT
    TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI
  END FUNCTION SAG_BuildContour

  INTEGER FUNCTION SAG_TriangleFunction( triT,UserFunction,UserDisplay,grd,use8 )
    USE sagtri_fd
    USE sagstr_fd
    TYPE( SAGtriangleT_str ), POINTER :: triT
    INTERFACE
      INTEGER FUNCTION UserFunction( nid,node,dat,nfld,UserDisplay,grd )
        USE sagnod_fd
        USE sagstr_fd
        INTEGER,                           INTENT( IN ) :: nfld
        INTEGER,             DIMENSION(3), INTENT( IN ) :: nid
        TYPE( SAGnode_str ), DIMENSION(:), POINTER      :: node
        REAL,                DIMENSION(:), POINTER      :: dat
        INTEGER, EXTERNAL :: UserDisplay
        TYPE( SAGgrid_str ), POINTER  :: grd
      END FUNCTION UserFunction
    END INTERFACE
    INTEGER, EXTERNAL :: UserDisplay
    TYPE ( SAGgrid_str ),     POINTER      :: grd
    LOGICAL,                               INTENT( IN ) :: use8
  END FUNCTION SAG_TriangleFunction

  INTEGER FUNCTION SAG_UserContour( contour,UserFunction,grd )
    USE sagcnt_fd
    USE sagstr_fd
    TYPE( SAGcontour_str ), INTENT( IN ) :: contour
    INTEGER, EXTERNAL                    :: UserFunction
    TYPE ( SAGgrid_str ),     POINTER      :: grd
  END FUNCTION SAG_UserContour
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_DrawField = SAG_ERROR

!==============================================================================
! Check for contour definitions
!==============================================================================
IF (draw%nlev == 0 .OR. .NOT.ASSOCIATED(draw%iplev) )THEN
  LastError = SAG_ERR_NOLEV
  GOTO 9999
END IF

!==============================================================================
! Set Triangle building flag
!==============================================================================
needTriangles = draw%FillContour .OR. draw%DrawContour .OR. Draw%lAreaTri

!==============================================================================
! Build triangles if needed and not already available
!==============================================================================
IF( needTriangles )THEN
  nfld    = 1
  ifld(1) = draw%ifld
  IF( .NOT.ASSOCIATED(triT%iptri) )THEN
    irv = SAG_Triangles( grd,nfld,ifld,.TRUE.,triT )
    IF( irv /= SAG_OK )THEN
      CALL SAG_FreeTriangle( triT )
      GOTO 9999
    END IF
  ELSE
    node => triT%nodeT
    irv = SAG_Nodes( grd,nfld,ifld,node )
    IF( irv /= SAG_OK )THEN
      GOTO 9999
    END IF
  END IF
END IF

!==============================================================================
! Fill Contours
!==============================================================================
IF( draw%FillContour )THEN

  irv = SAG_TriangleFunction( triT,SAG_TriangleFill,UserFill,grd,.FALSE. )
  IF( irv /= SAG_OK )THEN
    GOTO 9999
  END IF

END IF

!==============================================================================
! Draw Contours
!==============================================================================
IF( draw%DrawContour )THEN

  contour%mxpts = 0
  contour%mxlns = 0
  NULLIFY(contour%ippts)
  NULLIFY(contour%iplns)

  DO ic = draw%start,draw%stop

    CALL SAG_InitContourDraw( contour,draw,1,ic )

    irv = SAG_BuildContour( triT,contour )
    IF( irv /= SAG_OK )THEN
      GOTO 9998
    END IF

    node => triT%nodeT

    irv = SAG_UserContour( contour,UserDraw,grd )
    IF( irv /= SAG_OK )THEN
      GOTO 9998
    END IF

  END DO

END IF

!==============================================================================
! Pop/Area Calculations
!==============================================================================
!   Areal Calculation
!==============================================================================
IF( draw%lAreaTri )THEN

  threadID = OMP_GET_THREAD_NUM()
  irv = SAG_TriangleFunction( triT,SAG_TriangleFill,UserArea,grd,.TRUE. )
  IF( irv /= SAG_OK )THEN
    GOTO 9998
  END IF

!==============================================================================
!   Expected Calculation
!==============================================================================
ELSE IF( draw%lAreaCell )THEN

  nfld     = 2
  ifld(1)  = 1
  ifld(2)  = 2
  irv = SAG_BottomFunction( grd,UserArea,nfld,ifld,.FALSE.,.TRUE. )
  IF( irv /= SAG_OK )THEN
    GOTO 9998
  END IF

END IF

!==============================================================================
! Set return value
!==============================================================================
SAG_DrawField = SAG_OK

!==============================================================================
! Clean up - Free contour space
!==============================================================================
9998 CONTINUE
IF( draw%DrawContour )CALL SAG_FreeContour( contour )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!               SAG_DrawCell
!*******************************************************************************
INTEGER FUNCTION SAG_DrawCell( grd,UserDraw )

USE sagstr_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE ( SAGgrid_str ), POINTER  :: grd      !SAG grid structure
INTEGER,              EXTERNAL :: UserDraw !User Draw function

!==============================================================================
! Function calls
!==============================================================================
INTERFACE
  INTEGER FUNCTION SAG_GridFunction( grd,UserFunction,UserDisplay )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER  :: grd
    INTEGER,             EXTERNAL :: UserFunction
    INTEGER,             EXTERNAL :: UserDisplay
  END FUNCTION SAG_GridFunction
END INTERFACE

INTEGER, EXTERNAL :: SAG_CellDraw

!==============================================================================
! Call Cell function
!==============================================================================
SAG_DrawCell = SAG_GridFunction( grd,SAG_CellDraw,UserDraw )

RETURN
END
!*******************************************************************************
!               SAG_DrawTriangle
!*******************************************************************************
INTEGER FUNCTION SAG_DrawTriangle( triT,UserDraw,grd )

USE sagtri_fd
USE sagstr_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE ( SAGtriangleT_str ), POINTER :: triT     !SAG triangle structure
INTEGER, EXTERNAL                  :: UserDraw !User Draw function
TYPE( SAGgrid_str ),       POINTER :: grd

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: SAG_TriangleDraw
INTERFACE
  INTEGER FUNCTION SAG_TriangleFunction( triT,UserFunction,UserDisplay,grd,use8 )
    USE sagtri_fd
    USE sagstr_fd
    TYPE( SAGtriangleT_str ), POINTER :: triT
    INTERFACE
      INTEGER FUNCTION UserFunction( nid,node,dat,nfld,UserDisplay,grd,use8 )
        USE sagnod_fd
        USE sagstr_fd
        INTEGER,                           INTENT( IN ) :: nfld
        INTEGER,             DIMENSION(3), INTENT( IN ) :: nid
        TYPE( SAGnode_str ), DIMENSION(:), POINTER      :: node
        REAL,                DIMENSION(:), POINTER      :: dat
        INTEGER, EXTERNAL :: UserDisplay
        TYPE( SAGgrid_str ), POINTER  :: grd
        LOGICAL,                           INTENT( IN ) :: use8
      END FUNCTION UserFunction
    END INTERFACE
    INTEGER, EXTERNAL :: UserDisplay
    TYPE( SAGgrid_str ),      POINTER :: grd
    LOGICAL,                               INTENT( IN ) :: use8
  END FUNCTION SAG_TriangleFunction
END INTERFACE

!==============================================================================
! Call Triangle function
!==============================================================================
SAG_DrawTriangle = SAG_TriangleFunction( triT,SAG_TriangleDraw,UserDraw,grd,.FALSE. )

RETURN
END


