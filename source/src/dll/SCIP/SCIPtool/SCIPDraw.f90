!*******************************************************************************
!                SCIPDrawField
!*******************************************************************************
INTEGER FUNCTION SCIPDrawField( UserID,grdI,Field,PlotType,contourHead,contourList,GUIdraw, &
                                UserFill,UserDraw )

USE field_fd
USE contourlist_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDrawField

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
! External Interface
!==============================================================================
INTERFACE
  INTEGER FUNCTION DrawFieldFunction( CallerID,grdID,Field,PlotType,contourHead, &
                                      contourList,GUIdraw,UserFill,UserDraw )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
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
  END FUNCTION DrawFieldFunction
END INTERFACE

SCIPDrawField = DrawFieldFunction( UserID,grdI,Field,PlotType,contourHead,contourList,GUIdraw, &
                                   UserFill,UserDraw )

RETURN
END
!*******************************************************************************
!                SCIPDrawGrid
!*******************************************************************************
INTEGER FUNCTION SCIPDrawGrid( UserID,grdI,UserDraw,Mode )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDrawGrid

INTEGER, INTENT( IN ) :: UserID       !USER ID tag
INTEGER, INTENT( IN ) :: grdI         !SAG grid ID
INTEGER, EXTERNAL     :: UserDraw     !User supplied draw function
INTEGER, INTENT( IN ) :: mode         !Draw instructions

!------ External Functions

INTEGER, EXTERNAL :: DrawGrid

SCIPDrawGrid = DrawGrid( UserID,grdI,UserDraw,Mode )

RETURN
END
!*******************************************************************************
!                SCIPGetFieldMinMax
!*******************************************************************************
INTEGER FUNCTION SCIPGetFieldMinMax( UserID,grdI,PlotType, &
                                     dmin,dmax,fmin,fmax,dmn,dmx )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldMinMax

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,               INTENT( IN ) :: UserID    !User ID tag
INTEGER,               INTENT( IN ) :: grdI      !SAG grid ID
TYPE( SCIPPlotTypeT ), INTENT( IN ) :: PlotType  !Plot Definition
REAL,                  INTENT( OUT ):: dmin,dmax !Mean Min/Max
REAL,                  INTENT( OUT ):: fmin,fmax !Variance Min/Max
REAL,                  INTENT( OUT ):: dmn,dmx   !Plot Field Min/Max

INTEGER, EXTERNAL :: GetFieldMinMaxFunction

SCIPGetFieldMinMax = GetFieldMinMaxFunction( UserID,grdI,PlotType, &
                                             dmin,dmax,fmin,fmax,dmn,dmx )

RETURN
END

