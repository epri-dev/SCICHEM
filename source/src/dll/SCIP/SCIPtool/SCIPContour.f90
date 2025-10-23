!*******************************************************************************
!                SCIPContourCount
!*******************************************************************************
INTEGER FUNCTION SCIPContourCount( UserID,grdI,Field,PlotType,contourHead, &
                                   contourList,Mode,nLine,nPoint )

USE field_fd
USE contourlist_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPContourCount

INTEGER,                                                            INTENT( IN  ) :: UserID       !USER ID tag
INTEGER,                                                            INTENT( IN  ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN  ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN  ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN  ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( IN  ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN  ) :: Mode         !Contour Mode
INTEGER,                                                            INTENT( OUT ) :: nLine        !Total number of lines
INTEGER,                                                            INTENT( OUT ) :: nPoint       !Total number of points

INTERFACE
  INTEGER FUNCTION ContourCountF( CallerID,grdID,Field,PlotType,contourHead, &
                                  contourList,Mode,nLine,nPoint )
    USE tooluser_fd
    INTEGER,                                     INTENT( IN  ) :: CallerID     !USER ID tag
    INTEGER,                                     INTENT( IN  ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                  INTENT( IN  ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                                 INTENT( IN  ) :: contourList  !Contour array
    INTEGER,                                     INTENT( IN  ) :: Mode         !Contour Mode
    INTEGER,                                     INTENT( OUT ) :: nLine        !Total number of lines
    INTEGER,                                     INTENT( OUT ) :: nPoint       !Total number of points
  END FUNCTION ContourCountF
END INTERFACE

SCIPContourCount = ContourCountF( UserID,grdI,Field,PlotType,contourHead, &
                                   contourList,Mode,nLine,nPoint )

RETURN
END
!*******************************************************************************
!                SCIPContourField
!*******************************************************************************
INTEGER FUNCTION SCIPContourField( UserID,grdI,Field,PlotType,contourHead, &
                                   contourList,Mode,Line,Point )

USE field_fd
USE contourlist_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPContourField

INTEGER,                                                            INTENT( IN    ) :: UserID       !USER ID tag
INTEGER,                                                            INTENT( IN    ) :: grdI         !SAG grid ID
TYPE( SCIPPlotFieldT ),                                             INTENT( IN    ) :: Field        !Field definition
TYPE( SCIPPlotTypeT ),                                              INTENT( IN    ) :: PlotType     !Plot definition
TYPE( SCIPContourHeaderT ),                                         INTENT( IN    ) :: contourHead  !Contour array header
TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, INTENT( INOUT ) :: contourList  !Contour array
INTEGER,                                                            INTENT( IN    ) :: Mode         !Contour Mode
TYPE( SCIPLineT  ), DIMENSION(*),                                   INTENT( OUT   ) :: Line         !Lines output array
TYPE( SCIPPointT ), DIMENSION(*),                                   INTENT( OUT   ) :: Point        !Points output array

INTERFACE
  INTEGER FUNCTION ContourFieldF( CallerID,grdID,Field,PlotType,contourHead, &
                                  contourList,Mode,Line,Point )
    USE tooluser_fd
    INTEGER,                                   INTENT( IN    ) :: CallerID     !USER ID tag
    INTEGER,                                   INTENT( IN    ) :: grdID        !SAG grid ID
    TYPE( SCIPPlotFieldT ),                    INTENT( IN    ) :: Field        !Field descriptor
    TYPE( SCIPPlotTypeT ),                     INTENT( IN    ) :: PlotType     !Plot definition
    TYPE( SCIPContourHeaderT ),                INTENT( IN    ) :: contourHead  !Contour array header
    TYPE( SCIPContourElementT ), DIMENSION(contourHead%number), TARGET, &
                                               INTENT( INOUT ) :: contourList  !Contour array
    INTEGER,                                   INTENT( IN    ) :: Mode         !Contour Mode
    TYPE( SCIPLineT  ), DIMENSION(*),          INTENT( OUT   ) :: Line         !Lines output array
    TYPE( SCIPPointT ), DIMENSION(*),          INTENT( OUT   ) :: Point        !Points output array
  END FUNCTION ContourFieldF
END INTERFACE

SCIPContourField = ContourFieldF( UserID,grdI,Field,PlotType,contourHead, &
                                  contourList,Mode,Line,Point )

RETURN
END
