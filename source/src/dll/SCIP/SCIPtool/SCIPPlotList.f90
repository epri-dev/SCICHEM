INTEGER FUNCTION SCIPNumPlotClasses( userID,Project,nClass,nChoice,nKind )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumPlotClasses

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: nClass, nChoice, nKind

INTEGER, EXTERNAL :: NumPlotClasses

SCIPNumPlotClasses = NumPlotClasses( userID,Project,nClass,nChoice,nKind )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetPlotClasses( userID,Project,ClassStr,ChoiceStr,KindStr, &
                                     CatClassArray,ClassChoiceArray,ProjectCoordinate )

!------ Set plot choice allocation requirements

USE plotlist_fd
USE charT_fd
USE prjstruct_fd
USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetPlotClasses

INTEGER,                                            INTENT( IN  ) :: userID            !USER ID tag
TYPE( projectIDT ),                                 INTENT( IN  ) :: Project           !Project ID
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ClassStr          !Class strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ChoiceStr         !Choice strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: KindStr           !Kind strings
TYPE( SCIPCategoryClassT ),  DIMENSION(HP_NUMCAT,*),INTENT( OUT ) :: CatClassArray     !Class/Category use array
TYPE( SCIPClassChoiceT ),    DIMENSION(*),          INTENT( OUT ) :: ClassChoiceArray  !Class/Choice use array
TYPE( SCIPFieldCoordinateT ),                       INTENT( OUT ) :: ProjectCoordinate !Project coordinate descriptor

INTEGER, EXTERNAL :: GetPlotClasses

SCIPGetPlotClasses = GetPlotClasses( userID,Project,ClassStr,ChoiceStr,KindStr, &
                                     CatClassArray,ClassChoiceArray,ProjectCoordinate )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPNumPlotTimes( userID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumPlotTimes

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN )  :: Project
INTEGER,            INTENT( OUT ) :: nTimePuff, nTimeSrf, nTimeMet
INTEGER,            INTENT( OUT ) :: nNotUsed

INTEGER, EXTERNAL :: NumPlotTimes

SCIPNumPlotTimes = NumPlotTimes( userID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetPlotTimes( userID,project,TimePuff,TimeSrf,TimeMet )

!------ Set plot choice allocation requirements

USE prjstruct_fd
USE plotlist_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetPlotTimes

INTEGER,                       INTENT( IN  ) :: userID
TYPE( projectIDT ),            INTENT( IN  ) :: Project
TYPE( SCIPTimeT ), DIMENSION(*), INTENT( OUT ) :: TimePuff, TimeSrf, TimeMet

INTEGER, EXTERNAL :: GetPlotTimes

SCIPGetPlotTimes = GetPlotTimes( userID,project,TimePuff,TimeSrf,TimeMet )

RETURN
END

