INTEGER FUNCTION InitPlotTool() RESULT( irv )

!------ Set plot choice allocation requirements

USE sagdef_fd
USE sagbck_fi
USE surface_fi
USE SCIMgr_fd
USE error_fi
USE scipuff_fi
USE sampler_fi
USE adjoint_fi
USE abort

IMPLICIT NONE

INTEGER, EXTERNAL :: SAG_InitList

CALL AbortInit()

CALL initClipNorm()
IF( nError /= NO_ERROR )THEN
  irv = SCIPfailure
  RETURN
END IF

irv = SAG_InitList()

IF( irv == SAG_OK )THEN
  irv = SCIPsuccess
ELSE
  irv = SCIPfailure
END IF

nBackUp = 0
srfdep  = 0
srfdos  = 0
nLocMax = 0

NULLIFY( nsmp_dep )
NULLIFY( nsmp_dos )

CALL InitReleaseSpec( currentSpec )

CALL InitReleaseSpec( InstReleaseList%relSpec )
NULLIFY( InstReleaseList%NextRelease )

srfados  = 0
NULLIFY( mat_mc%type )
NULLIFY( mat_mc%ID )

RETURN
END

!==============================================================================

INTEGER FUNCTION ExitPlotTool() RESULT( irv )

!------ Clear SAG linked list

USE sagdef_fd
USE SCIMgr_fd
USE adjoint_fi

IMPLICIT NONE

INTEGER, EXTERNAL :: SAG_ClearList

irv = SAG_ClearList()
IF( irv == SAG_OK )THEN
  irv = SCIPsuccess
ELSE
  irv = SCIPfailure
END IF

CALL exitClipNorm()

CALL exitAdjLocMax()

RETURN
END

!==============================================================================

INTEGER FUNCTION NumPlotClasses( userID,Project,nClass,nChoice,nKind )

!------ Set plot choice allocation requirements

USE error_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE abort

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: nClass, nChoice, nKind

INTEGER currentState,irv

!==== Initialize

NumPlotClasses = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

!==== Count classes

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL NumberPlotClasses( Project,nClass,nChoice,nKind )
  IF( nError == NO_ERROR )NumPlotClasses = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetPlotClasses( userID,Project,ClassStr,ChoiceStr,KindStr, &
                                 CatClassArray,ClassChoiceArray,ProjectCoordinate )

!------ Set plot choice allocation requirements

USE plotlist_fi
USE SCIMgr_fd
USE field_fd
USE SCIMgr_fi
USE scipuff_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,                                            INTENT( IN  ) :: userID            !USER ID tag
TYPE( projectIDT ),                                 INTENT( IN  ) :: Project           !Project ID
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ClassStr          !Class strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ChoiceStr         !Choice strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: KindStr           !Kind strings
TYPE( SCIPCategoryClassT ),  DIMENSION(HP_NUMCAT,*),INTENT( OUT ) :: CatClassArray     !Class/Category use array
TYPE( SCIPClassChoiceT ),    DIMENSION(*),          INTENT( OUT ) :: ClassChoiceArray  !Class/Choice use array
TYPE( SCIPFieldCoordinateT ),                       INTENT( OUT ) :: ProjectCoordinate !Project coordinate descriptor

INTEGER i, currentState

!==== initialize

GetPlotClasses = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

!==== Get classes

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL FindPlotClasses( Project,ClassStr,ChoiceStr,KindStr, &
                        CatClassArray,ClassChoiceArray,ProjectCoordinate )

  IF( nError == NO_ERROR )GetPlotClasses = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging( )

i = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION NumPlotTimes( userID,Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

!------ Set plot choice allocation requirements

USE scipuff_fi
USE plotlist_fi
USE prjstruct_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN )  :: Project
INTEGER,            INTENT( OUT ) :: nTimePuff, nTimeSrf, nTimeMet
INTEGER,            INTENT( OUT ) :: nNotUsed

INTEGER irv
INTEGER currentState

!==== initialize

NumPlotTimes = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

!==== count times

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL NumberPlotTimes( Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

  IF( nError == NO_ERROR )NumPlotTimes = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetPlotTimes( userID,project,TimePuff,TimeSrf,TimeMet )

!------ Set plot choice allocation requirements

USE error_fi
USE plotlist_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,                       INTENT( IN  ) :: userID
TYPE( projectIDT ),            INTENT( IN  ) :: Project
TYPE(SCIPTimeT), DIMENSION(*), INTENT( OUT ) :: TimePuff, TimeSrf, TimeMet

INTEGER irv
INTEGER currentState

!==== initialize

GetPlotTimes = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

!==== Get times

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL FindPlotTimes( project,TimePuff,TimeSrf,TimeMet )

  IF( nError == NO_ERROR )GetPlotTimes = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

SUBROUTINE NumberPlotClasses( Project,nClass,nChoice,nKind )

!------ Set plot choice allocation requirements

USE error_fi
USE plotlist_fi
USE prjstruct_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE abort

IMPLICIT NONE

TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: nClass, nChoice, nKind

!------ Read project file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( Project )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( Aborted() )GOTO 9999

!------ Build list of plot choices

CALL BuildPlotList()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

nClass = nPclassT; nChoice = nPchoiceT; nKind = nPkindT

9999 CONTINUE

IF( .NOT.MemoryField )THEN
  CALL deallocate_read_prj()
END IF

RETURN
END

!==============================================================================

SUBROUTINE FindPlotClasses( Project,ClassStr,ChoiceStr,KindStr, &
                            CatClassArray,ClassChoiceArray,ProjectCoordinate )

!------ Set plot choice allocation requirements

USE plotlist_fi
USE SCIMgr_fd
USE field_fd
USE SCIMgr_fi
USE scipuff_fi
USE abort

IMPLICIT NONE

TYPE( projectIDT ),                                 INTENT( IN  ) :: Project           !Project ID
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ClassStr          !Class strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: ChoiceStr         !Choice strings
TYPE( char64T ),             DIMENSION(*),          INTENT( OUT ) :: KindStr           !Kind strings
TYPE( SCIPCategoryClassT ),  DIMENSION(HP_NUMCAT,*),INTENT( OUT ) :: CatClassArray     !Class/Category use array
TYPE( SCIPClassChoiceT ),    DIMENSION(*),          INTENT( OUT ) :: ClassChoiceArray  !Class/Choice use array
TYPE( SCIPFieldCoordinateT ),                       INTENT( OUT ) :: ProjectCoordinate !Project coordinate descriptor

INTEGER i, j, ij

!------ Read project file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( Project )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( Aborted() )GOTO 9999

!------ Build list of plot choices

CALL BuildPlotList()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Copy out strings

DO i = 1,nPclassT
  ClassStr(i)%string = ClassString(i)
END DO

DO i = 1,nPchoiceT
  ChoiceStr(i)%string = ChoiceString(ChoiceOrder(i))
END DO

DO i = 1,nPkindT
  KindStr(i)%string = KindString(i)
END DO

IF( Aborted() )GOTO 9999

!------ Copy out plot combinations

DO j = 1,nPclassT
  DO i = 1,HP_NUMCAT
    CatClassArray(i,j) = CatClassComb(i,j)
  END DO
END DO

DO j = 1,nPchoiceT
  DO i = 1,nPclassT
    ij = (j-1)*nPclassT + i
    ClassChoiceArray(ij) = ClassChoiceComb(i,ChoiceOrder(j))
  END DO
END DO

IF( Aborted() )GOTO 9999

!------ Set project coordinate data

ProjectCoordinate%mode = lmap
SELECT CASE( ABS(ProjectCoordinate%mode) )
  CASE( HD_UTM )
    ProjectCoordinate%UTMZone       = utm_zone
    ProjectCoordinate%reference%x   = xref
    ProjectCoordinate%reference%y   = yref
    ProjectCoordinate%reference%lat = lat0
    ProjectCoordinate%reference%lon = lon0
  CASE( HD_CARTESIAN )
    ProjectCoordinate%UTMZone       = NOT_SET_I
    ProjectCoordinate%reference%x   = xref
    ProjectCoordinate%reference%y   = yref
    ProjectCoordinate%reference%lat = lat0
    ProjectCoordinate%reference%lon = lon0
  CASE DEFAULT
    ProjectCoordinate%UTMZone       = NOT_SET_I
    ProjectCoordinate%reference%x   = NOT_SET_R
    ProjectCoordinate%reference%y   = NOT_SET_R
    ProjectCoordinate%reference%lat = NOT_SET_R
    ProjectCoordinate%reference%lon = NOT_SET_R
END SELECT
ProjectCoordinate%vertSlice%resolution = NOT_SET_I
ProjectCoordinate%vertSlice%startPt%x  = NOT_SET_R
ProjectCoordinate%vertSlice%startPt%y  = NOT_SET_R
ProjectCoordinate%vertSlice%endPt      = ProjectCoordinate%vertSlice%startPt
ProjectCoordinate%horzSlice%height     = NOT_SET_R
ProjectCoordinate%horzSlice%mode       = NOT_SET_I

9999 CONTINUE

IF( .NOT.MemoryField )THEN
  CALL deallocate_read_prj()
END IF

RETURN
END

!==============================================================================

SUBROUTINE NumberPlotTimes( Project,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

!------ Set plot choice allocation requirements

USE scipuff_fi
USE plotlist_fi
USE prjstruct_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE abort

IMPLICIT NONE

TYPE( projectIDT ), INTENT( IN )  :: Project
INTEGER,            INTENT( OUT ) :: nTimePuff, nTimeSrf, nTimeMet
INTEGER,            INTENT( OUT ) :: nNotUsed

!------ Read project file

CALL ReadProject( Project )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Set internal counts

CALL SetPlotTimes()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Copy out no. of times

nTimePuff = nPuffTime
nTimeSrf  = nSrfTime
nNotUsed = 0
nTimeMet  = nMetTime

9999 CONTINUE

!------ Clean up Memory allocated

CALL deallocate_read_prj()
CALL ClearTimeLists()

RETURN
END

!==============================================================================

SUBROUTINE FindPlotTimes( project,TimePuff,TimeSrf,TimeMet )

!------ Set plot choice allocation requirements

USE error_fi
USE plotlist_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE abort

IMPLICIT NONE

TYPE( projectIDT ),            INTENT( IN  ) :: Project
TYPE(SCIPTimeT), DIMENSION(*), INTENT( OUT ) :: TimePuff, TimeSrf, TimeMet

INTEGER i

!------ Read project file

CALL ReadProject( Project )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Set internal counts

CALL SetPlotTimes()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Copy out time lists

DO i = 1,nPuffTime
  TimePuff(i) = PuffTime(i)
END DO

DO i = 1,nSrfTime
  TimeSrf(i) = SrfTime(i)
END DO

DO i = 1,nMetTime
  TimeMet(i) = MetTime(i)
END DO

9999 CONTINUE

!------ Clean up Memory allocated

CALL deallocate_read_prj()
CALL ClearTimeLists()

RETURN
END

!==============================================================================

SUBROUTINE SetPlotTimes()

!------ Set plot choice allocation requirements

USE scipuff_fi
USE files_fi
USE plotlist_fi
USE SCIMgr_fd
USE abort

IMPLICIT NONE

INTEGER, PARAMETER :: SURFACE_BLOCK_VERSION = 1300

TYPE( startT ) start

!------ Load start time for time conversions

CALL LoadStart( start )

IF( Aborted() )GOTO 9999

!------ Deallocate time lists if already allocated

CALL ClearTimeLists()

!------ Build list of puff times

CALL ReadPuffTime( start )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Build list of surface times

IF( ntyps > 0 )THEN

  CALL ReadSrfTime( lun_dep,file_dep,start )
  IF( nError /= NO_ERROR )GOTO 9999

ELSE IF( ntypd > 0 )THEN

  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    CALL ReadSrfTime( lun_dos,TRIM(file_dos)//'001',start )
  ELSE
    CALL ReadSrfTime( lun_dos,file_dos,start )
  END IF
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

  nSrfTime = 0

END IF

IF( Aborted() )GOTO 9999

!------ Build list of met times - really

CALL ReadMetTime( start )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
