INTEGER FUNCTION GetProjectPlotLists( thisProject )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

TYPE( projectIDT ):: thisProject

INTEGER irv

GetProjectPlotLists = SCIPfailure

irv = SCIPNumPlotClasses( callerID,thisProject,nClass,nChoice,nKind )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed retrieving number of project plot classes' )
  GOTO 9999
END IF

CALL allocatePlotLists()
IF( nError /= NO_ERROR )GOTO 9999

irv = SCIPGetPlotClasses( callerID,thisProject,ClassStr,ChoiceStr,KindStr, &
                          CatClassArray,ClassChoiceArray,ProjectCoordinate )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed retrieving project plot classes' )
  GOTO 9999
END IF

GetProjectPlotLists = SCIPsuccess

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION GetProjectPlotTimes( thisProject )

USE SCIPtool
USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

TYPE( projectIDT ):: thisProject

INTEGER irv

GetProjectPlotTimes = SCIPfailure

irv = SCIPNumPlotTimes( CallerID,thisProject,nTimePuff,nTimeSrf,nTimeMet,nNotUsed )

IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed retrieving number of project plot times' )
  GOTO 9999
END IF

CALL allocatePlotTimes()
IF( nError /= NO_ERROR )GOTO 9999

irv = SCIPGetPlotTimes( callerID,thisProject,TimePuff,TimeSrf,TimeMet )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed retrieving project plot times' )
  GOTO 9999
END IF


GetProjectPlotTimes = SCIPsuccess

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION GetProjectModes()

USE param_fd
USE SCIPtool
USE Extract_fi
USE GetTimes_fi

IMPLICIT NONE

INTEGER i, j
CHARACTER(128) string
INTEGER irv
TYPE( pFlagsT ) :: Flags

GetProjectModes = SCIPfailure

irv = SCIPDefaultFlagsF( callerID,flags )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed setting default flags for project' )
  GOTO 9999
END IF

Flags%project = Project
irv = SCIPLoadFlagsF( callerID,Flags )
IF( irv /= SCIPsuccess )THEN
  CALL toolError( 'Failed retrieving number of project plot classes' )
  GOTO 9999
END IF

ldynamic = BTEST(Flags%flags%mode,HFB_DYNAMIC)
lstatic  = BTEST(Flags%flags%mode,HFB_STATIC)
ldense   = BTEST(Flags%flags%mode,HFB_DENSE)
lReverse = BTEST(Flags%flags%mode,HFB_REVERSE)

string = 'Concentration'
j = LEN_TRIM(string)
DO i = 1,nClass
  IF( string(1:j) == ClassStr(i)%string(1:j) )THEN
    has3D = .TRUE.
    EXIT
  END IF
END DO

has3D = has3D .AND. (nTimePuff > 0)
has3D = has3D .AND. allow3D

GetProjectModes = SCIPsuccess

9999 CONTINUE

RETURN
END

