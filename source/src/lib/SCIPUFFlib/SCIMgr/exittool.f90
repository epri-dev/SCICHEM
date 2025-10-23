!*******************************************************************************
!            Exit SCIP Tool
!*******************************************************************************
INTEGER FUNCTION ExitTool()

USE SCIMgr_fd
USE files_fi
USE SCIMgr_fi
USE search_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER irv
INTEGER currentState

LOGICAL lrv

INTEGER, EXTERNAL :: ExitPlotTool, SWIMexit, SimpleExitTool

ExitTool = SCIPfailure

irv = SimpleExitTool()

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN !Available only while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== Exit SWIM

irv = SWIMexit()

!==== Clear SAG linked list

irv = ExitPlotTool()

!==== Deallocate memory

CALL scipuff_deallocate()

!==== Set return value

ExitTool = SCIPsuccess

irv = SCIMgrSetState( currentState )

RETURN
END
