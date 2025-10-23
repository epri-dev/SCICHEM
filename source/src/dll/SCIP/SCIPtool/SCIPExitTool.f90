!*******************************************************************************
!            Exit SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SCIPExitTool()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPExitTool

INTEGER, EXTERNAL :: ExitTool

SCIPExitTool = ExitTool()

RETURN
END
