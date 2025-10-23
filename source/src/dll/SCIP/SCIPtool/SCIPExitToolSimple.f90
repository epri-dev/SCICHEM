!*******************************************************************************
!            Exit SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SCIPExitToolSimple()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPExitToolSimple

INTEGER, EXTERNAL :: SimpleExitTool

SCIPExitToolSimple = SimpleExitTool()

RETURN
END
