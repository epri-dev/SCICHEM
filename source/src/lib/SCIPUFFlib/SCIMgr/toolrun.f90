!*******************************************************************************
!            Load Project Run Input
!*******************************************************************************
INTEGER FUNCTION Load_Run( tool )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pendT ), INTENT( INOUT ) :: tool

INTEGER, EXTERNAL :: Load_EndX

Load_Run = Load_EndX( tool,TRUE )

RETURN
END
!*******************************************************************************
!            Write Project Run Input
!*******************************************************************************
INTEGER FUNCTION Write_Run( tool )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pendT ), INTENT( IN ) :: tool

TYPE( pctrlT ) ctrl

INTEGER, EXTERNAL :: Write_CtrlX
INTEGER, EXTERNAL :: Write_EndX

ctrl%project = tool%project
Write_Run = Write_CtrlX( ctrl,TRUE )

IF( Write_Run == SCIPsuccess )Write_Run = Write_EndX( tool,TRUE )

RETURN
END

