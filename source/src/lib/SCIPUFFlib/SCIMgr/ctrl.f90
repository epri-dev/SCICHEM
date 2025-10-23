!*******************************************************************************
!            Load Project Ctrl Input
!*******************************************************************************
INTEGER FUNCTION Load_Ctrl( tool)

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pctrlT ), INTENT( INOUT ) :: tool

INTEGER, EXTERNAL :: Load_CtrlX

Load_Ctrl = Load_CtrlX( tool,FALSE )

RETURN
END
!*******************************************************************************
!            Load Project Ctrl Input
!*******************************************************************************
INTEGER FUNCTION Load_CtrlX( tool,extFlag )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,        INTENT( IN    ) :: extFlag
TYPE( pctrlT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_CtrlX = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

IF( extFlag == FALSE )CALL UnloadCtrl( tool%ctrl )

!==== Read namelist

IF( extFlag == FALSE )THEN
  filename = TRIM(AddExtension( tool%project%name,'inp' ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'rst' ))
END IF
CALL AddPath( filename,tool%project%path )
CALL ReadCtrl( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

IF( extFlag == FALSE )CALL LoadCtrl( tool%ctrl )

!==== Return

Load_CtrlX = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Ctrl Input
!*******************************************************************************
INTEGER FUNCTION Write_Ctrl( tool )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pctrlT ), INTENT( IN ) :: tool

INTEGER, EXTERNAL :: Write_CtrlX

Write_Ctrl = Write_CtrlX( tool,FALSE )

RETURN
END
!*******************************************************************************
!            Write Project Ctrl Input
!*******************************************************************************
INTEGER FUNCTION Write_CtrlX( tool,extFlag )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pctrlT ), INTENT( IN ) :: tool
INTEGER,        INTENT( IN ) :: extFlag

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_CtrlX = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

IF( extFlag == FALSE )CALL UnloadCtrl( tool%ctrl )

!==== Write namelist

IF( extFlag == FALSE )THEN
  filename = TRIM(AddExtension( tool%project%name,'inp' ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'rst' ))
END IF
CALL AddPath( filename,tool%project%path )
CALL WriteCtrl( filename,lun_tmp,extFlag )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_CtrlX = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Ctrl Input
!*******************************************************************************
INTEGER FUNCTION Default_Ctrl( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( ctrlT ), INTENT( OUT ) :: tool

Default_Ctrl = SCIPfailure

tool%runTime = NOT_SET_R
tool%name    = ' '
tool%path    = ' '

Default_Ctrl = SCIPsuccess

RETURN

END

