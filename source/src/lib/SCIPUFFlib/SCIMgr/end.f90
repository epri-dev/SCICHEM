!*******************************************************************************
!            Load Project End Input
!*******************************************************************************
INTEGER FUNCTION Load_End( tool )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pendT ), INTENT( INOUT ) :: tool

INTEGER, EXTERNAL :: Load_EndX

Load_End = Load_EndX( tool,FALSE )

RETURN
END
!*******************************************************************************
!            Load Project End Input
!*******************************************************************************
INTEGER FUNCTION Load_EndX( tool,extFlag )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pendT ), INTENT( INOUT ) :: tool
INTEGER,       INTENT( IN    ) :: extFlag

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_EndX = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadEnd( tool%end )

!==== Read namelist

IF( extFlag == FALSE )THEN
  filename = TRIM(AddExtension(tool%project%name,'inp'))
ELSE
  filename = TRIM(AddExtension(tool%project%name,'rst'))
END IF
CALL AddPath( filename,tool%project%path )
CALL ReadEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from namelist results (SCIPUFF memory)

CALL LoadEnd( tool%end )

!==== Return

Load_EndX = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project End Input
!*******************************************************************************
INTEGER FUNCTION Write_End( tool )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pendT ), INTENT( IN ) :: tool

INTEGER, EXTERNAL :: Write_EndX

Write_End = Write_EndX( tool,FALSE )

RETURN
END
!*******************************************************************************
!            Write Project End Input
!*******************************************************************************
INTEGER FUNCTION Write_EndX( tool,extFlag )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pendT ), INTENT( IN ) :: tool
INTEGER,       INTENT( IN ) :: extFlag

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_EndX = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadEnd( tool%end )

!==== Write namelist

IF( extFlag == FALSE )THEN
  filename = TRIM(AddExtension( tool%project%name,'inp' ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'rst' ))
END IF
CALL AddPath( filename,tool%project%path )
CALL WriteEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_EndX = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project End Input
!*******************************************************************************
INTEGER FUNCTION Default_End( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( endT ), INTENT( OUT ) :: tool

Default_End = SCIPfailure

tool%step%max       = 900.
tool%step%output    = 2.0
tool%time%reference = HT_UTC
tool%time%year      = NOT_SET_I
tool%time%month     = NOT_SET_I
tool%time%day       = NOT_SET_I
tool%time%hour      = 4.0
tool%time%runTime   = 4.0

Default_End = SCIPsuccess

RETURN
END
