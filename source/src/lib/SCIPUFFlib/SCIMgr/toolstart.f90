!*******************************************************************************
!            Load Project Start Input
!*******************************************************************************
INTEGER FUNCTION Load_Start( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pstartT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Start = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadStart( tool%start )

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL ReadStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadStart( tool%start )

!==== Return

Load_Start = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Start Input
!*******************************************************************************
INTEGER FUNCTION Write_Start( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pstartT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Start = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadStart( tool%start )

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL WriteStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Start = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Start Input
!*******************************************************************************
INTEGER FUNCTION Default_Start( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( startT ), INTENT( OUT ) :: tool

Default_Start = SCIPfailure

tool%zone           = NOT_SET_R
tool%time%reference = HT_UTC
tool%time%year      = NOT_SET_I
tool%time%month     = NOT_SET_I
tool%time%day       = NOT_SET_I
tool%time%hour      = 0.0
tool%time%runTime   = 0.0

Default_Start = SCIPsuccess

RETURN
END
