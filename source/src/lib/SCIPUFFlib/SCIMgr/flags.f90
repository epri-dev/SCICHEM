!*******************************************************************************
!            Load Project Flags Input
!*******************************************************************************
INTEGER FUNCTION Load_Flags( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pflagsT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Flags = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadFlags( tool%flags )

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL ReadFlags( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadFlags( tool%flags )

!==== Return

Load_Flags = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Flags Input
!*******************************************************************************
INTEGER FUNCTION Write_Flags( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pflagsT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Flags = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadFlags( tool%flags )

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL WriteFlags( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Flags = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Flags Input
!*******************************************************************************
INTEGER FUNCTION Default_Flags( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( flagsT ), INTENT( OUT ) :: tool

TYPE( char128T ) charStruct

INTEGER irv

INTEGER, EXTERNAL :: GetVersionString

Default_Flags = SCIPfailure

tool%start         = HF_OFF
tool%method        = HF_STATIC
tool%mode          = HF_OFF
tool%prjEffects    = NOT_SET_I
tool%audit%title   = ' '
tool%audit%analyst = ' '
tool%audit%class   = ' '
irv = GetVersionString( 0,charStruct )
tool%audit%version = TRIM(charStruct%string)
tool%audit%date    = CHAR(0)

Default_Flags = SCIPsuccess

RETURN
END
