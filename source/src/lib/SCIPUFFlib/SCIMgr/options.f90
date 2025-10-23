!*******************************************************************************
!            Load Project Options Input
!*******************************************************************************
INTEGER FUNCTION Load_Options( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( poptionsT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Options = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadOptions( tool%option )

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL ReadOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadOptions( tool%option )

!==== Return

Load_Options = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Options Input
!*******************************************************************************
INTEGER FUNCTION Write_Options( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( poptionsT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Options = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadOptions( tool%option )

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL WriteOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Options = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Options Input
!*******************************************************************************
INTEGER FUNCTION Default_Options( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( optionsT ), INTENT( OUT ) :: tool

Default_Options = SCIPfailure

tool%nzBL            = 11
tool%mGrd            = 2
tool%substrate       = 0
tool%timeAvg         = DEF_VAL_R
tool%massMin         = 1.E-20
tool%delMin          = DEF_VAL_R
tool%wwTrop          = 0.01
tool%epsTrop         = 4.0E-4
tool%slTrop          = 10.0
tool%uuCalm          = 0.25
tool%slCalm          = 1000.
tool%zDosage         = 0.
tool%dtSampler       = DEF_VAL_R
tool%samplerFile     = ' '
tool%lOutputVariance = .FALSE.
Default_Options      = SCIPsuccess

RETURN
END
