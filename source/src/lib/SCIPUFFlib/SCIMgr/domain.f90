!*******************************************************************************
!            Load Project Domain Input
!*******************************************************************************
INTEGER FUNCTION Load_Domain( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pspatialT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Domain = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadDomain( tool%spatial )

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL ReadDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from namelist results (SCIPUFF memory)

CALL LoadDomain( tool%spatial )

!==== Return

Load_Domain = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Domain Input
!*******************************************************************************
INTEGER FUNCTION Write_Domain( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pspatialT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Domain = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadDomain( tool%spatial )

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )
CALL WriteDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Domain = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Domain Input
!*******************************************************************************
INTEGER FUNCTION Default_Domain( tool )

USE SCIMgr_fd
USE default_fd

IMPLICIT NONE

TYPE( spatialT ), INTENT( OUT ) :: tool

Default_Domain = SCIPfailure

tool%domain%coord   = HD_LATLON
tool%domain%zoneUTM = NOT_SET_I
tool%domain%xMax    = DEF_VAL_R
tool%domain%xMin    = DEF_VAL_R
tool%domain%yMax    = DEF_VAL_R
tool%domain%yMin    = DEF_VAL_R
tool%domain%zMax    = 2500.
tool%domain%hRes    = DEF_VAL_R
tool%domain%vRes    = DEF_VAL_R
tool%reference%x    = NOT_SET_R
tool%reference%y    = NOT_SET_R
tool%reference%lat  = NOT_SET_R
tool%reference%lon  = NOT_SET_R

Default_Domain = SCIPsuccess

RETURN
END
