!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION Load_Inp( tool,mtlList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pinputT ),                 INTENT( INOUT ) :: tool
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Inp = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadCtrl( tool%input%ctrl )

CALL UnloadStart( tool%input%time%start )

CALL UnloadEnd( tool%input%time%end )

CALL UnloadFlags( tool%input%flags )

CALL UnloadDomain( tool%input%domain )

CALL UnloadOptions( tool%input%option )

CALL UnloadMaterial( tool%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Read namelist

searchName = ' '
filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )

CALL ReadCtrl( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadFlags( filename,lun_tmp)
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadMaterial( filename,lun_tmp,searchName ) !Material name
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadCtrl( tool%input%ctrl )

CALL LoadStart( tool%input%time%start )

CALL LoadEnd( tool%input%time%end )

CALL LoadFlags( tool%input%flags )

CALL LoadDomain( tool%input%domain )

CALL LoadOptions( tool%input%option )

CALL LoadMaterial( tool%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_Inp = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION Write_Inp( tool,mtlList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pinputT ),                 INTENT( IN ) :: tool
TYPE( materialT ), DIMENSION(*), INTENT( IN ) :: mtlList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

INTEGER extFlag

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Inp = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadCtrl( tool%input%ctrl )

CALL UnloadStart( tool%input%time%start )

CALL UnloadEnd( tool%input%time%end )

CALL UnloadFlags( tool%input%flags )

CALL UnloadDomain( tool%input%domain )

CALL UnloadOptions(tool%input%option)

CALL UnloadMaterial( tool%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Write namelist

extFlag = FALSE

searchName = ' '

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )

CALL WriteCtrl( filename,lun_tmp, extFlag )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteFlags( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteMaterial( filename,lun_tmp,searchName ) !Material name
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Inp = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Input
!*******************************************************************************
INTEGER FUNCTION Default_Inp( tool,mtlList )

USE SCIMgr_fd

IMPLICIT NONE

TYPE( pinputT ),                 INTENT( INOUT ) :: tool
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlList

INTEGER irv

INTEGER, EXTERNAL :: Default_Ctrl
INTEGER, EXTERNAL :: Default_Time
INTEGER, EXTERNAL :: Default_Flags
INTEGER, EXTERNAL :: Default_Domain
INTEGER, EXTERNAL :: Default_Options
INTEGER, EXTERNAL :: Default_Material

Default_Inp = SCIPfailure

irv = Default_Ctrl( tool%input%ctrl )
IF( irv /= SCIPsuccess )GOTO 9999

irv = Default_Time( tool%input%time )
IF( irv /= SCIPsuccess )GOTO 9999

irv = Default_Flags( tool%input%flags )
IF( irv /= SCIPsuccess )GOTO 9999

irv = Default_Domain( tool%input%domain )
IF( irv /= SCIPsuccess )GOTO 9999

irv = Default_Options( tool%input%option )
IF( irv /= SCIPsuccess )GOTO 9999

irv = Default_Material( tool%input%mtlHead,mtlList )
IF( irv /= SCIPsuccess )GOTO 9999

Default_Inp = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Load Project Special Restart Input
!*******************************************************************************
INTEGER FUNCTION Load_Rst( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pinputT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Rst = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadCtrl( tool%input%ctrl )

CALL UnloadEnd( tool%input%time%end )

CALL UnloadFlags( tool%input%flags )

CALL UnloadDomain( tool%input%domain )

CALL UnloadOptions( tool%input%option )

!==== Read namelist

searchName = ' '
filename = TRIM(AddExtension( tool%project%name,'sri' ))
CALL AddPath( filename,tool%project%path )

CALL ReadCtrl( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadFlags( filename,lun_tmp)
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadCtrl( tool%input%ctrl )

CALL LoadEnd( tool%input%time%end )

CALL LoadFlags( tool%input%flags )

CALL LoadDomain( tool%input%domain )

CALL LoadOptions( tool%input%option )

!==== Return

Load_Rst = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Special Restart Input
!*******************************************************************************
INTEGER FUNCTION Write_Rst( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( pinputT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(16)             searchName

INTEGER extFlag

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Rst = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadCtrl( tool%input%ctrl )

CALL UnloadEnd( tool%input%time%end )

CALL UnloadFlags( tool%input%flags )

CALL UnloadDomain( tool%input%domain )

CALL UnloadOptions(tool%input%option)

!==== Write namelist

extFlag = FALSE

searchName = ' '

filename = TRIM(AddExtension( tool%project%name,'sri' ))
CALL AddPath( filename,tool%project%path )

CALL WriteCtrl( filename,lun_tmp, extFlag )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteFlags( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteDomain( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteOptions( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Rst = SCIPsuccess

9999 CONTINUE

RETURN
END
