!*******************************************************************************
!            Load Project Release Input
!*******************************************************************************
INTEGER FUNCTION Load_Release( tool,relList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),              INTENT( INOUT ) :: tool
TYPE( releaseT ), DIMENSION(*), INTENT( OUT   ) :: relList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(128)            searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTERFACE
  SUBROUTINE ReadRelease( file,lunit,searchID,scnHead,relList,relMCList )
    USE list_fd
    USE release_fd
    CHARACTER(*),                   INTENT( IN  )    :: file
    INTEGER,                        INTENT( IN  )    :: lunit
    CHARACTER(*),                   INTENT( IN  )    :: searchID
    TYPE( listHeadT ),              INTENT( INOUT  ) :: scnHead
    TYPE( releaseT ), DIMENSION(*), INTENT( OUT )    :: relList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( OUT ) :: relMCList
  END SUBROUTINE ReadRelease
END INTERFACE

!==== Initialize

Load_Release = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in ReadRelease

!==== Read namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = tool%control%searchID
  IF( BTEST(tool%control%mode,HCB_REPLACE) )searchID = '!'//TRIM(searchID)
ELSE
  searchID = ' '
  IF( .NOT.BTEST(tool%control%mode,HCB_APPEND) )tool%scnHead%number = 0
END IF

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL ReadRelease( filename,lun_tmp,searchID,tool%scnHead,relList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_Release = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Load Project Multicomponent Release Input
!*******************************************************************************
INTEGER FUNCTION Load_ReleaseMC( tool,relList,relMCList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),                INTENT( INOUT ) :: tool
TYPE( releaseT ),   DIMENSION(*), INTENT( OUT   ) :: relList
TYPE( releaseMCT ), DIMENSION(*), INTENT( OUT   ) :: relMCList

CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(128)            searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTERFACE
  SUBROUTINE ReadRelease( file,lunit,searchID,scnHead,relList,relMCList )
    USE list_fd
    USE release_fd
    CHARACTER(*),                   INTENT( IN  )    :: file
    INTEGER,                        INTENT( IN  )    :: lunit
    CHARACTER(*),                   INTENT( IN  )    :: searchID
    TYPE( listHeadT ),              INTENT( INOUT  ) :: scnHead
    TYPE( releaseT ), DIMENSION(*), INTENT( OUT )    :: relList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( OUT ) :: relMCList
  END SUBROUTINE ReadRelease
END INTERFACE

!==== Initialize

Load_ReleaseMC = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in ReadRelease

!==== Read namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = tool%control%searchID
  IF( BTEST(tool%control%mode,HCB_REPLACE) )searchID = '!'//TRIM(searchID)
ELSE
  searchID = ' '
  IF( .NOT.BTEST(tool%control%mode,HCB_APPEND) )tool%scnHead%number = 0
END IF

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL ReadRelease( filename,lun_tmp,searchID,tool%scnHead,relList,relMCList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Load_ReleaseMC = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Release Input
!*******************************************************************************
INTEGER FUNCTION Write_Release( tool,relList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),              INTENT( IN ) :: tool
TYPE( releaseT ), DIMENSION(*), INTENT( IN ) :: relList

LOGICAL append
CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(20)             searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTERFACE
  SUBROUTINE WriteRelease( file,lunit,append,searchID,scnHead,relList,nMC,relMCList )
    USE list_fd
    USE release_fd
    CHARACTER(*),                   INTENT( IN  )    :: file
    INTEGER,                        INTENT( IN  )    :: lunit
    LOGICAL,                        INTENT( IN  )    :: append
    CHARACTER(*),                   INTENT( IN  )    :: searchID
    TYPE( listHeadT ),              INTENT( IN  )    :: scnHead
    TYPE( releaseT ), DIMENSION(*), INTENT( IN  )    :: relList
    INTEGER,                          OPTIONAL ,INTENT( IN ) :: nMC
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN ) :: relMCList
  END SUBROUTINE WriteRelease
END INTERFACE

!==== Initialize

Write_Release = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in WriteRelease

!==== Write namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = TRIM(tool%control%searchID)
ELSE
  searchID = ' '
END IF

append = BTEST(tool%control%mode,HCB_APPEND)

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL WriteRelease( filename,lun_tmp,append,searchID,tool%scnHead,relList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Release = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Release Input
!*******************************************************************************
INTEGER FUNCTION Write_ReleaseMC( tool,relList,nMC,relMCList )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( preleaseT ),                INTENT( IN ) :: tool
TYPE( releaseT ), DIMENSION(*),   INTENT( IN ) :: relList
INTEGER,                          INTENT( IN ) :: nMC
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN ) :: relMCList

LOGICAL append
CHARACTER(PATH_MAXLENGTH) filename
CHARACTER(20)             searchID

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTERFACE
  SUBROUTINE WriteRelease( file,lunit,append,searchID,scnHead,relList,nMC,relMCList )
    USE list_fd
    USE release_fd
    CHARACTER(*),                   INTENT( IN )    :: file
    INTEGER,                        INTENT( IN )    :: lunit
    LOGICAL,                        INTENT( IN )    :: append
    CHARACTER(*),                   INTENT( IN )    :: searchID
    TYPE( listHeadT ),              INTENT( IN )    :: scnHead
    TYPE( releaseT ), DIMENSION(*), INTENT( IN )    :: relList
    INTEGER,                          OPTIONAL ,INTENT( IN ) :: nMC
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN ) :: relMCList
  END SUBROUTINE WriteRelease
END INTERFACE

!==== Initialize

Write_ReleaseMC = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data
!     NOTE - For Releases this must be done as part of the loop in WriteRelease

!==== Write namelist

IF( BTEST(tool%control%mode,HCB_SEARCH) )THEN
  searchID = TRIM(tool%control%searchID)
ELSE
  searchID = ' '
END IF

append = BTEST(tool%control%mode,HCB_APPEND)

IF( BTEST(tool%control%mode,HCB_FILE) )THEN
  filename = TRIM(AddExtension( tool%project%name,tool%control%fileExtension ))
ELSE
  filename = TRIM(AddExtension( tool%project%name,'scn' ))
END IF
CALL AddPath( filename,tool%project%path )

CALL WriteRelease( filename,lun_tmp,append,searchID,tool%scnHead,relList,nMC,relMCList ) !Release list
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_ReleaseMC = SCIPsuccess

9999 CONTINUE

RETURN
END

