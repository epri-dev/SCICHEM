!*******************************************************************************
!            Get Project Version
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectVersion( UserID,project )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectVersion

INTEGER,            INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectIDT ), INTENT( INOUT ) :: project !Project ID

INTEGER, EXTERNAL :: GetProjectVersion

SCIPGetProjectVersion = GetProjectVersion( UserID,project )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetProjectAudit( UserID,audit )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectAudit

INTEGER,         INTENT( IN    ) :: UserID !USER ID Tag
TYPE( pauditT ), INTENT( INOUT ) :: audit !Project ID

INTEGER, EXTERNAL :: GetProjectAudit

SCIPGetProjectAudit = GetProjectAudit( UserID,audit )

RETURN
END
