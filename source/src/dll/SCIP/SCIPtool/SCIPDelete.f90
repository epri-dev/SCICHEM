!*******************************************************************************
!            SCIPDeleteProject
!*******************************************************************************
INTEGER FUNCTION SCIPDeleteProject( UserID,project,request )

USE prjstruct_fd

!     Deletes SCIP project files

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDeleteProject

INTEGER,             INTENT( IN ) :: UserID !USER ID tag
TYPE ( projectIDT ), INTENT( IN ) :: project !Project ID
INTEGER,             INTENT( IN ) :: request !Delete instructions

INTEGER, EXTERNAL :: DeleteProject

SCIPDeleteProject = DeleteProject( UserID,project,request )

RETURN
END
