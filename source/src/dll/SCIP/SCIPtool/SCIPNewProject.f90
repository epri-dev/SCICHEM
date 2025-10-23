!*******************************************************************************
!            Create a Project
!*******************************************************************************
INTEGER FUNCTION SCIPNewProject( UserID,createNew,mtlList,relList )

USE SCIMgr_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNewProject

INTEGER,                         INTENT( IN )    :: UserID !USER ID Tag
TYPE( createNewT ),              INTENT( INOUT ) :: createNew !Project ID
TYPE( materialT ),DIMENSION(*),  INTENT( INOUT ) :: mtlList !Material list
TYPE( releaseT ), DIMENSION(*),  INTENT( INOUT ) :: relList !Release list

INTERFACE
  INTEGER FUNCTION NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite )
    USE SCIMgr_fd
    INTEGER,                                    INTENT( IN    ) :: UserID    !USER ID Tag
    TYPE( createNewT ),                 TARGET, INTENT( INOUT ) :: createNew !Project ID
    TYPE( materialT ),  DIMENSION(*),   TARGET, INTENT( INOUT ) :: mtlList   !Material list
    TYPE( releaseT ),   DIMENSION(*),   TARGET, INTENT( INOUT ) :: relList   !Release list
    INTEGER,                          OPTIONAL, INTENT( IN    ) :: nMC       !Size of relMCList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN    ) :: relMCList !Release multicomponent list
    LOGICAL,                          OPTIONAL, INTENT( IN    ) :: lWrite    !Write input files
  END FUNCTION NewProject
END INTERFACE

SCIPNewProject = NewProject( UserID,createNew,mtlList,relList )

RETURN
END
!*******************************************************************************
!            Create a Multicomponent Project
!*******************************************************************************
INTEGER FUNCTION SCIPNewProjectMC( UserID,createNew,mtlList,relList,nMC,relMCList )

USE SCIMgr_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNewProjectMC

INTEGER,                          INTENT( IN    ) :: UserID !USER ID Tag
TYPE( createNewT ),               INTENT( INOUT ) :: createNew !Project ID
TYPE( materialT ),  DIMENSION(*), INTENT( INOUT ) :: mtlList !Material list
TYPE( releaseT ),   DIMENSION(*), INTENT( INOUT ) :: relList !Release list
INTEGER,                          INTENT( IN    ) :: nMC       !Size of relMCList
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN    ) :: relMCList !Release multicomponent list

INTERFACE
  INTEGER FUNCTION NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite )
    USE SCIMgr_fd
    INTEGER,                                    INTENT( IN    ) :: UserID    !USER ID Tag
    TYPE( createNewT ),                 TARGET, INTENT( INOUT ) :: createNew !Project ID
    TYPE( materialT ),  DIMENSION(*),   TARGET, INTENT( INOUT ) :: mtlList   !Material list
    TYPE( releaseT ),   DIMENSION(*),   TARGET, INTENT( INOUT ) :: relList   !Release list
    INTEGER,                          OPTIONAL, INTENT( IN    ) :: nMC       !Size of relMCList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN    ) :: relMCList !Release multicomponent list
    LOGICAL,                          OPTIONAL, INTENT( IN    ) :: lWrite    !Write input files
  END FUNCTION NewProject
END INTERFACE

SCIPNewProjectMC = NewProject( UserID,createNew,mtlList,relList,nMC,relMCList )

RETURN
END
!*******************************************************************************
!            Create a Process Project
!*******************************************************************************
INTEGER FUNCTION ProcessNewProject( UserID,createNew,mtlList,relList )

USE SCIMgr_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: ProcessNewProject

INTEGER,                         INTENT( IN    ) :: UserID !USER ID Tag
TYPE( createNewT ),              INTENT( INOUT ) :: createNew !Project ID
TYPE( materialT ),DIMENSION(*),  INTENT( INOUT ) :: mtlList !Material list
TYPE( releaseT ), DIMENSION(*),  INTENT( INOUT ) :: relList !Release list

INTERFACE
  INTEGER FUNCTION NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite )
    USE SCIMgr_fd
    INTEGER,                                    INTENT( IN    ) :: UserID    !USER ID Tag
    TYPE( createNewT ),                 TARGET, INTENT( INOUT ) :: createNew !Project ID
    TYPE( materialT ),  DIMENSION(*),   TARGET, INTENT( INOUT ) :: mtlList   !Material list
    TYPE( releaseT ),   DIMENSION(*),   TARGET, INTENT( INOUT ) :: relList   !Release list
    INTEGER,                          OPTIONAL, INTENT( IN    ) :: nMC       !Size of relMCList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN    ) :: relMCList !Release multicomponent list
    LOGICAL,                          OPTIONAL, INTENT( IN    ) :: lWrite    !Write input files
  END FUNCTION NewProject
END INTERFACE

ProcessNewProject = NewProject( UserID,createNew,mtlList,relList,lWrite=.FALSE. )

RETURN
END
!*******************************************************************************
!            Create a Multicomponent Project
!*******************************************************************************
INTEGER FUNCTION ProcessNewProjectMC( UserID,createNew,mtlList,relList,nMC,relMCList )

USE SCIMgr_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: ProcessNewProjectMC

INTEGER,                          INTENT( IN    ) :: UserID !USER ID Tag
TYPE( createNewT ),               INTENT( INOUT ) :: createNew !Project ID
TYPE( materialT ),  DIMENSION(*), INTENT( INOUT ) :: mtlList !Material list
TYPE( releaseT ),   DIMENSION(*), INTENT( INOUT ) :: relList !Release list
INTEGER,                          INTENT( IN    ) :: nMC       !Size of relMCList
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN    ) :: relMCList !Release multicomponent list

INTERFACE
  INTEGER FUNCTION NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite )
    USE SCIMgr_fd
    INTEGER,                                    INTENT( IN    ) :: UserID    !USER ID Tag
    TYPE( createNewT ),                 TARGET, INTENT( INOUT ) :: createNew !Project ID
    TYPE( materialT ),  DIMENSION(*),   TARGET, INTENT( INOUT ) :: mtlList   !Material list
    TYPE( releaseT ),   DIMENSION(*),   TARGET, INTENT( INOUT ) :: relList   !Release list
    INTEGER,                          OPTIONAL, INTENT( IN    ) :: nMC       !Size of relMCList
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN    ) :: relMCList !Release multicomponent list
    LOGICAL,                          OPTIONAL, INTENT( IN    ) :: lWrite    !Write input files
  END FUNCTION NewProject
END INTERFACE

ProcessNewProjectMC = NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite=.FALSE. )

RETURN
END
