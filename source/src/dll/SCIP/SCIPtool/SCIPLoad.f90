!*******************************************************************************
!            Load Project Ctrl
!*******************************************************************************
INTEGER FUNCTION SCIPLoadCtrlF( UserID,ctrlIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadCtrlF

INTEGER,        INTENT( IN    ) :: UserID
TYPE( pctrlT ), INTENT( INOUT ) :: ctrlIO

INTEGER, EXTERNAL :: LoadCtrlF

SCIPLoadCtrlF = LoadCtrlF( UserID,ctrlIO )

RETURN
END
!*******************************************************************************
!            Load Project Start
!*******************************************************************************
INTEGER FUNCTION SCIPLoadStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadStartF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pstartT ), INTENT( INOUT ) :: startIO

INTEGER, EXTERNAL :: LoadStartF

SCIPLoadStartF = LoadStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION SCIPLoadEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadEndF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER, EXTERNAL :: LoadEndF

SCIPLoadEndF = LoadEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION SCIPLoadEndXF( UserID,endIO,extFlag )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadEndXF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO
INTEGER,       INTENT( IN    ) :: extFlag

INTEGER, EXTERNAL :: LoadEndXF

!==== Initialize

SCIPLoadEndXF = LoadEndXF( UserID,endIO,extFlag )

RETURN
END
!*******************************************************************************
!            Load Project Flags
!*******************************************************************************
INTEGER FUNCTION SCIPLoadFlagsF( UserID,flagsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadFlagsF

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pflagsT ), INTENT( INOUT ) :: flagsIO

INTEGER, EXTERNAL :: LoadFlagsF

SCIPLoadFlagsF = LoadFlagsF( UserID,flagsIO )

RETURN
END
!*******************************************************************************
!            Load Project Domain
!*******************************************************************************
INTEGER FUNCTION SCIPLoadDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadDomainF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pspatialT ), INTENT( INOUT ) :: domainIO

INTEGER, EXTERNAL :: LoadDomainF

SCIPLoadDomainF = LoadDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Load Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPLoadOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadOptionsF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( poptionsT ), INTENT( INOUT ) :: optionsIO

INTEGER, EXTERNAL :: LoadOptionsF

SCIPLoadOptionsF = LoadOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Load Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPLoadMaterialF( UserID,materialIO,mtlListIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadMaterialF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: LoadMaterialF

SCIPLoadMaterialF = LoadMaterialF( UserID,materialIO,mtlListIO )

RETURN
END
!*******************************************************************************
!            Load Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPLoadReleaseF( UserID,releaseIO,relListIO )

USE relstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadReleaseF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( preleaseT ),               INTENT( INOUT ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( INOUT ) :: relListIO

INTEGER, EXTERNAL :: LoadReleaseF

SCIPLoadReleaseF = LoadReleaseF( UserID,releaseIO,relListIO )

RETURN
END
!*******************************************************************************
!            Load Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPLoadReleaseMCF( UserID,releaseIO,relListIO,relMCListIO )

USE relstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadReleaseMCF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( preleaseT ),               INTENT( INOUT ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( INOUT ) :: relListIO
TYPE( releaseMCT), DIMENSION(*), INTENT( INOUT ) :: relMCListIO

INTEGER, EXTERNAL :: LoadReleaseMCF

SCIPLoadReleaseMCF = LoadReleaseMCF( UserID,releaseIO,relListIO,relMCListIO )

RETURN
END
!*******************************************************************************
!            Load Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPLoadWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadWeatherF

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pweatherT ), INTENT( INOUT ) :: weatherIO

INTEGER, EXTERNAL :: LoadWeatherF

SCIPLoadWeatherF = LoadWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Load Project Run
!*******************************************************************************
INTEGER FUNCTION SCIPLoadRunF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadRunF

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER, EXTERNAL :: LoadRunF

SCIPLoadRunF = LoadRunF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Load Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPLoadTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadTimeF

INTEGER,            INTENT( IN    ) :: UserID
TYPE( ptemporalT ), INTENT( INOUT ) :: timeIO

INTEGER, EXTERNAL :: LoadTimeF

SCIPLoadTimeF = LoadTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPLoadInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadInpF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: LoadInpF

SCIPLoadInpF = LoadInpF( UserID,inputIO,mtlListIO )

RETURN
END

!*******************************************************************************
!            Load Project Special Restart Input
!*******************************************************************************
INTEGER FUNCTION SCIPLoadRstF( UserID,inputIO )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadRstF

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO

INTEGER, EXTERNAL :: LoadRst

SCIPLoadRstF = LoadRst( UserID,inputIO )

RETURN
END
!*******************************************************************************
!            Load Met Output
!*******************************************************************************
INTEGER FUNCTION SCIPNumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                   numGrids,numTimes )

!------ Set met plot allocation requirements

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPNumMetOutput

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: num2Dfields, num3Dfields, numGrids, numTimes

INTEGER , EXTERNAL :: NumMetOutput

SCIPNumMetOutput = NumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                                 numGrids,numTimes )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                   grid,time,field2Dlist,field3Dlist )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd
USE plotlist_fd
USE plotmet_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetMetOutput

INTEGER,                                 INTENT( IN  ) :: userID
TYPE( projectIDT ),                      INTENT( IN  ) :: Project
INTEGER,                                 INTENT( IN  ) :: n2D, n3D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: name2D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: units2D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: name3D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: units3D
TYPE( metGridT ),      DIMENSION(*),     INTENT( OUT ) :: grid
TYPE( SCIPTimeT ),     DIMENSION(*),     INTENT( OUT ) :: time
TYPE( metGridFieldT ), DIMENSION(n2D,*), INTENT( OUT ) :: field2Dlist
TYPE( metGridFieldT ), DIMENSION(n3D,*), INTENT( OUT ) :: field3Dlist

INTEGER , EXTERNAL :: GetMetOutput

SCIPGetMetOutput = GetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                                   grid,time,field2Dlist,field3Dlist )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetMetVertGrid( userID,Project,gridID,sigma )

!------ Set met plot allocation requirements

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetMetVertGrid

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( IN  ) :: gridID
REAL, DIMENSION(*), INTENT( OUT ) :: sigma

INTEGER, EXTERNAL :: GetMetVertGrid

SCIPGetMetVertGrid = GetMetVertGrid( userID,Project,gridID,sigma )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGet2DMetData( userID,Project,gridID,timeID,numFields, &
                                   nameFields,nx,ny,data )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGet2DMetData

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,                               INTENT( IN  ) :: nx, ny
REAL,      DIMENSION(nx,ny,numFields), INTENT( OUT ) :: data

INTEGER, EXTERNAL :: Get2DMetData

SCIPGet2DMetData = Get2DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,nx,ny,data )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGet3DMetData( userID,Project,gridID,timeID,numFields, &
                                   nameFields,centerFlag,nx,ny,nz,data )

!------ Set met plot allocation requirements

USE prjstruct_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGet3DMetData

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
INTEGER,                               INTENT( IN  ) :: nx, ny, nz
REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ) :: data

INTERFACE
  INTEGER FUNCTION Get3DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,centerFlag,nx,ny,nz,data )
    USE SCIMgr_fd
    INTEGER,                               INTENT( IN  ) :: userID
    TYPE( projectIDT ),                    INTENT( IN  ) :: Project
    INTEGER,                               INTENT( IN  ) :: gridID
    INTEGER,                               INTENT( IN  ) :: TimeID
    INTEGER,                               INTENT( IN  ) :: numFields
    TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
    INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
    INTEGER,                               INTENT( IN  ) :: nx, ny, nz
    REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ),              &
                                       TARGET        :: data
  END FUNCTION Get3DMetData
END INTERFACE

SCIPGet3DMetData = Get3DMetData( userID,Project,gridID,timeID,numFields, &
                                 nameFields,centerFlag,nx,ny,nz,data )

RETURN
END
!*******************************************************************************
!            Load Puff Output
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectPuffHeader( UserID,puffHead,timeID )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuffHeader

INTEGER,            INTENT( IN    ) :: UserID   !USER ID Tag
TYPE( ppuffHeadT ), INTENT( INOUT ) :: puffHead !Project ID
INTEGER,            INTENT( IN    ) :: timeID   !Time ID for reading puffs

INTEGER, EXTERNAL :: GetProjectPuffHeader

SCIPGetProjectPuffHeader = GetProjectPuffHeader( UserID,puffHead,timeID )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPGetProjectPuff( UserID,puffHead,timeID,flag,puffList,auxData,typeList,mcList )

USE spcstruct_fd
USE mcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuff

INTEGER,                           INTENT( IN    ) :: UserID      !USER ID Tag
TYPE( ppuffHeadT ),                INTENT( INOUT ) :: puffHead    !Project ID
INTEGER,                           INTENT( IN    ) :: timeID      !Time ID for reading puffs
INTEGER,                           INTENT( IN    ) :: flag        !Bits to transform to LLA, MSL, terrain slope
TYPE( puffT ),       DIMENSION(*), INTENT( OUT   ) :: puffList    !Puffs
REAL,                DIMENSION(*), INTENT( OUT   ) :: auxData     !Puff auxiliary data
TYPE( puffTypeT ),   DIMENSION(*), INTENT( OUT   ) :: typeList    !Puffs types
TYPE( material_MClist ),           INTENT( OUT   ) :: mcList      !multicomponent types

INTEGER, EXTERNAL :: GetProjectPuff

SCIPGetProjectPuff = GetProjectPuff( UserID,puffHead,timeID,flag,puffList,auxData,typeList,mcList )

RETURN
END
!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPSizeProject( UserID,project,nMtl,nRel )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPSizeProject

INTEGER,             INTENT( IN  ) :: UserID  !USER ID tag
TYPE ( projectIDT ), INTENT( IN  ) :: project !Project ID
INTEGER,             INTENT( OUT ) :: nMtl    !number of materials in file
INTEGER,             INTENT( OUT ) :: nRel    !number of releases in file

INTEGER nMCrel

INTEGER, EXTERNAL :: SizeProject

SCIPSizeProject = SizeProject( UserID,project,nMtl,nRel,nMCrel )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPLoadProject( UserID,project,mtlList,relList )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadProject

INTEGER,                        INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectT ),               INTENT( INOUT ) :: project !Project ID
TYPE( materialT ),DIMENSION(*), INTENT( OUT   ) :: mtlList !Material list
TYPE( releaseT ), DIMENSION(*), INTENT( OUT   ) :: relList !Release list

INTERFACE
  INTEGER FUNCTION LoadProject( UserID,project,mtlList,relList,relMCList )
    USE SCIMgr_fd
    INTEGER,                          INTENT( IN    ) :: UserID !USER ID Tag
    TYPE( projectT ),                 INTENT( INOUT ) :: project !Project ID
    TYPE( materialT ),  DIMENSION(*), INTENT( OUT   ) :: mtlList !Material list
    TYPE( releaseT ),   DIMENSION(*), INTENT( OUT   ) :: relList !Release list
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( OUT ) :: relMCList !Release multicomponent list
  END FUNCTION LoadProject
END INTERFACE

SCIPLoadProject = LoadProject( UserID,project,mtlList,relList )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPSizeProjectMC( UserID,project,nMtl,nRel,nMCrel )

USE prjstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPSizeProjectMC

INTEGER,             INTENT( IN  ) :: UserID !USER ID tag
TYPE ( projectIDT ), INTENT( IN  ) :: project !Project ID
INTEGER,             INTENT( OUT ) :: nMtl   !number of materials in file
INTEGER,             INTENT( OUT ) :: nRel   !number of releases in file
INTEGER,             INTENT( OUT ) :: nMCrel !number of MC release records in file

INTEGER, EXTERNAL :: SizeProject

SCIPSizeProjectMC = SizeProject( UserID,project,nMtl,nRel,nMCrel )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPLoadProjectMC( UserID,project,mtlList,relList,relMCList )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPLoadProjectMC

INTEGER,                          INTENT( IN    ) :: UserID    !USER ID Tag
TYPE( projectT ),                 INTENT( INOUT ) :: project   !Project ID
TYPE( materialT ),  DIMENSION(*), INTENT( OUT   ) :: mtlList   !Material list
TYPE( releaseT ),   DIMENSION(*), INTENT( OUT   ) :: relList   !Release list
TYPE( releaseMCT ), DIMENSION(*), INTENT( OUT   ) :: relMCList !Release multicomponent list

INTERFACE
  INTEGER FUNCTION LoadProject( UserID,project,mtlList,relList,relMCList )
    USE SCIMgr_fd
    INTEGER,                          INTENT( IN    ) :: UserID !USER ID Tag
    TYPE( projectT ),                 INTENT( INOUT ) :: project !Project ID
    TYPE( materialT ),  DIMENSION(*), INTENT( OUT   ) :: mtlList !Material list
    TYPE( releaseT ),   DIMENSION(*), INTENT( OUT   ) :: relList !Release list
    TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( OUT ) :: relMCList !Release multicomponent list
  END FUNCTION LoadProject
END INTERFACE

SCIPLoadProjectMC = LoadProject( UserID,project,mtlList,relList,relMCList )

RETURN
END
!*******************************************************************************
!            Get Project Puff Chem
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectPuffChem( UserID,project,chemOut,doData )

USE prjstruct_fd
USE multcomp_fd
USE SCIPresults_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectPuffChem

INTEGER,                             INTENT( IN    ) :: UserID     !USER ID Tag
TYPE( projectIDT ),                  INTENT( IN    ) :: project    !Project ID
TYPE( ChemMC_out ), DIMENSION(*),    INTENT( INOUT ) :: chemOut
INTEGER,                             INTENT( IN    ) :: doData

INTEGER, EXTERNAL :: GetProjectPuffChem

SCIPGetProjectPuffChem = GetProjectPuffChem( UserID,project,chemOut,(doData==SCIPtrue) )

RETURN
END




