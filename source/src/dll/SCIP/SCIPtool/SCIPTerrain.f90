!*******************************************************************************
!            Get Project Terrain Header
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectTerrainHeader( UserID,terrain )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectTerrainHeader

INTEGER,               INTENT( IN    ) :: UserID !USER ID Tag
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID

INTEGER, EXTERNAL :: GetProjectTerrainHeader

SCIPGetProjectTerrainHeader = GetProjectTerrainHeader( UserID,1,terrain )

RETURN
END
!*******************************************************************************
!            Get Project Terrain Header
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectTerrainHeaderIn( UserID,ifld,terrain )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectTerrainHeaderIn

INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID

INTEGER, EXTERNAL :: GetProjectTerrainHeader

SCIPGetProjectTerrainHeaderIn = GetProjectTerrainHeader( UserID,ifld,terrain )

RETURN
END
!*******************************************************************************
!            Get Project Terrain
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectTerrain( UserID,terrain,ths,tddx,tddy )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectTerrain

INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
REAL,DIMENSION(*),     INTENT( OUT   ) :: ths     !Terrain array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddx    !Slope array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddy    !Slope array

INTEGER, EXTERNAL :: GetProjectTerrain

SCIPGetProjectTerrain = GetProjectTerrain( UserID,1,terrain,ths,tddx,tddy )

RETURN
END
!*******************************************************************************
!            Get Project Terrain
!*******************************************************************************
INTEGER FUNCTION SCIPGetProjectTerrainIn( UserID,ifld,terrain,ths,tddx,tddy )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetProjectTerrainIn

INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
REAL,DIMENSION(*),     INTENT( OUT   ) :: ths     !Terrain array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddx    !Slope array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddy    !Slope array

INTEGER, EXTERNAL :: GetProjectTerrain

SCIPGetProjectTerrainIn = GetProjectTerrain( UserID,ifld,terrain,ths,tddx,tddy )

RETURN
END
!*******************************************************************************
!            Get Project Terrain
!*******************************************************************************
INTEGER FUNCTION SCIPGetTerrainSlice( UserID,Project,nPts,Location,Ht )

USE prjstruct_fd
USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetTerrainSlice

INTEGER,                             INTENT( IN  ) :: UserID  !USER ID Tag
TYPE( ProjectIDT ),                  INTENT( IN  ) :: Project !Project ID
INTEGER,                             INTENT( IN  ) :: nPts    !No. of location points
TYPE( SCIPpointT ), DIMENSION(nPts), INTENT( IN  ) :: Location    !location array
REAL,DIMENSION(nPts),                INTENT( OUT ) :: Ht    !terrain height array

INTEGER, EXTERNAL :: GetTerrainSlice

SCIPGetTerrainSlice = GetTerrainSlice( UserID,Project,nPts,Location,Ht )

RETURN
END

