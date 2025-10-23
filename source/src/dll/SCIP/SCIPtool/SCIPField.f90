INTEGER FUNCTION SCIPCreateField( UserID,FieldX,ClassData )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCreateField

INTEGER,                INTENT( IN  )   :: UserID       !USER ID Tag
TYPE( SCIPPlotFieldT ), INTENT( INOUT ) :: FieldX       !Field descriptor
REAL, DIMENSION(*),     INTENT( IN  )   :: ClassData    !Additional Class data

INTEGER, EXTERNAL :: CreateFieldF

SCIPCreateField = CreateFieldF( UserID,FieldX,ClassData )

RETURN
END

!==============================================================================

INTEGER FUNCTION SCIPDeleteField( userID,FieldID )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPDeleteField

INTEGER, INTENT( IN    ) :: userID       !USER ID tag
INTEGER, INTENT( INOUT ) :: FieldID      !SAG grid ID

INTEGER, EXTERNAL :: DeleteFieldF

SCIPDeletefield = DeleteFieldF( userID,FieldID )

RETURN
END

!====================================================================

INTEGER FUNCTION SCIPGetFieldDomain( userID,grdI,m0,n0,xmin,ymin,dx,dy )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: userID       !USER ID tag
INTEGER, INTENT( IN  ) :: grdI         !SAG grid ID
INTEGER, INTENT( OUT ) :: m0           !Base grid number X direction
INTEGER, INTENT( OUT ) :: n0           !Base grid number Y direction
REAL,    INTENT( OUT ) :: xmin         !Origin X direction
REAL,    INTENT( OUT ) :: ymin         !Origin Y direction
REAL,    INTENT( OUT ) :: dx           !Base grid size X direction
REAL,    INTENT( OUT ) :: dy           !Base grid size Y direction

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldDomain

INTEGER, EXTERNAL :: GetFieldDomainF

SCIPGetFieldDomain = GetFieldDomainF( userID,grdI,m0,n0,xmin,ymin,dx,dy )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetFieldValue( userID,grdI,Field,PlotType,xpnt,ypnt,fpnt )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldValue

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
REAL,                  INTENT( IN  ) :: xpnt          !Location (project coords)
REAL,                  INTENT( IN  ) :: ypnt          !Location (project coords)
REAL,                  INTENT( OUT ) :: fpnt          !Field value

INTEGER, EXTERNAL :: GetFieldValueF

SCIPGetFieldValue = GetFieldValueF( userID,grdI,Field,PlotType,xpnt,ypnt,fpnt )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetFieldValues( userID,grdI,Field,PlotType,npnt,xpnt,ypnt,fpnt )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldValues

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: grdI          !Field ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field         !Field definition
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType      !Plot definition
INTEGER,               INTENT( IN  ) :: npnt          !Number of points
REAL, DIMENSION(npnt), INTENT( IN  ) :: xpnt          !Location (project coords)
REAL, DIMENSION(npnt), INTENT( IN  ) :: ypnt          !Location (project coords)
REAL, DIMENSION(npnt), INTENT( OUT ) :: fpnt          !Field value

INTEGER, EXTERNAL :: GetFieldValuesF

SCIPGetFieldValues = GetFieldValuesF( userID,grdI,Field,PlotType,npnt,xpnt,ypnt,fpnt )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetPrjCoord( userID,npnt,lonpnt,latpnt,xpnt,ypnt )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetPrjCoord

INTEGER,               INTENT( IN  ) :: userID        !Caller ID
INTEGER,               INTENT( IN  ) :: npnt          !Number of points
REAL, DIMENSION(npnt), INTENT( IN  ) :: lonpnt        !Location (longitude)
REAL, DIMENSION(npnt), INTENT( IN  ) :: latpnt        !Location (latitude)
REAL, DIMENSION(npnt), INTENT( OUT ) :: xpnt          !Location (project coords)
REAL, DIMENSION(npnt), INTENT( OUT ) :: ypnt          !Location (project coords)

INTEGER, EXTERNAL :: GetPrjCoordF

SCIPGetPrjCoord = GetPrjCoordF( userID,npnt,lonpnt,latpnt,xpnt,ypnt )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetFieldSize( UserID,grdI,Field,PlotType,nNode,nTri )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldSize

INTEGER,               INTENT( IN  ) :: UserID     !User ID tag
INTEGER,               INTENT( IN  ) :: grdI       !SAG grid ID
TYPE( SCIPPlotFieldT ),INTENT( IN  ) :: Field        !Field descriptor
TYPE( SCIPPlotTypeT ), INTENT( IN  ) :: PlotType     !Plot definition
INTEGER,               INTENT( OUT ) :: nNode,nTri !Number of Nodes,Triangles

INTEGER, EXTERNAL :: GetFieldSizeF

SCIPGetFieldSize = GetFieldSizeF( UserID,grdI,Field,PlotType,nNode,nTri )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetField( UserID,grdI,Field,PlotType,nodes,triangles )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetField

INTEGER,                                     INTENT( IN  ) :: UserID     !User ID tag
INTEGER,                                     INTENT( IN  ) :: grdI       !SAG grid ID
TYPE( SCIPPlotFieldT ),                      INTENT( IN  ) :: Field        !Field descriptor
TYPE( SCIPPlotTypeT ),                       INTENT( IN  ) :: PlotType     !Plot definition
TYPE( SCIPPlotFieldNodeT ),     DIMENSION(*),INTENT( OUT ) :: nodes        !Plot definition
TYPE( SCIPPlotFieldTriangleT ), DIMENSION(*),INTENT( OUT ) :: triangles    !Plot definition

INTEGER, EXTERNAL :: GetFieldF

SCIPGetField = GetFieldF( UserID,grdI,Field,PlotType,nodes,triangles )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetFieldAssocSize( UserID,grdI,nPlots,nLines,nPoints )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldAssocSize

INTEGER, INTENT( IN  ) :: UserID
INTEGER, INTENT( IN  ) :: grdI
INTEGER, INTENT( OUT ) :: nPlots
INTEGER, INTENT( OUT ) :: nLines
INTEGER, INTENT( OUT ) :: nPoints

INTEGER, EXTERNAL :: GetFieldAssocSizeF

SCIPGetFieldAssocSize = GetFieldAssocSizeF( UserID,grdI,nPlots,nLines,nPoints )

RETURN
END

!============================================================================

INTEGER FUNCTION SCIPGetFieldAssocData( UserID,grdI,Lines,Points,Titles,Axes,LineID )

USE field_fd
USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetFieldAssocData

INTEGER,                          INTENT( IN  ) :: UserID
INTEGER,                          INTENT( IN  ) :: grdI
TYPE( SCIPLineT  ), DIMENSION(*), INTENT( OUT ) :: Lines
TYPE( SCIPPointT ), DIMENSION(*), INTENT( OUT ) :: Points
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Titles
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: Axes
TYPE( char64T    ), DIMENSION(*), INTENT( OUT ) :: LineID

INTEGER, EXTERNAL :: GetFieldAssocDataF

SCIPGetFieldAssocData = GetFieldAssocDataF( UserID,grdI,Lines,Points,Titles,Axes,LineID )

RETURN
END
