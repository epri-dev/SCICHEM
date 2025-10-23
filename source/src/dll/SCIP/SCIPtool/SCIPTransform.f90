!*******************************************************************************
!                SCIPTransform
!*******************************************************************************
INTEGER FUNCTION SCIPTransform( Cin,Cout,np,xp,yp )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTransform

TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
REAL, DIMENSION(np),          INTENT( INOUT ) :: xp   !x coordinate arrray
REAL, DIMENSION(np),          INTENT( INOUT ) :: yp   !y coordinate arrray

INTEGER, EXTERNAL :: TransformF

SCIPTransform = TransformF( Cin,Cout,np,xp,yp )

RETURN
END
!*******************************************************************************
!                SCIPTransformXY
!*******************************************************************************
INTEGER FUNCTION SCIPTransformXY( Cin,Cout,xp,yp )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTransformXY

TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ), INTENT( IN    ) :: Cout !Output Coordinate data
REAL,                         INTENT( INOUT ) :: xp   !x coordinate arrray
REAL,                         INTENT( INOUT ) :: yp   !y coordinate arrray

INTEGER, EXTERNAL :: TransformXYF

SCIPTransformXY = TransformXYF( Cin,Cout,xp,yp )

RETURN
END
!*******************************************************************************
!                SCIPTransformPt
!*******************************************************************************
INTEGER FUNCTION SCIPTransformPt( Cin,Cout,np,pt )

USE field_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTransformPt

TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( SCIPFieldCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                           INTENT( IN    ) :: np   !number of points
TYPE( SCIPPointT ), DIMENSION(np), INTENT( INOUT ) :: pt   !pt arrray

INTEGER, EXTERNAL :: TransformPtF

SCIPTransformPt = TransformPtF( Cin,Cout,np,pt )

RETURN
END
!*******************************************************************************
!                SCIPTransformPuff
!*******************************************************************************
INTEGER FUNCTION SCIPTransformPuff( Cin,Cout,np,puff )

USE spcstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTransformPuff

TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cin  !Input Coordinate data
TYPE( puffCoordinateT ),      INTENT( IN    ) :: Cout !Output Coordinate data
INTEGER,                      INTENT( IN    ) :: np   !number of points
TYPE( puffT ), DIMENSION(np), INTENT( INOUT ) :: puff !puff arrray

INTEGER, EXTERNAL :: TransformPuffF

SCIPTransformPuff = TransformPuffF( Cin,Cout,np,puff )

RETURN
END
