!==============================================================================
!==============================================================================
!==============================================================================
INTEGER FUNCTION SCIPAdjointReleaseFilter( CallerID,nRel,relList,nMat,matList,maxHit )

!--- Filter adjoint releases - discarded releases have zero mass

USE release_fd
USE material_fd
USE AdjointFilter_fi

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPAdjointReleaseFilter

INTEGER,                                INTENT( IN    ) :: CallerID
INTEGER,                                INTENT( INOUT ) :: nRel
TYPE( releaseT  ),DIMENSION(*), TARGET, INTENT( INOUT ) :: relList
INTEGER,                        TARGET, INTENT( INOUT ) :: nMat
TYPE( materialT ),DIMENSION(*),         INTENT( INOUT ) :: matList
INTEGER,                                INTENT( IN    ) :: maxHit

INTERFACE
  INTEGER FUNCTION AdjointReleaseFilterF( userID,nRel,relList,nMat,matList )
    USE release_fd
    USE material_fd

    INTEGER,                                INTENT( IN    ) :: userID
    INTEGER,                                INTENT( INOUT ) :: nRel
    TYPE( releaseT  ),DIMENSION(*), TARGET, INTENT( INOUT ) :: relList
    INTEGER,                        TARGET, INTENT( INOUT ) :: nMat
    TYPE( materialT ),DIMENSION(*),         INTENT( INOUT ) :: matList
  END FUNCTION AdjointReleaseFilterF
END INTERFACE

nhit_target = maxHit

SCIPAdjointReleaseFilter = AdjointReleaseFilterF( CallerID,nRel,relList,nMat,matList )

RETURN
END
