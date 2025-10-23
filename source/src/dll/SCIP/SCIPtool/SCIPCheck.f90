!*******************************************************************************
!            Check Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPCheckInput( UserID,iInput,tInput0,tInput1 )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckInput

INTEGER,               INTENT( IN ) :: UserID
INTEGER,               INTENT( IN ) :: iInput
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput0
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput1

INTEGER, EXTERNAL :: CheckInput

SCIPCheckInput = CheckInput( UserID,iInput,tInput0,tInput1 )

RETURN
END
!*******************************************************************************
!            Check Project Start Time
!*******************************************************************************
INTEGER FUNCTION SCIPCheckStartF( UserID,startIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckStartF

INTEGER,        INTENT( IN ) :: UserID
TYPE( startT ), INTENT( IN ) :: startIO

INTEGER, EXTERNAL :: CheckStartF

!==== Initialize

SCIPCheckStartF = CheckStartF( UserID,startIO )

RETURN
END
!*******************************************************************************
!            Check ProjectDomain
!*******************************************************************************
INTEGER FUNCTION SCIPCheckDomainF( UserID,domainIO )

USE domstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckDomainF

INTEGER,          INTENT( IN    ) :: UserID
TYPE( spatialT ), INTENT( INOUT ) :: domainIO

INTEGER, EXTERNAL :: CheckDomainF

SCIPCheckDomainF = CheckDomainF( UserID,domainIO )

RETURN
END
!*******************************************************************************
!            Check Project Options
!*******************************************************************************
INTEGER FUNCTION SCIPCheckOptionsF( UserID,optionsIO )

USE inpstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckOptionsF

INTEGER,          INTENT( IN ) :: UserID
TYPE( optionsT ), INTENT( IN ) :: optionsIO

INTEGER, EXTERNAL :: CheckOptionsF

SCIPCheckOptionsF = CheckOptionsF( UserID,optionsIO )

RETURN
END
!*******************************************************************************
!            Check Project Material
!*******************************************************************************
INTEGER FUNCTION SCIPCheckMaterialF( UserID,materialIO )

USE mtlstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckMaterialF

INTEGER,           INTENT( IN ) :: UserID
TYPE( materialT ), INTENT( IN ) :: materialIO

INTEGER, EXTERNAL :: CheckMaterialF

SCIPCheckMaterialF = CheckMaterialF( UserID,materialIO )

RETURN
END
!*******************************************************************************
!            Check Project Release
!*******************************************************************************
INTEGER FUNCTION SCIPCheckReleaseF( UserID,releaseIO,materialIO )

USE mtlstruct_fd
USE release_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckReleaseF

INTEGER,           INTENT( IN ) :: UserID
TYPE( releaseT ),  INTENT( IN ) :: releaseIO
TYPE( materialT ), INTENT( IN ) :: materialIO

INTEGER, EXTERNAL :: CheckReleaseF

SCIPCheckReleaseF = CheckReleaseF( UserID,releaseIO,materialIO )

RETURN
END
!*******************************************************************************
!            Check Project Weather
!*******************************************************************************
INTEGER FUNCTION SCIPCheckWeatherF( UserID,weatherIO )

USE metstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckWeatherF

INTEGER,          INTENT( IN ) :: UserID
TYPE( weatherT ), INTENT( IN ) :: weatherIO

INTEGER, EXTERNAL :: CheckWeatherF

SCIPCheckWeatherF = CheckWeatherF( UserID,weatherIO )

RETURN
END
!*******************************************************************************
!            Check Project End Time
!*******************************************************************************
INTEGER FUNCTION SCIPCheckEndF( UserID,endIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckEndF

INTEGER,      INTENT( IN ) :: UserID
TYPE( endT ), INTENT( IN ) :: endIO

INTEGER, EXTERNAL :: CheckEndF

SCIPCheckEndF = CheckEndF( UserID,endIO )

RETURN
END
!*******************************************************************************
!            Check Project Time
!*******************************************************************************
INTEGER FUNCTION SCIPCheckTimeF( UserID,timeIO )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckTimeF

INTEGER,           INTENT( IN ) :: UserID
TYPE( temporalT ), INTENT( IN ) :: timeIO

INTEGER, EXTERNAL :: CheckTimeF

SCIPCheckTimeF = CheckTimeF( UserID,timeIO )

RETURN
END
!*******************************************************************************
!            Check Project Input
!*******************************************************************************
INTEGER FUNCTION SCIPCheckInpF( UserID,inputIO,mtlListIO )

USE structure_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPCheckInpF

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( inputT ),                  INTENT( INOUT ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER, EXTERNAL :: CheckInpF

SCIPCheckInpF = CheckInpF( UserID,inputIO,mtlListIO )

RETURN
END

