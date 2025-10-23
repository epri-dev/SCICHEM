INTEGER FUNCTION SWIMversion( iversion_code )

USE SWIMparam_fd

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMversion

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: iversion_code

!**************************** VERSION 1.4 **************************************
!
iversion_code = 1400

SWIMversion = SWIMsuccess

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMPathLength()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMPathLength

!------ initialize land use categories

USE param_fd

IMPLICIT NONE

SWIMPathLength = PATH_MAXLENGTH

RETURN
END


