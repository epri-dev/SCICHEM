!*******************************************************************************
!            Get scipuff version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetScipuffVersion()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetScipuffVersion

INTEGER, EXTERNAL :: GetScipuffVersion

SCIPGetScipuffVersion = GetScipuffVersion()

RETURN
END
!*******************************************************************************
!            Get Tool API version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetAPIVersion()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetAPIVersion

INTEGER, EXTERNAL :: GetAPIVersion

SCIPGetAPIVersion = GetAPIVersion()

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version number
!*******************************************************************************
INTEGER FUNCTION SCIPGetVersion()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetVersion

INTEGER, EXTERNAL :: GetVersionValue

SCIPGetVersion = GetVersionValue()

RETURN
END
!*******************************************************************************
!            Get SCIP Tool version string
!*******************************************************************************
INTEGER FUNCTION SCIPGetVersionString( iflag,charString )

USE charT_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetVersionString

INTEGER         , INTENT(INOUT) :: iflag
TYPE( char128T ), INTENT(OUT  ) :: charString

INTEGER, EXTERNAL :: GetVersionString

SCIPGetVersionString = GetVersionString( iflag,charString )

RETURN
END
!*******************************************************************************
!            Get maximum length of filenames
!*******************************************************************************
INTEGER FUNCTION SCIPGetPathMaxLength()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetPathMaxLength

INTEGER, EXTERNAL :: GetPathMaxLength

SCIPGetPathMaxLength = GetPathMaxLength()

RETURN
END
