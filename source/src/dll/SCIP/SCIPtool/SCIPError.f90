!*******************************************************************************
!            Get SCIP Tool Error
!*******************************************************************************
INTEGER FUNCTION SCIPGetLastError( error )

USE message_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPGetLastError

TYPE( messageT ), INTENT( OUT ) :: error

INTEGER, EXTERNAL :: ModuleGetLastError

SCIPGetLastError = ModuleGetLastError( error )

RETURN
END
!***********************************************************************
!               SCIPInitError
!***********************************************************************
SUBROUTINE SCIPInitError()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitError

CALL ModuleInitError()

RETURN
END
