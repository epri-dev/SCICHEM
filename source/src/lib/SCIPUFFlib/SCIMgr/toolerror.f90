!*******************************************************************************
!            Get Last Error
!*******************************************************************************
INTEGER FUNCTION ModuleGetLastError( error )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

IMPLICIT NONE

TYPE( messageT ), INTENT( OUT ) :: error

!==== Initialize

ModuleGetLastError = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available

  ModuleGetLastError = SCIPsuccess

!==== Load message structure

  error%iParm   = nError
  error%jParm   = LastError

  error%aString = eMessage
  error%bString = eInform
  error%cString = eAction

  error%routine = eRoutine

  CALL ModuleInitError()

END IF

RETURN
END
!***********************************************************************
!               ModuleInitError
!***********************************************************************
SUBROUTINE ModuleInitError()

USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState

IMPLICIT NONE

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available
  CALL init_error()
  LastError = 0
END IF

RETURN
END
!***********************************************************************
!               ModuleOK
!***********************************************************************
LOGICAL FUNCTION ModuleOK( flag )

USE SCIMgr_fd

!     This routine checks the flag for SCIPsuccess

IMPLICIT NONE

INTEGER flag

ModuleOK = flag == SCIPsuccess

RETURN
END

