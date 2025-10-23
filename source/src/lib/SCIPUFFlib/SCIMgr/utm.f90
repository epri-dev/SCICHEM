!***********************************************************************
!               InitDatums
!***********************************************************************
LOGICAL FUNCTION InitDatums( INIfile )

USE error_fi
USE datums
USE DefSize_fd

!     checks to see if UTM installed OK

IMPLICIT NONE

CHARACTER(*) INIfile

CHARACTER(PATH_MAXLENGTH) string

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

string = AddNull( INIfile )

InitDatums = InitUTM( INIfile )

CALL ExceptionTest()

IF( .NOT.InitDatums )THEN
  nError   = IV_ERROR
  eRoutine = 'InitDatums'
  eMessage = 'Failed to initialize UTM transforms - Check installation'
  eInform  = 'Creation of UTM projects will be disabled'
  eAction  = 'Plotting of UTM projects will be approximate'
  CALL ErrorMessage()
  CALL ModuleInitError()
END IF

RETURN
END
