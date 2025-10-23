SUBROUTINE showError()

USE Extract_fi

IMPLICIT NONE

IF( nError /= EX_ERROR )THEN
  WRITE(6,*)'Routine:',TRIM(eRoutine)
  WRITE(6,*)'Error  :',nError
  WRITE(6,*)'       :',TRIM(eMessage)
  WRITE(6,*)'       :',TRIM(eInform)
  WRITE(6,*)'       :',TRIM(eAction)
END IF

RETURN
END

!******************************************************************************
!******************************************************************************

SUBROUTINE toolError( failMessage )

USE Extract_fi
USE SCIPtool

IMPLICIT NONE

CHARACTER(*) failMessage

INTEGER :: irv
TYPE( messageT )  :: error

irv = SCIPGetLastError( error )
IF( irv == SCIPfailure )THEN
  nError   = UK_ERROR
  eMessage = TRIM(failMessage)
  eInform  = 'Unknown error - SCIPGetLastError failure'
  eAction  = 'Exiting : Unable to continue'
  eRoutine = 'toolError'
ELSE
  nError   = error%iParm
  eMessage = TRIM(error%aString)
  eInform  = TRIM(error%bString)
  eAction  = TRIM(error%cString)
  eRoutine = TRIM(error%routine)
END IF

RETURN
END
