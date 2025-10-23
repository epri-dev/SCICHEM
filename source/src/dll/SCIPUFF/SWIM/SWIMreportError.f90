SUBROUTINE SWIMreportError( errorReport )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMreportError

USE SWIM_fi
USE SWIMparam_fd
USE message_fd

IMPLICIT NONE

TYPE( messageT ), INTENT( OUT ) :: errorReport

errorReport%iParm   = error%Number
errorReport%routine = TRIM(error%Routine)
errorReport%aString = TRIM(error%Message)
errorReport%bString = TRIM(error%Inform)
errorReport%cString = TRIM(error%Action)

RETURN
END

!==============================================================================

SUBROUTINE SWIMclearError()

USE SWIM_fi
USE SWIMparam_fd

error%Number  = NO_ERROR
error%Routine = ' '
error%Message = ' '
error%Inform  = ' '
error%Action  = ' '

RETURN
END

!==============================================================================

CHARACTER(*) FUNCTION ArraySizeStr( ndim,dim )

IMPLICIT NONE

INTEGER,                  INTENT( IN ) :: ndim
INTEGER, DIMENSION(ndim), INTENT( IN ) :: dim

INTEGER ios, i

CHARACTER(32) numStr

IF( ndim < 1 )THEN
  ArraySizeStr = ' '
  RETURN
END IF

ArraySizeStr = 'Array size = ('

DO i = 1,ndim
  WRITE(numStr,FMT='(I32)',IOSTAT=ios) dim(i)
  IF( ios == 0 )THEN
    ArraySizeStr = TRIM(ArraySizeStr)//TRIM(ADJUSTL(numStr))
  ELSE
    ArraySizeStr = 'Array size too big'
    EXIT
  END IF
  IF( i == ndim )THEN
    ArraySizeStr = TRIM(ArraySizeStr)//')'
  ELSE
    ArraySizeStr = TRIM(ArraySizeStr)//','
  END IF
END DO

RETURN

END

!==============================================================================

SUBROUTINE SWIMgetLandUseError()

USE message_fd
USE SWIM_fi

IMPLICIT NONE

TYPE( messageT ) messageLU

INTEGER irv

INTEGER, EXTERNAL :: GetLastLandUseError

messageLU%aString = ' '
messageLU%bString = ' '
messageLU%cString = ' '
messageLU%routine = ' '

irv = GetLastLandUseError( messageLU )

error%Number  = messageLU%iParm
error%Routine = TRIM(messageLU%routine)
error%Message = TRIM(messageLU%aString)
error%Inform  = TRIM(messageLU%bString)
error%Action  = TRIM(messageLU%cString)

RETURN
END
