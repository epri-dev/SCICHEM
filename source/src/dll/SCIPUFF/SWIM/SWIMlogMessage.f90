INTEGER FUNCTION SWIMaddLogMessage( string )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

INTEGER alloc_stat, nch

SWIMaddLogMessage = SWIMfailure

!------ Build log message structure

nch = LEN_TRIM(string)
IF( nch == 0 )THEN
  SWIMaddLogMessage = SWIMresult
  GOTO 9999
END IF
nch = MIN(nch,128)

IF( .NOT.ASSOCIATED(FirstLogMsg) )THEN

  ALLOCATE( FirstLogMsg,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMaddLogMessage'
    error%Message = 'Error setting log file message'
  END IF
  LogMsg => FirstLogMsg

ELSE

  ALLOCATE( LogMsg%next,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMaddLogMessage'
    error%Message = 'Error setting log file message'
  END IF
  LogMsg => LogMsg%next

END IF

LogMsg%string = string(1:nch)
NULLIFY(LogMsg%next)

SWIMaddLogMessage = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

CHARACTER(128) FUNCTION SWIMgetLogMessage()

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMgetLogMessage

USE SWIM_fi

INTEGER alloc_stat

!------ Set null string if first string not associated

IF( .NOT.ASSOCIATED(FirstLogMsg) )THEN
  SWIMgetLogMessage = CHAR(0)
  RETURN
END IF

!------ Return string

SWIMgetLogMessage = FirstLogMsg%string

!------ Clear current string; move-up next string

LogMsg => FirstLogMsg%next
DEALLOCATE( FirstLogMsg,STAT=alloc_stat )

FirstLogMsg => LogMsg

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMclearLogMessage()

USE SWIM_fi
USE SWIMparam_fd

INTEGER alloc_stat

DO WHILE( ASSOCIATED(FirstLogMsg) )

  LogMsg => FirstLogMsg%next
  DEALLOCATE( FirstLogMsg,STAT=alloc_stat )

  FirstLogMsg => LogMsg

END DO

NULLIFY(FirstLogMsg); NULLIFY(LogMsg)

SWIMclearLogMessage = SWIMresult

RETURN
END
