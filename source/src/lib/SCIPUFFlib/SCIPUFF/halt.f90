!==============================================================================
!  SCIPUFF halt functions
!==============================================================================

INTEGER FUNCTION setSCIPUFFhalt( mode ) RESULT( handledStop )

USE scipuff_fi
USE basic_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode

!==== Check to see if this is for SCIPUFF to handle

IF( SCIPUFFinProgress )THEN

!==== Check mode
!     Abort - Set error

  IF( mode < 0 )THEN

    nError   = AB_ERROR
    eRoutine = 'SCIPUFFhalt'
    eMessage = 'User requested abort detected by SCIPUFF'
    eInform  = 'SCIPUFF aborted'

!     Stop - Save for later checking - reset buttons

  ELSE

    istop = mode
    CALL enableSCIPUFFhalt( istop )

  END IF

  handledStop = TRUE

ELSE

  handledStop = FALSE

END IF

RETURN
END
