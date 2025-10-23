!==============================================================================
! SCIMgrInitState
!==============================================================================
SUBROUTINE SCIMgrInitState()

USE SCIMgr_fi

IMPLICIT NONE

toolState = 0

RETURN
END
!==============================================================================
! SCIMgrGetState
!==============================================================================
INTEGER FUNCTION SCIMgrGetState()

USE SCIMgr_fi

IMPLICIT NONE

SCIMgrGetState = toolState

RETURN
END
!==============================================================================
! SCIMgrSetState
!==============================================================================
INTEGER FUNCTION SCIMgrSetState( state )

USE SCIMgr_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: state

SCIMgrSetState = toolState
toolState    = state

RETURN
END
!==============================================================================
! SCIMgrIsBusy
!==============================================================================
LOGICAL FUNCTION SCIMgrIsBusy()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsBusy = toolState /= HS_IDLE

RETURN
END
!==============================================================================
! SCIMgrIsIdle
!==============================================================================
LOGICAL FUNCTION SCIMgrIsIdle()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsIdle = toolState == HS_IDLE

RETURN
END
!==============================================================================
! SCIMgrIsRun
!==============================================================================
LOGICAL FUNCTION SCIMgrIsRun()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsRun = BTEST(toolState,HSB_RUN)

RETURN
END
!==============================================================================
! SCIMgrIsSS
!==============================================================================
LOGICAL FUNCTION SCIMgrIsSS()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsSS = BTEST(toolState,HSB_STEP)

RETURN
END
!==============================================================================
! SCIMgrIsSync
!==============================================================================
LOGICAL FUNCTION SCIMgrIsSync()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsSync = BTEST(toolState,HSB_SYNC)

RETURN
END
!==============================================================================
! SCIMgrIsWait
!==============================================================================
LOGICAL FUNCTION SCIMgrIsWait()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsWait = BTEST(toolState,HSB_WAIT)

RETURN
END
!==============================================================================
! SCIMgrIsLoad
!==============================================================================
LOGICAL FUNCTION SCIMgrIsLoad()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrIsLoad = BTEST(toolState,HSB_LOAD)

RETURN
END
!==============================================================================
! SCIMgrHasProgress
!==============================================================================
LOGICAL FUNCTION SCIMgrHasProgress()

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

SCIMgrHasProgress = BTEST(toolState,HSB_PBOX)

RETURN
END
!==============================================================================
! SCIMgrSetBusyMsg
!==============================================================================
SUBROUTINE SCIMgrSetBusyMsg()

USE error_fi

IMPLICIT NONE

nError = BZ_ERROR
eRoutine = 'SCIMgrSetBusyMsg'
eMessage = 'SCIPUFFManager is busy'
eInform  = 'Current request cannot be processed'
eAction  = ' '

RETURN
END
!==============================================================================
! SCIMgrCheckState
!==============================================================================
LOGICAL FUNCTION SCIMgrCheckState( state )

USE SCIMgr_fi
USE state_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: state

LOGICAL,EXTERNAL :: SCIMgrIsBusy      !State /= HS_IDLE
LOGICAL,EXTERNAL :: SCIMgrIsIdle      !State == HS_IDLE
LOGICAL,EXTERNAL :: SCIMgrIsRun       !State == HS_RUN  bit is set
LOGICAL,EXTERNAL :: SCIMgrIsSS        !State == HS_STEP bit is set (single step)
LOGICAL,EXTERNAL :: SCIMgrIsSync      !State == HS_SYNC bit is set
LOGICAL,EXTERNAL :: SCIMgrIsWait      !State == HS_WAIT bit is set
LOGICAL,EXTERNAL :: SCIMgrIsLoad      !State == HS_LOAD bit is set

!==============================================================================
!Check basic state
!==============================================================================

SELECT CASE( ABS(state) )
  CASE( HS_ANYSTATE )
    SCIMgrCheckState = .TRUE.
  CASE( HS_IDLESTATE )
    SCIMgrCheckState = SCIMgrIsIdle()
  CASE( HS_WAITSTATE )
    SCIMgrCheckState = SCIMgrIsWait()
  CASE( HS_RUNSTATE )
    SCIMgrCheckState = SCIMgrIsWait() .AND. SCIMgrIsRun()
  CASE( HS_SINGLESTEP )
    SCIMgrCheckState = SCIMgrIsSS() .AND. SCIMgrIsRun() .AND. SCIMgrIsBusy()
  CASE( HS_SYNCSTATE )
    SCIMgrCheckState = SCIMgrIsWait() .AND. SCIMgrIsRun() .AND. SCIMgrIsSync()
  CASE( HS_IDLEWAIT )
    SCIMgrCheckState = SCIMgrIsIdle() .OR. SCIMgrIsWait()
  CASE( HS_IDLERUN )
    SCIMgrCheckState = SCIMgrIsIdle() .OR. ( SCIMgrIsWait() .AND. SCIMgrIsRun() )
  CASE( HS_IDLESYNC )
    SCIMgrCheckState = SCIMgrIsIdle() .OR. ( SCIMgrIsWait() .AND. SCIMgrIsRun() .AND. SCIMgrIsSync() )
  CASE( HS_IDLEBUSY )
    SCIMgrCheckState = SCIMgrIsIdle() .OR. .NOT.SCIMgrIsWait()
  CASE( HS_LOADSYNC )
    SCIMgrCheckState = SCIMgrIsLoad() .OR. ( SCIMgrIsWait() .AND. SCIMgrIsRun() .AND. SCIMgrIsSync() )
  CASE DEFAULT
    SCIMgrCheckState = .FALSE.
END SELECT

!==============================================================================
!Check to see if already busy
!==============================================================================
IF( state < 0 )THEN
  SCIMgrCheckState = SCIMgrCheckState .AND. .NOT.BTEST(toolState,HSB_BUSY)
END IF

RETURN
END


