!***********************************************************************
!                start_message
!***********************************************************************
SUBROUTINE start_message( iflag )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to check for Progress Box messages

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iflag

INTEGER irv

INTEGER, EXTERNAL :: PostStartMessage
INTEGER, EXTERNAL :: PostWaitMessage

INTEGER currentState
INTEGER waitState
INTEGER value

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback
value = iflag

irv = PostStartMessage( value )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'StartMessage' )

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostWaitMessage( TRUE )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'StartMessage' )

last_message%aString  = ' '
last_message%bString  = ' '
last_message%cString  = ' '

RETURN
END
!***********************************************************************
!                stop_message
!***********************************************************************
SUBROUTINE stop_message( input )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to check for Progress Box messages

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: input

INTEGER irv

INTEGER, EXTERNAL :: PostStopMessage

INTEGER currentState
INTEGER waitState

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostStopMessage( input )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL setMessageHandlerError( 'StopMessage' )

RETURN
END
!***********************************************************************
!                check_progress
!***********************************************************************
RECURSIVE SUBROUTINE check_progress()

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to check for Progress Box messages

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: PostCheckMessage

INTEGER currentState
INTEGER waitState

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostCheckMessage()

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv == SCIPfailure )CALL SetMessageHandlerError( 'CheckProgress' )

RETURN
END
!***********************************************************************
!                start_clock
!***********************************************************************
SUBROUTINE start_clock()

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to check for Progress Box messages

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: PostClockStartMessage

INTEGER currentState
INTEGER waitState

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostClockStartMessage( )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'StartClock' )

RETURN
END
!***********************************************************************
!                Step_clock
!***********************************************************************
SUBROUTINE step_clock()

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to start clock messages

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostClockStepMessage

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostClockStepMessage( )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv == SCIPfailure )CALL SetMessageHandlerError( 'StepClock' )

RETURN
END
!***********************************************************************
!                Stop_clock
!***********************************************************************
SUBROUTINE stop_clock()

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to start clock messages

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostClockStopMessage

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostClockStopMessage( )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'StartClock' )

RETURN
END
!***********************************************************************
!                enableSCIPUFFhalt
!***********************************************************************
SUBROUTINE enableSCIPUFFhalt( iflag )

USE SCIMgr_fd
USE SCIMgr_fi
USE scipuff_fi
USE SCIMgrState

!     GUI Callback to enable buttons in Progress Box

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iflag

INTEGER irv
INTEGER ButtonState
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostButtonStateMessage
INTEGER, EXTERNAL :: PostButtonLabelMessage
INTEGER, EXTERNAL :: PostWaitMessage

!==== Set Button STATIC labels

!==== Button identifier : (bit 0 (Left), 1 (Middle), 2 (Right))

message%iParm = 7 !Bits 0 and 2 -> Left and Right

!==== Tag label :  FALSE->Buttons , TRUE->Static labels

message%jParm = TRUE

!==== Labels
!       iflag < 0 -> Mass-consistent iterations
!       iflag > 0 -> SCIPUFF step

message%routine = CHAR(0)
message%aString = 'Next Output'
message%bString = 'Next Step'
message%cString = 'Calculation'

!==== Set Button State
!       Show(set)/Hide(clear)      : (bit 0 (Left), 1 (Middle), 2 (Right))
!       Enable(set)/Disable(clear) : (bit 3 (Left), 4 (Middle), 5 (Right))
!                                         9        18          36
SELECT CASE( iflag )
  CASE( SCIPUFF_ENABLE )
    ButtonState = 63 !Enable/Show Left,Middle and Right

  CASE( SCIPUFF_HALT )
    ButtonState = 36 !Enable/Show Right

  CASE( SCIPUFF_STOP )
    ButtonState = 54 !Enable/Show Middle and Right

  CASE( 3 )
    ButtonState = 18 !Enable/Show Middle

  CASE( 4 )
    ButtonState = 45 !Enable/Show Left and Right

  CASE DEFAULT
    ButtonState = 0 !None

END SELECT

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostButtonStateMessage( ButtonState )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv == SCIPfailure )THEN
  CALL SetMessageHandlerError( 'EnableButton' )
  GOTO 9999
END IF

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostButtonLabelMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv == SCIPfailure )THEN
  CALL SetMessageHandlerError( 'EnableButton' )
  GOTO 9999
END IF

!==== If showing any button - make a HM_RELEASEWAIT call

IF( ButtonState /= 0 )THEN

!==== Set tool state to WAIT

  waitState    = IBSET( toolState,HSB_WAIT )
  currentState = SCIMgrSetState( waitState )

!==== Make Callback

  irv = PostWaitMessage( FALSE )

!==== Reset Tool state

  waitState = SCIMgrSetState( currentState )

!==== Check Error

  IF( irv == SCIPfailure )THEN
    CALL SetMessageHandlerError( 'EnableButton' )
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!                InfoMessage
!***********************************************************************
SUBROUTINE InfoMessage( )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to display an informational message (simple OK Message Box)

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostInfoMessage

!==== Copy Info Message

message%iParm    = NO_ERROR
message%jParm    = FALSE
message%routine  = eRoutine
message%aString  = eMessage
message%bString  = eInform
message%cString  = eAction

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostInfoMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Message return

IF( irv /= SCIPsuccess )THEN                     !Messaging Error
  CALL SetMessageHandlerError( 'InfoMessage' )
ELSE                                             !Messaging Success
  CALL ModuleInitError()
END IF

RETURN
END
!***********************************************************************
!                CautionMessage
!***********************************************************************
SUBROUTINE CautionMessage( )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to record a Cauton message to the Caution log

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostCautionMessage

!==== Copy Info Message

message%iParm    = NO_ERROR
message%jParm    = FALSE
message%routine  = eRoutine
message%aString  = eMessage
message%bString  = eInform
message%cString  = eAction

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostCautionMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Message return

IF( irv /= SCIPsuccess )THEN                     !Messaging Error
  CALL SetMessageHandlerError( 'CautionMessage' )
ELSE                                             !Messaging Success
  CALL ModuleInitError()
END IF

RETURN
END
!***********************************************************************
!                ErrorMessage
!***********************************************************************
SUBROUTINE ErrorMessage( )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to display an error message (simple OK Message Box)

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostErrorMessage

!==== Copy Error Message

message%iParm    = nError
message%jParm    = FALSE
message%routine  = eRoutine
message%aString  = eMessage
message%bString  = eInform
message%cString  = eAction

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostErrorMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Message return

IF( irv /= SCIPsuccess )THEN
  CALL SetMessageHandlerError( 'ErrorMessage' )
END IF

RETURN
END
!***********************************************************************
!                WarningMessage
!***********************************************************************
SUBROUTINE WarningMessage( default )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to display a warning message (OK/CANCEL Message Box)

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: default

INTEGER reply
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostReplyMessage

!==== Copy Warning Message

message%iParm    = nError
message%routine  = eRoutine
message%aString  = eMessage
message%bString  = eInform
IF( LEN_TRIM(eAction) <= 0 .OR. eAction(1:1)==CHAR(0) )THEN
  message%cString  = 'Do you want to continue?'
ELSE
  message%cString  = eAction
END IF

!==== Set default reply

IF( default )THEN
  message%jParm  = SCIPtrue
ELSE
  message%jParm  = SCIPfalse
END IF

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

reply = PostReplyMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Message return

IF( reply == SCIPaffirmative )THEN         !YES selected
  CALL ModuleInitError()
ELSE IF( reply == SCIPnull )THEN           !Messaging error
  CALL SetMessageHandlerError( 'ReplyMessage' )
ELSE                                       !NO selected
  nError = IV_ERROR
END IF

RETURN
END
!***********************************************************************
!                write_progress_bar
!***********************************************************************
SUBROUTINE write_progress_bar( iProgress )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to update the progress bar in the Progress Box

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iProgress ! (range 0-64)

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostProgressBarMessage

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostProgressBarMessage( iProgress )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'ProgressBar' )

RETURN
END
!***********************************************************************
!                write_progress
!***********************************************************************
SUBROUTINE write_progress( s1,s2,s3 )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to update the progress messages in the Progress Box

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: s1,s2,s3

INTEGER irv
INTEGER currentState
INTEGER waitState

INTEGER, EXTERNAL :: PostProgressMessage

!==== Prepare Message

message%iParm   = NO_ERROR
message%jParm   = FALSE
message%routine = 'Progress'

IF( TRIM(s1) == CHAR(0) )THEN
  message%aString = last_message%aString
ELSE
  message%aString = s1
END IF

IF( TRIM(s2) == CHAR(0) )THEN
  message%bString = last_message%bString
ELSE
  message%bString = s2
END IF

IF( TRIM(s3) == CHAR(0) )THEN
  message%cString = last_message%cString
ELSE
  message%cString = s3
END IF

!==== Set tool state to WAIT

waitState    = IBSET( toolState,HSB_WAIT )
currentState = SCIMgrSetState( waitState )

!==== Make Callback

irv = PostProgressMessage( message )

!==== Reset Tool state

waitState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'ProgressBar' )

last_message = message

RETURN
END
!***********************************************************************
!                set_messaging
!***********************************************************************
SUBROUTINE set_messaging( ID )

USE SCIMgr_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

INTEGER,              EXTERNAL :: SetMessageCallerID
INTEGER(LEN_ADDRESS), EXTERNAL :: SetMessageHandler
INTEGER,              EXTERNAL :: SetMessageClockDelay
INTEGER,              EXTERNAL :: SetMessageClockUpdate

prevCallBack    = SetMessageHandler( SCIPCallBack )
prevCallerID    = SetMessageCallerID( ID )
prevClockDelay  = SetMessageClockDelay( clockDelay )
prevClockUpdate = SetMessageClockUpdate( clockUpdate )

RETURN
END
!***********************************************************************
!                reset_messaging
!***********************************************************************
SUBROUTINE reset_messaging()

USE SCIMgr_fi

IMPLICIT NONE

INTEGER irv

INTEGER,              EXTERNAL :: SetMessageCallerID
INTEGER(LEN_ADDRESS), EXTERNAL :: SetMessageHandler
INTEGER,              EXTERNAL :: SetMessageClockDelay
INTEGER,              EXTERNAL :: SetMessageClockUpdate

irv = SetMessageHandler( prevCallBack )
irv = SetMessageCallerID( prevCallerID )
irv = SetMessageClockDelay( prevClockDelay )
irv = SetMessageClockUpdate( prevClockUpdate )

RETURN
END
!***********************************************************************
!                synchronize
!***********************************************************************
SUBROUTINE synchronize( t0,t1 )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!     GUI Callback to synchronize with calling routine

IMPLICIT NONE

REAL, INTENT( IN ) :: t0,t1

INTEGER irv
INTEGER currentState
INTEGER syncState

REAL, DIMENSION(2) :: input

INTEGER, EXTERNAL :: PostSyncMessage

!==== Prepare Message

input(1) = t0
input(2) = t1

!==== Make Callback

syncState    = IBSET( toolState,HSB_SYNC )
syncState    = IBSET( syncState,HSB_WAIT )
currentState = SCIMgrSetState( syncState )

irv = PostSyncMessage( input )

syncState = SCIMgrSetState( currentState )

!==== Check Error

IF( irv /= SCIPsuccess )CALL SetMessageHandlerError( 'Synchronize' )

RETURN
END
!***********************************************************************
!               WriteCaution
!***********************************************************************
SUBROUTINE writeCaution()

USE error_fi
USE message_fd
USE files_fi


IMPLICIT NONE

INTEGER          :: ios
CHARACTER(12)    :: number
TYPE( messageT ) :: save_error

save_error%iParm = nError

IF( nError /= NO_ERROR )THEN
  save_error%aString = eMessage
  save_error%bString = eInform
  save_error%cString = eAction
  save_error%routine = eRoutine
END IF

IF( nRelOutsideDomain > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nRelOutsideDomain
  IF( ios /= 0 )number = '??'
  eMessage = TRIM(ADJUSTL(number))//' instantaneous release(s) were ignored'
  eInform  = 'These releases were outside the spatial domain'
  eAction  = 'Consider increasing the spatial domain'
  CALL CautionMessage()
END IF

IF( nCRelOutsideDomain > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nCRelOutsideDomain
  IF( ios /= 0 )number = '??'
  eMessage = TRIM(ADJUSTL(number))//' continuous release(s) were ignored'
  eInform  = 'These releases were outside the spatial domain'
  eAction  = 'Consider increasing the spatial domain'
  CALL CautionMessage()
END IF

IF( nRelBeforeStart > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nRelBeforeStart
  IF( ios /= 0 )number = '??'
  eMessage = TRIM(ADJUSTL(number))//' release(s) were ignored'
  eInform  = 'These releases were before the start of the run'
  eAction  = 'Consider moving the start time earlier'
  CALL CautionMessage()
END IF

IF( nStopSplit > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nStopSplit
  IF( ios /= 0 )number = '??'
  eMessage = 'Splitting was halted during a step '//TRIM(ADJUSTL(number))//' time(s). Resolution may be compromised'
  eInform  = 'Splitting was halted because of too many puffs.'
  eAction  = 'Consider increasing the maximum number of puffs allowed'
  CALL CautionMessage()
END IF

IF( nPuffReflect > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nPuffReflect
  IF( ios /= 0 )number = '??'
  eMessage = TRIM(ADJUSTL(number))//' puff(s) encountered a puff reflection problem. Resolution may be compromised'
  IF( nStopSplit > 0 )THEN
    eInform  = 'This may be a result of splitting being halted because of too many puffs.'
    eAction  = 'Consider increasing the maximum number of puffs allowed'
  ELSE
    eInform  = ' '
    eAction  = ' '
  END IF
  CALL CautionMessage()
END IF

IF( nDezoneSrf > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nDezoneSrf
  IF( ios /= 0 )number = '??'
  eMessage = 'A surface grid was dezoned '//TRIM(ADJUSTL(number))//' time(s)'
  eInform  = 'Resolution may be compromised, usually at short-ranges'
  eAction  = 'Consider increasing MaxGrid in INI file, especially if short-range results are of interest'
  CALL CautionMessage()
END IF

IF( nRelInterIter > 0 )THEN
  WRITE(number,*,IOSTAT=ios)nRelInterIter
  IF( ios /= 0 )number = '??'
  eMessage = 'Velocity iterations in Continuous Release interactions failed to converge '//TRIM(ADJUSTL(number))//' times'
  eInform  = ' '
  eAction  = ' '
  CALL CautionMessage()
END IF

IF( save_error%iParm /= NO_ERROR )THEN
  nError   = save_error%iParm
  eMessage = save_error%aString
  eInform  = save_error%bString
  eAction  = save_error%cString
  eRoutine = save_error%routine
END IF

RETURN
END

