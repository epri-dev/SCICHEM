SUBROUTINE setSWIMcallback( Callback_IN,CallerID_IN,ClockDelay_IN,ClockUpdate_IN )

!DEC$ ATTRIBUTES DLLEXPORT :: setSWIMcallback

USE SWIM_fi

IMPLICIT NONE

INTEGER(LEN_ADDRESS), INTENT( IN ) :: Callback_IN
INTEGER,              INTENT( IN ) :: CallerID_IN
INTEGER,              INTENT( IN ) :: ClockDelay_IN
INTEGER,              INTENT( IN ) :: ClockUpdate_IN

INTEGER irv

INTEGER,              EXTERNAL :: InitMessageHandler
INTEGER,              EXTERNAL :: SetMessageCallerID
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler

Callback    = Callback_IN
CallerID    = CallerID_IN
ClockDelay  = ClockDelay_IN
ClockUpdate = ClockUpdate_IN

IF( GetMessagehandler() == 0 )THEN
  irv = InitMessageHandler( Callback )
END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMsetMessaging( )

USE SWIM_fi

IMPLICIT NONE

INTEGER,              EXTERNAL :: SetMessageCallerID
INTEGER(LEN_ADDRESS), EXTERNAL :: SetMessageHandler
INTEGER,              EXTERNAL :: SetMessageClockDelay
INTEGER,              EXTERNAL :: SetMessageClockUpdate

prevCallBack = SetMessageHandler( CallBack )
prevCallerID = SetMessageCallerID( CallerID )
prevClockDelay = SetMessageClockDelay( ClockDelay )
prevClockUpdate = SetMessageClockUpdate( ClockUpdate )

SWIMmessageEnabled = .TRUE.

RETURN
END
!***********************************************************************
!                SWIMResetMessaging
!***********************************************************************
SUBROUTINE SWIMResetMessaging( )

USE SWIM_fi

IMPLICIT NONE

INTEGER irv

INTEGER,              EXTERNAL :: SetMessageCallerID
INTEGER(LEN_ADDRESS), EXTERNAL :: SetMessageHandler
INTEGER,              EXTERNAL :: SetMessageClockDelay
INTEGER,              EXTERNAL :: SetMessageClockUpdate

IF( SWIMmessageEnabled )THEN
  irv = SetMessageHandler( prevCallBack )
  irv = SetMessageCallerID( prevCallerID )
  irv = SetMessageClockDelay( prevClockDelay )
  irv = SetMessageClockUpdate( prevClockUpdate )
  SWIMmessageEnabled = .FALSE.
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMWarningMessage( NotAffirmativeString )

!----- Display warning message with yes/no buttons

USE SWIM_fi
USE SWIMparam_fd
USE basic_fd
USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: NotAffirmativeString

INTEGER irv

INTEGER, EXTERNAL :: PostReplyMessage

SWIMWarningMessage = SWIMfailure

!------ Copy Warning Message

message%iParm   = error%Number !=WN_ERROR??
message%jParm   = TRUE !FALSE !0
message%routine = TRIM(error%Routine)
message%aString = TRIM(error%Message)
message%bString = TRIM(error%Inform)
message%cString = TRIM(error%Action)

!------ Post warning

irv = PostReplyMessage( message )

!------ If message is not affirmative, set Action and return

IF( irv /= SCIPaffirmative )THEN
  error%Action = TRIM(NotAffirmativeString)
  GOTO 9999
END IF

!------ Otherwise, clear error

CALL SWIMclearError()

SWIMWarningMessage = SWIMsuccess

9999 CONTINUE

RETURN
END
