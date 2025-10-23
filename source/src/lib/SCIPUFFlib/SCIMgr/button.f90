!*******************************************************************************
!            ButtonHandler
!*******************************************************************************
INTEGER FUNCTION ButtonHandler( userID,buttonID )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE SCIMgrState

!Handle Button clicks in User-supplied in Progress dialog

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: userID      !USER ID Tag
INTEGER, INTENT( IN  ) :: buttonID    !Progress Box Button ID

INTEGER StopMode
INTEGER ButtonState

INTEGER, EXTERNAL :: PostButtonStateMessage
INTEGER, EXTERNAL :: setSWIMhalt
INTEGER, EXTERNAL :: setSCIPUFFhalt

!==== Initialize

ButtonHandler = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available
  IF( .NOT.SCIMgrHasProgress() )THEN        !Button clicks only valid if made when
    nError = BZ_ERROR                     !a progress box is available
    eRoutine = 'ButtonHandler'
    eMessage = 'Progress box not active'
    eInform  = 'No buttons to set'
    GOTO 9999
  END IF
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Set stop mode and resulting button state based on button ID

SELECT CASE( buttonID )
  CASE( HC_LEFTBUTTON ) !Stop next output
    StopMode    = 2
    ButtonState = 54
  CASE( HC_MIDDLEBUTTON ) !Stop next step
    StopMode    = 1
    ButtonState = 36
  CASE( HC_RIGHTBUTTON ) !Abort
    StopMode    = -1
    ButtonState = 0
  CASE DEFAULT
END SELECT

!==== Give SWIM a chance to handle it

IF( setSWIMhalt(StopMode) == TRUE )THEN
  ButtonHandler = SCIPsuccess
  GOTO 9999

!==== Give SCIPUFF a chance to handle it

ELSE IF( setSCIPUFFhalt(StopMode) == TRUE )THEN
  ButtonHandler = SCIPsuccess
  GOTO 9999

ELSE

!==== Must be up to the Tool to handle it - only know about aborts

  IF( StopMode < 0 )THEN

    nError = AB_ERROR
    eMessage ='User requested Abort of SCIPtool function detected'
    eInform  = CHAR(0)
    eAction  = CHAR(0)
    eRoutine ='AbortSCIPtool'

!==== Reset button states

    ButtonHandler = PostButtonStateMessage( ButtonState )
    IF( ButtonHandler /= SCIPsuccess )THEN
      CALL SetMessageHandlerError( 'ButtonHandler' )
      GOTO 9999
    END IF
  ELSE
  END IF
END IF

!==== Return

9999 CONTINUE

CALL reset_messaging()

RETURN
END

!*******************************************************************************
!            CheckButtons
!*******************************************************************************
INTEGER FUNCTION CheckButtons( userID )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi
USE files_fi
USE SCIMgrState

!Handle Button clicks in User-supplied in Progress dialog

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: userID      !USER ID Tag

LOGICAL lexist
INTEGER irv
INTEGER, EXTERNAL  :: ButtonHandler
INTEGER, EXTERNAL  :: sysDeleteFile

!==== Initialize

CheckButtons = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN     !Always available
  IF( .NOT.SCIMgrHasProgress() )THEN        !Button clicks only valid if made when
    nError = BZ_ERROR                       !a progress box is available
    eRoutine = 'CheckButtons'
    eMessage = 'Progress box not active'
    eInform  = 'No buttons to set'
    GOTO 9999
  END IF
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

INQUIRE(FILE=file_abort,EXIST=lexist)
IF( lexist )THEN
  CheckButtons = ButtonHandler(userID,HC_RIGHTBUTTON)
  irv = sysDeleteFile( file_abort )
  GOTO 9999
END IF

INQUIRE(FILE=file_halt,EXIST=lexist)
IF( lexist )THEN
  CheckButtons = ButtonHandler(userID,HC_MIDDLEBUTTON)
  irv = sysDeleteFile( file_halt )
  GOTO 9999
END IF

INQUIRE(FILE=file_stop,EXIST=lexist)
IF( lexist )THEN
  CheckButtons = ButtonHandler(userID,HC_LEFTBUTTON)
  irv = sysDeleteFile( file_stop )
  GOTO 9999
END IF

CheckButtons = SCIPsuccess

!==== Return

9999 CONTINUE

CALL reset_messaging()

RETURN
END
