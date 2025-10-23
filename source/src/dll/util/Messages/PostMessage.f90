!===============================================================================

INTEGER FUNCTION PostErrorMessage( errorMessage )

!DEC$ ATTRIBUTES DLLEXPORT :: PostErrorMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: errorMessage

PostErrorMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_ERROR,errorMessage )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostInfoMessage( infoMessage )

!DEC$ ATTRIBUTES DLLEXPORT :: PostInfoMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: infoMessage

PostInfoMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_INFO,infoMessage )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostReplyMessage( replyMessage )

!DEC$ ATTRIBUTES DLLEXPORT :: PostReplyMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: replyMessage

PostReplyMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_REPLY,replyMessage )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostCautionMessage( cautionMessage )

!DEC$ ATTRIBUTES DLLEXPORT :: PostCautionMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: cautionMessage

PostCautionMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_CAUTION,cautionMessage )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostCheckMessage( )

!DEC$ ATTRIBUTES DLLEXPORT :: PostCheckMessage

USE MessageHandler

IMPLICIT NONE

IF( GetCallBack() /= 0 )THEN
  PostCheckMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_CHECK,0 )
ELSE
  PostCheckMessage = SCIPtrue
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION PostClockStartMessage( )

!DEC$ ATTRIBUTES DLLEXPORT :: PostClockStartMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, EXTERNAL :: sysGetTick

PostClockStartMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_SETCLOCK,0 )

CALL SetclockStart( sysGetTick( 0 ) )
CALL SetclockDelta( GetclockDelay() )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostClockStepMessage( )

!DEC$ ATTRIBUTES DLLEXPORT :: PostClockStepMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, EXTERNAL :: sysGetTick

IF( GetclockStart() /= CLOCK_INIT .AND. sysGetTick(GetclockStart()) > GetclockDelta() )THEN

  PostClockStepMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_STEPCLOCK,0 )

  CALL SetclockStart( sysGetTick( 0 ) )
  CALL SetclockDelta( GetclockDelay() )

ELSE

  PostClockStepMessage = SCIPnull

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION PostClockStopMessage( )

!DEC$ ATTRIBUTES DLLEXPORT :: PostClockStopMessage

USE MessageHandler

IMPLICIT NONE

PostClockStopMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_STOPCLOCK,0 )

CALL SetclockStart( CLOCK_INIT )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostProgressBarMessage( progress )

!DEC$ ATTRIBUTES DLLEXPORT :: PostProgressBarMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: progress

PostProgressBarMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_PROGRESSBAR,progress )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostProgressMessage( progressMessage )

!DEC$ ATTRIBUTES DLLEXPORT :: PostProgressMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: progressMessage

PostProgressMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_PROGRESSMSG,progressMessage )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostButtonStateMessage( button )

!DEC$ ATTRIBUTES DLLEXPORT :: PostButtonStateMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: button

PostButtonStateMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_BUTTONSTATE,button )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostButtonTagMessage( tag )

!DEC$ ATTRIBUTES DLLEXPORT :: PostButtonTagMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: tag


TYPE( messageT ) :: button

button = tag
button%jParm = FALSE
PostButtonTagMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_BUTTONTAG,button )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostButtonLabelMessage( label )

!DEC$ ATTRIBUTES DLLEXPORT :: PostButtonLabelMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: label


TYPE( messageT ) :: button

button = label
button%jParm = TRUE
PostButtonLabelMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_BUTTONTAG,button )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostReleaseMessage( release )

!DEC$ ATTRIBUTES DLLEXPORT :: PostReleaseMessage

USE MessageHandler
USE release_fd

IMPLICIT NONE

TYPE( releaseT ), INTENT( IN ) :: release

PostReleaseMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_RELEASE,release )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostUpdateMessage( update )

!DEC$ ATTRIBUTES DLLEXPORT :: PostUpdateMessage

USE MessageHandler
USE update_fd
USE errorParam_fd

IMPLICIT NONE

TYPE( updateRelT ), INTENT( INOUT ) :: update

PostUpdateMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_UPDATEREL,update )

IF( PostUpdateMessage == SCIPfailure )THEN
  CALL SetLastError( UK_ERROR,0,'PostUpdateMessage','Callback failed to update release',' ',' ' )
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION PostUpdateMCMessage( update )

!DEC$ ATTRIBUTES DLLEXPORT :: PostUpdateMCMessage

USE MessageHandler
USE update_fd
USE errorParam_fd

IMPLICIT NONE

TYPE( updateRelMCT ), INTENT( INOUT ) :: update

PostUpdateMCMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_UPDATERELMC,update )

IF( PostUpdateMCMessage == SCIPfailure )THEN
  CALL SetLastError( UK_ERROR,0,'PostUpdateMCMessage','Callback failed to update release',' ',' ' )
END IF

RETURN
END


!===============================================================================

INTEGER FUNCTION PostSyncMessage( sync )

!DEC$ ATTRIBUTES DLLEXPORT :: PostSyncMessage

USE MessageHandler

IMPLICIT NONE

REAL, DIMENSION(*) :: sync

PostSyncMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_SYNC,sync )

RETURN
END


!===============================================================================

INTEGER FUNCTION PostStartMessage( flag )

!DEC$ ATTRIBUTES DLLEXPORT :: PostStartMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: flag

PostStartMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_START,flag )

RETURN
END


!===============================================================================

INTEGER FUNCTION PostStopMessage( message )

!DEC$ ATTRIBUTES DLLEXPORT :: PostStopMessage

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: message

PostStopMessage = callback_ByReference( GetCallBack(),GetCallerID(),HM_STOP,message )

RETURN
END

!===============================================================================

INTEGER FUNCTION PostWaitMessage( flag )

!DEC$ ATTRIBUTES DLLEXPORT :: PostWaitMessage

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: flag

IF( flag == TRUE )THEN

  PostWaitMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_SETWAIT,0 )

ELSE

  PostWaitMessage = callback_ByValue( GetCallBack(),GetCallerID(),HM_RELEASEWAIT,0 )

END IF

RETURN
END




