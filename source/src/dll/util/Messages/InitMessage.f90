!===============================================================================

INTEGER FUNCTION InitMessageHandler( callBackAddress )

!DEC$ ATTRIBUTES DLLEXPORT :: InitMessageHandler

USE MessageHandler
USE errorParam_fd

IMPLICIT NONE

INTEGER(LEN_ADDRESS), INTENT( IN ) :: callBackAddress

CALL SetCallBack( callBackAddress )
CALL SetCallerID( 0 )

CALL SetLastError( NO_ERROR,0,'InitMessage',' ',' ',' ' )

CALL SetclockStart( CLOCK_INIT )
CALL SetclockDelay( CLOCK_DELAY )
CALL SetclockUpdate( CLOCK_UPDATE )
CALL SetclockDelta( 0 )

InitMessageHandler = SCIPsuccess

RETURN
END

!===============================================================================

FUNCTION SetMessageHandler( address )

!DEC$ ATTRIBUTES DLLEXPORT :: SetMessageHandler

USE MessageHandler

IMPLICIT NONE

INTEGER(LEN_ADDRESS)               :: SetMessageHandler
INTEGER(LEN_ADDRESS), INTENT( IN ) :: address

SetMessageHandler = GetCallBack()

CALL SetCallBack( address )

RETURN
END

!===============================================================================

FUNCTION GetMessageHandler()

!DEC$ ATTRIBUTES DLLEXPORT :: GetMessageHandler

USE MessageHandler

IMPLICIT NONE

INTEGER(LEN_ADDRESS) :: GetMessageHandler

GetMessageHandler = GetCallBack()

RETURN
END

!===============================================================================

INTEGER FUNCTION SetMessageCallerID( ID )

!DEC$ ATTRIBUTES DLLEXPORT :: SetMessageCallerID

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ID

SetMessageCallerID = GetCallerID()

CALL SetCallerID( ID )

RETURN
END

!===============================================================================

INTEGER FUNCTION GetMessageCallerID( )

!DEC$ ATTRIBUTES DLLEXPORT :: GetMessageCallerID

USE MessageHandler

IMPLICIT NONE

GetMessageCallerID = GetCallerID()

RETURN
END

!===============================================================================

INTEGER FUNCTION SetMessageClockDelay( delay )

!DEC$ ATTRIBUTES DLLEXPORT :: SetMessageClockDelay

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: delay

SetMessageClockDelay = GetclockDelay()

CALL SetclockDelay( delay )

RETURN
END

!===============================================================================

INTEGER FUNCTION SetMessageClockUpdate( update )

!DEC$ ATTRIBUTES DLLEXPORT :: SetMessageClockUpdate

USE MessageHandler

IMPLICIT NONE

INTEGER, INTENT( IN ) :: update

SetMessageClockUpdate = GetclockUpdate()

CALL SetclockUpdate( update )

RETURN
END

!===============================================================================

INTEGER FUNCTION GetMessageClockDelay( )

!DEC$ ATTRIBUTES DLLEXPORT :: GetMessageClockDelay

USE MessageHandler

IMPLICIT NONE

GetMessageClockDelay = GetclockDelay()

RETURN
END

!===============================================================================

INTEGER FUNCTION GetMessageClockUpdate( )

!DEC$ ATTRIBUTES DLLEXPORT :: GetMessageClockUpdate

USE MessageHandler

IMPLICIT NONE

GetMessageClockUpdate = GetclockUpdate()

RETURN
END

!===============================================================================

SUBROUTINE GetMessageHandlerError( error )

!DEC$ ATTRIBUTES DLLEXPORT :: GetMessageHandlerError

USE MessageHandler

IMPLICIT NONE

TYPE( messageT ), INTENT( OUT ) :: error

CALL GetLastError( error )

RETURN
END

