SUBROUTINE getLandUseError()

USE message_fd
USE error_fi

IMPLICIT NONE

TYPE( messageT ) message

INTEGER irv

INTEGER, EXTERNAL :: GetLastLandUseError

message%aString = ' '
message%bString = ' '
message%cString = ' '
message%routine = ' '

irv = GetLastLandUseError( message )

nError   = message%iParm
eMessage = TRIM(message%aString)
eInform  = TRIM(message%bString)
eAction  = TRIM(message%cString)
eRoutine = TRIM(message%routine)

RETURN
END
