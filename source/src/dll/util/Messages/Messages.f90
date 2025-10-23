MODULE MessageHandler

  USE message_fd
  USE SCIMgrparam_fd
  USE basic_fd

  IMPLICIT NONE

  INTEGER, PARAMETER :: CLOCK_INIT = -1
  INTEGER, PARAMETER :: CLOCK_UPDATE = 1000
  INTEGER, PARAMETER :: CLOCK_DELAY  = 200

!==== Clock

  INTEGER, PRIVATE :: clockDelay
  INTEGER, PRIVATE :: clockUpdate
  INTEGER, PRIVATE :: clockStart
  INTEGER, PRIVATE :: clockDelta

!$OMP THREADPRIVATE(clockDelay,clockUpdate,clockStart,clockDelta)

!==== CallBack

  INTEGER,              PRIVATE :: CallerID
  INTEGER(LEN_ADDRESS), PRIVATE :: CallBack

!==== LastError

  TYPE( messageT ), PRIVATE :: LastError

!==== Initialization

  DATA clockDelay  / CLOCK_DELAY /
  DATA clockUpdate / CLOCK_UPDATE /
  DATA clockStart  / CLOCK_INIT /
  DATA clockDelta  / 0 /
  DATA CallerID    / 0 /
  DATA CallBack    / 0 /

!==== Functions

  INTEGER, EXTERNAL :: CALLBACK_BYREFERENCE
  INTEGER, EXTERNAL :: CALLBACK_BYVALUE

  CONTAINS

    SUBROUTINE SetclockDelay( delay )
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: delay
      clockDelay = delay
      RETURN
    END SUBROUTINE SetclockDelay

    SUBROUTINE SetclockUpdate( update )
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: update
      clockUpdate = update
      RETURN
    END SUBROUTINE SetclockUpdate

    SUBROUTINE SetclockStart( start )
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: start
      clockStart = start
      RETURN
    END SUBROUTINE SetclockStart

    SUBROUTINE SetclockDelta( delta )
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: delta
      clockDelta = delta
      RETURN
    END SUBROUTINE SetclockDelta

    SUBROUTINE SetCallBack( address )
      IMPLICIT NONE
      INTEGER(LEN_ADDRESS), INTENT( IN ) :: address
      CallBack = address
      RETURN
    END SUBROUTINE SetCallBack

    SUBROUTINE SetCallerID( ID )
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: ID
      CallerID = ID
      RETURN
    END SUBROUTINE SetCallerID

    SUBROUTINE SetLastError( I,J,R,A,B,C )
      IMPLICIT NONE
      INTEGER,      INTENT( IN ) :: I
      INTEGER,      INTENT( IN ) :: J
      CHARACTER(*), INTENT( IN ) :: R
      CHARACTER(*), INTENT( IN ) :: A
      CHARACTER(*), INTENT( IN ) :: B
      CHARACTER(*), INTENT( IN ) :: C
      LastError%iParm = I
      LastError%jParm = J
      LastError%routine = R
      LastError%aString = A
      LastError%bString = B
      LastError%cString = C
      RETURN
    END SUBROUTINE SetLastError

    INTEGER FUNCTION GetclockDelay( )
      IMPLICIT NONE
      GetclockDelay = clockDelay
      RETURN
    END FUNCTION GetclockDelay

    INTEGER FUNCTION GetclockUpdate( )
      IMPLICIT NONE
      GetclockUpdate = clockUpdate
      RETURN
    END FUNCTION GetclockUpdate

    INTEGER FUNCTION GetclockStart( )
      IMPLICIT NONE
      GetclockStart = clockStart
      RETURN
    END FUNCTION GetclockStart

    INTEGER FUNCTION GetclockDelta( )
      IMPLICIT NONE
      GetclockDelta = clockDelta
      RETURN
    END FUNCTION GetclockDelta

    FUNCTION GetCallBack( )
      IMPLICIT NONE
      INTEGER(LEN_ADDRESS) :: GetCallBack
      GetCallBack = CallBack
      RETURN
    END FUNCTION GetCallBack

    INTEGER FUNCTION GetCallerID( )
      IMPLICIT NONE
      GetCallerID = CallerID
      RETURN
    END FUNCTION GetCallerID

    SUBROUTINE GetLastError( error )
      IMPLICIT NONE
      TYPE( messageT ) error
      error = LastError
      RETURN
    END SUBROUTINE GetLastError

END MODULE MessageHandler
