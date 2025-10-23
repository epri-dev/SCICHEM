!===============================================================================
!     SCIPtool Internal CallBack Message Handler
!     Added to allow SCIPtool to trap CautionMessages and write them to .clog
!===============================================================================
RECURSIVE INTEGER FUNCTION InternalCallBack( arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2
USE basic_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE files_fi

IMPLICIT NONE

INTEGER(LEN_ADDRESS) :: arg1,arg2

INTEGER,DIMENSION(*) :: iParm

INTEGER iMessage
INTEGER(LEN_ADDRESS) jParm
LOGICAL opened
INTEGER ios
TYPE ( messageT   ) caution
CHARACTER(128) string(1:3)
CHARACTER(128), EXTERNAL :: StripNull

INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
INTERFACE
    RECURSIVE INTEGER FUNCTION PassMessageOn( ExternalCallBack,arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2
!DEC$ ATTRIBUTES VALUE :: ExternalCallBack
    USE basic_fd
    INTEGER(LEN_ADDRESS) :: arg1,arg2
    INTEGER(LEN_ADDRESS) :: ExternalCallBack
    INTEGER,DIMENSION(*) :: iParm
  END FUNCTION PassMessageOn
END INTERFACE

CALL VALUE_REFERENCE( arg2, iMessage )

InternalCallBack = SCIPsuccess

!---- Trap Caution Messages

IF( iMessage == HM_CAUTION )THEN

  jParm = ADDRESSOF( iParm )
  CALL ADDRESS_MESSAGE( jParm,caution )
  string(1) = StripNull( caution%aString )
  string(2) = StripNull( caution%bString )
  string(3) = StripNull( caution%cString )

  INQUIRE( FILE=file_clog,OPENED=opened )
  IF( .NOT.opened )THEN
    OPEN(FILE=file_clog,UNIT=lun_clog,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios)
  ELSE
    ios = 0
  END IF

  IF( ios == 0 )THEN
    WRITE(lun_clog,'(A)')'***** CAUTION *****'
    IF( LEN_TRIM(string(1)) > 0 )WRITE(lun_clog,'(A)')TRIM(string(1))
    IF( LEN_TRIM(string(2)) > 0 )WRITE(lun_clog,'(A)')TRIM(string(2))
    IF( LEN_TRIM(string(3)) > 0 )WRITE(lun_clog,'(A)')TRIM(string(3))
  END IF

END IF

!---- Pass the message on to the external call back routine

IF( ExternalCallBack > 0 )THEN
  InternalCallBack = PassMessageOn( ExternalCallBack,arg1,arg2,iParm )
ELSE
  InternalCallBack = SCIPsuccess
END IF

RETURN
END
!*******************************************************************************
!     Pass Message on to external callback routine, the one originally passed
!     to InitTool by the external calling program.
!*******************************************************************************
RECURSIVE INTEGER FUNCTION PassMessageOn( CallBackFunction, arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2

USE basic_fd

INTEGER(LEN_ADDRESS) :: arg1,arg2
INTEGER,DIMENSION(*) :: iParm
#if !defined(__INTEL_COMPILER)
INTEGER(LEN_ADDRESS) :: CallBackFunction

PassMessageOn = CallBackFunction
#else
INTERFACE
  INTEGER FUNCTION CallBackFunction( arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2
    USE basic_fd
    INTEGER(LEN_ADDRESS) :: arg1,arg2
    INTEGER,DIMENSION(*) :: iParm
  END FUNCTION CallBackFunction
END INTERFACE

PassMessageOn = CallBackFunction( arg1,arg2,iParm )
#endif

RETURN
END
