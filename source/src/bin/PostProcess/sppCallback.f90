RECURSIVE INTEGER FUNCTION sppCallback( arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2

USE tooluser_fd
USE basic_fd

IMPLICIT NONE

INTEGER iMessage
INTEGER iParm
INTEGER iCaller
INTEGER(LEN_ADDRESS) :: arg1,arg2

TYPE( messageT ) message

LOGICAL default
INTEGER nError
INTEGER(LEN_ADDRESS) jParm

CHARACTER(128) string(3),eRoutine
CHARACTER(128), EXTERNAL :: StripNull
INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
INTEGER, PARAMETER :: HM_COMPUTEEFF = HM_STRUCTURE + 32
INTEGER, PARAMETER :: HM_INITEFF = HM_ARRAY+32
INTEGER, PARAMETER :: HM_HASEFF  = HM_ARRAY+33
INTEGER, PARAMETER :: HM_EXITEFF = 32

sppCallback = SCIPfailure

CALL VALUE_REFERENCE( arg1, iCaller )
CALL VALUE_REFERENCE( arg2, iMessage )

IF( iMessage == HM_MESSAGE )THEN
  jParm = ADDRESSOF( iParm )
  CALL ADDRESS_MESSAGE(jParm,message)
  string(1) = StripNull( message%aString )
  string(2) = StripNull( message%bString )
  string(3) = StripNull( message%cString )
ELSE IF( iMessage > HM_MESSAGE )THEN
  jParm = ADDRESSOF( iParm )
  CALL ADDRESS_MESSAGE( jParm,message )
  nError    = message%iParm
  eRoutine  = StripNull( message%routine )
  string(1) = StripNull( message%aString )
  string(2) = StripNull( message%bString )
  string(3) = StripNull( message%cString )
  default   = message%jParm /= FALSE
END IF

sppCallback = SCIPsuccess
SELECT CASE( iMessage )
  CASE( HM_CHECK )
  CASE( HM_SETCLOCK )
  CASE( HM_STEPCLOCK )
  CASE( HM_STOPCLOCK )
  CASE( HM_SETWAIT )
  CASE( HM_RELEASEWAIT )
  CASE( HM_INFO )
    WRITE(6,*)'SCIPtool message'
    WRITE(6,*)'Routine='//TRIM(eRoutine)
    WRITE(6,*)'Message=',TRIM(string(1))
    WRITE(6,*)'        ',TRIM(string(2))
    WRITE(6,*)'        ',TRIM(string(3))
  CASE( HM_ERROR )
    WRITE(6,*)'SCIPtool error'
    WRITE(6,*)'Routine='//TRIM(eRoutine)
    WRITE(6,*)'Error  =',nError
    WRITE(6,*)'        ',TRIM(string(1))
    WRITE(6,*)'        ',TRIM(string(2))
    WRITE(6,*)'        ',TRIM(string(3))
  CASE( HM_REPLY )
    WRITE(6,*)'SCIPtool Warning'
    WRITE(6,*)'Routine='//TRIM(eRoutine)
    WRITE(6,*)'Warning=',TRIM(string(1))
    WRITE(6,*)'        ',TRIM(string(2))
    WRITE(6,*)'        ',TRIM(string(3))
    IF( default )THEN
      sppCallback = SCIPaffirmative
    ELSE
      sppCallback = SCIPnegative
    END IF
  CASE( HM_CAUTION )
    WRITE(6,*)'SCIPtool caution'
    WRITE(6,*)'Routine='//TRIM(eRoutine)
    WRITE(6,*)'Message=',TRIM(string(1))
    WRITE(6,*)'        ',TRIM(string(2))
    WRITE(6,*)'        ',TRIM(string(3))
  CASE( HM_PROGRESSMSG )
  CASE( HM_PROGRESSBAR )
  CASE( HM_BUTTONTAG )
  CASE( HM_BUTTONSTATE )
  CASE( HM_START )
  CASE( HM_STOP )
  CASE( HM_SYNC )
  CASE( HM_RELEASE )
    sppCallback = SCIPfailure
  CASE( HM_UPDATEREL )
    sppCallback = SCIPfailure
  CASE( HM_UPDATERELMC )
    sppCallback = SCIPfailure
  CASE( HM_COMPUTEEFF,HM_INITEFF,HM_EXITEFF,HM_HASEFF )
    WRITE(6,*)'NWPN Effect callback encountered'
    WRITE(6,*)'NWPN effects require use of NWPN source model'
    WRITE(6,*)'SAG2Uni does not handle NWPN Effects'
    WRITE(6,*)'SAG2Uni does not have access to source models'
    sppCallback = SCIPfailure
  CASE DEFAULT
END SELECT

9999 CONTINUE

RETURN
END
