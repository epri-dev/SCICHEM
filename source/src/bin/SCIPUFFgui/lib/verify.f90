!*******************************************************************************
!             VerifyButton
!*******************************************************************************
LOGICAL FUNCTION verify_button( iwnd,bmsg )

USE pcscipuf_fi
USE script_fi
USE SwinAPI

!---- This routine verifies a button click

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd   !Window handle
CHARACTER(*),         INTENT( IN ) :: bmsg   !Verify Question

INTEGER irv

CHARACTER(128) stringa
CHARACTER(32)  stringb

CHARACTER(128), EXTERNAL :: AddNull

!---- Check for SCRIPT mode

IF( .NOT.(BTEST(pcscipuf_mode,IPMB_INTERACTIVE)) )THEN
  verify_button = .TRUE.
  GOTO 9999
END IF

!---- Initialize Verify parameters

stringa = AddNull( 'Are you sure you want to'//CHAR(13)//TRIM(bmsg)//'?' )
stringb = AddNull( 'Please verify your response' )

verify_button = .FALSE.

!---- Pause Mouse

CALL MousePause()

!---- Get Verify Response

irv = MessageBox( iwnd,stringa,stringb,IOR(MB_YESNO,MB_ICONQUESTION) )

IF( irv == IDYES )verify_button = .TRUE.

!---- Resume Mouse

CALL MouseResume()

9999 CONTINUE

RETURN
END
