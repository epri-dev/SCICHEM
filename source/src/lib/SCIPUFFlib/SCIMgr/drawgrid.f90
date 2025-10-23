INTEGER FUNCTION DrawGrid( UserID,grdI,UserDraw,Mode )

USE SCIMgr_fd
USE error_fi
USE sagdef_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER, INTENT( IN ) :: UserID       !USER ID tag
INTEGER, INTENT( IN ) :: grdI         !SAG grid ID
INTEGER, EXTERNAL     :: UserDraw     !User supplied draw function
INTEGER, INTENT( IN ) :: mode         !Draw instructions

INTEGER irv
INTEGER currentState

!------ External Functions

INTEGER, EXTERNAL :: SAG_DrawCellID
INTEGER, EXTERNAL :: SAG_DrawTriangleID
INTEGER, EXTERNAL :: SAG_InitError

!------ Initialize

DrawGrid = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL init_error()
irv = SAG_InitError()

CALL set_messaging( userID )

IF( Aborted( ) )GOTO 9999

IF( BTEST(Mode,0) )THEN

  irv = SAG_DrawCellID( grdI,UserDraw )
  IF( irv /= SAG_OK )THEN
    nError = UK_ERROR
    eRoutine = 'DrawGrid'
    eMessage = 'Error drawing SAG Grid cells'
    GOTO 9999
  END IF

END IF

IF( Aborted( ) )GOTO 9999

IF( BTEST(Mode,1) )THEN

  irv = SAG_DrawTriangleID( grdI,UserDraw )
  IF( irv /= SAG_OK )THEN
    nError = UK_ERROR
    eRoutine = 'DrawGrid'
    eMessage = 'Error drawing SAG Grid triangles'
    GOTO 9999
  END IF

END IF

DrawGrid = SCIPsuccess

9999 CONTINUE

CALL AbortClear()

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END
