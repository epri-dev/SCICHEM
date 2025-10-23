!***********************************************************************
!                PROGPROC
!***********************************************************************
RECURSIVE FUNCTION PROGPROC( HDLG,MESSAGE,WPARAM,LPARAM )

!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PROGPROC' :: PROGPROC

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE script_fi
USE animate_fi
USE SwinAPI

!     This is the message handler for the progress dialog box.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: PROGPROC

INTEGER(POINTER_LEN), INTENT( IN ) :: HDLG,WPARAM,LPARAM
INTEGER MESSAGE
INTEGER ICOMM,IDUM
INTEGER irv,iflg

!---- Initialize Return Value

PROGPROC = FALSE

!---- Select Action Based on Message Received

SELECT CASE( MESSAGE )

  CASE( WM_INITDIALOG ) !INITIALIZE
    IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
      CALL position_progress( HDLG,LPARAM ) !  Position Box
      irv = ShowWindow( HDLG,SW_SHOW )      !  Show Dialog Box
    END IF
    PROGPROC = TRUE !  Set Return Value

  CASE( WM_CTLCOLORMSGBOX,WM_CTLCOLORDLG ) !STATIC COLOR
    PROGPROC = dlgBkBrush

  CASE( WM_CTLCOLOREDIT ) !STATIC COLOR

  CASE( WM_CTLCOLORSTATIC,WM_CTLCOLORBTN ) !STATIC COLOR
    irv = SetTextColor( WPARAM,dlgATxColor )
    irv = SetBkColor( WPARAM,dlgBkColor )
    PROGPROC = dlgBkBrush

  CASE( WM_COMMAND ) !COMMANDS
    PROGPROC = TRUE !  Set Return Value
    CALL split_param( WPARAM,ICOMM,IDUM ) !  Get Command and Focus

    SELECT CASE( ICOMM ) !  Select action by Command

      CASE (IDB_BUTTON2) !  HALT Button
        iflg = 1
        IF( lanimate )iflg = -iflg
        CALL HaltScipuff( iflg )

      CASE( IDB_BUTTON3 ) !  ABORT Button
        CALL AbortScipuff()

      CASE( IDB_BUTTON1 ) !  STOP Button
        iflg = 2
        IF( lanimate )iflg = -iflg
        CALL HaltScipuff( iflg )

      CASE DEFAULT

    END SELECT

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!                Halt Scipuff
!***********************************************************************
SUBROUTINE HaltScipuff( iflag )

USE tooluser_fd
USE animate_fi
USE GUItool_fi

!     This routine sets the SCIPUFF halt parameter

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iflag

INTEGER irv

SELECT CASE( iflag )

  CASE( 1 )
    irv = SCIPButton( ToolCallerID,HC_MIDDLEBUTTON )

  CASE( 2 )
    irv = SCIPButton( ToolCallerID,HC_LEFTBUTTON )

  CASE DEFAULT

END SELECT

lanimate = .FALSE.

RETURN
END
!***********************************************************************
!                Abort Scipuff
!***********************************************************************
SUBROUTINE AbortScipuff()

USE tooluser_fd
USE pcscipuf_fi
USE GUItool_fi
USE SwinAPI
USE SCIPtool

!     This routine sets the SCIPUFF Abort parameter

IMPLICIT NONE

LOGICAL lok
INTEGER irv

LOGICAL, EXTERNAL :: verify_button

!---- Hide Progress Box

CALL show_progress( FALSE,SW_HIDE )

!---- Verify ABORT

lok = verify_button( hwnd_pb,'Abort the SCIPUFF run' )

IF( lok )irv = SCIPButton( ToolCallerID,HC_RIGHTBUTTON )

!---- Show Progress Box

CALL show_progress( TRUE,SW_SHOW )

RETURN
END
