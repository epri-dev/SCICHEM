!***********************************************************************
!                AbortPrintProc
!***********************************************************************
INTEGER FUNCTION ABORTPROC( hdc,nCode )

!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'ABORTPROC' :: ABORTPROC

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: hdc
INTEGER,              INTENT( IN ) :: nCode

INTEGER irv

TYPE( T_MSG ) msg

!Check Message Queue for messages

DO WHILE( PeekMessage( msg,NULL_POINTER,NULL,NULL,PM_REMOVE ) /= FALSE )

!If ABORT dialog exists and this is a message for it - process

  IF( (IsWindow(hdlgp) /= FALSE) .AND. &
     (IsDialogMessage(hdlgp,msg)) /= FALSE )THEN
  ELSE
    irv = TranslateMessage( msg )
    irv = DispatchMessage( msg )
  END IF

END DO

!Set ABORTPROC true if abort print flag has been set

IF( lPrintIt )THEN
  ABORTPROC = TRUE
ELSE
  ABORTPROC = FALSE
END IF

RETURN
END
!***********************************************************************
!                AbortPrintDialogHandler
!***********************************************************************
FUNCTION ABORTDLG( HDLG,MESSAGE,WPARAM,LPARAM )

!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'ABORTDLG' :: ABORTDLG

USE resource_fd
USE winAPI_fd
!
!     This is the message handler for the ABORT box.
!
IMPLICIT NONE

INTEGER(POINTER_LEN) ::                ABORTDLG
INTEGER(POINTER_LEN), INTENT( IN ) ::  HDLG
INTEGER,              INTENT( IN ) ::  MESSAGE
INTEGER(POINTER_LEN), INTENT( IN ) ::  WPARAM
INTEGER(POINTER_LEN), INTENT( IN ) ::  LPARAM

INTEGER icmd,idum

ABORTDLG = FALSE

SELECT CASE( MESSAGE )

  CASE( WM_INITDIALOG )                 !INITIALIZE
    CALL position_abort( HDLG )
    ABORTDLG = TRUE

  CASE( WM_COMMAND )                    !COMMANDS
    CALL split_param( WPARAM,icmd,idum )
    IF( icmd == ID_CANCEL  )THEN       !CANCEL
      CALL abort_print( HDLG )
      ABORTDLG = TRUE
    END IF

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!                position_abort
!***********************************************************************
SUBROUTINE position_abort( HWND )

USE errorParam_fd
USE pcscipuf_fi
USE SwinAPI
!
!     This routine positions the ABORT PRINT box in the center of the
!     PLOT window
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: HWND !ABORT BOX Handle

INTEGER irv,iwid,ihgt,kwid,khgt,ilft,itop,iflg

TYPE( T_RECT ) Box

!     Initialize abort flag

iAbort = 0

!     PLOT Window ClientArea determines Width/Height centering rectangle

irv  = GetClientRect( hwnd_pw,Box )
iwid = Box%right  - Box%left + 1
ihgt = Box%bottom - Box%top  + 1

!     PLOT Window WindowRect determines position of centering rectangle

irv  = GetWindowRect( hwnd_pw,Box )
ilft = Box%left
itop = Box%top

!     ABORT Window WindowRect determines size of rectangle to center

irv  = GetWindowRect( HWND,Box )
kwid = Box%right  - Box%left + 1
khgt = Box%bottom - Box%top  + 1

!     Compute Top/Left Corner of ABORT box

ilft = ilft + iwid/2 - kwid/2
itop = itop + ihgt/2 - khgt/2

!     Dont change size of Z order of ABORT window

iflg = IOR( SWP_NOZORDER,SWP_NOSIZE )

!     Position ABORT window

irv = SetWindowPos( HWND,0,ilft,itop,0,0,iflg )
IF( irv == FALSE )THEN
  irv = GetLastError()
  WRITE(string1,'(A,I6)')'Last Error = ',irv
  CALL SetError( API_ERROR, &
                 'Failed to position AbortPrint box', &
                 TRIM(string1), &
                 'Call for Technical assistance', &
                 'Position Abort' )
  CALL ShowErrorMessage( hwnd_mw )
END IF

RETURN
END
!***********************************************************************
!                abort_print
!***********************************************************************
SUBROUTINE abort_print( HWND )

USE resource_fd
USE pcscipuf_fi
USE winAPI_fd
!
!     This routine sets the flag to cancel the current print job
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: HWND !ABORT BOX Handle

!     Step the cancel flag

lPrintIt = .FALSE.

!     Write Cancel message to ABORT box line 2

string1 = 'Canceling Print Job'
CALL SetControlText( HWND,IDB_STATIC02,string1 )

!     Clear ABORT box lines 1 and 3

string1 = cnull
CALL SetControlText( HWND,IDB_STATIC01,string1 )
CALL SetControlText( HWND,IDB_STATIC03,string1 )

!     Hide ABORT box CANCEL button

CALL EnableControl( HWND,ID_CANCEL,FALSE )
CALL ShowControl  ( HWND,ID_CANCEL,SW_HIDE )

RETURN
END
