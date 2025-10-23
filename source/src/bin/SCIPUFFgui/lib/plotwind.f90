SUBROUTINE get_pwsize( iwnd,itool,ibox,itop,ilft,ihgt,iwid )

USE resource_fd
USE SwinAPI

!     computes location and height,width of plot window

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd !Window Handle
INTEGER(POINTER_LEN) itool !TOOLBAR Handle
INTEGER(POINTER_LEN) ibox !Dialog Box handle
INTEGER              itop !Top of PLOT Window
INTEGER              ilft !Left of PLOT window
INTEGER              ihgt !Height of PLOT window
INTEGER              iwid !Width of PLOT window

LOGICAL lok,lfix

TYPE( T_RECT ) Box

INTEGER isiz,jhgt,jwid

!---- New Main Window size routine

CALL MainWindowSize( ilft,itop,jwid,jhgt )

!---- Check for TOOLBAR and set TOP accordingly

IF( IsWindow(itool) /= FALSE )THEN
  lok  = GetWindowRect(itool,Box)
  itop = (Box%bottom - Box%top)
ELSE
  itop = 0
END IF

!---- Get Dialog Box size and Set LEFT

lok  = GetWindowRect( ibox,Box )
ilft = Box%right - Box%left

!---- Open/Hide  Main Window if its Iconized

IF( IsIconic(iwnd) /= FALSE )THEN
  lok = ShowWindow( iwnd,SW_HIDE )
  lok = OpenIcon( iwnd )
  lfix = .TRUE.
ELSE
  lfix = .FALSE.
END IF

!---- Get Main Window Size

lok = GetWindowRect( iwnd,Box )
ihgt = Box%bottom-Box%top !  Set Height to MAIN height
iwid = Box%right-Box%left !  Set Width to MAIN width

!---- Get Main Window Client Size

lok = GetClientRect( iwnd,Box )

ihgt = ihgt - (Box%bottom-Box%top) !  MAIN Height - Client Height
iwid = iwid - (Box%right-Box%left) !  Client Width

jhgt = jhgt - ihgt
jwid = jwid - iwid !Adjust Height by difference

Box%right  = jwid
Box%bottom = jhgt

!---- Restore Main window if it was originally closed

IF( lfix )THEN
  lok = CloseWindow( iwnd )
  lok = ShowWindow( iwnd,SW_SHOW )
END IF

!---- Compute available space

ihgt = Box%bottom - itop
iwid = Box%right - ilft

!---- Compute PLOT Window Size

isiz = MIN(iwid,ihgt)

iwid = isiz
ihgt = isiz

RETURN
END

!===============================================================================

SUBROUTINE reposition_plotwind()

USE resource_fd
USE pcscipuf_fi
USE SwinAPI
USE guiAPI

IMPLICIT NONE

INTEGER iflg,irv,ilft,itop,iwid,ihgt
INTEGER id_dialog
INTEGER(POINTER_LEN)hwnd,iparam

id_dialog = IDB_PLOT - 100
iparam    = -IDB_PLOT
string1   = TRIM(DialogName(id_dialog))//cnull
hwnd      = CreateDialogParam( MyAppInst, &
                               string1, &
                               hwnd_mw, &
                               ADDRESSOF(DIALPROC), &
                               iparam )
IF( IsWindow(hwnd)==FALSE )GOTO 9998

CALL get_pwsize( hwnd_pw,hwnd_tb,hwnd,itop,ilft,ihgt,iwid )

irv = DestroyWindow( hwnd )

iflg = SWP_NOZORDER !Prevent change in Z order

irv = SetWindowPos( hwnd_pw,  &
                    0,  &
                    ilft,itop,  &
                    iwid,ihgt,  &
                    iflg ) !  Change flag

9998 CONTINUE

RETURN
END
