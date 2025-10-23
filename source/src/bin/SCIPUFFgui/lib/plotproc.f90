!***********************************************************************
!                PLOTWNDPROC
!***********************************************************************
RECURSIVE FUNCTION PLOTWNDPROC(HWND, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PLOTWNDPROC' :: PLOTWNDPROC
USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This is the message handler for the plot window.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: PLOTWNDPROC
INTEGER(POINTER_LEN)    HWND,WPARAM,LPARAM
INTEGER                 MESSAGE
LOGICAL MouseCheck
LOGICAL MouseUp
LOGICAL MouseDown
LOGICAL MouseMove
LOGICAL lDefWin
LOGICAL lTest

INTEGER IX
INTEGER IY

INTEGER ICOMM,IDUM

INTEGER irv

!---- Initialize Default processing and Repositioning flags

lDefWin     = .TRUE.

!---- Select Action Based on Message Received

SELECT CASE (MESSAGE)
 CASE (WM_CREATE) !CREATE
    CALL MouseLoad(MyAppInst) !
    lpaintit = .FALSE. !  Paint OFF
    lflashit = .FALSE. !  Flash OFF
    lDefWin  = .FALSE. !  Default OFF
    PlayBack = .FALSE. !  Animation PlayBack OFF
    SyncMode = .FALSE. !  Run-time plotting off
 CASE (WM_DESTROY) !DESTROY
    CALL MouseExit !
 CASE (WM_SETCURSOR) !CURSOR
    CALL MouseCursor !  Set Cursor
    lDefWin  = .FALSE. !  Default OFF
 CASE (WM_MOUSEMOVE) !MOUSE MOVE
    CALL split_param(LPARAM,IX,IY) !  Mouse position
	lTest = MouseMove(IX,IY)
    IF( lTest )THEN !  If Processed
      lDefWin  = .FALSE. !    Default OFF
    END IF !
 CASE (WM_LBUTTONDOWN) !LEFT BUTTON DOWN
    CALL split_param(LPARAM,IX,IY) !  Mouse position
    IF( MouseDown(IX,IY,.FALSE.) )THEN !  If Processed
      lDefWin  = .FALSE. !    Default OFF
    END IF !
 CASE (WM_RBUTTONDOWN) !RIGHT BUTTON DOWN
    CALL split_param(LPARAM,IX,IY) !  Mouse position
    IF( MouseDown(IX,IY,.TRUE.) )THEN !  If Processed
      lDefWin  = .FALSE. !    Default OFF
    END IF !
 CASE (WM_LBUTTONUP,WM_RBUTTONUP) !RIGHT BUTTON UP
    CALL split_param(LPARAM,IX,IY) !  Mouse position
    IF( MouseUp() )THEN !  If Processed
      lDefWin  = .FALSE. !    Default OFF
    END IF !
 CASE (WM_COMMAND) !COMMAND
    lDefWin = .FALSE. !  Default Off
    CALL split_param(WPARAM,ICOMM,IDUM) !  Get Command and Focus
    SELECT CASE (ICOMM) !  Select action by Command
      CASE (IDB_BUTTON18) !    ABORT plot
        CALL enable_plot(.FALSE.) !      Set Flag
      CASE DEFAULT !    DEFAULT
    END SELECT !      Nothing
 CASE (WM_PAINT) !PAINT
    IF( lflashit )THEN !  Mouse Message - Zooming
      lflashit = .FALSE. !    Flash OFF
      CALL FlashPaint !    Flash Zoom Box
    ELSE IF( lpaintit )THEN !  Button Message - Draw
      lpaintit = .FALSE. !    Paint OFF
      CALL DisplayIt(HWND) !    Draw Plot
      lDefWin  = .FALSE. !    Default OFF
    ELSE IF( PlayBack )THEN !  Button Message - Animate playBack
      PlayBack = .FALSE.
      CALL PlayItBack(HWND) !    Draw Plot
    ELSE !  System Message
      IF( lplotOK.AND.lprintOK )THEN !    If PLOT is OK
        CALL RefreshIt(HWND,hbitmap) !      Redraw - Bitmap
        lDefWin  = .FALSE. !      Default OFF
      ELSE !    Else
        CALL EraseIt(HWND) !      Redraw - Erase
        lDefWin  = .FALSE. !      Default OFF
      END IF !
    END IF !
 CASE (WM_PALETTECHANGED) !Palette Changed - Realize palette
    IF( WPARAM /= HWND )THEN !  Check to make sure it wasn't me who changed the palett
      IF( lplotOK.AND.lprintOK )THEN !    If PLOT is OK
        CALL RefreshIt(HWND,hbitmap) !      Redraw - Bitmap
        lDefWin  = .FALSE. !      Default OFF
      END IF !
    END IF !
 CASE (WM_TIMER) !TIME OUT
    IF( WPARAM == 1)CALL FlashBox !  ZOOM Time out - Set Flash Box
    lDefWin  = .FALSE. !  Default OFF
 CASE DEFAULT !DEFAULT
    IF( MouseCheck() )THEN !  If ZOOM is active
      irv = MessageBeep(MB_OK) !    Beep
      lDefWin  = .FALSE. !    Default OFF
    END IF !
END SELECT !

!---- Set Return value

IF( lDefWin )THEN
  PLOTWNDPROC = DefWindowProc(HWND, &
                        MESSAGE, &
                        WPARAM, &
                        LPARAM)
ELSE
  PLOTWNDPROC = NULL_POINTER
END IF

RETURN
END
!***********************************************************************
!     split_param
!***********************************************************************
SUBROUTINE split_param(iparam,ilo,ihi)

!     Splits a word into hi and low parts
USE winAPI_fd

IMPLICIT NONE

INTEGER,PARAMETER :: LOWWORD_MASK = 65535
INTEGER,PARAMETER :: HIWORD_SHIFT = 16

INTEGER(POINTER_LEN) iparam !Word
INTEGER ihi !Hi Part
INTEGER ilo !Low Part
INTEGER word32

word32 = iparam
ilo = IAND(word32,LOWWORD_MASK)
ihi = ISHFT(word32,-HIWORD_SHIFT)

RETURN

END
!***********************************************************************
!     split_param
!***********************************************************************
SUBROUTINE split_paramX(iparam,ilo,ihi)

!     Splits a word into hi and low parts
USE winAPI_fd

IMPLICIT NONE

INTEGER(POINTER_LEN),PARAMETER :: LOWWORD_MASK = 65535
INTEGER(POINTER_LEN),PARAMETER :: HIWORD_SHIFT = 16

INTEGER(POINTER_LEN) iparam
INTEGER ihi !Hi Part
INTEGER ilo !Low Part

ilo = IAND(iparam,LOWWORD_MASK)
ihi = ISHFT(iparam,-HIWORD_SHIFT)

RETURN

END
!***********************************************************************
!     make_param
!***********************************************************************
SUBROUTINE make_param(iparam,ilo,ihi)

!     Makes a word from hi and low parts

USE winAPI_fd
USE ifwbase

IMPLICIT NONE

INTEGER(POINTER_LEN) iparam !Word
INTEGER ihi !Hi Part
INTEGER ilo !Low Part
INTEGER(2) jhi !Hi Part
INTEGER(2) jlo !Low Part

jhi = ihi
jlo = ilo
iparam = MakeWParam(jlo,jhi)

RETURN

END
!***********************************************************************
!                enable_plot
!***********************************************************************
SUBROUTINE enable_plot(lok)

USE plotcan

IMPLICIT NONE

LOGICAL lok

lPlotIt = lok

RETURN
END
!***********************************************************************
!                check_plot
!***********************************************************************
SUBROUTINE check_plot(lok)

USE plotcan

IMPLICIT NONE

LOGICAL lok

lok = lok .AND. lPlotIt

RETURN
END
