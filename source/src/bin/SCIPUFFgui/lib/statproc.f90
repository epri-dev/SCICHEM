!***********************************************************************
!                STATWNDPROC
!***********************************************************************
RECURSIVE FUNCTION STATWNDPROC(HWND, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'STATWNDPROC' :: STATWNDPROC
USE SwinAPI
USE resource_fd
USE pcscipuf_fi

!     This is the message handler for the summary window.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: STATWNDPROC
INTEGER(POINTER_LEN)    HWND,WPARAM,LPARAM
INTEGER                 MESSAGE

INTEGER(POINTER_LEN) jdc
INTEGER irv

LOGICAL lDefWin

!---- Initialize Default processing and Repositioning flags

lDefWin     = .TRUE.

!---- Select Action Based on Message Received

SELECT CASE (MESSAGE)
 CASE (WM_CREATE) !CREATE
 CASE (WM_SYSKEYDOWN,WM_KEYDOWN, &
 WM_LBUTTONDOWN,WM_RBUTTONDOWN,WM_MBUTTONDOWN) !BUTTON DOWN
    lDefWin = .FALSE. !  Return Value
    irv = EnableWindow(hwnd_sw,FALSE) !  Disable STATS window
    irv = ShowWindow(hwnd_sw,SW_HIDE) !  Hide STATS window
    irv = EnableWindow(hwnd_pw,TRUE) !  Enable PLOT window
    irv = ShowWindow(hwnd_pw,SW_SHOW) !  Show PLOT window
    jdc = GetDC(hwnd_pw)
    CALL ResetPalette(jdc)
    CALL EnableControl(hwnd_db,IDB_BUTTON4,TRUE) !  Enable PRINT Button
    irv = ReleaseCapture() !  Release Mouse Capture
    irv = SetFocus(hwnd_db) !  Return Focus to Dialog Box
    CALL MouseResume !  Resume ZOOMing
 CASE (WM_DESTROY) !DESTROY
    lDefWin = .FALSE. !  Return Value
    CALL PostQuitMessage(NULL) !  Post Quit Message
 CASE (WM_PAINT) !PAINT
    IF( lpaintit )THEN !  PCSCIPUF Paint Message
      CALL DisplayStat(HWND) !    Draw
      lpaintit = .FALSE. !    Reset flag
      lDefWin = .FALSE. !    Return value
    END IF !
 CASE DEFAULT !DEFAULT
END SELECT

!---- Set Return value

IF( lDefWin )THEN
  STATWNDPROC = DefWindowProc(HWND, &
                        MESSAGE, &
                        WPARAM, &
                        LPARAM)
ELSE
  STATWNDPROC = NULL_POINTER
END IF

RETURN
END
