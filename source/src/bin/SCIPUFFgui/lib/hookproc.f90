!***********************************************************************
!                SaveAsHook
!***********************************************************************
RECURSIVE FUNCTION SaveAsHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'SaveAsHook' :: SaveAsHook
USE SwinAPI
USE resource_fd
USE files_fi
USE pcscipuf_fi

!     This is the hook message handler for the SaveAs Common dialog box.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: SaveAsHook
INTEGER(POINTER_LEN)    HDLG,WPARAM,LPARAM
INTEGER                 MESSAGE

LOGICAL   done
TYPE( CMD ) MyCmd

INTEGER ICOMM,IFOCUS,id_dialog,id_level

INTEGER irv,indx

!---- Initialize Return Value

SaveAsHook = FALSE

!---- Select Action Based on Message Received

SELECT CASE (MESSAGE)
  CASE (WM_INITDIALOG) !INITIALIZE
    CALL SetHwndList(HDLG,IDB_SAVFIL,id_level) !  Set Dialog ID into list
    CALL init_dialog_SaveAs(HDLG,id_level) !  Initialize the Dialog
    CALL center_window(HDLG) !  Center the Dialog Box in main window
    SaveAsHook = TRUE !  Set reurn value
  CASE (WM_CTLCOLORMSGBOX) !STATIC COLOR
    SaveAsHook = dlgBkBrush
  CASE (WM_CTLCOLORDLG) !STATIC COLOR
    SaveAsHook = dlgBkBrush
  CASE (WM_CTLCOLORSTATIC,WM_CTLCOLORBTN) !STATIC COLOR
    irv = SetTextColor(WPARAM,dlgATxColor)
    irv = SetBkColor(WPARAM,dlgBkColor)
    SaveAsHook = dlgBkBrush
  CASE (WM_DESTROY) !DESTROY
    CALL DeleteHwndList(HDLG) !  Remove Dialog ID from list
  CASE (WM_COMMAND) !COMMANDS
    CALL split_param(WPARAM,ICOMM,IFOCUS) !  Get Command and Focus
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    SELECT CASE (ICOMM) !  Select action by Command
      CASE (ID_OK) !    OK Button
        CALL save_dialog_SaveAs(id_level) !      Save parameters
      CASE (ID_CANCEL) !    CANCEL Button
      CASE (IDB_COMBO1,IDB_COMBO2) !    COMBOBOX
        MyCmd%hwnd   = HDLG !      Set Command Structure
        MyCmd%id     = id_dialog !
        MyCmd%level  = id_level !
        MyCmd%cntrl  = ICOMM !
        MyCmd%type   = ICOMM/100 !
        MyCmd%button = ICOMM - 100*MyCmd%type !
        done   =  IFOCUS == CBN_SELCHANGE !
        IF( done)CALL process_combo(HDLG,MyCmd) !         Process
        SaveAsHook = TRUE !      Set Return Value
      CASE (1136) !Save as file type selection
        IF( IFOCUS == CBN_SELCHANGE )THEN !  Selection changed
          IF( lsave_hook )THEN !  Export from Plot
            CALL GetListSel(HDLG,-1136,1,indx,irv) !  1136=File Type Combo Box
            IF( irv > 0 )THEN !       minus so my routines
              CALL GetListItem(HDLG,-1136,indx,string1,irv) !       see it as a combobox
              IF( irv > 0 )THEN !       instead of a listbox
                ext_hook = string1(irv-3:irv-1)//CHAR(0) !  Reset default Extension
              END IF !       assumes all strings
            END IF !       end with "(*.xxx)"
          END IF !
        END IF !
      CASE DEFAULT
    END SELECT
  CASE DEFAULT
END SELECT

RETURN

END
!***********************************************************************
!                OpenHook
!***********************************************************************
RECURSIVE FUNCTION OpenHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'OpenHook' :: OpenHook
USE SwinAPI
USE resource_fd
USE files_fi
USE pcscipuf_fi

!     This is the hook message handler for the Open Common dialog box.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: OpenHook
INTEGER(POINTER_LEN)   HDLG,WPARAM,LPARAM
INTEGER MESSAGE
INTEGER irv
INTEGER ICOMM,IFOCUS,id_dialog,id_level

LOGICAL   done
TYPE( CMD ) MyCmd

!---- Initialize Return Value

OpenHook = FALSE

!---- Select Action Based on Message Received

SELECT CASE (MESSAGE)
  CASE (WM_INITDIALOG) !INITIALIZE
    CALL SetHwndList(HDLG,IDB_OPNPRJ,id_level) !  Set Dialog ID into list
    CALL init_dialog_OpenFile(HDLG,id_level) !  Initialize the Dialog
    CALL center_window(HDLG) !  Center Dialog Box
    OpenHook = TRUE !  Set Return value
  CASE (WM_CTLCOLORMSGBOX) !STATIC COLOR
    OpenHook = dlgBkBrush
  CASE (WM_CTLCOLORDLG) !STATIC COLOR
    OpenHook = dlgBkBrush
  CASE (WM_CTLCOLORSTATIC,WM_CTLCOLORBTN) !STATIC COLOR
    irv = SetTextColor(WPARAM,dlgATxColor)
    irv = SetBkColor(WPARAM,dlgBkColor)
    OpenHook = dlgBkBrush
  CASE (WM_DESTROY) !DESTROY
    CALL DeleteHwndList(HDLG) !  Remove Dialog ID from list
  CASE (WM_COMMAND) !COMMANDS
    CALL split_param(WPARAM,ICOMM,IFOCUS) !  Get Command and Focus
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    SELECT CASE (ICOMM) !  Select action by Command
      CASE (ID_OK) !    OK Button
        CALL save_dialog_OpenFile(HDLG,id_level) !      Save parameters
      CASE (ID_CANCEL) !    CANCEL Button
      CASE (IDB_COMBO1) !    BUTTON
        MyCmd%hwnd   = HDLG !      Set Command Structure
        MyCmd%id     = id_dialog !
        MyCmd%level  = id_level !
        MyCmd%cntrl  = ICOMM !
        MyCmd%type   = ICOMM/100 !
        MyCmd%button = ICOMM - 100*MyCmd%type !
        done   =  IFOCUS == CBN_SELCHANGE !
        IF( done)CALL process_combo(HDLG,MyCmd) !         Process
        OpenHook = TRUE !      Set Return Value
      CASE (IDB_BUTTON1) !    BUTTON
        MyCmd%hwnd   = HDLG !      Set Command Structure
        MyCmd%id     = id_dialog !
        MyCmd%level  = id_level !
        MyCmd%cntrl  = ICOMM !
        MyCmd%type   = ICOMM/100 !
        MyCmd%button = ICOMM - 100*MyCmd%type !
        CALL process_button(HDLG,MyCmd) !      Process Button
        OpenHook = TRUE !      Set Return Value
      CASE (IDB_CHECK1) !    BUTTON
        MyCmd%hwnd   = HDLG !      Set Command Structure
        MyCmd%id     = id_dialog !
        MyCmd%level  = id_level !
        MyCmd%cntrl  = ICOMM !
        MyCmd%type   = ICOMM/100 !
        MyCmd%button = ICOMM - 100*MyCmd%type !
        CALL process_check(HDLG,MyCmd) !      Process Button
        OpenHook = TRUE !      Set Return Value
      CASE DEFAULT
    END SELECT
  CASE DEFAULT
END SELECT
RETURN
END
!***********************************************************************
!                PrintHook
!***********************************************************************
RECURSIVE FUNCTION PrintHook(HDLG, MESSAGE, WPARAM, LPARAM)
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'PrintHook' :: PrintHook
USE SwinAPI
USE resource_fd
USE files_fi
USE pcscipuf_fi

!     This is the hook message handler for the Print Common dialog box.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: PrintHook
INTEGER(POINTER_LEN)    HDLG,WPARAM,LPARAM
INTEGER MESSAGE
TYPE( CMD ) MyCmd

INTEGER ICOMM,IFOCUS,id_dialog,id_level

INTEGER irv

LOGICAL done

!---- Initialize Return Value

PrintHook = FALSE

!---- Select Action Based on Message Received

SELECT CASE (MESSAGE)
  CASE (WM_INITDIALOG) !INITIALIZE
    CALL SetHwndList(HDLG,IDB_PRTFIL,id_level) !  Set Dialog ID into list
    CALL init_dialog_Print(HDLG,id_level) !  Initialize the Dialog Box
    CALL center_window(HDLG) !  Center the DIlaog Box in Main Window
    PrintHook = TRUE !  Set return value
  CASE (WM_DESTROY) !DESTROY
    CALL DeleteHwndList(HDLG) !  Remove dialog ID from list
  CASE (WM_CTLCOLORMSGBOX) !STATIC COLOR
    PrintHook = dlgBkBrush
  CASE (WM_CTLCOLORDLG) !STATIC COLOR
    PrintHook = dlgBkBrush
  CASE (WM_CTLCOLORSTATIC,WM_CTLCOLORBTN) !STATIC COLOR
    irv = SetTextColor(WPARAM,dlgATxColor)
    irv = SetBkColor(WPARAM,dlgBkColor)
    PrintHook = dlgBkBrush
  CASE (WM_COMMAND) !COMMANDS
    CALL split_param(WPARAM,ICOMM,IFOCUS) !  Get Command and Focus
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    SELECT CASE (ICOMM) !  Select action by Command
      CASE (ID_OK) !    OK Button
        CALL save_dialog_Print(HDLG,id_level) !      Save parameters
        IF( .NOT.lokbutton)PrintHook = TRUE
      CASE (ID_CANCEL) !    CANCEL Button
      CASE (IDB_CHECK1:IDB_CHECK6) !    CHECKBOX
        MyCmd%hwnd   = HDLG !      Set Command Structure
        MyCmd%id     = id_dialog !
        MyCmd%level  = id_level !
        MyCmd%cntrl  = ICOMM !
        MyCmd%type   = ICOMM/100 !
        MyCmd%button = ICOMM - 100*MyCmd%type !
        CALL process_check(HDLG,MyCmd) !      Process Button - Normal CHECKBOX
        PrintHook = TRUE !      Set Return Value
      CASE (IDB_COMBO1) !    COMBO Box
        done   =  IFOCUS == CBN_SELCHANGE !
        IF( done )THEN !
          MyCmd%hwnd   = HDLG !      Set Command Structure
          MyCmd%id     = id_dialog !
          MyCmd%level  = id_level !
          MyCmd%cntrl  = ICOMM !
          MyCmd%type   = ICOMM/100 !
          MyCmd%button = ICOMM - 100*MyCmd%type !
          CALL process_combo(HDLG,MyCmd) !      Process Button - Normal COMBOBOX
        END IF !
        PrintHook = TRUE !      Set Return Value
      CASE DEFAULT
    END SELECT
  CASE DEFAULT
END SELECT
RETURN
END
!***********************************************************************
!                center_window
!***********************************************************************
SUBROUTINE center_window(HWND)

USE SwinAPI
USE resource_fd
USE files_fi
USE pcscipuf_fi

!     This routine positions the Common Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) HWND !Window handle

INTEGER irv
INTEGER iwid
INTEGER ihgt
INTEGER jwid
INTEGER jhgt
INTEGER kwid
INTEGER khgt
INTEGER ilft
INTEGER itop
INTEGER iflg

TYPE( T_RECT ) Box

!---- Check for Main Window - Center in Main Window

IF( IsWindow(hwnd_mw) /= FALSE )THEN

!------ Get Main window client width and height

  irv  = GetClientRect(hwnd_mw,Box)
  iwid = Box%right-Box%left
  ihgt = Box%bottom-Box%top

!------ Check for TOOLBAR and set Top/Left and adjust height

  IF( IsWindow(hwnd_tb) /= FALSE )THEN
    irv  = GetWindowRect(hwnd_tb,Box)
    ilft = Box%left
    itop = Box%bottom
    ihgt = ihgt - (Box%bottom-Box%top)
  ELSE
    irv  = GetWindowRect(hwnd_mw,Box)
    ilft = Box%left
    itop = Box%top + (Box%bottom-Box%top) - ihgt
  END IF

!---- Else center in SCREEN

ELSE

  iwid = GetSystemMetrics(SM_CXSCREEN)
  ihgt = GetSystemMetrics(SM_CYSCREEN)
  ilft = 0
  itop = 0

END IF

!---- Get Dialog Box Width and height

irv  = GetWindowRect(HWND,Box)
kwid = Box%right-Box%left
khgt = Box%bottom-Box%top

!---- Get Dialog Box Client Area

irv = GetClientRect(HWND,Box)
jwid = Box%right-Box%left
jhgt = Box%bottom-Box%top

!---- Adjust Top/Left to Center Dialog in Main window Client area

!      ilft = ilft + iwid/2 - jwid/2
!      itop = itop + ihgt/2 - jhgt/2 - (khgt-jhgt)
ilft = ilft + (iwid - kwid)/2
itop = itop + (ihgt - khgt)/2 !- (khgt-jhgt)

!---- Check to make sure its visible

jwid = GetSystemMetrics(SM_CXSCREEN)
jhgt = GetSystemMetrics(SM_CYSCREEN)

ilft = MAX(0,ilft)
ilft = MIN(ilft,jwid-kwid)
itop = MAX(0,itop)
itop = MIN(itop,jhgt-khgt)

!---- Set flag to prevent change in Z-order and size

iflg = IOR(SWP_NOZORDER,SWP_NOSIZE)

!---- Reposition window to itop,ilft

irv = SetWindowPos(HWND, &
             0, &
             ilft,itop, &
             0,0, &
             iflg)

RETURN
END

