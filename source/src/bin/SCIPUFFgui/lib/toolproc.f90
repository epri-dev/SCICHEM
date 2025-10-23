MODULE HtmlHelpAPI

  INTERFACE
    FUNCTION HTMLHELP( HWNDMAIN, LPSZHELP, UCOMMAND, DWDATA)
!DEC$ ATTRIBUTES DEFAULT, STDCALL, DECORATE, ALIAS: 'HtmlHelpA' :: HTMLHELP
      USE IFWINTY
      INTEGER(HANDLE) :: HTMLHELP
      INTEGER(HANDLE)    HWNDMAIN
!DEC$ ATTRIBUTES REFERENCE, ALLOW_NULL :: LPSZHELP
      CHARACTER*(*)      LPSZHELP
      INTEGER(UINT)      UCOMMAND
      INTEGER(DWORD_PTR) DWDATA
    END FUNCTION
  END INTERFACE

END MODULE HtmlHelpAPI
!***********************************************************************
!                TOOLPROC
!***********************************************************************
RECURSIVE FUNCTION TOOLPROC( HDLG,MESSAGE,WPARAM,LPARAM )

!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'TOOLPROC' :: TOOLPROC

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

!     This is the message handler for the dialog boxes.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: TOOLPROC
INTEGER(POINTER_LEN)    HDLG,WPARAM,LPARAM
INTEGER                 MESSAGE
INTEGER ICOMM
INTEGER IFOCUS
LOGICAL lok
LOGICAL check_tool

!---- Initialize Return Value

TOOLPROC = FALSE

!---- Select Action Based on Message Received

SELECT CASE( MESSAGE )
  CASE( WM_INITDIALOG ) !INITIALIZE
    TOOLPROC = TRUE
    CALL position_toolbar( HDLG,LPARAM )

  CASE( WM_CTLCOLORMSGBOX ) !STATIC COLOR
    TOOLPROC = dlgBkBrush

  CASE( WM_CTLCOLORDLG ) !STATIC COLOR
    TOOLPROC = dlgBkBrush

  CASE( WM_COMMAND ) !COMMANDS
    TOOLPROC = TRUE !  Set Return Value
    CALL split_param( WPARAM,ICOMM,IFOCUS ) !  Get Command and Focus

    SELECT CASE( ICOMM ) !  Select action by Command
      CASE( ID_OK ) !    EXIT Button
        CALL quit_tool( .TRUE. ) !      Exit with verification

      CASE( IDB_HELP ) !    HELP Button
        CALL help_tool() !      Start WinHelp

      CASE(IDB_OPNPRJ ) !    OPEN Button
        IF( check_tool(HDLG,ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL open_tool()             !        OpenFile Dialog
        END IF

      CASE( IDB_NEWPRJ ) !    NEW Button
        IF( check_tool(HDLG,ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL new_tool()           !        SaveAsFile Dialog
        END IF

      CASE( IDB_PLOT,IDB_SCIPUF ) !    PLOT,RUN Buttons
        IF( check_tool(HDLG,ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL dialog_tool( ICOMM ) !        Dialog
        END IF

      CASE( IDB_EDTPRJ ) !    EDIT PseudoButton
        IF( check_tool(HDLG,-ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL dialog_tool( ICOMM ) !        Dialog
        END IF

      CASE( IDB_VIEWPRJ ) !    VIEW button
        IF( check_tool(HDLG,ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL dialog_tool( IDB_EDTPRJ ) !        Dialog
        END IF

      CASE( IDB_STATIC02 ) !    PROJECT Box

      CASE( IDB_BUTTON1 ) !    F12 VirtualKey
        IF( check_tool(HDLG,-ICOMM) )THEN !      If status OK
          CALL f12_tool()   !        Return Main Window to original position
        END IF

      CASE( IDB_BUTTON2 ) !    Create PseadoButton
        IF( check_tool(HDLG,-ICOMM) )THEN !      If status OK
          CALL exit_MainLogo( .TRUE. )
          CALL create_project( hwnd_mw ) !         Create New Project
        END IF

      CASE DEFAULT !    UNKNOWN
        lok = MessageBeep( MB_OK ) !      Beep

    END SELECT

  CASE DEFAULT !DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!                HelpTool
!***********************************************************************
SUBROUTINE help_tool()

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI
USE HtmlHelpAPI
IMPLICIT NONE

LOGICAL lok
INTEGER id_level,id_dialog,id_context,irv
INTEGER(POINTER_LEN)iwnd
INTEGER ilft,itop,jwid,jhgt

INTEGER,        EXTERNAL :: sysGetProfileString
CHARACTER(128), EXTERNAL :: AddNull

!---- Find Top Level Dialog Box

CALL FindHwndListTop( iwnd,id_dialog,id_level )

!---- Set Help Location Based on Dialog ID

SELECT CASE( id_dialog )
!        CASE (IDB_RELNEW,IDB_RELDEL,IDB_RELEDT)                        !Release Definitions
!        CASE (IDB_RELNEW)                                              !Release Definitions
!          id_context = IDB_RELDEF                                      !
!        CASE (IDB_MATNEW)                                              !Material Definitions
!          id_context = IDB_MATDEF                                      !
!        CASE (IDB_EDTLST,IDB_NEWLST)                                    !LIST Editing
!          id_context = listedt.dialog                                   !
  CASE( IDB_COMLST ) !LIST Editing
    id_context = listedt%dialog + 100

  CASE( IDB_METDEF ) !Met
    id_context = IDB_METDEF

  CASE( IDB_OPNPRJ ) !Standard Open/Save
    id_context = IDB_SAVFIL*10

  CASE( IDB_SAVFIL ) !Standard Open/Save
    id_context = IDB_SAVFIL*10
    IF( lsave_hook )THEN
      id_context = id_context + 1
    ELSE IF( lanim_hook )THEN
      id_context = id_context + 2
    END IF

  CASE( IDB_AUDIT ) !LIST Editing
    id_context = IDB_PRJDEF

  CASE DEFAULT !Default = Dialog ID
    id_context = id_dialog

END SELECT

!---- Set Help Filename

string1 = AddNull( 'Paths' )
string2 = AddNull( 'SCIPHelpDir' )
string3 = AddNull( TRIM(path_app) )
string4 = AddNull( TRIM(ini_file) )

irv = sysGetProfileString( string1,string2,string3,path_tmp,string4 )
IF( irv == SCIPfailure )path_tmp = path_app

string1 = TRIM(path_tmp)//'\SCICHEM.chm::/scipufpdContents__PCSCIPUFF_Help.htm>main'
string2 = AddNull( string1 )

!---- Help location is a context location

iwnd = HtmlHelp( hwnd_mw,string2,1,ADDRESSOF(id_context))
lok = iwnd /= NULL_POINTER
IF( lok )THEN
  CALL MainWindowSize( ilft,itop,jwid,jhgt )
  irv = SetWindowPos( iwnd, 0, ilft+jwid/4, itop+jhgt/4, jwid/2, jhgt/2, SWP_NOZORDER ) !  Change flag
END IF

!---- Error starting WinHelp

IF( .NOT.lok )THEN
  string2 = 'Help'//CHAR(0)
  string1 = 'WinHelp failed to execute properly'//CHAR(0)
  irv = MessageBox( hwnd_mw,string1,string2,MB_OK )
END IF

RETURN
END
!***********************************************************************
!                QuitTool
!***********************************************************************
SUBROUTINE quit_tool( lverify )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lverify

INTEGER irv
LOGICAL verify_button,lok

INTEGER(POINTER_LEN) hwnd
INTEGER id,ilev

!---- If toplevel Dialog Box exists - Disable it

CALL FindHwndListTop( hwnd,id,ilev )
IF( IsWindow(hwnd) /= FALSE )irv = EnableWindow( hwnd,FALSE )

!---- Put up Verify Box

IF( lverify )THEN
  string1 = 'SCIPUFF'
  lok = verify_button(hwnd_mw,'Quit the '//TRIM(string1)//' interface program')
ELSE
  lok = .TRUE.
END IF

!---- If toplevel Dialog Box exists - Enable it

IF( IsWindow(hwnd) /= FALSE )irv = EnableWindow( hwnd,TRUE )

!---- User Verified Exit - So Exit

IF( lok )THEN

!------ Destroy MODELESS Dialog Box if Active

  IF( IsWindow(hwnd_db) /= FALSE )irv = DestroyWindow( hwnd_db )

!------ Post QUIT Message

  irv = PostMessage( hwnd_mw,WM_DESTROY,0,0 )
END IF

RETURN
END
!***********************************************************************
!                DialogTool
!***********************************************************************
SUBROUTINE dialog_tool( ICOMM )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI
USE guiAPI

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ICOMM !Dialog ID

INTEGER id_dialog,irv
INTEGER(POINTER_LEN) iparam

CHARACTER(20) BOX

!---- Disable TOOLBOX Buttons (All Except EXIT)

LOGICAL disable_open,linteractive

linteractive = BTEST(pcscipuf_mode,IPMB_INTERACTIVE)

disable_open = ICOMM /= IDB_PLOT .AND. ICOMM /= IDB_EDTPRJ
disable_open = disable_open .OR. project(BASE_LEVEL)%Edit

CALL EnableControl( hwnd_tb,IDB_PLOT,FALSE )
IF( disable_open )CALL EnableControl( hwnd_tb,IDB_OPNPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_NEWPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_SCIPUF,FALSE )
CALL EnableControl( hwnd_tb,IDB_VIEWPRJ,FALSE )

!---- If TOOLBOX Dialog is Active - Destroy it

IF( IsWindow(hwnd_db) /= FALSE )THEN
  irv = DestroyWindow( hwnd_db )
  hwnd_db = 0
END IF

!---- If This is a PLOT Dialog and PLOT window exits - Show it

IF( ICOMM == IDB_PLOT .AND. linteractive )THEN
  IF( IsWindow(hwnd_pw) /= FALSE )THEN
    irv = EnableWindow( hwnd_pw,TRUE )
    irv = ShowWindow( hwnd_pw,SW_SHOW )
    irv = UpdateWindow( hwnd_pw )
  END IF
END IF

!---- Set DIALOG Template name

id_dialog = ICOMM - 100
BOX = TRIM(DialogName(id_dialog))//cnull

!---- Start Dialoging - MODELESS DIALOG

iparam = ICOMM
hwnd_db = CreateDialogParam( MyAppInst, &
                             BOX, &
                             hwnd_mw, &
                             ADDRESSOF(DIALPROC), &
                             iparam )
!---- If Setup - Do NewSetup Dialog First

RETURN
END
!***********************************************************************
!                OpenTool
!***********************************************************************
SUBROUTINE open_tool()

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

LOGICAL lok
INTEGER called_from,from_id,from_lev,irv

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN

!------ If TOOLBOX Dialog is Active - Destroy it

  IF( IsWindow(hwnd_db) /= FALSE )THEN
    CALL FindHwndListTop( called_from,from_id,from_lev )
    IF( called_from /= hwnd_db )THEN
      CALL SetError( OP_ERROR, &
                    'Unable to open project', &
                    'Dialog box is present', &
                    'Please close the current dialog box', &
                    'OpenProject' )
      CALL ShowErrorMessage( hwnd_db )
      RETURN
    ELSE
      CALL CallButton( hwnd_db,ID_CANCEL,irv ) !close with QUIT button
    END IF
  ELSE
    called_from = 0
  END IF
ELSE
  called_from = 0
END IF

!---- Disable TOOLBOX Buttons (All Except EXIT)

CALL EnableControl( hwnd_tb,IDB_PLOT  ,FALSE )
CALL EnableControl( hwnd_tb,IDB_OPNPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_NEWPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_SCIPUF,FALSE )
CALL EnableControl( hwnd_tb,IDB_VIEWPRJ,FALSE )

!---- OpenFile Common Dialog Box Driver

project_setup = .FALSE.

project(EDIT_LEVEL_1) = project(DEFAULT_LEVEL)

CALL GetPrjFile( hwnd_mw )

!---- Enable TOOLBOX Buttons

lok = .TRUE.
Call EnableControlL( hwnd_tb,IDB_PLOT  ,project(BASE_LEVEL)%Plot )
Call EnableControlL( hwnd_tb,IDB_OPNPRJ,lok )
Call EnableControlL( hwnd_tb,IDB_NEWPRJ,lok )
Call EnableControlL( hwnd_tb,IDB_SCIPUF,project(BASE_LEVEL)%Run )
Call EnableControlL( hwnd_tb,IDB_VIEWPRJ,project(BASE_LEVEL)%OK )

IF( called_from /= 0 )CALL PushButton( hwnd_tb,0,from_id,irv ) !close with QUIT button

RETURN
END
!***********************************************************************
!                NewTool
!***********************************************************************
SUBROUTINE new_tool()

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER irv
LOGICAL lok

!---- Disable TOOLBOX Buttons (All Except EXIT)

CALL EnableControl( hwnd_tb,IDB_PLOT  ,FALSE )
CALL EnableControl( hwnd_tb,IDB_OPNPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_NEWPRJ,FALSE )
CALL EnableControl( hwnd_tb,IDB_SCIPUF,FALSE )
CALL EnableControl( hwnd_tb,IDB_VIEWPRJ,FALSE )

!---- SaveAs File Common Dialog Box Driver

project(EDIT_LEVEL_1) = project(DEFAULT_LEVEL)

CALL SavePrjFile( hwnd_mw,lok )

!---- If SUCCESS - TOOLBAR Buttons Re-enabled from EDTPRJ tool

IF( lok )THEN

!------SCIPUFF Project - Use EDIT Psedobutton to get CREATE Dialog Box

!------ Open Project Files

  project(BASE_LEVEL)%Edit = .TRUE.
  DefinedOK = DF_CLEAR
  lok_create = .FALSE.
  IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
    project_setup = .TRUE.
    CALL PushButton( hwnd_tb,0,IDB_EDTPRJ,irv )
  ELSE
    project_setup = .FALSE.
  END IF

!---- CANCEL - Enable TOOLBOX Buttons

ELSE
  lok = .TRUE.
  CALL EnableControlL( hwnd_tb,IDB_PLOT,project(BASE_LEVEL)%Plot )
  CALL EnableControlL( hwnd_tb,IDB_OPNPRJ,lok )
  CALL EnableControlL( hwnd_tb,IDB_NEWPRJ,lok )
  CALL EnableControlL( hwnd_tb,IDB_SCIPUF,project(BASE_LEVEL)%Run )
  CALL EnableControlL( hwnd_tb,IDB_VIEWPRJ,project(BASE_LEVEL)%OK )
END IF

RETURN
END
!***********************************************************************
!                F12Tool
!***********************************************************************
SUBROUTINE f12_tool()

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
!
!     This routine repositions the main window to 0,0
!
IMPLICIT NONE

INTEGER irv,iflg,ileft,itop,iwid,ihgt

CALL MainWindowSize( ileft,itop,iwid,ihgt )

iflg = SWP_NOZORDER !Disable Z-order

irv = SetWindowPos( hwnd_mw,0,ileft,itop,iwid,ihgt,iflg )

CALL position_toolbar( hwnd_tb,hwnd_mw )
CALL reposition_dialog()
CALL reposition_plotwind()

RETURN
END
!***********************************************************************
!                CheckTool
!***********************************************************************
LOGICAL FUNCTION check_tool( iwnd,ibutton )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

!     Check Button/window status
!       ibutton > 0 -> Button must be enabled and main window not iconized
!       ibutton < 0 -> main window not iconized

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER,              INTENT( IN ) :: ibutton

INTEGER(POINTER_LEN) ictrl

IF( ibutton > 0 )THEN
  ictrl = GetDlgItem( iwnd,ibutton )
  check_tool = (IsWindowEnabled(ictrl) /= FALSE) .AND. (IsIconic(hwnd_mw) == FALSE)
ELSE
  check_tool = IsIconic(hwnd_mw) == FALSE
END IF

RETURN
END
!***********************************************************************
!                PositionTool
!***********************************************************************
SUBROUTINE position_toolbar( iwnd,jwnd )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd
INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd

INTEGER, PARAMETER :: NBUTTON = 6
REAL,    PARAMETER :: WBUTTON = .115
REAL,    PARAMETER :: HBUTTON = .04

TYPE( T_RECT ) Box

INTEGER irv,iwid,jwid,kwid,ihgt,iflg,ilft,itop,jhgt
INTEGER(POINTER_LEN)kwnd

CALL MainWindowSize( ilft,itop,jwid,jhgt )
irv  = GetWindowRect( jwnd,Box )    !Get MAIN size
ihgt = Box%bottom-Box%top           !Set Height to MAIN height
iwid = Box%right-Box%left           !Set Width to MAIN width

irv  = GetClientRect( jwnd,Box )   !  Get MAIN Client Area
ihgt = ihgt - (Box%bottom-Box%top) !  MAIN Height - Client Height
iwid = iwid - (Box%right-Box%left) !  Client Width

 !
jhgt = jhgt - ihgt !Adjust Height by difference
jwid = jwid - iwid !Adjust Height by difference
Box%left = 0
Box%top = 0
Box%right = jwid
Box%bottom = jhgt
!      irv  = GetClientRect(jwnd,Box)                              !Get MAIN size
iwid = Box%right-Box%left !  Set Width to MAIN width

jwid = NINT(FLOAT(iwid)*WBUTTON)

kwid = iwid - NBUTTON*jwid

ihgt = NINT(FLOAT(Box%bottom-Box%top)*HBUTTON)

iflg = SWP_NOZORDER .OR. SWP_NOMOVE

irv = SetWindowPos( iwnd,  &
                    0,  &
                    0,0,  &
                    iwid,ihgt,  &
                    iflg) !  Change flag

itop = 0
ilft = 0

iflg = SWP_NOZORDER

kwnd = GetDlgItem( iwnd,IDB_VIEWPRJ )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    kwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + kwid

kwnd = GetDlgItem( iwnd,IDB_NEWPRJ )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + jwid

kwnd = GetDlgItem( iwnd,IDB_OPNPRJ )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + jwid

kwnd = GetDlgItem( iwnd,IDB_SCIPUF )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + jwid

kwnd = GetDlgItem( iwnd,IDB_PLOT )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + jwid

kwnd = GetDlgItem( iwnd,IDB_HELP )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag
ilft = ilft + jwid

kwnd = GetDlgItem( iwnd,ID_OK )
irv = SetWindowPos( kwnd,  &
                    0,  &
                    ilft,itop,  &
                    jwid,ihgt,  &
                    iflg) !  Change flag

IF( lVGA )THEN
  CALL SetControlText( iwnd,IDB_NEWPRJ,'&New' )
  CALL SetControlText( iwnd,IDB_SCIPUF,'&Run' )
  CALL SetControlText( iwnd,IDB_OPNPRJ,'&Open' )
END IF

irv = ShowWindow( iwnd,SW_SHOW ) !   Show Dialog Box

RETURN
END
