RECURSIVE FUNCTION DIALPROC( HDLG, MESSAGE, WPARAM, LPARAM )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'DIALPROC' :: DIALPROC

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE script_fi
USE mouse_fi
USE GUItool_fi
USE SwinAPI
USE cscrollbar

!     This is the message handler for the dialog boxes.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: DIALPROC
INTEGER(POINTER_LEN)    HDLG,WPARAM,LPARAM
INTEGER MESSAGE

INTEGER ICOMM
INTEGER IFOCUS
INTEGER id_dialog
INTEGER id_level
LOGICAL done
LOGICAL lok
LOGICAL licon
INTEGER irv
TYPE( CMD ) MyCmd

!---- Initialize Return Value

DIALPROC = FALSE

!---- Select Action Based on Message Received

SELECT CASE( MESSAGE )
  CASE( WM_INITDIALOG ) !INITIALIZE
    id_dialog = ABS(LPARAM) !  Get Dialog ID from LPARAM
    IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
      CALL position_dialog( HDLG,id_dialog,1 ) !  Position Dialog Box
    END IF
    CALL SetHwndList( HDLG,id_dialog,id_level ) !  Set Dialog ID into List
    IF( LPARAM >= 0 )THEN !  Check for Initialization
      CALL init_dialog( HDLG,id_dialog,id_level ) !   Initialize Dialog Box
      IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
        irv = ShowWindow( HDLG,SW_SHOW ) !   Show Dialog Box
      END IF
    END IF
    DIALPROC = TRUE !  Set Return Value
    lcommand = .FALSE.
    IF( id_dialog == IDB_PLOT )THEN
    END IF

  CASE( WM_CTLCOLORMSGBOX ) !STATIC COLOR
    DIALPROC = dlgBkBrush

  CASE( WM_CTLCOLORDLG ) !STATIC COLOR
    DIALPROC = dlgBkBrush
!        CASE (WM_CTLCOLOREDIT)                                       !STATIC COLOR
!          DIALPROC = GetStockObject (0)
  CASE (WM_CTLCOLORLISTBOX) !STATIC COLOR
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    IF( id_dialog == IDB_PRJDEF )THEN
      DIALPROC = dlgBkBrush
    ELSE IF( id_dialog == IDB_SETUP )THEN
      DIALPROC = dlgBkBrush
    ELSE IF( id_dialog == IDB_RESTART )THEN
      DIALPROC = dlgBkBrush
    ELSE
      DIALPROC = GetStockObject (0)
    END IF
  CASE (WM_CTLCOLORSTATIC) !STATIC COLOR
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    irv = SetTextColor(WPARAM,dlgATxColor)
    irv = SetBkColor(WPARAM,dlgBkColor)
    DIALPROC = dlgBkBrush
  CASE (WM_CTLCOLORBTN) !STATIC COLOR
    CALL FindHwndList(HDLG,id_dialog,id_level) !  Find Dialog ID from List
    irv = SetTextColor(WPARAM,dlgATxColor)
    irv = SetBkColor(WPARAM,dlgBkColor)
    DIALPROC = dlgBkBrush

  CASE( WM_DESTROY ) !DESTROY
    CALL FindHwndList( HDLG,id_dialog,id_level ) !  Find Dialog ID from List
    CALL DeleteHwndList( HDLG )                  !  Remove Dialog from List
    lcommand = .FALSE.

  CASE( WM_HSCROLL ) !SCROLL
    CALL split_paramX( WPARAM,ICOMM,IFOCUS ) !  Get Command and Value
    done = (ICOMM == SB_ENDSCROLL)          !  Check for done scrolling
    IF( done )THEN
      CALL FindHwndList( HDLG,id_dialog,id_level ) !  Find Dialog ID from List
      MyCmd%hwnd   = HDLG         !    Build Command Structure
      MyCmd%id     = id_dialog
      MyCmd%level  = id_level
      MyCmd%cntrl  = LPARAM
      MyCmd%type   = ISCROLL
      MyCmd%button = IVALUE
      CALL process_scroll( HDLG,MyCmd ) !    Process
      lcommand = .FALSE.
    ELSE
       ISCROLL = ICOMM
       IVALUE  = IFOCUS
    END IF

  CASE( WM_DRAWITEM ) !COMMANDS
    DIALPROC = TRUE !  Set Return Value
    CALL owner_draw( HDLG,(LPARAM) )

  CASE( WM_COMMAND ) !COMMANDS
    CALL split_param( WPARAM,ICOMM,IFOCUS )      !  Get Command and Focus
    CALL FindHwndList( HDLG,id_dialog,id_level ) !  Find Dialog ID from List
	  IF( id_dialog > 0 )THEN
      DIALPROC = TRUE      !  Set Return Value
      SELECT CASE( ICOMM ) !  Select action by Command
        CASE( ID_OK )      !  OK Button
          IF( lcommand )THEN
            SELECT CASE( icommand )
              CASE( 5 )
                CALL process_edit( HDLG,OldCmd )
              CASE( 6 )
                CALL process_real( HDLG,OldCmd )
              CASE( 7 )
                CALL process_int( HDLG,OldCmd )
              CASE( 8 )
                CALL process_combo( HDLG,OldCmd )
              CASE( 12 )
                CALL process_real8( HDLG,OldCmd )
              CASE DEFAULT
            END SELECT
            lcommand = .FALSE.
          END IF
          CALL check_dialog( HDLG,id_dialog,id_level,lok ) ! Check Parameters
          IF( lok )THEN
            CALL save_dialog( HDLG,id_dialog,id_level )    !  Save Parameters
            CALL exit_dialog( HDLG,id_dialog )             !  Exit Dialog Box
          END IF

        CASE( ID_CANCEL ) !    CANCEL Button
          CALL cancel_dialog( HDLG,id_dialog )             !  Cancel Parameters
          CALL exit_dialog( HDLG,id_dialog )               !  Exit Dialog Box
          lcommand = .FALSE.

        CASE DEFAULT !    Other Commands
          MyCmd%hwnd   = HDLG !      Build Command Structure
          MyCmd%id     = id_dialog
          MyCmd%level  = id_level
          MyCmd%cntrl  = ICOMM !
          MyCmd%type   = ICOMM/100
          MyCmd%button = ICOMM - 100*MyCmd%type
          SELECT CASE( MyCmd%type ) !      Select Action by Type
            CASE( 2 ) !        PUSHBUTTON
              lok   = IFOCUS /= 1 !         Check for processing
              licon = IsIconic(hwnd_mw) /= FALSE
              done  = lok .OR. (.NOT.licon)
              IF( done )THEN
                IF( lcommand )THEN
                  SELECT CASE( icommand )
                    CASE( 5 )
                      CALL process_edit( HDLG,OldCmd )
                    CASE( 6 )
                      CALL process_real( HDLG,OldCmd )
                    CASE( 7 )
                      CALL process_int( HDLG,OldCmd )
                    CASE( 8 )
                      CALL process_combo( HDLG,OldCmd )
                    CASE( 12 )
                      CALL process_real8( HDLG,OldCmd )
                    CASE DEFAULT
                  END SELECT
                  lcommand = .FALSE.
                END IF
                CALL process_button( HDLG,MyCmd ) !         Process
              END IF

            CASE( 3 ) !        CHECKBOX
              CALL process_check( HDLG,MyCmd ) !         Process

            CASE( 4 ) !        RADIOBUTTON
              CALL process_radio( HDLG,MyCmd ) !         Process

            CASE( 5 ) !        EDIT - CHARACTER
              done = IFOCUS == EN_KILLFOCUS !         check for done editing
              IF( done )THEN
                CALL process_edit( HDLG,MyCmd ) !         Process
                lcommand = .FALSE.
              ELSE
                lcommand = .TRUE.
                icommand = MyCmd%type
                OldCmd   = MyCmd
              END IF

            CASE( 6 ) !        EDIT - REAL
              done = IFOCUS == EN_KILLFOCUS !         check for done editing
              IF( done )THEN
                IF( .NOT.LBDown )THEN
                  CALL process_real( HDLG,MyCmd ) !         Process
                  lcommand = .FALSE.
                END IF
              ELSE
                lcommand = .TRUE.
                icommand = MyCmd%type
                OldCmd   = MyCmd
              END IF

            CASE( 7 ) !        EDIT - INTEGER
              done = IFOCUS == EN_KILLFOCUS !         check for done editing
              IF( done )THEN
                CALL process_int( HDLG,MyCmd ) !         Process
                lcommand = .FALSE.
              ELSE
                lcommand = .TRUE.
                icommand = MyCmd%type
                OldCmd   = MyCmd
              END IF

            CASE( 8 ) !        COMBO BOX
              done = IFOCUS == CBN_SELCHANGE !
              IF( done )THEN
                CALL process_combo( HDLG,MyCmd ) !         Process
                lcommand = .FALSE.
              ELSE
                lcommand = .TRUE.
                icommand = MyCmd%type
                OldCmd   = MyCmd
              END IF

            CASE ( 9 ) !        STATIC BOX

            CASE( 12 ) !        EDIT - DOUBLE
              done = IFOCUS == EN_KILLFOCUS !         check for done editing
              IF( done )THEN
                IF( .NOT.LBDown )THEN
                  CALL process_real8( HDLG,MyCmd ) !         Process
                  lcommand = .FALSE.
                END IF
              ELSE
                lcommand = .TRUE.
                icommand = MyCmd%type
                OldCmd   = MyCmd
              END IF

            CASE DEFAULT !        UNKNOWN COMMAND
              irv = MessageBeep( NULL ) !         Beep

          END SELECT !
      END SELECT !
	  END IF

  CASE ( WM_TIMER ) !TIME OUT

  CASE DEFAULT !DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!                Exit Dialog Box
!***********************************************************************
SUBROUTINE exit_dialog( HDLG,id_dialog )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE script_fi
USE pltchoice_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) HDLG !Dialog Handle
INTEGER              id_dialog !Dialog ID

INTEGER irv
LOGICAL   lok

IF( ltool )THEN !TOOLBOX DIALOG - Modeless
  irv = DestroyWindow( HDLG ) !    Destroy Window
  hwnd_db = 0 !    Zero out Dialog handle
  lok = .TRUE.
  CALL EnableControlL(hwnd_tb,IDB_PLOT  ,project(BASE_LEVEL)%Plot) !    Enable PLOT Button
  CALL EnableControlL(hwnd_tb,IDB_OPNPRJ,lok) !    Enable OPEN Button
  CALL EnableControlL(hwnd_tb,IDB_NEWPRJ,lok) !    Enable NEW Button
  CALL EnableControlL(hwnd_tb,IDB_SCIPUF,project(BASE_LEVEL)%Run) !    Enable RUN Button
  CALL EnableControlL(hwnd_tb,IDB_VIEWPRJ,project(BASE_LEVEL)%OK) !    Enable RUN Button
  CALL MouseStop !    Stop the ZOOM
  IF( id_dialog == IDB_PLOT )THEN !    Check for PLOT Dialog
    CALL FreePlotField(0) !Clear plot fields
    irv = EnableWindow(hwnd_pw,FALSE) !      Disable PLOT window
    irv = ShowWindow(hwnd_pw,SW_HIDE) !      Hide PLOT Window
    lplotOK = .FALSE. !      Set PLOT flag false
  ELSE IF( id_dialog == IDB_SCIPUF )THEN
    IF( project(BASE_LEVEL)%Plot .AND. lokbutton )THEN
      CALL EnableControlL(hwnd_tb,IDB_PLOT,project(BASE_LEVEL)%Plot) !Enable Button
      CALL CallButton(hwnd_tb,IDB_PLOT,irv) !Call RUN Button
    END IF
  ELSE IF( id_dialog == IDB_EDTPRJ )THEN
    IF( .NOT.lokbutton )THEN
      IF( project(BASE_LEVEL)%OK .AND. project(BASE_LEVEL)%Edit .AND. &
                  BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
        IF( project(BASE_LEVEL)%Run )THEN
          CALL EnableControlL(hwnd_tb,IDB_SCIPUF,project(BASE_LEVEL)%Run) !Enable Button
          CALL PushButton(hwnd_tb,IDB_SCIPUF,IDB_SCIPUF,irv) !Call RUN Button
        ELSE
          IF( project(BASE_LEVEL)%Puff )THEN
            project(BASE_LEVEL)%Run = timePuff(nTimePuff).nItems > 0
            IF( .NOT.project(BASE_LEVEL)%Run )THEN
              nTimePuff = nTimePuff - 1
            END IF
          ELSE
            project(BASE_LEVEL)%Run = .NOT. project(BASE_LEVEL)%Grid
          END IF
          CALL PushButton( hwnd_tb,IDB_EDTPRJ,IDB_EDTPRJ,irv ) !Call EDIT Button
        END IF
      END IF
    END IF
  END IF
ELSE !MODAL DIALOG
  irv = EndDialog( HDLG,TRUE ) !  End Dialog
  IF( ldestroy )THEN           !  Check Mouse flag
    CALL MouseClear( .TRUE. )  !  Clear Mouse
    ldestroy = .FALSE.         !  Set Mouse flag
  END IF !
END IF

RETURN
END
!***********************************************************************
!                Position Dialog Box
!***********************************************************************
SUBROUTINE position_dialog( HWND,id_dialog,iszflg )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE files_fi
USE SwinAPI

!     This routine positions the dialog box

IMPLICIT NONE

INTEGER(POINTER_LEN) HWND !Dialog handle
INTEGER              iszflg !Sizing flag
INTEGER              id_dialog !Dialog ID

INTEGER irv
INTEGER ilft
INTEGER itop
INTEGER iflg
INTEGER iwid
INTEGER ihgt
INTEGER jhgt
INTEGER jwid
INTEGER(POINTER_LEN) IWND
REAL    XWID,YHGT
LOGICAL lok
LOGICAL lfix

TYPE( T_RECT ) Box

!---- Set Action base on Dialog ID

XWID = 0.0
YHGT = 0.0

SELECT CASE( id_dialog )
  CASE( IDB_PLOT,IDB_PLTOPT,IDB_PLTMORE,IDB_SLICE,IDB_ZOOM,IDB_MAPS,  &
        IDB_LABELS,IDB_AXES,IDB_CONTOUR,IDB_PLAY,IDB_PICK )
    IWND = hwnd_mw !  Parent = Main

  CASE( IDB_MATNEW ) !MATERIAL Dialogs
    CALL FindHwndListId( IDB_MATDEF,IWND,irv ) !  Parent = MATDEF

  CASE( IDB_COMLST,IDB_EDTLST,IDB_NEWLST ) !LIST Dialogs
    CALL FindHwndListId( listedt%dialog,IWND,irv ) !  Parent = listedt
    IF( listedt%dialog == IDB_CONTOUR )THEN
      IF( id_dialog == IDB_COMLST )THEN
        XWID = 0.8
        YHGT = -0.8
      ELSE
        XWID = 1.0
        YHGT = -1.4
      END IF
    END IF

  CASE( IDB_RELNEW ) !RELEASE Dialogs
    CALL FindHwndListId( IDB_RELDEF,IWND,irv ) !  Parent = RELDEF
    YHGT = 0.2

  CASE( IDB_EDTPRJ,IDB_SCIPUF,IDB_AUDIT,IDB_SETUP,IDB_ERRORBX ) !EDIT/RUN Dialogs
    CALL center_window( HWND ) !  Center Window
    IWND = 0                   !  No further action

  CASE( IDB_ANIMAT )           !Animate Dialogs
    CALL center_window( HWND ) !  Center Window
    IWND = 0                   !  No further action

  CASE( IDB_XYTABLE )          !XYTABLE Dialogs
    CALL center_window( HWND ) !  Center Window
    IWND = 0                   !  No further action

  CASE( IDB_DELPRJ )           !Animate Dialogs
    CALL center_window( HWND ) !  Center Window
    IWND = 0                   !  No further action

  CASE DEFAULT !DEFAULT
    IWND = 0 !  No further action

END SELECT !

!---- No Further Action required - Return

IF( IWND == 0 )RETURN

!---- Non-Plot Dialogs - Position in center of Parent

IF( IWND /= hwnd_mw )THEN

  irv  = GetWindowRect( IWND,Box ) !  Get Parent Size
  jwid = Box%right  - Box%left     !  Parent width
  jhgt = Box%bottom - Box%top      !  Parent Height

  itop = Box%top  !  Set Top to Parent top
  ilft = Box%left !  Set Left to Parent left

  irv  = GetWindowRect( HWND,Box ) !  Get Dialog Size
  iwid = Box%right  - Box%left     !  Dialog Width
  ihgt = Box%bottom - Box%top      !  Dialog Height

  itop = itop + jhgt/2 - ihgt/2   !Adjust top to center
  ilft = ilft + jwid/2 - iwid/2   !Adjust left to center
  IF( IWND == iwnd_dbr )THEN      !If RELDEL is parent
    itop = itop + 30 !  Adjust top down
  END IF

  ilft = ilft + NINT(XWID*FLOAT(iwid))
  itop = itop + NINT(YHGT*FLOAT(ihgt))

ELSE

!---- Plot Dialogs

  IF( IsIconic(hwnd_mw) /= FALSE )THEN  !  If MAIN is Iconic
    lok = ShowWindow( hwnd_mw,SW_HIDE ) !    Hide MAIN
    lok = OpenIcon( hwnd_mw )           !    Open MAIN to default size
    lfix = .TRUE.
  ELSE
    lfix = .FALSE.
  END IF !

  CALL MainWindowSize( ilft,itop,jwid,jhgt )

  irv = GetWindowRect( hwnd_mw,Box )  !  Get MAIN size
  ilft = Box%left                     !  Set Left to MAIN left
  itop = Box%top                      !  Set Top to MAIN top
  ihgt = Box%bottom - Box%top         !  Set Height to MAIN height
  iwid = Box%right  - Box%left        !  Set Width to MAIN width

  irv  = GetClientRect( hwnd_mw,Box ) !  Get MAIN Client Area
  ihgt = ihgt - (Box%bottom-Box%top)  !  MAIN Height - Client Height
  iwid = iwid - (Box%right-Box%left)  !  Client Width

  itop = itop + ihgt !Adjust Top by difference-Assumes all on top,not quite tr
  jhgt = jhgt - ihgt !Adjust Height by difference

  ilft = ilft + iwid/2 !Adjust Left by half difference

  IF( IsWindow(hwnd_tb) /= FALSE )THEN !  Check for TOOLBOX
    irv = GetWindowRect( hwnd_tb,Box ) !  Get TOOLBOX size
    itop = itop + (Box%bottom-Box%top) !  Adjust Top
    jhgt = jhgt - (Box%bottom-Box%top) !  Adjust height
  END IF

  irv  = GetWindowRect(HWND,Box) !  Get Dialog Size
  iwid = Box%right  - Box%left   !  Dialog Width
  ihgt = Box%bottom - Box%top    !  Dialog Height

  IF( iszflg <= 1 )THEN
    ihgt = MAX(ihgt,jhgt) !Adjust Dialog Height <= Height
  ELSE
    ihgt = jhgt
  END IF

  IF( lfix )THEN !  If flag set
    lok = CloseWindow( hwnd_mw )        !    Iconize MAIN
    lok = ShowWindow( hwnd_mw,SW_SHOW ) !    Show MAIN
  END IF
END IF

iflg = SWP_NOZORDER           !  Prevent change in Z order
IF( iszflg == 0 )THEN         !  If sizing flag not set
  iflg = iflg .OR. SWP_NOSIZE !  Prevent size change
END IF !

irv = SetWindowPos( HWND,  &
                    0,  &
                    ilft,itop,  &
                    iwid,ihgt,  &
                    iflg  ) !  Change flag

RETURN
END
!***********************************************************************
!                Reposition Dialog Box
!***********************************************************************
SUBROUTINE reposition_dialog()

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine repositions the DIALOG Box - Called when ever the
!     MAIN window moves

IMPLICIT NONE

INTEGER id_dialog
INTEGER id_level

IF( IsIconic(hwnd_mw) == FALSE )THEN   !  If MAIN is not Iconic
  IF( IsWindow(hwnd_db) /= FALSE )THEN !  If DIALOG is Valid window
    CALL FindHwndList( hwnd_db,id_dialog,id_level ) !    Find Dialog ID
    IF( id_dialog == IDB_PLOT .OR.  &
        id_dialog == IDB_SCIPUF .OR.  &
        id_dialog == IDB_EDTPRJ )THEN !
      CALL position_dialog( hwnd_db,id_dialog,2 ) !  Reposition
    END IF
  END IF
END IF

RETURN
END
