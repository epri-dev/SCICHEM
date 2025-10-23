!***********************************************************************
!               WinMain
!***********************************************************************
RECURSIVE INTEGER FUNCTION WinMain( hInstance,hPrevInst,lpCmdLine,nCmdSh )
!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'WinMain' :: WinMain

USE resource_fd
USE pcscipuf_fi
USE script_fi
USE SwinAPI
USE guiAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) hInstance,hPrevInst,lpCmdLine
INTEGER nCmdSh

INTEGER,              EXTERNAL :: MessageLoop
INTEGER,              EXTERNAL :: InitApp
INTEGER(POINTER_LEN), EXTERNAL :: InitInst

INTEGER irv

IF( hPrevInst /= 0 )GOTO 9999

!     Alternate Check - Check for windows of SCI_WINDOW Class

string2 = CHAR(0)
string1 = TRIM(SCI_WINDOW)//CHAR(0)
!IF( FindWindow(string1,string2) /= 0 )GOTO 9999

!     Application initialization

MainAtom = InitApp( hInstance )
IF( MainAtom == 0 )GOTO 9998

!     Initialize the instance

hwnd_mw = InitInst( hInstance,lpCmdLine,nCmdSh )
IF( hwnd_mw == 0 )GOTO 9997

CALL position_toolbar( hwnd_tb,hwnd_mw )
IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  irv = ShowWindow  ( hwnd_mw, nCmdSh )
  irv = UpdateWindow( hwnd_mw )
END IF

!     Load Accelerators

string1 = 'PlotAccel'//CHAR(0)
hacc_pd = LoadAccelerators( hInstance,ADDRESSOF(string1) )

string1 = 'ToolAccel'//CHAR(0)
hacc_tb = LoadAccelerators( hInstance,ADDRESSOF(string1) )

string1 = 'EditAccel'//CHAR(0)
hacc_ed = LoadAccelerators( hInstance,ADDRESSOF(string1) )

!     Windows event loop

WinMain = MessageLoop()

RETURN

!     Error Section

9999 CONTINUE
string3 = 'SCIPUFF is already in use'//CHAR(13)// &
          'Multiple instances not allowed'//CHAR(13)// &
          'Exit existing copy before launching new copy'
string1 = TRIM(string3)//CHAR(0)
GOTO 9000

9998 CONTINUE
string1 = 'Error initializing the application'//CHAR(0)
GOTO 9000

9997 CONTINUE
string1 = 'Error initializing the instance'//CHAR(0)
GOTO 9000

9000 CONTINUE
string2 ='SCIP'//CHAR(0)
irv = MessageBox( 0,string1,string2,IOR(MB_ICONHAND,MB_OK) )
CALL ExitProcess( 1 )

END
!***********************************************************************
!                MessageLoop
!***********************************************************************
INTEGER FUNCTION MessageLoop()

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER id_dialog
INTEGER id_level

TYPE( T_MSG ) Message

INTEGER irv

DO WHILE( GetMessage(Message,NULL_POINTER,NULL,NULL) /= FALSE )

  IF( IsWindow(hwnd_db) /= FALSE )THEN
    CALL FindHwndList( hwnd_db,id_dialog,id_level )
    IF( id_dialog == IDB_PLOT )THEN
      IF( TranslateAccelerator(hwnd_db,hacc_pd,Message) /= FALSE )CYCLE
    ELSE IF( id_dialog == IDB_EDTPRJ )THEN
      IF( TranslateAccelerator(hwnd_db,hacc_ed,Message) /= FALSE )CYCLE
    END IF
  END IF

  IF( IsWindow(hwnd_tb) /= FALSE )THEN
    IF( TranslateAccelerator(hwnd_tb,hacc_tb,Message) /= FALSE )CYCLE
  END IF

  IF( IsWindow(hwnd_db) /= FALSE )THEN
    IF( IsDialogMessage(hwnd_db,Message) /= FALSE )CYCLE
  END IF

  IF( IsWindow(hwnd_tb) /= FALSE )THEN
    IF( IsDialogMessage(hwnd_tb,Message) /= FALSE )CYCLE
  END IF

  IF( IsWindow(hwnd_pb) /= FALSE )THEN
    IF( IsDialogMessage(hwnd_pb,Message) /= FALSE )CYCLE
  END IF

  IF( IsWindow(hdlgp) /= FALSE )THEN
    IF( IsDialogMessage(hdlgp,Message) /= FALSE )CYCLE
  END IF

  irv = TranslateMessage( Message )
  irv = DispatchMessage ( Message )

END DO

MessageLoop = Message%wParam

RETURN
END
!***********************************************************************
!                InitApplication
!***********************************************************************
INTEGER(2) FUNCTION InitApp( hInstance )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE SwinAPI
USE guiAPI
!
!     This function registers the window class for the main windows.
!
IMPLICIT NONE

INTEGER(POINTER_LEN) hInstance !Instance handle
INTEGER(POINTER_LEN) icur, jnst

INTEGER, PARAMETER :: MY_DKGRAY = 128

TYPE( T_WNDCLASSEX ) WC

!     Set window colors

applBkColor = RGB(MY_DKGRAY,MY_DKGRAY,MY_DKGRAY)
applBkBrush = CreateSolidBrush( applBkColor )

!     Register MAIN Window class

string1 = TRIM(SCI_WINDOW)//CHAR(0)
string2 = TRIM(SCI_ICON)//CHAR(0)

icur = IDC_ARROW
jnst = 0

WC%cbSize        = SIZEOF(WC)
WC%style         = NULL
WC%lpfnWndProc   = ADDRESSOF(MAINWNDPROC)
WC%cbClsExtra    = 0
WC%cbWndExtra    = 0
WC%hInstance     = hInstance
WC%hIcon         = LoadIcon  ( hInstance, string2 )
WC%hCursor       = LoadCursor( jnst,icur )
WC%hbrBackground = applBkBrush
WC%lpszMenuName  = NULL_POINTER !ADDRESSOF(string3)
WC%lpszClassName = ADDRESSOF(string1)
WC%hIconSm       = NULL_POINTER

InitApp = RegisterClassEx( WC )
IF( InitApp == 0 )GOTO 9999

!     Register Plot Window class

string1 = TRIM(PLT_WINDOW)//CHAR(0)

WC%lpfnWndProc   = ADDRESSOF(PLOTWNDPROC)
WC%hIcon         = NULL_POINTER
WC%hCursor       = LoadCursor( jnst,icur )
WC%hbrBackground = GetStockObject( WHITE_BRUSH )
WC%lpszClassName = ADDRESSOF(string1)

PlotAtom = RegisterClassEx( WC )
IF( PlotAtom == 0 )THEN
  InitApp = 0
  GOTO 9999
END IF

!     Register Stat Window class

string1 = TRIM(STAT_WINDOW)//CHAR(0)

WC%lpfnWndProc   = ADDRESSOF(STATWNDPROC)
WC%lpszClassName = ADDRESSOF(string1)

StatAtom = RegisterClassEx( WC )
IF( StatAtom == 0 )THEN
  InitApp = 0
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!                InitInstance
!***********************************************************************
FUNCTION InitInst( hInstance,lpCmdLine,nCmdsh )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE script_fi
USE SwinAPI
USE guiAPI
USE mainbeta
USE SCIPtool

!
!     This function create an instance of this executable
!
IMPLICIT NONE

INTEGER(POINTER_LEN) :: InitInst

INTEGER(POINTER_LEN) hInstance !Instance Handle
INTEGER(POINTER_LEN) lpCmdLine !Command line address
INTEGER nCmdsh

INTEGER irv,jrv
INTEGER istyle
INTEGER iwid
INTEGER ihgt
INTEGER itop
INTEGER ileft

INTEGER ios

INTEGER,       EXTERNAL :: GetGUIVersion
INTEGER,       EXTERNAL :: GetGUIToolVersion
CHARACTER(32), EXTERNAL :: GetGUIVersionString

TYPE( char128T ) charStruct

INTEGER(POINTER_LEN) :: IIH

!     Save Instance handle

MyAppInst = hInstance

InitInst = 0

!     Create Main Window

tool_version    = SCIPGetAPIVersion()
scipuff_version = SCIPGetScipuffVersion()

ios = SCIPGetVersionString( 0,charStruct )
tool_version    = SCIPGetAPIVersion()
scipuff_version = SCIPGetScipuffVersion()

ios = SCIPGetVersionString( 0,charStruct )
ToolVersionString = charStruct%string

IF( tool_version/100 /= GetGUIToolVersion()/100 )THEN
  WRITE(string2,*)GetGUIToolVersion()/100
  string2 = ADJUSTL(string2)
  write(string3,*)tool_version/100
  string3 = ADJUSTL(string3)
  string1 = 'Invalid SCIPtool:User-Interface version'//CHAR(13)// &
            'User-Interface design version ='//TRIM(string2)// &
             CHAR(13)//'SCIPTool dll interface version = '// &
             TRIM(string3)//CHAR(0)
  string2 = 'SCIP'//CHAR(0)
  irv = MessageBox( 0,string1,string2,IOR(MB_ICONHAND,MB_OK) )
  GOTO 9999
END IF

gui_version      = GetGUIVersion()
GUIversionString = GetGUIVersionString()

irv = LoadString( MyAppInst, &
                  IDS_GUIPD, &
                  string3, &
                  LEN(string3) )
jrv = LoadString( MyAppInst, &
                  IDS_MODE, &
                  string1, &
                  LEN(string1) )
IF( jrv /= 0 )THEN
  string_mode = TRIM(string1(1:jrv))
ELSE
  string_mode = 'Unknown'
END IF
IF( irv == 0 )THEN
  string3 = 'Load String Error'
ELSE
  string1 = TRIM(string3(1:irv))//' Version G:'// &
            TRIM(GUIversionString)//'-'//TRIM(ToolVersionString)
  string3 = TRIM(string1)
  irv = LoadString( MyAppInst, &
                    IDS_TAG, &
                    string2, &
                    LEN(string2) )
  IF( irv /= 0 .OR. jrv /= 0 )THEN
    IF( irv /= 0 )THEN
      IF( jrv /= 0 )THEN
        string3 = TRIM(string3)//' ('//string2(1:irv)//':'//TRIM(string_mode)//' mode)'
      ELSE
        string3 = TRIM(string3)//' ('//TRIM(string_mode)//' mode)'
      END IF
    ELSE
      string3 = TRIM(string3)//' ('//TRIM(string_mode)//' mode)'
    END IF
  END IF
END IF

beta_flag = 1 !Devopmental message on main window

string1 = TRIM(SCI_WINDOW)//CHAR(0)
string2 = TRIM(string3)//CHAR(0)

istyle = IOR(WS_POPUP,IOR(WS_MAXIMIZE,IOR(WS_MINIMIZEBOX,IOR(WS_CAPTION,WS_SYSMENU))))

CALL MainWindowSize( ileft,itop,iwid,ihgt )

lDontMove = .FALSE.

IIH = CreateWindow( string1,  &
                         string2,  &
                         istyle,  &
                         ileft,  &
                         itop,  &
                         iwid,  &
                         ihgt,  &
                         NULL_POINTER,  &
                         NULL_POINTER,  &
                         MyAppInst,  &
                         NULL_POINTER ) !Create Param

!     Check for Failure to create the Window.
InitInst = IIH
IF( IIH /= 0 )CALL initialize_instance( IIH,lpCmdLine,nCmdsh )

9999 CONTINUE

RETURN
END
!===============================================================================
!     MainWindowSize
!===============================================================================
SUBROUTINE MainWindowSize( x0,y0,wid,hgt )

USE resource_fd
USE tooluser_fd
USE SwinAPI

IMPLICIT NONE

INTEGER x0,y0,wid,hgt
INTEGER(POINTER_LEN) nullWnd

CHARACTER(128) string,nullString

INTEGER(POINTER_LEN) hwnd
INTEGER irv

INTEGER Swid,Shgt,Bwid,Bhgt

TYPE( T_RECT ) box

Swid  = GetSystemMetrics( SM_CXSCREEN )
Shgt  = GetSystemMetrics( SM_CYSCREEN )
Bwid  = GetSystemMetrics( SM_CXFRAME )
Bhgt  = GetSystemMetrics( SM_CYFRAME )

nullWnd = 0
nullString = CHAR(0)
string = 'Shell_TrayWnd'//CHAR(0)
hwnd   = FindWindowEx( nullWnd,nullWnd,string,nullString )
IF( hwnd == 0 )GOTO 9999

irv = GetWindowRect( hwnd,box )
IF( irv == FALSE )GOTO 9999

IF( box%left <= 0 )THEN
  IF( box%right >= Swid )THEN !Horizontal task bar

    wid = Swid !+ 2*Bwid - 2
    x0  = 0    !-Bwid + 1

    IF( box%top <= 0 )THEN !Across Top

      hgt = Shgt - box%bottom ! + 2*Bhgt - 2
      y0  = box%bottom        !- Bhgt + 1

    ELSE !Across bottom

      hgt = box%top !+ 2*Bhgt - 2
      y0  = 0       !-Bhgt + 1

    END IF

  ELSE !Vertical on Left

    x0  = box%right ! - Bwid + 1
    y0  = 0         !-Bhgt + 1
    wid = Swid - box%right ! + 2*Bwid - 2
    hgt = Shgt             ! + 2*Bhgt - 2

  END IF

ELSE !Vertical on Right

  x0  = 0 !-Bwid + 1
  y0  = 0 !-Bhgt + 1
  wid = box%left !+ 2*Bwid - 2
  hgt = Shgt     !+ 2*Bhgt - 2

END IF

9998 CONTINUE

RETURN

9999 CONTINUE

x0  = 0 !-Bwid - 1
y0  = 0 !-Bhgt - 1
wid = Swid !+ 2*Bwid - 2
hgt = Shgt !+ 2*Bhgt - 2
GOTO 9998

END
!===============================================================================
!     initialize_instance
!===============================================================================
SUBROUTINE initialize_instance( HWND,ICMDL,CMDSH )

USE resource_fd
USE tooluser_fd
USE metparam_fd
USE files_fi
USE errorParam_fd
USE pcscipuf_fi
USE create_fi
USE dialog_fi
USE mettype_fd
USE script_fi
USE GUItool_fi
USE GUImatl_fi
USE SwinAPI
USE guiAPI
USE cscrollbar
USE MyClock

!     Read INI file, Create Toolbar, PlotWindow and SummaryWindow

IMPLICIT NONE

INTEGER(POINTER_LEN) HWND !Window Handle
INTEGER(POINTER_LEN) ICMDL !Command Line pointer
INTEGER              CMDSH

INTEGER, PARAMETER :: MY_GRAY        = 192
INTEGER, PARAMETER :: INACTIVE_RED   = 224
INTEGER, PARAMETER :: INACTIVE_BLUE  = 224
INTEGER, PARAMETER :: INACTIVE_GREEN = 224
INTEGER, PARAMETER :: ACTIVE_RED     = 0
INTEGER, PARAMETER :: ACTIVE_BLUE    = 0
INTEGER, PARAMETER :: ACTIVE_GREEN   = 0
INTEGER, PARAMETER :: CUSTOM_RED     = 255
INTEGER, PARAMETER :: CUSTOM_BLUE    = 0
INTEGER, PARAMETER :: CUSTOM_GREEN   = 0

INTEGER ppi,lsz
INTEGER(POINTER_LEN) hdc, iparam, cursor, jnst, nowind
INTEGER istyle
INTEGER ios
INTEGER ilft
INTEGER itop
INTEGER iwid
INTEGER ihgt
INTEGER id_dialog

INTEGER i
INTEGER j
INTEGER irv,save_mode

TYPE( T_RECT ) wbox

INTEGER request

CHARACTER(128) save_title
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, StripNull

LOGICAL lexist
INTEGER codelim(6)

TYPE( messageT ) errorMsg

TYPE( fileNameT ) ini_str

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine
LOGICAL, EXTERNAL :: hasError, hasWarning

!     Enable Exception control

CALL GUIException()

!     Initialize

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = ' '

lPrintIt = .FALSE.

hwnd_pw = 0
hwnd_sw = 0
hwnd_tb = 0
hwnd_db = 0
hwnd_pb = 0
hdlgp   = 0
hdcp    = 0

nowind=0

lcommand = .FALSE.
icommand = 0
ISCROLL  = 0
IVALUE   = 0
OldCmd%hwnd   = 0
OldCmd%id     = 0
OldCmd%level  = 0
OldCmd%cntrl  = 0
OldCmd%type   = 0
OldCmd%button = 0

! Changed from PARAMETERS to variables to make debugging easier

DEFAULT_LEVEL = -1
BASE_LEVEL    =  0
EDIT_LEVEL    =  1
EDIT_LEVEL_1  =  1
EDIT_LEVEL_2  =  2

CALL InitError()

!     Save the command line

CALL get_command_line( ICMDL,command_line,LEN(command_line) )

!     Initialize command line parameters

!==== Set default INI file value

string4 = 'scipuff.ini'
string2 = ' '
irv = GetModuleFileName( MyAppInst,string2,LEN(string2) )
CALL SplitName( TRIM(StripNull(string2)),string3,string1 )
ini_file = TRIM(string4)
CALL AddPath( ini_file,TRIM(string1) )
INQUIRE( FILE=ini_file,EXIST=lexist )

IF( .NOT.lexist )THEN
  irv = GetWindowsDirectory( string2,LEN(string2) )
  string1  = TRIM(StripNull(string2))//'\'
  ini_file = TRIM(string4)
  CALL AddPath( ini_file,TRIM(string1) )
END IF

Winsci      = .FALSE.
Winsci_Edit = .FALSE.
Winsci_Edit_Dom = .TRUE.
Winsci_Edit_Tim = .TRUE.
Winsci_Edit_Met = .TRUE.
Winsci_Edit_Mat = .TRUE.
Winsci_Edit_Rel = .TRUE.
Winsci_Edit_Opt = .TRUE.
project(DEFAULT_LEVEL)%audit%Classification = ' '
project(DEFAULT_LEVEL)%audit%Analyst        = ' '
file_def = ' '
file_met = ' '
UTM_init   = .TRUE.
Welcome    = .FALSE.
script_file = ' '
pcscipuf_mode = IPM_INTERACTIVE
maxList = MAX_MCX

!     Read command line and reset parameters

CALL read_command_line( HWND,command_line )

INQUIRE( FILE=ini_file,EXIST=lexist )
IF( .NOT.lexist )THEN
  WRITE(111,*)'STOPPING: NO INI FILE FOUND'
  STOP 111
END IF

!     Read INI File to set Paths, Filenames and Logical Unit Numbers

save_mode = pcscipuf_mode
CALL InitFileNames( MyAppInst,HWND,ini_file,string_mode, &
                    GUIversionString,SCIPversionString, &
                    loadfile,codelim, &
                    SCIPdataDir,SCIPtempDir )
pcscipuf_mode = save_mode
IF( hasError() )GOTO 9999

irv = GetWindowText( HWND,string1,LEN(string1) )
irv = INDEX(string1,'(')
string2 = string1(1:irv)//TRIM(string_mode)//' mode)'//CHAR(0)
irv = SetWindowText( HWND,string2 )

CALL init_MainLogo()

project(DEFAULT_LEVEL)%ID%path = TRIM(path_tmp)
i = LEN(TRIM(project(DEFAULT_LEVEL)%ID%path))
IF( project(DEFAULT_LEVEL)%ID%path(i:i) /= '\' )THEN
  i = MIN(i+1,LEN(project(DEFAULT_LEVEL)%ID%path))
  project(DEFAULT_LEVEL)%ID%path(i:i) = '\'
END IF
path_tmp = ' '

! Set Defaults

!==== SCIP Tool

ToolCallBackAddress = ADDRESSOF(ToolCallBack)
ToolCallerID        = 1234
PlotCallerID        = 5678

toolLimits%met1D       = codelim(1)  !machine-dependent INTEGER limit
toolLimits%puffs       = codelim(2)
toolLimits%surfaceGrid = codelim(3)

MAXREL  = codelim(4)
MAXMTYP = MAX(codelim(6),3)
MAXMAUX = MAXMTYP*30

ALLOCATE( dryFrac(MAXREL),STAT=ios )
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eRoutine = 'InitializeInstance'
  eMessage = 'Allocation Error : dryFrac'
  WRITE(eInform,*)'Request=',MAXREL,' : ErrorCode=',ios
  GOTO 9999
END IF
DO i = -1,2
  ALLOCATE( scenario(i)%release(MAXREL),STAT=ios )
  DO j = 1,MAXREL
    NULLIFY( scenario(i)%release(j)%mc )
  END DO
  IF( ios /= 0 )THEN
    nError   = SZ_ERROR
    eRoutine = 'InitializeInstance'
    eMessage = 'Allocation Error : scenario'
    WRITE(eInform,*)'Request=',MAXREL,' : ErrorCode=',ios
    GOTO 9999
  END IF
END DO
DO i = -1,2
  NULLIFY( materials(i)%material )
  NULLIFY( materials(i)%materialMC )
  NULLIFY( materials(i)%mat_aux )
END DO
DO i = -1,2
  CALL AllocateMaterial( materials(i),MAXMTYP,MAXMAUX )
  IF( hasError() )GOTO 9999
END DO

request = 0
IF( UTM_init )request = request + HI_UTM
ini_str%string = TRIM(ini_file)
irv = SCIPInitTool( ToolCallerID,ToolCallBackAddress,request,toolLimits,ini_str )
IF( irv == SCIPfailure )THEN
  irv = SCIPGetLastError( errorMsg )
  IF( irv == SCIPfailure )THEN
    nError   = IV_ERROR
    eRoutine = 'SCIPInitTool'
    eMessage = 'Failed to initialize SCIPtool dll'
    eInform  = 'Unable to continue'
    eAction  = ' '
  ELSE
    nError   = errorMsg%iParm
    eRoutine = 'SCIPInitTool'
    eMessage = errorMsg%aString
    eInform  = errorMsg%bString
    eAction  = errorMsg%cString
  END IF
  GOTO 9999
ELSE
  UTM_init   = UTM_init   .AND. BTEST(irv,HIB_UTM)
  PopData    = .FALSE.
  CALL InitError()
END IF

!==== SCIP Tool

CALL set_default( MyAppInst,HWND )
IF( hasError() )GOTO 9999

!     Load Resources

ClockOn = .FALSE.
Ncur    = 8
string1 = 'Clock0Cursor'//CHAR(0)
DO i = 1,Ncur
  WRITE(string1(6:6),'(I1)')i
  Hcur(i) = LoadCursor( MyAppInst,string1 )
END DO

jnst   = 0
cursor = IDC_CROSS
hcur_cross = LoadCursor( jnst,cursor )
cursor = IDC_ARROW
hcur_arrow = LoadCursor( jnst,cursor )
cursor = IDC_WAIT
hcur_wait  = LoadCursor( jnst,cursor )
string1    = 'OKCheckIcon'//CHAR(0)
check_hndl = LoadIcon( MyAppInst,string1 )
string1    = 'PassiveIcon'//CHAR(0)
scipuff_hndl = LoadIcon( MyAppInst,string1 )
string1    = 'DynamicIcon'//CHAR(0)
dynamic_hndl = LoadIcon( MyAppInst,string1 )
string1    = 'FastIcon'//CHAR(0)
fast_hndl  = LoadIcon( MyAppInst,string1 )
string1    = 'DenseIcon'//CHAR(0)
dense_hndl = LoadIcon( MyAppInst,string1 )

hdc = GetDC(HWND)
ppi = GetDeviceCaps(hdc, LOGPIXELSY)
lsz = NINT(SNGL(9.0D0*DBLE(ppi)/72.d0))
ppi = ReleaseDC(HWND, hdc)

string1  = 'Courier'//cnull
fixfont  = CreateFont( -lsz,0,0,0,400, &
                       0,0,0,0,0,0,1, &
                       0,string1 )

CALL MouseInit()
!     Set up Dialog colors

dlgBkColor = RGB(MY_GRAY,MY_GRAY,MY_GRAY)
dlgBkBrush = CreateSolidBrush( dlgBkColor )
dlgATxColor = RGB(  ACTIVE_RED,  ACTIVE_BLUE,  ACTIVE_GREEN)
dlgITxColor = RGB(INACTIVE_RED,INACTIVE_BLUE,INACTIVE_GREEN)
dlgCTxColor = RGB(  CUSTOM_RED,  CUSTOM_BLUE,  CUSTOM_GREEN)

!     Create Tool Bar

string1 = 'ToolSVGA'//cnull
hwnd_tb = CreateDialogParam( MyAppInst, &
                             string1, &
                             HWND, &
                             ADDRESSOF(TOOLPROC), &
                             HWND )
IF( IsWindow(hwnd_tb) == FALSE )THEN
  eMessage = 'Failed to create ToolBar'
  GOTO 9998
END IF

!     Determine Plot Window Size by opening Plot Dialog Box

id_dialog = IDB_PLOT - 100
iparam    = -IDB_PLOT
string1   = TRIM(DialogName(id_dialog))//cnull
hwnd_db   = CreateDialogParam( MyAppInst, &
                               string1, &
                               HWND, &
                               ADDRESSOF(DIALPROC), &
                               iparam )
IF( IsWindow(hwnd_db) == FALSE )THEN
  eMessage = 'Failed to create PlotDialog'
  GOTO 9998
END IF

CALL get_pwsize( HWND,hwnd_tb,hwnd_db,itop,ilft,ihgt,iwid )
IF( hasError() )GOTO 9999

!---- Need to show main window here for Windows 2000, which seems to
!     lose it when we DestroyWindow for the plot window

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  irv = ShowWindow  ( HWND,CMDSH )
  irv = UpdateWindow( HWND )
END IF

!     Close Dialog Box

irv = DestroyWindow( hwnd_db )
hwnd_db = 0

!     Create Plot Window

string1 = TRIM(PLT_WINDOW)//cnull
string2 = 'Plot'//cnull

istyle = WS_CHILD .OR. WS_VISIBLE .OR. WS_DLGFRAME

hwnd_pw = CreateWindow( string1,  &
                        string2,  &
                        istyle,  &
                        ilft,  &
                        itop,  &
                        iwid,  &
                        ihgt,  &
                        HWND,  &
                        NULL_POINTER,  &
                        MyAppInst,  &
                        NULL_POINTER ) !Create Param
IF( IsWindow(hwnd_pw) /= FALSE )THEN
  irv = ShowWindow  ( hwnd_pw,SW_HIDE )
  irv = UpdateWindow( hwnd_pw )
  CALL SetNCARWindow( hwnd_pw )
  irv = EnableWindow( hwnd_pw,FALSE )
ELSE
  eMessage = 'Failed to create PlotWindow'
  GOTO 9998
END IF

!     Determine Summary Window Size
!       Width = Plot Window Width
!       Height= Ratio to Plot Window to fill page
!               Client Size = 2.75/7.0 Plot Window (assumes page = 9.75x7.0 inches )
!               Window Size = Add difference between plot Window Rect and Client Rect

irv  = GetClientRect( hwnd_pw,wbox )
iwid = wbox%bottom-wbox%top
ihgt = INT((2.75*FLOAT(iwid))/7.0) - iwid

irv  = GetWindowRect( hwnd_pw,wbox )
ihgt = ihgt + wbox%bottom-wbox%top
iwid = wbox%right-wbox%left

!     Create Stat Window

string1 = TRIM(STAT_WINDOW)//cnull
string2 = 'Project Summary - Hit any Key/Button to exit'//cnull

istyle = WS_CHILD .or. WS_VISIBLE .or. WS_CAPTION

hwnd_sw = CreateWindow( string1,  &
                        string2,  &
                        istyle,  &
                        ilft,  &
                        itop,  &
                        iwid,  &
                        ihgt,  &
                        HWND,  &
                        NULL_POINTER,  &
                        MyAppInst,  &
                        NULL_POINTER ) !Create Param
IF( IsWindow(hwnd_sw) /= FALSE )THEN
  irv = ShowWindow  ( hwnd_sw,SW_HIDE )
  irv = UpdateWindow( hwnd_sw )
  irv = EnableWindow( hwnd_sw,FALSE )
ELSE
  eMessage = 'Failed to create SummaryWindow'
  GOTO 9998
END IF

!     Display CAUTION Dialog Box

string3 = AddNull( ' ' )

IF( BTEST(pcscipuf_mode,IPMB_SCRIPT) .AND. .NOT. &
    BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  i   = LEN(save_title)
  irv = GetWindowText( HWND,save_title,i )
  irv = ShowWindow( HWND,SW_SHOWMINIMIZED )
  script_level = 0
  script_table_input = " "
  CALL read_script_file( HWND,script_file )
  IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
    string1 = AddNull( TRIM(save_title) )
    irv = SetWindowText( hwnd_mw,string1 )
    irv = ShowWindow( HWND,SW_SHOWNORMAL )
  END IF
END IF

IF( hasWarning() )THEN
  CALL ShowWarningMessage( HWND, .TRUE. )
ELSE IF( hasError() )THEN
  CALL ShowErrorMessage( HWND )
END IF

RETURN

!     Error Section

9998 CONTINUE

itop     = GetLastError()
nError   = API_ERROR
eRoutine = 'initialize_main'
WRITE(eInform,'(A,I4)')'GetLastError=',itop

9999 CONTINUE

IF( nError /= NO_ERROR )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
CALL ShowErrorMessage( NULL_POINTER )
pcscipuf_mode = IPM_EXIT
CALL DeallocateRelList()
CALL DeallocateMtlList()
IF( ALLOCATED(dryFrac) )DEALLOCATE( dryFrac,STAT=irv )
DO i = -1,2
  IF( ASSOCIATED(scenario(i)%release) )THEN
    DO j = 1,MAXREL
      IF( ASSOCIATED(scenario(i)%release(j)%mc) )DEALLOCATE( scenario(i)%release(j)%mc,STAT=irv )
    END DO
    DEALLOCATE( scenario(i)%release,STAT=irv )
    NULLIFY( scenario(i)%release )
  END IF
  CALL DeallocateMaterial(materials(i))
END DO
CALL PostQuitMessage( NULL )

RETURN
END
!===============================================================================
!     close instance
!===============================================================================
SUBROUTINE close_instance()

USE resource_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE script_fi
USE dialog_fi
USE pltchoice_fi
USE GUItool_fi
USE create_fi
USE GUImatl_fi
USE SwinAPI

!     This routine cleans up before posting a quit message

IMPLICIT NONE

INTEGER i,irv
INTEGER j

CALL ExitPlot()

IF( ALLOCATED(timeRestart) )DEALLOCATE( timeRestart,STAT=irv )
IF( ALLOCATED(PrintTindx)  )DEALLOCATE( PrintTindx, STAT=irv )
IF( ALLOCATED(xytab)       )DEALLOCATE( xytab,      STAT=irv )
IF( ALLOCATED(dblst)       )DEALLOCATE( dblst,      STAT=irv )

irv = DeleteObject( fixfont )
irv = DeleteObject( hbitmap )

CALL exit_MainLogo( .FALSE. )

IF( ALLOCATED(dryFrac) )DEALLOCATE( dryFrac,STAT=irv )
CALL DeallocateRelList()
CALL DeallocateMtlList()

DO i = -1,2
  IF( ASSOCIATED(scenario(i)%release) )THEN
    DO j = 1,MAXREL
      IF( ASSOCIATED(scenario(i)%release(j)%mc) )DEALLOCATE( scenario(i)%release(j)%mc,STAT=irv )
    END DO
    DEALLOCATE( scenario(i)%release,STAT=irv )
    NULLIFY( scenario(i)%release )
  END IF
  CALL DeallocateMaterial( materials(i) )
END DO

!==== SCIP Tool

irv = SCIPExitTool()

CALL DestroyPrint()

IF( IsWindow(hwnd_tb) /= FALSE )irv = DestroyWindow( hwnd_tb ) !Close ToolBar
IF( IsWindow(hwnd_pw) /= FALSE )irv = DestroyWindow( hwnd_pw ) !Close Plot Window
IF( IsWindow(hwnd_sw) /= FALSE )irv = DestroyWindow( hwnd_sw ) !Close Summary Window

CALL ExitNCARGraphics()              !Close NCAR

irv = DeleteObject( applBkBrush )
irv = DeleteObject( dlgBkBrush )

RETURN
END
!===============================================================================
!     version_as_string
!===============================================================================
SUBROUTINE version_as_string( iver,cver )

IMPLICIT NONE

INTEGER iver
CHARACTER(*) cver

CHARACTER(32)  string
INTEGER        i,j

WRITE(string,*)0.001*FLOAT(iver)
string = ADJUSTL(string)

i = LEN(TRIM(string))
j = MAX(1,(i-1))

DO WHILE( i > 0 .AND. string(i:i) == '0' .AND. string(j:j) /= '.' )
  string(i:i) = ' '
  i = i - 1
  j = MAX(1,(i-1))
END DO

IF( string(1:1) == '.' )THEN
  cver = '0'//TRIM(string)
ELSE
  cver = TRIM(string)
END IF

RETURN
END
!===============================================================================
!     read_version_string
!===============================================================================
SUBROUTINE read_version_string( iver,cver )

USE pcscipuf_fi
USE errorParam_fd

IMPLICIT NONE

INTEGER iver
CHARACTER(*) cver

INTEGER iv, jv, j, ios
REAL    ver
LOGICAL period

IF( cver(1:1) == '.' )THEN !Old versions without leading 0
  READ(cver,'(1X,I1)',IOSTAT=ios)jv
  IF( ios /= 0 )GOTO 9998
  iver = 100*jv
ELSE IF( INDEX(cver,'-') <= 0 )THEN !SCIP 3.1 and prior
  READ(cver,'(I1,1X,I1)',IOSTAT=ios)iv,jv
  IF( ios /= 0 )GOTO 9998
  iver = 1000*iv + 100*jv
ELSE
  iv = INDEX(cver,'-') + 1 !SCIP 3.2 alpha4 and prior
  IF( cver(iv:iv) == 'S' )iv = iv + 2 !SCIP 3.2
  period = .FALSE.
  DO jv = iv+1,LEN_TRIM(cver)
    j = ICHAR(cver(jv:jv))
    IF( j >= ICHAR('0') .AND. j <= ICHAR('9') )THEN
      CYCLE
    ELSE
      IF( cver(jv:jv) == '.' )THEN
        IF( .NOT.period )THEN
          period = .TRUE.
          CYCLE
        ELSE
          EXIT
        END IF
      ELSE
        EXIT
      END IF
    END IF
  END DO
  jv = jv-1
  READ(cver(iv:jv),*,IOSTAT=ios)ver
  IF( ios /= 0 )GOTO 9998
  iver = NINT(1000.*ver)
END IF

9999 CONTINUE

RETURN

9998 CONTINUE
CALL SetError( RD_ERROR,'Unable to read project version number', &
               'version='//TRIM(project(BASE_LEVEL)%audit%Version), &
               ' ','read_version_string' )
GOTO 9999

END
!===============================================================================
!     get_command_line
!===============================================================================
SUBROUTINE get_command_line( parray,string,n )

IMPLICIT NONE

INTEGER n
POINTER (parray,iarray)
INTEGER(1),DIMENSION(n) :: iarray
CHARACTER(*), INTENT(OUT) :: string

INTEGER i

string = ' '

i = 1
DO WHILE( (i <= n) .AND. (iarray(i) /= 0) )
  string(i:i) = CHAR(iarray(i))
  i = i + 1
END DO

RETURN
END
!*******************************************************************************
!            Enable FORTRAN exception handling
!*******************************************************************************
SUBROUTINE GUIException()

USE resource_fd
USE tooluser_fd

IMPLICIT NONE

REAL testException

!call fpcontrol(1,FE_DIVBYZERO)
!call fpcontrol(1,FE_OVERFLOW)
!call fpcontrol(1,FE_INVALID)

testException = 12.0*24.0

RETURN
END
!===============================================================================
!                ReadScriptFile
!===============================================================================
RECURSIVE SUBROUTINE read_script_file( hwnd,read_file )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE dialog_fi
USE plotdlg_fi
USE script_fi
USE pltchoice_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) hwnd
CHARACTER(*)        read_file

CHARACTER(20)  keyword
CHARACTER(PATH_MAXLENGTH) valueA,valueB,valueC
CHARACTER(PATH_MAXLENGTH) line
INTEGER     ios,nch,ncc,irv,i,nn,NearestPlotTimeGUI,j
INTEGER     id_level
INTEGER(POINTER_LEN) iwnd_db
INTEGER     ictrl,itype,ibutton,idialog,lun
LOGICAL     lexist,lerr,lok,CheckFile
REAL        rval

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = ' '

!==== Open file

IF( .NOT.CheckFile(read_file) )THEN
  nError   = NF_ERROR
  eMessage = 'Script file not found'
  CALL ReportFileName( eInform,'File=',read_file )
  eAction  = 'Check Path/File name'
  GOTO 9998
END IF
lun = lun_dmp + script_level
OPEN(UNIT=lun,FILE=read_file,STATUS='OLD',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eMessage = 'Unable to open Script file'
  CALL ReportFileName( eInform,'File=',read_file )
  GOTO 9998
END IF

!==== Read keyword

IF( script_level == 0 )THEN
  script_matl  = NOT_SET_I
  script_group = NOT_SET_I
  script_time  = NOT_SET_R
  script_draw  = NOT_SET_I
  script_fill  = NOT_SET_I
END IF
ios = 0
string4 = 'error on first line'
DO WHILE( ios == 0 )

  line = ' '
  READ(lun,'(A)',IOSTAT=ios)line
  IF( ios < 0 )THEN
    GOTO 9998
  ELSE IF( ios > 0 )THEN
    nError = RD_ERROR
    eMessage = 'Error reading script line'
    eInform  = 'Last line='//TRIM(string4)
    eAction  = 'Script reading terminated'
    GOTO 9998
  END IF

  string4 = TRIM(line)

  nch = LEN(TRIM(line))
  IF( nch > 0 )THEN

    CALL get_c( line,nch,' ',keyword,ncc,lerr )
    IF( lerr )THEN
      nError = RD_ERROR
      eMessage = 'Error deciphering script line keyword'
      eInform  = 'Line='//TRIM(string4)
      eAction  = 'Script reading terminated'
      GOTO 9998
    END IF

    CALL cupper( keyword )
    IF( nch > 0 )THEN
      CALL get_c(line,nch,' ',valueA,ncc,lerr)
      IF( lerr )THEN
        nError = RD_ERROR
        eMessage = 'Error deciphering script line data(A)'
        eInform  = 'Line='//TRIM(string4)
        eAction  = 'Script reading terminated'
        GOTO 9998
      END IF
      IF( nch > 0 )THEN
        CALL get_c( line,nch,' ',valueB,ncc,lerr )
        IF( lerr )THEN
          nError = RD_ERROR
          eMessage = 'Error deciphering script line data(B)'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        IF( nch > 0 )THEN
          CALL get_c( line,nch,' ',valueC,ncc,lerr )
          IF( lerr )THEN
            nError = RD_ERROR
            eMessage = 'Error deciphering script line data(C)'
            eInform  = 'Line='//TRIM(string4)
            eAction  = 'Script reading terminated'
            GOTO 9998
          END IF
        ELSE
          valueC = ' '
        END IF
      ELSE
        valueB = ' '
        valueC = ' '
      END IF
    ELSE
      valueA = ' '
      valueB = ' '
      valueC = ' '
    END IF

  ELSE
    keyword = ' '
    valueA = ' '
    valueB = ' '
    valueC = ' '
  END IF

!====== Process keyword

  SELECT CASE( keyword )
    CASE( 'CD' )
      script_input = TRIM(valueA)
      CALL ChangePath( script_input )
      IF( hasError() )GOTO 9998

    CASE( 'TEST' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998

    CASE( 'FILL' )
      script_input = TRIM(valueA)
      CALL cupper( script_input )
      IF( TRIM(script_input) == 'ON' )THEN
        script_fill = TRUE
      ELSE IF( TRIM(script_input) == 'OFF' )THEN
        script_fill = FALSE
      ELSE
        nError = IV_ERROR
        eMessage = 'Invalid input to FILL keyword'
        eInform  = 'Input='//TRIM(script_input)//' : (ON/OFF)'
        eAction  = 'Script reading terminated'
        GOTO 9998
      END IF

    CASE( 'DRAW' )
      script_input = TRIM(valueA)
      CALL cupper( script_input )
      IF( TRIM(script_input) == 'ON' )THEN
        script_draw = TRUE
      ELSE IF( TRIM(script_input) == 'OFF' )THEN
        script_draw = FALSE
      ELSE
        nError = IV_ERROR
        eMessage = 'Invalid input to DRAW keyword'
        eInform  = 'Input='//TRIM(script_input)//' : (ON/OFF)'
        eAction  = 'Script reading terminated'
        GOTO 9998
      END IF

    CASE( 'OPEN' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      CALL open_tool()
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      string1 = AddNull( TRIM(project(BASE_LEVEL)%ID%name) )
      irv = SetWindowText( hwnd_mw,string1 )

    CASE( 'READ' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      script_level = script_level+1
      CALL read_script_file( hwnd,script_input )
      script_level = script_level-1
      IF( pcscipuf_mode /= IPM_SCRIPT )GOTO 9998

    CASE( 'NEW' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      CALL new_tool()
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      IF( valueB /= ' ' )THEN
        script_input = TRIM(valueB)
        CALL SetFullPath( script_input )
        IF( hasError() )GOTO 9998
      END IF
      CALL dialog_tool( IDB_EDTPRJ )
      IF( hasError() )GOTO 9998
      CALL CallButton( hwnd_db,IDB_BUTTON10,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      CALL CallButton( hwnd_db,IDB_BUTTON12,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      CALL dialog_tool( IDB_EDTPRJ )
      IF( hasError() )GOTO 9998
      CALL CallButton( hwnd_db,ID_CANCEL,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      string1 = AddNull( TRIM(project(BASE_LEVEL)%ID%name) )
      irv = SetWindowText( hwnd_mw,string1 )

    CASE( 'START','CONTINUE' )
      IF( valueA /= ' ' )THEN
        i = LEN(TRIM(valueA))
        CALL get_r( valueA,i,rval,lerr )
        IF( lerr )THEN
          nError = RD_ERROR
          eMessage = 'Error deciphering START/CONTINUE duration'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        dlgTime(BASE_LEVEL)%time%end%time%runTime = rval
        CALL ComputeEndTime( dlgTime(BASE_LEVEL)%time%start%time, &
                             dlgTime(BASE_LEVEL)%time%end%time,lok )
        IF( hasError() )GOTO 9998
      END IF
      IF( valueB /= ' ' )THEN
        i = LEN(TRIM(valueB))
        CALL get_r( valueB,i,rval,lerr )
        IF( lerr )THEN
          nError = RD_ERROR
          eMessage = 'Error deciphering START/CONTINUE'// &
                                        ' maximum time step'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        dlgTime(BASE_LEVEL)%time%end%step%max = rval
      END IF
      IF( valueC /= ' ' )THEN
        i = LEN(TRIM(valueC))
        CALL get_r( valueC,i,rval,lerr )
        IF( lerr )THEN
          nError = RD_ERROR
          eMessage = 'Error deciphering START/CONTINUE'// &
                                          ' output interval'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        dlgTime(BASE_LEVEL)%time%end%step%output = rval
        dlgTime(BASE_LEVEL)%default_save = dlgTime(BASE_LEVEL)%default_save &
              .AND. dlgTime(BASE_LEVEL)%time%end%step%output == dlgTime(DEFAULT_LEVEL)%time%end%step%output
      END IF
      IF( TRIM(keyword) == 'START' )THEN
        project(BASE_LEVEL)%Edit = .TRUE.
        CALL create_project( hwnd )
        IF( hasError() )goto 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF
      project(BASE_LEVEL)%Edit = .FALSE.
      CALL create_restart( hwnd )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998

    CASE( 'LOAD' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      CALL dialog_tool( IDB_PLOT )
      IF( hasError() )GOTO 9998
      CALL CallButton( hwnd_db,IDB_BUTTON14,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      CALL CallButton( hwnd_db,ID_OK,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998

    CASE( 'EXPORT' )
      CALL cupper( valueA )
      SELECT CASE( valueA )
        CASE( 'ARC' )
          script_value = isavarc
        CASE( 'BMP' )
          script_value = -999
        CASE( 'PCX' )
          script_value = -999
        CASE( 'OIL' )
          script_value = isavoil
        CASE( 'EIS' )
          script_value = isaveis
        CASE( 'OVL' )
          script_value = isavsci
        CASE( 'TAB' )
          IF( LEN_TRIM(script_table_input) > 0 )THEN
            script_value = isavtab
            nxytab = 0
            CALL read_xytab( hwnd_mw,2,script_table_input )
          END IF
        CASE( 'CTS' )
          script_value = isavcts
        CASE( 'AVS' )
          script_value = isavavs
        CASE( 'USA' )
          script_value = isavusa
        CASE DEFAULT
          script_value = isavsci
      END SELECT
      IF( script_value == -999 )THEN
        nError = RD_ERROR
        eMessage = 'Invalid Export type'
        eInform  = 'Type='//TRIM(valueA)
        eAction  = 'Script reading terminated'
        GOTO 9998
      END IF
      script_input = TRIM(valueB)
      IF( script_input == ' ' )THEN
        nError   = RD_ERROR
        eMessage = 'Empty Export filename'
        eAction  = 'Script reading terminated'
        GOTO 9998
      END IF
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      CALL dialog_tool( IDB_PLOT )
      IF( hasError() )GOTO 9998
      IF( script_matl /= NOT_SET_I )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
        PlotDef(EDIT_LEVEL)%Field%Choice = script_matl
        script_matl  = NOT_SET_I
        idialog = IDB_PLOT     !Prepare COMBO Box call
        ictrl   = CHOICE_COMBO !Control ID (Plot Type)
        itype   = CHOICE_COMBO/CONTROL_INDEX !Type ID (COMBO Box)
        ibutton = CHOICE_COMBO - CONTROL_INDEX*itype !Button ID (Plot Type)
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF
      IF( script_group /= NOT_SET_I )THEN
        IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%kind==SCIPtrue )THEN
          script_group = MAX(1,script_group)
          script_group = MIN(ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%nkind,script_group)
          PlotDef(EDIT_LEVEL)%Field%Kind = script_group + &
                   ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%ikind -1
          script_group  = NOT_SET_I
          idialog = IDB_PLOT   !Prepare COMBO Box call
          ictrl   = KIND_COMBO !Control ID (Plot Type)
          itype   = KIND_COMBO/CONTROL_INDEX !Type ID (COMBO Box)
          ibutton = KIND_COMBO - CONTROL_INDEX*itype !Button ID (Plot Type)
          IF( hasError() )GOTO 9998
          IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
	      ELSE
          script_group  = NOT_SET_I
	      END IF
      END IF
      IF( script_time /= NOT_SET_R )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
	      i = NearestPlotTimeGUI( timePlot,nTimePlot,script_time )
        CALL SetListSelString( iwnd_db,TIME_COMBO,timePlot(i)%string,irv )
        CALL GetListSel( iwnd_db,TIME_COMBO,1,j,irv )
        CALL GetListData( iwnd_db,TIME_COMBO,j,i,irv )
        PlotDef(EDIT_LEVEL)%Field%TimeID = i
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF
      CALL CallButton( hwnd_db,ID_OK,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      lplotOK  = project(BASE_LEVEL)%OK
      lprintOK = lplotOK
      CALL enable_plot( .TRUE. )
      IF( lprintOK )THEN
        CALL SavePltFile( hwnd_mw )
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF

    CASE( 'MATERIAL' )
      script_input = TRIM(valueA)
      CALL cupper( script_input )
      DO i = 1,nPltChoice
        string1 = TRIM(ChoiceStr(i)%string)
        CALL cupper( string1 )
        IF( TRIM(string1) == TRIM(script_input) )THEN
          script_matl = i
        END IF
      END DO
      IF( valueB /= ' ' .AND. script_matl > 0 )THEN
        i = LEN(TRIM(valueB))
        CALL get_i( valueB,i,nn,lerr )
        IF( lerr )THEN
          nError   = RD_ERROR
          eMessage = 'Error deciphering GROUP parameter'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        script_group = nn
      END IF

    CASE( 'TIME' )
      IF( valueA /= ' ' )THEN
        i = LEN(TRIM(valueA))
        CALL get_r( valueA,i,rval,lerr )
        IF( lerr )THEN
          nError   = RD_ERROR
          eMessage = 'Error deciphering START/CONTINUE duration'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        script_time = rval
      END IF

    CASE( 'PRINT' )
      CALL dialog_tool( IDB_PLOT )
      IF( hasError() )GOTO 9998
      IF( script_matl /= NOT_SET_I )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
        PlotDef(EDIT_LEVEL)%Field%Choice = script_matl
        script_matl  = NOT_SET_I
        idialog = IDB_PLOT     !Prepare COMBO Box call
        ictrl   = CHOICE_COMBO !Control ID (Plot Type)
        itype   = CHOICE_COMBO/CONTROL_INDEX !Type ID (COMBO Box)
        ibutton = CHOICE_COMBO - CONTROL_INDEX*itype !Button ID (Plot Type)
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF
      IF( script_group /= NOT_SET_I )THEN
        IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%kind == SCIPtrue )THEN
          script_group = MAX(1,script_group)
          script_group = MIN(ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class, &
                              PlotDef(EDIT_LEVEL)%Field%Choice)%nkind,script_group)
          PlotDef(EDIT_LEVEL)%Field%Kind = script_group + ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%ikind -1
          script_group  = NOT_SET_I
          idialog = IDB_PLOT   !Prepare COMBO Box call
          ictrl   = KIND_COMBO !Control ID (Plot Type)
          itype   = KIND_COMBO/CONTROL_INDEX !Type ID (COMBO Box)
          ibutton = KIND_COMBO - CONTROL_INDEX*itype !Button ID (Plot Type)
          IF( hasError() )GOTO 9998
          IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
		    ELSE
          script_group  = NOT_SET_I
	      END IF
      END IF
      IF( script_time /= NOT_SET_R )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
	      i = NearestPlotTimeGUI( timePlot,nTimePlot,script_time )
        CALL SetListSelString( iwnd_db,TIME_COMBO,timePlot(i)%string,irv )
        CALL GetListSel( iwnd_db,TIME_COMBO,1,j,irv )
        CALL GetListData( iwnd_db,TIME_COMBO,j,i,irv )
        PlotDef(EDIT_LEVEL)%Field%TimeID = i
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      END IF
      IF( script_fill /= NOT_SET_I )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
        lcheck(2,id_level) = script_fill == TRUE
      END IF
      IF( script_draw /= NOT_SET_I )THEN
        CALL FindHwndListId( IDB_PLOT,iwnd_db,id_level )
        lcheck(1,id_level) = script_draw == TRUE
      END IF
      CALL CallButton( hwnd_db,ID_OK,irv )
      IF( hasError() )GOTO 9998
      IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
      lplotOK  = project(BASE_LEVEL)%OK
      lprintOK = lplotOK
      IF( lprintOK )THEN
        CALL cupper( valueA )
        lpplot = .TRUE.
        lpstat = .TRUE.
        lpdate = .TRUE.
        lpbare = .FALSE.
        SELECT CASE( valueA )
          CASE( 'ALL' )
          CASE( 'PLOT' )
            lpstat = .FALSE.
          CASE( 'STAT ')
            lpplot = .FALSE.
          CASE( 'BARE' )
            lpbare = .TRUE.
            lpdate = .FALSE.
            lpstat = .FALSE.
          CASE DEFAULT
        END SELECT
        IF( Nmap_scales(axesdef(BASE_LEVEL)%MapCoord) <= 0 .OR. &
                PlotDef(BASE_LEVEL)%Field%Category == HP_HINT .OR. &
                PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE )THEN
          map_scale = DEF_VAL_R
        ELSE
          CALL cupper( valueB )
          IF( valueB == ' ' )THEN
            map_scale = DEF_VAL_R
          ELSE
            map_scale = DEF_VAL_R
            DO i = 1,Nmap_scales(axesdef(BASE_LEVEL)%MapCoord)
              CALL i_format( Smap_scales(i,axesdef(BASE_LEVEL)%MapCoord),nn,string2 )
              string1 = '1:'//string2
              IF( TRIM(string1) == TRIM(valueB) )THEN
                map_scale = FLOAT(Smap_scales(i,axesdef(BASE_LEVEL)%MapCoord))
              END IF
            END DO
          END IF
        END IF
        CALL PrintIt( hwnd )
        IF( hasError() )GOTO 9998
        IF( BTEST(pcscipuf_mode,IPMB_ERROR) )GOTO 9998
        lplotOK = .FALSE.
      END IF

    CASE( 'GOTO' )
      IF( valueA /= ' ' )THEN
        i = LEN(TRIM(valueA))
        string1 = TRIM(valueA)//':'
        CALL cupper( string1 )
        lexist = .FALSE.
        DO WHILE( .NOT.lexist )
          line = ' '
          READ(lun,'(A)',IOSTAT=ios)line
          IF( ios < 0 )THEN
            GOTO 9997
          ELSE IF( ios > 0 )THEN
            nError   = RD_ERROR
            eMessage = 'Error reading script line'
            eInform  = 'Last line='//TRIM(string4)
            eAction  = 'Script reading terminated'
            GOTO 9998
          END IF
          string4 = TRIM(line)
          CALL cupper( line )
          lexist = TRIM(line) == TRIM(string1)
        END DO
      END IF

    CASE( 'SKIP' )
      IF( valueA /= ' ' )THEN
        i = LEN(TRIM(valueA))
        CALL get_i( valueA,i,nn,lerr )
        IF( lerr )THEN
          nError   = RD_ERROR
          eMessage = 'Error deciphering SKIP parameter'
          eInform  = 'Line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        nn = MAX(1,nn)
      ELSE
        nn = 1
      END IF
      DO i = 1,nn
        line = ' '
        READ(lun,'(A)',IOSTAT=ios)line
        IF( ios < 0 )THEN
          GOTO 9997
        ELSE IF( ios > 0 )THEN
          nError   = RD_ERROR
          eMessage = 'Error reading script line'
          eInform  = 'Last line='//TRIM(string4)
          eAction  = 'Script reading terminated'
          GOTO 9998
        END IF
        string4 = TRIM(line)
      END DO

    CASE( 'TABLE' )
      script_input = TRIM(valueA)
      CALL SetFullPath( script_input )
      IF( hasError() )GOTO 9998
      script_table_input = TRIM(script_input)

    CASE( 'EXIT' )
      CLOSE(UNIT=lun,IOSTAT=ios)
      ios = -1
      pcscipuf_mode = IPM_EXIT
      GOTO 9998

    CASE DEFAULT

  END SELECT

END DO

!==== Close file

9997 CONTINUE
pcscipuf_mode = IPM_INTERACTIVE

9998 CONTINUE

CLOSE(UNIT=lun,IOSTAT=ios)
IF( nError /= NO_ERROR )THEN
  eRoutine = 'ReadScriptFile'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF
IF( script_level == 0 )THEN
  IF( pcscipuf_mode == IPM_EXIT )THEN
    irv = PostMessage(hwnd,WM_DESTROY,0,0)
  ELSE
    pcscipuf_mode = IPM_INTERACTIVE
  END IF
ELSE
  pcscipuf_mode = IPM_SCRIPT
END IF

RETURN
END
