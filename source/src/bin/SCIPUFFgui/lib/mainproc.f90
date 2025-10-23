!***********************************************************************
!                MAINWNDPROC
!***********************************************************************
RECURSIVE FUNCTION MAINWNDPROC( HWND,MESSAGE,WPARAM,LPARAM )

!DEC$ ATTRIBUTES STDCALL, DECORATE, ALIAS : 'MAINWNDPROC' :: MAINWNDPROC

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
USE openfix
USE mainbeta

!     This is the message handler for the main window.

IMPLICIT NONE

INTEGER(POINTER_LEN) :: MAINWNDPROC
INTEGER(POINTER_LEN)    HWND,WPARAM,LPARAM
INTEGER                 MESSAGE
LOGICAL lDefWin
LOGICAL lReposition
LOGICAL lPaint,lOK

!---- Initialize Default processing and Repositioning flags

lDefWin     = .TRUE.
lReposition = .FALSE.
lPaint      = .FALSE.

!---- Select Action Based on Message Received

SELECT CASE( MESSAGE )
 CASE( WM_CREATE ) !CREATE
   lDefWin  = .FALSE.  !  default OFF
   lOpening = .FALSE.  !  opening OFF

 CASE( WM_PAINT ) !PAINT
   lDefWin = .FALSE. !  default OFF
   lPaint  = .TRUE.

 CASE( WM_QUERYOPEN ) !OPENING
   lOpening = .TRUE. !  opening ON

 CASE (WM_WINDOWPOSCHANGING) !CHANGING
   IF( .NOT.lOpening )CALL window_change( %VAL(LPARAM) ) !  If not opening Check for repositioning
   lOpening = .FALSE. !  opening OFF

 CASE( WM_MOVE ) !MOVE
   lReposition = .TRUE. !  Reposition ON

 CASE( WM_DESTROY ) !DESTROY
   CALL exit_main() !  Exit
   lDefWin = .FALSE. !  default OFF

!      Following Actions are for menus if we decide to implement

!menu       CASE (WM_COMMAND)                                               !COMMAND
!menu         wNotifyCode = IAND(WPARAM,HI_WORD)                            !  Set Notification code
!menu         call PushButton(hwnd_tb,hwnd_mw,wNotifyCode,irv)
 CASE DEFAULT !DEFAULT

END SELECT

!---- Set Return value

IF( lDefWin )THEN
 MAINWNDPROC = DefWindowProc( HWND,    &
                              MESSAGE, &
                              WPARAM,  &
                              LPARAM )
 IF( lReposition )CALL reposition_dialog()
ELSE
 IF( lPaint )THEN
   CALL show_MainLogo( HWND,beta_flag,lOK )
   IF( lOK )THEN
     MAINWNDPROC = NULL_POINTER
   ELSE
     MAINWNDPROC = DefWindowProc( HWND,    &
                                  MESSAGE, &
                                  WPARAM,  &
                                  LPARAM )
   END IF
 ELSE
   MAINWNDPROC = NULL_POINTER
 END IF
END IF

RETURN
END
!***********************************************************************
!     exit_main
!***********************************************************************
SUBROUTINE exit_main()

USE resource_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE SwinAPI

!     This routine cleans up before posting a quit message

IMPLICIT NONE

!---- Clean up routine

CALL close_instance()

!---- Post Quit Message

CALL PostQuitMessage( NULL )

RETURN
END
!***********************************************************************
!     window_change
!***********************************************************************
SUBROUTINE window_change( wpos )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     Prevents the Main Window from moving if its not minumized if a
!     DialogBox is open and if the DontMove flag is set

IMPLICIT NONE

TYPE( T_WINDOWPOS) wpos

LOGICAL lIconic

!---- Check to see if Main window is Iconic

lIconic = (IsIconic(hwnd_mw) /= FALSE)

!---- Set Movement flag

IF( (IsWindow(hwnd_mw) /= FALSE ) .AND. &
    .NOT.lIconic                  .AND. &
    (IsWindow(hwnd_db) /= FALSE ) .AND. &
     lDontMove                          )THEN
  wpos%flags = IOR(wpos%flags,IOR(SWP_NOMOVE,SWP_NOSIZE))
END IF

RETURN
END
!***********************************************************************
!     init_MainLogo
!***********************************************************************
SUBROUTINE init_MainLogo()

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dib
USE SwinAPI
USE cDSWAlogo

!     initialize by getting handle to DSWA logo

IMPLICIT NONE

INTEGER(POINTER_LEN) LogoDIB
INTEGER sysGetProfileString,irv

CHARACTER(PATH_MAXLENGTH) Logofile

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

TYPE( T_BITMAPINFOHEADER ) hdr

LOGICAL, EXTERNAL :: CheckFile

DSWAbmp = 0
DSWApal = 0

string1 = 'Paths'//CHAR(0)
string4 = AddNull( ini_file )
Logofile = 'scidata'
CALL AddPath( Logofile,SCIPdataDir )
string2 = AddNull( 'scidatadir' )
string3 = AddNull( TRIM(Logofile) )
irv = sysGetProfileString( string1,string2,string3,Logofile,string4 )
IF( irv == SCIPfailure )GOTO 9999

string2 = TRIM(Logofile)//'/SCIPUFFlogo.bmp'//CHAR(0)

LogoDIB = OpenDIB( string2 )
IF( LogoDIB /= 0 )THEN
  irv = DibInfo( LogoDIB,hdr )
  DSWApal = CreateDibPalette( LogoDIB )
  DSWAbmp = BitmapFromDib( LogoDIB,DSWApal )
  IF( DSWAbmp /= 0 )THEN
    szDSWA%x = hdr%biWidth
    szDSWA%y = hdr%biHeight
  ELSE
    szDSWA%x = 0
    szDSWA%y = 0
  END IF
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!     exit_MainLogo
!***********************************************************************
SUBROUTINE exit_MainLogo(lupdate)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
USE cDSWAlogo
!     clean up by destroying handle to DSWA logo

IMPLICIT NONE

LOGICAL lupdate

INTEGER  irv

IF( DSWAbmp /= 0 )THEN
  irv = DeleteObject( DSWAbmp )
  DSWAbmp = 0
  irv = DeleteObject( DSWApal )
  DSWApal = 0
END IF

IF( lupdate )CALL MyUpdateWindow( hwnd_mw,.TRUE. )

RETURN
END
!***********************************************************************
!     show_MainLogo
!***********************************************************************
SUBROUTINE show_MainLogo( iwnd,iflag,lOK )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI
USE cDSWAlogo

!     display DSWA logo

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd
INTEGER              iflag
LOGICAL              lOK

INTEGER irv,ihgt,iwid,scale
INTEGER(POINTER_LEN)idc,jdc,hrv

TYPE( T_PAINTSTRUCT)     PS
TYPE( T_RECT ) Box

CHARACTER(128) eInform

lOK = .FALSE.

eInform = ' '

IF( IsWindowVisible(hwnd_sw) /= FALSE )RETURN !To eliminate problem with stat window

IF( DSWAbmp /= 0 .OR. iflag /= 0 )THEN

  irv = GetClientRect( iwnd,Box )
  irv = InvalidateRect(iwnd,Box,TRUE )

  idc = BeginPaint( iwnd,PS )
  IF( idc == NULL_POINTER )THEN
    eInform = 'Unable to get window DC'
    GOTO 9999
  END IF

  IF( DSWApal /= 0 )THEN
    irv = SelectPalette( idc,DSWApal,TRUE )
    irv = RealizePalette( idc )
    irv = UpdateColors( idc )
  END IF

  IF( IsWindow(hwnd_tb) /= FALSE )THEN
    irv  = GetWindowRect( hwnd_tb,Box )
    ihgt = Box%bottom - Box%top
    CALL background_message( MyAppInst,iwnd,hwnd_tb,idc,dlgATxColor )
  ELSE
    ihgt = 0
  END IF

  IF( DSWAbmp /= 0 )THEN
    jdc = CreateCompatibleDC( idc )
    IF( jdc == NULL_POINTER )THEN
      eInform = 'Unable to create memory DC'
      GOTO 9999
    END IF

    irv = GetClientRect( iwnd,Box )
    IF( irv == FALSE )THEN
      eInform = 'Unable to get client rectangle'
      GOTO 9999
    END IF
    Box%top = Box%top + ihgt

    iwid = Box%right - Box%left
    ihgt = Box%bottom - Box%top
    IF( iwid <= 700 )THEN
      scale = 2
    ELSE
      scale = 1
    END IF
    iwid = Box%left + MAX(0,(iwid/2 - szDSWA%x/(2*scale)))
    ihgt = Box%top  + MAX(0,(ihgt/2 - szDSWA%y/(2*scale)))

    IF( DSWApal /= 0 )THEN
      irv = SelectPalette( jdc,DSWApal,TRUE )
      irv = RealizePalette( jdc )
      irv = UpdateColors( jdc )
    END IF

    hrv = SelectObject( jdc,DSWAbmp )
    IF( hrv == NULL_POINTER )THEN
      eInform = 'Unable to select object into DC'
      GOTO 9999
    END IF

    irv = StretchBlt( idc,iwid,ihgt, &
                      szDSWA%x/scale,szDSWA%y/scale, &
                      jdc,0,0, &
                      szDSWA%x,szDSWA%y, &
                      SRCCOPY )
    IF( irv == NULL )THEN
      eInform = 'Unable to copy bitmap'
      GOTO 9999
    END IF
  ELSE
    jdc = 0
  END IF

  irv = EndPaint( iwnd,PS )

  IF( jdc /= 0 )THEN
    irv = DeleteDC( jdc )
    IF( irv == NULL )THEN
      eInform = 'Unable to delete memory DC'
      GOTO 9999
    END IF
  END IF

  lOK = .TRUE.

END IF

RETURN

9999 CONTINUE

irv = GetLastError()
WRITE(string1,*)'API Error : ',irv
CALL SetError( API_ERROR,string1,eInform,' ','Mainlogo' )
CALL ShowErrorMessage( NULL_POINTER )

RETURN
END

!==============================================================================

SUBROUTINE background_message( iapp,iwnd,twnd,idc,itxcol )

USE resource_fd
USE tooluser_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iapp,iwnd,twnd,idc,inew,iold
INTEGER itxcol

TYPE( T_RECT ) Box

CHARACTER(80) string

INTEGER    nch,imode,irv,ialign,ix,iy,isiz,itcol

IF( IsWindow(twnd) /= FALSE )THEN
  irv = GetWindowRect( twnd,Box )
  nch = -(Box%top - Box%bottom)
ELSE
  nch = 0
END IF

irv = GetClientRect( iwnd,Box )
imode = SetBkMode( idc,TRANSPARENT )
isiz  = Box%right - Box%left
IF( isiz <= 700 )THEN
  isiz = 36
ELSE
  isiz = 64
END IF

Box%top = Box%top + nch

string = 'Arial'
CALL SetFont( idc,string,isiz,0,400,inew,iold )

irv = TA_BASELINE .OR. TA_CENTER
ialign = SetTextAlign( idc,irv )

itcol = SetTextColor( idc,itxcol )

ix = (Box%left+Box%right)/2
iy =  Box%top + isiz

nch = LoadString( iapp, &
                  IDS_UPPER, &
                  string, &
                  LEN(string) )
string(nch+1:nch+1)=CHAR(0)

irv = TextOut( idc,ix,iy,string,nch )

iy =  Box%bottom - isiz/4

nch = LoadString( iapp, &
                  IDS_LOWER, &
                  string, &
                  LEN(string) )
string(nch+1:nch+1)=CHAR(0)

irv = TextOut( idc,ix,iy,string,nch )

irv = SetTextAlign( idc,ialign )
CALL RestoreFont( idc,inew,iold )

irv = SetBkMode( idc,imode )
irv = SetTextColor( idc,itcol )

RETURN
END
