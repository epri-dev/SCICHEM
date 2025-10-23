!***********************************************************************
!                Create Progress Box
!***********************************************************************
SUBROUTINE create_progress( iwnd,iflg )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI
USE guiAPI
USE cfocus_pb

!     This routine creates the progress box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd !Window Handle
INTEGER,              INTENT( IN ) :: iflg !Creation Flag

INTEGER irv,jflg
INTEGER(POINTER_LEN)iparam

!---- Create Dialog Box

IF( iflg < 0 )THEN
  jflg = iflg + 2
ELSE
  jflg = iflg
END IF

ifocus_pb = GetFocus()

IF( jflg == 3 )THEN
  string1 = 'Small'//TRIM(DB_PROGRESS)//CHAR(0)
ELSE
  string1 = TRIM(DB_PROGRESS)//CHAR(0)
END IF

iparam = jflg
hwnd_pb = CreateDialogParam( MyAppInst,string1, &
                             iwnd, &
                             ADDRESSOF(PROGPROC),iparam )

!---- Check for SUCCESS

IF( hwnd_pb == NULL_POINTER )THEN
  irv = GetLastError()
  WRITE(string1,'(A,I6)')'Last Error = ',irv
  CALL SetError( API_ERROR, &
                'Failed to create progress dialog box', &
                 string1, &
                'Call for Technical assistance', &
                'Create Progress' )
  GOTO 9999
END IF

CALL init_progressbar()

!---- Disable TOOLBAR EXIT Button

IF( IsWindow(hwnd_tb) /= FALSE )CALL EnableControl( hwnd_tb,ID_OK,FALSE )

IF( iflg >= 0 )THEN
  irv = SetCapture( hwnd_pb )  !  Capture mouse
  irv = SetCursor( hcur_wait ) !  Set Arrow
END IF

9999 CONTINUE

RETURN
END
!***********************************************************************
!                Kill Progress Box
!***********************************************************************
SUBROUTINE kill_progress()

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI
USE cfocus_pb

!     This routine kills the progress box

IMPLICIT NONE

INTEGER irv

!---- Kill Dialog Box

irv = DestroyWindow( hwnd_pb )

hwnd_pb = 0

irv = SetCursor( hcur_arrow ) !  Set Arrow
irv = ReleaseCapture()        !  Release Mouse

IF( IsWindow(hwnd_db) /= FALSE )irv = EnableWindow( hwnd_db,TRUE )

!---- Enable TOOLBAR EXIT Button

IF( IsWindow(hwnd_tb) /= FALSE )CALL EnableControl( hwnd_tb,ID_OK,TRUE )

irv = SetFocus( ifocus_pb )

RETURN
END
!***********************************************************************
!                Enable Halt Button in  Progress Box
!***********************************************************************
SUBROUTINE enable_halt( iflag )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine enables the halt button in the progress box

IMPLICIT NONE

INTEGER, INTENT( IN ) ::iflag

INTEGER irv,jflag

jflag = ABS(iflag)

!---- Enable/Show HALT Button (IDCANCEL - Middle Button)

!     ID_CANCEL <=> IDB_BUTTON2
IF( jflag /= 1 )THEN
  CALL EnableControl( hwnd_pb,IDB_BUTTON2,TRUE )
  CALL ShowControl  ( hwnd_pb,IDB_BUTTON2,SW_SHOWNORMAL )
  IF( iflag >= 0 )CALL ShowControl( hwnd_pb,IDB_STATIC12,SW_SHOWNORMAL )
END IF

!---- Enable/Show STOP Button (IDB_BUTTON2 - Left Button)

!     IDB_BUTTON2 <=> IDB_BUTTON1
IF( iflag <= 0 .OR. iflag == -1 )THEN
  CALL EnableControl( hwnd_pb,IDB_BUTTON1,TRUE )
  CALL ShowControl  ( hwnd_pb,IDB_BUTTON1,SW_SHOWNORMAL )
  IF( iflag < 0 )THEN
    string1 = 'Next Iteration'
  ELSE
    string1 = 'Next Output'
  END IF
  CALL SetControlText( hwnd_pb,IDB_STATIC11,string1 )
  CALL ShowControl   ( hwnd_pb,IDB_STATIC11,SW_SHOWNORMAL )
END IF

!---- Enable/Show ABORT Button (IDB_BUTTON1 - Right Button)

!     IDB_BUTTON1 <=> IDB_BUTTON3
IF( iflag >= -1 )THEN
  CALL EnableControl( hwnd_pb,IDB_BUTTON3,TRUE )
  CALL ShowControl  ( hwnd_pb,IDB_BUTTON3,SW_SHOWNORMAL )
  IF( iflag < 0 )THEN
    string1 = 'SCIPUFF'
  ELSE
    string1 = 'Immediate'
  END IF
  CALL SetControlText( hwnd_pb,IDB_STATIC13,string1 )
  CALL ShowControl   ( hwnd_pb,IDB_STATIC13,SW_SHOWNORMAL )
END IF

IF( IsWindow(hwnd_db) /= FALSE )irv = EnableWindow( hwnd_db,FALSE )

!---- Redraw the Dialog Box

irv = UpdateWindow( hwnd_pb )

irv = SetCursor( hcur_arrow ) !Set Arrow
irv = ReleaseCapture()        !Release Mouse

RETURN
END
!***********************************************************************
!                Disable Halt Button in  Progress Box
!***********************************************************************
SUBROUTINE disable_halt( iflag )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine disables the halt button in the progress box

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iflag

INTEGER irv

!---- Enable/Show HALT Button

!     ID_CANCEL <=> IDB_BUTTON2
IF( iflag <= 1 )THEN
  CALL EnableControl( hwnd_pb,IDB_BUTTON2,FALSE )
  CALL ShowControl  ( hwnd_pb,IDB_BUTTON2,SW_HIDE )
  CALL ShowControl  ( hwnd_pb,IDB_STATIC12,SW_HIDE )
END IF

!---- Enable/Show Stop Button

!     IDB_BUTTON2 <=> IDB_BUTTON1
CALL EnableControl( hwnd_pb,IDB_BUTTON1,FALSE )
CALL ShowControl  ( hwnd_pb,IDB_BUTTON1,SW_HIDE )
CALL ShowControl  ( hwnd_pb,IDB_STATIC11,SW_HIDE )

!---- Enable/Show ABORT Button

!     IDB_BUTTON1 <=> IDB_BUTTON3
IF( iflag <= 0 )THEN
  CALL EnableControl( hwnd_pb,IDB_BUTTON3,FALSE )
  CALL ShowControl  ( hwnd_pb,IDB_BUTTON3,SW_HIDE )
  CALL ShowControl  ( hwnd_pb,IDB_STATIC13,SW_HIDE )
END IF

!---- Redraw the Dialog Box

irv = UpdateWindow( hwnd_pb )

IF( iflag == 0 )THEN
  irv = SetCursor( hcur_wait ) !Set Cursor
  irv = SetCapture( hwnd_pb )  !Capture mouse
END IF

RETURN
END
!***********************************************************************
!                Check Progress Box
!***********************************************************************
SUBROUTINE check_messages()

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine checks for progress box messages

IMPLICIT NONE

TYPE( T_MSG) Message

INTEGER irv

!---- Check Main Window Messages

DO WHILE( PeekMessage(Message,hwnd_mw,NULL,NULL,1) /= FALSE )
  irv = TranslateMessage( Message )
  irv = DispatchMessage( Message )
END DO

!---- Check TOOLBAR messages

IF( IsWindow(hwnd_tb) /= FALSE )THEN
  DO WHILE( PeekMessage(Message,hwnd_tb,NULL,NULL,1) /= FALSE )
    IF( IsDialogMessage(hwnd_tb,Message) == FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO
END IF

!---- Check Dialog Box Messages

IF( IsWindow(hwnd_db) /= FALSE )THEN
  DO WHILE( PeekMessage(Message,hwnd_db,NULL,NULL,1) /= FALSE )
    IF( IsDialogMessage(hwnd_db,Message) == FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO
END IF

!---- Check Progress Box Messages

IF( IsWindow(hwnd_pb) /= FALSE )THEN
  DO WHILE( PeekMessage(Message,hwnd_pb,NULL,NULL,1) /= FALSE )
    IF( IsDialogMessage(hwnd_pb,Message) == FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO
END IF

RETURN
END
!***********************************************************************
!                Write Progress Box
!***********************************************************************
SUBROUTINE post_write_progress( m1,m2,m3 )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine updates the progress box display

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: m1,m2,m3

CHARACTER(128) postit

INTEGER irv

!---- Line 1

IF( m1(1:1) /= CHAR(0) )THEN
  postit = ADJUSTL(m1)
  CALL SetControlText( hwnd_pb,IDB_STATIC01,postit )
END IF

!---- Line 2

IF( m2(1:1) /= CHAR(0) )THEN
  postit = ADJUSTL(m2)
  CALL SetControlText( hwnd_pb,IDB_STATIC02,postit )
END IF

!---- Line 3

IF( m3(1:1) /= CHAR(0) )THEN
  postit = ADJUSTL(m3)
  CALL SetControlText( hwnd_pb,IDB_STATIC03,postit )
END IF

!---- Refresh Display

irv = UpdateWindow( hwnd_pb )

RETURN
END
!***********************************************************************
!                Position Progress Box
!***********************************************************************
SUBROUTINE position_progress( HWND,iflg )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI

!     This routine positions the progress box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: HWND !Window Handle
INTEGER,              INTENT( IN ) :: iflg !Creation flag

CHARACTER(32) cver

INTEGER irv,iwid,ihgt,jwid,jhgt,kwid,khgt,ilft,itop,istyl

TYPE( T_RECT ) Box

!---- Position in middle of lower half of Dialog Box : FLAG=1

IF( iflg == 1 )THEN
  irv  = GetClientRect( hwnd_db,Box ) !DialogBox Client
  iwid = Box%right-Box%left           !Dialog Width
  ihgt = 0.52*(Box%bottom-Box%top)    !Dialog half Height
  irv  = GetWindowRect( hwnd_db,Box ) !Dialog Position
  ilft = Box%left                     !Dialog LEFT
  itop = Box%top + ihgt               !Dialog TOP

!---- Position in middle of PLOT window : FLAG=2

ELSE IF( iflg == 2 )THEN
  irv  = GetClientRect( hwnd_pw,Box ) !PLOT Client
  iwid = Box%right-Box%left           !PLOT Width
  ihgt = Box%bottom-Box%top           !PLOT Height
  irv  = GetWindowRect( hwnd_pw,Box ) !PLOT Position
  ilft = Box%left                     !PLOT LEFT
  itop = Box%top                      !PLOT TOP

!---- Position at the top of Dialog Box : FLAG=3

ELSE IF( iflg == 3 )THEN
  irv  = GetClientRect( hwnd_db,Box ) !DialogBox Client
  iwid = Box%right-Box%left           !Dialog Width
  irv  = GetWindowRect( hwnd_db,Box ) !Dialog Position
  ilft = Box%left                     !Dialog LEFT
  itop = Box%top                      !Dialog TOP

!---- Position in middle of Main window

ELSE
  irv  = GetClientRect( hwnd_mw,Box ) !MAIN Client
  iwid = Box%right-Box%left           !MAIN Width
  ihgt = Box%bottom-Box%top           !MAIN Height
  IF( IsWindow(hwnd_tb) /= FALSE )THEN !Check to TOOLBAR
    irv  = GetWindowRect( hwnd_tb,Box ) !  TOOLBAR Size/position
    ilft = Box%left                    !  MAIN LEFT
    itop = Box%bottom                  !  MAIN TOP
    ihgt = ihgt - (Box%bottom-Box%top) !  ADJUST MAIN Height
  ELSE !NO TOOLBAR
    ilft = 0 !  MAIN LEFT
    itop = 0 !  MAIN TOP
  END IF
END IF

!---- Get Progress Box dimensions

irv  = GetWindowRect( HWND,Box ) !Progress Size
kwid = Box%right-Box%left        !Progress Width
khgt = Box%bottom-Box%top        !Progress Height

irv  = GetClientRect( HWND,Box ) !Progress Client
jwid = Box%right-Box%left        !Progress Client Width
jhgt = Box%bottom-Box%top        !Progress Client Height

!---- Compute Progress Box Position

IF( iflg /= 3 )THEN
  ilft = ilft + iwid/2 - jwid/2               !Progress LEFT - centered
  itop = itop + ihgt/2 - jhgt/2 - (khgt-jhgt) !Progress TOP - centered and
END IF

!---- Position Progress Box

istyl = SWP_NOSIZE !Prevent Size change

irv = SetWindowPos( HWND,HWND_TOPMOST,ilft,itop,0,0,istyl )
IF( irv == FALSE )THEN
  irv = GetLastError()
  WRITE(string1,'(A,I6)')'Last Error = ',irv
  CALL SetError( API_ERROR, &
                'Failed to position progress dialog box', &
                 string1, &
                'Call for Technical assistance', &
                'Position Progress' )
  GOTO 9999
END IF

!---- Set Progress Box Title

SELECT CASE( iflg )

  CASE( 1 )
    irv = LoadString( MyAppInst,IDS_GUIPD,cver,LEN(cver) )
    string1 = 'ARAP : '//TRIM(cver(1:irv))//TRIM(ToolVersionString)//CHAR(0)

  CASE( 2 )
    string1 = 'Copy Plot'//CHAR(0)

  CASE( 3 )
    string1 = 'Animate'//CHAR(0)

  CASE DEFAULT
    irv = LoadString( MyAppInst,IDS_SCIPUFF,cver,LEN(cver) )
    string1 = 'ARAP : '//TRIM(cver(1:irv))//TRIM(ToolVersionString)//CHAR(0)

END SELECT

irv = SetWindowText( HWND,string1 )

!---- Refresh Windows

irv = UpdateWindow( hwnd_mw )
IF( IsWindow(hwnd_tb) /= FALSE )irv = UpdateWindow( hwnd_tb )
IF( IsWindow(hwnd_db) /= FALSE )irv = UpdateWindow( hwnd_db )
irv = UpdateWindow( hwnd_pb )

9999 CONTINUE

RETURN
END
!***********************************************************************
!                Pause Progress Box
!***********************************************************************
SUBROUTINE pause_progress()

USE SwinAPI

!     This routine sleeps for 2 seconds - Only works in NT

IMPLICIT NONE

CALL Sleep( 2000 )

RETURN
END
!***********************************************************************
!                Show/Hide Progress Box
!***********************************************************************
SUBROUTINE show_progress( jflg,iflg )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine creates the progress box

IMPLICIT NONE

INTEGER, INTENT( IN ) :: jflg !Enable Flag
INTEGER, INTENT( IN ) :: iflg !Show Flag

INTEGER irv

IF( IsWindow(hwnd_pb) /= FALSE )THEN !Verify its a Window

  irv = ShowWindow( hwnd_pb,iflg )   !SHOW/HIDE Window
  irv = EnableWindow( hwnd_pb,jflg ) !ENABLE/DISABLE Window
  irv = UpdateWindow( hwnd_pb )      !Refresh Window

END IF

RETURN
END
!***********************************************************************
!                Write Progress Box
!***********************************************************************
SUBROUTINE post_write_progress_bar( m )

USE resource_fd
USE pcscipuf_fi
USE files_fi
USE SwinAPI

!     This routine updates the progress box display

IMPLICIT NONE

INTEGER, INTENT( IN ) :: m

!INTEGER i
INTEGER irv

CHARACTER(128) postit

!IF( m >= 0 )THEN
!  postit = ' '
!  IF( m > 0 )THEN
!    DO i = 1,m
!      postit(i:i) = CHAR(149)
!    END DO
!  END IF
!  CALL SetControlText( hwnd_pb,IDB_STATIC04,postit )
!END IF

IF( m >= 0 )THEN
  CALL set_progressbar(m)
END IF

!---- Refresh Display

irv = UpdateWindow( hwnd_pb )

RETURN
END
!***********************************************************************
!                Initialize Progress bar
!***********************************************************************
SUBROUTINE init_progressbar()

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

IMPLICIT NONE

INTEGER irv
INTEGER(POINTER_LEN) iparm

CALL make_param(iparm,0,64)

irv = SendDlgItemMessage(hwnd_pb, IDB_STATIC24, PBM_SETRANGE, 0, iparm);
irv = SendDlgItemMessage(hwnd_pb, IDB_STATIC24, PBM_SETSTEP,  1, 0);

RETURN
END
!***********************************************************************
!                Set Progressbar position
!***********************************************************************
SUBROUTINE set_progressbar(ib)

USE resource_fd
USE pcscipuf_fi
USE SwinAPI

INTEGER, INTENT(IN) :: ib

INTEGER irv
INTEGER(POINTER_LEN) iparm

iparm = ib

irv = SendDlgItemMessage(hwnd_pb, IDB_STATIC24, PBM_SETPOS, iparm, 0);

RETURN
END
