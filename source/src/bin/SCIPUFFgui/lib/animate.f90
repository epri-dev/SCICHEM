!***********************************************************************
!               AnimateIt
!***********************************************************************
SUBROUTINE AnimateIt( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE animate_fi
USE pltchoice_fi
USE SwinAPI

IMPLICIT NONE

! This routine creates animation files

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) ::  id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) ::  id_button !Button ID number
INTEGER,              INTENT( IN ) ::  id_level  !Dialog level (for data storage)

INTEGER irv,i,inc,iframe,ii,current
INTEGER(POINTER_LEN)Mybmp
INTEGER ilft,itop,iwid,ihgt,jlft,jtop,jwid,jhgt,iflg

CHARACTER(3)   ext
CHARACTER(64)  cmsg,cmsg2,cmsg3
CHARACTER(PATH_MAXLENGTH) filenam
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, AddExtension

TYPE( T_RECT ) Box
LOGICAL lok,animateOK,lexistT,CheckFile

LOGICAL, EXTERNAL :: hasError

!==== Initialize

lok      = .FALSE.
string1  = ' '
animateOK= .FALSE.

!==== Get Basename using Filename dialog

CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,string1 )

IF( lok )THEN
  CALL SplitName( string1,file_animate,path_animate )
  CALL RemoveExtension( file_animate )
ELSE
  GOTO 9000
END IF

!==== Set increment

IF( start_frame <= end_frame )THEN
  inc = 1
ELSE
  inc = -1
END IF

!==== Check for files

iframe  = 0
lexistT = .FALSE.

DO i = start_frame,end_frame,inc

  iframe = iframe + 1
  WRITE(ext,'(i3.3)') iframe

  string2 = AddExtension( file_animate,ext )
  CALL AddPath( string2,path_animate )

  filenam = AddNull( string2 )
  lexistT = lexistT .OR. CheckFile( filenam )

END DO

CALL InitError()

IF( lexistT )THEN
  AnimDel = .TRUE.
  CALL LetsDialog( iwnd_db,IDB_DELPRJ )
  AnimDel = .FALSE.
  IF( .NOT.lokbutton )GOTO 9000
END IF

!==== Prepare to loop

lanimate = .TRUE.

iframe = 0
Mybmp  = 0

IF( IsWindow(hwnd_tb) /= FALSE )THEN
  lok  = GetWindowRect( hwnd_tb,Box )
  jtop = Box%bottom - Box%top
ELSE
  jtop = 0
END IF
lok  = GetWindowRect( hwnd_db,Box )
jlft = Box%right - Box%left

irv  = GetWindowRect( hwnd_pw,Box ) !Get MAIN size
jhgt = Box%bottom-Box%top           !  Set Height to MAIN height
jwid = Box%right-Box%left           !  Set Width to MAIN width
iwid = MIN(MAX_SIZE,jwid)
ihgt = MIN(MAX_SIZE,jhgt)
itop = jtop + (jhgt-ihgt)/2
ilft = jlft + (jwid-iwid)/2

iflg = SWP_NOZORDER

irv = SetWindowPos( hwnd_pw,0,  &
                    ilft,itop,  &
                    iwid,ihgt,  &
                    iflg)

!==== Loop over frames

irv = SetCapture( hwnd_pw )  !  Capture mouse
irv = SetCursor( hcur_wait ) !  Set cursor to Wait

CALL create_progress( iwnd_db,3 )
IF( hasError() )GOTO 9999

cmsg  = 'Generating animation frames'
cmsg2 = ''C
cmsg3 = ''C
CALL post_write_progress( cmsg,cmsg2,cmsg3 )
IF( hasError() )GOTO 9999

CALL enable_halt( -2 )

animateOK = .TRUE.

DO i = start_frame,end_frame,inc

  IF( .NOT.lanimate )GOTO 9100

!==== Increment frame counter

  iframe = iframe + 1

!==== Select Time

  idbcmbo(2,id_level) = i
  CALL format_time( timePlot(i)%time%runTime,string1,0 )
  dbcmbo(2,id_level) = string1
  CALL SetListSelString( iwnd_db,TIME_COMBO,string1,irv ) !  Select this time in COMBO BOX
  cmsg3 = 'Time='//TRIM(string1)

!==== Prepare to draw - Set Read/Slice flags, time index

  lcheck(10,id_level) = .TRUE. !lread
  lcheck( 9,id_level) = .TRUE. !lslice

  CALL GetListSel( iwnd_db,TIME_COMBO,1,ii,irv )
  CALL GetListData (iwnd_db,TIME_COMBO,ii,current,irv )
  PlotDef(EDIT_LEVEL)%Field%TimeID = current

!==== Create file name

  WRITE(ext,'(I3.3)') iframe
  string1 = AddExtension( file_animate,ext )
  CALL ReportFileName( cmsg2,'File=',string1 )
  CALL AddPath( string1,path_animate )
  filenam = AddNull( string1 )

  CALL post_write_progress( cmsg,cmsg2,cmsg3 )
  IF( hasError() )GOTO 9999

!==== Draw plot

  CALL check_messages()
  IF( hasError() )GOTO 9999
  IF( .NOT.lanimate )GOTO 9998

  CALL CallButton(iwnd_db,IDB_BUTTON10,irv) !DRAW button

!==== Copy Window to Bitmap

  CALL check_messages()
  IF( hasError() )GOTO 9999
  IF( .NOT.lanimate )GOTO 9998

  CALL copy_bmp( hwnd_pw,Mybmp,0,0,1 ) !Copy Plot window to Bitmap
  IF( hasError() )GOTO 9999

!==== Save Bitmap to disk

  CALL check_messages()
  IF( hasError() )GOTO 9999
  IF( .NOT.lanimate )GOTO 9998

  CALL SaveAnimFile( filenam,Mybmp ) !SAVE Dialog Box
  IF( hasError() )GOTO 9999

END DO

!==== Clean up

9100 CONTINUE

lanimate = .FALSE.
CALL kill_progress()
irv = DeleteObject( Mybmp )
irv = SetWindowPos( hwnd_pw,0,  &
                    jlft,jtop,  &
                    jwid,jhgt,  &
                    iflg)

irv = SetCursor( hcur_arrow ) ! Set cursor to Arrow
irv = ReleaseCapture()        ! Release Mouse
IF( animateOK )THEN
  CALL PlayIt( iwnd_db,id_dialog,id_button,id_level )
ELSE
  CALL CallButton(iwnd_db,IDB_BUTTON10,irv) !DRAW button
END IF

9000 CONTINUE

RETURN

9999 CONTINUE

CALL ShowErrorMessage( hwnd_mw, ) !Post Error Messages

9998 CONTINUE

animateOK = .FALSE.
GOTO 9100

END
!***********************************************************************
!               PlayIt
!***********************************************************************
SUBROUTINE PlayIt( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE animate_fi
USE SwinAPI

IMPLICIT NONE

! This routine displays animation files

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) ::  id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) ::  id_button !Button ID number
INTEGER,              INTENT( IN ) ::  id_level  !Dialog level (for data storage)

INTEGER irv,iframe
INTEGER ilft,itop,iwid,ihgt,jlft,jtop,jwid,jhgt,iflg

TYPE( T_RECT ) Box
LOGICAL lok,CheckFile
CHARACTER(3)   ext
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, AddExtension

LOGICAL, EXTERNAL :: hasError

!==== Initialize

lanimate = .TRUE.
lok      = .FALSE.
string1  = ' '

IF( Play )THEN

!==== Get Basename

  CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,string1 )

  IF( lok )THEN
    CALL SplitName( string1,file_animate,path_animate )
    CALL RemoveExtension( file_animate )
  ELSE
    GOTO 9999
  END IF

END IF

!==== Prepare to loop

string2 = file_animate
CALL AddPath( string2,path_animate )
iframe = 1
WRITE(ext,'(I3.3)') iframe
string1 = AddExtension( string2,ext)
DO WHILE( CheckFile(string1) )
  iframe = iframe + 1
  WRITE(ext,'(I3.3)') iframe
  string1 = AddExtension( string2,ext)
END DO

CALL InitError()

iframe = iframe - 1
IF( iframe <= 0 )THEN
  CALL SetError( OP_ERROR, &
                 'Invalid animation bitmap sequence file name', &
                 'Sequence must begin with .001', &
                 ' ','PlayIt' )
  irv = EnableWindow( hwnd_pw,FALSE )
  CALL ShowErrorMessage( iwnd_db )
  irv = EnableWindow( hwnd_pw,FALSE )
  GOTO 9999
END IF

start_frame   = 1
current_frame = 0
end_frame     = iframe
lanim_cont    = .FALSE.
AnimBmap      = 0

!==== Determine file type

string1 = AddExtension( string2,'001')
string2 = AddNull(string1)

CALL get_animate_type( string2,bitmap_type )
IF( hasError() )THEN
  irv = EnableWindow( hwnd_pw,FALSE )
  CALL ShowErrorMessage( iwnd_db )
  irv = EnableWindow( hwnd_pw,FALSE )
  GOTO 9999
END IF

IF( IsWindow(hwnd_tb) /= FALSE )THEN
  lok  = GetWindowRect( hwnd_tb,Box )
  jtop = Box%bottom - Box%top
ELSE
  jtop = 0
END IF
lok  = GetWindowRect( hwnd_db,Box )
jlft = Box%right - Box%left

irv  = GetWindowRect( hwnd_pw,Box ) ! Get MAIN size
jhgt = Box%bottom-Box%top           !  Set Height to MAIN height
jwid = Box%right-Box%left           !  Set Width to MAIN width
iwid = MIN(MAX_SIZE,jwid)
ihgt = MIN(MAX_SIZE,jhgt)
itop = jtop + (jhgt-ihgt)/2
ilft = jlft + (jwid-iwid)/2

iflg = SWP_NOZORDER

irv = SetWindowPos( hwnd_pw,0,  &
                    ilft,itop,  &
                    iwid,ihgt,  &
                    iflg)

!==== Display first frame

lplotOK = .FALSE.
CALL MyUpdateWindow( hwnd_pw,.TRUE. )

!==== Start Play control dialog box

CALL LetsDialog( iwnd_db,IDB_PLAY )

!==== Clean up

irv = SetWindowPos( hwnd_pw,0,  &
                    jlft,jtop,  &
                    jwid,jhgt,  &
                    iflg)

9999 CONTINUE

lanimate = .FALSE.
lplotOK  = .FALSE.
CALL MyUpdateWindow( hwnd_pw,.TRUE. )

RETURN
END
!*******************************************************************************
!            Initialize Animate Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_animate( iwnd_db,id_level )

USE pcscipuf_fi

IMPLICIT NONE

! This routine initializes the Animate Dialog Box

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db  !Dialog handle
INTEGER, INTENT( IN )              ::  id_level !Data level

!==== Radio Buttons
!
nradio(1,id_level) = 3 !Create,Play,Delete

IF( AnimDel )THEN
  ichoice(1,id_level) = 3 !Delete
ELSE IF( Play )THEN
  ichoice(1,id_level) = 2 !Play
ELSE
  ichoice(1,id_level) = 1 !Create
END IF

CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

RETURN
END
!*******************************************************************************
!            Initialize Play Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_play( iwnd_db,id_level )

USE resource_fd
USE pcscipuf_fi
USE animate_fi

! This routine initializes the Play Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) ::  id_level !Data level

INTEGER irv,imin,imax,ipos
INTEGER(POINTER_LEN) iscroll

dbint(1,id_level) = MAX_PAUSE - INIT_PAUSE
iscroll = IDB_SCROLL1

CALL SetScrollControlRange( iwnd_db,iscroll,0,MAX_PAUSE,irv )
CALL SetScrollControlPos( iwnd_db,iscroll,dbint(1,id_level),irv )

CALL loadpal()
CALL setpal( .FALSE. )
CALL GetNCARPalette( AnimPal )
CALL CallButton( iwnd_db,IDB_BUTTON5,irv )

RETURN
END
!*******************************************************************************
!            save animate Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_animate( id_level )

USE pcscipuf_fi

IMPLICIT NONE

! This routine saves the animate Dialog Box Parameters

INTEGER, INTENT( IN ) ::  id_level !Data level

Play    = ichoice(1,id_level) == 2
AnimDel = ichoice(1,id_level) == 3

RETURN
END
!*******************************************************************************
!            save play Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_play()

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE animate_fi
USE SwinAPI
USE Pcx

IMPLICIT NONE

! This routine saves the Play Dialog Box Parameters

INTEGER irv
INTEGER(POINTER_LEN) jrv

IF( bitmap_type == BTYPE_BMP )THEN
  jrv = GlobalFree( AnimDIB )
END IF
AnimDIB = 0

irv = DeleteObject( AnimBmap )
AnimBmap = 0

RETURN
END
!***********************************************************************
!               PlayButton
!***********************************************************************
SUBROUTINE play_button( iwnd_db,id_button )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE animate_fi
USE winAPI_fd

IMPLICIT NONE

! This routine processes PUSHBUTTONs from Play Dialog Box

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) ::  id_button !Button ID number

!==== Select by Button number

SELECT CASE( id_button )
  CASE( 1 )                                         !step back
    inc_frame = -1
  CASE( 2 )                                         !play back
    inc_frame  = -1
    lanim_cont = .TRUE.
    CALL EnableControl( iwnd_db,IDOK,FALSE )        !Disable DONE
    CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE ) !Disable STEP
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE ) !Disable STEP
    CALL EnableControl( iwnd_db,IDB_SCROLL1,FALSE ) !Disable STEP
  CASE( 3 )                                         !pause
    lanim_cont = .FALSE.
    CALL EnableControl( iwnd_db,IDOK,TRUE )         !Enable DONE
    CALL EnableControl( iwnd_db,IDB_BUTTON1,TRUE )  !Enable STEP
    CALL EnableControl( iwnd_db,IDB_BUTTON5,TRUE )  !Enable STEP
    CALL EnableControl( iwnd_db,IDB_SCROLL1,TRUE )  !Disable STEP
  CASE( 4 )                                         !play forward
    inc_frame  = 1
    lanim_cont = .TRUE.
    CALL EnableControl( iwnd_db,IDOK,FALSE )        !Disable DONE
    CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE ) !Disable STEP
    CALL EnableControl( iwnd_db,IDB_BUTTON5,FALSE ) !Disable STEP
    CALL EnableControl( iwnd_db,IDB_SCROLL1,FALSE ) !Disable STEP
  CASE( 5 )                                         !step forward
    inc_frame = 1
  CASE DEFAULT
END SELECT

IF( id_button /= 3 )THEN
  PlayBack = .TRUE.
  CALL MyUpdateWindow( hwnd_pw,.FALSE. )
END IF

RETURN
END
!*******************************************************************************
!            Play Back Bitmaps
!*******************************************************************************
SUBROUTINE PlayItBack( hwnd )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE animate_fi
USE dib
USE SwinAPI
USE Pcx

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: hwnd !Window handle

INTEGER(POINTER_LEN) jwnd_db,jwnd_pause,ibtn,jrv
INTEGER jd_level,irv,i,iSleep
TYPE( T_MSG ) Message

CHARACTER(3)   ext
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, AddExtension

!==== Check Message Queue for Mouse clicks on control buttons

CALL FindHwndListId( IDB_PLAY,jwnd_db,jd_level ) !  Parent = PLAY

IF( IsWindow(jwnd_db) /= FALSE )THEN
  DO WHILE( PeekMessage(Message,jwnd_db,NULL,NULL,1) /= FALSE )
    IF( IsDialogMessage(jwnd_db,Message)==FALSE )THEN
      irv = TranslateMessage( Message )
      irv = DispatchMessage( Message )
    END IF
  END DO
  jwnd_pause = GetDlgItem( jwnd_db,IDB_BUTTON3 )
ELSE
  jwnd_pause = 0
END IF

!==== Clean up any existing memory handles

IF( bitmap_type == BTYPE_BMP )THEN
  jrv = GlobalFree( AnimDIB )
END IF
AnimDIB = 0

irv = DeleteObject( AnimBmap )
AnimBmap = 0

!==== Set the Frame counter

current_frame = current_frame + inc_frame
IF( current_frame <= 0 )current_frame = end_frame
IF( current_frame > end_frame )current_frame = start_frame

!==== Generate the file name

WRITE(ext,'(I3.3)') current_frame
string1 = AddExtension( file_animate,ext )
CALL AddPath( string1,path_animate )
string2 = AddNull(string1)

!==== Load the Bitmap from file

IF( bitmap_type == BTYPE_BMP )THEN
  AnimDIB  = OpenDIB( string2 )
  AnimBmap = BitmapFromDib( AnimDIB,AnimPal )
ELSE
  CALL load_pcx( hwnd,AnimBmap,string2 )
END IF

!==== Display the Bitmap

CALL RefreshIt( hwnd,AnimBmap )

!==== If in continuous play mode send message to display next frame

IF( lanim_cont )THEN
  IF( dbint(1,jd_level) < MAX_PAUSE )THEN
    iSleep = (MAX_PAUSE - dbint(1,jd_level))/LINE_PAUSE
    i = 0
    DO WHILE ( i < iSleep .AND. lanim_cont )
      CALL Sleep( LINE_PAUSE )
      i = i + 1
      CALL check_messages()
      IF( IsWindow(jwnd_pause) /= FALSE )THEN
        DO WHILE( PeekMessage(Message,jwnd_pause,NULL,NULL,1) /= FALSE)
          irv = TranslateMessage( Message )
          irv = DispatchMessage( Message )
        END DO
      END IF
    END DO
  END IF
END IF
IF( lanim_cont )THEN
  IF( inc_frame >= 0 )THEN
    ibtn = IDB_BUTTON4
  ELSE
    ibtn = IDB_BUTTON2
  END IF
  irv = PostMessage( jwnd_db,WM_COMMAND,ibtn,0 )
END IF

RETURN
END
!***********************************************************************
!               DeleteAnim
!***********************************************************************
SUBROUTINE DeleteAnim( iwnd_db,id_dialog,id_button,id_level )

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE animate_fi

! This routine deletes animation files

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) ::  iwnd_db   !Dialog Box handle
INTEGER,              INTENT( IN ) ::  id_dialog !Dialog ID number
INTEGER,              INTENT( IN ) ::  id_button !Button ID number
INTEGER,              INTENT( IN ) ::  id_level  !Dialog level (for data storage)

LOGICAL lok

!==== Initialize

lanimate = .TRUE.
lok      = .FALSE.
string1  = ' '

!==== Get Basename

CALL GetFile( id_dialog,id_button,id_level,lok,iwnd_db,string1 )

IF( lok )THEN
  CALL SplitName( string1,file_animate,path_animate )
  CALL RemoveExtension( file_animate )
ELSE
  GOTO 9999
END IF

!==== Start Play control dialog box

CALL LetsDialog( iwnd_db,IDB_DELPRJ )

9999 CONTINUE

lanimate = .FALSE.
AnimDel  = .FALSE.

RETURN
END
