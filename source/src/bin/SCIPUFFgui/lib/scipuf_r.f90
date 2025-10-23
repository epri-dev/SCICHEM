!*******************************************************************************
!                run_scipuff
!*******************************************************************************
SUBROUTINE run_scipuff( iwnd_db,StartFlag,filename )

USE resource_fd
USE tooluser_fd
USE mettype_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE errorParam_fd
USE script_fi
USE create_fi
USE GUItool_fi
USE pltchoice_fi
USE SwinAPI
USE GUIerror_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Handle
LOGICAL,              INTENT( IN ) :: StartFlag
CHARACTER(*),         INTENT( IN ) :: filename

INTEGER irv,n

LOGICAL lDone

TYPE( pendT ) run

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError
INTEGER, EXTERNAL :: sysDeleteFile, sysCheckFile

CHARACTER(128), EXTERNAL :: AddNull

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'RunScipuff'

!---- Disable Buttons

IF( StartFlag )THEN !New Projects
  Call EnableControl( hwnd_tb,IDB_PLOT  ,FALSE )
  Call EnableControl( hwnd_tb,IDB_OPNPRJ,FALSE )
  Call EnableControl( hwnd_tb,IDB_NEWPRJ,FALSE )
  Call EnableControl( hwnd_tb,IDB_SCIPUF,FALSE )
  Call EnableControl( hwnd_tb,IDB_VIEWPRJ,FALSE )
ELSE !Restarts
  CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE ) !  RUN
  CALL EnableControl( iwnd_db,ID_CANCEL  ,FALSE ) !  CANCEL
  CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE ) !  DONE
END IF

!---- Post Message - Restarts Only

IF( .NOT.StartFlag )THEN
  IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
    string1 = 'Running SCIPUFF'
    string2 = AddNull(TRIM(string1))
    CALL SetControlText( iwnd_db,IDB_STATIC06,string2 )
  END IF
END IF

!---- Start Progress Box

!---- Run SCIPUFF

IF( StartFlag )THEN

  IF( project(BASE_LEVEL)%Restart )THEN

    irv = SCIPRestartProject( ToolCallerID,createRst )

  ELSE

    IF( ALLOCATED(relMCList) )THEN
      irv = SCIPNewProjectMC( ToolCallerID,createNew, &
                              mtlList, &
                              relList, &
                              SIZE(relMCList), &
                              relMCList)
    ELSE
      irv = SCIPNewProject( ToolCallerID,createNew, &
                            mtlList, &
                            relList)
    END IF
  END IF
ELSE
  CALL GUI_SCIP_end( project(BASE_LEVEL),dlgTime(BASE_LEVEL),run )

  irv = SCIPRunProject( ToolCallerID,run )
END IF
IF( irv == SCIPfailure )THEN
  CALL GetToolError( eRoutine )
  nError = lastError()
  GOTO 9999
END IF

!---- Remove Progress Box

9999 CONTINUE

CALL DeallocateRelList()
CALL DeallocateMtlList()

!---- Check Exit Status/ Post Message if any /Report Status if Restart

lDone = .FALSE.
SELECT CASE( nError )
  CASE( NO_ERROR ) !SUCCESS
    string1 = 'SCIPUFF Completed Successfully'
    lDone   = .TRUE.
  CASE( S0_ERROR ) !HALTED
    string1 = 'User Halted SCIPUFF before completion'
    nError  = NO_ERROR
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
  CASE( AB_ERROR ) !ABORTED
    string1 = 'User Aborted SCIPUFF before completion'
    nError  = NO_ERROR
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
  CASE( WN_ERROR ) !WARNING
    string1 = 'SCIPUFF Stopped on Warning'
    IF( StartFlag )GOTO 2000
  CASE DEFAULT !ERROR
    string1 = 'SCIPUFF encountered a RunTime Error'
    CALL ShowErrorMessage( iwnd_db )
    IF( StartFlag )GOTO 2000
END SELECT

IF( sysCheckFile( file_clog ) == SCIPsuccess )THEN
  error%iParm = WN_ERROR
  error%jParm = 0
  error%routine = 'RunScipuff '
  error%aString = 'SCIPUFF reported cautions while running'
  error%bString = ' '
  error%cString = 'Do you want to view the caution messages?'
  CALL ShowWarningMessage( iwnd_db,.TRUE. )
  IF( error%iparm == NO_ERROR )THEN
    CALL ViewCautionMessages(iwnd_db)
  ELSE
    CALL InitError()
  END IF
END IF

!---- Post Results - RESTART Only

IF( .NOT.StartFlag )THEN
  IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
    string2 = AddNull( TRIM(string1) )
    CALL SetControlText( iwnd_db,IDB_STATIC06,string2 )
  END IF
  file_inp = TRIM(filename)
END IF

!---- Clear Error flags

CALL InitError()

!---- Open Project Files

project(BASE_LEVEL)%OK = .TRUE.
CALL OpenProject( iwnd_db )
IF( project(BASE_LEVEL)%OK )THEN !  SUCCESS
  string2 = TRIM(project(BASE_LEVEL)%ID%name)
  IF( project(BASE_LEVEL)%Edit )THEN
    string1 = '&Edit '//TRIM(string2) !    Build TOOLBAR Message
  ELSE
    string1 = 'Re&view '//TRIM(string2) !    Build TOOLBAR Message
  END IF
  CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,string1 ) !    Post TOOLBAR Message
ELSE !  FAILURE
  string1 = ' ' !    Build TOOLBAR Message
  CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,string1 ) !    Post TOOLBAR Message
END IF

!---- Post SCIPUFF Stats - RESTART Only

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  IF( .NOT.StartFlag )THEN
    CALL EnableControlL( iwnd_db,IDB_BUTTON1,project(BASE_LEVEL)%Run ) !Enable RUN  Button
    CALL EnableControl( iwnd_db,ID_CANCEL,FALSE )             !Enable DONE Button
    CALL EnableControl( iwnd_db,IDB_BUTTON2,TRUE )            !Enable DONE Button
    CALL ShowControl( iwnd_db,ID_CANCEL,SW_HIDE )             !Enable DONE Button
    CALL ShowControl( iwnd_db,IDB_BUTTON2,SW_SHOWNORMAL )     !Enable DONE Button
    IF( project(BASE_LEVEL)%Puff )THEN !Set Statistic Strings
      CALL c_format( timePuff(nTimePuff)%time%runTime,n,string2 )
      string1 = TRIM(timePuff(nTimePuff)%string)//' ( '//TRIM(string2)//'hrs )'
      WRITE(string2,*)timePuff(nTimePuff)%nItems !  Puffs
      string2 = ADJUSTL(TRIM(string2)) !
    ELSE !
      string1 = '????' !  Failure
      string2 = 'No PUFF Data' !  Failure
    END IF !
    CALL SetControlText( iwnd_db,IDB_STATIC32,string1 ) !
    CALL SetControlText( iwnd_db,IDB_STATIC33,string2 ) !

    IF( lDone )CALL CallButton( iwnd_db,IDB_BUTTON2,irv ) !Call RUN Button

!---- Call TOOLBAR RUN Button - NEW Only

  ELSE
    CALL PushButton( hwnd_tb,IDB_SCIPUF,IDB_EDTPRJ,irv )  !Call EDIT Button
  END IF
END IF

!---- Done

1000 RETURN

!---- NEW Project Error - Return to EDTPRJ

2000 CONTINUE

CALL InitError()
IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  CALL PushButton( hwnd_tb,IDB_EDTPRJ,IDB_EDTPRJ,irv ) !Call EDTPRJ Button
END IF

GOTO 1000
END
!===============================================================================
!===============================================================================
!===============================================================================
!===============================================================================
!===============================================================================
RECURSIVE INTEGER FUNCTION ToolCallBack( arg1,arg2,iParm )
!DEC$ ATTRIBUTES REFERENCE :: arg1, arg2

USE resource_fd
USE tooluser_fd
USE param_fd
USE errorParam_fd
USE pcscipuf_fi
USE mettype_fd
USE dialog_fi
USE mouse_fi
USE SwinAPI
USE pltchoice_fi
USE files_fi
USE PlotOpt_fd
USE MyEffect
USE MyClock
USE MySync
USE GUItool_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) arg1,arg2
INTEGER,DIMENSION(*) :: iParm

INTEGER(POINTER_LEN) jParm
INTEGER iMessage
INTEGER iCaller

TYPE( messageT ) message
TYPE( releaseT ) release

REAL, DIMENSION(2) :: rArray

LOGICAL default,label,lok

INTEGER iflag,controlID,i,enable,show,irv,id_dialog,ios,kParm
INTEGER(POINTER_LEN) hwnd
CHARACTER(128) string(3)

TYPE( T_POINT) Pt
INTEGER ilft,iwid,ihgt,isiz

INTEGER, EXTERNAL :: VALUEOF
LOGICAL, EXTERNAL :: hasError

CHARACTER(128), EXTERNAL :: StripNull

ToolCallBack = SCIPfailure

CALL check_messages()

CALL VALUE_REFERENCE( arg1,iCaller )
CALL VALUE_REFERENCE( arg2,iMessage )

IF( iMessage == HM_MESSAGE )THEN
  jParm = ADDRESSOF( iParm )
  CALL ADDRESS_MESSAGE( jParm,message )
  string1 = StripNull( message%aString )
  string2 = StripNull( message%bString )
  string3 = StripNull( message%cString )
ELSE IF( iMessage == HM_BUTTONTAG )THEN
  jParm = ADDRESSOF(iParm)
  CALL ADDRESS_MESSAGE(jParm,message)
  string(1) = StripNull( message%aString )
  string(2) = StripNull( message%bString )
  string(3) = StripNull( message%cString )
  iflag     = message%iParm
  label     = message%jParm /= FALSE
ELSE IF( iMessage > HM_MESSAGE )THEN
  jParm = ADDRESSOF(iParm)
  CALL ADDRESS_MESSAGE( jParm,message )
  CALL SetErrorT( message )
  default  = message%jParm /= FALSE
ELSE IF( iMessage == HM_RELEASE )THEN
  jParm = ADDRESSOF( iParm )
  CALL ADDRESS_RELEASE( jParm,release )
ELSE IF( iMessage == HM_SYNC )THEN
  jParm = ADDRESSOF(iParm)
  CALL ADDRESS_REALARRAY(jParm,rArray,1)
ELSE
  CALL VALUE_REFERENCE( iParm(1),kParm )
END IF

ToolCallBack = SCIPsuccess

SELECT CASE( iMessage )
  CASE( HM_CHECK )

  CASE( HM_SETCLOCK )
    IF( Ncur > 0 )THEN
      Icur = 0
      ClockOn = .TRUE.
    END IF

  CASE( HM_STEPCLOCK )
    IF( Ncur > 0 .AND. ClockOn )THEN
      IF( Icur == 0 )CALL ShowControl( hwnd_pb,IDB_STATIC51,SW_SHOWNORMAL )
      Icur = Icur + 1
      IF( Icur > Ncur )Icur = 1
      CALL SetControlIcon( hwnd_pb,IDB_STATIC51,Hcur(Icur) )
    END IF

  CASE( HM_STOPCLOCK )
    IF( Ncur > 0 .AND. ClockOn )THEN
      IF( Icur /= 0 )CALL ShowControl( hwnd_pb,IDB_STATIC51,SW_HIDE )
      ClockOn = .FALSE.
      Icur = 0
    END IF

  CASE( HM_SETWAIT )
    irv = SetCursor( hcur_wait ) !Set Cursor
    irv = SetCapture( hwnd_pb )  !Capture mouse

  CASE( HM_RELEASEWAIT )
    irv = SetCursor( hcur_arrow ) !Set Arrow
    irv = ReleaseCapture()        !Release Mouse

  CASE( HM_INFO )
    IF( IsWindow(hwnd_pb) /= FALSE )THEN
      hwnd = hwnd_pb
    ELSE IF( IsWindow(hwnd_db) /= FALSE )THEN
      hwnd = hwnd_db
    ELSE
      hwnd = hwnd_mw
    END IF
    CALL ShowInfoMessage( hwnd )

  CASE( HM_ERROR )
    IF( IsWindow(hwnd_pb) /= FALSE )THEN
      hwnd = hwnd_pb
    ELSE IF( IsWindow(hwnd_db) /= FALSE )THEN
      hwnd = hwnd_db
    ELSE
      hwnd = hwnd_mw
    END IF
    CALL ShowErrorMessage( hwnd )

  CASE( HM_REPLY )
    IF( IsWindow(hwnd_pb) /= FALSE )THEN
      hwnd = hwnd_pb
    ELSE IF( IsWindow(hwnd_db) /= FALSE )THEN
      hwnd = hwnd_db
    ELSE
      hwnd = hwnd_mw
    END IF
    CALL ShowWarningMessage( hwnd,default )
    IF( hasError() )THEN
      ToolCallBack = SCIPnegative
    ELSE
      ToolCallBack = SCIPaffirmative
    END IF
    CALL InitError()

  CASE( HM_CAUTION )

  CASE( HM_PROGRESSMSG )
    CALL post_write_progress( string1,string2,string3 )

  CASE( HM_PROGRESSBAR )
    CALL post_write_progress_bar( kParm )

  CASE( HM_BUTTONTAG )
    IF( label )THEN
      controlID = STATIC_BASE + 10
    ELSE
      controlID = BUTTON_BASE
    END IF
    DO i = 1,3
      IF( BTEST(iflag,i-1) )THEN
        CALL SetControlText( hwnd_pb,controlID+i,string(i) )
      END IF
    END DO

  CASE( HM_BUTTONSTATE )
    DO i = 1,3
      IF( BTEST(kParm,i-1) )THEN
        show = SW_SHOWNORMAL
      ELSE
        show = SW_HIDE
      END IF
      IF( BTEST(kParm,i+2) )THEN
        enable = TRUE
      ELSE
        enable = FALSE
      END IF
      CALL ShowControl  ( hwnd_pb,STATIC_BASE+i+10,show )
      CALL ShowControl  ( hwnd_pb,BUTTON_BASE+i,   show )
      CALL EnableControl( hwnd_pb,BUTTON_BASE+i,   enable )
    END DO

  CASE( HM_START )
    nWpn   = 0
    lastID = 0
    CALL create_progress( hwnd_mw,kParm )
    IF( kParm == -1 )THEN
      IF( rundef(BASE_LEVEL)%on )THEN
        SyncMode = .TRUE.
        iSync = -1
        nSync = rundef(BASE_LEVEL)%update
        pbFlag = kParm
        IF( IsWindow(hwnd_db) /= FALSE )THEN
          lok   = GetWindowRect( hwnd_db,dbBox )
          iflag = IOR(SWP_NOZORDER,SWP_NOSIZE)
          Pt%x = 3
          Pt%y = dbBox%top
          irv = SetWindowPos( hwnd_db,  &
                              0,  &
                              Pt%x,Pt%y,  &
                              0,0,  &
                              iflag) !  Change flag
        END IF
        IF( IsWindow(hwnd_pb) /= FALSE )THEN
          iflag = IOR(SWP_NOZORDER,SWP_NOSIZE)
          lok  = GetWindowRect( hwnd_pb,pbBox )
          IF( IsWindow(hwnd_db) /= FALSE )THEN
            ilft = 3 + pbBox%left - dbBox%left
          ELSE
            ilft = 3
          END IF
          Pt%x = ilft
          Pt%y = pbBox%top
          irv = SetWindowPos( hwnd_pb,  &
                              0,  &
                              Pt%x,Pt%y,  &
                              0,0,  &
                              iflag ) !  Change flag
        END IF
        IF( IsWindow(hwnd_pw) /= FALSE )THEN
          irv = EnableWindow( hwnd_pw,TRUE )
          irv = ShowWindow( hwnd_pw,SW_SHOW )
          lok = GetWindowRect( hwnd_pw,pwBox )
          lok = GetWindowRect( hwnd_mw,mwBox )
          iflag = SWP_NOZORDER
          IF( IsWindow(hwnd_db) /= FALSE )THEN
            ilft = 3 + dbBox%right - dbBox%left + 1
          ELSE IF( IsWindow(hwnd_pb) /= FALSE )THEN
            ilft = 3 + pbBox%right - pbBox%left + 1
          ELSE
            ilft = 3
          END IF
          lok  = GetWindowRect( hwnd_db,dbBox )
          Pt%x = ilft
          Pt%y = dbBox%top
          lok = ScreenToClient( hwnd_mw,Pt )
          iwid = mwBox%right-5-ilft
          ihgt = mwBox%bottom - dbBox%top
          isiz = MIN(iwid,ihgt)
          irv  = SetWindowPos( hwnd_pw,  &
                               0,  &
                               Pt%x,Pt%y,  &
                               isiz,isiz,  &
                               iflag) !  Change flag
        END IF
      END IF
    END IF

  CASE( HM_STOP )
    IF( SyncMode )THEN
      IF( IsWindow(hwnd_pw) /= FALSE )THEN
        iflag = SWP_NOZORDER
        iwid = pwBox%right-pwBox%left
        ihgt = pwBox%bottom-pwBox%top
        Pt%x = pwBox%left
        Pt%y = pwBox%top
        lok = ScreenToClient( hwnd_mw,Pt )
        irv = SetWindowPos( hwnd_pw,  &
                            0,  &
                            Pt%x,Pt%y,  &
                            iwid,ihgt,  &
                            iflag ) !  Change flag
        irv = ShowWindow( hwnd_pw,SW_HIDE ) !      Hide PLOT Window
        irv = EnableWindow( hwnd_pw,FALSE ) !      Disable PLOT window
      END IF
      IF( IsWindow(hwnd_db) /= FALSE )THEN
        CALL FindHwndList( hwnd_db,id_dialog,irv )
        CALL position_dialog( hwnd_db,id_dialog,1 ) !  Position Dialog Box
      END IF
      IF( IsWindow(hwnd_pb) /= FALSE )THEN
        CALL position_progress( hwnd_pb,pbFlag ) !  Position Box
      END IF
      SyncMode = .FALSE.
    END IF
    CALL kill_progress()

  CASE( HM_SYNC )
    IF( SyncMode )THEN
      IF( iSync == -1 )THEN
        CALL init_plot_choice( file_inp,SyncMode )
        IF( LEN_TRIM(rundef(BASE_LEVEL)%optionsFile) > 0 )THEN
          string1 = TRIM(rundef(BASE_LEVEL)%optionsFile)
          CALL read_plotopt( 0,INTERACTIVE_OPTION,string1 )
        END IF
        CALL ResetPlotField( BASE_LEVEL,' ' )
        CALL DeallocatePlotTime( -1 )
        nTimePlot = 1
        ALLOCATE( timePlot(nTimePlot),STAT=ios )
        timePlot(nTimePlot)%time = dlgTime(BASE_LEVEL)%time%start%time
      END IF
      iSync = iSync + 1
      IF( iSync >= nSync )THEN
        iSync = 0
        SyncTime = rArray(1)
        IF( IsWindow(hwnd_pw) /= FALSE )THEN
          lplotOK  = .TRUE.
          lpaintit = .TRUE. !Plot
          PlotDef(BASE_LEVEL)%Created        = .FALSE.
          PlotDef(BASE_LEVEL)%Field%timeID   = nTimePlot
          PlotDef(BASE_LEVEL)%Field%userTime = SyncTime/3600.
          CALL enable_plot( .TRUE. )
          CALL MyUpdateWindow( hwnd_pw,.TRUE. )
          CALL check_plot( lprintOK )
        END IF
      END IF
    END IF

  CASE( HM_RELEASE )
    ToolCallBack = SCIPnull
    release%tRel = 1.0

  CASE DEFAULT

END SELECT

9999 CONTINUE

CALL check_messages()

RETURN
END

!===============================================================================
!===============================================================================
!===============================================================================
!===============================================================================
!===============================================================================
RECURSIVE INTEGER FUNCTION SCIP32ToolCallBack( iMessage,iParm )

USE resource_fd
USE tooluser_fd
USE param_fd
USE pcscipuf_fi
USE mettype_fd
USE dialog_fi
USE mouse_fi
USE guiAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) iMessage
INTEGER, DIMENSION(*) :: iParm

INTEGER(POINTER_LEN) callerID

callerID = 0

SCIP32ToolCallBack = ToolCallBack( callerID,iMessage,iParm )

RETURN
END
