!***********************************************************************
!               InitError
!***********************************************************************
SUBROUTINE InitError()

USE GUIerror_fi
USE errorParam_fd

IMPLICIT NONE

error%iParm = NO_ERROR
error%jParm = 0
error%routine = ' '
error%aString = 'No error message'
error%bString = ' '
error%cString = ' '

RETURN

END SUBROUTINE InitError
!***********************************************************************
!               hasError
!***********************************************************************
LOGICAL FUNCTION hasError() RESULT( answer )

USE GUIerror_fi
USE errorParam_fd

IMPLICIT NONE

answer = error%iParm /= NO_ERROR

RETURN

END FUNCTION hasError
!***********************************************************************
!               hasWarning
!***********************************************************************
LOGICAL FUNCTION hasWarning() RESULT( answer )

USE GUIerror_fi
USE errorParam_fd

IMPLICIT NONE

answer = error%iParm == WN_ERROR

RETURN

END FUNCTION hasWarning
!***********************************************************************
!               lastError
!***********************************************************************
INTEGER FUNCTION lastError() RESULT( answer )

USE GUIerror_fi

IMPLICIT NONE

answer = error%iParm

RETURN

END FUNCTION lastError
!***********************************************************************
!               SetError
!***********************************************************************
SUBROUTINE SetError( nError,eMessage,eInform,eAction,eRoutine )

USE GUIerror_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: nError
CHARACTER(*), INTENT( IN ) :: eMessage
CHARACTER(*), INTENT( IN ) :: eInform
CHARACTER(*), INTENT( IN ) :: eAction
CHARACTER(*), INTENT( IN ) :: eRoutine

error%iParm = nError
error%jParm = 0
error%routine = TRIM(eRoutine)
error%aString = TRIM(eMessage)
error%bString = TRIM(eInform)
error%cString = TRIM(eAction)

RETURN

END SUBROUTINE SetError
!***********************************************************************
!               SetErrorT
!***********************************************************************
SUBROUTINE SetErrorT( message )

USE GUIerror_fi

IMPLICIT NONE

TYPE( messageT ), INTENT( IN ) :: message

CHARACTER(128), EXTERNAL :: StripNull

error = message

error%routine = StripNull( error%routine )
error%aString = StripNull( error%aString )
error%bString = StripNull( error%bString )
error%cString = StripNull( error%cString )

RETURN

END SUBROUTINE SetErrorT
!***********************************************************************
!               AddErrorAction
!***********************************************************************
SUBROUTINE AddErrorAction( eAction )

USE GUIerror_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: eAction

error%cString = TRIM(eAction)

RETURN

END SUBROUTINE AddErrorAction
!***********************************************************************
!               AddErrorInform
!***********************************************************************
SUBROUTINE AddErrorInform( eInform )

USE GUIerror_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: eInform

error%bString = TRIM(eInform)

RETURN

END SUBROUTINE AddErrorInform
!***********************************************************************
!               AddErrorRoutine
!***********************************************************************
SUBROUTINE AddErrorRoutine( eRoutine )

USE GUIerror_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: eRoutine

error%routine = TRIM(eRoutine)

RETURN

END SUBROUTINE AddErrorRoutine
!***********************************************************************
!               GetToolError
!***********************************************************************
SUBROUTINE GetToolError( title )

USE SCIPtool
USE GUIerror_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: title

INTEGER irv

irv = SCIPGetLastError(error)

IF( LEN_TRIM(title) > 0 )error%routine = TRIM(title)//':'//TRIM(error%routine)

RETURN
END

!***********************************************************************
!               ShowErrorMessage
!***********************************************************************
SUBROUTINE ShowErrorMessage( jwnd )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd

INTEGER(POINTER_LEN) iwnd
INTEGER              irv
CHARACTER(512)       message,eString

!---- SCRIPT mode

IF( .NOT.(BTEST(pcscipuf_mode,IPMB_INTERACTIVE)) )THEN
  error%cString = 'Exit current script file'
  CALL WriteMessage( 'Error' )
  CALL InitError()

!---- INTERACTIVE mode

ELSE

!---- Special Error Message with Static boxes

  IF( error%iParm == DM_ERROR )THEN
    CALL LetsDialog( jwnd,IDB_ERRORBX )

!---- Normal Error Message

  ELSE

    IF( jwnd > 0 )THEN !Parent specified
      iwnd = jwnd      !
    ELSE               !No Parent specified
      iwnd = 0         !
    END IF

!---- Build Message from error separated by carriage returns
!---- Initialize Message string

    message(1:) = ' '

!---- Add aString if not empty

    IF( LEN_TRIM(error%aString) > 0 )THEN
      message = TRIM(message)//TRIM(error%aString)//CHAR(13)
    END IF

!---- Add bString if not empty

    IF( LEN_TRIM(error%bString) > 0 )THEN
      message = TRIM(message)//TRIM(error%bString)//CHAR(13)
    END IF

!---- Add cString if not empty

    IF( LEN_TRIM(error%cString) > 0 )THEN
      message = TRIM(message)//TRIM(error%cString)//CHAR(13)
    END IF

!---- Remove last character if it is a carriage return

    irv = LEN_TRIM(message)
    IF( irv == 0 )THEN
      message = 'Empty error message'
    ELSE
      IF( message(irv:irv) /= CHAR(13) )THEN
        irv = irv + 1
      END IF
      message(irv:) = ' '
    END IF

!---- Add NULL character to end of string

    irv = MIN(LEN(TRIM(message)),LEN(message)-1)
    message(irv+1:irv+1) = CHAR(0)

!---- Set Message Box Title String

    eString = TRIM(error%routine)//CHAR(0)

!---- Put up Message Box

    irv = MessageBox( iwnd,message,eString,IOR(MB_OK,MB_ICONHAND) )

!---- Clear the error

    CALL InitError()

  END IF

END IF

RETURN
END
!***********************************************************************
!               ShowWarningMessage
!***********************************************************************
SUBROUTINE ShowWarningMessage( jwnd,defaultYes )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd
LOGICAL,              INTENT( IN ) :: defaultYes

CHARACTER(512) message,eString

INTEGER irv,iflg
INTEGER(POINTER_LEN)iwnd

!---- SCRIPT mode

IF( BTEST(pcscipuf_mode,IPMB_SCRIPT) .OR. pcscipuf_mode == IPM_EXIT )THEN
  IF( defaultYes )THEN
    error%cString = 'Do you want to continue? (YES)'
  ELSE
    error%cString = 'Do you want to continue? (NO)'
  END IF
  CALL WriteMessage( 'Warning' )
  IF( defaultYes )THEN
    CALL InitError()
  ELSE
    IF( .NOT.BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
      pcscipuf_mode = IBSET(pcscipuf_mode,IPMB_ERROR)
    END IF
  END IF

!---- INTERACTIVE mode

ELSE

!---- Hide Progress Box if there is one

  CALL show_progress( FALSE,SW_HIDE )

!---- Special Error Message with Static boxes

  IF( error%iParm == DM_ERROR )THEN
    error%iParm = WN_ERROR
    CALL LetsDialog( jwnd,IDB_ERRORBX )
    IF( ErrorBoxResult )CALL InitError()

  ELSE

!---- Normal Error Message

    IF( jwnd > 0 )THEN !Parent specified
      iwnd = jwnd      !
    ELSE               !No Parent specified
      iwnd = 0         !
    END IF

    iflg = IOR(MB_TASKMODAL,MB_SETFOREGROUND)

!---- Build Message from error separated by carriage returns
!---- Initialize Message string

    message(1:) = ' '

!---- Add aString if not empty

    IF( LEN_TRIM(error%aString) > 0 )THEN
      message = TRIM(message)//TRIM(error%aString)//CHAR(13)
    END IF

!---- Add bString if not empty

    IF( LEN_TRIM(error%bString) > 0 )THEN
      message = TRIM(message)//TRIM(error%bString)//CHAR(13)
    END IF

!---- Add cString if not empty

    IF( LEN_TRIM(error%cString) > 0 )THEN
      message = TRIM(message)//TRIM(error%cString)//CHAR(13)
    ELSE
      message = TRIM(message)//'Do you want to CONTINUE?'//CHAR(13)
    END IF

!---- Remove last character if it is a carriage return

    irv = LEN_TRIM(message)
    IF( irv == 0 )THEN
      message = 'Empty error message'
    ELSE
      IF( message(irv:irv) /= CHAR(13) )irv = irv + 1
      message(irv:) = ' '
    END IF

!---- Add NULL character to end of string

    irv = MIN(LEN(TRIM(message)),LEN(message)-1)
    message(irv+1:irv+1) = CHAR(0)

!---- Set Message Box Title String

    eString = TRIM(error%routine)//CHAR(0)

!------ Set Message Box type to add Yes/No buttons and Question mark

    iflg = IOR(iflg,IOR(MB_YESNO,MB_ICONQUESTION))

!------ Display Message Box

    irv = MessageBox( iwnd,message,eString,iflg )

!------ If YES button clicked clear eror and continue

    IF( irv == IDYES )CALL InitError()

  END IF

  CALL show_progress( TRUE,SW_SHOW )

END IF

RETURN
END
!***********************************************************************
!               ShowInfoMessage
!***********************************************************************
SUBROUTINE ShowInfoMessage( jwnd )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd

CHARACTER(512) message,eString

INTEGER irv,iflg
INTEGER(POINTER_LEN)iwnd

!---- SCRIPT mode

IF( BTEST(pcscipuf_mode,IPMB_SCRIPT) )THEN
  CALL WriteMessage( 'Info' )
  CALL InitError()
  pcscipuf_mode = IBCLR(pcscipuf_mode,IPMB_ERROR)

!---- INTERACTIVE mode

ELSE

!---- Hide Progress Box if there is one

  CALL show_progress( FALSE,SW_HIDE )

!---- Set MessageBox Type and Parent Window handle

  IF( jwnd > 0 )THEN !Parent specified
    iwnd = jwnd      !
  ELSE               !No Parent specified
    iwnd = 0         !
  END IF

  iflg = IOR(MB_TASKMODAL,MB_SETFOREGROUND) !

!---- Build Message from error separated by carriage returns
!---- Initialize Message string

  message(1:) = ' '

!---- Add aString if not empty

  IF( LEN_TRIM(error%aString) > 0 )THEN
    message = TRIM(message)//TRIM(error%aString)//CHAR(13)
  END IF

!---- Add bString if not empty

  IF( LEN_TRIM(error%bString) > 0 )THEN
    message = TRIM(message)//TRIM(error%bString)//CHAR(13)
  END IF

!---- Add cString if not empty

  IF( LEN_TRIM(error%cString) > 0 )THEN
    message = TRIM(message)//TRIM(error%cString)//CHAR(13)
  END IF

!---- Remove last character if it is a carriage return

  irv = LEN_TRIM(message)
  IF( irv == 0 )THEN
    message = 'Empty error message'
  ELSE
    IF( message(irv:irv) /= CHAR(13) )THEN
      irv = irv + 1
    END IF
    message(irv:) = ' '
  END IF

!---- Add NULL character to end of string

  irv = MIN(LEN(TRIM(message)),LEN(message)-1)
  message(irv+1:irv+1) = CHAR(0)

!---- Set Message Box Title String

  eString = TRIM(error%routine)//CHAR(0)

!---- Set Message Box type to add OK button

  iflg = IOR(iflg,MB_OK)

!---- Display Message Box

  irv = MessageBox( iwnd,message,eString,iflg )

!---- Clear Error

  CALL InitError()

!---- Show Progress Box if necessary

  CALL show_progress( TRUE,SW_SHOW )

END IF

RETURN
END
!***********************************************************************
!               ViewCautionMessages
!***********************************************************************
SUBROUTINE ViewCautionMessages( jwnd )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI
USE files_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd

LOGICAL lExist, next
INTEGER ios
CHARACTER(196)line
LOGICAL, EXTERNAL :: ShowCaution

INQUIRE( FILE=file_clog,EXIST=lExist )
IF( lExist )THEN
  OPEN(FILE=file_clog,UNIT=lun_clog,STATUS='OLD',IOSTAT=ios)
  IF( ios == 0 )THEN
    READ(lun_clog,'(A)',IOSTAT=ios)line
    next = INDEX(line,'***** CAUTION *****') == 1
    CALL InitError()
    DO WHILE( next )
      READ(lun_clog,'(A)',IOSTAT=ios)line
      IF( ios == 0 )THEN
        ios = INDEX(line,'***** CAUTION *****')
        IF( ios == 0 )THEN
          error%aString = TRIM(line)
          READ(lun_clog,'(A)',IOSTAT=ios)line
          IF( ios == 0 )THEN
            ios = INDEX(line,'***** CAUTION *****')
            IF( ios == 0 )THEN
              error%bString = TRIM(line)
              READ(lun_clog,'(A)',IOSTAT=ios)line
              IF( ios == 0 )THEN
                ios = INDEX(line,'***** CAUTION *****')
                IF( ios == 0 )THEN
                  error%cString = TRIM(line)
                  READ(lun_clog,'(A)',IOSTAT=ios)line
                  IF( ios == 0 )THEN
                    ios = INDEX(line,'***** CAUTION *****')
                    IF( ios == 0 )THEN
                      next = ShowCaution(jwnd,.FALSE.)
                    ELSE
                      next = ShowCaution(jwnd,.TRUE.)
                    END IF
                  ELSE
                    GOTO 9999
                  END IF
                ELSE
                  next = ShowCaution(jwnd,.TRUE.)
                END IF
              ELSE
                GOTO 9999
              END IF
            ELSE
              next = ShowCaution(jwnd,.TRUE.)
            END IF
          ELSE
            GOTO 9999
          END IF
        ELSE
          next = ShowCaution(jwnd,.TRUE.)
        END IF
      ELSE
        GOTO 9999
      END IF
    END DO
  ELSE
    error%iParm = OP_ERROR
    error%jParm = 0
    error%routine = 'ViewCautionMessages '
    error%aString = 'Unable to open the CAUTION file'
    CALL ReportFileName( error%bString,'File=',file_clog )
    WRITE(error%cString,*)'IOS=',ios
    CALL ShowErrorMessage( jwnd )
    CALL InitError()
  END IF
ELSE
  error%iParm = OP_ERROR
  error%jParm = 0
  error%routine = 'ViewCautionMessages '
  error%aString = 'Unable to file the CAUTION file'
  CALL ReportFileName( error%bString,'File=',file_clog )
  CALL ShowErrorMessage( jwnd )
  CALL InitError()
END IF

1000 CONTINUE

CLOSE(UNIT=lun_clog,IOSTAT=ios)

RETURN

9999 CONTINUE
IF( ios <  0 )THEN !EOF
  next = ShowCaution(jwnd,.FALSE.)
ELSE
  error%iParm = RD_ERROR
  error%jParm = 0
  error%routine = 'ViewCautionMessages '
  error%aString = 'Error reading the CAUTION file'
  CALL ReportFileName( error%bString,'File=',file_clog )
  WRITE(error%cString,*)'IOS=',ios
  CALL ShowErrorMessage( jwnd )
  CALL InitError()
END IF
GOTO 1000

END
!***********************************************************************
!               ShowCaution
!***********************************************************************
LOGICAL FUNCTION ShowCaution( jwnd,next )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE script_fi
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: jwnd
LOGICAL,              INTENT( IN ) :: next

CHARACTER(512) message,eString

INTEGER irv,iflg
INTEGER(POINTER_LEN)iwnd

!---- SCRIPT mode

IF( BTEST(pcscipuf_mode,IPMB_SCRIPT) )THEN
  CALL WriteMessage( 'Caution' )
  CALL InitError()
  pcscipuf_mode = IBCLR(pcscipuf_mode,IPMB_ERROR)
  ShowCaution = next

!---- INTERACTIVE mode

ELSE

!---- Hide Progress Box if there is one

  CALL show_progress( FALSE,SW_HIDE )

!---- Set MessageBox Type and Parent Window handle

  IF( jwnd > 0 )THEN !Parent specified
    iwnd = jwnd      !
  ELSE               !No Parent specified
    iwnd = 0         !
  END IF

  iflg = IOR(MB_TASKMODAL,MB_SETFOREGROUND) !

!---- Build Message from error separated by carriage returns
!---- Initialize Message string

  message(1:) = ' '

!---- Add aString if not empty

  IF( LEN_TRIM(error%aString) > 0 )THEN
    message = TRIM(message)//TRIM(error%aString)//CHAR(13)
  END IF

!---- Add bString if not empty

  IF( LEN_TRIM(error%bString) > 0 )THEN
    message = TRIM(message)//TRIM(error%bString)//CHAR(13)
  END IF

!---- Add cString if not empty

  IF( LEN_TRIM(error%cString) > 0 )THEN
    message = TRIM(message)//TRIM(error%cString)//CHAR(13)
  END IF

!---- Add continuation message if appropriate

  IF( next )THEN
    message = TRIM(message)//CHAR(13)//'Show next Caution Message?'//CHAR(13)
  END IF

!---- Remove last character if it is a carriage return

  irv = LEN_TRIM(message)
  IF( irv == 0 )THEN
    message = 'Empty caution message'
  ELSE
    IF( message(irv:irv) /= CHAR(13) )THEN
      irv = irv + 1
    END IF
    message(irv:) = ' '
  END IF

!---- Add NULL character to end of string

  irv = MIN(LEN(TRIM(message)),LEN(message)-1)
  message(irv+1:irv+1) = CHAR(0)

!---- Set Message Box Title String

  eString = 'ViewCaution'//CHAR(0)

!---- Set Message Box type to add OK button

  IF( next )THEN
    iflg = IOR(iflg,IOR(MB_YESNO,MB_ICONQUESTION))
  ELSE
    iflg = IOR(iflg,MB_OK)
  END IF

!---- Display Message Box

  irv = MessageBox( iwnd,message,eString,iflg )

  ShowCaution = next .AND. (irv == IDYES)

!---- Clear Error

  CALL InitError()

!---- Show Progress Box if necessary

  CALL show_progress( TRUE,SW_SHOW )

END IF

RETURN
END
!*******************************************************************************
!            Initialize Special Error Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_errorbox( iwnd_db )

USE resource_fd
USE errorParam_fd
USE GUIerror_fi
USE pcscipuf_fi
USE SwinAPI
!
!     This routine initializes the Special error Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

LOGICAL lok

INTEGER irv,ilft,itop,iwid,ihgt,scalex,offx,scaley,offy
INTEGER(POINTER_LEN)ictrl,ipc,icon

REAL    xmina,xmaxa,ymina,ymaxa
REAL    xminb,xmaxb,yminb,ymaxb
REAL    xmin ,xmax ,ymin ,ymax
REAL    dx,areaa,areab

TYPE( T_RECT  ) Box
TYPE( T_POINT ) Pt

CHARACTER(128), EXTERNAL :: AddNull

ErrorBoxResult = .FALSE.

string2 = 'Error Report : '//TRIM(error%routine)
string1 = AddNull( TRIM(string2) )
lok = SetWindowText( iwnd_db,string1 )

CALL SetControlText( iwnd_db,IDB_STATIC01,error%aString )

string2 = TRIM(error%cString)
string3 = TRIM(error%bString)

!---- Read Domains from Error strings

100 CONTINUE

irv = INDEX(string2,':')
IF( irv > 0 )THEN
  string1 = string2(irv+1:)
  READ(string1,*,ERR=1000,END=1000)xmina,xmaxa,ymina,ymaxa
ELSE
  GOTO 1000
END IF

irv = INDEX(string3,':')
IF( irv > 0 )THEN
  string1 = string3(irv+1:)
  READ(string1,*,ERR=1000,END=1000)xminb,xmaxb,yminb,ymaxb
ELSE
  GOTO 1000
END IF

!---- Set Max size

xmin = MIN(xmina,xminb)
xmax = MAX(xmaxa,xmaxb)
ymin = MIN(ymina,yminb)
ymax = MAX(ymaxa,ymaxb)

!---- Check size and switch so string2 represents larger area

areaa = (xmaxa-xmina)*(ymaxa-ymina)
areab = (xmaxb-xminb)*(ymaxb-yminb)
IF( areab > areaa )THEN
  string3 = TRIM(error%cString)
  string2 = TRIM(error%bString)
  GOTO 100
END IF

!---- Get Drawing area

ictrl = GetDlgItem( iwnd_db,IDB_STATIC06 )
irv   = GetWindowRect( ictrl,Box )

iwid = Box%right - Box%left
ihgt = Box%bottom - Box%top

scalex = (iwid*90)/100

scaley = (ihgt*90)/100

dx = MAX(xmax-xmin,ymax-ymin)

offx = (iwid - NINT(FLOAT(scalex)*(xmax-xmin)/dx))/2
offy = (ihgt - NINT(FLOAT(scaley)*(ymax-ymin)/dx))/2

ilft = Box%left + offx
itop = Box%top + offy

!---- Set size/position of larger domain

ictrl = GetDlgItem( iwnd_db,IDB_STATIC20 )

Pt%x = ilft + NINT(FLOAT(scalex)*(xmina-xmin)/dx)
Pt%y = itop + NINT(FLOAT(scaley)*(ymax-ymaxa)/dx)
irv  = ScreenToClient( iwnd_db,Pt )

iwid = MAX(NINT(FLOAT(scalex)*(xmaxa-xmina)/dx),10)
ihgt = MAX(NINT(FLOAT(scaley)*(ymaxa-ymina)/dx),10)

irv = SetWindowPos( ictrl,  &
                    0,  &
                    Pt%x,Pt%y,  &
                    iwid,ihgt,  &
                    SWP_NOZORDER ) !  Change flag

ictrl = GetDlgItem( iwnd_db,IDB_STATIC21 )
irv = SetWindowPos( ictrl,  &
                    0,  &
                    Pt%x,Pt%y,  &
                    iwid,ihgt,  &
                    SWP_NOZORDER ) !  Change flag

!---- Set size/position of smaller domain

ictrl = GetDlgItem( iwnd_db,IDB_STATIC30 )

Pt%x = ilft + MIN(NINT(FLOAT(scalex)*(xminb-xmin)/dx),scalex-10)
Pt%y = itop + MIN(NINT(FLOAT(scaley)*(ymax-ymaxb)/dx),scaley-10)
irv  = ScreenToClient( iwnd_db,Pt )

iwid = MAX(NINT(FLOAT(scalex)*(xmaxb-xminb)/dx),10)
ihgt = MAX(NINT(FLOAT(scaley)*(ymaxb-yminb)/dx),10)

irv = SetWindowPos( ictrl,  &
                    0,  &
                    Pt%x,Pt%y,  &
                    iwid,ihgt,  &
                    SWP_NOZORDER ) !  Change flag

ictrl = GetDlgItem( iwnd_db,IDB_STATIC31 )
irv = SetWindowPos( ictrl,  &
                    0,  &
                    Pt%x,Pt%y,  &
                    iwid,ihgt,  &
                    SWP_NOZORDER ) !  Change flag

1000 CONTINUE

CALL SetControlText( iwnd_db,IDB_STATIC02,string2 )
CALL SetControlText( iwnd_db,IDB_STATIC03,string3 )
icon = IDI_EXCLAMATION
ictrl = NULL_POINTER
ipc = LoadIcon( ictrl,icon )
IF( ipc > 0 )CALL SetControlIcon( iwnd_db,IDB_STATIC88,ipc )

!     Warning/Error differences

IF( error%iParm == WN_ERROR )THEN
  icon = IDI_QUESTION
  string3 = ' '
  CALL SetControlText( iwnd_db,IDB_STATIC11,string3 )
ELSE
  icon = IDI_HAND
  CALL EnableControl( iwnd_db,ID_OK,FALSE )
  CALL ShowControl( iwnd_db,ID_OK,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )
  string3 = 'OK'
  CALL SetControlText( iwnd_db,ID_CANCEL,string3 )
END IF

ipc = LoadIcon( ictrl,icon )
IF( ipc > 0 )CALL SetControlIcon( iwnd_db,IDB_STATIC89,ipc )

RETURN
END
!*******************************************************************************
!            Save Special Error Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_errorbox()

USE GUIerror_fi
!
!     This routine saves the Special error Dialog Box
!
IMPLICIT NONE

ErrorBoxResult = .TRUE.

RETURN
END
!***********************************************************************
!               WriteMessage
!***********************************************************************
SUBROUTINE WriteMessage( eType )

USE files_fi
USE GUIerror_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: eType

CHARACTER(128) eString
CHARACTER(12)  eLeader
LOGICAL        lOpened
INTEGER        lun, ios

!==== Initialize strings

eLeader = ' ********** '
eString = TRIM(eType)//' Message'

!==== Decide appropiate file to write the message
!     Log file if available else debug file

INQUIRE(FILE=file_log,opened=lOpened)
IF( lOpened )THEN
  lun = lun_log
ELSE
  lun = lun_dbg
END IF

!==== Write message

WRITE(lun,100,IOSTAT=ios)eLeader
WRITE(lun,100,IOSTAT=ios)eLeader//TRIM(eString)//eLeader
WRITE(lun,100,IOSTAT=ios)eLeader
WRITE(lun,100,IOSTAT=ios)eLeader//'Routine = '//TRIM(error%routine)
WRITE(lun,100,IOSTAT=ios)eLeader//'Message = '//TRIM(error%aString)
IF( LEN_TRIM(error%bString) > 0 )WRITE(lun,100,IOSTAT=ios)eLeader//'          '//TRIM(error%bString)
IF( LEN_TRIM(error%cString) > 0 )WRITE(lun,100,IOSTAT=ios)eLeader//'          '//TRIM(error%cString)
WRITE(lun,100,IOSTAT=ios)eLeader

RETURN
100 FORMAT(A)
END
