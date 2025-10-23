!*******************************************************************************
!            Run a Project
!*******************************************************************************
INTEGER FUNCTION RunProject( UserID,run )

USE files_fi
USE error_fi
USE SCIMgr_fi
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER ios

INTEGER,       INTENT( IN ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN ) :: run !Run input

INTEGER irv
INTEGER currentState

CHARACTER(PATH_MAXLENGTH) string1, string2, string3

LOGICAL lexist

INTEGER,              EXTERNAL :: sysDeleteFile, sysGetLastError
INTEGER,              EXTERNAL :: Write_Run
INTEGER,              EXTERNAL :: Check_End
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripExtension

!==== Initialize

RunProject = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY+HS_RUN )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Check the Input

irv = Check_End( run%end )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Set CallBack routine

CALL setSWIMcallback( GetMessageHandler(),userID,clockDelay,clockUpdate )

!==== Set file names

string1 = TRIM(run%project%name)
CALL AddPath( string1,run%project%path )
string1 = TRIM(AddExtension(string1,'inp'))

CALL SetFileNamesT( string1 )

file_inp = StripExtension( file_inp )
file_inp = AddExtension( file_inp,'rst' )

ProjectID = run%project

!==== Inform GUI that SCIPUFF is starting

CALL start_message( -1 )
IF( nError /= NO_ERROR )GOTO 9999

toolState = IBSET( toolState,HSB_PBOX )

!==== Build Restart file

INQUIRE(FILE=file_inp,EXIST=lexist)
IF( lexist )THEN
  irv = sysDeleteFile( file_inp )
  IF( irv == SCIPfailure )THEN
    nError   = IV_ERROR
    eRoutine = 'RunProject'
    eMessage = 'sysDeleteFile error'
    CALL ReportFileName( eInform,'File=',file_inp )
    LastError = sysGetLastError()
    GOTO 9999
  END IF
END IF

irv = Write_Run( run )
IF( irv == SCIPfailure )GOTO 9999

!==== Run Scipuff

string1 = 'SCIPUFF starting dispersion calculation'
string2 = TRIM(run%project%name)
CALL AddPath( string2,TRIM(run%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9998

CALL initCaution()

CALL RunScipuff( -1 )
IF( nError /= NO_ERROR )GOTO 9998

!==== Set return value

RunProject = SCIPsuccess

!==== Inform calling program

9998 CONTINUE

CALL writeCaution()

string1 = 'SCIPUFF finished dispersion calculation'
string2 = TRIM(run%project%name)
CALL AddPath( string2,TRIM(run%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )

!==== Close LOG file

9999 CONTINUE
CLOSE(UNIT=lun_log,IOSTAT=ios)
CLOSE(UNIT=lun_clog,IOSTAT=ios)

CALL stop_message( message )

toolState = IBCLR( toolState,HSB_PBOX )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Initialize a Project to run in single step mode
!*******************************************************************************
INTEGER FUNCTION InitProjectSS( UserID,run )

USE files_fi
USE error_fi
USE SCIMgr_fi
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN ) :: run !Run input

INTEGER irv

CHARACTER(PATH_MAXLENGTH) string1, string2, string3

LOGICAL lexist

INTEGER,              EXTERNAL :: sysDeleteFile, sysGetLastError
INTEGER,              EXTERNAL :: Write_Run
INTEGER,              EXTERNAL :: Check_End
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripExtension

!==== Initialize

InitProjectSS = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  SingleStepState = SCIMgrSetState( HS_BUSY+HS_RUN )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Check the Input

irv = Check_End( run%end )
IF( irv /= SCIPsuccess )GOTO 9999

!==== Set CallBack routine

CALL setSWIMcallback(  GetMessageHandler(),userID,clockDelay,clockUpdate )

!==== Set file names

string1 = TRIM(run%project%name)
CALL AddPath( string1,run%project%path )
string1 = TRIM(AddExtension(string1,'inp'))

CALL SetFileNamesT( string1 )

file_inp = StripExtension( file_inp )
file_inp = AddExtension( file_inp,'rst' )

ProjectID = run%project

!==== Inform GUI that SCIPUFF is starting

CALL start_message( 0 )
IF( nError /= NO_ERROR )GOTO 9999

toolState = IBSET( toolState,HSB_PBOX )

!==== Build Restart file

INQUIRE(FILE=file_inp,EXIST=lexist)
IF( lexist )THEN
  irv = sysDeleteFile( file_inp )
  IF( irv == SCIPfailure )THEN
    nError   = IV_ERROR
    eRoutine = 'InitProjectSS'
    eMessage = 'sysDeleteFile error'
    CALL ReportFileName( eInform,'File=',file_inp )
    LastError = sysGetLastError()
    GOTO 9999
  END IF
END IF

irv = Write_Run( run )
IF( irv == SCIPfailure )GOTO 9999

!==== Initialize Scipuff

string1 = 'SCIPUFF starting dispersion calculation'
string2 = TRIM(run%project%name)
CALL AddPath( string2,TRIM(run%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9998

CALL RunScipuff( 0 )
IF( nError /= NO_ERROR )GOTO 9998

!==== Set return value

InitProjectSS = SCIPsuccess

!==== Inform calling program

9998 CONTINUE
string1 = 'SCIPUFF initialized dispersion calculation'
string2 = TRIM(run%project%name)
CALL AddPath( string2,TRIM(run%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )

!==== Set SCIPtool state to single step mode

9999 CONTINUE

toolState = IBSET(toolState,HSB_STEP)

RETURN
END
!*******************************************************************************
!            Run a Project for single step
!*******************************************************************************
INTEGER FUNCTION RunProjectSS( t )

USE SCIMgr_fd
USE error_fi
USE SCIMgr_fi
USE SCIMgrState

IMPLICIT NONE

REAL, INTENT( OUT ) :: t

LOGICAL lcontinue

LOGICAL, EXTERNAL :: StepScipuff

!==== Initialize

RunProjectSS = SCIPfailure

IF( SCIMgrCheckState(HS_SINGLESTEP) )THEN !Only available in Single Step mode
  toolState = IBCLR(toolState,HSB_STEP)
  toolState = IBCLR(toolState,HSB_SYNC)
  toolState = IBCLR(toolState,HSB_WAIT)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!------ Run for one step

lcontinue = StepScipuff( t )
IF( nError /= NO_ERROR .AND. nError /= AB_ERROR )GOTO 9999

!==== Set return value

IF( lcontinue )THEN
  RunProjectSS = SCIPsuccess
ELSE
  RunProjectSS = SCIPnull
END IF

9999 CONTINUE

toolState = IBSET(toolState,HSB_STEP)
toolState = IBSET(toolState,HSB_SYNC)
toolState = IBSET(toolState,HSB_WAIT)

RETURN
END
!*******************************************************************************
!            Exit Project from single step mode
!*******************************************************************************
INTEGER FUNCTION ExitProjectSS()

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIMgr_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER ios
INTEGER irv

!==== Initialize

ExitProjectSS = SCIPfailure

IF( SCIMgrCheckState(HS_SINGLESTEP) )THEN !Only available in Single Step mode
  toolState = IBCLR(toolState,HSB_STEP)
  toolState = IBCLR(toolState,HSB_WAIT)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!------ Terminate the run

CALL ExitScipuff( .TRUE. )
IF( nError == S0_ERROR .OR. nError == AB_ERROR )CALL InfoMessage( )

message%iParm   = nError
message%jParm   = FALSE
message%routine = eRoutine
message%aString = eMessage
message%bString = eInform
message%cString = eAction

IF( nError /= NO_ERROR )GOTO 9999

!==== Set return value

ExitProjectSS = SCIPsuccess

!==== Close LOG file

9999 CONTINUE

CLOSE(UNIT=lun_log,IOSTAT=ios)
CLOSE(UNIT=lun_clog,IOSTAT=ios)

CALL stop_message( message )

irv = SCIMgrSetState( SingleStepState )

CALL reset_messaging()

RETURN
END
