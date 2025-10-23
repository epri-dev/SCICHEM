!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION LoadProject( UserID,project,mtlList,relList,relMCList )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIPresults_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectT ),                 INTENT( INOUT ) :: project !Project ID
TYPE( materialT ),  DIMENSION(*), INTENT( OUT   ) :: mtlList !Material list
TYPE( releaseT ),   DIMENSION(*), INTENT( OUT   ) :: relList !Release list
TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( OUT ) :: relMCList !Release multicomponent list

INTEGER currentState, ios, irv
INTEGER save_status, i

TYPE( pweatherT ) weather
TYPE( preleaseT ) scenario
TYPE( pctrlT    ) ctrl
TYPE( pspatialT ) domain
TYPE( ptemporalT) time

CHARACTER(PATH_MAXLENGTH) string, filename

INTEGER, EXTERNAL :: Load_Domain, Load_Time, Load_Ctrl, Load_Release
INTEGER, EXTERNAL :: Load_Weather
INTEGER, EXTERNAL :: ExtracProjectVersion
INTEGER, EXTERNAL :: Load_ReleaseMC

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension, StripNull

!==== Initialize

LoadProject = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Initialize status

project%current%status = SCIPnull

!==== Load input data

!==== Load Restart data since its not saved on project file

ctrl%project = project%project
ctrl%ctrl    = project%input%ctrl

irv = Load_Ctrl( ctrl )
IF( irv == SCIPfailure )GOTO 9999

!==== Load Start data since time status is not saved on project file

time%project = project%project
time%time    = project%input%time

irv = Load_Time( time )
IF( irv == SCIPfailure )GOTO 9999

!==== Load Domain data since domain status is not saved on project file

domain%project = project%project
domain%spatial = project%input%domain

irv = Load_Domain( domain )
IF( irv == SCIPfailure )GOTO 9999

!==== Unload other structures to ensure that SCIPUFF common values have been initialized

CALL UnloadFlags( project%input%flags )

CALL UnloadOptions( project%input%option )

CALL UnloadMaterial( project%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Read project file

string = TRIM(project%project%name)
CALL AddPath( string,TRIM(project%project%path) )
string = TRIM(AddExtension(string,'inp'))
CALL SetFileNamesT( string )

CALL SetSCIPUFFVersion()

CALL read_prj()
IF( nError == VN_ERROR )CALL ModuleInitError() ! Ignore version errors here
IF( nError /= NO_ERROR )GOTO 9999            ! since they only effect running a project

!==== Load data

project%project%version = ExtracProjectVersion()

project%input%ctrl     = ctrl%ctrl
project%input%time%end = time%time%end

CALL LoadStart( project%input%time%start )

CALL LoadFlags( project%input%flags )
IF( irv /= SCIPsuccess )GOTO 9999

CALL LoadOptions( project%input%option )

CALL LoadDomain( project%input%domain )

CALL LoadMaterial( project%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

IF( ctrl%ctrl%runTime > 0.0 .AND. LEN_TRIM(ctrl%ctrl%name) > 0 )THEN
  project%input%flags%start = IBSET(project%input%flags%start,HFB_RESTART)
END IF

CALL ResetProjectData(project%input%time%start,project%current)
IF( nError /= NO_ERROR )GOTO 9999

!==== Restore original input data

project%input%domain%domain%vRes = domain%spatial%domain%vRes
project%input%domain%domain%zMax = domain%spatial%domain%zMax

!==== Load Met data

weather%project = project%project
weather%weather = project%weather

irv = Load_Weather( weather )
IF( irv == SCIPfailure )GOTO 9999

project%weather = weather%weather

!==== Load/Update Release data

scenario%project = project%project
scenario%scnHead = project%scnHead
scenario%control%mode          = SCIPnull
scenario%control%searchID      = ' '
scenario%control%fileExtension = ' '

IF( PRESENT(relMCList) )THEN
  irv = Load_ReleaseMC( scenario,relList,relMCList )
ELSE
  irv = Load_Release( scenario,relList )
END IF
IF( irv == SCIPfailure )GOTO 9999

project%scnHead = scenario%scnHead

!==== Update from log file

scenario%control%mode          = HC_FILE + HC_SEARCH + HC_REPLACE
scenario%control%fileExtension ='log'

filename = TRIM(AddExtension(scenario%project%name,scenario%control%fileExtension))
CALL AddPath( filename,scenario%project%path )
OPEN(UNIT=lun_tmp,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'LoadProject'
  eMessage = 'Error opening SCIPUFF log file'//TRIM(filename)
  GOTO 9999
END IF

DO i = 1,project%scnHead%number
  IF( LEN_TRIM(relList(i)%relName) > 0 )THEN
    save_status = relList(i)%status
    scenario%control%searchID = TRIM(StripNull( relList(i)%relName ))
    IF( PRESENT(relMCList) )THEN
      irv = Load_ReleaseMC( scenario,relList,relMCList )
    ELSE
      irv = Load_Release( scenario,relList )
    END IF
    IF( irv == SCIPfailure )THEN
      IF( nError == EOF_ERROR )THEN
        CALL ModuleInitError()
      ELSE
        CLOSE(UNIT=lun_tmp,IOSTAT=ios)
        GOTO 9999
      END IF
    ELSE
      relList(i)%status = save_status
    END IF
  END IF
END DO

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

project%scnHead = scenario%scnHead

!------ Set flags for single/multi-thread

multiThread  = nthreads > 1
singleThread = .NOT.multiThread

!==== Set return value

LoadProject = SCIPsuccess

9999 CONTINUE

CALL deallocate_read_prj()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Size
!*******************************************************************************
INTEGER FUNCTION SizeProject( UserID,project,nMtl,nRel,nMCrel )

USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,             INTENT( IN  ) :: UserID !USER ID tag
TYPE ( projectIDT ), INTENT( IN  ) :: project !Project ID
INTEGER,             INTENT( OUT ) :: nMtl   !number of materials in file
INTEGER,             INTENT( OUT ) :: nRel   !number of releases in file
INTEGER,             INTENT( OUT ) :: nMCrel !number of MC release records in file

INTEGER irv, nr, nm, currentState
INTEGER nmc

CHARACTER(PATH_MAXLENGTH) string

TYPE( fileNameT ) file

INTEGER, EXTERNAL :: CountRelease, CountMaterial

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

SizeProject = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Read project file

string = TRIM(project%name)
CALL AddPath( string,TRIM(project%path) )
string = TRIM(AddExtension(string,'inp'))
file%string = TRIM(string)

irv = CountMaterial( UserID,file,nm )
IF( irv == SCIPfailure )GOTO 9999

!==== Count Releases

string = TRIM(project%name)
CALL AddPath( string,TRIM(project%path) )
string = TRIM(AddExtension(string,'scn'))
file%string = TRIM(string)

irv = CountRelease( UserID,file,nr,nmc )
IF( irv == SCIPfailure )GOTO 9999

!==== Set return value

nMtl   = nm
nRel   = nr
nMCrel = nmc

SizeProject = SCIPsuccess

9999 CONTINUE

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Version
!*******************************************************************************
INTEGER FUNCTION GetProjectVersion( UserID,project )

USE SCIMgr_fd
USE error_fi
USE SCIMgr_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: UserID !USER ID Tag
TYPE( projectIDT ), INTENT( INOUT ) :: project !Project ID

INTEGER irv
INTEGER currentState

CHARACTER(PATH_MAXLENGTH) string

INTEGER,                   EXTERNAL :: ExtracProjectVersion
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

GetProjectVersion = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Load input data

!==== Read project file

string = TRIM(project%name)
CALL AddPath( string,TRIM(project%path) )
string = TRIM(AddExtension(string,'inp'))
CALL SetFileNamesT( string )

CALL SetSCIPUFFVersion()

CALL read_prj()
IF( nError == VN_ERROR )CALL ModuleInitError() !Ignore version errors here
IF( nError /= NO_ERROR )GOTO 9999            !since they only effect running a project

!==== Load data

project%version = ExtracProjectVersion()

!==== Set return value

GetProjectVersion = SCIPsuccess

9999 CONTINUE

CALL deallocate_read_prj()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Audit
!*******************************************************************************
INTEGER FUNCTION GetProjectAudit( UserID,audit )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: UserID !USER ID Tag
TYPE( pauditT ), INTENT( INOUT ) :: audit !Project ID

INTEGER irv
INTEGER currentState

CHARACTER(PATH_MAXLENGTH) string

TYPE( flagsT ) :: flags

INTEGER,                   EXTERNAL :: ExtracProjectVersion
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

GetProjectAudit = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Load input data

!==== Read project file

string = TRIM(audit%project%name)
CALL AddPath( string,TRIM(audit%project%path) )
string = TRIM(AddExtension(string,'inp'))
CALL SetFileNamesT( string )

CALL SetSCIPUFFVersion()

CALL read_prj()
IF( nError == VN_ERROR )CALL ModuleInitError() !Ignore version errors here
IF( nError /= NO_ERROR )GOTO 9999            !since they only effect running a project

!==== Load data

audit%project%version = ExtracProjectVersion()

CALL LoadFlags( flags )

audit%audit = flags%audit

!==== Set return value

GetProjectAudit = SCIPsuccess

9999 CONTINUE

CALL deallocate_read_prj()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Reset Project Input data
!*******************************************************************************
SUBROUTINE ResetProjectData( start,current )

USE SCIMgr_fd
USE scipuff_fi
USE files_fi

IMPLICIT NONE

TYPE( statusT ), INTENT( INOUT ) :: current
TYPE( startT  ), INTENT( IN    ) :: start

REAL    tx
INTEGER np, ios

INTEGER, EXTERNAL :: ComputeTime

IF( .NOT.BTEST(run_mode,FAST_MODE) )THEN
  IF( delmin == 0.0 )THEN
    delmin = DEF_VAL_R
  END IF
END IF

IF( lter )THEN
  current%status = IBSET(current%status,HSB_HASTERRAIN)
ELSE
  current%status = IBCLR(current%status,HSB_HASTERRAIN)
END IF

IF( dose )THEN
  current%status = IBSET(current%status,HSB_HASDOS)
ELSE
  current%status = IBCLR(current%status,HSB_HASDOS)
END IF

IF( surface )THEN
  current%status = IBSET(current%status,HSB_HASDEP)
ELSE
  current%status = IBCLR(current%status,HSB_HASDEP)
END IF

current%nPuffType = ntypp

!------ get last puff time

CALL CurrentPuffHeader( tx, np )

IF( np > 0 )THEN
  current%status = IBSET(current%status,HSB_HASPUFFS)
ELSE
  current%status = IBCLR(current%status,HSB_HASPUFFS)
END IF

ios = ComputeTime( start,tx/3600.,current%time )

RETURN
END

