!*******************************************************************************
!            Create a Project
!*******************************************************************************
INTEGER FUNCTION NewProject( UserID,createNew,mtlList,relList,nMC,relMCList,lWrite )

USE files_fi
USE error_fi
USE SCIMgr_fi
USE default_fd
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER ios

INTEGER,                                    INTENT( IN    ) :: UserID    !USER ID Tag
TYPE( createNewT ),                 TARGET, INTENT( INOUT ) :: createNew !Project ID
TYPE( materialT ),  DIMENSION(*),   TARGET, INTENT( INOUT ) :: mtlList   !Material list
TYPE( releaseT ),   DIMENSION(*),   TARGET, INTENT( INOUT ) :: relList   !Release list
INTEGER,                          OPTIONAL, INTENT( IN    ) :: nMC       !Size of relMCList
TYPE( releaseMCT ), DIMENSION(*), OPTIONAL, INTENT( IN    ) :: relMCList !Release multicomponent list
LOGICAL,                          OPTIONAL, INTENT( IN    ) :: lWrite    !Write input files

INTEGER irv
INTEGER request

CHARACTER(PATH_MAXLENGTH) string1,string2,string3
CHARACTER(PATH_MAXLENGTH*2) test_string

TYPE( pinputT    ) tinput
TYPE( preleaseT  ) trelease
TYPE( pweatherT  ) tweather

LOGICAL lmcw, lscn, doWrite

INTEGER currentState

INTEGER iRel
INTEGER iMat

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTEGER,              EXTERNAL :: sysGetLastError
INTEGER,              EXTERNAL :: DeleteProject
INTEGER,              EXTERNAL :: Write_Inp, Write_Release, Write_Weather
INTEGER,              EXTERNAL :: Check_Inp, Check_Weather, Check_Release
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler
INTEGER,              EXTERNAL :: Check_StackReleases
INTEGER,              EXTERNAL :: Write_ReleaseMC
INTEGER,              EXTERNAL :: AdjointReleaseFilter

!==== Initialize

NewProject = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Set Write flag

IF( PRESENT(lWrite) )THEN
  doWrite = lWrite
ELSE
  doWrite = .TRUE.
END IF

IF( BTEST(createNew%input%flags%mode,HFB_REVERSE) .AND. doWrite )THEN
  irv = AdjointReleaseFilter( createNew%scnHead%number,relList, &
                              createNew%input%mtlHead%number,mtlList )
  IF( irv == SCIPfailure )GOTO 9999
END IF

!==== Initialize error

CALL ModuleInitError()

!==== Set CallBack routine

CALL setSWIMcallback( GetMessageHandler(),userID,clockDelay,clockUpdate )

CALL ExceptionTest()

!==== Check total project file name length

test_string = TRIM(createNew%project%name); CALL AddPath( test_string,TRIM(createNew%project%path) )
request = LEN_TRIM(test_string)
IF( request+5 > PATH_MAXLENGTH )THEN
  eMessage = 'Project name w/ path and extension is too long'
  WRITE(eInform,'(A,I4,A,I4)') 'Current length: ',request,' Max is ',PATH_MAXLENGTH-5
  GOTO 1100
END IF

!==== Set file names

string1 = TRIM(createNew%project%name)
CALL AddPath( string1,TRIM(createNew%project%path) )
string1 = TRIM(AddExtension(string1,'inp'))
CALL SetFileNamesT( string1 )

ProjectID = createNew%project

!==== Start GUI Progress Box

!==== Inform GUI that SCIPUFF is starting

CALL start_message( -2 )
IF( nError /= NO_ERROR )GOTO 9999

toolState = IBSET( toolState,HSB_PBOX )

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - NewProject'
!WRITE(lun_dbg,'(A)')'Project : '//TRIM(createNew%project%name)
!WRITE(lun_dbg,'(A)')'Path    : '//TRIM(createNew%project%path)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpTime(createNew%input%time)
!CALL DumpDomain(createNew%input%domain)
!CALL DumpFlags(createNew%input%flags)
!CALL DumpOptions(createNew%input%option)
!CALL DumpWeather(createNew%weather)
!CALL DumpMaterial(createNew%input%mtlHead%number,mtlList)
!CALL DumpRelease(createNew%scnHead%number,relList)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF
!==== Check Input data validity

string1 = 'Preparing to create project'
string2 = TRIM(createNew%project%name)
CALL AddPath( string2,TRIM(createNew%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = 'Validating input data'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

tinput%project       = createNew%project
tinput%input%time    = createNew%input%time
tinput%input%flags   = createNew%input%flags
tinput%input%domain  = createNew%input%domain
IF( tinput%input%domain%domain%coord == HD_UTM )THEN
  tinput%input%domain%reference%lat = NOT_SET_R
  tinput%input%domain%reference%lon = NOT_SET_R
  tinput%input%domain%reference%x   = NOT_SET_R
  tinput%input%domain%reference%y   = NOT_SET_R
END IF
tinput%input%option  = createNew%input%option
tinput%input%mtlHead = createNew%input%mtlHead
tinput%input%ctrl%runTime = 0.0
tinput%input%ctrl%name    =' '
tinput%input%ctrl%path    =' '
tinput%input%flags%start  = IBCLR(tinput%input%flags%start,HFB_RESTART)

irv = Check_Inp( tinput%input,mtlList )
IF( irv == SCIPfailure )GOTO 9999

irv = Check_Weather( createNew%weather )
IF( irv == SCIPfailure )GOTO 9999

DO iRel = 1,createNew%scnHead%number
  IF( relList(iRel)%type == HR_FILE )THEN
    iMat = 1
  ELSE
    DO iMat = 1,createNew%input%mtlHead%number
      IF( TRIM(relList(iRel)%material) == TRIM(mtlList(iMat)%name) )EXIT
    END DO
    iMat = MIN(iMat,createNew%input%mtlHead%number)
  END IF
  irv = Check_Release( relList(iRel),mtlList(iMat) )
  IF( irv == SCIPfailure )GOTO 9999
END DO

irv = Check_StackReleases( createNew%scnHead%number,relList, &
                           createNew%input%mtlHead%number,mtlList )
IF( irv == SCIPfailure )GOTO 9999

!==== Delete Project files

string1 = 'Preparing to create project'
string2 = TRIM(createNew%project%name)
CALL AddPath( string2,TRIM(createNew%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = 'Deleting existing project files'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

lmcw = BTEST(createNew%weather%met%type,HWB_METMED) .OR. &
       BTEST(createNew%weather%met%type,HWB_METMRF)

IF( lmcw )THEN
  string1 = TRIM(file_mcw); CALL cupper( string1 )
  string2 = TRIM(createNew%weather%met%input(1)); CALL cupper( string2 )
  lmcw = TRIM(string1) == TRIM(string2)
END IF

lscn = createNew%scnHead%max >= 0

request = HD_OUTPUT
request = request + HD_SPSFILE

IF( doWrite )request = request + HD_INPUT

IF( .NOT.lmcw )THEN
  request = IBSET(request,HDB_MCWFILE)
ELSE
  request = IBCLR(request,HDB_MCWFILE)
END IF

IF( lscn .AND. doWrite )THEN
  request = IBSET(request,HDB_SCNFILE)
ELSE
  request = IBCLR(request,HDB_SCNFILE)
END IF

irv = DeleteProject( UserID,ProjectID,request )
IF( nError /= NO_ERROR )GOTO 9999
IF( irv /= request )THEN
  eMessage = 'DeleteProject error'
  WRITE(eInform,*,IOSTAT=ios)'Request=',request,' : Reply=',irv
  GOTO 1100
END IF

!==== Write Input files

IF( doWrite )THEN
  string1 = 'Creating project input files'
  string2 = TRIM(createNew%project%name)
  CALL AddPath( string2,TRIM(createNew%project%path) )
  string2 = 'Project='//TRIM(string2)
  string3 = 'Writing INP file'
  CALL write_progress( string1,string2,string3 )
  IF( nError /= NO_ERROR )GOTO 9999

  tinput%project       = createNew%project
  tinput%input%time    = createNew%input%time
  tinput%input%flags   = createNew%input%flags
  tinput%input%domain  = createNew%input%domain
  tinput%input%option  = createNew%input%option
  tinput%input%mtlHead = createNew%input%mtlHead
  tinput%input%ctrl%runTime = 0.0
  tinput%input%ctrl%name    = ' '
  tinput%input%ctrl%path    = ' '
  tinput%input%flags%start  = IBCLR(tinput%input%flags%start,HFB_RESTART)

  irv = Write_Inp( tinput,mtlList )
  IF( irv == SCIPfailure )GOTO 9999

  IF( lscn )THEN
    string1 = CHAR(0)
    string2 = CHAR(0)
    string3 = 'Writing SCN file'
    CALL write_progress( string1,string2,string3 )
    IF( nError /= NO_ERROR )GOTO 9999

    trelease%project          = createNew%project
    trelease%scnHead          = createNew%scnHead
    trelease%control%mode     = SCIPnull
    trelease%control%searchID = ' '

    IF( PRESENT(relMCList) )THEN
      irv = Write_ReleaseMC( trelease,relList,nMC,relMCList )
    ELSE
      irv = Write_Release( trelease,relList )
    END IF
    IF( irv == SCIPfailure )GOTO 9999
  END IF

  string1 = CHAR(0)
  string2 = CHAR(0)
  string3 = 'Writing MSC file'
  CALL write_progress( string1,string2,string3 )
  IF( nError /= NO_ERROR )GOTO 9999

  tweather%project = createNew%project
  tweather%weather = createNew%weather

  irv = Write_Weather( tweather )
  IF( irv == SCIPfailure )GOTO 9999
END IF

!==== Run Scipuff

CALL ExceptionTest()

string1 = 'SCIPUFF starting creation of project files'
string2 = TRIM(createNew%project%name)
CALL AddPath( string2,TRIM(createNew%project%path) )
string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9998

CALL initCaution()

CALL RunScipuff( -2 )
IF( nError /= NO_ERROR )GOTO 9998

!==== Set Return value

NewProject = SCIPsuccess

!==== Inform calling program

9998 CONTINUE

CALL writeCaution()

string1 = 'SCIPUFF finished creating project files'
string2 = TRIM(createNew%project%name)
CALL AddPath( string2,TRIM(createNew%project%path) )
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

1100 CONTINUE
nError    = IV_ERROR
eRoutine  = 'NewProject'
LastError = sysGetLastError()
GOTO 9999

END

