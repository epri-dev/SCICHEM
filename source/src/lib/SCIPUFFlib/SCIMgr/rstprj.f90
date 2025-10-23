!*******************************************************************************
!            Create a Project
!*******************************************************************************
INTEGER FUNCTION RestartProject( UserID,createRst )

USE files_fi
USE error_fi
USE SCIMgr_fi
USE SCIMgr_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,                    INTENT( IN ) :: UserID !USER ID Tag
TYPE( createRstT ), TARGET, INTENT( IN ) :: createRst !Project input

INTEGER ios, irv, request, nMtl, currentState
LOGICAL lmcw

TYPE( materialT ), ALLOCATABLE, DIMENSION(:), TARGET :: mtlList !Material list

CHARACTER(PATH_MAXLENGTH)   string1,string2,string3
CHARACTER(PATH_MAXLENGTH*2) test_string

TYPE( pinputT   ) tinput
TYPE( pweatherT ) tweather
TYPE( fileNameT ) cFile

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

INTEGER,              EXTERNAL :: CountMaterial
INTEGER,              EXTERNAL :: Write_Inp, Write_Weather, Load_Release
INTEGER,              EXTERNAL :: sysGetLastError,sysCopyFile
INTEGER,              EXTERNAL :: DeleteProject
INTEGER,              EXTERNAL :: Check_Inp, Check_Weather
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler

!==== Initialize
RestartProject = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Set CallBack routine

CALL setSWIMcallback( GetMessageHandler(),userID,clockDelay,clockUpdate )

!==== Check total project file name length

test_string = TRIM(createRst%project%name)
CALL AddPath( test_string,TRIM(createRst%project%path) )

request = LEN_TRIM(test_string)
IF( request+5 > PATH_MAXLENGTH )THEN
  eMessage = 'Project name w/ path and extension is too long'
  WRITE(eInform,'(A,I4,A,I4)') 'Current length: ',request,' Max is ',PATH_MAXLENGTH-5
  GOTO 9999
END IF

!==== Set file names to restart project names

string1 = TRIM(createRst%input%ctrl%name)
CALL AddPath( string1,TRIM(createRst%input%ctrl%path) )

string1 = TRIM(AddExtension(string1,'inp'))
CALL SetFileNamesT( string1 )

!==== Allocate space for materials

cFile%string = TRIM(string1)
irv = CountMaterial( UserID,cFile,nMtl )
IF( irv /= SCIPsuccess )THEN
  nError   = SZ_ERROR
  eRoutine = 'RestartProject'
  eMessage = 'Error counting materials in restart project'
  CALL ReportFileName( eInform,'File=',string1 )
  GOTO 9999
END IF

IF( nMtl <= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'RestartProject'
  eMessage = 'Error counting materials in restart project'
  WRITE(eInform,*)'Number =',nMtl
  GOTO 9999
END IF

ALLOCATE( mtlList(nMtl),STAT=ios )
IF( ios /= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'RestartProject'
  eMessage = 'Error allocating space for material structures'
  WRITE(eInform,*)'Number requested =',nMtl
  GOTO 9999
END IF

tinput%input%mtlHead%max = nMtl

!==== Start GUI Progress Box

!==== Inform GUI that SCIPUFF is starting

CALL start_message( -2 )
IF( nError /= NO_ERROR )GOTO 9999

toolState = IBSET(toolState,HSB_PBOX)

!!DEC$ IF DEFINED (DBGOUT)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')'Input Dump - RestartProject'
!WRITE(lun_dbg,'(A)')'Project : '//TRIM(createRst%project%name)
!WRITE(lun_dbg,'(A)')'Path    : '//TRIM(createRst%project%path)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!CALL DumpEnd(createRst%input%end)
!CALL DumpDomain(createRst%input%domain)
!CALL DumpAudit(createRst%input%audit)
!CALL DumpOptions(createRst%input%option)
!CALL DumpWeather(createRst%weather)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!WRITE(lun_dbg,'(A)')REPEAT('*',80)
!!DEC$ ENDIF

!==== Read old project

string1 = 'Preparing to create project'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = TRIM(createRst%input%ctrl%name)
CALL AddPath( string3,TRIM(createRst%input%ctrl%path) )

string3 = 'Reading '//TRIM(string3)
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

CALL SetSCIPUFFVersion()

CALL read_prj()
IF( nError == VN_ERROR )CALL ModuleInitError() !Ignore version errors here
IF( nError /= NO_ERROR )GOTO 9999            !since they only affect running a project

CALL LoadStart( tinput%input%time%start )

CALL LoadFlags( tinput%input%flags )
IF( irv /= SCIPsuccess )GOTO 9999

CALL LoadMaterial( tinput%input%mtlHead,mtlList )
IF( nError /= NO_ERROR )GOTO 9999

!==== Check Input data validity

string1 = 'Preparing to create project'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = 'Validating input data'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

tinput%project           = createRst%project
tinput%input%time%end    = createRst%input%end
tinput%input%flags%audit = createRst%input%audit
tinput%input%domain      = createRst%input%domain
tinput%input%option      = createRst%input%option
tinput%input%ctrl        = createRst%input%ctrl
tinput%input%flags%start = IBSET(tinput%input%flags%start,HFB_RESTART)

irv = Check_Inp( tinput%input,mtlList )
IF( irv == SCIPfailure )GOTO 9999

irv = Check_Weather( createRst%weather )
IF( irv == SCIPfailure )GOTO 9999

!==== Reset file names to new project names

string1 = TRIM(createRst%project%name)
CALL AddPath( string1,TRIM(createRst%project%path) )

string1 = TRIM(AddExtension(string1,'inp'))
CALL SetFileNamesT( string1 )

ProjectID = createRst%project

!==== Delete Project files

string1 = 'Preparing to create project'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = 'Deleting existing project files'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

lmcw = BTEST(createRst%weather%met%type,HWB_METMED) .OR. &
       BTEST(createRst%weather%met%type,HWB_METMRF)

IF( lmcw )THEN
  string1 = TRIM(file_mcw); CALL cupper( string1 )
  string2 = TRIM(createRst%weather%met%input(1)); CALL cupper( string2 )
  lmcw = TRIM(string1) == TRIM(string2)
END IF

request = HD_INPUT + HD_OUTPUT
IF( .NOT.lmcw )THEN
  request = IBSET(request,HDB_MCWFILE)
ELSE
  request = IBCLR(request,HDB_MCWFILE)
END IF

irv = DeleteProject( UserID,ProjectID,request )
IF( irv /= request )THEN
  eMessage = 'DeleteProject error'
  WRITE(eInform,*)'Request=',request,' : Reply=',irv
  GOTO 9997
END IF

!==== Write Input files

string1 = 'Creating project input files'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = 'Writing INP file'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

irv = Write_Inp( tinput,mtlList )
IF( irv == SCIPfailure )GOTO 9999

string1 = CHAR(0)
string2 = CHAR(0)
string3 = 'Copying SCN file'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

string1 = TRIM(tinput%input%ctrl%name)
CALL AddPath( string1,TRIM(tinput%input%ctrl%path) )

string1 = TRIM(AddExtension(string1,'scn'))
irv = sysCopyFile( string1,file_scn )
IF( irv == SCIPfailure )THEN
  eMessage = 'sysCopyFile error'
  eInform  = TRIM(string1)//'=>'//TRIM(file_scn)
  GOTO 9997
END IF

string1 = CHAR(0)
string2 = CHAR(0)
string3 = 'Writing MSC file'
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9999

tweather%project = createRst%project
tweather%weather = createRst%weather
irv = Write_Weather( tweather )
IF( irv == SCIPfailure )GOTO 9999

!==== Run Scipuff

CALL ExceptionTest()

string1 = 'SCIPUFF starting creation of project files'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )
IF( nError /= NO_ERROR )GOTO 9998

CALL initCaution()

CALL RunScipuff( -2 )
IF( nError /= NO_ERROR )GOTO 9998

!==== Set Return value

RestartProject = SCIPsuccess

!==== Inform calling program

9998 CONTINUE

CALL writeCaution()

string1 = 'SCIPUFF finished creating project files'
string2 = TRIM(createRst%project%name)
CALL AddPath( string2,TRIM(createRst%project%path) )

string2 = 'Project='//TRIM(string2)
string3 = ' '
CALL write_progress( string1,string2,string3 )

!==== Close LOG file

9999 CONTINUE

CLOSE( UNIT=lun_log, IOSTAT=ios )
CLOSE( UNIT=lun_clog,IOSTAT=ios )
IF( ALLOCATED(mtlList) )DEALLOCATE( mtlList )

CALL stop_Message( message )

toolState = IBCLR(toolState,HSB_PBOX)

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN

9997 CONTINUE

nError    = IV_ERROR
eRoutine  = 'RestartProject'
LastError = sysGetLastError()
GOTO 9999

END

