!*******************************************************************************
!                InitFileNames
!*******************************************************************************
SUBROUTINE InitFileNames( hInstance,hWindow,iniFile,Smode, &
                          Hversion,Pversion,loadfile,CodeLim, &
                          datapath,temppath )

USE resource_fd
USE mettype_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN) hInstance  !Instance Handle
INTEGER(POINTER_LEN) hWindow    !Window Handle
CHARACTER(*) iniFile            !INI file name and path
CHARACTER(*) Smode              !SCIPUFF Mode string
CHARACTER(*) Hversion           !GUI Version string
CHARACTER(*) Pversion           !Tool Version string
CHARACTER(*) loadfile(0:21)     !Default Load directories
INTEGER      CodeLim(6)         !Maximum size for OpView/DTED - to be written to the INI
CHARACTER(*) datapath           !Data Dir
CHARACTER(*) temppath           !Temp Dir

CHARACTER(PATH_MAXLENGTH) string1,string2,string3,string4,path,default,mode
CHARACTER(PATH_MAXLENGTH) climo_dir(4)

INTEGER irv,ios,i

LOGICAL,        EXTERNAL :: CheckFile_NoError, CheckPath, CheckFile
INTEGER,        EXTERNAL :: sysGetProfileString,sysGetProfileInt,sysWriteProfileString
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, StripNull

!     Set Permanent Path Names

path_app(1:) = ' ' !PCSCIPUF Directory
path_tmp(1:) = ' ' !Current Working Directory
path_inv(1:) = ' ' !SCIPUFF Data directory (temp)
path_usr(1:) = ' ' !HASCAL Main directory
path_map(1:) = ' ' !ARAP/DCW Map Directory

!==== Set INI file name

string4 = AddNull( TRIM(iniFile) )

!==== Get Default Bin value

irv = GetModuleFileName( hInstance,string2,LEN(string2) )
IF( irv == FALSE )THEN
  default = 'C:\SCIPUFF\BIN'
ELSE
  CALL SplitName( TRIM(StripNull(string2)),string1,default )
  irv = LEN_TRIM(default)
  IF( default(irv:irv) == '\' )default(irv:) = ' '
END IF

!==== Get Bin directory

string1 = AddNull( 'Paths' )
string2 = AddNull( 'scimaindir' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,path_app,string4 )
IF( irv == SCIPnull )THEN
  string2 = AddNull( 'scimaindir' )
  irv =sysGetProfileString( string1,string2,string3,path_app,string4 )
  IF( irv == SCIPnull )THEN
    path_app = TRIM(default)
  ELSE IF( irv == SCIPfailure )THEN
    GOTO 9998
  END IF
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF

!==== Get Debug directory

string2 = AddNull( 'scidbgdir' )
irv =sysGetProfileString( string1,string2,string3,file_tmp,string4 )
IF( irv == SCIPnull )THEN
  file_tmp = TRIM(path_app)
ELSE IF( irv == SCIPfailure )THEN
  GOTO 9998
END IF
IF( .NOT.CheckPath(file_tmp) )THEN
  file_tmp = default
  CALL InitError()
END IF

!====Get Debug file name

default = 'scipuff.dbg'
string1 = AddNull( 'Sci_Files' )
string2 = AddNull( 'DbgFile' )
string3 = AddNull( TRIM(default) )
irv = sysGetProfileString( string1,string2,string3,file_dbg,string4 )
IF( irv /= SCIPsuccess )file_dbg = TRIM(default)
CALL AddPath( file_dbg,file_tmp )

!==== Open Debug File

IF( CheckFile_NoError(file_dbg) )THEN
  string1 = AddNull( TRIM(file_dbg) )
  ios     = DeleteFile( string1 )
END IF

lun_dbg=11

!==== Check Bin dir

IF( .NOT.CheckPath(path_app) )GOTO 9999

!==== Gets SCIPData dir

string1 = TRIM(path_app)
CALL BackUpPath( string1,1 )
default = 'data'
CALL AddPath( default,string1 )
string1 = AddNull( 'Paths' )
string2 = AddNull( 'scidatadir' )
string3 = AddNull( TRIM(default) )

irv = sysGetProfileString( string1,string2,string3,datapath,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(datapath) )GOTO 9999

!==== Get SciData path

default = 'Scidata'
CALL AddPath( default,datapath )
string2 = AddNull( 'scidatadir' )
string3 = AddNull( TRIM(default) )
irv = sysGetProfileString( string1,string2,string3,path_inv,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path_inv) )GOTO 9999

!==== Get Default Project path

string3 = AddNull( 'current' )
string2 = AddNull( 'ProjectDir' )
irv = sysGetProfileString( string1,string2,string3,path_tmp,string4 )
IF( irv == SCIPfailure )GOTO 9998
string2 = TRIM(path_tmp)
CALL cupper( string2 )
IF( TRIM(string2) == 'CURRENT' )THEN
  irv = GetCurrentDirectory( LEN(path_tmp),path_tmp )
  path_tmp = StripNull( path_tmp )
END IF
IF( .NOT.CheckPath(path_tmp) )GOTO 9999

!==== Get map path

default = 'Maps'
CALL AddPath( default,datapath )
string2 = AddNull( 'MapDir' )
string3 = AddNull( TRIM(default) )
irv = sysGetProfileString( string1,string2,string3,path_map,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path_map) )THEN
  CALL AddErrorInform( 'Unable to locate path specified by INI entry "mapdir"' )
  CALL AddErrorAction( 'DCW Map drawing will not be available in this session' )
  CALL ShowErrorMessage( hWindow )
  path_map = ' '
END IF

!==== Set SCIPUFF permanent file Names

string1 = AddNull( 'Sci_Files' )

file_pal(1:) = ' ' !PALETTE File
file_tmp(1:) = ' ' !Temporary file
file_lus(1:) = ' ' !LandUse database
file_ter(1:) = ' ' !Default terrain file (temporary)

!==== Palette file

string2 = AddNull( 'PalFile' )
string3 = AddNull( 'scipuff.pal' )
irv = sysGetProfileString( string1,string2,string3,file_pal,string4 )
IF( irv == SCIPfailure) GOTO 9998
CALL AddPath( file_pal,path_inv )
IF( .NOT.CheckFile(file_pal) )GOTO 9999

!==== Default Plot Options file

IF( file_def == ' ' )THEN
  string2 = AddNull( 'SciPltDefFile' )
  string3 = ' '
  irv = sysGetProfileString( string1,string2,string3,file_def,string4 )
  IF( irv == SCIPsuccess )THEN
    CALL AddPath( file_def,path_inv )
    IF( .NOT.CheckFile(file_def) )GOTO 9999
  ELSE
    file_def = ' '
  END IF
END IF

!==== Default Mass-consistent vertical grid

IF( file_met == ' ' )THEN
  string2 = AddNull( 'GrdDefFile' )
  string3 = ' '
  irv = sysGetProfileString( string1,string2,string3,file_met,string4 )
  IF( irv == SCIPsuccess )THEN
    CALL AddPath( file_met,path_inv )
    IF( .NOT.CheckFile(file_met) )GOTO 9999
  ELSE
    file_met = ' '
  END IF
END IF

!==== Default Terrain file

string2 = AddNull( 'TerDefFile' )
string3 = ' '
irv = sysGetProfileString( string1,string2,string3,file_ter,string4 )
IF( irv /= SCIPsuccess )file_ter = ' '

!==== LandUse data file

string1  = AddNull( 'Land_Use' )
string2  = AddNull( 'LandUseDataFile' )
file_lus = 'landuse.dat'
CALL AddPath( file_lus,path_inv )
string3  = AddNull( TRIM(file_lus) )
file_lus = ' '
irv = sysGetProfileString( string1,string2,string3,file_lus,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckFile(file_lus) )GOTO 9999

!==== Set Default Load Directories/files

string3 = ' '
string1 = AddNull( 'Paths' )

!==== Project files - Project directory

loadfile(0) = TRIM(path_tmp)//'\*.prj' !Project
loadfile(1) = TRIM(path_tmp)//'\*.inp' !Time
loadfile(2) = TRIM(path_tmp)//'\*.inp' !Domain
loadfile(3) = TRIM(path_tmp)//'\*.inp' !Options
loadfile(5) = TRIM(path_tmp)//'\*.scn' !Releases
loadfile(6) = TRIM(path_tmp)//'\*.msc' !Met scenario
loadfile(7) = TRIM(path_tmp)//'\*.inp' !Audit

!==== Plot files

string2 = AddNull( 'PltOptLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(8) = TRIM(path)//'\*.opt'
loadfile(9) = TRIM(path)//'\*.clv'

!==== Met Obs files

string2 = AddNull( 'MetObsLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(12) = TRIM(path)//'\*.prf'
loadfile(17) = TRIM(path)//'\*.sfc'

!==== Met gridded files

string2 = AddNull( 'MetGrdLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(13) = TRIM(path)//'\*.grd'
loadfile(18) = TRIM(path)//'\*.fmt'
loadfile(20) = TRIM(path)//'\*.mlf'
loadfile(21) = TRIM(path)//'\*.lis'

!==== Material files

string2 = AddNull( 'MatDefLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile( 4) = TRIM(path)//'\*.mtl'
loadfile(10) = TRIM(path)//'\*.bin'

!==== Terrain files

string2 = AddNull( 'TerFilLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(11) = TRIM(path)//'\*.grd'
loadfile(14) = TRIM(path)//'\*.ter'

!==== Check Default Terrain file

IF( LEN_TRIM(file_ter) > 0 )THEN
  CALL AddPath( file_ter,path )
  IF( .NOT.CheckFile(file_ter) )THEN
    file_ter = ' '
    CALL InitError()
  END IF
END IF

!==== Release files

string2 = AddNull( 'RelFilLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(15) = TRIM(path)//'\*.rel'

!==== Sampler files

string2 = AddNull( 'SamFilLoadDir' )
string3 = AddNull( TRIM(path_tmp) )
irv = sysGetProfileString( string1,string2,string3,path,string4 )
IF( irv == SCIPfailure )GOTO 9998
IF( .NOT.CheckPath(path) )GOTO 9999
loadfile(16) = TRIM(path)//'\*.sam'

!==== Code Limits

string1 = AddNull( 'SCIPMode' )
string2 = AddNull( 'GUIMode' )
string3 = 'Standard'
irv = sysGetProfileString( string1,string2,string3,mode,string4 )
CALL cupper( mode )
SELECT CASE( TRIM(mode) )
  CASE( 'STANDARD' )
    Smode = 'Operational'
    CodeLim(1) = HUGE(i)
    CodeLim(2) = 20000
    Codelim(3) = 25000
    CodeLim(4) = 100
    CodeLim(5) = 25
    CodeLim(6) = 40
  CASE( 'OPERATIONAL' )
    Smode = 'Operational'
    CodeLim(1) = HUGE(i)
    CodeLim(2) = 20000
    Codelim(3) = 25000
    CodeLim(4) = 100
    CodeLim(5) = 25
    CodeLim(6) = 40
  CASE( 'EXTENDED' )
    Smode = 'Extended'
    CodeLim(1) = HUGE(i)
    CodeLim(2) = 40000
    Codelim(3) = 85000
    CodeLim(4) = 1000
    CodeLim(5) = 100
    CodeLim(6) = 40
  CASE( 'ULTIMATE' )
    Smode = 'Ultimate'
    CodeLim(1) = HUGE(i)
    CodeLim(2) = 60000
    Codelim(3) = 100000
    CodeLim(4) = 8000
    CodeLim(5) = 100
    CodeLim(6) = 40
  CASE DEFAULT
    Smode = 'Custom'
    CodeLim(1) = HUGE(i)
    CodeLim(2) = 20000
    Codelim(3) = 25000
    CodeLim(4) = 100
    CodeLim(5) = 25
    CodeLim(6) = 40
    string2 = AddNull( 'MaxMet1D' )
    irv =sysGetProfileInt(string1,string2,CodeLim(1),CodeLim(1),string4)
    string2 = AddNull( 'MaxPuff' )
    irv =sysGetProfileInt(string1,string2,CodeLim(2),CodeLim(2),string4)
    string2 = AddNull( 'MaxGrid' )
    irv =sysGetProfileInt(string1,string2,CodeLim(3),CodeLim(3),string4)
    string2 = AddNull( 'MaxRelease' )
    irv =sysGetProfileInt(string1,string2,CodeLim(4),CodeLim(4),string4)
    string2 = AddNull( 'MaxMaterial' )
    irv =sysGetProfileInt(string1,string2,CodeLim(6),CodeLim(6),string4)
END SELECT

!     Set SCIPUFF Logical units

lun_prj = 10
lun_err = 12
lun_inp = 13
lun_msc = 14
lun_scn = 15
lun_pal = 16
lun_puf = 17
lun_dep = 18
lun_dos = 19
lun_tmp = 21
lun_sfc = 22
lun_met = 23
lun_log = 24
lun_mcw = 25
lun_ter = 26
lun_smp = 27
lun_dmp = 33
lun_clog = 34
lun_sps = 35
lun_amr = 36
lun_dgn = 37
lun_asmp = 38
lun_ados = 39

!==== Modify the Currency Section

string1 = AddNull( 'Currency' )

9999 CONTINUE

RETURN

9998  CONTINUE
CALL SetError( IV_ERROR, &
              'Failure from system function : '//TRIM(string1)//':'//TRIM(string2), &
               TRIM(string4),' ', &
              'sysGetProfileString' )
GOTO 9999
END
!*******************************************************************************
!                SetFileNames
!*******************************************************************************
SUBROUTINE SetFileNames( basename,path )

USE files_fi

IMPLICIT NONE

CHARACTER(*)   basename !BASE file name
CHARACTER(*)   path !PATH name
CHARACTER(PATH_MAXLENGTH) tmpPath
INTEGER        last

!---- Set Project File names - Subsequent calls

tmpPath = TRIM(path)
last = LEN_TRIM(tmpPath)
IF( tmpPath(last:last) /= '\' )tmpPath(last+1:last+1) = '\'

path_tmp(1:) = TRIM(tmpPath)
file_dep(1:) = TRIM(tmpPath)//TRIM(basename)//'.dep'
file_dmp(1:) = TRIM(tmpPath)//TRIM(basename)//'.dmp'
file_dos(1:) = TRIM(tmpPath)//TRIM(basename)//'.dos'
file_err(1:) = TRIM(tmpPath)//TRIM(basename)//'.err'
file_inp(1:) = TRIM(tmpPath)//TRIM(basename)//'.inp'
file_msc(1:) = TRIM(tmpPath)//TRIM(basename)//'.msc'
file_prj(1:) = TRIM(tmpPath)//TRIM(basename)//'.prj'
file_puf(1:) = TRIM(tmpPath)//TRIM(basename)//'.puf'
file_scn(1:) = TRIM(tmpPath)//TRIM(basename)//'.scn'
file_sfc(1:) = TRIM(tmpPath)//TRIM(basename)//'.sfo'
file_log(1:) = TRIM(tmpPath)//TRIM(basename)//'.log'
file_mcw(1:) = TRIM(tmpPath)//TRIM(basename)//'.mcw'
file_smp(1:) = TRIM(tmpPath)//TRIM(basename)//'.smp'
file_sps(1:) = TRIM(tmpPath)//TRIM(basename)//'.sps'
file_clog(1:) = TRIM(tmpPath)//TRIM(basename)//'.clog'

RETURN
END
!*******************************************************************************
!                CheckFile
!*******************************************************************************
LOGICAL FUNCTION CheckFile( file )

USE tooluser_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*) file

CHARACTER(128) eString

LOGICAL, EXTERNAL :: CheckPath

INTEGER sysCheckFile,irv

CheckFile = .FALSE.

1000 CONTINUE

irv = sysCheckFile(file)
IF( irv == SCIPfailure )THEN
  CALL ReportFileName( eString,'File=',file )
  CALL SetError( NF_ERROR, &
                'File not Found', &
                 eString, &
                'Check file and/or INI file definitions', &
                'CheckFile' )
ELSE IF( irv == SCIPnull )THEN
  IF( CheckPath(file) )GOTO 1000
ELSE
  CheckFile = .TRUE.
END IF

RETURN
END
!*******************************************************************************
!                CheckFile_NoError
!*******************************************************************************
LOGICAL FUNCTION CheckFile_NoError( file )

IMPLICIT NONE

CHARACTER(*) file

INQUIRE(FILE=file,EXIST=CheckFile_NoError)

RETURN
END
!*******************************************************************************
!                CheckPath
!*******************************************************************************
LOGICAL FUNCTION CheckPath( path )

USE tooluser_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*) path

CHARACTER(128) eString

INTEGER irv
LOGICAL CheckDrive
INTEGER sysCheckPath

CheckPath = .FALSE.

irv = sysCheckPath( path )
IF( irv == SCIPfailure )THEN
  CheckPath = CheckDrive( path )
ELSE IF( irv == SCIPnull )THEN
  CheckPath = .FALSE.
ELSE
  CheckPath = .TRUE.
END IF

IF( .NOT.CheckPath )THEN
  CALL ReportFileName( eString,'Path=',path )
  CALL SetError( NF_ERROR, &
                'Path not Found', &
                 eString, &
                'Check path and/or INI file definitions', &
                'CheckPath' )
END IF

RETURN
END
!*******************************************************************************
!                CheckDrive
!*******************************************************************************
LOGICAL FUNCTION CheckDrive( file )

USE tooluser_fd
USE errorParam_fd

IMPLICIT NONE

CHARACTER(*) file

INTEGER, PARAMETER :: DRIVE_REMOTE = 4
INTEGER, PARAMETER :: DRIVE_CDROM  = 5

INTEGER sysDriveType,drive,irv
INTEGER sysCheckDrive

CHARACTER(PATH_MAXLENGTH) check,AddNull

LOGICAL, EXTERNAL :: hasError

IF( file(2:2) == ':' )THEN
  check = AddNull( file(1:2) )
ELSE
  check = AddNull( TRIM(file) )
END IF

drive = sysDriveType( check )

1000 CONTINUE

CheckDrive   = .FALSE.

IF( drive == DRIVE_CDROM )THEN
  CALL SetError( WN_ERROR, &
                'CDROM path not found : '//TRIM(file), &
                'Please verify that the correct CD is in the CDROM drive', &
                'Click YES to Retry : NO to Ignore', &
                'CheckDrive' )
  CALL ShowWarningMessage( 0,.FALSE. )
  CheckDrive = .NOT.hasError()
ELSE IF( drive == DRIVE_REMOTE )THEN
  CALL SetError( WN_ERROR, &
                'Remote(Network) path not found : '//TRIM(file), &
                'Please verify that you are connected to the Network Drive', &
                'Click YES to Retry : NO to Ignore', &
                'CheckDrive' )
  CALL ShowWarningMessage( 0,.FALSE. )
  CheckDrive = .NOT.hasError()
END IF

IF( CheckDrive )THEN
  irv = sysCheckDrive(file) == SCIPsuccess
  IF( irv == SCIPfailure )GOTO 1000
END IF

RETURN
END
!*******************************************************************************
!                SetFullPath
!*******************************************************************************
SUBROUTINE SetFullPath( filename )

USE errorParam_fd
USE SwinAPI
USE DefSize_fd

IMPLICIT NONE

CHARACTER(*) filename

CHARACTER(PATH_MAXLENGTH) current,check,file,path,StripNull

INTEGER irv
LOGICAL, EXTERNAL :: hasError

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

IF( hasError() )RETURN

nError = NO_ERROR

irv = GetCurrentDirectory( LEN(current),current )
IF( irv > LEN(current) )THEN
  nError = SZ_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'String too small to get current directory'
  WRITE(eInform,'(A,2I4)')'Current/Needed sizes = ',LEN(current), irv
  eAction  = 'Call for Technical Assistance'
  GOTO 9999
ELSE IF( irv == 0 )THEN
  irv = GetLastError()
  nError = UK_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'Unable to get current directory'
  WRITE(eInform,'(A,I4)')'Last Error = ',irv
  eAction  = 'Call for Technical Assistance'
  GOTO 9999
END IF
current(irv+1:) = ' '

CALL SplitName( filename,file,path )
IF( '<>'//TRIM(path) == '<>' )path = TRIM(current)

check = TRIM(path)//CHAR(0)
irv = SetCurrentDirectory( check )
IF( irv == FALSE )THEN
  irv = GetLastError()
  nError = NF_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'Full directory Not Found : '//TRIM(path)
  WRITE(eInform,'(A,I4)')'Last Error = ',irv
  eAction = 'Check path'
END IF

irv = GetCurrentDirectory( LEN(path),path )
IF( irv > LEN(path) )THEN
  nError = SZ_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'String too small to get full directory'
  WRITE(eInform,'(A,2I4)')'Current/Needed sizes = ',LEN(path),irv
  eAction  = 'Call for Technical Assistance'
  GOTO 9999
ELSE IF( irv == 0 )THEN
  irv = GetLastError()
  nError = UK_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'Unable to get full directory'
  WRITE(eInform,'(A,I4)')'Last Error = ',irv
  eAction = 'Call for Technical Assistance'
  GOTO 9999
END IF

filename = TRIM(StripNull(path))//'\'//TRIM(file)

check = TRIM(current)//CHAR(0)
irv = SetCurrentDirectory( check )
IF( irv == FALSE )THEN
  irv = GetLastError()
  nError = NF_ERROR
  eRoutine = 'SetFullPath'
  eMessage = 'Current directory Not Found : '//TRIM(current)
  WRITE(eInform,'(A,I4)')'Last Error = ',irv
  eAction = 'Check path'
END IF

9999 CONTINUE

IF( nError /= NO_ERROR )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )

RETURN
END
!*******************************************************************************
!                ChangePath
!*******************************************************************************
SUBROUTINE ChangePath( path )

USE SwinAPI
USE errorParam_fd
USE DefSize_fd

IMPLICIT NONE

CHARACTER(*) path

INTEGER irv

CHARACTER(PATH_MAXLENGTH) check

LOGICAL, EXTERNAL :: hasError

IF( hasError() )RETURN

check = TRIM(path)//CHAR(0)
irv   = SetCurrentDirectory( check )
IF( irv == FALSE )THEN
  irv = GetLastError()
  WRITE(check,'(A,I4)')'Last Error = ',irv
  CALL SetError( NF_ERROR, &
                'Directory Not Found : '//TRIM(path), &
                 check, &
                'Check path', &
                'ChangePath' )
END IF

RETURN
END
