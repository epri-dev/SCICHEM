INTEGER FUNCTION SWIMInit( request,reply,INIfile )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMInit

USE SWIMparam_fd
USE SWIM_fi
USE SCIMgrparam_fd
USE charT_fd

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: request
INTEGER,      INTENT( IN ) :: reply
CHARACTER(*), INTENT( IN ) :: INIfile

INTEGER irv, iversion, i, j

CHARACTER(128) section, string, version
CHARACTER(128) defstr
CHARACTER(PATH_MAXLENGTH) pathstr
INTEGER, EXTERNAL :: sysWriteProfileString, SWIMVersion
INTEGER, EXTERNAL :: sysGetProfileString
INTEGER, EXTERNAL :: sysCheckFile

!------ Init SWIM

SWIMInit           = SWIMfailure
SWIMinProgress     = .FALSE.
SWIMiteration      = .FALSE.
SWIMresult         = SWIMsuccess
SWIMmessageEnabled = .FALSE.

CALL SWIMclearError()

!------ Write SWIM version to INI file

irv = SWIMVersion( iversion )

section = 'Currency'
string  = 'SWIM_Version'

WRITE(version,FMT=*) iversion/1000.
version = ADJUSTL(version)

i = LEN(TRIM(version))
j = MAX(1,(i-1))

DO WHILE( i > 0 .AND. version(i:i) == '0' .AND. version(j:j) /= '.' )
  version(i:i) =' '
  i = i - 1
  j = MAX(1,(i-1))
END DO

IF( version(1:1) == '.' )THEN
  string ='0'//TRIM(version)
END IF

irv = sysWriteProfileString( section,string,version,INIfile )
IF( irv == SCIPfailure )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMInit'
  error%Message = 'Error writing SWIM version to INI file'
  GOTO 9999
END IF

!------ Check for surface met station file

lISDavail      = .FALSE.
ISDhistoryFile = ''

section = 'Paths'
string  = 'SciDataDir'
defstr  = ''
irv = sysGetProfileString( section,string,defstr,pathstr,INIfile )
IF( irv == SCIPsuccess )THEN
  ISDhistoryFile = 'isd-history.csv'
  CALL AddPATH( ISDhistoryFile,pathstr )
  irv = sysCheckFile( ISDhistoryFile )
  IF( irv == SCIPsuccess )THEN
    lISDavail = .TRUE.
  END IF
END IF


!------ Nullify log message list

NULLIFY(FirstLogMsg); NULLIFY(LogMsg)

numField    = 0
numFieldMax = 0

!------ SWIMming success

SWIMInit = SWIMresult

9999 CONTINUE

RETURN
END
