!*******************************************************************************
!            Initialize Tool
!*******************************************************************************
INTEGER FUNCTION SimpleInitTool( UserID,UserCall,request,cstr_INIfile )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIMgr_fi
USE matl_fi
USE param_fd
USE SCIMgrState
USE InitTool_fi
USE AdjointFilter_fi

IMPLICIT NONE

INTEGER, PARAMETER :: CURRENT_LAND_USE_VERSION = 200
INTEGER, PARAMETER :: CURRENT_SYSTOOL_VERSION  = 200
INTEGER, PARAMETER :: CURRENT_SWIM_VERSION     = 1400
INTEGER, PARAMETER :: CLOCK_UPDATE = 1000
INTEGER, PARAMETER :: CLOCK_DELAY  = 200
REAL,    PARAMETER :: DEFAULT_CASUALTY_CUTOFF = 0.0  ![0-100] percent

INTEGER,              INTENT( IN    ) :: UserID
INTEGER(LEN_ADDRESS), INTENT( IN    ) :: UserCall
INTEGER,              INTENT( INOUT ) :: request
TYPE( fileNameT ),    INTENT( IN    ) :: cstr_INIfile

INTEGER initValue
INTEGER landuseVersion, sysVersion
INTEGER irv

CHARACTER(PATH_MAXLENGTH) section,string,defstr

TYPE( char128T ) charStruct

TYPE( messageT ) error

#if defined(__INTEL_COMPILER)
INTEGER,              EXTERNAL :: InternalCallBack
#endif
INTEGER,              EXTERNAL :: InitMessageHandler
INTEGER(LEN_ADDRESS), EXTERNAL :: GetMessageHandler
INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
LOGICAL,              EXTERNAL :: CheckPathT

LOGICAL, EXTERNAL :: InitDatums
INTEGER, EXTERNAL :: GetVersionString

INTEGER, EXTERNAL :: sysCurrentDirectory,sysGetProfileInt
INTEGER, EXTERNAL :: sysDeleteFile,sysGetLastError,sysWriteProfileString
INTEGER, EXTERNAL :: sysGetProfileString

INTEGER, EXTERNAL :: GetLandUseVersion, GetLandUsePathLength
INTEGER, EXTERNAL :: sysGetVersion, sysGetPathLength

!==== Initialize tool state and error

CALL SCIMgrInitState()
CALL init_error()
clockUpdate = CLOCK_UPDATE
clockdelay  = CLOCK_DELAY

!==== Initialize return

irv = SCIMgrSetState( HS_IDLE )

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available but shouldn't be called
  irv = SCIMgrSetState( HS_BUSY )
END IF

CALL ExceptionTest()

SimpleInitTool = SCIPfailure

#if !defined(__INTEL_COMPILER)
SCIPCallBack = UserCall
#else
ExternalCallBack = UserCall
SCIPCallBack = ADDRESSOF(InternalCallBack)
#endif
IF( GetMessageHandler() == 0 )THEN
  irv = InitMessageHandler( SCIPCallBack )
  IF( irv == SCIPfailure )THEN
    CALL GetMessageHandlerError( error )
    nError   = error%iParm
    eRoutine = 'InitTool'
    eMessage = TRIM(error%aString)
    eInform  = TRIM(error%bString)
    GOTO 9999
  END IF
END IF

CALL set_messaging( userID )

SCIPinProgress = .FALSE.

!==== Initialize file unit numbers

INIfile = cstr_INIfile%string
CALL SetFileUnitsT()


!==== Initialize dll

LastError = 0

!==== Get Default Bin path - same as INIfile path

CALL SplitName( INIfile,string,binpath )

!==== Check INI file for SCIPbin, SCIPdata and SCIPTempDir entries
! Default Directories
!  SCIPBinDir  = INI file path
!  SCIPDataDir = SCIPBinDir\..\data
!  SCIPTempDir = SCIPDataDir\temp
!  SciDataDir  = SCIPDataDir\scidata

section = 'Paths'
defstr  = ' '
string  = 'SCIPBinDir'
irv = sysGetProfileString( section,string,defstr,datapath,INIfile )
IF( irv == SCIPnull )THEN
  string  ='scimaindir'
  irv = sysGetProfileString( section,string,defstr,datapath,INIfile )
  IF( irv == SCIPnull )THEN
    datapath = binpath
  END IF
END IF
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF
binpath = datapath

string = binpath
CALL BackUpPath( string,1 )
defstr = 'data'
CALL AddPath( defstr,string )
string  ='SCIPDataDir'
irv = sysGetProfileString( section,string,defstr,datapath,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

defstr = 'temp'
CALL AddPath( defstr,datapath )
string = 'SCIPTempDir'
irv = sysGetProfileString( section,string,defstr,temppath,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

defstr = 'Scidata'
CALL AddPath( defstr,datapath )
string = 'SciDataDir'
irv = sysGetProfileString( section,string,defstr,scipath,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

!==== Find Landuse file

section = 'Land_Use'
defstr  = 'landuse.dat'
CALL AddPath( defstr,scipath )
string = 'LandUseDataFile'
irv = sysGetProfileString( section,string,defstr,file_lus,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

!==== Check LandUse Currency

landuseVersion = GetLandUseVersion()

IF( landuseVersion/100 /= CURRENT_LAND_USE_VERSION/100 )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incorrect version of the LandUse dll detected'
  WRITE(eInform,*)'Version=',landuseVersion, &
                 ' Expected=',CURRENT_LAND_USE_VERSION
  GOTO 9999
END IF

IF( GetLandUsePathLength() /= PATH_MAXLENGTH )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incompatible path length in the LandUse dll'
  WRITE(eInform,*)'LandUse=',GetLandUsePathLength(), &
                 ' Expected=',PATH_MAXLENGTH
  GOTO 9999
END IF

!==== Check sysTool Currency

sysVersion = sysGetVersion()
IF( sysVersion/100 /= CURRENT_SYSTOOL_VERSION/100 )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incorrect version of the sysTool dll detected'
  WRITE(eInform,*)'Version=',sysVersion, &
                 ' Expected=',CURRENT_SYSTOOL_VERSION
  GOTO 9999
END IF

IF( sysGetPathLength() /= PATH_MAXLENGTH )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incompatible path length in the sysTool dll'
  WRITE(eInform,*)'sysTool=',sysGetPathLength(), &
                 ' Expected=',PATH_MAXLENGTH
  GOTO 9999
END IF

!==== Get Clock update parameters

section = 'SCIPtool'
string  = 'ClockDelay'
irv = sysGetProfileInt( section,string,CLOCK_DELAY,clockDelay,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Failure reading clock delay parameter from INI file'
  eInform  = 'INI file='//TRIM(INIfile)
  GOTO 9999
END IF

string = 'ClockUpdate'
irv = sysGetProfileInt( section,string,CLOCK_UPDATE,clockUpdate,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Failure reading clock update parameter from INI file'
  eInform  = 'INI file='//TRIM(INIfile)
  GOTO 9999
END IF

!==== Get Adjoint filter target hit number - default is 30

section = 'SourceEstimation'
string  = 'MaxAdjointHits'
irv = sysGetProfileInt( section,string,30,nhit_target,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Failure reading adjoint filter target hit number from INI file'
  eInform  = 'INI file='//TRIM(INIfile)
  GOTO 9999
END IF

!==== Nullify material auxiliary array pointer

NULLIFY( mat_aux )

!==== Initialize return value

initValue = SCIPnull

!==== Init UTM

IF( BTEST(request,HIB_UTM) )THEN
  request = IBCLR(request,HIB_UTM)
  IF( InitDatums(INIfile) )THEN
    initValue = IBSET(initValue,HIB_UTM)
  ELSE
    initValue = IBCLR(initValue,HIB_UTM)
  END IF
ELSE
  initValue = IBCLR(initValue,HIB_UTM)
END IF

AvailUTM = BTEST(initValue,HIB_UTM)

!==== Update Currency

section = 'Currency'
string  = 'SCIPtoolVersion'
irv = GetVersionString( 1,charStruct )
irv = sysWriteProfileString( section,string,charStruct%string,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysWriteProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

section = 'Currency'
string  = 'SCIPUFF_Version'
irv = GetVersionString( 2,charStruct )
irv = sysWriteProfileString( section,string,charStruct%string,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysWriteProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

section = 'Currency'
string  = 'LandUse_Version'
CALL WriteVersionString( landuseVersion,defstr )
irv = sysWriteProfileString( section,string,defstr,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysWriteProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

section = 'Currency'
string  = 'SYStool_Version'
CALL WriteVersionString( sysVersion,defstr )
irv = sysWriteProfileString( section,string,defstr,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysWriteProfileString returned an error'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF

!==== return

SimpleInitTool = initValue

9999 CONTINUE

irv = SCIMgrSetState( HS_IDLE )

CALL reset_messaging()

RETURN
END
