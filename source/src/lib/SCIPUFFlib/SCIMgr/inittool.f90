!*******************************************************************************
!            Initialize Tool
!*******************************************************************************
INTEGER FUNCTION InitTool( UserID,UserCall,request,limit,cstr_INIfile )

USE SCIMgr_fd
USE SWIMparam_fd
USE files_fi
USE error_fi
USE SCIMgr_fi
USE plotlist_fi, ONLY:MemoryField, UsePlotCN2
USE metparam_fd
USE puff_fi
USE matl_fi
USE surface_fi
USE param_fd
USE search_fd
USE SCIMgrState
USE InitTool_fi
USE AdjointFilter_fi
USE omp_lib

IMPLICIT NONE

INTEGER, PARAMETER :: CURRENT_SWIM_VERSION     = 1400

INTEGER,              INTENT( IN    ) :: UserID
INTEGER(LEN_ADDRESS), INTENT( IN    ) :: UserCall
INTEGER,              INTENT( INOUT ) :: request
TYPE( limitT ),       INTENT( IN    ) :: limit
TYPE( fileNameT ),    INTENT( IN    ) :: cstr_INIfile

INTEGER ios, initValue
INTEGER versionSWIM
INTEGER irv

CHARACTER(PATH_MAXLENGTH) section,string,defstr
CHARACTER(PATH_MAXLENGTH) rtnstr


INTEGER,              EXTERNAL :: SWIMInit
LOGICAL,              EXTERNAL :: CheckPathT

INTEGER, EXTERNAL :: InitPlotTool

INTEGER, EXTERNAL :: SAG_InitList

INTEGER, EXTERNAL :: sysGetProfileInt
INTEGER, EXTERNAL :: sysGetProfileString

INTEGER, EXTERNAL :: SWIMversion, SWIMPathLength
INTEGER, EXTERNAL :: SimpleInitTool

!==== Do Simple Initialize

InitTool = SCIPfailure

initValue = SimpleInitTool( UserID,UserCall,request,cstr_INIfile )  !InitValue has bit values initialized by SimpleInitTool, request has remaining values
IF( initValue == SCIPfailure )GOTO 9999

!====

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN !Always available but shouldn't be called
  irv = SCIMgrSetState( HS_BUSY )
END IF

CALL set_messaging( userID )

MemoryField = .FALSE.


!==== Initialize plotting

irv = InitPlotTool()
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'InitPlotTool error'
  GOTO 9999
END IF

!==== Set number of threads

nthreads = 1
#if defined(_OPENMP)
section = 'SCIPMode'
string  = 'NumThreads'
irv = sysGetProfileInt( section,string,1,nthreads,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Failure reading number of OpenMP threads from INI file'
  eInform  = 'INI file='//TRIM(INIfile)
  GOTO 9999
END IF

#endif
CALL OMP_SET_NUM_THREADS( nthreads )

!------ Set flags for single/multi-thread

multiThread  = nthreads > 1
singleThread = .NOT.multiThread

CALL InitOMPError()

!==== Initialize dll


!==== Size parameters

MAX1D_MET = limit%met1D
MAXPUF    = limit%puffs
MAXSG     = limit%surfaceGrid

IF( MAX1D_MET <= 0 )MAX1D_MET = HUGE(ios)
IF( MAXPUF    <= 0 )MAXPUF    = MAXPUF_DEF
IF( MAXSG     <= 0 )MAXSG     = MAXSG_DEF

!==== Read flag for CN2 processing

section = 'SCIPmode'
string  = 'Plot2Gauss'
defstr  = ' '
rtnstr  = defstr
irv = sysGetProfileString( section,string,defstr,rtnstr,INIfile )
IF( irv == SCIPfailure )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'sysGetProfileString returned an error on retrieving the 2-Gaussian Plot string'
  WRITE(eInform,*)'Error =',irv
  GOTO 9999
END IF
IF( LEN_TRIM(rtnstr) == 0 )THEN
  UsePlotCN2 = .FALSE.
ELSE
  CALL cupper( rtnstr )
  IF( TRIM(rtnstr) == 'ON' )THEN
    UsePlotCN2 = .TRUE.
  ELSE
    UsePlotCN2 = .FALSE.
  END IF
END IF

!==== Check SWIM Currency

irv = SWIMversion( versionSWIM )

IF( versionSWIM/100 /= CURRENT_SWIM_VERSION/100 )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incorrect version of the SWIM dll detected'
  WRITE(eInform,*)'Version=',versionSWIM, &
                 ' Expected=',CURRENT_SWIM_VERSION
  GOTO 9999
END IF

IF( SWIMPathLength() /= PATH_MAXLENGTH )THEN
  nError   = IV_ERROR
  eRoutine = 'InitTool'
  eMessage = 'Incompatible path length in the SWIM dll'
  WRITE(eInform,*)'SWIM=',SWIMPathLength(), &
                 ' Expected=',PATH_MAXLENGTH
  GOTO 9999
END IF

!==== Init SWIM DLL

irv = SWIMInit( request,initValue,INIfile )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMInit' )
  GOTO 9999
END IF


!------ Initialize SAG linked list of grid structures

irv = SAG_InitList()

!------ Initialize inverse error function table

CALL init_erfci()

!==== return

InitTool = initValue

9999 CONTINUE

irv = SCIMgrSetState( HS_IDLE )

CALL reset_messaging()

RETURN
END
