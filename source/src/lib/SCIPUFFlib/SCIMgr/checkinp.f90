!*******************************************************************************
!            Check Project Input
!*******************************************************************************
INTEGER FUNCTION CheckInput( UserID,iInput,tInput0,tInput1 )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: UserID
INTEGER,               INTENT( IN ) :: iInput
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput0
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput1

INTEGER, PARAMETER :: SIZE_I = KIND(1)

INTEGER ReturnValue, currentState, irv, alloc_stat, n, m, mlst

TYPE( startT    ) :: startIO
TYPE( endT      ) :: endIO
TYPE( spatialT  ) :: domainIO
TYPE( optionsT  ) :: optionsIO
TYPE( materialT ) :: materialIO
TYPE( releaseT  ) :: releaseIO
TYPE( weatherT  ) :: weatherIO
TYPE( temporalT ) :: timeIO
TYPE( inputT    ) :: inputIO

TYPE( materialT  ), DIMENSION(:), ALLOCATABLE :: mtlListIO

INTEGER, EXTERNAL :: Check_Time
INTEGER, EXTERNAL :: Check_Inp
INTEGER, EXTERNAL :: Check_Weather
INTEGER, EXTERNAL :: Check_Release
INTEGER, EXTERNAL :: Check_Material
INTEGER, EXTERNAL :: Check_Options
INTEGER, EXTERNAL :: Check_Domain
INTEGER, EXTERNAL :: Check_End
INTEGER, EXTERNAL :: Check_Start

!==== Initialize

CheckInput = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

ReturnValue = SCIPfailure

!==== Initialize error

CALL ModuleInitError()

!==== Select Check routine

SELECT CASE( iInput )

  CASE( SCIP_START )

    n = SIZE_startT / SIZE_I
    startIO = TRANSFER(tInput0(1:n),startIO)

    ReturnValue = Check_Start( startIO )

  CASE( SCIP_END )

    n = SIZE_endT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Check_End( endIO )

  CASE( SCIP_FLAGS )

    ReturnValue = SCIPsuccess

  CASE( SCIP_DOMAIN )

    n = SIZE_spatialT / SIZE_I
    domainIO = TRANSFER(tInput0(1:n),domainIO)

    ReturnValue = Check_Domain( domainIO )

  CASE( SCIP_OPTIONS )

    n = SIZE_optionsT / SIZE_I
    optionsIO = TRANSFER(tInput0(1:n),optionsIO)

    ReturnValue = Check_Options( optionsIO )

  CASE( SCIP_MATERIAL )

    n = SIZE_materialT / SIZE_I
    materialIO = TRANSFER(tInput0(1:n),materialIO)

    ReturnValue = Check_Material( materialIO,.FALSE. )

  CASE( SCIP_RELEASE )

    n = SIZE_releaseT / SIZE_I
    releaseIO = TRANSFER(tInput0(1:n),releaseIO)

    n = SIZE_materialT / SIZE_I
    materialIO = TRANSFER(tInput1(1:n),materialIO)

    ReturnValue = Check_Release( releaseIO,materialIO )

  CASE( SCIP_WEATHER )

    n = SIZE_weatherT / SIZE_I
    weatherIO = TRANSFER(tInput0(1:n),weatherIO)

    ReturnValue = Check_Weather( weatherIO )

  CASE( SCIP_RUN )

    n = SIZE_endT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Check_End( endIO )

  CASE( SCIP_TIME )

    n = SIZE_temporalT / SIZE_I
    timeIO = TRANSFER(tInput0(1:n),timeIO)

    ReturnValue = Check_Time( timeIO )

  CASE( SCIP_INPUT )

    n = SIZE_inputT / SIZE_I
    inputIO = TRANSFER(tInput0(1:n),inputIO)

    m = MAX(inputIO%mtlHead%number,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'CheckInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( inputIO%mtlHead%number > 0 )THEN
      mlst = inputIO%mtlHead%number
      m    = mlst*SIZE_materialT / SIZE_I
      mtlListIO(1:mlst) = TRANSFER(tInput1(1:m),mtlListIO(1:mlst))
    END IF

    ReturnValue = Check_Inp( inputIO,mtlListIO )

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_COMPLETE )

  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckInput'
    eMessage = 'Invalid check request'
    WRITE(eInform,*,IOSTAT=irv)'Request =',iInput

END SELECT

9999 CONTINUE

CheckInput = ReturnValue

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Start Time
!*******************************************************************************
INTEGER FUNCTION CheckStartF( UserID,startIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,        INTENT( IN ) :: UserID
TYPE( startT ), INTENT( IN ) :: startIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Start

!==== Initialize

CheckStartF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckStartF = Check_Start( startIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check ProjectDomain
!*******************************************************************************
INTEGER FUNCTION CheckDomainF( UserID,domainIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: UserID
TYPE( spatialT ), INTENT( INOUT ) :: domainIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Domain

!==== Initialize

CheckDomainF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckDomainF = Check_Domain( domainIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Options
!*******************************************************************************
INTEGER FUNCTION CheckOptionsF( UserID,optionsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: UserID
TYPE( optionsT ), INTENT( IN ) :: optionsIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Options

!==== Initialize

CheckOptionsF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckOptionsF = Check_Options( optionsIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Material
!*******************************************************************************
INTEGER FUNCTION CheckMaterialF( UserID,materialIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( materialT ), INTENT( IN ) :: materialIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Material

!==== Initialize

CheckMaterialF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckMaterialF = Check_Material( materialIO,.FALSE. )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Release
!*******************************************************************************
INTEGER FUNCTION CheckReleaseF( UserID,releaseIO,materialIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( releaseT ),  INTENT( IN ) :: releaseIO
TYPE( materialT ), INTENT( IN ) :: materialIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Release

!==== Initialize

CheckReleaseF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckReleaseF = Check_Release( releaseIO,materialIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Weather
!*******************************************************************************
INTEGER FUNCTION CheckWeatherF( UserID,weatherIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: UserID
TYPE( weatherT ), INTENT( IN ) :: weatherIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Weather

!==== Initialize

CheckWeatherF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckWeatherF = Check_Weather( weatherIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project End Time
!*******************************************************************************
INTEGER FUNCTION CheckEndF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: UserID
TYPE( endT ), INTENT( IN ) :: endIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_End

!==== Initialize

CheckEndF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckEndF = Check_End( endIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Time
!*******************************************************************************
INTEGER FUNCTION CheckTimeF( UserID,timeIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( temporalT ), INTENT( IN ) :: timeIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Time

!==== Initialize

CheckTimeF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckTimeF = Check_Time( timeIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Check Project Input
!*******************************************************************************
INTEGER FUNCTION CheckInpF( UserID,inputIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( inputT ),                  INTENT( INOUT ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Check_Inp

!==== Initialize

CheckInpF = SCIPfailure

IF( SCIMgrCheckState(HS_ANYSTATE) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Check routine

CheckInpF = Check_Inp( inputIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END

