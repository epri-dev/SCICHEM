!*******************************************************************************
!            Default Project Input
!*******************************************************************************
INTEGER FUNCTION DefaultInput( UserID,iInput,tInput0,tInput1 )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: UserID
INTEGER,               INTENT( IN    ) :: iInput
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput0
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput1

INTEGER, PARAMETER :: SIZE_I = KIND(1)

INTEGER ReturnValue, irv, currentState, alloc_stat, n, m, mlst

TYPE( pctrlT     ) :: ctrlIO
TYPE( pstartT    ) :: startIO
TYPE( pendT      ) :: endIO
TYPE( pflagsT    ) :: flagsIO
TYPE( pspatialT  ) :: domainIO
TYPE( poptionsT  ) :: optionsIO
TYPE( pmaterialT ) :: materialIO
TYPE( pweatherT  ) :: weatherIO
TYPE( ptemporalT ) :: timeIO
TYPE( pinputT    ) :: inputIO

TYPE( materialT ), DIMENSION(:), ALLOCATABLE :: mtlListIO

INTEGER, EXTERNAL :: Default_Time
INTEGER, EXTERNAL :: Default_Material
INTEGER, EXTERNAL :: Default_Options
INTEGER, EXTERNAL :: Default_Domain
INTEGER, EXTERNAL :: Default_Flags
INTEGER, EXTERNAL :: Default_End
INTEGER, EXTERNAL :: Default_Start
INTEGER, EXTERNAL :: Default_Ctrl
INTEGER, EXTERNAL :: Default_Weather
INTEGER, EXTERNAL :: Default_Inp

!==== Initialize

DefaultInput = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

ReturnValue = SCIPfailure

!==== Initialize error

CALL ModuleInitError()

!==== Select Default routine

SELECT CASE( iInput )

  CASE( SCIP_CTRL )

    n = SIZE_pctrlT / SIZE_I
    ctrlIO = TRANSFER(tInput0(1:n),ctrlIO)

    ReturnValue = Default_Ctrl( ctrlIO%ctrl )

    tInput0(1:n) = TRANSFER(ctrlIO,tInput0(1:n))

  CASE( SCIP_START )

    n = SIZE_pstartT / SIZE_I
    startIO = TRANSFER(tInput0(1:n),startIO)

    ReturnValue = Default_Start( startIO%start )

    tInput0(1:n) = TRANSFER(startIO,tInput0(1:n))

  CASE( SCIP_END )

    n = SIZE_pendT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Default_End( endIO%end )

    tInput0(1:n) = TRANSFER(endIO,tInput0(1:n))

  CASE( SCIP_FLAGS )

    n = SIZE_pflagsT / SIZE_I
    flagsIO = TRANSFER(tInput0(1:n),flagsIO)

    ReturnValue = Default_Flags( flagsIO%flags )

    tInput0(1:n) = TRANSFER(flagsIO,tInput0(1:n))

  CASE( SCIP_DOMAIN )

    n = SIZE_pspatialT / SIZE_I
    domainIO = TRANSFER(tInput0(1:n),domainIO)

    ReturnValue = Default_Domain( domainIO%spatial )

    tInput0(1:n) = TRANSFER(domainIO,tInput0(1:n))

  CASE( SCIP_OPTIONS )

    n = SIZE_poptionsT / SIZE_I
    optionsIO = TRANSFER(tInput0(1:n),optionsIO)

    ReturnValue = Default_Options( optionsIO%option )

    n = SIZE_poptionsT / SIZE_I
    tInput0(1:n) = TRANSFER(optionsIO,tInput0(1:n))

  CASE( SCIP_MATERIAL )

    n = SIZE_pmaterialT / SIZE_I
    materialIO = TRANSFER(tInput0(1:n),materialIO)

    m = MAX(materialIO%mtlHead%number,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'DefaultInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( materialIO%mtlHead%number > 0 )THEN
      mlst = materialIO%mtlHead%number
      m    = mlst*SIZE_materialT / SIZE_I
      mtlListIO(1:mlst) = TRANSFER(tInput1(1:m),mtlListIO(1:mlst))
    END IF

    ReturnValue = Default_Material( materialIO%mtlHead,mtlListIO )

    tInput0(1:n) = TRANSFER(materialIO,tInput0(1:n))
    IF( materialIO%mtlHead%number > 0 )THEN
      mlst = materialIO%mtlHead%number
      m    = mlst*SIZE_materialT / SIZE_I
      tInput1(1:m) = TRANSFER(mtlListIO(1:mlst),tInput1(1:m))
    END IF
    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_RELEASE )

  CASE( SCIP_WEATHER )

    n = SIZE_pweatherT / SIZE_I
    weatherIO = TRANSFER(tInput0(1:n),weatherIO)

    ReturnValue = Default_Weather( weatherIO )

    tInput0(1:n) = TRANSFER(weatherIO,tInput0(1:n))

  CASE( SCIP_TIME )

    n = SIZE_ptemporalT / SIZE_I
    timeIO = TRANSFER(tInput0(1:n),timeIO)

    ReturnValue = Default_Time( timeIO%time )

    tInput0(1:n) = TRANSFER(timeIO,tInput0(1:n))

  CASE( SCIP_INPUT )

    n = SIZE_pinputT / SIZE_I
    inputIO = TRANSFER(tInput0(1:n),inputIO)

    m = MAX(inputIO%input%mtlHead%number,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'DefaultInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( inputIO%input%mtlHead%number > 0 )THEN
      mlst = inputIO%input%mtlHead%number
      m    = mlst*SIZE_materialT / SIZE_I
      mtlListIO(1:mlst) = TRANSFER(tInput1(1:m),mtlListIO(1:mlst))
    END IF

    ReturnValue = Default_Inp( inputIO,mtlListIO )

    tInput0(1:n) = TRANSFER(inputIO,tInput0(1:n))
    IF( inputIO%input%mtlHead%number > 0 )THEN
      mlst = inputIO%input%mtlHead%number
      m    = mlst*SIZE_materialT / SIZE_I
      tInput1(1:m) = TRANSFER(mtlListIO(1:mlst),tInput1(1:m))
    END IF

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_COMPLETE )

  CASE DEFAULT

END SELECT

9999 CONTINUE

DefaultInput = ReturnValue

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Ctrl
!*******************************************************************************
INTEGER FUNCTION DefaultCtrlF( UserID,ctrlIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,        INTENT( IN    ) :: UserID
TYPE( pctrlT ), INTENT( INOUT ) :: ctrlIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Ctrl

!==== Initialize

DefaultCtrlF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultCtrlF = Default_Ctrl( ctrlIO%ctrl )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Start
!*******************************************************************************
INTEGER FUNCTION DefaultStartF( UserID,startIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pstartT ), INTENT( INOUT ) :: startIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Start

!==== Initialize

DefaultStartF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultStartF = Default_Start( startIO%start )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project End
!*******************************************************************************
INTEGER FUNCTION DefaultEndF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_End

!==== Initialize

DefaultEndF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultEndF = Default_End( endIO%end )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Flags
!*******************************************************************************
INTEGER FUNCTION DefaultFlagsF( UserID,flagsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pflagsT ), INTENT( INOUT ) :: flagsIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Flags

!==== Initialize

DefaultFlagsF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultFlagsF = Default_Flags( flagsIO%flags )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Domain
!*******************************************************************************
INTEGER FUNCTION DefaultDomainF( UserID,domainIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pspatialT ), INTENT( INOUT ) :: domainIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Domain

!==== Initialize

DefaultDomainF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultDomainF = Default_Domain( domainIO%spatial )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Options
!*******************************************************************************
INTEGER FUNCTION DefaultOptionsF( UserID,optionsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( poptionsT ), INTENT( INOUT ) :: optionsIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Options

!==== Initialize

DefaultOptionsF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultOptionsF = Default_Options( optionsIO%option )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Material
!*******************************************************************************
INTEGER FUNCTION DefaultMaterialF( UserID,materialIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( pmaterialT ),              INTENT( INOUT ) :: materialIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Material

!==== Initialize

DefaultMaterialF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultMaterialF = Default_Material( materialIO%mtlHead,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Weather
!*******************************************************************************
INTEGER FUNCTION DefaultWeatherF( UserID,weatherIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pweatherT ), INTENT( INOUT ) :: weatherIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Weather

!==== Initialize

DefaultWeatherF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultWeatherF = Default_Weather( weatherIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Time
!*******************************************************************************
INTEGER FUNCTION DefaultTimeF( UserID,timeIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: UserID
TYPE( ptemporalT ), INTENT( INOUT ) :: timeIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Time

!==== Initialize

DefaultTimeF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultTimeF = Default_Time( timeIO%time )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Default Project Input
!*******************************************************************************
INTEGER FUNCTION DefaultInpF( UserID,inputIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( pinputT ),                 INTENT( INOUT ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Default_Inp

!==== Initialize

DefaultInpF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Default routine

DefaultInpF = Default_Inp( inputIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END

