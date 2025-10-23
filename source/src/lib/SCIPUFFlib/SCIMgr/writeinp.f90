!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION WriteInput( UserID,iInput,tInput0,tInput1 )

USE SCIMgr_fd
USE error_fi
USE statstruct_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: UserID
INTEGER,               INTENT( IN ) :: iInput
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput0
INTEGER, DIMENSION(*), INTENT( IN ) :: tInput1

INTEGER, PARAMETER :: SIZE_I = KIND(1)

INTEGER ReturnValue, currentState, irv, alloc_stat, n, m, mlst

TYPE( pctrlT     ) :: ctrlIO
TYPE( pstartT    ) :: startIO
TYPE( pendT      ) :: endIO
TYPE( pflagsT    ) :: flagsIO
TYPE( pspatialT  ) :: domainIO
TYPE( poptionsT  ) :: optionsIO
TYPE( pmaterialT ) :: materialIO
TYPE( preleaseT  ) :: releaseIO
TYPE( pweatherT  ) :: weatherIO
TYPE( pstatusT   ) :: statusIO
TYPE( ptemporalT ) :: timeIO
TYPE( pinputT    ) :: inputIO

TYPE( materialT  ), DIMENSION(:), ALLOCATABLE :: mtlListIO
TYPE( releaseT   ), DIMENSION(:), ALLOCATABLE :: relListIO
INTEGER,            DIMENSION(:), ALLOCATABLE :: statListIO

INTEGER, EXTERNAL :: Write_Time
INTEGER, EXTERNAL :: Write_Run
INTEGER, EXTERNAL :: Write_Inp
INTEGER, EXTERNAL :: Write_Rst
INTEGER, EXTERNAL :: Write_Release
INTEGER, EXTERNAL :: Write_Weather
INTEGER, EXTERNAL :: Write_Material
INTEGER, EXTERNAL :: Write_Options
INTEGER, EXTERNAL :: Write_Domain
INTEGER, EXTERNAL :: Write_Flags
INTEGER, EXTERNAL :: Write_End
INTEGER, EXTERNAL :: Write_Start
INTEGER, EXTERNAL :: Write_Ctrl

!==== Initialize

WriteInput = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

ReturnValue = SCIPsuccess

!==== Initialize error

CALL ModuleInitError()

CALL SetFileUnitsT()

!==== Select Write routine

SELECT CASE( iInput )

  CASE( SCIP_CTRL )

    n = SIZE_pctrlT / SIZE_I
    ctrlIO = TRANSFER(tInput0(1:n),ctrlIO)

    ReturnValue = Write_Ctrl( ctrlIO )

  CASE( SCIP_START )

    n = SIZE_pstartT / SIZE_I
    startIO = TRANSFER(tInput0(1:n),startIO)

    ReturnValue = Write_Start( startIO )

  CASE( SCIP_END )

    n = SIZE_pendT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Write_End( endIO )

  CASE( SCIP_FLAGS )

    n = SIZE_pflagsT / SIZE_I
    flagsIO = TRANSFER(tInput0(1:n),flagsIO)

    ReturnValue = Write_Flags( flagsIO )

  CASE( SCIP_DOMAIN )

    n = SIZE_pspatialT / SIZE_I
    domainIO = TRANSFER(tInput0(1:n),domainIO)

    ReturnValue = Write_Domain( domainIO )

  CASE( SCIP_OPTIONS )

    n = SIZE_poptionsT / SIZE_I
    optionsIO = TRANSFER(tInput0(1:n),optionsIO)

    ReturnValue = Write_Options( optionsIO )

  CASE( SCIP_MATERIAL )

    n = SIZE_pmaterialT / SIZE_I
    materialIO = TRANSFER(tInput0(1:n),materialIO)

    m = MAX(materialIO%mtlHead%number,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'WriteInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( materialIO%mtlHead%number > 0 )THEN
      mlst = materialIO%mtlHead%number
      m    = SIZE_materialT / SIZE_I
      DO irv = 1,mlst
        mtlListIO(irv) = TRANSFER(tInput1((irv-1)*m+1:irv*m),mtlListIO(irv))
      END DO
    END IF

    ReturnValue = Write_Material( materialIO,mtlListIO )

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_RELEASE )

    n = SIZE_preleaseT / SIZE_I
    releaseIO = TRANSFER(tInput0(1:n),releaseIO)

    m = MAX(releaseIO%scnHead%number,1)
    ALLOCATE( relListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'WriteInput'
      eMessage    = 'Error allocating release list array'
      GOTO 9999
    END IF

    IF( releaseIO%scnHead%number > 0 )THEN
      mlst = releaseIO%scnHead%number
      m    = SIZE_releaseT / SIZE_I
      DO irv = 1,mlst
        relListIO(irv) = TRANSFER(tInput1((irv-1)*m+1:irv*m),relListIO(irv))
      END DO
    END IF

    ReturnValue = Write_Release( releaseIO,relListIO )

    DEALLOCATE( relListIO,STAT=alloc_stat )

  CASE( SCIP_WEATHER )

    n = SIZE_pweatherT / SIZE_I
    weatherIO = TRANSFER(tInput0(1:n),weatherIO)

    ReturnValue = Write_Weather( weatherIO )

  CASE( SCIP_RUN )

    n = SIZE_pendT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Write_Run( endIO )

  CASE( SCIP_TIME )

    n = SIZE_ptemporalT / SIZE_I
    timeIO = TRANSFER(tInput0(1:n),timeIO)

    ReturnValue = Write_Time( timeIO )

  CASE( SCIP_INPUT )

    n = SIZE_pinputT / SIZE_I
    inputIO = TRANSFER(tInput0(1:n),inputIO)

    m = MAX(inputIO%input%mtlHead%number,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'WriteInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( inputIO%input%mtlHead%number > 0 )THEN
      mlst = inputIO%input%mtlHead%number
      m    = SIZE_materialT / SIZE_I
      DO irv = 1,mlst
        mtlListIO(irv) = TRANSFER(tInput1((irv-1)*m+1:irv*m),mtlListIO(irv))
      END DO
    END IF

    ReturnValue = Write_Inp( inputIO,mtlListIO )

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_RESTART )

    n = SIZE_pinputT / SIZE_I
    inputIO = TRANSFER(tInput0(1:n),inputIO)

    ReturnValue = Write_Rst( inputIO )

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_COMPLETE )

  CASE DEFAULT

END SELECT

9999 CONTINUE

WriteInput = ReturnValue

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Ctrl
!*******************************************************************************
INTEGER FUNCTION WriteCtrlF( UserID,ctrlIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,        INTENT( IN ) :: UserID
TYPE( pctrlT ), INTENT( IN ) :: ctrlIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Ctrl

!==== Initialize

WriteCtrlF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteCtrlF = Write_Ctrl( ctrlIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Start
!*******************************************************************************
INTEGER FUNCTION WriteStartF( UserID,startIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: UserID
TYPE( pstartT ), INTENT( IN ) :: startIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Start

!==== Initialize

WriteStartF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteStartF = Write_Start( startIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project End
!*******************************************************************************
INTEGER FUNCTION WriteEndF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_End

!==== Initialize

WriteEndF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteEndF = Write_End( endIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Flags
!*******************************************************************************
INTEGER FUNCTION WriteFlagsF( UserID,flagsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN ) :: UserID
TYPE( pflagsT ), INTENT( IN ) :: flagsIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Flags

!==== Initialize

WriteFlagsF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteFlagsF = Write_Flags( flagsIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Domain
!*******************************************************************************
INTEGER FUNCTION WriteDomainF( UserID,domainIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( pspatialT ), INTENT( IN ) :: domainIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Domain

!==== Initialize

WriteDomainF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteDomainF = Write_Domain( domainIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Options
!*******************************************************************************
INTEGER FUNCTION WriteOptionsF( UserID,optionsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( poptionsT ), INTENT( IN ) :: optionsIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Options

!==== Initialize

WriteOptionsF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteOptionsF = Write_Options( optionsIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Material
!*******************************************************************************
INTEGER FUNCTION WriteMaterialF( UserID,materialIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Material

!==== Initialize

WriteMaterialF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteMaterialF = Write_Material( materialIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Release
!*******************************************************************************
INTEGER FUNCTION WriteReleaseF( UserID,releaseIO,relListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN ) :: UserID
TYPE( preleaseT ),               INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( IN ) :: relListIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Release

!==== Initialize

WriteReleaseF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteReleaseF = Write_Release( releaseIO,relListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project ReleaseMC
!*******************************************************************************
INTEGER FUNCTION WriteReleaseMCF( UserID,releaseIO,relListIO,nMC,relMCList )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN ) :: UserID
TYPE( preleaseT ),                INTENT( IN ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*),  INTENT( IN ) :: relListIO
INTEGER,                          INTENT( IN ) :: nMC
TYPE( releaseMCT ), DIMENSION(*), INTENT( IN ) :: relMCList

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_ReleaseMC

!==== Initialize

WriteReleaseMCF = SCIPfailure

IF( SCIMgrCheckState( HS_ANYSTATE ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteReleaseMCF = Write_ReleaseMC( releaseIO,relListIO,nMC,relMCList )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Weather
!*******************************************************************************
INTEGER FUNCTION WriteWeatherF( UserID,weatherIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN ) :: UserID
TYPE( pweatherT ), INTENT( IN ) :: weatherIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Weather

!==== Initialize

WriteWeatherF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteWeatherF = Write_Weather( weatherIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Run
!*******************************************************************************
INTEGER FUNCTION WriteRunF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN ) :: UserID
TYPE( pendT ), INTENT( IN ) :: endIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Run

!==== Initialize

WriteRunF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteRunF = Write_Run( endIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Time
!*******************************************************************************
INTEGER FUNCTION WriteTimeF( UserID,timeIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: UserID
TYPE( ptemporalT ), INTENT( IN ) :: timeIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Time

!==== Initialize

WriteTimeF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteTimeF = Write_Time( timeIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION WriteInpF( UserID,inputIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO
TYPE( materialT ), DIMENSION(*), INTENT( IN ) :: mtlListIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Inp

!==== Initialize

WriteInpF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteInpF = Write_Inp( inputIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END

!*******************************************************************************
!            Write Project Input
!*******************************************************************************
INTEGER FUNCTION WriteRst( UserID,inputIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN ) :: UserID
TYPE( pinputT ),                 INTENT( IN ) :: inputIO

INTEGER currentState, irv

INTEGER, EXTERNAL :: Write_Rst

!==== Initialize

WriteRst = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Write routine

WriteRst = Write_Rst( inputIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
