!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION LoadInput( UserID,iInput,tInput0,tInput1 )

USE SCIMgr_fd
USE error_fi
USE statstruct_fd
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: UserID
INTEGER,               INTENT( IN    ) :: iInput
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput0
INTEGER, DIMENSION(*), INTENT( INOUT ) :: tInput1

INTEGER, PARAMETER :: SIZE_I = KIND(1)

INTEGER ReturnValue, irv, currentState, alloc_stat, n, n1, m, mlst, i

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

INTEGER, EXTERNAL :: Load_Time
INTEGER, EXTERNAL :: Load_Run
INTEGER, EXTERNAL :: Load_Inp
INTEGER, EXTERNAL :: Load_Weather
INTEGER, EXTERNAL :: Load_Release
INTEGER, EXTERNAL :: Load_Material
INTEGER, EXTERNAL :: Load_Options
INTEGER, EXTERNAL :: Load_Domain
INTEGER, EXTERNAL :: Load_Flags
INTEGER, EXTERNAL :: Load_End
INTEGER, EXTERNAL :: Load_Start
INTEGER, EXTERNAL :: Load_Ctrl

!==== Initialize

LoadInput = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

ReturnValue = SCIPsuccess

!==== Initialize error

CALL ModuleInitError()

!==== Select Load routine

SELECT CASE( iInput )

  CASE( SCIP_CTRL )

    n = SIZE_pctrlT / SIZE_I
    ctrlIO = TRANSFER(tInput0(1:n),ctrlIO)

    ReturnValue = Load_Ctrl( ctrlIO )

    tInput0(1:n) = TRANSFER(ctrlIO,tInput0(1:n))

  CASE( SCIP_START )

    n = SIZE_pstartT / SIZE_I
    startIO = TRANSFER(tInput0(1:n),startIO)

    ReturnValue = Load_Start( startIO )

    tInput0(1:n) = TRANSFER(startIO,tInput0(1:n))

  CASE( SCIP_END )

    n = SIZE_pendT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Load_End( endIO )

    tInput0(1:n) = TRANSFER(endIO,tInput0(1:n))

  CASE( SCIP_FLAGS )

    n = SIZE_pflagsT / SIZE_I
    flagsIO = TRANSFER(tInput0(1:n),flagsIO)

    ReturnValue = Load_Flags( flagsIO )

    tInput0(1:n) = TRANSFER(flagsIO,tInput0(1:n))

  CASE( SCIP_DOMAIN )

    n = SIZE_pspatialT / SIZE_I
    domainIO = TRANSFER(tInput0(1:n),domainIO)

    ReturnValue = Load_Domain( domainIO )

    tInput0(1:n) = TRANSFER(domainIO,tInput0(1:n))

  CASE( SCIP_OPTIONS )

    n = SIZE_poptionsT / SIZE_I
    optionsIO = TRANSFER(tInput0(1:n),optionsIO)

    ReturnValue = Load_Options( optionsIO )

    tInput0(1:n) = TRANSFER(optionsIO,tInput0(1:n))

  CASE( SCIP_MATERIAL )

    n  = SIZE_pmaterialT / SIZE_I
    n1 = SIZE_materialT  / SIZE_I

    materialIO = TRANSFER(tInput0(1:n),materialIO)

    m = MAX(materialIO%mtlHead%max,1)
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'LoadInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( materialIO%mtlHead%number > 0 )THEN
      mlst = materialIO%mtlHead%number
      m    = 0
      DO i = 1,mlst
        mtlListIO(i) = TRANSFER(tInput1(m+1:m+n1),mtlListIO(i))
        m = m + n1
      END DO
    END IF

    ReturnValue = Load_Material( materialIO,mtlListIO )

    tInput0(1:n) = TRANSFER(materialIO,tInput0(1:n))

    IF( materialIO%mtlHead%number > 0 )THEN
      mlst = materialIO%mtlHead%number
      m    = 0
      DO i = 1,mlst
        tInput1(m+1:m+n1) = TRANSFER(mtlListIO(i),tInput1(m+1:m+n1))
        m = m + n1
      END DO
    END IF
    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_RELEASE )

    n  = SIZE_preleaseT / SIZE_I
    n1 = SIZE_releaseT  / SIZE_I

    releaseIO = TRANSFER(tInput0(1:n),releaseIO)

    m = MAX(releaseIO%scnHead%max,1)
    ALLOCATE( relListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'LoadInput'
      eMessage    = 'Error allocating release list array'
      GOTO 9999
    END IF

    IF( releaseIO%scnHead%number > 0 )THEN
      mlst = releaseIO%scnHead%number
      m    = 0
      DO i = 1,mlst
        relListIO(i) = TRANSFER(tInput1(m+1:m+n1),relListIO(i))
        m = m + n1
      END DO
    END IF

    ReturnValue = Load_Release( releaseIO,relListIO )

    tInput0(1:n) = TRANSFER(releaseIO,tInput0(1:n))

    IF( releaseIO%scnHead%number > 0 )THEN
      mlst = releaseIO%scnHead%number
      m    = 0
      DO i = 1,mlst
        tInput1(m+1:m+n1) = TRANSFER(relListIO(i),tInput1(m+1:m+n1))
        m = m + n1
      END DO
    END IF

    DEALLOCATE( relListIO,STAT=alloc_stat )

  CASE( SCIP_WEATHER )

    n = SIZE_pweatherT / SIZE_I
    weatherIO = TRANSFER(tInput0(1:n),weatherIO)

    ReturnValue = Load_Weather( weatherIO )

    tInput0(1:n) = TRANSFER(weatherIO,tInput0(1:n))

  CASE( SCIP_RUN )

    n = SIZE_pendT / SIZE_I
    endIO = TRANSFER(tInput0(1:n),endIO)

    ReturnValue = Load_Run( endIO )

    tInput0(1:n) = TRANSFER(endIO,tInput0(1:n))

  CASE( SCIP_TIME )

    n = SIZE_ptemporalT / SIZE_I
    timeIO = TRANSFER(tInput0(1:n),timeIO)

    ReturnValue = Load_Time( timeIO )

    tInput0(1:n) = TRANSFER(timeIO,tInput0(1:n))

  CASE( SCIP_INPUT )

    n  = SIZE_pinputT   / SIZE_I
    n1 = SIZE_materialT / SIZE_I

    inputIO = TRANSFER(tInput0(1:n),inputIO)

    m = inputIO%input%mtlHead%max
    ALLOCATE( mtlListIO(m),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      ReturnValue = SCIPfailure
      nError      = UK_ERROR
      eRoutine    = 'LoadInput'
      eMessage    = 'Error allocating material list array'
      GOTO 9999
    END IF

    IF( inputIO%input%mtlHead%number > 0 )THEN
      mlst = inputIO%input%mtlHead%number
      m    = 0
      DO i = 1,mlst
        mtlListIO(i) = TRANSFER(tInput1(m+1:m+n1),mtlListIO(i))
        m = m + n1
      END DO
    END IF

    ReturnValue = Load_Inp( inputIO,mtlListIO )

    tInput0(1:n) = TRANSFER(inputIO,tInput0(1:n))

    IF( inputIO%input%mtlHead%number > 0 )THEN
      mlst = inputIO%input%mtlHead%number
      m    = 0
      DO i = 1,mlst
        tInput1(m+1:m+n1) = TRANSFER(mtlListIO(i),tInput1(m+1:m+n1))
        m = m + n1
      END DO
    END IF

    DEALLOCATE( mtlListIO,STAT=alloc_stat )

  CASE( SCIP_COMPLETE )

  CASE DEFAULT

END SELECT

9999 CONTINUE

LoadInput = ReturnValue

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Ctrl
!*******************************************************************************
INTEGER FUNCTION LoadCtrlF( UserID,ctrlIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,        INTENT( IN    ) :: UserID
TYPE( pctrlT ), INTENT( INOUT ) :: ctrlIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Ctrl

!==== Initialize

LoadCtrlF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadCtrlF = Load_Ctrl( ctrlIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Start
!*******************************************************************************
INTEGER FUNCTION LoadStartF( UserID,startIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pstartT ), INTENT( INOUT ) :: startIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Start

!==== Initialize

LoadStartF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadStartF = Load_Start( startIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION LoadEndF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_End

!==== Initialize

LoadEndF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadEndF = Load_End( endIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project End
!*******************************************************************************
INTEGER FUNCTION LoadEndXF( UserID,endIO,extFlag )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO
INTEGER,       INTENT( IN    ) :: extFlag

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_EndX

!==== Initialize

LoadEndXF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadEndXF = Load_EndX( endIO, extFlag )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Flags
!*******************************************************************************
INTEGER FUNCTION LoadFlagsF( UserID,flagsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,         INTENT( IN    ) :: UserID
TYPE( pflagsT ), INTENT( INOUT ) :: flagsIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Flags

!==== Initialize

LoadFlagsF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadFlagsF = Load_Flags( flagsIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Domain
!*******************************************************************************
INTEGER FUNCTION LoadDomainF( UserID,domainIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pspatialT ), INTENT( INOUT ) :: domainIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Domain

!==== Initialize

LoadDomainF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadDomainF = Load_Domain( domainIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Options
!*******************************************************************************
INTEGER FUNCTION LoadOptionsF( UserID,optionsIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( poptionsT ), INTENT( INOUT ) :: optionsIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Options

!==== Initialize

LoadOptionsF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadOptionsF = Load_Options( optionsIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Material
!*******************************************************************************
INTEGER FUNCTION LoadMaterialF( UserID,materialIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pmaterialT ),               INTENT( INOUT ) :: materialIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Material

!==== Initialize

LoadMaterialF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadMaterialF = Load_Material( materialIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Release
!*******************************************************************************
INTEGER FUNCTION LoadReleaseF( UserID,releaseIO,relListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( preleaseT ),               INTENT( INOUT ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( INOUT ) :: relListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Release

!==== Initialize

LoadReleaseF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadReleaseF = Load_Release( releaseIO,relListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Release
!*******************************************************************************
INTEGER FUNCTION LoadReleaseMCF( UserID,releaseIO,relListIO,relMCListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: UserID
TYPE( preleaseT ),               INTENT( INOUT ) :: releaseIO
TYPE( releaseT  ), DIMENSION(*), INTENT( INOUT ) :: relListIO
TYPE( releaseMCT), DIMENSION(*), INTENT( INOUT ) :: relMCListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_ReleaseMC

!==== Initialize

LoadReleaseMCF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadReleaseMCF = Load_ReleaseMC( releaseIO,relListIO,relMCListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Weather
!*******************************************************************************
INTEGER FUNCTION LoadWeatherF( UserID,weatherIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: UserID
TYPE( pweatherT ), INTENT( INOUT ) :: weatherIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Weather

!==== Initialize

LoadWeatherF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadWeatherF = Load_Weather( weatherIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Run
!*******************************************************************************
INTEGER FUNCTION LoadRunF( UserID,endIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,       INTENT( IN    ) :: UserID
TYPE( pendT ), INTENT( INOUT ) :: endIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Run

!==== Initialize

LoadRunF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadRunF = Load_Run( endIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Time
!*******************************************************************************
INTEGER FUNCTION LoadTimeF( UserID,timeIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: UserID
TYPE( ptemporalT ), INTENT( INOUT ) :: timeIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Time

!==== Initialize

LoadTimeF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadTimeF = Load_Time( timeIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Load Project Input
!*******************************************************************************
INTEGER FUNCTION LoadInpF( UserID,inputIO,mtlListIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO
TYPE( materialT  ), DIMENSION(*), INTENT( INOUT ) :: mtlListIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Inp

!==== Initialize

LoadInpF = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadInpF = Load_Inp( inputIO,mtlListIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END

!*******************************************************************************
!            Load Project Specail Restart Input
!*******************************************************************************
INTEGER FUNCTION LoadRst( UserID,inputIO )

USE SCIMgr_fd
USE error_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                          INTENT( IN    ) :: UserID
TYPE( pinputT ),                  INTENT( INOUT ) :: inputIO

INTEGER irv, currentState

INTEGER, EXTERNAL :: Load_Rst

!==== Initialize

LoadRst = SCIPfailure

IF( SCIMgrCheckState( HS_IDLEBUSY ) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Call Load routine

LoadRst = Load_Rst( inputIO )

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END


