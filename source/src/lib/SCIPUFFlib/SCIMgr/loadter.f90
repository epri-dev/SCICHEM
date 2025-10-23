!*******************************************************************************
!            Get Project Terrain Header for Specific Met Field
!*******************************************************************************
INTEGER FUNCTION GetProjectTerrainHeader( UserID,ifld,terrain )

USE SCIMgr_fd
USE scipuff_fi
USE met_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID

INTEGER irv, currentState
LOGICAL MemoryField

!==== Initialize

GetProjectTerrainHeader = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Read project file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( terrain%project )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Initialize terrain header

terrain%terrain%status = SCIPnull
terrain%terrain%hmin   = NOT_SET_R
terrain%terrain%max    = NOT_SET_I
terrain%terrain%nx     = NOT_SET_I
terrain%terrain%ny     = NOT_SET_I
terrain%terrain%dx     = NOT_SET_R
terrain%terrain%dy     = NOT_SET_R
terrain%terrain%x0     = NOT_SET_R
terrain%terrain%y0     = NOT_SET_R

IF( lter )THEN

  IF( ifld > numMet .OR. ifld < 1 )THEN
    nError = IV_ERROR
    eRoutine = 'GetProjectTerrainHeaderIn'
    eMessage = 'Invalid field id for terrain'
    WRITE(eInform,*)'Must be 1 to',numMet
    GOTO 9999
  END IF

  IF( MetGrid(ifld)%lter )THEN
    terrain%terrain%status = IBSET(terrain%terrain%status,HSB_HASTERRAIN)

    terrain%terrain%hmin = MetGrid(ifld)%Hmin
    terrain%terrain%nx   = MetGrid(ifld)%nx
    terrain%terrain%ny   = MetGrid(ifld)%ny
    terrain%terrain%x0   = MetGrid(ifld)%xminPrj
    terrain%terrain%y0   = MetGrid(ifld)%yminPrj
    terrain%terrain%dx   = (MetGrid(ifld)%xmaxPrj-MetGrid(ifld)%xminPrj)/MAX(MetGrid(ifld)%nx-1,1)
    terrain%terrain%dy   = (MetGrid(ifld)%ymaxPrj-MetGrid(ifld)%yminPrj)/MAX(MetGrid(ifld)%ny-1,1)
  END IF

END IF

!==== Set return value

GetProjectTerrainHeader = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )CALL deallocate_read_prj()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Terrain for Specific Met Field
!*******************************************************************************
INTEGER FUNCTION GetProjectTerrain( UserID,ifld,terrain,ths,tddx,tddy )

USE SCIMgr_fd
USE scipuff_fi
USE met_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: UserID  !USER ID Tag
INTEGER,               INTENT( IN    ) :: ifld    !Met field ID
TYPE( pterrainHeadT ), INTENT( INOUT ) :: terrain !Project ID
REAL,DIMENSION(*),     INTENT( OUT   ) :: ths     !Terrain array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddx    !Slope array
REAL,DIMENSION(*),     INTENT( OUT   ) :: tddy    !Slope array

INTEGER i,currentState
LOGICAL MemoryField

!==== Initialize

GetProjectTerrain = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Read project file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( terrain%project )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Load data

terrain%terrain%status = SCIPnull
terrain%terrain%hmin   = NOT_SET_R
terrain%terrain%nx     = NOT_SET_I
terrain%terrain%ny     = NOT_SET_I
terrain%terrain%dx     = NOT_SET_R
terrain%terrain%dy     = NOT_SET_R
terrain%terrain%x0     = NOT_SET_R
terrain%terrain%y0     = NOT_SET_R

IF( lter )THEN

  IF( ifld > numMet .OR. ifld < 1 )THEN
    nError = IV_ERROR
    eRoutine = 'GetProjectTerrain'
    eMessage = 'Invalid field id for terrain'
    WRITE(eInform,*)'Must be 1 to',numMet
    GOTO 9999
  END IF

  IF( MetGrid(ifld)%lter )THEN

    terrain%terrain%status = IBSET(terrain%terrain%status,HSB_HASTERRAIN)

    IF( terrain%terrain%max < MetGrid(ifld)%nx*MetGrid(ifld)%ny )THEN
      nError = SZ_ERROR
      eRoutine ='GetProjectTerrain'
      eMessage ='Insufficient space to load terrain arrays'
      WRITE(eInform,*)'Max =',terrain%terrain%max,' : Need=', MetGrid(ifld)%nx*MetGrid(ifld)%ny !nxb*nyb
      GOTO 9999
    END IF

    terrain%terrain%hmin = MetGrid(ifld)%Hmin
    terrain%terrain%nx   = MetGrid(ifld)%nx
    terrain%terrain%ny   = MetGrid(ifld)%ny
    terrain%terrain%x0   = MetGrid(ifld)%xminPrj
    terrain%terrain%y0   = MetGrid(ifld)%yminPrj
    terrain%terrain%dx   = (MetGrid(ifld)%xmaxPrj-MetGrid(ifld)%xminPrj)/MAX(MetGrid(ifld)%nx-1,1)
    terrain%terrain%dy   = (MetGrid(ifld)%ymaxPrj-MetGrid(ifld)%yminPrj)/MAX(MetGrid(ifld)%ny-1,1)
    DO i = 1,MetGrid(ifld)%nx*MetGrid(ifld)%ny
      ths(i)  = MetGrid(ifld)%H(i)
      tddx(i) = MetGrid(ifld)%Hx(i)
      tddy(i) = MetGrid(ifld)%Hy(i)
    END DO
  END IF

END IF

!==== Set return value

GetProjectTerrain = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )CALL deallocate_read_prj()

i = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Terrain
!*******************************************************************************
INTEGER FUNCTION GetTerrainSlice(  UserID,Project,nPts,Location,Ht  )

USE prjstruct_fd
USE field_fd
USE SCIMgr_fd
USE scipuff_fi
USE met_fi
USE SCIMgrState

IMPLICIT NONE

INTEGER,                             INTENT( IN  ) :: UserID  !USER ID Tag
TYPE( ProjectIDT ),                  INTENT( IN  ) :: Project !Project ID
INTEGER,                             INTENT( IN  ) :: nPts    !No. of location points
TYPE( SCIPpointT ), DIMENSION(nPts), INTENT( IN  ) :: Location    !location array
REAL,DIMENSION(nPts),                INTENT( OUT ) :: Ht    !terrain height array

INTEGER i, currentState
LOGICAL MemoryField
REAL    hx, hy

!==== Initialize

GetTerrainSlice = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Initialize error

CALL ModuleInitError()

!==== Read project file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( Project )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Load data

DO i = 1,nPts
  CALL get_topogPrj( Location(i)%x,Location(i)%y,Ht(i),hx,hy )
  Ht(i) = Ht(i) + hmin
END DO

GetTerrainSlice = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )CALL deallocate_read_prj()

i = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END


