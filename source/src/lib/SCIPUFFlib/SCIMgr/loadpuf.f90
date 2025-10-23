!*******************************************************************************
!            Get Project Puff Header
!*******************************************************************************
INTEGER FUNCTION GetProjectPuffHeader( UserID,puffHead,timeID )

USE SCIMgr_fd
USE scipuff_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: UserID   !USER ID Tag
TYPE( ppuffHeadT ), INTENT( INOUT ) :: puffHead !Project ID
INTEGER,            INTENT( IN    ) :: timeID   !Time ID for reading puffs

INTEGER irv, currentState, tID
LOGICAL MemoryField

INTEGER, EXTERNAL :: CheckPuffTime
INTEGER, EXTERNAL :: countPuffAuxs

!==== Initialize

GetProjectPuffHeader = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( puffHead%project )

!==== Initialize error

IF( Aborted() )GOTO 9999

CALL ModuleInitError()

!==== Read puff file

IF( .NOT.MemoryField )THEN

  CALL ReadProject( puffHead%project )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() )GOTO 9999

  tID = CheckPuffTime( timeID )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() )GOTO 9999

  CALL ReadPuffsID( timeID )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( Aborted() )GOTO 9999

!==== Initialize puff header

puffHead%puff%nPuff = npuf
puffHead%puff%nAux  = countPuffAuxs( npuf,puff )
puffHead%puff%nType = ntypp
puffHead%puff%time  = t/3600.

puffHead%puff%nMCtype = mat_mc%nMCtype

!==== Set the puff header coordinate

puffHead%puff%coordinate%mode = lmap
SELECT CASE( ABS(puffHead%puff%coordinate%mode) )
  CASE( HD_UTM )
    puffHead%puff%coordinate%UTMZone       = utm_zone
    puffHead%puff%coordinate%reference%x   = xref
    puffHead%puff%coordinate%reference%y   = yref
    puffHead%puff%coordinate%reference%lat = lat0
    puffHead%puff%coordinate%reference%lon = lon0
  CASE( HD_CARTESIAN )
    puffHead%puff%coordinate%UTMZone       = NOT_SET_I
    puffHead%puff%coordinate%reference%x   = xref
    puffHead%puff%coordinate%reference%y   = yref
    puffHead%puff%coordinate%reference%lat = lat0
    puffHead%puff%coordinate%reference%lon = lon0
  CASE DEFAULT
    puffHead%puff%coordinate%UTMZone       = NOT_SET_I
    puffHead%puff%coordinate%reference%x   = NOT_SET_R
    puffHead%puff%coordinate%reference%y   = NOT_SET_R
    puffHead%puff%coordinate%reference%lat = NOT_SET_R
    puffHead%puff%coordinate%reference%lon = NOT_SET_R
END SELECT

!==== Set return value

GetProjectPuffHeader = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )THEN
  CALL deallocate_read_prj()
  CALL deallocatePuffs()
END IF

CALL AbortClear()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Get Project Puff
!*******************************************************************************
INTEGER FUNCTION GetProjectPuff( UserID,puffHead,timeID,flag,puffList,auxData,typeList,mcList )

USE SCIMgr_fd
USE scipuff_fi
USE SCIMgrState
USE SCIPresults_fd
USE abort
USE met_fi

IMPLICIT NONE

INTEGER,                           INTENT( IN    ) :: UserID      !USER ID Tag
TYPE( ppuffHeadT ),                INTENT( INOUT ) :: puffHead    !Project ID
INTEGER,                           INTENT( IN    ) :: timeID      !Time ID for reading puffs
INTEGER,                           INTENT( IN    ) :: flag        !Bits to transform to LLA, MSL, terrain slope
TYPE( puffT ),       DIMENSION(*), INTENT( OUT   ) :: puffList    !Puffs
REAL,                DIMENSION(*), INTENT( OUT   ) :: auxData     !Puff auxiliary data
TYPE( puffTypeT ),   DIMENSION(*), INTENT( OUT   ) :: typeList    !Puffs types
TYPE(material_MClist),             INTENT( INOUT ) :: mcList      !multicomponent types

INTEGER i, currentState, tID, npaux, iaux
REAL    h, hx, hy
LOGICAL doPuffs, MemoryField

TYPE( puffCoordinateT )  :: LLACoordinate

INTEGER, EXTERNAL :: getPuffifld
INTEGER, EXTERNAL :: CheckPuffTime
INTEGER, EXTERNAL :: countPuffAuxs

!==== Initialize

GetProjectPuff = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( puffHead%project )

!==== Initialize error

IF( Aborted() )GOTO 9999

CALL ModuleInitError()

!==== Read puff file

IF( .NOT.MemoryField )THEN
  CALL ReadProject( puffHead%project )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() )GOTO 9999

  tID = CheckPuffTime(timeID)
  IF( nError /= NO_ERROR )GOTO 9999

  IF( Aborted() )GOTO 9999

  CALL ReadPuffsID( timeID )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

IF( Aborted() )GOTO 9999

!==== Set project dynamics flags needed to interpret aux values

IF( dynamic )THEN
  puffHead%puff%dynamic = SCIPtrue
  IF( dense_gas )THEN
    puffHead%puff%dense = SCIPtrue
  ELSE
    puffHead%puff%dense = SCIPfalse
  END IF
  IF( buoy_gas )THEN
    puffHead%puff%buoy = SCIPtrue
  ELSE
    puffHead%puff%buoy = SCIPfalse
  END IF
ELSE
  puffHead%puff%dynamic = SCIPfalse
  puffHead%puff%dense   = SCIPfalse
  puffHead%puff%buoy    = SCIPfalse
END IF

IF( mat_mc%nMCtype > 0 )THEN
  IF( mcList%nMCtype >= mat_mc%nMCtype )THEN
    IF( ASSOCIATED(mcList%type) )THEN
      DO i=1,mat_mc%nMCtype
        mcList%type(i) = mat_mc%type(i)
      END DO
    END IF
    IF( ASSOCIATED(mcList%ID) )THEN
      DO i=1,mat_mc%nMCtype
        mcList%ID(i) = mat_mc%ID(i)
      END DO
    END IF
  END IF
END IF

!==== Initialize puff

npaux = countPuffAuxs( npuf,puff )

puffHead%puff%nPuff = npuf
puffHead%puff%nAux  = npaux
puffHead%puff%nType = ntypp
puffHead%puff%time  = t/3600.

!==== Set the puff header coordinate

puffHead%puff%coordinate%mode = lmap
SELECT CASE( ABS(puffHead%puff%coordinate%mode) )
  CASE( HD_UTM )
    puffHead%puff%coordinate%UTMZone       = utm_zone
    puffHead%puff%coordinate%reference%x   = xref
    puffHead%puff%coordinate%reference%y   = yref
    puffHead%puff%coordinate%reference%lat = lat0
    puffHead%puff%coordinate%reference%lon = lon0
  CASE( HD_CARTESIAN )
    puffHead%puff%coordinate%UTMZone       = NOT_SET_I
    puffHead%puff%coordinate%reference%x   = xref
    puffHead%puff%coordinate%reference%y   = yref
    puffHead%puff%coordinate%reference%lat = lat0
    puffHead%puff%coordinate%reference%lon = lon0
  CASE DEFAULT
    puffHead%puff%coordinate%UTMZone       = NOT_SET_I
    puffHead%puff%coordinate%reference%x   = NOT_SET_R
    puffHead%puff%coordinate%reference%y   = NOT_SET_R
    puffHead%puff%coordinate%reference%lat = NOT_SET_R
    puffHead%puff%coordinate%reference%lon = NOT_SET_R
END SELECT

doPuffs = .FALSE.
IF( puffHead%puff%maxPuff > 0 )THEN
  IF( puffHead%puff%maxPuff >= npuf )THEN
    doPuffs = .TRUE.
    DO i = 1,npuf
      CALL LoadPuff( puff(i),puffList(i) ) !puff%naux=>puffList%iaux corrected below
    END DO
  ELSE
    nError   = SZ_ERROR
    eRoutine = 'LoadProjectPuff'
    eMessage = 'Insufficient space to load puff data'
    WRITE(eInform,*)'Max =',puffHead%puff%maxPuff,' : Need =',npuf
    GOTO 9999
  END IF
END IF

IF( puffHead%puff%maxAux > 0 )THEN
  IF( puffHead%puff%maxAux >= npaux )THEN
    iaux = 0
    DO i = 1,npuf
      IF( puff(i)%naux > 0 )THEN
        puffList(i)%iaux = iaux + 1
        auxData(iaux+1:iaux+puff(i)%naux) = puff(i)%aux
        iaux = iaux + puff(i)%naux
      ELSE
        puffList(i)%iaux = 0
      END IF
    END DO
  ELSE
    nError   = SZ_ERROR
    eRoutine = 'GetProjectPuff'
    eMessage = 'Insufficient space to load puff auxiliary data'
    WRITE(eInform,*)'Max =',puffHead%puff%maxAux,' : Need =',npaux
    GOTO 9999
  END IF
END IF

IF( Aborted() ) GOTO 9999

IF( puffHead%puff%maxType > 0 )THEN
  IF( puffHead%puff%maxType >= ntypp )THEN
    DO i = 1,ntypp
      CALL LoadPuffType( typeList(i),typeID(i),material(typeID(i)%imat)%cmat )
    END DO
  ELSE
    nError   = SZ_ERROR
    eRoutine = 'GetProjectPuff'
    eMessage = 'Insufficient space to load puff type data'
    WRITE(eInform,*)'Max =',puffHead%puff%maxType,' : Need =',ntypp
    GOTO 9999
  END IF
END IF

!------ Adjust p%zbar, p%zc and p%zi to be above ground (instead of above hmin) or above MSL
!       Put terrain elevation (MSL) in puff element SR
!       Optionlly, put terrain slopes (hx,hy) in p%axx and p%ayy

IF ( doPuffs )THEN
  DO i = 1,npuf
    CALL get_topogIn( SNGL(puff(i)%xbar),SNGL(puff(i)%ybar),h,hx,hy,getPuffifld(puff(i)) )
    IF( BTEST(flag,PPB_MSL) )THEN
        puffList(i)%zbar = puffList(i)%zbar+hmin
        IF( puffList(i)%zc > 0 )puffList(i)%zc = puffList(i)%zc+hmin
        IF( puffList(i)%zi > 0 )puffList(i)%zi = puffList(i)%zi+hmin
    ELSE
      puffList(i)%zbar = MAX(puffList(i)%zbar-h,0.0)
      IF( puffList(i)%zc > 0 )puffList(i)%zc = MAX(puffList(i)%zc-h,0.0)
      IF( puffList(i)%zi > 0 )puffList(i)%zi = MAX(puffList(i)%zi-h,0.0)
    END IF
    puffList(i)%sr = h + hmin
    IF( BTEST(flag,PPB_SLOPE) )THEN
      puffList(i)%axx = hx
      puffList(i)%ayy = hy
    END IF
  END DO
END IF

IF( Aborted() )GOTO 9999

!------ Transform to LLA if asked for

IF( doPuffs .AND. BTEST(Flag,PPB_TRANS) )THEN
  LLACoordinate%mode          = HD_LATLON
  LLACoordinate%UTMZone       = NOT_SET_I
  LLACoordinate%reference%x   = NOT_SET_R
  LLACoordinate%reference%y   = NOT_SET_R
  LLACoordinate%reference%lat = NOT_SET_R
  LLACoordinate%reference%lon = NOT_SET_R
  CALL TransformPuff( puffHead%puff%coordinate,LLACoordinate,npuf,PuffList )
  IF( nError /= NO_ERROR )THEN
    IF( nError /= AB_ERROR)THEN
      nError   = UK_ERROR
      eRoutine = 'GetProjectPuff'
      eMessage = 'Error transforming puff coordinates to LatLon'
    END IF
    GOTO 9999
  END IF
  puffHead%puff%coordinate = LLACoordinate
END IF

!==== Set return value

GetProjectPuff = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )THEN
  CALL deallocate_read_prj()
  CALL deallocatePuffs()
END IF

CALL AbortClear()

i = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!*******************************************************************************
!            Move pufftype
!*******************************************************************************
SUBROUTINE LoadPuffType( pOut,pIn,material )

USE spcstruct_fd
USE typestruct_fd
USE SCIPresults_fd

IMPLICIT NONE

TYPE( type_str  ), INTENT( IN  ) :: pIn
CHARACTER(*),      INTENT( IN  ) :: material
TYPE( puffTypeT ), INTENT( OUT ) :: pOut

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid

pOut%material = TRIM(material)
pOut%imat     = pIn%imat
pOut%icls     = pIn%icls
pOut%npaux    = pIn%npaux
IF( IsGas(pIn%icls) )THEN
  pOut%igrp = 1
ELSE IF( IsLiquid(pIn%icls) )THEN
  pOut%igrp = 2
ELSE
  pOut%igrp = pIn%igrp
END IF
pOut%ipmc     = pIn%ipmc
pOut%mcID     = pIn%mcID
IF( pIn%ltot )THEN
  pOut%ltot = SCIPtrue
ELSE
  pOut%ltot = SCIPfalse
END IF

RETURN
END
!*******************************************************************************
!            CheckPuffTime
!*******************************************************************************
INTEGER FUNCTION CheckPuffTime( timeID )

USE time_fd
USE plotlist_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: timeID

INTEGER n

INTEGER, EXTERNAL :: NumberPuffTimes

CheckPuffTime = -1

!------ Count the number of puff time

n = NumberPuffTimes( .FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

IF( timeID > 0 .AND. timeID < n+1 )THEN
  CheckPuffTime = timeID
ELSE
  CheckPuffTime = n
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Get Project Puff Chem
!*******************************************************************************
INTEGER FUNCTION GetProjectPuffChem( UserID,project,chemOut,doData )

USE SCIMgr_fd
USE scipuff_fi
USE SCIMgrState
USE abort
USE chem_fi
USE multcomp_fd

IMPLICIT NONE

INTEGER,                             INTENT( IN    ) :: UserID     !USER ID Tag
TYPE( projectIDT ),                  INTENT( IN    ) :: project    !Project ID
TYPE( ChemMC_out ), DIMENSION(*),    INTENT( INOUT ) :: chemOut
LOGICAL,                             INTENT( IN    ) :: doData

INTEGER irv, currentState, i, n
LOGICAL MemoryField

!==== Initialize

GetProjectPuffChem = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN    !Available only while idle
  MemoryField = .FALSE.
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( project )

!==== Initialize error

IF( Aborted() )GOTO 9999

CALL ModuleInitError()

!==== Read puff file

IF( .NOT.MemoryField )THEN

  CALL ReadProject( project )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

IF( Aborted() )GOTO 9999

!==== Initialize puff header

DO i = 1,mat_mc%nMCtype

  SELECT CASE( mat_mc%type(i) )
    CASE( MC_CHEM )
      IF( doData )THEN
        chem =>chemMC(i,1)
        IF( ASSOCIATED(chemOut(i)%species) )THEN
          DO n = 1,chem%nspecies
            chemOut(i)%species(n)%name  = chem%species(n)%name
            chemOut(i)%species(n)%lstar = chem%species(n)%lstar
            chemOut(i)%species(n)%mass  = 0.
            chemOut(i)%species(n)%conc  = 0.
            chemOut(i)%species(n)%amb   = 0.
          END DO
        END IF
      ELSE
        chemOut(i)%nFast        = chem%nFast
        chemOut(i)%nSlow        = chem%nSlow
        chemOut(i)%nParticle    = chem%nParticle
        chemOut(i)%nEquilibrium = chem%nEquilibrium
        chemOut(i)%nStar        = chem%nStar
        chemOut(i)%nSpecies     = chem%nSpecies
      END IF
    CASE DEFAULT
  END SELECT

END DO

!==== Set return value

GetProjectPuffChem = SCIPsuccess

9999 CONTINUE

IF( .NOT.MemoryField )THEN
  CALL deallocate_read_prj()
END IF

CALL AbortClear()

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
