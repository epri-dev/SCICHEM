INTEGER FUNCTION GetFieldTableSizeF( UserID,FieldX,ClassData,mTable,mCol,mRow )

USE scipuff_fi
USE prjstruct_fd
USE plotlist_fi
USE field_fd
USE SCIMgr_fd
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,                INTENT( IN  ) :: UserID
TYPE( SCIPPlotFieldT ), INTENT( IN  ) :: FieldX
REAL, DIMENSION(*),     INTENT( IN  ) :: ClassData
INTEGER,                INTENT( OUT ) :: mTable,mCol,mRow

INTEGER currentState,irv
TYPE( projectIDT ) :: project

!==== initialize

GetFieldTableSizeF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )THEN
  project%ID      = 0
  project%version = 0
  project%path    = TRIM(FieldX%path)
  project%name    = TRIM(FieldX%project)
  CALL SetupFileNames( project )
END IF

!==== Get table size

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL GetFieldTableSize( FieldX,ClassData,mTable,mCol,mRow )

  IF( nError == NO_ERROR )GetFieldTableSizeF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetFieldTableF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table )

USE scipuff_fi
USE surface_fi
USE srfaux_fi
USE plotlist_fi
USE field_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,               INTENT( IN )    :: UserID
TYPE( SCIPPlotFieldT ),INTENT( INOUT ) :: FieldX
REAL, DIMENSION(*),    INTENT( IN )    :: ClassData

TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: TableTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: ColTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: RowTitle
INTEGER,         DIMENSION(*), INTENT( OUT ) :: Table

TYPE( spatialT ) :: domain

INTEGER,EXTERNAL :: GetFieldTableExtendedF

GetFieldTableF = GetFieldTableExtendedF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,domain )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetFieldTableExtendedF( UserID,FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )

USE scipuff_fi
USE surface_fi
USE srfaux_fi
USE plotlist_fi
USE field_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE abort

IMPLICIT NONE

INTEGER,               INTENT( IN )    :: UserID
TYPE( SCIPPlotFieldT ),INTENT( INOUT ) :: FieldX
REAL, DIMENSION(*),    INTENT( IN )    :: ClassData

TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: TableTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: ColTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: RowTitle
INTEGER,         DIMENSION(*), INTENT( OUT ) :: Table
TYPE( spatialT ),              INTENT( OUT ) :: Domain

INTEGER irv, currentState
TYPE( projectIDT ) :: project

!==== initialize

GetFieldTableExtendedF = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN     !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )THEN
  project%ID      = 0
  project%version = 0
  project%path    = TRIM(FieldX%path)
  project%name    = TRIM(FieldX%project)
  CALL SetupFileNames( project )
END IF

!==== Get table

IF( .NOT. Aborted() )THEN

  CALL init_error()

  CALL GetFieldTable( FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )

  IF( nError == NO_ERROR )GetFieldTableExtendedF = SCIPsuccess

END IF

CALL AbortClear()

!==== finish

CALL reset_messaging()

irv = SCIMgrSetState( currentState )

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE GetFieldTableSize( FieldX,ClassData,mTable,mCol,mRow )

USE scipuff_fi
USE prjstruct_fd
USE plotlist_fi
USE field_fd
USE SCIMgr_fd
USE abort

IMPLICIT NONE

TYPE( SCIPPlotFieldT ), INTENT( IN  ) :: FieldX
REAL, DIMENSION(*),     INTENT( IN  ) :: ClassData
INTEGER,                INTENT( OUT ) :: mTable,mCol,mRow

TYPE( projectIDT )     :: Project
TYPE( SCIPPlotFieldT ) :: Field

IF( Aborted() )GOTO 9999

Field = FieldX

IF( Field%category /= HP_TABLE )GOTO 9999

!------ Read project file

Project%path = TRIM(Field%path)
Project%name = TRIM(Field%project)
CALL ReadProject( Project )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Build list of plot choices

CALL BuildPlotList()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Reset plot choice

Field%choice = ChoiceOrder(FieldX%choice)

!------ Check plot request

IF( Field%category /= HP_TABLE )THEN
  nError   = IV_ERROR
  eRoutine = 'GetFieldTableSize'
  eMessage = 'Invalid field request (category)'
  eInform  = 'Use SCIPCreateField for plot requests'
  GOTO 9999
END IF

CALL CheckPlotReq( Field )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

SELECT CASE( ClassID(Field%class) )

  CASE DEFAULT

    nError   = UK_ERROR
    eRoutine = 'GetFieldTableSize'
    eMessage = 'Unknown table request (class)'
    GOTO 9999

END SELECT

9999 CONTINUE

CALL deallocate_read_prj()

RETURN
END

!==============================================================================

SUBROUTINE GetFieldTable( FieldX,ClassData,TableTitle,ColTitle,RowTitle,Table,Domain )

USE scipuff_fi
USE surface_fi
USE srfaux_fi
USE plotlist_fi
USE field_fd
USE SCIMgr_fd
USE SCIMgr_fi
USE abort
USE classData_fd, ONLY : CD_CUTOFF

IMPLICIT NONE

TYPE( SCIPPlotFieldT ),INTENT( INOUT ) :: FieldX
REAL, DIMENSION(*),    INTENT( IN )    :: ClassData

TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: TableTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: ColTitle
TYPE( char32T ), DIMENSION(*), INTENT( OUT ) :: RowTitle
INTEGER,         DIMENSION(*), INTENT( OUT ) :: Table
TYPE( spatialT ),              INTENT( OUT ) :: Domain

TYPE( projectIDT )     :: Project
TYPE( SCIPPlotFieldT ) :: Field

INTEGER ichoice, alloc_stat

IF( Aborted() )GOTO 9999

Field = FieldX

IF( Field%category /= HP_TABLE )GOTO 9999

!------ Read project file

Project%path = TRIM(Field%path)
Project%name = TRIM(Field%project)
CALL ReadProject( Project )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Build list of plot choices

CALL BuildPlotList()
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Reset plot choice

Field%choice = ChoiceOrder(FieldX%choice)

!------ Check plot request

IF( Field%category /= HP_TABLE )THEN
  nError   = IV_ERROR
  eRoutine = 'GetFieldTable'
  eMessage = 'Invalid field request (category)'
  eInform  = 'Use SCIPCreateField for plot requests'
  GOTO 9999
END IF

CALL CheckPlotReq( Field )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Setup surface blocks (currently required for all tables)

CALL init_srf_blocks( ntypm )
IF( nError /= NO_ERROR )GOTO 9999

IF( Aborted() )GOTO 9999

!------ Create appropriate output table

CasualtyCutOff = ClassData( CD_CUTOFF )

SELECT CASE( ClassID(Field%class) )

  CASE DEFAULT

    nError   = UK_ERROR
    eRoutine = 'GetFieldTable'
    eMessage = 'Unknown table request (class)'
    GOTO 9999

END SELECT

9999 CONTINUE

!--- Set return Field

ichoice = FieldX%choice
FieldX  = Field
FieldX%choice = ichoice

!------ Deallocate plot choice arrays

CALL ClearPlotLists()

IF( .NOT.MemoryField )THEN

!------ Deallocate SCIPUFF arrays (that may have been used in creating plot)

  IF( ALLOCATED( srfnam    ) )DEALLOCATE( srfnam,    STAT=alloc_stat )
  IF( ALLOCATED( srftyp    ) )DEALLOCATE( srftyp,    STAT=alloc_stat )
  IF( ALLOCATED( srf_block ) )DEALLOCATE( srf_block, STAT=alloc_stat )
  IF( ALLOCATED( srf_effect) )DEALLOCATE( srf_effect,STAT=alloc_stat )
  CALL deallocateSrfPuff()
  IF( ALLOCATED(evapTemp)     )DEALLOCATE( evapTemp,    STAT=alloc_stat )
  IF( ALLOCATED(evapFld)      )DEALLOCATE( evapFld,     STAT=alloc_stat )
  IF( ALLOCATED(evapTempID)   )DEALLOCATE( evapTempID,  STAT=alloc_stat )
  IF( ASSOCIATED( ibaux_srf ) )DEALLOCATE( ibaux_srf,STAT=alloc_stat )
  IF( ASSOCIATED( cmaux_srf ) )DEALLOCATE( cmaux_srf,STAT=alloc_stat )
  IF( ASSOCIATED( cvaux_srf ) )DEALLOCATE( cvaux_srf,STAT=alloc_stat )
  CALL deallocatePuffs()                       !Should already have been done but just in case
  CALL deallocate_read_prj()

END IF

RETURN
END


