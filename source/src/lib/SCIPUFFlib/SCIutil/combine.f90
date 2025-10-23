!*******************************************************************************
!            Combine common functions
!*******************************************************************************
!=============================================================================================

SUBROUTINE InitGrid( file,grdI,times )

USE sagdef_fd
USE sagstr_fd
USE error_fi
USE simple_fi
USE PtrGrdStrItf
USE reallocate

IMPLICIT NONE

CHARACTER(*),       INTENT( IN    ) :: file
INTEGER,            INTENT( INOUT ) :: grdI
TYPE( timeStruct ), INTENT( INOUT ) :: times

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER irv, lun, alloc_stat

INTEGER, EXTERNAL :: SAG_InitGridID, SAG_OpenID, SAG_ReadHeaderID, SAG_FindTime
INTEGER, EXTERNAL :: SAG_NewGrdStr

!------ Get a new SAG structure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'Error creating SAG surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Initialize SAG structure

lun = 100 + grdI
irv = SAG_InitGridID( file,lun,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!------ Open surface file; read nvart for block version

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer

times%nTimes = 0
DO
  irv = SAG_ReadHeaderID( grdI )
  IF( irv == SAG_EOF )EXIT
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'InitGrid'
    eMessage = 'Error reading SAG file'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
  times%nTimes = times%nTimes + 1
  IF( times%nTimes == 1 )THEN
    ALLOCATE( times%times(1),STAT=alloc_stat )
  ELSE
    alloc_stat = reallocate_real1d( times%times,1 )
  END IF
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'InitGrid'
    eMessage = 'Error allocating time list'
    GOTO 9999
  END IF
  times%times(times%nTimes) = grd%time
END DO

IF( times%nTimes <= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'File has no times : Project must be run first'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

irv = SAG_FindTime( grdI,-999. )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitGrid'
  eMessage = 'Error rewinding file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE ReinitGridID( grdI )

USE sagstr_fd
USE error_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: grdI

TYPE( SAGgrid_str ), POINTER :: grd

INTEGER alloc_stat

grd => SAG_PtrGrdStr( grdI ) ! Associate "local" grid structure pointer

IF( ASSOCIATED(grd%ipgrd) )THEN
  DEALLOCATE( grd%ipgrd,STAT=alloc_stat )
  NULLIFY( grd%ipgrd )
END IF

IF( ASSOCIATED(grd%ipdat) )THEN
  DEALLOCATE( grd%ipdat,STAT=alloc_stat )
  NULLIFY( grd%ipdat )
END IF

grd%mxgrd = 0

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE MatchDezoneLevels( Aid,aTyp,Bid,bTyp,doDep )

USE combine_fd
USE sagdef_fd
USE sagstr_fd
USE error_fi
USE PtrGrdStrItf
USE dezone_fi

IMPLICIT NONE

INTEGER,            INTENT( IN ) :: Aid
TYPE( typStruct ),  INTENT( IN ) :: aTyp
INTEGER,            INTENT( IN ) :: Bid
TYPE( typStruct ),  INTENT( IN ) :: bTyp
LOGICAL,            INTENT( IN ) :: doDep

INTEGER irv, i, tStart, tEnd
INTEGER alev, blev

TYPE( SAGgrid_str ), POINTER :: grda, grdb

INTEGER, EXTERNAL :: SAG_FindMaxLevID, SAG_Dezone
INTEGER, EXTERNAL :: DezoneCell

grda => SAG_PtrGrdStr( Aid ) ! Associate "local" grid structure pointer
grdb => SAG_PtrGrdStr( Bid ) ! Associate "local" grid structure pointer

alev = SAG_FindMaxLevID( Aid )
blev = SAG_FindMaxLevID( Bid )
IF( grdb%maxlev > 0 .AND. grdb%maxlev < alev )THEN
  IF( doDep )THEN
    tStart = 1
    tEnd   = aTyp%nDep
  ELSE
    tStart = aTyp%nDep + 1
    tEnd   = aTyp%nTypes
  END IF
  CALL init_dezone( grda%nvart,aTyp%types(tStart:tEnd) )
  IF( nError /= NO_ERROR )GOTO 9999
  alev = alev - grdb%maxlev
  DO i = 1,alev
    irv = SAG_Dezone( Aid,0,DezoneCell )
    IF( irv /= SAG_OK )THEN
      CALL exit_dezone()
      nError   = UK_ERROR
      eRoutine = 'MatchDezoneLevels'
      eMessage = 'Error dezoning A grid'
      GOTO 9999
    END IF
  END DO
  CALL exit_dezone()
ELSE IF( grda%maxlev > 0 .AND. grda%maxlev < blev )THEN
  IF( doDep )THEN
    tStart = 1
    tEnd   = bTyp%nDep
  ELSE
    tStart = bTyp%nDep + 1
    tEnd   = aTyp%nTypes
  END IF
  CALL init_dezone( grdb%nvart,bTyp%types(tSTart:tEnd) )
  IF( nError /= NO_ERROR )GOTO 9999
  blev = blev - grda%maxlev
  DO i = 1,blev
    irv = SAG_Dezone( Bid,0,DezoneCell )
    IF( irv /= SAG_OK )THEN
      CALL exit_dezone()
      nError   = UK_ERROR
      eRoutine = 'MatchDezoneLevels'
      eMessage = 'Error dezoning B grid'
      GOTO 9999
    END IF
  END DO
  CALL exit_dezone()
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE SaveTypes( typs )

USE SCIMgr_fd
USE error_fi
USE surface_fi
USE scipuff_fi
USE combine_fd

IMPLICIT NONE

TYPE( typStruct ), INTENT( INOUT ) :: typs

INTEGER n, alloc_stat, i

n = SIZE(srftyp)
ALLOCATE( typs%types(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SaveTypes'
  eMessage = 'Error allocating Type list'
  GOTO 9999
END IF

typs%nTypes = n
typs%nDep   = ntyps
DO i = 1,n
  typs%types(i) = srftyp(i)
END DO

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CreateCombineGrid( Aid,Bid,Cid )

USE sagdef_fd
USE sagstr_fd
USE error_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: Aid, Bid, Cid

INTEGER irv, ios, maxlev, nvart, i, icopy

TYPE( SAGgrid_str ), POINTER :: grda, grdb, grdc

INTEGER, EXTERNAL :: SAG_CopyGridID, SAG_CombineGridID, SAG_FindMaxLevID
INTEGER, EXTERNAL :: CombineNull

IF( Aid > 0 )grda => SAG_PtrGrdStr( Aid ) ! Associate "local" grid structure pointer
grdb => SAG_PtrGrdStr( Bid ) ! Associate "local" grid structure pointer
grdc => SAG_PtrGrdStr( Cid ) ! Associate "local" grid structure pointer

IF( Aid > 0 )THEN
  irv = SAG_CombineGridID( Bid,Aid,CombineNull )
  IF( grdb%maxlev > 0 .OR. grda%maxlev > 0 )THEN
    maxlev = SAG_FindMaxLevID( Bid )
  ELSE
    maxlev = -99
  END IF
ELSE
  maxlev = grdb%maxlev
END IF

CALL ReinitGridID( Cid )

grdc%mxgrd  = grdb%mxgrd
grdc%maxlev = maxlev
ALLOCATE( grdc%ipgrd(grdc%mxgrd),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateCombineGrid'
  eMessage = 'Error allocating combine grid'
  GOTO 9999
END IF

ALLOCATE( grdc%ipdat(grdc%mxgrd*grdc%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateCombineGrid'
  eMessage = 'Error allocating combine data'
  GOTO 9999
END IF
DO i = 1,grdc%mxgrd*grdc%mxfld
  grdc%ipdat(i) = 0.0
END DO
DO i = 1,grdc%mxfld
  grdc%ipflg(i) = 0
END DO

nvart = grdc%nvart

irv = SAG_CopyGridID( Bid,Cid,SAG_COPY_GRID )
DO i = 1,grdb%nvart
  iCopy = SAG_COPY_FIELD + i*SAG_TO_FIELD + i
  IF( irv == SAG_OK )irv = SAG_CopyGridID( Bid,Cid,iCopy )
END DO
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateCombineGrid'
  eMessage = 'Error copying base grid to combine grid'
  GOTO 9999
END IF

grdc%nvart = nvart

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE InitMaxConcGrid( Aid,Bid,Cid )

USE sagdef_fd
USE sagstr_fd
USE error_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: Aid, Bid, Cid

INTEGER irv, ios, maxlev, nvart, i

TYPE( SAGgrid_str ), POINTER :: grda, grdb, grdc

INTEGER, EXTERNAL :: SAG_CopyGridID, SAG_CombineGridID, SAG_FindMaxLevID
INTEGER, EXTERNAL :: CombineNull

IF( Aid > 0 )grda => SAG_PtrGrdStr( Aid ) ! Associate "local" grid structure pointer
grdb => SAG_PtrGrdStr( Bid ) ! Associate "local" grid structure pointer
grdc => SAG_PtrGrdStr( Cid ) ! Associate "local" grid structure pointer

IF( Aid > 0 )THEN
  irv = SAG_CombineGridID( Bid,Cid,CombineNull )
  irv = SAG_CombineGridID( Bid,Aid,CombineNull )
  IF( grdb%maxlev > 0 .OR. grda%maxlev > 0 )THEN
    maxlev = SAG_FindMaxLevID( Bid )
  ELSE
    maxlev = -99
  END IF
  GOTO 9999
ELSE
  maxlev = grdb%maxlev
END IF

CALL ReinitGridID( Cid )

grdc%mxgrd  = grdb%mxgrd
grdc%maxlev = maxlev
ALLOCATE( grdc%ipgrd(grdc%mxgrd),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitMaxConcGrid'
  eMessage = 'Error allocating combine grid'
  GOTO 9999
END IF

ALLOCATE( grdc%ipdat(grdc%mxgrd*grdc%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'InitMaxConcGrid'
  eMessage = 'Error allocating combine data'
  GOTO 9999
END IF
DO i = 1,grdc%mxgrd*grdc%mxfld
  grdc%ipdat(i) = 0.0
END DO
DO i = 1,grdc%mxfld
  grdc%ipflg(i) = 0
END DO

nvart = grdc%nvart

irv = SAG_CopyGridID( Bid,Cid,SAG_COPY_GRID )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'InitMaxConcGrid'
  eMessage = 'Error copying base grid to combine grid'
  GOTO 9999
END IF

grdc%nvart = nvart

9999 CONTINUE

RETURN
END

