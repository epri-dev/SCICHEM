!*******************************************************************************
!            Combine and filter surface dosage or deposition files
!*******************************************************************************
INTEGER FUNCTION CombineAndFilter( primary,secondary,mode,nc,combine,filter,mxCells )

USE SCIMgr_fd
USE error_fi
USE simple_fi
USE scipuff_fi

IMPLICIT NONE

TYPE( projectIDT ),                   INTENT( IN ) :: primary
TYPE( projectIDT ),                   INTENT( IN ) :: secondary
INTEGER,                              INTENT( IN ) :: mode
INTEGER,                              INTENT( IN ) :: nc
TYPE( CombineRecord ), DIMENSION(nc), INTENT( IN ) :: combine
REAL,                                 INTENT( IN ) :: filter
INTEGER,                              INTENT( IN ) :: mxCells

LOGICAL lRadPrj
INTEGER irv

INTEGER, EXTERNAL :: SAG_InitList

!==== Initialize

CALL init_error()

irv = SAG_InitList()

CombineAndFilter = SCIPfailure

!==== Initialze and check input

CALL initialize( primary,secondary,mode,nc,combine,filter,mxCells )
IF( nError /= NO_ERROR )GOTO 9999

!==== Get surface block types for possible dezoning

CALL ReadProject( primary )
IF( nError /= NO_ERROR )GOTO 9999

CALL init_srf_blocks( ntypm )
IF( nError /= NO_ERROR )GOTO 9999

CALL SaveTypes( primaryTypes )
IF( nError /= NO_ERROR )GOTO 9999

CALL scipuff_deallocate()

IF( doCombine )THEN
  CALL ReadProject( secondary )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL init_srf_blocks( ntypm )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL SaveTypes( secondaryTypes )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL scipuff_deallocate()
END IF

!==== Combine

IF( doCombine )THEN
  CALL InitGrid( primaryFile,Pid,primaryTimes )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL InitGrid( secondaryFile,Sid,secondaryTimes )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL combineGrids()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Filter

IF( doFilter )THEN
  CALL InitGrid( primaryFile,Pid,primaryTimes )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL filterGrid()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== set return

CombineAndFilter = SCIPsuccess

9999 CONTINUE

!==== Clean up

CALL DeallocateSimpleData()

RETURN
END

!=============================================================================================

INTEGER FUNCTION SetAEGLFilter( primary,nfilt,filter,is,ie )

USE sagstr_fd
USE sagdef_fd
USE sagerr_fd
USE SCIMgr_fd
USE error_fi
USE simple_fi
USE scipuff_fi
USE files_fi
USE PtrGrdStrItf

IMPLICIT NONE

TYPE( projectIDT ),     INTENT( IN  ) :: primary
INTEGER,                INTENT( IN  ) :: nfilt
REAL, DIMENSION(nfilt), INTENT( IN  ) :: filter
INTEGER,                INTENT( OUT ) :: is
INTEGER,                INTENT( OUT ) :: ie

INTEGER irv, grdI, nTime, ios, i, lun
REAL    del

REAL, DIMENSION(:), ALLOCATABLE :: SrfTime

INTEGER, EXTERNAL :: SAG_InitList, SAG_ClearList, SAG_CloseID
INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_OpenID, SAG_ReadHeaderID

CHARACTER(PATH_MAXLENGTH) string
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

TYPE( SAGgrid_str ), POINTER :: grd

string = TRIM(primary%name)
CALL AddPath( string,TRIM(primary%path) )
string = TRIM(AddExtension(string,'dos'))

!==== Initialize

CALL init_error()

irv = SAG_InitList()

SetAEGLFilter = SCIPfailure

irv = SAG_NewGrdStr( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error creating surface grid'
  GOTO 9999
END IF

grd => SAG_PtrGrdStr( grdI )

lun = 10

irv = SAG_InitGridID( string,lun,SAG_GRID_BOTH,0,0,0,grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',string )
  GOTO 9999
END IF

irv = SAG_OpenID( grdI )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error opening SAG file'
  CALL ReportFileName( eInform,'File=',string )
  GOTO 9999
END IF

!------ Read header until EOF

nTime = 0

DO
  irv = SAG_ReadHeaderID( grdI )
  IF( irv /= SAG_OK )THEN
    IF( grd%status == SAG_EOF_REC )EXIT
    nError   = UK_ERROR
    eRoutine = 'SetAEGLFilter'
    eMessage = 'Error reading SAG times'
    CALL ReportFileName( eInform,'File=',string )
    GOTO 9999
  END IF
  nTime = nTime + 1
END DO

irv = SAG_CloseID( grdI ) ! close so can read again

!------ Allocate array for plot times

ALLOCATE( SrfTime(nTime),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error allocating surface time list'
  GOTO 9999
END IF

!------ Read times from header

DO i = 1,nTime
  irv = SAG_ReadHeaderID( grdI )
  SrfTime(i) = grd%time
END DO

!--- Check for uniform spacing

IF( nTime > 2 )THEN
  del = SrfTime(1)
  DO i = 2,nTime-1
    IF( ABS(SrfTime(i)-SrfTime(i-1)-del) > 0.001*del )THEN
      nError   = UK_ERROR
      eRoutine = 'SetAEGLFilter'
      eMessage = 'Error: non-constant time interval'
      CALL ReportFileName( eInform,'File=',string )
      GOTO 9999
    END IF
  END DO
ELSE
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error: less than 3 times available'
  CALL ReportFileName( eInform,'File=',string )
  GOTO 9999
END IF

is = 0
DO i = 1,nfilt
  IF( del > 0.5*filter(i) )THEN
    CYCLE
  ELSE
    is = i
    EXIT
  END IF
END DO

IF( is == 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetAEGLFilter'
  eMessage = 'Error: time interval too large for any filter'
  CALL ReportFileName( eInform,'File=',string )
  GOTO 9999
END IF

ie = is
DO i = is+1,nfilt
  IF( SrfTime(nTime) > 2.*filter(i) )THEN
    ie = i
  END IF
END DO

SetAEGLFilter = SCIPsuccess

9999 CONTINUE

irv = SAG_ClearList()

RETURN
END

!=============================================================================================

INTEGER FUNCTION FilterMaxConc( primary,nfilt,filter,prob )

USE SCIMgr_fd
USE error_fi
USE simple_fi
USE scipuff_fi
USE surface_fi
USE srfparam_fd

IMPLICIT NONE

TYPE( projectIDT ),     INTENT( IN ) :: primary
INTEGER,                INTENT( IN ) :: nfilt
REAL, DIMENSION(nfilt), INTENT( IN ) :: filter
REAL,                   INTENT( IN ) :: prob

LOGICAL lRadPrj
INTEGER irv, i, nb

INTEGER, EXTERNAL :: SAG_InitList

!==== Initialize

CALL init_error()

irv = SAG_InitList()

FilterMaxConc = SCIPfailure

!==== Initialize and check input

CALL initMaxConc( primary,filter(1),prob )
IF( nError /= NO_ERROR )GOTO 9999

!==== Get surface block types for possible dezoning

CALL ReadProject( primary )
IF( nError /= NO_ERROR )GOTO 9999

CALL init_srf_blocks( ntypm )
IF( nError /= NO_ERROR )GOTO 9999

!---- Check for surface dosage blocks

IF( nsrf_blocks-ndep_blocks > 0 )THEN
  nb = 0
  DO i = ndep_blocks+1,nsrf_blocks
    IF( srf_block(i)%type == SBLK_STD )THEN
      nb = nb + 1
    END IF
    IF( nb == 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'FilterMaxConc'
      eMessage = 'Invalid project'
      eInform  = 'No material dosage blocks found'
      GOTO 9999
    END IF
  END DO
ELSE
  nError   = IV_ERROR
  eRoutine = 'FilterMaxConc'
  eMessage = 'Invalid project'
  eInform  = 'No dosage blocks found'
  GOTO 9999
END IF

CALL SaveTypes( primaryTypes )
IF( nError /= NO_ERROR )GOTO 9999

!==== Filter

CALL InitGrid( primaryFile,Pid,primaryTimes )
IF( nError /= NO_ERROR )GOTO 9999

CALL filterMaxConcGrid( nfilt,filter )
IF( nError /= NO_ERROR )GOTO 9999

!==== set return

FilterMaxConc = SCIPsuccess

9999 CONTINUE

!==== Clean up

CALL DeallocateMaxConcData()

RETURN
END

!=============================================================================================

SUBROUTINE initialize( primary,secondary,mode,nc,combine,filter,mxCells )

USE SCIMgr_fd
USE simple_fi
USE error_fi

IMPLICIT NONE

TYPE( projectIDT ),                   INTENT( IN ) :: primary
TYPE( projectIDT ),                   INTENT( IN ) :: secondary
INTEGER,                              INTENT( IN ) :: mode
INTEGER,                              INTENT( IN ) :: nc
TYPE( CombineRecord ), DIMENSION(nc), INTENT( IN ) :: combine
REAL,                                 INTENT( IN ) :: filter
INTEGER,                              INTENT( IN ) :: mxCells

INTEGER ios, i
CHARACTER(4) ext

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension
INTEGER,                   EXTERNAL :: sysCheckFile
INTEGER,                   EXTERNAL :: sysDeleteFile

maxCells = mxCells
doCombine = BTEST(mode, SCB_COMBINE)
doFilter  = BTEST(mode, SCB_FILTER)
doDep     = BTEST(mode, SCB_DEP)
doMaximize= BTEST(mode, SCB_MAXIMIZE)

IF( .NOT.(doCombine .OR. doFilter) )THEN
  nError   = IV_ERROR
  eRoutine = 'Initialize'
  eMessage = 'Invalid mode'
  eInform  = 'Mode must include at least one of (SC_COMBINE,SC_FILTER)'
  GOTO 9999
END IF

IF( doDEP )THEN
  ext = 'dep'
ELSE
  ext = 'dos'
END IF

primaryFile = TRIM( AddExtension( primary%name,TRIM(ext) ) )
CALL AddPath( primaryFile,primary%path )
ios = sysCheckFile( primaryFile )
IF( ios /= SCIPsuccess )THEN
  IF( ios == SCIPnull )THEN
    eMessage = 'Invalid primary file specification: path not found'
  ELSE
    eMessage = 'Invalid primary file specification: file not found'
  END IF
  nError   = NF_ERROR
  eRoutine = 'Initialize'
  CALL ReportFileName( eInform,'File=',primaryFile )
  GOTO 9999
END IF

processedFile = TRIM( AddExtension( primaryFile,'tmp' ) )
ios = sysCheckFile( processedFile )
IF( ios == SCIPsuccess )THEN
  ios = sysDeleteFile( processedFile )
  IF( ios == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Error removing existing combined grid'
    CALL ReportFileName( eInform,'File=',processedFile )
    GOTO 9999
  END IF
END IF

backupFile = TRIM( AddExtension( primaryFile,'last' ) )
ios = sysCheckFile( backupFile )
IF( ios == SCIPsuccess )THEN
  ios = sysDeleteFile( backupFile )
  IF( ios == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Error removing existing backup grid'
    CALL ReportFileName( eInform,'File=',backupFile )
    GOTO 9999
  END IF
END IF

IF( doFilter )THEN
  IF( filter <= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Invalid filter time'
    eInform  = 'Filter time must be greater than 0.0'
    GOTO 9999
  ELSE
    filterTime = filter
  END IF
ELSE
  filterTime = 0.0
END IF

IF( doCombine )THEN
  secondaryFile = TRIM( AddExtension( secondary%name,TRIM(ext) ) )
  CALL AddPath( secondaryFile,secondary%path )
  ios = sysCheckFile( secondaryFile )
  IF( ios /= SCIPsuccess )THEN
    IF( ios == SCIPnull )THEN
      eMessage = 'Invalid secondary file specification: path not found'
    ELSE
      eMessage = 'Invalid secondary file specification: file not found'
    END IF
    nError   = NF_ERROR
    eRoutine = 'Initialize'
    CALL ReportFileName( eInform,'File=',secondaryFile )
    GOTO 9999
  END IF
  IF( nc <= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Invalid number of combine records'
    eInform  = 'Must specify at least one combine record'
    GOTO 9999
  ELSE
    nCombine = nc
  END IF
  ALLOCATE( combineRec(nCombine),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Error allocating space for combine records'
    WRITE(eInform,'(A,I5)')'IOS = ',ios
    GOTO 9999
  END IF
  DO i = 1,nCombine
    IF( LEN_TRIM(combine(i)%primaryField) <= 0 )THEN
      nError   = IV_ERROR
      eRoutine = 'Initialize'
      eMessage = 'Invalid combine record'
      eInform  = 'Missing primary field name'
      GOTO 9999
    END IF
    IF( LEN_TRIM(combine(i)%secondaryField) <= 0 )THEN
      nError = IV_ERROR
      eRoutine = 'Initialize'
      eMessage = 'Invalid combine record'
      eInform  = 'Missing secondary field name'
      GOTO 9999
    END IF
    IF( combine(i)%primaryFactor == 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'Initialize'
      eMessage = 'Invalid combine record'
      eInform  = 'Primary field factor must not be 0.0'
      GOTO 9999
    END IF
    IF( combine(i)%secondaryFactor == 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'Initialize'
      eMessage = 'Invalid combine record'
      eInform  = 'Secondary field factor must not be 0.0'
      GOTO 9999
    END IF
    IF( ABS(combine(i)%correlation) > 1.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'Initialize'
      eMessage = 'Invalid combine record'
      eInform  = 'Correlation factor must be in range [-1.0,1.0]'
      GOTO 9999
    END IF
    combineRec(i) = combine(i)
  END DO
ELSE
  nCombine = 0
  secondaryFile = ' '
  IF( ALLOCATED(combineRec) )DEALLOCATE( combineRec,STAT=ios )
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE initMaxConc( primary,filter,prob )

USE SCIMgr_fd
USE simple_fi
USE error_fi

IMPLICIT NONE

TYPE( projectIDT ), INTENT( IN ) :: primary
REAL,               INTENT( IN ) :: filter
REAL,               INTENT( IN ) :: prob

INTEGER ios

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension
INTEGER,                   EXTERNAL :: sysCheckFile
INTEGER,                   EXTERNAL :: sysDeleteFile

maxCells   = 0
doCombine  = .FALSE.
doFilter   = .TRUE.
doDep      = .FALSE.
doMaximize = .TRUE.

pExceed = 0.01*prob

primaryFile = TRIM( AddExtension( primary%name,'dos' ) )
CALL AddPath( primaryFile,primary%path )
ios = sysCheckFile( primaryFile )
IF( ios /= SCIPsuccess )THEN
  IF( ios == SCIPnull )THEN
    eMessage = 'Invalid primary file specification: path not found'
  ELSE
    eMessage = 'Invalid primary file specification: file not found'
  END IF
  nError   = NF_ERROR
  eRoutine = 'Initialize'
  CALL ReportFileName( eInform,'File=',primaryFile )
  GOTO 9999
END IF

processedFile = TRIM( AddExtension( primary%name,'ave' ) )
CALL AddPath( processedFile,primary%path )
ios = sysCheckFile( processedFile )
IF( ios == SCIPsuccess )THEN
  ios = sysDeleteFile( processedFile )
  IF( ios == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Error removing existing combined grid'
    CALL ReportFileName( eInform,'File=',processedFile )
    GOTO 9999
  END IF
END IF

IF( doFilter )THEN
  IF( filter <= 0.0 )THEN
    nError   = IV_ERROR
    eRoutine = 'Initialize'
    eMessage = 'Invalid filter time'
    eInform  = 'Filter time must be greater than 0.0'
    GOTO 9999
  ELSE
    filterTime = filter
  END IF
ELSE
  filterTime = 0.0
END IF

nCombine = 0

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE DeallocateSimpleData()

USE SCIMgr_fd
USE simple_fi
USE error_fi

IMPLICIT NONE

INTEGER alloc_stat
INTEGER irv

INTEGER, EXTERNAL :: SAG_ClearList, SYSDeleteFile, SAG_CLoseID

irv = SAG_CloseID( Pid )
irv = SAG_CloseID( Sid )
irv = SAG_CloseID( Fid )

CALL scipuff_deallocate()

IF( ALLOCATED(filterTimes) )DEALLOCATE( filterTimes,STAT=alloc_stat )
IF( ALLOCATED(combineRec)  )DEALLOCATE( combineRec, STAT=alloc_stat )
IF( ALLOCATED(combineBlk)  )DEALLOCATE( combineBlk, STAT=alloc_stat )
IF( ASSOCIATED(primaryTimes%times)   )DEALLOCATE( primaryTimes%times,  STAT=alloc_stat )
IF( ASSOCIATED(secondaryTimes%times) )DEALLOCATE( secondaryTimes%times,STAT=alloc_stat )
IF( ASSOCIATED(primaryTypes%types)   )DEALLOCATE( primaryTypes%types,  STAT=alloc_stat )
IF( ASSOCIATED(secondaryTypes%types) )DEALLOCATE( secondaryTypes%types,STAT=alloc_stat )

irv = sysDeleteFile( processedFile )
IF( irv == SCIPfailure )THEN
  nError   = UK_ERROR
  eRoutine = 'DeallocateData'
  eMessage = TRIM(eMessage)//':Error removing combined grid after failure'
END IF

irv = SAG_ClearList()

RETURN
END

!=============================================================================================

SUBROUTINE DeallocateMaxConcData()

USE SCIMgr_fd
USE simple_fi
USE error_fi

IMPLICIT NONE

INTEGER alloc_stat, irv

INTEGER, EXTERNAL :: SAG_ClearList

CALL scipuff_deallocate()

IF( ALLOCATED(filterTimes)         )DEALLOCATE( filterTimes,       STAT=alloc_stat )
IF( ASSOCIATED(primaryTimes%times) )DEALLOCATE( primaryTimes%times,STAT=alloc_stat )
IF( ASSOCIATED(primaryTypes%types) )DEALLOCATE( primaryTypes%types,STAT=alloc_stat )

irv = SAG_ClearList()

RETURN
END

!=============================================================================================

SUBROUTINE combineGrids()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

INTEGER i, irv

INTEGER, EXTERNAL :: SAG_CloseID, SAG_WriteBreakID, SAG_RmvGrdStr

CALL CheckCombine()
IF( nError /= NO_ERROR )GOTO 9999

CALL CreateProcessGrid()
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,primaryTimes%nTimes

  CALL UpdateGrids( primaryTimes%times(i) )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL MatchDezoneLevels( Sid,secondaryTypes,Pid,primaryTypes,doDep )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL CreateCombineGrid( Sid,Pid,Fid )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL DoCombineGrid()
  IF( nError /= NO_ERROR )GOTO 9999

  irv = SAG_WriteBreakID( Fid,primaryTimes%times(i) )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'combineGrids'
    eMessage = 'Error writing combine grid to file'
    GOTO 9999
  END IF

END DO

irv = SAG_CloseID( Pid )
irv = SAG_CloseID( Sid )
irv = SAG_CloseID( Fid )

irv = SAG_RmvGrdStr( Pid )
irv = SAG_RmvGrdStr( Sid )
irv = SAG_RmvGrdStr( Fid )

CALL CopyCombineGrid()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CopyCombineGrid()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: sysMoveFile

!--- Move current file for safe keeping

irv = sysMoveFile( primaryFile,backupFile )
IF( irv == SCIPfailure )THEN
  nError   = UK_ERROR
  eRoutine = 'CopyCombineGrid'
  eMessage = 'Error backing up primary file'
  GOTO 9999
END IF

!--- Move comb file to primary

irv = sysMoveFile( processedFile,primaryFile )
IF( irv == SCIPfailure )THEN
  nError   = UK_ERROR
  eRoutine = 'CopyCombineGrid'
  eMessage = 'Error replacing primary file with combined file'
  irv = sysMoveFile( backupFile,primaryFile )
  IF( irv == SCIPfailure )THEN
    eMessage = TRIM(eMessage)//':Error restoring primary file after error'
  END IF
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE UpdateGrids( tim )

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: tim

CHARACTER(4), DIMENSION(1) :: names

INTEGER irv

INTEGER, EXTERNAL :: SAG_ReadNextBreakID, SAG_FindTime

names(1) = ' '

CALL ReinitGridID( Pid )
IF( nError /= NO_ERROR )GOTO 9999

irv = SAG_ReadNextBreakID( Pid,-1,names )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateGrids'
  eMessage = 'Error reading primary file'
  GOTO 9999
END IF

CALL ReinitGridID( Sid )
IF( nError /= NO_ERROR )GOTO 9999

irv = SAG_FindTime( Sid,tim  )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateGrids'
  eMessage = 'Error finding time on secondary file'
  GOTO 9999
END IF

irv = SAG_ReadNextBreakID( Sid,-1,names )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateGrids'
  eMessage = 'Error reading secondary file'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE DoCombineGrid()

USE simple_fi
USE sagdef_fd
USE sagstr_fd
USE error_fi
USE PtrGrdStrItf
USE dezone_fi
USE surface_fi

IMPLICIT NONE

INTEGER irv, i, j, ic, tStart, tEnd
REAL    x, y, h

TYPE( SAGgrid_str ), POINTER :: grdP, grdS, grdF

INTEGER, EXTERNAL :: SAG_CopyGridID, SAG_CombineGridID, SAG_FindMaxLevID, SAG_Dezone
INTEGER, EXTERNAL :: CombineNull, DezoneCell

INTERFACE
  RECURSIVE SUBROUTINE CombineCell( x,y,h,ic,grdc,ia,grda )
    USE sagstr_fd
    REAL,               INTENT( IN ) :: x, y, h
    TYPE( SAGgrid_str ), POINTER     :: grda, grdc
    INTEGER,            INTENT( IN ) :: ic, ia
  END SUBROUTINE

  INTEGER FUNCTION SAG_BottomValue( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomValue
END INTERFACE

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer
grdS => SAG_PtrGrdStr( Sid ) ! Associate "local" grid structure pointer
grdF => SAG_PtrGrdStr( Fid ) ! Associate "local" grid structure pointer

!---- Push combined fields to bottom, if required

DO i = 1,grdF%nvart
  IF( combineBlk(i)%Sid > 0 .AND. combineBlk(i)%push )THEN

    irv = SAG_BottomValue( grdF,i )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'CombineGrids'
      eMessage = 'Error pushing primary grid'
      GOTO 9999
    END IF

    irv = SAG_BottomValue( grdS,combineBlk(i)%Sid )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'CombineGrids'
      eMessage = 'Error pushing secondary grid'
      GOTO 9999
    END IF

  END IF
END DO

!---- Combine grids

DO i = 1,grdF%nx*grdF%ny
  h = 1.0
  x = FLOAT(MOD(i,grdF%nx)) - 0.5
  y = FLOAT((i-1)/grdF%nx) + 0.5
  CALL CombineCell( x,y,h,i,grdF,i,grdS )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

IF( maxCells > 0 )THEN
  IF( grdF%ncells > maxCells )THEN
    IF( doDep )THEN
      tStart = 1
      tEnd   = primaryTypes%nDep
    ELSE
      tStart = primaryTypes%nDep + 1
      tEnd   = primaryTypes%nTypes
    END IF
    CALL init_dezone( grdF%nvart,primaryTypes%types(tStart:tEnd) )
    IF( nError /= NO_ERROR )GOTO 9999
    DO WHILE( grdF%ncells > maxCells )
      irv = SAG_Dezone( Fid,0,DezoneCell )
      IF( irv /= SAG_OK )THEN
        CALL exit_dezone()
        nError   = UK_ERROR
        eRoutine = 'CombineGrids'
        eMessage = 'Error dezoning combined grid'
        GOTO 9999
      END IF
    END DO
    CALL exit_dezone()
  END IF
END IF

!---- Check combined fields, if subtracting

DO j = 1,grdF%nvart
  IF( combineBlk(j)%Sid > 0 .AND. combineBlk(j)%push )THEN

!----- Ensure bottom cells are positive

    ic = (j-1)*grdF%mxgrd
    DO i = 1,grdF%ncells
      ic = ic + 1
      IF( grdF%ipdat(ic) < 0.0 )THEN
        grdF%ipdat(ic) = 0.0
      END IF
    END DO

  END IF
END DO

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE DoMaxConcGrid( n,nfilt )

USE simple_fi
USE sagdef_fd
USE sagstr_fd
USE error_fi
USE PtrGrdStrItf
USE dezone_fi
USE surface_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: n          !Filter ID
INTEGER, INTENT( IN ) :: nfilt      !Number of Filters

INTEGER irv, i, j, ic, tStart, Tend
REAL    x, y, h

TYPE( SAGgrid_str ), POINTER :: grdP, grdS, grdF

INTEGER, EXTERNAL :: SAG_CopyGridID, SAG_CombineGridID, SAG_FindMaxLevID, SAG_Dezone
INTEGER, EXTERNAL :: CombineNull, DezoneCell

INTERFACE
  RECURSIVE SUBROUTINE AveConcCell( x,y,h,ia,grda,ib,grdb )
    USE sagstr_fd
    REAL,               INTENT( IN ) :: x, y, h
    TYPE( SAGgrid_str ), POINTER     :: grda, grdb
    INTEGER,            INTENT( IN ) :: ia, ib
  END SUBROUTINE

  RECURSIVE SUBROUTINE MaxConcCell( x,y,h,ic,grdc,ia,grda,n,nfilt )
    USE sagstr_fd
    REAL,               INTENT( IN ) :: x, y, h
    TYPE( SAGgrid_str ), POINTER     :: grda, grdc
    INTEGER,            INTENT( IN ) :: ic,ia
    INTEGER,            INTENT( IN ) :: n, nfilt
  END SUBROUTINE

  INTEGER FUNCTION SAG_BottomValue( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomValue
END INTERFACE

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer
grdS => SAG_PtrGrdStr( Sid ) ! Associate "local" grid structure pointer
grdF => SAG_PtrGrdStr( Fid ) ! Associate "local" grid structure pointer

!---- Combine grids

DO i = 1,grdP%nx*grdP%ny
  h = 1.0
  x = FLOAT(MOD(i,grdF%nx)) - 0.5
  y = FLOAT((i-1)/grdF%nx) + 0.5
  CALL AveConcCell( x,y,h,i,grdS,i,grdP )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

DO i = 1,grdP%nblk
  irv = SAG_BottomValue( grdP,grdP%ipblk(i)%ifld )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CombineGrids'
    eMessage = 'Error pushing primary grid'
    GOTO 9999
  END IF
  irv = SAG_BottomValue( grdP,grdP%ipblk(i)%ifld+1 )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CombineGrids'
    eMessage = 'Error pushing primary grid'
    GOTO 9999
  END IF
END DO

DO i = 1,grdF%nvart
  irv = SAG_BottomValue( grdF,i )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'CombineGrids'
    eMessage = 'Error pushing output grid'
    GOTO 9999
  END IF
END DO

DO i = 1,grdF%nx*grdF%ny
  h = 1.0
  x = FLOAT(MOD(i,grdF%nx)) - 0.5
  y = FLOAT((i-1)/grdF%nx) + 0.5
  CALL MaxConcCell( x,y,h,i,grdF,i,grdP,n,nfilt )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

IF( maxCells > 0 )THEN
  IF( grdF%ncells > maxCells )THEN
    IF( doDep )THEN
      tStart = 1
      tEnd   = primaryTypes%nDep
    ELSE
      tStart = primaryTypes%nDep + 1
      tEnd   = primaryTypes%nTypes
    END IF
    CALL init_dezone( grdF%nvart,primaryTypes%types(tStart:tEnd) )
    IF( nError /= NO_ERROR )GOTO 9999
    DO WHILE( grdF%ncells > maxCells )
      irv = SAG_Dezone( Fid,0,DezoneCell )
      IF( irv /= SAG_OK )THEN
        CALL exit_dezone()
        nError   = UK_ERROR
        eRoutine = 'CombineGrids'
        eMessage = 'Error dezoning combined grid'
        GOTO 9999
      END IF
    END DO
    CALL exit_dezone()
  END IF
END IF

!---- Check combined fields, if subtracting

DO j = 1,grdF%nvart
!----- Ensure bottom cells are positive

  ic = (j-1)*grdF%mxgrd
  DO i = 1,grdF%ncells
    ic = ic + 1
    IF( grdF%ipdat(ic) < 0.0 )THEN
      grdF%ipdat(ic) = 0.0
    END IF
  END DO

END DO

9999 CONTINUE

RETURN
END

!=============================================================================================

RECURSIVE SUBROUTINE CombineCell( x,y,h,ic,grdF,is,grdS )

USE simple_fi
USE sagstr_fd
USE error_fi

IMPLICIT NONE

REAL,               INTENT( IN ) :: x, y, h
TYPE( SAGgrid_str ), POINTER     :: grdS, grdF
INTEGER,            INTENT( IN ) :: ic, is

INTEGER js, jc, i, j, k, ks, kc

REAL(8) vs, vf
REAL    xx, yy, hh

IF( doMaximize )THEN
  DO i = 1,grdF%nvart
    IF( combineBlk(i)%Sid > 0 )THEN
      IF( combineBlk(i)%Pblk > 0 )THEN
        jc = (i-1)*grdF%mxgrd + ic
        js = (combineBlk(i)%Sid-1)*grdS%mxgrd + is
        vf = DBLE(grdF%ipdat(jc))
        vs = DBLE(grdS%ipdat(js))
        IF( vs > vf )THEN
          grdF%ipdat(jc) = grdS%ipdat(js)
          DO k = 1,grdF%nvart
            IF( -combineBlk(k)%Pblk == combineBlk(i)%Pblk )THEN
              kc = (k-1)*grdF%mxgrd + ic
              ks = (combineBlk(k)%Sid-1)*grdS%mxgrd + is
              grdF%ipdat(kc) = grdS%ipdat(ks)
            END IF
          END DO
        END IF
      END IF
    END IF
  END DO
ELSE
  DO i = 1,grdF%nvart
    IF( combineBlk(i)%Sid > 0 )THEN
      jc = (i-1)*grdF%mxgrd + ic
      js = (combineBlk(i)%Sid-1)*grdS%mxgrd + is
      vf = DBLE(grdF%ipdat(jc))
      vs = DBLE(grdS%ipdat(js))
      IF( combineBlk(i)%cFactor /= 0.0 )THEN
        grdF%ipdat(jc) = combineBlk(i)%pFactor*vf + combineBlk(i)%sFactor*vs + combineBlk(i)%cFactor*SQRT(vf*vs)
      ELSE
        grdF%ipdat(jc) = combineBlk(i)%pFactor*vf + combineBlk(i)%sFactor*vs
      END IF
    END IF
  END DO
END IF

IF( grdF%ipgrd(ic) > 0 )THEN
  IF( grdS%ipgrd(is) <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CombineCell'
    eMessage = 'Primary grid refined but secondary grid is not'
    GOTO 9999
  ELSE
    jc = grdF%ipgrd(ic)
    js = grdS%ipgrd(is)
    hh = 0.5*h
    DO j = 1,4
      xx = x + 0.5*hh - FLOAT(MOD(j,2))*hh
      yy = y - 0.5*hh + FLOAT((j-1)/2)*hh
      CALL CombineCell( xx,yy,hh,jc,grdF,js,grdS )
      jc = jc + 1
      js = js + 1
    END DO
  END IF
ELSE
  IF( grdS%ipgrd(is) > 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'CombineCell'
    eMessage = 'Secondary grid refined but primary grid is not'
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

RECURSIVE SUBROUTINE AveConcCell( x,y,h,is,grdS,ip,grdP )

USE simple_fi
USE sagstr_fd
USE error_fi

IMPLICIT NONE

REAL,               INTENT( IN ) :: x,y,h
TYPE( SAGgrid_str ), POINTER     :: grdS, grdP
INTEGER,            INTENT( IN ) :: is,ip

INTEGER jp, js, i, j
REAL    xx, yy, hh, Tavg

Tavg = FilterTime*3600.0

DO i = 1,grdP%nblk
  jp = (grdP%ipblk(i)%ifld-1)*grdP%mxgrd + ip
  js = (grdP%ipblk(i)%ifld-1)*grdS%mxgrd + is
  grdP%ipdat(jp) = (grdP%ipdat(jp) - grdS%ipdat(js))/Tavg
  jp = jp + grdP%mxgrd
  js = js + grdS%mxgrd
  grdP%ipdat(jp) = (grdP%ipdat(jp) - grdS%ipdat(js))/Tavg/Tavg
END DO

IF( grdP%ipgrd(ip) > 0 )THEN
  IF( grdS%ipgrd(is) <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AveConcCell'
    eMessage = 'Primary grid refined but secondary grid is not'
    GOTO 9999
  ELSE
    jp = grdP%ipgrd(ip)
    js = grdS%ipgrd(is)
    hh = 0.5*h
    DO j = 1,4
      xx = x + 0.5*hh - FLOAT(MOD(j,2))*hh
      yy = y - 0.5*hh + FLOAT((j-1)/2)*hh
      CALL AveConcCell( xx,yy,hh,js,grdS,jp,grdP )
      jp = jp + 1
      js = js + 1
    END DO
  END IF
ELSE
  IF( grdS%ipgrd(is) > 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AveConcCell'
    eMessage = 'Secondary grid refined but primary grid is not'
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

RECURSIVE SUBROUTINE MaxConcCell( x,y,h,ic,grdF,ip,grdP,n,nfilt )

USE simple_fi
USE sagstr_fd
USE error_fi
USe surface_fi
USE srfparam_fd
USE scipuff_fi
USE constants_fd

IMPLICIT NONE

REAL,               INTENT( IN ) :: x, y, h
TYPE( SAGgrid_str ), POINTER     :: grdF, grdP
INTEGER,            INTENT( IN ) :: ic, ip
INTEGER,            INTENT( IN ) :: n, nfilt

INTEGER jp, jc, i, j, nblk, nvar, ioff, jblk
REAL    xx, yy, hh, sig, v1, gamma, soc, gbar, sigg

REAL, EXTERNAL :: erfci, erfc

nblk = grdF%nblk/nfilt/2     !No. of output block per filter time
nvar = grdF%nvart/nfilt      !No. of out fields per filter time

ioff = (n-1)*nvar            !Offset for output field
jblk = ndep_blocks           !Offset for input grid block

jc = (ioff-1)*grdF%mxgrd + ic

DO i = 1,nblk
  jc = jc + grdF%mxgrd
  jblk = jblk + 1
  DO WHILE( srf_block(jblk)%type < 0 )
    jblk = jblk + 1
  END DO
  jp = (grdP%ipblk(jblk-ndep_blocks)%ifld-1)*grdP%mxgrd + ip
  IF( grdP%ipdat(jp) > grdF%ipdat(jc) )THEN
    grdF%ipdat(jc) = grdP%ipdat(jp)
    jc = jc + grdF%mxgrd
    grdF%ipdat(jc) = grdP%ipdat(jp+grdP%mxgrd)
  ELSE
    jc = jc + grdF%mxgrd
  END IF
  jc = jc + grdF%mxgrd
  grdF%ipdat(jc) = grdP%ipdat(jp)
  jc = jc + grdF%mxgrd
  grdF%ipdat(jc) = grdP%ipdat(jp+grdP%mxgrd)
END DO

IF( grdF%ipgrd(ic) > 0 )THEN
  IF( grdP%ipgrd(ip) <= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'MaxConcCell'
    eMessage = 'Primary grid refined but secondary grid is not'
    GOTO 9999
  ELSE
    jc = grdF%ipgrd(ic)
    jp = grdP%ipgrd(ip)
    hh = 0.5*h
    DO j = 1,4
      xx = x + 0.5*hh - FLOAT(MOD(j,2))*hh
      yy = y - 0.5*hh + FLOAT((j-1)/2)*hh
      CALL MaxConcCell( xx,yy,hh,jc,grdF,jp,grdP,n,nfilt )
      jc = jc + 1
      jp = jp + 1
    END DO
  END IF
ELSE
  IF( grdP%ipgrd(ip) > 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'MaxConcCell'
    eMessage = 'Secondary grid refined but primary grid is not'
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CheckCombine()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE srfparam_fd

IMPLICIT NONE

INTEGER i, j, kp, ks, ios, iblk, jblk, tp, ts
REAL    delta

TYPE( SAGgrid_str ), POINTER :: grdP, grdS

INTEGER, EXTERNAL :: NearestTime

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer
grdS => SAG_PtrGrdStr( Sid ) ! Associate "local" grid structure pointer

!---- Check grids

IF( grdP%nx /= grdS%nx .OR. grdP%ny /= grdS%ny )THEN
  nError   = SZ_ERROR
  eRoutine = 'CheckCombine'
  eMessage = 'Grid mismatch: Number of points'
  WRITE(eInform,'(A,I3,A,I3)')'Primary   grid:',grdP%nx,' X ',grdP%ny
  WRITE(eAction,'(A,I3,A,I3)')'Secondary grid:',grdS%nx,' X ',grdS%ny
  GOTO 9999
END IF

IF( grdP%xmin /= grdS%xmin .OR. grdP%ymin /= grdS%ymin )THEN
  nError   = SZ_ERROR
  eRoutine = 'CheckCombine'
  eMessage = 'Grid mismatch: Grid location'
  WRITE(eInform,'(A,F10.3,A,F10.3)')'Primary   grid:',grdP%xmin,' , ',grdP%ymin
  WRITE(eAction,'(A,F10.3,A,F10.3)')'Secondary grid:',grdS%xmin,' , ',grdS%ymin
  GOTO 9999
END IF

IF( grdP%dx /= grdS%dx .OR. grdP%dy /= grdS%dy )THEN
  nError = SZ_ERROR
  eRoutine = 'CheckCombine'
  eMessage = 'Grid mismatch: Grid size'
  WRITE(eInform,'(A,F10.3,A,F10.3)')'Primary   grid:',grdP%dx,' , ',grdP%dy
  WRITE(eAction,'(A,F10.3,A,F10.3)')'Secondary grid:',grdS%dx,' , ',grdS%dy
  GOTO 9999
END IF

!---- Check times

DO i = 1,primaryTimes%ntimes
  j = NearestTime( secondaryTimes%times,secondaryTimes%ntimes,primaryTimes%times(i) )
  delta = ABS(primaryTimes%times(i)-secondaryTimes%times(j))
  IF( delta/primaryTimes%times(i) > TIME_EPS )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckCombine'
    eMessage = 'Time mismatch: Unable to locate appropriate time on secondary file'
    WRITE(eInform,'(A,F10.3)')'Primary   time:',primaryTimes%times(i)
    WRITE(eAction,'(A,F10.3)')'Secondary time:',secondaryTimes%times(j)
    GOTO 9999
  END IF
END DO

!---- Check blocks and create combine parameters

ALLOCATE( combineBlk(grdP%nvart),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CheckCombine'
  eMessage = 'Error allocating space for combine blocks'
  WRITE(eInform,'(A,I5)')'IOS = ',ios
  GOTO 9999
END IF

DO i = 1,grdP%nvart
  combineBlk(i)%Sid     = 0
  combineBlk(i)%Pblk    = 0
  combineBlk(i)%pFactor = 0.0
  combineBlk(i)%sFactor = 0.0
  combineBlk(i)%cFactor = 0.0
  combineBlk(i)%push    = .FALSE.
  combineBlk(i)%dummy   = 0
END DO

DO i = 1,nCombine
  DO iblk = 1,grdP%nblk
    IF( TRIM(grdP%ipblk(iblk)%name) == TRIM(combineRec(i)%primaryField) )EXIT
  END DO
  IF( iblk > grdP%nblk )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckCombine'
    eMessage = 'Block mismatch: Unable to locate primary field'
    eInform  = 'Primary field: '//TRIM(combineRec(i)%primaryField)
    GOTO 9999
  END IF
  DO jblk = 1,grdS%nblk
    IF( TRIM(grdS%ipblk(jblk)%name) == TRIM(combineRec(i)%secondaryField) )EXIT
  END DO
  IF( jblk > grdS%nblk )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckCombine'
    eMessage = 'Block mismatch: Unable to locate secondary field'
    eInform  = 'Secondary field: '//TRIM(combineRec(i)%secondaryField)
    GOTO 9999
  END IF
  IF( grdP%ipblk(iblk)%nfld /= grdS%ipblk(jblk)%nfld )THEN
    nError   = IV_ERROR
    eRoutine = 'CheckCombine'
    eMessage = 'Type mismatch: Primary and secondary fields are of different sizes'
    WRITE(eInform,'(A,I7)')'Primary   nfld:',grdP%ipblk(iblk)%nfld
    WRITE(eAction,'(A,I7)')'Secondary nfld:',grdS%ipblk(jblk)%nfld
    GOTO 9999
  END IF
  DO j = 1,grdP%ipblk(iblk)%nfld
    kp = grdP%ipblk(iblk)%ifld + j - 1
    ks = grdS%ipblk(jblk)%ifld + j - 1
    IF( doDep )THEN
      tp = kp
      ts = ks
    ELSE
      tp = kp + primaryTypes%nDep
      ts = ks + secondaryTypes%nDep
    END IF
    IF( secondaryTypes%types(ts) /= primaryTypes%types(tp) )THEN
      nError   = IV_ERROR
      eRoutine = 'CheckCombine'
      eMessage = 'Type mismatch: Primary and secondary fields are of different types'
      WRITE(eInform,'(A,I7)')'Primary   type:',primaryTypes%types(tp)
      WRITE(eAction,'(A,I7)')'Secondary type:',secondaryTypes%types(ts)
      GOTO 9999
    END IF
    IF( grdP%ipblk(iblk)%fldnam(j) == 'Mean' )THEN
      combineBlk(kp)%Pblk = iblk
    ELSE
      combineBlk(kp)%Pblk = -iblk
    END IF
    combineBlk(kp)%Sid = ks
    SELECT CASE( primaryTypes%types(tp) )
      CASE( DEZONE_MEAN )
        combineBlk(kp)%pFactor = combineRec(i)%primaryFactor
        combineBlk(kp)%sFactor = combineRec(i)%secondaryFactor
        combineBlk(kp)%cFactor = 0.0
      CASE( DEZONE_VAR )
        combineBlk(kp)%pFactor = combineRec(i)%primaryFactor**2
        combineBlk(kp)%sFactor = combineRec(i)%secondaryFactor**2
        combineBlk(kp)%cFactor = 2.*combineRec(i)%correlation*combineRec(i)%primaryFactor*combineRec(i)%secondaryFactor
      CASE( DEZONE_SCALE )
        combineBlk(kp)%pFactor = combineRec(i)%primaryFactor**2
        combineBlk(kp)%sFactor = combineRec(i)%secondaryFactor**2
        combineBlk(kp)%cFactor = 2.*combineRec(i)%correlation*combineRec(i)%primaryFactor*combineRec(i)%secondaryFactor
      CASE( DEZONE_AVG )
        combineBlk(kp)%pFactor = combineRec(i)%primaryFactor
        combineBlk(kp)%sFactor = combineRec(i)%secondaryFactor
        combineBlk(kp)%cFactor = 0.0
      CASE DEFAULT
        nError   = IV_ERROR
        eRoutine = 'CheckCombine'
        eMessage = 'Invalid field type'
        eInform  = 'Fields must be of type [DEZONE_MEAN,DEZONE_VAR,DEZONE_SCALE,DEZONE_ADD]'
        WRITE(eAction,'(A,I7)')'Field type:',primaryTypes%types(kp)
        GOTO 9999
    END SELECT
    combineBlk(kp)%push = doMaximize .OR. (combineRec(i)%primaryFactor*combineRec(i)%secondaryFactor) < 0.0
  END DO
END DO

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CreateProcessGrid()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE srfparam_fd

IMPLICIT NONE

INTEGER i, ios, nvart, nblk, iversion, irv, lun

TYPE( SAGgrid_str ), POINTER :: grdP, grdF

INTEGER,       DIMENSION(:), ALLOCATABLE :: ifld, iaux
CHARACTER(64), DIMENSION(:), ALLOCATABLE :: blkname

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_InitGridFileID
INTEGER, EXTERNAL :: SAG_Create

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer

!---- Create combine grid

irv = SAG_NewGrdStr( Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error creating surface grid for combine'
  GOTO 9999
END IF

lun = 100 + Fid
irv = SAG_InitGridID( processedFile,lun,SAG_GRID_BOTH,0,grdP%nvart,grdP%nvart,Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',processedFile )
  GOTO 9999
END IF

nblk = grdP%nblk
nvart = grdP%nvart
iversion = grdP%version

ALLOCATE( ifld(nblk),iaux(nblk),STAT=ios )
IF( ios == 0 )ALLOCATE( blkname(nblk),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error allocating surface data structures'
  GOTO 9999
END IF

DO i = 1,nblk
  blkname(i) = grdP%ipblk(i)%name
  ifld(i)    = grdP%ipblk(i)%ifld
  iaux(i)    = grdP%ipblk(i)%iaux
END DO

irv = SAG_InitGridFileID( Fid,nblk,nvart,iversion,ifld,iaux, &
                                           blkname,grdP%ipnam )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error initializing surface grid'
  GOTO 9999
END IF

grdF => SAG_PtrGrdStr( Fid ) ! Associate "local" grid structure pointer
ALLOCATE( grdF%ipflg(grdF%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

irv = SAG_Create( Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error creating surface file'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CreateMaxConcGrid( nfilt,filter )

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE srfparam_fd
USE surface_fi

IMPLICIT NONE

INTEGER,                INTENT( IN ) :: nfilt
REAL, DIMENSION(nfilt), INTENT( IN ) :: filter

INTEGER i, j, n, ios, nvart, nblk, iversion, irv, lun, nb, nv
INTEGER ib, iv, ifilt
REAL    tfilt

CHARACTER(4) namex, unit

TYPE( SAGgrid_str ), POINTER :: grdP, grdF

INTEGER,       DIMENSION(:), ALLOCATABLE :: ifld, iaux
CHARACTER(64), DIMENSION(:), ALLOCATABLE :: blkname
CHARACTER(4),  DIMENSION(:), ALLOCATABLE :: names !variable name data

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_InitGridFileID
INTEGER, EXTERNAL :: SAG_Create

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer

!---- Create combine grid

irv = SAG_NewGrdStr( Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error creating surface grid for combine'
  GOTO 9999
END IF

!---- Set up surface dosage blocks

nb = 0
nv = 0
DO i = ndep_blocks+1,nsrf_blocks
  IF( srf_block(i)%type == SBLK_STD )THEN
    nb = nb + 1
    nv = nv + 1
  ELSE
    srf_block(i)%type = -1
  END IF
END DO

nblk  = 2 *nb * nfilt
nvart = 2 *nv * nfilt
iversion = grdP%version

lun = 100 + Fid
irv = SAG_InitGridID( processedFile,lun,SAG_GRID_BOTH,0,nvart,nvart,Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',processedFile )
  GOTO 9999
END IF

ALLOCATE( ifld(nblk),iaux(nblk),names(nvart),STAT=ios )
IF( ios == 0 )ALLOCATE( blkname(nblk),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error allocating surface data structures'
  GOTO 9999
END IF

ib = 0
iv = 0
DO n = 1,nfilt
  j = ndep_blocks
  tfilt = filter(n)*60.
  IF( tfilt < 5.0 )THEN
    ifilt = NINT(tfilt*60.)
    unit  = 'sec'
  ELSE IF( tfilt < 59.9 )THEN
    ifilt = NINT(tfilt)
    unit  = 'min'
  ELSE
    ifilt = NINT(filter(n))
    unit  = 'hr'
  END IF
  WRITE(namex,'(I2.1)') ifilt
  DO i = 1,nb
!-----  First block is max average conc
    ib = ib + 1
    j  = j  + 1
    DO WHILE( srf_block(j)%type < 0 )
      j = j + 1
    END DO
    blkname(ib) = TRIM(grdP%ipblk(j-ndep_blocks)%name)//' Max '//TRIM(namex)//TRIM(unit)//' Ave'
    iaux(ib)    = grdP%ipblk(j-ndep_blocks)%iaux
    iv = iv + 1
    ifld(ib)    = iv
    names(iv)   = 'Mean'
      iv = iv + 1
      names(iv) = 'Var'
!-----  Second block is current average conc
    ib = ib + 1
    blkname(ib) = TRIM(grdP%ipblk(j-ndep_blocks)%name)//' '//TRIM(namex)//TRIM(unit)//' Ave'
    iaux(ib)    = grdP%ipblk(j-ndep_blocks)%iaux
    iv = iv + 1
    ifld(ib)    = iv
    names(iv)   = 'Mean'
      iv = iv + 1
      names(iv) = 'Var'
  END DO
END DO

irv = SAG_InitGridFileID( Fid,nblk,nvart,iversion,ifld,iaux, &
                                           blkname,names )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error initializing surface grid'
  GOTO 9999
END IF

grdF => SAG_PtrGrdStr( Fid ) ! Associate "local" grid structure pointer
ALLOCATE( grdF%ipflg(grdF%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

irv = SAG_Create( Fid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateMaxConcGrid'
  eMessage = 'Error creating surface file'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE filterGrid()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

INTEGER i, irv

INTEGER, EXTERNAL :: SAG_CloseID, SAG_WriteBreakID, SAG_RmvGrdStr

CALL SetFilterTimes()
IF( nError /= NO_ERROR )GOTO 9999

CALL CreateProcessGrid()
IF( nError /= NO_ERROR )GOTO 9999

CALL CreateFilterGrid()
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,primaryTimes%nTimes

  CALL UpdateFilterGrids( primaryTimes%times(i),filterTimes(i) )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( filterTimes(i) > 0.0 )THEN
    CALL MatchDezoneLevels( Sid,primaryTypes,Pid,primaryTypes,doDep )
    IF( nError /= NO_ERROR )GOTO 9999

    CALL CreateCombineGrid( Sid,Pid,Fid )
    IF( nError /= NO_ERROR )GOTO 9999

    CALL DoCombineGrid()
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    CALL CreateCombineGrid( -1,Pid,Fid )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  irv = SAG_WriteBreakID( Fid,primaryTimes%times(i) )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'combineGrids'
    eMessage = 'Error writing combine grid to file'
    GOTO 9999
  END IF

END DO

irv = SAG_CloseID( Pid )
irv = SAG_CloseID( Fid )

irv = SAG_RmvGrdStr( Pid )
irv = SAG_RmvGrdStr( Sid )
irv = SAG_RmvGrdStr( Fid )

CALL CopyFilterGrid()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE filterMaxConcGrid( nfilt,filter )

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE surface_fi

IMPLICIT NONE

INTEGER,                INTENT( IN ) :: nfilt
REAL, DIMENSION(nfilt), INTENT( IN ) :: filter

INTEGER i, irv, n

INTEGER, EXTERNAL :: SAG_CloseID, SAG_WriteBreakID, SAG_RmvGrdStr

CALL CreateMaxConcGrid( nfilt,filter )
IF( nError /= NO_ERROR )GOTO 9999

CALL CreateFilterGrid()
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,primaryTimes%nTimes

  DO n = 1,nfilt

    filterTime = filter(n)
    CALL SetFilterTimes()
    IF( nError /= NO_ERROR )GOTO 9999

    CALL UpdateFilterGrids( primaryTimes%times(i),filterTimes(i) )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( filterTimes(i) <= 0.0 )THEN
      CALL SetZeroConcGrid( Sid,.TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999
      IF( i == 1 )THEN
        CALL SetZeroConcGrid( Fid,.FALSE. )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END IF

    CALL MatchDezoneLevels( Sid,primaryTypes,Pid,primaryTypes,doDep )
    IF( nError /= NO_ERROR )GOTO 9999

    CALL InitMaxConcGrid( Sid,Pid,Fid )
    IF( nError /= NO_ERROR )GOTO 9999

    CALL DoMaxConcGrid( n,nfilt )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO

  irv = SAG_WriteBreakID( Fid,primaryTimes%times(i) )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'combineGrids'
    eMessage = 'Error writing combine grid to file'
    GOTO 9999
  END IF

END DO

irv = SAG_CloseID( Pid )
irv = SAG_CloseID( Fid )

irv = SAG_RmvGrdStr( Pid )
irv = SAG_RmvGrdStr( Sid )
irv = SAG_RmvGrdStr( Fid )

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE SetFilterTimes()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE PtrGrdStrItf
USE srfparam_fd

IMPLICIT NONE

INTEGER i, j, ios, nfilt, offset
REAL    ftime, delta, delta0

TYPE( SAGgrid_str ), POINTER :: grdP

INTEGER, EXTERNAL :: NearestTime

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer

IF( ALLOCATED(filterTimes) )DEALLOCATE( filterTimes,STAT=ios )

ALLOCATE( filterTimes(primaryTimes%nTimes),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetFilterTimes'
  eMessage = 'Error allocating filter time array'
  GOTO 9999
END IF

delta0 = primaryTimes%Times(2) - primaryTimes%Times(1)
DO i = 1,primaryTimes%nTimes
  ftime = primaryTimes%times(i) - filterTime
  IF( (ftime - primaryTimes%times(1)) < -TIME_EPS*delta0 )THEN
    filterTimes(i) = 0.0  !No filtering
  ELSE
    j = NearestTime( primaryTimes%times,primaryTimes%ntimes,ftime )
    delta = ABS(ftime-primaryTimes%times(j))
    IF( delta/ftime > TIME_EPS )THEN
      IF( i == primaryTimes%nTimes )THEN
        filterTimes(i) = 0.0  !No filtering
      ELSE
        nError   = IV_ERROR
        eRoutine = 'SetFilterTimes'
        eMessage = 'Time mismatch: Unable to locate appropriate filter time'
        WRITE(eInform,'(A,F10.3,A,F10.3)')'Primary time:',primaryTimes%times(i),'  Filter  time:',ftime
        WRITE(eAction,'(A,F10.3)')'Nearest available time:',primaryTimes%times(j)
        GOTO 9999
      END IF
    END IF
    filterTimes(i) = primaryTimes%times(j)
  END IF
END DO

!---- Check blocks and create combine parameters

IF( ALLOCATED(combineBlk) )DEALLOCATE( combineBlk,STAT=ios )
ALLOCATE( combineBlk(grdP%nvart),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetFilterTimes'
  eMessage = 'Error allocating space for filter blocks'
  WRITE(eInform,'(A,I3)')'IOS = ',ios
  GOTO 9999
END IF

IF( doDep )THEN
  offset = 0
ELSE
  offset = primaryTypes%nDep
END IF
nfilt = 0
DO i = 1,grdP%nvart
  nfilt = nfilt + 1
  combineBlk(i)%Sid = i
  SELECT CASE( primaryTypes%types(i+offset) )
    CASE( DEZONE_MEAN )
      combineBlk(i)%pFactor =  1.0
      combineBlk(i)%sFactor = -1.0
      combineBlk(i)%cFactor =  0.0
    CASE( DEZONE_VAR )
      combineBlk(i)%pFactor =  1.0
      combineBlk(i)%sFactor = -1.0
      combineBlk(i)%cFactor =  0.0
    CASE( DEZONE_SCALE )
      combineBlk(i)%pFactor =  1.0
      combineBlk(i)%sFactor = -1.0
      combineBlk(i)%cFactor =  0.0
    CASE( DEZONE_AVG )
      combineBlk(i)%pFactor =  1.0
      combineBlk(i)%sFactor = -1.0
      combineBlk(i)%cFactor =  0.0
    CASE DEFAULT
      nfilt = nfilt - 1
      combineBlk(i)%Sid = 0
      combineBlk(i)%pFactor = 0.0
      combineBlk(i)%sFactor = 0.0
      combineBlk(i)%cFactor = 0.0
  END SELECT
  combineBlk(i)%push = .FALSE.
END DO
IF( nfilt == 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetFilterTimes'
  eMessage = 'No filterable blocks found on file'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE UpdateFilterGrids( timp, tims )

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf

IMPLICIT NONE

REAL, INTENT ( IN ) :: timp, tims

CHARACTER(4), DIMENSION(1) :: names

INTEGER irv, nvart, icopy, i, ios

TYPE( SAGgrid_str ), POINTER :: grdP, grdS

INTEGER, EXTERNAL :: SAG_ReadNextBreakID, SAG_FindTime, SAG_CopyGridID

names(1) = ' '

IF( tims > 0.0 )THEN
  CALL ReinitGridID( Pid )
  IF( nError /= NO_ERROR )GOTO 9999

  irv = SAG_FindTime( Pid,tims  )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'UpdateGrids'
    eMessage = 'Error finding filter time on file'
    GOTO 9999
  END IF

  irv = SAG_ReadNextBreakID( Pid,-1,names )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'UpdateGrids'
    eMessage = 'Error reading filter time from file'
    GOTO 9999
  END IF

  CALL ReinitGridID( Sid )
  IF( nError /= NO_ERROR )GOTO 9999

  !---- Get grid structure pointers

  grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer
  grdS => SAG_PtrGrdStr( Sid ) ! Associate "local" grid structure pointer

  ALLOCATE( grdS%ipgrd(grdP%mxgrd),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'UpdateFilterGrids'
    eMessage = 'Error allocating filter grid'
    GOTO 9999
  END IF

  ALLOCATE( grdS%ipdat(grdP%mxgrd*grdP%mxfld),STAT=ios )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'UpdateFilterGrids'
    eMessage = 'Error allocating filter data'
    GOTO 9999
  END IF

  grdS%mxgrd  = grdP%mxgrd
  grdS%maxlev = grdP%maxlev
  grdS%time   = grdP%time

  nvart = grdP%nvart

  irv = SAG_CopyGridID( Pid,Sid,SAG_COPY_GRID )
  DO i = 1,nvart
    iCopy = SAG_COPY_FIELD + i*SAG_TO_FIELD + i
    IF( irv == SAG_OK )irv = SAG_CopyGridID( Pid,Sid,iCopy )
  END DO
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'UpdateFilterGrids'
    eMessage = 'Error copying filter grid from primary grid'
    GOTO 9999
  END IF
END IF

CALL ReinitGridID( Pid )
IF( nError /= NO_ERROR )GOTO 9999

irv = SAG_FindTime( Pid,timp  )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateGrids'
  eMessage = 'Error reading finding primary time on file'
  GOTO 9999
END IF

irv = SAG_ReadNextBreakID( Pid,-1,names )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateGrids'
  eMessage = 'Error reading primary time from file'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE SetZeroConcGrid( Aid,flag )

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER, INTENT( IN ) :: Aid
LOGICAL, INTENT( IN ) :: flag

INTEGER ios, i, nvart, irv

TYPE( SAGgrid_str ), POINTER :: grdP, grdS

INTEGER, EXTERNAL :: SAG_CopyGridID

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer
grdS => SAG_PtrGrdStr( Aid ) ! Associate "local" grid structure pointer

IF( ASSOCIATED(grdS%ipgrd) )DEALLOCATE( grdS%ipgrd,STAT=ios )
ALLOCATE( grdS%ipgrd(grdP%mxgrd),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateFilterGrids'
  eMessage = 'Error allocating filter grid'
  GOTO 9999
END IF

grdS%mxgrd  = grdP%mxgrd
grdS%maxlev = grdP%maxlev
grdS%time   = grdP%time

IF( flag )THEN
  nvart = grdP%nvart
ELSE
  nvart = grdS%nvart
END IF

IF( ASSOCIATED(grdS%ipdat) )DEALLOCATE( grdS%ipdat,STAT=ios )
ALLOCATE( grdS%ipdat(grdP%mxgrd*nvart),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateFilterGrids'
  eMessage = 'Error allocating filter data'
  GOTO 9999
END IF

irv = SAG_CopyGridID( Pid,Aid,SAG_COPY_GRID )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'UpdateFilterGrids'
  eMessage = 'Error copying filter grid from primary grid'
  GOTO 9999
END IF

grdS%nvart = nvart

DO i = 1,nvart*grdS%mxgrd
  grdS%ipdat(i) = 0.0
END DO

9999 CONTINUE

RETURN
END
!=============================================================================================

SUBROUTINE CopyFilterGrid()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

INTEGER irv

INTEGER, EXTERNAL :: sysMoveFile, sysDeleteFile

!--- Move current file for safe keeping

IF( .NOT.doCombine )THEN
  irv = sysMoveFile( primaryFile, backupFile )
  IF( irv == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'CopyFilterGrid'
    eMessage = 'Error backing up primary file'
    GOTO 9999
  END IF
ELSE
  irv = sysDeleteFile( primaryFile )
  IF( irv == SCIPfailure )THEN
    nError   = UK_ERROR
    eRoutine = 'CopyFilterGrid'
    eMessage = 'Error deleting primary file (results of combine)'
    GOTO 9999
  END IF
END IF

!Move comb file to primary

irv = sysMoveFile( processedFile, primaryFile )
IF( irv == SCIPfailure )THEN
  nError   = UK_ERROR
  eRoutine = 'combineFile'
  eMessage = 'Error replacing primary file with combined file'
  irv = sysMoveFile( backupFile, primaryFile )
  IF( irv == SCIPfailure )THEN
    eMessage = TRIM(eMessage)//':Error restoring primary file after error'
  END IF
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

SUBROUTINE CreateFilterGrid()

USE SCIMgr_fd
USE simple_fi
USE error_fi
USE sagstr_fd
USE sagdef_fd
USE PtrGrdStrItf
USE srfparam_fd

IMPLICIT NONE

INTEGER irv, ios

TYPE( SAGgrid_str ), POINTER :: grdP, grdS

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID

!---- Get grid structure pointers

grdP => SAG_PtrGrdStr( Pid ) ! Associate "local" grid structure pointer

!---- Create combine grid

irv = SAG_NewGrdStr( Sid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateFilterGrid'
  eMessage = 'Error creating surface grid for filter'
  GOTO 9999
END IF

irv = SAG_InitGridID( ' ',0,SAG_GRID_BOTH,0,grdP%nvart,grdP%nvart,Sid )
IF( irv /= SAG_OK )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateFilterGrid'
  eMessage = 'Error initializing SAG surface grid'
  CALL ReportFileName( eInform,'File=',processedFile )
  GOTO 9999
END IF

grdS => SAG_PtrGrdStr( Sid ) ! Associate "local" grid structure pointer
ALLOCATE( grdS%ipflg(grdS%mxfld),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CreateProcessGrid'
  eMessage = 'Error allocating surface grid arrays'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!=============================================================================================

INTEGER FUNCTION NearestTime( tList,nList,time )

IMPLICIT NONE

REAL, DIMENSION(*) :: tList
REAL               :: time
INTEGER            :: nList

REAL    dtp,dtm
INTEGER ip,im

NearestTime = 1

IF( nList > 1 )THEN
  ip = 2
  DO WHILE( time > tList(ip) .AND. ip < nList )
    ip = ip + 1
  END DO
  im = ip - 1

  dtp = ABS(time-tList(ip))
  dtm = ABS(time-tList(im))

  IF( dtp < dtm )THEN
    NearestTime = ip
  ELSE
    NearestTime = im
  END IF
END IF

RETURN
END

