INTEGER FUNCTION SWIMbuildObsGrid( Obs )

USE SWIM_fi
USE VertGrid_fd
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER, PARAMETER :: MAX_CHECK = 30

REAL(8), DIMENSION(:),   ALLOCATABLE :: var8
REAL,    DIMENSION(:,:), ALLOCATABLE :: zb_obs, tem
INTEGER, DIMENSION(:),   ALLOCATABLE :: nzb_obs

CHARACTER(64) ObsID, CurrentID

INTEGER :: max_zb = 3 !Initial value for vertical levels

INTEGER i, k, nrec0, nrec, ncheck, alloc_stat, irv, i2, n_obs
REAL    timeObs, timeBin, zObs, CurrentTime, zmax
LOGICAL lend, lValidPrf, lFixedData

TYPE( VertGrid ) :: vgrid

INTEGER, EXTERNAL :: SWIMreadVarObs

SWIMbuildObsGrid = SWIMfailure

lend        = .FALSE.
nrec0       = 0
n_obs       = 0
lValidPrf   = .TRUE.
CurrentID   = 'Ill-Defined'
CurrentTime = NOT_SET_R

lFixedData = Obs%nVarFixed > 0

!------ Set number of profiles to be checked

IF( BTEST(Obs%type,OTB_FCST) .OR. BTEST(Obs%type,OTB_ANLY) )THEN
  ncheck = 1
ELSE
  ncheck = MAX_CHECK
END IF

!------ Loop through first ncheck (good) stations and build grids for each one

ALLOCATE( zb_obs(max_zb,ncheck),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( nzb_obs(ncheck),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( var8(Obs%nVar),STAT=alloc_stat )

IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMbuildObsGrid'
  error%Message = 'Error allocating arrays for obs vertical levels'
  GOTO 9999
END IF

zb_obs = -1.

var8 = DBLE(NOT_SET_R)

LoopOverRecords : DO

  irv = SWIMreadVarObs( Obs,var8,lend,nrec,ObsID )
  IF( irv /= SWIMsuccess )GOTO 9999
  nrec0 = nrec0 + nrec

  IF( lend )EXIT LoopOverRecords

  IF( ObsID /= 'NOT SET' )THEN
    CALL SWIMgetTimeObs( Obs,var8,timeObs,timeBin )
    IF( error%Number /= NO_ERROR )GOTO 9999
  ELSE
    ObsID = CurrentID
  END IF

!------ Test for new profile

  IF( CurrentID /= ObsID .OR. CurrentTime /= timeObs )THEN

    CurrentID   = ObsID
    CurrentTime = timeObs

    IF( lValidPrf )n_obs = n_obs + 1
    IF( n_obs > ncheck )THEN
      n_obs = ncheck; EXIT
    END IF
    nzb_obs(n_obs) = 0
    lValidPrf      = .FALSE.

    IF( lFixedData )THEN
      irv = SWIMreadVarObs( Obs,var8,lend,nrec,ObsID )
      IF( irv /= SWIMsuccess )GOTO 9999
      nrec0 = nrec0 + nrec
    END IF

  END IF

!------ Check for valid data

  LoopOverVar : DO i = Obs%nVarFixed+1,Obs%nVar

    SELECT CASE( Obs%VarID(i) )

      CASE( OVP_U,OVP_DIR )     !Check for valid velocity

        i2 = NINT(Obs%Conv(i))
        IF( var8(i) == OBP_BADDATA .OR. var8(i2) == OBP_BADDATA )CYCLE LoopOverRecords

      CASE( OVP_Z )             !Get height
        IF( var8(i) == OBP_BADDATA )CYCLE LoopOverRecords
        zObs = SNGL(var8(i))*Obs%Conv(i)

    END SELECT

  END DO LoopOverVar

!------ Add grid level

  nzb_obs(n_obs) = nzb_obs(n_obs) + 1
  lValidPrf      = .TRUE.

  IF( nzb_obs(n_obs) > max_zb )THEN  !Check if reallocating required

    ALLOCATE( tem(max_zb,ncheck),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMbuildObsGrid'
      error%Message = 'Error allocating temporary array for obs vertical levels'
      GOTO 9999
    END IF

    tem(1:max_zb,1:n_obs) = zb_obs(1:max_zb,1:n_obs); DEALLOCATE( zb_obs )

    ALLOCATE( zb_obs(2*max_zb,ncheck),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMbuildObsGrid'
      error%Message = 'Error allocating array for obs vertical levels'
      GOTO 9999
    END IF

    zb_obs = -1.

    zb_obs(1:max_zb,1:n_obs) = tem(1:max_zb,1:n_obs); DEALLOCATE( tem ); max_zb = 2*max_zb

  END IF

  zb_obs(nzb_obs(n_obs),n_obs) = zObs

END DO LoopOverRecords

IF( .NOT.lValidPrf )n_obs = n_obs - 1

IF( n_obs == 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMbuildObsGrid'
  error%Message = 'No valid profiles found'
  CALL ReportFileName( error%Inform,'File=',Obs%source )
  GOTO 9999
END IF

!------ Construct grid based on ordering of obs levels

zmax = 0.0
DO i = 1,n_obs
  DO k = 1,nzb_obs(i)
    zmax = MAX(zmax,zb_obs(k,i))
  END DO
END DO

CALL build_vert_grid( zb_obs,nzb_obs,max_zb,zmax,n_obs,vgrid )
IF( error%Number /= NO_ERROR )GOTO 9999

Obs%nz = vgrid%nz
ALLOCATE( Obs%z(Obs%nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMbuildObsGrid'
  error%Message = 'Error vertical grid'
  CALL ReportFileName( error%Action,'File=',Obs%Source )
  GOTO 9999
END IF

Obs%z(1:Obs%nz) = vgrid%z(1:Obs%nz)

DEALLOCATE( vgrid%z,STAT=alloc_stat )

!------ Rewind file to beginning of data section

DO i = 1,nrec0
  BACKSPACE(Obs%unit,IOSTAT=alloc_stat)
END DO

!------ SWIMming success

SWIMbuildObsGrid = SWIMresult

9999 CONTINUE

IF( ALLOCATED(zb_obs)   )DEALLOCATE( zb_obs,STAT=alloc_stat )
IF( ALLOCATED(nzb_obs)  )DEALLOCATE( nzb_obs,STAT=alloc_stat )
IF( ALLOCATED(tem)      )DEALLOCATE( tem,STAT=alloc_stat )
IF( ALLOCATED(var8)     )DEALLOCATE( var8,STAT=alloc_stat )
IF( ASSOCIATED(vgrid%z) )DEALLOCATE( vgrid%z,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetSrfRef( Obs )

!------ Get average obs level for surface met (if reference height not given)

USE SWIM_fi
USE VertGrid_fd
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER, PARAMETER :: MAX_CHECK = 30

CHARACTER(64) ObsID

REAL(8), DIMENSION(:),   ALLOCATABLE :: var8

INTEGER i, nrec0, nrec, ncheck, alloc_stat, irv, i2, n_obs
REAL    zObs, sum
LOGICAL lend

INTEGER, EXTERNAL :: SWIMreadVarObs

SWIMsetSrfRef = SWIMfailure

!------ Make sure this is a surface obs

IF( .NOT.BTEST(Obs%type,OTB_SRF) )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMsetSrfRef'
  error%Message = 'Invalid obs met type'
  error%Action  = 'Attempting to define reference height'
  GOTO 9999
END IF

!------ And no reference height is defined

IF( Obs%zref > 0. )THEN
  SWIMsetSrfRef = SWIMsuccess
  GOTO 9999
END IF

!------ If height is not given for each obs, default is 10 meters

IF( .NOT.BTEST(Obs%type,OTB_Z) )THEN
  Obs%zref = 10.
  SWIMsetSrfRef = SWIMsuccess
  GOTO 9999
END IF

!------ Set number of stations and/or times to be checked

IF( BTEST(Obs%type,OTB_FCST) .OR. BTEST(Obs%type,OTB_ANLY) )THEN
  ncheck = 1
ELSE
  ncheck = MAX_CHECK
END IF

!------- Setup for reading obs records

ALLOCATE( var8(Obs%nVar),STAT=alloc_stat )

IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMsetSrfRef'
  error%Message = 'Error allocating arrays for obs vertical levels'
  GOTO 9999
END IF

lend  = .FALSE.
nrec0 = 0
n_obs = 0
sum   = 0.

var8 = DBLE(NOT_SET_R)

!------ Loop through first ncheck (good) stations and get heights

LoopOverRecords : DO

  irv = SWIMreadVarObs( Obs,var8,lend,nrec,ObsID )
  IF( irv /= SWIMsuccess )GOTO 9999
  nrec0 = nrec0 + nrec

  IF( lend )EXIT LoopOverRecords

!------ Check for valid data

  LoopOverVar : DO i = 1,Obs%nVar

    SELECT CASE( Obs%VarID(i) )

      CASE( OVP_U,OVP_DIR )     !Check for valid velocity

        i2 = NINT(Obs%Conv(i))
        IF( var8(i) == OBP_BADDATA .OR. var8(i2) == OBP_BADDATA )CYCLE LoopOverRecords

      CASE( OVP_Z )             !Get height
        zObs = SNGL(var8(i))*Obs%Conv(i)

    END SELECT

  END DO LoopOverVar

  sum   = sum + zObs
  n_obs = n_obs + 1
  IF( n_obs == ncheck )EXIT

END DO LoopOverRecords

!------ Set reference to average height (or 10 meters if no valid data found)

IF( n_obs == 0 )THEN
  Obs%zref = 10.
ELSE
  Obs%zref = sum / FLOAT(n_obs)
END IF

!------ Rewind file to beginning of data section

DO i = 1,nrec0
  BACKSPACE(Obs%unit,IOSTAT=alloc_stat)
END DO

!------ SWIMming success

SWIMsetSrfRef = SWIMresult

9999 CONTINUE

DEALLOCATE( var8,STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE build_vert_grid( z,nz,max_zb,zmax,n,vgrid )

!------ Build vertical grid based on observational profiles

USE SWIM_fi
USE SWIMparam_fd
USE VertGrid_fd
USE Zsort

IMPLICIT NONE

INTEGER,                      INTENT( IN  ) :: n
INTEGER,                      INTENT( IN  ) :: max_zb
INTEGER, DIMENSION(n),        INTENT( IN  ) :: nz
REAL,                         INTENT( IN  ) :: zmax
REAL,    DIMENSION(max_zb,n), INTENT( IN  ) :: z
TYPE( VertGrid ),             INTENT( OUT ) :: vgrid

INTEGER i, k, m, nzm, alloc_stat
REAL    zbmin, zbmax, dzchk, dzchkD, dzc

TYPE( SortNode ), POINTER :: Root
TYPE( SortNode ), POINTER :: Zord, Zptr, Zl, Zg

INTERFACE

  RECURSIVE SUBROUTINE tree( Znode,zlev )
    USE Zsort
    TYPE( SortNode ), POINTER :: Znode
    REAL,        INTENT( IN ) :: zlev
  END SUBROUTINE tree

  RECURSIVE SUBROUTINE sort_zlev( Znode,Zord,n )
    USE Zsort
    TYPE( SortNode ), POINTER :: Znode
    TYPE( SortNode ), POINTER :: Zord
    INTEGER,  INTENT( INOUT ) :: n
  END SUBROUTINE sort_zlev

  LOGICAL FUNCTION ZgridTooClose( Zptr,dzchk )
    USE Zsort
    IMPLICIT NONE
    TYPE( SortNode ), POINTER :: Zptr
    REAL,        INTENT( IN ) :: dzchk
  END FUNCTION ZgridTooClose

END INTERFACE

!------ Sort all obs levels using a very sophisticated binary tree

IF( n > 1 )THEN

!------ Initialize list

  NULLIFY( Root )
  ALLOCATE( Root,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = IV_ERROR
    error%Routine = 'build_vert_grid'
    error%Message = 'Error allocating Root for vertical grid'
    GOTO 9999
  END IF

  nzm   = 0
  zbmax = 0.
  zbmin = HUGE(0.)

!------ Loop over levels of each station and build binary list

  staLoop : DO i = 1,n

    zLoop : DO k = 1,nz(i)

      IF( i == 1 .AND. k == 1 )THEN
        Root%z = z(1,1)
        Root%n = 1
        NULLIFY( Root%Greater,Root%Lesser )
      ELSE
        CALL tree( Root,z(k,i) )
        IF( error%Number /= NO_ERROR )GOTO 9999
      END IF

      nzm   = MAX(nzm,k)
      zbmin = MIN(zbmin,z(k,i))
      zbmax = MAX(zbmax,z(k,i))
      IF( z(k,i) >= zmax )EXIT zLoop

    END DO zLoop

  END DO staLoop

!------ Build ordered list

  NULLIFY( Zord )
  ALLOCATE( Zord,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number   = UK_ERROR
    error%Routine = 'build_vert_grid'
    error%Message = 'Error allocating Zord for vertical grid'
    GOTO 9999
  END IF

  m = 0
  NULLIFY( Zord%Greater,Zord%Lesser )
  Zord%n = 0
  Zord%z = 0.

  Zptr => Zord
  CALL sort_zlev( Root,Zptr,m )
  IF( error%Number /= NO_ERROR )THEN
    error%Routine = 'build_vert_grid'
    error%Message = 'Error allocating Zord for vertical grid'
    GOTO 9999
  END IF

!------ Find number of obs below dispersion domain height

  m = 0
  Zptr => Zord%Greater
  DO WHILE( ASSOCIATED(Zptr) )
    IF( Zptr%z <= Prj%Zmax )THEN
      m = m + 1
    ELSE
      EXIT
    END IF
    Zptr => Zptr%Greater
  END DO

!------ Criterion for pruning list, i.e., minimum grid spacing

  dzchk  = MIN(zmax,zbmax-zbmin)/FLOAT(MAX(2*nzm-1,1))
  dzchk  = MAX( dzchk,1. )
  dzchkD = MIN( dzchk,Prj%Zmax/FLOAT(MAX(2*m-1,1)))

!------ Sweep up list and prune

  m           =  1
  Zptr        => Zord%Greater
  Zptr%Lesser => Zord

  Zptr => Zptr%Greater

  DO WHILE( ASSOCIATED(Zptr) )

    IF( Zptr%z <= Prj%Zmax )THEN
      dzc = dzchkD
    ELSE
      dzc = dzchkD + (Zptr%z-Prj%Zmax)/(zmax-Prj%Zmax)*(dzchk-dzchkD)
    END IF

    Zl => Zptr%Lesser  !Save links to levels below/above current level
    Zg => Zptr%Greater

    IF( ZgridTooClose( Zptr,dzc ) )THEN  !Throw out current level if spacing is too small
      Zl%Greater => Zptr%Greater
      IF( ASSOCIATED(Zg) )zg%Lesser => Zptr%Lesser
      DEALLOCATE( Zptr )
    ELSE
      m = m + 1
    END IF

    Zptr => Zg

  END DO

!------ Construct final grid and put in z(:,1)
!       First get pruned list

  ALLOCATE( vgrid%z(m),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'build_vert_grid'
    error%Message = 'Error allocating vertical grid'
    GOTO 9999
  END IF

  vgrid%nz = m

  Zptr => Zord%Greater
  m    =  0
  DO
    m = m + 1
    vgrid%z(m) = Zptr%z
    DEALLOCATE(Zptr%Lesser)
    IF( .NOT.ASSOCIATED(Zptr%Greater) )THEN
      DEALLOCATE(Zptr)
      EXIT
    END IF
    Zptr => Zptr%Greater
  END DO

ELSE

  ALLOCATE( vgrid%z(nz(1)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'build_vert_grid'
    error%Message = 'Error allocating vertical grid'
    GOTO 9999
  END IF
  vgrid%nz = nz(1)
  vgrid%z = z(1:nz(1),1)

END IF

!------ Make sure bottom grid level is above ground

IF(  vgrid%z(1) <= 0. )THEN
  IF( vgrid%nz > 1 )THEN
    vgrid%z(1) = MIN(10.,0.1*vgrid%z(2))
  ELSE
    vgrid%z(1) = 10.
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

LOGICAL FUNCTION ZgridTooClose( Zptr,dzchk )

USE Zsort

IMPLICIT NONE

REAL, PARAMETER :: ZGRDFAC = 0.2

TYPE( SortNode ), POINTER :: Zptr
REAL,        INTENT( IN ) :: dzchk

ZgridTooClose = Zptr%z - Zptr%Lesser%z < MIN( ZGRDFAC*Zptr%Lesser%z,dzchk )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE tree( Znode,zlev )

USE Zsort
USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

REAL, PARAMETER :: DZFAC = 0.01

TYPE( SortNode ), POINTER :: Znode
REAL,        INTENT( IN ) :: zlev

INTEGER alloc_stat
REAL    dzl

dzl = DZFAC*zlev

IF( .NOT.ASSOCIATED(Znode) )THEN

  ALLOCATE( Znode,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    error%Routine = 'tree'
    error%Message = 'Error allocating node for sorting'
    GOTO 9999
  END IF
  Znode%z = zlev
  Znode%n = 1
  NULLIFY( Znode%Greater,Znode%Lesser )

ELSE IF( zlev < Znode%z-dzl )THEN

  CALL tree( Znode%Lesser,zlev )
  IF( error%Number /= NO_ERROR )GOTO 9999

ELSE IF( zlev > Znode%z+dzl )THEN

  CALL tree( Znode%Greater,zlev )
  IF( error%Number /= NO_ERROR )GOTO 9999

ELSE

  Znode%n = Znode%n + 1

END IF

9999 CONTINUE

RETURN

END SUBROUTINE tree

!===============================================================================

RECURSIVE SUBROUTINE sort_zlev( Znode,Zord,n )

USE Zsort
USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( SortNode ), POINTER :: Znode
TYPE( SortNode ), POINTER :: Zord
INTEGER,  INTENT( INOUT ) :: n

INTEGER alloc_stat

IF( ASSOCIATED(Znode) )THEN

  CALL sort_zlev( Znode%Lesser,Zord,n )
  IF( error%Number /= NO_ERROR )GOTO 9999

  n = n + 1
  ALLOCATE( Zord%Greater,STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number = UK_ERROR
    GOTO 9999
  END IF
  Zord%Greater%Lesser => Zord
  Zord   => Zord%Greater
  Zord%z = Znode%z
  Zord%n = Znode%n
  NULLIFY( Zord%Greater )

  CALL sort_zlev( Znode%Greater,Zord,n )
  IF( error%Number /= NO_ERROR )GOTO 9999

  DEALLOCATE( Znode ); NULLIFY( Znode )

END IF

9999 CONTINUE

RETURN

END SUBROUTINE sort_zlev
