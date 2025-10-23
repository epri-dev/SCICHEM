INTEGER FUNCTION NumMetOutput( userID,Project,num2Dfields,num3Dfields, &
                               numGrids,numTimes )

!------ Set met plot allocation requirements

USE SCIMgr_fd
USE metoutput_fd

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( OUT ) :: num2Dfields, num3Dfields, numGrids, numTimes

INTEGER  n2D, n3D

n2D = 0
n3D = 0

NumMetOutput = InitMetOutput( userID,Project,n2D,n3D,num2Dfields=num2Dfields, &
                                                     num3Dfields=num3Dfields, &
                                                     numGrids=numGrids, &
                                                     numTimes=numTimes )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetMetOutput( userID,project,n2D,n3D,name2D,units2D,name3D,units3D, &
                               grid,time,field2Dlist,field3Dlist )

!------ Set met plot allocation requirements

USE error_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE plotmet_fd
USE metoutput_fd
USE plotmet_fi
USE met_fi, ONLY : nBaseMet
USE abort

IMPLICIT NONE

INTEGER,                                 INTENT( IN  ) :: userID
TYPE( projectIDT ),                      INTENT( IN  ) :: Project
INTEGER,                                 INTENT( IN  ) :: n2D, n3D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: name2D
TYPE( char64T ),       DIMENSION(n2D),   INTENT( OUT ) :: units2D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: name3D
TYPE( char64T ),       DIMENSION(n3D),   INTENT( OUT ) :: units3D
TYPE( metGridT ),      DIMENSION(*),     INTENT( OUT ) :: grid
TYPE( SCIPTimeT ),     DIMENSION(*),     INTENT( OUT ) :: time
TYPE( metGridFieldT ), DIMENSION(n2D,*), INTENT( OUT ) :: field2Dlist
TYPE( metGridFieldT ), DIMENSION(n3D,*), INTENT( OUT ) :: field3Dlist

GetMetOutput = InitMetOutput( userID,Project,n2D,n3D,name2D=name2D,units2D=units2D, &
                                                     name3D=name3D,units3D=units3D, &
                                                     grid=grid, &
                                                     time=time, &
                                                     field2Dlist=field2Dlist, &
                                                     field3Dlist=field3Dlist )

RETURN
END

!==============================================================================

INTEGER FUNCTION InitMetOutput( userID,Project,n2D,n3D,num2Dfields,num3Dfields, &
                                numGrids,numTimes,name2D,units2D,name3D,units3D, &
                                grid,time,field2Dlist,field3Dlist )

!------ Set met plot allocation requirements

USE error_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE plotmet_fd
USE plotmet_fi
USE met_fi, ONLY : nBaseMet, MetGrid
USE SWIMparam_fd
USE constants_fd
USE default_fd
USE abort

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( IN  ) :: n2D, n3D
INTEGER,                                     OPTIONAL, INTENT( OUT ) :: num2Dfields, num3Dfields
INTEGER,                                     OPTIONAL, INTENT( OUT ) :: numGrids, numTimes
TYPE( char64T ),       DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: name2D, units2D
TYPE( char64T ),       DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: name3D, units3D
TYPE( metGridT ),      DIMENSION(*), TARGET, OPTIONAL, INTENT( OUT ) :: grid
TYPE( SCIPTimeT ),     DIMENSION(*),         OPTIONAL, INTENT( OUT ) :: time
TYPE( metGridFieldT ), DIMENSION(n2D,*),     OPTIONAL, INTENT( OUT ) :: field2Dlist
TYPE( metGridFieldT ), DIMENSION(n3D,*),     OPTIONAL, INTENT( OUT ) :: field3Dlist

INTEGER currentState, irv, alloc_stat
INTEGER i, j, k, n2, n3, num2D, num3D
LOGICAL found, lDoLL, lDoZ3D

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: varname

TYPE( startT ) start

!==== Initialize

InitMetOutput = SCIPfailure

IF( PRESENT(num2Dfields) )THEN
  num2Dfields = 0
  num3Dfields = 0
  numGrids    = 0
  numTimes    = 0
END IF

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL LoadStart( start )

!------ Read project file

  IF( .NOT.MemoryField )THEN
    CALL ReadProject( Project )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ Read Met times

  CALL ReadMetTime( start )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( nMetTime == 0 )THEN
    InitMetOutput = SCIPnull
    GOTO 9999
  END IF

  hasPlotTimes(HP_METTIME +1) = .TRUE.

  IF( PRESENT(grid) )THEN
    pMetGrid => grid(1:nBaseMet)
    ALLOCATE( istgMet(nBaseMet),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'InitMetOutput'
      eMessage = 'Error allocating field stagger array'
      GOTO 9999
    END IF
  ELSE
    NULLIFY( pMetGrid )
  END IF

!------ Setup lists of 2d & 3d fields

  CALL CountMetChoices( n2,n3 )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( PRESENT(numTimes) )THEN
    numTimes = nMetTime
    numGrids = nBaseMet
  END IF

!------ Copy time structure

  IF( PRESENT(time) )THEN
    DO i = 1,nMetTime
      time(i) = MetTime(i)
    END DO
  END IF

!------ Setup met grids

  DO i = 1,nBaseMet
    lDoLL = MetGrid(i)%coord%type /= I_LATLON
    IF( lDoLL )EXIT
  END DO

  IF( PRESENT(grid) )THEN
    DO i = 1,nBaseMet
      IF( i == 1 )THEN
        grid(i)%name%string = 'Outer Grid'
      ELSE IF( i < 10 )THEN
        WRITE(grid(i)%name%string,FMT='("Nest Grid ",I1)',IOSTAT=irv) i
      ELSE IF( i < 100 )THEN
        WRITE(grid(i)%name%string,FMT='("Nest Grid ",I2)',IOSTAT=irv) i
      ELSE
        WRITE(grid(i)%name%string,FMT='("Nest Grid")',IOSTAT=irv) i
      END IF
      grid(i)%x0 = MetGrid(i)%xmin
      grid(i)%y0 = MetGrid(i)%ymin
      grid(i)%dx = MetGrid(i)%dx
      grid(i)%dy = MetGrid(i)%dy
      grid(i)%nx = MetGrid(i)%nx
      grid(i)%ny = MetGrid(i)%ny
      grid(i)%coordHoriz%type = MetGrid(i)%coord%type
      grid(i)%coordHoriz%zone = MetGrid(i)%coord%zone
      grid(i)%coordHoriz%reference = MetGrid(i)%coord%reference
      grid(i)%coordHoriz%Lat0 = MetGrid(i)%coord%Lat0
      grid(i)%coordHoriz%Lon0 = MetGrid(i)%coord%Lon0
      grid(i)%coordHoriz%Lat1 = MetGrid(i)%coord%Lat1
      grid(i)%coordHoriz%Lat2 = MetGrid(i)%coord%Lat2
      IF( MetGrid(i)%coord%Rearth == NOT_SET_R )THEN
        grid(i)%coordHoriz%Rearth = Rearth*1.E-3
      ELSE
        grid(i)%coordHoriz%Rearth = MetGrid(i)%coord%Rearth
      END IF
    END DO
  END IF

!------ Loop over met grids and get list of all unique names

  i = MAX(MAXVAL(nMet2D),MAXVAL(nMet3D))
  ALLOCATE( varname(i),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'InitMetOutput'
    eMessage = 'Error allocating field name array'
    GOTO 9999
  END IF

  varname = ' '
  num2D   = 0

  DO i = 1,nBaseMet
    DO j = 1,nMet2D(i)
      found = .FALSE.
      DO k = 1,num2D
        IF( TRIM(MetVar2D(i,j)) == TRIM(varname(k)) )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        num2D = num2D + 1
        varname(num2D)        = TRIM(MetVar2D(i,j))
        IF( PRESENT(units2D) )units2D(num2D)%string = TRIM(MetUnit2D(i,j))
      END IF
    END DO
  END DO

  IF( PRESENT(name2D) )THEN

    DO i = 1,num2D
      SELECT CASE( TRIM(varname(i)) )
        CASE( 'REL' )
          name2D(i)%string = 'ELEV'
        CASE( 'ALBD' )
          name2D(i)%string = 'ALBEDO'
        CASE( 'BR' )
          name2D(i)%string = 'BOWEN'
        CASE( 'HCNP' )
          name2D(i)%string = 'CANOPY_HEIGHT'
        CASE( 'ALPH' )
          name2D(i)%string = 'CANOPY_ALPHA'
        CASE( 'ZI' )
          name2D(i)%string = 'PBL_HEIGHT'
        CASE( 'HFLX' )
          name2D(i)%string = 'SFC_HFLX'
        CASE( 'USTR' )
          name2D(i)%string = 'USTAR'
        CASE DEFAULT
          name2D(i)%string = varname(i)
      END SELECT
    END DO

    IF( lDoLL )THEN
      name2D(num2D+1)%string = 'LON'; units2D(num2D+1)%string = 'DEG-E'
      name2D(num2D+2)%string = 'LAT'; units2D(num2D+2)%string = 'DEG-N'
    END IF

    field2Dlist(1:n2d,1:nBaseMet)%avail = SCIPfalse
    field2Dlist(1:n2d,1:nBaseMet)%stgX  = 0
    field2Dlist(1:n2d,1:nBaseMet)%stgY  = 0
    field2Dlist(1:n2d,1:nBaseMet)%stgZ  = 0

    DO i = 1,nBaseMet
      DO j = 1,nMet2D(i)
        DO k = 1,num2D
          IF( TRIM(MetVar2D(i,j)) == TRIM(varname(k)) )THEN
            field2Dlist(j,i)%avail = SCIPtrue
            EXIT
          END IF
        END DO
      END DO
      IF( lDoLL )THEN
        DO j = num2D+1,num2D+2
          IF( MetGrid(i)%coord%type /= I_LATLON )field2Dlist(j,i)%avail = SCIPtrue
        END DO
      END IF
    END DO

  END IF

  varname = ' '
  num3D   = 0
  lDoZ3D  = .TRUE.

  DO i = 1,nBaseMet
    DO j = 1,nMet3D(i)
      IF( TRIM(MetVar3D(i,j)) == 'Vector' )CYCLE
      IF( TRIM(MetVar3D(i,j)) == 'Z' )lDoZ3D = .FALSE.
      found = .FALSE.
      DO k = 1,num3D
        IF( TRIM(MetVar3D(i,j)) == TRIM(varname(k)) )THEN
          found = .TRUE.
          EXIT
        END IF
      END DO
      IF( .NOT.found )THEN
        num3D = num3D + 1
        varname(num3D) = TRIM(MetVar3D(i,j))
        IF( PRESENT(units3D) )units3D(num3D)%string = TRIM(MetUnit3D(i,j))
      END IF
    END DO
  END DO

  IF( PRESENT(num2Dfields) )THEN
    num2Dfields = num2D
    num3Dfields = num3D
    IF( lDoLL )THEN
      num2Dfields = num2Dfields + 2
      num3Dfields = num3Dfields + 2
    END IF
    IF( lDoZ3D )num3Dfields = num3Dfields + 1
  END IF

  IF( PRESENT(name3D) )THEN

    DO i = 1,num3D
      SELECT CASE( TRIM(varname(i)) )
        CASE( 'T' )
          name3D(i)%string = 'THETA'
        CASE( 'H' )
          name3D(i)%string = 'HUMID'
        CASE( 'Z' )
          name3D(i)%string = 'Z3D'
        CASE DEFAULT
          name3D(i)%string = varname(i)
      END SELECT
    END DO

    IF( lDoLL )THEN
      name3D(num3D+1)%string = 'ULL'; units3D(num3D+1)%string = 'M/S'
      name3D(num3D+2)%string = 'VLL'; units3D(num3D+2)%string = 'M/S'
    END IF

    IF( lDoZ3D )THEN
      IF( lDoLL )THEN
        name3D(num3D+3)%string = 'Z3D'; units3D(num3D+3)%string = 'M'
      ELSE
        name3D(num3D+1)%string = 'Z3D'; units3D(num3D+1)%string = 'M'
      END IF
    END IF

    field3Dlist(1:n3d,1:nBaseMet)%avail = SCIPfalse
    field3Dlist(1:n3d,1:nBaseMet)%stgX  = 0
    field3Dlist(1:n3d,1:nBaseMet)%stgY  = 0
    field3Dlist(1:n3d,1:nBaseMet)%stgZ  = 0

    DO i = 1,nBaseMet
      DO j = 1,num3D
        DO k = 1,num3D
          IF( TRIM(MetVar3D(i,j)) == TRIM(varname(k)) )THEN
            field3Dlist(j,i)%avail = SCIPtrue
            IF( BTEST(istgMet(i),GTB_STAGGER) )THEN
              SELECT CASE( TRIM(varname(k)) )
                CASE( 'U' )
                  field3Dlist(j,i)%stgX = 1
                  IF( BTEST(istgMet(i),GTB_STAGGERB) )field3Dlist(j,i)%stgY = 1
                CASE( 'V' )
                  field3Dlist(j,i)%stgY = 1
                  IF( BTEST(istgMet(i),GTB_STAGGERB) )field3Dlist(j,i)%stgX = 1
                CASE( 'W' )
                  IF( BTEST(istgMet(i),GTB_STAGGERZ) )field3Dlist(j,i)%stgZ = 1
              END SELECT
            END IF
            EXIT
          END IF
        END DO
      END DO
      IF( lDoLL )THEN
        DO j = num3D+1,num3D+2
          IF( MetGrid(i)%coord%type /= I_LATLON )field3Dlist(j,i)%avail = SCIPtrue
        END DO
      END IF
      IF( lDoZ3D )THEN
        j = num3D + 1
        IF( lDoLL )j = j + 2
        field3Dlist(j,i)%avail = SCIPtrue
      ELSE
        DO j = 1,num3D
          IF( TRIM(name3D(j)%string) == 'Z3D' )field3Dlist(j,i)%avail = SCIPtrue
        END DO
      END IF
    END DO

  END IF

  InitMetOutput = SCIPsuccess

END IF

9999 CONTINUE

IF( ALLOCATED( varname) )DEALLOCATE( varname,STAT=alloc_stat )
IF( ALLOCATED( MetTime) )DEALLOCATE( MetTime,STAT=alloc_stat  )

IF( .NOT.MemoryField )CALL deallocate_read_prj()

CALL ClearPlotLists()

CALL AbortClear()

!==== finish

CALL reset_messaging( )

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION GetMetVertGrid( userID,Project,gridID,sigma )

!------ Get 1d vertical met grid

USE error_fi
USE files_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE plotmet_fd
USE plotmet_fi
USE met_fi, ONLY : nBaseMet, MetGrid
USE abort

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: userID
TYPE( projectIDT ), INTENT( IN  ) :: Project
INTEGER,            INTENT( IN  ) :: gridID
REAL, DIMENSION(*), INTENT( OUT ) :: sigma

INTEGER currentState, irv, ios
INTEGER i, n, n3D, idum, ndum, kmax
REAL    dum

CHARACTER(8) cdum

TYPE( startT ) start

LOGICAL, EXTERNAL :: GetMCWformat

!==== Initialize

GetMetVertGrid = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL LoadStart( start )

!------ Read project file

  IF( .NOT.MemoryField )THEN
    CALL ReadProject( Project )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ Build file name

  IF( gridID == 1 )THEN
    file_tmp = file_mcw
  ELSE
    i = LEN_TRIM(file_mcw)
    WRITE(file_tmp,'(A,I1.0)')file_mcw(1:i-1),gridID
  END IF

!------ Check format

  lformat = GetMCWformat()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Read sigma levels

  CALL SkipModelHeader()
  IF( nError /= NO_ERROR )GOTO 9999

  IF( lformat )THEN
    READ(lun_mcw,*,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,'(6(I12,1X))',IOSTAT=ios) idum,idum,kmax,ndum,n3D,idum
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,'(6(F12.4,1X))',IOSTAT=ios) (sigma(i),i=1,kmax),(dum,i=1,11)
  ELSE
    READ(lun_mcw,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios) idum,idum,kmax,ndum,n3D,idum
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios) (sigma(i),i=1,kmax),(dum,i=1,11)
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'GetMetVertGrid'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  ALLOCATE( MetVar3D(1,n3D),STAT=irv )
  IF( irv /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'GetMetVertGrid'
    eMessage = 'Error allocating field name array for MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  IF( lformat )THEN
    IF( ndum > 0 )THEN
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) (cdum,n=1,ndum),(MetVar3D(1,n),n=1,n3D)
    ELSE
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) (MetVar3D(1,n),n=1,n3D)
    END IF
  ELSE
    IF( ndum > 0 )THEN
      READ(lun_mcw,IOSTAT=ios) (cdum,n=1,ndum),(MetVar3D(1,n),n=1,n3D)
    ELSE
      READ(lun_mcw,IOSTAT=ios) (MetVar3D(1,n),n=1,n3D)
    END IF
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'GetMetVertGrid'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    eInform  = 'Reading 3d field names'
    GOTO 9999
  END IF

  DO i = 1,n3D
    IF( TRIM(MetVar3D(1,i)) == 'Z' )THEN
      nError   = IV_ERROR
      eRoutine = 'GetMetVertGrid'
      eMessage = 'Invalid sigma request MET output file: '//TRIM(file_tmp)
      eInform  = 'Met grid uses 3D heights'
      GOTO 9999
    END IF
  END DO

  GetMetVertGrid = SCIPsuccess

END IF

9999 CONTINUE

CLOSE( lun_mcw,IOSTAT=ios )

IF( .NOT.MemoryField )CALL deallocate_read_prj()

CALL ClearPlotLists()

CALL AbortClear()

!==== finish

CALL reset_messaging( )

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION Get2DMetData( userID,Project,gridID,timeID,numFields, &
                               nameFields,nx,ny,data )

!------ Set met plot allocation requirements

USE files_fi
USE error_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE plotmet_fd
USE plotmet_fi
USE met_fi, ONLY : nBaseMet, MetGrid
USE datums
USE SWIMparam_fd
USE SWIMgridStr_fd
USE constants_fd
USE abort

IMPLICIT NONE

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,                               INTENT( IN  ) :: nx, ny
REAL,      DIMENSION(nx,ny,numFields), INTENT( OUT ) :: data

INTEGER currentState, irv, ios, alloc_stat
INTEGER i, j, k, n, n2, nread, ilat, ilon
INTEGER imax, jmax, kmax, ndum
REAL    x, y, lat, lon, dum, xmap, ymap
LOGICAL found, lDoLL

CHARACTER(8) vname, cdum

INTEGER, DIMENSION(:), ALLOCATABLE :: varOut

TYPE( startT ) start

TYPE( SWIMgridStr ), POINTER :: grd

LOGICAL, EXTERNAL :: GetMCWformat

INTEGER, EXTERNAL :: InitLambert,  Lambert2LL
INTEGER, EXTERNAL :: InitPolar,    Polar2LL
INTEGER, EXTERNAL :: InitRotPolar, RotPolar2LL
INTEGER, EXTERNAL :: InitRotLL,    RotLL2LL
INTEGER, EXTERNAL :: InitMercator, Mercator2LL
REAL,    EXTERNAL :: cosd

!==== Initialize

Get2DMetData = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL LoadStart( start )

!------ Read project file

  IF( .NOT.MemoryField )THEN
    CALL ReadProject( Project )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ Build file name

  IF( gridID == 1 )THEN
    file_tmp = file_mcw
  ELSE
    i = LEN_TRIM(file_mcw)
    WRITE(file_tmp,'(A,I1.0)')file_mcw(1:i-1),gridID
  END IF

!------ Check format

  lformat = GetMCWformat()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Skip model header section

  CALL SkipModelHeader()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Skip to requested time
!       (Need number of 2d/3d fields)

  ALLOCATE( nMet2D(1),nMet3D(1),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Error allocating 2d/3d var arrays'
    GOTO 9999
  END IF

  IF( lformat )THEN
    READ(lun_mcw,*,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,'(6(I12,1X))',IOSTAT=ios) imax,jmax,kmax,ndum,nMet3D(1),nMet2D(1)
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,'(6(F12.4,1X))',IOSTAT=ios) (dum,k=1,kmax+11)
  ELSE
    READ(lun_mcw,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios) imax,jmax,kmax,ndum,nMet3D(1),nMet2D(1)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  IF( imax > nx .OR. jmax > ny )THEN
    nError   = IV_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Grid size is too small to read fields from MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  ALLOCATE( MetVar2D(1,nMet2D(1)),STAT=ios )
  IF( ios /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Error allocating 2d field names'
    GOTO 9999
  END IF

  IF( lformat )THEN
    IF( ndum > 0 )THEN
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) (cdum,n=1,ndum), &
                  (cdum,n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    ELSE
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) &
                  (cdum,n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    END IF
  ELSE
    IF( ndum > 0 )THEN
      READ(lun_mcw,IOSTAT=ios) (cdum,n=1,ndum), &
                  (cdum,n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    ELSE
      READ(lun_mcw,IOSTAT=ios) &
                  (cdum,n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    END IF
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    eInform  = 'Reading 2d field names'
    GOTO 9999
  END IF

  REWIND( lun_mcw,IOSTAT=ios )

  CALL SkipModelHeader()
  IF( nError /= NO_ERROR )GOTO 9999

  nMet3D(1) = nMet3D(1) + 1 !For vector field in met plotting utility

!------ Skip to requested time

  DO i = 1,TimeID-1
    CALL SkipMetTime( 1 )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

!------ Get grid extents

  CALL SkipMetHeader( imax,jmax,kmax )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Skip 3d fields

  DO i = 1,nMet3D(1)-1
    CALL SkipMet3D( imax,jmax,kmax )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

!------ Build list of output indices

  ALLOCATE( varOut(nMet2d(1)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Get2DMetData'
    eMessage = 'Error allocating array for MET output list'
    GOTO 9999
  END IF

  varOut = -1
  ilon   = 0
  ilat   = 0
  lDoLL  = .FALSE.
  nread  = numFields

  DO i = 1,numFields

    IF( TRIM(nameFields(i)%string) == 'LON' )THEN  !Check for "computed" fields (not read directly)
      ilon  = i
      lDoLL = .TRUE.
      nread = nread - 1
      CYCLE
    ELSE IF( TRIM(nameFields(i)%string) == 'LAT' )THEN
      ilat  = i
      lDoLL = .TRUE.
      nread = nread - 1
      CYCLE
    END IF

    SELECT CASE( TRIM(nameFields(i)%string) ) !Translate certain names
      CASE(  'ELEV' )
        vname = 'REL'
      CASE( 'ALBEDO' )
        vname = 'ALBD'
      CASE( 'BOWEN' )
        vname = 'BR'
      CASE( 'CANOPY_HEIGHT' )
        vname = 'HCNP'
      CASE( 'CANOPY_ALPHA' )
        vname = 'ALPH'
      CASE( 'PBL_HEIGHT' )
        vname = 'ZI'
      CASE( 'SFC_HFLX' )
        vname = 'HFLX'
      CASE( 'USTAR' )
        vname = 'USTR'
      CASE DEFAULT
        vname = nameFields(i)%string
    END SELECT

    found = .FALSE.

    DO j = 1,nMet2d(1)
      IF( TRIM(vname) == TRIM(MetVar2D(1,j)) )THEN
        varOut(j) = i
        found     = .TRUE.
        EXIT
      END IF
    END DO

    IF( .NOT.found )THEN
      nError   = IV_ERROR
      eRoutine = 'Get2DMetData'
      eMessage = 'Invalid 2D field name: '//TRIM(nameFields(i)%string)
      GOTO 9999
    END IF

  END DO

  IF( lDoLL )THEN
    grd => MetGrid(gridID)
    IF( grd%coord%type == I_LATLON )THEN
      nError   = IV_ERROR
      eRoutine = 'Get2DMetData'
      eMessage = 'Invalid 2D field name: LAT/LON'
      eInform  = 'Met grid is lat/lon'
      GOTO 9999
    END IF
  END IF

!------ Read 2d data

  n2 = 0
  DO j = 1,nMet2d(1)
    IF( varOut(j) == -1 )THEN
      CALL SkipMet2D( imax,jmax )
    ELSE
      CALL Read2DMetField( nx,ny,data(1,1,varOut(j)),imax,jmax )
      n2 = n2 + 1
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
    IF( n2 == nread )EXIT
  END DO

!------ Set lat/lon if requested

  IF( lDoLL )THEN

    SELECT CASE( grd%coord%type )

      CASE( I_CARTESIAN, I_METERS )
        IF( grd%coord%type == I_METERS )THEN
          xmap = 1.
        ELSE
          xmap = 1.E3
        END IF

        ymap = SPHFACR * xmap
        xmap = ymap/cosd( grd%coord%reference%lat )

        DO j = 1,grd%ny
          IF( ilat > 0 )THEN
            y = grd%ymin + FLOAT(j-1)*grd%dy - grd%coord%reference%y
            data(1:grd%nx,j,ilat) = grd%coord%reference%lat + y*ymap
          END IF
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx - grd%coord%reference%x
            IF( ilon > 0 )data(i,j,ilon) = grd%coord%reference%lon + x*xmap
          END DO
        END DO

      CASE( I_UTM )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = UTM2LL( grd%coord%zone,x,y,lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

      CASE( I_LAMBERT )
        irv = InitLambert( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Lat2,grd%coord%Rearth, &
                           grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = Lambert2LL( x,y,grd%coord%Lon0,grd%coord%n,grd%coord%f,grd%coord%y0,lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

      CASE( I_POLAR )
        irv = InitPolar( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Rearth, &
                         grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = Polar2LL( x,y,grd%coord%Lon0,grd%coord%n,grd%coord%f,grd%coord%y0,lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

      CASE( I_RPOLAR )
        irv = InitRotPolar( grd%coord%Lat0,grd%coord%Lon0, &
                            grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                            grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0  )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = RotPolar2LL( x,y,grd%coord%Rearth,grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                                                grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0, &
                                                lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

      CASE( I_ROTLL )
        irv = InitRotLL( grd%coord%Lat0,grd%coord%sp0,grd%coord%cp0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = RotLL2LL( x,y,grd%coord%lon0,grd%coord%sp0,grd%coord%cp0,lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

      CASE( I_MERCATOR )
        irv = InitMercator( grd%coord%Lat1,Rearth*1.E-3,grd%coord%f,grd%coord%m0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = Mercator2LL( x,y,grd%coord%Lon0,grd%coord%f,lat,lon )
            IF( ilon > 0 )data(i,j,ilon) = lon
            IF( ilat > 0 )data(i,j,ilat) = lat
          END DO
        END DO

    END SELECT

  END IF

END IF

Get2DMetData = SCIPsuccess

9999 CONTINUE

CLOSE( lun_mcw,IOSTAT=irv )

IF( .NOT.MemoryField )CALL deallocate_read_prj()

CALL ClearPlotLists()

CALL AbortClear()

!==== finish

CALL reset_messaging( )

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

INTEGER FUNCTION Get3DMetData( userID,Project,gridID,timeID,numFields, &
                               nameFields,centerFlag,nx,ny,nz,data )

!------ Set met plot allocation requirements

USE files_fi
USE error_fi
USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE plotlist_fi
USE plotmet_fd
USE plotmet_fi
USE met_fi, ONLY : nBaseMet, MetGrid
USE datums
USE SWIMparam_fd
USE SWIMgridStr_fd
USE constants_fd
USE abort

IMPLICIT NONE

INTEGER,                               INTENT( IN  ) :: userID
TYPE( projectIDT ),                    INTENT( IN  ) :: Project
INTEGER,                               INTENT( IN  ) :: gridID
INTEGER,                               INTENT( IN  ) :: TimeID
INTEGER,                               INTENT( IN  ) :: numFields
TYPE( char64T ), DIMENSION(numFields), INTENT( IN  ) :: nameFields
INTEGER,         DIMENSION(numFields), INTENT( IN  ) :: centerFlag
INTEGER,                               INTENT( IN  ) :: nx, ny, nz
REAL,   DIMENSION(nx,ny,nz,numFields), INTENT( OUT ),              &
                                       TARGET        :: data

INTEGER currentState, irv, ios, alloc_stat
INTEGER i, j, k, n, iv, itype, n3, nread
INTEGER iushft, jushft, ivshft, jvshft, iuLL, ivLL, iz3D, iw
INTEGER imax, jmax, kmax, ndum
REAL    x, y, lat, lon, rot, s, c, dum
REAL    Ptop, Ztop, hmin, fac, a1, a2, pt, riso, ziso, aiso, r, ps, Psrf, zMSL
LOGICAL found, lDoLL, lDoZ3D, lNeedU, lNeedV

CHARACTER(8) vname, cdum

INTEGER, DIMENSION(:),     ALLOCATABLE :: varOut
REAL,    DIMENSION(:),     ALLOCATABLE :: sigma
REAL,    DIMENSION(:,:),   ALLOCATABLE :: terr
REAL,    DIMENSION(:,:,:), ALLOCATABLE,               &
                                TARGET :: udata, vdata
REAL,    DIMENSION(:,:,:),     POINTER :: pu, pv

TYPE( startT ) start

TYPE( SWIMgridStr ), POINTER :: grd

INTERFACE
  SUBROUTINE RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,dataOut,kmax )
    INTEGER,                     INTENT( IN  ) :: i, j
    INTEGER,                     INTENT( IN  ) :: iuLL, ivLL
    INTEGER,                     INTENT( IN  ) :: nx, ny, nz
    REAL,                        INTENT( IN  ) :: rot
    REAL, DIMENSION(:,:,:),      POINTER       :: pu, pv
    REAL, DIMENSION(nx,ny,nz,*), INTENT( OUT ) :: dataOut
    INTEGER,                     INTENT( IN  ) :: kmax
  END SUBROUTINE RotateUV_vertPrf
END INTERFACE

LOGICAL, EXTERNAL :: GetMCWformat
INTEGER, EXTERNAL :: InitLambert, LL2Lambert, Lambert2LL
INTEGER, EXTERNAL :: InitPolar,   LL2Polar,   Polar2LL
INTEGER, EXTERNAL :: InitRotPolar,LL2RotPolar,RotPolar2LL
INTEGER, EXTERNAL :: InitRotLL,   LL2RotLL,   RotLL2LL
INTEGER, EXTERNAL :: InitMercator,LL2Mercator,Mercator2LL
REAL,    EXTERNAL :: SIND, COSD

!==== Initialize

Get3DMetData = SCIPfailure

IF( SCIMgrCheckState(HS_IDLEWAIT) )THEN     !Available during any callback or while idle
  IF( SCIMgrCheckState(HS_WAITSTATE) )THEN
    MemoryField = .TRUE.    !Plot from memory during run
  ELSE
    MemoryField = .FALSE.
  END IF
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

IF( .NOT.MemoryField )CALL SetupFileNames( Project )

IF( .NOT.Aborted() )THEN

  CALL init_error()

  CALL LoadStart( start )

!------ Read project file

  IF( .NOT.MemoryField )THEN
    CALL ReadProject( Project )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ Build file name

  IF( gridID == 1 )THEN
    file_tmp = file_mcw
  ELSE
    i = LEN_TRIM(file_mcw)
    WRITE(file_tmp,'(A,I1.0)')file_mcw(1:i-1),gridID
  END IF

!------ Point to met grid structure (read from project file)

  grd => MetGrid(gridID)

!------ Check format

  lformat = GetMCWformat()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Skip model header section

  CALL SkipModelHeader()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Skip to requested time
!       (Need number of 2d/3d fields)

  ALLOCATE( nMet2D(1),nMet3D(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error allocating 2d/3d var arrays'
    GOTO 9999
  END IF

  IF( lformat )THEN
    READ(lun_mcw,*,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,'(6(I12,1X))',IOSTAT=ios) imax,jmax,kmax,ndum,nMet3D(1),nMet2D(1)
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
    IF( ios == 0 )READ(lun_mcw,*,IOSTAT=ios)
  ELSE
    READ(lun_mcw,IOSTAT=ios)                 !Skip format record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip codename record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip time record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios) imax,jmax,kmax,ndum,nMet3D(1),nMet2D(1)
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip dummy record
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios)   !Skip dummy record
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  IF( imax > nx .OR. jmax > ny .OR. kmax > nz )THEN
    nError   = IV_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Grid size is too small to read fields from MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  ALLOCATE( MetVar3D(1,nMet3D(1)),MetVar2D(1,nMet2D(1)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError = UK_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error allocating 2d/3d field names'
    GOTO 9999
  END IF

  ALLOCATE( sigma(kmax),STAT=alloc_stat ) !To read sigma just in case it's needed
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error allocating 1D sigma array'
    GOTO 9999
  END IF

  IF( lformat )THEN
    IF( ios == 0 )READ(lun_mcw,'(6(F12.4,1X))',IOSTAT=ios) (sigma(k),k=1,kmax),(dum,k=1,10),Ztop
  ELSE
    IF( ios == 0 )READ(lun_mcw,IOSTAT=ios) (sigma(k),k=1,kmax),(dum,k=1,10),Ztop
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  IF( lformat )THEN
    IF( ndum > 0 )THEN
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) (cdum,n=1,ndum), &
                  (MetVar3D(1,n),n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    ELSE
      READ(lun_mcw,'(6(A8,1X))',IOSTAT=ios) &
                  (MetVar3D(1,n),n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    END IF
  ELSE
    IF( ndum > 0 )THEN
      READ(lun_mcw,IOSTAT=ios) (cdum,n=1,ndum), &
                  (MetVar3D(1,n),n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    ELSE
      READ(lun_mcw,IOSTAT=ios) &
                  (MetVar3D(1,n),n=1,nMet3D(1)),(cdum,n=1,nMet3D(1)), &
                  (MetVar2D(1,n),n=1,nMet2D(1))
    END IF
  END IF

  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
    eInform  = 'Reading 3d field names'
    GOTO 9999
  END IF

  REWIND( lun_mcw,IOSTAT=ios )

  nMet3D(1) = nMet3D(1) + 1 !For vector field in met plotting utilities

!------ Read special header section

  ALLOCATE( pMetGrid(1),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error allocating grid structure'
    GOTO 9999
  END IF

  CALL ReadModelHeader( 1,itype,Ptop )

!------ Skip to requested time

  DO i = 1,TimeID-1
    CALL SkipMetTime( 1 )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

!------ Get grid extents

  CALL SkipMetHeader( imax,jmax,kmax )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Build list of output indices
!       N.B. Assumes U & V are first two 3D fields

  ALLOCATE( varOut(nMet3d(1)),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'Get3DMetData'
    eMessage = 'Error allocating array for MET output list'
    GOTO 9999
  END IF

  varOut = -1
  iuLL   = 0
  ivLL   = 0
  iz3D   = 0
  iw     = 0
  lDoLL  = .FALSE.
  lDoZ3D = .FALSE.
  nread  = numFields

  DO i = 1,numFields

    IF( TRIM(nameFields(i)%string) == 'ULL' )THEN  !Check for "computed" fields (not read directly)
      iuLL  = i
      lDoLL = .TRUE.
      nread = nread - 1
      CYCLE
    ELSE IF( TRIM(nameFields(i)%string) == 'VLL' )THEN
      ivLL  = i
      lDoLL = .TRUE.
      nread = nread - 1
      CYCLE
    ELSE IF( TRIM(nameFields(i)%string) == 'Z3D' )THEN
      IF( pMetGrid(1)%coordVert%type /= I_Z3D )THEN
        iz3D   = i
        lDoZ3D = .TRUE.
        nread  = nread - 1
        CYCLE
      END IF
    END IF

    SELECT CASE( TRIM(nameFields(i)%string) )  !Translate certain names
      CASE(  'THETA' )
        vname = 'T'
      CASE( 'HUMID' )
        vname = 'H'
      CASE( 'Z3D' )
        vname = 'Z'
      CASE DEFAULT
        vname = nameFields(i)%string
    END SELECT

    found = .FALSE.

    DO iv = 1,nMet3d(1)-1
      IF( TRIM(vname) == TRIM(MetVar3D(1,iv)) )THEN
        varOut(iv) = i
        IF( TRIM(vname) == 'W' )iw = iv
        found     = .TRUE.
        EXIT
      END IF
    END DO

    IF( .NOT.found )THEN
      nError   = IV_ERROR
      eRoutine = 'Get3DMetData'
      eMessage = 'Invalid 3D field name: '//TRIM(nameFields(i)%string)
      GOTO 9999
    END IF

  END DO

!------ Checks for LL velocity output
!       N.B. LL velocity output is always at cell centers

  IF( lDoLL )THEN

    IF( grd%coord%type == I_LATLON )THEN
      nError   = IV_ERROR
      eRoutine = 'Get3DMetData'
      eMessage = 'Invalid 3D field name: ULL/VLL'
      eInform  = 'U,V are in lat/lon coordinates'
      GOTO 9999
    END IF

    lNeedU = varOut(1) == 0 .OR. &
             (BTEST(itype,GTB_STAGGER) .AND. centerFlag(varOut(1)) /= 1)

    IF( lNeedU )THEN
      ALLOCATE( udata(nx,ny,nz),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'Get3DMetData'
        eMessage = 'Error allocating 3D U-array'
        GOTO 9999
      END IF
    END IF

    lNeedV = varOut(2) == 0 .OR. &
             (BTEST(itype,GTB_STAGGER) .AND. centerFlag(varOut(2)) /= 1)

    IF( lNeedV )THEN
      ALLOCATE( vdata(nx,ny,nz),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'Get3DMetData'
        eMessage = 'Error allocating 3D V-array'
        GOTO 9999
      END IF
    END IF

  ELSE

    lNeedU = .FALSE.
    lNeedV = .FALSE.

  END IF

!------ Setup parameters for averaging horizontal velocity

  IF( ANY((/varOut(1)>0,varOut(2)>0,lNeedU,lNeedV/))  )THEN

    iushft = 0; jushft = 0;
    ivshft = 0; jvshft = 0;
    IF( BTEST(itype,GTB_STAGGER) )THEN
      iushft = 1; jvshft = 1
      IF( BTEST(itype,GTB_STAGGERB) )THEN
        jushft = 1; ivshft = 1
      END IF
    END IF

  END IF

!------ Set to position file after 3d fields if 3d height field is to be computed

  IF( lDoZ3D )nread = nMet3d(1)-1

!------ Read 3d data; put U,V onto cell centers if requested

  n3 = 0

  DO iv = 1,nMet3d(1)-1

!------ Skip fields that don't go into output array except for U,V if needed for ULL,VLL

    IF( varOut(iv) == -1 )THEN

      IF( lNeedU .AND. iv == 1 )THEN  !Check if U is needed for ULL,VLL

        CALL Read3DMetField( nx,ny,nz,udata,imax,jmax,kmax )
        IF( nError /= NO_ERROR )GOTO 9999

        IF( BTEST(itype,GTB_STAGGER) )THEN
          CALL CenterAvg( iushft,jushft,nx,ny,nz,udata,imax,jmax,kmax )
        END IF

      ELSE IF( lNeedV .AND. iv == 2 )THEN  !Check if V is needed for ULL,VLL

        CALL Read3DMetField( nx,ny,nz,vdata,imax,jmax,kmax )
        IF( nError /= NO_ERROR )GOTO 9999

        IF( BTEST(itype,GTB_STAGGER) )THEN
          CALL CenterAvg( ivshft,jvshft,nx,ny,nz,vdata,imax,jmax,kmax )
        END IF

      ELSE

        CALL SkipMet3D( imax,jmax,kmax )
        IF( nError /= NO_ERROR )GOTO 9999

      END IF

!------ Read fields directly into output array

    ELSE

      CALL Read3DMetField( nx,ny,nz,data(1,1,1,varOut(iv)),imax,jmax,kmax )
      IF( nError /= NO_ERROR )GOTO 9999

      n3 = n3 + 1

      IF( iv == 1 )THEN  !Check U for cell center output and copying for ULL,VLL

        IF( BTEST(itype,GTB_STAGGER) .AND. centerFlag(varOut(iv)) == 1 )THEN
          CALL CenterAvg( iushft,jushft,nx,ny,nz,data(1,1,1,varOut(iv)),imax,jmax,kmax )
        END IF

        IF( lNeedU )THEN

          DO k = 1,nz
            DO j = 1,ny
              DO i = 1,nx
                udata(i,j,k) = data(i,j,k,varOut(iv))
              END DO
            END DO
          END DO

          CALL CenterAvg( iushft,jushft,nx,ny,nz,udata,imax,jmax,kmax ) !Must average if lNeedU here

        END IF

      ELSE IF( iv == 2 )THEN  !Check V for cell center output and copying for ULL,VLL

        IF( BTEST(itype,GTB_STAGGER) .AND. centerFlag(varOut(iv)) == 1 )THEN
          CALL CenterAvg( ivshft,jvshft,nx,ny,nz,data(1,1,1,varOut(iv)),imax,jmax,kmax )
        END IF

        IF( lNeedV )THEN

          DO k = 1,nz
            DO j = 1,ny
              DO i = 1,nx
                vdata(i,j,k) = data(i,j,k,varOut(iv))
              END DO
            END DO
          END DO

          CALL CenterAvg( ivshft,jvshft,nx,ny,nz,vdata,imax,jmax,kmax ) !Must average if lNeedV here

        END IF

      ELSE IF( iv == iw )THEN  !Check W for cell center output

        IF( BTEST(itype,GTB_STAGGERZ) .AND. centerFlag(varOut(iv)) == 1 )THEN

          DO k = kmax,2,-1
            DO j = 1,jmax
              DO i = 1,imax
                data(i,j,k,varOut(iv)) = 0.5*(data(i,j,k,varOut(iv)) + data(i,j,k-1,varOut(iv)))
              END DO
            END DO
          END DO
          DO j = 1,jmax
            DO i = 1,imax
              data(i,j,1,varOut(iv)) = 0.5*data(i,j,1,varOut(iv))
            END DO
          END DO

        END IF

      END IF

    END IF

    IF( n3 == nread )EXIT

  END DO

!------ Set lat/lon velocity components if requested
!       N.B. Native coordinate components are already on cell centers

  IF( lDoLL )THEN

    IF( lNeedU )THEN
      pu => udata
    ELSE
      pu => data(1:nx,1:ny,1:nz,varOut(1))
    END IF

    IF( lNeedV )THEN
      pv => vdata
    ELSE
      pv => data(1:nx,1:ny,1:nz,varOut(2))
    END IF

    SELECT CASE( grd%coord%type )

      CASE( I_CARTESIAN, I_METERS )
        nError   = IV_ERROR
        eRoutine = 'Get3DMetData'
        eMessage = 'Invalid 3D field name: LAT/LON'
        eInform  = 'Met grid is cartesian'
        GOTO 9999

      CASE( I_UTM, I_MERCATOR )

        IF( iuLL > 0 )THEN
          DO j = 1,grd%ny
            DO i = 1,grd%nx
              data(i,j,1:kmax,iuLL) = pu(i,j,1:kmax)
            END DO
          END DO
        END IF
        IF( ivLL > 0 )THEN
          DO j = 1,grd%ny
            DO i = 1,grd%nx
              data(i,j,1:kmax,ivLL) = pv(i,j,1:kmax)
            END DO
          END DO
        END IF

      CASE( I_LAMBERT )
        irv = InitLambert( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Lat2,grd%coord%Rearth, &
                           grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = Lambert2LL( x,y,grd%coord%Lon0,grd%coord%n,grd%coord%f,grd%coord%y0,lat,lon )
            rot = (lon - grd%coord%Lon0)*PI180 * grd%coord%n
            CALL RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,data,kmax )
          END DO
        END DO

      CASE( I_POLAR )
        irv = InitPolar( grd%coord%Lat0,grd%coord%Lat1,grd%coord%Rearth, &
                         grd%coord%n,grd%coord%f,grd%coord%m0,grd%coord%y0 )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = Polar2LL( x,y,grd%coord%Lon0,grd%coord%n,grd%coord%f,grd%coord%y0,lat,lon )
            rot = (lon - grd%coord%Lon0)*PI180
            CALL RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,data,kmax )
          END DO
        END DO

      CASE( I_RPOLAR )
        irv = InitRotPolar( grd%coord%Lat0,grd%coord%Lon0, &
                            grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                            grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0  )
        DO j = 1,grd%ny
          y = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            x = grd%xmin + FLOAT(i-1)*grd%dx
            irv = RotPolar2LL( x,y,grd%coord%Rearth,grd%coord%sp0,grd%coord%cp0,grd%coord%sl0,grd%coord%cl0, &
                                                grd%coord%cc0,grd%coord%sc0,grd%coord%cs0,grd%coord%ss0, &
                                                lat,lon )
            rot = SIND(lat)
            c   = 1. + rot**2
            s   = (c*grd%coord%sp0 + grd%coord%cp0**2*rot)*SIND(lon-grd%coord%Lon0)
            c   = c*COSD(lon-grd%coord%Lon0) + grd%coord%cp0*COSD(lat)
            rot = ATAN2(s,c)
            CALL RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,data,kmax )
          END DO
        END DO

      CASE( I_ROTLL )
        irv = InitRotLL( grd%coord%Lat0,grd%coord%sp0,grd%coord%cp0 )
        DO j = 1,grd%ny
          lat = grd%ymin + FLOAT(j-1)*grd%dy
          DO i = 1,grd%nx
            lon = grd%xmin + FLOAT(i-1)*grd%dx
            c   = grd%coord%cp0*COSD(lat) - grd%coord%sp0*SIND(lat)*COSD(lon)
            s   = grd%coord%sp0*SIND(lon)
            rot = ATAN2(s,c)
            CALL RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,data,kmax )
          END DO
        END DO

    END SELECT

  END IF

!------ Build 3D height field if necessary

  IF( lDoZ3D )THEN

!------ Check for terrain; read if on file

    found = nMet2d(1) > 1
    IF( found )found = TRIM(MetVar2D(1,1)) == 'REL'

    IF( found )THEN

      ALLOCATE( terr(imax,jmax),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'Get3DMetData'
        eMessage = 'Error allocating terrain array for 3D height calculation'
        GOTO 9999
      END IF

      CALL ReadMet2D( terr,imax,jmax )
      IF( nError /= NO_ERROR )GOTO 9999

      IF( pMetGrid(1)%coordVert%type == I_SIGMAZM )THEN

        hmin = HUGE(0.)
        DO j = 1,imax
          DO i = 1,imax
            hmin = MIN(terr(i,j),hmin)
          END DO
        END DO

        DO j = 1,imax
          DO i = 1,imax
            terr(i,j) = terr(i,j)-hmin
          END DO
        END DO

      END IF

      SELECT CASE( pMetGrid(1)%coordVert%type )

        CASE( I_SIGMAZM,I_SIGMAZ )

          DO j = 1,imax
            DO i = 1,imax
              fac = 1. - terr(i,j)/Ztop
              data(i,j,1:kmax,iZ3D) = sigma(1:kmax)*fac
            END DO
          END DO

        CASE( I_SIGMAP )

          a2 = -RGAS* pMetGrid(1)%coordVert%lapseRef/(2.*G0)
          a1 = -RGAS* pMetGrid(1)%coordVert%TsrfRef/G0
          pt =  pMetGrid(1)%coordVert%Ptop/pMetGrid(1)%coordVert%P00

          IF(  pMetGrid(1)%coordVert%Tiso > 0. ) THEN
            riso = (pMetGrid(1)%coordVert%Tiso-pMetGrid(1)%coordVert%TsrfRef)/pMetGrid(1)%coordVert%lapseRef
            ziso = a2*riso*riso + a1*riso
            aiso = RGAS* pMetGrid(1)%coordVert%Tiso/G0
          ELSE
            riso = -HUGE(0.)
          END IF

          DO j = 1,ny
            DO i = 1,nx

              r  = -(a1 + SQRT(a1*a1 + 4.*a2*terr(i,j)))/ (2.*a2)
              ps = EXP(r)
              Psrf = ps - pt

              DO k = 1,kmax
                r  = LOG(sigma(k)*Psrf + pt)
                IF( r > riso )THEN
                  zMSL = a2*r*r + a1*r
                ELSE
                  zMSL = ziso + aiso*(riso-r)
                END IF
                data(i,j,k,iZ3D) = zMSL - terr(i,j)
              END DO

            END DO
          END DO

        END SELECT

    ELSE

      DO k = 1,kmax
        DO j = 1,imax
          DO i = 1,imax
            data(i,j,k,iZ3D) = sigma(k)
          END DO
        END DO
      END DO

    END IF

  END IF

END IF

Get3DMetData = SCIPsuccess

9999 CONTINUE

CLOSE( lun_mcw,IOSTAT=ios )

IF( ALLOCATED(varOut) )DEALLOCATE( varOut,STAT=alloc_stat )
IF( ALLOCATED(udata)  )DEALLOCATE( udata, STAT=alloc_stat )
IF( ALLOCATED(vdata)  )DEALLOCATE( vdata, STAT=alloc_stat )
IF( ALLOCATED(sigma)  )DEALLOCATE( sigma, STAT=alloc_stat )
IF( ALLOCATED(terr)   )DEALLOCATE( terr,  STAT=alloc_stat )

IF( .NOT.MemoryField )CALL deallocate_read_prj()

CALL ClearPlotLists()

CALL AbortClear()

!==== finish

CALL reset_messaging( )

irv = SCIMgrSetState( currentState )

RETURN
END

!==============================================================================

LOGICAL FUNCTION GetMCWformat() RESULT( IsFormatted )

!------ Check format of a MEDOC file;
!       Opens and then rewinds file but leaves open
!       N.B. Intended for met output file with name in file_tmp

USE files_fi
USE error_fi

INTEGER ios

CHARACTER(8) fflag

IsFormatted = .TRUE.

OPEN( FILE=TRIM(file_tmp),UNIT=lun_mcw,STATUS='OLD',ACTION='READ',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'GetMCWformat'
  eMessage = 'Error opening MET output file: '//TRIM(file_tmp)
  WRITE(eInform,'(A,I5)')'IOSTAT=',ios
  GOTO 9999
END IF

READ(lun_mcw,'(A8)',IOSTAT=ios) fflag
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'GetMCWformat'
  eMessage = 'Error reading MET input file'
  CALL ReportFileName( eInform,'File=',file_tmp )
  GOTO 9999
END IF

!------ If first record begins with 'F', this is a formatted MEDOC file

IF( fflag(1:1) /= 'F' )THEN

  CLOSE(lun_mcw,IOSTAT=ios)
  OPEN(UNIT=lun_mcw,FILE=TRIM(file_tmp),STATUS='OLD',FORM='UNFORMATTED', &
                                           ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'GetMCWformat'
    eMessage = 'Error opening MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF
  READ(lun_mcw,IOSTAT=ios) fflag

  IF( fflag(1:1) /= 'B' )THEN
    nError   = UK_ERROR
    eRoutine = 'GetMCWformat'
    eMessage = 'Unrecognized MET output file: '//TRIM(file_tmp)
    GOTO 9999
  END IF

  IsFormatted = .FALSE.

ELSE

  IsFormatted = .TRUE.

END IF

REWIND(lun_mcw,IOSTAT=ios)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE Read3DMetField( n1,n2,n3,wrk,imax,jmax,kmax )

USE files_fi
USE error_fi
USE plotmet_fi

IMPLICIT NONE

INTEGER,                   INTENT( IN  ) :: n1, n2, n3
REAL, DIMENSION(n1,n2,n3), INTENT( OUT ) :: wrk
INTEGER,                   INTENT( IN  ) :: imax, jmax, kmax

INTEGER i, j, k, ios

IF( lformat )THEN
  READ(lun_mcw,'(6(F12.4,1X))',IOSTAT=ios) (((wrk(i,j,k),i=1,imax),j=1,jmax),k=1,kmax)
ELSE
  READ(lun_mcw,IOSTAT=ios) (((wrk(i,j,k),i=1,imax),j=1,jmax),k=1,kmax)
END IF

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'Read3DMetField'
  eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
END IF

RETURN
END

!==============================================================================

SUBROUTINE Read2DMetField( n1,n2,wrk,imax,jmax )

USE files_fi
USE error_fi
USE plotmet_fi

IMPLICIT NONE

INTEGER,                INTENT( IN  ) :: n1, n2
REAL, DIMENSION(n1,n2), INTENT( OUT ) :: wrk
INTEGER,                INTENT( IN  ) :: imax, jmax

INTEGER i, j, ios

IF( lformat )THEN
  READ(lun_mcw,'(6(F12.4,1X))',IOSTAT=ios) ((wrk(i,j),i=1,imax),j=1,jmax)
ELSE
  READ(lun_mcw,IOSTAT=ios) ((wrk(i,j),i=1,imax),j=1,jmax)
END IF

IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'Read2DMetField'
  eMessage = 'Error reading MET output file: '//TRIM(file_tmp)
END IF

RETURN
END

!==============================================================================

SUBROUTINE CenterAvg( ishft,jshft,nx,ny,nz,wrk,imax,jmax,kmax )

!------ Average staggered 3D field wrk onto cell centers

USE error_fi

IMPLICIT NONE

INTEGER,                   INTENT( IN  )   :: ishft, jshft
INTEGER,                   INTENT( IN  )   :: nx, ny, nz
REAL, DIMENSION(nx,ny,nz), INTENT( INOUT ) :: wrk
INTEGER,                   INTENT( IN    ) :: imax, jmax, kmax

INTEGER alloc_stat, i, j, k, im, jm

REAL, DIMENSION(:,:), ALLOCATABLE :: zslice

INTEGER, EXTERNAL :: limit

IF( ishft == 0 .AND. jshft == 0 )RETURN

ALLOCATE( zslice(imax,jmax),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'CenterAvg'
  eMessage = 'Error allocating array for averaging onto cell centers'
  GOTO 9999
END IF

DO k = 1,kmax

  DO j = 1,jmax
    DO i = 1,imax
      zslice(i,j) = wrk(i,j,k)
    END DO
  END DO

  DO j = 1,jmax
    jm = limit( j-jshft,1,jmax )
    DO i = 1,imax
      im = limit( i-ishft,1,imax )
      wrk(i,j,k) = 0.25*(zslice(i,j)+zslice(im,j)+zslice(i,jm)+zslice(im,jm))
    END DO
  END DO

END DO

9999 CONTINUE

IF( ALLOCATED(zslice) )DEALLOCATE( zslice,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE RotateUV_vertPrf( i,j,iuLL,ivLL,nx,ny,nz,rot,pu,pv,dataOut,kmax )

IMPLICIT NONE

INTEGER,                     INTENT( IN  ) :: i, j
INTEGER,                     INTENT( IN  ) :: iuLL, ivLL
INTEGER,                     INTENT( IN  ) :: nx, ny, nz
REAL,                        INTENT( IN  ) :: rot
REAL, DIMENSION(:,:,:),      POINTER       :: pu, pv
REAL, DIMENSION(nx,ny,nz,*), INTENT( OUT ) :: dataOut
INTEGER,                     INTENT( IN  ) :: kmax

REAL c, s

IF( ABS(rot) < 0.001 )THEN
  IF( iuLL > 0 )dataOut(i,j,1:kmax,iuLL) = pu(i,j,1:kmax)
  IF( ivLL > 0 )dataOut(i,j,1:kmax,ivLL) = pv(i,j,1:kmax)
  RETURN
END IF

c = COS(rot);  s = SIN(rot)

IF( iuLL > 0 )dataOut(i,j,1:kmax,iuLL) =  c*pu(i,j,1:kmax) + s*pv(i,j,1:kmax)
IF( ivLL > 0 )dataOut(i,j,1:kmax,ivLL) = -s*pu(i,j,1:kmax) + c*pv(i,j,1:kmax)

RETURN
END
