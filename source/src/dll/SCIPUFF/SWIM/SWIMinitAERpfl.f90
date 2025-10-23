INTEGER FUNCTION SWIMinitAERpfl( Obs )

!------ Read obs file header and set FirstObs structure accordingly
!       Position file to read obs for first met field

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER,      PARAMETER :: MAXN = 50
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

TYPE ObsTimeStr
  REAL                        :: time
  INTEGER                     :: rec
  LOGICAL                     :: goodVel
  TYPE( ObsTimeStr ), POINTER :: next
  TYPE( ObsTimeStr ), POINTER :: prev
END TYPE ObsTimeStr

TYPE( ObsTimeStr ), POINTER :: FirstObsTime, LastObsTime, ObsTimeRec

INTEGER ios, lun, irv, nch, n_arg, nv, i, nrec0, nrec, iz
REAL    timeObs, timeBin, timeObsLast, z
LOGICAL lerr, lend, lFixedData

CHARACTER(256) line, kwrd
CHARACTER(128) c_arg(MAXN)
CHARACTER(64)  StaID, StaString

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: var_name, var_unit
REAL(8),      DIMENSION(:), ALLOCATABLE :: var8

INTEGER, EXTERNAL :: SWIMobsKeyword, SWIMobsName, SWIMobsID, SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMsetSrfRef, SWIMreadVarObs
REAL,    EXTERNAL :: SWIMtimeOffset
LOGICAL, EXTERNAL :: SWIMcheckVel

SWIMinitAERpfl = SWIMfailure

!------ Parse file name and supplementary data

i = INDEX(Obs%Source,QQ,BACK=.TRUE.)

line = TRIM(Obs%Source(i+1:))

#if defined(LINUX)
CALL get_next_data_NO_CUPPER( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
#else
CALL get_next_data( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
#endif

Obs%Source = Obs%Source(2:i-1)

#if defined(LINUX)
CALL get_next_data( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )

#endif
CALL ReportFileName( line,'Reading obs file ',Obs%Source )
irv = SWIMaddLogMessage( line )
IF( irv /= SWIMsuccess )GOTO 9999

IF( n_arg > 0 )THEN
  StaID = TRIM(c_arg(1))
  CALL StripLeading0( StaID )
ELSE
  StaID = NOT_SET_C
END IF

!------ Search for matching SFC file to get location

Obs%lAERMET = .FALSE.
Obs%lASOS1  = .FALSE.

Obs%nASOS1repeat = 0

DO i = 1,numObsSrc
  IF( ObsSrc(i)%lAERMET )THEN
    IF( TRIM(StaID) == TRIM(ObsSrc(i)%AERid)    .OR. &
        TRIM(StaID) == TRIM(ObsSrc(i)%AER_UAid) )THEN
      Obs%lAERMET = .TRUE.
      Obs%AERid   = TRIM(StaID)
      Obs%Lat     = ObsSrc(i)%Lat
      Obs%Lon     = ObsSrc(i)%Lon
      EXIT
    END IF
  END IF
END DO

IF( .NOT.Obs%lAERMET )THEN
  IF( numObsSrc == 2 )THEN
    Obs%lAERMET = .TRUE.
    Obs%AERid   = TRIM(ObsSrc(1)%AERid)
    Obs%Lat     = ObsSrc(1)%Lat
    Obs%Lon     = ObsSrc(1)%Lon
  ELSE
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitAERpfl'
    error%Message = 'ID required to match AERMET SFC file'
    error%Inform  = '(SFC file must come before PFL in list)'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999
  END IF
END IF

!------ Loop for profile base elevation

Obs%BaseElev = NOT_SET_R

DO i = 1,n_arg
  IF( TRIM(c_arg(i)) == 'BASELEV' )THEN
    IF( n_arg > i )THEN
      READ(c_arg(i+1),*,IOSTAT=ios) Obs%BaseElev
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMinitAERpfl'
        error%Message = 'Error reading profile base elevation'
        CALL ReportFileName( error%Inform,'File= ',Obs%Source )
        GOTO 9999
      END IF
    END IF
    EXIT
  END IF
END DO

!------ Open obs file

lun = Obs%unit

OPEN(UNIT=lun,FILE=Obs%Source,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error opening obs file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%type  = 0
Obs%local = .TRUE. !*****Prj%localMet
Obs%tFcst = NOT_SET_R

Prj%localMet = Obs%local !*****


!------ Set fixed number of variables

Obs%type = IBSET(Obs%type,OTB_PRF)
Obs%nVar      = 14 !11 time-varying + LL + ID
Obs%nVarFixed = 3  !LL + ID
lFixedData    = .TRUE.

!------ Allocate arrays for variables names and units

nv = Obs%nVar

ALLOCATE( var_name(nv),var_unit(nv),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error allocating variable name and unit arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

!------ Set fixed variable names and units

var_name(1) = 'ID'
var_unit(1) = ' '

var_name(2) = 'LAT'
var_unit(2) = 'N'

var_name(3) = 'LON'
var_unit(3) = 'E'

!------ Set time-varying variable names

i = Obs%nVarFixed

var_name(i+ 1) = 'YEAR' ; var_unit(i+ 1) = ' '
var_name(i+ 2) = 'MONTH'; var_unit(i+ 2) = ' '
var_name(i+ 3) = 'DAY'  ; var_unit(i+ 3) = ' '
var_name(i+ 4) = 'HOUR' ; var_unit(i+ 4) = ' '
var_name(i+ 5) = 'Z'    ; var_unit(i+ 5) = 'M'
var_name(i+ 6) = 'TOP'  ; var_unit(i+ 6) = ' '
var_name(i+ 7) = 'WDIR' ; var_unit(i+ 7) = 'DEG'
var_name(i+ 8) = 'WSPD' ; var_unit(i+ 8) = 'M/S'
var_name(i+ 9) = 'T'    ; var_unit(i+ 9) = 'C'
var_name(i+10) = 'SIGD' ; var_unit(i+10) = 'DEG'
var_name(i+11) = 'SIGW' ; var_unit(i+11) = 'M/S'

!------ Missing/bad data string; overall reference height (not set)

Obs%BadString  = '-999.'  !This string not utilized uniformly; need to check individual values

!------ Setup indicies and conversion factors for obs variables

ALLOCATE( Obs%VarID(Obs%nVar),Obs%Conv(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error allocating obs variable id and conversion factor arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

irv = SWIMaddLogMessage( 'Obs variables:' )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMobsID( nv,var_name,var_unit,Obs )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set time parameters

Obs%timeOffset = SWIMtimeOffset(); IF( error%Number /= NO_ERROR )GOTO 9999
Obs%timeBin    = Prj%timeBin

!------ Build vertical grid (in case required for met field)
!       Set vertical grid based on first profile

ALLOCATE( var8(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error allocating arrays for obs record'
  GOTO 9999
END IF

DO i = Obs%nVarFixed+1,nv
  IF( Obs%VarID(i) == OVP_Z )THEN
    iz = i; EXIT
  END IF
END DO

i = Obs%nVarFixed

Obs%nz = 0
z      = -10.

DO
  READ(lun,*,IOSTAT=ios) var8(i+1:nv)
  IF( ios > 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitAERpfl'
    error%Message = 'Error reading PFL data'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  ELSE IF( ios < 0 )THEN
    REWIND(lun,IOSTAT=ios); EXIT
  END IF
  IF( var8(iz) > z )THEN
    Obs%nz = Obs%nz + 1
    z = var8(iz)
  ELSE
    REWIND(lun,IOSTAT=ios); EXIT
  END IF
END DO

ALLOCATE( Obs%z(Obs%nz),STAT=ios)
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error allocating height array for surface obs'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

DO nrec = 1,Obs%nz
  READ(lun,*,IOSTAT=ios) var8(i+1:nv)
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitAERsfc'
    error%Message = 'Error reading PFL data'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF
  Obs%z(nrec) = var8(iz)
END DO

REWIND(lun,IOSTAT=ios)

!------ Avoid reading obs during CREATE

IF( Prj%Create )THEN
  Obs%time       = NOT_SET_R
  Obs%PrevTime   = NOT_SET_R
  Obs%NumObs     = 0
  Obs%NumObsDom  = 0
  SWIMinitAERpfl = SWIMresult
  GOTO 9999
END IF

!------ Read first time on obs file and initialize linked-list to keep track of
!       records associated with different times

var8 = DBLE(NOT_SET_R)

NULLIFY( FirstObsTime,LastObsTime,ObsTimeRec )
ALLOCATE( FirstObsTime,LastObsTime,STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERpfl'
  error%Message = 'Error allocating pointers for linked-lists'
  GOTO 9999
END IF

irv = SWIMreadVarObs( Obs,var8,lend,nrec0,StaID )
IF( irv /= SWIMsuccess )GOTO 9999

StaString = TRIM(StaID)

FirstObsTime%rec = nrec0

IF( lFixedData )THEN
  irv = SWIMreadVarObs( Obs,var8,lend,nrec,StaID )
  IF( irv /= SWIMsuccess )GOTO 9999
  nrec0 = nrec0 + nrec
END IF

CALL SWIMgetTimeObs( Obs,var8,timeObs,timeBin )
IF( error%Number /= NO_ERROR )GOTO 9999

FirstObsTime%time    = timeBin
FirstObsTime%goodVel = SWIMcheckVel( Obs,var8 )
NULLIFY(FirstObsTime%next,FirstObsTime%prev)

timeObsLast = timeObs

!------ Set analysis time for forecast type obs

IF( BTEST(Obs%type,OTB_FCST) )THEN
  IF( Obs%tFcst == NOT_SET_R )THEN
    Obs%tFcst = timeBin
  ELSE
    Obs%tFcst = Obs%tFcst - Obs%timeOffset  !Was set without time offset in SWIMobsKeyword
  END IF
END IF

ObsTimeRec => FirstObsTime

!------ Read until obs time is greater than SCIPUFF time, keeping track of records
!       in linked-list

DO

!------ Read record; increment total record count

  irv = SWIMreadVarObs( Obs,var8,lend,nrec,StaID )
  IF( irv /= SWIMsuccess )GOTO 9999
  nrec0 = nrec0 + nrec

!------ Read next "height" record if this is an 'ID:' record (to fill var8)

  IF( TRIM(StaID) /= 'NOT SET' )THEN
    IF( lFixedData )THEN
      irv = SWIMreadVarObs( Obs,var8,lend,nrec,StaID )
      IF( irv /= SWIMsuccess )GOTO 9999
      nrec0 = nrec0 + nrec
    END IF
    StaString = TRIM(StaID)
  END IF

  IF( lend )EXIT !end-of-file

!------ Get binned time for this record

  CALL SWIMgetTimeObs( Obs,var8,timeObs,timeBin )
  IF( error%Number /= NO_ERROR )GOTO 9999

!------ Error if time < previous obs time

  IF( timeObs < timeObsLast )THEN

    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitAERpfl'
    CALL ReportFileName( error%Message,'Non-sequential times on obs file ',Obs%Source )
    WRITE(error%Inform,'(A,F8.1,A,F8.1)')'New time (from start of run) = ',timeObs/3600., &
                                         ' Previous time = ',timeObsLast/3600.
    error%Action = 'Obs station='//TRIM(StaString)
    GOTO 9999

!------ New obs time

  ELSE IF( timeBin > ObsTimeRec%time )THEN

    ALLOCATE( ObsTimeRec%next,STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinitAERpfl'
      error%Message = 'Error allocating time record pointer'
      GOTO 9999
    END IF
    ObsTimeRec%next%prev => ObsTimeRec
    ObsTimeRec => ObsTimeRec%next
    ObsTimeRec%time    = timeBin
    IF( lFixedData )THEN
      ObsTimeRec%rec = nrec0 - nrec
    ELSE
      ObsTimeRec%rec = nrec0
    END IF
    ObsTimeRec%goodVel = SWIMcheckVel( Obs,var8 )
    NULLIFY(ObsTimeRec%next)
    IF( timeBin > Prj%Time )EXIT
    timeObsLast = timeObs

!------ Same as current obs time; just check for good velocity

  ELSE

    IF( .NOT.ObsTimeRec%goodVel )ObsTimeRec%goodVel = SWIMcheckVel( Obs,var8 )
    timeObsLast = timeObs

  END IF

END DO

LastObsTime => ObsTimeRec
IF( .NOT.lend )THEN
  IF( ASSOCIATED(LastObsTime%prev) )THEN
    ObsTimeRec  => LastObsTime%prev
  END IF
END IF

!------ Setup Obs linked-list for first time <= SCIPUFF time with at least one good obs
!       Otherwise, read forward to first time > SCIPUFF time

ReadTimeLoop : DO

  DO nrec = nrec0,ObsTimeRec%rec,-1
    BACKSPACE(Obs%unit,IOSTAT=ios)
  END DO
  nrec0 = nrec

  IF( ObsTimeRec%goodVel )EXIT ReadTimeLoop   !Found at least 1 good obs

  IF(  ASSOCIATED(ObsTimeRec%prev) )THEN      !Go to previous time

      ObsTimeRec => ObsTimeRec%prev
      CYCLE ReadTimeLoop

  ELSE                                        !No good times; read forward

    DO nrec = 1,LastObsTime%rec-nrec0-1
      READ(Obs%unit,*,IOSTAT=ios)
    END DO
    nrec0 = nrec0 + LastObsTime%rec-nrec0-1
    EXIT ReadTimeLoop

  END IF

END DO ReadTimeLoop

!------ Deallocate time record linked-list

ObsTimeRec => FirstObsTime
DO WHILE( ASSOCIATED(ObsTimeRec%next) )
  ObsTimeRec => ObsTimeRec%next
  DEALLOCATE(ObsTimeRec%prev,STAT=ios); NULLIFY(ObsTimeRec%prev)
END DO
DEALLOCATE(ObsTimeRec,STAT=ios)

!------ Initial time 'not set'

Obs%time     = NOT_SET_R
Obs%PrevTime = NOT_SET_R

Obs%NumObs        = 0
Obs%NumObsDom     = 0
Obs%PrevNumObs    = 0
Obs%PrevNumObsDom = 0

Obs%PrevZmax = 0.

Obs%lend = .FALSE.

!------ Nullify linked-list for nearest obs search

NULLIFY( Obs%GridList,Obs%PrevGridList )

SWIMinitAERpfl = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var_name) )DEALLOCATE( var_name,STAT=ios )
IF( ALLOCATED(var_unit) )DEALLOCATE( var_unit,STAT=ios )
IF( ALLOCATED(var8)     )DEALLOCATE( var8,STAT=ios )

RETURN
END
