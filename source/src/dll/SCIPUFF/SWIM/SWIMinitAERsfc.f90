INTEGER FUNCTION SWIMinitAERsfc( Obs )

!------ Read obs file header and set FirstObs structure accordingly
!       Position file to read obs for first met field

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER,      PARAMETER :: AERMETver0 = 14134
INTEGER,      PARAMETER :: MAXN = 50
INTEGER,      PARAMETER :: NVAR_MAX = 20
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

TYPE ObsTimeStr
  REAL                        :: time
  INTEGER                     :: rec
  LOGICAL                     :: goodVel
  TYPE( ObsTimeStr ), POINTER :: next
  TYPE( ObsTimeStr ), POINTER :: prev
END TYPE ObsTimeStr

TYPE( ObsTimeStr ), POINTER :: FirstObsTime, LastObsTime, ObsTimeRec

INTEGER ios, lun, irv, iAERMETver
INTEGER nch, n_arg, nv, i, nrec0, nrec
REAL    timeObs, timeBin, timeObsLast
LOGICAL lerr, lend, lFixedData

CHARACTER(256) line, kwrd
CHARACTER(124) c_arg(MAXN)
CHARACTER(64)  StaID, StaString, StaID0

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: var_name, var_unit
REAL(8),      DIMENSION(:), ALLOCATABLE :: var8

TYPE ( messageT ) :: caution

INTEGER, EXTERNAL :: SWIMaddLogMessage, SWIMwarningMessage
INTEGER, EXTERNAL :: SWIMobsID, SWIMsetSrfRef, SWIMreadVarObs
REAL,    EXTERNAL :: SWIMtimeOffset
LOGICAL, EXTERNAL :: SWIMcheckVel
INTEGER, EXTERNAL :: PostCautionMessage

SWIMinitAERsfc = SWIMfailure

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
  StaID0 = TRIM(c_arg(1)); CALL StripLeading0( StaID0 )
ELSE
  StaID0 = NOT_SET_C
END IF

IF( n_arg > 1 )StaString = TRIM(c_arg(2))

!------ Open obs file

lun = Obs%unit

OPEN(UNIT=lun,FILE=Obs%Source,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error opening obs file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%type  = 0
Obs%local = .TRUE. !*****Prj%localMet
Obs%tFcst = NOT_SET_R

Prj%localMet = Obs%local !*****

!------ Set fixed number of variables
!       First check number of variables in file and AERMET version

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,-MAXN,lerr )
IF( lerr )GOTO 9999

iAERMETver = NOT_SET_I
DO i = 1,n_arg
  IF( c_arg(i)(1:7) == 'VERSION' )THEN
    IF( LEN_TRIM(c_arg(i)) == 13 )THEN
      READ(c_arg(i)(9:13),*,IOSTAT=ios) iAERMETver
    ELSE IF( n_arg > i )THEN
      READ(c_arg(i+1),*,IOSTAT=ios) iAERMETver
    END IF
    EXIT
  END IF
END DO

caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMinitAERsfc'
IF( iAERMETver > AERMETver0 )THEN
  caution%aString = 'AERMET file version number beyond last supported version'
  WRITE(caution%bString,'(A,I5,A,I5)',IOSTAT=ios) 'File version: ',iAERMETver,'; Supported version: ',AERMETver0
  caution%cString = 'Results cannot be guaranteed'
  irv = PostCautionMessage( caution )
ELSE IF( iAERMETver == NOT_SET_I )THEN
  caution%aString = 'AERMET file version number not found'
  WRITE(caution%bString,'(A,I5)',IOSTAT=ios) 'Latest supported version: ',AERMETver0
  caution%cString = 'Results cannot be guaranteed'
  irv = PostCautionMessage( caution )
END IF

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,-MAXN,lerr )
IF( lerr )GOTO 9999

REWIND(lun,IOSTAT=ios)

Obs%nVarFixed = 3  !LL + ID
Obs%nVar      = Obs%nVarFixed + MIN(n_arg,NVAR_MAX)  !N.B. there are 20 "standard" time-varying variables
IF( Obs%nVar < 23 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Insufficient data in file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%type = IBSET(Obs%type,OTB_SRF)
lFixedData    = .TRUE.

!------ Allocate arrays for variables names and units

nv = Obs%nVar

ALLOCATE( var_name(nv),var_unit(nv),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error allocating variable name and unit arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

!------ Read fixed variable names and units from first record

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,-MAXN,lerr )

var_name(1) = 'ID'
var_unit(1) = ' '

StaID = TRIM(c_arg(6)); CALL StripLeading0( StaID )

IF( TRIM(StaID0) /= NOT_SET_C )THEN
  IF( TRIM(StaID) /= TRIM(StaID0) )THEN
    error%Number  = WN_ERROR
    error%Routine = 'SWIMinitAERsfc'
    error%Message = 'SFC Station ID mismatch'
    error%Inform  = 'ME ID: '//TRIM(StaID0)//', File ID: '//TRIM(StaID)
    error%Action  = 'Do you want to continue?'
    irv = SWIMwarningMessage( 'User elected not to continue' )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
END IF

var_name(2) = 'LAT'
i = LEN_TRIM(c_arg(1))
READ(c_arg(1)(1:i-1),*) Obs%Lat
IF( c_arg(1)(i:i) == 'S' )Obs%Lat = -Obs%Lat
var_unit(2) = 'N'

var_name(3) = 'LON'
i = LEN_TRIM(c_arg(2))
READ(c_arg(2)(1:i-1),*) Obs%Lon
IF( c_arg(2)(i:i) == 'W' )Obs%Lon = -Obs%Lon
var_unit(3) = 'E'

Obs%lAERMET  = .TRUE.
Obs%lASOS1   = .FALSE.
Obs%AERid    = TRIM(StaID)
Obs%AER_UAid = TRIM(c_arg(4))  !ID for corresponding upper air file

Obs%nASOS1repeat = 0

CALL StripLeading0( Obs%AER_UAid )

Obs%BaseElev = NOT_SET_R

!------ Set time-varying variable names:

i = Obs%nVarFixed

var_name(i+ 1) = 'YEAR' ; var_unit(i+ 1) = ' '
var_name(i+ 2) = 'MONTH'; var_unit(i+ 2) = ' '
var_name(i+ 3) = 'DAY'  ; var_unit(i+ 3) = ' '
var_name(i+ 4) = 'JDAY' ; var_unit(i+ 4) = ' '
var_name(i+ 5) = 'HOUR' ; var_unit(i+ 5) = ' '
var_name(i+ 6) = 'HFLUX'; var_unit(i+ 6) = 'W/M2'
var_name(i+ 7) = 'USTAR'; var_unit(i+ 7) = 'M/S'
var_name(i+ 8) = 'WSTAR'; var_unit(i+ 8) = 'M/S'
var_name(i+ 9) = 'VPTG' ; var_unit(i+ 9) = 'K/M'  !d(theta)/dz above PBL (not used)
var_name(i+10) = 'ZI'   ; var_unit(i+10) = 'M'    !Convectively-generated BL depth (PBL)
var_name(i+11) = 'SBL'  ; var_unit(i+11) = 'M'    !Mechanically-generated BL depth (SBL not used)
var_name(i+12) = 'LMO'  ; var_unit(i+12) = 'M'    !Monin-Obukhov length (not used)
var_name(i+13) = 'Z0'   ; var_unit(i+13) = 'M'    !Surface roughness
var_name(i+14) = 'B'    ; var_unit(i+14) = 'M'
var_name(i+15) = 'ALB'  ; var_unit(i+15) = 'M'
var_name(i+16) = 'WSPD' ; var_unit(i+16) = 'M/S'
var_name(i+17) = 'WDIR' ; var_unit(i+17) = 'DEG'
var_name(i+18) = 'Z'    ; var_unit(i+18) = 'M'
var_name(i+19) = 'T'    ; var_unit(i+19) = 'K'
var_name(i+20) = 'ZT'   ; var_unit(i+20) = 'M'    !Height for temperature (not used)

IF( Obs%nVar > i+20 )THEN
  DO nrec = i+21,Obs%nVar
    var_name(nrec) = 'UNUSED' ; var_unit(nrec) = ' '
  END DO
END IF

!------ Missing/bad data string; overall reference height (not set)

Obs%BadString  = '-999.'  !This string not utilized uniformly; need to check individual values

Obs%zref = -999. !Not set

!------ Setup indicies and conversion factors for obs variables

ALLOCATE( Obs%VarID(Obs%nVar),Obs%Conv(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERsfc'
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

!------ Set "vertical grid" based on first record

ALLOCATE( var8(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error allocating arrays for obs record'
  GOTO 9999
END IF

Obs%nz = 1
ALLOCATE( Obs%z(1),STAT=ios)
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error allocating height array for surface obs'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

i = Obs%nVarFixed

var8(i+NVAR_MAX+1:nv) = 0.
READ(lun,*,IOSTAT=ios) var8(i+1:i+NVAR_MAX)
IF( ios /= 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error reading SFC data'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF
DO i = Obs%nVarFixed+1,nv
  IF( Obs%VarID(i) == OVP_Z )THEN
    Obs%z(1) = var8(i)            !Assuming this value is always valid
    EXIT
  END IF
END DO

BACKSPACE(lun,IOSTAT=ios)

!------ Avoid reading obs during CREATE

IF( Prj%Create )THEN
  Obs%time       = NOT_SET_R
  Obs%PrevTime   = NOT_SET_R
  Obs%NumObs     = 0
  Obs%NumObsDom  = 0
  SWIMinitAERsfc = SWIMresult
  GOTO 9999
END IF

!------ Read first time on obs file and initialize linked-list to keep track of
!       records associated with different times

var8 = DBLE(NOT_SET_R)

NULLIFY( FirstObsTime,LastObsTime,ObsTimeRec )
ALLOCATE( FirstObsTime,LastObsTime,STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitAERsfc'
  error%Message = 'Error allocating pointers for linked-lists'
  GOTO 9999
END IF

irv = SWIMreadVarObs( Obs,var8,lend,nrec0,StaID )
IF( irv /= SWIMsuccess )GOTO 9999

StaString = TRIM(StaID)

FirstObsTime%rec = nrec0

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
    StaString = TRIM(StaID0)
  END IF

  IF( lend )EXIT !end-of-file

!------ Get binned time for this record

  CALL SWIMgetTimeObs( Obs,var8,timeObs,timeBin )
  IF( error%Number /= NO_ERROR )GOTO 9999

!------ Error if time < previous obs time

  IF( timeObs < timeObsLast )THEN

    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitAERsfc'
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
      error%Routine = 'SWIMinitAERsfc'
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
    IF( timeBin > Prj%time )EXIT
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

SWIMinitAERsfc = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var_name) )DEALLOCATE( var_name,STAT=ios )
IF( ALLOCATED(var_unit) )DEALLOCATE( var_unit,STAT=ios )
IF( ALLOCATED(var8)     )DEALLOCATE( var8,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE StripLeading0( string )

IMPLICIT NONE

CHARACTER(*), INTENT( INOUT ) :: string

INTEGER i, n

n = LEN_TRIM(string)

DO i = 1,n
  IF( string(i:i) /= '0' )EXIT
  string(i:i) = ' '
END DO

string = ADJUSTL(string)

RETURN
END

