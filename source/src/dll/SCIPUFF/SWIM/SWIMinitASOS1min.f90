INTEGER FUNCTION SWIMinitASOS1min( Obs )

!------ Set FirstObs structure
!       Position file to read obs for first met field

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER,      PARAMETER :: MAXN = 50
INTEGER,      PARAMETER :: NVAR_MAX = 20
CHARACTER(1), PARAMETER :: QQ    = CHAR(34)  !Double quote
CHARACTER(1), PARAMETER :: COMMA = CHAR(44)
CHARACTER(1), PARAMETER :: SPACE = CHAR(32)

TYPE ObsTimeStr
  REAL                        :: time
  INTEGER                     :: rec
  LOGICAL                     :: goodVel
  TYPE( ObsTimeStr ), POINTER :: next
  TYPE( ObsTimeStr ), POINTER :: prev
END TYPE ObsTimeStr

TYPE( ObsTimeStr ), POINTER :: FirstObsTime, LastObsTime, ObsTimeRec

INTEGER ios, lun, lun1, lun2, irv, nch, n_arg, nv, i, j, i1, i2, nrec0, nrec
INTEGER iyear, imonth, iday, ihr, imn, jhr, jmn, jdate
REAL    timeObs, timeBin, timeObsLast
LOGICAL lerr, lend

CHARACTER(256) line0, line, kwrd
CHARACTER(128) c_arg(MAXN)
CHARACTER(64)  StaID, StaString, StaID0

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: var_name, var_unit
REAL(8),      DIMENSION(:), ALLOCATABLE :: var8

TYPE ( messageT ) :: caution

INTEGER, EXTERNAL :: SWIMaddLogMessage, SWIMwarningMessage
INTEGER, EXTERNAL :: SWIMobsID
INTEGER, EXTERNAL :: SWIMsetSrfRef, SWIMreadVarObs
REAL,    EXTERNAL :: SWIMtimeOffset
LOGICAL, EXTERNAL :: SWIMcheckVel
INTEGER, EXTERNAL :: PostCautionMessage

SWIMinitASOS1min = SWIMfailure

Obs%lASOSthermo = .FALSE.
Obs%AuxSource   = ''

!------ Make sure this is a year-month-day calculation

IF( Prj%yearStart == NOT_SET_I )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Year and date required when using ASOS 1 minute data'
  GOTO 9999
END IF

!------ Parse file name(s) and location

i = INDEX(Obs%Source,QQ,BACK=.TRUE.) !Index of last QQ

line  = TRIM(Obs%Source(2:i-1)) !Assume first and last characters are QQ
line0 = Obs%Source(i+1:)        !Lat/lon, if given

nch = LEN_TRIM(line)
i1 = -1; i2 = -1
DO j = 1,nch                   !Look for 2nd file name
  IF( line(j:j) == QQ )THEN
    IF( i1 == -1 )THEN
      i1 = j
    ELSE IF( i2 == -1 )THEN
      i2 = j
    END IF
  END IF
END DO

IF( i1*i2 < 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'ASOS wind and thermodynamic file names not specified correctly with qoutes'
  error%Inform  = 'Bad string: '//TRIM(Obs%Source)
  GOTO 9999
END IF

IF( i1 > -1 )THEN
  Obs%Source      = TRIM(line(1:i1-1))
  Obs%lASOSthermo = .TRUE.
  Obs%AuxSource   = TRIM(line(i2+1:))
ELSE
  Obs%Source = TRIM(line)
END IF

CALL get_next_data( 0,line0,nch,kwrd,n_arg,c_arg,MAXN,lerr ) !Read lat/lon

IF( n_arg < 2 )THEN

  IF( lISDavail )THEN
    Obs%Lat = NOT_SET_R
    Obs%Lon = NOT_SET_R
  ELSE
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitASOS1min'
    error%Message = 'Latitude/longitude not specified for ASOS 1 minute data'
    GOTO 9999
  END IF

ELSE

  READ(c_arg(1),*,IOSTAT=ios) Obs%Lat
  IF( ios /= 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitASOS1min'
    error%Message = 'Error reading latitude for ASOS 1 minute data'
    GOTO 9999
  END IF

  READ(c_arg(2),*,IOSTAT=ios) Obs%Lon
  IF( ios /= 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitASOS1min'
    error%Message = 'Error reading longitude for ASOS 1 minute data'
    GOTO 9999
  END IF

END IF

CALL ReportFileName( line,'Reading obs file ',Obs%Source )
irv = SWIMaddLogMessage( line )
IF( irv /= SWIMsuccess )GOTO 9999

IF( LEN_TRIM(Obs%AuxSource) == 0 )THEN
  CALL ReportFileName( line,'Reading associated file ',Obs%AuxSource )
  irv = SWIMaddLogMessage( line )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Open obs file

lun = Obs%unit

OPEN(UNIT=lun,FILE=Obs%Source,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Error opening ASOS 1 minute data file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

IF( Obs%lASOSthermo )THEN
  lun1 = lun + 1
  OPEN(UNIT=lun1,FILE=Obs%AuxSource,STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'SWIMinitASOS1min'
    error%Message = 'Error opening ASOS 1 minute thermodynamic data file'
    CALL ReportFileName( error%Inform,'File= ',Obs%AuxSource )
    GOTO 9999
  END IF
ELSE
  lun1 = lun
END IF

Obs%type  = 0
Obs%local = .FALSE. !*****
Obs%tFcst = NOT_SET_R

Obs%lWarnTooMuchObsDAta = .TRUE.  !Do not report extra variables in SWIMreadVarObs

Prj%localMet = Obs%local !*****

irv = SWIMaddLogMessage( 'ASOS 1 minute data' )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Parse first line for ID

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
IF( lerr )GOTO 9999

StaID0 = TRIM(c_arg(1)) !WBAN number in 1st 5 characters
StaID  = StaID0

REWIND( lun )

!------ Read ISD history file if available
!       Get start date and lat/lon if not specified
!       N.B. Check for multiple listings; use latest date
!       N.B. Assumes first 2 strings between quotes contain no blanks
!       Similarly, assumes last 5 (2 dates, elev. & lat/lon) contain no blanks

IF( lISDavail )THEN

  lun2 = lun1+1
  OPEN(lun2,FILE=TRIM(ISDhistoryFile),ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = OP_ERROR
    error%Routine = 'SWIMinitASOS1min'
    error%Message = 'Error opening ISD History file'
    error%Inform  = 'Attempting to read lat/lon of ASOS 1 minute data station'
    CALL ReportFileName( error%Action,'File= ',ISDhistoryFile )
    GOTO 9999
  END IF

  jdate = 0

  DO

    READ(lun2,'(A)',IOSTAT=ios) line0
    IF( ios /= 0 )EXIT

    nch = LEN_TRIM(line0)
    DO i = 1,nch
      IF( line0(i:i) == COMMA )line0(i:i) = SPACE
    END DO

    CALL get_next_data( 0,line0,nch,kwrd,n_arg,c_arg,MAXN,lerr )
    IF( lerr )GOTO 9999

    nch = LEN_TRIM(c_arg(2))

    IF( StaID(1:5) == c_arg(2)(2:nch-1) )THEN  !Match string between quoates

      i = n_arg - 1
      nch = LEN_TRIM(c_arg(i))
      c_arg(i) = c_arg(i)(2:nch-1)
      READ(c_arg(i),'(I8)',IOSTAT=ios) iyear
      IF( ios /= 0 )EXIT

      IF( iyear > jdate )THEN
        jdate = iyear
        line  = line0  !Save entire line for latest entry to read lat/lon
      END IF

    END IF

  END DO

  CLOSE(lun2,IOSTAT=ios)

  IF( jdate > 0 )THEN

    iyear = Prj%yearStart
    CALL set_year( iyear )

    iyear = 10000*iyear + 100*Prj%monthStart + Prj%dayStart

    IF( iyear < jdate )THEN
      caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMinitASOS1min'
      caution%aString = 'Calculation starts before valid date of ASOS 1 minute data'
      caution%bString = 'Station = '//TRIM(StaID)
      WRITE(caution%cString,'(A,I8)') 'Observation start date: ',jdate
      irv = PostCautionMessage( caution )
    END IF

    IF( Obs%lat == NOT_SET_R )THEN  !Read lat/lon if not set

      CALL get_next_data( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
      IF( lerr )GOTO 9999

      i = n_arg - 4
      nch = LEN_TRIM(c_arg(i))
      IF( nch > 2 )THEN
        c_arg(i) = c_arg(i)(2:nch-1)
        READ(c_arg(i),*,IOSTAT=ios) Obs%Lat
        IF( ios /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SWIMinitASOS1min'
          error%Message = 'Error reading latitude for ASOS 1 minute data from ISD history file'
          error%Inform  = 'Station = '//TRIM(StaID)
          CALL ReportFileName( error%Action,'File= ',ISDhistoryFile )
          GOTO 9999
        END IF
      END IF

      i = i + 1
      nch = LEN_TRIM(c_arg(i))
      IF( nch > 2 )THEN
        c_arg(i) = c_arg(i)(2:nch-1)
        READ(c_arg(i),*,IOSTAT=ios) Obs%Lon
        IF( ios /= 0 )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SWIMinitASOS1min'
          error%Message = 'Error reading longitude for ASOS 1 minute data from ISD history file'
          error%Inform  = 'Station = '//TRIM(StaID)
          CALL ReportFileName( error%Action,'File= ',ISDhistoryFile )
          GOTO 9999
        END IF
      END IF

      IF( Obs%lat /= NOT_SET_R .AND. Obs%lon /= NOT_SET_R )THEN
        CALL ReportFileName( line0,'Read ASOS station lat/lon from ',ISDhistoryFile )
        irv = SWIMaddLogMessage( line0 )
        IF( irv /= SWIMsuccess )GOTO 9999
        WRITE(line0,'(A,F7.3,A,F8.3,A)') 'Station = '//TRIM(StaID)//'  (Lat,Lon) = (',Obs%lat,',',Obs%lon,')'
        irv = SWIMaddLogMessage( line0 )
        IF( irv /= SWIMsuccess )GOTO 9999
      END IF

    END IF

  END IF

END IF

IF( Obs%lat == NOT_SET_R .OR.  Obs%lon == NOT_SET_R )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Lat/lon not set for ASOS 1 minute data'
  error%Inform  = 'ISD History file not available or station not found'
  error%Action  = 'Specify lat/lon with ASOS file name'
  GOTO 9999
END IF

!------- Parse time for UTC offset

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
IF( lerr )GOTO 9999

READ( c_arg(2),'(3X,I4,2I2,2I2,2I2)',IOSTAT=ios) iyear,imonth,iday,ihr,imn,jhr,jmn
IF( ios /= 0 )THEN
  error%Inform = 'Error parsing date and time of ASOS 1 minute data'
  GOTO 9999
END IF

DO WHILE( ihr /= 0 )
  CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
  IF( lerr )GOTO 9999
  READ( c_arg(2),'(3X,I4,2I2,2I2,2I2)',IOSTAT=ios) iyear,imonth,iday,ihr,imn,jhr,jmn
  IF( ios /= 0 )THEN
    error%Inform = 'Error finding 00 hour for ASOS 1 minute data'
    GOTO 9999
  END IF
END DO

jhr = jhr-ihr
IF( jhr > 12 )jhr = jhr - 24

Obs%hourOffset = jhr

WRITE(c_arg(1),'(A,I2)') 'Offset (hours) from UTC = ',jhr
irv = SWIMaddLogMessage( c_arg(1) )
IF( irv /= SWIMsuccess )GOTO 9999

ihr = INT(Obs%lon+7.5)/15
IF( Obs%lon < -7.5 )ihr = ihr - 1.
ihr = -ihr

IF( ihr /= jhr )THEN
  caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMinitASOS1min'
  caution%aString = 'Offset from UTC of ASOS 1 minute data differs from nominal based on longitude'
  WRITE(caution%bString,'(A,F8.3)') 'Station = '//TRIM(StaID)//'  Longitude = ',Obs%lon
  WRITE(caution%cString,'(A,I3,A,I3,A)') 'Nominal:',ihr,'   From file:',jhr,' hours'
  irv = PostCautionMessage( caution )
END IF

REWIND(lun,IOSTAT=ios)

!------ Set fixed number of variables

Obs%nVarFixed = 3  !ID + LL
Obs%nVar      = Obs%nVarFixed + 4 + 2  !Year,month,day,hour + wind speed & direction

IF( Obs%lASOSthermo )Obs%nVar = Obs%nVar + 3 !Temperature, pressure, relative humidity

Obs%type = IBSET(Obs%type,OTB_SRF)

!------ Arrays for variables names and units

nv = Obs%nVar

ALLOCATE( var_name(nv),var_unit(nv),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Error allocating variable name and unit arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

var_name(1) = 'ID'
var_unit(1) = ' '

var_name(2) = 'LAT'
var_unit(2) = 'N'

var_name(3) = 'LON'
var_unit(3) = 'E'

Obs%lASOS1   = .TRUE.
Obs%lAERMET  = .FALSE.
Obs%AERid    = TRIM(StaID)
Obs%AER_UAid = 'NOT_USED'

Obs%nASOS1repeat = 0

Obs%BaseElev = NOT_SET_R

!------ Set time-varying variable names:

i = Obs%nVarFixed

var_name(i+1) = 'YEAR' ; var_unit(i+1) = ' '
var_name(i+2) = 'MONTH'; var_unit(i+2) = ' '
var_name(i+3) = 'DAY'  ; var_unit(i+3) = ' '
var_name(i+4) = 'HOUR' ; var_unit(i+4) = ' '
var_name(i+5) = 'WDIR' ; var_unit(i+5) = ''
var_name(i+6) = 'WSPD' ; var_unit(i+6) = 'KTS'

IF( Obs%lASOSthermo )THEN
  i = i+6
  var_name(i+1) = 'P'; var_unit(i+1) = 'IN_HG' !Inches of mercury
  var_name(i+2) = 'T'; var_unit(i+2) = 'C'     !Dry bulb temperatrue (converted from degF)
  var_name(i+3) = 'H'; var_unit(i+3) = '%'     !RH based on dew point and dry bulb temp.
END IF

!------ Missing/bad data string; overall reference height assumed to be 10m

Obs%BadString  = '-999.'  !This string not utilized

Obs%zref = 10.

!------ Setup indicies and conversion factors for obs variables

ALLOCATE( Obs%VarID(Obs%nVar),Obs%Conv(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitASOS1min'
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
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Error allocating arrays for obs record'
  GOTO 9999
END IF

Obs%nz = 1
ALLOCATE( Obs%z(1),STAT=ios)
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitASOS1min'
  error%Message = 'Error allocating height array for surface obs'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF
Obs%z(1) = Obs%zref

!------ Avoid reading obs during CREATE

IF( Prj%Create )THEN
  Obs%time       = NOT_SET_R
  Obs%PrevTime   = NOT_SET_R
  Obs%NumObs     = 0
  Obs%NumObsDom  = 0
  SWIMinitASOS1min = SWIMresult
  GOTO 9999
END IF

!------ Read first time on obs file and initialize linked-list to keep track of
!       records associated with different times

var8 = DBLE(NOT_SET_R)

NULLIFY( FirstObsTime,LastObsTime,ObsTimeRec )
ALLOCATE( FirstObsTime,LastObsTime,STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitASOS1min'
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

  IF( TRIM(StaID) /= 'NOT SET' )StaString = TRIM(StaID0)

  IF( lend )EXIT !end-of-file

!------ Get binned time for this record

  CALL SWIMgetTimeObs( Obs,var8,timeObs,timeBin )
  IF( error%Number /= NO_ERROR )GOTO 9999

!------ Error if time < previous obs time

  IF( timeObs < timeObsLast )THEN

    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitASOS1min'
    CALL ReportFileName( error%Message,'Non-sequential times on obs file ',Obs%Source )
    WRITE(error%Inform,'(A,F8.1,A,F8.1)')'New time (from start of run) = ',timeObs/3600., &
                                         ' Previous time = ',timeObsLast/3600.
    error%Action = 'Obs station='//TRIM(StaString)
    IF( Obs%lASOS1 )CALL ReportASOSerror( Obs)
    GOTO 9999

!------ New obs time

  ELSE IF( timeBin > ObsTimeRec%time )THEN

    ALLOCATE( ObsTimeRec%next,STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinitASOS1min'
      error%Message = 'Error allocating time record pointer'
      GOTO 9999
    END IF
    ObsTimeRec%next%prev => ObsTimeRec
    ObsTimeRec => ObsTimeRec%next
    ObsTimeRec%time = timeBin
    ObsTimeRec%rec  = nrec0
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
    IF( Obs%lASOSthermo )BACKSPACE(Obs%unit+1,IOSTAT=ios)
  END DO
  nrec0 = nrec

  IF( ObsTimeRec%goodVel )EXIT ReadTimeLoop   !Found at least 1 good obs

  IF(  ASSOCIATED(ObsTimeRec%prev) )THEN      !Go to previous time

      ObsTimeRec => ObsTimeRec%prev
      CYCLE ReadTimeLoop

  ELSE                                        !No good times; read forward

    DO nrec = 1,LastObsTime%rec-nrec0-1
      READ(Obs%unit,*,IOSTAT=ios)
      IF( Obs%lASOSthermo )READ(Obs%unit+1,IOSTAT=ios)
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

SWIMinitASOS1min = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var_name) )DEALLOCATE( var_name,STAT=ios )
IF( ALLOCATED(var_unit) )DEALLOCATE( var_unit,STAT=ios )
IF( ALLOCATED(var8)     )DEALLOCATE( var8,STAT=ios )

RETURN
END

!==============================================================================

SUBROUTINE ReportASOSerror( Obs)

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( IN ) :: Obs

INTEGER ios, irv

CHARACTER(256) line, linex

TYPE ( messageT ) :: caution

INTEGER, EXTERNAL :: PostCautionMessage

BACKSPACE( Obs%unit,IOSTAT=ios)      !Read previous line
READ(Obs%unit,'(A)',IOSTAT=ios) line

IF( Obs%lASOSthermo )THEN
  BACKSPACE( Obs%unit+1,IOSTAT=ios)
  READ(Obs%unit+1,'(A)',IOSTAT=ios) linex
END IF

caution%iParm = 0; caution%jParm = 0; caution%routine = ''
CALL ReportFileName( caution%aString,'Error reading ASOS file ',Obs%Source )
caution%bString = 'Error may have occured reading the following or nearby line:'
caution%cString = TRIM(line)
irv = PostCautionMessage( caution )

IF( Obs%lASOSthermo )THEN
  CALL ReportFileName( caution%aString,'Also read thermodynamics from ',Obs%AuxSource )
  caution%bString = 'Last line read was or near the following:'
  caution%cString = TRIM(linex)
  irv = PostCautionMessage( caution )
END IF

caution%aString = 'Check for time/date discrepancies or other anomalies'
caution%bString = 'You may be able edit the file(s) and re-try'
caution%cString = ''
irv = PostCautionMessage( caution )

RETURN
END

!==============================================================================

REAL FUNCTION humid( db,dp ) RESULT( rh )

! --- Calculate relative humidity (percent) given the dry-bulb and dew
!     point temperatures. DB is dry bulb temperature in C, and
!     DP is dew point temperature in C.  Percent relative humidity (RH)
!     is returned.

!**** Taken from AERMET

IMPLICIT NONE

REAL, INTENT( IN ) :: db, dp

REAL ttdb, ttdp, vpsat, vpact
REAL dbk, dpk

dbk = db + 273.15
dpk = dp + 273.15

ttdb = (1.0/273.15) - (1.0/dbk)
ttdp = (1.0/273.15) - (1.0/dpk)

vpsat = 6.11 * EXP(5418.0 * ttdb)
vpact = 6.11 * EXP(5418.0 * ttdp)

rh = 100. * (vpact/vpsat)

rh = MAX(rh,0.)
rh = MIN(rh,100.)

RETURN
END
