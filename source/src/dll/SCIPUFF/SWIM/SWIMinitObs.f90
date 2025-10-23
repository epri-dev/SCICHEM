INTEGER FUNCTION SWIMinitObs( Obs )

!------ Read obs file header and set FirstObs structure accordingly
!       Position file to read obs for first met field

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: Obs

INTEGER, PARAMETER :: MAXN = 50

TYPE ObsTimeStr
  REAL                        :: time
  INTEGER                     :: rec
  LOGICAL                     :: goodVel
  TYPE( ObsTimeStr ), POINTER :: next
  TYPE( ObsTimeStr ), POINTER :: prev
END TYPE ObsTimeStr

TYPE( ObsTimeStr ), POINTER :: FirstObsTime, LastObsTime, ObsTimeRec

INTEGER ios, lun, irv, nch, n_arg, nv, i, nrec0, nrec
REAL    timeObs, timeBin, timeObsLast
LOGICAL lerr, lend, lFixedData

CHARACTER(256) line, kwrd
CHARACTER(16)  c_arg(MAXN)
CHARACTER(64)  StaID, StaString

CHARACTER(8), DIMENSION(:), ALLOCATABLE :: var_name, var_unit
REAL(8),      DIMENSION(:), ALLOCATABLE :: var8

INTEGER, EXTERNAL :: SWIMobsKeyword, SWIMobsName, SWIMobsID, SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMsetSrfRef, SWIMBuildObsGrid, SWIMreadVarObs
REAL,    EXTERNAL :: ObsTimeOffset
LOGICAL, EXTERNAL :: SWIMcheckVel

SWIMinitObs = SWIMfailure

CALL ReportFileName( line,'Reading obs file ',Obs%Source )
irv = SWIMaddLogMessage( line )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Open obs file

lun = Obs%unit

i = INDEX(Obs%Source,CHAR(34),BACK=.TRUE.) !Look for closing quote
IF( i > 0 )THEN
  Obs%Source = Obs%Source(2:i-1)
END IF

OPEN(UNIT=lun,FILE=Obs%Source,STATUS='OLD',ACTION='READ',IOSTAT=ios)
IF( ios /= 0 )THEN
  error%Number  = OP_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error opening obs file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%type  = 0
Obs%local = Prj%localMet
Obs%tFcst = NOT_SET_R

Obs%lAERMET = .FALSE.
Obs%lASOS1  = .FALSE.
Obs%AERid   = 'NOT_SET'
Obs%Lat     = NOT_SET_R
Obs%Lon     = NOT_SET_R

Obs%nASOS1repeat = 0

Obs%lWarnTooMuchObsDAta = .FALSE.

!------ Check for keyword records

DO

  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitObs'
    error%Message = 'Error reading obs file'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF

  IF( line(1:1) == '#' )THEN
    irv = SWIMobsKeyword( line,Obs )
    IF( irv /= SWIMsuccess )GOTO 9999
  ELSE
    EXIT
  END IF

END DO

!------ Over-ride surface type if MODE = PROF

IF( TRIM(ADJUSTL(line)) == 'SURFACE' .AND. BTEST(Obs%type,OTB_PRF) ) &
  line = 'PROFILE'

!------ Determine file type & read number of variables

SELECT CASE( TRIM(ADJUSTL(line)) )

  CASE( 'SURFACE' )

    READ(lun,*,IOSTAT=ios) Obs%nVar
    lerr = ios /= 0
    IF( .NOT.lerr )THEN
      Obs%nVarFixed = 0
      Obs%type = IBSET(Obs%type,OTB_SRF)
    END IF

  CASE( 'PROFILE' )

    CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
    IF( .NOT.lerr )THEN
      IF( n_arg == 1 )THEN
        Obs%nVarFixed = 0
        READ(c_arg(1),*,IOSTAT=ios) Obs%nVar
      ELSE
        READ(c_arg(1),*,IOSTAT=ios) Obs%nVarFixed
        READ(c_arg(2),*,IOSTAT=ios) Obs%nVar
      END IF
      lerr = ios /= 0
      IF( .NOT.lerr )THEN
        Obs%type = IBSET(Obs%type,OTB_PRF)
        Obs%nVar = Obs%nVarFixed + Obs%nVar
      END IF
    END IF

  CASE DEFAULT

    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitObs'
    error%Message = 'Error reading obs file'
    error%Inform  = 'File type not found (SURFACE or PROFILE)'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999

END SELECT

IF( lerr )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error reading obs file'
  error%Inform  = 'Number of variables record'
  CALL ReportFileName( error%Action,'File= ',Obs%Source )
  GOTO 9999
END IF

!------ Allocate arrays for variables names and units

nv = Obs%nVar

ALLOCATE( var_name(nv),var_unit(nv),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error allocating variable name and unit arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

lFixedData = Obs%nVarFixed > 0

!------ Read variable names and units

IF( lFixedData )THEN
  irv = SWIMobsName( lun,Obs%nVarFixed,var_name,var_unit )
  IF( irv /= SWIMsuccess )THEN
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999
  END IF
END IF

irv = SWIMobsName( lun,nv-Obs%nVarFixed,var_name(Obs%nVarFixed+1),var_unit(Obs%nVarFixed+1) )
IF( irv /= SWIMsuccess )THEN
  CALL ReportFileName( error%Action,'File= ',Obs%Source )
  GOTO 9999
END IF

DO i = 1,nv
  CALL cupper( var_name(i) )
  CALL cupper( var_unit(i) )
END DO

!------ Read missing/bad data string

CALL get_next_data( lun,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
IF( lerr )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error reading bad data string'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
ELSE IF( n_arg > 2 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error reading bad data string'
  error%Inform  = 'Record with bad data string may be missing'
  CALL ReportFileName( error%Action,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%BadString  = TRIM(ADJUSTL(c_arg(1)))

IF( LEN_TRIM(Obs%BadString) > 8 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Bad data string cannot exceed 8 characters'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

IF( INDEX(TRIM(Obs%BadString),' ') > 0 )THEN
  error%Number  = RD_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Bad data string cannot contain blanks'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

irv = SWIMaddLogMessage( 'Obs bad data string = '//TRIM(Obs%BadString) )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read reference height for surface variables

Obs%zref = -999. !Not set

IF( BTEST(Obs%type,OTB_SRF) .AND. n_arg > 1 )THEN
  READ(c_arg(2),*,IOSTAT=ios) Obs%zref
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'SWIMinitObs'
    error%Message = 'Error reading reference height'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF
  IF( Obs%zref <= 0.0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMinitObs'
    error%Message = 'Reference height must be positive'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF
END IF

!------ Setup indicies and conversion factors for obs variables

ALLOCATE( Obs%VarID(Obs%nVar),Obs%Conv(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error allocating obs variable id and conversion factor arrays'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

irv = SWIMaddLogMessage( 'Obs variables:' )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMobsID( nv,var_name,var_unit,Obs )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set time parameters

Obs%timeOffset = ObsTimeOffset( Obs%local ); IF( error%Number /= NO_ERROR )GOTO 9999
Obs%timeBin    = Prj%timeBin

!------ Build vertical grid (in case required for met field)

IF( BTEST(Obs%type,OTB_PRF) )THEN

  irv = SWIMBuildObsGrid( Obs )
  IF( irv /= SWIMsuccess )GOTO 9999

ELSE

  Obs%nz = 1
  ALLOCATE( Obs%z(1),STAT=ios)
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMinitObs'
    error%Message = 'Error allocating height array for surface obs'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF

  IF( Obs%zref == -999. )THEN
    irv = SWIMsetSrfRef( Obs )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF
  Obs%z(1) = Obs%zref

END IF

!------ Avoid reading obs during CREATE

IF( Prj%Create )THEN
  SWIMinitObs = SWIMresult
  GOTO 9999
END IF

!------ Read first time on obs file and initialize linked-list to keep track of
!       records associated with different times

ALLOCATE( var8(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error allocating arrays for obs record'
  GOTO 9999
END IF

var8 = DBLE(NOT_SET_R)

NULLIFY( FirstObsTime,LastObsTime,ObsTimeRec )
ALLOCATE( FirstObsTime,LastObsTime,STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMinitObs'
  error%Message = 'Error allocating pointers for linked-lists'
  GOTO 9999
END IF

NULLIFY( FirstObsTime%next,FirstObsTime%prev )
NULLIFY( LastObsTime%next,LastObsTime%prev )

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
    error%Routine = 'SWIMinitObs'
    CALL ReportFileName( error%Message,'Non-sequential times on obs file',Obs%Source )
    WRITE(error%Inform,'(A,F8.1,A,F8.1)')'New time (from start of run) = ',timeObs/3600., &
                                         ' Previous time = ',timeObsLast/3600.
    error%Action = 'Obs station='//TRIM(StaString)
    GOTO 9999

!------ New obs time

  ELSE IF( timeBin > ObsTimeRec%time )THEN

    ALLOCATE( ObsTimeRec%next,STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMinitObs'
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
Obs%Zmax          = 0.
Obs%PrevNumObs    = 0
Obs%PrevNumObsDom = 0
Obs%PrevZmax      = 0.

Obs%lend = .FALSE.

!------ Nullify linked-list for nearest obs search

NULLIFY( Obs%GridList,Obs%PrevGridList )

SWIMinitObs = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var_name) )DEALLOCATE( var_name,STAT=ios )
IF( ALLOCATED(var_unit) )DEALLOCATE( var_unit,STAT=ios )
IF( ALLOCATED(var8)     )DEALLOCATE( var8,STAT=ios )

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMobsKeyword( line,Obs )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

CHARACTER(*),     INTENT( INOUT ) :: line
TYPE( FirstObs ), INTENT( INOUT ) :: Obs

CHARACTER(128) :: kwrd, string

INTEGER nch, nchi, i, j, ios, year, month, day, ihour, imin, isec, irv
REAL    hour, minutes

INTEGER, EXTERNAL :: SWIMwarningMessage
REAL,    EXTERNAL :: GetTimeMet

SWIMobsKeyword = SWIMfailure

!------ Make case insensitive

CALL cupper( line )

nch = LEN_TRIM(line)

!------ Search for ":"

i = INDEX(line,':') - 1

IF( i > 0 )THEN

!------ Extract keyword

  nchi = LEN_TRIM(line(1:i))
  j = 2
  DO WHILE( line(j:j) == ' ' )
    j = j + 1
  END DO
  kwrd = line(j:nchi)

!------ Extract corresponding data string

  j = i+2
  DO WHILE( line(j:j) == ' ' .AND. j <= nch )
    j = j + 1
  END DO
  IF( j < nch )THEN
    string = line(j:nch)
  ELSE
    string = ' '
  END IF

!------ Check for recognized keywords and data

  IF( kwrd == 'TYPE' )THEN

    SELECT CASE( TRIM(string) )
      CASE( 'OBSERVATION' )
        !DEFAULT
      CASE( 'ANALYSIS' )
        Obs%type = IBSET(Obs%type,OTB_ANLY)
      CASE( 'FORECAST' )
        Obs%type = IBSET(Obs%type,OTB_FCST)
      CASE DEFAULT
        error%Number  = UK_ERROR
        error%Routine = 'SWIMobskeyword'
        error%Message = 'Unknown TYPE on met obs file'
        error%Inform  = 'Type= '//TRIM(string)
        CALL ReportFileName( error%Action,'File= ',Obs%Source )
        GOTO 9999
    END SELECT

  ELSE IF( kwrd == 'ANALYSIS' )THEN

    READ(string,*,IOSTAT=ios) year,month,day,hour
    IF( ios /= 0 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'SWIMobskeyword'
      error%Message = 'Error reading ANALYSIS time on met obs file'
      CALL ReportFileName( error%Inform,'File= ',Obs%Source )
      GOTO 9999
    END IF

    IF( year  <  0 .OR. &
        month <= 0 .OR. month > 12 .OR. &
        day   <= 0 .OR. day   > 31 )THEN
      error%Number  = RD_ERROR
      error%Routine = 'SWIMobskeyword'
      error%Message = 'Error in ANALYSIS time on met obs file'
      CALL ReportFileName( error%Inform,'File= ',Obs%Source )
      GOTO 9999
    END IF

    ihour = INT(hour)
    imin  = INT((hour - ihour)*60.)
    isec  = INT((hour - ihour)*3600. - imin*60.)
    Obs%tFcst = GetTimeMet( day,month,year,ihour,imin,isec,0. )  !Set without time offset

  ELSE IF( kwrd == 'TIMEREFERENCE' )THEN

    IF( INDEX(string,'LOCAL') > 0 )THEN

      Obs%local = .TRUE.

      i = INDEX(string,'L',BACK=.TRUE.)  !Check for time zone input
      IF( LEN_TRIM(string) > i )THEN
        string = string(i+1:)
        i = INDEX(string,':')
        hour = NOT_SET_R
        IF( i > 0 )THEN
          READ(string(1:i-1),*,IOSTAT=ios) ihour
          IF( ios == 0 )THEN
            READ(string(i+1:),*,IOSTAT=ios) minutes
            hour = FLOAT(ihour) + FLOAT(SIGN(1,ihour))*minutes/60.
          END IF
        ELSE
          READ(string,*,IOSTAT=ios) hour
        END IF
        IF( hour /= NOT_SET_R )THEN
          IF( ABS(hour) > 12. )THEN
            hour = hour - SIGN(24.,hour)
          END IF
          IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
            Prj%timeZone = hour
          ELSE
            IF( ABS(Prj%timeZone-hour) > 0.25 )THEN
              error%Number  = WN_ERROR
              error%Routine = 'SWIMobskeyword'
              CALL ReportFileName( error%Message,'Time Zone mismatch with obs file ',Obs%Source )
              WRITE(error%Inform,'(A,F6.2,A,F6.2)',IOSTAT=ios) 'Project= ',Prj%timeZone, &
                                                               ' Obs=',hour
              error%Action  = 'Project value will be used.  OK?'
              irv = SWIMwarningMessage( 'User elected not to continue' )
              IF( irv /= SWIMsuccess )GOTO 9999
            END IF
          END IF
        END IF
      END IF

    ELSE IF( string == 'UTC' .OR. string == ' ' )THEN
      Obs%local = .FALSE.

    ELSE
      error%Number  = IV_ERROR
      error%Routine = 'SWIMobskeyword'
      error%Message = 'TIMEREFERENCE error in met obs file'
      error%Inform  = 'Unrecognized keyword: must be LOCAL or UTC'
      CALL ReportFileName( error%Action,'File= ',Obs%Source )
      GOTO 9999
    END IF

  ELSE IF( kwrd == 'MODE' )THEN  !Check for prf file (perhaps for pseudo-surface file)

    IF( string(1:4) == 'PROF' )Obs%type = IBSET(Obs%type,OTB_PRF)

  END IF

END IF

SWIMobsKeyword = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMobsName( lun,nv,name,unit )

USE SWIMparam_fd
USE SWIM_fi, ONLY: SWIMresult, error

IMPLICIT NONE

INTEGER,                    INTENT( IN  ) :: lun
INTEGER,                    INTENT( IN  ) :: nv
CHARACTER(8), DIMENSION(*), INTENT( OUT ) :: name, unit

INTEGER, PARAMETER :: MAXL = 30   !Max number of lines for names input

INTEGER i, j, nv_read, nline, ios

CHARACTER(512) line

INTEGER, DIMENSION(MAXL) :: nv_line

SWIMobsName = SWIMfailure

error%Number  = RD_ERROR
error%Routine = 'SWIMobsName'

nv_read = 0
nline   = 0

!------ Read variable names

DO WHILE( nv_read < nv )

  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios /= 0 )THEN
    error%Message = 'Error reading variable names'
    GOTO 9999
  END IF

  nline = nline + 1
  IF( nline > MAXL )GOTO 9999

  nv_line(nline) = INT((LEN_TRIM(line)-1)/8) + 1

  IF( nv_read+nv_line(nline) > nv )THEN
    WRITE(error%Message,'(A,I3)') 'Too many variable names: ',nv_line(nline)
    WRITE(error%Inform, '(A,I3)') 'Specified number: ',nv
    GOTO 9999
  END IF

  READ(line,'(30A8)',IOSTAT=ios) (name(nv_read+i),i=1,nv_line(nline))
  IF( ios /= 0 )THEN
    error%Message = 'Error reading variable names'
    GOTO 9999
  END IF

  nv_read = nv_read + nv_line(nline)

END DO

!------ Read unit names

nv_read = 0

DO i = 1,nline

  READ(lun,'(30A8)',IOSTAT=ios) (unit(nv_read+j),j=1,nv_line(i))
  IF( ios /= 0 )THEN
    error%Message = 'Error reading variable units'
    GOTO 9999
  END IF

  nv_read = nv_read + nv_line(i)

END DO

!------ Trim leading & trailing blank spaces

DO i = 1,nv
  name(i) = TRIM(ADJUSTL(name(i)))
  unit(i) = TRIM(ADJUSTL(unit(i)))
END DO

SWIMobsName = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMobsID( nv,var_name,var_unit,Obs )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,                     INTENT( IN    ) :: nv
CHARACTER(8), DIMENSION(nv), INTENT( IN    ) :: var_name
CHARACTER(8), DIMENSION(nv), INTENT( IN    ) :: var_unit
TYPE( FirstObs ),            INTENT( INOUT ) :: Obs

INTEGER i, i1, i2, irv, iMOL, iPGT
LOGICAL lu, lv, luv

INTEGER, EXTERNAL :: SWIMsetObsVar, SWIMwarningMessage, SWIMaddLogMessage


SWIMobsID = SWIMfailure

!------ Loop over all variables and set id and conversion factor

Obs%VarID = OVP_NONE  !Initialize
Obs%Conv  = 1.

DO i = 1,nv
  irv = SWIMsetObsVar( var_name(i),var_unit(i),Obs%VarID(i),Obs%Conv(i),Obs%type )
  IF( irv /= SWIMsuccess )GOTO 9999
END DO

!------ Warning if Cartesian and Lat/Lon locations are given

IF( BTEST(Obs%type,OTB_LL) .AND. BTEST(Obs%type,OTB_XY) )THEN

  IF( Prj%create )THEN

    error%Number  = WN_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Both Cartesian and Lat/Lon locations given on obs file'
    error%Action  = 'Do you wish to continue?'

    IF( Prj%coord == I_LATLON )THEN
      error%Inform  = 'X/Y will be ignored'
    ELSE
      error%Inform  = 'Lat/Lon will be ignored'
    END IF

    irv = SWIMwarningMessage( 'User elected not to continue' )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

  IF( Prj%coord == I_LATLON )THEN
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_X )Obs%VarID(i) = OVP_NONE
      IF( Obs%VarID(i) == OVP_Y )Obs%VarID(i) = OVP_NONE
    END DO
    Obs%type = IBCLR(Obs%type,OTB_XY)
  ELSE
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_LAT )Obs%VarID(i) = OVP_NONE
      IF( Obs%VarID(i) == OVP_LON )Obs%VarID(i) = OVP_NONE
    END DO
    Obs%type = IBCLR(Obs%type,OTB_LL)
  END IF

END IF

!------ Warning if Cartesian components and wind speed, direction are given

IF( BTEST(Obs%type,OTB_UV) .AND. BTEST(Obs%type,OTB_SPD) )THEN

  IF( Prj%create )THEN

    error%Number  = WN_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Both U,V and Speed & Direction are given on obs file'
    error%Inform  = 'Speed & Direction will be ignored'
    error%Action  = 'Do you wish to continue?'

    irv = SWIMwarningMessage( 'User elected not to continue' )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_SPD )Obs%VarID(i) = OVP_NONE
    IF( Obs%VarID(i) == OVP_DIR )Obs%VarID(i) = OVP_NONE
  END DO

  Obs%type = IBCLR(Obs%type,OTB_SPD)

END IF

!------ Check if location variables properly setup

IF( .NOT.(BTEST(Obs%type,OTB_LL) .OR. BTEST(Obs%type,OTB_XY)) )THEN

  error%Number  = IV_ERROR
  error%Routine = 'SWIMobsID'
  error%Message = 'No recognized location variables given on obs file'
  error%Inform  = 'Must be LAT,LON or X,Y'

ELSE

  i1 = 0; i2 = 0

  IF( BTEST(Obs%type,OTB_LL) )THEN
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_LON )i1 = i
      IF( Obs%VarID(i) == OVP_LAT )i2 = i
    END DO
  ELSE
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_X )i1 = i
      IF( Obs%VarID(i) == OVP_Y )i2 = i
    END DO
  END IF

  IF( i1*i2 == 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    IF( BTEST(Obs%type,OTB_LL) )THEN
      IF( i2 == 0 )THEN
        error%Message = 'Missing LATITUDE variable from obs file'
        error%Inform  = 'Variable name must be LAT'
      ELSE
        error%Message = 'Missing LONGITUDE variable from obs file'
        error%Inform  = 'Variable name must be LON'
      END IF
    ELSE
      IF( i1 == 0 )THEN
        error%Message = 'Missing X variable from obs file'
        error%Inform  = 'Variable name must be X'
      ELSE
        error%Message = 'Missing Y variable from obs file'
        error%Inform  = 'Variable name must be Y'
      END IF
    END IF
    GOTO 9999
  END IF

END IF

!------ Check velocity components

i1 = 0; i2 = 0

IF( BTEST(Obs%type,OTB_UV) )THEN  !If cartesian velocity components given

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_U   )i1 = i
    IF( Obs%VarID(i) == OVP_V   )i2 = i
  END DO

ELSE                              !Else speed and direction

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_DIR )i1 = i
    IF( Obs%VarID(i) == OVP_SPD )i2 = i
  END DO

END IF

IF( i1*i2 == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMobsID'
  CALL ReportFileName( error%Action,'File= ',Obs%Source )
  IF( BTEST(Obs%type,OTB_UV) )THEN
    IF( i1 == 0 )THEN
      error%Message = 'Missing U-velocity component from obs file'
      error%Inform  = 'Variable name must be U'
    ELSE
      error%Message = 'Missing V-velocity component from obs file'
      error%Inform  = 'Variable name must be V'
    END IF
  ELSE
    IF( i1 == 0 )THEN
      error%Message = 'Missing Wind Direction variable from obs file'
      error%Inform  = 'Variable name must be WSPD,SPEED or SPD'
    ELSE
      error%Message = 'Missing Wind Speed variable from obs file'
      error%Inform  = 'Variable name must be DIR or WDIR'
    END IF
  END IF
  GOTO 9999
END IF

Obs%Conv(i1) = FLOAT(i2)     !Conversion factor is defined for 'pointee'

!------ Setup velocity combination in conversion factor: U points to V or Direction points to Speed

i1 = 0; i2 = 0

IF( BTEST(Obs%type,OTB_UV) )THEN  !If cartesian velocity components given

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_U   )i1 = i
    IF( Obs%VarID(i) == OVP_V   )i2 = i
  END DO

ELSE                              !Else speed and direction

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_DIR )i1 = i
    IF( Obs%VarID(i) == OVP_SPD )i2 = i
  END DO

END IF

IF( i1*i2 == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMobsID'
  error%Message = 'Missing velocity component from obs file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999
END IF

Obs%Conv(i1) = FLOAT(i2)     !Conversion factor is defined for 'pointee'

!------- Setup indices for ID and Z

Obs%index%z  = 0
Obs%index%ID = 0

DO i = 1,nv
  IF( Obs%VarID(i) == OVP_Z  )Obs%index%z  = i
  IF( Obs%VarID(i) == OVP_ID )Obs%index%id = i
END DO

IF( BTEST(Obs%type,OTB_PRF) .AND. Obs%index%z == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMobsID'
  error%Message = 'Height not given in obs file'
  error%Inform  = 'Variable Z required in profile file'
  CALL ReportFileName( error%Action,'File= ',Obs%Source )
  GOTO 9999
END IF

!------- Check BL input:
!        Ignore PGT if MOL present; ignore MOL (or PGT) if ZI and HFLX present

IF( BTEST(Obs%type,OTB_MOL) )THEN

  iMOL = 0
  iPGT = 0

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_MOL )iMOL = i
    IF( Obs%VarID(i) == OVP_PGT )iPGT = i
  END DO

  IF( iMOL > 0 .AND. iPGT > 0 )THEN
    Obs%VarID(iPGT) = OVP_NONE
    irv = SWIMaddLogMessage( 'Obs variable PGT superseded by MOL' )
  END IF

  IF( BTEST(Obs%type,OTB_ZI) .AND. BTEST(Obs%type,OTB_HFLX) )THEN
    Obs%type = IBCLR(Obs%type,OTB_MOL)
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_MOL )THEN
        Obs%VarID(i) = OVP_NONE
        irv = SWIMaddLogMessage( 'Obs variable MOL superseded by HFLUX and ZI' )
        EXIT
      ELSE IF( Obs%VarID(i) == OVP_PGT )THEN
        Obs%VarID(i) = OVP_NONE
        irv = SWIMaddLogMessage( 'Obs variable PGT superseded by HFLUX and ZI' )
        EXIT
      END IF
    END DO
  END IF

END IF

!------- Check LSV

IF( BTEST(Obs%type,OTB_LSV) )THEN

  lu = .FALSE.; lv = .FALSE.; luv = .FALSE.

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_UL  )lu  = .TRUE.
    IF( Obs%VarID(i) == OVP_VL  )lv  = .TRUE.
    IF( Obs%VarID(i) == OVP_UVL )luv = .TRUE.
  END DO

  IF( .NOT.(lu .AND. lv .AND. luv) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Large-scale variance requires UL, VL and UVL'
    CALL ReportFileName( error%Inform,'File= ',Obs%Source )
    GOTO 9999
  END IF

END IF

!------- Check Profile BL

IF( Prj%BL%type == BLP_PROF .OR. (Prj%BL%type == BLP_OPER .AND. BTEST(Obs%type,OTB_UU)) )THEN

  i1 = 0

  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_UU )i1 = i1 + 1
    IF( Obs%VarID(i) == OVP_VV )i1 = i1 + 1
    IF( Obs%VarID(i) == OVP_WW )i1 = i1 + 1
    IF( Obs%VarID(i) == OVP_WT )i1 = i1 + 1
    IF( Obs%VarID(i) == OVP_SL )i1 = i1 + 1
    IF( Obs%VarID(i) == OVP_SZ )i1 = i1 + 1
  END DO

  IF( Prj%BL%type == BLP_OPER )THEN

    IF( i1 == 6 )THEN
      i1 = 0
      DO i = 1,nv
        IF( Obs%VarID(i) == OVP_ZI   )i1 = i1 + 1
        IF( Obs%VarID(i) == OVP_HFLX )i1 = i1 + 1
        IF( Obs%VarID(i) == OVP_MOL  )i1 = i1 + 1
        IF( Obs%VarID(i) == OVP_PGT  )i1 = i1 + 1
      END DO
      IF( i1 > 0 )THEN
        irv = SWIMaddLogMessage( 'Obs variable changes for Operational BL:' )
        IF( irv /= SWIMsuccess )GOTO 9999
        DO i = 1,nv
          SELECT CASE( Obs%VarID(i) )
            CASE( OVP_ZI,OVP_HFLX,OVP_MOL,OVP_PGT )
              Obs%VarID(i) = OVP_NONE
              irv = SWIMaddLogMessage( '+ '//var_name(i)//'   : Ignored      : Using Profile variables UU, etc.' )
          END SELECT
        END DO
        Obs%type = IBCLR(Obs%type,OTB_ZI)
        Obs%type = IBCLR(Obs%type,OTB_HFLX)
        Obs%type = IBCLR(Obs%type,OTB_MOL)
      END IF
    ELSE IF( i1 > 0 )THEN !Should always be the case with OTB_UU on
      irv = SWIMaddLogMessage( 'Obs variable changes for Operational BL:' )
      IF( irv /= SWIMsuccess )GOTO 9999
      DO i = 1,nv
        SELECT CASE( Obs%VarID(i) )
          CASE( OVP_UU,OVP_VV,OVP_WW,OVP_WT,OVP_SL,OVP_SZ )
            Obs%VarID(i) = OVP_NONE
            irv = SWIMaddLogMessage( '+ '//var_name(i)//'   : Ignored      : Not all Profile variables given' )
        END SELECT
      END DO
      Obs%type = IBCLR(Obs%type,OTB_UU)
    END IF

  ELSE IF( i1 /= 6 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'All turbulence profiles must be on upper air file'
    error%Action  = 'Need UU, VV, WW, WT, SL and SZ'
    GOTO 9999
  END IF

END IF

!------ Setup time structure

Obs%index%TimeID%type  = -999
Obs%index%TimeID%hour  = 0
Obs%index%TimeID%day   = 0
Obs%index%TimeID%month = 0
Obs%index%TimeID%year  = 0

IF( BTEST(Obs%type,OTB_TIME) )THEN  !Set time without date reference

  IF( BTEST(Obs%type,OTB_YMD) .OR. BTEST(Obs%type,OTB_YR) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Invalid time set on obs file'
    error%Inform  = 'TIME not set with date input'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999
  END IF

  Obs%index%TimeID%type = OBP_NODATE
  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_TIME )Obs%index%TimeID%hour = i
  END DO

ELSE IF( BTEST(Obs%type,OTB_YR) )THEN !Set time with full date, i.e. including year

  IF( BTEST(Obs%type,OTB_JDAY) )THEN
    Obs%index%TimeID%type = OBP_YJDAY
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_YR   )Obs%index%TimeID%year  = i
      IF( Obs%VarID(i) == OVP_JDAY )Obs%index%TimeID%day  = i
      IF( Obs%VarID(i) == OVP_HR   )Obs%index%TimeID%hour = i
    END DO
    IF( Obs%index%TimeID%hour == 0 )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMobsID'
      error%Message = 'Invalid time set on obs file'
      error%Inform  = 'HOUR required with Julian day'
      CALL ReportFileName( error%Action,'File= ',Obs%Source )
      GOTO 9999
    END IF
  ELSE
    Obs%index%TimeID%type = OBP_YDATE
    DO i = 1,nv
      IF( Obs%VarID(i) == OVP_YR  )Obs%index%TimeID%year  = i
      IF( Obs%VarID(i) == OVP_MO  )Obs%index%TimeID%month = i
      IF( Obs%VarID(i) == OVP_DAY )Obs%index%TimeID%day   = i
      IF( Obs%VarID(i) == OVP_HR  )Obs%index%TimeID%hour  = i
    END DO
    IF( Obs%index%TimeID%month*Obs%index%TimeID%day*Obs%index%TimeID%hour == 0 )THEN
      error%Number  = IV_ERROR
      error%Routine = 'SWIMobsID'
      error%Message = 'Invalid time set on obs file'
      error%Inform  = 'MONTH, DAY, & HOUR required with YEAR'
      CALL ReportFileName( error%Action,'File= ',Obs%Source )
      GOTO 9999
    END IF
  END IF

ELSE IF( BTEST(Obs%type,OTB_YMD) )THEN !Set time with full date, parse YYYYMMDD

  Obs%index%TimeID%type = OBP_YMD
  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_YMD )Obs%index%TimeID%year = i
    IF( Obs%VarID(i) == OVP_HR  )Obs%index%TimeID%hour = i
  END DO
  IF( Obs%index%TimeID%hour == 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Invalid time set on obs file'
    error%Inform  = 'HOUR required with YYYYMMDD'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999
  END IF

ELSE IF( BTEST(Obs%type,OTB_JDAY) )THEN !Set time with Julian day, no year

  Obs%index%TimeID%type = OBP_JDAY
  DO i = 1,nv
    IF( Obs%VarID(i) == OVP_JDAY )Obs%index%TimeID%day  = i
    IF( Obs%VarID(i) == OVP_HR   )Obs%index%TimeID%hour = i
  END DO
  IF( Obs%index%TimeID%hour == 0 )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMobsID'
    error%Message = 'Invalid time set on obs file'
    error%Inform  = 'HOUR required with Julian day'
    CALL ReportFileName( error%Action,'File= ',Obs%Source )
    GOTO 9999
  END IF

ELSE

  error%Number  = IV_ERROR
  error%Routine = 'SWIMobsID'
  error%Message = 'Invalid time set on obs file'
  CALL ReportFileName( error%Inform,'File= ',Obs%Source )
  GOTO 9999

END IF

!------ A SWIMming Success

SWIMobsID = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetObsVar( name,unit,id,cnv,type )

USE SWIMparam_fd
USE SWIM_fi
USE constants_fd

IMPLICIT NONE

CHARACTER(8), INTENT( IN    ) :: name, unit
INTEGER,      INTENT( OUT   ) :: id
REAL,         INTENT( INOUT ) :: cnv
INTEGER(8),   INTENT( INOUT ) :: type

INTEGER irv

INTEGER, EXTERNAL :: SetVelCnv, SWIMaddLogMessage

SWIMsetObsVar = SWIMfailure

id = OVP_NONE !Initialize as unrecognized

!------ Check variables (unaffected by project input)

SELECT CASE( TRIM(name) )

  CASE( 'ID' )
    id = OVP_ID

  CASE( 'TIME' )
    id = OVP_TIME
    SELECT CASE( TRIM(unit) )
      CASE( 'HRS','HOURS' )
        cnv = 3600.
      CASE( 'MIN','MINUTES' )
        cnv = 60.
      CASE( 'SEC','S','SECONDS' )
        cnv = 1.
      CASE DEFAULT
        error%Message = 'Improper time units on obsr file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_TIME)

  CASE( 'YEAR' )
    id = OVP_YR
    type = IBSET(type,OTB_YR)

  CASE( 'MONTH' )
    id = OVP_MO

  CASE( 'DAY' )
    id = OVP_DAY

  CASE( 'HOUR' )
    id = OVP_HR
    cnv = 3600.

  CASE( 'YYMMDD','YYYYMMDD' )
    id = OVP_YMD
    type = IBSET(type,OTB_YMD)

  CASE( 'JDAY' )
    id = OVP_JDAY
    type = IBSET(type,OTB_JDAY)

  CASE( 'X' )
    id = OVP_X
    SELECT CASE( TRIM(unit) )
      CASE( 'KM' )
        cnv = 1.
      CASE( 'M' )
        cnv = 1.E-3
      CASE DEFAULT
        error%Message = 'X must be in KM or M on upper air file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_XY)

  CASE( 'Y' )
    id = OVP_Y
    SELECT CASE( TRIM(unit) )
      CASE( 'KM' )
        cnv = 1.
      CASE( 'M' )
        cnv = 1.E-3
      CASE DEFAULT
        error%Message = 'Y must be in KM or M on obs file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_XY)

  CASE( 'LAT' )
    id = OVP_LAT
    SELECT CASE( TRIM(unit) )
      CASE( 'S' )
        cnv = -1.
      CASE DEFAULT
        cnv = 1.
    END SELECT
    type = IBSET(type,OTB_LL)

 CASE( 'LON' )
    id = OVP_LON
    SELECT CASE( TRIM(unit) )
      CASE( 'W' )
        cnv = -1.
      CASE DEFAULT
        cnv = 1.
    END SELECT
    type = IBSET(type,OTB_LL)

  CASE( 'U' )
    id = OVP_U
    irv = SetVelCnv( unit,cnv )
    IF( irv /= SWIMsuccess )GOTO 9998
    type = IBSET(type,OTB_UV)

  CASE( 'V' )
    id = OVP_V
    irv = SetVelCnv( unit,cnv )
    IF( irv /= SWIMsuccess )GOTO 9998
    type = IBSET(type,OTB_UV)

  CASE( 'WSPD','SPEED','SPD' )
    id = OVP_SPD
    irv = SetVelCnv( unit,cnv )
    IF( irv /= SWIMsuccess )GOTO 9998
    type = IBSET(type,OTB_SPD)

  CASE( 'DIR','WDIR' )
    id = OVP_DIR
    type = IBSET(type,OTB_SPD)

  CASE( 'Z' )
    id = OVP_Z
    SELECT CASE( TRIM(unit) )
      CASE( 'M' )
        cnv = 1.
      CASE( 'KM' )
        cnv = 1.E3
      CASE( 'FEET','FT' )
        cnv = 0.3048
      CASE DEFAULT
        error%Message = 'Improper height units on obs file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_Z)

  CASE( 'P' )
    id = OVP_P
    SELECT CASE( TRIM(unit) )
      CASE( 'IN_HG' )
        cnv = 33.8639
      CASE( 'IN_MM' )
        cnv = 1.333
      CASE( 'MB','MBAR' )
        cnv = 1.
      CASE( 'PA' )
        cnv = 100.
      CASE DEFAULT
        error%Message = 'Improper pressure units on obs file'
        error%Action  = 'Only MB, IN_MG, MM_MG or PA accepted'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_P)

  CASE( 'T','TEMP' )
    id = OVP_T
    type = IBSET(type,OTB_TCNV)  !Convert to potential temperature
    SELECT CASE( TRIM(unit) )
      CASE( 'C' )
        cnv = FLOAT(OBP_CELSIUS)
      CASE( 'K' )
        cnv = FLOAT(OBP_KELVIN)
      CASE( 'F' )
        cnv = FLOAT(OBP_FAHRENHEIT)
      CASE( 'POT' )
        cnv = FLOAT(OBP_POTENTIAL)
        type = IBCLR(type,OTB_TCNV)
      CASE DEFAULT
        error%Message = 'Improper temperature units on obs file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_T)

  CASE( 'H','HUMID','Q' )
    id = OVP_H
    type = IBSET(type,OTB_HCNV)
    SELECT CASE( TRIM(unit) )
      CASE( 'GM/GM','G/G' )
        cnv = 1.
      CASE( 'GM/KG','G/KG' )
        cnv = 1.E-3
      CASE( '%' )
        cnv  = -1.
        type = IBCLR(type,OTB_HCNV)
      CASE DEFAULT
        error%Message = 'Improper humidity units on obs file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_H)

  CASE( 'QCLOUD','QCLD','CLOUD','CLD' )
    id = OVP_QCLD
    SELECT CASE( TRIM(unit) )
      CASE( 'GM/GM','G/G','KG/KG' )
        cnv = 1.E+3
        type = IBSET(type,OTB_QCNV)
      CASE( 'GM/KG','G/KG' )
        cnv = 1.
        type = IBSET(type,OTB_QCNV)
      CASE( 'G/M3','GM/M3')
        cnv  = 1.
      CASE DEFAULT
        error%Message = 'Improper cloud water units on obs file'
        GOTO 9998
    END SELECT
    type = IBSET(type,OTB_QCLD)

  CASE( 'R','RI','RINFL' )
    id = OVP_RIFL
    SELECT CASE( TRIM(unit) )
    CASE( 'M' )
      cnv = 1.
    CASE( 'KM' )
      cnv = 1.E3
    CASE DEFAULT
      error%Message = 'Radius of influence must be in KM or M on obs file'
      GOTO 9998
    END SELECT
    type = IBSET(type,OTB_RIFL)

END SELECT

!------ Check LSV variables only if requested by user

IF( Prj%LSVType == LVP_MET .OR. Prj%LSVType == LVP_OPER )THEN

  SELECT CASE( TRIM(name) )
    CASE( 'UL' )
      id = OVP_UL
      irv = SetVelCnv( unit,cnv )
      IF( irv /= SWIMsuccess )GOTO 9998
      cnv = -cnv
      type = IBSET(type,OTB_LSV)
      Prj%LSVType = LVP_MET

    CASE( 'VL' )
      id = OVP_VL
      irv = SetVelCnv( unit,cnv )
      IF( irv /= SWIMsuccess )GOTO 9998
      cnv = -cnv
      type = IBSET(type,OTB_LSV)
      Prj%LSVType = LVP_MET

    CASE( 'UUL' )
      id = OVP_UL
      IF( TRIM(unit) /= 'M2/S2' )THEN
        error%Message = 'Improper UUL units on obs file'
        GOTO 9998
      END IF
      type = IBSET(type,OTB_LSV)
      Prj%LSVType = LVP_MET

    CASE( 'VVL' )
      id = OVP_VL
      IF( TRIM(unit) /= 'M2/S2' )THEN
        error%Message = 'Improper VVL units on obs file'
        GOTO 9998
      END IF
      type = IBSET(type,OTB_LSV)
      Prj%LSVType = LVP_MET

    CASE( 'UVL' )
      id = OVP_UVL
      type = IBSET(type,OTB_LSV)
      Prj%LSVType = LVP_MET

    CASE( 'SHL' )
      id = OVP_SHL
      IF( TRIM(unit) /= 'M' )THEN
        error%Message = 'Improper SHL units on obs file'
        GOTO 9998
      END IF
      type = IBSET(type,OTB_LSVL)
      Prj%LSVType = LVP_MET

  END SELECT

END IF

!------ Check BL profile variables only if requested by user

IF( Prj%BL%type == BLP_PROF .OR. Prj%BL%type == BLP_OPER )THEN

  SELECT CASE( TRIM(name) )
    CASE( 'UU' )
      id = OVP_UU
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'M2/S2' )THEN
        error%Message = 'Improper UU units on obs file'
        GOTO 9998
      END IF

    CASE( 'VV' )
      id = OVP_VV
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'M2/S2' )THEN
        error%Message = 'Improper VV units on obs file'
        GOTO 9998
      END IF

    CASE( 'WW' )
      id = OVP_WW
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'M2/S2' )THEN
        error%Message = 'Improper WW units on obs file'
        GOTO 9998
      END IF

    CASE( 'WT' )
      id = OVP_WT
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'K-M/S' .AND. unit /= 'C-M/S' )THEN
       error%Message = 'Improper WT units on obs file'
        GOTO 9998
      END IF

    CASE( 'SL' )
      id = OVP_SL
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'M' )THEN
        error%Message = 'Improper SL units on obs file'
        GOTO 9998
      END IF

    CASE( 'SZ' )
      id = OVP_SZ
      type = IBSET(type,OTB_UU)
      IF( TRIM(unit) /= 'M' )THEN
        error%Message = 'Improper SZ units on obs file'
        GOTO 9998
      END IF

  END SELECT

END IF

IF( Prj%BL%type /= BLP_NONE )THEN

  SELECT CASE( TRIM(name) )

    CASE( 'Z0','ZRUF','ZROUGH','ZO' )
      id = OVP_ZRUF
      SELECT CASE( TRIM(unit) )
        CASE( 'M' )
          cnv = 1.
        CASE( 'CM' )
          cnv = 1.E-2
        CASE( 'KM' )
          cnv = 1.E3
        CASE( 'FEET','FT' )
          cnv = 0.3048
        CASE DEFAULT
          error%Message = 'Improper roughness height units on obs file'
          GOTO 9998
      END SELECT
      type = IBSET(type,OTB_ZRUF)

    CASE( 'HCNP','HCANP','CANOPY','CANOPYHT' )
      id = OVP_HCNP
      SELECT CASE( TRIM(unit) )
        CASE( 'M' )
          cnv = 1.
        CASE( 'CM' )
          cnv = 1.E-2
        CASE( 'KM' )
          cnv = 1.E3
        CASE( 'FEET','FT' )
          cnv = 0.3048
        CASE DEFAULT
          error%Message = 'Improper canopy height units on obs file'
          GOTO 9998
      END SELECT
      type = IBSET(type,OTB_HCNP)

    CASE( 'ACNP','ADEN','ADENS' )
      id = OVP_ACNP
      type = IBSET(type,OTB_ACNP)

    CASE( 'ALPHA','ALPH','ALPHC' )
      id = OVP_ALPH
      type = IBSET(type,OTB_ALPH)

    CASE( 'LAI' )
      id = OVP_LAI
      type = IBSET(type,OTB_LAI)

  END SELECT
END IF

!------ Check BL parameters only if requested by user

IF( Prj%BL%type == BLP_MET .OR. Prj%BL%type == BLP_OPER )THEN

  SELECT CASE( TRIM(name) )

    CASE( 'ZI' )
      id = OVP_ZI
      SELECT CASE( TRIM(unit) )
        CASE( 'M' )
          cnv = 1.
        CASE( 'KM' )
          cnv = 1.E3
        CASE( 'FEET','FT' )
          cnv = 0.3048
        CASE DEFAULT
          error%Message = 'Improper inversion height units on obs file'
          GOTO 9998
      END SELECT
      type = IBSET(type,OTB_ZI)
      type = IBSET(type,OTB_BLV)  !Indicates that BL parameter(s) available

    CASE( 'HFLUX','HFLX' )
      id = OVP_HFLX
      SELECT CASE( TRIM(unit) )
        CASE( 'C-M/S' )
          cnv = 1.
        CASE( 'K-M/S' )
          cnv = 1.
        CASE( 'W/M/M','W/M2' )
          cnv = 1./RHOCP
        CASE DEFAULT
          error%Message = 'Improper heat flux units on obs file'
          GOTO 9998
      END SELECT
      type = IBSET(type,OTB_HFLX)
      type = IBSET(type,OTB_BLV)

    CASE( 'PGT'  )
      id   = OVP_PGT
      type = IBSET(type,OTB_MOL)
      type = IBSET(type,OTB_BLV)

    CASE( 'MOL','L' )
      id = OVP_MOL
      type = IBSET(type,OTB_MOL)
      type = IBSET(type,OTB_BLV)

    CASE( 'USTAR','UST','USTR','U*' )
      id   = OVP_UST
      type = IBSET(type,OTB_UST)
      type = IBSET(type,OTB_BLV)

    CASE( 'CC','FCC' )
      id = OVP_CC
      type = IBSET(type,OTB_CC)
      type = IBSET(type,OTB_BLV)
      IF( TRIM(unit) == '%' )cnv = 1.E-2

  END SELECT

ELSE IF( Prj%BL%type == BLP_CALC )THEN

  SELECT CASE( TRIM(name) )
    CASE( 'CC','FCC' )
      id = OVP_CC
      type = IBSET(type,OTB_CC)
      type = IBSET(type,OTB_BLV)
      IF( TRIM(unit) == '%' )cnv = 1.E-2
  END SELECT

END IF

!------ Check Precip only if requested by user

IF( Prj%BL%pr_type == -1. )THEN

  SELECT CASE( TRIM(name) )

    CASE( 'PRCP' )
      id = OVP_PRCP
      SELECT CASE( TRIM(unit) )
        CASE( 'MM/HR'  )
          type = IBSET(type,OTB_PRATE)
        CASE( 'KG/M2/HR' )
          type = IBSET(type,OTB_PRATE)
          cnv  = 1000./RHO_WATER
        CASE( 'KG/M2/S' )
          type = IBSET(type,OTB_PRATE)
          cnv  = 1000./RHO_WATER * 3600.
        CASE DEFAULT
          type = IBSET(type,OTB_PRCP)
      END SELECT
      type = IBSET(type,OTB_BLV)

    CASE( 'PRATE' )
      id = OVP_PRCP
      type = IBSET(type,OTB_PRATE)
      SELECT CASE( TRIM(unit) )
        CASE( 'MM/HR'  )
          cnv = 1.
        CASE( 'KG/M2/HR' )
          cnv = 1000./RHO_WATER
        CASE( 'KG/M2/S' )
          cnv = 1000./RHO_WATER * 3600.
      END SELECT
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC0' )
      id = OVP_FPC0
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC1' )
      id = OVP_FPC1
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC2' )
      id = OVP_FPC2
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC3' )
      id = OVP_FPC3
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC4' )
      id = OVP_FPC4
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC5' )
      id = OVP_FPC5
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)

    CASE( 'FPC6' )
      id = OVP_FPC6
      type = IBSET(type,OTB_PRCP)
      type = IBSET(type,OTB_BLV)
  END SELECT

END IF

!------ Log file message for unrecognized/unused variables

IF( id == OVP_NONE )THEN

  SELECT CASE( TRIM(name) )
    CASE( 'UL','VL','UVL','SHL','UUL','VVL' )
      irv = SWIMaddLogMessage( '+ '//name//'   : Ignored      : LSV type not MetFile or Operational' )

    CASE( 'SIGU','SIGV','SUV','RHOUV','SHU','UUE','VVE','UVE' )
      irv = SWIMaddLogMessage( '+ '//name//'   : Ignored      : No Hazard calculation' )

    CASE( 'UU','VV','WW','WT','SL','SZ' )
      irv = SWIMaddLogMessage( '+ '//name//'   : Ignored      : BL type not Profile or Operational' )

    CASE( 'ZI','HFLUX','PGT','MOL','L','CC' )
      irv = SWIMaddLogMessage( '+ '//name//'   : Ignored      : BL type not MetFile or Operational' )

    CASE( 'PRCP','FPC0','FPC1','FPC2','FPC3','FPC4','FPC5','FPC6' )
      irv = SWIMaddLogMessage( '+ '//name//'   : Ignored      : Precip type not MetFile' )

    CASE( 'NOT_USED' )
      irv = SWIMaddLogMessage( '+ '//unit//'   : Not used     : HAZARD type' )

    CASE DEFAULT
      irv = SWIMaddLogMessage( '* '//name//'   : Unrecognized : Data ignored' )

  END SELECT

ELSE

  irv = SWIMaddLogMessage( '  '//name//'   : Recognized   : Units='//TRIM(unit) )

END IF

IF( irv /= SWIMsuccess )GOTO 9999

SWIMsetObsVar = SWIMresult

9999 CONTINUE

RETURN

9998 CONTINUE
error%Number = IV_ERROR
error%Routine = 'SWIMsetObsVar'
GOTO 9999

END

!==============================================================================

INTEGER FUNCTION SetVelCnv( unit,cnv )

USE SWIMparam_fd
USE SWIM_fi

CHARACTER(*), INTENT( IN  ) :: unit
REAL,         INTENT( OUT ) :: cnv

SetVelCnv = SWIMresult

SELECT CASE( TRIM(unit) )
  CASE( 'KNOTS','KTS' )
    cnv = 0.514444
  CASE( 'M/S' )
    cnv = 1.
  CASE( 'KPH' )
   cnv = 0.27777778
  CASE( 'MPH' )
    cnv = 0.44704
  CASE DEFAULT
    SetVelCnv  = SWIMfailure
    error%Message = 'Improper velocity units on met file'
END SELECT

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMreadVarObs( Obs,var,lend,nrec,StaID )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs),      INTENT( INOUT ) :: Obs
REAL(8),DIMENSION(*), INTENT( INOUT ) :: var
LOGICAL,              INTENT( OUT   ) :: lend
INTEGER,              INTENT( OUT   ) :: nrec
CHARACTER(*),         INTENT( OUT   ) :: StaID

INTEGER, PARAMETER :: MAXN = 200

CHARACTER(256) line, kwrd
CHARACTER(32)  c_arg(MAXN)
CHARACTER(256) linex
CHARACTER(32)  x_arg(MAXN)

LOGICAL lerr
INTEGER i, nch, ios, i0, irv, n_arg
INTEGER nrecx, nx_arg, np, j, dif
INTEGER iyear, imonth, iday, ihr, imn, jday0
INTEGER jyear, jmonth, jday, jhr, jmn, jdayx
INTEGER wspd, wdir
REAL    p, p1, tc, tdpt, rh
LOGICAL lSecond, lBadThermo

INTEGER, EXTERNAL :: SWIMaddLogMessage, SWIMwarningMessage
INTEGER, EXTERNAL :: julian_day
REAL,    EXTERNAL :: humid
LOGICAL, EXTERNAL :: CheckNumChar

SWIMreadVarObs = SWIMresult

nrec  = 0
nrecx = 0
1000 CONTINUE
lend  = .FALSE.
line  = ' '
StaID = 'NOT SET'

!------ Read next line; check for eof

ReadLine: DO WHILE( line == ' ' )

  nrec = nrec + 1
  READ(Obs%unit,'(A)',IOSTAT=ios) line

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios < 0 )THEN
    lend = .TRUE.; GOTO 9999
  ELSE IF( ios > 0 )THEN
    GOTO 9998
  END IF

END DO ReadLine

!------ Parse data into character strings

CALL get_next_data( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
IF( lerr )GOTO 9998

!------ Check number of arguments

IF( TRIM(c_arg(1)(1:3)) == 'ID:' )THEN

  IF( TRIM(c_arg(1)) == 'ID:' )THEN  !Handle ID:staxxx v. ID: staxxx
    nch = n_arg
  ELSE
    nch = n_arg+1
  END IF

  IF( Obs%nVarFixed == 0 )THEN
    error%Inform = 'Invalid ID record.  No fixed variables defined'
    error%Action = 'Check header records'
    GOTO 9998
  ELSE IF( nch < Obs%nVarFixed+1 )THEN
    error%Inform = 'Not enough data on ID line'
    error%Action = 'Line: '//TRIM(line)
    GOTO 9998
  ELSE IF( nch > Obs%nVarFixed+1 )THEN
    IF( .NOT.Obs%lWarnTooMuchObsDAta )THEN
      error%Number  = WN_ERROR
      error%Routine = 'SWIMreadVarObs'
      error%Message = 'Too much data on ID line'
      error%Inform  = 'Line: '//TRIM(line)
      error%Action  = 'Do you wish to ignore the extra data?'
      irv = SWIMwarningMessage( 'User elected not to continue' )
      IF( irv /= SWIMsuccess )GOTO 9998
      Obs%lWarnTooMuchObsDAta = .TRUE.
    END IF
    n_arg = Obs%nVarFixed+1
  END IF

ELSE IF( n_arg < Obs%nVar - Obs%nVarFixed )THEN

  IF( .NOT.Obs%lASOS1 )THEN
  WRITE(error%Inform,FMT='(A,I2,A,I2)') 'Insufficient data: ',n_arg,' instead of ',Obs%nVar-Obs%nVarFixed
  error%Action = 'Line: '//TRIM(line)
  GOTO 9998
  END IF

ELSE IF( n_arg > Obs%nVar - Obs%nVarFixed )THEN

  IF( .NOT.Obs%lWarnTooMuchObsDAta )THEN
    error%Number  = WN_ERROR
    error%Routine = 'SWIMreadVarObs'
    WRITE(error%Message,FMT='(A,I2,A,I2)') 'Too much data: ',n_arg,' values instead of ',Obs%nVar-Obs%nVarFixed
    error%Message = 'Too much data on ID line'
    error%Inform  = 'Line: '//TRIM(line)
    error%Action  = 'Do you wish to ignore the extra data?'
    irv = SWIMwarningMessage( 'User elected not to continue' )
    IF( irv /= SWIMsuccess )GOTO 9998
    Obs%lWarnTooMuchObsDAta = .TRUE.
  END IF
  n_arg = Obs%nVar - Obs%nVarFixed

END IF

!------ Read data

i  = 1
i0 = Obs%nVarFixed

IF( Obs%lASOS1 )lSecond = .FALSE.

DO

!------ Handle ASOS 1 minute data:
!       Parse date & time. N.B. Convert local hour+minute to UTC
!       Skip odd minutes
!       Extract speed and direction from specific character range
!       Check for non-numeric values and exact number of arguments
!       Check and skip repeated times

  IF( Obs%lASOS1 )THEN
    IF( TRIM(c_arg(1)) /= Obs%AERid )THEN
      error%Inform = 'ID mismatch reading ASOS 1 minute data'
      GOTO 9998
    END IF

    READ( c_arg(2),'(3X,I4,2I2,2I2)',IOSTAT=ios) iyear,imonth,iday,ihr,imn
    IF( ios /= 0 )THEN
      error%Inform = 'Error parsing data and time of ASOS 1 minute data'
      GOTO 9998
    END IF

!------ Check for repeated times

    CALL get_next_data( Obs%unit,linex,nch,kwrd,n_arg,x_arg,MAXN,lerr )
    IF( lerr )THEN
      BACKSPACE(Obs%unit,IOSTAT=ios)
    ELSE
      READ( x_arg(2),'(3X,I4,2I2,2I2)',IOSTAT=ios) jyear,jmonth,jday,jhr,jmn
      IF( ios /= 0 )THEN
        error%Inform = 'Error parsing data and time of ASOS 1 minute data'
        GOTO 9998
      END IF
      Obs%errorLine2 = TRIM(linex)
      IF( iyear==jyear .AND. imonth==jmonth .AND. iday==jday .AND. ihr==jhr .AND. imn==jmn )THEN
        Obs%nASOS1repeat = Obs%nASOS1repeat + 1
        IF( Obs%nASOS1repeat == 1 )Obs%errorLine1 = TRIM(linex)
        nrec = nrec + 1
      ELSE
        BACKSPACE(Obs%unit,IOSTAT=ios)  !Read next time if not a repeat
      END IF

    END IF

!------ Read thermo file

    IF( Obs%lASOSthermo )THEN

      jday0 = julian_day( imonth,iday,iyear )

1001  CONTINUE
      nrecx = nrecx + 1
      READ(Obs%unit+1,'(A)',IOSTAT=ios) linex

      ! Check for end of file in all compilers
      IF( IS_IOSTAT_END(ios) )ios = -1

      IF( ios < 0 )THEN
        lend = .TRUE.; GOTO 9999
      ELSE IF( ios > 0 )THEN
        GOTO 9998
      END IF

      CALL get_next_data( 0,linex,nch,kwrd,nx_arg,x_arg,MAXN,lerr )
      IF( lerr )GOTO 9998

      IF( TRIM(x_arg(1)) /= Obs%AERid )THEN
        error%Inform = 'ID mismatch reading ASOS 1 minute thermodynamic data'
        GOTO 9998
      END IF

      READ( x_arg(2),'(3X,I4,2I2,2I2)',IOSTAT=ios) jyear,jmonth,jday,jhr,jmn
      IF( ios /= 0 )THEN
        error%Inform = 'Error parsing data and time of ASOS 1 minute thermodynamic data'
        GOTO 9998
      END IF

      jdayx = julian_day( jmonth,jday,jyear )
      CALL SetObsJulian( jdayx,jyear,iyear )

      lBadThermo = .FALSE.

      dif = (jday0 - jdayx)*24*60 + (ihr-jhr)*60 + imn-jmn  !Check for matching dates and time (within 1 minute)
      IF( dif > 1 )THEN
        GOTO 1001
      ELSE IF( dif < -1 )THEN   !Don't read thermo if more than 2 minutes beyond wind data
        lBadThermo = .TRUE.
        BACKSPACE( Obs%unit+1,IOSTAT=ios)
      END IF

    END IF

    IF( lSecond .AND. MOD(imn,2) /= 0 )THEN
      nrec = nrec - 1
      BACKSPACE(Obs%unit,IOSTAT=ios)
      IF( Obs%lASOSthermo )BACKSPACE(Obs%unit+1,IOSTAT=ios)
      EXIT
    END IF

    lerr = .FALSE.

!------ Look at specific range of characters for wind data
!       First check for non-numeric characters

    DO i = 68,90
      IF( .NOT.CheckNumChar(line(i:i)) )THEN
        lerr = .TRUE.
        EXIT
      END IF
    END DO

    IF( .NOT.lerr )THEN
      line = line(68:90)
      CALL get_next_data( 0,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
    END IF

    lerr = lerr .OR. n_arg /= 4  !Check for exactly 4 numbers in this range

    IF( .NOT.lerr )THEN
      READ(c_arg(1),*,IOSTAT=ios) wdir
      lerr = (ios/=0)
      IF( .NOT.lerr ) lerr = wdir > 360 .OR. wdir < -360
    END IF

    IF( .NOT.lerr )THEN
      READ(c_arg(2),*,IOSTAT=ios) wspd
      lerr = (ios/=0)
      IF( .NOT.lerr )lerr = wspd > 50 .OR. wspd < 0
    END IF

    IF( lerr )THEN
      IF( lSecond )EXIT
      GOTO 1000 !Read next line if this one was bad (but not if skipped a good odd minute)
    END IF

    ihr = ihr + Obs%hourOffset

    var(i0+1) = DBLE(iyear)
    var(i0+2) = DBLE(imonth)
    var(i0+3) = DBLE(iday)
    var(i0+4) = DBLE(ihr) + DBLE(imn)/60.D0

    var(i0+5) = DBLE(wdir)
    var(i0+6) = DBLE(wspd)

!------ Read thermodynamics
!       Assume these are in the last 5 discrete strings
!       Skip if there are any non-numeric characters in last 2 strings

    IF( Obs%lASOSthermo )THEN

      var(i0+7) = OBP_BADDATA
      var(i0+8) = OBP_BADDATA
      var(i0+9) = OBP_BADDATA

      IF( .NOT.lBadThermo  .AND. nx_arg > 3 )THEN

!------- First read dew point and dry bulb temperature

        tdpt = NOT_SET_R
        j    = nx_arg
        DO i = 1,LEN_TRIM(x_arg(j))
          IF( .NOT.CheckNumChar(x_arg(j)(i:i)) )THEN
            lerr = .TRUE.
            EXIT
          END IF
        END DO

        IF( .NOT.lerr )THEN

          READ(x_arg(j),*,IOSTAT=ios) p1
          IF( ios == 0 )THEN
            IF( p1 > -60. .AND. p1 < 150. )THEN
              tdpt = (p1-32.)*5./9.
            END IF
          END IF

          j  = nx_arg - 1
          tc = NOT_SET_R
          DO i = 1,LEN_TRIM(x_arg(j))
            IF( .NOT.CheckNumChar(x_arg(j)(i:i)) )THEN
              lerr = .TRUE.
              EXIT
            END IF
          END DO

          IF( .NOT.lerr )THEN
            READ(x_arg(j),*,IOSTAT=ios) p1
            IF( ios == 0 )THEN
              IF( p1 > -60. .AND. p1 < 150. )THEN
                tc = (p1-32.)*5./9.
                var(i0+8) = DBLE(tc)
              END IF
            END IF

            IF( tc /= NOT_SET_R .AND. tdpt /= NOT_SET_R )THEN
              rh = humid( tc,tdpt )
              var(i0+9) = DBLE(rh)
            END IF

!------- Now look for valid pressure measurements

            p = 0.; np = 0

            IF( nx_arg > 4 )THEN

              DO j = nx_arg-2,nx_arg-4,-1

                DO i = 1,LEN_TRIM(x_arg(j))
                  IF( .NOT.CheckNumChar(x_arg(j)(i:i)) )THEN
                    EXIT
                  END IF
                END DO

                READ(x_arg(j),*,IOSTAT=ios) p1
                IF( ios == 0 )THEN
                  IF( p1 > 10. .AND. p1 < 33. )THEN  !Valid range between 10 and 33 in. Hg
                    p = p + p1; np = np + 1
                  ELSE
                    EXIT
                  END IF
                END IF

              END DO

              IF( np > 0 )THEN
                p = p / FLOAT(np)
                var(i0+7) = DBLE(p)
              END IF

            END IF

          END IF
        END IF

      END IF
    END IF

    IF( MOD(imn,2) /= 0 )THEN
      CALL get_next_data( Obs%unit,line,nch,kwrd,n_arg,c_arg,MAXN,lerr )
      IF( lerr )THEN
        BACKSPACE(Obs%unit,IOSTAT=ios)
        !IF( Obs%lASOSthermo )BACKSPACE(Obs%unit+1,IOSTAT=ios)
        EXIT
      END IF
      nrec  = nrec + 1
      nrecx = nrecx + 1
      lSecond = .TRUE.
      CYCLE
    END IF

    EXIT  !That's all

  END IF

  IF( TRIM(c_arg(i)(1:3)) == 'ID:' )THEN
    IF( i /= 1 )THEN
      error%Inform = 'ID: string must be a beginning of line'
      GOTO 9998
    END IF
    IF( LEN_TRIM(c_arg(1)) == 3 )THEN !Increment index, reset offset & set station id
      i  = 2
      i0 = -1
      StaID  = TRIM(c_arg(i))
    ELSE                              !Read ID following colon, reset offset & set station id
      i0 = 0
      StaID  = TRIM(c_arg(i)(4:))
    END IF
    var(1) = OBP_BADDATA

  ELSE IF( i == Obs%index%ID .AND. Obs%nVarFixed == 0 )THEN
    StaID  = TRIM(c_arg(i))   !Set station id
    var(i) = OBP_BADDATA

  ELSE IF( Obs%lAERMET )THEN

    !--- AERMET Surface file
    IF( BTEST(Obs%type,OTB_SRF) )THEN

      ! 1:Yr, 2:mo, 3:dy, 4:jul, 5:Hr, 6:H, 7:u*, 8:w*, 9:VPTG, 10:Zic, 11:Zim, 12:L, 13:zo, 14:Bo, 15:r,
      ! 16:Ws, 17:Wd, 18:zref, 19:temp, 20:ztemp, 21: prec, 22:precip, 23: RH, 24:pres, 25:ccvr
      READ(c_arg(i),*,IOSTAT=ios) var(i+i0)
      IF( ios /= 0 )GOTO 9998

      ! Range settings based on CHKMSG routine in metext.f (AERMOD 15181)
      SELECT CASE( i )
        CASE( 6 )
          IF( var(i+i0) <= -999. )var(i+i0) = OBP_BADDATA
        CASE( 7 )
          IF( var(i+i0) < 0. .OR. var(i+i0) >= 9. )var(i+i0) = OBP_BADDATA
        CASE( 8,9,13,14,15,18,20,22 )
          IF( c_arg(i)(1:3) == '-9.' )var(i+i0) = OBP_BADDATA
        CASE( 10,11 )
          IF( c_arg(i)(1:5) == '-999.' )var(i+i0) = OBP_BADDATA ! -1.E+20  !Convective BL depth
        CASE( 12 )
          IF( c_arg(i)(1:7) == '-99999.' )var(i+i0) = OBP_BADDATA
        CASE( 16 )
          IF( var(i+i0) < 0. .OR. var(i+i0) >= 90. )var(i+i0) = OBP_BADDATA
        CASE( 17 )
          IF( var(i+i0) <= -9. .OR. var(i+i0) > 900. )var(i+i0) = OBP_BADDATA
        CASE( 19 )
          IF( var(i+i0) <= 0.  .OR. var(i+i0) > 900. )var(i+i0) = OBP_BADDATA
        CASE( 21 )
          IF( c_arg(i)(1:4) == '9999' )var(i+i0) = OBP_BADDATA
        CASE( 23 )
          IF( c_arg(i)(1:4) == '999.' )var(i+i0) = OBP_BADDATA
        CASE( 24 )
          IF( c_arg(i)(1:6) == '99999.' )var(i+i0) = OBP_BADDATA
      END SELECT

      IF( i > 25 )var(i+i0) = OBP_BADDATA    !Extra col with characters

    END IF

    !--- AERMET Profile file
    IF( BTEST(Obs%type,OTB_PRF) )THEN

        !1:Year, 2:month, 3:day, 4:hour, 5:height, 6:top, 7:WDnn, 8:WSnn, 9:TTnn, 10:SAnn, 11:SWnn
        READ(c_arg(i),*,IOSTAT=ios) var(i+i0)
        IF( ios /= 0 )GOTO 9998

        ! Range settings based on PFLCNV routine in metext.f (AERMOD 15181)
        SELECT CASE( i )
          CASE( 7 )
            IF( var(i+i0) <= -9. .OR. var(i+i0) > 900. )var(i+i0) = OBP_BADDATA
          CASE( 8 )
            IF( var(i+i0) < 0. .OR. var(i+i0) >= 90. )var(i+i0) = OBP_BADDATA
          CASE( 9 )
            IF( var(i+i0) <= -90.  .OR. var(i+i0) >= 90. )var(i+i0) = OBP_BADDATA
          CASE( 10,11 )
            IF( c_arg(i)(1:3) == '99.' )var(i+i0) = OBP_BADDATA
        END SELECT

        IF( i > 11 )var(i+i0) = OBP_BADDATA    !Extra col with characters

    END IF

  ELSE IF( TRIM(c_arg(i)) == TRIM(Obs%BadString) )THEN
    var(i+i0) = OBP_BADDATA   !Bad data numerical value

  ELSE
    READ(c_arg(i),*,IOSTAT=ios) var(i+i0)
    IF( ios /= 0 )GOTO 9998

  END IF

  i = i + 1
  IF( i > n_arg )EXIT

END DO

IF( Obs%lAERMET .OR. Obs%lASOS1 )StaID = Obs%AERid

9999 CONTINUE

RETURN

!------ Set read error and goto return

9998 CONTINUE
SWIMreadVarObs = SWIMfailure
error%Number   = RD_ERROR
error%Routine  = 'SWIMreadVarObs'
CALL ReportFileName( error%Message,'Error reading observation file ',Obs%Source )
error%Inform = 'Station '//StaID
error%Action = 'Line: '//TRIM(line)

irv = SWIMaddLogMessage( 'Error in SWIMreadVarObs' )
IF( irv == SWIMsuccess )irv = SWIMaddLogMessage( 'Station '//StaID )
IF( irv == SWIMsuccess )irv = SWIMaddLogMessage( TRIM(line) )
GOTO 9999

END

!==============================================================================

SUBROUTINE SWIMgetTimeObs( Obs,var8,timeObs,timeBin )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ),      INTENT( INOUT ) :: Obs
REAL(8), DIMENSION(*), INTENT( IN    ) :: var8
REAL,                  INTENT( OUT   ) :: timeObs, timeBin

INTEGER iymd, iyy, imm, idd, jul_obs, k

INTEGER, EXTERNAL :: julian_day

!------ Initialize error condition

error%Number  = IV_ERROR
error%Routine = 'SWIMgetTimeObs'
CALL ReportFileName( error%Inform,'File= ',Obs%Source )
error%Action  = 'Value = '

timeObs = NOT_SET_R
timeBin = NOT_SET_R

!------ Convert to standard SCIPUFF time based on input variables

SELECT CASE( Obs%index%TimeID%type )

!------ Set time without date (set to start julian day)

  CASE( OBP_NODATE )

    jul_obs = Obs%index%TimeID%day_start

!------ Set time based on hour and julian day

  CASE( OBP_JDAY )

    IF( var8(Obs%index%TimeID%day) == OBP_NODATA )THEN
      error%Message = 'Julian Day not set for observations'
      error%Action  = ' '
      GOTO 9999
    END IF
    jul_obs = NINT(var8(Obs%index%TimeID%day))

    IF( jul_obs < 1 .OR. jul_obs > 366 )THEN
      error%Message = 'Invalid Julian Day for met observations'
      CALL i_format( jul_obs,k,error%Action(9:) )
      GOTO 9999
    END IF

!------ Set time based on year, month, day or julian day and hour

  CASE( OBP_YMD )

    IF( var8(Obs%index%TimeID%year) == OBP_NODATA )THEN
      error%Message = 'Year not set for observations'
      error%Action  = ' '
      GOTO 9999
    END IF

    iymd = NINT(var8(Obs%index%TimeID%year))
    iyy  = iymd/10000
    imm  = (iymd-10000*iyy)/100
    idd  = iymd-10000*iyy-100*imm

    IF( iyy < 0  .OR. iyy > 9999 )THEN
      error%Message = 'Invalid year for met observations (YYYYMMDD format)'
      CALL i_format( iyy,k,error%Action(9:) )
      GOTO 9999
    END IF

    IF( imm < 1  .OR. imm > 12 )THEN
      error%Message = 'Invalid month for met observations (YYYYMMDD format)'
      CALL i_format( imm,k,error%Action(9:) )
      GOTO 9999
    END IF

    IF( idd < 1  .OR. idd > 31 )THEN
      error%Message = 'Invalid day-of-month for met observations (YYYYMMDD format)'
      CALL i_format( idd,k,error%Action(9:) )
      GOTO 9999
    END IF

    CALL SetYear( iyy )
    jul_obs = julian_day( imm,idd,iyy )
    CALL SetObsJulian( jul_obs,iyy,Obs%index%TimeID%year_start )

  CASE( OBP_YDATE )

    IF( var8(Obs%index%TimeID%year) == OBP_NODATA )THEN
      error%Message = 'Year not set for observations'
      error%Action  = ' '
      GOTO 9999
    ELSE IF( var8(Obs%index%TimeID%month) == OBP_NODATA )THEN
      error%Message = 'Month not set for observations'
      error%Action  = ' '
      GOTO 9999
    ELSE IF( var8(Obs%index%TimeID%day) == OBP_NODATA )THEN
      error%Message = 'Day of month not set for observations'
      error%Action  = ' '
      GOTO 9999
    END IF

    iyy  = NINT(var8(Obs%index%TimeID%year))
    imm  = NINT(var8(Obs%index%TimeID%month))
    idd  = NINT(var8(Obs%index%TimeID%day))

    IF( iyy < 1  .OR. iyy > 9999 )THEN
      error%Message = 'Invalid year for met observations'
      CALL i_format( iyy,k,error%Action(9:) )
      GOTO 9999
    END IF

    IF( imm < 1  .OR. imm > 12 )THEN
      error%Message = 'Invalid month for met observations'
      CALL i_format( imm,k,error%Action(9:) )
      GOTO 9999
    END IF

    IF( idd < 1  .OR. idd > 31 )THEN
      error%Message = 'Invalid day-of-month for met observations'
      CALL i_format( idd,k,error%Action(9:) )
      GOTO 9999
    END IF

    CALL SetYear( iyy )
    jul_obs = julian_day( imm,idd,iyy )
    CALL SetObsJulian( jul_obs,iyy,Obs%index%TimeID%year_start )

  CASE( OBP_YJDAY )

    IF( var8(Obs%index%TimeID%year) == OBP_NODATA )THEN
      error%Message = 'Year not set for observations'
      error%Action  = ' '
      GOTO 9999
    ELSE IF( var8(Obs%index%TimeID%day) == OBP_NODATA )THEN
      error%Message = 'Julian Day not set for observations'
      error%Action  = ' '
      GOTO 9999
    END IF

    iyy      = NINT(var8(Obs%index%TimeID%year))
    jul_obs  = NINT(var8(Obs%index%TimeID%day))

    IF( iyy < 1  .OR. iyy > 9999 )THEN
      error%Message = 'Invalid year for met observations'
      CALL i_format( iyy,k,error%Action(9:) )
      GOTO 9999
    END IF

    IF( jul_obs < 1 .OR. jul_obs > 366 )THEN
      error%Message = 'Invalid Julian Day for met observations'
      CALL i_format( jul_obs,k,error%Action(9:) )
      GOTO 9999
    END IF

    CALL SetYear( iyy )
    CALL SetObsJulian( jul_obs,iyy,Obs%index%TimeID%year_start )

END SELECT

IF( Obs%index%TimeID%type /= OBP_NODATE )THEN
  IF( var8(Obs%index%TimeID%hour) == OBP_NODATA )THEN
    error%Message = 'Hour not set for observations'
    error%Action  = ' '
    GOTO 9999
  END IF
END IF

timeObs = (jul_obs-Obs%index%TimeID%day_start)*86400. + &
             SNGL(var8(Obs%index%TimeID%hour)) * Obs%Conv(Obs%index%TimeID%hour)

IF( Obs%lAERMET )timeObs = timeObs - 3600. !Shift 1 hour for AERMOD met, e.g., hour 1 = 0000

timeBin = timeObs
CALL bin_time( timeBin,Obs%timeBin )

timeObs = timeObs - Obs%timeOffset
timeBin = timeBin - Obs%timeOffset

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetObsJulian( jul_obs,year_obs,year0 )

!------ Set julian day of year_obs relative to year0

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: jul_obs, year_obs
INTEGER, INTENT( IN    ) :: year0

INTEGER jyy

INTEGER, EXTERNAL :: days_in_year

CALL SetYear( year_obs )

IF( year_obs > year0 )THEN

  DO jyy = year0,year_obs-1
    jul_obs = jul_obs + days_in_year( jyy )
  END DO

ELSE IF( year_obs < year0 )THEN

  DO jyy = year0-1,year_obs,-1
    jul_obs = jul_obs - days_in_year( jyy )
  END DO

END IF

RETURN
END

!===============================================================================

SUBROUTINE SetYear( iyy )

USE default_fd

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: iyy

IF( iyy /= NOT_SET_I )THEN

  IF( iyy < 50 )THEN
    iyy = iyy + 2000
  ELSE IF( iyy < 100 )THEN
    iyy = iyy + 1900
  END IF

END IF

RETURN
END

!===============================================================================

SUBROUTINE bin_time( time,bin )

USE default_fd

IMPLICIT NONE

REAL, INTENT( INOUT ):: time
REAL, INTENT( IN    ):: bin

REAL, PARAMETER :: EPS = 1.E-6

IF( bin == NOT_SET_R .OR. bin == DEF_VAL_R .OR. bin == 0. )GOTO 9999

IF( time < 0. )time = time - bin

time = bin*INT(time/bin+0.5-EPS)

9999 CONTINUE

RETURN
END

!==============================================================================

LOGICAL FUNCTION SWIMcheckVel( Obs,var8 )

USE SWIM_fi
USE SWIMparam_fd

TYPE( FirstObs ),      INTENT( IN ) :: Obs
REAL(8), DIMENSION(*), INTENT( IN ) :: var8

INTEGER i, i2

SWIMcheckVel = .FALSE.

DO i = Obs%nVarFixed+1,Obs%nVar

  SELECT CASE( Obs%VarID(i) )

    CASE( OVP_U,OVP_DIR )     !Check for valid velocity

      i2 = NINT(Obs%Conv(i))
      SWIMcheckVel = var8(i) /= OBP_BADDATA .AND. var8(i2) /= OBP_BADDATA
      EXIT

  END SELECT

END DO

RETURN
END

!==============================================================================

LOGICAL FUNCTION CheckNumChar( c )

!------- Check if character is numerical or blank

IMPLICIT NONE

CHARACTER(1), INTENT( IN ) :: c

INTEGER i

i = IACHAR(c)
CheckNumChar = (i == 32 .OR. i >=48 .AND. i <= 57)  !Blank or 0 to 9

RETURN
END
