INTEGER FUNCTION SWIMInitRun( SCIPUFFinit )

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMInitRun

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE constants_fd

IMPLICIT NONE

TYPE( SWIMinitmet ), INTENT( INOUT ) :: SCIPUFFinit

INTEGER irv, i, jObs, alloc_stat, unit, numGridded, nzStnd, ifld, n
INTEGER grdField, numTer, numObsSrc0, ios
REAL    soq, zmaxStnd
LOGICAL lflag

CHARACTER(128) string

INTEGER, EXTERNAL :: SWIMclearLogMessage, SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMinitObs, SWIMinitFixed, SWIMcheckPolar
INTEGER, EXTERNAL :: SWIMinitObsField, SWIMinitGridded
INTEGER, EXTERNAL :: SWIMreallocMetField, SWIMaddSmoothPotential
INTEGER, EXTERNAL :: SWIMinitGriddedNest
INTEGER, EXTERNAL :: SetGridLL, InitLSV, CopyPrjInput
INTEGER, EXTERNAL :: PostProgressMessage, GridLogMsg, InitStndAtmos
LOGICAL, EXTERNAL :: HasPrjReference
INTEGER, EXTERNAL :: SWIMinitObsAssim
INTEGER, EXTERNAL :: SWIMinitASOS1min
INTEGER, EXTERNAL :: SWIMinitAERsfc, SWIMinitAERpfl
INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS
REAL,    EXTERNAL :: SWIMsetHmin
INTERFACE

  INTEGER FUNCTION ReadZgrid( VertGridFile,nz,z )
    CHARACTER(*), INTENT( IN  ) :: VertGridFile
    INTEGER,      INTENT( OUT ) :: nz
    REAL, DIMENSION(:), POINTER :: z
  END FUNCTION ReadZgrid

  INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )
    USE SWIM_fi
    REAL, DIMENSION(:),      POINTER         :: zMC
    INTEGER,                 INTENT( IN    ) :: nzMC
    TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid
  END FUNCTION SWIMinitMcWIF

END INTERFACE

!------ Initialize success value

SWIMresult = SWIMsuccess
StopMode   = SWIM_ENABLE

CALL SWIMsetMessaging()

!------ Post message

message%iParm    = NO_ERROR
message%jParm    = 0 !FALSE
message%routine  = 'SWIM'

message%aString  = 'Initializing SCIPUFF Weather Input Module'
message%bString  = ' '
message%cString  = ' '

irv = PostProgressMessage( message )

SWIMInitRun = SWIMfailure

!------ Initial message to SCIPUFF log file

irv = SWIMclearLogMessage()  !Nullify linked-list for log messages

string = CHAR(13)  !Carriage return
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

irv = SWIMaddLogMessage( message%aString )

IF( irv /= SWIMsuccess )GOTO 9999


numThreads = OMP_GET_MAX_THREADS()
!------ Set default values for assimilation

SCM_MAXITER   = 10    !Max. number of "iterations to optimal solution"
DU2EPS        = 0.001 !Convergence criterion (normalized change in squared velocity)
R_OBS         = 10.E3 !Radius of influence (meters)
E2BO          = 9.    !Ratio of background error variance to obs; "standard value" from Kovalets, et al. (2004)
MCWIFWtFac    = 0.    !Multiplies obs interpolation weights in exponential argument. 0 = no obs weight

!------ Save project input

irv = CopyPrjInput( SCIPUFFinit%prj,Prj,.TRUE. )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Setup project coordinate structure (may be reset after reading met input)

CALL SWIMsetPrjCoord()

lInitOutput = .FALSE.

!------ Setup standard atmosphere on a uniform grid

zmaxStnd = MAX(Prj%Zmax,20000.)

irv = InitStndAtmos( zmaxStnd,nzStnd )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Make sure simple BL parameters are reasonable

IF( Prj%BL%ZImin <= 0. )Prj%BL%ZImin = 50.
IF( Prj%BL%ZImax <= 0. )Prj%BL%ZImax = 1000.

Prj%BL%ZImin = MIN( Prj%BL%ZImin,5000.)
Prj%BL%ZImax = MIN( Prj%BL%ZImax,5000.)

Prj%BL%HFLXmin = MAX( Prj%BL%HFLXmin,-0.1 )
Prj%BL%HFLXmax = MIN( Prj%BL%HFLXmax, 1.0 )

!------ Check if time zone required (BL requirements checked later)

IF( Prj%Local .NEQV. Prj%LocalMet )THEN
  IF( Prj%timeZone == NOT_SET_R .OR. Prj%timeZone == DEF_VAL_R )THEN
    error%Number   = IV_ERROR
    error%Routine  = 'SWIMinitRun'
    error%Message  = 'Project and meteorology time references differ'
    error%Inform   = 'Time zone must be set'
    GOTO 9999
  END IF
END IF

IF( Prj%decay )THEN
  IF( .NOT.( Prj%Local             .OR. &
             Prj%coord == I_LATLON .OR. &
             Prj%coord == I_UTM    .OR. &
             HasPrjReference()        ) )THEN
    IF( Prj%TimeZone == NOT_SET_R .OR. Prj%TimeZone == DEF_VAL_R )THEN
      error%Number   = IV_ERROR
      error%Routine  = 'SWIMinitRun'
      error%Message  = 'Cannot compute activity decay without time zone'
      GOTO 9999
    END IF
  END IF
END IF

Reverse = Prj%dayEnd == -1

!------ Get number of obs and gridded fields

numObsSrc  = 0
numGridded = 0
numTer     = 0

DO i = 1,SCIPUFFinit%nMetSrc

  SELECT CASE( SCIPUFFinit%type(i) )

    CASE( SWIMobs,SWIMfixed )
      numObsSrc = numObsSrc + 1

    CASE( SWIMAERsfc,SWIMAERpfl,SWIMASOS1M )
      numObsSrc = numObsSrc + 1

    CASE( SWIMterAssm)
      IF( LEN_TRIM(SCIPUFFinit%MetSrc(i)) > 0 )THEN
        numTer = numTer + 1
      ELSE
        SCIPUFFinit%type(i) = -1 !Ignore blank terrain file names
      END IF

    CASE( SWIMvrtGrid)
      IF( LEN_TRIM(SCIPUFFinit%MetSrc(i)) == 0 )THEN
        SCIPUFFinit%type(i) = -1 !Ignore blank vert grid file names
      END IF

    CASE( SWIMgrid,SWIMSCIP,SWIMMEDOC )
      numGridded = numGridded + 1

    CASE( SWIMWRF )
      numGridded = numGridded + 1

    CASE( SWIMMEDLIST )
      numGridded = numGridded + 1

    CASE DEFAULT
      !Skip

  END SELECT

END DO

IF( numObsSrc == 0 .AND. numGridded == 0 )THEN
  error%Number  = IV_ERROR
  error%Routine = 'SWIMInitRun'
  error%Message = 'No observation or gridded met specified in list file'
  GOTO 9999
END IF

!------ Checks for terrain file specification with list input

IF( (numObsSrc > 0 .AND. numGridded > 0) .OR. numGridded > 1 .OR. numTer > 0 )THEN
  IF( BTEST(Prj%MC%type,MCB_TER) .OR. BTEST(Prj%MC%type,MCB_LC) )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMInitRun'
    error%Message = 'Terrain file and/or mass-consisent calculation specified in msc file'
    error%Inform  = 'Invalid with assimilation/list input'
    GOTO 9999
  END IF
END IF

!------ Exit if obs met is used for reverse calculation

IF( Reverse )THEN
  IF( numObsSrc /= 0 )THEN
    error%Number  = RV_ERROR
    error%Routine = 'SWIMInitRun'
    error%Message = 'Observational met invalid for time-reverse calculation'
    error%Inform  = 'Must be MEDOC file w/ boundary layer fields'
    GOTO 9999
  END IF
END IF

!------ Allocate obs field structures

IF( numObsSrc > 0 )THEN

  ALLOCATE( ObsSrc(numObsSrc),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMInitRun'
    error%Message = 'Error allocating ObsSrc'
    GOTO 9999
  END IF
  DO jObs = 1,numObsSrc
    NULLIFY(ObsSrc(jObs)%VarID)
    NULLIFY(ObsSrc(jObs)%Conv)
    NULLIFY(ObsSrc(jObs)%z)
    NULLIFY(ObsSrc(jObs)%Obs,ObsSrc(jObs)%PrevObs )
    NULLIFY(ObsSrc(jObs)%GridList,ObsSrc(jObs)%PrevGridList )
    NULLIFY(ObsSrc(jObs)%ObsArray )
    ObsSrc(jObs)%lAERMET      = .FALSE.
    ObsSrc(jObs)%lASOS1       = .FALSE.
    ObsSrc(jObs)%lASOSthermo  = .FALSE.
    ObsSrc(jObs)%nASOS1repeat = 0
  END DO
END IF

!------ Setup met fields

unit        = SWIMunit
jObs        = 0
numField    = 0
numFieldMax = 0
grdField    = 0
numTer      = 0
numObsSrc0  = 0

DO i = 1,SCIPUFFinit%nMetSrc

  SELECT CASE( SCIPUFFinit%type(i) )

!------ Read assimilation parameters

    CASE( ASSIM_R_OBS )
      READ(SCIPUFFinit%MetSrc(i),*,IOSTAT=ios) R_OBS
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error reading assimilation parameter R_OBS'
        GOTO 9999
      END IF

    CASE( ASSIM_E2BO )
      READ(SCIPUFFinit%MetSrc(i),*,IOSTAT=ios) E2BO
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error reading assimilation parameter E2BO'
        GOTO 9999
      END IF

    CASE( ASSIM_DU2EPS )
      READ(SCIPUFFinit%MetSrc(i),*,IOSTAT=ios) DU2EPS
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error reading assimilation parameter DU2EPS'
        GOTO 9999
      END IF

    CASE( ASSIM_SCM_MAXITER )
      READ(SCIPUFFinit%MetSrc(i),*,IOSTAT=ios) SCM_MAXITER
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error reading assimilation parameter ASSIM_MAXITER'
        GOTO 9999
      END IF

    CASE( ASSIM_MCWIFWtFac )
      READ(SCIPUFFinit%MetSrc(i),*,IOSTAT=ios) MCWIFWtFac
      IF( ios /= 0 )THEN
        error%Number  = RD_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error reading assimilation parameter MCWIFWtFac'
        GOTO 9999
      END IF

!------ Observational-type met

    CASE( SWIMobs,SWIMfixed,SWIMAERsfc,SWIMAERpfl,SWIMASOS1M )
      jObs = jObs + 1
      unit = unit + 1

      ObsSrc(jObs)%Source = TRIM(SCIPUFFinit%MetSrc(i))
      ObsSrc(jObs)%unit   = unit

      ObsSrc(jObs)%index%TimeID%day_start  = SCIPUFFinit%prj%julStart
      ObsSrc(jObs)%index%TimeID%year_start = SCIPUFFinit%prj%yearStart

      SELECT CASE( SCIPUFFinit%type(i) )

        CASE( SWIMobs )
          irv = SWIMinitObs( ObsSrc(jObs) )

        CASE( SWIMAERsfc )
          irv = SWIMinitAERsfc( ObsSrc(jObs) )

        CASE( SWIMAERpfl )
          irv = SWIMinitAERpfl( ObsSrc(jObs) )

        CASE( SWIMASOS1M )
          irv = SWIMinitASOS1min( ObsSrc(jObs) )
          IF( ObsSrc(jobs)%lASOSthermo )unit = unit + 1

        CASE( SWIMfixed )
          unit = unit - 1                    !No unit number required
          ObsSrc(jObs)%unit = -999
          irv = SWIMinitFixed( ObsSrc(jObs) )

      END SELECT
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( grdField > 0 )THEN

!        IF( Prj%Create )CYCLE               !Ignore during CREATE

        ifld = MAX(grdField,numTer)  !Associate with gridded field
        field(ifld)%nObsSource = field(ifld)%nObsSource + 1
        IF( field(ifld)%nObsSource > MAXOBSASSM )THEN
          error%Number  = IV_ERROR
          error%Routine = 'SWIMInitRun'
          error%Message = 'Too many observation sources for assimilation'
          WRITE(error%Message,FMT='(A,I3)') 'Maximum is ',MAXOBSASSM
          GOTO 9999
        END IF
        field(ifld)%iObsSource(field(ifld)%nObsSource) = jObs

      ELSE

!------ Initialize the top level met field associated with observations only,
!       i.e., w/o assimilation into gridded background
!       N.B. Setup met field if this is the first MetSrc

        IF( numField == 0 )THEN

          irv = SWIMreallocMetField( 1 )    !Increments numField
          IF( irv /= SWIMsuccess )GOTO 9999

          field(1)%index  = 0
          CALL setFieldIndex( field(1)%index,numField )

        END IF

        numObsSrc0 = numObsSrc0 + 1

      END IF

    CASE( SWIMterAssm )

      IF( numField == 0 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SWIMinitRun'
        error%Message = 'Invalid placement of terrain file name in assimilation/list input'
        error%Inform  = 'Terrain file must come after gridded or observation file name(s)'
        GOTO 9999
      END IF

      IF( grdField > 0 )THEN              !Check if terrain file is associated with gridded field

        IF( Prj%Create )CYCLE             !Ignore during CREATE

        irv = SWIMreallocMetField( 1 )    !Increments numField
        IF( irv /= SWIMsuccess )GOTO 9999

        numTer = numField

        ALLOCATE( field(numField)%gridSource%Source(2),STAT=alloc_stat )  !Allocate 2 to allow for vertical grid file
        IF( alloc_stat /= 0 )THEN
          error%Number  = UK_ERROR
          error%Routine = 'SWIMinitRun'
          error%Message = 'Error allocating array for nested terrain file name'
          GOTO 9999
        END IF

        field(numField)%gridSource%Source(1) = TRIM(SCIPUFFinit%MetSrc(i))
        field(numField)%gridSource%nSource   = 1
        field(numField)%gridSource%unit      = grdField

        field(numField)%type = IBSET(0,FTB_NEST)

      ELSE                                !Terrain file used with observations (no gridded file)
                                          !N.B. Only one such file utilized (last supersedes)

        Prj%MC%type = IBSET(0,MCB_TER)
        Prj%MC%type = IBSET(Prj%MC%type,MCB_LC)
        Prj%MC%type = IBSET(Prj%MC%type,MCB_MCWIF)
        Prj%MC%TerFile  = TRIM(SCIPUFFinit%MetSrc(i))

      END IF

    CASE( SWIMvrtGrid )

      IF( numField == 0 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SWIMinitRun'
        error%Message = 'Invalid placement of vertical grid file name in assimilation/list input'
        error%Inform  = 'Grid file must come after terrain, gridded or observation file name(s)'
        GOTO 9999
      END IF

      IF( numTer > 0 )THEN                !Check if vertical grid file is associated with terrain file

        IF( Prj%Create )CYCLE             !Ignore during CREATE

        field(numField)%gridSource%Source(2) = TRIM(SCIPUFFinit%MetSrc(i))
        field(numField)%gridSource%nSource   = 2

      ELSE                                !Use with terrain file and observations (no gridded file)
                                          !N.B. supersedes existing grid in MC structure
        IF( BTEST(Prj%MC%type,MCB_TER) )THEN
          IF( Prj%MC%nz > 0 )DEALLOCATE(Prj%MC%z,STAT=alloc_stat)
          irv = ReadZgrid( SCIPUFFinit%MetSrc(i),Prj%MC%nz,Prj%MC%z )
          IF( irv /= SWIMsuccess )GOTO 9999
        ELSE IF( .NOT.(numObsSrc > 0 .AND. numGridded > 0) )THEN  !Only read if not doing assimilation into MEDOC gridded field
          irv = ReadZgrid( SCIPUFFinit%MetSrc(i),field(numField)%grid%nZ,field(numField)%grid%z )
          IF( irv /= SWIMsuccess )GOTO 9999
        END IF

      END IF

!------ Gridded-type met

    CASE( SWIMgrid,SWIMMEDOC,SWIMSCIP,SWIMWRF,SWIMMEDLIST )

      IF( (BTEST(Prj%MC%type,MCB_TER) .OR. BTEST(Prj%MC%type,MCB_LC)) .AND. numField > 1 )THEN
        error%Number  = IV_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Terrain file specified before gridded file'
        error%Inform  = 'Invalid with assimilation/list input'
        GOTO 9999
      END IF

      irv = SWIMreallocMetField( 1 )    !Increments numField
      IF( irv /= SWIMsuccess )GOTO 9999

      field(numField)%gridSource%nSource = 1
      ALLOCATE( field(numField)%gridSource%Source(1),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        error%Number  = UK_ERROR
        error%Routine = 'SWIMInitRun'
        error%Message = 'Error allocating string for gridded file name'
        GOTO 9999
      END IF

      unit = unit + 1
      field(numField)%gridSource%Source(1) = TRIM(SCIPUFFinit%MetSrc(i))
      field(numField)%gridSource%unit      = unit
      field(numField)%index                = 0
      CALL setFieldIndex( field(numField)%index,numField )

      SELECT CASE( SCIPUFFinit%type(i) )  !Set grid source type, if known

        CASE( SWIMMEDOC )
          field(numField)%gridSource%type = IBSET(field(numField)%gridSource%type,GSB_MEDOC)

        CASE( SWIMWRF )
          field(numField)%gridSource%type = IBSET(field(numField)%gridSource%type,GSB_INITWRF)

        CASE( SWIMMEDLIST )
          field(numField)%gridSource%type = IBSET(field(numField)%gridSource%type,GSB_INITMEDOC)

        CASE( SWIMSCIP )
          field(numField)%gridSource%type = IBSET(field(numField)%gridSource%type,GSB_SCIP)

        CASE DEFAULT
          !TYPE not set

      END SELECT

      ifld = numField
      grdField = ifld
      numTer   = 0

      irv = SWIMinitGridded( ifld,unit,SCIPUFFinit%type(i) )
      IF( irv /= SWIMsuccess )GOTO 9999

  END SELECT

END DO

!------ Check input for time-reverse calculation

IF( Reverse )THEN
  DO i = 1,numField
    lflag = .NOT.BTEST(field(i)%type,FTB_MEDOC)
    lflag = lflag .OR. .NOT.BTEST(field(i)%type,FTB_WRF)
    IF( .NOT.lflag )THEN
      error%Number  = RV_ERROR
      error%Routine = 'SWIMInitRun'
      error%Message = 'Invalid met input for time-reverse calculation'
      error%Inform  = 'Must be gridded file w/ boundary layer fields'
    GOTO 9999
    END IF
  END DO
END IF

!------ Set number of obs sources associated with top level met field

IF( numObsSrc0 > 0 )THEN

  IF( numObsSrc0> MAXOBSASSM )THEN
    error%Number  = IV_ERROR
    error%Routine = 'SWIMInitRun'
    error%Message = 'Too many observation sources'
    WRITE(error%Inform,FMT='(A,I3)') 'Maximum is ',MAXOBSASSM
    GOTO 9999
  END IF

  field(1)%nObsSource = numObsSrc0
  field(1)%iObsSource(1:numObsSrc0) = (/ (jobs,jObs=1,numObsSrc0) /)

  irv = SWIMinitObsField( field(1),SCIPUFFinit%type(1) )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Some final things

DO i = 1,numField

  IF( BTEST(field(i)%type,FTB_NEST) )CYCLE  !Nests will be fully setup later

!------ Set initialization status flags for BL

  field(i)%status = IBSET(field(i)%status,FSB_DOINITBL)
  field(i)%status = IBSET(field(i)%status,FSB_DOINITBL2)

!------ Compute lat/lon arrays if possible

  irv = SetGridLL( field(i)%grid )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Initialize large-scale variability

  irv = InitLSV( field(i) )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Log messages

  irv = GridLogMsg( field(i) )
  IF( irv /= SWIMsuccess )GOTO 9999

END DO

!------ Check for nest-in-gridded fields

IF( .NOT.Prj%Create )THEN

  IF( BTEST(Prj%MC%type,MCB_GRIDNEST) )THEN

    irv = SWIMinitGriddedNest( 0 )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE

!------ Setup nests from list input; also for obs assimilation w/o terrain nest

    DO i = 1,numField

!------ Create nests based on terrain files

      IF( BTEST(field(i)%type,FTB_NEST) )THEN
        irv = SWIMinitGriddedNest( i )
        IF( irv /= SWIMsuccess )GOTO 9999

!------ Setup field for obs source assimilation into gridded background

      ELSE IF( field(i)%nObsSource > 0 .AND. .NOT.BTEST(field(i)%type,FTB_OBS) )THEN
        irv = SWIMinitObsAssim( field(i) )
        IF( irv /= SWIMsuccess )GOTO 9999

        irv = SWIMinitMcWIF( field(i)%grid%Z,-1,field(i)%grid ) !Initialize for mass-consistent adjustment (-1 => grid is unchanged)
        IF( irv /= SWIMsuccess )GOTO 9999

        field(i)%type = IBSET(field(i)%type,FTB_MCWIF)

      END IF

    END DO

  END IF

  unitMet = unit  !Save last (largest) permananent unit number

END IF

!------ Check if polar field(s) should be added

DO i = 1,numField
  irv = SWIMcheckPolar( i )
  IF( irv /= SWIMsuccess )GOTO 9999
END DO

!------ Overall minimum elevation

Prj%Hmin = SWIMsetHmin()

!------ Set troposphere vertical diffusivity constants

soq          = Prj%BL%SLtrop/SQRT(MAX(3.*Prj%BL%WWtrop,1.E-6))
Prj%BL%Atrop = soq/a*Prj%BL%WWtrop
Prj%BL%Btrop = 2.*EQF*soq**2

!------ Allocate potential smooth fields

n = numField

DO i = 1,n
  irv = SWIMaddSmoothPotential( i )
  IF( irv /= SWIMsuccess )GOTO 9999
END DO

!------ Let SCIPUFF know about modifications to project parameters

irv = CopyPrjInput( Prj,SCIPUFFinit%prj,.FALSE. )

!------ SWIMming success

SWIMInitRun = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION CopyPrjInput( PrjIn,PrjOut,lCopyZ )

USE SWIM_fi
USE SWIMparam_fd

TYPE( PrjInput ), INTENT( IN  ) :: PrjIn
TYPE( PrjInput ), INTENT( OUT ) :: PrjOut
LOGICAL,          INTENT( IN  ) :: lCopyZ

INTEGER alloc_stat

REAL, DIMENSION(:), POINTER :: zptr

CopyPrjInput = SWIMfailure

!------ Save pointer to Z array in output structure, if not copying

IF( .NOT.lCopyZ ) zptr => PrjOut%MC%Z

!------ Set equal to define "scalar" integer and real components

PrjOut = PrjIn

!------ Copy Z array, if requested

IF( lCopyZ )THEN

!------ First nullify pointer to Z array in output structure

  NULLIFY( PrjOut%MC%Z )

!------ Now allocate and copy array

  IF( PrjOut%MC%nz > 0 )THEN

    ALLOCATE( PrjOut%MC%z(PrjOut%MC%nz),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'CopyPrjInput'
      error%Message = 'Error allocating Mass-consistent model Z array'
      WRITE(error%inform,'(A,I0,A,I0)')'ALLOC=',alloc_stat,' : SIZE=',PrjOut%MC%nz
      GOTO 9999
    END IF

    PrjOut%MC%z = PrjIn%MC%z

  END IF


ELSE

!------ Reset Z pointer since array was not copied

  PrjOut%MC%z => zptr

END IF

CopyPrjInput = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION GridLogMsg( fld )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetField ), INTENT( IN ) :: fld

CHARACTER(128) string

INTEGER irv, ios, k, n1, n2
INTEGER is
INTEGER(8) :: ftype

CHARACTER(PATH_MAXLENGTH), DIMENSION(:), POINTER :: pSource

INTEGER, EXTERNAL :: SWIMaddLogMessage, getFieldIndex
LOGICAL, EXTERNAL :: HasPrjReference

GridLogMsg = SWIMfailure

string = CHAR(13)  !Carriage return
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

WRITE(string,'("Meteorology grid ",I2)') getFieldIndex( fld%index )
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

SELECT CASE( fld%grid%coord%type )
  CASE( I_LATLON )
    WRITE(string,'(A)') 'Lat/lon coordinates '

  CASE( I_UTM )
    WRITE(string,'(A,I3)',IOSTAT=ios) 'UTM coordinates: Zone = ',fld%grid%coord%zone

  CASE( I_LAMBERT )
    WRITE(string,'(A,F9.4,F10.4)',IOSTAT=ios) 'Lambert Conformal coordinates: Reference Lat,Lon = ', &
                                              fld%grid%coord%Lat0,fld%grid%coord%Lon0

  CASE( I_POLAR )
    WRITE(string,'(A,F9.4,F10.4)',IOSTAT=ios) 'Polar Stereographic coordinates: Reference Lat,Lon = ', &
                                              fld%grid%coord%Lat0,fld%grid%coord%Lon0

  CASE( I_RPOLAR )
    WRITE(string,'(A,F9.4,F10.4)',IOSTAT=ios) 'Rotated Polar Stereographic coordinates: Reference Lat,Lon = ', &
                                              fld%grid%coord%Lat0,fld%grid%coord%Lon0

  CASE( I_ROTLL )
    WRITE(string,'(A,F9.4,F10.4)',IOSTAT=ios) 'Rotated Spherical coordinates: Rotation (Lat,Lon) = ', &
                                              fld%grid%coord%Lat0,fld%grid%coord%Lon0

  CASE( I_MERCATOR )
    WRITE(string,'(A,F9.4,F10.4)',IOSTAT=ios) 'Mercator coordinates: Reference Lat,Lon = ', &
                                              fld%grid%coord%Lat1,fld%grid%coord%Lon0

  CASE DEFAULT !is cartesian
    IF( fld%grid%coord%type == I_METERS )THEN
      WRITE(string,'(A)',IOSTAT=ios) 'Cartesian coordinates (meters)'
    ELSE
      WRITE(string,'(A)',IOSTAT=ios) 'Cartesian coordinates (kilometers)'
    END IF
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

    IF( HasPrjReference() )THEN
      WRITE(string,"('Reference location(',F10.2,',',F10.2, ')')",IOSTAT=ios) Prj%Xref,Prj%Yref
      irv = SWIMaddLogMessage( string )
      IF( irv /= SWIMsuccess )GOTO 9999
      WRITE(string,"('Reference lat/lon (',F10.4,',',F10.4, ')')",IOSTAT=ios) Prj%Lat0,Prj%Lon0
    END IF

END SELECT
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

WRITE(string,101,IOSTAT=ios) fld%grid%Xmin,fld%grid%Xmax,fld%grid%dX,fld%grid%nX
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

WRITE(string,102,IOSTAT=ios) fld%grid%Ymin,fld%grid%Ymax,fld%grid%dY,fld%grid%nY
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(fld%grid%type,GTB_Z3D) )THEN
  irv = SWIMaddLogMessage( 'z:   3d height field.  Representative values reported later' )
ELSE
  irv = SWIMaddLogMessage( 'z:   Terrain-following coordinate.  Height (AGL) at domain center' )
  is = MAX( (fld%grid%nY/2-1)*fld%grid%nX + fld%grid%nX/2,1 )
  n1 = 1
  n2 = MIN0(fld%grid%nZ,n1+5)
  IF( BTEST(fld%type,FTB_MEDOC) .AND. (.NOT.BTEST(fld%type,FTB_W) .AND. BTEST(fld%grid%type,GTB_TERRAIN)) )THEN
    DO WHILE( n1 <= fld%grid%nZ )
      WRITE(string,"('   ',6ES12.4)",IOSTAT=irv) (fld%grid%Z(k),k=n1,n2)
      irv = SWIMaddLogMessage( string )
      IF( irv /= SWIMsuccess )GOTO 9999
      n1 = n1+6; n2 = MIN0(fld%grid%nZ,n1+5)
    END DO
  ELSE
    DO WHILE( n1 <= fld%grid%nZ )
      WRITE(string,"('   ',6ES12.4)",IOSTAT=irv) (fld%grid%Z(k)*fld%grid%terrain%d(is),k=n1,n2)
      irv = SWIMaddLogMessage( string )
      IF( irv /= SWIMsuccess )GOTO 9999
      n1 = n1+6; n2 = MIN0(fld%grid%nZ,n1+5)
    END DO
  END IF
END IF

IF( BTEST(fld%grid%type,GTB_TERRAIN) )THEN
  WRITE(string,'(A,2ES12.4)',IOSTAT=ios) 'Minimum/maximum terrain elevation =',fld%grid%Hmin,fld%grid%Hmax+fld%grid%Hmin
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( fld%BLType /= BLP_NONE )THEN

  WRITE(string,105,IOSTAT=ios) fld%BLaux%nz
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!----- Write assimilation parameters to log file

IF( BTEST(fld%status,FSB_FIRSTASSM) )THEN

  irv = SWIMaddLogMessage( "*****************************************************" )
  IF( irv /= SWIMsuccess )GOTO 9999
  irv = SWIMaddLogMessage( "Assimilating observations with background meteorology" )
  IF( irv /= SWIMsuccess )GOTO 9999

  IF( BTEST(fld%type,FTB_NEST) )THEN
    k = fld%gridSource%unit
    ftype = field(k)%gridSource%type
    pSource => field(k)%gridSource%Source
  ELSE
    ftype = fld%gridSource%type
    pSource => fld%gridSource%Source
  END IF

  IF( BTEST(ftype,GSB_MEDOC) )THEN
    string = 'MEDOC file: '//TRIM(pSource(1))
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE IF( BTEST(ftype,GSB_SCIP) )THEN
    string = 'SCIP gridded file: '//TRIM(pSource(1))
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

  ELSE IF( BTEST(ftype,GSB_INITWRF) )THEN
    string = 'WRF list file: '//TRIM(pSource(1))
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999

  END IF

  IF( BTEST(fld%type,FTB_NEST) )THEN
    string = 'TER file: '//TRIM(fld%gridSource%Source(1))
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  DO k = 1,fld%nObsSource
    string = 'OBS file: '//TRIM(ObsSrc(fld%iObsSource(k))%Source)
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END DO

  WRITE(string,"('Error variance ratio :',ES9.2)",IOSTAT=irv) E2BO
  IF( irv == 0 )THEN
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  WRITE(string,"('Influence radius (km):',ES9.2)",IOSTAT=irv) R_OBS*1.E-3
  IF( irv == 0 )THEN
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  WRITE(string,"('MC adjustment factor :',ES9.2)",IOSTAT=irv) MCWIFWtFac
  IF( irv == 0 )THEN
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  WRITE(string,"('Convergence criterion:',ES9.2)",IOSTAT=irv) DU2EPS
  IF( irv == 0 )THEN
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  WRITE(string,"('Maximum iterations   :',I3)",IOSTAT=irv) SCM_MAXITER
  IF( irv == 0 )THEN
    irv = SWIMaddLogMessage( string )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

  irv = SWIMaddLogMessage( "*****************************************************" )
  IF( irv /= SWIMsuccess )GOTO 9999

  string = CHAR(13)  !Carriage return
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

GridLogMsg = SWIMresult

9999 CONTINUE

RETURN

!------ Log file message formats

101 FORMAT('xmin,xmax,dx,nx: ',3ES12.4,I4)
102 FORMAT('ymin,ymax,dy,ny: ',3ES12.4,I4)
103 FORMAT('z: ',6ES12.4)
104 FORMAT('   ',6ES12.4)
105 FORMAT('nzBL: ',I4)

END
