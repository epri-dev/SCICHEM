INTEGER FUNCTION SWIMbuildObsList( t,grid,Obs,lAssm )

!------ Build linked-list of observations
!       N.B. Assumes that file is positioned to read first record at next obs time

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE SWIMobsInterp_fd
USE constants_fd

IMPLICIT NONE

REAL,             INTENT( IN    ) :: t
TYPE( MetGrid  ), INTENT( INOUT ) :: grid
TYPE( FirstObs ), INTENT( INOUT ) :: Obs
LOGICAL,          INTENT( IN    ) :: lAssm

INTEGER nrec, ios, irv, n1, n2, i, n, nObsID, nObsAvg, nGoodVel
REAL    timeObs, timeNext, timeBin, zmax
INTEGER nBadVel
REAL    x, y
LOGICAL lend, lUnique
LOGICAL lymd
INTEGER hour, min, sec, year, month, day
INTEGER nTemp, nPress, nHumid
REAL    tGap

TYPE ( messageT ) :: caution

CHARACTER(64) :: currentID

CHARACTER(PATH_MAXLENGTH) :: string

REAL(8), DIMENSION(:), ALLOCATABLE :: var8

TYPE( ObsMet ), POINTER :: CurrentObs
TYPE( ObsMet ), POINTER :: prevObs, nextObs

TYPE( MapCoord ) :: ObsCoord

TYPE( ObsInterp ) :: AvgObs

CHARACTER(64),  DIMENSION(:), ALLOCATABLE :: ObsID
INTEGER,        DIMENSION(:), ALLOCATABLE :: NumObs

INTERFACE

  SUBROUTINE SWIMclearObsList( Obs )
    USE SWIMobs_fd
    TYPE( ObsMet ), POINTER :: Obs
  END SUBROUTINE SWIMclearObsList

  SUBROUTINE SWIMclearGridList( First )
    USE SWIMobs_fd
    TYPE( FirstObsGridList ), POINTER :: First
  END SUBROUTINE SWIMclearGridList

  INTEGER FUNCTION SWIMnewObs( First,obs )
    USE SWIMobs_fd
    TYPE( FirstObs ),INTENT( INOUT ) :: First
    TYPE( ObsMet   ),POINTER         :: obs
  END FUNCTION SWIMnewObs

  INTEGER FUNCTION SWIMparseObsRecord( First,obs,var8,n1,n2 )
    USE SWIMobs_fd
    TYPE( FirstObs ),               INTENT( IN ) :: First
    TYPE( ObsMet   ),               POINTER      :: obs
    REAL(8), DIMENSION(First%nVar), INTENT( IN ) :: var8
    INTEGER,                        INTENT( IN ) :: n1, n2
  END FUNCTION SWIMparseObsRecord

  SUBROUTINE SWIMgetObsTerrain( grid,First,Obs )
    USE SWIMmetField_fd
    USE SWIMobs_fd
    TYPE( MetGrid ), INTENT( IN ) :: grid
    TYPE( FirstObs ),INTENT( IN ) :: First
    TYPE( ObsMet  ),      POINTER :: Obs
  END SUBROUTINE SWIMgetObsTerrain

  INTEGER FUNCTION SWIMconvertObsThermo( grid,Obs,CurrentObs )
    USE SWIMmetField_fd
    USE SWIMobs_fd
    TYPE( MetGrid  ), INTENT( IN ) :: grid
    TYPE( FirstObs ), INTENT( IN ) :: Obs
    TYPE( ObsMet ),   POINTER      :: CurrentObs
  END FUNCTION SWIMconvertObsThermo

  SUBROUTINE CheckObsFirstZlev( Obs )
    USE SWIMobs_fd
    TYPE( ObsMet  ), POINTER :: Obs
  END SUBROUTINE CheckObsFirstZlev

  SUBROUTINE PutObsInCell( First,CurrentObs )
    USE SWIMobs_fd
    TYPE( FirstObsGridList ), INTENT( INOUT ) :: First
    TYPE( ObsMet   ),         POINTER         :: CurrentObs
  END SUBROUTINE PutObsInCell

  SUBROUTINE SWIMnullifyObs( obs )
    USE SWIMobs_fd
    TYPE( ObsMet ), POINTER :: obs
  END SUBROUTINE SWIMnullifyObs

END INTERFACE

INTEGER, EXTERNAL :: SWIMreadVarObs, SWIMsetGridList
INTEGER, EXTERNAL :: SWIMnullifyInterpPrf, SWIMallocAvgObs
INTEGER, EXTERNAL :: SWIMbinAvgObs, SWIMgenAvgObs
INTEGER, EXTERNAL :: PostProgressMessage
INTEGER, EXTERNAL :: SWIMaddLogMessage
INTEGER, EXTERNAL :: SWIMcnvCoord, CheckLon
LOGICAL, EXTERNAL :: HasPrjReference, CheckInDomain
INTEGER, EXTERNAL :: PostCautionMessage

message%bString = 'reading observation meteorology file(s)'

irv = PostProgressMessage( message )

SWIMbuildObsList = SWIMfailure

!------ Check if list needs to be updated

IF( Obs%time /= NOT_SET_R .AND. Obs%time > t )THEN
  SWIMbuildObsList = SWIMresult
  GOTO 9999
END IF

CALL ReportFileName( string,'reading observation meteorology file ',Obs%Source )
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Setup obs coordinate structure (used to convert to met field coordinates, if needed)

IF( BTEST(Obs%type,OTB_LL) )THEN
  ObsCoord%type = I_LATLON

ELSE IF( Prj%coord == I_UTM )THEN  !XY assumed UTM if project is UTM
  ObsCoord%type = I_UTM
  ObsCoord%type = Prj%UTMzone

ELSE

  ObsCoord%type = I_CARTESIAN      !XY assumed Cartesian if project not UTM
  IF( HasPrjReference() )THEN
    ObsCoord%reference%lat = Prj%Lat0
    ObsCoord%reference%lon = Prj%Lon0
    ObsCoord%reference%x   = Prj%Xref
    ObsCoord%reference%y   = Prj%Yref
  ELSE
    ObsCoord%reference%lat = NOT_SET_R
    ObsCoord%reference%lon = NOT_SET_R
    ObsCoord%reference%x   = NOT_SET_R
    ObsCoord%reference%y   = NOT_SET_R
  END IF

END IF

!------ Allocate array for reading data

ALLOCATE( var8(Obs%nVar),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMbuildObsList'
  error%Message = 'Error allocating arrays for obs record'
  GOTO 9999
END IF

!------ Clear previous list; move list pointer

IF( ASSOCIATED(Obs%PrevObs)      )CALL SWIMclearObsList( Obs%PrevObs )
IF( ASSOCIATED(Obs%PrevGridList) )CALL SWIMclearGridList( Obs%PrevGridList )
NULLIFY( Obs%PrevGridList )

Obs%PrevNumObs    =  Obs%numObs
Obs%PrevNumObsDom =  Obs%numObsDom
Obs%PrevTime      =  Obs%time
Obs%PrevZmax      =  Obs%Zmax
Obs%PrevObs       => Obs%Obs
Obs%PrevGridList  => Obs%GridList

!------ Starting point for reading
!       N.B. Return to this point if there are no valid velocity obs
!       or no obs are within assimilation domain

nBadVel = 0

1000 CONTINUE !****************************************************************

NULLIFY( Obs%Obs ); Obs%numObs = 0
NULLIFY( Obs%GridList )

var8 = DBLE(NOT_SET_R)

!------ Read first record to define time and first station ID

IF( .NOT.Obs%lend )THEN
  irv = SWIMreadVarObs( Obs,var8,lend,nrec,CurrentID )
  IF( irv /= SWIMsuccess )GOTO 9999
ELSE
  lend = .TRUE.
END IF

IF( lend )THEN
  SWIMbuildObsList = SWIMresult
  Obs%time         = DEF_VAL_R  !A large positive number
  Obs%Zmax         = -999.
  GOTO 9999
END IF

IF( Obs%lAERMET .OR. Obs%lASOS1 )THEN
  var8(2) = Obs%lat
  var8(3) = Obs%lon
  CurrentID = TRIM(Obs%AERid)
END IF

CALL SWIMgetTimeObs( Obs,var8,timeObs,Obs%time )
IF( error%Number /= NO_ERROR )GOTO 9999

CALL c_format( Obs%time/3600.,n,message%cString )
message%cString = 't= '//message%cString(1:n)//'hrs'
irv = PostProgressMessage( message )

WRITE(string,'(A,ES12.4)')'Obs (run) time = ',Obs%time/3600.
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

CALL SWIMsetParse( Obs,n1,n2 )

!------ Associate first obs in list and set first data level

irv = SWIMnewObs( Obs,CurrentObs )
IF( irv /= SWIMsuccess )GOTO 9999

Obs%obs => CurrentObs

CurrentObs%id = TRIM(CurrentID)

irv = SWIMparseObsRecord( Obs,CurrentObs,var8,n1,n2 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Read obs records and add levels until time changes

timeNext = timeObs

DO

  irv = SWIMreadVarObs( Obs,var8,lend,nrec,CurrentID )
  IF( irv /= SWIMsuccess )GOTO 9999
  IF( lend )THEN
    Obs%lend = .TRUE.
    EXIT
  END IF
  IF( CurrentID == 'NOT SET' )THEN
    CurrentID = TRIM(CurrentObs%ID)
    n1 = Obs%nVarFixed+1; n2 = Obs%nVar
  ELSE
    CALL SWIMgetTimeObs( Obs,var8,timeNext,timeBin ) !Check current time
    IF( error%Number /= NO_ERROR )GOTO 9999
    IF( timeBin > Obs%time )THEN
      DO i = 1,nrec
        BACKSPACE(Obs%unit,IOSTAT=ios) !Position for building list at next time
      END DO
      EXIT
    END IF
  END IF

!------ Check that time does not decrease

  IF( timeNext < timeObs )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMbuildObsList'
    CALL ReportFileName( error%Message,'Non-sequential times on obs file ',Obs%Source )
    WRITE(error%Inform,'(A,F8.1,A,F8.1)')'New time (from start of run) = ',timeNext/3600., &
                                         ' Previous time = ',timeObs/3600.
    error%Action = 'Obs station='//TRIM(CurrentID)
    IF( Obs%lASOS1 )CALL ReportASOSerror( Obs)
    GOTO 9999
  END IF

!------ Check ID

  IF( TRIM(CurrentID) /= TRIM(CurrentObs%ID) .OR. timeNext > timeObs )THEN
    irv = SWIMnewObs( Obs,CurrentObs%nextObs )
    IF( irv /= SWIMsuccess )GOTO 9999
    CurrentObs%nextObs%prevObs => CurrentObs
    CurrentObs => CurrentObs%nextObs
    CurrentObs%id = TRIM(CurrentID)
    timeObs = timeNext
    IF( Obs%nVarFixed > 0 )CALL SWIMsetParse( Obs,n1,n2 )
  END IF

  irv = SWIMparseObsRecord( Obs,CurrentObs,var8,n1,n2 )
  IF( irv /= SWIMsuccess )GOTO 9999

END DO

!------ Extra processing (move down linked-list)

CurrentObs => Obs%obs

zmax     = 0.
nGoodVel = 0

DO WHILE( ASSOCIATED(CurrentObs) )

!------ Check lowest level

  CALL CheckObsFirstZlev( CurrentObs )

!------ Convert horizontal location to proper coordinates

  IF( grid%coord%type /= ObsCoord%type )THEN
    x = CurrentObs%x; y = CurrentObs%y
    irv = SWIMcnvCoord( x,y,ObsCoord,CurrentObs%x,CurrentObs%y,grid%coord )
    IF( irv /= SWIMsuccess )GOTO 9999
  ELSE IF( grid%coord%type == I_LATLON )THEN
    irv = CheckLon( CurrentObs%x,grid%xmin,grid%xmax )
    IF( irv /= SWIMsuccess )GOTO 9999
  END IF

!------ Check if assimilation obs are within field domain
!       If not, remove from linked list

  IF( lAssm )THEN
    IF( .NOT.CheckInDomain(CurrentObs%x,CurrentObs%y,grid) )THEN

      Obs%numObs = Obs%numObs - 1

      prevObs => CurrentObs%prevObs
      nextObs => CurrentObs%nextObs

      IF( ASSOCIATED(prevObs) )THEN

        IF( ASSOCIATED(nextObs) )THEN
          prevObs%nextObs => nextObs
          nextObs%prevObs => prevObs
        ELSE
          NULLIFY(prevObs%nextObs)
        END IF

      ELSE                             !If prevObs unassociated, this must be first in list

        IF( ASSOCIATED(nextObs) )THEN
          Obs%obs => CurrentObs%nextObs
          NULLIFY(nextObs%prevObs)
        ELSE
          NULLIFY(Obs%obs)
        END IF
      END IF

      CALL SWIMnullifyObs( CurrentObs )

      IF( Obs%numObs == 0 )THEN
        caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMbuilObsList'
        caution%aString = 'No valid velocity observations within assimilation domain'
        lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
        CALL TimeConvert( Obs%time,Prj%local,lymd,hour,min,sec,year,month,day,string )
        caution%bString = 'Obs Source: '//TRIM(Obs%Source)
        caution%cString = 'Observation time: '//string
        irv = PostCautionMessage( caution )
        GOTO 1000                      !No obs in domain; read next time
      ELSE
        CurrentObs => nextObs
        CYCLE                          !Otherwise, check next obs
      END IF

    END IF
  END IF

  IF( CurrentObs%Vel%nZ > 0 )nGoodVel = nGoodVel + 1

!------ Thermodynamic conversions, i.e. relative humidity from
!       absolute and conversion to potential temperature

  irv = SWIMconvertObsThermo( grid,Obs,CurrentObs )
  IF( irv /= SWIMsuccess )GOTO 9999

!------ Get max level (for interpolation weight in SWIMvertWt)

  IF( CurrentObs%Vel%nz > 0 ) &
    zmax = MAX(CurrentObs%Vel%z(CurrentObs%Vel%nz),zmax)

!------ Point to next obs

  CurrentObs => CurrentObs%nextObs

END DO

!------ Skip this time bin if there are no valid velocity observations

IF( nGoodVel == 0 )THEN
  lymd = .NOT.( Prj%julStart == 0 .OR. Prj%julStart == NOT_SET_I )
  CALL TimeConvert( Obs%time,Prj%local,lymd,hour,min,sec,year,month,day,string )
  IF( nBadVel == 0 )THEN
    caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMbuildObsList'
    caution%bString = TRIM(string)
    caution%cString = TRIM(string)
  ELSE
    caution%cString = TRIM(string)
  END IF
  nBadVel = nBadVel + 1
  GOTO 1000
ELSE IF( nBadVel > 0 )THEN   !Report period of missing velocity observations
  CALL  SplitName( Obs%Source,caution%aString,string )
  caution%aString = 'No valid velocity observations: '//TRIM(caution%aString)
  caution%bString = 'Starting '//TRIM(caution%bString)
  caution%cString = 'Through  '//TRIM(caution%cString)
  irv = PostCautionMessage( caution )
END IF

Obs%Zmax = zmax

!------ Average observations with the same id and time bin

IF( Obs%timeBin > 0. )THEN

  ALLOCATE( ObsID(Obs%numObs),NumObs(Obs%numObs),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMbuildObsList'
    error%Message = 'Error allocating Obs ID array'
    GOTO 9999
  END IF

  nObsID  = 0
  ObsId   = ' '
  NumObs  = 0
  nObsAvg = 0

  CurrentObs => Obs%obs

!------ Get number of unique stations; see if any need to be averaged

  DO WHILE( ASSOCIATED(CurrentObs) )

    lUnique = .TRUE.
    DO i = 1,nObsID
      IF( TRIM(CurrentObs%id) == TRIM(ObsID(i)) )THEN
        lUnique   = .FALSE.
        NumObs(i) = NumObs(i) + 1
        IF( NumObs(i) == 2 )nObsAvg = nObsAvg + 1
        EXIT
      END IF
    END DO

    IF( lUnique )THEN
      nObsID    = nObsID + 1
      NumObs(i) = 1
      ObsID(nObsId) = TRIM(CurrentObs%id)
    END IF

    CurrentObs => CurrentObs%nextObs

  END DO

!------ Average: loop over unique stations

  IF( nObsAvg > 0 )THEN

    irv = SWIMnullifyInterpPrf( AvgObs )
    IF( irv /= SWIMsuccess )GOTO 9999

    DO i = 1,nObsID
      IF( NumObs(i) > 1 )THEN

        irv = SWIMallocAvgObs( Obs,AvgObs,ObsID(i) )
        IF( irv /= SWIMsuccess )GOTO 9999

        CALL SWIMzeroAvgObs( Obs,AvgObs )

        irv = SWIMbinAvgObs( Obs,AvgObs,ObsID(i) )
        IF( irv /= SWIMsuccess )GOTO 9999

        irv = SWIMgenAvgObs( Obs,AvgObs,ObsID(i) )
        IF( irv /= SWIMsuccess )GOTO 9999

        IF( Obs%lASOS1 .AND. Obs%lASOSthermo )THEN  !Save no. of observations for thermo averages
          nTemp  = NINT(AvgObs%Tpot%wt(1))          !N.B. Only 1 obs station and 1 level in this case
          nPress = NINT(AvgObs%Press%wt(1))
          nHumid = NINT(AvgObs%Humid%wt(1))
        END IF

        CALL SWIMdeallocAvgObs( Obs,AvgObs )

      END IF
    END DO

  END IF

END IF

!------ Diagnostics for ASOS 1 minute data

IF( Obs%lASOS1 )THEN

  caution%iParm = 0; caution%jParm = 0; caution%routine = 'SWIMbuildObsList'
  IF( Obs%timeBin >= 120. )THEN  !Only if time bin is at least 2 minutes
    i = INT((Obs%timeBin+1.))/120.
    IF( NumObs(1) < i-1 )THEN
      CALL  SplitName( Obs%Source,caution%aString,string )
      caution%aString = 'Incomplete average of ASOS 1 minute data from file '//TRIM(caution%aString)
      CALL TimeConvert( Obs%Time,Prj%local,.TRUE.,hour,min,sec,year,month,day,string )
      caution%bString = 'Computed at '//TRIM(string)
      WRITE(string,"('Used',I3,' observations instead of nominal',I3)",IOSTAT=ios ) NumObs(1),i
      caution%cString = TRIM(string)
      irv = PostCautionMessage( caution )
    ELSE IF( Obs%lASOSthermo  )THEN  !Check no. of observations for thermo averages
      IF( nTemp < i-1 )THEN
        CALL  SplitName( Obs%AuxSource,caution%aString,string )
        caution%aString = 'Incomplete average of ASOS 1 minute thermodynamic data from file '//TRIM(caution%aString)
        CALL TimeConvert( Obs%Time,Prj%local,.TRUE.,hour,min,sec,year,month,day,string )
        caution%bString = 'Computed at '//TRIM(string)
        WRITE(string,"('Used',I3,' observations instead of nominal',I3)",IOSTAT=ios ) nTemp,i
        caution%cString = TRIM(string)
        irv = PostCautionMessage( caution )
      ELSE IF( nPress < i-1 )THEN
        CALL  SplitName( Obs%AuxSource,caution%aString,string )
        caution%aString = 'Incomplete average of ASOS 1 minute pressure data from file '//TRIM(caution%aString)
        CALL TimeConvert( Obs%Time,Prj%local,.TRUE.,hour,min,sec,year,month,day,string )
        caution%bString = 'Computed at '//TRIM(string)
        WRITE(string,"('Used',I3,' observations instead of nominal',I3)",IOSTAT=ios ) nPress,i
        caution%cString = TRIM(string)
        irv = PostCautionMessage( caution )
      END IF
    END IF
  END IF

  tGap = MAX(Obs%timeBin,900.)
  IF( Obs%PrevTime /= NOT_SET_R )THEN
    IF( Obs%time - Obs%PrevTime > 1.1*tGap )THEN
      CALL  SplitName( Obs%Source,caution%aString,string )
      caution%aString = 'Time gap in ASOS 1 minute data file '//TRIM(caution%aString)
      CALL TimeConvert( Obs%PrevTime,Prj%local,.TRUE.,hour,min,sec,year,month,day,string )
      caution%bString = 'Starting '//TRIM(string)
      CALL TimeConvert( Obs%Time,Prj%local,.TRUE.,hour,min,sec,year,month,day,string )
      caution%cString = 'Through  '//TRIM(string)
      irv = PostCautionMessage( caution )
    END IF
  END IF

END IF

!------ Set terrain height and landuse parameters; add to profile levels
!       N.B. This is done after time-bin averaging so vertical levels
!       used there are AGL

CurrentObs => Obs%obs

DO WHILE( ASSOCIATED(CurrentObs) )
  CALL SWIMgetObsTerrain( grid,Obs,CurrentObs )
  CurrentObs => CurrentObs%nextObs
END DO

!------ Check if worth building grid for nearest-obs searches

irv = SWIMsetGridList( Obs,field(1)%grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ If so, put in cell for nearest obs search

IF( ASSOCIATED(Obs%GridList) )THEN

  CurrentObs => Obs%obs

  DO WHILE( ASSOCIATED(CurrentObs) )
    CALL PutObsInCell( Obs%GridList,CurrentObs )
    CurrentObs => CurrentObs%nextObs
  END DO

END IF

SWIMbuildObsList = SWIMresult

9999 CONTINUE

IF( ALLOCATED(var8)   )DEALLOCATE( var8,  STAT=ios )
IF( ALLOCATED(ObsID)  )DEALLOCATE( ObsID, STAT=ios )
IF( ALLOCATED(NumObs) )DEALLOCATE( NumObs,STAT=ios )

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMnewObs( First,obs )

!----- Initialize a new obs derived-type and add to linked list

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ),INTENT( INOUT ) :: First
TYPE( ObsMet   ),POINTER         :: obs

INTEGER, PARAMETER :: maxz = 3

INTEGER ios

SWIMnewObs = SWIMfailure

!------ Allocate next obs in list

ALLOCATE( obs,STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'SWIMnewObs'
  error%Message = 'Error allocating space for new met obs'
  GOTO 9999
END IF

NULLIFY( obs%nextObs ) !For linked list
NULLIFY( obs%prevObs )

!------ Allocate velocity pointers

IF( BTEST(First%type,OTB_UV) .OR. BTEST(First%type,OTB_SPD) )THEN

  ALLOCATE( Obs%Vel%z(maxz),Obs%Vel%u(maxz),Obs%Vel%v(maxz),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for velocity profiles'
    GOTO 9999
  END IF

!------ Allocate LSV and/or Met Uncertainty pointers

  IF( BTEST(First%type,OTB_LSV) )THEN
    ALLOCATE( Obs%Vel%LSV%uu(maxz),Obs%Vel%LSV%vv(maxz),Obs%Vel%LSV%uv(maxz),STAT=ios )
    IF( ios == 0 .AND. BTEST(First%type,OTB_LSVL) )THEN
      ALLOCATE( Obs%Vel%LSV%sl(maxz),STAT=ios )
    ELSE
      NULLIFY( Obs%Vel%LSV%sl )
    END IF
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMnewObs'
      error%Message = 'Error allocating space for LSV profiles'
      GOTO 9999
    END IF
  ELSE
    NULLIFY( Obs%Vel%LSV%uu,Obs%Vel%LSV%vv,Obs%Vel%LSV%uv,Obs%Vel%LSV%sl )
  END IF

!------ Allocate Profile Boundar Layer pointers

  IF( BTEST(First%type,OTB_UU) )THEN
    ALLOCATE( Obs%Vel%BLprof%uu(maxz), &
              Obs%Vel%BLprof%vv(maxz), &
              Obs%Vel%BLprof%ww(maxz), &
              Obs%Vel%BLprof%wt(maxz), &
              Obs%Vel%BLprof%sl(maxz), &
              Obs%Vel%BLprof%sz(maxz), STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMnewObs'
      error%Message = 'Error allocating space for BL profile'
      GOTO 9999
    END IF

  ELSE
    NULLIFY( Obs%Vel%BLprof%uu,Obs%Vel%BLprof%vv,Obs%Vel%BLprof%ww, &
             Obs%Vel%BLprof%wt,Obs%Vel%BLprof%sl,Obs%Vel%BLprof%sz )
  END IF

ELSE

  NULLIFY( Obs%Vel%z,Obs%Vel%u,Obs%Vel%v )
  NULLIFY( Obs%Vel%LSV%uu,Obs%Vel%LSV%vv,Obs%Vel%LSV%uv,Obs%Vel%LSV%sl )
  NULLIFY( Obs%Vel%BLprof%uu,Obs%Vel%BLprof%vv,Obs%Vel%BLprof%ww, &
           Obs%Vel%BLprof%wt,Obs%Vel%BLprof%sl,Obs%Vel%BLprof%sz )

END IF

Obs%Vel%nz = 0

!------ Allocate temperature pointers

IF( BTEST(First%type,OTB_T) )THEN

  ALLOCATE( Obs%Tpot%z(maxz),STAT=ios )
  IF( ios == 0 )ALLOCATE( Obs%Tpot%obs(maxz),STAT=ios )

  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for met temperature obs'
    GOTO 9999
  END IF

ELSE

  NULLIFY( Obs%Tpot%z,Obs%Tpot%obs )

END IF

Obs%Tpot%nz = 0

IF( BTEST(First%type,OTB_T) .AND. BTEST(First%type,OTB_P) )THEN

  ALLOCATE( Obs%TpotStA%z(maxz),STAT=ios )
  IF( ios == 0 )ALLOCATE( Obs%TpotStA%obs(maxz),STAT=ios )

  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for met temperature obs (stnd atmos)'
    GOTO 9999
  END IF

  First%type = IBSET(First%type,OTB_TSTA)

ELSE

  NULLIFY( Obs%TpotStA%z,Obs%TpotStA%obs )

END IF

Obs%TpotStA%nz = 0

!------ Allocate humidity pointers

IF( BTEST(First%type,OTB_H) )THEN

  ALLOCATE( Obs%Humid%z(maxz),STAT=ios )
  IF( ios == 0 )ALLOCATE( Obs%Humid%obs(maxz),STAT=ios )

  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for met humidity obs'
    GOTO 9999
  END IF

ELSE

  NULLIFY( Obs%Humid%z,Obs%Humid%obs )

END IF

Obs%Humid%nz = 0

!------ Allocate pressure pointers

IF( BTEST(First%type,OTB_P) )THEN

  ALLOCATE( Obs%Press%z(maxz),STAT=ios )
  IF( ios == 0 )ALLOCATE( Obs%Press%obs(maxz),STAT=ios )

  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for met pressure obs'
    GOTO 9999
  END IF

ELSE

  NULLIFY( Obs%Press%z,Obs%Press%obs )

END IF

Obs%Press%nz = 0

!------ Allocate cloud liquid water pointers

IF( BTEST(First%type,OTB_QCLD) )THEN

  ALLOCATE( Obs%Qcloud%z(maxz),STAT=ios )
  IF( ios == 0 )ALLOCATE( Obs%Qcloud%obs(maxz),STAT=ios )

  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMnewObs'
    error%Message = 'Error allocating space for met cloud water obs'
    GOTO 9999
  END IF

ELSE

  NULLIFY( Obs%Qcloud%z,Obs%Qcloud%obs )

END IF

Obs%Qcloud%nz = 0

!------ Initialize thermo variables saved for extrapolation

Obs%Stnd%Ttop    = OBP_NODATA
Obs%Stnd%Tbot    = OBP_NODATA
Obs%Stnd%LogPtop = OBP_NODATA
Obs%Stnd%LogPbot = OBP_NODATA
Obs%Stnd%RHtop   = OBP_NODATA
Obs%Stnd%PHtop   = OBP_NODATA
Obs%Stnd%RHbot   = OBP_NODATA
Obs%Stnd%PHbot   = OBP_NODATA

!------ Initialize surface (BL) data

Obs%varSrf%zi         = OBP_NODATA
Obs%varSrf%hflux      = OBP_NODATA
Obs%varSrf%ustr       = OBP_NODATA
Obs%varSrf%invL       = OBP_NODATA
Obs%varSrf%prcp       = OBP_NODATA
Obs%varSrf%cloudcover = OBP_NODATA

Obs%BL%surface%zruf  = OBP_NODATA
Obs%BL%surface%hc    = OBP_NODATA
Obs%BL%surface%alpha = OBP_NODATA
Obs%BL%surface%adens = OBP_NODATA

Obs%x = OBP_NODATA
Obs%y = OBP_NODATA

NULLIFY( Obs%SrfPrf%z,Obs%SrfPrf%fsl )

!------ Initialize vertical extrapolation scales to very large (but specific) values

Obs%vscaleBot = -OBP_NODATA
Obs%vscaleTop = -OBP_NODATA

!------ Increment obs counter

First%numObs = First%numObs + 1

SWIMnewObs = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SWIMsetParse( Obs,n1,n2 )

USE SWIM_fi

IMPLICIT NONE

TYPE( FirstObs ),      INTENT( INOUT ) :: Obs
INTEGER,               INTENT( OUT   ) :: n1, n2

IF( Obs%lAERMET .OR. Obs%lASOS1 )THEN
  n1 = Obs%nVarFixed+1; n2 = Obs%nVar
ELSE
IF( Obs%nVarFixed > 0 )THEN
  n1 = 1; n2 = Obs%nVarFixed
ELSE
  IF( Obs%index%ID == 1 )THEN
    n1 = 2
  ELSE
    n1 = 1
  END IF
  n2 = Obs%nVar
END IF
END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMclearObsList( Obs )

USE SWIM_fi

IMPLICIT NONE

TYPE( ObsMet ), POINTER :: Obs

TYPE( ObsMet ), POINTER :: Next

INTERFACE
  SUBROUTINE SWIMnullifyObs( obs )
    USE SWIM_fi
    TYPE( ObsMet ), POINTER :: obs
  END SUBROUTINE SWIMnullifyObs
END INTERFACE

DO WHILE( ASSOCIATED(Obs) )
  Next => Obs%nextObs
  CALL SWIMnullifyObs( Obs )
  Obs => Next
END DO

RETURN
END

!==============================================================================

SUBROUTINE SWIMnullifyObs( obs )

USE SWIM_fi

IMPLICIT NONE

TYPE( ObsMet ), POINTER :: obs

INTEGER alloc_stat

IF( ASSOCIATED(obs%Vel%z)         )DEALLOCATE( obs%Vel%z,obs%Vel%u,obs%Vel%v,STAT=alloc_stat)
IF( ASSOCIATED(obs%Vel%LSV%uu)    )DEALLOCATE( obs%Vel%LSV%uu,obs%Vel%LSV%vv,obs%Vel%LSV%uv,STAT=alloc_stat )
IF( ASSOCIATED(obs%Vel%LSV%sl)    )DEALLOCATE( obs%Vel%LSV%sl,STAT=alloc_stat )
IF( ASSOCIATED(obs%Vel%BLprof%uu) )DEALLOCATE( obs%Vel%BLprof%uu,obs%Vel%BLprof%vv,obs%Vel%BLprof%ww, &
                                               obs%Vel%BLprof%sl,obs%Vel%BLprof%sz,obs%Vel%BLprof%wt,STAT=alloc_stat )

IF( ASSOCIATED(obs%Tpot%z)  )DEALLOCATE( obs%Tpot%z,obs%Tpot%obs,STAT=alloc_stat)
IF( ASSOCIATED(obs%Humid%z) )DEALLOCATE( obs%Humid%z,obs%Humid%obs,STAT=alloc_stat)
IF( ASSOCIATED(obs%Press%z) )DEALLOCATE( obs%Press%z,obs%Press%obs,STAT=alloc_stat)
IF( ASSOCIATED(obs%TpotStA%z) )DEALLOCATE( obs%TpotStA%z,obs%TpotStA%obs,STAT=alloc_stat)
IF( ASSOCIATED(obs%Qcloud%z) )DEALLOCATE( obs%Qcloud%z,obs%Qcloud%obs,STAT=alloc_stat)

IF( ASSOCIATED(obs%SrfPrf%z) )DEALLOCATE( obs%SrfPrf%z,obs%SrfPrf%fsl,STAT=alloc_stat )

obs%Vel%nz   = 0
obs%Tpot%nz  = 0
obs%Press%nz = 0
obs%Humid%nz = 0
obs%TpotStA%nz  = 0
obs%Qcloud%nz = 0

DEALLOCATE( obs,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE SWIMclearGridList( First )

USE SWIM_fi

IMPLICIT NONE

TYPE( FirstObsGridList ), POINTER :: First

INTEGER alloc_stat, i

TYPE( ObsGridList ), POINTER :: Obs, Next

IF( ASSOCIATED(First) )THEN
  IF( First%Nx*First%Ny > 0 )THEN
    DO i = 1,First%Nx*First%Ny
      IF( ASSOCIATED(First%GridList(i)%Obs) )THEN
        NULLIFY(First%GridList(i)%Obs)
        Obs => First%GridList(i)%Next
        DO WHILE( ASSOCIATED(Obs) )
          NULLIFY(Obs%obs)
          Next => Obs%Next
          DEALLOCATE( Obs,STAT=alloc_stat )
          Obs => Next
        END DO
      END IF
    END DO
    DEALLOCATE( First%GridList,STAT=alloc_stat )
    DEALLOCATE( First%NumObsCell,STAT=alloc_stat )
    First%Nx = 0
    First%Ny = 0
  END IF
END IF

DEALLOCATE( First,STAT=alloc_stat )

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMparseObsRecord( First,obs,var8,n1,n2 )

!----- Parse obs data record and build profiles

USE SWIM_fi
USE SWIMparam_fd
USE reallocate
USE constants_fd

IMPLICIT NONE

TYPE( FirstObs ),               INTENT( IN ) :: First
TYPE( ObsMet   ),               POINTER      :: obs
REAL(8), DIMENSION(First%nVar), INTENT( IN ) :: var8
INTEGER,                        INTENT( IN ) :: n1, n2

REAL,    PARAMETER :: DZLIM   = 500.
INTEGER, PARAMETER :: MAX_INC = 10

INTEGER irv, i, i2, k, ku
REAL    zObs, uObs, vObs, tObs, hObs, ufac, Tmax
REAL    qcObs
REAL    uul, vvl, uvl, shl
REAL    uue, vve, uve, sle

INTERFACE

  INTEGER FUNCTION ReallocateObsVel( First,ObsVel,inc )
    USE SWIM_fi
    TYPE( FirstObs  ), INTENT( IN )    :: First
    TYPE( ObsVelPrf ), INTENT( INOUT ) :: ObsVel
    INTEGER,           INTENT( IN    ) :: inc
  END FUNCTION ReallocateObsVel

  INTEGER FUNCTION CheckObsHeight( ID,z,k,zObs )
    CHARACTER(*),  INTENT( IN ) :: ID
    INTEGER,       INTENT( IN ) :: k
    REAL,          INTENT( IN ) :: zObs
    REAL, DIMENSION(:), POINTER :: z
  END FUNCTION CheckObsHeight

  INTEGER FUNCTION CheckObsPressure( ID,k,zObs,pObs )
    CHARACTER(*),  INTENT( IN ) :: ID
    INTEGER,       INTENT( IN ) :: k
    REAL, DIMENSION(:), POINTER :: zObs, pObs
  END FUNCTION CheckObsPressure

END INTERFACE

REAL,  EXTERNAL :: SWIMPGTtoInvL, InvLfromL

SWIMparseObsRecord = SWIMfailure
error%Number  = IV_ERROR
error%Routine = 'SWIMparseObsRecord'
error%Action  = 'Value = '

!------ Set location for AERMOD met

IF( First%lAERMET .OR. First%lASOS1)THEN
  Obs%x = First%Lon
  Obs%y = First%Lat
END IF

!------ Set height

IF( n1 > First%nVarFixed )THEN
  IF( BTEST(First%type,OTB_Z) )THEN
    IF( var8(First%index%z) == OBP_BADDATA .OR. &
        var8(First%index%z) < 0. )THEN
      SWIMparseObsRecord = SWIMresult
      CALL SWIMclearError()
      GOTO 9999
    END IF
    zObs = SNGL(var8(First%index%z))*First%Conv(First%index%z)
  ELSE
    zObs = First%zref
  END IF
END IF

!------ Setup for LSV & Uncertainty

ku   = Obs%Vel%nz
uObs = -999.
uul  = -999.
vvl  = -999.
uvl  = -999.
shl  = -999.
uue  = -999.
vve  = -999.
uve  = -999.
sle  = -999.

!------ Loop over data and add to obs structure

LoopOverVar : DO i = n1,n2

  IF( var8(i) == OBP_BADDATA )CYCLE LoopOverVar !Ignore missing/bad data

  SELECT CASE( First%VarID(i) )

    CASE( OVP_X,OVP_LON )

      Obs%x = SNGL(var8(i)*DBLE(First%Conv(i)))

    CASE( OVP_Y,OVP_LAT )

      Obs%y = SNGL(var8(i)*DBLE(First%Conv(i)))

    CASE( OVP_RIFL )

      IF( var8(i) <= 0.D0 )THEN
        error%Message = 'Invalid influence radius'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF
      Obs%a2 = 1./(SNGL(var8(i)*DBLE(First%Conv(i))))**2

    CASE( OVP_U,OVP_DIR )

      i2 = NINT(First%Conv(i))
      IF( var8(i2) == OBP_BADDATA )CYCLE LoopOverVar
      ufac = First%Conv(i2)
      IF( First%VarID(i) == OVP_U )THEN
        IF( zObs <= ZMAX_CHECK )THEN
          IF( (var8(i)*ufac)**2 + (var8(i2)*ufac)**2 > MAXLIM_VEL2 )THEN
            error%Message = 'Invalid wind speed'
            error%Inform  = 'Maximum speed is '; ku = LEN_TRIM(error%Inform) + 1
            CALL c_format( MAXLIM_VEL,k,error%Inform(ku+1:) )
            error%Inform = TRIM(error%Inform)//' m/s'
            error%Action  = 'U,V = '; ku = LEN_TRIM(error%Action) + 1
            CALL c_format( SNGL(var8(i))*ufac,k,error%Action(ku+1:) )
            error%Action(ku+k:ku+k) = ','; ku = LEN_TRIM(error%Action)
            CALL c_format( SNGL(var8(i2))*ufac,k,error%Action(ku+1:) )
            GOTO 9999
          END IF
        END IF
        uObs = SNGL(var8(i ))*ufac
        vObs = SNGL(var8(i2))*ufac
      ELSE
        IF( ABS(var8(i)) > 360.D00 )THEN
          error%Message = 'Invalid wind direction'
          error%Inform  = 'Valid range is -360 to +360'
          CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
          GOTO 9999
        END IF
        IF( zObs <= ZMAX_CHECK )THEN
          IF( ABS(var8(i2))*ufac > MAXLIM_VEL )THEN
            error%Message = 'Invalid wind speed'
            error%Inform  = 'Maximum is '; ku = LEN_TRIM(error%Inform) + 1
            CALL c_format( MAXLIM_VEL,k,error%Inform(ku+1:) )
            error%Inform = TRIM(error%Inform)//' m/s'
            CALL c_format( SNGL(var8(i2)),k,error%Action(9:) )
            GOTO 9999
          END IF
        END IF
        uObs = -SNGL(var8(i2))*ufac * SIN( SNGL(var8(i))*PI180 )
        vObs = -SNGL(var8(i2))*ufac * COS( SNGL(var8(i))*PI180 )
      END IF

      k = Obs%Vel%nz + 1; Obs%Vel%nz = k
      irv = CheckObsHeight( Obs%id,Obs%Vel%z,k,zObs )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( k > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF

      Obs%Vel%z(k) = zObs
      Obs%Vel%u(k) = uObs; Obs%Vel%v(k) = vObs

    CASE( OVP_UL )

      IF( var8(i) < 0.D0 )THEN
        error%Message = 'Invalid large-scale variability'
        error%Inform  = 'UL or UUL cannot be negative'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

      uul = SNGL(var8(i))*First%Conv(i)
      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF

    CASE( OVP_VL )

      IF( var8(i) < 0. )THEN
        error%Message = 'Invalid large-scale variability'
        error%Inform  = 'VL or VVL cannot be negative'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

      vvl = SNGL(var8(i))*First%Conv(i)
      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF

    CASE( OVP_UVL )

      uvl = SNGL(var8(i))*First%Conv(i)
      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF

    CASE( OVP_SHL )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      IF( var8(i) > 0.D0 )THEN
        shl = SNGL(var8(i))*First%Conv(i)
      ELSE
        error%Message = 'Invalid large-scale variability lengthscale'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_T )

      Tmax = MAXLIM_TEMP
      SELECT CASE( NINT(First%Conv(i)) )
        CASE( OBP_CELSIUS )
          tObs = SNGL(var8(i)) - ABSZERO
        CASE( OBP_KELVIN )
          tObs = SNGL(var8(i))
        CASE( OBP_POTENTIAL )
          tObs = SNGL(var8(i)) * (PSURF/1000.)**KAPPA !Convert to SWIM definition
          Tmax = MAXLIM_TPOT
        CASE( OBP_FAHRENHEIT )
          tObs = (SNGL(var8(i))-32.)*0.555555 - ABSZERO
      END SELECT

      IF( zObs <= ZMAX_CHECK )THEN
        IF( tObs < MINLIM_TEMP .OR. tObs > Tmax )THEN
          error%Message = 'Invalid temperature'
          error%Inform  = 'Valid range is '; ku = LEN_TRIM(error%Inform) + 1
          CALL c_format( MINLIM_TEMP,k,error%Inform(ku+1:) )
          error%Inform = TRIM(error%Inform)//' to '; ku = LEN_TRIM(error%Inform) + 1
          CALL c_format( Tmax,k,error%Inform(ku+1:) )
          error%Inform = TRIM(error%Inform)//' K'
          CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
          CALL c_format( tObs,k,error%Action(9:) )
          GOTO 9999
        END IF
      END IF

      k = Obs%Tpot%nz + 1; Obs%Tpot%nz = k
      irv = CheckObsHeight( Obs%id,Obs%Tpot%z,k,zObs )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( k > SIZE(Obs%Tpot%z) )THEN
        irv = reallocate_real1d( Obs%Tpot%z,MAX_INC ) !increment space
        IF( irv == 0 )irv = reallocate_real1d( Obs%Tpot%obs,MAX_INC )
        IF( irv /= 0 )GOTO 9998
      END IF

      Obs%Tpot%z(k)   = zObs
      Obs%Tpot%obs(k) = tObs

      IF( ASSOCIATED(Obs%TpotStA%z) )THEN

        Obs%TpotStA%nz = k

        IF( k > SIZE(Obs%TpotStA%z) )THEN
          irv = reallocate_real1d( Obs%TpotStA%z,MAX_INC ) !increment space
          IF( irv == 0 )irv = reallocate_real1d( Obs%TpotStA%obs,MAX_INC )
          IF( irv /= 0 )GOTO 9998
        END IF

        Obs%TpotStA%z(k)   = zObs
        Obs%TpotStA%obs(k) = tObs

      END IF

    CASE( OVP_H )

      IF( First%Conv(i) < 0. )THEN
        IF( var8(i) < 0.D00 .OR. var8(i) > 1.001D02  )THEN
          error%Message = 'Invalid relative humidity'
          error%Inform  = 'Valid range is 0 to 100'
          CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
          GOTO 9999
        END IF
        hObs = SNGL(var8(i))
      ELSE
        hObs = SNGL(var8(i))*First%Conv(i)
      END IF

      k = Obs%Humid%nz + 1; Obs%Humid%nz = k
      irv = CheckObsHeight( Obs%id,Obs%Humid%z,k,zObs )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( k > SIZE(Obs%Humid%z) )THEN
        irv = reallocate_real1d( Obs%Humid%z,MAX_INC ) !increment space
        IF( irv == 0 )irv = reallocate_real1d( Obs%Humid%obs,MAX_INC )
        IF( irv /= 0 )GOTO 9998
      END IF

      Obs%Humid%z(k)   = zObs
      Obs%Humid%obs(k) = hObs

    CASE( OVP_P )

      k = Obs%Press%nz + 1; Obs%Press%nz = k
      irv = CheckObsHeight( Obs%id,Obs%Press%z,k,zObs )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( var8(i) <= 0. .OR. var8(i) > 1.5D03  )THEN
        error%Message = 'Invalid pressure'
        error%Inform  = 'Valid range is 0.1 to 1200 mb'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

      IF( k > SIZE(Obs%Press%z) )THEN
        irv = reallocate_real1d( Obs%Press%z,MAX_INC ) !increment space
        IF( irv == 0 )irv = reallocate_real1d( Obs%Press%obs,MAX_INC )
        IF( irv /= 0 )GOTO 9998
      END IF

      Obs%Press%z(k)   = zObs
      Obs%Press%obs(k) = LOG(First%Conv(i)*SNGL(var8(i))/PSURF)

      irv = CheckObsPressure( Obs%id,k,Obs%Press%z,Obs%Press%obs )
      IF( irv /= SWIMsuccess )GOTO 9999

    CASE( OVP_QCLD )

      IF( First%Conv(i) < 0. )THEN
        qcObs = SNGL(var8(i))
      ELSE
        qcObs = SNGL(var8(i))*First%Conv(i)
      END IF

      k = Obs%Qcloud%nz + 1; Obs%Qcloud%nz = k
      irv = CheckObsHeight( Obs%id,Obs%Qcloud%z,k,zObs )
      IF( irv /= SWIMsuccess )GOTO 9999

      IF( k > SIZE(Obs%Qcloud%z) )THEN
        irv = reallocate_real1d( Obs%Qcloud%z,MAX_INC ) !increment space
        IF( irv == 0 )irv = reallocate_real1d( Obs%Qcloud%obs,MAX_INC )
        IF( irv /= 0 )GOTO 9998
      END IF

      Obs%Qcloud%z(k)   = zObs
      Obs%Qcloud%obs(k) = qcObs

    CASE( OVP_UU )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      Obs%Vel%BLprof%uu(ku+1) = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_VV )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      Obs%Vel%BLprof%vv(ku+1) = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_WW )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      Obs%Vel%BLprof%ww(ku+1) = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_WT )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      Obs%Vel%BLprof%wt(ku+1) = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_SL )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      IF( var8(i) > 0. )THEN
        Obs%Vel%BLprof%sl(ku+1) = SNGL(var8(i))*First%Conv(i)
      ELSE
        error%Message = 'Invalid BL buoyancy lengthscale'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_SZ )

      IF( ku+1 > SIZE(Obs%Vel%z) )THEN
        irv = ReallocateObsVel( First,Obs%Vel,MAX_INC )
        IF( irv /= SWIMsuccess )GOTO 9998
      END IF
      IF( var8(i) > 0. )THEN
        Obs%Vel%BLprof%sz(ku+1) = SNGL(var8(i))*First%Conv(i)
      ELSE
        error%Message = 'Invalid BL shear lengthscale'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_ZI )

      IF( First%lAERMET )THEN
        Obs%varSrf%zi = var8(i)*First%Conv(i)
        IF( Obs%varSrf%zi <= 0. )THEN
          IF( var8(i+1) == OBP_BADDATA )THEN
            Obs%varSrf%zi = OBP_BADDATA
            CYCLE LoopOverVar
          ELSE
            Obs%varSrf%zi = var8(i+1)*First%Conv(i)
          END IF
        ELSE IF( var8(i+1) /= OBP_BADDATA )THEN
          Obs%varSrf%zi = MAX(Obs%varSrf%zi,var8(i+1)*First%Conv(i))
        END IF
      ELSE
        Obs%varSrf%zi = var8(i)*First%Conv(i)
      END IF
      IF( Obs%varSrf%zi <= 0. )THEN
        error%Message = 'Invalid boundary layer depth'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( Obs%varSrf%zi,k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_HFLX )

      Obs%varSrf%hflux = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_UST )

      Obs%varSrf%ustr = SNGL(var8(i))*First%Conv(i)
      IF( var8(i) <= 0. )THEN
        error%Message = 'Invalid friction velocity'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_PGT )

      IF( var8(i) < 1.D0 .OR. var8(i) > 7.D0 )THEN
        error%Message = 'Invalid PGT category'
        error%Inform  = 'Valid range is 1 to 7'
        CALL c_format( SNGL(var8(i)),i2,error%Action(9:) )
        GOTO 9999
      END IF
      Obs%varSrf%invL = SWIMPGTtoInvL( var8(i) )

    CASE( OVP_MOL )

      IF( var8(i) == 0.D0 )THEN
        error%Message = 'Invalid Monin-Obukhov length'
        error%Inform  = 'Cannot be zero'
        GOTO 9999
      END IF

      Obs%varSrf%invL = InvLfromL( SNGL(var8(i)) )

    CASE( OVP_ZRUF )

      Obs%BL%surface%zruf = SNGL(var8(i))*First%Conv(i)
      IF( var8(i) <= 0. )THEN
        error%Message = 'Invalid surface roughness'
        error%Inform  = 'Must be greater than zero'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

    CASE( OVP_HCNP )

      Obs%BL%surface%hc = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_ACNP )

      Obs%BL%surface%adens = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_ALPH )

      Obs%BL%surface%alpha = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_PRCP )

      IF( BTEST(First%type,OTB_PRCP) )THEN
      IF( var8(i) < 0.D0 .OR. var8(i) > 6.D0 )THEN
        error%Message = 'Invalid precipitation category'
        error%Inform  = 'Valide range is 0 to 6'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF
      END IF
      Obs%varSrf%prcp = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_CC )

      IF( SNGL(var8(i))*First%Conv(i) < 0. .OR. SNGL(var8(i))*First%Conv(i) > 1. )THEN
        error%Message = 'Invalid cloud cover'
        error%Inform  = 'Range is 0 to 1 or 100%'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

      Obs%varSrf%cloudcover = SNGL(var8(i))*First%Conv(i)

    CASE( OVP_LAI )

      IF( var8(i) < 0.D0 )THEN
        error%Message = 'Invalid leaf area index'
        error%Inform  = 'Cannot be negative'
        CALL c_format( SNGL(var8(i)),k,error%Action(9:) )
        GOTO 9999
      END IF

      Obs%varSrf%LAI = SNGL(var8(i))*First%Conv(i)

  END SELECT

END DO LoopOverVar

error%Action = ' '

!------ Check that location has been set

IF( obs%x == OBP_NODATA .OR. obs%y == OBP_NODATA )THEN
  error%Message = 'Invalid observation location'
  error%Inform  = 'Obs ID='//TRIM(obs%ID)
  CALL ReportFileName( error%Action,'File=',First%Source )
  GOTO 9999
END IF

!------ Check LSV & Uncertainty

IF( uObs /= -999. )THEN

  IF( BTEST(First%type,OTB_LSV) )THEN
    IF( uul == -999. .OR. vvl == -999. .OR. uvl == -999. )THEN
      error%Message = 'Missing large-scale variance data'
      CALL ReportFileName( error%Inform,'File=',First%source )
      GOTO 9999
    ELSE IF( uul < 0. .OR. vvl < 0. )THEN !Std. deviation & correlation coeff.
      Obs%Vel%LSV%uv(ku+1) = uvl*uul*vvl
      Obs%Vel%LSV%uu(ku+1) = uul**2
      Obs%Vel%LSV%vv(ku+1) = vvl**2
    ELSE                                  !Variances
      Obs%Vel%LSV%uv(ku+1) = uvl
      Obs%Vel%LSV%uu(ku+1) = uul
      Obs%Vel%LSV%vv(ku+1) = vvl
    END IF
    IF( Obs%Vel%LSV%uv(ku+1)**2 > Obs%Vel%LSV%uu(ku+1)*Obs%Vel%LSV%vv(ku+1) )THEN
      error%Message = 'Invalid meteorology large-scale velocity correlation'
      CALL ReportFileName( error%Inform,'File=',First%source )
      GOTO 9999
    END IF
  END IF
  IF( BTEST(First%type,OTB_LSVL) )THEN
    IF( shl == -999. )THEN
      error%Message = 'Missing large-scale lengthscale'
      CALL ReportFileName( error%Inform,'File=',First%source )
      GOTO 9999
    ELSE
      Obs%Vel%LSV%sl(ku+1) = shl
    END IF
  END IF

END IF

SWIMparseObsRecord = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

IF( SWIMparseObsRecord == SWIMfailure .AND. First%lASOS1 )CALL ReportASOSerror( First )

RETURN

9998 CONTINUE

error%Number  = UK_ERROR
error%Message = 'Error re-allocating met obs profiles'
CALL ReportFileName( error%Inform,'File= ',First%source )
error%Action  = ' '
GOTO 9999

END

!==============================================================================

INTEGER FUNCTION CheckObsHeight( ID,z,k,zObs ) RESULT( irv )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

CHARACTER(*),  INTENT( IN ) :: ID
INTEGER,       INTENT( IN ) :: k
REAL,          INTENT( IN ) :: zObs
REAL, DIMENSION(:), POINTER :: z

irv = SWIMresult

IF( k > 1 )THEN
  IF( zObs <= z(k-1) )THEN
    irv = SWIMfailure
    error%Number = IV_ERROR
    error%Routine = 'CheckObsHeight'
    error%Message = 'Observation heights must increase monotonically'
    WRITE(error%Inform,'(A,2ES12.4)') 'Heights=',z(k-1),zObs
    WRITE(error%Action,'(A)') 'Station='//TRIM(ID)
  END IF
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION CheckObsPressure( ID,k,zObs,pObs ) RESULT( irv )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

IMPLICIT NONE

CHARACTER(*),  INTENT( IN ) :: ID
INTEGER,       INTENT( IN ) :: k
REAL, DIMENSION(:), POINTER :: zObs, pObs

REAL ptem, ptmm

irv = SWIMresult

IF( k > 1 )THEN
  IF( zObs(k) > zObs(k-1) + 1. )THEN
    IF( pObs(k) > pObs(k-1) )THEN
      ptmm = EXP(pObs(k-1))*PSURF
      ptem = EXP(pObs(k))*PSURF
      irv = SWIMfailure
      error%Number = IV_ERROR
      error%Routine = 'CheckObsPressure'
      WRITE(error%Message,'(A)') 'Station='//TRIM(ID)//': Pressure cannot increase with height'
      WRITE(error%Inform,'(A,2ES12.4,A)') 'Height/Pressure =',zObs(k-1),ptmm,' mb'
      WRITE(error%Action,'(A,2ES12.4,A)') 'Height/Pressure =',zObs(k),ptem,' mb'
    END IF
  END IF
END IF

RETURN
END

!===============================================================================

SUBROUTINE CheckObsFirstZlev( Obs )

!------ Check first obs levels
!       N.B. Assumes all negative levels have been removed

USE SWIM_fi

IMPLICIT NONE

TYPE( ObsMet ), POINTER :: obs

INTERFACE
  SUBROUTINE CheckFirstZlev( z,nz )
    REAL, DIMENSION(:), POINTER      :: z
    INTEGER,            INTENT( IN ) :: nz
  END SUBROUTINE CheckFirstZlev
END INTERFACE

IF( Obs%Vel%nz   > 0 )CALL CheckFirstZlev( Obs%Vel%z,  Obs%Vel%nz   )
IF( Obs%Tpot%nz  > 0 )CALL CheckFirstZlev( Obs%Tpot%z, Obs%Tpot%nz  )
IF( Obs%Press%nz > 0 )CALL CheckFirstZlev( Obs%Press%z,Obs%Press%nz )
IF( Obs%Humid%nz > 0 )CALL CheckFirstZlev( Obs%Humid%z,Obs%Humid%nz )
IF( Obs%TpotStA%nz  > 0 )CALL CheckFirstZlev( Obs%TpotStA%z,Obs%TpotStA%nz  )

RETURN
END

!===============================================================================

SUBROUTINE CheckFirstZlev( z,nz )

!------ Reset first level if zero

IMPLICIT NONE

REAL, DIMENSION(:), POINTER      :: z
INTEGER,            INTENT( IN ) :: nz

IF( z(1) <= 0. )THEN
  IF( nz > 1 )THEN
    z(1) =  MIN(10.,0.5*z(2))    !N.B. z is assumed AGL here
  ELSE
    z(1) = 10.
  END IF
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMconvertObsThermo( grid,Obs,CurrentObs )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer
USE SWIMpuff_fd
USE constants_fd

IMPLICIT NONE

TYPE( MetGrid  ), INTENT( IN ) :: grid
TYPE( FirstObs ), INTENT( IN ) :: Obs
TYPE( ObsMet ),   POINTER      :: CurrentObs

INTEGER ios, k, nz
REAL    dum1, dum2, hsx, hmin, zp, pres

REAL, DIMENSION(:), POINTER :: pfac, temp

TYPE( PuffMetRequest ) :: Request
TYPE( met1dh )         :: mx, mxu, my, myv
TYPE( meth   )         :: mxy

INTERFACE
  SUBROUTINE InterpExtrapObs( zIn,nzIn,ObsIn,zOut,nzOut,obsOut,hmin,StndFunc )
    REAL, DIMENSION(:), POINTER :: zIn,  ObsIn
    REAL, DIMENSION(:), POINTER :: zOut, ObsOut
    INTEGER,       INTENT( IN ) :: nzIn, nzOut
    REAL,          INTENT( IN ) :: hmin
    REAL,          EXTERNAL     :: StndFunc
  END SUBROUTINE InterpExtrapObs
END INTERFACE

REAL, EXTERNAL :: StndLogP, StndTpot, StndTemp
REAL, EXTERNAL :: SWIMrlimit
REAL, EXTERNAL :: fun_rhoa

SWIMconvertObsThermo = SWIMfailure

!----- Get terrain elevation for standard atmosphere calls
!      N.B. Obs levels are relative to local terrain at this point

IF( BTEST(grid%type,GTB_TERRAIN) )THEN

  Request%X = CurrentObs%x
  Request%Y = CurrentObs%y

  CALL SetXYfac( Request,grid,mx,my,mxu,myv )
  CALL SetMXY( mx,my,mxy,grid%nX,grid%nXY,.TRUE. )
  CALL IntXY( mxy,grid%terrain%H,hmin )

ELSE

  hmin = 0.

END IF

hmin = hmin + grid%Hmin

!------ Convert mixing ratio to relative humidity; also save pressure at top level

IF( BTEST(Obs%type,OTB_H) )THEN

  nz = CurrentObs%Humid%nz

  IF( nz > 0 )THEN

    ALLOCATE( pfac(CurrentObs%Humid%nz),temp(CurrentObs%Humid%nz),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMconvertObsThermo'
      error%Message = 'Error allocating arrays to convert humidity'
      GOTO 9999
    END IF

!------ Set temperature and pressure values for standard atmosphere
!       (if required to fill in for non-existent obs)

    IF( .NOT.BTEST(Obs%type,OTB_P) .OR.  CurrentObs%Press%nz == 0 .OR. &
        .NOT.BTEST(Obs%type,OTB_T) .OR.  CurrentObs%Tpot%nz  == 0 )THEN
      DO k = 1,CurrentObs%Humid%nz
        zp = CurrentObs%Humid%z(k)+hmin
        CALL stnd_atmos( zp,pfac(k),temp(k),dum2,SA_TA )
      END DO
    END IF

!------ Interpolate/extrapolate pressure onto humidity levels

    IF( BTEST(Obs%type,OTB_P) .AND.  CurrentObs%Press%nz > 0 )THEN

      CALL InterpExtrapObs( CurrentObs%Press%z,CurrentObs%Press%nz,CurrentObs%Press%obs, &
                            CurrentObs%Humid%z,CurrentObs%Humid%nz,pfac,hmin,StndLogP )
      pfac = EXP(pfac)

    END IF

!------ Convert to relative humidity if needed

    IF( BTEST(Obs%type,OTB_HCNV) )THEN

!------ Interpolate/extrapolate temperature onto humidity levels

      IF( BTEST(Obs%type,OTB_T) .AND.  CurrentObs%Tpot%nz > 0 )THEN

        IF( BTEST(Obs%type,OTB_TCNV) )THEN
          CALL InterpExtrapObs( CurrentObs%Tpot%z,CurrentObs%Tpot%nz,CurrentObs%Tpot%obs, &
                                CurrentObs%Humid%z,CurrentObs%Humid%nz,temp,hmin,StndTemp )
        ELSE
          CALL InterpExtrapObs( CurrentObs%Tpot%z,CurrentObs%Tpot%nz,CurrentObs%Tpot%obs, &
                                CurrentObs%Humid%z,CurrentObs%Humid%nz,temp,hmin,StndTpot )
          temp = temp*pfac**KAPPA
        END IF

      END IF

!------ Get relative humidity (in percent)

      DO k = 1,nz
        pres = PSURF*pfac(k)
        CALL RelHumid( CurrentObs%Humid%obs(k),temp(k),pres,hsx )
        CurrentObs%Humid%obs(k) = hsx
      END DO

    END IF

!------ Save top & bottom RH and pressure for possible extrapolation

    CurrentObs%Stnd%RHbot = CurrentObs%Humid%obs(1)
    CurrentObs%Stnd%RHtop = CurrentObs%Humid%obs(nz)

    CurrentObs%Stnd%PHbot = PSURF*pfac(1)
    CurrentObs%Stnd%PHtop = PSURF*pfac(nz)

    IF( CurrentObs%Stnd%PHtop < 200. )THEN
      CurrentObs%Stnd%extrapRHslope = 0.
    ELSE
      CurrentObs%Stnd%extrapRHslope = (RHSTRAT-CurrentObs%Stnd%RHtop)/(PSTRAT-CurrentObs%Stnd%PHtop)
    END IF

    DEALLOCATE( pfac,temp,STAT=ios )
    NULLIFY( pfac,temp )

  END IF

END IF

!------ Convert actual to potential temperature

IF( BTEST(Obs%type,OTB_TCNV) )THEN

  nz = CurrentObs%Tpot%nz

  IF( nz > 0 )THEN

!------ Interpolate/extrapolate pressure onto temperature levels and set pressure ratio

    ALLOCATE( pfac(nz),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMconvertObsThermo'
      error%Message = 'Error allocating pressure array to convert to potential temperature'
      GOTO 9999
    END IF

    IF( BTEST(Obs%type,OTB_P) .AND.  CurrentObs%Press%nz > 0 )THEN

      CALL InterpExtrapObs( CurrentObs%Press%z,CurrentObs%Press%nz,CurrentObs%Press%obs, &
                            CurrentObs%Tpot%z,CurrentObs%Tpot%nz,pfac,hmin,StndLogP )
      pfac = EXP(pfac)

    ELSE

      DO k = 1,nz
        zp = CurrentObs%Tpot%z(k)+hmin
        CALL stnd_atmos( zp,pfac(k),dum1,dum2,SA_TA )
      END DO

    END IF

!------ Convert

    DO k = 1,nz
      CurrentObs%Tpot%obs(k) = CurrentObs%Tpot%obs(k)/pfac(k)**KAPPA
    END DO

    IF( BTEST(Obs%type,OTB_TSTA) )THEN
      IF( CurrentObs%Press%nz > 0 )THEN
        DO k = 1,nz
          zp = CurrentObs%Tpot%z(k)+hmin
          CALL stnd_atmos( zp,pres,dum1,dum2,SA_TA )
          CurrentObs%TpotSta%obs(k) = CurrentObs%TpotSta%obs(k)/pres**KAPPA
        END DO
      ELSE
        DO k = 1,nz
          CurrentObs%TpotSta%obs(k) = CurrentObs%Tpot%obs(k)
        END DO
      END IF
    END IF

    DEALLOCATE( pfac,STAT=ios )
    NULLIFY( pfac )

  END IF

ELSE IF( BTEST(Obs%type,OTB_TSTA) .AND. CurrentObs%TpotStA%nz > 0  .AND. CurrentObs%Press%nz > 0 )THEN

  nz = CurrentObs%TpotStA%nz

!------ Interpolate/extrapolate pressure onto temperature levels and set pressure ratio

  ALLOCATE( pfac(nz),STAT=ios )
  IF( ios /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMconvertObsThermo'
    error%Message = 'Error allocating pressure array to convert to potential temperature'
    GOTO 9999
  END IF

  CALL InterpExtrapObs( CurrentObs%Press%z,CurrentObs%Press%nz,CurrentObs%Press%obs, &
                        CurrentObs%TpotStA%z,CurrentObs%Tpot%nz,pfac,hmin,StndLogP )
  pfac = EXP(pfac)

!------ Convert

  DO k = 1,nz
    zp = CurrentObs%Tpot%z(k)+hmin
    CALL stnd_atmos( zp,pres,dum1,dum2,SA_TA )
    pfac(k) = pfac(k)/pres
    CurrentObs%TpotSta%obs(k) = CurrentObs%TpotSta%obs(k)*pfac(k)**KAPPA
  END DO

  DEALLOCATE( pfac,STAT=ios )
  NULLIFY( pfac )

END IF

!------ Convert cloud water mixing ratio to g/m^3 if necessary

IF( BTEST(Obs%type,OTB_QCNV) )THEN

  nz = CurrentObs%Qcloud%nz

  IF( nz > 0 )THEN

    ALLOCATE( pfac(CurrentObs%Qcloud%nz),temp(CurrentObs%Qcloud%nz),STAT=ios )
    IF( ios /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMconvertObsThermo'
      error%Message = 'Error allocating arrays to convert cloud water'
      GOTO 9999
    END IF

!------ Set temperature and pressure values for standard atmosphere
!       (if required to fill in for non-existent obs)

    IF( .NOT.BTEST(Obs%type,OTB_P) .OR.  CurrentObs%Press%nz == 0 .OR. &
        .NOT.BTEST(Obs%type,OTB_T) .OR.  CurrentObs%Tpot%nz  == 0 )THEN
      DO k = 1,CurrentObs%Qcloud%nz
        zp = CurrentObs%Qcloud%z(k)+hmin
        CALL stnd_atmos( zp,pfac(k),temp(k),dum2,SA_TA )
      END DO
    END IF

!------ Interpolate/extrapolate pressure onto cloud water levels

    IF( BTEST(Obs%type,OTB_P) .AND.  CurrentObs%Press%nz > 0 )THEN

      CALL InterpExtrapObs( CurrentObs%Press%z,CurrentObs%Press%nz,CurrentObs%Press%obs, &
                            CurrentObs%Qcloud%z,CurrentObs%Qcloud%nz,pfac,hmin,StndLogP )
      pfac = EXP(pfac)

    END IF

!------ Interpolate/extrapolate temperature onto cloud water levels
!       N.B. temperature is always potential at this point

    IF( BTEST(Obs%type,OTB_T) .AND.  CurrentObs%Tpot%nz > 0 )THEN

      CALL InterpExtrapObs( CurrentObs%Tpot%z,CurrentObs%Tpot%nz,CurrentObs%Tpot%obs, &
                            CurrentObs%Humid%z,CurrentObs%Humid%nz,temp,hmin,StndTemp )
      temp = temp*pfac**KAPPA

    END IF

!------ Compute air density and multiply to get gm/m^3
!       N.B. already scaled by 1E3 if units are g/g or kg/kg

    DO k = 1,nz
      pres = PSURF*pfac(k)
      CurrentObs%Qcloud%obs(k) = CurrentObs%Qcloud%obs(k) * fun_rhoa( temp(k),pres )
    END DO

    DEALLOCATE( pfac,temp,STAT=ios )
    NULLIFY( pfac,temp )

  END IF

END IF

SWIMconvertObsThermo = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE InterpExtrapObs( zIn,nzIn,ObsIn,zOut,nzOut,obsOut,hmin,StndFunc )

!------ Interpolate/extrapolate (if necessary) for obs thermo fields

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

REAL, DIMENSION(:), POINTER :: zIn,  ObsIn
REAL, DIMENSION(:), POINTER :: zOut, ObsOut
INTEGER,       INTENT( IN ) :: nzIn, nzOut
REAL,          INTENT( IN ) :: hmin
REAL,          EXTERNAL     :: StndFunc

INTEGER k
REAL    zx, zp, del

!------ Interpolate

CALL Int1D( zIn,nzIn,obsIn,zOut,nzOut,obsOut )

!------ Extrapolate below first level

IF( zOut(1) < zIn(1) )THEN
  zx  = zIn(1)
  del = obsIn(1) - StndFunc( zx+hmin )
  DO k = 1,nzOut
    zp = zOut(k)
    IF( zp >= zx )EXIT
    obsOut(k) = StndFunc( zp+hmin ) + del
  END DO
END IF

!------ Extrapolate above top level

IF( zOut(nzOut) > zIn(nzIn) )THEN
  zx  = zIn(nzIn)
  del = obsIn(nzIn) - StndFunc( zx+hmin )
  DO k = nzOut,1,-1
    zp = zOut(k)
    IF( zp <= zx )EXIT
    obsOut(k) = StndFunc( zp+hmin ) + del
  END DO
END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMgetObsTerrain( grid,First,Obs )

USE SWIM_fi
USE SWIMpuff_fd
USE SWIMparam_fd
USE SWIMinterpPointer
USE constants_fd

IMPLICIT NONE

TYPE( MetGrid  ), INTENT( IN ) :: grid
TYPE( FirstObs ), INTENT( IN ) :: First
TYPE( ObsMet   ),      POINTER :: Obs

TYPE( PuffMetRequest ) :: Request
TYPE( met1dh )         :: mx, mxu, my, myv
TYPE( meth   )         :: mxy

INTEGER i
REAL    h, zrufMax
LOGICAL lObsZruf

!------ Set roughness / canopy parameters for obs location
!       N.B. No canopy if height or alpha/density missing

lObsZruf = BTEST(First%type,OTB_ZRUF) .AND. Obs%BL%surface%zruf  /= OBP_NODATA

IF( Obs%BL%surface%hc == OBP_NODATA )THEN
  Obs%BL%surface%hc    = 0.
  Obs%BL%surface%alpha = 0.

ELSE IF( BTEST(First%type,OTB_ALPH) )THEN
  IF( Obs%BL%surface%alpha == OBP_NODATA )THEN
    IF( Obs%BL%surface%adens /= OBP_NODATA )THEN
      Obs%BL%surface%alpha = Obs%BL%surface%adens * AREA_DENSITY_TO_ALPHA
    ELSE
      Obs%BL%surface%hc    = 0.
      Obs%BL%surface%alpha = 0.
    END IF
  END IF

ELSE IF( BTEST(First%type,OTB_ACNP) )THEN
  IF( Obs%BL%surface%adens /= OBP_NODATA )THEN
    Obs%BL%surface%alpha = Obs%BL%surface%adens * AREA_DENSITY_TO_ALPHA
  ELSE
    Obs%BL%surface%hc    = 0.
    Obs%BL%surface%alpha = 0.
  END IF

END IF

IF( Obs%Vel%nz > 0 )THEN
  zrufMax = Obs%Vel%z(1) * 0.01  !z is AGL: z=10m => max zruf=10cm
ELSE
  zrufMax = 0.1
END IF

IF( BTEST(grid%type,GTB_TERRAIN) )THEN

  Request%X = Obs%x
  Request%Y = Obs%y

  CALL SetXYfac( Request,grid,mx,my,mxu,myv )
  CALL SetMXY( mx,my,mxy,grid%nX,grid%nXY,.TRUE. )

  CALL IntXY( mxy,grid%terrain%H,Obs%BL%h )

  IF( .NOT.lObsZruf )THEN
    IF( BTEST(grid%type,GTB_ZRUF) )THEN
      CALL IntXY( mxy,grid%landcover%roughness,Obs%BL%surface%zruf )
    ELSE
      Obs%BL%surface%zruf = Prj%BL%zruf
    END IF
  END IF

  IF( BTEST(grid%type,GTB_ALBEDO) )THEN
    CALL IntXY( mxy,grid%landcover%albedo,Obs%BL%surface%albedo )
  ELSE
    Obs%BL%surface%albedo = Prj%BL%albedo
  END IF

  IF( BTEST(grid%type,GTB_BOWEN) )THEN
    CALL IntXY( mxy,grid%landcover%Bowen,Obs%BL%surface%Bowen )
  ELSE
    Obs%BL%surface%Bowen  = Prj%BL%Bowen
  END IF

ELSE

  Obs%BL%h = 0.

  Obs%BL%surface%albedo = Prj%BL%albedo
  Obs%BL%surface%Bowen  = Prj%BL%Bowen

  IF( .NOT.lObsZruf )Obs%BL%surface%zruf = Prj%BL%zruf

END IF

IF( .NOT.lObsZruf )Obs%BL%surface%zruf = MIN(Obs%BL%surface%zruf,zrufMax)

!------ Add terrain to profile levels

h = Obs%BL%h

IF( (BTEST(First%type,OTB_UV) .OR. BTEST(First%type,OTB_SPD)) .AND. Obs%Vel%nz > 0 )THEN
  DO i = 1,Obs%Vel%nz
    Obs%Vel%z(i) = Obs%Vel%z(i) + h
  END DO
END IF

IF( BTEST(First%type,OTB_T) .AND. Obs%Tpot%nz > 0 )THEN
  DO i = 1,Obs%Tpot%nz
     Obs%Tpot%z(i) = Obs%Tpot%z(i) + h
  END DO
END IF

IF( BTEST(First%type,OTB_TSTA) .AND. Obs%Tpot%nz > 0 )THEN
  DO i = 1,Obs%Tpot%nz
     Obs%TpotStA%z(i) = Obs%TpotStA%z(i) + h
  END DO
END IF

IF( BTEST(First%type,OTB_P) .AND. Obs%Press%nz > 0 )THEN
  DO i = 1,Obs%Press%nz
     Obs%Press%z(i) = Obs%Press%z(i) + h
  END DO
END IF

IF( BTEST(First%type,OTB_H) .AND. Obs%Humid%nz > 0 )THEN
  DO i = 1,Obs%Humid%nz
     Obs%Humid%z(i) = Obs%Humid%z(i) + h
  END DO
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION ReallocateObsVel( First,ObsVel,inc )

!----- Add space to obs velocity arrays

USE SWIM_fi
USE SWIMparam_fd
USE reallocate

IMPLICIT NONE

TYPE( FirstObs  ), INTENT( IN )    :: First
TYPE( ObsVelPrf ), INTENT( INOUT ) :: ObsVel
INTEGER,           INTENT( IN    ) :: inc

INTEGER irv

              irv = reallocate_real1d( ObsVel%z,inc )
IF( irv == 0 )irv = reallocate_real1d( ObsVel%u,inc )
IF( irv == 0 )irv = reallocate_real1d( ObsVel%v,inc )

IF( BTEST(First%type,OTB_LSV) )THEN
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%LSV%uu,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%LSV%vv,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%LSV%uv,inc )
END IF

IF( BTEST(First%type,OTB_UU) )THEN
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%uu,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%vv,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%ww,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%wt,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%sl,inc )
  IF( irv == 0 )irv = reallocate_real1d( ObsVel%blProf%sz,inc )
END IF

IF( irv == 0 )THEN
  ReallocateObsVel = SWIMresult
ELSE
  ReallocateObsVel = SWIMfailure
END IF

RETURN
END

!=======================================================================

INTEGER FUNCTION WrtLogWxsList( First )

!------ Write positions of wx obs station to log file

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( IN ) :: First

TYPE( ObsMet ), POINTER :: Obs

CHARACTER(128) string

INTEGER irv, ios, i

INTEGER, EXTERNAL :: SWIMaddLogMessage

WrtLogWxsList = SWIMfailure

irv = SWIMaddLogMessage( '&wxsloc' )
IF( irv /= SWIMsuccess )GOTO 9999
IF( BTEST(First%type,OTB_SRF) )THEN
  irv = SWIMaddLogMessage( ' wx = ''SFC''' )
ELSE
  irv = SWIMaddLogMessage( ' wx = ''PRF''' )
END IF
IF( irv /= SWIMsuccess )GOTO 9999

WRITE(string,101,IOSTAT=ios) First%numObs
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

Obs => First%Obs
i   =  0

DO WHILE( ASSOCIATED(Obs) )
  i = i + 1
  WRITE(string,102,IOSTAT=ios) i,Obs%x,i,Obs%y
  irv = SWIMaddLogMessage( string )
  IF( irv /= SWIMsuccess )GOTO 9999
  Obs => Obs%nextObs
END DO
irv = SWIMaddLogMessage( '/' )
IF( irv /= SWIMsuccess )GOTO 9999

101 FORMAT(1X,'nwx = ',I4)
102 FORMAT(1x,'xwx(',I4,') = ',ES15.7,',ywx(',I4,') = ',ES15.7)

WrtLogWxsList = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SWIMsetGridList( First,grid )

!------ Check if worth building grid for nearest-obs searches

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( FirstObs ), INTENT( INOUT ) :: First
TYPE( MetGrid  ), INTENT( IN    ) :: grid

INTEGER, PARAMETER :: NUMOBS_CELL   = 4
INTEGER, PARAMETER :: NUMOBS_SEARCH = 4*NUMOBS_CELL

INTEGER alloc_stat, n, i, nx, ny
REAL    fx, fy

SWIMsetGridList = SWIMfailure

IF( First%numObs > NUMOBS_SEARCH .AND. grid%Nx > 1 )THEN

  fx = (grid%Xmax-grid%Xmin) / (grid%Ymax-grid%Ymin)
  fx = SQRT(fx*First%numObs/FLOAT(NUMOBS_CELL))
  fy = First%numObs/FLOAT(NUMOBS_CELL) / fx
  nx = MAX(NINT(fx),1)
  ny = MAX(NINT(fy),1)

  IF( nx > 2 .OR. ny > 2 )THEN  !Only proceed if number of cells exceeds 2
                                !in at least one direction
    ALLOCATE( First%GridList,STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMsetGridList'
      error%Message = 'Error allocating first pointer for nearest obs linked-list'
      GOTO 9999
    END IF

    First%GridList%Nx = nx
    First%GridList%Ny = ny

    n = nx*ny
    ALLOCATE( First%GridList%GridList(n),First%GridList%NumObsCell(n),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMsetGridList'
      error%Message = 'Error allocating space for nearest obs lists'
      GOTO 9999
    END IF

    DO i = 1,n
      NULLIFY(First%GridList%GridList(i)%Obs)
      NULLIFY(First%GridList%GridList(i)%Next)
      First%GridList%NumObsCell(i) = 0
    END DO

    First%GridList%Xmin = grid%Xmin;  First%GridList%Xmax = grid%Xmax
    First%GridList%Ymin = grid%Ymin;  First%GridList%Ymax = grid%Ymax

  END IF

END IF

SWIMsetGridList = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE PutObsInCell( First,CurrentObs )

!----- Find cell and put in linked-list

USE SWIM_fi
USE SWIMparam_fd
USE reallocate

IMPLICIT NONE

TYPE( FirstObsGridList ), INTENT( INOUT ) :: First
TYPE( ObsMet   ),         POINTER         :: CurrentObs

TYPE( ObsGridList ), POINTER :: ListObs

INTEGER alloc_stat, i, j
REAL    frac

INTEGER, EXTERNAL :: SWIMlimit

frac = (CurrentObs%x - First%Xmin)/(First%Xmax - First%Xmin)
i    =  SWIMlimit( INT(frac*First%Nx+1.),1,First%Nx )

frac = (CurrentObs%y - First%Ymin)/(First%Ymax - First%Ymin)
j    =  SWIMlimit( INT(frac*First%Ny+1.),1,First%Ny )

i = (j-1)*First%Nx + i

First%NumObsCell(i) = First%NumObsCell(i) + 1

IF( ASSOCIATED(First%GridList(i)%Obs) )THEN  !Go down linked-list

  ListObs => First%GridList(i)

  DO WHILE( ASSOCIATED(ListObs%Next) )
    ListObs => ListObs%next
  END DO

  ALLOCATE(ListObs%Next,STAT=alloc_stat)

  ListObs%Next%Obs => CurrentObs
  NULLIFY( ListObs%Next%Next )

ELSE                                   !First obs in this grid cell

  First%GridList(i)%Obs => CurrentObs
  NULLIFY(First%GridList(i)%Next)

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMallocAvgObs( Obs,InterpPrf,ObsID )

!------ Allocate profiles for averaging time-binned obs with the same ID
!       Set enough levels to accomodate the union of all unique levels

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE reallocate

IMPLICIT NONE

TYPE( FirstObs ),  INTENT( IN  ) :: Obs
TYPE( ObsInterp ), INTENT( OUT ) :: InterpPrf
CHARACTER(*),      INTENT( IN  ) :: ObsID

INTEGER alloc_stat, irv, n, k

TYPE( ObsMet ), POINTER :: CurrentObs

INTERFACE
  INTEGER FUNCTION UnionGrid( nu,zu,nz,z )
    INTEGER,            INTENT ( INOUT ) :: nu
    REAL, DIMENSION(:), POINTER          :: zu
    INTEGER,            INTENT ( IN    ) :: nz
    REAL, DIMENSION(:), POINTER          :: z
  END FUNCTION UnionGrid
END INTERFACE

SWIMallocAvgObs = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMallocAvgObs'
error%Message = 'Error allocating profiles for time-bin averages'

!------ Find first obs with this ID

CurrentObs => Obs%obs
DO WHILE( ASSOCIATED(CurrentObs) )
  IF( TRIM(CurrentObs%id) == TRIM(ObsID) )EXIT
  CurrentObs => CurrentObs%nextObs
END DO

IF( TRIM(CurrentObs%id) /= TRIM(ObsID) )THEN
  error%Inform = 'Error matching ID'
  GOTO 9999
END IF

!------ Find union of all unique levels
!       Initialize to first wind velocity levels (if they exist)

n = MAX(10,CurrentObs%Vel%nz,CurrentObs%Tpot%nz)

ALLOCATE( InterpPrf%z(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

IF( CurrentObs%Vel%nz > 0 )THEN
  n = CurrentObs%Vel%nz
  DO k = 1,n
    InterpPrf%z(k) = CurrentObs%Vel%z(k)
  END DO
ELSE
  n = 1
  InterpPrf%z(1) = 10.  !Set one level at 10m if missing velocity obs
END IF

DO WHILE( ASSOCIATED(CurrentObs) )

  IF( TRIM(CurrentObs%id) /= TRIM(ObsID) )THEN
    CurrentObs => CurrentObs%nextObs; CYCLE
  END IF

  irv = UnionGrid( n,InterpPrf%z,CurrentObs%Vel%nz,CurrentObs%Vel%z )
  IF( irv /= 0 )GOTO 9999

  IF( BTEST(Obs%type,OTB_T) )THEN
    irv = UnionGrid( n,InterpPrf%z,CurrentObs%Tpot%nz,CurrentObs%Tpot%z )
    IF( irv /= 0 )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_H) )THEN
    irv = UnionGrid( n,InterpPrf%z,CurrentObs%Humid%nz,CurrentObs%Humid%z )
    IF( irv /= 0 )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_P) )THEN
    irv = UnionGrid( n,InterpPrf%z,CurrentObs%Press%nz,CurrentObs%Press%z )
    IF( irv /= 0 )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_QCLD) )THEN
    irv = UnionGrid( n,InterpPrf%z,CurrentObs%Qcloud%nz,CurrentObs%Qcloud%z )
    IF( irv /= 0 )GOTO 9999
  END IF

  CurrentObs => CurrentObs%nextObs

END DO

InterpPrf%nz = n

ALLOCATE( InterpPrf%U%obs(n),InterpPrf%U%wt(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

ALLOCATE( InterpPrf%V%obs(n),InterpPrf%V%wt(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

IF( BTEST(Obs%type,OTB_T) )THEN
  ALLOCATE( InterpPrf%Tpot%obs(n),InterpPrf%Tpot%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_TSTA) )THEN
  ALLOCATE( InterpPrf%TpotStA%obs(n),InterpPrf%TpotStA%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_H) )THEN
  ALLOCATE( InterpPrf%Humid%obs(n),InterpPrf%Humid%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_P) )THEN
  ALLOCATE( InterpPrf%Press%obs(n),InterpPrf%Press%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_QCLD) )THEN
  ALLOCATE( InterpPrf%Qcloud%obs(n),InterpPrf%Qcloud%wt(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_LSV) )THEN
  ALLOCATE( InterpPrf%UUL%obs(n),InterpPrf%UUL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%VVL%obs(n),InterpPrf%VVL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%UVL%obs(n),InterpPrf%UVL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  IF( BTEST(Obs%type,OTB_LSVL) )THEN
    ALLOCATE( InterpPrf%SHL%obs(n),InterpPrf%SHL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  END IF
END IF

IF( BTEST(Obs%type,OTB_UU) )THEN
  ALLOCATE( InterpPrf%UU%obs(n),InterpPrf%UU%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%VV%obs(n),InterpPrf%VV%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%WW%obs(n),InterpPrf%WW%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%WT%obs(n),InterpPrf%WT%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%SL%obs(n),InterpPrf%SL%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
  ALLOCATE( InterpPrf%SZ%obs(n),InterpPrf%SZ%wt(n),STAT=alloc_stat );IF( alloc_stat /= 0 )GOTO 9999
END IF

SWIMallocAvgObs = SWIMresult

CALL SWIMclearError()

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SWIMzeroAvgObs( Obs,InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( FirstObs ),  INTENT( IN    ) :: Obs
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf

INTEGER i

DO i = 1,InterpPrf%nz
  InterpPrf%U%obs(i) = 0.; InterpPrf%U%wt(i) = 0.
  InterpPrf%V%obs(i) = 0.; InterpPrf%V%wt(i) = 0.
END DO

IF( BTEST(Obs%type,OTB_T) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Tpot%obs(i) = 0.; InterpPrf%Tpot%wt(i) = 0.
  END DO
END IF

IF( BTEST(Obs%type,OTB_TSTA) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%TpotStA%obs(i) = 0.; InterpPrf%TpotStA%wt(i) = 0.
  END DO
END IF

IF( BTEST(Obs%type,OTB_H) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Humid%obs(i) = 0.; InterpPrf%Humid%wt(i) = 0.
  END DO
END IF

IF( BTEST(Obs%type,OTB_P) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Press%obs(i) = 0.; InterpPrf%Press%wt(i) = 0.
  END DO
END IF

IF( BTEST(Obs%type,OTB_QCLD) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%Qcloud%obs(i) = 0.; InterpPrf%Qcloud%wt(i) = 0.
  END DO
END IF

IF( BTEST(Obs%type,OTB_LSV) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%UUL%obs(i) = 0.; InterpPrf%UUL%wt(i) = 0.
    InterpPrf%VVL%obs(i) = 0.; InterpPrf%VVL%wt(i) = 0.
    InterpPrf%UVL%obs(i) = 0.; InterpPrf%UVL%wt(i) = 0.
    IF( BTEST(Obs%type,OTB_LSVL) )THEN
      InterpPrf%SHL%obs(i) = 0.; InterpPrf%SHL%wt(i) = 0.
    END IF
  END DO
END IF

IF( BTEST(Obs%type,OTB_UU) )THEN
  DO i = 1,InterpPrf%nz
    InterpPrf%UU%obs(i) = 0.; InterpPrf%UU%wt(i) = 0.
    InterpPrf%VV%obs(i) = 0.; InterpPrf%VV%wt(i) = 0.
    InterpPrf%WW%obs(i) = 0.; InterpPrf%WW%wt(i) = 0.
    InterpPrf%WT%obs(i) = 0.; InterpPrf%WT%wt(i) = 0.
    InterpPrf%SL%obs(i) = 0.; InterpPrf%SL%wt(i) = 0.
    InterpPrf%SZ%obs(i) = 0.; InterpPrf%SZ%wt(i) = 0.
  END DO
END IF

!------ Zero surface quantities

IF( BTEST(Obs%type,OTB_ZI) )THEN
  InterpPrf%Zi%obs = 0.; InterpPrf%Zi%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_HFLX) )THEN
  InterpPrf%Hflux%obs = 0.; InterpPrf%Hflux%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_MOL) )THEN
  InterpPrf%invL%obs = 0.; InterpPrf%invL%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_PRCP) .OR. BTEST(Obs%type,OTB_PRATE) )THEN
  InterpPrf%Prcp%obs = 0.; InterpPrf%Prcp%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_CC) )THEN
  InterpPrf%CldCv%obs = 0.; InterpPrf%CldCv%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_UST) )THEN
  InterpPrf%Ust%obs = 0.; InterpPrf%Ust%wt = 0.
END IF

IF( BTEST(Obs%type,OTB_LAI) )THEN
  InterpPrf%LAI%obs = 0.; InterpPrf%LAI%wt = 0.
END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION SWIMbinAvgObs( Obs,InterpPrf,ObsID )

!------ Combine profiles with the same ID within the same time bin

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE reallocate

IMPLICIT NONE

TYPE( FirstObs ),  INTENT( IN    ) :: Obs
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf
CHARACTER(*),      INTENT( IN    ) :: ObsID

TYPE( ObsMet ), POINTER :: CurrentObs

SWIMbinAvgObs = SWIMfailure

!------ Loop over linked-list; find those with this ID

CurrentObs => Obs%obs

DO WHILE( ASSOCIATED(CurrentObs) )

  IF( TRIM(CurrentObs%id) /= TRIM(ObsID) )THEN
    CurrentObs => CurrentObs%nextObs; CYCLE
  END IF

!------ Profiles

  CALL AddObsVel( InterpPrf,CurrentObs%Vel,Obs%type )
  IF( error%Number /= NO_ERROR )GOTO 9999

  IF( BTEST(Obs%type,OTB_T) )THEN
    CALL AddObs( InterpPrf,InterpPrf%Tpot,CurrentObs%Tpot )
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_TSTA) )THEN
    CALL AddObs( InterpPrf,InterpPrf%TpotStA,CurrentObs%TpotStA )
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_H) )THEN
    CALL AddObs( InterpPrf,InterpPrf%Humid,CurrentObs%Humid )
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_P) )THEN
    CALL AddObs( InterpPrf,InterpPrf%Press,CurrentObs%Press )
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF

  IF( BTEST(Obs%type,OTB_QCLD) )THEN
    CALL AddObs( InterpPrf,InterpPrf%Qcloud,CurrentObs%Qcloud )
    IF( error%Number /= NO_ERROR )GOTO 9999
  END IF


!------ Surface quantities

  IF( BTEST(Obs%type,OTB_ZI) )THEN
    CALL AddSrfObs( InterpPrf%Zi,CurrentObs%varSrf%zi )
  END IF

  IF( BTEST(Obs%type,OTB_HFLX) )THEN
    CALL AddSrfObs( InterpPrf%Hflux,CurrentObs%varSrf%hflux )
  END IF

  IF( BTEST(Obs%type,OTB_MOL) )THEN
    CALL AddSrfObs( InterpPrf%invL,CurrentObs%varSrf%invL )
  END IF

  IF( BTEST(Obs%type,OTB_PRCP) .OR. BTEST(Obs%type,OTB_PRATE) )THEN
    CALL AddSrfObs( InterpPrf%Prcp,CurrentObs%varSrf%prcp )
  END IF

  IF( BTEST(Obs%type,OTB_CC) )THEN
    CALL AddSrfObs( InterpPrf%CldCv,CurrentObs%varSrf%cloudcover )
  END IF

  IF( BTEST(Obs%type,OTB_UST) )THEN
    CALL AddSrfObs( InterpPrf%Ust,CurrentObs%varSrf%ustr )
  END IF

  IF( BTEST(Obs%type,OTB_LAI) )THEN
    CALL AddSrfObs( InterpPrf%LAI,CurrentObs%varSrf%LAI )
  END IF

  CurrentObs => CurrentObs%nextObs

END DO

!------ Compute averages

CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%U )
CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%V )

IF( BTEST(Obs%type,OTB_T) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%Tpot )
END IF

IF( BTEST(Obs%type,OTB_TSTA) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%TpotStA )
END IF

IF( BTEST(Obs%type,OTB_H) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%Humid )
END IF

IF( BTEST(Obs%type,OTB_P) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%Press )
END IF

IF( BTEST(Obs%type,OTB_QCLD) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%Qcloud )
END IF

IF( BTEST(Obs%type,OTB_LSV) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%UUL )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%VVL )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%UVL )
  IF( BTEST(Obs%type,OTB_LSVL) )CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%SHL )
END IF

IF( BTEST(Obs%type,OTB_UU) )THEN
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%UU )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%VV )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%WW )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%WT )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%SL )
  CALL ComputePrfAvg( InterpPrf%nz,InterpPrf%SZ )
END IF

!------ Surface quantities

IF( BTEST(Obs%type,OTB_ZI)   )CALL ComputeSrfAvg( InterpPrf%zi )

IF( BTEST(Obs%type,OTB_HFLX) )CALL ComputeSrfAvg( InterpPrf%hflux )

IF( BTEST(Obs%type,OTB_MOL)  )CALL ComputeSrfAvg( InterpPrf%invL )

IF( BTEST(Obs%type,OTB_PRCP) .OR. BTEST(Obs%type,OTB_PRATE) )CALL ComputeSrfAvg( InterpPrf%Prcp )

IF( BTEST(Obs%type,OTB_CC)   )CALL ComputeSrfAvg( InterpPrf%CldCv )

IF( BTEST(Obs%type,OTB_UST)  )CALL ComputeSrfAvg( InterpPrf%Ust )

IF( BTEST(Obs%type,OTB_LAI)   )CALL ComputeSrfAvg( InterpPrf%LAI )

!------ Done

SWIMbinAvgObs = SWIMresult

9999 CONTINUE

RETURN
END


!===============================================================================

INTEGER FUNCTION SWIMgenAvgObs( Obs,InterpPrf,ObsID )

!------ Create averaged obs profile and fix linked-list

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd
USE reallocate

IMPLICIT NONE

TYPE( FirstObs ),  INTENT( INOUT ) :: Obs
TYPE( ObsInterp ), INTENT( IN    ) :: InterpPrf
CHARACTER(*),      INTENT( IN    ) :: ObsID

TYPE( ObsMet ), POINTER :: CurrentObs, PrevObs

INTEGER, EXTERNAL :: GenAvgPrfVel, GenAvgPrf

INTEGER irv

SWIMgenAvgObs = SWIMfailure

!------ Loop over linked-list; find first with this ID

CurrentObs => Obs%obs

DO WHILE( ASSOCIATED(CurrentObs) )
  IF( TRIM(CurrentObs%id) == TRIM(ObsID) )EXIT
  CurrentObs => CurrentObs%nextObs
END DO

IF( TRIM(CurrentObs%id) /= TRIM(ObsID) )GOTO 9999

!------ Replace profiles with averaged values

irv = GenAvgPrfVel( InterpPrf,CurrentObs%Vel,Obs )
IF( irv /= SWIMsuccess )GOTO 9999

IF( BTEST(Obs%type,OTB_T) )THEN
  irv = GenAvgPrf( InterpPrf,InterpPrf%Tpot,CurrentObs%Tpot )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_TSTA) )THEN
  irv = GenAvgPrf( InterpPrf,InterpPrf%TpotStA,CurrentObs%TpotStA )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_H) )THEN
  irv = GenAvgPrf( InterpPrf,InterpPrf%Humid,CurrentObs%Humid )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_P) )THEN
  irv = GenAvgPrf( InterpPrf,InterpPrf%Press,CurrentObs%Press )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

IF( BTEST(Obs%type,OTB_QCLD) )THEN
  irv = GenAvgPrf( InterpPrf,InterpPrf%Qcloud,CurrentObs%Qcloud )
  IF( irv /= SWIMsuccess )GOTO 9999
END IF

!------ Surface quantities

IF( BTEST(Obs%type,OTB_ZI)   )CurrentObs%varSrf%zi    = InterpPrf%Zi%obs
IF( BTEST(Obs%type,OTB_HFLX) )CurrentObs%varSrf%hflux = InterpPrf%Hflux%obs
IF( BTEST(Obs%type,OTB_MOL)  )CurrentObs%varSrf%invL  = InterpPrf%invL%obs
IF( BTEST(Obs%type,OTB_PRCP) .OR. BTEST(Obs%type,OTB_PRATE) )CurrentObs%varSrf%prcp = InterpPrf%Prcp%obs
IF( BTEST(Obs%type,OTB_CC)   )CurrentObs%varSrf%cloudcover = InterpPrf%CldCv%obs
IF( BTEST(Obs%type,OTB_UST)  )CurrentObs%varSrf%ustr = InterpPrf%Ust%obs
IF( BTEST(Obs%type,OTB_LAI)  )CurrentObs%varSrf%LAI = InterpPrf%LAI%obs

!------ Deallocate other obs with this id; modify linked-list

CurrentObs => CurrentObs%nextObs

DO WHILE( ASSOCIATED(CurrentObs) )
  IF( TRIM(CurrentObs%id) == TRIM(ObsID) )THEN

    IF( ASSOCIATED(CurrentObs%nextObs) )THEN
      CurrentObs%nextObs%prevObs => CurrentObs%prevObs
      CurrentObs%prevObs%nextObs => CurrentObs%nextObs
    ELSE
      NULLIFY(CurrentObs%prevObs%nextObs) !End of list
    END IF

    PrevObs => CurrentObs%prevObs

    DEALLOCATE( CurrentObs,STAT=irv )
    IF( irv /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMgenAvgObs'
      error%Message = 'Error compressing obs linked-list with time-bin-averaging'
      GOTO 9999
    END IF

    CurrentObs => PrevObs

    Obs%numObs = Obs%numObs - 1

  END IF

  CurrentObs => CurrentObs%nextObs

END DO

!------ Done

SWIMgenAvgObs = SWIMresult

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SWIMdeallocAvgObs( Obs,InterpPrf )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMobsInterp_fd

IMPLICIT NONE

TYPE( FirstObs  ), INTENT( IN    ) :: Obs
TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf

INTEGER alloc_stat, irv
INTEGER, EXTERNAl :: SWIMdeallocObsInterpPrf

IF( ASSOCIATED(InterpPrf%z) )DEALLOCATE( InterpPrf%z,STAT=alloc_stat )

irv = SWIMdeallocObsInterpPrf( InterpPrf%U )
irv = SWIMdeallocObsInterpPrf( InterpPrf%V )

IF( BTEST(Obs%type,OTB_T) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Tpot )
END IF

IF( BTEST(Obs%type,OTB_TSTA) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%TpotStA )
END IF

IF( BTEST(Obs%type,OTB_H) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Humid )
END IF

IF( BTEST(Obs%type,OTB_P) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Press )
END IF

IF( BTEST(Obs%type,OTB_QCLD) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%Qcloud )
END IF

IF( BTEST(Obs%type,OTB_LSV) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UUL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%VVL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UVL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SHL )
END IF

IF( BTEST(Obs%type,OTB_UU) )THEN
  irv = SWIMdeallocObsInterpPrf( InterpPrf%UU )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%VV )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%WW )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%WT )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SL )
  irv = SWIMdeallocObsInterpPrf( InterpPrf%SZ )
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION UnionGrid( nu,zu,nz,z ) RESULT( irv )

USE reallocate

IMPLICIT NONE

INTEGER,            INTENT ( INOUT ) :: nu
REAL, DIMENSION(:), POINTER          :: zu
INTEGER,            INTENT ( IN   )  :: nz
REAL, DIMENSION(:), POINTER          :: z

REAL, PARAMETER :: EPS = 1.E-2  !Tolerance (m) for unique level

INTEGER i, k, nt, i1
LOGICAL lMatch, lSort

REAL, DIMENSION(:), ALLOCATABLE :: zsort

irv = 0 !Success

nt = SIZE(zu)

IF( nz+nu > nt )THEN
  irv = reallocate_real1d( zu,nz )
  IF( irv /= 0 )RETURN
END IF

lSort = .FALSE.
i1    = 1

DO k = 1,nz
  lMatch = .FALSE.
  DO i = i1,nu
    IF( ABS(z(k)-zu(i)) < EPS )THEN
      lMatch = .TRUE.; i1 = i; EXIT
    END IF
  END DO
  IF( lMatch )CYCLE
  lSort = lSort .OR. z(k) < zu(nu)
  nu = nu + 1
  zu(nu) = z(k)
END DO

IF( lSort )THEN
  ALLOCATE( zsort(nu),STAT=irv )
  IF( irv /= 0 )RETURN
  zsort(1:nu) = zu(1:nu)
  CALL usort( zsort,nu )
  zu(1:nu) = zsort(1:nu)
  DEALLOCATE( zsort,STAT=irv )
END IF

RETURN
END

!==============================================================================

SUBROUTINE AddObs( InterpPrf,AvgPrf,Prf )

USE SWIM_fi
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

TYPE( ObsInterp ),    INTENT( INOUT ) :: InterpPrf
TYPE( ObsInterpPrf ), INTENT( INOUT ) :: AvgPrf
TYPE( ObsPrf ),       INTENT( IN    ) :: Prf

INTEGER alloc_stat, k, nz

REAL, DIMENSION(:), ALLOCATABLE :: zIntrp
REAL, DIMENSION(:), ALLOCATABLE :: z, p

CHARACTER(128), EXTERNAL :: ArraySizeStr

nz = Prf%nz

IF( nz == 0 )RETURN

ALLOCATE( zIntrp(InterpPrf%nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'AddObs'
  error%Message = 'Error allocating zIntrp array for time-bin-averaged observation'
  error%Inform  = ArraySizeStr( 1,(/InterpPrf%nz/) )
  GOTO 9999
END IF

ALLOCATE( z(nz),p(nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'AddObs'
  error%Message = 'Error allocating arrays for time-bin-averaged observation'
  error%Inform  = ArraySizeStr( 1,(/nz/) )
  GOTO 9999
END IF

nz = 0

!------ Construct observation profiles

DO k = 1,Prf%nz
  IF( Prf%obs(k) == NOT_SET_R )CYCLE
  nz    = nz + 1
  z(nz) = Prf%z(k)
  p(nz) = Prf%obs(k)
END DO

!----- Interpolate onto union grid

zIntrp(1:InterpPrf%nz) = InterpPrf%z(1:InterpPrf%nz)

CALL InterpProfile( nz,z,p,InterpPrf%nz,zIntrp,AvgPrf )

9999 CONTINUE

IF( ALLOCATED(p)      )DEALLOCATE(p,     STAT=alloc_stat)
IF( ALLOCATED(zIntrp) )DEALLOCATE(zIntrp,STAT=alloc_stat)

RETURN
END

!==============================================================================

SUBROUTINE AddObsVel( InterpPrf,Prf,ObsType )

!------ Add current velocity profile for bin averaging
!       N.B. Interpolate obs to union grid

USE SWIM_fi
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

TYPE( ObsInterp ), INTENT( INOUT ) :: InterpPrf
TYPE( ObsVelPrf ), INTENT( IN    ) :: Prf
INTEGER(KIND=8),   INTENT( IN    ) :: ObsType

INTEGER alloc_stat, k, nz, nzl, nzp

REAL, DIMENSION(:), ALLOCATABLE :: zIntrp
REAL, DIMENSION(:), ALLOCATABLE :: z, u, v
REAL, DIMENSION(:), ALLOCATABLE :: zl, uul, vvl, uvl, shl
REAL, DIMENSION(:), ALLOCATABLE :: zp, uu,  vv,  ww,  wt, sl, sz

CHARACTER(128), EXTERNAL :: ArraySizeStr

nz = Prf%nz

IF( nz == 0 )RETURN

ALLOCATE( zIntrp(InterpPrf%nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'AddObsVel'
  error%Message = 'Error allocating zIntrp array for time-bin-averaged observation'
  error%Inform  = ArraySizeStr( 1,(/InterpPrf%nz/) )
  GOTO 9999
END IF

ALLOCATE( z(nz),u(nz),v(nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'AddObsVel'
  error%Message = 'Error allocating u,v arrays for time-bin-averaged observation'
  error%Inform  = ArraySizeStr( 1,(/nz/) )
  GOTO 9999
END IF

IF( BTEST(ObsType,OTB_LSV) )THEN
  ALLOCATE( zl(nz),uul(nz),vvl(nz),uvl(nz),STAT=alloc_stat )
  IF( alloc_stat == 0 .AND. BTEST(ObsType,OTB_LSVL) )THEN
    ALLOCATE( shl(nz),STAT=alloc_stat )
  END IF
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'AddObsVel'
    error%Message = 'Error allocating LSV arrays for time-bin-averaged observation'
    error%Inform  = ArraySizeStr( 1,(/nz/) )
    GOTO 9999
  END IF
END IF

IF( BTEST(ObsType,OTB_UU) )THEN
  ALLOCATE( zp(nz),uu(nz),vv(nz),ww(nz),wt(nz),sl(nz),sz(nz),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'AddObsVel'
    error%Message = 'Error allocating turbulence profile arrays for time-bin-averaged observation'
    error%Inform  = ArraySizeStr( 1,(/nz/) )
    GOTO 9999
  END IF
END IF

nz = 0; nzl = 0; nzp = 0

!------ Construct observation profiles

DO k = 1,Prf%nz

  IF( Prf%u(k) == NOT_SET_R )CYCLE

  nz    = nz + 1
  z(nz) = Prf%z(k)
  u(nz) = Prf%u(k)
  v(nz) = Prf%v(k)

  IF( BTEST(ObsType,OTB_LSV) )THEN
    IF( Prf%LSV%uu(k) /= NOT_SET_R )THEN
      nzl      = nzl + 1
      zl(nzl)  = Prf%z(k)
      uul(nzl) = Prf%LSV%uu(k)
      vvl(nzl) = Prf%LSV%vv(k)
      uvl(nzl) = Prf%LSV%uv(k)
      IF( BTEST(ObsType,OTB_LSVL) )shl(nzl) = Prf%LSV%sl(k)
    END IF
  END IF

  IF( BTEST(ObsType,OTB_UU) )THEN
    IF( Prf%BLprof%uu(k) /= NOT_SET_R )THEN
      nzp     = nzp + 1
      zp(nzp) = Prf%z(k)
      uu(nzp) = Prf%BLprof%uu(k)
      vv(nzp) = Prf%BLprof%vv(k)
      ww(nzp) = Prf%BLprof%ww(k)
      wt(nzp) = Prf%BLprof%wt(k)
      sl(nzp) = Prf%BLprof%sl(k)
      sz(nzp) = Prf%BLprof%sz(k)
    END IF
  END IF

END DO

!----- Interpolate onto union grid

zIntrp(1:InterpPrf%nz) = InterpPrf%z(1:InterpPrf%nz)

CALL InterpProfile( nz,z,u,InterpPrf%nz,zIntrp,InterpPrf%U )
CALL InterpProfile( nz,z,v,InterpPrf%nz,zIntrp,InterpPrf%V )

IF( BTEST(ObsType,OTB_LSV) )THEN
  CALL InterpProfile( nzl,zl,uul,InterpPrf%nz,zIntrp,InterpPrf%UUL )
  CALL InterpProfile( nzl,zl,vvl,InterpPrf%nz,zIntrp,InterpPrf%VVL )
  CALL InterpProfile( nzl,zl,uvl,InterpPrf%nz,zIntrp,InterpPrf%UVL )
  IF( BTEST(ObsType,OTB_LSVL) )CALL InterpProfile( nzl,zl,shl,InterpPrf%nz,zIntrp,InterpPrf%SHL )
END IF

IF( BTEST(ObsType,OTB_UU) )THEN
  CALL InterpProfile( nzp,zp,uu,InterpPrf%nz,zIntrp,InterpPrf%UU )
  CALL InterpProfile( nzp,zp,vv,InterpPrf%nz,zIntrp,InterpPrf%VV )
  CALL InterpProfile( nzp,zp,ww,InterpPrf%nz,zIntrp,InterpPrf%WW )
  CALL InterpProfile( nzp,zp,wt,InterpPrf%nz,zIntrp,InterpPrf%WT )
  CALL InterpProfile( nzp,zp,sl,InterpPrf%nz,zIntrp,InterpPrf%SL )
  CALL InterpProfile( nzp,zp,sz,InterpPrf%nz,zIntrp,InterpPrf%SZ )
END IF

CALL SWIMclearError()

9999 CONTINUE

IF( ALLOCATED(u)   )DEALLOCATE(u,  STAT=alloc_stat)
IF( ALLOCATED(v)   )DEALLOCATE(v,  STAT=alloc_stat)
IF( ALLOCATED(uul) )DEALLOCATE(uul,STAT=alloc_stat)
IF( ALLOCATED(vvl) )DEALLOCATE(vvl,STAT=alloc_stat)
IF( ALLOCATED(uvl) )DEALLOCATE(uvl,STAT=alloc_stat)
IF( ALLOCATED(shl) )DEALLOCATE(shl,STAT=alloc_stat)
IF( ALLOCATED(uu ) )DEALLOCATE(uu ,STAT=alloc_stat)
IF( ALLOCATED(vv ) )DEALLOCATE(vv ,STAT=alloc_stat)
IF( ALLOCATED(ww ) )DEALLOCATE(ww ,STAT=alloc_stat)
IF( ALLOCATED(wt ) )DEALLOCATE(wt ,STAT=alloc_stat)
IF( ALLOCATED(sl ) )DEALLOCATE(sl ,STAT=alloc_stat)
IF( ALLOCATED(sz ) )DEALLOCATE(sz ,STAT=alloc_stat)

IF( ALLOCATED(zIntrp) )DEALLOCATE(zIntrp,STAT=alloc_stat)

RETURN
END

!==============================================================================

SUBROUTINE InterpProfile( nz,z,p,nzAvg,zAvg,AvgPrf )

USE SWIMobsInterp_fd

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: nz
REAL, DIMENSION(nz),    INTENT( IN    ) :: z, p
INTEGER,                INTENT( IN    ) :: nzAvg
REAL, DIMENSION(nzAvg), INTENT( IN    ) :: zAvg
TYPE( ObsInterpPrf ),   INTENT( INOUT ) :: AvgPrf

INTEGER i

REAL, DIMENSION(nzAvg) :: pAvg

IF( nz == nzAVG )THEN
  pAvg = P                              !No interpolation required (z=zAvg)
ELSE
  CALL interp( z,nz,p,zAvg,nzAvg,pAvg ) !Linear interpolation
END IF

!------ Add to sums, but do not extrapolate above or below obs profile

DO i = 1,nzAvg
  IF( zAvg(i) >= z(1) .AND. zAvg(i) <= z(nz) )THEN
    AvgPrf%obs(i) = AvgPrf%obs(i) + pAvg(i)
    AvgPrf%wt(i)  = AvgPrf%wt(i)  + 1.
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE AddSrfObs( AvgSrf,SrfObs )

USE SWIMobs_fd
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

TYPE( ObsInterpSrf ), INTENT( INOUT ) :: AvgSrf
REAL,                 INTENT( IN    ) :: SrfObs

IF( SrfObs /= NOT_SET_R )THEN
  AvgSrf%obs = AvgSrf%obs + SrfObs
  AvgSrf%wt  = AvgSrf%wt  + 1.
END IF

RETURN
END

!==============================================================================

SUBROUTINE ComputePrfAvg( nz,AvgPrf )

USE SWIMobs_fd
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

INTEGER,              INTENT( IN    ) :: nz
TYPE( ObsInterpPrf ), INTENT( INOUT ) :: AvgPrf

INTEGER k

DO k = 1,nz
  IF( AvgPrf%wt(k) > 0. )THEN
    AvgPrf%obs(k) = AvgPrf%obs(k) / AvgPrf%wt(k)
  ELSE
    AvgPrf%obs(k) = NOT_SET_R
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE ComputeSrfAvg( AvgSrf )

USE SWIMobs_fd
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd

IMPLICIT NONE

TYPE( ObsInterpSrf ), INTENT( INOUT ) :: AvgSrf

IF( AvgSrf%wt > 0. )THEN
  AvgSrf%obs = AvgSrf%obs / AvgSrf%wt
ELSE
  AvgSrf%obs = NOT_SET_R
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION GenAvgPrfVel( InterpPrf,Prf,First )

USE SWIM_fi
USE SWIMobs_fd
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd
USE reallocate

IMPLICIT NONE

TYPE( ObsInterp ), INTENT( IN  ) :: InterpPrf
TYPE( ObsVelPrf ), INTENT( OUT ) :: Prf
TYPE( FirstObs ),  INTENT( IN  ) :: First

INTEGER i, nz, irv

INTERFACE
  INTEGER FUNCTION ReallocateObsVel( First,ObsVel,inc )
    USE SWIM_fi
    TYPE( FirstObs  ), INTENT( IN )    :: First
    TYPE( ObsVelPrf ), INTENT( INOUT ) :: ObsVel
    INTEGER,           INTENT( IN    ) :: inc
  END FUNCTION ReallocateObsVel
END INTERFACE

GenAvgPrfVel = SWIMfailure

nz = 0

DO i = 1,InterpPrf%nz
  IF( InterpPrf%U%obs(i) /= NOT_SET_R )nz = nz + 1
END DO

irv = ReallocateObsVel( First,Prf,-nz )  !Delete previous profiles; then allocate exact size
IF( irv /= SWIMsuccess )GOTO 9999

nz = 0

DO i = 1,InterpPrf%nz
  IF( InterpPrf%U%obs(i) == NOT_SET_R )CYCLE

  nz = nz + 1

  Prf%z(nz) = InterpPrf%z(i)
  Prf%u(nz) = InterpPrf%U%obs(i)
  Prf%v(nz) = InterpPrf%V%obs(i)

  IF( BTEST(First%type,OTB_LSV) )THEN
    IF( InterpPrf%UUL%obs(i) /= NOT_SET_R )THEN
      Prf%LSV%uu(nz) = InterpPrf%UUL%obs(i)
      Prf%LSV%vv(nz) = InterpPrf%VVL%obs(i)
      Prf%LSV%uv(nz) = InterpPrf%UVL%obs(i)
      IF( BTEST(First%type,OTB_LSVL) )Prf%LSV%sl(nz) = InterpPrf%SHL%obs(i)
    ELSE
      Prf%LSV%uu(nz) = NOT_SET_R
      Prf%LSV%vv(nz) = NOT_SET_R
      Prf%LSV%uv(nz) = NOT_SET_R
      IF( BTEST(First%type,OTB_LSVL) )Prf%LSV%sl(nz) = NOT_SET_R
    END IF
  END IF

  IF( BTEST(First%type,OTB_UU) )THEN
    IF( InterpPrf%UU%obs(i) /= NOT_SET_R )THEN
      Prf%BLprof%uu(nz) = InterpPrf%UU%obs(i)
      Prf%BLprof%vv(nz) = InterpPrf%VV%obs(i)
      Prf%BLprof%ww(nz) = InterpPrf%WW%obs(i)
      Prf%BLprof%wt(nz) = InterpPrf%WT%obs(i)
      Prf%BLprof%sl(nz) = InterpPrf%SL%obs(i)
      Prf%BLprof%sz(nz) = InterpPrf%SZ%obs(i)
    ELSE
      Prf%BLprof%uu(nz) = NOT_SET_R
      Prf%BLprof%vv(nz) = NOT_SET_R
      Prf%BLprof%ww(nz) = NOT_SET_R
      Prf%BLprof%wt(nz) = NOT_SET_R
      Prf%BLprof%sl(nz) = NOT_SET_R
      Prf%BLprof%sz(nz) = NOT_SET_R
    END IF
  END IF

END DO

Prf%nz = nz

GenAvgPrfVel = SWIMresult

9999 CONTINUE

RETURN
END
!==============================================================================

INTEGER FUNCTION GenAvgPrf( InterpPrf,AvgPrf,Prf )

USE SWIM_fi
USE SWIMobs_fd
USE SWIMobsInterp_fd
USE SWIMparam_fd
USE default_fd
USE reallocate

IMPLICIT NONE

TYPE( ObsInterp ),    INTENT( IN  ) :: InterpPrf
TYPE( ObsInterpPrf ), INTENT( IN  ) :: AvgPrf
TYPE( ObsPrf ),       INTENT( OUT ) :: Prf

INTEGER i, nz, alloc_stat

GenAvgPrf = SWIMfailure

nz = 0

DO i = 1,InterpPrf%nz
  IF( AvgPrf%obs(i) /= NOT_SET_R )nz = nz + 1
END DO

nz = MAX(nz,1)

DEALLOCATE( Prf%obs,Prf%z,STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

ALLOCATE( Prf%obs(nz),Prf%z(nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

nz = 0

DO i = 1,InterpPrf%nz
  IF( AvgPrf%obs(i) /= NOT_SET_R )THEN
    nz = nz + 1
    Prf%z(nz)   = InterpPrf%z(i)
    Prf%obs(nz) = AvgPrf%obs(i)
  END IF
END DO

Prf%nz = nz

GenAvgPrf = SWIMresult

9999 CONTINUE

RETURN
END
