SUBROUTINE ReadSamplerInput()

!------ Routines to read sensor input (.sam) file, either in "new" SCIP4 format or
!       "old" SCIP32 format

USE scipuff_fi
USE sampler_fi
USE files_fi

IMPLICIT NONE

INTEGER ios

CHARACTER(128) string

!------ Open sampler input file

OPEN( UNIT=lun_tmp,FILE=smpfile,STATUS='OLD',ACTION='READ',FORM='FORMATTED',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error opening sampler file'
  CALL ReportFileName( eInform,'File=',smpfile )
  GOTO  9999
END IF

!------ Read first line to determine input style type (generalized or "old" pre-4.0)

READ( lun_tmp,'(A)',IOSTAT=ios ) string
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error reading sampler file header'
  CALL ReportFileName( eInform,'File=',smpfile )
  GOTO  9999
END IF

CALL cupper( string )

!------ Read file; setup sensor structures

SensorInputStyle : IF( TRIM(string(1:11)) == 'SCIP SENSOR' .OR. &
                       TRIM(string(1:15)) == 'SCIPUFF SENSOR' )THEN

  WRITE(lun_log,FMT='(A)',IOSTAT=ios) 'Reading a SENSOR style sampler input: '//TRIM(smpfile)
  CALL SCIPsensorInput( lun_tmp )  !Generalized sensor input file
  IF( nError /= NO_ERROR )GOTO 9999

  IF( TRIM(string(1:11)) == 'SCIP SENSOR' )THEN
    string = 'SCIP Sensor Output'
  ELSE
    string = 'Sensor Output'
  END IF

ELSE

  WRITE(lun_log,FMT='(A)',IOSTAT=ios) 'Reading an old style sampler input: '//TRIM(smpfile)
  CALL SCIP32sensorInput( lun_tmp )
  IF( nError /= NO_ERROR )GOTO 9999

END IF SensorInputStyle

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SCIPsensorInput( lun )

!------ Read input information for generalized (SCIP40) sensors

USE scipuff_fi
USE met_fi
USE sampler_fi
USE SamplerUtil
USE SamplerGridOuput
USE files_fi
USE los_fd
USE UtilMtlAux
USE reallocate
USE surface_fi
USE ParseLine_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

INTEGER i, j, k, alloc_stat, ios
INTEGER, DIMENSION(:), ALLOCATABLE :: mcID0
LOGICAL lGrid
INTEGER iSampClass
LOGICAL lFirstClass, lOptions

TYPE( SampClassT ), POINTER :: SampClass

!------ Count number of sensors and allocate

nsmp   = 0
string = ' '
nSampClass = 0
lOptions   = .FALSE.

DO   !Read until EOF
  CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
  IF( lerr )EXIT
  IF( c_arg(1) == 'OPTIONS' )THEN
    lOptions = .TRUE.
    DO
      CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
      IF( lerr )EXIT
      CALL cupper( c_arg(1) )
      IF( c_arg(1) == 'END' )EXIT
    END DO
  END IF
  IF( c_arg(1) == 'CLASS' )THEN
    DO
      CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
      IF( lerr )EXIT
      CALL cupper( c_arg(1) )
      IF( c_arg(1) == 'END' )EXIT
      nSampClass = nSampClass + 1
    END DO
    CYCLE
  END IF
  nsmp = nsmp + MAX(nSampClass,1)
  CALL cupper( c_arg(1) )
  IF( c_arg(1) == 'MOVING' )THEN
    DO
      CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
      IF( lerr )EXIT
      CALL cupper( c_arg(1) )
      IF( c_arg(1) == 'END' )EXIT
    END DO
  END IF
END DO

REWIND(lun,ERR=9998)

IF( nsmp > MAXSMP )THEN
  nError   = IV_ERROR
  eRoutine = 'SCIPSensorInput'
  WRITE(eMessage,'("Number of samplers exceed the maximum allowed value of ",I3)') MAXSMP
  eInform = "Use postprocessor to sample the integrated concentration output field"
  GOTO  9999
END IF

!------ Allocate nsmp sensors

CALL AllocateSensor()
IF( nError /= NO_ERROR )GOTO 9999

!------ Check first record keywords, e.g., coordinate system, output format, output times, title

CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
IF( lerr )GOTO 9998

lSmpCnv   = .FALSE.
lWrap     = .FALSE.
lAvg      = .FALSE.
lSmpOutList  = .FALSE.
lSmpOut   = .FALSE.
lGridOut  = .FALSE.

tStartSamp = 0.
tEndSamp   = HUGE(0.)
tolSmpOut  = 0.
dtSmpOut   = 0.
tSmpOut    = 0.

lBinOut   = .FALSE.
lDosSmp   = .FALSE.
lLOSSmp   = .FALSE.
lDepS     = .FALSE.

NULLIFY( FirstSampClass )

lIsSPSOpened    = .FALSE.
lOutputVariance = .FALSE.

!------ Pre-allocate sampler name and units arrays

nvarAlloc = 0
CALL reallocate_smp_vname( nsmp*2 )
IF( nError /= NO_ERROR )GOTO 9999

nsmp0 = nsmp

!------ Parse keywords and corresponding input

IF( lOptions )THEN
  CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
  IF( lerr )GOTO 9998
  IF( TRIM(c_arg(1)) /= 'OPTIONS' )GOTO 9998
END IF

IF( n_arg > 2 .OR. lOptions )THEN
  iarg = 3

  DO !Loop over keywords

    IF( lOptions )THEN
      CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
      IF( lerr )EXIT
      CALL cupper( c_arg(1) )
      IF( c_arg(1) == 'END' )EXIT
      iarg = 1
    END IF

    SELECT CASE( TRIM(c_arg(iarg)) )

      CASE( 'LATLON','LL','UTM','UTM-M','CART','CARTESIAN','KM','KILOMETERS','METERS' ) !Valid map keywords

        CALL SetSampCoord()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE( 'WRAP','SCIP32' )                 !Wrapped output (SCIP3.2 and earlier)
        lWrap = .TRUE.

      CASE( 'SINGLE','SPREADSHEET','NOWRAP' ) !Spread-sheet format (default)
        lWrap = .FALSE.

      CASE( 'ASCII','TEXT' )         !Binary output

        lBinOut = .FALSE.

      CASE( 'BINARY','BIN','BINOUT' )         !Binary output

        lBinOut = .TRUE.
        lWrap   = .FALSE.

      CASE( 'DOS','DOSEGRID','DOSE' )         !Sample DOS fields for CONC:INT sensors

        lDosSmp = .TRUE.

      CASE( 'AVERAGE','AVG','TAVG' )          !Valid keywords denoting time-averaging

        CALL SetSampTavg()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE( 'START' ) !Starting time for sensor calculations

        CALL SetSampStart()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE( 'STOP' ) !Last time for sensor calculations

        CALL SetSampEnd()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE( 'OUTPUT' ) !Valid keywords denoting specific output time

        CALL SetSampOutput()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE( 'TITLE' ) !Optional title; only used for gridded XML output

        CALL SetSampTitle()
        IF( nError /= NO_ERROR )GOTO 9999

      CASE DEFAULT
        WRITE(lun_log,FMT='(A)',IOSTAT=ios) 'Unrecognized sensor header keyword: '//TRIM(c_arg(iarg))

    END SELECT

    IF( .NOT.lOptions )THEN
    iarg = iarg + 1
    IF( iarg > n_arg )EXIT
    END IF

  END DO

END IF

!------ Check time-averaging and specifying output times since they are mutually exclusive

IF( lAvg .AND. lSmpOut )THEN
  nError   = IV_ERROR
  eRoutine = 'SCIPSensorInput'
  eMessage = 'Sampler time-averaging and output times cannot be specified together'
  GOTO  9999
END IF

lOutToSmp = .FALSE.

!------ Setup time-averaging or output times

IF( lAvg .OR. lSmpOut )THEN

  CALL SetSampOutputTimes( lun )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!------ Zero number of output variables and sensors

nvarsmp  = 0
nsmp     = 0

NULLIFY( nsmp_dep )
NULLIFY( nsmp_dos )
ndepblk_smp = 0

!------ Loop over sensors and parse input

SamplerLoop : DO

!------ Read and parse line

  CALL get_next_data( lun,string,nch,kwrd,n_arg,c_arg,MAXN_ARG,lerr )
  IF( lerr )EXIT SamplerLoop      !Error terminates loop; assume end-of-file

  IF( TRIM(c_arg(1)) == 'CLASS' )THEN
    CALL BuildSampClassList( lun )
    IF( nError /= NO_ERROR )GOTO 9999
    CYCLE
  END IF

  nsmp = nsmp + 1

!------ Set sensor type, e.g. moving, los, gridded

  CALL SetSampType( lGrid )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Read sensor horizontal location, optional LOS parameters and/or grid-generation parameters

  CALL SetSampLoc( lGrid )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Read sensor output type and name

  READ(c_arg(iarg+1),'(A)',ERR=9998,END=9998) smp(nsmp)%type
  READ(c_arg(iarg+2),'(A)',ERR=9998,END=9998) smp(nsmp)%var
  IF( iarg+3 <= n_arg )READ(c_arg(iarg+3),'(A)',ERR=9998,END=9998) smp(nsmp)%name

!------ Read waypoints for moving sensor

  IF( BTEST(smp(nsmp)%stype,STB_MOVING) )THEN

    CALL SetWayPoints( lun )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

!------ Check for time-integrated output

  CALL CheckTimeInt( lGrid )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Set for moving sensor output, i.e., position and LOS parameters

  CALL SetMovingOutput()
  IF( nError /= NO_ERROR )GOTO 9999

!------ Check variable based on type

  SELECT CASE( TRIM(smp(nsmp)%type) )

    CASE( 'CONC','CONC_ND' )

      CALL SetConcOutput( lGrid )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'MC' )

      CALL SetMCOutput()
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'DEP' )

      CALL SetDepOutput()
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'DOS' )

      CALL SetDosOutput()
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'MET' )

      CALL SetMetOutput()
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'CORR' )

      CALL SetCorrOutput( lGrid )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( 'BY_SIZE' ,'BY_SIZE_ND',  &
          'BYSIZE'  ,'BYSIZE_ND' ,  &
          'PARTICLE','PARTICLE_ND' )

      CALL SetBySizeOutput( lGrid )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE DEFAULT

      nError   = IV_ERROR
      eRoutine = 'SCIPSensorInput'
      eMessage = 'Invalid sensor input definition'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999

  END SELECT

!------ Allocate output array

  ALLOCATE( smp(nsmp)%dsmp(smp(nsmp)%nvar),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'SCIPSensorInput'
    eMessage = 'Error allocating array for sensor output'
    CALL SensorNumStr( nsmp,eInform )
    GOTO  9999
  END IF
  smp(nsmp)%dsmp(1:smp(nsmp)%nvar) = 0.

  smp(nsmp)%csum = 0.

  IF( BTEST(smp(nsmp)%stype,STB_FXGRID) )THEN

    ALLOCATE( smp(nsmp)%gsmp(smp(nsmp)%nx,smp(nsmp)%ny,smp(nsmp)%nvar),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'SCIPSensorInput'
      eMessage = 'Error allocating array for gridded sensor output'
      CALL SensorNumStr( nsmp,eInform )
      GOTO  9999
    END IF

    DO k = 1,smp(nsmp)%nvar
      DO j = 1,smp(nsmp)%ny
        DO i = 1,smp(nsmp)%nx
          smp(nsmp)%gsmp(i,j,k) = 0.
        END DO
      END DO
    END DO

  END IF

  IF( nvarAlloc-nvarsmp < 10 )THEN
    j = CEILING(FLOAT(nvarsmp)/FLOAT(nsmp))
    j = MAX(j,2)
    i = MAX((nsmp0-nsmp)*j,10)
    CALL reallocate_smp_vname( i )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO SamplerLoop

!------ Check for valid combinations when using gridded output

IF( lGridOut )THEN
  CALL CheckGridOutSamp()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

! Check if compNames are same for all MC samplers
nmc0 = -1
DO i = 1,nsmp
  IF( smp(i)%nmc > 0 )THEN
    IF( nmc0 < 0 )THEN
      nmc0 = smp(i)%nmc
      ALLOCATE( mcID0(nmc0),STAT=alloc_stat )
      mcID0 = smp(i)%mcID
    ELSE
      IF( nmc0 /= smp(i)%nmc )THEN
        nError = IV_ERROR
        EXIT
      ELSE
        IF( .NOT. ALL(mcID0 .EQ. smp(i)%mcID) )THEN
          nError = IV_ERROR
          EXIT
        END IF
      END IF
    END IF
  END IF
END DO
IF( ALLOCATED(mcID0) )DEALLOCATE(mcID0,STAT=alloc_stat)
IF( nError /= 0 )THEN
  eRoutine = 'SCIPSensorInput'
  eMessage = 'Multicomponent species names do not match for all MC samplers'
  eInform  = 'All MC samplers must have the same species list'
  GOTO 9999
END IF
!IF( multicomp .AND. nmc0 == -1 )THEN
!  nError   = IV_ERROR
!  eRoutine = 'SCIPSensorInput'
!  eMessage = 'No MC type sampler found'
!  eInform  = 'At least one MC type sampler must be specified for a multicomponent project'
!  GOTO 9999
!END IF

9999 CONTINUE

CLOSE( lun,IOSTAT=ios )

IF( ALLOCATED(compName) )DEALLOCATE( compName,STAT=alloc_stat )

RETURN

!------ Set read error

9998 CONTINUE
nError   = RD_ERROR
eRoutine = 'SCIPSensorInput'
eMessage = 'Error reading sensor input'
GOTO  9999

END

!==============================================================================

SUBROUTINE SCIP32sensorInput( lun )

!------ Read input information for old style sensor

USE scipuff_fi
USE sampler_fi
USE SamplerUtil
USE files_fi
USE los_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: lun

CHARACTER(128) string

REAL    xs, ys, zs, hx, hy ,hz
INTEGER ios, alloc_stat
INTEGER is, imat, icls, i, nch
LOGICAL lerr

LOGICAL, EXTERNAL :: IsGas, IsParticle
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle

!------ Set defaults

lWrap    = .TRUE.
lAvg     = .FALSE.
lSmpOut  = .FALSE.
lGridOut = .FALSE.
lSmpOutList = .FALSE.
lBinOut   = .FALSE.
nSampClass = 0
NULLIFY( FirstSampClass )
lOutputVariance = .FALSE.
lIsSPSOpened    = .FALSE.

tStartSamp = 0.
tEndSamp   = HUGE(0.)
tolSmpOut  = 0.
dtSmpOut   = 0.
tSmpOut    = 0.

!------ Rewind and read material name

REWIND(lun )

READ( lun,'(A)',IOSTAT=ios ) string
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'SCIP32sensorInput'
  eMessage = 'Error reading sampler file header'
  CALL ReportFileName( eInform,'File=',smpfile )
  GOTO 9999
END IF

!----- Get material name

nch = LEN_TRIM(string)
CALL get_c( string,nch,' ',matname,i,lerr )
IF( lerr )THEN
  nError   = RD_ERROR
  eRoutine = 'read_smp'
  eMessage = 'Error reading sampler material'
  CALL ReportFileName( eInform,'File=',smpfile )
  GOTO 9999
END IF
CALL cupper( matname )

!----- Get material subgroup

IF( nch > 0 )THEN
  READ( string,*,IOSTAT=ios )isg
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error reading sampler subgroup'
    CALL ReportFileName( eInform,'File=',smpfile )
    GOTO 9999
  END IF
ELSE
  isg = 0
END IF

!------ Count number of samplers and allocate

nsmp = 0

DO   !Read until EOF
  READ( lun,*,IOSTAT=ios ) xs,ys,zs
  IF( ios /= 0 )EXIT
  nsmp = nsmp + 1
END DO

REWIND(lun,IOSTAT=ios)
READ(lun,*,IOSTAT=ios)

CALL AllocateSensor()
IF( nError /= NO_ERROR )GOTO 9999

!------ Pre-allocate sampler name and units arrays

nvarAlloc = 0
CALL reallocate_smp_vname( nsmp*3 )
IF( nError /= NO_ERROR )GOTO 9999

!------ Get sampler locations

is = 0

DO   !Read until EOF

  READ( lun,*,IOSTAT=ios ) xs,ys,zs

  ! Check for end of file in all compilers
  IF( IS_IOSTAT_END(ios) )ios = -1

  IF( ios < 0 )EXIT
  IF( ios > 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error reading sampler locations'
    CALL ReportFileName( eInform,'File=',smpfile )
    GOTO 9999
  END IF
  is         = is + 1
  smp(is)%x  = xs
  smp(is)%y  = ys
  smp(is)%zh = zs
  IF( lter )THEN
    CALL get_topogPrj( xs,ys,hz,hx,hy )
    smp(is)%h = hz
    smp(is)%z = zs + hz
  ELSE
    smp(is)%z = zs
  END IF

END DO

nsmp = is

!------ Set puff types

CALL check_smp_matl( matname,isg,itys,itye )
IF( nError /= NO_ERROR )GOTO 9999

!------ Save material class

imat = typeID(itys)%imat
icls = material(imat)%icls

!------ Set output string

string = 'Sampler time histories for '//TRIM(matname)
nch = LEN_TRIM(string)

IF( isg > 0 )THEN

  IF( IsParticle(icls) .OR. IsWetParticle(icls) )THEN
    string = string(1:nch)//' subgroup '
    WRITE(string(nch+11:),'(I3)') isg
  ELSE IF( IsLiquid(icls) )THEN
    IF( isg == 2 )THEN
      string = string(1:nch)//' liquid'
    ELSE
      string = string(1:nch)//' vapor'
    END IF
  END IF

ELSE

  string = string(1:nch)//' total'

END IF

nch  = LEN_TRIM(string)
string = string(1:nch)//' for project '//TRIM(name)

!------ Initialize sampler structures

DO is = 1,nsmp
  smp(is)%time      = 0.
  smp(is)%type      = 'CONC'
  smp(is)%var       = matname
  smp(is)%is        = itys
  smp(is)%ie        = itye
  smp(is)%nvar      = 3
  smp(is)%stype     = 0
  smp(is)%stype     = IBSET(smp(is)%stype,STB_AGL)
  NULLIFY( smp(is)%nextpoint )
  smp(is)%az        = NOT_SET_R
  smp(is)%el        = NOT_SET_R
  smp(is)%dist      = NOT_SET_R
  CALL reallocate_smp_vname( 3 )
  IF( nError /= NO_ERROR )GOTO 9999
  nvarsmp = nvarsmp + 1
  smp_vname(nvarsmp) = 'C'; smp_units(nvarsmp) = TRIM(material(imat)%unit)//'/m3'
  nvarsmp = nvarsmp + 1
  smp_vname(nvarsmp) = 'V'; smp_units(nvarsmp) = TRIM(material(imat)%unit)//'2/m6'
  nvarsmp = nvarsmp + 1
  smp_vname(nvarsmp) = 'T'; smp_units(nvarsmp) = 's'
  ALLOCATE( smp(is)%dsmp(smp(is)%nvar),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'read_smp'
    eMessage = 'Error allocating array for sensor output'
    GOTO 9999
  END IF
  smp(is)%dsmp(1:smp(is)%nvar) = 0.
  smp(is)%csum = 0.
  IF( isg == 0 .AND. (smp(is)%ie > smp(is)%is .OR. IsWetParticle(icls)) )THEN
    smp(is)%stype = IBSET(smp(is)%stype,STB_TOTALVAR)
  END IF

END DO

9999 CONTINUE

CLOSE( lun,IOSTAT=ios )

RETURN
END
