SUBROUTINE init_met()

USE scipuff_fi
USE met_fi
USE files_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE datums
USE SCIPresults_fd
USE landuse_fd
USE charT_fd

IMPLICIT NONE

INTEGER, PARAMETER :: MAXNOBS = 8
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

INTEGER irv, ios, i, n, ndum
REAL    uFixed, vFixed
LOGICAL lexist
REAL    dtOut
INTEGER n_arg, nch
INTEGER j
INTEGER k
LOGICAL lFixed
LOGICAL lerr
LOGICAL lflag

CHARACTER(80)  cmsg, cmsg2, cmsg3
CHARACTER(PATH_MAXLENGTH) string
CHARACTER(4)   kwrd

INTEGER, PARAMETER :: MAX_ARG = 10
CHARACTER(PATH_MAXLENGTH), DIMENSION(MAX_ARG) :: c_arg

CHARACTER(1), DIMENSION(1) :: cdum

TYPE( SWIMinitmet  ) :: initSWIM
TYPE( landuse_init ) :: file_landuse
TYPE( char64T      ) :: lu_string
TYPE( mapCoord )     :: coord

INTEGER, EXTERNAL :: SWIMInitRun, SWIMupdateMet, SWIMinitOutput
INTEGER, EXTERNAL :: SWIMputMixingHt, SWIMgetMixingHt, SWIMgetRefLoc, SetMetGrid
INTEGER, EXTERNAL :: SWIMexitRun, SWIMoutput, julian_day
INTEGER, EXTERNAL :: InitLandUse, ExitLandUse, LandUseCatFromString
INTEGER, EXTERNAL :: SWIMcnvCoord

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: BuildFileName

!------ Initialize met parameters

cmsg  = CHAR(0)
cmsg2 = CHAR(0)
cmsg3 = 'Initializing meteorology parameters'
CALL write_progress( cmsg,cmsg2,cmsg3 )

CALL init_met_params()

NULLIFY( initSWIM%MetSrc,initSWIM%type,initSWIM%prj%MC%z )

lFixed = .FALSE.

!------ Read met scenario file

cmsg  = CHAR(0)
cmsg2 = CHAR(0)
cmsg3 = 'Reading meteorology input file'
CALL write_progress( cmsg,cmsg2,cmsg3 )

cdum = ' '
ndum = 0

CALL ReadWeather( file_msc,lun_msc,cdum,0,ndum )
IF( nError /= NO_ERROR )GOTO 9999

cmsg = TRIM(name)//' : Initializing'
IF( create )THEN
  cmsg2 = 'Validating meteorology input'
ELSE
  cmsg2 = 'Initializing meteorology input'
END IF
cmsg3 = 'Preparing meteorology'
CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ Check output parameters; set lout_mc=T when outputting every time
!       met is updated, otherwise output frequency based on tout_met

IF( lout_met .OR. lout_mc )THEN

  IF( tout_met == NOT_SET_R .OR. tout_met == DEF_VAL_R )THEN
    lout_mc  = .TRUE.
    nout_met = -1
  ELSE
    lout_mc  = .FALSE.
    nout_met = MAX(NINT(tout_met/delt),1)
  END IF
  lout_met = .TRUE.

  IF( .NOT.create )THEN
    IF( t == 0. )THEN
      timeOutMet = NOT_SET_R
    ELSE
      timeOutMet = t
    END IF
  ELSE
    timeOutMet = NOT_SET_R
  END IF

END IF

!------ Initialize mass-consistency if required

IF( .NOT.create )mcTypeMet = mcTypePrj

!------ Check if reference location must be set for UTM project

IF( lmap == I_UTM )THEN
  lflag = lmc_ua
  SELECT CASE( TRIM(met_type) )
    CASE( 'MEDOC','MRF','GRIDDED','ASSIM','SCIPUFF_LIST','WRF' )
      lflag = .TRUE.
    CASE( 'MEDOC_LIST' )
      lflag = .TRUE.
  END SELECT
  IF( .NOT.lflag )THEN
    IF( xmin /= NOT_SET_R .AND. xmin /= DEF_VAL_R .AND. &
        xmax /= NOT_SET_R .AND. xmax /= DEF_VAL_R .AND. &
        ymin /= NOT_SET_R .AND. ymin /= DEF_VAL_R .AND. &
        ymax /= NOT_SET_R .AND. ymax /= DEF_VAL_R )THEN
      xref = 0.5*(xmin+xmax)
      yref = 0.5*(ymin+ymax)
      irv = UTM2LL( utm_zone,xref,yref,lat0,lon0 )
      IF( irv /= 0 )THEN
        CALL setUTMerror( 'UTM2LL',irv )
        GOTO 9999
      END IF
    END IF
  END IF
END IF

!------ Set landuse category index from string

file_landuse%lun  = 0
file_landuse%file = TRIM(file_lus)

irv = InitLandUse( file_landuse )
IF( irv /= 1 )THEN
  CALL getLandUseError()
  GOTO 9999
END IF

lu_string%string = TRIM(landuse)

i_cat = LandUseCatFromString( lu_string )

irv = ExitLandUse()
IF( irv /= 1 )THEN
  CALL getLandUseError()
  GOTO 9999
END IF

!------ checks for boundary layer files, variables, compatibility, etc.

IF( lbl )THEN

  CALL check_bl()
  IF( nError /= NO_ERROR )GOTO 9999

ELSE

!------ check for boundary layer if surface evaporation flag is set

  IF( evaporation )THEN
    nError   = UK_ERROR
    eRoutine = 'init_met'
    eMessage = 'No surface evaporation allowed without boundary layer'
    eInform  = 'Set a type of boundary layer'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

END IF

!------ Set boundary layer type for SWIM

SELECT CASE( TRIM(bl_type) )
  CASE( 'SBL' )
    initSWIM%prj%BL%type = BLP_SBL
  CASE( 'OPER' )
    initSWIM%prj%BL%type = BLP_OPER
  CASE( 'OBS','MEDOC' )
    initSWIM%prj%BL%type = BLP_MET
  CASE( 'CALC' )
    initSWIM%prj%BL%type = BLP_CALC
  CASE( 'PROF' )
    initSWIM%prj%BL%type = BLP_PROF
  CASE( 'NONE' )
    initSWIM%prj%BL%type = BLP_NONE
  CASE DEFAULT
    initSWIM%prj%BL%type = BLP_OPER
END SELECT

!------ Set large-scale variability

SELECT CASE( TRIM(ensm_type) )
  CASE( 'OFF','NONE' )
    initSWIM%prj%LSVtype = LVP_OFF
  CASE( 'INPUT' )
    initSWIM%prj%LSVtype = LVP_INPUT
  CASE( 'MODEL' )
    initSWIM%prj%LSVtype = LVP_MODEL
  CASE( 'OBS' )
    initSWIM%prj%LSVtype = LVP_MET
  CASE( 'OPER' )
    initSWIM%prj%LSVtype = LVP_OPER
END SELECT

initSWIM%prj%LSVuu    = uu_ensm
initSWIM%prj%LSVscale = sl_ensm
initSWIM%prj%epstrop  = epstrop

!------ Set met input sources for SWIM

SELECT CASE( met_type )

  CASE( 'OBS' )

    n = 0
    IF( lsfc )n = n + 1
    IF( lua  )n = n + 1

    initSWIM%nMetSrc = n
    ALLOCATE( initSWIM%MetSrc(n),initSWIM%type(n),STAT=ios )
    IF( ios /= 0 )THEN
      nError = UK_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error allocating SWIM input'
      GOTO 9999
    END IF

    n = 0

    IF( lsfc )THEN
      n = n + 1
      initSWIM%MetSrc(n) = TRIM(file_sfc)
      initSWIM%type(n)   = SWIMobs
    END IF

    IF( lua )THEN
      n = n + 1
      initSWIM%MetSrc(n) = TRIM(file_met)
      initSWIM%type(n)   = SWIMobs
    END IF

  CASE( 'FIXED' )

    ALLOCATE( initSWIM%MetSrc(1),initSWIM%type(1),STAT=ios )
    IF( ios /= 0 )THEN
      nError = UK_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error allocating SWIM input'
      GOTO 9999
    END IF
    initSWIM%nMetSrc = 1
    initSWIM%type(1) = SWIMfixed

    CALL SetFixedWind( uFixed,vFixed )
    WRITE( initSWIM%MetSrc(1),* ) uFixed,vFixed
    lFixed = .TRUE.

  CASE( 'ASSIM','SCIPUFF_LIST' )

    OPEN(UNIT=lun_tmp,FILE=file_met,STATUS='OLD',FORM='FORMATTED',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error opening meteorology list input file'
      GOTO 9999
    END IF

    READ(lun_tmp,FMT='(A)',IOSTAT=ios) string
    IF( ios /= 0 )THEN
      nError   = RD_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error reading meteorology list input file'
      GOTO 9999
    END IF

    CALL cupper( string )
    SELECT CASE( TRIM(ADJUSTL(string)) )
      CASE( 'ASSIM','SCIPUFF_LIST','SCICHEM_LIST','MET_LIST','LIST' )
      CASE DEFAULT
      nError   = IV_ERROR
      eRoutine = 'init_met'
      eMessage = 'Invalid format for Met Assimilation input file'
      GOTO 9999
    END SELECT

!------ Get number of met input files

    n = 0

    DO
      READ(lun_tmp,FMT='(A)',IOSTAT=ios) string

      ! Check for end of file in all compilers
      IF( IS_IOSTAT_END(ios) )ios = -1

      IF( ios == 0 )THEN
        IF( LEN_TRIM(string) <= 1 .OR. string(1:1) == '#' )CYCLE
        n = n + 1
      ELSE IF( ios < 0 )THEN
        REWIND(lun_tmp,IOSTAT=ios)  !Assumed to be EOF
        EXIT
      ELSE
        nError   = RD_ERROR
        eRoutine = 'init_met'
        eMessage = 'Error reading Met Assimilation input file'
        GOTO 9999
      END IF
    END DO

!------ Allocate for multiple input files

    initSWIM%nMetSrc = n
    ALLOCATE( initSWIM%MetSrc(n),initSWIM%type(n),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error allocating SWIM input'
      GOTO 9999
    END IF

    READ(lun_tmp,*,IOSTAT=ios) !Skip 'ASSIM' line

!------ Set met field types and file names

    DO i = 1,n

      c_arg = ' '

#if defined(LINUX)
      CALL get_next_data_NO_CUPPER( lun_tmp,string,nch,kwrd,n_arg,c_arg,MAX_ARG,lerr )
#else
      CALL get_next_data( lun_tmp,string,nch,kwrd,n_arg,c_arg,MAX_ARG,lerr )
#endif
      IF( lerr )THEN
        nError   = RD_ERROR
        eRoutine = 'init_met'
        eMessage = 'Error reading met assimilation input file'
        eInform  = 'Line must contain file type and name'
        GOTO 9999
      END IF

      CALL cupper( c_arg(1) )

      SELECT CASE( TRIM(c_arg(1)) )
        CASE( 'GRIDDED','MEDOC','MRF','SCIP','GRIDMC' )
          initSWIM%type(i)   = SWIMgrid !Generic gridded file
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,''))

          CALL CheckHresDefault( hres )
          IF( nError /= NO_ERROR )GOTO 9999

        CASE( 'WRF' )
          initSWIM%type(i)   = SWIMWRF
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,'')) !Point to another list file

          CALL CheckHresDefault( hres )
          IF( nError /= NO_ERROR )GOTO 9999

        CASE( 'MEDOC_LIST','MEDOCLIST' )
          initSWIM%type(i)   = SWIMMEDLIST
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,'')) !Point to another list file

          CALL CheckHresDefault( hres )
          IF( nError /= NO_ERROR )GOTO 9999

        CASE( 'PRF','SFC','OBS' )
          initSWIM%type(i)   = SWIMobs
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,''))

        CASE( 'AERSFC' )
          initSWIM%type(i)   = SWIMAERsfc
          initSWIM%MetSrc(i) = QQ//TRIM(BuildFileName( c_arg(2),string,''))//QQ !TRIM(c_arg(2))

          IF( c_arg(2)(1:1) == QQ )THEN
            k = 0
            DO j = 2,n_arg
              IF( INDEX(c_arg(j),QQ) > 0 )k = j+1
            END DO
          ELSE
            k = 3
          END IF

          IF( n_arg >= k )THEN
            DO j = k,n_arg
              initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//TRIM(c_arg(j))
            END DO
          END IF

        CASE( 'AERPFL' )
          initSWIM%type(i)   = SWIMAERpfl
          initSWIM%MetSrc(i) = QQ//TRIM(BuildFileName( c_arg(2),string,''))//QQ !TRIM(c_arg(2))

          IF( c_arg(2)(1:1) == QQ )THEN
            k = 0
            DO j = 2,n_arg
              IF( INDEX(c_arg(j),QQ) > 0 )k = j+1
            END DO
          ELSE
            k = 3
          END IF

          IF( n_arg >= k )THEN
            DO j = k,n_arg
              initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//TRIM(c_arg(j))
            END DO
          END IF

        CASE( 'ASOS1MIN' )
          initSWIM%type(i)   = SWIMASOS1M
          initSWIM%MetSrc(i) = QQ//TRIM(BuildFileName(c_arg(2),string,''))//QQ

          IF( c_arg(2)(1:1) == QQ )THEN  !Look for possible 2nd file name

            k = 0
            DO j = 3,n_arg
              IF( INDEX(c_arg(j)(1:1),QQ) > 0 )THEN
                k = J; EXIT
              END IF
            END DO
            IF( k > 0 )THEN
              string = ''
              DO j = k,n_arg
                string = TRIM(string)//' '//TRIM(c_arg(j))
              END DO
              initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//QQ//TRIM(BuildFileName(c_arg(k),string,''))//QQ
            END IF

            IF( n_arg >= 4 )THEN
              k = 2
              DO j = n_arg,2,-1
                n = LEN_TRIM(c_arg(j))
                IF( INDEX(c_arg(j)(n:n),QQ) > 0 )THEN
                  k = j; EXIT
                END IF
              END DO
              IF( n_arg == k+2 )initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//TRIM(c_arg(k+1))//' '//TRIM(c_arg(k+2))
            END IF

          ELSE  !Quotes not used for file names

            IF( n_arg == 3 .OR. n_arg == 5 )initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//QQ//TRIM(c_arg(3))//QQ
            IF( n_arg == 4 .OR. n_arg == 5 )initSWIM%MetSrc(i) = TRIM(initSWIM%MetSrc(i))//' '//TRIM(c_arg(n_arg-1))//' '//TRIM(c_arg(n_arg))

          END IF

        CASE( 'VERTGRID','VRTGRID','VGRID','VGRD' )
          initSWIM%type(i)   = SWIMvrtGrid
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,''))

        CASE( 'TER' )
          initSWIM%type(i)   = SWIMterAssm
          initSWIM%MetSrc(i) = TRIM(BuildFileName( c_arg(2),string,''))

        CASE( 'NEST','SUBDOMAIN' )
          initSWIM%type(i)   = SWIMterAssm
          initSWIM%MetSrc(i) = ADJUSTL(TRIM(string))

        CASE( 'FIXED' )
          initSWIM%type(i) = SWIMfixed

          IF( n_arg < 2 )THEN
            nError   = IV_ERROR
            eRoutine = 'init_met'
            eMessage = 'Error reading met list input'
            eInform  = 'Fixed input requires at least speed & direction'
            GOTO 9999
          END IF

          k = 1; IF( c_arg(2)(1:1) == QQ )k = 2
          READ( c_arg(2)(k:LEN_TRIM(c_arg(2))),*,IOSTAT=ios) fixed_spd
          IF( ios /= 0 )THEN
            nError   = RD_ERROR
            eRoutine = 'init_met'
            eMessage = 'Error reading fixed wind speed from met list input'
            GOTO 9999
          END IF

          k = LEN_TRIM(c_arg(3)); IF( c_arg(3)(k:k) == QQ )k = k-1
          READ( c_arg(3)(1:k),*,IOSTAT=ios) fixed_dir
          IF( ios /= 0 )THEN
            nError   = RD_ERROR
            eRoutine = 'init_met'
            eMessage = 'Error reading fixed wind direction from met list input'
            GOTO 9999
          END IF

          IF( n_arg > 3 )THEN
            k = LEN_TRIM(c_arg(4)); IF( c_arg(4)(k:k) == QQ )k = k-1
            READ( c_arg(4)(1:k),*,IOSTAT=ios) unit_spd
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading fixed wind speed units from met list input'
              GOTO 9999
            END IF
          ELSE
            unit_spd = 1
          END IF

          CALL SetFixedWind( uFixed,vFixed )
          WRITE( initSWIM%MetSrc(1),* ) uFixed,vFixed

          lFixed = .TRUE.

        CASE( 'R_OBS' )
          IF( n_arg > 1 )THEN
            initSWIM%type(i) = ASSIM_R_OBS
            WRITE(initSWIM%MetSrc(i),'(A)',IOSTAT=ios) TRIM(c_arg(2))
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading assimilation parameter R_OBS'
              GOTO 9999
            END IF
          ELSE
            initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          END IF

        CASE( 'E2BO' )
          IF( n_arg > 1 )THEN
            initSWIM%type(i) = ASSIM_E2BO
            WRITE(initSWIM%MetSrc(i),'(A)',IOSTAT=ios)  TRIM(c_arg(2))
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading assimilation parameter E2BO'
              GOTO 9999
            END IF
          ELSE
            initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          END IF

        CASE( 'DU2EPS','ASSIM_EPS' )
          IF( n_arg > 1 )THEN
            initSWIM%type(i) = ASSIM_DU2EPS
            WRITE(initSWIM%MetSrc(i),'(A)',IOSTAT=ios)  TRIM(c_arg(2))
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading assimilation parameter DU2EPS'
              GOTO 9999
            END IF
          ELSE
            initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          END IF

        CASE( 'SCM_MAXITER','ASSIM_MAXITER' )
          IF( n_arg > 1 )THEN
            initSWIM%type(i) = ASSIM_SCM_MAXITER
            WRITE(initSWIM%MetSrc(i),'(A)',IOSTAT=ios)  TRIM(c_arg(2))
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading assimilation parameter SCM_MAXITER'
              GOTO 9999
            END IF
          ELSE
            initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          END IF

        CASE( 'MCWIFWTFAC','MC_OBSWTFAC' )
          IF( n_arg > 1 )THEN
            initSWIM%type(i) = ASSIM_MCWIFWtFac
            WRITE(initSWIM%MetSrc(i),'(A)',IOSTAT=ios) TRIM(c_arg(2))
            IF( ios /= 0 )THEN
              nError   = RD_ERROR
              eRoutine = 'init_met'
              eMessage = 'Error reading assimilation parameter MCWIFWtFac'
              GOTO 9999
            END IF
          ELSE
            initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          END IF

        CASE DEFAULT
          initSWIM%type(i) = -1  !Set to skip in SWIMinitRun
          CYCLE

      END SELECT

    END DO

    CLOSE(UNIT=lun_tmp,IOSTAT=ios)

  CASE DEFAULT !( 'GRIDDED','MEDOC','MRF','GRIDMC','CLI3D','CLI3DMC', etc. )

    CALL CheckHresDefault( hres )
    IF( nError /= NO_ERROR )GOTO 9999

    ALLOCATE( initSWIM%MetSrc(1),initSWIM%type(1),STAT=ios )
    IF( ios /= 0 )THEN
      nError = UK_ERROR
      eRoutine = 'init_met'
      eMessage = 'Error allocating SWIM input'
      GOTO 9999
    END IF
    initSWIM%nMetSrc = 1
    initSWIM%type(1) = SWIMgrid !Generic gridded file

    initSWIM%MetSrc(1) = TRIM(file_met)

END SELECT

!------ Check if fixed wind is inappropriate

IF( lFixed )THEN
  IF( initSWIM%nMetSrc > 1 )THEN
    DO i = 1,initSWIM%nMetSrc
      IF( initSWIM%type(i) == SWIMfixed   )CYCLE
      IF( initSWIM%type(i) == SWIMterAssm )CYCLE
      IF( initSWIM%type(i) == SWIMvrtGrid )CYCLE
      nError   = IV_ERROR
      eRoutine = 'init_met'
      eMessage = 'Invalid combination of met input with fixed winds'
      eInform  = 'Only terrain and/or vertical grid may be specified'
      GOTO 9999
    END DO
  END IF
END IF

!---- Check Hres for terrain input

IF( lmc_ua .AND. LEN_TRIM(file_ter) /= 0 )THEN
  CALL CheckHresDefault( hres )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Set basic project parameters for SWIM

initSWIM%prj%prjName = TRIM(file_prj)
CALL RemoveExtension( initSWIM%prj%prjName )

initSWIM%prj%coord   = lmap
initSWIM%prj%Xmin    = xmin
initSWIM%prj%Xmax    = xmax
initSWIM%prj%Ymin    = ymin
initSWIM%prj%Ymax    = ymax
initSWIM%prj%Zmax    = zmax
initSWIM%prj%hres    = hres
initSWIM%prj%Xref    = xref
initSWIM%prj%Yref    = yref
initSWIM%prj%Lat0    = lat0
initSWIM%prj%Lon0    = lon0
initSWIM%prj%UTMzone = utm_zone
initSWIM%prj%Hmin    = 0.  !hmin - could be used in future for
                           !elevation in the absence of a terrain file
initSWIM%prj%local      = local
initSWIM%prj%timeBin    = tbin_met
initSWIM%prj%timeZone   = tzone

initSWIM%prj%time    = t
initSWIM%prj%timeEnd = t + tend_r

initSWIM%prj%hourStart = tstart

initSWIM%prj%dayStart   = day_start
initSWIM%prj%monthStart = month_start
initSWIM%prj%yearStart  = year_start
initSWIM%prj%julStart   = jul_start

initSWIM%prj%dayEnd   = day_end
IF( BTEST(run_mode,REVERSE_MODE) )initSWIM%prj%dayEnd = -1    !Special flag for reverse calculation
initSWIM%prj%monthEnd = month_end
initSWIM%prj%yearEnd  = year_end

initSWIM%prj%localMet = local_met

initSWIM%prj%MAX1D_MET  = MAX1D_MET

initSWIM%prj%lOut3D = lout_3d .AND. lout_met
initSWIM%prj%lOut2D = lout_2d .AND. lout_met

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  initSWIM%prj%lOut3D = .FALSE.
  initSWIM%prj%lOut2D = .FALSE.
  lout_met            = .FALSE.
END IF

initSWIM%prj%OutFile = TRIM(file_mcw)

n = LEN_TRIM(initSWIM%prj%OutFile)
initSWIM%prj%OutFile = initSWIM%prj%OutFile(1:n-3)//'mcw'

initSWIM%prj%lCreateOut = t == 0.   !Delete existing file(s) at start of run
initSWIM%prj%lFormat    = lformat

IF( n_sfc_max /= DEF_VAL_I .AND. n_obs_max /= DEF_VAL_I )THEN
  initSWIM%prj%maxObs = MAX(n_sfc_max,n_obs_max)
ELSE IF( n_sfc_max /= DEF_VAL_I )THEN
  initSWIM%prj%maxObs = n_sfc_max
ELSE IF( n_obs_max /= DEF_VAL_I )THEN
  initSWIM%prj%maxObs = n_obs_max
ELSE
  initSWIM%prj%maxObs = MAXNOBS
END IF
IF( initSWIM%prj%maxObs < 1 )initSWIM%prj%maxObs = MAXNOBS

initSWIM%prj%BL%nzbl    = nzbl
initSWIM%prj%BL%UUcalm  = uu_calm
initSWIM%prj%BL%SLcalm  = sl_calm
initSWIM%prj%BL%WWtrop  = wwtrop
initSWIM%prj%BL%SLtrop  = sltrop
initSWIM%prj%BL%zruf    = zruf
initSWIM%prj%BL%hc      = MAX(h_cnp,0.)
initSWIM%prj%BL%alpha   = alpha_cnp
initSWIM%prj%BL%Bowen   = bowen
initSWIM%prj%BL%albedo  = albedo
initSWIM%prj%BL%cc      = cloud_cover
initSWIM%prj%BL%pr_type = pr_rate
initSWIM%prj%BL%i_wet   = i_wet
initSWIM%prj%BL%i_cat   = i_cat

initSWIM%prj%BL%HFLXmin = hconst / RHOCP
initSWIM%prj%BL%HFLXmax = hdiur  / RHOCP
initSWIM%prj%BL%ZImin   = zimin
initSWIM%prj%BL%ZImax   = zimax

!------ Set mass-consistent parameters

IF( lmc_ua )THEN
  initSWIM%prj%MC%type = mcTypeMet
ELSE
  initSWIM%prj%MC%type = 0
END IF

initSWIM%prj%MC%TerFile      = TRIM(file_ter)
initSWIM%prj%MC%LandUseFile  = TRIM(file_lus)
initSWIM%prj%MC%MaxIterRelax = max_iter_ac
initSWIM%prj%MC%MaxIterFFT   = max_iter
initSWIM%prj%MC%alphaMax     = alpha_max
initSWIM%prj%MC%alphaMin     = alpha_min
initSWIM%prj%MC%epsRelax     = ac_eps
initSWIM%prj%MC%epsFFT       = p_eps
initSWIM%prj%MC%nz           = nzMC

IF( nzMC > 0 )THEN
  ALLOCATE( initSWIM%prj%MC%z(nzMC),STAT=ios )
  initSWIM%prj%MC%z = zMC
END IF

initSWIM%prj%create = create
initSWIM%prj%decay  = ldecay .OR. multicomp

!------ Jump in and SWIM

irv = SWIMInitRun( initSWIM )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'init_met' )

!------ Check error for reverse mode: clear SWIM and check for/generate MEDOC file

  IF( BTEST(run_mode,REVERSE_MODE) .AND. nError == RV_ERROR )THEN

    CALL init_error()
    irv = SWIMexitRun()

!------ Check if output MEDOC file exists

    INQUIRE( FILE=TRIM(initSWIM%prj%OutFile),EXIST=lexist )

!------ MEDOC file does not exist: setup SWIM to generate one

    IF( .NOT.lexist )THEN

!------ Setup start time by subtracting run duration

      CALL year_month_day( tstart-tend_hr,year_start,month_start,day_start, &
                           initSWIM%prj%hourStart,  &
                           initSWIM%prj%yearStart,  &
                           initSWIM%prj%monthStart, &
                           initSWIM%prj%dayStart )

      initSWIM%prj%julStart = julian_day( initSWIM%prj%monthStart, &
                                          initSWIM%prj%dayStart,   &
                                          initSWIM%prj%yearStart )

      initSWIM%prj%dayEnd  = day_start
      initSWIM%prj%lFormat = .FALSE.
      initSWIM%prj%lOut3d  = .TRUE.
      initSWIM%prj%lOut2d  = .TRUE.
      initSWIM%prj%Create  = .FALSE.

      irv = SWIMInitRun( initSWIM )
      IF( irv /= SWIMsuccess )THEN
        CALL setSWIMerror( 'init_met' )
        GOTO 9999
      END IF

      t = 0.
      timeOutMet = -HUGE(0.)

!------ Set met output increment (from user input; default max is 1 hour)

      IF( tout_met == NOT_SET_R .OR. tout_met == DEF_VAL_R )THEN
        dtOut  = 3600.
      ELSE
        dtOut = MIN(tout_met,3600.)
      END IF
      dtOut = MAX(dtOut,delt)

!------ Initialize output

      irv = SWIMinitOutput()
      IF( irv /= SWIMsuccess )GOTO 9999

!------ Loop over time; output whenever met updated (from source) or next timeOutMet
!       increment, whichever comes first

      DO

        irv = SWIMupdateMet( t,SWIMstatus )
        CALL enableSCIPUFFhalt( SCIPUFF_HALT )      !Show just abort button
        IF( irv /= SWIMsuccess )THEN
          CALL setSWIMerror( 'SWIMupdateMet' )
          GOTO 9999
        END IF

        IF( BTEST(SWIMstatus,SSB_NEWMET) .OR. t >= timeOutMet )THEN
          irv = SWIMoutput()
          IF( irv /= SWIMsuccess )THEN
            CALL setSWIMerror( 'init_met' )
            GOTO 9999
          END IF
          timeOutMet = t + dtOut
          CALL WriteSWIMlog()
        END IF

!------ Increment time and check for end

        t = t + delt
        IF( t > tend_r )EXIT

      END DO

!------ Done generating MEDOC file(s); clear SWIM and reset input structure and SCIPUFF time

      irv = SWIMexitRun()

      t = initSWIM%prj%time
      timeOutMet = NOT_SET_R

      initSWIM%prj%hourStart  = tstart
      initSWIM%prj%dayStart   = day_start
      initSWIM%prj%monthStart = month_start
      initSWIM%prj%yearStart  = year_start
      initSWIM%prj%julStart   = jul_start

      initSWIM%prj%dayEnd = -1    !Re-set for reverse calculation
      initSWIM%prj%Create = create

    END IF

!------ Change SWIM input structure to use MEDOC file

    initSWIM%nMetSrc   = 1
    initSWIM%MetSrc(1) = TRIM(initSWIM%prj%OutFile)
    initSWIM%type(1)   = SWIMgrid

    initSWIM%prj%BL%type = BLP_MET
    initSWIM%prj%lOut3d  = .FALSE.
    initSWIM%prj%lOut2d  = .FALSE.

    initSWIM%prj%MC%type = 0

    irv = SWIMInitRun( initSWIM )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'init_met' )
      GOTO 9999
    END IF

  ELSE  !Error from SWIMinitRun not associated with Reverse calculation

    GOTO 9999

  END IF

END IF

!------ Take note of SWIM changes to domain

xmin = initSWIM%prj%Xmin
xmax = initSWIM%prj%Xmax
ymin = initSWIM%prj%Ymin
ymax = initSWIM%prj%Ymax

CALL WriteSWIMlog()

!------ Save MC type

IF( .NOT.BTEST(run_mode,REVERSE_MODE) )mcTypePrj = initSWIM%prj%MC%type

!------ Get reference location (since may have been modified in SWIM)

irv = SWIMgetRefLoc( xref,yref,lat0,lon0 )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Define reference location if not defined (UTM (since may have been modified in SWIM)
!       to allow plots in lat/lon
IF( .NOT.create .AND. lat0 == NOT_SET_R .AND. lmap == I_UTM )THEN
  xref = 0.5*(xmin+xmax)
  yref = 0.5*(ymin+ymax)
  coord = PrjCoord
  coord%type = I_LATLON
  irv = SWIMcnvCoord( xref,yref,PrjCoord,lon0,lat0,coord )
END IF

!------ Move saved zi field to SWIM for 'real' restart

IF( t > 0. .AND. numMet > 0 )THEN
  DO i = 1,numMet
    irv = SWIMputMixingHt( i,MetGrid(i) )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMputMixingHt' )
      GOTO 9999
    END IF
  END DO
END IF

!------ Initialize SWIM met

IF( .NOT.create )THEN

  irv = SWIMupdateMet( t,SWIMstatus )
  CALL enableSCIPUFFhalt( SCIPUFF_HALT )      !Show just abort button
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMupdateMet' )
    GOTO 9999
  END IF

  IF( t == 0. .AND. BTEST(SWIMstatus,SSB_EXPAND) )THEN
    irv = SetMetGrid()
    IF( irv /= SCIPsuccess )GOTO 9999
    DO i = 1,numMet
      irv = SWIMgetMixingHt( i,MetGrid(i) )
      IF( irv /= SWIMsuccess )THEN
        CALL setSWIMerror( 'SWIMgetMixingHt' )
        GOTO 9999
      END IF
    END DO
  END IF

!------ Initialize output

  irv = SWIMinitOutput()
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMinitOutput' )
    GOTO 9999
  END IF

END IF

CALL WriteSWIMlog()

9999 CONTINUE

IF( ASSOCIATED(initSWIM%MetSrc)   )DEALLOCATE( initSWIM%MetSrc,  STAT=ios )
IF( ASSOCIATED(initSWIM%type)     )DEALLOCATE( initSWIM%type,    STAT=ios )
IF( ASSOCIATED(initSWIM%prj%MC%z) )DEALLOCATE( initSWIM%prj%MC%z,STAT=ios )

INQUIRE(UNIT=lun_tmp,OPENED=lerr)
IF( lerr )CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN
END

!===============================================================================

SUBROUTINE setSWIMerror( routine )

USE error_fi
USE message_fd
USE files_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine

TYPE( messageT ) :: SWIM_error

CALL SWIMreportError( SWIM_error )

CALL WriteSWIMlog()

IF( SWIM_error%iParm /= NO_ERROR )THEN
  nError = SWIM_error%iParm
  WRITE(lun_log,*)' SWIM reported error no. ',nError
ELSE
  WRITE(lun_log,*)' SWIM reported NO_ERROR'
  nError = UK_ERROR
END IF
eRoutine = TRIM(routine)//':'//TRIM(SWIM_error%routine)
eMessage = TRIM(SWIM_error%aString)
eInform  = TRIM(SWIM_error%bString)
eAction  = TRIM(SWIM_error%cString)

RETURN
END

!==============================================================================

SUBROUTINE SetFixedWind( uFixed,vFixed )

!------ Set fixed wind components

USE met_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( OUT ) :: uFixed, vFixed

REAL spd, ang

SELECT CASE( unit_spd )
  CASE( 1 )      !m/s
    spd = 1.0
  CASE( 2 )       !knots
    spd = 0.51444444
  CASE( 3 )       !mph
    spd = 0.44704
  CASE( 4 )       !kph
    spd = 0.27777778
  CASE( 5 )       !ft/sec
    spd = 0.3048
  CASE DEFAULT    !assume m/s
    spd = 1.0
END SELECT

spd = spd * fixed_spd
ang = fixed_dir * PI180

uFixed = -spd * SIN(ang)
vFixed = -spd * COS(ang)

RETURN
END

!===============================================================================

SUBROUTINE set_year( iyy )

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

SUBROUTINE CheckHresDefault( hres )

USE error_fi
USE default_fd

IMPLICIT NONE

REAL, INTENT( INOUT ) :: hres

IF( hres /= DEF_VAL_R )THEN
  nError   = IV_ERROR
  eRoutine = 'init_met'
  eMessage = 'Invalid Horizontal Resolution'
  eInform  = 'Must be set to DEFAULT for gridded input'
  eAction  = 'Do you want SCIPUFF to reset to DEFAULT and continue?'
  CALL WarningMessage( .TRUE. )
  IF( nError /= NO_ERROR )THEN
    nError = WN_ERROR
    GOTO 9999
  ELSE
    hres = DEF_VAL_R
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE check_bl()

USE scipuff_fi
USE met_fi
USE files_fi
USE landuse_fd

IMPLICIT NONE

!------ Make sure zruf is set (since it is always required if lbl is true)
!       unless landuse category is set

IF( zruf /= DEF_VAL_R .AND. &
    zruf /= NOT_SET_R .AND. &
    zruf /= DEFERRED_R )THEN
  IF( zruf == 0. .AND. (i_cat == NOT_SET_I .or. i_cat == 0)  )THEN
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'ZRUF must be greater than zero with any BL_TYPE'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF
END IF

IF( h_cnp == 0. )THEN
  nError   = IV_ERROR
  eRoutine = 'check_bl'
  eMessage = 'Canopy height cannot equal zero'
  CALL ReportFileName( eInform,'File=',file_msc )
  GOTO 9999
END IF

!------ check some variables for bl_type=CALC

IF( bl_type == 'CALC' )THEN

  IF( cloud_cover == NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set CLOUD COVER for boundary layer calculation'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

!-------- check if lat/lon info is available

  IF( ( (lmap == I_CARTESIAN) .OR. & !(lmap == I_UTM     ) .OR. &
        (lmap == I_METERS  ) ) .AND. &
        (lat0 == NOT_SET_R .OR. lon0 == NOT_SET_R) )THEN
    nError   = UK_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must define map reference location for a UTM' &
               //' or Cartesian run when using BL_TYPE='//TRIM(bl_type)
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

!-------- need year-month-day-hour for calculated boundary layer

  IF( .NOT.lymd )THEN
    nError   = UK_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Incorrect time specification for BL_TYPE='//bl_type
    eInform  = 'Must use year-month-day-hour'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

ELSE IF( cloud_cover == NOT_SET_R )THEN

  cloud_cover = 0. !Default for bl_type /= 'CALC'

END IF

!------ check SBL variables

IF( bl_type == 'SBL' )THEN

!------ check zi

  IF( zimin == NOT_SET_R .OR. zimax == NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set ZIMIN, ZIMAX for boundary layer'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

!------ check heat flux

  IF( hconst == NOT_SET_R .OR. hdiur == NOT_SET_R )THEN
    nError   = IV_ERROR
    eRoutine = 'check_bl'
    eMessage = 'Must set HCONST, HDIUR for boundary layer'
    CALL ReportFileName( eInform,'File=',file_msc )
    GOTO 9999
  END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE init_met_params()

USE scipuff_fi
USE met_fi
USE files_fi

IMPLICIT NONE

!------ initialize all met field parameters

tbin_met       = NOT_SET_R
lua            = .FALSE.
lbl            = .FALSE.
lensm          = .FALSE.
lsv_oper       = .FALSE.
lsfc           = .FALSE.
lformat        = .FALSE.
lmc_ua         = .FALSE.
mcTypeMet      = mcTypePrj
local_met      = .FALSE.
sl_haz         = NOT_SET_R
met_type       = NOT_SET_C
bl_type        = NOT_SET_C
ensm_type      = NOT_SET_C
zimin          = NOT_SET_R
zimax          = NOT_SET_R
hconst         = NOT_SET_R
hdiur          = NOT_SET_R
h_cnp          = -1.0
alpha_cnp      = 0.0
zruf           = NOT_SET_R
bowen          = NOT_SET_R
albedo         = NOT_SET_R
cloud_cover    = NOT_SET_R
pr_type        ='NONE'
pr_rate        = NOT_SET_R
tprcp          = NOT_SET_R
tprcpn         = NOT_SET_R
lwash          = .TRUE.
hmin           = 0.
alpha_min      = 0.
alpha_max      = 1.
max_iter_ac    = 10000
max_iter       = 30
p_eps          = 1.E-5
ac_eps         = 1.E-2
lout_mc        = .FALSE.
file_met       = ' '
file_sfc       = ' '
lout_met       = .FALSE.
tout_met       = NOT_SET_R
lout_3d        = .TRUE.
lout_2d        = .TRUE.

RETURN
END

!==============================================================================

INTEGER FUNCTION SetMetGrid()

USE SCIPresults_fd
USE scipuff_fi
USE met_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER alloc_stat, irv, i

INTEGER, EXTERNAL :: SWIMnumFields, SWIMgetGrid, SWIMgetTerrain

SetMetGrid = SCIPfailure

CALL ClearMetGrid()

numMet = SWIMnumFields( numMetMax )
IF( numMet < 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'SetMetGrid'
  eMessage = 'Invalid met'
  GOTO 9999
END IF

ALLOCATE( MetGrid(numMetMax),lMetGridComplete(numMetMax),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SetMetGrid'
  eMessage = 'Error allocating met grids'
  GOTO 9999
END IF

lter = .FALSE.

lMetGridComplete = .FALSE.

DO i = 1,numMetMax

  IF( i > numMet )THEN
    MetGrid(i)%nx = 0
    MetGrid(i)%delx2 = NOT_SET_R
    CYCLE
  ELSE
    irv = SWIMgetGrid( i,MetGrid(i) )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMgetGrid' )
      GOTO 9999
    END IF
  END IF

  MetGrid(i)%lter = MetGrid(i)%lter .AND. .NOT.create

  CALL AllocMetGridArrays( MetGrid(i) )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( MetGrid(i)%lter )THEN
    lter = .TRUE.
    irv = SWIMgetTerrain( i,MetGrid(i) )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMgetTerrain' )
      GOTO 9999
    END IF
  END IF

  IF( i == 1 )THEN
    MetGrid(i)%delx2 = delx2
  ELSE
    MetGrid(i)%delx2 = MetGrid(i)%delx2*(FLOAT(2**ABS(mgrd))*dxsplt)**2
  END IF

  lMetGridComplete(i) = .TRUE.

END DO

SetMetGrid = SCIPsuccess

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION AddMetGrid( ifld )

USE SCIPresults_fd
USE scipuff_fi
USE met_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ifld

INTEGER irv, i

INTEGER, EXTERNAL :: SWIMgetGrid, SWIMgetTerrain

!$OMP CRITICAL (ADDSMTH)
AddMetGrid = SCIPfailure

!$OMP ATOMIC
numMet = MAX(numMet,ifld)

IF( numMet > numMetMax )THEN
  nError   = IV_ERROR
  eRoutine = 'AddMetGrid'
  eMessage = 'Exceeding number of allocated met fields'
  GOTO 9999
END IF

i = ifld

irv = SWIMgetGrid( i,MetGrid(i) )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMgetGrid' )
  GOTO 9999
END IF

MetGrid(i)%lter = MetGrid(i)%lter .AND. .NOT.create

CALL AllocMetGridArrays( MetGrid(i) )
IF( nError /= NO_ERROR )GOTO 9999

IF( MetGrid(i)%lter )THEN
  lter = .TRUE.
  irv = SWIMgetTerrain( i,MetGrid(i) )
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMgetTerrain' )
    GOTO 9999
  END IF

END IF

MetGrid(i)%delx2 = MetGrid(i)%delx2*(FLOAT(2**ABS(mgrd))*dxsplt)**2

lMetGridComplete(i) = .TRUE.

!$OMP FLUSH( MetGrid )
!$OMP FLUSH( lMetGridComplete )

AddMetGrid = SCIPsuccess

9999 CONTINUE

!$OMP END CRITICAL (ADDSMTH)
RETURN
END

!==============================================================================

SUBROUTINE WriteSWIMlog()

!------ SWIM log file messages

USE files_fi

IMPLICIT NONE

CHARACTER(128) string

CHARACTER(128), EXTERNAL ::  SWIMgetLogMessage

DO
  string = SWIMgetLogMessage()
  IF( string == CHAR(0) )EXIT
  WRITE(lun_log,'(A)') TRIM(string)
END DO

RETURN
END

!==============================================================================

CHARACTER(*) FUNCTION BuildFileName( string,line,path_in )

USE DefSize_fd

!------ Build file name from string: strip quotes and add path if none included
!       in string

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string  !First string after pathway & keyword
CHARACTER(*), INTENT( IN ) :: line    !Full line
CHARACTER(*), INTENT( IN ) :: path_in !Project path

CHARACTER(1), PARAMETER :: Q  = CHAR(39)  !Single quote
CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

INTEGER i

CHARACTER(PATH_MAXLENGTH) string1, string2

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

INTERFACE
  SUBROUTINE ADDPATH( file,path )
    CHARACTER(*), INTENT ( INOUT ) :: file
    CHARACTER(*), INTENT ( IN )    :: path
  END SUBROUTINE
  SUBROUTINE SPLITNAME( name,file,path )
    CHARACTER(*), INTENT ( IN  ) :: name
    CHARACTER(*), INTENT ( OUT ) :: file
    CHARACTER(*), INTENT ( OUT ) :: path
  END SUBROUTINE
END INTERFACE

!------ Check for quotes (used to specify file names with spaces)
!       N.B. Assumes quotes are only used for file names

i = INDEX(line,QQ)
IF( i == 0 )THEN
  i = INDEX(line,Q)
END IF

!------ No quotes: use first string after pathway/keyword

IF( i == 0 )THEN

  BuildFileName = TRIM(string)

ELSE !- Use string between quotes

  string1 = line(i+1:)
  i = INDEX(string1,QQ)
  IF( i == 0 )THEN
    i = INDEX(string1,Q)
  END IF

  IF( i == 0 )THEN
    BuildFileName = TRIM(string1)
  ELSE
    BuildFileName = TRIM(string1(1:i-1))
  END IF

END IF

IF( LEN_TRIM(path_in) > 0 )THEN
  string1 = string
  CALL SplitName( BuildFileName,string1,string2 )
  string2 = StripNull( string2 )
  IF( LEN_TRIM(string2) == 0 )CALL AddPath( BuildFileName,TRIM(path_in) )
END IF

RETURN
END
