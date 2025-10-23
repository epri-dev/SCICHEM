SUBROUTINE initial()

USE SCIPresults_fd
USE scipuff_fi
USE met_fi
USE surface_fi
USE files_fi
USE inter_fi
USE sampler_fi
USE SWIMparam_fd
USE chem_fi
USE step_p_fi, ONLY: washPuff,myTid
USE cont_rel_functions

!   Perform all initialization requirements for SCIPUFF run

IMPLICIT NONE

CHARACTER(12)  cdefs, cpuffs
CHARACTER(128) cmsg, cmsg2, cmsg3
CHARACTER(PATH_MAXLENGTH) name1, name2

INTEGER ios, i, irv, ifldp

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension
CHARACTER(12), EXTERNAL :: FormatPuffs

INTEGER, EXTERNAL :: SWIMupdateMet, SWIMputMixingHt, SWIMoutput, SWIMaddSmoothField
INTEGER, EXTERNAL :: SetMetGrid, CheckPoleField, sysCopyFile
REAL,    EXTERNAL :: SWIMgetHmin

!----- Clear surface grid ID's, so don't need to call close_surface
!      Set number of samplers to zero
!      Nullify washPuff (POINTER to puff_str)

srfdep = 0
srfdos = 0
nsmp   = 0
lBinOut        = .FALSE.
srfados        = 0
lChemAmbDosRes = .FALSE.

NULLIFY( washPuff )
NULLIFY( Psrfdep, Psrfdos, Pauxdep, Pauxdos )
NULLIFY( Psrfados, Pauxados )
!----- Set interaction flag to default

lProcessAll = .FALSE.

!----- Initialize any control parameters used in exit_scipuff

nsmp = 0
multicomp = .FALSE.
sampamb = .FALSE.
myTid = 1

!------ get name

CALL SplitName( file_inp,name2,name1 )
i = INDEX(name2,'.')
IF( i > 1 .AND. i-1 <= LEN(name) )THEN
  name = name2(1:i-1)
ELSE
  nError   = IV_ERROR
  eRoutine = 'initial'
  eMessage = 'Project name too long.  Must be < 65 characters.'
  CALL ReportFileName( eInform,'Name=',name )
  GOTO 9999
END IF

!------ initialize

OPEN(UNIT=lun_inp,FILE=file_inp,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'initial'
  eMessage = 'Error opening SCIPUFF main input file'
  CALL ReportFileName( eInform,'File=',file_inp )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

cmsg  = 'Initializing SCIPUFF'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

CALL init_clock()
IF( nError /= NO_ERROR )GOTO 9999

CALL enableSCIPUFFhalt( SCIPUFF_HALT )      !Show abort button

CALL ReadNamelistCtrl( lun_inp )
IF( nError /= NO_ERROR )GOTO 9997

CALL set_mxgrd( 99 )

!------ open/create/read files

cmsg  = TRIM(name)//' : Initializing'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

IF( restart )THEN

  cmsg  = CHAR(0)
  cmsg2 = 'Reading project data files'
  cmsg3 = CHAR(0)
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999
  create = .FALSE.

  CALL old_start()
  IF( nError /= NO_ERROR )GOTO 9999

  cmsg  = CHAR(0)
  cmsg2 = 'Initializing data'
  cmsg3 = CHAR(0)
  CALL write_progress( cmsg,cmsg2,cmsg3 )

ELSE

  cmsg3 = CHAR(0)
  cmsg2 = 'Reading project input data'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  CALL init_time1()

  CALL ReadNamelistTime1( lun_inp )
  IF( nError /= NO_ERROR )GOTO 9997

  CALL init_time2()

  CALL ReadNamelistTime2( lun_inp )
  IF( nError /= NO_ERROR )GOTO 9997

  CALL new_start()
  IF( nError /= NO_ERROR )GOTO 9999

  cmsg  = CHAR(0)
  cmsg3 = CHAR(0)
  cmsg2 = 'Initializing data'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

END IF

!------ Set material-dependent flags

CALL set_class_run_flags()
IF( nError /= NO_ERROR )GOTO 9999

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

!------ End time

CALL set_end_time()
IF( nError /= NO_ERROR )GOTO 9999

!------ Initialize multicomponent chemistry background

IF( multicomp )THEN
  CALL initRunMC()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Initialize met file

cmsg = CHAR(0)
IF( create )THEN
  cmsg2 = 'Validating meteorology input'
ELSE
  cmsg2 = 'Initializing meteorology'
END IF
cmsg3 = CHAR(0)
CALL write_progress( cmsg,cmsg2,cmsg3 )

CALL init_met()
IF( nError /= NO_ERROR )GOTO 9999

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

cmsg  = TRIM(name)//' : Initializing'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

!------ Set washout time scale if precipitation

IF( lwash )THEN
  CALL init_wash()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!  Initialize adjoint data from scn file first

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  CALL InitAdjointMat()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Set up grids and create surface/output files if necessary

IF( .NOT.restart )THEN

!------ Get met grid and terrain to save with project

  hmin = SWIMgetHmin()

  irv = SetMetGrid()
  IF( irv /= SCIPsuccess )GOTO 9999

!------ Set SCIPUFF grid

  CALL set_grid()
  IF( nError /= NO_ERROR )GOTO 9999

!------ zero ipgrd

  CALL allocate_ip( nz )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL zero_ip()

  cmsg  = CHAR(0)
  cmsg2 = 'Creating project output files'
  cmsg3 = CHAR(0)
  CALL write_progress( cmsg,cmsg2,cmsg3 )

! --- Create diagnostics and ambient files

  IF( multicomp )THEN
    CALL CreateChemOutput()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!------ create surface integral fields

  CALL create_surface()
  IF( nError /= NO_ERROR )GOTO 9999

  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Creating project puff file'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ write output and project files

  CALL create_output()
  IF( nError /= NO_ERROR )GOTO 9999

!------ check if restarting from existing project

  IF( file_rst /= ' ' )THEN

    cmsg  = CHAR(0)
    cmsg2 = CHAR(0)
    cmsg3 = 'Reading restart puff file'
    CALL write_progress( cmsg,cmsg2,cmsg3 )

    CALL puff_rst()
    IF( nError /= NO_ERROR )GOTO 9999

    IF( npuf > 0 )THEN
      CALL set_ip( 1,npuf )
      IF( nError /= NO_ERROR )GOTO 9999
      CALL set_tlev( .TRUE. )
    END IF

    IF( surface )THEN
      cmsg  = CHAR(0)
      cmsg2 = CHAR(0)
      cmsg3 = 'Reading restart surface deposition file'
      CALL write_progress( cmsg,cmsg2,cmsg3 )

      file_tmp = AddExtension( file_rst,'dep' )
      CALL AddPath( file_tmp,path_rst )

      CALL surf_rst( lun_tmp,srfdep,file_tmp )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

    IF( dose )THEN
      cmsg  = CHAR(0)
      cmsg2 = CHAR(0)
      cmsg3 = 'Reading restart surface dosage file'
      CALL write_progress( cmsg,cmsg2,cmsg3 )

      file_tmp = AddExtension( file_rst,'dos' )
      CALL AddPath( file_tmp,path_rst )

      CALL surf_rst( lun_tmp,srfdos,file_tmp )
      IF( nError /= NO_ERROR )GOTO 9999
      IF( multicomp )THEN
        ! Ambient concentration Dosage
        cmsg  = CHAR(0)
        cmsg2 = CHAR(0)
        cmsg3 = 'Reading restart surface ambient dosage file'
        CALL write_progress( cmsg,cmsg2,cmsg3 )

        file_tmp = AddExtension( file_rst,'ados' )
        CALL AddPath( file_tmp,path_rst )

        CALL surf_rst( lun_tmp,srfados,file_tmp )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END IF

    IF( .NOT.create )THEN
      cmsg  = CHAR(0)
      cmsg2 = CHAR(0)
      cmsg3 = 'Writing output file(s)'
      CALL write_progress( cmsg,cmsg2,cmsg3 )

      CALL write_puff()
      IF( nError /= NO_ERROR )GOTO 9999

    END IF

    IF( LEN_TRIM(smpfile) > 0 )THEN
      file_tmp = AddExtension( file_rst,'smp' )
      CALL AddPath( file_tmp,path_rst )
      irv = sysCopyFile( file_tmp,file_smp )
      IF( irv == SCIPfailure )THEN
        eMessage = 'sysCopyFile error'
        eInform  = TRIM(file_tmp)//'=>'//TRIM(file_smp)
        GOTO 9999
      END IF
    END IF

    WRITE(lun_log,111,ERR=9998)'Restart from '//TRIM(file_rst) &
                  //' completed at t =',t/3600.,'hrs.'

    cmsg  = CHAR(0)
    cmsg2 = CHAR(0)
    cmsg3 = 'Creating project PRJ file'
    CALL write_progress( cmsg,cmsg2,cmsg3 )

    CALL write_prj()
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

ELSE

  IF( t == 0. .AND. numMet > 0 )THEN !MetGrid set in read_prj if t>0
    hmin = SWIMgetHmin()
    irv = SetMetGrid()
    IF( irv /= SCIPsuccess )GOTO 9999
  END IF

  CALL init_srf_blocks( ntypm )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!------ Stop if only creating files

IF( create )THEN
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Writing output file(s)'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  CALL write_puff()
  IF( nError /= NO_ERROR )GOTO 9999

  WRITE(lun_log,111,ERR=9998)'Setup completed at t =',t/3600.,'hrs.'

  GOTO 9999
END IF

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

IF( t > 0. .AND. numMet > 0 )THEN

!------ Move saved zi field to SWIM (if 'real' restart)

  DO i = 1,numMet
    irv = SWIMputMixingHt( i,MetGrid(i) )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMputMixingHt' )
      GOTO 9999
    END IF
  END DO

!------ Build smooth fields if necessary

  DO i = nBaseMet+1,numMet
    ifldp =  MetGrid(i)%basic
    irv = SWIMaddSmoothField( ifldp )
    IF( irv == SWIMfailure )THEN
      CALL setSWIMerror( 'SWIMaddSmoothField' )
      GOTO 9999
    ELSE IF( irv == SWIMnull )THEN
      nError   = IV_ERROR
      eRoutine = 'initial'
      WRITE(eMessage,'(A,I2)') 'Error rebuilding smooth field ',i
      WRITE(eInform,'(A,I2)')  'from parent field ',MetGrid(i)%basic
      eAction = 'Project file is invalid'
      GOTO 9999
    END IF
  END DO

END IF

!------ Set minimum elevation

hmin = SWIMgetHmin()

!------ Reset MetGrid to account for nests and/or potential smooth fields

irv = SetMetGrid()
IF( irv /= SCIPsuccess )GOTO 9999

!------ Find polar met fields if needed

polefld_n = CheckPoleField( polarcap_n,NORTHPOLE )
IF( polefld_n < 0 )THEN
  IF( nError == NO_ERROR )THEN
    nError   = IV_ERROR
    eRoutine = 'initial'
    eMessage = 'Requested domain requires meteorology for North polar cap '
    eInform  = 'Invalid meteorology type or domain'
    eAction  = 'Must be gridded and Lat >= 86 degrees '
  END IF
  GOTO 9999
END IF

polefld_s = CheckPoleField( polarcap_s,SOUTHPOLE )
IF( polefld_s < 0 )THEN
  IF( nError == NO_ERROR )THEN
    nError   = IV_ERROR
    eRoutine = 'initial'
    eMessage = 'Requested domain requires meteorology for South polar cap '
    eInform  = 'Invalid meteorology type or domain'
    eAction  = 'Must be gridded  and Lat <= -86 degrees '
  END IF
  GOTO 9999
END IF

!------ Set sampler flags and read data if necessary

int_sensor = .FALSE.

CALL read_smp()
IF( nError /= NO_ERROR )GOTO 9999

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

!------ Output initial met fields

IF( lout_met )THEN

  IF( .NOT.restart .OR. t == 0. )THEN

    cmsg  = 'Outputting initial wind field'
    cmsg2 = ' '
    cmsg3 = ' '
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    IF( nError /= NO_ERROR )GOTO 9999

    irv = SWIMoutput()
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMoutput' )
      GOTO 9999
    END IF
    CALL WriteSWIMlog()
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

  timeOutMet = t  !Assumes that time was already written previously for restart

END IF

!------ Position scenario file

cmsg  = CHAR(0)
cmsg2 = 'Initializing sources'
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

CALL scn_init()
IF( nError /= NO_ERROR )GOTO 9999

!------ Report status

cpuffs = FormatPuffs( npuf )
IF( npuf > 0 )THEN
  cdefs  = FormatPuffs( countDefinitions() )
  IF( restart )THEN
    WRITE(lun_log,111,ERR=9998)'Run  restarted   at t =',t/3600., &
                       'hrs. with NCREL = '//TRIM(cdefs)//' and NPUFF = '//TRIM(cpuffs)
111 FORMAT(A,F7.2,A)
  ELSE
    WRITE(lun_log,111,ERR=9998)'Run    started   at t =',t/3600., &
                       'hrs. with NCREL = '//TRIM(cdefs)//' and NPUFF = '//TRIM(cpuffs)
  END IF
END IF

cmsg  = CHAR(0)
cmsg2 = 'Beginning run with '//TRIM(cpuffs)//' puffs'
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

9999 CONTINUE

CLOSE( lun_inp,IOSTAT=ios )

cmsg  = TRIM(name)//' : Initialized'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

RETURN

!------ set log write error and goto return

9998 CONTINUE
nError   = WR_ERROR
eRoutine = 'initial'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

!------ set namelist error and goto return

9997 CONTINUE
eRoutine = 'initial'
CALL ReportFileName( eInform,'File=',file_inp )
GOTO 9999

END

!=======================================================================

SUBROUTINE init_clock()

USE scipuff_fi
USE files_fi
USE cont_rel_fi

IMPLICIT NONE

INTEGER ios

CHARACTER(12)           :: sysTime
CHARACTER(12), EXTERNAL :: sysGetTime

!------ Get time

ios = 0
sysTime = sysGetTime()
WRITE(lun_log,'(A)',IOSTAT=ios)'Starting run setup at '//TRIM(sysTime)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'init_clock'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

!------ Set Defaults

CALL InitReleaseSpec( currentSpec )

restart      = .FALSE.
ActiveSource = .TRUE.

CALL InitReleaseSpec( InstReleaseList%relSpec )
NULLIFY( InstReleaseList%NextRelease )

initStatics    = .FALSE.
numCollection  = 0
maxCollection  = 0
numDefinition  = 0
maxDefinition  = 0
runUpdates   = .FALSE.

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE init_time1()

USE scipuff_fi

IMPLICIT NONE

!------ Set Defaults

tstart      = 0.
year_start  = NOT_SET_I
month_start = NOT_SET_I
day_start   = NOT_SET_I

local       = .FALSE.
tzone       = DEF_VAL_R

RETURN
END

!=======================================================================

SUBROUTINE init_time2()

USE scipuff_fi

IMPLICIT NONE

!------ Set Defaults

tend        = NOT_SET_R
tend_hr     = NOT_SET_R
year_end    = NOT_SET_I
month_end   = NOT_SET_I
day_end     = NOT_SET_I

dt_save     = 1.E+36
delt        = 0.0

RETURN
END

!=======================================================================

SUBROUTINE scn_init()

USE scipuff_fi
USE files_fi
USE cont_rel_fi
USE cont_rel_functions
USE convert_fd

IMPLICIT NONE

INTEGER ios, mpuf, mcrel, crmode
INTEGER n1, n2
REAL    t_last

CHARACTER(256) cmsg, cmsg2, cmsg3

REAL, EXTERNAL :: ScaleReal
LOGICAL, EXTERNAL :: check_scn_time
CHARACTER(12), EXTERNAL :: FormatPuffs

!------ open scenario events file

OPEN(UNIT=lun_scn,FILE=file_scn,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'scn_init'
  eMessage = 'Error opening SCIPUFF release scenario input file'
  CALL ReportFileName( eInform,'File=',file_scn )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

!------ check first release time

CALL start_clock()

init_source       = 0

CALL get_scn( lun_scn,file_scn,currentSpec )
IF( nError /= NO_ERROR )GOTO 9999

!------ On start check for first release at t=0.0

IF( t == 0.0 )THEN
  CALL check_first_scn( currentSpec )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ Report error of no releases found after t=0.0

IF( .NOT.ActiveSource )THEN
  nError   = RD_ERROR
  eRoutine = 'scn_init'
  eMessage = 'No release found on scn file'
  CALL ReportFileName( eInform,'File=',file_scn )
  GOTO 9999
END IF

!------ initialize any active plumes (C-type)

mpuf   = npuf
mcrel  = count_nrel()
t_last = t_old_r

DO WHILE( ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC) < t+delt )

  init_source = init_source + 1

  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)

!------ Check if scn is to processed or skipped (restarts)

  IF( check_scn_time(t_last) )THEN

    IF( LEN_TRIM(currentSpec%release%relDisplay) > 0 )THEN
      WRITE(cmsg3,'(A,I5,''('',A,'')'')')'Preparing source',init_source,TRIM(currentSpec%release%relDisplay)
    ELSE
      WRITE(cmsg3,'(A,I5)')'Preparing source',init_source
    END IF
    CALL write_progress( cmsg,cmsg2,cmsg3 )

    IF( ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC) > t_last )THEN        !Clear Initialize
      crmode = CRMODE_START
    ELSE IF( ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC) < t_last )THEN   !Clear Re-initialize
      crmode = CRMODE_RESTART
    ELSE
      IF( ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC) < t )THEN           !Re-initialize an existing release (resume)
        crmode = CRMODE_RESTART
      ELSE                                                                     !Initialize a new release (special restart with trel = restart time)
        crmode = CRMODE_START
      END IF
    END IF

!------ process scn

    CALL process_scn( crmode,currentSpec )
    IF( nError /= NO_ERROR )GOTO 9999

    IF( crmode == CRMODE_RESTART .AND. count_nrel() > mcrel )THEN
      nError = IV_ERROR
      eMessage = 'Error updating source data'
      eInform  = 'Continuous sources do not match restart data'
      eRoutine = 'scn_init'
      GOTO 9999
    END IF

  ELSE

    IF( LEN_TRIM(currentSpec%release%relDisplay) > 0 )THEN
      WRITE(cmsg3,'(A,I5,''('',A,'')'')')'Skipping source',init_source,TRIM(currentSpec%release%relDisplay)
    ELSE
      WRITE(cmsg3,'(A,I5)')'Skipping source',init_source
    END IF
    CALL write_progress( cmsg,cmsg2,cmsg3 )

  END IF

  CALL step_clock()

!----- Save time of initialized release (for restart)

  t_old_r = MAX(t_old_r,ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC))

  CALL get_scn( lun_scn,file_scn,currentSpec )
  IF( nError /= NO_ERROR )GOTO 9999

END DO

IF( count_nrel() < mcrel )THEN
  nError = RD_ERROR
  eMessage = 'Failed to read all continuous source data'
  eRoutine = 'Initial'
  GOTO 9999
END IF

!----- Initialize puff interactions

IF( npuf > mpuf )THEN
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Initializing '//TRIM(FormatPuffs(npuf-mpuf))//' new puffs'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  n1 = mpuf + 1
  n2 = npuf
  CALL set_rel( n1,n2,0 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!----- Initialize continuous release interactions

IF( count_nrel() > 0 )THEN
  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Initializing '//TRIM(FormatPuffs(count_nrel()))//' new releases'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  CALL InteractContinuousReleases( .TRUE. )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

CALL clear_random_loc()

CALL stop_clock()

RETURN
END

!=======================================================================

LOGICAL FUNCTION check_scn_time( t_last ) RESULT( yes )

USE scipuff_fi
USE convert_fd

REAL, INTENT( IN ) :: t_last

REAL tdur,trel

REAL, EXTERNAL :: ScaleReal

trel = ScaleReal(currentSpec%release%trel,HCF_HOUR2SEC)
IF( BTEST(currentSpec%release%type,HRB_CONT) )THEN
  CALL getReleaseDuration( currentSpec%release,tdur )
  yes = (trel+tdur-t > 1.0E-4*tdur .OR. tdur == DEF_VAL_R )  !Same as deactivation check in c_release
ELSE IF( BTEST(currentSpec%release%type,HRB_INST) )THEN
  yes = ( trel > t_last )
END IF

RETURN
END

!=======================================================================

SUBROUTINE check_first_scn( relSpec )

USE scipuff_fi
USE files_fi
USE cont_rel_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec

INTEGER ios
CHARACTER(128) cmsg, cmsg2, cmsg3

!==== Check first release time

IF( relSpec%release%trel /= 0. )THEN

!==== Inform user that first release is not at t=0.

  nError   = WN_ERROR
  eRoutine = 'check_first_scn'
  eMessage = 'First release is not at run start time'
  IF( relSpec%release%trel < 0.0 )THEN
    eInform = 'Releases earlier than start time will be ignored'
  ELSE
    WRITE(eInform,*)'No puffs will be released until t =',relSpec%release%trel,' hrs'
  END IF

  CALL WarningMessage( .TRUE. )
  IF( nError /= NO_ERROR )GOTO 9999

!==== Skip releases before t=0.

  DO WHILE( relSpec%release%trel < 0.0 )
    WRITE(lun_log,*,IOSTAT=ios)'Skipping release at t =',relSpec%release%trel
    nRelBeforeStart = nRelBeforeStart + 1
    init_source = init_source + 1
    cmsg  = CHAR(0)
    cmsg2 = CHAR(0)
    IF( LEN_TRIM(relSpec%release%relDisplay) > 0 )THEN
      WRITE(cmsg3,'(A,I5,''('',A,'')'')')'Skipping source',init_source,TRIM(relSpec%release%relDisplay)
    ELSE
      WRITE(cmsg3,'(A,I5)')'Skipping source',init_source
    END IF
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    CALL get_scn( lun_scn,file_scn,relSpec )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

END IF

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE process_scn( crmode,relSpec )

USE scipuff_fi
USE files_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER,             INTENT( IN   ) :: crmode
TYPE( releaseSpecT), INTENT( INOUT) :: relSpec

!----- update scn

CALL update_scn()
IF( nError == SKP_ERROR )THEN
  CALL init_error()
  GOTO 9999
ELSE
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!----- check scn settings

CALL valid_scn( relSpec )
IF( nError /= NO_ERROR )GOTO 9999

!----- initialize for random locations

CALL init_random_loc( relSpec )
IF( nError /= NO_ERROR )GOTO 9999

!----- process scn

IF( BTEST(relSpec%release%type,HRB_CONT) )THEN
  CALL process_scn_cont( relSpec,crmode )
ELSE IF( BTEST(relSpec%release%type,HRB_INST) )THEN
  CALL i_release( relSpec,.TRUE. )
END IF
IF( nError /= NO_ERROR )GOTO 9999

!----- clear random locations

CALL clear_random_loc()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE InitAdjointMat()

USE scipuff_fi
USE files_fi
USE adjoint_fi

IMPLICIT NONE

INTEGER ios, i, imat, nrd
REAL tdur, cmass

TYPE( releaseSpecT ) :: relSpec

LOGICAL, EXTERNAL :: IsNullSensor

!---- Only initialize data for start of run, otherwise set last hit time

IF( t > 0.0 )THEN
  tFirstTrigger = 0.0
  DO i = 1,ntypm
    IF( .NOT.IsNullSensor(material(i)%icls) )THEN
      tFirstTrigger = MAX(tFirstTrigger,AdjMat(i)%trel)
    END IF
  END DO
  GOTO 9999
END IF

!---- Initialize from scn file

IF( ALLOCATED(AdjMat) )DEALLOCATE( AdjMat,STAT=ios )

ALLOCATE( AdjMat(ntypm),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Failed to allocate adjoint material storage'
  eRoutine = 'InitAdjointMat'
  GOTO 9999
END IF

!------ open scenario events file

OPEN(UNIT=lun_scn,FILE=file_scn,STATUS='OLD',ACTION="READ",IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'InitAdjointMat'
  eMessage = 'Error opening SCIPUFF release scenario input file'
  CALL ReportFileName( eInform,'File=',file_scn )
  eAction  = 'Make sure file exists'
  GOTO 9999
END IF

nrd = 0

tFirstTrigger = 0.0

CALL InitReleaseSpec( relSpec )

DO
  CALL get_scn( lun_scn,file_scn,relSpec )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( relSpec%release%trel == 1.0E36 )EXIT

  imat = 0
  DO i = 1,ntypm
    IF( TRIM(relSpec%release%material) == TRIM(material(i)%cmat) )imat = i
  END DO

  IF( imat == 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'InitAdjointMat'
    eMessage = 'Material in scenario file not found in material list:'//TRIM(currentSpec%release%material)
    CALL ReportFileName( eInform,'File=',file_scn )
    GOTO 9999
  END IF

  CALL getReleaseDuration( relSpec%release,tdur )
  CALL getReleaseMass( relSpec%release,cmass )
  AdjMat(imat)%xrel = SNGL(relSpec%release%xrel)
  AdjMat(imat)%yrel = SNGL(relSpec%release%yrel)
  AdjMat(imat)%zrel = relSpec%release%zrel
  AdjMat(imat)%trel = relSpec%release%trel
  AdjMat(imat)%tdur = tdur
  AdjMat(imat)%mass = cmass
  AdjMat(imat)%umet = NOT_SET_R
  AdjMat(imat)%vmet = NOT_SET_R

  nrd = nrd + 1

  IF( .NOT.IsNullSensor(material(imat)%icls) )THEN
    tFirstTrigger = MAX(tFirstTrigger,relSpec%release%trel)
  END IF

END DO

IF( nrd /= ntypm )THEN
  nError   = UK_ERROR
  eRoutine = 'InitAdjointMat'
  eMessage = 'No. of releases in scenario file does not match material list'
  CALL ReportFileName( eInform,'File=',file_scn )
  eAction  = 'Sensor releases must use unique material names'
  GOTO 9999
END IF

9999 CONTINUE

ActiveSource = .TRUE.

CLOSE(UNIT=lun_scn,IOSTAT=ios)

RETURN
END

!===============================================================================

SUBROUTINE set_end_time()

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER jul_end, jyy
LOGICAL lymd_end
REAL    tx, tend_hx

REAL, PARAMETER :: TEND_HR_MAX = 100.*365.*24.

INTEGER, EXTERNAL :: julian_day

lymd_end = year_end /= NOT_SET_I .OR. month_end /= NOT_SET_I &
       .OR. day_end /= NOT_SET_I .OR. tend /= NOT_SET_R

IF( lymd )THEN

  IF( lymd_end )THEN

    IF( year_end  == NOT_SET_I )year_end  = year_start
    IF( month_end == NOT_SET_I )month_end = month_start
    IF( day_end   == NOT_SET_I )day_end   = day_start
    IF( tend      == NOT_SET_R )tend      = tstart

    CALL set_year( year_end )

    IF( year_end < year_start )THEN
      nError   = RD_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Cannot set end year before start year'
      CALL ReportFileName( eInform,'File=',file_inp )
      GOTO 9999
    END IF

    jul_end = julian_day( month_end,day_end,year_end )
    IF( jul_end == -999 )THEN
      nError   = UK_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Incorrect end day, month, year'
      CALL ReportFileName( eInform,'File=',file_inp )
      GOTO 9999
    END IF

    IF( year_end /= year_start )THEN
      DO jyy = year_start,year_end-1
        IF( MOD(jyy,4) == 0 )THEN
          jul_end = jul_end + 366
        ELSE
          jul_end = jul_end + 365
        END IF
      END DO
    END IF

    tend_hx = tend - tstart + (jul_end - jul_start)*24.

  ELSE

    IF( tend_hr == NOT_SET_R )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) or end time'
      GOTO 9999
    ELSE IF( tend_hr <= 0. )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) > 0 '
      GOTO 9999
    ELSE IF( tend_hr > TEND_HR_MAX )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Run time (duration in hrs) too large'
      WRITE(eInform,'(A,ES8.2,A)') 'Maximum is ',TEND_HR_MAX,' (equivalent to 100 years)'
      GOTO 9999
    END IF
    tx      = tstart + tend_hr
    tend_hx = NOT_SET_R

    CALL year_month_day( tx,year_start,month_start,day_start &
                        ,tend,year_end,month_end,day_end )

  END IF

ELSE ! .not. lymd

  IF( tend /= NOT_SET_R .OR. day_end /= NOT_SET_I )THEN
    IF( tend == NOT_SET_R )tend = tstart
    tend_hx = tend - tstart
    IF( day_start /= NOT_SET_I )THEN
      IF( day_end == NOT_SET_I )day_end = day_start
      tend_hx = tend_hx + 24.*FLOAT(day_end-day_start)
    ELSE
      IF( day_end /= NOT_SET_I )tend_hx = tend_hx + 24.*FLOAT(day_end)
    END IF
  ELSE
    IF( tend_hr == NOT_SET_R )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) or ' &
                                              //'end time'
      GOTO 9999
    ELSE IF( tend_hr <= 0. )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Must set run time (duration in hrs) > 0 '
      GOTO 9999
    ELSE IF( tend_hr > TEND_HR_MAX )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Run time (duration in hrs) too large'
      WRITE(eInform,'(A,ES8.2,A)') 'Maximum is ',TEND_HR_MAX,' (equivalent to 100 years)'
      GOTO 9999
    END IF
    tend    = tstart + tend_hr
    tend_hx = NOT_SET_R
  END IF

END IF

IF( tend_hx /= NOT_SET_R )THEN
  IF( tend_hr /= NOT_SET_R )THEN
    IF( ABS(tend_hr-tend_hx) > 0.01 )THEN
      nError   = WN_ERROR
      eRoutine = 'set_end_time'
      eMessage = 'Run time (duration in hrs) inconsistent with end time'
      eInform  = 'Run time will be ignored'
      CALL WarningMessage( .TRUE. )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
  tend_hr = tend_hx
END IF

tend_r = tend_hr*3600.
IF( tend_r < t )THEN
  nError   = UK_ERROR
  eRoutine = 'set_end_time'
  eMessage = 'End time is less than current time'
  CALL ReportFileName( eInform,'File=',file_inp )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_class_run_flags()

USE scipuff_fi
USE files_fi
USE substrate_fi
USE landuse_fd

!----- initialization on runtime class flags

IMPLICIT NONE

TYPE( landuse_init)     file_str
TYPE( liquid_material ) pmatl
TYPE( puff_material )   pmatlIn

INTEGER i
INTEGER ityp, ios, nsub, nliq

CHARACTER(16), DIMENSION(:), ALLOCATABLE :: sub_names, liq_names
REAL,          DIMENSION(:), ALLOCATABLE :: liq_sf

LOGICAL, EXTERNAL :: IsEvap
INTEGER, EXTERNAL :: GetSubstrateParams, GetNumSubstrates, GetSubstrates

!------ Set activity decay flag

i = 1
ldecay = .FALSE.
DO WHILE( .NOT.ldecay .AND. i <= ntypm )
  IF( material(i)%prop(1) + material(i)%prop(2) > 0 )THEN
    ldecay = .TRUE.; EXIT
  END IF
  i = i + 1
END DO

!----- Secondary Evaporation

evaporation = .FALSE.
mxlev_evap  = -1

DO i = 1,ntypm
  evaporation = evaporation .OR. IsEvap( material(i)%icls )
END DO

!----- Get substrate parameters if necessary

IF( evaporation )THEN

  IF( substrate_type /= 0 )THEN

!---- Check that viscosity is set for any liquid materials with
!     secondary evaporation

    nliq = 0

    DO i = 1,ntypm
      IF( IsEvap(material(i)%icls) )THEN
        nliq = nliq + 1
        ityp = material(i)%ioffp + 2
        CALL get_puff_material( ityp,pmatlIn )
        pmatl = TRANSFER(pmatlIn,pmatl)
        IF( pmatl%viscosity == NOT_SET_R .OR. pmatl%viscosity <= 0.0 )THEN
          nError = IV_ERROR
          eRoutine = 'set_class_run_flags'
          eMessage = 'Must set a positive liquid viscosity '
          eInform  = 'for secondary evaporation with permeable surface'
          eAction  = 'Material is '//TRIM(material(i)%cmat)
          GOTO 9999
        END IF
      END IF
    END DO

  END IF

    IF( substrate_type /= 0 )THEN

      IF( .NOT.restart )THEN   !Read substrate parameters only on initialization
        file_str%lun  = lun_tmp
        file_str%file = TRIM(file_lus)
        i = GetSubstrateParams( file_str,substrate_type,porosity, &
                                                     tortuosity,grain_size )
        IF( i == 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'GetSubstrateParams'
          eMessage = 'Error reading substrate parameters'
          WRITE(eInform,'(A,I3)',IOSTAT=ios) 'Type = ',substrate_type
          GOTO 9999
        END IF

!---- write substrate info to log file

        i = GetNumSubstrates( file_str,nsub )
        IF( i /= 0 )ALLOCATE( sub_names(nsub),STAT=ios )
        IF( ios /= 0 .OR. i == 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'set_class_run_flags'
          eMessage = 'Error allocating substrate names'
          WRITE(eInform,'(A,I3)',IOSTAT=ios) 'Number substrate types = ',nsub
          GOTO 9999
        END IF

        i = GetSubstrates( file_str,nsub,sub_names )
        IF( i == 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'GetSubstrates'
          eMessage = 'Error reading substrate names'
          GOTO 9999
        END IF

        WRITE(lun_log,'(A)',IOSTAT=ios)'*********** Substrate parameters *************'
        WRITE(lun_log,'(A)',IOSTAT=ios)'   Type       = '// &
                                                 TRIM(sub_names(substrate_type))
        WRITE(lun_log,'(A,1PE10.4)',IOSTAT=ios)'   Porosity   = ',porosity
        WRITE(lun_log,'(A,1PE10.4)',IOSTAT=ios)'   Tortuosity = ',tortuosity
        WRITE(lun_log,'(A,1PE10.4)',IOSTAT=ios)'   Grain Size = ',grain_size
        WRITE(lun_log,'(A)',IOSTAT=ios)'********* End substrate parameters ***********'
      END IF

      k_substrate = grain_size*(porosity**5)*0.7071 / &
                    (9.0*tortuosity*(1.0-porosity)**2)
    ELSE

      substrate_type = 0
      porosity       = NOT_SET_R
      tortuosity     = NOT_SET_R
      grain_size     = NOT_SET_R

    END IF

ELSE

  substrate_type = 0
  porosity       = NOT_SET_R
  tortuosity     = NOT_SET_R
  grain_size     = NOT_SET_R

END IF

9999 CONTINUE

IF( ALLOCATED(sub_names) )DEALLOCATE( sub_names,STAT=ios )
IF( ALLOCATED(liq_names) )DEALLOCATE( liq_names,STAT=ios )
IF( ALLOCATED(liq_sf   ) )DEALLOCATE( liq_sf   ,STAT=ios )

RETURN
END

!===============================================================================

INTEGER FUNCTION CheckPoleField( polarcap,poletype )

USE scipuff_fi
USE met_fi
USE SWIMparam_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: polarcap
INTEGER, INTENT( IN ) :: poletype

INTEGER i, ftype, irv

INTEGER, EXTERNAL :: SWIMGetGridType

IF( polarcap )THEN
  CheckPoleField = -1
  DO i = 1,numMet
    irv = SWIMGetGridType( i,ftype )
    IF( irv /= SWIMsuccess )THEN
      nError   = IV_ERROR
      eRoutine = 'CheckPoleField'
      eMessage = 'Invalid field number'
      GOTO 9999
    END IF
    IF( ftype == poletype )THEN
      CheckPoleField = i
      EXIT
    END IF
  END DO
ELSE
  CheckPoleField = 0
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckGlobalMet()

USE scipuff_fi
USE met_fi

IMPLICIT NONE

SELECT CASE( met_type )
  CASE( 'GRIDDED','MEDOC','MRF','WRF' )
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'CheckGlobalMet'
    eMessage = 'Requested domain requires polar cap meteorology'
    eInform  = 'but meteorology input is invalid'
    eAction  = 'Must be gridded (MEDOC, SCIP or 3D Climatology)'
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE initRunMC()

USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER ii, n
INTEGER ip

! -- Initialize background (if required) for any chemistry multicomponents

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n,1)

    IF( chem%lStepAmb )THEN
      CALL InitChemStepAmb( .FALSE. )
      IF( nError /= NO_ERROR )GOTO 9999
      IF( chem%lSfcFlx )THEN
        CALL InitChemSfcFlux()
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    ELSE IF( chem%lAmbFile )THEN
      CALL InitChemAmb()
      IF( nError /= NO_ERROR )GOTO 9999
      IF( nthreads > 1 )THEN
        DO ip = 2,nthreads
          chemMC(n,ip)%Ambient = chem%Ambient
        END DO
      END IF
    END IF
  END IF

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckAmbDomain()

USE met_fi
USE scipuff_fi
USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER ii, n, nxb, nyb
REAL    dxb, dyb, xmin1, xmax1, ymin1, ymax1
LOGICAL lgrdx, lgrdy

LOGICAL, EXTERNAL :: CheckDomainOverlap

!----- Check ambient domain for overlap with project domain

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n    = mat_mc%ID(ii)
    chem => chemMC(n,1)

    IF( chem%lAmbFile )THEN
      IF( chem%Ambient%nx > 2 .AND. chem%Ambient%nx > 2 )THEN

        xmin1 = chem%Ambient%x0; nxb = chem%Ambient%nx; dxb = chem%Ambient%dx
        ymin1 = chem%Ambient%y0; nyb = chem%Ambient%ny; dyb = chem%Ambient%dy

        xmax1 = xmin1 + FLOAT(nxb-1)*dxb
        ymax1 = ymin1 + FLOAT(nyb-1)*dyb

        IF( CheckDomainOverlap( xmin1,xmax1,ymin1,ymax1,dxb,dyb,lgrdx,lgrdy ) )THEN
          nError   = DM_ERROR
          eRoutine = 'CheckAmbDomain: SCIPUFF/Ambient domain mismatch '
          eMessage = 'Do you want SCIPUFF to adjust the project domain and continue?'
          WRITE(eInform,'(A,4F10.2)') 'Ambient Domain : ',xmin1,xmax1,ymin1,ymax1
          WRITE(eAction,'(A,4F10.2)') 'Project Domain : ',xmin,xmax,ymin,ymax
          CALL WarningMessage( .FALSE. )
          IF( nError /= NO_ERROR )THEN
            CALL init_error()
            GOTO 9999
          ELSE
            xmin = MAX(xmin,xmin1)
            xmax = MIN(xmax,xmax1)
            ymin = MAX(ymin,ymin1)
            ymax = MIN(ymax,ymax1)
            IF( xmin > xmax .OR. ymin > ymax )THEN
              nError   = DM_ERROR
              eMessage = 'Project domain is outside the ambient grid'
              GOTO 9999
            END IF
          END IF
        END IF

      END IF
    END IF

  END IF

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE CheckSFluxDomain()

USE met_fi
USE scipuff_fi
USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER ii, n, nxb, nyb
REAL    dxb, dyb, xmin1, xmax1, ymin1, ymax1
LOGICAL lgrdx, lgrdy

LOGICAL, EXTERNAL :: CheckDomainOverlap

!----- Check ambient domain for overlap with project domain

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n    = mat_mc%ID(ii)
    chem => chemMC(n,1)

    IF( chem%lSfcFlx )THEN

      IF( chem%sFlux%nx > 2 .AND. chem%sFlux%nx > 2 )THEN

        xmin1 = chem%sFlux%x0; nxb = chem%sFlux%nx; dxb = chem%sFlux%dx
        ymin1 = chem%sFlux%y0; nyb = chem%sFlux%ny; dyb = chem%sFlux%dy

        xmax1 = xmin1 + FLOAT(nxb-1)*dxb
        ymax1 = ymin1 + FLOAT(nyb-1)*dyb

        IF( CheckDomainOverlap( xmin1,xmax1,ymin1,ymax1,dxb,dyb,lgrdx,lgrdy ) )THEN
          nError   = DM_ERROR
          eRoutine = 'CheckAmbDomain: SCIPUFF/SFlux domain mismatch '
          eMessage = 'Do you want SCIPUFF to adjust the project domain and continue?'
          WRITE(eInform,'(A,4F10.2)') 'SFlux   Domain : ',xmin1,xmax1,ymin1,ymax1
          WRITE(eAction,'(A,4F10.2)') 'Project Domain : ',xmin,xmax,ymin,ymax
          CALL WarningMessage( .FALSE. )
          IF( nError /= NO_ERROR )THEN
            CALL init_error()
            GOTO 9999
          ELSE
            xmin = MAX(xmin,xmin1)
            xmax = MIN(xmax,xmax1)
            ymin = MAX(ymin,ymin1)
            ymax = MIN(ymax,ymax1)
            IF( xmin > xmax .OR. ymin > ymax )THEN
              nError   = DM_ERROR
              eMessage = 'Project domain is outside the SFlux grid'
              GOTO 9999
            END IF
          END IF
        END IF

      END IF
    END IF

  END IF

END DO

9999 CONTINUE

RETURN
END
!=======================================================================

SUBROUTINE create_surface()

!  Initialize the surface field arrays

USE scipuff_fi
USE surface_fi
USE files_fi
USE chem_fi
USE srfparam_fd
USE sagdef_fd
USE PtrGrdStrItf
USE srfaux_fi
USE sagerr_fd

IMPLICIT NONE

CHARACTER(128) cmsg, cmsg2, cmsg3

INTEGER,       DIMENSION(:), ALLOCATABLE :: ifld, iaux
CHARACTER(64), DIMENSION(:), ALLOCATABLE :: blkname

INTEGER ndos_blocks, irv, ios, i
INTEGER id, imat

CHARACTER(PATH_MAXLENGTH) :: file_dosAdj

INTEGER, EXTERNAL :: SAG_NewGrdStr, SAG_InitGridID, SAG_InitGridFileID, SAG_LastError
INTEGER, EXTERNAL :: SAG_CloseID

!------ Progress box message

cmsg  = CHAR(0)
cmsg2 = CHAR(0)
cmsg3 = 'Creating surface output files'
CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ Initialize surface blocks

CALL init_srf_blocks( ntypm )
IF( nError /= NO_ERROR )GOTO 9999

NULLIFY( ibaux_srf )
NULLIFY( cmaux_srf )
NULLIFY( cvaux_srf )

!------ Create deposition file

IF( surface )THEN

  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Creating surface deposition file'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  irv = SAG_NewGrdStr( srfdep )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error creating surface deposition grid'
    GOTO 9999
  END IF

  irv = SAG_InitGridID( file_dep,lun_dep,SAG_GRID_BOTH,MAXSG, &
                                              ntyps,ntyps,srfdep )
  IF( irv /= SAG_OK )THEN
    irv = SAG_LastError(eInform, eAction)
    eRoutine = 'create_surface'
    nError = UK_ERROR
    eMessage = 'Error creating surface deposition grid'
    IF( irv /= SAG_ERR_INT4 )THEN
      eInform=''
      eAction=''
    END IF
    GOTO 9999
  END IF

  ALLOCATE( ifld(ndep_blocks),iaux(ndep_blocks),STAT=ios )
  IF( ios == 0 )ALLOCATE( blkname(ndep_blocks),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error allocating surface data structures'
    GOTO 9999
  END IF

  DO i = 1,ndep_blocks
    blkname(i) = srf_block(i)%name
    ifld(i)    = srf_block(i)%field
    iaux(i)    = srf_block(i)%iaux
  END DO

  irv = SAG_InitGridFileID( srfdep,ndep_blocks,ntyps,iversion,ifld,iaux, &
                                                          blkname,srfnam )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error creating surface deposition field'
    GOTO 9999
  END IF

  DEALLOCATE( blkname,  STAT=ios )
  DEALLOCATE( ifld,iaux,STAT=ios )

  CALL create_it( srfdep )
  IF( nError /= NO_ERROR) GOTO 9999

!----- Get pointer to dep structure

  Psrfdep => SAG_PtrGrdStr( srfdep ) ! Associate "local" grid structure pointer

!----- Get pointer to aux data structure

  Pauxdep => SAG_PtrAuxStr( srfdep ) ! Associate "local" grid structure pointer

END IF

!------ Create dose file

IF( dose )THEN

  cmsg  = CHAR(0)
  cmsg2 = CHAR(0)
  cmsg3 = 'Creating surface dosage file'
  CALL write_progress( cmsg,cmsg2,cmsg3 )

  IF( BTEST(run_mode,REVERSE_MODE) )THEN

    ndos_blocks = nsrf_blocks - ndep_blocks
    IF( ndos_blocks > 999 )THEN
      nError   = UK_ERROR
      eRoutine = 'create_surface'
      eMessage = 'Too many adjoint materials'
      eInform  = 'Max is 999 for continous release search'
      GOTO 9999
    END IF

    ALLOCATE( srfdosAdj(ndos_blocks),STAT=ios )

    IF( ios == 0 )ALLOCATE( ifld(1),iaux(1),blkname(1),STAT=ios )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'create_surface'
      eMessage = 'Error allocating surface data structures'
      GOTO 9999
    END IF

    DO i = 1,ndos_blocks

      irv = SAG_NewGrdStr( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'create_surface'
        eMessage = 'Error creating surface dose grid'
        GOTO 9999
      END IF

      WRITE(file_dosAdj,'(A,I3.3)') TRIM(file_dos),i

      irv = SAG_InitGridID( file_dosAdj,lun_dos,SAG_GRID_BOTH,MAXSG, &
                                                    3,3,srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'create_surface'
        eMessage = 'Error creating surface dose grid'
        CALL ReportFileName( eInform,'File=',file_dosAdj )
        GOTO 9999
      END IF

      blkname(1) = srf_block(i+ndep_blocks)%name
      ifld(1)    = srf_block(i+ndep_blocks)%field
      iaux(1)    = srf_block(i+ndep_blocks)%iaux

      irv = SAG_InitGridFileID( srfdosAdj(i),1,3,iversion,ifld,iaux, &
                                       blkname,srfnam(ntyps+1+3*(i-1)) )
      IF( irv /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'create_surface'
        eMessage = 'Error creating surface dose field'
        CALL ReportFileName( eInform,'File=',file_dosAdj )
        GOTO 9999
      END IF

      CALL create_it( srfdosAdj(i) )
      IF( nError /= NO_ERROR) GOTO 9999

      irv = SAG_CloseID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        nError   = UK_ERROR
        eRoutine = 'create_surface'
        eMessage = 'Error closing surface dose field'
        CALL ReportFileName( eInform,'File=',file_dosAdj )
        GOTO 9999
      END IF

    END DO

    DEALLOCATE( blkname,  STAT=ios )
    DEALLOCATE( ifld,iaux,STAT=ios )

  ELSE
  irv = SAG_NewGrdStr( srfdos )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error creating surface dose grid'
    GOTO 9999
  END IF

  irv = SAG_InitGridID( file_dos,lun_dos,SAG_GRID_BOTH,MAXSG, &
                                          ntypd,ntypd,srfdos )
  IF( irv /= SAG_OK )THEN
    irv = SAG_LastError(eInform, eAction)
    eRoutine = 'create_surface'
    nError = UK_ERROR
    eMessage = 'Error creating surface dosage grid'
    IF( irv /= SAG_ERR_INT4 )THEN
      eInform=''
      eAction=''
    END IF
    GOTO 9999
  END IF

  ndos_blocks = nsrf_blocks - ndep_blocks

  ALLOCATE( ifld(ndos_blocks),iaux(ndos_blocks),STAT=ios )
  IF( ios == 0 )ALLOCATE( blkname(ndos_blocks),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error allocating surface data structures'
    GOTO 9999
  END IF
  IF( multicomp )THEN
    ! Ambient dosage
    irv = SAG_NewGrdStr( srfados )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'create_surface'
      eMessage = 'Error creating surface ambient dose grid'
      GOTO 9999
    END IF

    irv = SAG_InitGridID( file_ados,lun_ados,SAG_GRID_BOTH,MAXSG, &
                                            ntypd,ntypd,srfados )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'create_surface'
      eMessage = 'Error creating surface ambient dose grid'
      GOTO 9999
    END IF
  END IF
  DO i = 1,ndos_blocks
    blkname(i) = srf_block(i+ndep_blocks)%name
    ifld(i)    = srf_block(i+ndep_blocks)%field
    iaux(i)    = srf_block(i+ndep_blocks)%iaux
  END DO

  irv = SAG_InitGridFileID( srfdos,ndos_blocks,ntypd,iversion,ifld,iaux, &
                                   blkname,srfnam(ntyps+1) )
  IF( irv /= SAG_OK )THEN
    nError   = UK_ERROR
    eRoutine = 'create_surface'
    eMessage = 'Error creating surface dose field'
    GOTO 9999
  END IF
  IF( multicomp )THEN
    ! Ambient dosage
    irv = SAG_InitGridFileID( srfados,ndos_blocks,ntypd,iversion,ifld,iaux, &
                                     blkname,srfnam(ntyps+1) )
    IF( irv /= SAG_OK )THEN
      nError   = UK_ERROR
      eRoutine = 'create_surface'
      eMessage = 'Error creating surface dose field'
      GOTO 9999
    END IF

    IF( chem%lAddAmb )THEN
      xChemAmbDosRes = HUGE(0.)
      DO i = 1,ndos_blocks
        IF( srf_block(i+ndep_blocks)%type == SBLK_MULTI_DOS )THEN
          id   = srf_block(i+ndep_blocks)%id
          imat = id - (id/65536)*65536
          CALL SetChemAmbDosRes( imat )
        END IF
      END DO
    END IF
    CALL create_it( srfados )
    IF( nError /= NO_ERROR) GOTO 9999
    !lChemAmbDosRes = .FALSE.          !Reset for other calls to create_it
    !----- Get pointer to dos structure
    Psrfados => SAG_PtrGrdStr( srfados ) ! Associate "local" grid structure pointer
    !----- Get pointer to aux data structure
    Pauxados => SAG_PtrAuxStr( srfados ) ! Associate "local" grid structure pointer
  END IF

  DEALLOCATE( blkname,  STAT=ios )
  DEALLOCATE( ifld,iaux,STAT=ios )

  CALL create_it( srfdos )
  IF( nError /= NO_ERROR) GOTO 9999

  lChemAmbDosRes = .FALSE.  !Reset for other calls to create_it

!----- Get pointer to dos structure

  Psrfdos => SAG_PtrGrdStr( srfdos ) ! Associate "local" grid structure pointer

!----- Get pointer to aux data structure

  Pauxdos => SAG_PtrAuxStr( srfdos ) ! Associate "local" grid structure pointer

  END IF

END IF

9999 CONTINUE

IF( ALLOCATED(blkname) )DEALLOCATE( blkname,STAT=ios )
IF( ALLOCATED(ifld)    )DEALLOCATE( ifld,   STAT=ios )
IF( ALLOCATED(iaux)    )DEALLOCATE( iaux,   STAT=ios )

RETURN
END
