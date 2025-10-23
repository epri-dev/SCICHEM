MODULE StepScipuff_fi

! Stepping information

  SAVE

  LOGICAL lcontinue
  INTEGER istep, nstep, nend

END MODULE StepScipuff_fi

!===============================================================================

SUBROUTINE scipuff()

!-----  3d-puff dynamic model with second-order closure integrals

USE scipuff_fi
USE StepScipuff_fi
USE adjoint_fi
USE files_fi
USE chem_fi

IMPLICIT NONE

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER istat

!------ Initialize the project for calculation

CALL InitScipuff()
IF( nError /= NO_ERROR )GOTO 9999

!------ main integration loop  -------

MainLoop: DO WHILE( lcontinue )

!------ increment output time

  IF( restart )THEN
    restart = .FALSE.
    IF( t >= t_save )THEN
      t_save = t_save + dt_save
    END IF
  ELSE
    t_save = t_save + dt_save
  END IF

!------- compute no. of steps to next output time or end of run

  IF( t_save > tend_r )THEN
    nstep = HUGE(nstep)
  ELSE
    nstep = INT((t_save-t)/delt+0.5)
  END IF
  nend  = INT((tend_r-t)/delt+0.5)
  nstep = MIN(nstep,nend)
  nstep = MAX(nstep,1)

  istep     = 1
  lcontinue = .TRUE.

!-------- calculation loop

  DO WHILE( istep <= nstep .AND. lcontinue )

    CALL run( lcontinue,istep )
    IF( nError /= NO_ERROR )GOTO 9999

  END DO

!-------  output

  lcontinue = lcontinue .AND. (t+0.5*delt <= tend_r) .AND. (istop /= 2)

  CALL write_progress_bar( 0 )

  IF( .NOT.lcontinue .AND. npuf==0 )THEN

    eRoutine = 'scipuff'
    eMessage = 'Stopping run'
    WRITE(eInform,'(A,1PG11.4,A)')'No more puffs at Time = ',t/3600.,' hrs'
    CALL InfoMessage( )

!  ELSE IF( BTEST(run_mode,REVERSE_MODE) )THEN

!    IF( t <= tFirstTrigger )CYCLE MainLoop

    cmsg  = TRIM(name)//' : Writing output'
    cmsg2 = CHAR(0)
    cmsg3 = CHAR(0)
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999

END DO MainLoop

9999 CONTINUE

!------ Exit SCIPUFF, even on error

CALL ExitScipuff( .FALSE. )

RETURN
END

!===============================================================================

SUBROUTINE InitScipuff()

!-----  Initialize SCIPUFF calculation

USE scipuff_fi
USE files_fi
USE StepScipuff_fi

IMPLICIT NONE

INTEGER ios

CHARACTER(12)           :: sysTime, cPuff
CHARACTER(12), EXTERNAL :: sysGetTime
CHARACTER(12), EXTERNAL :: FormatPuffs

!------ set code version number

CALL set_version( iversion_code )
SCIPUFFinProgress = .TRUE.

!------ initialization

CALL InitOMPError()
IF( nError /= NO_ERROR )GOTO 9999

CALL initial()
IF( nError /= NO_ERROR )GOTO 9999

lsplit_report = .TRUE.
lcontinue     = .NOT.create
istop         = 0

IF( t_save > tend_r )THEN
  nstep = HUGE(nstep)
ELSE
  nstep = INT((t_save-t)/delt+0.5)
END IF
nend  = INT((tend_r-t)/delt+0.5)
nstep = MIN(nstep,nend)
nstep = MAX(nstep,1)

istep = 0

!------ exit if only doing CREATE

IF( create )GOTO 9999

!------ setup for integration

sysTime = SysGetTime()
cPuff   = FormatPuffs(npuf)

WRITE(lun_log,111,IOSTAT=ios)'Starting run at t =',t/3600., &
                             'hrs. with NPUFF = '//TRIM(cPuff)//' at '//sysTime
111 FORMAT(A,1PG11.4,A)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'scipuff'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

CALL enableSCIPUFFhalt( istop )

9999 CONTINUE

RETURN
END

!===============================================================================
SUBROUTINE CheckPause()

!------ Pause calculation if PAUSE file exists
!       Check for RESUME file every 2 seconds

USE SCIPUFF_fi
USE files_fi
USE SCIPresults_fd

IMPLICIT NONE

INTEGER irv
LOGICAL lexist

INTEGER, DIMENSION (8) :: values

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: sysDeleteFile
CHARACTER(12), EXTERNAL :: FormatPuffs

INQUIRE(FILE=file_pause,EXIST=lexist)
IF( lexist )THEN

  irv = sysDeleteFile( file_pause )
  IF( irv /= SCIPsuccess )THEN
    nError = UK_ERROR
    eRoutine = 'CheckPause'
    eMessage = 'Error deleting PAUSE file'
    GOTO 9999
  END IF

  INQUIRE(FILE=file_resum,EXIST=lexist)
  IF( lexist )THEN
    irv = sysDeleteFile( file_resum )
    IF( irv /= SCIPsuccess )THEN
      nError = UK_ERROR
      eRoutine = 'CheckPause'
      eMessage = 'Error deleting RESUME file'
      GOTO 9999
    END IF
  END IF

  CALL DATE_AND_TIME(VALUES = values)
  WRITE(cmsg,'(A,": Paused at system time ",I2.2,":",I2.2,":",I2.2)')TRIM(name),values(5),values(6),values(7)
  CALL time_message( cmsg2,t )
  cmsg3 = TRIM(FormatPuffs(npuf))//' Puffs'
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999

  WRITE(lun_log,'(A)',IOSTAT=irv)TRIM(cmsg)
  FLUSH(lun_log,IOSTAT=irv) !N.B. May still need to "type" file twice
                            !to fully display log file
  DO

    INQUIRE(FILE=file_resum,EXIST=lexist)

    IF( lexist )THEN

      irv = sysDeleteFile( file_resum )
      IF( irv /= SCIPsuccess )THEN
        nError = UK_ERROR
        eRoutine = 'CheckPause'
        eMessage = 'Error deleting RESUME file'
        GOTO 9999
      END IF

      CALL DATE_AND_TIME(VALUES = values)
      WRITE(cmsg,'(A,": Resumed at system time ",I2.2,":",I2.2,":",I2.2)')TRIM(name),values(5),values(6),values(7)
      cmsg2 = ''
      cmsg3 = ''
      CALL write_progress( cmsg,cmsg2,cmsg3 )
      IF( nError /= NO_ERROR )GOTO 9999
      WRITE(lun_log,'(A)',IOSTAT=irv)TRIM(cmsg)
      FLUSH(lun_log,IOSTAT=irv)

      EXIT

    END IF

    CALL sysSleep( 2000 )  !Wait 2 seconds

  END DO

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

LOGICAL FUNCTION StepScipuff( tStep )

USE scipuff_fi
USE StepScipuff_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: tStep    !SCIPUFF time at end of step

!-----  Take single SCIPUFF step

CHARACTER(80) cmsg,cmsg2,cmsg3

StepScipuff = .FALSE.

IF( .NOT.lcontinue )GOTO 9999

IF( istep == 0 )THEN

!------ increment output time

  IF( restart )THEN
    restart = .FALSE.
    IF( t >= t_save )THEN
      t_save = t_save + dt_save
    END IF
  ELSE
    t_save = t_save + dt_save
  END IF

!------- compute no. of steps to next output time or end of run

  IF( t_save > tend_r )THEN
    nstep = HUGE(nstep)
  ELSE
    nstep = INT((t_save-t)/delt+0.5)
  END IF
  nend  = INT((tend_r-t)/delt+0.5)
  nstep = MIN(nstep,nend)
  nstep = MAX(nstep,1)

END IF

!-------- calculation step

CALL run( lcontinue,istep )
IF( nError /= NO_ERROR )GOTO 9999

lcontinue = lcontinue .AND. (t+0.5*delt <= tend_r) .AND. (istop /= 2)

!------- Check for output

IF( istep == nstep )THEN

  CALL write_progress_bar( 0 )

  IF( .NOT.lcontinue .AND. npuf==0 )THEN

    eRoutine = 'scipuff'
    eMessage = 'Stopping run'
    WRITE(eInform,'(A,1PG11.4,A)')'No more puffs at Time = ',t/3600.,' hrs'
    CALL InfoMessage( )

  ELSE

    cmsg  = TRIM(name)//' : Writing output'
    cmsg2 = CHAR(0)
    cmsg3 = CHAR(0)
    CALL write_progress( cmsg,cmsg2,cmsg3 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999

!----- Reset step counter

  istep = 0

END IF

StepScipuff = lcontinue

9999 CONTINUE

tStep = t

RETURN
END

!===============================================================================

SUBROUTINE ExitScipuff( ssFlag )

USE scipuff_fi
USE StepScipuff_fi
USE met_fi
USE SWIMparam_fd
USE sampler_fi

IMPLICIT NONE

!-----  Exit SCIPUFF calculation

LOGICAL, INTENT( IN ) :: ssFlag  !Flag for Single Step mode

INTEGER irv

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: SWIMupdateMet
INTEGER, EXTERNAL :: SWIMoutput

IF( nError /= NO_ERROR )GOTO 9999

!------ End of integration loop - show just abort button

CALL enableSCIPUFFhalt( SCIPUFF_HALT )

!------ Check output at end of run for Single Step mode

IF( istep /= nstep .AND. ssFlag )THEN
  CALL output_all()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ output met at end of run

IF( .NOT.create .AND. lout_met .AND. t > timeOutMet )THEN

!------ interpolate met to current time & set turbulence

  irv = SWIMupdateMet( t,SWIMstatus )
  CALL enableSCIPUFFhalt( SCIPUFF_HALT )  !Show just abort button
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMupdateMet' )
    GOTO 9999
  END IF

  CALL WriteSWIMlog()

!------ output met fields

  cmsg  = 'Outputting wind field at end of run'
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

END IF

!------ Output current (partial) sampler averages if not at output time

IF( .NOT.create )THEN
  IF( nsmp > 0 )THEN
    IF( (int_sensor .AND. lUpdateSum) .AND. (lSmpOut .OR. lavg) )THEN
      IF( t > tStartSamp )THEN
        IF( lavg )THEN
          IF( .NOT.lOutToSmp )CALL out_smp()
        ELSE
          IF( t < tSmpOut-tolSmpOut )CALL out_smp()
        END IF
      END IF
    END IF
  END IF
END IF

IF( nsmp > 0 )THEN
  CALL finalize_binary_sampler()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

!------ close down

CALL end_scipuff()
CALL enableSCIPUFFhalt( SCIPUFF_DISABLE )
SCIPUFFinProgress = .FALSE.

RETURN
END

!===============================================================================

SUBROUTINE output_all()

USE scipuff_fi
USE surface_fi
USE files_fi
USE sagdef_fd
USE chem_fi
USE cont_rel_functions
USE StepScipuff_fi
USE step_p_fi, ONLY: myTId
USE met_fi
USE omp_lib
USE met_fi
USE SWIMparam_fd
USE SCIPresults_fd
USE PtrGrdStrItf

IMPLICIT NONE

CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER ios, irv
INTEGER i
INTEGER maxlev
INTEGER ndos_blocks
REAL    tdum

REAL tmp

INTEGER, EXTERNAL :: SAG_WriteBreakID
INTEGER, EXTERNAL :: SWIMupdateMet
INTEGER, EXTERNAL :: SetMetGrid
INTEGER, EXTERNAL :: SAG_ClearGridID
INTEGER, EXTERNAL :: SAG_OpenID, SAG_LastTime, SAG_CloseID

CHARACTER(12)           :: sysTime, cPuffs, cRels
CHARACTER(12), EXTERNAL :: sysGetTime
CHARACTER(12), EXTERNAL :: FormatPuffs

cmsg = TRIM(name)//' : Saving Output'
CALL time_message( cmsg2,t )
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

!------ Update met to current time
!       N.B. Mainly for saved BL depth and subsequent restarts

IF( .NOT.create )THEN
  irv = SWIMupdateMet( t,SWIMstatus )
  CALL enableSCIPUFFhalt( SCIPUFF_HALT )  !Show just abort button
  IF( irv /= SWIMsuccess )THEN
    CALL setSWIMerror( 'SWIMupdateMet' )
    GOTO 9999
  END IF

  IF( BTEST(SWIMstatus,SSB_EXPAND) )THEN
    irv = SetMetGrid()
    IF( irv /= SCIPsuccess )GOTO 9999
  END IF

  CALL WriteSWIMlog()
END IF

!$OMP PARALLEL  DEFAULT(SHARED) PRIVATE(irv,tmp,i,ndos_blocks) num_threads(MIN(nthreads,4))
!$OMP SINGLE
cmsg3 = 'Completed puff,prj'

!$OMP TASK
myTId = OMP_GET_THREAD_NUM() + 1
IF( omp_error(myTId)%nError == NO_ERROR )THEN
  CALL compress_Definition()

  CALL write_puff()
  IF( nError /= NO_ERROR )THEN
    omp_error(myTId)%nError   = nError
    omp_error(myTId)%eRoutine = eRoutine
    omp_error(myTId)%eMessage = eMessage
    omp_error(myTId)%eInform  = eInform
    omp_error(myTId)%eAction  = eAction
  END IF
END IF

IF( omp_error(myTId)%nError == NO_ERROR )THEN
  CALL write_prj()
  IF( nError /= NO_ERROR )THEN
    omp_error(myTId)%nError   = nError
    omp_error(myTId)%eRoutine = eRoutine
    omp_error(myTId)%eMessage = eMessage
    omp_error(myTId)%eInform  = eInform
    omp_error(myTId)%eAction  = eAction
  END IF
END IF
!$OMP END TASK

IF( surface )THEN
cmsg3 = TRIM(cmsg3)//',dep'

!$OMP TASK
  myTId = OMP_GET_THREAD_NUM() + 1
  IF( omp_error(myTId)%nError /= NO_ERROR )GOTO 99999
  tmp = t/3600.
  irv = SAG_WriteBreakID( srfdep,tmp )
  IF( irv /= SAG_OK )THEN
    omp_error(myTId)%nError   = WR_ERROR
    omp_error(myTId)%eRoutine = 'output_all'
    omp_error(myTId)%eMessage = 'Error writing surface deposition file'
    GOTO 99999
  END IF
  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( istep >= nstep )THEN
      Psrfdep => SAG_PtrGrdStr( srfdep )
      maxlev = Psrfdep%maxlev
      irv = SAG_ClearGridID( srfdep )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = WR_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error clearing surface dep grid'
        GOTO 99999
      END IF
      tLastDep = t
      Psrfdep%maxlev = maxlev
    END IF
  END IF
99999 CONTINUE
!$OMP END TASK
END IF

IF( dose )THEN
cmsg3 = TRIM(cmsg3)//',dos'

!$OMP TASK
  myTId = OMP_GET_THREAD_NUM() + 1
!$OMP FLUSH( omp_error )
  IF( omp_error(myTId)%nError == NO_ERROR )THEN
  tmp = t/3600.
  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    ndos_blocks = ntypd/3
    DO i = 1,ndos_blocks
      irv = SAG_OpenID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = UK_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error closing surface dose field'
!$OMP FLUSH( omp_error )
        GOTO 99998
      END IF

      irv = SAG_LastTime( srfdosAdj(i),tdum,.TRUE. )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = WR_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error positioning surface dose file'
!$OMP FLUSH( omp_error )
        GOTO 99998
      END IF

      irv = SAG_WriteBreakID( srfdosAdj(i),tmp )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = WR_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error writing surface dose file'
!$OMP FLUSH( omp_error )
        GOTO 99998
      END IF

      irv = SAG_CloseID( srfdosAdj(i) )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = WR_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error closing surface dose field'
!$OMP FLUSH( omp_error )
        GOTO 99998
      END IF
    END DO
  ELSE

  srfdos = Psrfdos%ID
  IF( multicomp )srfados = Psrfados%ID
  irv = SAG_WriteBreakID( srfdos,tmp )
  IF( irv /= SAG_OK )THEN
    omp_error(myTId)%nError   = WR_ERROR
    omp_error(myTId)%eRoutine = 'output_all'
    omp_error(myTId)%eMessage = 'Error writing surface dose file'
!$OMP FLUSH( omp_error )
    GOTO 99998
  END IF
  ! Ambient dose
  IF( multicomp )THEN
    irv = SAG_WriteBreakID( srfados,tmp )
    IF( irv /= SAG_OK )THEN
      omp_error(myTId)%nError   = WR_ERROR
      omp_error(myTId)%eRoutine = 'output_all'
      omp_error(myTId)%eMessage = 'Error writing surface ambient dose file'
!$OMP FLUSH( omp_error )
      GOTO 99998
    END IF
  END IF

  IF( BTEST(run_mode,DINCRMNT) )THEN
    IF( istep >= nstep )THEN
      Psrfdos => SAG_PtrGrdStr( srfdos )
      maxlev = Psrfdos%maxlev
      irv = SAG_ClearGridID( srfdos )
      IF( irv /= SAG_OK )THEN
        omp_error(myTId)%nError   = WR_ERROR
        omp_error(myTId)%eRoutine = 'output_all'
        omp_error(myTId)%eMessage = 'Error clearing surface dos grid'
!$OMP FLUSH( omp_error )
        GOTO 99998
      END IF
      Psrfdos%maxlev = maxlev
     ! Ambient dose
      IF( multicomp )THEN
        Psrfados => SAG_PtrGrdStr( srfados )
        maxlev = Psrfados%maxlev
        irv = SAG_ClearGridID( srfados )
        IF( irv /= SAG_OK )THEN
          omp_error(myTId)%nError   = WR_ERROR
          omp_error(myTId)%eRoutine = 'output_all'
          omp_error(myTId)%eMessage = 'Error clearing surface ambient dos grid'
!$OMP FLUSH( omp_error )
          GOTO 99998
        END IF
        Psrfados%maxlev = maxlev
      END IF
      tLastDos = t
    END IF
  END IF

  END IF
99998 CONTINUE
  END IF
!$OMP END TASK
END IF

cmsg3 = TRIM(cmsg3)//' files'
!$OMP END SINGLE
!$OMP END PARALLEL
CALL write_progress( cmsg,cmsg2,cmsg3 )
!$OMP FLUSH( omp_error )
IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
  DO i = 1,nthreads
    IF( omp_error(i)%nError /= NO_ERROR )THEN
      nError   = omp_error(i)%nError
      eRoutine = omp_error(i)%eRoutine
      eMessage = omp_error(i)%eMessage
      eInform  = omp_error(i)%eInform
    END IF
  END DO
  GOTO 9999
END IF

IF( multicomp )THEN
  IF( DoChemistry )THEN
  WRITE(lun_log,112,IOSTAT=ios)'Number of successful chemistry steps =',ngd_chem
  WRITE(lun_log,112,IOSTAT=ios)'Number of aborted chemistry steps   =',nbad_chem
  112 FORMAT(A,1PG11.4)
  END IF
  ngd_chem  = 0
  nbad_chem = 0
  CALL CreateChemOutput()
END IF

sysTime = SysGetTime()
cPuffs  = FormatPuffs(npuf)
cRels   = FormatPuffs(count_nrel())
WRITE(lun_log,111,IOSTAT=ios)'Output completed at t =',t/3600., &
                    'hrs. with NCREL = '//TRIM(cRels)//' and NPUFF = ' &
                     //TRIM(cPuffs)//' at '//sysTime
111 FORMAT(A,1PG11.4,A)

IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'output_all'
  eMessage = 'Error writing SCIPUFF log file'
  CALL ReportFileName( eInform,'File=',file_log )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

CHARACTER(*) FUNCTION FormatPuffs( n )
IMPLICIT NONE
INTEGER, INTENT(IN) :: n

INTEGER ios
CHARACTER(12) ctmp

WRITE(ctmp,'(I10)',IOSTAT=ios)n

FormatPuffs = TRIM(ADJUSTL(ctmp))

RETURN
END

!===============================================================================

SUBROUTINE end_scipuff()

USE scipuff_fi
USE files_fi
USE met_fi

IMPLICIT NONE

INTEGER ios, nch
INTEGER j
CHARACTER(80) cmsg,cmsg2,cmsg3

INTEGER, EXTERNAL :: SWIMexitRun
CHARACTER(12), EXTERNAL :: FormatPuffs

cmsg = TRIM(name)//' : Exiting SCIPUFF'
cmsg2 = ' '
cmsg3 = ' '
CALL write_progress( cmsg,cmsg2,cmsg3 )

IF( nError == NO_ERROR )THEN
  eRoutine = 'EndScipuff'
  eInform  = 'No. Puffs = '//TRIM(FormatPuffs(npuf))
  IF( istop < 0 )THEN
    nError = UK_ERROR
    WRITE(eMessage,'(A,1PG11.4,A)')'Abnormal termination detected at Time = ',t/3600.,' hrs'
  ELSE IF( istop > 0 )THEN
    nError = S0_ERROR
    SELECT CASE( istop )
      CASE (1)
        WRITE(eMessage,'(A,1PG11.4,A)')'User requested Halt detected at Time = ',t/3600.,' hrs'
      CASE (2)
        WRITE(eMessage,'(A,1PG11.4,A)')'User StopOnOutput request detected at Time = ',t/3600.,' hrs'
      CASE DEFAULT
        WRITE(eMessage,'(A,1PG11.4,A)')'Unknown Halt detected at Time = ',t/3600.,' hrs'
    END SELECT
    eInform = 'No. Puffs = '//TRIM(FormatPuffs(npuf))
  ELSE
    WRITE(eMessage,'(A,1PG11.4,A)')'Normal termination detected at Time = ',t/3600.,' hrs'
  END IF
END IF

IF( nError == S0_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'************HALT***************'
ELSE IF( nError == WN_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'**********WARNING**************'
ELSE IF( nError /= NO_ERROR )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'***********ERROR***************'
ELSE
  WRITE(lun_log,'(A)',IOSTAT=ios)'************DONE***************'
END IF

nch = LEN_TRIM(eRoutine)
IF( nch > 1 )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eRoutine)
END IF
WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eMessage)
WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eInform)
nch = LEN_TRIM(eAction)
IF( nch > 1 )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)TRIM(eAction)
END IF
WRITE(lun_log,'(A)',IOSTAT=ios)'*********************************'

IF( ios /= 0 )GOTO 9998

9997 CONTINUE

OPEN(UNIT=lun_err,FILE=file_err,STATUS='UNKNOWN',IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'end_scipuff'
  eMessage = 'Error opening SCIPUFF error file'
  CALL ReportFileName( eInform,'File=',file_err )
ELSE
  WRITE(lun_err,'(I6,A)',IOSTAT=ios)nError,' :Scipuff Exit Status'
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eRoutine)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eMessage)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eInform)
  WRITE(lun_err,'(A)',IOSTAT=ios)TRIM(eAction)
  CLOSE(UNIT=lun_err,IOSTAT=ios)
END IF

CLOSE(UNIT=lun_puf,IOSTAT=ios)
CLOSE(UNIT=lun_prj,IOSTAT=ios)
CLOSE(UNIT=lun_tmp,IOSTAT=ios)
CLOSE(UNIT=lun_inp,IOSTAT=ios)
CLOSE(UNIT=lun_scn,IOSTAT=ios)
CLOSE(UNIT=lun_mcw,IOSTAT=ios)
CLOSE(UNIT=lun_msc,IOSTAT=ios)
CLOSE(UNIT=lun_met,IOSTAT=ios)
CLOSE(UNIT=lun_sfc,IOSTAT=ios)
CLOSE(UNIT=lun_ter,IOSTAT=ios)
CLOSE(UNIT=lun_smp,IOSTAT=ios)
CLOSE(UNIT=lun_sps,IOSTAT=ios)

CALL scipuff_deallocate()

!------ Get last SWIM log messages & deallocate arrays

ios = SWIMexitRun()

CALL WriteSWIMlog()

RETURN

!------ set log write error and goto return

9998 CONTINUE

nError   = WR_ERROR
eRoutine = 'end_scipuff'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9997

END
