SUBROUTINE step()

USE scipuff_fi
USE surface_fi
USE files_fi
USE sampler_fi
USE omp_lib
USE step_p_fi, ONLY: myTId, iPass, puffSplit, puffTemp, npufTemp, nDimTemp
USE error_fi,  ONLY: multiThread
USE grdlock_fi
USE met_fi
USE cont_rel_fi
USE sciprime_fi
USE srfparam_fd
USE chem_fi, ONLY: chemMC,ngd_chem,nbad_chem,tot_bad
USE srfparam_fd
USE cont_rel_functions

IMPLICIT NONE

INTEGER mstep, istep, lev, lev2, npufo
INTEGER ipuf, ilev, nch
INTEGER mchg, nxtlev, ntpuf, nspuf
INTEGER ntem
INTEGER i, id, imat, ifld
INTEGER SmpOutType
INTEGER ios, ipl, loop_threads
#if defined(_OPENMP)
LOGICAL :: lThread = .TRUE.
#endif
INTEGER alloc_stat, j
INTEGER, DIMENSION(:), ALLOCATABLE :: puffList
LOGICAL myOK
REAL    dts, dt, t0_step, t_step, t1_step, dt_dose, fac_srf
LOGICAL compress, lsplit, lUpdateSmp

CHARACTER(80) cmsg,cmsg2,cmsg3
CHARACTER(32) ctem

INTEGER, EXTERNAL :: set_lev
CHARACTER(12), EXTERNAL :: FormatPuffs
INTEGER, EXTERNAL :: extraUpdateDefinition

CALL start_clock()

IF( ALLOCATED(concPrime) )concPrime = 0.

IF( npuf == 0 .AND. countDefinitions() == 0 .AND. &
    InstReleaseList%relSpec%release%trel == NOT_SET_R )THEN

  t = t + delt

  IF( depint )CALL grdose( t,delt )

  cmsg = TRIM(name)//' : Calculating '
  CALL time_message( cmsg2,t )
  IF( nError /= NO_ERROR )GOTO 9999
  CALL c_format( delt,nch,ctem )
  cmsg3 = TRIM(FormatPuffs(npuf))//' Puffs  '//ctem(1:nch)//'s timestep'
  CALL write_progress( cmsg,cmsg2,cmsg3 )
  IF( nError /= NO_ERROR )GOTO 9999

  ntem = 0
  CALL write_progress_bar( ntem )

  IF( lsmp )THEN
    SmpOutType = 0
    SmpOutType = IBSET(SmpOutType,SOB_NOPUFF)
    SmpOutType = IBSET(SmpOutType,SOB_LARGEDELT)
    lUpdateSmp = .FALSE.
    IF( lSmpOut )THEN
      IF( t >= tSmpOut-tolSmpOut )THEN
        lUpdateSmp = .TRUE.
      ELSE IF( int_sensor )THEN
        SmpOutType = IBSET(SmpOutType,SOB_INTONLY)
        lUpdateSmp = .TRUE.
      END IF
    ELSE
      lUpdateSmp = .TRUE.
    END IF
    IF( lUpdateSmp )THEN
      CALL update_smp( SmpOutType )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF

  GOTO 9999

END IF

CALL step_clock()

!------ Update multicomponent chemistry background

IF( multicomp )THEN
  CALL updateRunMC()
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ initialize for large timestep

ntpuf    = npuf
nxtlev   = mxtlev
compress = .FALSE.
lsplit   = .TRUE.


IF( ALLOCATED(iPass) )DEALLOCATE( iPass )
ALLOCATE( iPass(MAXPUF) )
iPass = 0

!------ set time stepping for surface evaporation

IF( evaporation )THEN
  CALL set_evap_step()
  IF( nError /= NO_ERROR )GOTO 9999
  nxtlev = MAX(nxtlev,mxtlev_evap)
  CALL step_clock()
END IF

mstep = 2**nxtlev
dts   = delt/FLOAT(mstep)

!------ report run progress

cmsg = TRIM(name)//' : Calculating '
CALL time_message( cmsg2,t )
IF( nError /= NO_ERROR )GOTO 9999
CALL c_format( dts,nch,ctem )
cmsg3 = TRIM(FormatPuffs(npuf))//' Puffs  '//ctem(1:nch)//'s timestep'
CALL write_progress( cmsg,cmsg2,cmsg3 )
IF( nError /= NO_ERROR )GOTO 9999

ntem = 0
CALL write_progress_bar( ntem )

!-----  Loop over small timesteps

t0_step = t
t1_step = t + delt

t_step  = 0.
istep   = 0
dt_dose = 0.

DO WHILE( istep < mstep )

  istep = istep + 1
  lev   = nxtlev - set_lev( istep )
  lev2  = nxtlev

!-------- always try to split level-0 puffs

  IF( istep == mstep )lsplit = .TRUE.

!-------- continuous releases

  CALL c_release( dts,t1_step,lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

!-------- instantaneous releases

  CALL CheckInstRelease( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL step_clock()

!-------- adjust minimum step if necessary after c_release

  IF( lev2 > nxtlev )THEN
    mchg   = 2**(lev2-nxtlev)
    mstep  = mstep*mchg
    istep  = (istep-1)*mchg + 1
    dts    = delt/FLOAT(mstep)
    nxtlev = lev2
    lev    = nxtlev - set_lev( istep )
  END IF

  npufo = npuf

!-------- Check if any puffs are being stepped

  nspuf = 0
  DO ilev = lev,nxtlev
    nspuf = nspuf + ntlev(ilev)
  END DO

!-------- Loop over time levels

  IF( nspuf > 0 )THEN

    ALLOCATE( puffList(0:nspuf),STAT=alloc_stat )
    IF( multiThread )THEN
      IF( alloc_stat == 0 )ALLOCATE( puffSplit(npuf,3), STAT=alloc_stat )
      IF( alloc_stat == 0 )ALLOCATE( npufTemp(nthreads),STAT=alloc_stat )
      IF( alloc_stat /= 0 )THEN
        nError   = SZ_ERROR
        eRoutine = 'step'
        eMessage = 'Error allocating temporary split storage'
        WRITE(eInform,*) 'npuf = ',npuf
        GOTO 9999
      END IF
    END IF
#if defined(_OPENMP)
    IF( lThread )THEN
      WRITE(lun_log,'(A,I0)',IOSTAT=ios)'Max. number of threads for OpenMP = ',nthreads
      IF( ios /= 0 )GOTO 9998
      lThread = .FALSE.
    END IF
#endif

    myOK = ManageGrdLock( nthreads,2,INIT_GRDLOCK )
    myOK = ManageAuxLock( nthreads,2,-1,INIT_GRDLOCK )

    DO ipl = 1,nthreads
      omp_error(ipl)%nError   = 0
      omp_error(ipl)%eRoutine = eRoutine
      omp_error(ipl)%eMessage = eMessage
      omp_error(ipl)%eInform  = eInform
      omp_error(ipl)%eAction  = eAction
    END DO

    CALL step_clock()

    DO ilev = lev,nxtlev

      dt = dts*FLOAT(2**(nxtlev-ilev))
      puffList(0) = 0

      ipuf = itfrst(ilev)
      IF( ipuf > 0 )THEN
        puffList(0) = puffList(0) + 1
        puffList(1) = ipuf
        DO
          ipuf = puff(ipuf)%idtn
          IF( ipuf <= 0 )EXIT
          puffList(0) = puffList(0) + 1
          puffList(puffList(0)) = ipuf
        END DO
      END IF
      ALLOCATE( nDimTemp(nthreads),STAT=alloc_stat )
      IF( npuf + 8*ntlev(ilev) > MAXPUF )THEN
        lsplit = .FALSE.
        IF( lsplit_report )THEN
          WRITE(lun_log,'(A)',IOSTAT=ios)'********** WARNING **********'
          WRITE(lun_log,*,IOSTAT=ios)'Time = ',t,'( ',t/3600.,' )'
          WRITE(lun_log,*,IOSTAT=ios)'Splitting halted temporarily'
          WRITE(lun_log,*,IOSTAT=ios)'Too many puffs'
          WRITE(lun_log,*,IOSTAT=ios)'Resolution may be compromised'
          WRITE(lun_log,'(A)',IOSTAT=ios)'*****************************'
          lsplit_report = .FALSE.
        END IF
        nStopSplit = nStopSplit + 1
      ELSE
        lsplit = .TRUE.
      END IF
      IF( multiThread )THEN
        IF( lsplit )THEN
          nDimTemp = 4*(INT(FLOAT(ntlev(ilev))/FLOAT(nthreads)) + 1)
        ELSE
          nDimTemp = INT(FLOAT(ntlev(ilev))/FLOAT(nthreads)) + 1
        END IF
        CALL allocate_puffTemp( alloc_stat )
        IF( alloc_stat /= 0 )THEN
          nError   = SZ_ERROR
          eRoutine = 'step'
          eMessage = 'Error allocating temporary split storage'
          WRITE(eInform,*) 'nDimTemp = ',nDimTemp(1)
          GOTO 9999
        END IF

        npufTemp(1:nthreads) = 0
      END IF
      IF( nEvapBlk > 0 )CALL ClearEvapTemp()
      loop_threads = MIN(nthreads,puffList(0))
!$OMP PARALLEL IF(loop_threads > 1)
!$OMP SINGLE
      ipuf = itfrst(ilev)

      DO WHILE( ipuf > 0 )
!$OMP TASK FIRSTPRIVATE(ipuf) PRIVATE(ipl,fac_srf) DEFAULT(SHARED)
!$OMP FLUSH( omp_error )
        IF( ANY(omp_error(:)%nError /= NO_ERROR) )GOTO 12345
        myTId = OMP_GET_THREAD_NUM() + 1
        IF( multiThread )THEN
          puffSplit(ipuf,1) = myTId
          puffSplit(ipuf,2) = 0
        END IF

!-----------  integrate

        fac_srf = NOT_SET_R
        CALL step_p( dt,puff(ipuf),ipuf,lev,lev2,fac_srf )
        IF( nError /= NO_ERROR )THEN
          omp_error(myTId)%nError   = nError
          omp_error(myTId)%eRoutine = eRoutine
          omp_error(myTId)%eMessage = eMessage
          omp_error(myTId)%eInform  = eInform
          omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
          GOTO 12345
        END IF
        IF( lsplit )THEN
          CALL split( ipuf,lsplit )
          IF( nError /= NO_ERROR )THEN
            omp_error(myTId)%nError   = nError
            omp_error(myTId)%eRoutine = eRoutine
            omp_error(myTId)%eMessage = eMessage
            omp_error(myTId)%eInform  = eInform
            omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
            GOTO 12345
          END IF
        END IF

        IF( myTId == nthreads )CALL step_clock()
12345   CONTINUE
!$OMP END TASK

        ipuf = puff(ipuf)%idtn

      END DO
!$OMP END SINGLE
!$OMP END PARALLEL
      IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
        DO ipl = 1,nthreads
          IF( omp_error(ipl)%nError /= NO_ERROR )THEN
            nError   = omp_error(ipl)%nError
            eRoutine = omp_error(ipl)%eRoutine
            eMessage = omp_error(ipl)%eMessage
            eInform  = omp_error(ipl)%eInform
          END IF
        END DO
        GOTO 9999
      END IF

      IF( multiThread )THEN
        DO ipl = 1,puffList(0)
          ipuf = puffList(ipl)
          IF( puffSplit(ipuf,2) > 0 )THEN
            myTId = puffSplit(ipuf,1)
            DO j = puffSplit(ipuf,3)+1,puffSplit(ipuf,3)+puffSplit(ipuf,2)
              npuf = npuf + 1
              CALL check_newpuff()
              IF( nError /= NO_ERROR )GOTO 9999
              CALL copy_puff( puffTemp(myTid)%p(j),puff(npuf) )
              IF( ASSOCIATED(puffTemp(myTid)%p(j)%aux) )DEALLOCATE( puffTemp(myTid)%p(j)%aux,STAT=alloc_stat )
            END DO
          END IF
        END DO
        IF( ALLOCATED(puffTemp) )CALL deallocate_puffTemp( alloc_stat )
      END IF
      IF( nEvapBlk > 0 )CALL MoveEvapTemp()

      CALL check_progress()
      IF( nError /= NO_ERROR )GOTO 9999

    END DO

    IF( ALLOCATED(puffSplit) )DEALLOCATE( puffSplit,STAT=alloc_stat )
    IF( ALLOCATED(npufTemp)  )DEALLOCATE( npufTemp, STAT=alloc_stat )
    IF( ALLOCATED(ndimTemp)  )DEALLOCATE( ndimTemp, STAT=alloc_stat )
    IF( ALLOCATED(puffList) )DEALLOCATE( puffList, STAT=alloc_stat )

    myOK = ManageGrdLock( 0,0,CLR_GRDLOCK )
    myOK = ManageAuxLock( 0,0,0,CLR_GRDLOCK )

  ELSE

!-----------  no puffs, so increase timestep if no evaporation

    IF( nxtlev > 0 .AND. MOD(istep,2) /= 0 )THEN
      IF( (InstReleaseList%relSpec%release%trel == NOT_SET_R .AND. readyDefinition() == 0 ) .OR. &
          ( nxtlev > 7 .AND. extraUpdateDefinition() == 0) )THEN
        IF( mxlev_evap < nxtlev .AND. tlevDefinition() < nxtlev )THEN
          mstep  = mstep/2
          istep  = istep/2
          dts    = dts + dts
          nxtlev = nxtlev - 1
          IF( istep < mstep )CYCLE
        END IF
      END IF
    END IF

  END IF

!-------- Modified accumulation of time to eliminate deactivation/static source problems
!         due to round-off errors when making a long run with a small time step.  (SFP 6/3/98)

  t_step = t_step + dts
  t      = t0_step + t_step

!------ Update multicomponent chemistry background

  IF( multicomp )THEN
    CALL updateRunMC()
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  ntem = NINT(64.*t_step/delt)
  CALL write_progress_bar( ntem )

  dt_dose = dt_dose + dts

  IF( nspuf > 0 )THEN

!----- Set compress flag if too many puffs for splitting
    IF( .NOT.lsplit .AND. (4*npuf > 3*MAXPUF) )compress = .TRUE.

!----- Update surface radiation dose integrals
    IF( depint )CALL grdose( t,dt_dose )
    dt_dose = 0.0

  END IF

  CALL step_clock()

!-------- calculate surface evaporation

  IF( mxlev_evap >= 0 )THEN
    CALL step_srf_evap( lev,lev2 )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL step_clock()
  END IF
!-------  add new puffs into time lists

  IF( npuf > npufo )CALL add_tlev( npufo+1,npuf )

!-------  check for merge and puffs off grid

  CALL merge( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

!-------  check for compress and reset time lists

  IF( istep == mstep )THEN

    CALL compress_puff_list()
    CALL find_mxgrd( nz )
    CALL set_tlev( .FALSE. )

  ELSE

    IF( compress )CALL compress_puff_list()
    CALL reset_tlev()

  END IF

  CALL step_clock()

!-------  compute interaction terms

  CALL inter( lev,lev2 )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL step_clock()

!-------  check for puff cleanup

  IF( istep /= mstep )THEN

    ntpuf = 0
    DO ilev = 0,lev2
      ntpuf = ntpuf + ntlev(ilev)
    END DO
    compress = (npuf > MAXPUF/3) .AND. ((npuf-ntpuf) > npuf/4)
  ELSE

    compress = .FALSE.

  END IF

  CALL step_clock()

!-------- adjust minimum step if necessary

  IF( evaporation )lev2 = MAX( lev2,mxtlev_evap )

  IF( lev2 > nxtlev )THEN
    mchg   = 2**(lev2-nxtlev)
    mstep  = mstep*mchg
    istep  = istep*mchg
    dts    = delt/FLOAT(mstep)
    nxtlev = lev2
  END IF

!------- Compute sampler output

  IF( lsmp )THEN

    istepP = istep  !Save for "advecting" puffs in sampler (GetPuffVal)
    dtsP   = dts
    mstepP = mstep

    SmpOutType = 0
    IF( istep == mstep )SmpOutType = IBSET(SmpOutType,SOB_LARGEDELT)

    lUpdateSmp = .FALSE.
    IF( lSmpOut )THEN
      IF( t >= tSmpOut-tolSmpOut )THEN
        SmpOutType = IBCLR(SmpOutType,SOB_INTONLY); lUpdateSmp = .TRUE.
      ELSE IF( int_sensor )THEN
        SmpOutType = IBSET(SmpOutType,SOB_INTONLY); lUpdateSmp = .TRUE.
      END IF
    ELSE IF( lev <= mxlev_smp )THEN
      SmpOutType = IBSET(SmpOutType,SOB_LARGEDELT)
      SmpOutType = IBCLR(SmpOutType,SOB_INTONLY);   lUpdateSmp = .TRUE.
    ELSE IF( int_sensor )THEN
      SmpOutType = IBSET(SmpOutType,SOB_INTONLY);   lUpdateSmp = .TRUE.
    END IF
    IF( lUpdateSmp )THEN
      CALL update_smp( SmpOutType )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

  END IF

  CALL step_clock()

  CALL check_progress()
  IF( nError /= NO_ERROR )GOTO 9999

END DO

t = t1_step          !Reset t to avoid accumulation errors

runUpdates = .TRUE.  !Run updates at start of next step

!------ Add ambient concentration contribution to multicomponent doses

IF( dose )THEN
  DO i = ndep_blocks+1,nsrf_blocks
    IF( srf_block(i)%type == SBLK_MULTI_DOS )THEN
      id   = srf_block(i)%id
      imat = id - (id/65536)*65536
      ifld = srf_block(i)%Field + 2 !Offset accounting for mean and var
      IF( chemMC(mat_mc%ID( material(imat)%mcID),1)%lAddAmb )THEN
        CALL SetChemAmbDos( imat,ifld,delt )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    END IF
  END DO
END IF

CALL check_progress()
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

IF( ALLOCATED(iPass) )DEALLOCATE( iPass )
IF( ALLOCATED(puffList)  )DEALLOCATE( puffList, STAT=alloc_stat )
IF( ALLOCATED(puffSplit) )DEALLOCATE( puffSplit,STAT=alloc_stat )
IF( ALLOCATED(puffTemp)  )CALL deallocate_puffTemp( alloc_stat )
IF( ALLOCATED(npufTemp)  )DEALLOCATE( npufTemp, STAT=alloc_stat )
IF( ALLOCATED(ndimTemp)  )DEALLOCATE( ndimTemp, STAT=alloc_stat )

CALL stop_clock()

RETURN

!------ set log write error and goto return

9998 CONTINUE

nError   = WR_ERROR
eRoutine = 'step'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

END

!===============================================================================

SUBROUTINE updateRunMC()

USE error_fi
USE files_fi
USE matl_fi
USE chem_fi

IMPLICIT NONE

INTEGER ii, n

! -- Initialize background (if required) for any chemistry multicomponents

DO ii = 1,mat_mc%nMCtype

  IF( mat_mc%type(ii) == MC_CHEM )THEN

    n = mat_mc%ID(ii)

    chem => chemMC(n,1)

    IF( chem%lStepAmb )THEN
      IF( DoChemistry )THEN
        CALL StepChemAmb()
        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    ELSE IF( chem%lAmbFile )THEN
      CALL UpdateAmbFields()
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

  END IF

END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ClearEvapTemp()

USE surface_fi

IMPLICIT NONE

INTEGER i, j, j0

!$OMP PARALLEL DO PRIVATE(j0)
DO j = 1,nEvapFld*nEvapBlk
  j0 = (j-1)*MAXSG
  DO i = 1,PsrfDep%ncells
    evapTemp(i+j0) = 0.D0
  END DO
END DO
!$OMP END PARALLEL DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE MoveEvapTemp()

USE surface_fi

IMPLICIT NONE

INTEGER i, j, k, i0, j0

DO j = 1,nEvapBlk
  DO k = 1,nEvapFld
    i0 = (evapFld(j)+k-2)*PsrfDep%mxgrd
    j0 = ((j-1)*nEvapFld+k-1)*MAXSG
!$OMP PARALLEL DO SHARED(i0,j0)
    DO i = 1,PsrfDep%ncells
      PsrfDep%ipdat(i0+i) = PsrfDep%ipdat(i0+i) + SNGL(evapTemp(i+j0))
    END DO
!$OMP END PARALLEL DO
  END DO
END DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE allocate_PuffTemp( alloc_stat )

USE step_p_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: alloc_stat

INTEGER i, j

ALLOCATE( PuffTemp(nthreads),STAT=alloc_stat )
DO i = 1,nthreads
  IF( alloc_stat== 0 )ALLOCATE( PuffTemp(i)%p(nDimTemp(i)),STAT=alloc_stat )
  IF( alloc_stat== 0 )THEN
    DO j = 1,nDimTemp(i)
      NULLIFY( PuffTemp(i)%p(j)%aux )
    END DO
  END IF
END DO

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE deallocate_PuffTemp( alloc_stat )

USE step_p_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: alloc_stat

INTEGER i, j

DO i = 1,nthreads
  DO j = 1,nDimTemp(i)
    IF( ASSOCIATED(PuffTemp(i)%p(j)%aux) )DEALLOCATE( PuffTemp(i)%p(j)%aux,STAT=alloc_stat )
  END DO
  DEALLOCATE( puffTemp(i)%p,STAT=alloc_stat )
END DO
DEALLOCATE( PuffTemp,STAT=alloc_stat )

RETURN
END

!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE AddEvapTemp( ic )

USE scipuff_fi
USE surface_fi
USE step_p_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ic

INTEGER alloc_stat, ii, ic0, myId

IF( PuffSplit(ic,1) /= 0 )THEN
  myId = PuffSplit(ic,1)
  CALL check_newpuff()
  IF( nError /= NO_ERROR )GOTO 9999
  npuf = npuf + 1
  CALL copy_puff( puffTemp(myId)%p(PuffSplit(ic,2)),puff(npuf) )
  IF( ASSOCIATED(puffTemp(myId)%p(PuffSplit(ic,2))%aux) ) &
      DEALLOCATE( puffTemp(myId)%p(PuffSplit(ic,2))%aux,STAT=alloc_stat )
END IF

IF( PsrfDep%ipgrd(ic) > 0 )THEN
  DO ii = 0,3
    ic0 = PsrfDep%ipgrd(ic) + ii
    CALL AddEvapTemp( ic0 )
  END DO
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

SUBROUTINE ExpandPuffTemp( i )

USE step_p_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN )  :: i

INTEGER n, alloc_stat, j, k

TYPE( puff_str ), DIMENSION(:), ALLOCATABLE :: Temp

n = nDimTemp(i) + INT(nDimTemp(i)/2) + 1

ALLOCATE( Temp(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'ExpandPuffTemp'
  eMessage = 'Error allocating temporary puff storage'
  WRITE(eInform,*) 'npuf = ',n
  GOTO 9999
END IF

DO j = 1,nDimTemp(i)
  Temp(j) = PuffTemp(i)%p(j)
  IF( PuffTemp(i)%p(j)%naux > 0 )THEN
    ALLOCATE( Temp(j)%aux(PuffTemp(i)%p(j)%naux),STAT=alloc_stat )
    DO k = 1,PuffTemp(i)%p(j)%naux
      Temp(j)%aux(k) = PuffTemp(i)%p(j)%aux(k)
    END DO
    IF( ASSOCIATED(PuffTemp(i)%p(j)%aux) )DEALLOCATE( PuffTemp(i)%p(j)%aux,STAT=alloc_stat )
  ELSE
    NULLIFY( Temp(j)%aux )
  END IF
END DO

IF( ASSOCIATED(PuffTemp(i)%p) )DEALLOCATE( PuffTemp(i)%p,STAT=alloc_stat )

ALLOCATE( PuffTemp(i)%p(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = SZ_ERROR
  eRoutine = 'ExpandPuffTemp'
  eMessage = 'Error allocating temporary puff storage'
  WRITE(eInform,*) 'npuf = ',n
  GOTO 9999
END IF

DO j = 1,nDimTemp(i)
  PuffTemp(i)%p(j) = Temp(j)
  IF( Temp(j)%naux > 0 )THEN
    ALLOCATE( PuffTemp(i)%p(j)%aux(Temp(j)%naux),STAT=alloc_stat )
    DO k = 1,Temp(j)%naux
      PuffTemp(i)%p(j)%aux(k) = Temp(j)%aux(k)
    END DO
    IF( ASSOCIATED(Temp(j)%aux) )DEALLOCATE( Temp(j)%aux,STAT=alloc_stat )
  ELSE
    NULLIFY( PuffTemp(i)%p(j)%aux )
  END IF
END DO

DO j = nDimTemp(i)+1,n
  NULLIFY( PuffTemp(i)%p(j)%aux )
END DO

IF( ALLOCATED(Temp) )DEALLOCATE( Temp,STAT=alloc_stat )

nDimTemp(i) = n

9999 CONTINUE

IF( ALLOCATED(Temp) )DEALLOCATE( Temp,STAT=alloc_stat )

RETURN
END
