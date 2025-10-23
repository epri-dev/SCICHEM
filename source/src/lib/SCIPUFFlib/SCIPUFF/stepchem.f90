RECURSIVE SUBROUTINE StepChem( ID,p,dt )

USE scipuff_fi
USE chem_fi
USE files_fi
USE step_p_fi
USE met_fi
USE mpi_fi, ONLY: isSerial, myid, ierr
USE AqAer_fi, ONLY: ChemMet_str
USE diagnostics_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ID   ! -- Chemistry multi-component ID
TYPE( puff_str ), INTENT( INOUT ) :: p    ! -- puff structure
REAL,             INTENT( IN    ) :: dt   ! -- timestep (sec)

INTEGER i, j, alloc_stat, ios

LOGICAL leqm_set
REAL    ctem, vols
REAL    carea, pscale
REAL    spec_mass1, spec_mass2, mass_diff
REAL    nox_conc, noy_conc

REAL, PARAMETER :: NOXRATIO_MIN = 0.02
REAL, PARAMETER :: MINCONC = 1.E-30
REAL, PARAMETER :: MINORGCONC = 1.E-20

REAL, DIMENSION(:), ALLOCATABLE :: fac_dist
REAL, DIMENSION(:), ALLOCATABLE :: oldAmb

LOGICAL :: setNOy
LOGICAL :: minBENZ, minTOL, minXYL, minISOP, minTERP, minSESQ

REAL, EXTERNAL :: SetSpeciesVol


TYPE( ChemMet_str ) :: chemMet
REAL, EXTERNAL      :: SetPrate
INTERFACE
  SUBROUTINE SetChemAmbient( xIn,yIn,zIn,t,leq,lmet,lplot )
    REAL,    INTENT( IN ) :: xIn, yIn, zIn, t
    LOGICAL, INTENT( IN ) :: leq
    LOGICAL, INTENT( IN ) :: lmet
    LOGICAL, OPTIONAL, INTENT( IN ) :: lplot
  END SUBROUTINE
END INTERFACE

chem => chemMC(ID,myTid)

IF( p%idtl == I_STATIC .OR. isSerial )THEN  !Othewise set in GetChemMPI

!-----  get multi-components

CALL GetChemAux( ID,p )

END IF

nfast     = chem%nFast
nslow     = chem%nSlow
nequil    = chem%nEquilibrium
ngaseous  = chem%nGaseous
nparticle = chem%nParticle
nspecies  = nfast + nslow + nparticle
nspectot  = nspecies + nequil
nambient  = chem%nSpecies - nspectot

species => chem%species

IF( chem%nReactions == 0 )RETURN
IF( chem%nStar /= 0 )THEN
  IF( vol == 0. .OR. vself == 0. )RETURN
END IF

fast  => chem%fast
slow  => chem%slow
equil => chem%equil

nsolve_eq = chem%nSolveEq
nlin_eq   = chem%nLinearEq
ndir_eq   = chem%nDirectEq
nsubs_eq  = chem%nSubstEq

indx_eq  => chem%IndexEq
itype_eq => chem%TypeEq
irow_eq  => chem%RowEq

rtol = chem%rtol

ResetEq = .FALSE.

ALLOCATE( pdnrate(nspectot),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'StepRadicals'
  eMessage = 'Error allocating work arrays'
  GOTO 9999
END IF
IF( nslow > 0 .AND. alloc_stat == 0 )ALLOCATE( ydots(nslow),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'StepChem'
  eMessage = 'Error allocating work arrays'
  GOTO 9999
END IF

IF( p%idtl == I_STATIC .OR. isSerial )THEN
  IF( chem%Ambient%InterpAmb )THEN
    chemID = ID
    DO j = 1,2
      IF( MonthDay(j,2) == 0 )THEN
        ! Call StepRadicals here instead of in SetChemAmbient
        CALL StepRadicals( j )
        IF( nError /= NO_ERROR )THEN
          nError   = IV_ERROR
          eRoutine = 'StepRadicals'
          eMessage = 'Error from StepRadicals module.'
          GO TO 9999
        END IF
      END IF
    END DO
    chemID = 999
  END IF

!====   set ambient concentrations

CALL SetChemAmbient( SNGL(p%xbar),SNGL(p%ybar),p%zbar-hp,t,.TRUE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

END IF

ALLOCATE( oldAmb(nspectot),STAT=alloc_stat )

carea = p%sr/csav
pscale   = SQRT(PI2*p%szz)

!====   update rate constants

zlev = p%zbar - hp !Puff height relative to surface
CALL UpdateChemRate( .FALSE. )
IF( carea > 0. .AND. (ANY(chem%species(:)%ldep) .eqv. .TRUE.) )THEN
  CALL set_sp_vdep( p )
ELSE
!  DO i = 1,nspectot  !No need to re-set; vdep always multiplied by carea (or initialized to zero already)
!    IF( species(i)%class /= ID_SPECIES_PARTICLE )species(i)%vdep = 0.
!  END DO
END IF
IF( nError /= NO_ERROR )GOTO 9999

!====   Load working arrays

IF( chem%nStar == 0 )THEN
  volx = PI3*SQRT(p%det)
ELSE
  volx   = 1.0/vol
  vselfx = 1.0/vself
END IF

leqm_set = .FALSE.

!====   Save ambient concentrations

DO i = 1,nspectot
  oldAmb(i) = species(i)%amb
END DO

!====   Set ambient concentrations of aromatic SOA precursor radicals to zero
!====   to avoid spurious formation of aromatic SOAs. Also check if SOA
!====   precursor concentrations are zero
DO i = 1,nspectot
  SELECT CASE( TRIM(species(i)%name) )
    CASE( 'BENZRO2','TOLRO2','XYLRO2' )
      species(i)%amb = 0.
    CASE( 'BENZENE' )
      if ( species(i)%conc <= MINORGCONC ) then
        minBENZ = .TRUE.
      else
        minBENZ = .FALSE.
      end if
    CASE( 'TOL' )
      if ( species(i)%conc <= MINORGCONC ) then
        minTOL = .TRUE.
      else
        minTOL = .FALSE.
      end if
    CASE( 'XYL' )
      if ( species(i)%conc <= MINORGCONC ) then
        minXYL = .TRUE.
      else
        minXYL = .FALSE.
      end if
    CASE( 'ISOP' )
      if ( species(i)%conc <= MINORGCONC ) then
        minISOP = .TRUE.
      else
        minISOP = .FALSE.
      end if
    CASE( 'TERP' )
      if ( species(i)%conc <= MINORGCONC ) then
        minTERP = .TRUE.
      else
        minTERP = .FALSE.
      end if
    CASE( 'SESQ' )
      if ( species(i)%conc <= MINORGCONC ) then
        minSESQ = .TRUE.
      else
        minSESQ = .FALSE.
      end if
  END SELECT
END DO

!====   Save initial puff masses and concentrations

DO i = 1,nspectot
  SELECT CASE( species(i)%class )
    CASE( ID_SPECIES_PARTICLE )
      species(i)%mass = MAX(species(i)%mass,0.)
      species(i)%conc = MAX(species(i)%conc,0.)
  END SELECT
  SELECT CASE( TRIM(species(i)%name) )
    CASE( 'NH3','SO2','SULF' )
      species(i)%mass = MAX(species(i)%mass,0.)
      species(i)%conc = MAX(species(i)%conc,0.)
  END SELECT
  IF(INDEX(species(i)%name,'SV_') > 0 .or. &
     INDEX(species(i)%name,'RXN') > 0) THEN
      species(i)%mass = MAX(species(i)%mass,0.)
      species(i)%conc = MAX(species(i)%conc,0.)
  END IF
  IF(ABS(species(i)%conc) < MINCONC)species(i)%conc = MINCONC


  species(i)%msav = species(i)%mass
  species(i)%conc = MAX(species(i)%conc,-species(i)%amb)
  species(i)%csav = species(i)%conc
  species(i)%taudry = species(i)%vdep*carea
END DO

IF( lwash )THEN
  IF( prate == NOT_SET_R )THEN
    prate = SetPrate( p%zbar,zinv,hp,prbl )
  END IF
  DO i = 1,nspectot
    species(i)%tauwet = species(i)%scav*prate
  END DO
ELSE
  DO i = 1,nspectot
    species(i)%tauwet = 0.
  END DO
END IF

IF( chem%lBalance )then

  CALL GetSpeciesMass( spec_mass1 )
  ! Chemical criteria
  nox_conc = 0.
  noy_conc = 0.
  setNOy = .FALSE.
  DO i = 1,nspectot
    IF( species(i)%nit > 0. )THEN
      noy_conc = noy_conc + species(i)%nit*species(i)%conc
      IF( species(i)%name == 'NO' .OR. &
          species(i)%name == 'NO2' )nox_conc = nox_conc + species(i)%conc
    END IF
  END DO
  IF( noy_conc > 0. .AND. nox_conc > 0. )THEN
    IF( nox_conc/noy_conc <= NOXRATIO_MIN )setNOy = .TRUE.
    IF( nox_conc > noy_conc )setNOy = .TRUE.
  ELSE
    setNOy = .TRUE.
  ENDIF

  IF( setNOy )THEN
    DO i = 1,nspectot
      IF( species(i)%nit > 0. )THEN
        species(i)%csav_noy = species(i)%conc
        species(i)%conc = 0.
        species(i)%csav = species(i)%conc
      END IF
    END DO
  END IF

  ! --- Set ambient concs of NOz species to zero
  DO i = 1,nspectot
    IF( species(i)%nit > 0. )THEN
      IF( TRIM(species(i)%name) /= 'NO' .AND. TRIM(species(i)%name) /= 'NO2' )&
        species(i)%amb = 0.
    END IF
  END DO

END IF

!====   Advance gas-phase chemistry for total concentration

CALL StepGasPhase( dt,leqm_set )
IF( nError /= NO_ERROR )GOTO 9998

!====   Reset ambient concentrations of aromatic SOA precursor radicals
DO i = 1,nspectot
  SELECT CASE( TRIM(species(i)%name) )
    CASE( 'BENZRO2','TOLRO2','XYLRO2' )
      species(i)%amb = oldAmb(i)
  END SELECT
END DO

!====   Don't allow plume soa if precursor species concentrations in plume
!====   are zero
DO i = 1,nspectot
  SELECT CASE( TRIM(species(i)%name) )
    CASE( 'BENZENE','BENZRO2','BNZNRXN','BNZHRXN' )
      if ( minBENZ ) species(i)%conc = 0.
    CASE( 'TOL','TOLRO2','TOLNRXN','TOLHRXN' )
      if ( minTOL ) species(i)%conc = 0.
    CASE( 'XYL','XYLRO2','XYLNRXN','XYLHRXN' )
      if ( minXYL ) species(i)%conc = 0.
    CASE( 'ISOP','ISOPRXN' )
      if ( minISOP ) species(i)%conc = 0.
    CASE( 'TERP','TRPRXN' )
      if ( minTERP ) species(i)%conc = 0.
    CASE( 'SESQ','SESQRXN' )
      if ( minSESQ ) species(i)%conc = 0.
    CASE( 'MGLY' )
      if ( minISOP .AND. minTOL .AND. minXYL ) species(i)%conc = 0.
  END SELECT
END DO
!--Reset NOy species to saved values if NOy < 0 or NOx/NOy < 2%
IF( chem%lBalance )THEN
  IF( SetNOy )THEN
    DO i = 1,nspectot
      IF( species(i)%nit > 0 )THEN
        species(i)%csav = species(i)%csav_noy
        species(i)%conc = species(i)%csav_noy
      END IF
    END DO
  END IF
  ! --- Reset ambient concs of NOy species
  DO i = 1,nspectot
    IF( species(i)%nit > 0. )species(i)%amb = oldAmb(i)
  END DO
END IF

!====   Set equilibrium species for total concentration

IF( nequil > 0 )THEN
  CALL SetChemEquilibrium( .FALSE. )
  IF( nError /= NO_ERROR )THEN
    nError = NO_ERROR
!$OMP CRITICAL (LOG)
    WRITE(lun_log,'(A)',IOSTAT=ios) 'Error setting equilibrium in StepChem-1 - will continue'
!$OMP END CRITICAL (LOG)
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'StepChem'
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF
  END IF
END IF

IF( tb > 253. .AND. tb < 373. )THEN

  chemMet%tab      = tb                                ! temp(K)
  chemMet%pb       = pb/1013.25                        ! press(atm)
  chemMet%hb       = hb                                ! humidity(g H2O/g dry air)
  chemMet%cldall   = cw                                ! cloud liquid water content(g/m3)
  chemMet%cldbot   = cldbot                            ! cloud bottom (m-agl)
  chemMet%cldtop   = cldtop                            ! cloud top (m-agl)
  chemMet%cldavg   = cw_avg                            ! average cloud liquid water content
  IF( prate == NOT_SET_R )THEN
    chemMet%pratepuf = SetPrate( p%zbar,zinv,hp,prbl ) ! precip rate (mm/hr)
  ELSE
    chemMet%pratepuf = prate                           ! convert type to precip rate (mm/hr)
  END IF
  chemMet%fcc      = cldcvr                            ! cloud cover fraction
  chemMet%fprcpc   = 0.0
  chemMet%xml      = xml
  chemMet%zb1      = 10.
  chemMet%zruf     = zruf
  chemMet%us2      = us2
  chemMet%ws2      = ws2

  CALL SetChemAqaer( )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL StepAerAqEx( dt,nspecies,carea,pscale,chem_aqaer,chemMet,nError )
  IF( nError /= NO_ERROR )THEN
    nError   = IV_ERROR
    eRoutine = 'StepAerAq'
    eMessage = 'Error from AqAer module. See AqAer log for details'
    GO TO 9999
  END IF

  CALL GetChemAqaer( )

END IF

!==== Advance species masses

DO i = 1,nspecies
  IF( species(i)%lstar )THEN
    vols = SetSpeciesVol( species(i)%msav,species(i)%csav,species(i)%conc, &
                          volx,vselfx )
    species(i)%mass = species(i)%mass + (species(i)%conc - species(i)%csav)*vols
  ELSE
    species(i)%mass = species(i)%conc
  END IF
END DO

DO i = 1,nequil
  vols = SetSpeciesVol( equil(i)%s%msav,equil(i)%s%csav,equil(i)%s%conc, &
                        volx,vselfx )
  equil(i)%s%mass = equil(i)%s%mass + (equil(i)%s%conc - equil(i)%s%csav)*vols
END DO

!====   Force species balance
IF( chem%lBalance )THEN

  CALL GetSpeciesMass( spec_mass2 )
  mass_diff = spec_mass2 - spec_mass1

  ALLOCATE( fac_dist(nspecies),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'StepChem'
    eMessage = 'Error allocating fac_dist arrays'
    GOTO 9999
  END IF

  CALL GetSpeciesConc( fac_dist )

  DO i = 1,nspecies
    species(i)%mass = species(i)%mass - mass_diff*fac_dist(i)
  ENDDO

  DEALLOCATE( fac_dist,STAT=alloc_stat )

END IF

9900 CONTINUE

!====   chemistry diagnostic
IF( lDiagno )chemistry(i) = chemistry(i) + species(i)%mass - species(i)%msav
IF( p%idtl == I_STATIC .OR. isSerial )CALL PutChemAux( ID,p )  !Otherwise done in PutChemMPI

9999 CONTINUE
IF( ASSOCIATED(pdnrate) )THEN
  DEALLOCATE( pdnrate, STAT=alloc_stat )
  NULLIFY( pdnrate )
END IF
IF( ASSOCIATED(ydots) )THEN
  DEALLOCATE( ydots, STAT=alloc_stat )
  NULLIFY( ydots )
END IF
IF( ALLOCATED(oldAmb) )DEALLOCATE( oldAmb,  STAT=alloc_stat )

RETURN

9998 CONTINUE
!-- Reset concentrations if error in stepping chemistry
IF( p%idtl /= I_STATIC .AND. .NOT.isSerial )THEN
  DO i = 1,nspectot
    species(i)%conc = species(i)%csav
    species(i)%mass = species(i)%msav
  END DO
END IF

nError    = NO_ERROR
!$OMP ATOMIC
nbad_chem = nbad_chem + 1
!$OMP ATOMIC
tot_bad   = tot_bad   + 1
GOTO 9900

END

!------------------------------------------------------------------------------

REAL FUNCTION SetSpeciesVol( msav,csav,conc,volx,vselfx )

REAL, INTENT( IN ) :: msav     !Old mass
REAL, INTENT( IN ) :: csav     !Old overlap concentration
REAL, INTENT( IN ) :: conc     !New concentration after chemistry
REAL, INTENT( IN ) :: volx     !Overlap volume
REAL, INTENT( IN ) :: vselfx   !Self-overlap volume

IF( msav*csav > 0.0 )THEN

  IF( ABS(msav) > vselfx*ABS(csav) )THEN
    SetSpeciesVol = vselfx
  ELSE
    SetSpeciesVol = msav/csav
  END IF

ELSE IF( msav*csav < 0.0 )THEN

  IF( msav*(conc - csav) > 0.0 )THEN

    IF( ABS(msav) > vselfx*ABS(csav) )THEN
      SetSpeciesVol = -vselfx
    ELSE
      SetSpeciesVol = msav/csav
    END IF

  ELSE

    SetSpeciesVol = volx

  END IF

ELSE

  SetSpeciesVol = volx

END IF

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE StepGasPhase( dt,leqm_set )

USE chem_fi
USE error_fi
USE files_fi

IMPLICIT NONE

REAL,    INTENT( IN    ) :: dt
LOGICAL, INTENT( INOUT ) :: leqm_set

INTEGER ierr, i, ios

CALL step_ode( dt,leqm_set )

IF( nError /= NO_ERROR )THEN

  IF( TRIM(eRoutine) == 'SetChemEquilibrium' )THEN
    ierr = 1                                     !error with equilibrium species
  ELSE IF( TRIM(eRoutine) == 'SetDtSlow' )THEN
    ierr = 2                                     !error with slow species
  ELSE
    ierr = 4                                     !some other error
  END IF

  IF( ierr < 4 )THEN

    nError = NO_ERROR
!$OMP CRITICAL (LOG)
    WRITE(lun_log,'(A)',IOSTAT=ios) 'Second try to step gas-phase chemistry'
!$OMP END CRITICAL (LOG)
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'StepGasPhase'
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF

!-- reset concentrations

    DO i = 1,nspectot
      species(i)%conc = species(i)%csav
      species(i)%mass = species(i)%msav
    END DO

    IF( ierr == 1 )THEN

      CALL ResetEqFast()
      IF( nError /= NO_ERROR )GOTO 9999

    ELSE IF( ierr == 2 )THEN

!change slow to fast permanently

    END IF

!-- restep gas-phase chemistry

    leqm_set = .TRUE.

    CALL step_ode( dt,leqm_set )
    IF( nError /= NO_ERROR )THEN
      GOTO 9999
    ELSE
!$OMP ATOMIC
      ngd_chem = ngd_chem + 1
    END IF

  ELSE

    GOTO 9999

  END IF

ELSE

!$OMP ATOMIC
  ngd_chem = ngd_chem + 1

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE ResetEqFast()

USE chem_fi
USE error_fi

IMPLICIT NONE

INTEGER i, alloc_stat

!-- set equilibrium to fast species (temporarily)

NULLIFY( fast )
ALLOCATE( fast(nfast+nequil),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'ResetEqFast'
  eMessage = 'Error allocating work array'
  GOTO 9999
END IF

DO i = 1,nfast
  fast(i)%s => chem%fast(i)%s
END DO

DO i = 1,nequil
  fast(i+nfast)%s => chem%equil(i)%s
END DO

nfast    = nfast + nequil
nequil   = 0
nspecies = nfast + nslow

ResetEq = .TRUE.

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE step_ode( dt,leqm_set )

USE chem_fi
USE error_fi
USE files_fi
USE step_p_fi, ONLY: myTid

IMPLICIT NONE

REAL,    INTENT( IN    ) :: dt        ! -- time step (sec)
LOGICAL, INTENT( INOUT ) :: leqm_set  ! -- whether or not equilibriums have been set already

INTEGER i, itol, itask, istate, mf, iopt, lrw, liw
INTEGER ii, alloc_stat, ios
REAL    dtsm, dttotl, dtsmin
REAL    tx, dely
CHARACTER(128) cmsg

INTEGER, DIMENSION(1) :: neq_lsode
REAL   , DIMENSION(1) :: rtol_lsode

REAL,    DIMENSION(:), ALLOCATABLE :: atols
REAL,    DIMENSION(:), ALLOCATABLE :: yeq
REAL,    DIMENSION(:), ALLOCATABLE :: atol, rwork
INTEGER, DIMENSION(:), ALLOCATABLE :: iwork

EXTERNAL DerivFast
EXTERNAL jacob

IF( nfast > 0 .AND. chem%nFastReact > 0 )THEN

!====   Set up LSODE

  numode = nspectot
  IF( chem%thermal )numode = numode + 1

  ALLOCATE( atol(numode),atols(numode),yeq(numode),STAT=alloc_stat )
  lrw = 22 + 9*numode + numode**2
  liw = 30 + numode
  IF( alloc_stat == 0 )ALLOCATE( rwork(lrw),iwork(liw),STAT=alloc_stat )
  IF( alloc_stat == 0 )THEN
      IF( .NOT. ASSOCIATED(pdm) )ALLOCATE( pdm(numode,numode),STAT=alloc_stat )
  END IF
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'step_ode'
    eMessage = 'Error allocating work arrays'
    GOTO 9999
  END IF

  tx     = 0.0
  itol   = 2
  itask  = 1
  istate = 1
  iopt   = 1
  mf     = 21  !user-supplied jacobian, use 22 for internally generated jacobian
  DO ii = 5,10
    rwork(ii) = 0.0
    iwork(ii) = 0
  END DO
  iwork(6) = 1000
  rwork(6) = MIN(dt,300.)

ELSE IF( nslow > 0 .AND. chem%nSlowReact > 0 )THEN

  ALLOCATE( atol(nspectot),atols(nspectot),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'step_ode'
    eMessage = 'Error allocating work arrays'
    GOTO 9999
  END IF

END IF

!====   Update steady state species

IF( .NOT.leqm_set .AND. nequil > 0 )THEN
  CALL SetChemEquilibrium( .FALSE. )
  IF( nError /= NO_ERROR )THEN
    nError = NO_ERROR
!$OMP CRITICAL (LOG)
    WRITE(lun_log,'(A)',IOSTAT=ios) 'Error setting equilibrium in step_ode'
!$OMP END CRITICAL (LOG)
    IF( ios /= 0 )THEN
      nError   = WR_ERROR
      eRoutine = 'step_ode'
      eMessage = 'Error writing SCIPUFF log file'
      CALL ReportFileName( eInform,'File=',file_log )
      GOTO 9999
    END IF
    CALL ResetEqFast()
    IF( nError /= NO_ERROR )GOTO 9999
  ELSE
    leqm_set = .TRUE.
  END IF
END IF

!===== Save initial equilibrium concentrations

DO i = 1,nequil
  equil(i)%s%csav = equil(i)%s%conc
END DO

!====   Find atol for fast species

DO i = 1,nfast
  atol(i) = fast(i)%s%tol
  IF( .NOT.fast(i)%s%lstar .AND. volx /= 0. )atol(i) = atol(i)*volx
END DO

IF( chem%thermal )atol(nfast+1) = 0.01

!====   Find atol for slow species

DO i = 1,nslow
  atols(i) = slow(i)%s%tol
  IF( .NOT.slow(i)%s%lstar .AND. volx /= 0. )atols(i) = atols(i)*volx
END DO

!====== Advance fast species (plus slow species advanced by lsode_sl)

IF( nfast > 0 .AND. chem%nFastReact > 0 )THEN

  reaction => chem%fReact
  nrxn     =  chem%nFastReact

!---- load LSODE array

  DO i = 1,nfast
    yeq(i) = fast(i)%s%conc
  END DO

  IF( chem%thermal )THEN
    neq_lsode(1) = nfast + 1
    yeq(nfast+1) = 0.0
  ELSE
    neq_lsode(1) = nfast
  END IF

  rtol_lsode(1) = rtol

  CALL lsode_sl( DerivFast,neq_lsode,yeq,tx,tx+dt,itol,rtol_lsode,atol, &
                 itask,istate,iopt,rwork,lrw,iwork,liw,jacob,mf )
  IF( istate < 0 )THEN
    nError = IV_ERROR
    eRoutine = 'step_ode'
    eMessage = 'Negative value of istate returned from lsode'
    eAction  = 'See the log file for lsode messages'
    WRITE(eInform,'(A,I4)') 'Istate is',istate
    IF( .FALSE. )THEN
      CALL GetLSODEerror( istate,cmsg )
!$OMP CRITICAL (LOG)
      WRITE(lun_log,'(A)',IOSTAT=ios) TRIM(cmsg)
      WRITE(lun_log,'(A)',IOSTAT=ios) 'Values in last solution (yeq,atol):'
      DO i = 1,nfast
        WRITE(lun_log,'(A16,2X,1P2E12.4)',IOSTAT=ios) TRIM(fast(i)%s%name),yeq(i),atol(i)
      END DO
!$OMP END CRITICAL (LOG)
    END IF
    GOTO 9999
  END IF

!--- unload LSODE array

  DO i = 1,nfast
    IF( fast(i)%s%lstar )THEN
      fast(i)%s%conc = MAX(-fast(i)%s%amb,yeq(i))
    ELSE
      fast(i)%s%conc = yeq(i)
    END IF
  END DO
  IF( ResetEq )THEN
    DEALLOCATE( fast,STAT=alloc_stat )
    ResetEq = .FALSE.
  END IF

ELSE IF( nslow > 0 .AND. chem%nSlowReact > 0 )THEN

!====== Advance slow species

  reaction => chem%sReact
  nrxn     =  chem%nSlowReact

!====  Initialize time stepping

  dtsmin  = 0.01*dt
  dttotl  = 0.

!====== Beginning of time loop

  Timeloop: DO WHILE( dttotl < dt )
    dtsm = MIN(dt-dttotl,120.0)

!====   Update steady state species

    IF( nequil > 0 )THEN
      CALL SetChemEquilibrium( .FALSE. )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF

!--- Calculate derivative

    CALL DerivSlow()
    IF( nError /= NO_ERROR )GOTO 9999

!--- Set time step

    CALL SetDtSlow( atol,dt,dttotl,dtsmin,dtsm )
    IF( nError /= NO_ERROR )GOTO 9999

!--- Advance concentrations

    DO i = 1,nslow
      dely = dtsm*ydots(i)
      IF( slow(i)%s%lstar )THEN
        slow(i)%s%conc = MAX(-slow(i)%s%amb,slow(i)%s%conc + dely)
      ELSE
        slow(i)%s%conc = slow(i)%s%conc + dely
      END IF
    END DO

!====== Advance time

    dttotl = dttotl + dtsm

  END DO Timeloop

END IF

!===== End  of time loop

9999 CONTINUE

IF( ALLOCATED(atol)  )DEALLOCATE( atol, STAT=alloc_stat )
IF( ALLOCATED(atols) )DEALLOCATE( atols,STAT=alloc_stat )
IF( ALLOCATED(yeq)   )DEALLOCATE( yeq,  STAT=alloc_stat )
IF( ALLOCATED(rwork) )DEALLOCATE( rwork,STAT=alloc_stat )
IF( ALLOCATED(iwork) )DEALLOCATE( iwork,STAT=alloc_stat )
IF( ASSOCIATED(pdm)  )THEN
  DEALLOCATE( pdm,STAT=alloc_stat )
  NULLIFY(pdm)
END IF

IF( ResetEq )THEN
  DEALLOCATE( fast,STAT=alloc_stat )
  ResetEq = .FALSE.
END IF

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE slow_step( tn,tout,tlast,yeq )

!--- Advance the slow species after each step completed within LSODE
!--- Called from lsode_sl

USE chem_fi, ONLY: nfast, nslow, nequil, nrxn, chem, fast, slow, ydots, reaction
USE error_fi

IMPLICIT NONE

REAL,                  INTENT( IN    ) :: tn       ! -- time now
REAL,                  INTENT( IN    ) :: tout     ! -- end time for LSODE
REAL,                  INTENT( INOUT ) :: tlast    ! -- last time slow_step was called
REAL,    DIMENSION(*), INTENT( IN    ) :: yeq      ! -- species concentrations

INTEGER i
REAL    dt

IF( nequil > 0 )THEN

!====   Update fast species

  DO i = 1,nfast
    fast(i)%s%conc = yeq(i)
  END DO

!====   Update steady state species

  CALL SetChemEquilibrium( .FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!===== Check for slow species

IF( nslow < 1 )RETURN

dt = MIN(tn,tout) - tlast

!===== Don't step slow species if less than 30s

IF( dt < 30. .AND. tn < tout )RETURN

tlast = tn

reaction => chem%sReact
nrxn     =  chem%nSlowReact

!--- Calculate derivative

CALL DerivSlow()
IF( nError /= NO_ERROR )GOTO 9999

!====   Step concentrations

DO i = 1,nslow
  slow(i)%s%conc = MAX(-slow(i)%s%amb,slow(i)%s%conc + dt*ydots(i))
END DO

!---- Reset for fast reactions

reaction => chem%fReact
nrxn     =  chem%nFastReact

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE DerivSlow()

USE chem_fi

IMPLICIT NONE

INTEGER i, nn

!====   Compute derivatives

CALL SetProdRate( .FALSE. )

DO i = 1,nslow
  nn = slow(i)%s%ID
  ydots(i) = pdnrate(nn)
END DO

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE SetProdRate( ltherm )

USE chem_fi, ONLY: pdnrate, dTdt, reaction, nspectot, nrxn
USE multcomp_fd

IMPLICIT NONE

LOGICAL, INTENT( IN )  :: ltherm

INTEGER i, ir

!====   Clear working derivative arrays

DO i = 1,nspectot
  pdnrate(i) = 0.
END DO

dTdt = 0.0

!====   Compute production and loss terms

DO ir = 1,nrxn
  IF( BTEST(reaction(ir)%r%class,ID_REACT_LINEAR) )THEN
    CALL ReactLinear( ir,ltherm )
  ELSE
    CALL ReactQuad( ir,ltherm )
  END IF
END DO

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE ReactLinear( ir,ltherm )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ir            ! -- reaction ID
LOGICAL, INTENT( IN ) :: ltherm

REAL    kr
INTEGER i, iP, iA

TYPE( ChemReact_str ), POINTER :: r

r => reaction(ir)%r

!====   Set rate

IF( r%k == 0.0 )RETURN

!====   Adjust derivative

iA = r%A
kr = r%k*species(iA)%conc

pdnrate(iA) = pdnrate(iA) - kr

DO i = 1,r%nP
  iP = r%P(i)
  pdnrate(iP) = pdnrate(iP) + kr*r%fP(i)
END DO

IF( ltherm .AND. BTEST(r%class,ID_REACT_THERMAL) )dTdt = dTdt + kr*r%H

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE ReactQuad( ir,ltherm )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ir            ! -- reaction ID
LOGICAL, INTENT( IN ) :: ltherm

INTEGER iA, iB, iP, i
REAL    cr, kr

TYPE( ChemReact_str ), POINTER :: r

!====   Set local pointer

r => reaction(ir)%r

!====   Set rate

kr = r%k
IF( kr == 0.0 )RETURN

iA = r%A
iB = r%B

!====   Adjust production and loss terms

! dAp/dt = -k(ApBp + ApBa + AaBp) = -kAp(Bp+Ba) - kAaBp
!        = -loss*Ap    + prod

cr = kr*(species(iA)%conc*species(iB)%conc + species(iA)%amb*species(iB)%conc &
                                           + species(iB)%amb*species(iA)%conc)

pdnrate(iA) = pdnrate(iA) - cr
pdnrate(iB) = pdnrate(iB) - cr*r%fB

DO i = 1,r%nP
  iP          = r%P(i)
  pdnrate(iP) = pdnrate(iP) + cr*r%fP(i)
END DO

IF( ltherm .AND. BTEST(r%class,ID_REACT_THERMAL) )dTdt = dTdt + cr*r%H

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE SetDtSlow( atol,dt,dttotl,dtsmin,dtsm )

USE chem_fi
USE files_fi
USe error_fi

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( IN    ) :: atol      ! -- tolerances for the slow species
REAL,               INTENT( IN    ) :: dt        ! -- large time step (sec)
REAL,               INTENT( IN    ) :: dttotl    ! -- total time stepped so far (sec), goes to dt
REAL,               INTENT( IN    ) :: dtsmin    ! -- minimum small time step allowed (sec)
REAL,               INTENT( INOUT ) :: dtsm      ! -- small time step for slow species (sec)

REAL    eps, tau, cval
INTEGER i, j, ios
LOGICAL reset

TYPE( ChemReact_str ), POINTER :: r

reset = .FALSE.

DO i = 1,nslow

  IF( ydots(i) /= 0.0 )THEN

    IF( slow(i)%s%lstar )THEN
      cval = MAX(ABS(slow(i)%s%conc),slow(i)%s%conc+slow(i)%s%amb)
    ELSE
      cval = MAX(ABS(slow(i)%s%conc),slow(i)%s%conc+slow(i)%s%amb*volx)
    END IF
    eps = rtol*cval + atol(i)
    tau = 10.*eps/(ABS(ydots(i)))

    IF( tau <= dtsmin )THEN

!$OMP CRITICAL (LOG)
      IF( .NOT.reset )WRITE(lun_log, *)'The following species are now modeled as fast:'

      WRITE(lun_log,'(A8,/, 6(A8, 1PE12.4,/) )',IOSTAT=ios) slow(i)%s%name, &
            'tchem = ', tau, 'dt = ', dtsm,'rate = ',ydots(i), &
            'mass = ', slow(i)%s%mass,'conc = ', slow(i)%s%conc, &
            'atol = ',atol(i)

      IF( slow(i)%s%lstar )THEN
        WRITE(lun_log,'(A)',IOSTAT=ios) 'Determined by concentration rate of change'
      ELSE
        WRITE(lun_log,'(A)',IOSTAT=ios) 'Determined by mass rate of change'
      END IF
!$OMP END CRITICAL (LOG)
      IF( ios /= 0 )THEN
        nError   = WR_ERROR
        eRoutine = 'SetDtSlow'
        eMessage = 'Error writing SCIPUFF log file'
        CALL ReportFileName( eInform,'File=',file_log )
        GOTO 9999
      END IF

      slow(i)%s%class = ID_SPECIES_FAST !reset to fast
      reset = .TRUE.

    END IF

    dtsm = MIN(dtsm,tau)

  END IF

END DO

dtsm = MAX(dtsmin,dtsm)

IF( reset )THEN

!--- Reset Reaction classes

  DO i = 1,chem%nReactions

    r => chem%reaction(i)
    IF( BTEST(r%class,ID_REACT_LINEAR) )THEN
      r%class = 0
      r%class = IBSET(r%class,ID_REACT_LINEAR)
    ELSE
      r%class = 0
    END IF
    CALL SetReactionClass( chem%species(r%A)%class,r%class )
    IF( r%B /= 0 ) CALL SetReactionClass( chem%species(r%B)%class,r%class )
    DO j = 1,r%nP
      CALL SetReactionClass( chem%species(r%P(j))%class,r%class )
    END DO

  END DO

  CALL SetChemPointers()
  IF( nError /= NO_ERROR )GOTO 9999

  nfast  = chem%nFast
  nslow  = chem%nSlow
  nequil = chem%nEquilibrium

  fast  => chem%fast
  slow  => chem%slow
  equil => chem%equil

  nError   = IV_ERROR
  eRoutine = 'SetDtSlow'
  eMessage = 'Fast species modeled as slow in the plume'
  eInform  = 'Species changed permanently to fast (see log file)'
  eAction  = CHAR(0)

END IF

IF( dttotl + dtsm > dt )dtsm = dt - dttotl

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE DerivFast( neq,tdum,yeq,ydot )

USE chem_fi, ONLY: nfast, chem, fast, pdnrate, dTdt
USE error_fi

IMPLICIT NONE

INTEGER, DIMENSION(*), INTENT( IN  ) :: neq      ! -- number of equations/species
REAL,                  INTENT( IN  ) :: tdum     ! -- not used
REAL,    DIMENSION(*), INTENT( IN  ) :: yeq      ! -- species concentrations
REAL,    DIMENSION(*), INTENT( OUT ) :: ydot     ! -- species concentration derivatives

INTEGER i, j

IF( nError /= NO_ERROR )GOTO 9998

!====   Update working arrays from LSODE species array

DO i = 1,nfast
  fast(i)%s%conc = yeq(i)
END DO

!====   Compute derivatives

CALL SetProdRate( chem%thermal )

!====   Load LSODE derivative array

DO i = 1,nfast
  j = fast(i)%s%ID
  ydot(i) = pdnrate(j)
END DO

IF( chem%thermal )ydot(nfast+1) = dTdt

9999 CONTINUE

RETURN

9998 CONTINUE

DO i = 1,neq(1)
  ydot(i) = 0.
END DO

GOTO 9999

END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE jacob( neq,tdum,yeq,ml,mu,pd,ndim1 )

USE chem_fi, ONLY: nfast, numode, nrxn, chem, fast, pdm, reaction, ID_REACT_FAST, ID_REACT_LINEAR

IMPLICIT NONE

INTEGER, DIMENSION(*),    INTENT( IN  ) :: neq      ! -- number of ODEs solved
INTEGER,                  INTENT( IN  ) :: ml, mu   ! -- half-band width parameters
INTEGER,                  INTENT( IN  ) :: ndim1    ! -- dimension for jacobian
REAL,                     INTENT( IN  ) :: tdum     ! -- time
REAL, DIMENSION(*),       INTENT( IN  ) :: yeq      ! -- concentrations
REAL, DIMENSION(ndim1,*), INTENT( OUT ) :: pd       ! -- jacobian

INTEGER i, j, ir
INTEGER ii, jj

!====   Clear working jacobian arrays

DO i = 1,numode
  DO j = 1,numode
    pdm(i,j) = 0.
  END DO
END DO

DO i = 1,nfast
  fast(i)%s%conc = yeq(i)
END DO

!====   Compute jacobian

DO ir = 1,nrxn

  IF( BTEST(reaction(ir)%r%class,ID_REACT_FAST) )THEN
    IF( BTEST(reaction(ir)%r%class,ID_REACT_LINEAR) )THEN
      CALL JacobLin( ir )
    ELSE
      CALL JacobQuad( ir )
    END IF
  END IF
END DO

!====   Load LSODE jacobian array

DO i = 1,nfast
  ii = fast(i)%s%ID
  DO j = 1,nfast
    jj = fast(j)%s%ID
    pd(i,j) = pdm(ii,jj)
  END DO
END DO

IF( chem%thermal )THEN
  DO i = 1,nfast
    ii = fast(i)%s%ID
    pd(i,nfast+1) = 0.0
    pd(nfast+1,i) = pdm(ii,numode)
  END DO
  pd(nfast+1,nfast+1) = 0.0
END IF

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE JacobLin( ir )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ir  ! -- reaction ID

REAL    dc
INTEGER i, iA, iP

TYPE( ChemReact_str ), POINTER :: r

!====   Set local pointer

r => reaction(ir)%r

!====   Set deriv with respect to A

dc = r%k
IF( dc == 0.0 )RETURN

!====   Adjust derivative

iA = r%A
pdm(iA,iA) = pdm(iA,iA) - dc

DO i = 1,r%nP
  iP = r%P(i)
  pdm(iP,iA) = pdm(iP,iA) + dc*r%fP(i)
END DO

IF( BTEST(r%class,ID_REACT_THERMAL) )THEN
  pdm(numode,iA) = pdm(numode,iA) + dc*r%H
END IF

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE JacobQuad( ir )

USE chem_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ir    ! -- reaction ID

INTEGER iA, iB, iP, i
REAL    dcadca, dcadcb

TYPE( ChemReact_str ), POINTER :: r

!====   Set local pointer

r => reaction(ir)%r
IF( r%k == 0.0 )RETURN

iA = r%A
iB = r%B

!====   Set derivatives with respect to A and iB

dcadca = r%k*(species(iB)%conc + species(iB)%amb)
dcadcb = r%k*(species(iA)%conc + species(iA)%amb)

!====   Adjust jacobian

pdm(iA,iA) = pdm(iA,iA) - dcadca
pdm(iA,iB) = pdm(iA,iB) - dcadcb
pdm(iB,iA) = pdm(iB,iA) - dcadca*r%fB
pdm(iB,iB) = pdm(iB,iB) - dcadcb*r%fB

DO i = 1,r%nP
  iP = r%P(i)
  pdm(iP,iA) = pdm(iP,iA) + dcadca*r%fP(i)
  pdm(iP,iB) = pdm(iP,iB) + dcadcb*r%fP(i)
END DO

IF( BTEST(r%class,ID_REACT_THERMAL) )THEN
  pdm(numode,iA) = pdm(numode,iA) + dcadca*r%H
  pdm(numode,iB) = pdm(numode,iB) + dcadcb*r%H
END IF

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE SetChemEquilibrium( lAmbient )

USE chem_fi
USE error_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lAmbient  ! -- whether or not it is setting the ambient (versus plume)

REAL    aa, bb
INTEGER i, j, k, iA, iB, iP, jA, jB, alloc_stat
INTEGER iiA, iiB, ii, id, is, jd, js, i1, i2
REAL    kr, arg, fP
LOGICAL nonlinear

REAL,    DIMENSION(:,:,:), ALLOCATABLE :: Cq
REAL,    DIMENSION(:,:),   ALLOCATABLE :: Cl, fjac
REAL,    DIMENSION(:),     ALLOCATABLE :: Cc, ctem, fvec, rwrk, vwrk, x, tolx
INTEGER, DIMENSION(:),     ALLOCATABLE :: iwrk

TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: eReaction

EXTERNAL f_conc

!====   Check for equilibrium species

IF( nequil <= 0 )RETURN

!====   Initialize locals

i = nequil
ALLOCATE( Cq(i,i,i),CL(i,i),Cc(i),STAT=alloc_stat)
IF( alloc_stat == 0 )ALLOCATE( fjac(i,i),fvec(i),iwrk(i),rwrk(i),vwrk(i),x(i), &
                               tolx(i),ctem(nspectot+nambient),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'SetChemEquilibrium'
  eMessage = 'Error allocating work arrays'
  GOTO 9999
END IF

DO i = 1,nequil
  fjac(i,:) = 0.0
  fvec(i)   = 0.0
  rwrk(i)   = 0.0
  vwrk(i)   = 0.0
END DO

eReaction => chem%eReact

!====   Set Ambient/Perturbation concentration switch
! (uses total concentration)

DO i = 1,nspectot+nambient

  SELECT CASE( species(i)%class )
    CASE( ID_SPECIES_EQUILIBRIUM )
      ctem(i) = species(i)%tol    ! Set first guess to tolerance

    CASE( ID_SPECIES_AMBIENT )
      ctem(i) = species(i)%amb

    CASE DEFAULT
      IF( lAmbient )THEN
        ctem(i) = MAX(species(i)%amb,species(i)%tol)
      ELSE
        ctem(i) = MAX(species(i)%conc+species(i)%amb,species(i)%tol)
      END IF

  END SELECT

END DO

!====   Clear solver arrays

DO i = 1,nequil
  Cc(i) = 0.0
  DO j = 1,nequil
    Cl(i,j) = 0.0
    DO k = 1,nequil
      Cq(i,j,k) = 0.0
    END DO
  END DO
  ii = irow_eq(i)
  Cl(ii,ii) = 0.
END DO

!====   Loop over reactions and set solver coefficients

nonlinear = .FALSE.

DO i = 1,chem%nEquilReact

  kr = eReaction(i)%r%k

  IF( kr == 0.0 )CYCLE

  jA = eReaction(i)%r%A
  iA = species(jA)%eqID

!======== Linear Reactions

  IF( BTEST(eReaction(i)%r%class,ID_REACT_LINEAR) )THEN

!==========     Equilibrium reactant

    IF( iA > 0 )THEN

      iiA = irow_eq(iA)
      arg = kr
      Cl(iiA,iiA) = Cl(iiA,iiA) - arg
      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cl(iP,iiA) = Cl(iP,iiA) + fP*arg
        END IF
      END DO

!==========     Non Equilibrium reactant

    ELSE

      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cc(iP) = Cc(iP) - fP*kr*ctem(jA)
        END IF
      END DO

    END IF

!======== Quadratic reaction

  ELSE

    jB  = eReaction(i)%r%B
    iB  = species(jB)%eqID

!==========     Equilibrium A : Non equilibrium B

    IF( iA > 0 .AND. iB == 0 )THEN

      iiA = irow_eq(iA)
      arg = kr*ctem(jB)
      Cl(iiA,iiA) = Cl(iiA,iiA) - arg
      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cl(iP,iiA) = Cl(iP,iiA) + fP*arg
        END IF
      END DO

!==========     Equilibrium B : Non equilibrium A

    ELSE IF( iB > 0 .AND. iA == 0 )THEN

      iiB = irow_eq(iB)
      arg = kr*ctem(jA)
      Cl(iiB,iiB) = Cl(iiB,iiB) - eReaction(i)%r%fB*arg
      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cl(iP,iiB) = Cl(iP,iiB) + fP*arg
        END IF
      END DO

!==========     Equilibrium A : Equilibrium B -> Nonlinear terms

    ELSE IF( iB > 0 .AND. iA > 0 )THEN

      iiA = irow_eq(iA)
      iiB = irow_eq(iB)
      IF( iiA < iiB )THEN
        i1 = iiB
        i2 = iiA
      ELSE
        i1 = iiA
        i2 = iiB
      END IF
      Cq(iiA,i1,i2) = Cq(iiA,i1,i2) - kr
      Cq(iiB,i1,i2) = Cq(iiB,i1,i2) - eReaction(i)%r%fB*kr
      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cq(iP,i1,i2) = Cq(iP,i1,i2) + fP*kr
        END IF
      END DO
      nonlinear = .TRUE.

!==========     Non equilibrium A : Non equilibrium B

    ELSE

      DO j = 1,eReaction(i)%r%nP
        iP = eReaction(i)%r%P(j)
        iP = species(iP)%eqID
        IF( iP > 0 )THEN
          iP = irow_eq(iP)
          fP = eReaction(i)%r%fP(j)
          Cc(iP) = Cc(iP) - fP*kr*ctem(jA)*ctem(jB)
        END IF
      END DO

    END IF
  END IF

END DO

!====   Preprocess matrices

IF( ndir_eq > 0 )THEN
  DO i = 1,ndir_eq
    id = nequil - i + 1
    IF( Cl(id,id) == 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetChemEquilibrium'
      eMessage = 'Zero destruction rate'
      eInform  = 'Unable to solve for '//TRIM(equil(id)%s%name)
      IF( lAmbient )THEN
        eAction = 'Solving for ambient radicals'
      ELSE
        eAction = 'Solving for plume radicals'
      END IF
      GO TO 9999
    END IF
    x(id) = Cc(id)/Cl(id,id)
    DO j = 1,id-1
      Cc(j) = Cc(j) - x(id)*Cl(j,id)
    END DO
  END DO
END IF

IF( nlin_eq > 0 )THEN

  DO i = 1,nlin_eq
    id = nsolve_eq + nlin_eq - i + 1
    js = itype_eq(indx_eq(id))
    jd = irow_eq(js)
    IF( Cl(id,id) == 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetChemEquilibrium'
      eMessage = 'Zero destruction rate'
      eInform  = 'Unable to solve for '//TRIM(equil(id)%s%name)
      IF( lAmbient )THEN
        eAction = 'Solving for ambient radicals'
      ELSE
        eAction = 'Solving for plume radicals'
      END IF
      GO TO 9999
    END IF

    Cl(id,jd) = Cl(id,jd)/Cl(id,id)
    Cc(id)    = Cc(id)/Cl(id,id)

    aa = -Cl(id,jd)
    bb = Cc(id)
    DO j = 1,id - 1
      IF( Cl(j,id) /= 0.0 )THEN
        Cl(j,jd) = Cl(j,jd) + aa*Cl(j,id)
        Cc(j)    = Cc(j)    - bb*Cl(j,id)
      END IF
    END DO
  END DO

END IF

!====   Solve - nonlinear

IF( nonlinear )THEN

!====   Use previous values as initial guesses

  DO is = 1,nsolve_eq

    i = indx_eq(is)
    x(is) = ctem(equil(i)%s%ID)

!====   Find tolx for equilibrium species

    tolx(is) = equil(i)%s%tol

  END DO

!====   Call Newton-Raphson solver

  CALL mnewt( Cq,Cl,Cc,x,nsolve_eq,nequil,iwrk,f_conc, &
              tolx,fjac,fvec,rwrk,vwrk )

  IF( nError /= NO_ERROR )THEN
    CALL CheckChemEquilib( nequil,Cq,Cl,x,lAmbient )
    GO TO 9999
  END IF

  DO i = 1,nsolve_eq
    Cc(i) = x(i)
  END DO

!====   Solve - linear (LU Decomposition and Backsubstitution)

ELSE

  CALL ludcmp( Cl,nsolve_eq,nsolve_eq,iwrk,arg,vwrk )
  IF( nError /= NO_ERROR )THEN
    CALL CheckChemEquilib( nequil,Cq,Cl,x,lAmbient )
    GO TO 9999
  END IF
  CALL lubksb( Cl,nsolve_eq,nsolve_eq,iwrk,Cc )

END IF

!====   Complete solution

DO i = 1,nsolve_eq
  x(i) = Cc(i)
END DO

IF( nlin_eq > 0 )THEN
  DO i = 1,nlin_eq
    id = i + nsolve_eq
    js = itype_eq(indx_eq(id))
    jd = irow_eq(js)
    x(id) = -Cl(id,jd)*x(jd) + Cc(id)
  END DO
END IF

IF( nsubs_eq > 0 )THEN
  DO i = 1,nsubs_eq
    id = i + nsolve_eq + nlin_eq
    x(id) = -Cc(id)
    DO j = 1,nsolve_eq + nlin_eq
      x(id) = x(id) + Cl(id,j)*x(j)
    END DO
    IF( Cl(id,id) == 0.0 )THEN
      nError   = IV_ERROR
      eRoutine = 'SetChemEquilibrium'
      eMessage = 'Zero destruction rate'
      eInform  = 'Unable to solve for '//TRIM(equil(indx_eq(id))%s%name)
      IF( lAmbient )THEN
        eAction = 'Solving for ambient radicals'
      ELSE
        eAction = 'Solving for plume radicals'
      END IF
      GO TO 9999
    END IF
    x(id) = -x(id)/Cl(id,id)
  END DO
END IF

!====   Put solution into working arrays

!====   Ambient equilibrium concentration only

IF( lAmbient )THEN

  DO i = 1,nequil
    ii = indx_eq(i)
    id = irow_eq(ii)
    equil(ii)%s%amb = MAX(0.,x(id))
  END DO

!====   Equilibrium species concentration and mass

ELSE

  DO i = 1,nequil
    ii = indx_eq(i)
    id = irow_eq(ii)
    equil(ii)%s%conc = MAX(0.,x(id)) - equil(ii)%s%amb
    equil(ii)%s%mass = equil(ii)%s%conc*volx
  END DO

END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE CheckChemEquilib( n,Cq,Cl,x,lAmbient )

USE chem_fi
USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,                INTENT( IN ) :: n          ! -- array size
REAL, DIMENSION(n,n,n), INTENT( IN ) :: Cq         ! -- quadratic terms
REAL, DIMENSION(n,n),   INTENT( IN ) :: Cl         ! -- linear terms
REAL, DIMENSION(n),     INTENT( IN ) :: x          ! -- values in solution
LOGICAL,                INTENT( IN ) :: lAmbient   ! -- whether it is solving for the ambient

INTEGER i, j, ii, jj, idi, idj, ios, alloc_stat

CHARACTER(16) equilib

REAL, DIMENSION(:), ALLOCATABLE :: tchem

!====   Set Ambient/Perturbation concentration switch

IF( lAmbient )THEN
  equilib = 'the ambient'
ELSE
  equilib = 'the plume'
END IF

ALLOCATE( tchem(nequil),STAT=alloc_stat)
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'CheckEquilibrium'
  eMessage = 'Error allocating work arrays'
  GOTO 9999
END IF

DO i = 1,nequil
  tchem(i) = 0.
END DO

DO ii = 1,nsolve_eq
  idi = indx_eq(ii)
  i = irow_eq(idi)
  DO jj = 1,nsolve_eq
    idj = indx_eq(jj)
    j = irow_eq(idj)
    IF( i == j )THEN
      tchem(idi) = tchem(idi) + Cq(i,i,j)*x(j)
    ELSE
      tchem(idi) = tchem(idi) + (Cq(i,i,j) + Cq(i,j,i))*x(j)
    END IF
  END DO
  tchem(idi) = tchem(idi) + Cl(i, i)
END DO

!$OMP CRITICAL (LOG)
WRITE(lun_log,'(A)',IOSTAT=ios) &
     'The following are reaction timescales for equilibrium species:'
DO ii = 1,nsolve_eq
  i = indx_eq(ii)
  IF( tchem(i) /= 0. )THEN
    WRITE(lun_log,'(A,1PG11.4)',IOSTAT=ios) equil(i)%s%name,ABS(1./tchem(i))
  ELSE
    WRITE(lun_log,'(A)',IOSTAT=ios) equil(i)%s%name//'  Infinity'
  END IF
END DO
IF( chem%nZenith > 0 )WRITE(lun_log,'(A,1PG11.4)',IOSTAT=ios) 'Zenith angle = ',zen_ang
WRITE(lun_log,'(A,1PG11.4)',IOSTAT=ios) 'Temperature  = ',temp
WRITE(lun_log,'(A,1PG11.4)',IOSTAT=ios) 'Pressure     = ',pres
nError   = IV_ERROR
eMessage = 'Nonconverging equilibrium species for ' // equilib
eAction  = 'See log file for more information'
!$OMP END CRITICAL (LOG)

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE mnewt( q,l,c,x,n,np,indx,usrfunc,tolx,fjac,fvec,p,vwrk )

USE error_fi

IMPLICIT NONE

INTEGER,                      INTENT( IN    ) :: n      ! - number of equations
INTEGER,                      INTENT( IN    ) :: np     ! - dimension
REAL,    DIMENSION(np,np,np), INTENT( INOUT ) :: q      ! - quadratic terms of eqs
REAL,    DIMENSION(np,np),    INTENT( INOUT ) :: l      ! - linear terms of eqs
REAL,    DIMENSION(np),       INTENT( INOUT ) :: c      ! - constant terms of eqs
INTEGER, DIMENSION(np),       INTENT( OUT   ) :: indx   ! - work array used in lubcksb and ludcmp
EXTERNAL                                      :: usrfunc ! - calculated right hand side and jacobian
REAL,    DIMENSION(n),        INTENT( INOUT ) :: tolx   ! - tolerances
REAL,    DIMENSION(n),        INTENT( INOUT ) :: x      ! - initial guess and then final solution
REAL,    DIMENSION(np,np),    INTENT( INOUT ) :: fjac   ! - jacobian
REAL,    DIMENSION(np),       INTENT( INOUT ) :: fvec   ! - right hand side of eqns
REAL,    DIMENSION(np),       INTENT( INOUT ) :: p      ! - work array
REAL,    DIMENSION(np),       INTENT( INOUT ) :: vwrk   ! - work array

REAL,    PARAMETER :: RTOL = .0001
INTEGER, PARAMETER :: NTRIAL = 20

INTEGER i, k
REAL    d
LOGICAL converged

DO k = 1,NTRIAL

  CALL usrfunc( q,l,c,x,n,np,fvec,fjac )

  DO i = 1,n
    p(i) = -fvec(i)
  END DO

  CALL ludcmp( fjac,n,np,indx,d,vwrk )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL lubksb( fjac,n,np,indx,p )

  converged = .TRUE.
  DO i = 1,n
    x(i) = x(i) + p(i)
    IF( ABS(p(i)) > RTOL*ABS(x(i))+tolx(i) )converged = .FALSE.
  END DO

  IF( converged )GOTO 9999

END DO

!-------Did not converge in ntrial iterations

nError   = IV_ERROR
eRoutine = 'mnewt'

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE f_conc( q,l,c,x,n,np,fvec,fjac )

IMPLICIT NONE

INTEGER,                      INTENT( IN  ) :: n      ! - number of equations
INTEGER,                      INTENT( IN  ) :: np     ! - dimension
REAL,    DIMENSION(np,np,np), INTENT( IN  ) :: q      ! - quadratic terms of eqs
REAL,    DIMENSION(np,np),    INTENT( IN  ) :: l      ! - linear terms of eqs
REAL,    DIMENSION(np),       INTENT( IN  ) :: c      ! - constant terms of eqs
REAL,    DIMENSION(np),       INTENT( IN  ) :: x      ! - concentrations
REAL,    DIMENSION(np),       INTENT( OUT ) :: fvec   ! - right hand side of eqns
REAL,    DIMENSION(np,np),    INTENT( OUT ) :: fjac   ! - jacobian

INTEGER  i, j, k

!-------initial fvec and fjac to zero

DO i = 1,n
  fvec(i) = 0.
  DO j = 1, n
    fjac(i,j) = 0.
  END DO
END DO

!-------calculate fvec and fjac

DO i = 1,n
  DO j = 1,n
    DO k = j,n
      IF( q(i,j,k) /= 0. )THEN
        fvec(i) = fvec(i) + q(i,j,k)*x(j)*x(k)
        IF( j == k )THEN
          fjac(i,j) = fjac(i,j) + 2.*q(i,j,k)*x(k)
        ELSE
          fjac(i,j) = fjac(i,j) + q(i,j,k)*x(k)
        END IF
      END IF
    END DO
    fvec(i)   = fvec(i)   + l(i,j)*x(j)
    fjac(i,j) = fjac(i,j) + l(i,j)
  END DO
  fvec(i) = fvec(i) - c(i)
END DO

RETURN
END

!------------------------------------------------------------------------------
RECURSIVE SUBROUTINE UpdateChemRate( lamb )

!====   Set reaction rates

USE scipuff_fi
USE met_fi
USE chem_fi
USE step_p_fi, ONLY:tdyn

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: lamb

! Variables for cloud attenuation of photolysis rate
REAL  lwp                ! liquid water path--lwc*dz (g/m2)
REAL  clod               ! cloud optical depth
REAL  trans              ! transmitivity
REAL  coszen             ! cosine of zenith angle
REAL  zrel               ! in cloud height
REAL  x1                 ! cloud attenuation interpolation term
REAL  x2                 ! cloud attenuation interpolation term
REAL  fclda              ! above cloud top factor
REAL  fcldb              ! below cloud base factor

REAL, EXTERNAL :: asind

!====   Get zenith angle

IF( chem%nZenith > 0 )THEN
  zen_ang = ABS(90.-MAX(0.0,asind(sun_fac)))
ELSE
  zen_ang = 0.0
END IF

!====   Set temperature from absolute temperature

temp = tb
pres = pb/1013.25

!====   Update non-radiation dependent rates

CALL SetRateCoeff()

!====   Update radiation dependent rates if zenith angle changes

! Cloud attenuation factor

! Check if clouds are present in the column where puff is located
IF( cw_avg > 0. )THEN
  cldcvr = MAX(cldcvr, 0.05) ! Set 5% as min. cloud cover for non-zero cw
  lwp = (cldtop - cldbot) * cw_avg

!...Calculate the cloud optical depth using a formula derived from
!...  Stephens (1978), JAS(35), pp2111-2132.
!...  only calculate the cloud optical depth when the liquid water
!...  path is >= 10 g/m2
  IF( lwp >= 10.0 )THEN
    clod = 10.0**( 0.2633 + 1.7095 * LOG( LOG10( lwp ) ) )
  ELSE
    clod = 0.0
  END IF

!...If no cloud or optical depth < 5, set clear sky values.
!...  (i.e. don't do anything)

  IF( clod >= 5.0 )THEN
    trans = ( 5.0 - EXP ( -clod ) ) / ( 4.0 + 0.42 * clod )

!...calculate cloud correction factors
    zrel = ( zlev - cldbot ) / ( cldtop - cldbot )

!...  below cloud base
    coszen = COS(zen_ang*PI180)
    fcldb = 1.0 + cldcvr * ( 1.6 * coszen * trans - 1.0 )

!...  above cloud top
    x1 = cldcvr * coszen * ( 1.0 - trans )
    fclda = 1.0 + x1

    IF( zlev < cldbot )THEN
      cld_att = fcldb
    ELSE IF( zlev >  cldtop )THEN
      cld_att = fclda
    ELSE
!...  in cloud - linearly interpolate between base and top value
      x2 = fcldb * ( 1.0 - zrel )
      cld_att = fclda * zrel + x2
    END IF
  ELSE
    cld_att = 1.0 ! No effect of clouds
  END IF
ELSE
  cld_att = 1.0 ! No effect of clouds
END IF

CALL SetPhotoRate()

9999 CONTINUE

RETURN
END

!========================================================================
RECURSIVE SUBROUTINE GetLSODEerror( i,cmsg )

IMPLICIT NONE

INTEGER,      INTENT( IN  ) :: i
CHARACTER(*), INTENT( OUT ) :: cmsg

SELECT CASE( i )
  CASE( -1 )
    cmsg = 'Excess work done on this call (perhaps wrong mf).'
  CASE( -2 )
    cmsg = 'Excess accuracy requested (tolerances too small).'
  CASE( -3 )
    cmsg = 'Illegal input detected (see printed message).'
  CASE( -4 )
    cmsg = 'Repeated error test failures (check all inputs).'
  CASE( -5 )
    cmsg = 'Repeated convergence failures (perhaps bad jacobian '// &
           'supplied or wrong choice of mf or tolerances).'
  CASE( -6 )
    cmsg = 'Error weight became zero during problem (solution '// &
           'component i vanished, and atol or atol(i) = 0.)'
  CASE DEFAULT
    cmsg = ' '
END SELECT

RETURN
END

!========================================================================

RECURSIVE SUBROUTINE StepChemDep( p,ID,dt,fac_srf )

USE chem_fi
USE scipuff_fi
USE met_fi
USE step_p_fi
USE diagnostics_fi
USE mpi_fi, ONLY: isSerial

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ID
REAL,             INTENT( IN    ) :: dt
REAL,             INTENT( IN    ) :: fac_srf

INTEGER i, nmc
REAL    carea, taux, ppt, tfacd, tfacw
REAL    tautot, depspec, ddepspec, wdepspec
REAL    f1,cA
chem => chemMC(ID,myTid)

species => chem%species

CALL GetChemAux( ID,p )

nmc = nspecies + nequil

DO i = 1,nmc
  species(i)%msav = 0.0
END DO

carea = p%sr/csav

IF( lwash )THEN
  ppt = pratebl(iprtyp)
ELSE
  ppt = 0.
END IF

IF( p%sr > 0.0 .OR. lwash )THEN
IF( volx == 0. )RETURN

!--- Calculate deposition mass

  DO i = 1,nmc

    !====   deposition

    tautot = chem%species(i)%taudry + chem%species(i)%tauwet

    IF( chem%species(i)%taudry > 0. .OR. chem%species(i)%tauwet > 0. )THEN

      IF( chem%species(i)%taudry > 0. )THEN   !Time scale for implicit step (as in step_p)
        taux = chem%species(i)%taudry*dt
        tfacd = taux/(1.+taux)
      ELSE
        tfacd = 0.
      END IF
      IF( chem%species(i)%tauwet > 0. )THEN
        taux = chem%species(i)%tauwet*dt
        tfacw = taux/(1.+taux)
      ELSE
        tfacw = 0.
      END IF
!      IF( species(i)%class == ID_SPECIES_PARTICLE )THEN
      IF( chem%species(i)%mass > 0. .AND. ABS(chem%species(i)%conc/chem%species(i)%mass-1.) < 1.E-4 )THEN  !Check for cases where species(i)%conc = species(i)%mass, e.g., particles
        ddepspec = chem%species(i)%mass*tfacd
        wdepspec = chem%species(i)%mass*tfacw
      ELSE
        cA = MAX( chem%species(i)%conc,-chem%species(i)%amb )
        ddepspec = cA*tfacd*volx
        wdepspec = cA*tfacw*volx
      END IF
      IF( fac_srf /= NOT_SET_R )THEN
        ddepspec = ddepspec*fac_srf
        wdepspec = wdepspec*fac_srf
      END IF
      depspec  = ddepspec + wdepspec
      IF( chem%species(i)%mass >= -chem%species(i)%amb*volx )THEN
        IF( chem%species(i)%mass - depspec < -chem%species(i)%amb*volx )THEN
          f1 = ddepspec/depspec
          depspec = chem%species(i)%amb*volx + chem%species(i)%mass
          ddepspec = f1*depspec
          wdepspec = (1.-f1)*depspec
        END IF
      ELSE
        IF( depspec > 0. )THEN
          ddepspec = 0.
          wdepspec = 0.
          depspec = 0.
        END IF
      END IF
      IF( lDiagno )THEN
        ddeposition(i) = ddeposition(i) + ddepspec !dry deposition diagnostic
        wdeposition(i) = wdeposition(i) + wdepspec !wet deposition diagnostic
      END IF
      species(i)%mddp = ddepspec ! Dry deposition mass
      species(i)%mwdp = wdepspec ! Wet deposition mass
      chem%species(i)%mass = chem%species(i)%mass - depspec

      IF( wdepspec > 0. )lscav = .TRUE.

    ELSE
      species(i)%mddp = 0.
      species(i)%mwdp = 0.
    END IF

  END DO

  CALL PutChemAux( ID,p )

END IF

9999 CONTINUE

RETURN
END

!========================================================================

SUBROUTINE SetChemSrfDepMC( s,cfac,ng,ig )

USE chem_fi
USE srfdos_fd
USE constants_fd

IMPLICIT NONE

TYPE( srf_gauss_str ), INTENT( IN    ) :: s
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac
INTEGER,               INTENT( INOUT ) :: ng
INTEGER, DIMENSION(*), INTENT( INOUT ) :: ig

INTEGER i, nmc
INTEGER igrp, icomp
REAL    f
!----- assume that the chem species are already in memory (following StepChem)

nmc = nspecies + nequil

DO i = 1,nmc

  IF( species(i)%ldep )THEN
    ng = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = s%voli*species(i)%mddp
    ng = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = s%voli*species(i)%mwdp
    IF( chem%oUnits == UNIT_UGM3 .AND. &
        species(i)%class /= ID_SPECIES_PARTICLE )THEN
        cfac(ng-1) = cfac(ng-1)*PPM2UGM3*species(i)%MW
        cfac(ng)   = cfac(ng)*PPM2UGM3*species(i)%MW
    END IF
  END IF

END DO

IF( chem%nOutGroup > 0 )THEN
  DO igrp = 1,chem%nOutGroup
    ng       = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = 0.
    ng       = ng + 1
    ig(ng)   = ig(ng-1) + 1
    cfac(ng) = 0.
    DO icomp = 1,chem%OutGroup(igrp)%nComp
      i = chem%OutGroup(igrp)%iComp(icomp)
      f = chem%OutGroup(igrp)%fComp(icomp)
      IF( chem%oUnits == UNIT_UGM3 .AND. &
          species(i)%class /= ID_SPECIES_PARTICLE )&
          f = f*PPM2UGM3*chem%species(i)%MW
      cfac(ng-1) = cfac(ng-1) + f*s%voli*species(i)%mddp
      cfac(ng)   = cfac(ng)   + f*s%voli*species(i)%mwdp
    END DO
  END DO
END IF

RETURN
END

!========================================================================

SUBROUTINE GetSpeciesMass( spec_mass )

USE chem_fi

IMPLICIT NONE

REAL, INTENT( OUT ) :: spec_mass

INTEGER i

spec_mass = 0.

DO i = 1,nspectot
  spec_mass = spec_mass + species(i)%nit*species(i)%mass
END DO

RETURN
END

!========================================================================

SUBROUTINE GetSpeciesConc( fac_dist )

USE chem_fi

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( INOUT ) :: fac_dist

INTEGER i
REAL    total_conc, total_nit

total_conc = 0.
total_nit  = 0.

DO i = 1,nspecies
  total_conc = total_conc + species(i)%nit*(species(i)%conc + species(i)%amb)
  total_nit  = total_nit  + species(i)%nit
END DO

IF( total_conc /= 0. )THEN
  DO i = 1,nspecies
    ! Distribute according to total concentrations
    fac_dist(i) = species(i)%nit*(species(i)%conc + species(i)%amb)/total_conc
  END DO
ELSE
  DO i = 1,nspecies
    ! Distribute evenly
    fac_dist(i) = species(i)%nit/total_nit
  END DO
END IF

RETURN
END

