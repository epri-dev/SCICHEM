SUBROUTINE process_scn_cont( relSpec,crmode,defIN )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions, except_self => process_scn_cont ! Don't use interface to current function
USE convert_fd
USE met_fi        !For interface to get_met
USE files_fi

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec
INTEGER,              INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE
INTEGER, OPTIONAL,    INTENT( IN    ) :: defIN       !Release ID (for update only)

INTEGER defID, ios

REAL cmass, tdur, buoy, sz, shh
REAL sigx, sigy, sigz, sigRxy, sigRxz, sigRyz
REAL, DIMENSION(3) :: vel, mom
LOGICAL moving, pool
CHARACTER(PATH_MAXLENGTH) :: file

REAL,    EXTERNAL :: ScaleReal
LOGICAL, EXTERNAL :: still_active_rel
LOGICAL, EXTERNAL :: in_domain_rel

CALL getReleaseMass( relSpec%release,cmass )
CALL getReleaseSigmas( relSpec%release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )
CALL getReleaseVelocity( relSpec%release,vel )
CALL getReleaseDuration( relSpec%release,tdur )
CALL getReleaseFile( relSpec%release,file )

!------ Get tb for stack releases before getting dynamics

IF( BTEST(relSpec%release%type,HRB_STACK) )THEN
  sz = MAX(0.01*sigy,0.01*relSpec%release%zrel)
  shh = 0.5*(sigx**2+sigy**2)
  CALL get_met( SNGL(relSpec%release%xrel),SNGL(relSpec%release%yrel),relSpec%release%zrel,sz*sz,0.,0,Shh=shh )
END IF

CALL getReleaseDynamics( relSpec%release,buoy,mom )

moving = BTEST(relSpec%release%type,HRB_MOVE)
pool = BTEST(relSpec%release%type,HRB_POOL)

SELECT CASE( crmode )
  CASE( CRMODE_START )
    IF( cmass < 0.0          )GOTO 9999
    IF( .NOT.in_domain_rel(moving,tdur,relSpec%release%xrel,relSpec%release%yrel,sigx,sigy,vel) )THEN
      nError   = IV_ERROR
      eRoutine = 'process_scn_cont'
      WRITE(eMessage,'(A,F7.2,A)',IOSTAT=ios)'Continuouse release  at t =', &
                                  t/3600.,'hrs. outside domain'
      WRITE(eInform,'(A,F10.4,1X,F10.4)',IOSTAT=ios)'Location: ',relSpec%release%xrel,relSpec%release%yrel
    END IF
    defID = nextDefinition()
  CASE( CRMODE_RESTART )
    IF( cmass <  0.0            )GOTO 9999
    IF( .NOT.still_active_rel(pool,ScaleReal(relSpec%release%trel,HCF_HOUR2SEC),tdur) )GOTO 9999
    IF( .NOT.in_domain_rel(moving,tdur,relSpec%release%xrel,relSpec%release%yrel,sigx,sigy,vel) )THEN
      nError   = IV_ERROR
      eRoutine = 'process_scn_cont'
      WRITE(eMessage,'(A,F7.2,A,I5)',IOSTAT=ios)'Continuouse release  at t =', &
                                  t/3600.,'hrs. outside domain - ignored'
      WRITE(eInform,'(A,F10.4,1X,F10.4)',IOSTAT=ios)'Location: ',relSpec%release%xrel,relSpec%release%yrel
    END IF
    IF( moving )CALL restart_moving_cont(relSpec%release%xrel,relSpec%release%yrel,relSpec%release%zrel,&
                                         ScaleReal(relSpec%release%trel,HCF_HOUR2SEC),vel) !Reset moving release start position on a restart
    defID = nextDefinition()
  CASE( CRMODE_UPDATE )
    IF( PRESENT(defIN) )THEN
      defID = defIN
      IF( .NOT.validDefID(defID) )THEN
        nError   = UK_ERROR
        eRoutine = 'process_scn_cont'
        eMessage = 'Request to update continuous release failed'
        WRITE(eInform,'(A,I0,A,I0)')'Invalid defID. ID=',defID,' : num def=',numDefinition
      END IF
    ELSE
      nError   = IV_ERROR
      eRoutine = 'process_scn_cont'
      eMessage = 'Request to update continuous release failed'
      eInform  = 'No continuous release definition specified'
    END IF
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'process_scn_cont'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

CALL create_Definition( relSpec,defID,crmode )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! create_Definition
!*******************************************************************************
SUBROUTINE create_Definition( relSpec,defIN,crmode )

USE scipuff_fi
USE cont_rel_fi
USE relparam_fd
USE cont_rel_functions
USE default_fd
USE files_fi
USE convert_fd

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( INOUT ) :: relSpec !Release Specification
INTEGER, INTENT( IN ) :: defIN       !cDefinition structure ID
INTEGER, INTENT( IN ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER ios
REAL    h0, hx, hy
REAL    tdur
REAL    sigx, sigy, sigz, sigRxy, sigRxz, sigRyz
REAL, DIMENSION(3) :: vel

REAL,    EXTERNAL :: set_moving_step
REAL,    EXTERNAL :: ScaleReal
LOGICAL, EXTERNAL :: IsReady
REAL,    EXTERNAL :: GetNextUpdtTime

CALL getReleaseVelocity( relSpec%release,vel )
CALL getReleaseDuration( relSpec%release,tdur )
CALL getReleaseSigmas( relSpec%release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )

!---- Set release definition parameters used in the colocation check for interactions

cDefinition(defIN)%loc%x = relSpec%release%xrel
cDefinition(defIN)%loc%y = relSpec%release%yrel

IF( relSpec%release%type == HR_MOVE )THEN
  cDefinition(defIN)%isMoving = .TRUE.
  cDefinition(defIN)%vel%x = vel(1)
  cDefinition(defIN)%vel%y = vel(2)
  cDefinition(defIN)%vel%z = vel(3)
END IF

cDefinition(defIN)%mID = typeID(relSpec%ityp)%imat

!---- Set release definition parameters

cDefinition(defIN)%isPool    = BTEST(relSpec%release%type,HRB_POOL)
IF( crmode /= CRMODE_UPDATE)cDefinition(defIN)%time = MAX(ScaleReal( relSpec%release%trel,HCF_HOUR2SEC ),t)
cDefinition(defIN)%dur    = tdur

cDefinition(defIN)%update = ( .NOT.IsReady(relSpec%release%relName,relSpec%release%status) ) .AND. .NOT.cDefinition(defIN)%isPool !Pools get updated only initially
IF( cDefinition(defIN)%update )THEN
  CALL copyReleaseSpec(relSpec,cDefinition(defIN)%relSpec)
  cDefinition(defIN)%extraUpdate = (GetNextUpdtTime(cDefinition(defIN)) /= DEF_VAL_R)
END IF

wake  = .FALSE.
prise = .FALSE.

IF( cDefinition(defIN)%isPool )THEN
  cDefinition(defIN)%loc%z = 0.0D0
  cDefinition(defIN)%end   = DEF_VAL_R
  CALL build_contRel_pool( cDefinition(defIN),relSpec,crmode )
ELSE
  CALL get_topog( SNGL(relSpec%release%xrel),SNGL(relSpec%release%yrel),h0,hx,hy )
  cDefinition(defIN)%loc%z = DBLE(relSpec%release%zrel+h0)
  cDefinition(defIN)%end   = ScaleReal( relSpec%release%trel,HCF_HOUR2SEC ) +  tdur
  IF( cDefinition(defIN)%isMoving )THEN
    cDefinition(defIN)%dtr  = set_moving_step(vel,tdur,sigx,sigy,sigz)
  ELSE
    cDefinition(defIN)%dtr = tdur
  END IF
  CALL build_contRel( cDefinition(defIN),relSpec,crmode )
END IF

IF( cDefinition(defIN)%time > t )THEN
  cDefinition(defIN)%state = CR_READY
ELSE
  cDefinition(defIN)%state = CR_ACTIVE
END IF

IF( crmode /= CRMODE_UPDATE )THEN
  CALL AddToCollection( cDefinition(defIN) )
  IF( crmode == CRMODE_START )THEN
    WRITE(lun_log,'(A,F7.2,A,I4)',IOSTAT=ios) &
          'C-release activated at t =',t/3600.,'hrs. with ncrel  = ',count_nrel()
  ELSE
    WRITE(lun_log,'(A,F7.2,A,I4)',IOSTAT=ios) &
          'C-release reactivated at t =',t/3600.,'hrs. with ncrel  = ',count_nrel()
  END IF
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'create_Definition'
    eMessage = 'Error writing SCIPUFF log file'
    CALL ReportFileName( eInform,'File=',file_log )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel
!*******************************************************************************
SUBROUTINE build_contRel( def,relSpec,crmode )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions
USE sciprime_fi

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
TYPE( releaseSpecT ),     INTENT( INOUT ) :: relSpec !Release Specification
INTEGER,                  INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER irel, nrels
LOGICAL dyn_src
INTEGER ios
REAL    plen
REAL    massfrac
REAL    sigx, sigy, sigz, sigRxy, sigRxz, sigRyz, buoy, mmd, sigma, frac, cmass
REAL, DIMENSION(3) :: mom
INTEGER subgroup

TYPE( ReleaseT ) release

TYPE( cont_release_mass ) :: rmass
TYPE( cont_release_met )  :: contMet     !Extra save values from call get_met

REAL zrel,sz

REAL,                      EXTERNAL :: init_plen
TYPE( cont_release_mass ), EXTERNAL :: computeReleaseWeights
TYPE( cont_release_mass ), EXTERNAL :: computeZeroReleaseWeights
LOGICAL,                   EXTERNAL :: IsGas
INTEGER,                   EXTERNAL :: deallocatePuffAux

release = relSpec%release

CALL getReleaseMass( release,cmass )
CALL getReleaseDryFraction( release,frac )
CALL getReleaseDistribution( release,subgroup,mmd,sigma )
CALL getReleaseSigmas( release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )

IF( BTEST(release%type,HRB_POOL) )THEN
  zrel = 0.0
ELSE
  zrel = release%zrel
END IF
IF( BTEST(release%type,HRB_STACK) .OR. BTEST(release%type,HRB_POOL) )THEN
  sz = MAX(0.01*sigy,0.01*zrel)
ELSE
  sz = sigz
END IF

!==== get met required for initializing basePuffs

contMet%init  = .TRUE.
contMet%doSrf = .FALSE.
CALL get_contMet( contMet,release%xrel,release%yrel,zrel,sigx,sigy,sz )

! Must be placed after get_contMet as requires background temperature for stack releases
CALL getReleaseDynamics( release,buoy,mom )

!Stack sources input stack exit speed and temperature. Need to convert this to momentum and buoyancy
!Conversion of stack diameter to sigmas should have been done earlier.

CALL set_stack_rel_prime_null()

IF( BTEST(release%type, HRB_STACK) )THEN
  IF( BTEST(release%type,HRB_PRIME) )THEN
    IF( INDEX(release%relDisplay,'.pri') > 0 )THEN
      IF( cmass > 0. )THEN
        CALL set_stack_rel_prime( release,massfrac )
      ELSE
        CALL set_stack_rel_prime_null()
      END IF
      IF( nError /= NO_ERROR )THEN
        prise = .TRUE.
        CALL set_stack_rel_prise( release )  !use plume rise formula (if error in set_stack_rel_prime)
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE IF( .NOT.wake )THEN
        prise = .TRUE.   !Plume rise computed in set_stack_rel_prime (no building plume)
        wake  = .TRUE.   !Set so full plume info will be used
!        CALL set_stack_rel_prise( release )  !use plume rise formula
!        IF( nError /= NO_ERROR )GOTO 9999
      END IF
    ELSE
      prise = .TRUE.                         !should never go here
      CALL set_stack_rel_prise( release )    !use plume rise formula
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
END IF

!==== Set the number of releases

IF( cmass == 0. .AND. (BTEST(release%type,HRB_FILE) ) )THEN
  rmass = computeZeroReleaseWeights( release%xrel,release%yrel,contMet,relSpec%ityp,relSpec%distrib,cmass,frac,mmd,sigma )
ELSE
rmass = computeReleaseWeights( release%xrel,release%yrel,contMet,relSpec%ityp,relSpec%distrib,cmass,frac,mmd,sigma )
END IF
IF( rmass%num == 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'build_contRel'
  eMessage = 'computeReleaseWeights returned 0 groups with mass > 0.0'
  GOTO 9999
END IF

nrels = rmass%num
IF( wake .AND. .NOT.prise)THEN
  nrels = 2*nrels
END IF

!------ Define plume length for zero-mass releases so it's defined
!       if/when turned back on

IF( crmode == CRMODE_UPDATE .AND. cmass == 0. )THEN
    plen = init_plen( sigx,sigy,sigz )
    IF( nrels > 0 )THEN
      DO irel = 1,nrels
        def%rSet%rels(irel)%plen = plen
      END DO
    END IF
END IF

SELECT CASE( crmode )
  CASE( CRMODE_START )
    plen = init_plen( sigx,sigy,sigz )
    IF( nrels > 0 )THEN
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,nrels
        def%rSet%rels(irel)%plen = plen
      END DO
    END IF
  CASE( CRMODE_RESTART )
    IF( nrels /= def%rSet%nrel )THEN
      nError   = IV_ERROR
      eRoutine = 'build_contRel'
      eMessage = 'standard nrel inconsistent with restart data'
      WRITE(eInform,'(A,I0,A,I0)')'def%rSet%nrel=',def%rSet%nrel,' : nrels=',nrels
    END IF
  CASE( CRMODE_UPDATE )
    IF( nrels /= def%rSet%nrel )THEN
      plen = 0.0
      DO irel = 1,def%rSet%nrel
        plen = plen + def%rSet%rels(irel)%plen
      END DO
      plen = plen/FLOAT(MAX(def%rSet%nrel,1))
      ios = deallocate_Definition_rel( def )
      IF( ios /= 0 )THEN
        nError   = UK_ERROR
        eRoutine = 'build_contRel'
        eMessage = 'Error deallocating scipuff release specification'
        WRITE(eInform,'(A,I0)')'IOS = ',ios
        GOTO 9999
      END IF
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,def%rSet%nrel
        def%rSet%rels(irel)%plen = plen
      END DO
    ELSE
      DO irel = 1,def%rSet%nrel
        ios = deallocate_contRel_puff( def%rSet%rels(irel) )
        IF( ios /= 0 )GOTO 9999
        ios = deallocatePuffAux( def%rSet%rels(irel)%basePuff )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'build_contRel'
          eMessage = 'Error deallocating standard scipuff base puff'
          WRITE(eInform,'(A,I0)')'IOS = ',ios
          GOTO 9999
        END IF
      END DO
    END IF
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'build_contRel'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

!==== Determine if this is a dynamic definition

dyn_src = (mom(1) /= 0.0 .OR. mom(2) /= 0.0 .OR. mom(3) /= 0.0 .OR. buoy /= 0)
IF( .NOT.prise )dyn_src = dyn_src .OR. (ANY(wmom_prm(:) /= 0.) .OR. ANY(buoy_prm(:) /= 0.))
def%isDynamic = .FALSE.
IF( dynamic ) THEN
  DO irel = 1,rmass%num
    def%isDynamic = def%isDynamic .OR. (IsGas(typeID(rmass%ityp(irel))%icls) &
                    .AND. (dyn_src .OR. buoy_fac(rmass%ityp(irel)) /= 0.0) )
  END DO
END IF

!==== initialize basePuffs

IF( nrels > 0 )THEN
  CALL build_contRel_base( def,relSpec,def%rSet,rmass,contMet )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel_base
!*******************************************************************************
SUBROUTINE build_contRel_base( def,relSpec,set,rmass,contMet )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def  ), INTENT( IN    ) :: def         !cDefinition structure
TYPE( releaseSpecT  ),     INTENT( INOUT ) :: relSpec     !reelase specification (In cDefinition structure only on updates)
TYPE( cont_release_set  ), INTENT( INOUT ) :: set         !cDefinition base structure
TYPE( cont_release_mass ), INTENT( IN    ) :: rmass       !mass/iytp for each base
TYPE( cont_release_met  ), INTENT( IN    ) :: contMet     !extra met data

INTEGER irel, ios
INTEGER iwake, jrel
REAL    pmass

INTEGER, EXTERNAL :: allocatePuffAux

IF( wake .OR. prise )THEN
  IF( wake .AND. .NOT.prise)THEN
    DO iwake = 1,2
      relSpec%release%padding = iwake
      CALL getReleaseMass( relSpec%release,pmass )
      DO jrel = 1,set%nrel,2
        irel = jrel + (iwake - 1)
        set%rels(irel)%time = def%time
        set%rels(irel)%loc = def%loc
        set%rels(irel)%basePuff%naux = typeID(rmass%ityp(jrel))%npaux
        ios = allocatePuffAux( set%rels(irel)%basePuff )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'build_contRel_base'
          eMessage = 'Error allocating scipuff puff auxiliary array'
          WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(irel)%basePuff%naux,'  : Error = ',ios
          GOTO 9999
        END IF
        CALL set_basePuff( set%rels(irel)%basePuff,relSpec,pmass,rmass%ityp(jrel),contMet )
      END DO
    END DO
  ELSE
    relSpec%release%padding = 1
    CALL getReleaseMass( relSpec%release,pmass )
    set%rels(1)%time = def%time
    set%rels(1)%loc = def%loc
    set%rels(1)%basePuff%naux = typeID(rmass%ityp(1))%npaux
    ios = allocatePuffAux( set%rels(1)%basePuff )
    IF( ios /= 0 )THEN
      nError   = UK_ERROR
      eRoutine = 'build_contRel_base'
      eMessage = 'Error allocating scipuff puff auxiliary array'
      WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(1)%basePuff%naux,'  : Error = ',ios
      GOTO 9999
    END IF
    CALL set_basePuff( set%rels(1)%basePuff,relSpec,pmass,rmass%ityp(1),contMet )
  END IF
  GOTO 9999
END IF

DO irel = 1,set%nrel
  set%rels(irel)%time = def%time
  set%rels(irel)%loc = def%loc
  set%rels(irel)%basePuff%naux = typeID(rmass%ityp(irel))%npaux
  ios = allocatePuffAux( set%rels(irel)%basePuff )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'build_contRel_base'
    eMessage = 'Error allocating scipuff puff auxiliary array'
    WRITE(eInform,'(A,I0,A,I0)')'Request=',set%rels(irel)%basePuff%naux,'  : Error = ',ios
    GOTO 9999
  END IF
  CALL set_basePuff( set%rels(irel)%basePuff,relSpec,rmass%mass(irel),rmass%ityp(irel),contMet )
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
! computeReleaseWeights
!*******************************************************************************
FUNCTION computeReleaseWeights( xrel,yrel,contMet,ityp,distrib,cmass,frac,mmd,sigma ) RESULT( relMass )

USE cont_rel_fd
USE scipuff_fi
USE error_fi
USE param_fd
USE relparam_fd

IMPLICIT NONE

REAL(8),                   INTENT( IN    ) :: xrel
REAL(8),                   INTENT( IN    ) :: yrel
TYPE( cont_release_met  ), INTENT( IN    ) :: contMet     !Extra save values from call get_met
INTEGER,                   INTENT( INOUT ) :: ityp
INTEGER,                   INTENT( INOUT ) :: distrib
REAL,                      INTENT( IN    ) :: cmass
REAL,                      INTENT( IN    ) :: frac
REAL,                      INTENT( IN    ) :: mmd
REAL,                      INTENT( IN    ) :: sigma
TYPE( cont_release_mass )                  :: relMass

INTEGER jtyp, i, imat, nsg
LOGICAL l2phase
REAL    rxx, cmass_liq
REAL    pbounds(MAXSGP+1)
REAL    weight(MAXSGP)

LOGICAL, EXTERNAL :: IsWetParticle

relMass%num    = 0
relMass%mass = 0.0
relMass%ityp   = 0

!-----  Set parameters for simple continuous release

imat = typeID(ityp)%imat

!-----  Check 2-phase liquid release

CALL check_liquid_reltyp( ityp,xrel,yrel,contMet%zbar,distrib,mmd,frac,l2phase )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check for wet particle release

CALL check_wet_reltyp( ityp,frac )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check distribution dist < 0 implies single bin

IF( distrib <= 0 )THEN

!---------  Set single puff release

  IF( l2phase )THEN

    cmass_liq = cmass*frac
    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass_liq                  !liquid
    relMass%ityp(relMass%num) = ityp

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass - cmass_liq          !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1

  ELSE

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass
    relMass%ityp(relMass%num) = ityp

  END IF

ELSE

!---------  Set lognormal distribution for release

  CALL get_bounds( material(imat),nsg,pbounds )
  IF( nsg < 2 )THEN
    eMessage = 'Request for a lognormal distribution of release mass but'
    eInform  = TRIM(ADJUSTL(material(imat)%cmat))//' has only a single bin'
    eAction  = 'It is more efficient to use a single bin release'
    CALL CautionMessage()
  END IF

  IF( IsWetParticle(typeID(ityp)%icls) )CALL set_wetbin( pbounds,nsg,imat,frac )
  CALL check_logn( pbounds(1),pbounds(nsg+1),mmd,sigma,rxx )
  IF( rxx > 0.05 )THEN
    nError   = WN_ERROR
    eRoutine = 'computeReleaseWeights'
    WRITE(eMessage,*) NINT(rxx*100.0)
    eMessage = TRIM(ADJUSTL(eMessage))//'% of the mass is outside the bin range'
    eInform  = 'This mass will be lumped into the first and last bins'
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )THEN
      eMessage = 'More than 5% of the mass is outside the bin range'
      eInform  = 'Please adjust the MMD and/or sigma'
      eAction  = '(or redefine the material size bins)'
      GOTO 9999
    END IF
  END IF

  CALL logn_bin( pbounds,nsg,mmd,sigma,weight )

!---- Check for 2-phase liquid release; reset weights and release vapor puff

  IF( l2phase )THEN

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass*(1.0-frac)  !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1
    DO i = 1,nsg
      weight(i) = weight(i)*frac                  !liquid
    END DO

  END IF

  DO i = 1,nsg
    jtyp = ityp + i - 1
    IF( cmass*weight(i) > 0.0 )THEN
      relMass%num = relMass%num + 1
      relMass%mass(relMass%num) = cmass*weight(i)
      relMass%ityp(relMass%num) = jtyp
    END IF
  END DO
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! computeZeroReleaseWeights
!*******************************************************************************
FUNCTION computeZeroReleaseWeights( xrel,yrel,contMet,ityp,distrib,cmass,frac,mmd,sigma ) RESULT( relMass )

!------ Special case of zero-mass continuous release defined from a file

USE cont_rel_fd
USE scipuff_fi
USE error_fi
USE param_fd
USE relparam_fd

IMPLICIT NONE

REAL(8),                   INTENT( IN    ) :: xrel
REAL(8),                   INTENT( IN    ) :: yrel
TYPE( cont_release_met  ), INTENT( IN    ) :: contMet     !Extra save values from call get_met
INTEGER,                   INTENT( INOUT ) :: ityp
INTEGER,                   INTENT( INOUT ) :: distrib
REAL,                      INTENT( IN    ) :: cmass
REAL,                      INTENT( IN    ) :: frac
REAL,                      INTENT( IN    ) :: mmd
REAL,                      INTENT( IN    ) :: sigma
TYPE( cont_release_mass )                  :: relMass

INTEGER jtyp, i, imat, nsg
LOGICAL l2phase
REAL    rxx, cmass_liq
REAL    pbounds(MAXSGP+1)
REAL    weight(MAXSGP)

LOGICAL, EXTERNAL :: IsWetParticle

relMass%num  = 0
relMass%mass = 0.0
relMass%ityp = 0

!-----  Set parameters for simple continuous release

imat = typeID(ityp)%imat

!-----  Check 2-phase liquid release

CALL check_liquid_reltyp( ityp,xrel,yrel,contMet%zbar,distrib,mmd,frac,l2phase )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check for wet particle release

CALL check_wet_reltyp( ityp,frac )
IF( nError /= NO_ERROR )GOTO 9999

!-----  Check distribution dist < 0 implies single bin

IF( distrib <= 0 )THEN

!---------  Set single puff release

  IF( l2phase )THEN

    cmass_liq = cmass*frac
    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass_liq                  !liquid
    relMass%ityp(relMass%num) = ityp

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass - cmass_liq          !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1

  ELSE

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass
    relMass%ityp(relMass%num) = ityp

  END IF

ELSE

!---------  Set lognormal distribution for release

  CALL get_bounds( material(imat),nsg,pbounds )
  IF( nsg < 2 )THEN
    eMessage = 'Request for a lognormal distribution of release mass but'
    eInform  = TRIM(ADJUSTL(material(imat)%cmat))//' has only a single bin'
    eAction  = 'It is more efficient to use a single bin release'
    CALL CautionMessage()
  END IF

  IF( IsWetParticle(typeID(ityp)%icls) )CALL set_wetbin( pbounds,nsg,imat,frac )
  CALL check_logn( pbounds(1),pbounds(nsg+1),mmd,sigma,rxx )
  IF( rxx > 0.05 )THEN
    nError   = WN_ERROR
    eRoutine = 'computeReleaseWeights'
    WRITE(eMessage,*) NINT(rxx*100.0)
    eMessage = TRIM(ADJUSTL(eMessage))//'% of the mass is outside the bin range'
    eInform  = 'This mass will be lumped into the first and last bins'
    CALL WarningMessage( .TRUE. )
    IF( nError /= NO_ERROR )THEN
      eMessage = 'More than 5% of the mass is outside the bin range'
      eInform  = 'Please adjust the MMD and/or sigma'
      eAction  = '(or redefine the material size bins)'
      GOTO 9999
    END IF
  END IF

  CALL logn_bin( pbounds,nsg,mmd,sigma,weight )

!---- Check for 2-phase liquid release; reset weights and release vapor puff

  IF( l2phase )THEN

    relMass%num = relMass%num + 1
    relMass%mass(relMass%num) = cmass*(1.0-frac)  !vapor
    relMass%ityp(relMass%num) = material(imat)%ioffp + 1

    DO i = 1,nsg
      weight(i) = weight(i)*frac                  !liquid
    END DO

  END IF

  DO i = 1,nsg
    jtyp = ityp + i - 1
    IF( weight(i) > 0.0 )THEN
      relMass%num = relMass%num + 1
      relMass%mass(relMass%num) = 0.
      relMass%ityp(relMass%num) = jtyp
    END IF
  END DO
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! build_contRel_pool
!*******************************************************************************
SUBROUTINE build_contRel_pool( def,relSpec,crmode )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
TYPE( releaseSpecT ),     INTENT( INOUT ) :: relSpec     !Release Specification
INTEGER,                  INTENT( IN    ) :: crmode      !CRMODE_START, CRMODE_RESTART or CRMODE_UPDATE

INTEGER nrels, irel
REAL    plen, zrel, sz
REAL    cmass, sigx, sigy, sigz, sigRxy, sigRxz, sigRyz

TYPE( cont_release_met )  :: contMet     !Extra save values from call get_met
TYPE( cont_release_mass ) :: rmass

TYPE( ReleaseT ) release

REAL,           EXTERNAL :: init_plen
INTEGER,        EXTERNAL :: allocatePuffAux

release = relSpec%release
CALL getReleaseMass( release,cmass )
CALL getReleaseSigmas( release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )

IF( BTEST(release%type, HRB_POOL) )THEN
  zrel = 0.0
ELSE
  zrel = release%zrel
END IF
IF( BTEST(release%type,HRB_STACK) .OR. BTEST(release%type,HRB_POOL) )THEN
  sz = MAX(0.01*sigy,0.01*zrel)
ELSE
  sz = sigz
END IF

!==== get met required for initializing basePuffs

contMet%init  = .TRUE.
contMet%doSrf = .TRUE.
CALL get_contMet( contMet,release%xrel,release%yrel,zrel,sigx,sigy,sz )

!==== Set the number of releases
!     For pool releases
nrels = 1

rmass%num     = 1
rmass%mass(1) = cmass
rmass%ityp(1) = relSpec%ityp

SELECT CASE( crmode )
  CASE( CRMODE_START )
    plen = init_plen(sigx,sigy,sigz)
    IF( nrels > 0 )THEN
      def%rSet%nrel = nrels
      CALL allocate_contSet_rel( def%rSet )
      DO irel = 1,nrels
        def%rSet%rels(irel)%plen = plen
          CALL init_poolAux( def,cmass,def%rSet%rels(irel) )
      END DO
    END IF
  CASE( CRMODE_RESTART )
    IF( nrels /= def%rSet%nrel )THEN
      nError   = IV_ERROR
      eRoutine = 'build_contRel_pool'
      eMessage = 'standard nrel inconsistent with restart data'
      WRITE(eInform,'(A,I0,A,I0)')'def%rSet%nrel=',def%rSet%nrel,' : nrels=',nrels
      GOTO 9999
    END IF
    CALL check_poolAux( def )
    IF( def%time == DEF_VAL_R )GOTO 9999   !This pool has completed.
  CASE( CRMODE_UPDATE )
    nError   = IV_ERROR
    eRoutine = 'build_contRel_pool'
    eMessage = 'Request to update pool release failed'
    eInform  = 'Pool updates after release has started not yet enabled'
  CASE DEFAULT
    nError   = IV_ERROR
    eRoutine = 'build_contRel_pool'
    eMessage = 'Unrecognized continuous release mode'
    WRITE(eInform,'(A,I0)')'mode =',crmode
END SELECT
IF( nError /= NO_ERROR )GOTO 9999

def%isDynamic = .FALSE.

!==== Set the basePuff

IF( nrels > 0 )THEN
  CALL build_contRel_base( def,relSpec,def%rSet,rmass,contMet )
  IF( nError /= NO_ERROR )GOTO 9999
  def%rSet%tlev = 0
  DO irel = 1,nrels
      CALL init_tlev_pool( def%rSet%rels(irel)%basePuff )
    IF( nError /= NO_ERROR )GOTO 9999
    def%rSet%tlev = MAX(def%rSet%tlev,def%rSet%rels(irel)%basePuff%idtl)
  END DO
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! init_tlev_pool
!*******************************************************************************
SUBROUTINE init_tlev_pool( p )

USE scipuff_fi
USE met_fi
USE error_fi
USE srfevap_fi
USE files_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL    del, u2, v2
INTEGER ilev

INTEGER, EXTERNAL :: time_level

!-----  Set time-level for the pool release based on pool size
!       and local surface wind speed (get_met and get_srf_met should have already been called)

u2  = ubsrf*ubsrf + uub + uubl + vvbl
v2  = vbsrf*vbsrf + vvb + uubl + vvbl
del = MAX(SQRT(u2/p%sxx),SQRT(v2/p%syy),1.0/delt)
del = 2.0/del

ilev = time_level( del )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'init_tlev_pool'
  WRITE(lun_log,*,ERR=9998)'******* TIME LEVEL ERROR ********'
  WRITE(lun_log,*,ERR=9998)TRIM(eRoutine)
  WRITE(lun_log,*,ERR=9998)TRIM(eInform)
  WRITE(lun_log,*,ERR=9998)'DT=',del
  WRITE(lun_log,*,ERR=9998)'DELT=',delt
  WRITE(lun_log,*,ERR=9998)'LEVEL=',ilev,MAXTLV
  WRITE(lun_log,*,ERR=9998)'U,V=',ubsrf,vbsrf
  CALL dump_puff( 0,p )
  GOTO 9999
END IF

p%idtl = ilev
mxtlev = MAX(mxtlev,ilev)

9999 CONTINUE

RETURN

9998 CONTINUE
nError   = UK_ERROR
eMessage = 'Error writing to log file'
GOTO 9999

END
!*******************************************************************************
! set_basePuff
!*******************************************************************************
SUBROUTINE set_basePuff( p,relSpec,rmass,ityp,contMet )

USE scipuff_fi
USE cont_rel_fi
USE relparam_fd

IMPLICIT NONE

TYPE( puff_str ),         INTENT( INOUT ) :: p
TYPE( ReleaseSpecT ),     INTENT( IN    ) :: relSpec
REAL,                     INTENT( IN    ) :: rmass
INTEGER,                  INTENT( IN    ) :: ityp
TYPE( cont_release_met ), INTENT( IN    ) :: contMet     !Extra save values

REAL sigx, sigy, sigz, sigRxy, sigRxz, sigRyz, frac, tdur, zbar
REAL(8) xbar, ybar
REAL, DIMENSION(3) :: vel
REAL kyprm, kzprm

CALL getReleaseSigmas( relSpec%release,sigx,sigy,sigz,sigRxy,sigRxz,sigRyz )
Call getReleaseActiveFraction( relSpec%release,frac )
CALL getReleaseVelocity( relSpec%release,vel )
CALL getReleaseDuration( relSpec%release,tdur )
CALL getReleaseLocation( relSpec%release,contMet,xbar,ybar,zbar )

p%c    = rmass
p%xbar = xbar
p%ybar = ybar
p%zbar = zbar
IF( sigx /= DEF_VAL_R .AND. sigx /= NOT_SET_R )THEN
  p%sxx  = sigx*sigx
  p%syy  = sigy*sigy
  p%szz  = sigz*sigz
  IF( BTEST(relSpec%release%type,HRB_OFFDIAG) )THEN
    p%sxy = sigRxy*sigx*sigy
    p%sxz = sigRxz*sigx*sigz
    p%syz = sigRyz*sigy*sigz
  END IF
  IF( sigx == 0.0 )THEN
    p%si  = sigy
    p%si2 = sigy
  ELSE
    p%si  = MIN(sigx,sigy)
    p%si2 = MAX(sigx,sigy)
  END IF
  IF( sigz == 0.0 )THEN
    p%sv = MIN(sigx,sigy)
  ELSE
    p%sv = sigz
  END IF
END IF
IF( wake )THEN
  CALL getReleasePrime( relSpec%release,kyprm,kzprm )
  p%yvsc = rmass*kyprm
  p%zwc  = rmass*kzprm
END IF
p%cfo = frac

p%ityp = ityp

CALL setPuffifld( p,contMet%ifld )
CALL setPuffirel( p,0 )

p%uo = vel(1)
p%vo = vel(2)
p%wo = vel(3)

CALL set_baseAux( p,relSpec )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! set_baseAux
!*******************************************************************************
SUBROUTINE set_baseAux( p,relSpec )

USE scipuff_fi
USE relparam_fd
USE UtilMtlAux
USE convert_fd

IMPLICIT NONE

TYPE( puff_str ),     INTENT( INOUT ) :: p
TYPE( releaseSpecT ), INTENT( IN    ) :: relSpec

TYPE( puff_liquid   ) pq
TYPE( puff_aerosol  ) pa
TYPE( puff_dynamics ) pd

TYPE( puff_material   ) pmatl
TYPE( part_material   ) pmatpart
TYPE( liquid_material ) pmatliq

INTEGER i, ityp, icls, imat
REAL    rat
REAL buoy, frac
REAL, DIMENSION(3) :: mom

LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
INTEGER, EXTERNAL :: allocatePuffAux
REAL,    EXTERNAL :: ScaleReal

!---- Initial checks and setup

IF( p%naux == 0 )GOTO 9999

IF( .NOT.ASSOCIATED(p%aux) )THEN
  nError   = UK_ERROR
  eRoutine = 'set_basePuff'
  eMessage = 'Error setting puff auxiliary array'
  eInform  = 'Puff auxiliary array not allocated'
  GOTO 9999
END IF

ityp = p%ityp
icls = typeID(ityp)%icls

!---- dynamic aux

IF( dynamic )THEN
  CALL get_dynamics( p,pd )
  CALL getReleaseDynamics( relSpec%release,buoy,mom )
  pd%w = mom(3)
  pd%t = buoy
  pd%un = mom(1)
  pd%vn = mom(2)
  CALL put_dynamics( p,pd )
END IF

!---- Liquid material specific aux

CALL getReleaseDryFraction( relSpec%release,frac )
IF( IsLiquid(icls) )THEN

  CALL get_liquid( p,pq )
  CALL get_puff_material( ityp,pmatl )
  pmatliq = TRANSFER(pmatl,pmatliq)
  pq%d    = pmatliq%dbar
  pq%sigd = (pmatliq%dmax-pmatliq%dmin)/(2.*SQRT3)
  pq%tevap = 0.0
  pq%t    = 0.0
  pq%ccs  = p%c
  CALL put_liquid( p,pq )

ELSE IF( IsWetParticle(icls) )THEN

  CALL get_liquid( p,pq )
  i = ityp - GetSubgroups( material(typeID(ityp)%imat),mat_aux )
  CALL get_puff_material( i,pmatl )
  pmatpart = TRANSFER(pmatl,pmatpart)
  imat = typeID(ityp)%imat
  IF( material(imat)%AuxMatID /= NOT_SET_I )THEN
    rat = pmatpart%rho/WetPartLiquidMat(material(imat)%AuxMatID)%rho
  ELSE
    rat = pmatpart%rho/RHO_WATER
  END IF
  rat = (1.0+rat*(1.0/frac-1.0))**0.3333333
  pq%d    = pmatpart%dbar * rat
  pq%sigd = (pmatpart%dmax-pmatpart%dmin)/(2.*SQRT3) * rat
  pq%tevap = 0.0
  pq%t     = 0.0
  pq%ccs   = p%c
  CALL put_liquid( p,pq )

END IF

IF( IsAerosol(icls) )THEN
  pa%fl    = frac
  pa%fw    = 1.0E-10
  pa%tevap = 0.0
  pa%co    = 0.0
  CALL put_aerosol( p,pa )
END IF

!---- multicomponent aux

IF( IsMulti(icls) )THEN
  CALL InitMCinst( relSpec,p )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! init_poolAux
!*******************************************************************************
SUBROUTINE init_poolAux( def,cmass,rel )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure
REAL,                     INTENT( IN    ) :: cmass
TYPE( cont_release_rel ), INTENT( INOUT ) :: rel         !contRel structure

REAL age, offset

!---- Call InitPoolAux

age    = 0.0     !Initial pool age
offset = 0.0     !Initial pool temperature offset

CALL InitPoolAux( def%mID,cmass,age,offset,rel )

9999 CONTINUE

RETURN
END
!*******************************************************************************
! check_poolAux
!*******************************************************************************
SUBROUTINE check_poolAux( def )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def         !cDefinition structure

INTEGER i, naux
LOGICAL inactive

inactive = .TRUE.
DO i = 1,def%rSet%nrel
  IF( .NOT.ASSOCIATED(def%rSet%rels(i)%saux) )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with unassociated saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  naux = SIZE(def%rSet%rels(i)%saux)
  IF(  naux /= def%rSet%rels(i)%naux )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with wrong size saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  IF(  naux /= SCIPUFF_POOLSRCPARAM .AND. naux /= 1 )THEN
    nError   = IV_ERROR
    eMessage = 'Error restarting auxiliary source data'
    eInform  = 'Standard Pool release with invalid size saux'
    eRoutine = 'check_poolAux'
    GOTO 9999
  END IF
  inactive = inactive .AND. (naux == 1)
END DO

IF( inactive )THEN
  def%time = DEF_VAL_R  !This pool is complete. Set so it will not restart.
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! get_contMet
!*******************************************************************************
SUBROUTINE get_contMet( contMet,xrel,yrel,zrel,sigx,sigy,sigz )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE met_fi        !For interface to get_met

IMPLICIT NONE

TYPE( cont_release_met ), INTENT( INOUT ) :: contMet
REAL(8),                  INTENT( IN    ) :: xrel
REAL(8),                  INTENT( IN    ) :: yrel
REAL,                     INTENT( IN    ) :: zrel
REAL,                     INTENT( IN    ) :: sigx
REAL,                     INTENT( IN    ) :: sigy
REAL,                     INTENT( IN    ) :: sigz

REAL h, hx, hy, x, y

x = SNGL(xrel)
y = SNGL(yrel)

IF( contMet%init )THEN
  contMet%shh = 0.5*(sigx**2+sigy**2)

  CALL get_topogOut( x,y,h,hx,hy,contMet%shh,contMet%ifld )

  contMet%zbar = zrel + h

  contMet%szz = sigz*sigz

  contMet%init = .FALSE.
END IF

CALL get_met( x,y,contMet%zbar,contMet%szz,0.,0,inField=contMet%ifld,Shh=contMet%shh )
IF( nError /= NO_ERROR )GOTO 9999

IF( contMet%doSrf )THEN
  CALL get_srf_met( x,y )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! still_active_rel
!*******************************************************************************
LOGICAL FUNCTION still_active_rel(pool,trel,tdur) RESULT( yes )

USE scipuff_fi
USE error_fi
USE default_fd

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: pool
REAL,    INTENT( IN ) :: trel
REAL,    INTENT( IN ) :: tdur

!Pool releases are always "active" becuase they do not have a predefined end time
!Pool releases get checked later to see if they still have mass to release

yes = pool
IF( .NOT.yes )yes = (trel+tdur > t)

RETURN
END
!*******************************************************************************
! in_domain_rel
!*******************************************************************************
LOGICAL FUNCTION in_domain_rel(moving,tdur,xrel,yrel,sigx,sigy,vel) RESULT( yes )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

LOGICAL,            INTENT( IN ) :: moving
REAL,               INTENT( IN ) :: tdur
REAL(8),            INTENT( IN ) :: xrel
REAL(8),            INTENT( IN ) :: yrel
REAL,               INTENT( IN ) :: sigx
REAL,               INTENT( IN ) :: sigy
REAL, DIMENSION(3), INTENT( IN ) :: vel

REAL    xmap, ymap, x, y
REAL    xx, yy, rxx, ryy
REAL    dtm
INTEGER i, n

LOGICAL, EXTERNAL :: chkgrd

IF( moving )THEN
  n = 11
  dtm = 0.1*tdur
ELSE
  n = 1
  dtm = 0.
END IF

x = SNGL(xrel)
y = SNGL(yrel)

DO i = 1,n

  CALL mapfac( x,y,xmap,ymap )

  xx   = (x-xmin)/dxg
  yy   = (y-ymin)/dyg
  rxx  = (sigx*xmap/dxg)**2
  ryy  = (sigy*ymap/dyg)**2

  yes = chkgrd(xx,yy,rxx,ryy)

  IF( yes )EXIT

  x = x + dtm*vel(1)*xmap
  y = y + dtm*vel(2)*ymap

END DO

RETURN
END
!*******************************************************************************
! init_plen
!*******************************************************************************
REAL FUNCTION init_plen(sigx,sigy,sigz) RESULT( plen )

USE scipuff_fi
USE cont_rel_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: sigx
REAL, INTENT( IN ) :: sigy
REAL, INTENT( IN ) :: sigz

IF( sigx == 0.0 )THEN
  plen = MIN(sigz,sigy)  !Standard continuous release
ELSE
  plen = MIN(sigx,sigy)  !Pool, stack releases
END IF

RETURN
END
!*******************************************************************************
! restart_moving_cont
!*******************************************************************************
SUBROUTINE restart_moving_cont(xrel,yrel,zrel,trel,vel)

USE scipuff_fi

IMPLICIT NONE

REAL(8),            INTENT( INOUT ) :: xrel
REAL(8),            INTENT( INOUT ) :: yrel
REAL,               INTENT( INOUT ) :: zrel
REAL,               INTENT( IN    ) :: trel
REAL, DIMENSION(3), INTENT( IN    ) :: vel

REAL xmap, ymap, tmov

IF( t > trel )THEN
  CALL mapfac( SNGL(xrel),SNGL(yrel),xmap,ymap )
  tmov = t - trel
  xrel = xrel + tmov*vel(1)*xmap
  yrel = yrel + tmov*vel(1)*ymap
  zrel = zrel + tmov*vel(3)
END IF

RETURN
END
!*******************************************************************************
! set_moving_step
!*******************************************************************************
REAL FUNCTION set_moving_step(vel,tdur,sigx,sigy,sigz) RESULT( dtr )

USE scipuff_fi

IMPLICIT NONE

REAL, DIMENSION(3), INTENT( IN    ) :: vel
REAL,               INTENT( IN    ) :: tdur
REAL,               INTENT( INOUT ) :: sigx
REAL,               INTENT( INOUT ) :: sigy
REAL,               INTENT( INOUT ) :: sigz

INTEGER, PARAMETER :: NMOVP = 10

REAL    v
INTEGER nstp

v = SQRT(vel(1)*vel(1)+vel(2)*vel(2)+vel(3)*vel(3))

IF( sigx == 0.0 )THEN
  v = v/MIN(sigy,sigz)
ELSE IF( sigz == 0.0 )THEN
  v = v/MIN(sigy,sigx)
ELSE
  v = v/MIN(sigx,sigy,sigz)
END IF

nstp = MIN(NMOVP,INT(v*tdur)+1)
nstp = MAX(nstp,INT(0.003*v*tdur)+1)

dtr = tdur/FLOAT(nstp)

RETURN
END
!*******************************************************************************
! set_stack_params
!*******************************************************************************
SUBROUTINE set_stack_params(sigx,sigy,buoy,mom)

USE scipuff_fi
USE met_fi

IMPLICIT NONE

REAL,               INTENT( IN    ) :: sigx
REAL,               INTENT( IN    ) :: sigy
REAL,               INTENT( INOUT ) :: buoy
REAL, DIMENSION(3), INTENT( INOUT ) :: mom

!  Set true release parameters for STACK release. Requires met.
!  STACK release contains EXIT TEMPERATURE (C) in buoy
!                         EXIT VELOCITY in wmom
!                         EXIT DIAMETER (m) converted to sigmas in set_stack_sig

REAL tstack, wstack, astack, bfac
REAL ustack, vstack, v, tmom

IF( buoy == DEF_VAL_R )THEN
  tstack = tb
ELSE
  tstack = buoy - ABSZERO
END IF

astack = PI*sigx*sigy
bfac   = tb/MAX(tstack,tb)
IF( mom(1) == DEF_VAL_R )THEN
  wstack = mom(3)
  buoy   = astack*wstack*(tstack-tb)*bfac
  mom(3)   = astack*wstack*ABS(wstack)*bfac
  mom(1)   = 0.0
  mom(2)   = 0.0
ELSE
  wstack = mom(3)
  vstack = mom(2) - vb
  ustack = mom(1) - ub
  v    = SQRT( ustack*ustack + vstack*vstack + wstack*wstack)
  IF( v == 0.0 )THEN
    buoy = 0.0
    mom(3) = 0.0
    mom(1) = 0.0
    mom(2) = 0.0
  ELSE
    buoy = astack*v*(tstack-tb)*bfac
    tmom  = astack*v*bfac
    mom(1) = tmom*ustack
    mom(2) = tmom*vstack
    mom(3) = tmom*wstack
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
! set_area_params
!*******************************************************************************
SUBROUTINE set_area_params(area,buoy,mom)

USE scipuff_fi
USE met_fi

IMPLICIT NONE

REAL,               INTENT( IN    ) :: area
REAL,               INTENT( INOUT ) :: buoy
REAL, DIMENSION(3), INTENT( INOUT ) :: mom

!  Set true release parameters for AREA release. Requires met.
!  AREA release contains EXIT TEMPERATURE (C) in buoy
!                        EXIT VELOCITY in wmom
!                        XLEN, YLEN (m), ANG (deg) converted to sigmas in SetAreaSig

REAL tstack, wstack, bfac

IF( buoy == DEF_VAL_R )THEN
  tstack = tb
ELSE
  tstack = buoy - ABSZERO
END IF

bfac   = tb/MAX(tstack,tb)
wstack = mom(3)
buoy   = area*wstack*(tstack-tb)*bfac
mom(3) = area*wstack*ABS(wstack)*bfac
mom(1) = 0.0
mom(2) = 0.0

RETURN
END

SUBROUTINE copyReleaseSpec( specIn,specOut )
USE release_fd

IMPLICIT NONE

TYPE( releaseSpecT ), INTENT( IN    ) :: specIn
TYPE( releaseSpecT ), INTENT( INOUT ) :: specOut

specOut%ityp    = specIN%ityp
specOut%distrib = specIN%distrib

specOut%release = specIN%release
CALL CopyMCrelList( specIn%MClist,specOut%MClist )

RETURN
END
