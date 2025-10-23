SUBROUTINE c_release( dts,tstep,lev1,lev2 )

USE scipuff_fi
USE cont_rel_fi
USE error_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,    INTENT( IN    ) :: dts        ! current timestep
REAL,    INTENT( IN    ) :: tstep      ! end time for current large timestep
INTEGER, INTENT( IN    ) :: lev1       ! current time level
INTEGER, INTENT( INOUT ) :: lev2       ! max time level

INTEGER mpuf

!==== Check for continuous releases

IF( countDefinitions() == 0 )GOTO 9999

!==== Update releases (active cDefinition)

IF( runUpdates )THEN            !Start of a step
  runUpdates = .FALSE.
  CALL UpdateContinuousReleases( tstep )
  IF( nError /= NO_ERROR )GOTO 9999
ELSE                            !Extra updates
  CALL UpdateContinuousReleasesExtra( t+0.5*dts,tstep,lev1 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Activate releases (inactive cDefinition)

CALL ActivateContinuousReleases( t+0.5*dts,lev1 )
IF( nError /= NO_ERROR )GOTO 9999

!==== Update interactions (nonPool cCollection basePuffs)

CALL InteractContinuousReleases( .FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

!==== Create relPuff is necessary (active cDefinition relPuffs)

CALL InitContinuousReleases()
IF( nError /= NO_ERROR )GOTO 9999

!==== Save the current number of puffs

mpuf = npuf

!==== Create statics (nonPool cCollection)

IF( static )THEN
  CALL CheckStaticContinuousReleases( tstep,lev1 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== Release puffs (active cDefinition)

CALL ReleaseContinuousReleases( dts,lev1,lev2 )
IF( nError /= NO_ERROR )GOTO 9999

!==== Put new puffs on puff grid and initialize time levels

IF( mpuf < npuf )THEN

  CALL set_ip( mpuf+1,npuf )
  IF( nError /= NO_ERROR )GOTO 9999

  CALL add_tlev( mpuf+1,npuf )
  IF( nError /= NO_ERROR )GOTO 9999

END IF

!==== Deactivate releases (active cDefinition)

CALL DeactivateContinuousReleases( dts,tstep,lev1 )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  UpdateContinuousReleases
!*******************************************************************************
SUBROUTINE UpdateContinuousReleases( tstep )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL, INTENT( IN ) :: tstep      ! end time for current large timestep

INTEGER idef

DO idef = 1,numDefinition
  IF( (cDefinition(idef)%state == CR_ACTIVE) .AND. cDefinition(idef)%update )THEN

    CALL ProcessUpdateRelease( idef,tstep )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF
END DO

9999 CONTINUE

RETURN
END

!*******************************************************************************
!  UpdateContinuousReleasesExtra
!*******************************************************************************
SUBROUTINE UpdateContinuousReleasesExtra( time,tstep,lev )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,    INTENT( IN ) :: time       ! current time
REAL,    INTENT( IN ) :: tstep      ! end time for current large timestep
INTEGER, INTENT( IN ) :: lev        ! Curent time level

INTEGER idef, ilev

REAL updateTime

REAL, EXTERNAL :: GetNextUpdtTime

DO idef = 1,numDefinition
  IF( (cDefinition(idef)%state == CR_ACTIVE) .AND. cDefinition(idef)%extraUpdate )THEN
    updateTime = 3600.0*GetNextUpdtTime( cDefinition(idef) )
    ilev = cDefinition(idef)%rSet%tlev

    IF( time >= updateTime .AND. ilev >= lev )THEN
      CALL ProcessUpdateRelease( idef,tstep )
      IF( nError /= NO_ERROR )GOTO 9999
    END IF
  END IF
END DO

9999 CONTINUE

RETURN
END

!*******************************************************************************
!  ProcessUpdateRelease
!*******************************************************************************
SUBROUTINE ProcessUpdateRelease( idef,tstep )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER, INTENT( IN ) :: idef      ! definition index
REAL,    INTENT( IN ) :: tstep     ! end time for current large timestep

REAL, EXTERNAL :: GetNextUpdtTime

TYPE( releaseSpecT ) :: relSpec
TYPE( releaseSpecT ) oldSpec

TYPE( releaseSpecT ), EXTERNAL :: UpdateRelease

!==== Get updated release

CALL InitReleaseSpec( relSpec )
CALL InitReleaseSpec( oldSpec )

CALL copyReleaseSpec( cDefinition(idef)%relSpec,oldSpec )

relSpec = UpdateRelease( oldSpec,tstep )
IF( nError /= NO_ERROR )GOTO 9999

!==== Check the new release (mainly to call set_rel_type)

CALL valid_scn( relSpec )
IF( nError /= NO_ERROR )GOTO 9999

!==== Process the new release

CALL process_scn_cont( relSpec,CRMODE_UPDATE,defIN=idef )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set update cc flag

cCollection(cDefinition(idef)%cID)%update_cc = .NOT.cCollection(cDefinition(idef)%cID)%isPool

!==== reset extraUpdate flag

cDefinition(idef)%extraUpdate = cDefinition(idef)%update .AND. (GetNextUpdtTime(cDefinition(idef)) /= DEF_VAL_R)

9999 CONTINUE

RETURN
END

!*******************************************************************************
!  ActivateContinuousReleases
!*******************************************************************************
SUBROUTINE ActivateContinuousReleases( time,lev )

USE scipuff_fi
USE cont_rel_fi
USE cont_rel_functions
USE error_fi

IMPLICIT NONE

REAL,    INTENT( IN ) :: time
INTEGER, INTENT( IN ) :: lev

INTEGER idef, ilev

DO idef = 1,numDefinition
  IF( cDefinition(idef)%state /= CR_READY )CYCLE  !Skip active and empty(deactivated pools) definitions
  ilev = cDefinition(idef)%rSet%tlev
  IF( time > cDefinition(idef)%time .AND. ilev >= lev )THEN
    cDefinition(idef)%state = CR_ACTIVE
    cCollection(cDefinition(idef)%cID)%update_cc = .NOT.cDefinition(idef)%isPool
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  InteractContinuousReleases
!*******************************************************************************
SUBROUTINE InteractContinuousReleases( doAll )

USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: doAll

INTEGER icol, jcol
LOGICAL resetInitStatics

DO icol = 1,numCollection
  IF( .NOT.cCollection(icol)%isActive )CYCLE
  IF( initStatics .OR. cCollection(icol)%update_cc )THEN
    !Save initial value of initStatics since it can change in remove_static_puffs
    resetInitStatics = .NOT.initStatics

    CALL remove_static_puffs( cCollection(icol) )
    IF( nError /= NO_ERROR )GOTO 9999

    !remove_static_puffs for A Moving Release will reset initStatic true so need
    !to recheck all previous previous collections to see if not already removed
    !because of update_cc.
    IF( resetInitStatics .AND. initStatics .AND. icol > 1 )THEN
      DO jcol = 1,icol-1
        IF( (.NOT.cCollection(jcol)%isActive) .OR. cCollection(jcol)%update_cc )CYCLE
        CALL remove_static_puffs( cCollection(jcol) )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END IF
  END IF
  IF( cCollection(icol)%update_cc )THEN
    CALL InteractCollection( cCollection(icol),doAll )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  InteractCollection
!*******************************************************************************
SUBROUTINE InteractCollection( col,doAll )

USE error_fi
USE cont_rel_fi
USE scipuff_fi
USE inter_fi
USE default_fd
USE met_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_col ), INTENT( INOUT ) :: col
LOGICAL,                  INTENT( IN    ) :: doAll

INTEGER set_tlev
LOGICAL allActive
TYPE( cont_release_def ), POINTER :: def

!---- Initialize

NULLIFY( def )

allActive = .TRUE.

!-----  compute interaction terms

CALL InteractReleaseDefs( col,doAll,allActive )
IF( nError /= NO_ERROR )GOTO 9999

!---- reset time lev so all members of the collection use the same initial tlev

set_tlev = 0
def => col%firstDef
DO WHILE( ASSOCIATED(def) )
  IF( def%state == CR_ACTIVE .OR. doAll )THEN
    set_tlev = MAX( set_tlev,def%rSet%tlev )
  END IF
  def => def%nextDef
END DO
NULLIFY( def )

IF( static )THEN
  col%rStat%ilev = set_tlev
  col%rStat%tlev = set_tlev
END IF

IF( doAll )THEN
  col%update_cc = .NOT.allActive
ELSE
  col%update_cc = .FALSE.
END IF

9999 CONTINUE

NULLIFY( def )

RETURN
END
!*******************************************************************************
!  InteractReleaseDefs
!*******************************************************************************
SUBROUTINE InteractReleaseDefs( col,doAll,allActive )

USE error_fi
USE cont_rel_fi
USE scipuff_fi
USE inter_fi
USE default_fd
USE met_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_col ), INTENT( INOUT ) :: col
LOGICAL,                  INTENT( IN    ) :: doAll
LOGICAL,                  INTENT( OUT   ) :: allActive

INTEGER, PARAMETER :: ITER_MAX = 9999
REAL,    PARAMETER :: ITER_EPS = 0.0001

INTEGER irel, jrel, jrels
INTEGER colNrel, irelC, jrelC
INTEGER imat, jmat, ityp, jtyp, ilev, ifld, icls, ios, iter
INTEGER ilev_updt
REAL    del, faci, facj, facij, tem, um2, wb2, vel
REAL    xbar, ybar, zbar, sxx, syy, szz, del_tot
REAL    umovi, vmovi, wmovi
REAL    umovj, vmovj, wmovj
REAL    ub2, vb2
REAL    temu, temv, veln
LOGICAL lxxi, lxxj, lzzi, lzzj, lii, ldef

TYPE( cont_release_def ), POINTER :: defi, defj
TYPE( cont_release_set ), POINTER :: seti, setj
TYPE( puff_str ),         POINTER :: pufi, pufj

TYPE( puff_dynamics ) pdi, pdj
TYPE( puff_totalcc  ) pti, ptj
TYPE( puff_liquid   ) pqi, pqj

REAL, DIMENSION(:), ALLOCATABLE :: pbounds

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: getPuffifld
INTEGER, EXTERNAL :: GetNextUpdtTimeLevel
LOGICAL, EXTERNAL :: num_puff_types

!---- Initialize pointers

NULLIFY( defi )
NULLIFY( defj )
NULLIFY( seti )
NULLIFY( setj )
NULLIFY( pufi )
NULLIFY( pufj )

!-----  Clear interaction terms
!------ NB. Release puff interactions in 'cc' not 'ccb'

IF( multicomp )colNrel = 0
allActive = .TRUE.
defi => col%firstDef
DO WHILE( ASSOCIATED(defi) )
  allActive = allActive .AND. (defi%state == CR_ACTIVE)
  IF( defi%state == CR_ACTIVE .OR. doAll )THEN
    seti => defi%rSet
    IF( multicomp )colNrel = colNrel + seti%nrel
    DO irel = 1,seti%nrel

!---- Delete release puffs to force then to be regenerated from the updated basePuff

      IF( ASSOCIATED(seti%rels(irel)%relPuff) )THEN
        ios = deallocate_contRel_puff( seti%rels(irel) )
        IF( ios /= 0 )GOTO 9999
      END IF
      IF( ASSOCIATED(seti%rels(irel)%vapPuff) )THEN
        ios = deallocate_contRel_vapor( seti%rels(irel) )
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'InteractReleaseDefs'
          eMessage = 'Error deallocating scipuff vapor release puffs'
          WRITE(eInform,'(A,I0)')'IOS = ',ios
          GOTO 9999
        END IF
      END IF
      seti%rels(irel)%npuff = 0

      seti%rels(irel)%basePuff%cc = 0

      IF( dynamic )THEN
        CALL get_dynamics( seti%rels(irel)%basePuff,pdi )
        pdi%ucb = 0.0
        pdi%ucp = 0.0
        pdi%vcb = 0.0
        pdi%vcp = 0.0
        pdi%wcb = 0.0
        pdi%wcp = 0.0
        pdi%ctb = 0.0
        pdi%ctp = 0.0
        IF( buoy_gas )THEN
          pdi%bcb = 0.0
          pdi%bcp = 0.0
        END IF
        CALL put_dynamics( seti%rels(irel)%basePuff,pdi )
      END IF

      ityp = seti%rels(irel)%basePuff%ityp

      IF( typeID(ityp)%ltot )THEN
        CALL get_totalcc( seti%rels(irel)%basePuff,pti )
        pti%cct = 0.0
        CALL put_totalcc( seti%rels(irel)%basePuff,pti )
      END IF

      IF( IsLiquid(typeID(ityp)%icls) )THEN
        CALL get_liquid( seti%rels(irel)%basePuff,pqi )
        pqi%ccbg = 0.0
        CALL put_liquid( seti%rels(irel)%basePuff,pqi )
      END IF

      IF( IsMulti(typeID(ityp)%icls) )CALL ClearInterMC( seti%rels(irel)%basePuff )
      CALL step_clock()

    END DO
    NULLIFY( seti )
  END IF
  defi => defi%nextDef
END DO

NULLIFY( defi )

IF( multicomp )THEN
  CALL InitInterMC( 1,colNrel )
  IF( nError /= NO_ERROR )GOTO 9999
END IF
irelC = 0

defi => col%firstDef
DO WHILE( ASSOCIATED(defi) )
  IF( defi%state == CR_ACTIVE .OR. doAll )THEN

    IF( defi%extraUpdate )THEN
      ilev_updt = GetNextUpdtTimeLevel(defi%relSpec,t/3600.)
      IF( nError /= NO_ERROR )THEN
        eRoutine = 'InteractReleaseDefs:'//TRIM(eRoutine)
        GOTO 9999
      END IF
    ELSE
      ilev_updt = 0
    END IF

    umovi = defi%vel%x
    vmovi = defi%vel%y
    wmovi = defi%vel%z

    seti => defi%rSet
    DO irel = 1,seti%nrel

      pufi => seti%rels(irel)%basePuff

      irelC = irelC + 1

      xbar = SNGL(pufi%xbar)
      ybar = SNGL(pufi%ybar)
      zbar = pufi%zbar

      CALL mapfac( xbar,ybar,xmap_i,ymap_i )

!------ Get time level for zero-mass release (ignoring any dynamics) and skip out

      IF( pufi%syy <= 0. .OR. pufi%c == 0. )THEN
        IF( dynamic )CALL get_dynamics( seti%rels(irel)%basePuff,pdi )
        seti%rels(irel)%basePuff%axx = 0.0   !Flag for init_tlev
        CALL init_tlev( seti%rels(irel)%basePuff,pdi,ilev,xmap_i,ymap_i )
        IF( nError /= NO_ERROR )GOTO 9999
        pufi%idtl = ilev
        seti%tlev = MAX(seti%tlev,ilev)
        mxtlev    = MAX(mxtlev,ilev)
        CYCLE
      END IF

      ifld = getPuffifld( seti%rels(irel)%basePuff )
      ityp = pufi%ityp

      szz = pufi%szz
      CALL get_met( xbar,ybar,zbar,szz,0.0,0,inField=ifld )

      imat = typeID(ityp)%imat
      icls = typeID(ityp)%icls
      ltot = typeID(ityp)%ltot
      lmc  = IsMulti(icls)
      lliqi = IsLiquid(icls) .AND. (typeID(ityp)%igrp > 1)
      ldyni = defi%isDynamic .AND. IsGas(icls)

      IF( lter )THEN
        CALL get_topogIn( xbar,ybar,h,hx,hy,ifld )
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        h = 0.
      END IF

      IF( dynamic )CALL get_dynamics( seti%rels(irel)%basePuff,pdi )
      IF( ltot    )CALL get_totalcc(  seti%rels(irel)%basePuff,pti )
      IF( lliqi   )CALL get_liquid( seti%rels(irel)%basePuff,pqi )
      IF( lmc     )CALL GetInterMC1(  seti%rels(irel)%basePuff )
      jrelC = irelC - seti%nrel
      defj => defi
      jrels = irel
      ldef = .TRUE.
      DO WHILE( ASSOCIATED(defj) )
        IF( (defj%state == CR_ACTIVE .OR. doAll) .AND. (defi%end > defj%time) )THEN

          umovj = defj%vel%x
          vmovj = defj%vel%y
          wmovj = defj%vel%z

          setj => defj%rSet
          DO jrel = jrels,setj%nrel
            pufj => setj%rels(jrel)%basePuff

            jrelC = jrelC + 1

            lii = ( MAX(t,seti%rels(irel)%time) >=  setj%rels(jrel)%time )

            jtyp  = pufj%ityp
            jmat  = typeID(jtyp)%imat
            icls  = typeID(jtyp)%icls
            ldynj = defj%isDynamic .AND. IsGas(icls)

            lmat = (imat == jmat) .AND. (ltot .OR. lmc)
            IF( lliqi )THEN
              ltyp = lmat .AND. (typeID(jtyp)%igrp > 1)
            ELSE
              ltyp = ityp == jtyp
            END IF
            lsame = ltyp .OR. lmat

            IF( ldyni .OR. ldynj )THEN
              CALL get_dynamics( setj%rels(jrel)%basePuff,pdj )
              CALL check_dynamic_src( ldynj,pdj,jtyp )
            END IF

            lprocess = lsame .OR. ldynj .OR. ldyni

            lstatic = ldef .AND. (irel == jrel)

            del = (SNGL(pufi%xbar - pufj%xbar)/xmap_i)**2 + &
                  (SNGL(pufi%ybar - pufj%ybar)/ymap_i)**2 + &
                   (zbar - pufj%zbar)**2

            del = SQRT(del)

            del_tot = (umovi - umovj)**2 + &
                      (vmovi - vmovj)**2 + &
                      (wmovi - wmovj)**2 + del

!------ compute interactions between co-located releases

            IF( lprocess )THEN

              IF( del_tot == 0.0 )THEN
                lxxi = (pufi%sxx == 0.)
                lxxj = (pufj%sxx == 0.)
                lprocess = (lxxi .EQV. lxxj)
                IF( lprocess )THEN
                  lzzi = (pufi%szz == 0.)
                  lzzj = (pufj%szz == 0.)
                  lprocess = (lzzi .EQV. lzzj)
                END IF
              ELSE
                lprocess = .FALSE.
              END IF

            END IF

!------ compute interactions if necessary

            IF( lprocess )THEN

              IF( lxxi )THEN
                fac = PI2*SQRT((pufi%syy+pufj%syy)*(pufi%szz+pufj%szz))
              ELSE IF( lzzi )THEN
                fac = PI2*SQRT((pufi%syy+pufj%syy)*(pufi%sxx+pufj%sxx))
              ELSE
                ub2 = (umovi - ub)**2
                vb2 = (vmovi - vb)**2
                sxx = pufi%sxx + pufj%sxx
                syy = pufi%syy + pufj%syy
                IF( ub2+vb2 > 1.E-6 )THEN
                  syy = syy*(ub2+vb2)/(ub2+vb2*syy/sxx)
                ELSE
                  syy = 0.5*(sxx+syy)
                END IF
                fac = PI2*SQRT(syy*(pufi%szz+pufj%szz))
              END IF

              fac   = 1.0/fac
              faci  = pufi%c*fac
              facj  = pufj%c*fac
              facij = faci*pufj%c

              IF( ltyp )THEN
                IF( lii )pufi%cc = pufi%cc + facij
                IF( .NOT.lstatic )THEN
                  pufj%cc = pufj%cc + facij
                ELSE IF( lliqi )THEN
                  pqi%ccs = pufi%c
                  IF( irel == jrel )THEN
                    pqi%ccbg = pqi%ccbg + facij
                  ELSE IF( ityp == jtyp )THEN
                    CALL get_liquid( seti%rels(jrel)%basePuff,pqj )
                    pqj%ccbg = pqj%ccbg + facij
                    CALL put_liquid( seti%rels(jrel)%basePuff,pqj )
                  END IF
                END IF
              END IF

              IF( ldynj .AND. lii )THEN
                pdi%ucp = pdi%ucp + pdj%un*faci
                pdi%vcp = pdi%vcp + pdj%vn*faci
                pdi%wcp = pdi%wcp + pdj%w*faci
                pdi%ctp = pdi%ctp + pdj%t*faci
                IF( buoy_gas )pdi%bcp = pdi%bcp + buoy_fac(jtyp)*pufj%c*faci
              END IF

              IF( ldyni .AND. .NOT.lstatic )THEN
                pdj%ucp = pdj%ucp + pdi%un*facj
                pdj%vcp = pdj%vcp + pdi%vn*facj
                pdj%wcp = pdj%wcp + pdi%w*facj
                pdj%ctp = pdj%ctp + pdi%t*facj
                IF( buoy_gas )pdj%bcp = pdj%bcp + buoy_fac(ityp)*pufi%c*facj
                IF( pufj%c > 0.0 )THEN
                  tem  = pdj%wcp/pufj%c
                  temu = pdj%ucp/pufj%c
                  temv = pdj%vcp/pufj%c
                ELSE
                  tem  = 0.0
                  temu = 0.0
                  temv = 0.0
                END IF
                um2  = (umovj-ub)**2 + (vmovj-vb)**2 + (wmovj-wb)**2
                IF( temu == 0.0 .AND. temv == 0.0 )THEN
                  wb2 = MAX(0.,0.5*(SQRT(um2*um2+4.*tem*tem) - um2))
                  vel = SQRT(um2+wb2)
                ELSE
                  veln = SQRT(MAX(ABS(tem),ABS(temu),ABS(temv),um2))
                  vel  = 0.0
                  iter = 0
                  DO WHILE( ABS(vel-veln) > ITER_EPS*vel )
                    vel  = 0.5*(vel+veln)
                    veln =  ( ((umovj-ub)*vel-temu)**2 + ((vmovj-vb)*vel-temv)**2 + ((wmovj-wb)*vel-tem)**2 )**0.25
                    iter = iter + 1
                    IF( iter > ITER_MAX )THEN
                      vel = 0.5*(vel+veln)
                      nRelInterIter = nRelInterIter + 1
                      EXIT
                    END IF
                  END DO
                END IF
                pdj%ucb = temu*pufj%c/vel
                pdj%vcb = temv*pufj%c/vel
                pdj%wcb = tem *pufj%c/vel
                CALL put_dynamics( setj%rels(jrel)%basePuff,pdj )
              END IF

              IF( lmat )THEN
                IF( ltot )THEN
                  CALL get_totalcc( setj%rels(jrel)%basePuff,ptj )
                  IF( lii )pti%cct = pti%cct + facij
                  IF( .NOT.lstatic )THEN
                    ptj%cct = ptj%cct + facij
                    CALL put_totalcc( setj%rels(jrel)%basePuff,ptj )
                  END IF
                END IF
                IF( lmc )THEN
                  CALL GetInterMC2( setj%rels(jrel)%basePuff )
                  CALL InterMC( irelC,jrelC,seti%rels(irel)%basePuff,setj%rels(jrel)%basePuff )
                END IF
              END IF

            END IF

            CALL step_clock()

            NULLIFY( pufj )
          END DO
          NULLIFY( setj )
        END IF
        defj => defj%nextDef
        jrels = 1
        ldef  = .FALSE.
      END DO
      NULLIFY( defj )

      IF( dynamic )THEN
        IF( pufi%c > 0.0 )THEN
          tem  = pdi%wcp/pufi%c
          temu = pdi%ucp/pufi%c
          temv = pdi%vcp/pufi%c
        ELSE
          tem  = 0.0
          temu = 0.0
          temv = 0.0
        END IF
        um2  = (umovi-ub)**2 + (vmovi-vb)**2 + (wmovi-wb)**2
        IF( temu == 0.0 .AND. temv == 0.0 )THEN
          wb2 = MAX(0.,0.5*(SQRT(um2*um2+4.*tem*tem) - um2))
          vel = SQRT(um2+wb2)
        ELSE
          veln = SQRT(MAX(ABS(tem),ABS(temu),ABS(temv),um2))
          vel  = 0.0
          iter = 0
          DO WHILE( ABS(vel-veln) > ITER_EPS*vel )
            vel  = 0.5*(vel+veln)
            veln =  ( ((umovi-ub)*vel-temu)**2 + ((vmovi-vb)*vel-temv)**2 + ((wmovi-wb)*vel-tem)**2 )**0.25
            iter = iter + 1
            IF( iter > ITER_MAX )THEN
              vel = 0.5*(vel+veln)
              nRelInterIter = nRelInterIter + 1
              EXIT
            END IF
          END DO
        END IF
        IF( vel > SMALL )THEN
          pdi%ucb = temu*pufi%c/vel
          pdi%vcb = temv*pufi%c/vel
          pdi%wcb = tem *pufi%c/vel
        END IF
        CALL put_dynamics( seti%rels(irel)%basePuff,pdi )
      END IF

      IF( ltot )CALL put_totalcc( seti%rels(irel)%basePuff,pti )

      IF( lliqi )CALL put_liquid( seti%rels(irel)%basePuff,pqi )

      IF( lmc )THEN
        IF( lxxi )THEN
          fac  = PI2*SQRT(pufi%syy*pufi%szz)
        ELSE IF( lzzi )THEN
          fac  = PI2*SQRT(pufi%syy*pufi%sxx)
        ELSE
          ub2 = (umovi - ub)**2
          vb2 = (vmovi - vb)**2
          sxx = pufi%sxx
          syy = pufi%syy
          IF( ub2+vb2 > 1.E-6 )THEN
            syy = syy*(ub2+vb2)/(ub2+vb2*syy/sxx)
          ELSE
            syy = 0.5*(sxx+syy)
          END IF
          fac  = PI2*SQRT(syy*pufi%szz)
        END IF
        CALL InitContMC1( seti%rels(irel)%basePuff )
        CALL ResetInterMC( irelC,seti%rels(irel)%basePuff )
        IF( nError /= NO_ERROR )GOTO 9999
      END IF

      IF( seti%rels(irel)%basePuff%c > 0.0 )THEN
        seti%rels(irel)%basePuff%axx = 0.0   !Flag for init_tlev
        IF( dynamic )THEN
          pdi%ucp = pdi%ucb                    !Reset dynamics so that init_tlev gets correct values
          pdi%vcp = pdi%vcb                    !Release puffs have wrong dimensions for init_tlev
          pdi%wcp = pdi%wcb                    !ucb contains velocity * <C>, etc. DO NOT STORE WITH PUT_DYNAMICS
        END IF
        CALL init_tlev( seti%rels(irel)%basePuff,pdi,ilev,xmap_i,ymap_i )
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        ilev = 1
      END IF

      ilev = MAX(ilev,ilev_updt)
      pufi%idtl = ilev
      seti%tlev = MAX(seti%tlev,ilev)
      mxtlev    = MAX(mxtlev,ilev)

      CALL step_clock()

      NULLIFY( pufi )
    END DO
    NULLIFY( seti )
  END IF
  defi => defi%nextDef
END DO
NULLIFY( defi )

9999 CONTINUE

NULLIFY( pufi )
NULLIFY( pufj )
NULLIFY( seti )
NULLIFY( setj )
NULLIFY( defi )
NULLIFY( defj )

IF( multicomp )CALL ExitInterMC( )

RETURN
END
!*******************************************************************************
!  check_dynamic_src
!*******************************************************************************
SUBROUTINE check_dynamic_src( ldyn,pd,ityp )

USE scipuff_fi

IMPLICIT NONE

LOGICAL,               INTENT( INOUT ) :: ldyn
TYPE( puff_dynamics ), INTENT( IN )    :: pd
INTEGER,               INTENT( IN )    :: ityp

IF( ldyn )THEN
  ldyn = (pd%w /= 0.) .OR. (pd%un /= 0.) .OR. (pd%vn /= 0.) .OR. &
         (pd%t /= 0.) .OR. (buoy_fac(ityp) /= 0.)
END IF

RETURN
END
!*******************************************************************************
!  InitContinuousReleases
!*******************************************************************************
SUBROUTINE InitContinuousReleases()

USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

INTEGER idef

DO idef = 1,numDefinition
  IF( cDefinition(idef)%state == CR_ACTIVE )THEN
    CALL InitContinuousRelease( cDefinition(idef)%rset,cDefinition(idef)%isPool )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  InitContinuousRelease
!*******************************************************************************
SUBROUTINE InitContinuousRelease( set,isPool )

USE error_fi
USE cont_rel_fi
USE cont_rel_functions
USE scipuff_fi

IMPLICIT NONE

TYPE( cont_release_set ), INTENT( INOUT ) :: set
LOGICAL,                  INTENT( IN    ) :: isPool

INTEGER irel, init

LOGICAL, EXTERNAL :: IsLiquid
REAL,    EXTERNAL :: computeSkew

init = 0
DO irel = 1,set%nrel
  IF( set%rels(irel)%npuff == 0 )THEN  !Just initialized or statics reset
    set%rels(irel)%isStatic = .FALSE.
    set%rels(irel)%npuff = 1
    IF( .NOT.isPool )THEN
      set%rels(irel)%fracUP = computeSkew( set%rels(irel)%basePuff )
      IF( set%rels(irel)%fracUP < 1.0 )set%rels(irel)%npuff = set%rels(irel)%npuff + 1
    END IF
    CALL allocate_contRel_puff( set%rels(irel) )
    IF( nError /= NO_ERROR )GOTO 9999
    CALL InitContinuousReleasePuff( set%rels(irel) )
    IF( nError /= NO_ERROR )GOTO 9999
    init = init + 1
  END IF
END DO

IF( init > 0 )THEN
  IF( init /= set%nrel )THEN
    nError   = IV_ERROR
    eRoutine = 'InitContinuousRelease'
    eMessage = 'Failed to fully initialize release puffs'
    WRITE(eInform,'(A,I0,A,I0)')'nrel=',set%nrel,' : nInit=',init
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  InitContinuousReleasePuff
!*******************************************************************************
SUBROUTINE InitContinuousReleasePuff( rel )

USE error_fi
USE cont_rel_fi
USE cont_rel_functions
USE scipuff_fi

IMPLICIT NONE

TYPE( cont_release_rel ), INTENT( INOUT ) :: rel

INTEGER ipuf

DO ipuf = 1,rel%npuff
  CALL copy_puff( rel%basePuff,rel%relPuff(ipuf) )
  IF( nError /= NO_ERROR )GOTO 9999
END DO

IF( rel%fracUP < 1.0 )THEN
  CALL scale_puff(   rel%relPuff(1),rel%fracUP )
  CALL setPuffiskew( rel%relPuff(1),SKEW_UP )
  CALL scale_puff(   rel%relPuff(2),(1.0-rel%fracUP) )
  CALL setPuffiskew( rel%relPuff(2),SKEW_DOWN )
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  computeSkew
!*******************************************************************************
REAL FUNCTION computeSkew( p ) RESULT( frac )

USE scipuff_fi
USE met_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

INTEGER ifld
REAL    shh

REAL,    EXTERNAL :: SkewFraction
INTEGER, EXTERNAL :: getPuffifld

ifld = getPuffifld( p )
shh  = 0.5*(p%sxx+p%syy)

CALL get_met( SNGL(p%xbar),SNGL(p%ybar),p%zbar,p%szz,p%zc,0,inField=ifld,Shh=shh )

frac = SkewFraction( p%zbar )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  SkewFraction
!*******************************************************************************
REAL FUNCTION SkewFraction( z ) RESULT( frac )

USE met_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: z

REAL skewness, m2, r

IF( z < zinv )THEN
  IF( 0.6*(ws2/(ws2+4.*us2))**1.5 > 0.05 )THEN
    skewness = 0.6*(ws2/(ws2+4.*us2))**1.5
    m2 = (TwoThirds * skewness**0.3333333)**2
    r = (1.0 + m2)**3 * skewness**2 / (m2 * (3.0+m2)**2)
    frac = 0.5*(1.0 - SQRT(r/(4.0+r)))
  ELSE
    frac = 1.0
  END IF
ELSE
  frac = 1.0
END IF

RETURN
END
!*******************************************************************************
!  SplitSkewPuff
!*******************************************************************************
SUBROUTINE SplitSkewPuff( p,metFlag )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
LOGICAL,          INTENT( IN    ) :: metFlag   ! TRUE indicates get_met already called

REAL fracUP

REAL, EXTERNAL :: computeSkew
REAL, EXTERNAL :: SkewFraction

IF( metFlag )THEN
  fracUP = SkewFraction( p%zbar )
ELSE
  fracUP = computeSkew( p )
END IF

IF( fracUP < 1.0 )THEN
  CALL check_newpuff()
  IF( nError /= NO_ERROR )GOTO 9999
  npuf = npuf + 1

  CALL copy_puff( p,puff(npuf) )

  CALL scale_puff(   p,fracUP )
  CALL setPuffiskew( p,SKEW_UP )

  CALL scale_puff(   puff(npuf),1.0-fracUP )
  CALL setPuffiskew( puff(npuf),SKEW_DOWN )
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  ReleaseContinuousReleases
!*******************************************************************************
SUBROUTINE ReleaseContinuousReleases( dts,lev1,lev2 )

USE error_fi
USE cont_rel_fi
USE cont_rel_functions
USE scipuff_fi

IMPLICIT NONE

REAL,    INTENT( IN    ) :: dts        ! current timestep
INTEGER, INTENT( IN    ) :: lev1       ! current time level
INTEGER, INTENT( INOUT ) :: lev2       ! max time level

INTEGER idef

!----- Create new puffs

DO idef = 1,numDefinition
  IF( cDefinition(idef)%state == CR_ACTIVE )THEN
    IF( cDefinition(idef)%isPool )THEN
      CALL ReleasePoolRelease( dts,lev1,lev2,cDefinition(idef) )
    ELSE
      CALL ReleaseContinuousRelease( dts,lev1,lev2,cDefinition(idef) )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  ReleaseContinuousRelease
!*******************************************************************************
SUBROUTINE ReleaseContinuousRelease( dts,lev1,lev2,def )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,                     INTENT( IN    ) :: dts        ! current timestep
INTEGER,                  INTENT( IN    ) :: lev1       ! current time level
INTEGER,                  INTENT( INOUT ) :: lev2       ! max time level
TYPE( cont_release_def ), INTENT( INOUT ) :: def        ! continuous release definition to step

INTEGER irel
LOGICAL stepped
REAL    rtime, xmap, ymap
REAL(8) dtm

INTEGER, EXTERNAL :: GetNextUpdtTimeLevel

rtime   = HUGE(rtime)

IF( def%rSet%nrel > 0 )THEN
  stepped = .FALSE.
  DO irel = 1,def%rSet%nrel
    CALL ReleaseContinuousPuff( dts,lev1,lev2,def,def%rSet%rels(irel),cCollection(def%cID)%rStat,stepped )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

  IF( stepped )THEN
    def%rSet%tlev = 0
    DO irel = 1,def%rSet%nrel
      def%rSet%tlev = MAX(def%rSet%tlev,def%rSet%rels(irel)%basePuff%idtl)
      rtime = MIN(rtime,def%rSet%rels(irel)%time)
    END DO
    IF( def%extraUpdate )THEN
      def%rSet%tlev = MAX(def%rSet%tlev,GetNextUpdtTimeLevel(def%relSpec,t/3600.))
      IF( nError /= NO_ERROR )THEN
        eRoutine = 'ReleaseContinuousRelease:'//TRIM(eRoutine)
        GOTO 9999
      END IF
      mxtlev = MAX(mxtlev,def%rSet%tlev)
    END IF
  END IF
END IF

!==== Reset definition time/location

IF( rtime < HUGE(rtime) )THEN
  IF( def%isMoving )THEN
    dtm = DBLE(rtime-def%time)
    CALL mapfac( SNGL(def%loc%x),SNGL(def%loc%y),xmap,ymap )
    def%loc%x = def%loc%x + dtm*DBLE(def%vel%x*xmap)
    def%loc%y = def%loc%y + dtm*DBLE(def%vel%y*ymap)
    def%loc%z = def%loc%z + dtm*DBLE(def%vel%z)
  END IF
  def%time = rtime
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  ReleaseContinuousPuff
!*******************************************************************************
SUBROUTINE ReleaseContinuousPuff( dts,lev1,lev2,def,rel,stat,stepped )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,                        INTENT( IN    ) :: dts        ! current timestep
INTEGER,                     INTENT( IN    ) :: lev1       ! current time level
INTEGER,                     INTENT( INOUT ) :: lev2       ! max time level
TYPE( cont_release_def ),    INTENT( INOUT ) :: def        ! continuous release definition
TYPE( cont_release_rel ),    INTENT( INOUT ) :: rel        ! continuous release to step
TYPE( cont_release_static ), INTENT( IN    ) :: stat       ! statics
LOGICAL,                     INTENT( INOUT ) :: stepped    !

INTEGER nstp, istp, ipuf, jpuf, idtl, lev
REAL    dt, dtr, xmap, ymap, mdx, mdy, mdz
LOGICAL isStatic
LOGICAL vapor

TYPE( puff_static ) ps

INTEGER, EXTERNAL :: deallocatePuffAux
INTEGER, EXTERNAL :: CreateContinuousPuff

IF( rel%basePuff%idtl < lev1 )GOTO 9999

IF( t+0.5*dts <= rel%time )GOTO 9999 !Time step has been adjusted

!==== Set time step for this release

dt = MIN(t+dts,def%end) - rel%time

nstp = INT(dt/def%dtr) + 1
dt   = dt/FLOAT(nstp)

stepped = .TRUE.

isStatic = stat%hasStatics .AND. rel%isStatic

vapor = ASSOCIATED( rel%vapPuff )

DO istp = 1,nstp

!==== Update moving release locations for the continuous definition

  IF( def%isMoving )THEN
    CALL mapfac( SNGL(rel%loc%x),SNGL(rel%loc%y),xmap,ymap )
    IF( def%vel%x /= 0.0 )rel%loc%x = rel%loc%x + DBLE(def%vel%x*dt*xmap)
    IF( def%vel%y /= 0.0 )rel%loc%y = rel%loc%y + DBLE(def%vel%y*dt*ymap)
    IF( def%vel%z /= 0.0 )rel%loc%z = DBLE(SNGL(rel%loc%z) + def%vel%z*dt)
  END IF

  dtr = FLOAT(nstp-istp)*dt

  idtl = 0

  DO ipuf = 1,rel%npuff

    IF( rel%relPuff(ipuf)%c > 0.0 )THEN
      lev = CreateContinuousPuff( def,rel%relPuff(ipuf),isStatic,.FALSE., &
                                  dt,dtr,lev1,lev2,rel%plen,(ipuf==1) )
      IF( nError /= NO_ERROR )GOTO 9999
      idtl = MAX(idtl,lev)
    ELSE
      idtl = MAX(idtl,rel%relPuff(ipuf)%idtl)
    END IF

!==== Update moving release locations for the relPuff

    IF( def%isMoving )THEN
      IF( def%vel%x /= 0.0 )rel%relPuff(ipuf)%xbar = &
        (rel%relPuff(ipuf)%xbar - rel%basePuff%xbar) + rel%loc%x
      IF( def%vel%y /= 0.0 )rel%relPuff(ipuf)%ybar = &
        (rel%relPuff(ipuf)%ybar - rel%basePuff%ybar) + rel%loc%y
      IF( def%vel%z /= 0.0 )rel%relPuff(ipuf)%zbar = &
        (rel%relPuff(ipuf)%zbar - rel%basePuff%zbar) + SNGL(rel%loc%z)
    END IF

  END DO

  CALL check_progress()

  IF( vapor )THEN
    DO ipuf = 1,rel%npuff

      IF( rel%vapPuff(ipuf)%c > 0.0 )THEN
        lev = CreateContinuousPuff( def,rel%vapPuff(ipuf),isStatic,.FALSE., &
                                    dt,dtr,lev1,lev2,rel%plen,.FALSE. )
        IF( nError /= NO_ERROR )GOTO 9999
        idtl = MAX(idtl,lev)
      END IF

!==== Update moving release locations for the vapPuff

      IF( def%isMoving )THEN
        IF( def%vel%x /= 0.0 )rel%vapPuff(ipuf)%xbar = &
          (rel%vapPuff(ipuf)%xbar - rel%basePuff%xbar) + rel%loc%x
        IF( def%vel%y /= 0.0 )rel%vapPuff(ipuf)%ybar = &
          (rel%vapPuff(ipuf)%ybar - rel%basePuff%ybar) + rel%loc%y
        IF( def%vel%z /= 0.0 )rel%vapPuff(ipuf)%zbar = &
          (rel%vapPuff(ipuf)%zbar - rel%basePuff%zbar) + SNGL(rel%loc%z)
      END IF

    END DO
  END IF

  CALL check_progress()

!==== Update moving release locations for the basePuff

  IF( def%isMoving )THEN
    IF( def%vel%x /= 0.0 )rel%basePuff%xbar = rel%loc%x
    IF( def%vel%y /= 0.0 )rel%basePuff%ybar = rel%loc%y
    IF( def%vel%z /= 0.0 )rel%basePuff%zbar = SNGL(rel%loc%z)
  END IF

!==== Reset time levels if necessary

  IF( isStatic )THEN
    CALL adjustStaticRelease( def,rel,vapor,idtl )
  ELSE
    IF( idtl /= rel%basePuff%idtl )THEN
      rel%basePuff%idtl = idtl
      DO ipuf = 1,rel%npuff
        rel%relPuff(ipuf)%idtl = rel%basePuff%idtl
      END DO
    END IF
  END IF

END DO

!---------- Move static puffs, if necessary

IF( isStatic .AND. def%isMoving )THEN
  dt = dt*FLOAT(nstp)
  mdx = def%vel%x*dt
  mdy = def%vel%y*dt
  mdz = def%vel%z*dt
  DO ipuf = 1,rel%npuff
    CALL mapfac( SNGL(rel%relPuff(ipuf)%xbar),SNGL(rel%relPuff(ipuf)%ybar),xmap,ymap )
    jpuf = rel%relPuff(ipuf)%inxt
    DO WHILE( jpuf > 0 )
      puff(jpuf)%xbar = puff(jpuf)%xbar + DBLE(mdx*xmap)
      puff(jpuf)%ybar = puff(jpuf)%ybar + DBLE(mdy*ymap)
      puff(jpuf)%zbar = puff(jpuf)%zbar + mdz
      CALL get_static( puff(jpuf),ps )
      jpuf = ps%isnxt
    END DO
  END DO
END IF

!---------- Update release definition time

rel%time = MIN(t+dts,def%end)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  adjustStaticRelease
!*******************************************************************************
SUBROUTINE adjustStaticRelease( def,rel,vapor,idtl )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

TYPE( cont_release_def ),    INTENT( INOUT ) :: def        ! continuous release definition
TYPE( cont_release_rel ),    INTENT( INOUT ) :: rel        ! continuous release to step
LOGICAL,                     INTENT( IN    ) :: vapor      ! vapor flag
INTEGER,                     INTENT( IN    ) :: idtl       ! new time level

INTEGER ipuf

IF( idtl /= rel%basePuff%idtl )THEN
  rel%basePuff%idtl = idtl
  DO ipuf = 1,rel%npuff
    CALL adjustStaticPuff( rel%relPuff(ipuf),idtl )
    IF( vapor )CALL adjustStaticPuff( rel%vapPuff(ipuf),idtl )
  END DO
END IF
DO ipuf = 1,rel%npuff
  rel%relPuff(ipuf)%uo = def%vel%x
  rel%relPuff(ipuf)%vo = def%vel%y
  rel%relPuff(ipuf)%wo = def%vel%z
  IF( vapor )THEN
    rel%vapPuff(ipuf)%uo = def%vel%x
    rel%vapPuff(ipuf)%vo = def%vel%y
    rel%vapPuff(ipuf)%wo = def%vel%z
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  adjustStaticPuff
!*******************************************************************************
SUBROUTINE adjustStaticPuff( prel,idtl )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: prel       ! continuous release to step
INTEGER,          INTENT( IN    ) :: idtl       ! new time level

REAL ux, vx, wx, sxx, sxy, sxz, syy, syz, szz, dtc

LOGICAL, EXTERNAL :: PuffRealizable

IF( prel%idtl /= idtl )THEN

!----- Re-adjust static puffs if time level changed

  dtc = (0.5*delt)**2*(0.25**idtl - 0.25**prel%idtl)

  ux = prel%uo
  vx = prel%vo
  wx = prel%wo

  prel%sxx = prel%sxx + ux*ux*dtc
  prel%sxy = prel%sxy + ux*vx*dtc
  prel%sxz = prel%sxz + ux*wx*dtc
  prel%syy = prel%syy + vx*vx*dtc
  prel%syz = prel%syz + vx*wx*dtc
  prel%szz = prel%szz + wx*wx*dtc

  sxx = prel%sxx
  sxy = prel%sxy
  sxz = prel%sxz
  syy = prel%syy
  syz = prel%syz
  szz = prel%szz
  IF( .NOT.PuffRealizable(sxx,sxy,sxz,syy,syz,szz) )THEN
    prel%sxx = prel%sxx - ux*ux*dtc
    prel%sxy = prel%sxy - ux*vx*dtc
    prel%sxz = prel%sxz - ux*wx*dtc
    prel%syy = prel%syy - vx*vx*dtc
    prel%syz = prel%syz - vx*wx*dtc
    prel%szz = prel%szz - wx*wx*dtc
  END IF

  prel%idtl = idtl
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  ReleasePoolRelease
!*******************************************************************************
SUBROUTINE ReleasePoolRelease( dts,lev1,lev2,def )

!DIR$ OPTIMIZE:0
USE scipuff_fi
USE error_fi
USE cont_rel_fi

IMPLICIT NONE

REAL,                     INTENT( IN    ) :: dts        ! current timestep
INTEGER,                  INTENT( IN    ) :: lev1       ! current time level
INTEGER,                  INTENT( INOUT ) :: lev2       ! max time level
TYPE( cont_release_def ), INTENT( INOUT ) :: def        ! continuous release definition to step

INTEGER irel
LOGICAL stepped
REAL    rtime

!==== Loop over releases creating new puffs

rtime = HUGE(rtime)

IF( def%rSet%nrel > 0 )THEN
  stepped = .FALSE.
  DO irel = 1,def%rSet%nrel
    CALL ReleasePoolPuff( dts,lev1,lev2,def,def%rSet%rels(irel),stepped )
    IF( nError /= NO_ERROR )GOTO 9999
  END DO

  IF( stepped )THEN
    def%rSet%tlev = 0
    DO irel = 1,def%rSet%nrel
      def%rSet%tlev = MAX(def%rSet%tlev,def%rSet%rels(irel)%basePuff%idtl)
      rtime = MIN(rtime,def%rSet%rels(irel)%time)
    END DO
  END IF
END IF

!==== Reset definition time

IF( rtime < HUGE(rtime) )def%time = rtime

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  ReleasePoolPuff
!*******************************************************************************
SUBROUTINE ReleasePoolPuff( dts,lev1,lev2,def,rel,stepped )

USE scipuff_fi
USE error_fi
USE cont_rel_fi
USE cont_rel_functions
USE pool_fi, ONLY: mass_remaining

IMPLICIT NONE

REAL,                     INTENT( IN    ) :: dts        ! current timestep
INTEGER,                  INTENT( IN    ) :: lev1       ! current time level
INTEGER,                  INTENT( INOUT ) :: lev2       ! max time level
TYPE( cont_release_def ), INTENT( INOUT ) :: def        ! continuous release definition
TYPE( cont_release_rel ), INTENT( INOUT ) :: rel        ! continuous release to step
LOGICAL,                  INTENT( INOUT ) :: stepped

INTEGER ipuf, idtl, lev
REAL    dt

INTEGER, EXTERNAL :: CreateContinuousPuff

IF( rel%basePuff%idtl < lev1 )GOTO 9999

IF( rel%naux < SCIPUFF_POOLSRCPARAM )GOTO 9999 !Pool has completed

IF( rel%npuff > 1 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReleaseContinuousRelease'
  eMessage = 'Encountered release specification multiple puffs defined'
  eInform  = 'Pool releases not currently designed to handle multiple puffs'
  GOTO 9999
END IF

!==== Set time step for this release

dt = t + dts - rel%time

IF( dt <= 0 )GOTO 9999 !Time step has been adjusted

stepped = .TRUE.

!==== Get pool data needed in CreatePoolPuff called from CreateContinuousPuff
  CALL GetPoolAux( rel )

IF( nError /= NO_ERROR )GOTO 9999

idtl = 0

DO ipuf = 1,rel%npuff

  lev = CreateContinuousPuff( def,rel%relPuff(ipuf),.FALSE.,.FALSE., &
                              dt,0.0,lev1,lev2,rel%plen,(ipuf==1) )
  IF( nError /= NO_ERROR )GOTO 9999
  idtl = MAX(idtl,lev)

  CALL check_progress()

END DO

!==== Save pool data modified in CreatePoolPuff called from CreateContinuousPuff

  CALL PutPoolAux( rel )
IF( nError /= NO_ERROR )GOTO 9999

!==== Reset time levels if necessary

IF( idtl /= rel%basePuff%idtl )THEN
  rel%basePuff%idtl = idtl
  DO ipuf = 1,rel%npuff
    rel%relPuff(ipuf)%idtl = rel%basePuff%idtl
  END DO
END IF

rel%time = t + dts

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  CreateContinuousPuff
!*******************************************************************************
INTEGER FUNCTION CreateContinuousPuff( def,prel,isStatic,initStatic,dt,dtr,lev1,lev2,plen,setPlen ) RESULT( idtl )

USE scipuff_fi
USE met_fi
USE cont_rel_fi
USE pool_fi, ONLY: pool_offset

!---  Create the new puff from a continuous release

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( IN    ) :: def        !definition
TYPE( puff_str),          INTENT( INOUT ) :: prel       !release puff
LOGICAL,                  INTENT( IN    ) :: isStatic   !Static flag
LOGICAL,                  INTENT( IN    ) :: initStatic !initializing Static plume
REAL,                     INTENT( IN    ) :: dt         !release time step
REAL,                     INTENT( IN    ) :: dtr        !partial step offset for multiple release steps
INTEGER,                  INTENT( IN    ) :: lev1       !time level for stepping ( minimal step for new puff )
INTEGER,                  INTENT( INOUT ) :: lev2       !max time level so far
REAL,                     INTENT( INOUT ) :: plen       !plume length
LOGICAL,                  INTENT( IN    ) :: setPlen    !compute plume length flag (only on first of the npuff release puffs)

INTEGER ios

INTEGER, EXTERNAL :: NewContinuousPuff
INTEGER, EXTERNAL :: deallocatePuffAux

CALL check_newpuff()
IF( nError /= NO_ERROR )THEN
  eRoutine = 'CreateContinuousPuff'
  GOTO 9999
END IF

npuf = npuf + 1

idtl = NewContinuousPuff( def,prel,puff(npuf),def%isPool,isStatic,initStatic,  &
                          dt,dtr,lev1,lev2,plen,setPlen )
IF( nError /= NO_ERROR )GOTO 9999
IF( idtl < 0 )THEN                      !Puff too small - ignore
  ios = deallocatePuffAux( puff(npuf) )
  npuf = npuf - 1
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  NewContinuousPuff
!*******************************************************************************
INTEGER FUNCTION NewContinuousPuff( def,prel,pnew,isPool,isStatic,initStatic,dt,dtr, &
                                    lev1,lev2,plen,setPlen ) RESULT( idtl )

USE scipuff_fi
USE met_fi
USE cont_rel_fi
USE pool_fi, ONLY: pool_offset
USE files_fi

!---  Create the new puff from a continuous release

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( IN    ) :: def        !release definition
TYPE( puff_str),          INTENT( INOUT ) :: prel       !release puff
TYPE( puff_str),          INTENT( INOUT ) :: pnew       !puff to set
LOGICAL,                  INTENT( IN    ) :: isPool     !Pool flag
LOGICAL,                  INTENT( IN    ) :: isStatic   !Static flag
LOGICAL,                  INTENT( IN    ) :: initStatic !initializing Static plume
REAL,                     INTENT( IN    ) :: dt         !release time step
REAL,                     INTENT( IN    ) :: dtr        !partial step offset for multiple release steps
INTEGER,                  INTENT( IN    ) :: lev1       !time level for stepping ( minimal step for new puff )
INTEGER,                  INTENT( INOUT ) :: lev2       !max time level so far
REAL,                     INTENT( INOUT ) :: plen       !plume length
LOGICAL,                  INTENT( IN    ) :: setPlen    !compute plume length flag (only on first of the npuff release puffs)

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc  ) pt
TYPE( puff_material ) pmatl
TYPE( puff_liquid   ) pq
TYPE( puff_aerosol  ) pa

LOGICAL ltot
INTEGER ilev, ifld
INTEGER icls
REAL    xmap, ymap, velp, sly, ut, vt, wt, wcb
REAL    rat1, rat2, rat3
REAL    ucb, vcb
REAL    uubt, vvbt, fac, uvbt
REAL    vel2, xbar, ybar, zbar, h, hx, hy, tau, sz, syy, sxx
REAL    zmet, defu, defv, defw
REAL    bdyn, slp, wp2, vp2, qqvrtx, ri, qqp, difp, gt0
REAL    rhop

LOGICAL, EXTERNAL :: PuffRealizable, dense_effect
LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: getPuffipgrd, getPuffifld
INTEGER, EXTERNAL :: deallocatePuffAux
REAL,    EXTERNAL :: SWIMgetGamma
REAL,    EXTERNAL :: SkewFraction

!----- Initialize return value

idtl = -1

IF( prel%c <= 0.0 )GOTO 9999

!-----  Copy release puff staticPuff into new puff (NPUF)

CALL copy_puff( prel,pnew )
IF( nError /= NO_ERROR )GOTO 9999

icls = typeID(pnew%ityp)%icls

!-----  Store new puff location in locals for easy access

xbar = pnew%xbar
ybar = pnew%ybar
zbar = pnew%zbar

CALL mapfac( xbar,ybar,xmap,ymap )

!-----  Get auxiliary variables before puff mass is reset

ltot = typeID(pnew%ityp)%ltot
IF( ltot )CALL get_totalcc( pnew,pt )

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )CALL get_liquid( pnew,pq )

IF( IsAerosol(icls) )CALL get_aerosol( pnew,pa )

IF( dynamic )CALL get_dynamics( pnew,pd )

!-----  Get meteorology at release location

ifld = getPuffifld( pnew )

CALL get_met( xbar,ybar,zbar,pnew%szz,0.0,0,inField=ifld )

!----- Get local terrain height

IF( lter )THEN
  CALL get_topogIn( xbar,ybar,h,hx,hy,ifld )
  IF( nError /= NO_ERROR )GOTO 9999
ELSE
  h = 0.
END IF

IF( isPool )THEN

!-----  Set initial values for liquid pool source
    CALL CreatePoolPuff( pnew,dt,syy,rhop )
  IF( nError /= NO_ERROR )GOTO 9999

  IF( SkewFraction(pnew%zbar) < 1.0 )CALL setPuffiskew( pnew,SKEW_UP )

  fac = 4.*PI*SQRT(syy*pnew%szz)
  fac = (pnew%c**2)/fac

  ucb    = 0
  pd%ucp = 0.0
  pd%un  = 0.0
  vcb    = 0.0
  pd%vcp = 0.0
  pd%vn  = 0.0
  wcb    = 0.0
  pd%wcp = 0.0
  pd%w   = 0.0
  pd%t   = pool_offset*pnew%c/rhop
  pd%ctp = rhop*pd%t
  IF( buoy_gas )pd%bcp = buoy_fac(pnew%ityp)*fac

  pnew%cc = fac
  pt%cct  = fac

ELSE

!-----  Save vertical velocity in wcb

  IF( dynamic )THEN
    IF( .NOT.dense_effect( zbar-h,pnew%sv,pd%wcp,pnew%c ) )THEN
      IF( isStatic )THEN
        wcb = pd%wcp/pnew%c
      ELSE
        wcb = pd%wcb/pnew%c
      END IF
    ELSE
      wcb = 0.0
    END IF
    IF( isStatic )THEN
      ucb = pd%ucp/pnew%c
      vcb = pd%vcp/pnew%c
    ELSE
      ucb = pd%ucb/pnew%c
      vcb = pd%vcb/pnew%c
    END IF
  ELSE
    ucb = 0.0
    vcb = 0.0
    wcb = 0.0
  END IF

!----- Adjust initial szz for diffusive growth

  zmet = MAX(zbar-h,SQRT(pnew%szz))
  IF( zmet > 0. )THEN
    dddz = difb/zmet
  ELSE
    dddz = 0.0
  END IF
  sz = (SQRT(wwbl)+dddz)*dt
  IF( zmet > sz )THEN
    tau = sbz/(A*SQRT(MAX(qqs,1.E-6)))
    IF( dt/tau >= 0.00025 )THEN
      pnew%szz = pnew%szz + 2.*difb*(dt-tau*(1.-EXP(-dt/tau)))
    END IF
  ELSE
    pnew%szz = (SQRT(pnew%szz) + dddz*dt)**2
  END IF

!----- Limit initial szz if inside capped BL

  IF( dtdzs <= 0. .AND. zbar <= zinv )THEN
    IF( SWIMgetGamma(ifld,xbar,ybar,zinv) > 0.0 )THEN
      sz = 0.8*(zinv-h)
      IF( pnew%szz > sz**2 )THEN
        sz = sz/SQRT(puff(npuf)%szz)           !Maintain realizability
        pnew%szz = pnew%szz*sz**2
        pnew%sxz = pnew%sxz*sz
        pnew%syz = pnew%syz*sz
      END IF
    END IF
  END IF

END IF

!-----  Scale all puff mass variables

CALL scale_psum( pnew,dt )

IF( pnew%c <= 0.0 )GOTO 9999

IF( lsv_oper )CALL reset_lsv( pnew%si )

!------ scale turbulence for time-averaging

IF( t_avg /= DEF_VAL_R )THEN
  velp = ub*ub + vb*vb
  CALL turb_rescale( pnew%si,pnew%sv,velp )
END IF

!-----  Set puff location and sigma's

CALL set_turb( uubt,vvbt,uvbt )

!----- Total release velocity

defu = def%vel%x
defv = def%vel%y
defw = def%vel%z
ut = -ub + defu - ucb
vt = -vb + defv - vcb
wt = -wb + defw - wcb

velp = SQRT(ut*ut+vt*vt+uubt+vvbt+wt*wt)
IF( velp == 0.0 )THEN
  nError   = IV_ERROR
  eRoutine = 'NewContinuousPuff'
  eMessage = 'Continuous release with zero velocity'
  eInform  = 'Need a non-zero mean wind or dynamic source momentum'
  GOTO 9999
END IF

IF( setPlen )THEN
!------ set composite horizontal scale for limit to plen

  rat1 = 2.*uubl/(uubt+vvbt+SMALL)
  rat2 = 2.*vvbl/(uubt+vvbt+SMALL)
  rat3 = (uub+vvb)/(uubt+vvbt+SMALL)
  IF( rat1+rat2+rat3 > SMALL )THEN
    sly = rat1*sbls + rat2*sbl + rat3*sby
  ELSE ! for bl_type = none
    sly = MAX(sbls,sbl,sby)
  END IF

  plen = MIN(sly,plen+0.5*velp*dt)
END IF

!----- Initial location

pnew%xbar = pnew%xbar + DBLE(ub*dtr*xmap)
pnew%ybar = pnew%ybar + DBLE(vb*dtr*ymap)
pnew%zbar = pnew%zbar + wb*dtr

IF( .NOT.isStatic )THEN

  IF( pnew%sxx == 0.0 )THEN
    syy  = pnew%syy
    sxx  = MIN(syy,pnew%szz)
    vel2 = ut*ut + vt*vt
    IF( vel2 > SMALL )THEN
      pnew%sxx = (ut*ut*sxx + vt*vt*syy)/vel2
      pnew%syy = (ut*ut*syy + vt*vt*sxx)/vel2
      pnew%sxy =  ut*vt*(sxx-syy)/vel2
    ELSE
      pnew%sxx = sxx
    END IF
  ELSE
    sxx = (ut*ut+uubt)*pnew%sxx + &
          (vt*vt+vvbt)*pnew%syy + &
                 ut*vt*pnew%sxy + &
                 wt*wt*pnew%szz
    sxx = sxx/(velp*velp)
  END IF

!---- Use streamwise puff size to limit velp for light winds

  IF( .NOT.initStatic )velp = MAX(velp,2.*SQRT(sxx)/dt)
  ut = 0.5*ut*dt
  vt = 0.5*vt*dt
  wt = 0.5*wt*dt


  pnew%sxx = ut*ut + pnew%sxx
  pnew%syy = vt*vt + pnew%syy
  pnew%sxy = ut*vt + pnew%sxy
  pnew%szz = wt*wt + pnew%szz
  pnew%sxz = ut*wt + pnew%sxz
  pnew%syz = vt*wt + pnew%syz

END IF

IF( pnew%szz == 0.0 )THEN
  nError   = IV_ERROR
  eRoutine = 'NewContinuousPuff'
  eMessage = 'Continuous area release with zero vertical velocity'
  eInform  = 'Need vertical wind or dynamic source momentum'
  GOTO 9999
END IF

!------ Set initial variance to zero

IF( .NOT.isStatic )THEN
  pnew%ccb = pnew%cc/velp
  pnew%cc  = pnew%ccb
END IF

pnew%si2 = MAX(plen,pnew%si)

!------ Save initial background velocity

pnew%uo = ub
pnew%vo = vb
pnew%wo = wb

!-----  Set auxiliary variables

IF( IsLiquid(icls) .OR. IsWetParticle(icls) )THEN
  pq%tevap = 0.0
  pq%t     = tb
  pq%ccs   = pnew%c
  CALL put_liquid( puff(npuf),pq )
END IF

IF( ltot )THEN
  IF( isStatic )THEN
    pt%cctb = pt%cctb*dt
    pt%cct  = pt%cct *dt
  ELSE
    pt%cctb = pt%cct*dt/velp
    pt%cct  = pt%cctb
  END IF
  CALL put_totalcc( pnew,pt )
END IF

IF( dynamic )THEN
  pd%un = pd%un * dt
  pd%vn = pd%vn * dt
  pd%w = pd%w * dt
  pd%t = pd%t * dt
  IF( isStatic )THEN
    pd%ucp = pd%ucp * dt
    pd%vcp = pd%vcp * dt
    pd%wcp = pd%wcp * dt
    pd%ctp = pd%ctp * dt
    pd%ucb = pd%ucb * dt
    pd%vcb = pd%vcb * dt
    pd%wcb = pd%wcb * dt
    pd%ctb = pd%ctb * dt
    IF( buoy_gas )THEN
      pd%bcp = pd%bcp * dt
      pd%bcb = pd%bcb * dt
    END IF
  ELSE
    pd%ucb = pd%ucp*dt/velp
    pd%ucp = pd%ucb
    pd%vcb = pd%vcp*dt/velp
    pd%vcp = pd%vcb
    pd%wcb = pd%wcp*dt/velp
    pd%wcp = pd%wcb
    pd%ctb = pd%ctp*dt/velp
    pd%ctp = pd%ctb
    IF( buoy_gas )THEN
      pd%bcb = pd%bcp*dt/velp
      pd%bcp = pd%bcb
    END IF
  END IF
  CALL put_dynamics( pnew,pd )
END IF

IF( IsAerosol(icls) )THEN
  CALL get_puff_material( pnew%ityp,pmatl )
  CALL set_aerosol_cont( pnew,pd,pa,pmatl )
  IF( nError /= NO_ERROR )GOTO 9999
END IF
IF( IsLiquid(icls) )THEN
  CALL get_liquid( pnew,pq )
  IF( isStatic )THEN
    pq%ccbg = pq%ccbg*dt
  ELSE
    pq%ccbg = pq%ccbg*dt/velp
  END IF
  CALL put_liquid( pnew,pq )
END IF
IF( IsMulti(icls) )THEN
  CALL InitMCcont( pnew,dt,velp )
  IF( nError /= NO_ERROR )GOTO 9999
END IF


!------ Account for dynamic puff turbulence (buoyant only)

IF( dynamic .AND. .NOT.dense_effect(zbar-h,pnew%sv,pd%wcp,pnew%c) )THEN

  wp2  = ucb*ucb + vcb*vcb + wcb*wcb

  IF( wp2 > SMALL )THEN

    slp  = MIN( pnew%si,0.65*MAX(zbar-h,SQRT(pnew%szz)) )
    bdyn = (pd%ctb - thb*pd%bcb)/pnew%c
    vel2 = ub*ub + vb*vb
    vp2  = vel2 + uub + vvb + 1.E-6
    gt0  = G0/thb
    qqvrtx = CVRTX*(vp2*wcb*wcb+(ucb*vb-vcb*ub)**2)/(vp2+wp2)

    IF( ABS(bdyn) > dtdz*slp )THEN
      ri = gt0*ABS(bdyn)*slp
      IF( wp2 < 4.*ri )THEN
        fac = 2.0
      ELSE
        ri  = ri/wp2
        fac = 1.0 + 4.*ri
      END IF
    ELSE
      ri = gt0*(dtdz*slp-ABS(bdyn))*slp
      IF( wp2 < 1.0E-3*ri )THEN
        fac = 0.0
      ELSE
        ri  = ri/wp2
        fac = 1.0/(1.0 + 4.*ri )
      END IF
    END IF

    qqp  = fac*(CQB*wp2 + qqvrtx)
    difp = 0.15*SQRT(qqp)*slp

    pnew%szz = pnew%szz + 2.*difp*dt

  END IF
END IF

!------ Check puff realizability

IF( .NOT.PuffRealizable(pnew%sxx,pnew%sxy,pnew%sxz, &
                        pnew%syy,pnew%syz,pnew%szz) )THEN
  nError   = IV_ERROR
  eRoutine = 'NewContinuousPuff'
  eMessage = 'Error creating puff from continuous release'
  eInform  = 'Non-realizable puff moments'
  CALL dump_puff( npuf,pnew )
  GOTO 9999
END IF

!----- Calculate inverse sigma's

CALL siginv( pnew )

!----- Limit sv to sz

pnew%sv = MIN(pnew%sv,SQRT(pnew%szz))

!----- Set inversion parameters

pnew%zi = zinv
CALL set_zcap_rel( ifld,SNGL(pnew%xbar),SNGL(pnew%ybar),pnew%zbar, &
                        pnew%szz,zinv,dtdzs,pnew%zc,lbl )

!------ Update release puff time level

CALL init_tlev( pnew,pd,ilev,xmap,ymap )
IF( nError /= NO_ERROR )GOTO 9999

lev2 = MAX(lev2,ilev,pnew%idtl)
ilev = MAX(ilev,lev1)

!----- Statics: save velocity to reset puff time level if different from release time level

IF( isStatic )THEN

  prel%uo = ub - defu
  prel%vo = vb - defv
  prel%wo = wb - defw + wcb

END IF

!------ Remove small puffs - just decrement the puff counter

IF( pnew%c <= cmin .OR. pnew%ccb == 0.0 )GOTO 9999

idtl = ilev

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  CreatePoolPuff
!*******************************************************************************
SUBROUTINE CreatePoolPuff( p,dt,syy,rhop )

USE scipuff_fi
USE error_fi
USE met_fi
USE srfevap_fi
USE cont_rel_fi
USE pool_fi, ONLY:pool_offset, pool_age, tp

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: dt
REAL,             INTENT( OUT   ) :: syy
REAL,             INTENT( OUT   ) :: rhop   !Vapor release density

INTEGER  ityp, imat, ifld
REAL     sigze, dp, ua, wp, dtp
REAL     ub2, vb2, rate_p

INTEGER, EXTERNAL :: getPuffifld
LOGICAL, EXTERNAL :: IsMulti

!---- Get local wind speed

CALL get_srf_met( SNGL(p%xbar),SNGL(p%ybar) )
IF( nError /= NO_ERROR )GOTO 9999

ub2 = ubsrf**2
vb2 = vbsrf**2
ua  = SQRT(ub2 + vb2 + uub + vvb + 2.*uubl + 2.*vvbl )
ua  = MAX(ua,ustr,1.E-6)

!---- Set pool temperature and diameter

tp = pool_offset + tsrf
dp = 2.*(p%sxx*p%syy)**0.25

!---- Get material properties

ityp = p%ityp
imat = typeID(ityp)%imat
CALL SetPoolMaterial( imat )
IF( nError /= NO_ERROR )GOTO 9999

!---- Calculate mass evaporation rate

CALL StepPool( rate_p,rhop,dp,tsrf,ustr,z0,pb,p%c,dt )
IF( nError /= NO_ERROR )GOTO 9999

pool_age = pool_age + dt             !Pool age
pool_offset = tp - tsrf              !Pool temp. offset

!---- Limit evaporation rate by total remaining pool mass

rate_p = rate_p/p%c

!---- Vertical velocity from volume flow rate

wp = rate_p*p%c/(0.25*PI*dp*dp*rhop)

!---- Base vertical spread on timestep, but limit by fraction of transit time

dtp   = MAX(dt,dp/ua)
sigze = MAX(zruf,MIN((VONK*ustr/A+wp)*dtp,0.8*zisrf))

!---- Initialize new puff

CALL scale_psum( p,rate_p )

!---- Initialize multi-components

IF( IsMulti(typeID(p%ityp)%icls) )THEN
  CALL InitMCcont( p,rate_p,1.0 )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

p%szz = sigze**2
p%sv  = sigze

IF( ub2+vb2 > 1.E-6 )THEN
  syy = p%syy*(ub2+vb2)/(ub2+vb2*p%syy/p%sxx)
ELSE
  syy = 0.5*(p%sxx+p%syy)
END IF

!---- Reset effective mean velocity based on expected duration

sigze = MAX(sigze,MIN(VONK*ustr/A/rate_p,0.8*zisrf))

ifld = getPuffifld( p )

CALL get_met( SNGL(p%xbar),SNGL(p%ybar),sigze,0.,0.,1,inField=ifld )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  DeactivateContinuousReleases
!*******************************************************************************
SUBROUTINE DeactivateContinuousReleases( dts,tstep,lev )

USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,    INTENT( IN ) :: dts
REAL,    INTENT( IN ) :: tstep
INTEGER, INTENT( IN ) :: lev

INTEGER idef

DO idef = 1,numDefinition
  IF( cDefinition(idef)%state == CR_ACTIVE )THEN
    IF( cDefinition(idef)%isPool )THEN
        CALL DeactivatePoolRelease( cDefinition(idef) )
    ELSE
      CALL DeactivateRelease( dts,tstep,lev,cDefinition(idef) )
    END IF
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  DeactivatePoolRelease
!*******************************************************************************
SUBROUTINE DeactivatePoolRelease( def )

USE scipuff_fi
USE error_fi
USE files_fi
USE cont_rel_fi
USE cont_rel_functions
USE default_fd
USE param_fd

IMPLICIT NONE

TYPE( cont_release_def ), INTENT( INOUT ) :: def

INTEGER irel, ios
LOGICAL inactive

inactive = .TRUE.

IF( def%rSet%nrel > 0 )THEN
  DO irel = 1,def%rSet%nrel
    IF( def%rSet%rels(irel)%naux == SCIPUFF_POOLSRCPARAM )THEN !pool still active
      IF( def%rSet%rels(irel)%saux(1) <= 0.0  )THEN             !mass remaining
        ios = deallocate_contRel( def%rSet%rels(irel) )        !Pool complete - deallocate aux and puffs
        IF( ios /= 0 )THEN
          nError   = UK_ERROR
          eRoutine = 'DeactivatePoolRelease'
          eMessage = 'Deallocation of standard releases failed'
          WRITE(eInform,'(A,I0)')'IOS=',ios
          GOTO 9999
        END IF
        def%rSet%rels(irel)%naux = 1                           !Keep first aux with mass remaining for restarts
        CALL allocate_contRel_aux( def%rSet%rels(irel) )
        IF( nError /= NO_ERROR )GOTO 9999
        def%rSet%rels(irel)%saux(1) = 0.0
      ELSE
        inactive = .FALSE.                                    !This pool still has active members
      END IF
    END IF
  END DO
END IF

IF( inactive )THEN                                      !Just deactivate this release definition
  def%time  = DEF_VAL_R                                 !because we need the source aux on a restart
  def%state = CR_READY                                  !to know that the pool has completed
  CALL removeFromCollection( def )                      !Remove it from the pool collection
  WRITE(lun_log,"(' ',A,F7.2,A,I4)",IOSTAT=ios) &
        'P-Release deactivated at T = ',t/3600.,' with ncrel = ',count_nrel()
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'DeactivatePoolRelease'
    eMessage = 'Error writing SCIPUFF log file'
    CALL ReportFileName( eInform,'File=',file_log )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!  DeactivateRelease
!*******************************************************************************
SUBROUTINE DeactivateRelease( dts,tstep,lev,def )

USE scipuff_fi
USE error_fi
USE files_fi
USE cont_rel_fi
USE cont_rel_functions

IMPLICIT NONE

REAL,                     INTENT( IN    ) :: dts
REAL,                     INTENT( IN    ) :: tstep
INTEGER,                  INTENT( IN    ) :: lev
TYPE( cont_release_def ), INTENT( INOUT ) :: def

REAL    dt, dtt
LOGICAL completed, baseComplete
INTEGER ios, irel

IF( lev == 0 )THEN   !Completing large timestep - need consistent test for restart
  dt  = def%end - tstep
  dtt = 1.0E-4*def%dur
ELSE
  dt  = def%end - def%time
  dtt = 0.001*dts
END IF

completed = .TRUE.
IF( def%rSet%nrel > 0 )THEN
  DO irel = 1,def%rSet%nrel
    IF( def%rSet%rels(irel)%basePuff%idtl == I_REMOVE )CYCLE
    baseComplete = .NOT.(dt > dtt .OR. def%rSet%rels(irel)%basePuff%idtl < lev)
    IF( baseComplete )def%rSet%rels(irel)%basePuff%idtl = I_REMOVE
    completed = completed .AND. baseComplete
  END DO
END IF

IF( completed )THEN
  WRITE(lun_log,"(' ',A,F7.2,A,I4)",IOSTAT=ios) &
        'C-Release deactivated at T = ',t/3600.,' with ncrel = ',count_nrel()
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'DeactivateRelease'
    eMessage = 'Error writing SCIPUFF log file'
    CALL ReportFileName( eInform,'File=',file_log )
    GOTO 9999
  END IF
  CALL deactivate_Definition( def )
END IF

9999 CONTINUE

RETURN
END
