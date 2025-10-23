SUBROUTINE inter( ilev,jlev )

USE scipuff_fi
USE inter_fi
USE step_p_fi, ONLY: myTId
USE omp_lib

!  calculates the overlap integrals

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ilev, jlev

INTEGER ipuf, lev, inxt, alloc_stat
LOGICAL reset_cc
INTEGER ipl

IF( npuf == 0 )RETURN

!------ clear interaction terms

ALLOCATE( ptmp(npuf),kl(npuf),ku(npuf),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ptmp2(npuf),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ccb(npuf),cctb(npuf),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( ccbg(npuf),STAT=alloc_stat )
IF( dynamic )THEN
  IF( alloc_stat == 0 )ALLOCATE( ucb(npuf),vcb(npuf),wcb(npuf),STAT=alloc_stat )
  IF( alloc_stat == 0 )ALLOCATE( ctb(npuf),bcb(npuf),ctp(npuf),STAT=alloc_stat )
  IF( dense_gas )THEN
    IF( alloc_stat == 0 )ALLOCATE( udense(npuf),vdense(npuf),STAT=alloc_stat )
    IF( alloc_stat == 0 )ALLOCATE( dudx_d(npuf),dudy_d(npuf),dvdx_d(npuf),dvdy_d(npuf),STAT=alloc_stat )
  END IF
END IF
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'inter'
  eMessage = 'Error allocating interaction arrays'
  WRITE(eInform,'(A,I10)') 'Requested size =',npuf
  WRITE(eAction,'(A,I10)') 'Allocation error =',alloc_stat
  GOTO 9999
END IF

kl = -1  ! Initialize to negative number for check in loverlap
ku = -1

ipufMin = -1    !Use all puffs in interaction tests

reset_cc = .FALSE.

DO lev = ilev,jlev
!$OMP PARALLEL IF( nthreads > 1 )
!$OMP SINGLE
  ipuf = itfrst(lev)
  DO WHILE( ipuf > 0 )
!$OMP TASK FIRSTPRIVATE(ipuf) DEFAULT(SHARED)
    myTId = OMP_GET_THREAD_NUM() + 1
    IF( nError /= NO_ERROR )THEN
      omp_error(myTId)%nError   = nError
      omp_error(myTId)%eRoutine = eRoutine
      omp_error(myTId)%eMessage = eMessage
      omp_error(myTId)%eInform  = eInform
      omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
    END IF
    CALL clear_inter_omp( ipuf )
    IF( puff(ipuf)%cc < 0.0 )THEN
!$OMP ATOMIC WRITE
      reset_cc = .TRUE.
    END IF
!$OMP END TASK
    ipuf = puff(ipuf)%idtn
  END DO
!$OMP END SINGLE
!$OMP END PARALLEL
END DO

IF( multicomp )THEN
  CALL InitInterMC( 1,npuf )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!------ compute mutual overlap between ipuf and its friendly neighbors

irfrst = 0
irlast = 0
nrlist = 0

DO lev = ilev,jlev

!$OMP PARALLEL
!$OMP SINGLE
  iprv = 0
  ipuf = itfrst(lev)
  DO WHILE( ipuf > 0 )
!$OMP TASK FIRSTPRIVATE(ipuf) DEFAULT(SHARED)
!$OMP FLUSH( omp_error )
    IF( ANY(omp_error(:)%nError /= NO_ERROR) )GOTO 99999
    myTId = OMP_GET_THREAD_NUM() + 1
    CALL inter_puff( ipuf,lev,ilev )
    IF( nError /= NO_ERROR )THEN
      omp_error(myTId)%nError   = nError
      omp_error(myTId)%eRoutine = eRoutine
      omp_error(myTId)%eMessage = eMessage
      omp_error(myTId)%eInform  = eInform
      omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
    END IF
    IF( myTId == 1 )CALL step_clock()
99999 CONTINUE
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
END DO

DO lev = ilev,jlev
!$OMP PARALLEL IF( nthreads > 1 )
!$OMP SINGLE
  ipuf = itfrst(lev)
  DO WHILE( ipuf > 0 )
!$OMP TASK FIRSTPRIVATE(ipuf) DEFAULT(SHARED)
    CALL set_inter_omp( ipuf )
!$OMP END TASK
    ipuf = puff(ipuf)%idtn
  END DO
!$OMP END SINGLE
!$OMP END PARALLEL
END DO

!------ Check for small mass puffs outside parallel loop

DO lev = ilev,jlev

  iprv = 0
  ipuf = itfrst(lev)

  DO WHILE( ipuf > 0 )
    IF( puff(ipuf)%c <= cmin .OR. puff(ipuf)%ccb == 0.0 )THEN
      CALL add_remove_list( ipuf )
    END IF
    iprv = ipuf
    ipuf = puff(ipuf)%idtn
  END DO

END DO

!------ remove puffs : puffs with small mass or small self-interaction
!       The linked list was built by inter_puff
!       The linked list head is in irfrst
!       The linked list tail is in irlast
!       The linked list is in puff%cfo
!       The previous puff in the time list is in puff%sr

IF( irfrst > 0 )THEN

  ipuf = irfrst

  DO WHILE( ipuf > 0 )

!---------- Remove from the ipgrd list

    CALL remove_ipgrd( ipuf )

!---------- Remove from the tlev list
!           Check to make sure that it wasn't already removed since inter_puff removes puffs
!           Check to make sure the "previous" puff hasn't already been removed as well

    lev  = puff(ipuf)%idtl
    IF( lev >= 0 )THEN
      puff(ipuf)%idtl = I_REMOVE
      iprv = NINT(puff(ipuf)%sr)
      IF( iprv /= 0 )THEN
        DO WHILE( puff(iprv)%idtl == I_REMOVE )
          iprv = NINT(puff(iprv)%sr)
          IF( iprv == 0 )EXIT
        END DO
      END IF
      inxt = puff(ipuf)%idtn
      CALL remove_tlev( lev,iprv,inxt )
    END IF

    ipuf = NINT(puff(ipuf)%cfo)
    CALL step_clock()

  END DO

END IF

!------ set cc for liquid / particle material puffs

DO lev = ilev,jlev

  ipuf = itfrst(lev)

  DO WHILE( ipuf > 0 )
    CALL set_cc_liq( puff(ipuf),ptmp(ipuf),ptmp2(ipuf) )
    ipuf = puff(ipuf)%idtn
    CALL step_clock()
  END DO

END DO

!------ reset any muilticomponent interactions

IF( multicomp )THEN
  DO lev = ilev,jlev
    ipuf = itfrst(lev)
    DO WHILE( ipuf > 0 )
      CALL ResetInterMC( ipuf,puff(ipuf) )
      IF( nError /= NO_ERROR )GOTO 9999
      ipuf = puff(ipuf)%idtn
    END DO
  END DO
END IF

!------ Reset <cc> for new puffs from secondary evaporation

IF( reset_cc )THEN

  DO lev = ilev,jlev

    ipuf = itfrst(lev)

    DO WHILE( ipuf > 0 )
      IF( puff(ipuf)%cc < 0.0 )CALL set_cc_inter( puff(ipuf) )
      ipuf = puff(ipuf)%idtn
      CALL step_clock()
    END DO

  END DO

END IF

9999 CONTINUE

IF( ALLOCATED(ptmp)  )DEALLOCATE( ptmp, STAT=alloc_stat )
IF( ALLOCATED(ptmp2) )DEALLOCATE( ptmp2,STAT=alloc_stat )
IF( ALLOCATED(kl)    )DEALLOCATE( kl,   STAT=alloc_stat )
IF( ALLOCATED(ku)    )DEALLOCATE( ku,   STAT=alloc_stat )
IF( ALLOCATED(ccb)  )DEALLOCATE( ccb, STAT=alloc_stat )
IF( ALLOCATED(cctb) )DEALLOCATE( cctb,STAT=alloc_stat )
IF( ALLOCATED(cc)   )DEALLOCATE( cc,  STAT=alloc_stat )
IF( ALLOCATED(cct)  )DEALLOCATE( cct, STAT=alloc_stat )
IF( ALLOCATED(ccbg) )DEALLOCATE( ccbg,STAT=alloc_stat )
IF( dynamic )THEN
  IF( ALLOCATED(ucb) )DEALLOCATE( ucb,STAT=alloc_stat )
  IF( ALLOCATED(vcb) )DEALLOCATE( vcb,STAT=alloc_stat )
  IF( ALLOCATED(wcb) )DEALLOCATE( wcb,STAT=alloc_stat )
  IF( ALLOCATED(ctb) )DEALLOCATE( ctb,STAT=alloc_stat )
  IF( ALLOCATED(bcb) )DEALLOCATE( bcb,STAT=alloc_stat )
  IF( ALLOCATED(ctp) )DEALLOCATE( ctp,STAT=alloc_stat )
  IF( dense_gas )THEN
    IF( ALLOCATED(udense) )DEALLOCATE( udense,STAT=alloc_stat )
    IF( ALLOCATED(vdense) )DEALLOCATE( vdense,STAT=alloc_stat )
    IF( ALLOCATED(dudx_d) )DEALLOCATE( dudx_d,STAT=alloc_stat )
    IF( ALLOCATED(dudy_d) )DEALLOCATE( dudy_d,STAT=alloc_stat )
    IF( ALLOCATED(dvdx_d) )DEALLOCATE( dvdx_d,STAT=alloc_stat )
    IF( ALLOCATED(dvdy_d) )DEALLOCATE( dvdy_d,STAT=alloc_stat )
  END IF
END IF

IF( multicomp )CALL ExitInterMC()

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_proc( ipuf,jpuf,pdi,pti,pqi )

USE scipuff_fi
USE inter_fi
USE error_fi, ONLY:singleThread

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: ipuf,jpuf
TYPE( puff_dynamics ), INTENT( INOUT ) :: pdi
TYPE( puff_totalcc  ), INTENT( INOUT ) :: pti
TYPE( puff_liquid   ), INTENT( INOUT ) :: pqi

TYPE( puff_dynamics ) pdj
TYPE( puff_totalcc  ) ptj
TYPE( puff_liquid   ) pqj
TYPE( puff_aerosol  ) pa
TYPE( puff_material ) pmat

REAL bfac
REAL  lfac
REAL  du, dv, dudx, dudy, dvdx, dvdy

LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsAerosol

IF( ipuf == jpuf )THEN

!------ compute self-overlap integral factor

  delx = 0.
  dely = 0.
  delz = 0.

  IF( lrfl_ipuf )THEN
    CALL inter_facr_self( puff(ipuf) )
  ELSE
    CALL inter_fac_self( puff(ipuf) )
  END IF

!------ set inversion reflection

  CALL inter_faci_self( puff(ipuf) )

  facu = fac
  facv = fac
ELSE

!----- puff separation

  delx = SNGL(puff(jpuf)%xbar - puff(ipuf)%xbar)/xmap_i
  dely = SNGL(puff(jpuf)%ybar - puff(ipuf)%ybar)/ymap_i
  delz =  puff(jpuf)%zbar - puff(ipuf)%zbar

!------ check ground proximity for jpuf

  CALL test_refl( puff(jpuf),lrfl_jpuf,zp,hp,hxp,hyp )

!------ compute overlap integral factor

  IF( lrfl_ipuf .AND. lrfl_jpuf )THEN

    CALL inter_bsig( puff(jpuf) )
    CALL inter_facr( puff(ipuf),puff(jpuf) )
    IF( nError /= NO_ERROR )GOTO 9999

!-- Reset z1 to be relative to hmin for inter_faci
    z1 = z1 + hp

  ELSE

    CALL inter_fac( puff(ipuf),puff(jpuf) )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

!------ set inversion reflection

  IF( fac > 0. )CALL inter_faci( puff(ipuf),puff(jpuf) )

  facu = fac
  facv = fac
END IF

!------ set interaction factors

ctot  = puff(ipuf)%c
ctotp = puff(jpuf)%c

facc  = fac*ctot
faccp = fac*ctotp

facwp = facw*ctotp
facw  = facw*ctot

facup = facu*ctotp
facu  = facu*ctot

facvp = facv*ctotp
facv  = facv*ctot

!------ interaction with puff of same type

IF( ltyp )THEN
!$OMP ATOMIC
  ccb(ipuf) = ccb(ipuf) + DBLE(facc*ctotp)
  IF( lliqi )THEN
    CALL get_liquid( puff(jpuf),pqj )
    IF( lbinj > 0.0 .AND. lbini > 0.0 )THEN
      IF( ldiami > ldiamj )THEN
        lfac = (ldiamj + 0.5*lbinj - ldiami + 0.5*lbini)/lbini
      ELSE
        lfac = (ldiami + 0.5*lbini - ldiamj + 0.5*lbinj)/lbini
      END IF
    ELSE
      lfac = 0.0
    END IF
!$OMP ATOMIC
    ptmp(ipuf) = ptmp(ipuf) + DBLE(fac*pqi%ccs*pqj%ccs)
    IF( lfac > 0.0 )THEN
!$OMP ATOMIC
      ccbg(ipuf) = ccbg(ipuf) + DBLE(lfac*facc*ctotp)
    END IF
  END IF
  IF( .NOT. lstatic )THEN
!$OMP ATOMIC
    ccb(jpuf) = ccb(jpuf) + DBLE(faccp*ctot)
    IF( lliqi )THEN
      IF( lfac > 0.0 )THEN
!$OMP ATOMIC
        ccbg(jpuf) = ccbg(jpuf) + DBLE(lfac*faccp*ctot*lbini/lbinj)
      END IF
    END IF
    IF( multiThread )THEN
    IF( lliqi )THEN
!$OMP ATOMIC
      ptmp(jpuf) = ptmp(jpuf) + DBLE(fac*pqi%ccs*pqj%ccs)
    END IF
    ELSE
    IF( lliqi )ptmp(jpuf) = ptmp(jpuf) + fac*pqi%ccs*pqj%ccs
    END IF
    IF( jpuf < ipufMin )THEN
!$OMP ATOMIC
      cc(jpuf) = cc(jpuf) + MAX(1.D0,cc(ipuf))*DBLE(faccp*ctot)
    END IF
  END IF
END IF

IF( dynamic )THEN

!------ interaction with dynamic puff

!------ Get j-puff dynamics in case we need it. Already have i-puff dynamics.

  CALL get_dynamics( puff(jpuf),pdj )

!------ Adjust i-puff dynamics

  IF( ldynj )THEN

!$OMP ATOMIC
    wcb(ipuf) = wcb(ipuf) + DBLE(facw*pdj%w)
!$OMP ATOMIC
    ctb(ipuf) = ctb(ipuf) + DBLE(facc*pdj%t)
!$OMP ATOMIC
    ucb(ipuf) = ucb(ipuf) + DBLE(facu*pdj%un)
!$OMP ATOMIC
    vcb(ipuf) = vcb(ipuf) + DBLE(facv*pdj%vn)

    IF( dense_gas )THEN
      IF( ipuf == jpuf )THEN
        CALL inter_dense_self( ipuf,puff(ipuf),pdi )
      ELSE
        IF( pdj%u0 /= 0.0 )THEN
          CALL inter_dense( pdj,pdi,-delx,-dely,du,dv,dudx,dudy,dvdx,dvdy )
!$OMP ATOMIC
          udense(ipuf) = udense(ipuf) + DBLE(du)
!$OMP ATOMIC
          vdense(ipuf) = vdense(ipuf) + DBLE(dv)
!$OMP ATOMIC
          dudx_d(ipuf) = dudx_d(ipuf) + DBLE(dudx)
!$OMP ATOMIC
          dudy_d(ipuf) = dudy_d(ipuf) + DBLE(dudy)
!$OMP ATOMIC
          dvdx_d(ipuf) = dvdx_d(ipuf) + DBLE(dvdx)
!$OMP ATOMIC
          dvdy_d(ipuf) = dvdy_d(ipuf) + DBLE(dvdy)
        END IF
      END IF
    END IF

  END IF

  IF( lliqj )THEN
    CALL get_liquid( puff(jpuf),pqj )
!$OMP ATOMIC
    ctp(ipuf) = ctp(ipuf) + DBLE(facc*pqj%tevap)
  END IF

  IF( IsAerosol(typeID(puff(jpuf)%ityp)%icls) )THEN
    CALL get_aerosol( puff(jpuf),pa )
!$OMP ATOMIC
    ctp(ipuf) = ctp(ipuf) + DBLE(facc*pa%tevap)
  END IF

  IF( buoy_gas .AND. (ldynj .OR. lliqj) )THEN
    IF( IsAerosol(typeID(puff(jpuf)%ityp)%icls) )THEN
      CALL get_aerosol( puff(jpuf),pa )
      CALL get_puff_material( puff(jpuf)%ityp,pmat )
      CALL set_aerosol_buoy( pa,pmat,bfac,lliqm )
    ELSE
      IF( lliqj )THEN
        bfac = 1.     !/rhoair
      ELSE
        bfac = buoy_fac(puff(jpuf)%ityp)
      END IF
    END IF
!$OMP ATOMIC
    bcb(ipuf) = bcb(ipuf) + DBLE(facc*bfac*ctotp)
  END IF

!------ Adjust j-puff dynamics only if ipuff not equal to jpuff

  IF( .NOT.lstatic )THEN

    IF( ldyni )THEN

!$OMP ATOMIC
      wcb(jpuf) = wcb(jpuf) + DBLE(facwp*pdi%w)
!$OMP ATOMIC
      ctb(jpuf) = ctb(jpuf) + DBLE(faccp*pdi%t)
!$OMP ATOMIC
      ucb(jpuf) = ucb(jpuf) + DBLE(facup*pdi%un)
!$OMP ATOMIC
      vcb(jpuf) = vcb(jpuf) + DBLE(facvp*pdi%vn)

      IF( dense_gas )THEN
        IF( pdi%u0 /= 0.0 )THEN
          CALL inter_dense( pdi,pdj,delx,dely,du,dv,dudx,dudy,dvdx,dvdy )
!$OMP ATOMIC
          udense(jpuf) = udense(jpuf) + DBLE(du)
!$OMP ATOMIC
          vdense(jpuf) = vdense(jpuf) + DBLE(dv)
!$OMP ATOMIC
          dudx_d(jpuf) = dudx_d(jpuf) + DBLE(dudx)
!$OMP ATOMIC
          dudy_d(jpuf) = dudy_d(jpuf) + DBLE(dudy)
!$OMP ATOMIC
          dvdx_d(jpuf) = dvdx_d(jpuf) + DBLE(dvdx)
!$OMP ATOMIC
          dvdy_d(jpuf) = dvdy_d(jpuf) + DBLE(dvdy)
        END IF
      END IF

    END IF

    IF( .NOT.lstatic .AND. lliqi )THEN
      CALL get_liquid( puff(ipuf),pqi )
!$OMP ATOMIC
      ctp(jpuf) = ctp(jpuf) + DBLE(faccp*pqi%tevap)
    END IF

    IF( .NOT.lstatic .AND. laeri )THEN
      CALL get_aerosol( puff(ipuf),pa )
!$OMP ATOMIC
      ctp(jpuf) = ctp(jpuf) + DBLE(faccp*pa%tevap)
    END IF

    IF( buoy_gas .AND. (ldyni .OR. lliqi) )THEN
      IF( laeri )THEN
        CALL get_aerosol( puff(ipuf),pa )
        CALL get_puff_material( puff(ipuf)%ityp,pmat )
        CALL set_aerosol_buoy( pa,pmat,bfac,lliqm )
      ELSE
        IF( lliqi )THEN
          bfac = 1.     !/rhoair
        ELSE
          bfac = buoy_fac(puff(ipuf)%ityp)
        END IF
      END IF
!$OMP ATOMIC
      bcb(jpuf) = bcb(jpuf) + DBLE(faccp*bfac*ctot)
    END IF

  END IF

END IF

!------ interaction with puff of same material for total

IF( lmat .AND. ltot )THEN
!$OMP ATOMIC
  cctb(ipuf) = cctb(ipuf) + DBLE(facc*ctotp)
  IF( lliqm )THEN
    IF( .NOT.lliqi .AND. lliqj .AND. puff(ipuf)%cc >= 0.0 )THEN
      CALL get_liquid( puff(jpuf),pqj )
!$OMP ATOMIC
      ptmp(ipuf) = ptmp(ipuf) + DBLE(fac*ctot*pqj%ccs)
    ELSE IF( lliqi .AND. .NOT.lliqj .AND. puff(jpuf)%cc >= 0.0 )THEN
      IF( .NOT.lstatic )THEN
        CALL get_liquid( puff(ipuf),pqi )
!$OMP ATOMIC
        ptmp(jpuf) = ptmp(jpuf) + DBLE(fac*ctotp*pqi%ccs)
      END IF
    ELSE IF( .NOT.(lliqi .OR. lliqj .OR. ltyp) )THEN  !Vapor-aerosol overlap for liquid material
      IF( laeri )THEN
!$OMP ATOMIC
        ccb(ipuf) = ccb(ipuf) + DBLE(facc*ctotp)
        IF( .NOT. lstatic )THEN
          CALL get_aerosol( puff(ipuf),pa )
!$OMP ATOMIC
          ccb(jpuf) = ccb(jpuf) + DBLE(pa%fl*faccp*ctot)
        END IF
      ELSE
        CALL get_aerosol( puff(jpuf),pa )
!$OMP ATOMIC
          ccb(ipuf) = ccb(ipuf) + DBLE(pa%fl*facc*ctotp)
        IF( .NOT.lstatic )THEN
!$OMP ATOMIC
          ccb(jpuf) = ccb(jpuf) + DBLE(faccp*ctot)
        END IF
      END IF
    END IF
  END IF
  IF( .NOT.lstatic )THEN
!$OMP ATOMIC
    cctb(jpuf) = cctb(jpuf) + DBLE(faccp*ctot)
    IF( jpuf < ipufMin )THEN
!$OMP ATOMIC
      cct(jpuf) = cct(jpuf) + MAX(1.D0,cc(ipuf))*DBLE(faccp*ctot)
    END IF
  END IF
END IF

!------ interaction with puff of same material for multi-component

IF( lmat .AND. lmc )THEN
  CALL GetInterMC2( puff(jpuf) )
  CALL InterMC( ipuf,jpuf,puff(ipuf),puff(jpuf) )
END IF

9999 CONTINUE

RETURN
END

!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE clear_inter( p,ptmp,ptmp2 )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( OUT   ) :: ptmp
REAL,             INTENT( OUT   ) :: ptmp2

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc ) pt
TYPE( puff_liquid ) pq

LOGICAL, EXTERNAL :: IsParticle, IsGas
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid

ptmp  = 0.0
ptmp2 = 0.0

IF( p%naux > 0 )THEN

  IF( dynamic )THEN

    CALL get_dynamics( p,pd )

    pd%wcb = 0.0
    pd%ctb = 0.0
    pd%ucb = 0.0
    pd%vcb = 0.0

    IF( buoy_gas )pd%bcb = 0.0

    IF( dense_gas )THEN
      pd%u = 0.0
      pd%v = 0.0
      pd%dudx = 0.0
      pd%dudy = 0.0
      pd%dvdx = 0.0
      pd%dvdy = 0.0
      IF( IsGas(typeID(p%ityp)%icls) )CALL set_dense_gas( p,pd )
    END IF

    CALL put_dynamics( p,pd )

  END IF

  IF( typeID(p%ityp)%ltot )THEN

    CALL get_totalcc( p,pt )
    IF( IsParticle(typeID(p%ityp)%icls) )THEN
      IF( pt%cctb > SMALL )THEN
        ptmp = p%ccb/pt%cctb
      ELSE
        ptmp = 0.
      END IF
    END IF
    pt%cctb = 0.0
    CALL put_totalcc( p,pt )

  END IF
  IF( IsLiquid(typeID(p%ityp)%icls) )THEN

    CALL get_liquid( p,pq )
    IF( p%ccb > SMALL )THEN
      ptmp2 = pq%ccbg/p%ccb
    ELSE
      ptmp2 = 0.0
    END IF
    pq%ccbg = 0.0
    CALL put_liquid( p,pq )

  END IF

  IF( IsMulti(typeID(p%ityp)%icls) )CALL ClearInterMC( p )
END IF

p%ccb = 0.0

RETURN
END

!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE clear_inter_omp( ipuf )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc ) pt
TYPE( puff_liquid ) pq

LOGICAL, EXTERNAL :: IsParticle, IsGas
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid

ptmp(ipuf) = 0.D0

IF( dynamic )THEN

  ucb(ipuf) = 0.D0
  vcb(ipuf) = 0.D0
  wcb(ipuf) = 0.D0
  ctb(ipuf) = 0.D0

  bcb(ipuf) = 0.D0

  CALL get_dynamics( puff(ipuf),pd )
  ctp(ipuf) = DBLE(pd%ctp)

  IF( dense_gas )THEN
    udense(ipuf) = 0.D0
    vdense(ipuf) = 0.D0
    dudx_d(ipuf) = 0.D0
    dudy_d(ipuf) = 0.D0
    dvdx_d(ipuf) = 0.D0
    dvdy_d(ipuf) = 0.D0
    IF( IsGas(typeID(puff(ipuf)%ityp)%icls) )THEN
      CALL set_dense_gas( puff(ipuf),pd )
      CALL put_dynamics( puff(ipuf),pd )
    END IF
  END IF

END IF

IF( typeID(puff(ipuf)%ityp)%ltot )THEN

  CALL get_totalcc( puff(ipuf),pt )
  IF( IsParticle(typeID(puff(ipuf)%ityp)%icls) )THEN
    IF( pt%cctb > SMALL )THEN
      ptmp(ipuf) = puff(ipuf)%ccb/pt%cctb
    END IF
  END IF
  cctb(ipuf) = 0.D0

END IF

ptmp2(ipuf) = 0.D0
IF( IsLiquid(typeID(puff(ipuf)%ityp)%icls) )THEN

  CALL get_liquid( puff(ipuf),pq )
  IF( puff(ipuf)%ccb > SMALL )THEN
    ptmp2(ipuf) = pq%ccbg/puff(ipuf)%ccb
  END IF

END IF

IF( IsMulti(typeID(puff(ipuf)%ityp)%icls) )CALL ClearInterMC( puff(ipuf) )
ccb(ipuf) = 0.D0
ccbg(ipuf) = 0.D0

RETURN
END

!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE init_inter_omp( ipuf )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc ) pt
TYPE( puff_liquid ) pq

LOGICAL, EXTERNAL :: IsParticle, IsGas
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid

IF( dynamic )THEN

  CALL get_dynamics( puff(ipuf),pd )

  ucb(ipuf) = DBLE(pd%ucb)
  vcb(ipuf) = DBLE(pd%vcb)
  wcb(ipuf) = DBLE(pd%wcb)
  ctb(ipuf) = DBLE(pd%ctb)

  bcb(ipuf) = DBLE(pd%bcb)

  ctp(ipuf) = DBLE(pd%ctp)

  IF( dense_gas )THEN
    udense(ipuf) = DBLE(pd%u)
    vdense(ipuf) = DBLE(pd%v)
    dudx_d(ipuf) = DBLE(pd%dudx)
    dudy_d(ipuf) = DBLE(pd%dudy)
    dvdx_d(ipuf) = DBLE(pd%dvdx)
    dvdy_d(ipuf) = DBLE(pd%dvdy)
  END IF

END IF

IF( typeID(puff(ipuf)%ityp)%ltot )THEN

  CALL get_totalcc( puff(ipuf),pt )
  cctb(ipuf) = DBLE(pt%cctb)

END IF

IF( IsLiquid(typeID(puff(ipuf)%ityp)%icls) )THEN

  CALL get_liquid( puff(ipuf),pq )
  ccbg(ipuf) = DBLE(pq%ccbg)

END IF

ccb(ipuf) = DBLE(puff(ipuf)%ccb)

RETURN
END

!------------------------------------------------------------------------------

RECURSIVE SUBROUTINE set_inter_omp( ipuf )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf

TYPE( puff_dynamics ) pd
TYPE( puff_totalcc ) pt
TYPE( puff_liquid ) pq

LOGICAL, EXTERNAL :: IsParticle, IsGas
LOGICAL, EXTERNAL :: IsMulti
LOGICAL, EXTERNAL :: IsLiquid

IF( dynamic )THEN

  CALL get_dynamics( puff(ipuf),pd )

  pd%ucb = SNGL(ucb(ipuf))
  pd%vcb = SNGL(vcb(ipuf))
  pd%wcb = SNGL(wcb(ipuf))
  pd%ctb = SNGL(ctb(ipuf))

  pd%bcb = SNGL(bcb(ipuf))

  pd%ctp = SNGL(ctp(ipuf))

  IF( dense_gas )THEN
    pd%u = SNGL(udense(ipuf))
    pd%v = SNGL(vdense(ipuf))
    pd%dudx = SNGL(dudx_d(ipuf))
    pd%dudy = SNGL(dudy_d(ipuf))
    pd%dvdx = SNGL(dvdx_d(ipuf))
    pd%dvdy = SNGL(dvdy_d(ipuf))
  END IF

  CALL put_dynamics( puff(ipuf),pd )

END IF

IF( typeID(puff(ipuf)%ityp)%ltot )THEN

  CALL get_totalcc( puff(ipuf),pt )
  pt%cctb = SNGL(cctb(ipuf))
  IF( ipufMin > 0 )pt%cct = SNGL(cct(ipuf))
  CALL put_totalcc( puff(ipuf),pt )

END IF

IF( IsLiquid(typeID(puff(ipuf)%ityp)%icls) )THEN

  CALL get_liquid( puff(ipuf),pq )
  pq%ccbg = SNGL(ccbg(ipuf))
  CALL put_liquid( puff(ipuf),pq )

END IF

puff(ipuf)%ccb = SNGL(ccb(ipuf))
IF( ipufMin > 0 )puff(ipuf)%cc = SNGL(cc(ipuf))

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE inter_puff( ipuf,lev,ilev )

USE scipuff_fi
USE inter_fi
USE error_fi, ONLY:singleThread

!  calculates the overlap integrals

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ipuf
INTEGER, INTENT( IN    ) :: ilev, lev

INTEGER jpuf
INTEGER ityp, imat, jtyp, jmat, jgrd, igrd, i, j, k
INTEGER k0, i1, i2, j1, j2, ig1, ig2, nk
REAL    xp0, yp0

TYPE( puff_dynamics ) pdi
TYPE( puff_totalcc  ) pti
TYPE( puff_liquid   ) pqi, pqj

LOGICAL, EXTERNAL :: IsGas
LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsMulti

INTEGER, EXTERNAL :: getPuffipgrd, grid_mxlev

!------ save IPUF material and type

ityp  = puff(ipuf)%ityp
imat  = typeID(ityp)%imat
ltot  = typeID(ityp)%ltot
lmc   = IsMulti(typeID(ityp)%icls)
lliqm = IsLiquid(material(imat)%icls)
lliqi = IsLiquid(typeID(ityp)%icls)
laeri = IsAerosol(typeID(ityp)%icls)
IF( lliqi )THEN
  CALL get_liquid( puff(ipuf),pqi )
  ldiami = pqi%d
  lgrpi = typeID(ityp)%igrp - 1
  CALL get_liquid_bin( material(imat),lgrpi,lbini,puff(ipuf) )
END IF
ldyni = IsGas(typeID(ityp)%icls) .AND. dynamic

IF( dynamic )CALL get_dynamics( puff(ipuf),pdi )
IF( ltot    )CALL get_totalcc( puff(ipuf),pti )
IF( lmc     )CALL GetInterMC1( puff(ipuf) )

CALL mapfac( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),xmap_i,ymap_i )

CALL puff_grid( puff(ipuf),xp,yp,k0 )

!------ check ground proximity for ipuf

CALL test_refl( puff(ipuf),lrfl_ipuf,zp,h,hx,hy )

IF( lrfl_ipuf )THEN
  CALL inter_asig( puff(ipuf) )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

xp0 = MAX(0.,MIN(FLOAT(nx)-1.E-6,xp))
yp0 = MAX(0.,MIN(FLOAT(ny)-1.E-6,yp))

nk = INT(2.*SQRT(puff(ipuf)%szz)/dzg) + 1
!$OMP ATOMIC WRITE
ku(ipuf) = MIN(k0 + nk,nz)
!$OMP ATOMIC WRITE
kl(ipuf) = MAX(k0 - nk,1)

igrd = getPuffipgrd( puff(ipuf) )

!---- Set range of grid levels

IF( dynamic .OR. lmc )THEN
  ig1 = 0
ELSE
  ig1 = MAX(0,igrd-1)
END IF
ig2 = igrd + 1

IF( lProcessAll )ig2 = MAX(ig2,grid_mxlev(nz))

gridLevel: DO jgrd = ig1,ig2

  dgrd = 0.5**jgrd

  xp = (FLOAT(INT(xp0/dgrd)) + 0.5)*dgrd
  yp = (FLOAT(INT(yp0/dgrd)) + 0.5)*dgrd

  IF( global_lon )THEN
    i1 = -1
    i2 =  1
  ELSE
    CALL limint( xp,dgrd,i1,i2,nx )
  END IF
  CALL limint( yp,dgrd,j1,j2,ny )

  zGrid: DO k = kl(ipuf),ku(ipuf)

    ygrid: DO j = j1,j2

      yy = yp + FLOAT(j)*dgrd

      xGrid: DO i = i1,i2

        xx = xp + FLOAT(i)*dgrd

        IF( global_lon )CALL SetGlobalGrid( xx,nx )

        CALL find_cell( xx,yy,k,jgrd,nx,ny,jpuf )

        DO WHILE( jpuf /= 0 )

!-------------- check type
          jtyp  = puff(jpuf)%ityp
          jmat  = typeID(jtyp)%imat
          ldynj = IsGas( typeID(jtyp)%icls ) .AND. dynamic
          lliqj = IsLiquid( typeID(jtyp)%icls )

          lmat  = (imat == jmat) .AND. (ltot .OR. lmc)

          IF( lliqi )THEN
            ltyp = lmat .AND. lliqj
            IF( ltyp )THEN
              lgrpj = typeID(jtyp)%igrp - 1
              IF( IABS( lgrpi-lgrpj) <= 1 )THEN
                CALL get_liquid_bin( material(imat),lgrpj,lbinj,puff(jpuf) )
                CALL get_liquid( puff(jpuf),pqj )
                ldiamj = pqj%d
              ELSE
                lbinj = 0.0
              END IF
            END IF
          ELSE
            ltyp = ityp == jtyp
          END IF
          lsame = ltyp .OR. lmat

          CALL set_lprocess( ipuf,jpuf,lev,ilev,igrd,jgrd,k0,k )

!------ compute interactions if necessary

          IF( lprocess )THEN
            CALL inter_proc( ipuf,jpuf,pdi,pti,pqi )
            IF( nError /= NO_ERROR )GOTO 9999
          END IF

!------ find next guy and check again

          jpuf = puff(jpuf)%inxt

        END DO

      END DO xGrid
    END DO yGrid
  END DO zGrid
END DO gridLevel

IF( singleThread )THEN
IF( dynamic )CALL put_dynamics( puff(ipuf),pdi )
IF( ltot    )CALL put_totalcc(  puff(ipuf),pti )
ENDIF

9999 CONTINUE

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE set_lprocess( ipuf,jpuf,lev,ilev,igrd,jgrd,ki,kj )

USE puff_fi
USE inter_fi

!  Set flag to calculate interaction between IPUF and JPUF
!  Process   - higher/same time levels
!            - lower/same grid levels

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf, jpuf
INTEGER, INTENT( IN ) :: ilev
INTEGER, INTENT( IN ) :: igrd, jgrd
INTEGER, INTENT( IN ) :: lev, ki, kj

LOGICAL, EXTERNAL :: loverlap

IF( ilev >= 0 )THEN

!------- Call from subroutine INTER

  IF( jgrd > igrd )THEN                  ! JPUF is on smaller grid
   IF ( puff(jpuf)%idtl >= ilev )THEN   ! don't process unless static
      lprocess = .FALSE.
      RETURN
    END IF
    lstatic = .TRUE.
  ELSE                                   ! set static flag (i.e. don't update JPUF)
    lstatic = (puff(jpuf)%idtl < ilev) .OR. (ipuf == jpuf)
  END IF

!---- JPUF is on same/lower grid level (larger than IPUF)

  IF( jgrd < igrd-1 .AND. .NOT.ldynj )THEN ! only process much larger puffs if dynamic
    lprocess = .FALSE.
  ELSE IF( lsame .OR. ldynj )THEN        ! same material or dynamic JPUF
    IF( jgrd < igrd )THEN                ! process lower grid levels
      lprocess = .TRUE.
      lstatic  = lstatic .OR. (jgrd < igrd-1)
    ELSE
      lprocess = lstatic .OR. puff(jpuf)%idtl > lev
      IF( .NOT.lprocess .AND. puff(jpuf)%idtl == lev )THEN
        lprocess = loverlap( ipuf,jpuf,ki,kj )
      END IF
    END IF
  ELSE IF( ldyni .AND. .NOT.lstatic )THEN ! dynamic IPUF, different materials
    IF( puff(jpuf)%idtl == lev )THEN
      lprocess = loverlap( ipuf,jpuf,ki,kj )
    ELSE
      lprocess = puff(jpuf)%idtl > lev  ! don't process lower time levels
    END IF
  ELSE
    lprocess = .FALSE.
  END IF

ELSE

!------- Call from subroutine SET_CC

  lstatic  = (ipuf == jpuf)

  lprocess = lsame .OR. ldynj .OR. ldyni
  IF( lprocess )lprocess = loverlap( ipuf,jpuf,ki,kj )

END IF

RETURN
END

!=======================================================================

RECURSIVE LOGICAL FUNCTION loverlap( ipuf,jpuf,ki,kj )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf, jpuf, ki, kj

INTEGER nk

IF( jpuf >= ipuf )THEN

  loverlap = .TRUE.

ELSE

  IF( kl(jpuf) < 0 )THEN  !Set kl, ku if not set yet
    nk = INT(2.*SQRT(puff(jpuf)%szz)/dzg) + 1
!$OMP ATOMIC WRITE
    ku(jpuf) = MIN(kj + nk,nz)
!$OMP ATOMIC WRITE
    kl(jpuf) = MAX(kj - nk,1)
  END IF

  IF( jpuf < ipufMin )THEN     !Check for set_cc call
    loverlap = .TRUE.
  ELSE
    IF( puff(jpuf)%zbar < puff(ipuf)%zbar )THEN
      loverlap = ku(jpuf) < ki
    ELSE
      loverlap = kl(jpuf) > ki
    END IF
  END IF

END IF

RETURN
END

!=======================================================================

SUBROUTINE set_cc_inter( p )

USE scipuff_fi

!  Set <cc> based on concentration fluctuation ratio

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL cfr

TYPE( puff_totalcc ) pt

cfr = 1.0 - p%cc/p%c

p%cc = cfr*p%ccb

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc( p,pt )
  pt%cct = cfr*pt%cctb
  CALL put_totalcc( p,pt )
END IF

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE inter_dense( pd0,pd,xp,yp,delu,delv,deldudx,deldudy,deldvdx,deldvdy )

USE scipuff_fi
USE error_fi, ONLY: multiThread

!  Set dense gas velocity effects due to puff p0 for puff p

IMPLICIT NONE

REAL,                  INTENT( IN    ) :: xp,yp
TYPE( puff_dynamics ), INTENT( IN    ) :: pd0
REAL,                  INTENT( OUT   ) :: delu, delv
REAL,                  INTENT( OUT   ) :: deldudx, deldudy, deldvdx, deldvdy
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd

REAL u0, sn, cs, x1, y1, r2, fac, xx, yy, xl, facx, facy
REAL u, v, dudx, dudy, dvdx, dvdy

IF( pd0%u0 == 0.0 )RETURN

u0 = pd0%u0
sn = pd0%sn
cs = pd0%cs

xx = pd0%X**2
yy = pd0%Y**2
xl = SQRT( xx+yy )

!------ Rotate coordinates

x1 = ( cs*xp + sn*yp)/pd0%X
y1 = (-sn*xp + cs*yp)/pd0%Y

r2 = x1*x1 + y1*y1

IF( r2 > 20.0 )THEN       ! No interactions if too far away
  delu = 0.0
  delv = 0.0
  deldudx = 0.0
  deldudy = 0.0
  deldvdx = 0.0
  deldvdy = 0.0
  RETURN
END IF

fac = u0*EXP(-r2)
facx = fac*pd0%Y/xl
facy = fac*pd0%X/xl

u = x1*facx
v = y1*facy

dudx = (1.0 - 2.*x1*x1)*facx/pd0%X
dvdy = (1.0 - 2.*y1*y1)*facy/pd0%Y

dudy = -2.*x1*y1*fac/xl
dvdx = dudy

!------ Rotate velocity and gradients back and accumulate

IF( .TRUE. )THEN
delu = cs*u - sn*v
delv = sn*u + cs*v

deldudx = cs*cs*dudx + sn*sn*dvdy - sn*cs*(dudy+dvdx)
deldvdy = sn*sn*dudx + cs*cs*dvdy + sn*cs*(dudy+dvdx)
deldudy = cs*cs*dudy - sn*sn*dvdx + sn*cs*(dudx-dvdy)
deldvdx = cs*cs*dvdx - sn*sn*dudy + sn*cs*(dudx-dvdy)
ELSE
pd%u = pd%u + (cs*u - sn*v)
pd%v = pd%v + (sn*u + cs*v)

pd%dudx = pd%dudx + (cs*cs*dudx + sn*sn*dvdy - sn*cs*(dudy+dvdx))
pd%dvdy = pd%dvdy + (sn*sn*dudx + cs*cs*dvdy + sn*cs*(dudy+dvdx))
pd%dudy = pd%dudy + (cs*cs*dudy - sn*sn*dvdx + sn*cs*(dudx-dvdy))
pd%dvdx = pd%dvdx + (cs*cs*dvdx - sn*sn*dudy + sn*cs*(dudx-dvdy))
END IF

RETURN
END

!=======================================================================

SUBROUTINE inter_dense_self( i,p,pd )

USE scipuff_fi
USE inter_fi, ONLY: dudx_d, dudy_d, dvdx_d, dvdy_d
USE error_fi, ONLY: multiThread

!  Set dense gas velocity effects due to self-interaction for puff p

IMPLICIT NONE

INTEGER,               INTENT( IN   ) :: i
TYPE( puff_str      ), INTENT( INOUT ) :: p
TYPE( puff_dynamics ), INTENT( INOUT ) :: pd

REAL u0, sn, cs, xx, yy, xl, facx, facy
REAL dudx, dvdy, dudy

IF( pd%u0 == 0.0 )RETURN

u0 = pd%u0
sn = pd%sn
cs = pd%cs

xx = pd%X**2
yy = pd%Y**2
xl = SQRT( xx+yy )

facx = u0*pd%Y/xl
facy = u0*pd%X/xl

dudx = facx/pd%X
dvdy = facy/pd%Y

dudy = sn*cs*(dudx-dvdy)

!------ Rotate velocity and gradients back and accumulate

!$OMP ATOMIC
dudx_d(i) = dudx_d(i) + DBLE(cs*cs*dudx + sn*sn*dvdy)
!$OMP ATOMIC
dvdy_d(i) = dvdy_d(i) + DBLE(sn*sn*dudx + cs*cs*dvdy)
!$OMP ATOMIC
dudy_d(i) = dudy_d(i) +  DBLE(dudy)
!$OMP ATOMIC
dvdx_d(i) = dvdx_d(i) +  DBLE(dudy)

RETURN
END


!===============================================================================

SUBROUTINE set_cc_liq( p,ptmp8,ptmp28 )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL(8),          INTENT( IN    ) :: ptmp8
REAL(8),          INTENT( IN    ) :: ptmp28

REAL    fac, rat
REAL    ptmp
REAL    ptmp2
INTEGER imat, ityp

TYPE( puff_totalcc ) pt
TYPE( puff_liquid )pq

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid

ityp = p%ityp
imat = typeID(ityp)%imat

ptmp = SNGL(ptmp8)
ptmp2 = SNGL(ptmp28)

IF( IsLiquid(material(imat)%icls) )THEN

  IF( IsLiquid(typeID(ityp)%icls) )THEN

    CALL get_liquid( p,pq )
    IF( p%ccb > SMALL )THEN
      rat = pq%ccbg/p%ccb
    ELSE
      rat = 0.0
    END IF
    IF( rat > SMALL )THEN
     IF( rat > ptmp2 )THEN
        CALL get_totalcc( p,pt )
        fac    = ptmp2/rat
        pt%cct = pt%cct - (1.0-fac)*p%cc
        p%cc   = MAX(fac*p%cc,p%ccb)
        pt%cct = MAX(pt%cct,p%cc,pt%cctb)
        CALL put_totalcc( p,pt )
      END IF
    END IF

    IF( ptmp > SMALL )THEN
      rat  = MIN(1.0,p%ccb/ptmp)
      p%cc = MAX(p%cc*rat,p%ccb)
    END IF

  ELSE IF( p%cc > 0.0 .AND. ptmp > SMALL )THEN

    CALL get_totalcc( p,pt )
    rat  = MIN(1.0,(pt%cctb-p%ccb)/ptmp)
    p%cc = MAX(pt%cct - (pt%cct-p%cc)*rat,p%ccb)

  END IF

ELSE IF( IsParticle(material(imat)%icls) .AND. ptmp > SMALL )THEN

  CALL get_totalcc( p,pt )
  IF( pt%cctb > SMALL )THEN
    rat = p%ccb/pt%cctb
  ELSE
    rat = 0.0
  END IF
  IF( rat > SMALL )THEN
    IF( rat > ptmp )THEN
      fac = ptmp/rat
    ELSE
      fac = 1.
    END IF
    pt%cct = MAX(fac*pt%cct,p%cc)
  END IF
  pt%cct = MAX(pt%cct,pt%cctb)
  CALL put_totalcc( p,pt )

END IF

IF( p%cc >= 0.0 )THEN
  p%cc = MAX(p%cc,p%ccb)
  IF( typeID(ityp)%ltot )THEN
    CALL get_totalcc( p,pt )
    pt%cct = MAX(pt%cct,pt%cctb)
    CALL put_totalcc( p,pt )
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE InitInterMC( is,ie )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: is    !Start puff no,
INTEGER, INTENT( IN ) :: ie    !End puff no.

INTEGER i, nn

nn = ie - is + 1

DO i = 1,mat_mc%nMCtype
  SELECT CASE( mat_mc%type(i) )

  CASE( MC_CHEM )
      CALL InitChemCmax( mat_mc%ID(i),is,ie )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE DEFAULT

  END SELECT

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE ResetInterMC( ipuf,puffi )

USE scipuff_fi
USE error_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ipuf
TYPE( puff_str ), INTENT( INOUT ) :: puffi

INTEGER mcID

mcID = typeID(puffi%ityp)%mcID
IF( mcID > 0 )THEN
  IF( mat_mc%type(mcID) == MC_CHEM )THEN
    CALL ResetCstar( mat_mc%ID(mcID),ipuf,puffi )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE ExitInterMC()

USE scipuff_fi
USE error_fi

IMPLICIT NONE

INTEGER i

DO i = 1,mat_mc%nMCtype
  SELECT CASE( mat_mc%type(i) )

    CASE( MC_CHEM )
      CALL ClearChemCmax( mat_mc%ID(i) )

    CASE DEFAULT

  END SELECT

END DO

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE GetInterMC1( p )

USE scipuff_fi
USe inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

mcID = typeID(p%ityp)%mcID
IF( mcID > 0 )THEN
  IF( mat_mc%type(mcID) == MC_CHEM )THEN
    ID = mat_mc%ID(mcID)
    CALL GetChemInter1( ID,p )
  END IF
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE GetInterMC2( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  ID = mat_mc%ID(mcID)
  CALL GetChemInter2( ID,p )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE PutInterMC1( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  ID = mat_mc%ID(mcID)
  CALL PutChemInter1( ID,p )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE PutInterMC2( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  ID = mat_mc%ID(mcID)
  CALL PutChemInter2( ID,p )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE InitContMC1( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  ID = mat_mc%ID(mcID)
  CALL InitChemCont1( ID,p  )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE InterMC( ipuf,jpuf,puffi,puffj )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER,          INTENT( IN    ) :: ipuf, jpuf
TYPE( puff_str ), INTENT( INOUT ) :: puffi, puffj

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  CALL InterChem( ipuf,jpuf,puffi,puffj )
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE ClearInterMC( p )

USE scipuff_fi
USE inter_fi
USE chem_fi
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

INTEGER i

mcID = typeID(p%ityp)%mcID

IF( mat_mc%type(mcID) == MC_CHEM )THEN
  ID = mat_mc%ID(mcID)
  chem => chemMC(ID,myTid)
  IF( chem%nStar > 0 )THEN
    CALL GetChemAux( ID,p )
    DO i = 1,chem%nStar
      chem%star(i)%s%conc = 0.0
    END DO
    vol   = 0.0
    vself = 0.0
    CALL PutChemAux( ID,p )
  END IF
END IF

RETURN
END



