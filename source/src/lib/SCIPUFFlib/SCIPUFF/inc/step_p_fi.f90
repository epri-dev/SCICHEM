MODULE step_p_fi

  USE puffstruct_fd

  SAVE

! --- Local variables for STEP_P routines

  REAL, PARAMETER :: HSCALE  = 8000.     ! Atmospheric scale height
  REAL, PARAMETER :: WMX     = 0.8       ! Well-mixed criterion sz/zinv
  REAL, PARAMETER :: ZIFAC   = 1.1       ! Zi-split criterion (delta Zi)
  REAL, PARAMETER :: SZFAC   = 3.0       ! Zi-split criterion (height)
  REAL, PARAMETER :: ZIDEL   = 10.0      ! Zi-split criterion (height)

  REAL(8) xsav, ysav, lonsav

  REAL xmap, ymap, hp, hx, hy, area_fac, sz, zbar, ztop, zlim
  REAL cnew, csav, zsav, hsav, hxsav, hysav, zisav
  REAL uubt, vvbt, uvbt
  REAL si, si2, sv, qosi_cc, qi, qvi, qtot, fac_diss
  REAL rhod, vfall, sigvd, vdry, vdtot, cmin2
  REAL xuct, xvct, yvct, zwct, tauc, taur, fwash
  REAL siq, svq, aqoszt, qs
  REAL aqsosyt, aqbosyt, aqlosyt
  REAL wpuff, vel2
  REAL aspect, ztest, zexp_dos
  REAL dts, stest
  REAL udyn, vdyn, wdyn, tdyn, bdyn, ddyn, fdyn, zdyn
  REAL tauw, difp, gt0, bv, gamma, ti
  REAL tscale
  REAL rhogas, rhopuff
  REAL splitf

  REAL dtstep

  INTEGER imat
  REAL iSplit
  INTEGER iSkew
  INTEGER ityppr
  REAL    skewness

  LOGICAL lzinv, lsrf, ldos, ltot, lcap, lblcap
  LOGICAL lscale
  LOGICAL pole_transition
  LOGICAL ldense

  TYPE( puff_str ), POINTER :: washPuff
  INTEGER npuf_thread
!$OMP THREADPRIVATE(npuf_thread)
  TYPE puff_ptr
    TYPE( puff_str), DIMENSION(:), POINTER :: p
  END TYPE puff_ptr
  LOGICAL                                          useTemp
  INTEGER,          DIMENSION(:,:), ALLOCATABLE :: puffSplit
  INTEGER,          DIMENSION(:),   ALLOCATABLE :: nDimTemp
  INTEGER,          DIMENSION(:),   ALLOCATABLE :: npufTemp
  TYPE( puff_ptr ), DIMENSION(:),   ALLOCATABLE, TARGET :: puffTemp
  INTEGER myTId
  INTEGER, DIMENSION(:), ALLOCATABLE :: iPass
  COMMON /step_p_fi_cmn/ myTId
  COMMON /step_p_fi_cmn/ xmap, ymap, hp, hx, hy, area_fac, sz, zbar, ztop, zlim
  COMMON /step_p_fi_cmn/ cnew, csav, zsav, xsav, ysav, hsav, hxsav, hysav, zisav, lonsav
  COMMON /step_p_fi_cmn/ uubt, vvbt, uvbt
  COMMON /step_p_fi_cmn/ si, si2, sv, qosi_cc, qi, qvi, qtot, fac_diss
  COMMON /step_p_fi_cmn/ rhod, vfall, sigvd, vdry, vdtot, cmin2
  COMMON /step_p_fi_cmn/ xuct, xvct, yvct, zwct, tauc, taur, fwash
  COMMON /step_p_fi_cmn/ siq, svq, aqoszt, qs
  COMMON /step_p_fi_cmn/ aqsosyt, aqbosyt, aqlosyt
  COMMON /step_p_fi_cmn/ wpuff, vel2
  COMMON /step_p_fi_cmn/ aspect, ztest, zexp_dos
  COMMON /step_p_fi_cmn/ dts, stest
  COMMON /step_p_fi_cmn/ udyn, vdyn, wdyn, tdyn, bdyn, ddyn, fdyn, zdyn
  COMMON /step_p_fi_cmn/ tauw, difp, gt0, bv, gamma, ti
  COMMON /step_p_fi_cmn/ tscale
  COMMON /step_p_fi_cmn/ rhogas, rhopuff

  COMMON /step_p_fi_cmn/ dtstep

  COMMON /step_p_fi_cmn/ imat

  COMMON /step_p_fi_cmn/ iSkew, ityppr
  COMMON /step_p_fi_cmn/ skewness

  COMMON /step_p_fi_cmn/ lzinv, lsrf, ldos, ltot, lcap, lblcap
  COMMON /step_p_fi_cmn/ lscale
  COMMON /step_p_fi_cmn/ pole_transition
  COMMON /step_p_fi_cmn/ ldense

  COMMON /step_p_fi_cmn/ washPuff

!$OMP THREADPRIVATE(/step_p_fi_cmn/)

END MODULE step_p_fi

MODULE grdlock_fi
  INTEGER, PARAMETER :: RMV_ADD      = 1
  INTEGER, PARAMETER :: RMV_DZ       = 2
  INTEGER, PARAMETER :: SET_ADD      = 3
  INTEGER, PARAMETER :: SET_DZ       = 4
  INTEGER, PARAMETER :: WAIT_DZ      = 5
  INTEGER, PARAMETER :: WAIT_ADD     = 6
  INTEGER, PARAMETER :: INIT_GRDLOCK = 7
  INTEGER, PARAMETER :: CLR_GRDLOCK  = 8
  INTEGER, PARAMETER :: CHK_DZ       = 9
  INTEGER, PARAMETER :: CHK_ADD      = 10

  LOGICAL, DIMENSION(:,:), ALLOCATABLE, TARGET :: gridlock
  INTEGER, DIMENSION(:,:), ALLOCATABLE, TARGET :: auxlock

  INTEGER myLockID

  COMMON/ grdlock_cmn / myLockID
!$OMP THREADPRIVATE(/grdlock_cmn/)

  CONTAINS

  LOGICAL RECURSIVE FUNCTION ManageGrdLock( threadID,lockID,functionID ) RESULT( ok )
!DIR$ OPTIMIZE:0
  IMPLICIT NONE

  INTEGER, INTENT( IN ) :: threadID
  INTEGER, INTENT( IN ) :: lockID
  INTEGER, INTENT( IN ) :: functionID

  INTEGER alloc_stat

  SELECT CASE( functionID )
    CASE( INIT_GRDLOCK )
      ok = .NOT.ALLOCATED(gridlock)
      IF( ok )THEN
        ALLOCATE( gridlock(0:threadID,lockID),STAT=alloc_stat )
        ok = alloc_stat==0
        IF( ok )THEN
          gridlock = .FALSE.
        END IF
      END IF
    CASE( CLR_GRDLOCK )
      ok = ALLOCATED(gridlock)
      IF( ok )THEN
        DEALLOCATE( gridlock,STAT=alloc_stat )
        ok = alloc_stat==0
      END IF
    CASE DEFAULT
      ok = ALLOCATED(gridlock)
      IF( ok )THEN
        SELECT CASE( functionID )
        CASE( RMV_ADD )
!$OMP ATOMIC WRITE
            gridlock(threadID,lockID) = .FALSE.
!$OMP FLUSH( gridlock )
          CASE( RMV_DZ )
!$OMP ATOMIC WRITE
            gridlock(threadID,lockID) = .TRUE.
!$OMP ATOMIC WRITE
            gridlock(0,lockID) = .FALSE.
!$OMP FLUSH( gridlock )
          CASE( WAIT_DZ )
            DO WHILE ( gridlock(0,lockID) )
!$OMP FLUSH( gridlock )
              CONTINUE
            END DO
          CASE( WAIT_ADD )
            DO WHILE( ANY(gridlock(1:,lockID)) )
!$OMP FLUSH( gridlock )
              CONTINUE
            END DO
          CASE( SET_ADD )
!$OMP CRITICAL (GRDLOCK)
            DO WHILE( gridlock(0,lockID) )
!$OMP FLUSH( gridlock )
              CONTINUE
            END DO
!$OMP ATOMIC WRITE
            gridlock(threadID,lockID) = .TRUE.
!$OMP FLUSH( gridlock )
!$OMP END CRITICAL (GRDLOCK)
          CASE( SET_DZ )
!$OMP ATOMIC WRITE
            gridlock(threadID,lockID) = .FALSE.
!$OMP FLUSH( gridlock )
!$OMP CRITICAL (GRDLOCK)
!$OMP FLUSH( gridlock )
            ok = .NOT.gridlock(0,lockID)
            IF( ok )THEN
!$OMP ATOMIC WRITE
             gridlock(0,lockID) = .TRUE.
!$OMP FLUSH( gridlock )
            END IF
!$OMP END CRITICAL (GRDLOCK)
          CASE( CHK_DZ )
!$OMP ATOMIC READ
            ok = gridlock(0,lockID)
          CASE( CHK_ADD )
!$OMP ATOMIC READ
            ok =  gridlock(threadID,lockID)
          CASE DEFAULT
        END SELECT
      END IF
    END SELECT
  END FUNCTION ManageGrdLock

!----------------------------------------------------------------------------

  LOGICAL RECURSIVE FUNCTION ManageAuxLock( threadID,lockID,cellID,functionID ) RESULT( ok )
!DIR$ OPTIMIZE:0
  IMPLICIT NONE

  INTEGER, INTENT( IN ) :: threadID
  INTEGER, INTENT( IN ) :: lockID
  INTEGER, INTENT( IN ) :: cellID
  INTEGER, INTENT( IN ) :: functionID

  INTEGER alloc_stat

  SELECT CASE( functionID )
    CASE( INIT_GRDLOCK )
      ok = .NOT.ALLOCATED(auxlock)
      IF( ok )THEN
        ALLOCATE( auxlock(threadID,lockID),STAT=alloc_stat )
        ok = alloc_stat==0
        IF( ok )THEN
          auxlock = -1
        END IF
      END IF
    CASE( CLR_GRDLOCK )
      ok = ALLOCATED(auxlock)
      IF( ok )THEN
        DEALLOCATE( auxlock,STAT=alloc_stat )
        ok = alloc_stat==0
      END IF
    CASE DEFAULT
      ok = ALLOCATED(auxlock)
      IF( ok )THEN
        SELECT CASE( functionID )
        CASE( RMV_ADD )
!$OMP ATOMIC WRITE
            auxlock(threadID,lockID) = -1
!$OMP FLUSH( auxlock )
          CASE( SET_ADD )
!$OMP CRITICAL (AUXDATLOCK)
            DO WHILE( ANY(auxlock(:,lockID)==cellID) )
!$OMP FLUSH( auxlock )
              CONTINUE
            END DO
!$OMP ATOMIC WRITE
            auxlock(threadID,lockID) = cellID
!$OMP FLUSH( auxlock )
!$OMP END CRITICAL (AUXDATLOCK)
          CASE DEFAULT
        END SELECT
      END IF
    END SELECT
  END FUNCTION ManageAuxLock

END MODULE grdlock_fi
