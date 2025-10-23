!=======================================================================

RECURSIVE SUBROUTINE surface_dose( p,sdat,srfI,zsrf,sblk,spuff,stype,mask )

USE struct_fd
USE error_fi
USE surface_fd
USE srfparam_fd
USE matl_fi
USE type_fi

IMPLICIT NONE

TYPE( puff_str ),          INTENT( IN ) :: p
INTEGER,                   INTENT( IN ) :: srfI
INTEGER, DIMENSION(*),     INTENT( IN ) :: stype
REAL,                      INTENT( IN ) :: zsrf
REAL, DIMENSION(ISRF_TOT), INTENT( IN ) :: sdat
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN ) :: spuff
TYPE( mask_fd),       OPTIONAL,     INTENT( IN ) :: mask

INTERFACE
  SUBROUTINE puff_dos( zsrf,p,sdat,srfI,sblk,spuff,stype,mask )
    USE struct_fd
    USE surface_fd
    USE srfparam_fd
    TYPE( puff_str ),                   INTENT( IN ) :: p
    INTEGER,                            INTENT( IN ) :: srfI
    INTEGER, DIMENSION(*),              INTENT( IN ) :: stype
    REAL,                               INTENT( IN ) :: zsrf
    REAL, DIMENSION(ISRF_TOT),          INTENT( IN ) :: sdat
    TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
    TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN ) :: spuff
    TYPE( mask_fd),       OPTIONAL,     INTENT( IN ) :: mask
  END SUBROUTINE puff_dos
END INTERFACE

LOGICAL, EXTERNAL :: IsMultiDep

!------ calculate cdep at grid locations and increment dose

IF( spuff(p%ityp)%nblocks > 0 )THEN
  IF( sdat(ISRF_C) > 0. .OR. IsMultiDep(material(typeID(p%ityp)%imat)%icls) )THEN

    IF( PRESENT(mask) )THEN
      CALL puff_dos( zsrf,p,sdat,srfI,sblk,spuff,stype,mask )
    ELSE
      CALL puff_dos( zsrf,p,sdat,srfI,sblk,spuff,stype )
    END IF
    IF( nError /= NO_ERROR )GOTO 9999

  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE puff_dos( zsrf,p,sdat,srfI,sblk,spuff,stype,mask )

USE scipuff_fi
USE surface_fi
USE srfevap_fi
USE srfaux_fi
USE srfparam_fd
USE srfdos_fd
USE sagstr_fd
USE PtrGrdStrItf
USE step_p_fi, ONLY: myTid
USE grdlock_fi

IMPLICIT NONE

TYPE( puff_str ),          INTENT( IN ) :: p
INTEGER,                   INTENT( IN ) :: srfI
INTEGER, DIMENSION(*),     INTENT( IN ) :: stype
REAL,                      INTENT( IN ) :: zsrf
REAL, DIMENSION(ISRF_TOT), INTENT( IN ) :: sdat
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN ) :: spuff
TYPE( mask_fd),       OPTIONAL,     INTENT( IN ) :: mask

TYPE( SAGgrid_str ), POINTER :: srf
TYPE( srf_gauss_str ) :: s

INTEGER ng, n2, alloc_stat
LOGICAL lmask
LOGICAL myOK
INTEGER, DIMENSION(:), ALLOCATABLE :: ig, ig2
REAL,    DIMENSION(:), ALLOCATABLE :: cfac, cfac2

LOGICAL, EXTERNAL :: IsEvap


INTERFACE

  SUBROUTINE set_grid_puff( srf,s,stype )
    USE srfdos_fd
    USE sagstr_fd
    TYPE ( SAGgrid_str ),  POINTER         :: srf
    TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
    INTEGER, DIMENSION(*), INTENT( IN    ) :: stype
  END SUBROUTINE set_grid_puff

  SUBROUTINE grid_puff( srf,sblk,s,ng,ig,cfac,n2,ig2,cfac2 )
    USE srfdos_fd
    USE sagstr_fd
    USE surface_fd
    TYPE( SAGgrid_str ),                POINTER      :: srf
    TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
    TYPE( srf_gauss_str ),              INTENT( IN ) :: s
    INTEGER,                            INTENT( INOUT ) :: ng
    INTEGER, DIMENSION(MAX(ng,1)),      INTENT( INOUT ) :: ig
    REAL,    DIMENSION(MAX(ng,1)),      INTENT( INOUT ) :: cfac
    INTEGER,                            INTENT( INOUT ) :: n2
    INTEGER, DIMENSION(MAX(n2,1)),      INTENT( INOUT ) :: ig2
    REAL,    DIMENSION(MAX(n2,1)),      INTENT( INOUT ) :: cfac2
  END SUBROUTINE grid_puff

END INTERFACE

!----- set SAG grid structure

isDep = (srfI == srfdep) .AND. (nEvapBlk > 0)

srf => SAG_PtrGrdStr( srfI ) ! Associate "local" grid structure pointer
IF( .NOT.ASSOCIATED(srf))THEN
  nError   = UK_ERROR
  eRoutine = 'puff_dos'
  eMessage = 'Grid pointer not allocated'
  GOTO 9999
END IF

ALLOCATE( ig(srf%mxfld),ig2(srf%mxfld), &
          cfac(srf%mxfld),cfac2(srf%mxfld),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'puff_dos'
  eMessage = 'Error allocating grid'
  GOTO 9999
END IF
cfac = 0.0
cfac2 = 0.0
ig = 0
ig2 = 0

!------ Setup 2d gaussian

CALL set_2dgauss( zsrf,p,s )
IF( nError /= NO_ERROR )GOTO 9999

s%xbar = s%xbar - srf%xmin !define relative to surface grid
s%ybar = s%ybar - srf%ymin

!------ Put domain mask in gaussian structure

lmask = .FALSE.
IF( PRESENT(mask) )lmask = mask%xmin /= NOT_SET_R .AND. mask%ymin /= NOT_SET_R .AND. &
                           mask%xmax /= NOT_SET_R .AND. mask%ymax /= NOT_SET_R
IF( lmask )THEN
  s%xmins = mask%xmin - srf%xmin
  s%ymins = mask%ymin - srf%ymin
  s%xmaxs = mask%xmax - srf%xmin
  s%ymaxs = mask%ymax - srf%ymin
ELSE
  s%xmins = NOT_SET_R
  s%ymins = NOT_SET_R
  s%xmaxs = NOT_SET_R
  s%ymaxs = NOT_SET_R
END IF

!------ Set argmax (reduced for surface evap puffs)

CALL check_argmax( p,sblk,spuff,s )

!------ Set argmax (reduced for reverse projects)

IF( BTEST(run_mode,REVERSE_MODE) )s%argmax = 0.4*s%argmax

!------ Set grid level

CALL set_grid_puff( srf,s,stype )
IF( nError /= NO_ERROR .OR. s%gridlev < 0 )THEN
  myOK = ManageGrdLock( myTId,myLockID,RMV_ADD )
  GOTO 9999
END IF

!------ Set surface variables

CALL set_srf_var( p,sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )
IF( nError /= NO_ERROR .OR. ng+nbaux_srf <= 0 )THEN
  myOK = ManageGrdLock( myTId,myLockID,RMV_ADD )
  GOTO 9999
END IF

!------ Put 2d gaussian on grid

CALL grid_puff( srf,sblk,s,ng,ig,cfac,n2,ig2,cfac2 )
myOK = ManageGrdLock( myTId,myLockID,RMV_ADD )
IF( nError /= NO_ERROR )GOTO 9999

!------ Update secondary evaporation timestep

IF( IsEvap(typeID(p%ityp)%icls) )THEN
  CALL update_evap_step( sdat(ISRF_SL),s%gridlev )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(ig)    )DEALLOCATE( ig,STAT=alloc_stat )
IF( ALLOCATED(ig2)   )DEALLOCATE( ig2,STAT=alloc_stat )
IF( ALLOCATED(cfac)  )DEALLOCATE( cfac,STAT=alloc_stat )
IF( ALLOCATED(cfac2) )DEALLOCATE( cfac2,STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE check_argmax( p,sblk,spuff,s )

!------ check argmax and reduce for surface evaporation blocks

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE srfdos_fd

IMPLICIT NONE

TYPE( puff_str ),                   INTENT( IN )    :: p
TYPE( sfield_block ), DIMENSION(*), INTENT( IN )    :: sblk
TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN )    :: spuff
TYPE( srf_gauss_str ),              INTENT( INOUT ) :: s

INTEGER iptyp, k, iblk

iptyp = p%ityp

!------ Loop over surface blocks influenced by this puff

DO k = 1,spuff(iptyp)%nblocks

  iblk = spuff(iptyp)%iblk(k)

  IF( sblk(iblk)%type == SBLK_EVAP )THEN
    s%argmax  = 0.25*s%argmax
    scaleEvap = 1.0/(1.0-EXP(-s%argmax))
    EXIT
  END IF

END DO

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE set_grid_puff( srf,s,stype )
!DIR$ OPTIMIZE:0

USE srfdos_fd
USE sagstr_fd
USE sagdef_fd
USE constants_fd
USE utilsrf
USE scipuff_fi, ONLY: REVERSE_MODE,run_mode,nthreads
USE step_p_fi, ONLY: myTId
USE grdlock_fi
USE files_fi

IMPLICIT NONE

TYPE ( SAGgrid_str ),  POINTER         :: srf
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
INTEGER, DIMENSION(*), INTENT( IN    ) :: stype

INTEGER nxs, nys, mlev, nlev, j1, j2, i2, n1, m1
INTEGER i, ncell, irv, nxx, nyy
LOGICAL myOK

REAL    dxs, dys, xlam, del, volp, volg
REAL    dfac, delx, dely, xfac

INTEGER, EXTERNAL :: SAG_Dezone
         EXTERNAL :: DezoneCell
INTEGER, EXTERNAL :: SAG_DezoneAux8
         EXTERNAL :: DezoneCellEvap

s%gridlev = -1

nxs  = srf%nx
nys  = srf%ny
dxs  = srf%dx
dys  = srf%dy

xlam = 0.5* (s%axx + s%ayy + SQRT((s%axx-s%ayy)**2+4.*s%axy**2))
del  = SQRT(0.25/xlam)
IF( BTEST(run_mode,REVERSE_MODE) )del  = 0.5*del
mlev = MAX(INT(LOG(del/MAX(dxs/s%xmap,dys/s%ymap))*RLOGR2),0)

volp = s%voli

myOK = ManageGrdLock( myTId,myLockID,SET_ADD ) !waits for dezone

mlevLoop : DO !loop only if mlev changes due to dezoning

!------ Set grid parameters; reset if dezoning
  IF( mlev > ABS(srf%maxlev) )THEN
!$OMP ATOMIC WRITE
    srf%maxlev = ABS(srf%maxlev)
    mlev       = srf%maxlev
  END IF

  nlev = 2**mlev
  dfac = 1.0/FLOAT(nlev)
  delx = dxs*dfac
  dely = dys*dfac

  volg = s%xmap*s%ymap/(delx*dely)

!------ Set parameters for grid sweep

  xfac = s%argmax / (1.0 - s%axy*s%axy/s%axx/s%ayy)
  s%yp = s%ymap*SQRT( xfac/s%ayy )

  m1 = nxs*nlev
  n1 = nys*nlev

  j1 = MAX( INT( (s%ybar - s%yp)/dely ),1 )
  j2 = MIN( INT( (s%ybar + s%yp)/dely) + 1,n1 )

  IF( j2 < j1 )GOTO 9999

  s%xp = SQRT( s%argmax/s%axx ) * s%xmap
  i2   = MIN( m1,2*INT( s%xp/delx )+1 )

!------ Compute area of ellipse and
!       estimate number of cells

  xfac = PI * s%argmax / SQRT(s%axx*s%ayy-s%axy**2)

  ncell = xfac*s%xmap*s%ymap / (delx*dely)

!------ Check if dezoning required based on estimate of no. of cells
!       N.B. Assume puffs in parallel threads are adding the same no. of cells

  nxx = MaxNumCells( i2 )
  nyy = MaxNumCells( j2-j1+1 )
!  ncell = nxx*nyy ** now base initial count on ellipse area

  DO i = 1,mlev
    nxx = MaxNumCells( nxx/2 )
    nyy = MaxNumCells( nyy/2 )
    ncell = ncell + nxx*nyy
  END DO

!------ Reduce level if this puff exceeds maxgrd by itself and
!       the grid has not been previously refined
  IF( ncell*nthreads > 0.9*srf%mxgrd .AND. srf%ncells == srf%nx*srf%ny )THEN
    mlev = mlev - 1
    IF( mlev ==  0 )EXIT mlevLoop
    CYCLE mlevLoop
  END IF

  IF( srf%ncells+ncell*nthreads < srf%mxgrd )EXIT mlevLoop !don't need to dezone
  IF( mlev == 0                             )EXIT mlevLoop !can't dezone anymore

  myOK = ManageGrdLock( myTId,myLockID,SET_DZ )
  IF( myOK )THEN
    myOK = ManageGrdLock( myTId,myLockID,WAIT_ADD )
  ELSE
    myOK = ManageGrdLock( myTId,myLockID,SET_ADD ) !waits for dezone
    CYCLE mlevLoop
  END IF

  dezoneLoop : DO
    CALL init_dezone( srf%nvart,stype )
    IF( nError /= NO_ERROR )THEN
      myOK = ManageGrdLock( myTId,myLockID,RMV_DZ )
      GOTO 9999
    END IF

    nDezoneSrf = nDezoneSrf + 1

    IF( isDep )THEN
      irv = SAG_DezoneAux8( srf%id,0,nEvapBlk*nEvapFld,EvapTemp,DezoneCellEvap )
    ELSE
      irv = SAG_Dezone( srf%id,0,DezoneCell )
    END IF
    CALL exit_dezone()
    IF( irv /= SAG_OK )THEN
      myOK = ManageGrdLock( myTId,myLockID,RMV_DZ )
      nError   = UK_ERROR
      eRoutine = 'set_grid_puff'
      eMessage = 'Error dezoning surface grid'
      GOTO 9999
    END IF

!$OMP FLUSH (srf)
    IF( srf%maxlev < mlev )EXIT dezoneLoop !need to redefine parameters if mlev changes

    IF( srf%ncells+ncell*nthreads < srf%mxgrd )THEN !no further dezoning and
                                                    !mlev not changed
      myOK = ManageGrdLock( myTId,myLockID,RMV_DZ )
      EXIT mlevLoop
    END IF
  END DO dezoneLoop

  myOK = ManageGrdLock( myTId,myLockID,RMV_DZ )

END DO mlevLoop

s%gridlev = mlev

IF( volp > volg )THEN
  s%voli = volg
  s%facv = (volp/volg-1.0)
ELSE
  s%voli = volp
  s%facv = 0.
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE grid_puff( srf,sblk,s,ng,ig,cfac,n2,ig2,cfac2 )

USE scipuff_fi
USE srfdos_fd
USE sagstr_fd
USE accumsrf
USE utilsrf
USE surface_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),                POINTER      :: srf
TYPE( sfield_block ), DIMENSION(*), INTENT( IN ) :: sblk
TYPE( srf_gauss_str ),              INTENT( IN ) :: s
INTEGER,                            INTENT( INOUT ) :: ng
INTEGER, DIMENSION(MAX(ng,1)),      INTENT( INOUT ) :: ig
REAL,    DIMENSION(MAX(ng,1)),      INTENT( INOUT ) :: cfac
INTEGER,                            INTENT( INOUT ) :: n2
INTEGER, DIMENSION(MAX(n2,1)),      INTENT( INOUT ) :: ig2
REAL,    DIMENSION(MAX(n2,1)),      INTENT( INOUT ) :: cfac2

INTEGER nxs, nys, mlev, nlev, j1, j2, i1, i2, n1, m1
INTEGER i, j, k, icell, iout, ib, iblk, nsum
INTEGER i1S, i2S, j1S, j2S
REAL    dxs, dys, dfac, delx, dely, cxy, xplus, xminus, x, y, xp, yp
REAL    arg, darea, csum, fac, ym, del
LOGICAL interior

REAL, DIMENSION(MAX(ng,1)) :: ccell
INTEGER ibse, nse, ig0, igp, igchk
INTEGER, DIMENSION(nEvapFld) :: igse
REAL,    DIMENSION(nEvapFld) :: cfacse
REAL,    DIMENSION(nEvapFld) :: ccellse

!----- save grid parameters

nxs  = srf%nx
nys  = srf%ny
dxs  = srf%dx
dys  = srf%dy

mlev = s%gridlev
nlev = 2**mlev
dfac = 1.0/FLOAT(nlev)
delx = dxs*dfac
dely = dys*dfac

!------ Set parameters for grid sweep

m1 = nxs*nlev
n1 = nys*nlev

IF( s%yminS /= NOT_SET_R )THEN
  j1S = MAX(INT(s%yminS/dely),1)
ELSE
  j1S = 1
END IF
IF( s%ymaxS /= NOT_SET_R )THEN
  j2S = MIN(INT(s%ymaxS/dely)+1,n1)
ELSE
  j2S = n1
END IF
j1 = MAX(INT((s%ybar-s%yp)/dely)  ,j1S)
j2 = MIN(INT((s%ybar+s%yp)/dely)+1,j2S)
IF( j1 > j2 )RETURN

xplus  = s%xbar + s%xp
xminus = s%xbar - s%xp
cxy    = s%axy/s%axx

csum = 0.
nsum = MAX(ng,1)       !Check at least the first variable

IF( nblkse > 0 )THEN
  ibse  = evapTempID(nblkse)
  nse   = 0
  igchk = ig(ngse)
  ig0   = ig(ngse) - 1
  igp   = 0
  DO WHILE( igp < nEvapFld .AND. ngse+nse <= ng )
    igp  = igp + ig(ngse+nse) - ig0
    IF( igp > nEvapFld )EXIT
    ig0 = ig(ngse+nse)
    nse = nse + 1
    cfacse(nse) = cfac(ngse+nse-1)
    igse(nse)   = (ibse-1)*nEvapFld + igp
  END DO
  DO i = ngse,ng-nse
    ig(i)   =   ig(i+nse)
    cfac(i) = cfac(i+nse)
  END DO
  DO i = 1,n2
    IF( ig2(i) > ngse )ig2(i) = ig2(i) - nse
  END DO
  ng   = ng - nse
  nsum = MAX(ng,1)      !Reset
ELSE
  nse = 0
END IF

IF( s%xminS /= NOT_SET_R )THEN
  i1S = MAX(INT(s%xminS/delx),1)
ELSE
  i1S = 1
END IF
IF( s%xmaxS /= NOT_SET_R )THEN
  i2S = MIN(INT(s%xmaxS/delx)+1,m1)
ELSE
  i2S = m1
END IF
interior = (j1 > j1S) .AND. (j2 < j2S)

DO j = j1,j2

  y  = (FLOAT(j)-0.5)*dfac
  yp = (y*dys-s%ybar)/s%ymap
  x  = -cxy * yp * s%xmap

  i1 = INT((xminus + x)/delx)
  i2 = INT((xplus  + x)/delx) + 1

  IF( BTEST(run_mode,REVERSE_MODE) )THEN
    i1 = MAX(i1,i1S)
    i2 = MIN(i2,i2S)
    IF( i1 > i2 )CYCLE
  END IF

  IF( .NOT.global_lon )THEN
    i1 = MAX(i1, 1)
    i2 = MIN(i2,m1)
    interior = interior .AND. (i1 > 1) .AND. (i2 < m1)
  END IF

  IF( i2 >= i1 )THEN

    icell = 0

    DO i = i1,i2

      x   = (FLOAT(i)-0.5)*dfac
      xp  = (x*dxs-s%xbar)/s%xmap
      arg = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( global_lon )CALL SetGlobalGrid( x,nxs )

      IF( arg < s%argmax )THEN

        fac = EXP(-arg)
        DO k = 1,nsum
          ccell(k) = fac*cfac(k)
        END DO
        IF( n2 > 0 )THEN
          DO k = 1,n2
            ccell(ig2(k)) = ccell(ig2(k)) + cfac2(k)*fac*fac
          END DO
        END IF
        IF( nse > 0 )THEN
          DO k = 1,nse
            ccellse(k) = fac*cfacse(k)
          END DO
        END IF
        csum = csum + ccell(1)
        IF( ABS(fac*s%cmax) > s%conc_min )THEN
          IF( nse > 0 )THEN
            CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,iout=iout,nse=nse,igse=igse,ccellse=ccellse, &
                             isDep=isDep,igchk=igchk )
          ELSE
            CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,iout=iout,isDep=isDep )
          END IF
          IF( nError /= NO_ERROR )GOTO 9998
          IF( nbaux_srf > 0 .AND. iout > 0 )THEN
            DO ib = 1,nbaux_srf
              iblk = ibaux_srf(ib)
              CALL ProcessSrfAux( srf,ib,fac,iout,sblk(iblk) )
              IF( nError /= NO_ERROR )GOTO 9999
            END DO
          END IF
        ELSE
          icell = 0
        END IF

      ELSE
        icell = 0
      END IF ! if arg .lt. ARGMAX

    END DO ! do i=i1,i2

    IF( i2-i1 <= 1 )THEN
      DO k = 1,nsum
        ccell(k) = 0.0
      END DO
      i = MAX(i1-1,1)
      x = (FLOAT(i)-0.5)*dfac
      IF( global_lon )CALL SetGlobalGrid( x,nxs )
      icell = 0
      CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,isDep=isDep )
      IF( nError /= NO_ERROR )GOTO 9998
      i = MIN(i2+1,m1)
      x = (FLOAT(i)-0.5)*dfac
      IF( global_lon )CALL SetGlobalGrid( x,nxs )
      icell = 0
      CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,isDep=isDep )
      IF( nError /= NO_ERROR )GOTO 9998
    END IF

  END IF

END DO ! do j=j1,j2

IF( j2-j1 <= 1 )THEN

  DO k = 1,ng
    ccell(k) = 0.0
  END DO
  IF( i2-i1 <= 1 )THEN
    i1 = MAX(i1-1,1 )
    i2 = MIN(i2+1,m1)
  END IF

  j  = MAX(j1-1,1)
  ym = (FLOAT(j)-0.5)*dfac
  j  = MIN(j2+1,n1)
  yp = (FLOAT(j)-0.5)*dfac

  DO i = i1,i2
    x  = (FLOAT(i)-0.5)*dfac
    IF( global_lon )CALL SetGlobalGrid( x,nxs )
    icell = 0
    CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,isDep=isDep )
    IF( nError /= NO_ERROR )GOTO 9998
    x  = (FLOAT(i)-0.5)*dfac
    IF( global_lon )CALL SetGlobalGrid( x,nxs )
    icell = 0
    CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,isDep=isDep )
    IF( nError /= NO_ERROR )GOTO 9998
  END DO

END IF

!------ Adjust mass deposition if necessary

IF( interior )THEN

  darea = delx*dely/(s%xmap*s%ymap)
  IF( ABS(csum) < 0.9*ABS(s%pmass)/darea )THEN
    j = MAX(NINT(s%ybar/dely),1)
    y = (FLOAT(j)-0.5)*dfac
    i = MAX(NINT(s%xbar/delx),1)
    x = (FLOAT(i)-0.5)*dfac
    IF( global_lon )CALL SetGlobalGrid( x,nxs )

    fac = (ABS(s%pmass)/darea - ABS(csum)) / ABS(cfac(1))

    DO k = 1,nsum
      ccell(k) = fac*cfac(k)
    END DO
    IF( nse > 0 )THEN
      DO k = 1,nse
        ccellse(k) = fac*cfacse(k)
      END DO
    END IF

    icell = 0
    IF( nse > 0 )THEN
      CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,iout=iout,nse=nse,igse=igse,ccellse=ccellse, &
                       isDep=isDep,igchk=igchk )
    ELSE
      CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell,iout=iout,isDep=isDep )
    END IF
    IF( nError /= NO_ERROR )GOTO 9998

    IF( nbaux_srf > 0 .AND. iout > 0 )THEN
      DO ib = 1,nbaux_srf
        iblk = ibaux_srf(ib)
        CALL ProcessSrfAux( srf,ib,fac,iout,sblk(iblk) )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END IF

  END IF

END IF !interior

9999 CONTINUE

RETURN

9998 CONTINUE

CALL get_delmin( srf,del,k )
eRoutine = 'grid_puff'
eMessage = 'Too many surface cells'
WRITE(eInform,*)'Current minimum grid size is ',del,'m'
eAction  = 'Try increasing DELMIN'
GOTO 9999

END

!===============================================================================
RECURSIVE SUBROUTINE set_srf_var( p,sdat,sblk,spuff,s,ng,ig,cfac,n2,ig2,cfac2 )

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE srfdos_fd
USE srfaux_fi
USE UtilMtlAux

IMPLICIT NONE

TYPE( puff_str ),                   INTENT( IN    ) :: p
REAL,                 DIMENSION(*), INTENT( IN    ) :: sdat
TYPE( sfield_block ), DIMENSION(*), INTENT( IN    ) :: sblk
TYPE( sfield_puff ),  DIMENSION(*), INTENT( IN    ) :: spuff
TYPE( srf_gauss_str ),              INTENT( INOUT ) :: s
INTEGER,                            INTENT( OUT   ) :: ng
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: ig
REAL,                 DIMENSION(*), INTENT( OUT   ) :: cfac
INTEGER,                            INTENT( OUT   ) :: n2
INTEGER,              DIMENSION(*), INTENT( OUT   ) :: ig2
REAL,                 DIMENSION(*), INTENT( OUT   ) :: cfac2

INTEGER iptyp
INTEGER k, n0, iblk, type, ifld
INTEGER imat, nsg, ibWetInhale, i, j, ib
INTEGER icomp
REAL    rat, fac1, fac2
REAL    ccs, fac
REAL    d_drop, w_area
LOGICAL lEvap
REAL    cfo
LOGICAL ltot

TYPE( puff_totalcc ) pt
TYPE( puff_aerosol ) pa
TYPE( puff_material ) pmatl
TYPE( part_material ) pmatl_part

LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: IsAerosol
iptyp = p%ityp

nblkse = 0

! ---- Set surface fields

s%pmass  = sdat(ISRF_C )*(p%cfo)
s%cmax   = sdat(ISRF_C0)
s%tscale = sdat(ISRF_TSCL)

! --- Loop over blocks

ng        = 0
n2        = 0
nbaux_srf = 0

lEvap = .FALSE.

DO k = 1,spuff(iptyp)%nblocks

  iblk = spuff(iptyp)%iblk(k)
  ifld = sblk(iblk)%field
  type = sblk(iblk)%type
  ltot = BTEST(sblk(iblk)%flags,SFLAG_TOT) .AND. typeID(iptyp)%ltot

  IF( IsAerosol(typeID(iptyp)%icls) )THEN
    IF( BTEST(sblk(iblk)%flags,SFLAG_LIQ) )THEN
      CALL get_aerosol( p,pa )
      cfo = p%cfo * pa%fl
      IF( k == 1 )s%pmass = s%pmass * pa%fl
    ELSE IF( BTEST(sblk(iblk)%flags,SFLAG_VAP) )THEN
      CALL get_aerosol( p,pa )
      cfo = p%cfo * (1.0 - pa%fl)
      IF( k == 1 )s%pmass = s%pmass * (1.0 - pa%fl)
    ELSE
      cfo = p%cfo
    END IF
  ELSE
    cfo = p%cfo
  END IF

! --- Mean

  ng = ng + 1
  n0 = ng
  ig(ng)   = ifld
  cfac(ng) = s%voli*sdat(ISRF_C)*cfo !g/m**2

#if defined(LINUX)
  IF( cfac(n0) < TINY(cfac(1)) .AND. type /= SBLK_MULTI_DEP .AND. &
                                     type /= SBLK_PLOT_MC )THEN
#else
  IF( cfac(n0) < TINY(cfac) .AND. type /= SBLK_MULTI_DEP .AND. &
                                  type /= SBLK_PLOT_MC )THEN
#endif
    ng = n0 - 1 !No contribution - remove from list
    CYCLE
  END IF

! --- Var

  ng = ng + 1
  ig(ng) = ifld + 1

  IF( ltot )THEN

    cfac(ng) = s%voli*sdat(ISRF_CCT)*(cfo**2) !g**2/m**4
    IF( s%facv > SMALL )THEN
      CALL get_totalcc( p,pt )
      CALL inter_self( p,ccs )
      IF( nError /= NO_ERROR )GOTO 9999
      fac      = s%facv*MIN(1.E8,pt%cctb/MAX(ccs,1.E-30))
      cfac(ng) = cfac(ng) + fac*cfac(n0)**2
    END IF

  ELSE

    cfac(ng) = s%voli*sdat(ISRF_CC)*(cfo**2) !g**2/m**4
    IF( s%facv > SMALL )THEN
      CALL inter_self( p,ccs )
      IF( nError /= NO_ERROR )GOTO 9999
      fac      = s%facv*MIN(1.E8,p%ccb/MAX(ccs,1.E-30))
      cfac(ng) = cfac(ng) + fac*cfac(n0)**2
    END IF

  END IF

! ----- Remaining fields depend on block type

  SELECT CASE( type )

    CASE( SBLK_STD )
      ng = ng + 1
      ig(ng)   = ifld + 2
      cfac(ng) = cfac(n0+1)*sdat(ISRF_SL) !m-g**2/m**4


    CASE( SBLK_EVAP )   !NB - pq is in srfaux_fi
      CALL set_srf_evap( p,pq,d_drop,w_area )
      ng = ng + 1
      ig(ng)   = ifld + 2
      cfac(ng) = cfac(n0+1)*sdat(ISRF_SL) !Scale
      ng = ng + 1
      ig(ng)   = ifld + 3
      cfac(ng) = cfac(n0)*d_drop !Mass-weighted drop size
      ng = ng + 1
      ig(ng)   = ifld + 4
      cfac(ng) = cfac(n0)*w_area !N.B. Wetted area is not mass-weighted
      IF( substrate_type /= 0 )THEN
        ng = ng + 1
        ig(ng) = ifld + 5
        cfac(ng) = cfac(n0)      !Surface mass
        IF( BTEST(run_mode,EVAP2D) )THEN
          ng = ng + 1
          ig(ng)   = ifld + 7
          cfac(ng) = cfac(n0)*d_drop*d_drop ! mass-weighted drop size**2
        END IF
      ELSE  !substrate_type == 0
        IF( BTEST(run_mode,EVAP2D) )THEN
          ng = ng + 1
          ig(ng)   = ifld + 5
          cfac(ng) = cfac(n0)*d_drop*d_drop ! mass-weighted drop size**2
        END IF
      END IF  !substrate_type /= 0
      lEvap = .TRUE.

      nblkse = iblk
      ngse   = n0

    CASE( SBLK_MULTI_DOS )
      ng = ng + 1
      ig(ng)   = ifld + 2
      cfac(ng) = cfac(n0+1)*sdat(ISRF_SL) !m-g**2/m**4

      CALL SetSrfDosMC( p,sdat,s,cfac,ng,ig,ltot )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( SBLK_MULTI_DEP )
      ng = ng + 1
      ig(ng)   = ifld + 2
      cfac(ng) = cfac(n0+1)*sdat(ISRF_SL) !m-g**2/m**4

      CALL SetSrfDepMC( p,sdat,s,cfac,ng,ig,ltot )
      IF( nError /= NO_ERROR )GOTO 9999

    CASE( SBLK_PLOT_MC )

      IF( p%cfo > 0.0 )THEN
        rat = cfac(2)/(cfac(1)*cfac(1))
      ELSE
        fac1 = s%voli*sdat(ISRF_C)
        IF( ltot )THEN
          fac2 = s%voli*sdat(ISRF_CCT)
          IF( s%facv > SMALL )THEN
            CALL get_totalcc( p,pt )
            CALL inter_self( p,ccs )
            IF( nError /= NO_ERROR )GOTO 9999
            fac  = s%facv*MIN(1.E8,pt%cctb/MAX(ccs,1.E-30))
            fac2 = fac2 + fac*fac1**2
          END IF
        ELSE
          fac2 = s%voli*sdat(ISRF_CC)
          IF( s%facv > SMALL )THEN
            CALL inter_self( p,ccs )
            IF( nError /= NO_ERROR )GOTO 9999
            fac  = s%facv*MIN(1.E8,p%ccb/MAX(ccs,1.E-30))
            fac2 = fac2 + fac*fac1**2
          END IF
        END IF
        rat = fac2/(fac1*fac1)
      END IF

      icomp = sblk(iblk)%id/65536
      CALL SetPlotMC( p,icomp,sdat,s,cfac )
      IF( nError /= NO_ERROR )GOTO 9999

      cfac(2) = cfac(1)*cfac(1)*rat

    CASE( SBLK_PLOT_ADJ )
      ng = ng + 1
      ig(ng)   = ifld + 2
      cfac(ng) = cfac(n0+1)*p%si

      ng = ng + 1
      ig(ng)   = ifld + 3
      cfac(ng) = cfac(n0+1)*p%si2

      ng = ng + 1
      ig(ng)   = ifld + 4
      cfac(ng) = cfac(n0+1)*p%sv

    CASE( SBLK_EFFECTS )
      CALL set_srf_effects( p,sblk(iblk),sdat,s,cfac,n0,ifld )

      IF( cfac(n0) <= 0.0 )ng = n0 - 1  !Remove this block if no contributions

    CASE DEFAULT

  END SELECT

END DO

!--- If secondary evaporation material adjust for truncated gaussian
!--- See check_argmax

IF( lEvap )THEN
  DO k = 1,ng
    cfac(k) = scaleEvap*cfac(k)
  END DO
  DO k = 1,n2
    cfac2(k) = scaleEvap*cfac2(k)
  END DO
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetSrfDosMC( p,sdat,s,cfac,ng,ig,ltot )

USE scipuff_fi
USE surface_fi
USE srfdos_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
REAL, DIMENSION(*),    INTENT( IN    ) :: sdat
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac
INTEGER,               INTENT( INOUT ) :: ng
INTEGER, DIMENSION(*), INTENT( INOUT ) :: ig
LOGICAL,               INTENT( IN    ) :: ltot

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL SetChemSrfDosMC( p,mat_mc%ID(mcID),cfac,ng,ig,ig(1) )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetSrfDosMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetSrfDepMC( p,sdat,s,cfac,ng,ig,ltot )

USE scipuff_fi !struct_fd
USE surface_fi
USE srfparam_fd
USE srfdos_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
REAL, DIMENSION(*),    INTENT( IN    ) :: sdat
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac
INTEGER,               INTENT( INOUT ) :: ng
INTEGER, DIMENSION(*), INTENT( INOUT ) :: ig
LOGICAL,               INTENT( IN    ) :: ltot

INTEGER mcID

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL SetChemSrfDepMC( s,cfac,ng,ig )
  CASE DEFAULT
    nError   = UK_ERROR
    eMessage = 'Multicomponent error'
    eRoutine = 'SetSrfDepMC'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetPlotMC( p,icomp,sdat,s,cfac )

USE scipuff_fi !struct_fd
USE surface_fi
USE srfparam_fd
USE srfdos_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
INTEGER,               INTENT( IN    ) :: icomp
REAL, DIMENSION(*),    INTENT( IN    ) :: sdat
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac

INTEGER mcID
REAL    compMass, rat

mcID = typeID(p%ityp)%mcID

SELECT CASE( mat_mc%type(mcID) )
  CASE( MC_CHEM )
    CALL SetChemPlotMC( p,mat_mc%ID(mcID),icomp,compMass )
  CASE DEFAULT
    nError   = UK_ERROR
    eRoutine = 'SetPlotMC'
    eMessage = 'Multicomponent error'
    WRITE(eInform,'(A,I6)') 'Unknown multicomponent type :',mat_mc%type(mcID)
    GOTO 9999
END SELECT

rat = compMass/p%c

cfac(1) = s%voli*sdat(ISRF_C)*rat

!---  Reset surface mass integral and max puff concentration

s%pmass = sdat(ISRF_C )*rat
s%cmax  = sdat(ISRF_C0)*rat

9999 CONTINUE

RETURN
END

!-------------------------------------------------------------------------

SUBROUTINE SetChemPlotMC( p,ID,icomp,compMass )

USE chem_fi
USE error_fi
USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: p
INTEGER,          INTENT( IN  ) :: ID
INTEGER,          INTENT( IN  ) :: icomp
REAL,             INTENT( OUT ) :: compMass

CALL GetChemAux( ID,p )

compMass = chem%species(icomp)%mass

RETURN
END

!===============================================================================

SUBROUTINE set_srf_effects( p,block,sdat,s,cfac,n0,ifld )

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE srfdos_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( sfield_block ),  INTENT( IN    ) :: block
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
REAL, DIMENSION(*),    INTENT( INOUT ) :: cfac
REAL, DIMENSION(*),    INTENT( IN    ) :: sdat
INTEGER,               INTENT( IN    ) :: n0, ifld

INTEGER ieffect, id, iptyp, type

ieffect = block%id

type  = srf_effect(ieffect)%type
id    = srf_effect(ieffect)%id
iptyp = p%ityp

RETURN
END

!=======================================================================

SUBROUTINE grdose( t,dts )

USE surface_fi
USE utilsrf

IMPLICIT NONE

REAL, INTENT( IN ) :: t
REAL, INTENT( IN ) :: dts

INTEGER iblk

DO iblk = 1,nsrf_blocks

  IF( srf_block(iblk)%type == SBLK_EFFECTS )THEN

    CALL grdose_effect( dts,srf_block(iblk) )

  END IF

END DO

RETURN
END

!=======================================================================

SUBROUTINE grdose_effect( dts,block )

USE surface_fi
USE srfparam_fd

IMPLICIT NONE

REAL,                 INTENT( IN ) :: dts
TYPE( sfield_block ), INTENT( IN ) :: block

INTEGER ieffect, type, id, ifld

ieffect = block%id
ifld    = block%field
type    = srf_effect(ieffect)%type
id      = srf_effect(ieffect)%id

RETURN
END

!===============================================================================

SUBROUTINE srf_decay()

USE scipuff_fi
USE surface_fi
USE srfparam_fd
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: srf

INTEGER imat, iblk, ifld, i0
INTEGER i, j, j0, is, nv, alloc_stat
REAL    xx, yy, r1, r2, facx

REAL, DIMENSION(7) :: fac

REAL, DIMENSION(:), ALLOCATABLE :: srf_fac

REAL, EXTERNAL :: SWIMgetSunFac

INTERFACE
  RECURSIVE SUBROUTINE decay_cell( srf,icell0,i0,nv,fac )
    USE sagstr_fd
    TYPE( SAGgrid_str ),POINTER      :: srf
    INTEGER,            INTENT( IN ) :: icell0
    INTEGER,            INTENT( IN ) :: i0
    INTEGER,            INTENT( IN ) :: nv
    REAL, DIMENSION(*), INTENT( IN ) :: fac
  END SUBROUTINE decay_cell
END INTERFACE

!------ Set SAG grid structure

srf => Psrfdep ! Associate "local" grid structure pointer

!------ Interpolate solar angle factor [sin(ang)] on top-level grid

ALLOCATE( srf_fac(srf%nx*srf%ny),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'srf_decay'
  eMessage = 'Error allocating surface array for decay rate'
  GOTO 9999
END IF

!----- Get sun factors from SWIM

DO j = 1,srf%ny
  yy = srf%ymin + FLOAT(j-1)*srf%dy

  DO i = 1,srf%nx
    xx = srf%xmin + FLOAT(i-1)*srf%dx

    ifld = 0
    is   = (j-1)*srf%nx + i
    srf_fac(is) = SWIMgetSunFac( ifld,xx,yy )

  END DO
END DO

!------ Loop over all surface blocks

DO iblk = 1,ndep_blocks

  SELECT CASE( srf_block(iblk)%type )

    CASE( SBLK_EFFECTS )

      CYCLE

    CASE DEFAULT

      imat = MOD(srf_block(iblk)%id,65536)
      ifld = srf_block(iblk)%field

      r1 = material(imat)%prop(1)
      r2 = material(imat)%prop(2)

      IF( r1 + r2 > 0. )THEN

        i0 = (ifld-1)*srf%mxgrd
        nv = 3

        IF( srf_block(iblk)%type == SBLK_EVAP )THEN
          IF( substrate_type == 0 )THEN
            nv = 5
          ELSE
            nv = 7
          END IF
          IF( BTEST(run_mode,EVAP2D) )nv = nv + 1
        END IF

        DO j = 1,srf%ny
          j0 = (j-1)*srf%nx

          DO i = 1,srf%nx
            is = j0 + i

            facx   = MAX( r1*srf_fac(is),r2 )
            facx   = EXP(-facx*delt)
            fac(1) = facx
            fac(2) = facx*facx
            fac(3) = fac(2)

            IF( nv > 3 )fac(4:nv) = facx

           CALL decay_cell( srf,is,i0,nv,fac )

          END DO
        END DO

      END IF

  END SELECT

END DO

9999 CONTINUE

DEALLOCATE( srf_fac,STAT=alloc_stat )

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE decay_cell( srf,icell0,i0,nv,fac )

USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),POINTER      :: srf
INTEGER,            INTENT( IN ) :: icell0
INTEGER,            INTENT( IN ) :: i0
INTEGER,            INTENT( IN ) :: nv
REAL, DIMENSION(*), INTENT( IN ) :: fac

INTEGER n, ig, jg, icell

n  = srf%mxgrd
ig = i0 + icell0
jg = ig + (nv-1)*n
srf%ipdat(ig:jg:n) = srf%ipdat(ig:jg:n)*fac(1:nv)

icell = srf%ipgrd(icell0)
IF( icell == 0 )RETURN

DO ig = 0,3
  CALL decay_cell( srf,icell+ig,i0,nv,fac )
END DO

RETURN
END

!==============================================================================

SUBROUTINE set_2dgauss( zsrf,p,s )

USE scipuff_fi
USE refl_fi
USE srfdos_fd

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: zsrf
TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( srf_gauss_str ), INTENT( OUT ) :: s

REAL, DIMENSION(3) :: xr

REAL    h, hx, hy, dum, dummer, zp, zs, det
REAL(8) axx, axy, ayy
INTEGER imat

INTEGER, EXTERNAL :: getPuffifld

!------ Define 2d gaussian spread on surface

IF( lter )THEN

  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h,hx,hy,getPuffifld(p) )
  zp = p%zbar - (zsrf+h)
  zs = p%zbar - h
  CALL puff_reflect( zs,zp,p,hx,hy,.FALSE.,xr,dum,dummer )
  IF( nError /= NO_ERROR )THEN
    CALL dump_puff( 0,p )
    GOTO 9999
  END IF
  s%voli = SNGL(SQRT(deth))/PI
  axx = b_rfl(1,1)
  axy = b_rfl(1,2)
  ayy = b_rfl(2,2)
  s%axx = SNGL(axx*a_rfl(1,1)**2 + 2.*axy*a_rfl(1,1)*a_rfl(2,1) + ayy*a_rfl(2,1)**2)
  s%ayy = SNGL(axx*a_rfl(1,2)**2 + 2.*axy*a_rfl(1,2)*a_rfl(2,2) + ayy*a_rfl(2,2)**2)
  s%axy = SNGL(axx*a_rfl(1,1)*a_rfl(1,2) + ayy*a_rfl(2,1)*a_rfl(2,2) &
                        + axy*(a_rfl(1,1)*a_rfl(2,2)+a_rfl(1,2)*a_rfl(2,1)))

ELSE

  det   = p%axx*p%ayy - p%axy**2
  zp    = 2.*(p%zbar-zsrf)/det
  xr(1) = zp*(p%axz*p%ayy - p%ayz*p%axy)
  xr(2) = zp*(p%ayz*p%axx - p%axz*p%axy)
  s%voli = SQRT(det)/PI
  s%axx = p%axx
  s%ayy = p%ayy
  s%axy = p%axy

END IF

!------ Set gaussian centroid and map factors

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),s%xmap,s%ymap )

s%xbar = SNGL(p%xbar) + 0.5*xr(1)*s%xmap !- srf%xmin
s%ybar = SNGL(p%ybar) + 0.5*xr(2)*s%ymap !- srf%ymin

!------ set min. concentration

imat = typeID(p%ityp)%imat

s%conc_min = 0.0

s%argmax = ARGMAX

9999 CONTINUE

RETURN
END

