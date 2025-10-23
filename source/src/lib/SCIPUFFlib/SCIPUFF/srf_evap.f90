SUBROUTINE set_evap_step()

USE scipuff_fi
USE surface_fi
USE srfevap_fi

!  Sets surface evaporation time step information

IMPLICIT NONE

INTEGER ilev
REAL    umax, qmax, slmax
REAL    xx, yy

!-----  Find maximum BL velocity for the large timestep

CALL SWIMgetUmax( umax,qmax,slmax )

xx = xmin + 0.5*FLOAT(nx-1)*dxg
yy = ymin + 0.5*FLOAT(ny-1)*dyg

CALL mapfac( xx,yy,xmap_evap,ymap_evap )

!-----  Set time levels

DO ilev = 0,MAXLEV_EVAP
  lev_evap(ilev) = -1
END DO

mxlev_evap  = -1
mxtlev_evap = -1

dx_evap = MIN(Psrfdep%dx/xmap_evap,Psrfdep%dy/ymap_evap)

CALL set_del_evap( umax,qmax,slmax,del_evap,facu_evap )
CALL get_lim_evap( del_evap,facu_evap )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_del_evap( umax,qmax,slmax,del,facu )
USE scipuff_fi
USE surface_fi
USE srfevap_fi

IMPLICIT NONE

REAL, INTENT( IN  ) :: umax, qmax, slmax
REAL, INTENT( OUT ) :: del, facu

del = MAX(umax*xmap_evap/Psrfdep%dx,umax*ymap_evap/Psrfdep%dy)
IF( del > SMALL )THEN
  del = 1./del
ELSE
  del = delt
END IF

IF( qmax > SMALL )THEN
  facu = slmax**(0.333333)/qmax
ELSE
  facu = 1.E+20
END IF

RETURN
END

!===============================================================================

SUBROUTINE get_lim_evap( del,facu )

USE scipuff_fi
USE surface_fi
USE srfevap_fi
USE srfparam_fd

!  Gets maximum surface evaporation grid depth

IMPLICIT NONE

REAL, INTENT( IN ) :: del, facu

INTEGER i, ix, iy, mlevi, nxs, nys, ivm, ivv, ivs
INTEGER icell0

INTERFACE
  RECURSIVE SUBROUTINE limevap( srf,icell0,ivm,ivv,ivs,mlev0,dx0,dt0,facu )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: srf
    INTEGER,             INTENT( IN ) :: ivm, ivv, ivs
    INTEGER,             INTENT( IN ) :: icell0, mlev0
    REAL,                INTENT( IN ) :: dx0, dt0, facu
  END SUBROUTINE limevap
END INTERFACE

nxs = Psrfdep%nx
nys = Psrfdep%ny

IF( ndep_blocks == 0 )GOTO 9999

DO i = 1,ndep_blocks

  IF( srf_block(i)%type == SBLK_EVAP )THEN
    ivm = (srf_block(i)%field-1)*Psrfdep%mxgrd
    ivv = ivm + Psrfdep%mxgrd
    ivs = ivv + Psrfdep%mxgrd

    DO ix = 1,nxs
      DO iy = 1,nys
        icell0 = (iy-1)*nxs + ix
        mlevi  = 0
        CALL limevap( Psrfdep,icell0,ivm,ivv,ivs,mlevi,dx_evap,del,facu )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    END DO

  END IF

END DO

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE limevap( srf,icell0,ivm,ivv,ivs,mlev0,dx0,dt0,facu )

USE scipuff_fi
USE files_fi
USE srfevap_fi
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: srf
INTEGER,             INTENT( IN ) :: ivm, ivv, ivs
INTEGER,             INTENT( IN ) :: icell0, mlev0
REAL,                INTENT( IN ) :: dx0, dt0, facu

INTEGER mlev, i, icell, ilev, jlev, ios
REAL    si, fac, dti, dx

INTEGER, EXTERNAL :: time_level

IF( srf%ipdat(icell0+ivm) > SMALL .AND. srf%ipdat(icell0+ivv) > SMALL )THEN
  ilev = MAX(mlev0-2,0)
  si   = srf%ipdat(icell0+ivs)/srf%ipdat(icell0+ivv)
  fac  = 0.5**ilev
  dx   = dx0*fac
  dti  = dt0*fac
  dti  = MIN(si**TwoThirds*facu,dti)

  jlev = time_level( dti )
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'limevap'
    WRITE(lun_log,*,IOSTAT=ios)'******* TIME LEVEL ERROR ********'
    WRITE(lun_log,*,IOSTAT=ios)TRIM(eRoutine)
    WRITE(lun_log,*,IOSTAT=ios)TRIM(eInform)
    WRITE(lun_log,*,IOSTAT=ios)'DT=',dti
    WRITE(lun_log,*,IOSTAT=ios)'DELT=',delt
    WRITE(lun_log,*,IOSTAT=ios)'LEVEL=',jlev,MAXTLV,mlev0
    WRITE(lun_log,*,IOSTAT=ios)'dx,si,facu=',dx,si,facu
    WRITE(lun_log,*,IOSTAT=ios)'ivar,icell0=',ivm,icell0
    GOTO 9999
  END IF

  lev_evap(ilev) = MAX(lev_evap(ilev),jlev)
  mxlev_evap     = MAX(mxlev_evap,ilev)
  mxtlev_evap    = MAX(mxtlev_evap,lev_evap(ilev))
END IF

IF( srf%ipgrd(icell0) /= 0 )THEN
  DO i = 0,3
    icell = srf%ipgrd(icell0) + i
    mlev  = mlev0 + 1
    CALL limevap( srf,icell,ivm,ivv,ivs,mlev,dx0,dt0,facu )
  END DO
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE step_srf_evap( lev,lev2 )

USE scipuff_fi
USE surface_fi
USE srfevap_fi
USE srfparam_fd
USE omp_lib
USE step_p_fi, ONLY: myTId, nDimTemp, PuffTemp, PuffSplit, npufTemp
USE error_fi,  ONLY: multiThread

!  Steps surface deposition variables for secondary evaporation

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: lev
INTEGER, INTENT( INOUT ) :: lev2

INTEGER imat, ilev, nxs, nys
INTEGER i, ix, iy, icell0, ivar
INTEGER alloc_stat, ic
REAL    dxs, dys, xx, yy, dx, dy, xmins, ymins

TYPE( puff_material ) pmatlIn

INTERFACE
  RECURSIVE SUBROUTINE srfgrd_evap( srf,ivar,icell0,ilev,xx,yy,dx,dy,lev,lev2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: srf    !SAG grid structure
    INTEGER,             INTENT( IN    ) :: ivar   !Variable offset
    INTEGER,             INTENT( IN    ) :: icell0 !Grid cell
    INTEGER,             INTENT( IN    ) :: ilev   !Current grid level
    INTEGER,             INTENT( IN    ) :: lev    !Current time level
    INTEGER,             INTENT( INOUT ) :: lev2   !Current max time level
    REAL,                INTENT( IN    ) :: xx,yy  !Cell centroid location
    REAL,                INTENT( IN    ) :: dx,dy  !Cell size (project coord)
  END SUBROUTINE srfgrd_evap
END INTERFACE

INTEGER, EXTERNAL :: num_puff_types

!------ Check time level for this time step

IF( lev > mxtlev_evap )RETURN

!------ Loop over grid

nxs   = Psrfdep%nx
nys   = Psrfdep%ny
xmins = Psrfdep%xmin
ymins = Psrfdep%ymin
dxs   = Psrfdep%dx
dys   = Psrfdep%dy

IF( ndep_blocks == 0 )GOTO 9999

IF( multiThread )THEN
  DO i = 1,ndep_blocks

    IF( srf_block(i)%type == SBLK_EVAP )THEN

      ALLOCATE( nDimTemp(nthreads),npufTemp(nthreads),STAT=alloc_stat )
      IF( alloc_stat == 0 )nDimTemp = INT(0.1*FLOAT(Psrfdep%ncells)/FLOAT(nthreads)) + 1
      IF( alloc_stat == 0 )CALL allocate_PuffTemp( alloc_stat )
      IF( alloc_stat == 0 )ALLOCATE( PuffSplit(Psrfdep%ncells,2),STAT=alloc_stat )
      IF( alloc_Stat /= 0 )THEN
        nError   = SZ_ERROR
        eRoutine = 'step_srf_evap'
        eMessage = 'Error allocating temporary puff storage'
        WRITE(eInform,*) 'npuf = ',nDimTemp(1)
        GOTO 9999
      END IF
      EXIT
    END IF
  END DO
END IF

DO i = 1,ndep_blocks

  IF( srf_block(i)%type == SBLK_EVAP )THEN

    IF( multiThread )THEN
      npufTemp = 0

      DO ic = 1,Psrfdep%ncells
        PuffSplit(ic,1) = 0
      END DO
    END IF

    imat = MOD(srf_block(i)%id,65536)
    ivar = (srf_block(i)%field-1)*Psrfdep%mxgrd

!---- Get liquid puff properties

    ityp_evap = material(imat)%ioffp + 2
    CALL get_puff_material( ityp_evap,pmatlIn )
    pmatl = TRANSFER(pmatlIn,pmatl)
    evap_min = material(imat)%prop(3)

!---- Reset type for vapor puff

    ityp_evap = ityp_evap - 1

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ilev,xx,yy,dx,dy,ix,iy)
!$OMP DO SCHEDULE(DYNAMIC,1)
    DO icell0 = 1,nxs*nys
      iy = (icell0-1)/nxs + 1
      ix = icell0 - (iy-1)*nxs
      myTId = OMP_GET_THREAD_NUM() + 1
!$OMP FLUSH( omp_error )
      IF( ANY(omp_error(:)%nError /= NO_ERROR) )CYCLE
        ilev   = 0
        xx = xmins + (FLOAT(ix)-0.5)*dxs
        yy = ymins + (FLOAT(iy)-0.5)*dys
        CALL mapfac( xx,yy,xmap_evap,ymap_evap )
        dx = dxs
        dy = dys
        srf_met = .FALSE.
        rate    = 0.
        CALL srfgrd_evap( Psrfdep,ivar,icell0,ilev,xx,yy,dx,dy,lev,lev2 )
        IF( nError /= NO_ERROR )THEN
          omp_error(myTId)%nError   = nError
          omp_error(myTId)%eRoutine = eRoutine
          omp_error(myTId)%eMessage = eMessage
          omp_error(myTId)%eInform  = eInform
          omp_error(myTId)%eAction  = eAction
!$OMP FLUSH( omp_error )
          CYCLE
        END IF
      END DO
!$OMP END DO NOWAIT
!$OMP END PARALLEL
!$OMP FLUSH( omp_error )
    IF( ANY(omp_error(:)%nError /= NO_ERROR) )THEN
      DO ix = 1,nthreads
        IF( omp_error(ix)%nError /= NO_ERROR )THEN
          nError   = omp_error(ix)%nError
          eRoutine = omp_error(ix)%eRoutine
          eMessage = omp_error(ix)%eMessage
          eInform  = omp_error(ix)%eInform
        END IF
      END DO
      GOTO 9999
    END IF

    IF( multiThread )THEN

!--- Transfer temporary puffs into puff list
!    Process adaptive grid, since it can be constructed in differnt ordering

      DO icell0 = 1,nxs*nys
        CALL AddEvapTemp( icell0 )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO

    END IF

  END IF

END DO

9999 CONTINUE

IF( ALLOCATED(PuffTemp)  )CALL deallocate_PuffTemp( alloc_stat )
IF( ALLOCATED(PuffSplit) )DEALLOCATE( PuffSplit,STAT=alloc_stat )
IF( ALLOCATED(npufTemp)  )DEALLOCATE( npufTemp, STAT=alloc_stat )
IF( ALLOCATED(ndimTemp)  )DEALLOCATE( ndimTemp, STAT=alloc_stat )

RETURN
END

!===============================================================================

SUBROUTINE get_srf_met( xx,yy )

USE SWIMpuff_fd
USE SWIMparam_fd
USE srfevap_fi

!  Calculates surface meteorology needed for evaporation

IMPLICIT NONE

REAL, INTENT( IN ) :: xx, yy

TYPE( PuffMet ) :: Met

INTEGER irv

INTEGER, EXTERNAL :: SWIMcurrentSrfMet

irv = SWIMcurrentSrfMet( xx,yy,Met )
IF( irv /= SWIMsuccess )THEN
  CALL setSWIMerror( 'SWIMcurrentSrfMet' )
  GOTO 9999
END IF

zisrf   = Met%BL%MixingHt
hflxsrf = Met%BL%HeatFlux
tsrf    = Met%Mean%T
ubsrf   = Met%Mean%U
vbsrf   = Met%Mean%V
ustr    = Met%BL%UstarDep
z0      = Met%BL%zruf
srf_met = .TRUE.

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE srfgrd_evap( srf,ivar,icell0,ilev,xx,yy,dx,dy,lev,lev2 )

USE scipuff_fi
USE srfevap_fi
USE sagstr_fd

!  Calculates surface deposition from the adaptive grid

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER         :: srf    !SAG grid structure
INTEGER,             INTENT( IN    ) :: ivar   !Variable offset
INTEGER,             INTENT( IN    ) :: icell0 !Grid cell
INTEGER,             INTENT( IN    ) :: ilev   !Current grid level
INTEGER,             INTENT( IN    ) :: lev    !Current time level
INTEGER,             INTENT( INOUT ) :: lev2   !Current max time level
REAL,                INTENT( IN    ) :: xx,yy  !Cell centroid location
REAL,                INTENT( IN    ) :: dx,dy  !Cell size (project coord)

INTEGER ilevx, i, iy, icell
REAL    dtx, dxx, dyy, xxx, yyy

INTERFACE
  SUBROUTINE srf_evap( srf,ivar,icell0,ilev,xx,yy,dx,dy,dts,lev2 )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER         :: srf    !SAG grid structure
    INTEGER,             INTENT( IN    ) :: ivar   !Variable offset
    INTEGER,             INTENT( IN    ) :: icell0 !Grid cell
    INTEGER,             INTENT( IN    ) :: ilev   !Current grid level
    INTEGER,             INTENT( INOUT ) :: lev2   !Current max time level
    REAL,                INTENT( IN    ) :: xx,yy  !Cell centroid location
    REAL,                INTENT( IN    ) :: dx,dy  !Cell size (project coord)
    REAL,                INTENT( IN    ) :: dts    !Local time step
  END SUBROUTINE srf_evap
END INTERFACE

IF( lev_evap(ilev) < lev )THEN

!-------- Level below minimum, so continue to refine grid

  IF( srf%ipgrd(icell0) > 0 )THEN
    ilevx = ilev + 1
    dxx = 0.5*dx
    dyy = 0.5*dy
    DO i = 0,3
      icell = srf%ipgrd(icell0) + i
      iy = INT(i/2)
      xxx = xx + (FLOAT(i-iy-iy)-0.5)*dxx
      yyy = yy + (FLOAT(iy)-0.5)*dyy
      CALL srfgrd_evap( srf,ivar,icell,ilevx,xxx,yyy,dxx,dyy,lev,lev2 )
      IF( nError /= NO_ERROR )GOTO 9999
    END DO
  ELSE
    RETURN
  END IF

ELSE

  IF( srf%ipgrd(icell0) > 0 )THEN

    dtx = delt*(0.5**lev_evap(ilev))
    CALL srf_evap( srf,ivar,icell0,ilev,xx,yy,dx,dy,dtx,lev2 )
    IF( nError /= NO_ERROR )GOTO 9999

!---------- Check levels below if necessary

    IF( ilev < mxlev_evap .AND. found_subcell )THEN
      ilevx = ilev + 1
      dxx   = 0.5*dx
      dyy   = 0.5*dy
      DO i = 0,3
        icell = srf%ipgrd(icell0) + i
        iy = INT(i/2)
        xxx = xx + (FLOAT(i-iy-iy)-0.5)*dxx
        yyy = yy + (FLOAT(iy)-0.5)*dyy
        CALL srfgrd_evap( srf,ivar,icell,ilevx,xxx,yyy,dxx,dyy,lev,lev2 )
        IF( nError /= NO_ERROR )GOTO 9999
      END DO
    ELSE
      RETURN
    END IF
  ELSE
    RETURN
  END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE srf_evap( srf,ivar,icell0,ilev,xx,yy,dx,dy,dts,lev2 )

USE scipuff_fi
USE met_fi
USE srfevap_fi
USE sagstr_fd
USE srfevap2D_fd

!  Calculates surface deposition from the adaptive grid

IMPLICIT NONE

REAL, PARAMETER :: RT12 = 2.*SQRT3

TYPE( SAGgrid_str ), POINTER         :: srf    !SAG grid structure
INTEGER,             INTENT( IN    ) :: ivar   !Variable offset
INTEGER,             INTENT( IN    ) :: icell0 !Grid cell
INTEGER,             INTENT( IN    ) :: ilev   !Current grid level
INTEGER,             INTENT( INOUT ) :: lev2   !Current max time level
REAL,                INTENT( IN    ) :: xx,yy  !Cell centroid location
REAL,                INTENT( IN    ) :: dx,dy  !Cell size (project coord)
REAL,                INTENT( IN    ) :: dts    !Local time step

REAL, DIMENSION(8) :: msrf

REAL, DIMENSION(5) :: asrf

REAL    smass, dsrf, depth
REAL    mass, tau_evap
INTEGER iv, i, j, icell, jcell, i0, nv

INTEGER iy,jy
REAL    xxx, yyy, dxx, dyy
REAL    xxxx,yyyy,dxxx,dyyy

TYPE( var2Distrib ) var2d    !two distribution vars

INTERFACE
  SUBROUTINE srf_cell_evap( srf,ivar,icell0,ilev )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER                :: srf    !Dep grid structure
    INTEGER,                       INTENT( IN ) :: ivar   !Start of evap block
    INTEGER,                       INTENT( IN ) :: ilev   !Grid refinement level
    INTEGER,                       INTENT( IN ) :: icell0 !Upper cell location
  END SUBROUTINE srf_cell_evap

  SUBROUTINE srf_cell_evap_2D( srf,var2d,ivar,icell0,ilev )
    USE sagstr_fd
    USE srfevap_fi
    USE srfevap2D_fd

    TYPE( SAGgrid_str ), POINTER      :: srf    !Dep grid structure
    TYPE(var2Distrib),   INTENT( IN ) :: var2d  !two distribution evaporation vars
    INTEGER,             INTENT( IN ) :: ivar   !Start of evap block
    INTEGER,             INTENT( IN ) :: ilev   !Grid refinement level
    INTEGER,             INTENT( IN ) :: icell0 !Upper cell location
  END SUBROUTINE srf_cell_evap_2D
END INTERFACE

!------ Accumulate subcells for this level
!       msrf(1) = sum mean C
!       msrf(2) = sum variance C
!       msrf(3) = sum variance * length Scale
!       msrf(4) = sum mean * drop diameter
!       msrf(5) = total wetted area
!       substrate - optional
!       if substrate
!       msrf(6) = sum sfc mean C
!       msrf(7) = sum mean * front depth (m)
!       msrf(8) = sum mean * drop diameter**2
!       else
!       msrf(6) = sum mean * drop diameter**2
!
!       asrf(1) = sum mean C**2
!       asrf(2) = sum mean * xbar
!       asrf(3) = sum mean * xbar**2
!       asrf(4) = sum mean * ybar
!       asrf(5) = sum mean * ybar**2
!
!       sum total variance = msrf(2) + msrf(6) - msrf(1)**2/16

var2d%is2D    = .FALSE. !reset every loop
found_subcell = .FALSE.
IF( dx >= MetGrid(1)%dx )srf_met = .FALSE.

IF( substrate_type == 0 )THEN
  nv = 5
ELSE
  nv = 7
END IF

IF( BTEST(run_mode,EVAP2D) )nv = nv + 1
msrf = 0.0
asrf = 0.0

dxx  = 0.5*dx
dyy  = 0.5*dy
dxxx = 0.5*dxx
dyyy = 0.5*dyy
DO i = 0,3
  iy = INT(i/2)
  xxx = (FLOAT(i-iy-iy)-0.5)*dxx
  yyy = (FLOAT(iy)-0.5)*dyy
  icell = srf%ipgrd(icell0) + i
  IF( ilev == 0 )THEN
    DO iv = 1,nv
      i0 = (iv-1)*srf%mxgrd
      msrf(iv) = msrf(iv) + 4.*srf%ipdat(icell+ivar+i0)
    END DO
    smass   = srf%ipdat(icell+ivar)
    asrf(1) = asrf(1) + 4.*smass*smass
    asrf(2) = asrf(2) + 4.*smass*xxx
    asrf(3) = asrf(3) + 4.*smass*(xxx*xxx + (dxx/RT12)**2)
    asrf(4) = asrf(4) + 4.*smass*yyy
    asrf(5) = asrf(5) + 4.*smass*(yyy*yyy + (dyy/RT12)**2)
  END IF
  IF( srf%ipgrd(icell) > 0 )THEN
    found_subcell = .TRUE.
    DO j = 0,3
      jcell = srf%ipgrd(icell) + j
      DO iv = 1,nv
        i0 = (iv-1)*srf%mxgrd
        msrf(iv) = msrf(iv) + srf%ipdat(jcell+ivar+i0)
      END DO
      jy = INT(j/2)
      xxxx = xxx + (FLOAT(j-jy-jy)-0.5)*dxxx
      yyyy = yyy + (FLOAT(jy)-0.5)*dyyy
      smass   = srf%ipdat(jcell+ivar)
      asrf(1) = asrf(1) + smass*smass
      asrf(2) = asrf(2) + smass*xxxx
      asrf(3) = asrf(3) + smass*(xxxx*xxxx + (dxxx/RT12)**2)
      asrf(4) = asrf(4) + smass*yyyy
      asrf(5) = asrf(5) + smass*(yyyy*yyyy + (dyyy/RT12)**2)
    END DO
  END IF
END DO

IF( ilev == 0 )THEN
  DO iv = 1,nv
    i0 = (iv-1)*srf%mxgrd
    msrf(iv) = msrf(iv) + 16.*srf%ipdat(icell0+ivar+i0)
  END DO
  smass   = srf%ipdat(icell0+ivar)
  asrf(1) = asrf(1) + 16.*smass*smass
  asrf(3) = asrf(3) + 16.*smass*((dx/RT12)**2)
  asrf(5) = asrf(5) + 16.*smass*((dy/RT12)**2)
END IF

IF( asrf(1) > SMALL .AND. msrf(2) > SMALL )THEN !check for underflow

  xxx = asrf(2)/msrf(1)
  yyy = asrf(4)/msrf(1)
  dxx = SQRT(asrf(3)/msrf(1) - xxx*xxx)*RT12
  dyy = SQRT(asrf(5)/msrf(1) - yyy*yyy)*RT12
  xxx = xx + xxx
  yyy = yy + yyy

  IF( .NOT.srf_met )THEN
    CALL get_srf_met( xx,yy )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

!--- Set cell properties

  dsrf = MAX(msrf(4)/msrf(1),1.E-6)  !Mean droplet diameter (m)

  IF( substrate_type == 0 )THEN   !Impermeable surface
    smass = msrf(1)
    depth = 0.0
  ELSE
    smass = msrf(6)
    depth = msrf(7)/msrf(1)       !Front depth below sfc
  END IF

  CALL srf_evap_rate_2D( msrf,var2d,dsrf,depth,smass,dts )

  IF( var2D%is2D )THEN                               !two size split

    IF( 1.0-var2d%rate1 <= 0.0 )var2d%rate1 = 1.0
    IF( 1.0-var2d%rate2 <= 0.0 )var2d%rate2 = 1.0
    IF( var2d%rate1 == 1.0 .AND. var2d%rate2 == 1.0 )GOTO 9999   ! Skip out if no evaporated mass

    rate = var2d%alpha*var2d%rate1 + (1-var2d%alpha)*var2d%rate2 !combined rate
    mass = msrf(1)*(1.0-rate)*dx*dy/(xmap_evap*ymap_evap*16.)
    tau_evap = MIN(2.*dts/MAX((1.0-rate),1.E-6),t+dts)

!---- Next will use only first 3 msrf - no change for 2D evaporation

    CALL srf_puff_create( mass,msrf,asrf,xxx,yyy,dxx,dyy,dts,tau_evap,lev2,icell0 )
    IF( nError /= NO_ERROR )GOTO 9999

  ELSE                  !old way, NO two size split

    IF( 1.0-rate <= 0.0 )GOTO 9999   ! Skip out if no evaporated mass

!---- Total evaporated vapor mass

    mass = msrf(1)*(1.0-rate)*dx*dy/(xmap_evap*ymap_evap*16.)
    tau_evap = MIN(2.*dts/MAX((1.0-rate),1.E-6),t+dts)
    CALL srf_puff_create( mass,msrf,asrf,xxx,yyy,dxx,dyy,dts,tau_evap,lev2,icell0 )
    IF( nError /= NO_ERROR )GOTO 9999

  END IF

ELSE    !Zero any underflows

  rate   = 0.0
  rate_a = 0.0
  rate_d = 0.0

END IF !IF( asrf(1) > SMALL .AND. msrf(2) > SMALL )

IF( var2d%is2D )THEN
  CALL srf_cell_evap_2D( srf,var2d,ivar,icell0,ilev )
ELSE
  CALL srf_cell_evap( srf,ivar,icell0,ilev )
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE srf_evap_rate( dmass,diam,awet,depth,dsurf,dts )

USE constants_fd
USE default_fd
USE srfevap_fi
USE substrate_fi
USE srfevap2D_fd  !evap2D vars
USE scipuff_fi    !needed for run_mode access

IMPLICIT NONE

REAL, INTENT( IN ) :: dmass    !Deposition mass per unit area
REAL, INTENT( IN ) :: diam     !Surface droplet diameter (m)
REAL, INTENT( IN ) :: awet     !Wetted area per unit area
REAL, INTENT( IN ) :: depth    !Front depth (m)
REAL, INTENT( IN ) :: dsurf    !Deposition mass per unit area on surface
REAL, INTENT( IN ) :: dts      !Time step (s)

REAL tx, mtot, msfc, hvd, narea

!--- Set agent properties

IF( .NOT. BTEST(run_mode,EVAP2D) )THEN    !for EVAP2D lqd was called already in get_srf_evap_ratios
  CALL lqd( pmatl,tsrf,rhod,cs,hvd,difd )
END IF

darea = 0.25*PI*diam**2              !Droplet surface area

IF( porosity == NOT_SET_R )THEN      !Impermeable surface

  CALL srf_drop_evap( diam )
  rate_s = rate
  rate_a = 0.0
  rate_d = 0.0
  dfac   = 0.0

ELSE

  narea = awet/darea                 !Droplet number density
  tx    = 0.0

  IF( dsurf > SMALL )THEN            !Surface mass is present
    mtot = dmass/narea               !Single droplet mass total
    msfc = dsurf/narea               !Single droplet surface mass
    CALL srf_drop_evap( diam )
    CALL srf_drop_absorb( mtot,msfc,dts,tx )
    IF( depth > 0.0 )tx = 0.0
  ELSE
    rate   = 0.0
    rate_s = 0.0
    rate_a = 0.0
    msfc   = 0.0  !to avoid uninitialized variable (IVS)
  END IF

  IF( depth > 0.0 .OR. msfc == 0.0 )THEN !Subsurface mass is exposed
    dfac = rhod*darea*porosity
    CALL srf_drop_desorb( diam,depth,dts-tx )
    rate_d = rate_d*(1.0-tx/dts)
  ELSE
    dfac   = 0.0
    rate_d = 0.0
  END IF

END IF

!---- Set rate factors for surface grid - surface mass (absorption)

IF( porosity /= NOT_SET_R )THEN
  IF( dsurf > SMALL )THEN
    IF( depth > SMALL )THEN
      rate_d = rate_d*(1.0-dsurf/dmass)  ! Adjust rates for mixed evaporation
    END IF
    rate_a = 1.0 - (rate_s+rate_a)*awet*dts/(darea*dsurf)   !Fractional reduction
    rate_a = MAX(0.0,rate_a)
  ELSE
    rate_a = 1.0
  END IF
END IF

!---- Rate factor for total surface mass

rate = 1.0 - (rate_s+rate_d*dfac)*awet*dts/(darea*dmass)      !Fractional reduction

IF( rate < SMALL )rate = 0.0

!---- Rate factor for frontal depth

rate_d = rate_d*dts        !Delta-h frontal depth (m)

RETURN
END

!===============================================================================

SUBROUTINE srf_drop_evap( diam )

USE constants_fd
USE basic_fi
USE srfevap_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: diam   !Surface droplet diameter

!--- Calculate evaporation rate for single surface droplet
!    based on Baines and James model

REAL pe

REAL, EXTERNAL :: sherwd_sec

!------ Compute the Peclet number, Pe

pe = rhoair*(ustr*diam)**2 / (rmuair*difd)

!------ Compute the droplet mass flux

rate = PI2*difd*diam*cs * sherwd_sec( pe )

RETURN
END

!===============================================================================

SUBROUTINE srf_drop_absorb( mtot,msfc,dt,tx )

USE srfevap_fi
USE substrate_fi

IMPLICIT NONE

REAL, INTENT( IN    ) :: mtot        !Total mass (kg)
REAL, INTENT( INOUT ) :: msfc        !Mass on surface (kg)
REAL, INTENT( IN    ) :: dt          !Time step (s)
REAL, INTENT( OUT   ) :: tx          !Evaporation time (s)

REAL kperm, mnew, msub, mtem, tem

!------ Calculate absorption rate

msub = mtot - msfc           !Mass in substrate (kg)

kperm  = k_substrate*pmatl%st*(rhod*darea)**2/pmatl%viscosity
mnew   = SQRT( msub*msub + 2.0*kperm*dt )
rate_a = (mnew - msub)/dt

!----- Limit evaporation/absorption if all surface mass is lost

IF( (rate+rate_a)*dt > msfc )THEN !All sfc mass is absorbed/evaporated

  mtem = mtot*mtot-msub*msub

!---- Calculate time (tx) when sfc mass becomes zero

  IF( rate*(mtot+msub) > 1.0E-3*kperm )THEN  !Check evap time vs absorb time
    tem = mtot + kperm/rate
    tx  = (tem - SQRT(tem*tem-mtem))/rate
  ELSE
    tx  = 0.5*mtem/kperm
  END IF

!---- Adjust sfc mass loss rates to reflect shorter duration

  rate_a = 1.1*(msfc-rate*tx)/dt  !Factor of 1.1 ensures new sfc mass = 0
  rate_s = rate*tx/dt
  msfc   = 0.0                    !Set so desorption occurs for remainder

ELSE

  tx     = 0.0
  rate_s = rate

END IF

RETURN
END

!===============================================================================

SUBROUTINE srf_drop_desorb( diam,depth,dt )

USE constants_fd
USE srfevap_fi
USE substrate_fi

IMPLICIT NONE

REAL, INTENT( IN )  :: diam        !Droplet diameter (m)
REAL, INTENT( IN )  :: depth       !Frontal depth (m)
REAL, INTENT( IN )  :: dt          !Time step (s)

REAL deff, vr, tem, dnew

!------ Calculate desorption rate

deff = difd*porosity/tortuosity

IF( rate == 0.0 )CALL srf_drop_evap( diam )

rate_d = rate

vr = 4.0*rate_d/(PI*diam*diam*cs)

tem  = rate_d*deff*dt/dfac + deff*depth + 0.5*vr*depth*depth
dnew = (-deff + SQRT(deff*deff + 2.0*vr*tem))/vr

!-- Integral of dh/dt = rate_d*deff/dfac/(deff + vr*depth)

rate_d = (dnew - depth)/dt

RETURN
END

!===============================================================================

REAL FUNCTION sherwd_sec( pe )

IMPLICIT NONE

REAL, INTENT( IN ) :: pe

sherwd_sec = 0.105*pe**0.33333333

RETURN
END

!===============================================================================

SUBROUTINE srf_cell_evap( srf,ivar,icell0,ilev )

USE scipuff_fi
USE srfevap_fi
USE sagstr_fd

!Sets surface vars after secondary evaporation

IMPLICIT NONE


TYPE( SAGgrid_str ), POINTER                :: srf    !Dep grid structure
INTEGER,                       INTENT( IN ) :: ivar   !Start of evap block
INTEGER,                       INTENT( IN ) :: ilev   !Grid refinement level
INTEGER,                       INTENT( IN ) :: icell0 !Upper cell location

INTEGER i, j, icell, jcell, i0, i1, i2, i3, i4, i5
REAL    rate2, rate_awet, rate_diam
REAL    rate53   !rate**(5/3)

rate2 = rate*rate

rate_awet = rate2**0.3333333
rate_diam = rate_awet*rate_awet
IF( BTEST(run_mode,EVAP2D) )rate53 = rate**(5./3.)


i0 = ivar

i1 = i0 + srf%mxgrd
i2 = i1 + srf%mxgrd
i3 = i2 + srf%mxgrd
i4 = i3 + srf%mxgrd

IF( substrate_type == 0 )THEN
  i5 = i4 + srf%mxgrd
ELSE
  i5 = i4 + 3 * srf%mxgrd
END IF

DO i = 0,3
  icell = srf%ipgrd(icell0) + i
  IF( ilev == 0 )THEN
    srf%ipdat(icell+i0) = srf%ipdat(icell+i0)*rate
    srf%ipdat(icell+i1) = srf%ipdat(icell+i1)*rate2
    srf%ipdat(icell+i2) = srf%ipdat(icell+i2)*rate2
    srf%ipdat(icell+i3) = srf%ipdat(icell+i3)*rate_diam
    srf%ipdat(icell+i4) = srf%ipdat(icell+i4)*rate_awet
    IF( BTEST(run_mode,EVAP2D) )THEN    ! new MD^2 = old MD^2 * rate^(5/3)
      srf%ipdat(icell+i5) = srf%ipdat(icell+i5)*rate53
    END IF
  END IF
  IF( srf%ipgrd(icell) > 0 )THEN
    DO j = 0,3
      jcell = srf%ipgrd(icell) + j
      srf%ipdat(jcell+i0) = srf%ipdat(jcell+i0)*rate
      srf%ipdat(jcell+i1) = srf%ipdat(jcell+i1)*rate2
      srf%ipdat(jcell+i2) = srf%ipdat(jcell+i2)*rate2
      srf%ipdat(jcell+i3) = srf%ipdat(jcell+i3)*rate_diam
      srf%ipdat(jcell+i4) = srf%ipdat(jcell+i4)*rate_awet
      IF( BTEST(run_mode,EVAP2D) )THEN   !new MD^2 = old MD^2 * rate^(5/3)
         srf%ipdat(jcell+i5) = srf%ipdat(jcell+i5)*rate53
      END IF
    END DO
  END IF
END DO

IF( ilev == 0 )THEN
  srf%ipdat(icell0+i0) = srf%ipdat(icell0+i0)*rate
  srf%ipdat(icell0+i1) = srf%ipdat(icell0+i1)*rate2
  srf%ipdat(icell0+i2) = srf%ipdat(icell0+i2)*rate2
  srf%ipdat(icell0+i3) = srf%ipdat(icell0+i3)*rate_diam
  srf%ipdat(icell0+i4) = srf%ipdat(icell0+i4)*rate_awet
  IF( BTEST(run_mode,EVAP2D) )THEN   !new MD^2 = old MD^2 * rate^(5/3)
    srf%ipdat(icell0+i5) = srf%ipdat(icell0+i5)*rate53
  END IF
END IF

IF( substrate_type /= 0 )THEN

  i1 = i4 + srf%mxgrd
  i2 = i1 + srf%mxgrd
  DO i = 0,3
    icell = srf%ipgrd(icell0) + i
    IF( ilev == 0 )THEN
      srf%ipdat(icell+i1) = srf%ipdat(icell+i1)*rate_a
      srf%ipdat(icell+i2) = srf%ipdat(icell+i2)*rate &
                          + srf%ipdat(icell+i0)*rate_d
    END IF
    IF( srf%ipgrd(icell) > 0 )THEN
      DO j = 0,3
        jcell = srf%ipgrd(icell) + j
        srf%ipdat(jcell+i1) = srf%ipdat(jcell+i1)*rate_a
        srf%ipdat(jcell+i2) = srf%ipdat(jcell+i2)*rate &
                            + srf%ipdat(jcell+i0)*rate_d
      END DO
    END IF
  END DO

  IF( ilev == 0 )THEN
    srf%ipdat(icell0+i1) = srf%ipdat(icell0+i1)*rate_a
    srf%ipdat(icell0+i2) = srf%ipdat(icell0+i2)*rate &
                         + srf%ipdat(icell0+i0)*rate_d
  END IF

END IF

RETURN
END

!===============================================================================

SUBROUTINE SetStaticEvap( p,pq,dt,t_srf,fac_evap )

USE scipuff_fi
USE met_fi
USE step_p_fi, ONLY: imat
USE srfevap_fi

IMPLICIT NONE

TYPE( puff_str ),    INTENT( IN  ) :: p
TYPE( puff_liquid ), INTENT( IN  ) :: pq
REAL,                INTENT( IN  ) :: dt
REAL,                INTENT( IN  ) :: t_srf
REAL,                INTENT( OUT ) :: fac_evap  !Retained deposition fraction

REAL diam, depth, dmass, awet
REAL hvd   !used in CALL lqd() only

TYPE( puff_material ) pmatlIn

!---- Get liquid puff properties

ityp_evap = material(imat)%ioffp + 2
CALL get_puff_material( ityp_evap,pmatlIn )
pmatl = TRANSFER(pmatlIn,pmatl)

dmass = 1.0
depth = 0.0
diam  = pq%d*pmatl%sf
awet  = 3.0*dmass*pmatl%sf*pmatl%sf/(2.0*pq%d*pmatl%rho)

CALL get_srf_met( SNGL(p%xbar),SNGL(p%ybar) )

IF( BTEST(run_mode,EVAP2D) )THEN  !it won't be called in srf_evap_rate in this case, call it here
  CALL lqd( pmatl,tsrf,rhod,cs,hvd,difd )
END IF

!---- We shouldn't worry about evaporation 2D split is this case
!---- because diam  = pq%d*pmatl%sf
CALL srf_evap_rate( dmass,diam,awet,depth,dmass,dt )

IF( rate >= 1.0 )THEN
  fac_evap = 1.0
ELSE IF( rate > 0.0 )THEN
  fac_evap = MIN(1.0,-dt/t_srf/LOG(rate))
ELSE
  fac_evap = 0.0
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE srf_puff_create( mass,msrf,asrf,xx,yy,dx,dy,dts,tau_evap,lev2,ic )

USE scipuff_fi
USE met_fi
USE files_fi
USE srfevap_fi
USE step_p_fi, ONLY: myTId, nDimTemp, PuffTemp, PuffSplit, npufTemp
USE error_fi,  ONLY: multiThread

IMPLICIT NONE

REAL,               INTENT( IN    ) :: mass         !Puff vapor mass
REAL, DIMENSION(*), INTENT( IN    ) :: msrf, asrf   !Grid-averaged data
REAL,               INTENT( IN    ) :: xx, yy       !Puff location
REAL,               INTENT( IN    ) :: dx, dy       !Puff spread
REAL,               INTENT( IN    ) :: dts          !Time step
REAL,               INTENT( IN    ) :: tau_evap     !Estimated evaporation time
INTEGER,            INTENT( INOUT ) :: lev2         !Max time level
INTEGER,            INTENT( IN    ) :: ic           !Cell ID

TYPE ( puff_dynamics ) pd

INTEGER ilev, ios, ifld, next
REAL    xrele, yrele, delxe, delye, sigxe, sigye, sigze
REAL    hp, hx, hy, uubt, vvbt, uvbt, sly, rat
REAL    sxx, syy, szz, sxy, vol, si, velc, dtx
REAL    rat1, rat2, rat3

TYPE( puff_str ), DIMENSION(:), POINTER :: ptem

REAL,    EXTERNAL :: rlimit
REAL,    EXTERNAL :: SkewFraction
INTEGER, EXTERNAL :: time_level
LOGICAL, EXTERNAL :: check_slope
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: allocatePuffAux
INTEGER, EXTERNAL :: next_puff

sigxe = 0.5*dx/xmap_evap
sigye = 0.5*dy/ymap_evap
sigze = rlimit( VONK*ustr*dts/A,5.*z0,0.8*zisrf )

CALL get_met( xx,yy,0.0,sigze,0.0,1,Shh=0.5*(sigxe**2+sigye**2),outField=ifld )

dtx   = 0.5*dts
delxe = ub*dtx
delye = vb*dtx

xrele = xx + delxe*xmap_evap
yrele = yy + delye*ymap_evap

si = msrf(3)/msrf(2) + ustr*dts

IF( lsv_oper )CALL reset_lsv( si )

!------ scale turbulence for time-averaging

IF( t_avg /= DEF_VAL_R )THEN
  velc = ub*ub + vb*vb
  CALL turb_rescale( si,sigze,velc )
END IF

CALL set_turb( uubt,vvbt,uvbt )

!---- Set composite horizontal scale for limiting lengthwise puff scale

rat1 = 2.*uubl/(uubt+vvbt+SMALL)
rat2 = 2.*vvbl/(uubt+vvbt+SMALL)
rat3 = (uub+vvb)/(uubt+vvbt+SMALL)
IF( rat1+rat2+rat3 > SMALL )THEN
  sly = rat1*sbls + rat2*sbl + rat3*sby
ELSE ! for bl_type = none
  sly = MAX(sbls,sbl,sby)
END IF

velc = SQRT(ub**2 + vb**2 + qqsh + qqb + qql)

sxx  = sigxe*sigxe + delxe*delxe + uubt*dtx**2
syy  = sigye*sigye + delye*delye + vvbt*dtx**2
sxy  = delxe*delye + uvbt*dtx**2
szz  = sigze*sigze

vol = PI3*SQRT(szz*(sxx*syy-sxy**2))

rat = msrf(2)/asrf(1)

IF( SQRT(rat+1.)*mass < evap_min*vol )GOTO 9999

IF( multiThread )THEN
  puffSplit(ic,1) = myTId
  npufTemp(myTId) = npufTemp(myTId) + 1
  puffSplit(ic,2) = npufTemp(myTId)
  IF( npufTemp(myTId) > nDimTemp(myTid) )THEN
    CALL ExpandPuffTemp( myTId )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  next = npufTemp(myTId)
  ptem => puffTemp(myTId)%p

  NULLIFY( ptem(next)%aux )

  CALL zero_puff( ptem(next) )

  ptem(next)%naux = typeID(ityp_evap)%npaux
  ios = allocatePuffAux( ptem(next) )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'srf_puff_create'
    eMessage = 'Error allocating scipuff puff auxiliary array'
    WRITE(eInform,'(A,I0,A,I0)')'Request=',ptem(next)%naux,'  : Error = ',ios
    GOTO 9999
  END IF

  CALL setPuffifld( ptem(next),ifld )

  ptem(next)%c    = mass
  ptem(next)%xbar = DBLE(xrele)
  ptem(next)%ybar = DBLE(yrele)
  ptem(next)%zbar = 0.0
  ptem(next)%sxx  = sxx
  ptem(next)%syy  = syy
  ptem(next)%sxy  = sxy
  ptem(next)%szz  = szz
  ptem(next)%si   = si
  ptem(next)%si2  = rlimit( sly,si,0.25*tau_evap*velc )
  ptem(next)%sv   = sigze
  ptem(next)%cfo  = 1.

  IF( lsmp )THEN
    CALL set_tscale( ptem(next)%si,ptem(next)%si2,velc,ptem(next)%sr )
  END IF

  IF( lter )THEN
    CALL get_topogIn( xrele,yrele,hp,hx,hy,ifld )

    ptem(next)%zbar = hp
    ptem(next)%zi   = hp + zisrf

    IF( check_slope(hx,hy) )CALL set_puff_rot( ptem(next),hx,hy )
  ELSE
    ptem(next)%zi = zisrf
  END IF

  IF( hflxsrf >= 0.0 )THEN
    ptem(next)%zc = ptem(next)%zi
  ELSE
    ptem(next)%zc = 0.0
  END IF

  ptem(next)%cc   = -mass*MAX(rat,0.01)

  ptem(next)%ityp = ityp_evap
  ptem(next)%inxt = 0
  ptem(next)%iprv = 0
  CALL setPuffipgrd( ptem(next),0 )
  ptem(next)%idtn = 0

  ptem(next)%idtl = time_level( dts )
  IF( nError /= NO_ERROR )THEN
    eRoutine = 'srf_puff_create'
    WRITE(lun_log,*,IOSTAT=ios)'******* TIME LEVEL ERROR ********'
    WRITE(lun_log,*,IOSTAT=ios)TRIM(eRoutine)
    WRITE(lun_log,*,IOSTAT=ios)TRIM(eInform)
    WRITE(lun_log,*,IOSTAT=ios)'DT=',dts
    WRITE(lun_log,*,IOSTAT=ios)'DELT=',delt
    WRITE(lun_log,*,IOSTAT=ios)'LEVEL=',ptem(next)%idtl,MAXTLV
    CALL dump_puff( 0,ptem(next) )
    GOTO 9999
  END IF

  IF( dynamic )CALL get_dynamics( ptem(next),pd )

  CALL siginv( ptem(next) )
  CALL init_tlev( ptem(next),pd,ilev,xmap_evap,ymap_evap )
  ptem(next)%idtl = MAX(ptem(next)%idtl,ilev)

!$OMP ATOMIC
  lev2 = MAX(lev2,puffTemp(myTId)%p(next)%idtl)

  IF( SkewFraction(0.0) < 1.0 )CALL setPuffiskew( ptem(next),SKEW_UP )

  IF( IsMulti(typeID(ityp_evap)%icls) )THEN
    CALL InitMCvapor( ptem(next) )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

ELSE
CALL check_newpuff()
IF( nError /= NO_ERROR )THEN
  eRoutine = 'srf_puff_create'
  GOTO 9999
END IF

next = next_puff()

CALL zero_puff( puff(next) )

puff(next)%naux = typeID(ityp_evap)%npaux
ios = allocatePuffAux( puff(next) )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'srf_puff_create'
  eMessage = 'Error allocating scipuff puff auxiliary array'
  WRITE(eInform,'(A,I0,A,I0)')'Request=',puff(next)%naux,'  : Error = ',ios
  GOTO 9999
END IF

CALL setPuffifld( puff(next),ifld )

puff(next)%c    = mass
puff(next)%xbar = xrele
puff(next)%ybar = yrele
puff(next)%zbar = 0.0
puff(next)%sxx  = sxx
puff(next)%syy  = syy
puff(next)%sxy  = sxy
puff(next)%szz  = szz
puff(next)%si   = si
puff(next)%si2  = rlimit( sly,puff(next)%si,0.25*tau_evap*velc )
puff(next)%sv   = sigze
puff(next)%cfo  = 1.

IF( lsmp )THEN
  CALL set_tscale( puff(next)%si,puff(next)%si2,velc,puff(next)%sr )
END IF

IF( lter )THEN
  CALL get_topogIn( xrele,yrele,hp,hx,hy,ifld )

  puff(next)%zbar = hp
  puff(next)%zi   = hp + zisrf

  IF( check_slope(hx,hy) )CALL set_puff_rot( puff(next),hx,hy )
ELSE
  puff(next)%zi = zisrf
END IF

IF( hflxsrf >= 0.0 )THEN
  puff(next)%zc = puff(next)%zi
ELSE
  puff(next)%zc = 0.0
END IF

puff(next)%cc   = -mass*MAX(rat,0.01)

puff(next)%ityp = ityp_evap
puff(next)%inxt = 0
puff(next)%iprv = 0
CALL setPuffipgrd( puff(next),0 )
puff(next)%idtn = 0

puff(next)%idtl = time_level( dts )
IF( nError /= NO_ERROR )THEN
  eRoutine = 'srf_puff_create'
  WRITE(lun_log,*,IOSTAT=ios)'******* TIME LEVEL ERROR ********'
  WRITE(lun_log,*,IOSTAT=ios)TRIM(eRoutine)
  WRITE(lun_log,*,IOSTAT=ios)TRIM(eInform)
  WRITE(lun_log,*,IOSTAT=ios)'DT=',dts
  WRITE(lun_log,*,IOSTAT=ios)'DELT=',delt
  WRITE(lun_log,*,IOSTAT=ios)'LEVEL=',puff(next)%idtl,MAXTLV
  CALL dump_puff( next,puff(next) )
  GOTO 9999
END IF

IF( dynamic )CALL get_dynamics( puff(next),pd )

CALL siginv( puff(next) )
CALL init_tlev( puff(next),pd,ilev,xmap_evap,ymap_evap )
puff(next)%idtl = MAX(puff(next)%idtl,ilev)

!$OMP ATOMIC
lev2 = MAX(lev2,puff(next)%idtl)

IF( SkewFraction(0.0) < 1.0 )CALL setPuffiskew( puff(next),SKEW_UP )

IF( IsMulti(typeID(ityp_evap)%icls) )THEN
  CALL InitMCvapor( puff(next) )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_puff_rot( p,hx,hy )

USE puffstruct_fd

IMPLICIT NONE

TYPE( puff_str ),     INTENT( INOUT ) :: p
REAL,                 INTENT( IN    ) :: hx, hy

REAL(8), DIMENSION(3,3) :: arot, atrot, smat, stem
REAL(8), DIMENSION(3)   :: e1, e2, e3

REAL(8) sxx, syy, sxy, s11, s22, s33, hxd, hyd
REAL(8) c, s, ang

!------ Puff horizontal spread

sxx = DBLE(p%sxx); syy = DBLE(p%syy); sxy = DBLE(p%sxy)

!------ Find horizontal principle axes

ang = 0.5*ATAN2( 2.*sxy,sxx-syy )

c = COS(ang); s = SIN(ang)

s11 = c*c*sxx + s*s*syy + 2.*s*c*sxy
s22 = s*s*sxx + c*c*syy - 2.*s*c*sxy
s33 = DBLE(p%szz)

!------ Set tensor with principal axes (assume sxz=syz=0)

smat(:,1) = (/ s11 , 0.D0, 0.D0 /)
smat(:,2) = (/ 0.D0, s22 , 0.D0 /)
smat(:,3) = (/ 0.D0, 0.D0,  s33 /)

!------ Define rotation matrix [ e1
!                                e2
!                                e3 ]

!------ Normal to terrain

hxd = DBLE(hx)
hyd = DBLE(hy)

e3 = (/ -hxd,-hyd,1.D0 /) / SQRT(1.D0+hxd*hxd+hyd*hyd)

!------ Align rotated "x" coordinate along principle axis; set e1.e2 = 0

e1 = (/ c,s,hxd*c+hyd*s /) / SQRT(1.D0+(hxd*c+hyd*s)**2)

!------ Other axis defined by cross-product

e2 = (/ -e1(2)*e3(3)+e3(2)*e1(3),e1(1)*e3(3)-e3(1)*e1(3),-e1(1)*e3(2)+e3(1)*e1(2) /)

!------ Rotation matrix

arot(:,1) = e1
arot(:,2) = e2
arot(:,3) = e3

!------ Rotate principal axes into global coordinate system

atrot = TRANSPOSE( arot )
stem = MATMUL( arot,smat )
smat = MATMUL( stem,atrot )

!------ Copy out into puff structure

p%sxx = SNGL(smat(1,1)); p%sxy = SNGL(smat(2,1)); p%sxz = SNGL(smat(3,1))
                         p%syy = SNGL(smat(2,2)); p%syz = SNGL(smat(3,2))
                                                  p%szz = SNGL(smat(3,3))

RETURN
END

!=========================================================

SUBROUTINE srf_cell_evap_2D( srf,var2d,ivar,icell0,ilev )

USE scipuff_fi
USE srfevap_fi
USE sagstr_fd
USE substrate_fi
USE srfevap2D_fd

!Sets surface vars after secondary evaporation for 2 distribution evaporation

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: srf    !Dep grid structure
TYPE(var2Distrib),   INTENT( IN ) :: var2d  !two distribution evaporation vars
INTEGER,             INTENT( IN ) :: ivar   !Start of evap block
INTEGER,             INTENT( IN ) :: ilev   !Grid refinement level
INTEGER,             INTENT( IN ) :: icell0 !Upper cell location

INTEGER i, j, icell, jcell, i0,i1,i2,i3,i4,i5,i6,i7

REAL  rate_c, rate2                                 !combined rate, rate_c**2
REAL  rate_awet1, rate_awet2, rate_diam1,rate_diam2 !tmp vars for 2D evaporation
REAL  fac1, fac2, depth_fac                         !tmp vars for 2D evaporation
REAL  oldMass,oldDiam,oldDrH,newDrH,oldDepth        !tmp vars for 2D evaporation
REAL  rate53_1, rate53_2   !rate**(5/3)

rate_c = var2d%alpha*var2d%rate1 + (1.-var2d%alpha)*var2d%rate2 !combined rate
rate2  = rate_c*rate_c  !rate_c**2

rate_awet1 = (var2d%rate1*var2d%rate1)**0.3333333  !rate**(2/3)
rate_awet2 = (var2d%rate2*var2d%rate2)**0.3333333  !rate**(2/3)
rate_diam1 = rate_awet1*rate_awet1  !rate**(4/3)
rate_diam2 = rate_awet2*rate_awet2  !rate**(4/3)

rate53_1 = var2d%rate1*rate_awet1  !rate**(5/3)
rate53_2 = var2d%rate2*rate_awet2  !rate**(5/3)

i0 = ivar

i1 = i0 + srf%mxgrd !sum variance C
i2 = i1 + srf%mxgrd !sum variance * length Scale
i3 = i2 + srf%mxgrd !m.w.diameter
i4 = i3 + srf%mxgrd !wetted area
IF( substrate_type /= 0 )THEN
  i5 = i4 + srf%mxgrd !srf mass
  i6 = i5 + srf%mxgrd !mass*depth
  i7 = i6 + srf%mxgrd !new MD^2
ELSE
  i7 = i4 + srf%mxgrd !new MD^2
END IF

!---- awet factors

fac1 =      var2d%alpha * var2d%awetRatio / var2d%beta;
fac2 = (1.-var2d%alpha) * var2d%awetRatio / var2d%gamma;

IF( substrate_type /= 0 )THEN
!--- drop height = dropVolume in substrate / dropArea
!                = (4/3)*PI*(diam/2sf)**3 / (pi * (diam/2)**2 * porosity)
!                = diam * 2/(3*sf**3 * porosity) = diam * depth_fac
  depth_fac = 2.0/(3.0*pmatl%sf**3*porosity)
END IF

DO i = 0,3

  icell = srf%ipgrd(icell0) + i

  IF( ilev == 0 )THEN
    IF( srf%ipdat(icell+i0) /= 0.0 )THEN

      oldMass = srf%ipdat(icell+i0)

      srf%ipdat(icell+i0) = srf%ipdat(icell+i0)*rate_c    !sum mean C
      srf%ipdat(icell+i1) = srf%ipdat(icell+i1)*rate2     !sum variance C
      srf%ipdat(icell+i2) = srf%ipdat(icell+i2)*rate2     !sum variance * length Scale

      oldDiam = srf%ipdat(icell+i3) / oldMass

!---- m.w.diameter = mass * (alpha*diam1*rate1^(4/3) + (1-alpha)*diam2*rate2^(4/3)

      srf%ipdat(icell+i3) = oldMass * (var2d%alpha*var2d%diam1*rate_diam1 &
                              + (1.-var2d%alpha)*var2d%diam2*rate_diam2 )

!---- wetted area = awet1*rate1^(2/3) + awet2*rate2^(2/3)

      srf%ipdat(icell+i4) = srf%ipdat(icell+i4) &
                          * (fac1*rate_awet1 + fac2*rate_awet2)

      IF( substrate_type /= 0 )THEN

!---- surface mass = old_value * (alpha*rate_a1 + (1-alpha)*rate_a2)

        srf%ipdat(icell+i5) = srf%ipdat(icell+i5) &
                            * (var2d%alpha*var2d%rate_a1 + (1.-var2d%alpha)*var2d%rate_a2)

!---- sum mean * front depth (m) =
!---- new mass * (old drop Height (oldDrH) + old depth(oldDepth) - new drop Height (newDrH))

        oldDrH = depth_fac*oldDiam
        newDrH = depth_fac*srf%ipdat(icell+i3)/srf%ipdat(icell+i0)  !new diam
        oldDepth = srf%ipdat(icell+i6) / oldMass                    !front depth (m)

!---- make sure >= 0 as drop size increased

        srf%ipdat(icell+i6) = MAX(srf%ipdat(icell+i0)*(oldDrH+oldDepth-newDrH),0.0 )

      END IF !substrate_type /= 0

!---- new MD^2 = M * [ alpha*diam1^2*rate1^(5/3) + (1-alpha)*diam2^2*rate2^(5/3) ]

      srf%ipdat(icell+i7) = oldMass * &
                    (var2d%alpha*var2d%diam1*var2d%diam1*rate53_1 &
              + (1.-var2d%alpha)*var2d%diam2*var2d%diam2*rate53_2)

    END IF 	!srf%ipdat(icell+i0) /= 0.0
  END IF  !ilev == 0

  IF( srf%ipgrd(icell) > 0 )THEN

    DO j = 0,3

      jcell   = srf%ipgrd(icell) + j
      IF( srf%ipdat(jcell+i0) /= 0.0 )THEN

        oldMass = srf%ipdat(jcell+i0)

        srf%ipdat(jcell+i0) = srf%ipdat(jcell+i0)*rate_c !sum mean C
        srf%ipdat(jcell+i1) = srf%ipdat(jcell+i1)*rate2  !sum variance C
        srf%ipdat(jcell+i2) = srf%ipdat(jcell+i2)*rate2  !sum variance * length Scale

!---- m.w.diameter = mass * (alpha*diam1*rate1^(4/3) + (1-alpha)*diam2*rate2^(4/3)

        oldDiam = srf%ipdat(jcell+i3)/oldMass
        srf%ipdat(jcell+i3) = oldMass * (var2d%alpha*var2d%diam1*rate_diam1 &
                                + (1.-var2d%alpha)*var2d%diam2*rate_diam2 )

!---- wetted area = awet1*rate1^(2/3) + awet2*rate2^(2/3)

        srf%ipdat(jcell+i4) = srf%ipdat(jcell+i4) &
                            * (fac1*rate_awet1 + fac2*rate_awet2)

        IF( substrate_type /= 0 )THEN

!---- surface mass = old_value * (alpha*rate_a1 + (1-alpha)*rate_a2)

          srf%ipdat(jcell+i5) = srf%ipdat(jcell+i5) &
                              * (var2d%alpha*var2d%rate_a1 + (1.-var2d%alpha)*var2d%rate_a2)

!---- sum mean * front depth (m) =
!---- new mass * (old drop Height (oldDrH) + old depth(oldDepth) - new drop Height (newDrH))

          oldDrH = depth_fac * oldDiam
          newDrH = depth_fac * srf%ipdat(jcell+i3)/srf%ipdat(jcell+i0) !new diam
          oldDepth = srf%ipdat(jcell+i6) / oldMass   !front depth (m)

!---- make sure >= 0 as drop size increased

          srf%ipdat(jcell+i6) = MAX(srf%ipdat(jcell+i0)*(oldDrH+oldDepth-newDrH),0.0 )

        END IF   !substrate_type /= 0

!---- new MD^2 = M * [ alpha*diam1^2*rate1^(5/3) + (1-alpha)*diam2^2*rate2^(5/3) ]

        srf%ipdat(jcell+i7) = oldMass * &
                          (var2d%alpha*var2d%diam1*var2d%diam1*rate53_1 &
                    + (1.-var2d%alpha)*var2d%diam2*var2d%diam2*rate53_2)

      END IF  !srf%ipdat(jcell+i0) /= 0.0
    END DO

  END IF   !srf%ipgrd(icell) > 0

END DO

IF( ilev == 0 )THEN
  IF( srf%ipdat(icell0+i0) /= 0.0 )THEN

    oldMass = srf%ipdat(icell0+i0)

    srf%ipdat(icell0+i0) = srf%ipdat(icell0+i0)*rate_c   !sum mean C
    srf%ipdat(icell0+i1) = srf%ipdat(icell0+i1)*rate2    !sum variance C
    srf%ipdat(icell0+i2) = srf%ipdat(icell0+i2)*rate2    !sum variance * length Scale

!---- m.w.diameter = mass * (alpha*diam1*rate1^(4/3) + (1-alpha)*diam2*rate2^(4/3)

    oldDiam = srf%ipdat(icell0+i3) / oldMass
    srf%ipdat(icell0+i3) = oldMass * (var2d%alpha*var2d%diam1*rate_diam1 &
                             + (1.-var2d%alpha)*var2d%diam2*rate_diam2 )

!---- wetted area = awet1*rate1^(2/3) + awet2*rate2^(2/3)

    srf%ipdat(icell0+i4) = srf%ipdat(icell0+i4) &
              * (fac1*rate_awet1 + fac2*rate_awet2)

    IF( substrate_type /= 0 )THEN

!---- surface mass = old_value * (alpha*rate_a1 + (1-alpha)*rate_a2)

      srf%ipdat(icell0+i5) = srf%ipdat(icell0+i5) &
                          *(var2d%alpha*var2d%rate_a1 + (1.-var2d%alpha)*var2d%rate_a2)

!---- new mass * (old drop Height (oldDrH) + old depth(oldDepth) - new drop Height (newDrH))

      oldDrH = depth_fac * oldDiam
      newDrH = depth_fac * srf%ipdat(icell0+i3)/srf%ipdat(icell0+i0)   !new diam
      oldDepth = srf%ipdat(icell0+i6)/oldMass                          !front depth (m)

!---- make sure >= 0 as drop size increased

      srf%ipdat(icell0+i6) = MAX(srf%ipdat(icell0+i0)*(oldDrH+oldDepth-newDrH),0.0)

    END IF  !substrate_type /= 0

!---- new MD^2 = M * [ alpha*diam1^2*rate1^(5/3) + (1-alpha)*diam2^2*rate2^(5/3) ]

    srf%ipdat(icell0+i7) = oldMass * &
                   (var2d%alpha*var2d%diam1*var2d%diam1*rate53_1 &
             + (1.-var2d%alpha)*var2d%diam2*var2d%diam2*rate53_2)

  END IF 	!srf%ipdat(icell0+i0) /= 0.0
END IF  !ilev == 0

RETURN
END

!=======================================================================

SUBROUTINE srf_evap_rate_2D( msrf,var2d,dsrf,depth,smass,dts )

USE scipuff_fi
USE srfevap_fi
USE srfevap2D_fd

!---- Makes a decision and performs 1 or 2 size evaporation

IMPLICIT NONE

REAL, DIMENSION(*),  INTENT( IN )    :: msrf   !surface Data
TYPE( var2Distrib ), INTENT( INOUT ) :: var2d  !two distribution evaporation vars
REAL,                INTENT( IN )    :: dsrf   !diameter on the surface
REAL,                INTENT( IN )    :: depth  !frontal depth
REAL,                INTENT( IN )    :: smass  !surface mass
REAL,                INTENT( IN )    :: dts    !time step

REAL md2   !mass * diam**2

IF( BTEST(run_mode,EVAP2D) )THEN
  IF( substrate_type == 0 )THEN   !Impermeable surface
    md2 = msrf(6)
  ELSE
    md2 = msrf(8)
  END IF

  CALL try_2Dsplit( msrf(1),dsrf,md2,msrf(5),depth,smass,dts,var2d )

  IF( var2d%is2D )THEN
    CALL srf_evap_rate( var2d%dmass1,var2d%diam1,var2d%awet1,var2d%depth1,var2d%surf_mass1,dts )
    var2d%rate1   = rate
    var2d%rate_a1 = rate_a
    var2d%rate_d1 = rate_d
    CALL srf_evap_rate( var2d%dmass2,var2d%diam2,var2d%awet2,var2d%depth2,var2d%surf_mass2,dts )
    var2d%rate2   = rate
    var2d%rate_a2 = rate_a
    var2d%rate_d2 = rate_d
  ELSE  !old way, NO two size split
    CALL srf_evap_rate( msrf(1),dsrf,msrf(5),depth,smass,dts )
  END IF
ELSE
  CALL srf_evap_rate( msrf(1),dsrf,msrf(5),depth,smass,dts )
END IF

RETURN
END

!===============================================================================

SUBROUTINE get_srf_evap_ratios( dmass,diam,mwDiam2,awet,var2D )

!---- Will calculate awet ratio (r) and mass*diam^2 ratio (s^2)
!---- necessary to decide if split is required

USE srfevap_fi
USE srfevap2D_fd

IMPLICIT NONE

REAL,              INTENT( IN )    :: dmass    !material mass stored on the surface
REAL,              INTENT( IN )    :: diam     !Mean droplet diameter (m)
REAL,              INTENT( IN )    :: mwDiam2  !Average mass weighet droplet diameter^2 (kg*m^2)
REAL,              INTENT( IN )    :: awet     !Mean wetted area
TYPE(var2Distrib), INTENT( INOUT ) :: var2d    !two distribution evaporation vars

REAL mwDiam2C   !mass weighted diam^2 calculated
REAL awetCalc   !awet calculated based on diam
REAL hvd

CALL lqd( pmatl,tsrf,rhod,cs,hvd,difd )   !---- then don't call it in srf_evap_rate()

IF( awet == 0.0 .OR. dmass == 0.0 .OR. diam == 0.0 )THEN   !set variables for one size evaporation
  var2d%awetRatio = 1.0
  var2d%d2Ratio2  = 1.0
ELSE
  mwDiam2C = dmass * diam * diam                               !calculated mass weighted diam**2
  awetCalc = 3.0 * dmass * (pmatl%sf**3)/ ( 2.0 * diam * rhod) !calculated awet
  var2d%awetRatio = awetCalc/awet
  var2d%d2Ratio2  = mwDiam2/mwDiam2C
END IF

RETURN
END

!===============================================================================

SUBROUTINE try_2Dsplit( dmass,diam,mwDiam2,awet,depth,dsurf,dts,var2D )

!--- Subroutine will determine if split is required and if so, will try to do split
!--- if successful - var2d%is2D will be set as .TRUE.
!--- if not successful or split is not required - var2d%is2D will be  .FALSE.

USE constants_fd
USE default_fd
USE srfevap_fi
USE substrate_fi
USE srfevap2D_fd

IMPLICIT NONE

REAL,              INTENT( IN )    :: dmass      !Deposition mass per unit area
REAL,              INTENT( IN )    :: diam       !Surface droplet diameter (m)
REAL,              INTENT( IN )    :: mwDiam2    !Average mass weighet droplet diameter^2 (kg*m^2)
REAL,              INTENT( IN )    :: awet       !Wetted area per unit area
REAL,              INTENT( IN )    :: depth      !Front depth (m)
REAL,              INTENT( IN )    :: dsurf      !Deposition mass per unit area on surface
REAL,              INTENT( IN )    :: dts        !Time step (s)
TYPE(var2Distrib), INTENT( INOUT ) :: var2d      !two distribution vars

var2d%is2D = .FALSE.

CALL get_srf_evap_ratios( dmass,diam,mwDiam2,awet,var2D )

IF( var2D%awetRatio < awetRatioMax .AND. var2D%d2Ratio2 > mwDiam2RatioMin )THEN   !do 2 distribution evaporation
  CALL do_split( var2d,dmass,diam,mwDiam2,awet,depth,dsurf )
END IF

RETURN
END

!===================================================================

SUBROUTINE do_split( var2d,dmass,diam,mwDiam2,awet,depth,dsurf )

!--- Will do 2 size split and will set all variables
!--- if failed - var2d%is2D will stay .FALSE. and evaporation will be done as one size dpors

USE srfevap_fi
USE SCIPresults_fd
USE default_fd
USE substrate_fi
USE srfevap2D_fd

IMPLICIT NONE

TYPE(var2Distrib), INTENT( INOUT ) :: var2d     !two distribution vars
REAL,              INTENT( IN    ) :: dmass     !Deposition mass per unit area
REAL,              INTENT( IN    ) :: diam      !Surface droplet diameter (m)
REAL,              INTENT( IN    ) :: mwDiam2   !Average mass weighet droplet diameter^2 (kg*m^2)
REAL,              INTENT( IN    ) :: awet      !Wetted area per unit area
REAL,              INTENT( IN    ) :: depth     !Front depth (m)
REAL,              INTENT( IN    ) :: dsurf     !Deposition mass per unit area on surface

REAL alpha, beta, gamma
REAL hRatio !h/(h+H) for the drop in the substrate
REAL sf3  !pmatl%sf^3

INTEGER irv

INTEGER, EXTERNAL :: getABG

irv = getABG( var2d%awetRatio,var2d%d2Ratio2,alpha,beta,gamma )

IF( beta == 0.0 .OR. gamma == 0.0 .OR. irv /= SCIPsuccess )GOTO 9999

!---- set split vars

var2d%is2D  = .TRUE.
var2d%alpha = alpha
var2d%beta  = beta
var2d%gamma = gamma

var2d%dmass1 = alpha*dmass
var2d%dmass2 = (1.0 - alpha)*dmass

var2d%diam1  = beta*diam
var2d%diam2  = gamma*diam

var2d%awet1 = alpha * var2d%awetRatio * awet / beta
var2d%awet2 = (1.0 - alpha) * var2d%awetRatio * awet / gamma

IF( porosity ==  NOT_SET_R )THEN   !Impermeable surface

  var2d%surf_mass1 = var2d%dmass1
  var2d%surf_mass2 = var2d%dmass2

  var2d%depth1 = 0.0
  var2d%depth2 = 0.0
  var2d%dropH  = 0.0
  var2d%dropH1 = 0.0
  var2d%dropH2 = 0.0

  hRatio = 1.1

ELSE       !substrate

  sf3 = pmatl%sf*pmatl%sf*pmatl%sf
  var2d%surf_mass1 = alpha*dsurf
  var2d%surf_mass2 = (1.0 - alpha)*dsurf
  var2d%dropH = 2.0*diam/(3.0 * sf3 * porosity)

!---- Make sure no underflow/overflow
!---- Ian: Check that drH is greater than 1E-4*depth,
!---- otherwise just use the smaller of the combined depth and the Hmax based on current diameter.

  hRatio = depth / ( depth + var2d%dropH )

  var2d%dropH1 = 2.0*var2d%diam1/(3.0 * sf3 * porosity)
  IF( var2d%dropH1 > 1.0E-4*depth )THEN
    var2d%depth1 = hRatio * var2d%dropH1 / (1.0-hRatio)
  ELSE
    var2d%depth1 = MIN(depth,var2d%dropH1)
  END IF

  var2d%dropH2 = 2.0*var2d%diam2/(3.0 * sf3 * porosity)
  IF( var2d%dropH2 > 1.0E-4*depth )THEN
    var2d%depth2 = hRatio * var2d%dropH2 / (1.0-hRatio)
  ELSE
    var2d%depth2 = MIN(depth,var2d%dropH2)
  END IF
END IF

9999 CONTINUE

RETURN
END

!===================================================================

INTEGER FUNCTION getABG( r,s2,alpha,beta,gamma )

!--- Gets split coefficients alpha, beta, gamma for 2 distribution evaporation
!--- where dmass1=alpha*dmass; dmass2=(1-alpha)*dmass;
!--- diam1=beta*diam;
!--- diam2=gamma*diam;

USE srfevap_fi
USE error_fi
USE SCIPresults_fd
USE srfevap2D_fd

IMPLICIT NONE

REAL, INTENT( IN )  :: r     !ratio r=awet(diam)/awet
REAL, INTENT( IN )  :: s2    !ratio s^2 = average(diam^2)/ (average(diam)^2)
REAL, INTENT( OUT ) :: alpha, beta, gamma

REAL    min_dFx                     !min function derivative
INTEGER solutionN                   !solution number

REAL, DIMENSION(4) :: solutions     !NR solutions

getABG = SCIPfailure

solutions = -HUGE(0.0)
min_dFx   =  TINY(0.0)
solutionN = 0

CALL NRgetAllSolutions( r,s2,min_dFx,solutions,solutionN )
IF( solutionN /= 1 )THEN
  gamma = 0.
  beta =  1.
  alpha = 1.
ELSE
  gamma = solutions(1)
  beta  = (s2 - gamma)/(1. - gamma)
  alpha = (s2 - gamma*gamma)/(beta*beta - gamma*gamma)
  getABG = SCIPsuccess
END IF

RETURN
END

!===================================================================

SUBROUTINE NRgetAllSolutions( r,s2,min_dFx,solutions,solutionN )

!--- gets all solutions for given r and s2
!--- using Newton Raphson method algorithm
!--- on interval [Xfrom,Xto]

USE srfevap_fi
USE srfevap2D_fd
USE SCIPresults_fd

IMPLICIT NONE

REAL,               INTENT( IN )  :: r            !awetRatio=awet(diam)/awet
REAL,               INTENT( IN )  :: s2           !d2Ratio2 or s^2 = average(diam^2)/ (average(diam)^2)
REAL,               INTENT( IN )  :: min_dFx      !min function derivative
REAL, DIMENSION(4), INTENT( OUT ) :: solutions    !possible 4 solutions
INTEGER,            INTENT( OUT ) :: solutionN    !number of solutions found

REAL    previousSolution, x0, reslt

INTEGER, EXTERNAL :: newtonRaphson

solutionN = 0
previousSolution = HUGE(0.0)
solutions = -HUGE(0.0)

DO x0 = NR_X_FROM,NR_X_TO,NR_STEP
  reslt = -HUGE(0.0)
  IF( newtonRaphson(r,s2,x0,min_dFx,reslt) == NR_SUCCESS )THEN
    IF( reslt < NR_X_FROM .OR. reslt > NR_X_TO )THEN
      previousSolution = -HUGE(0.0)
      CYCLE
    END IF
    IF( ABS(reslt - previousSolution) > NR_PRECISION*10 )THEN
      solutionN = solutionN + 1
      solutions(solutionN) = reslt
      previousSolution     = reslt
      IF( solutionN > 1 )THEN !compare with the previous and remove if the same
        IF( ABS(solutions(solutionN-1) - solutions(solutionN)) < NR_PRECISION*10. )THEN
          solutions(solutionN) = -HUGE(0.0)
          solutionN = solutionN - 1
        END IF
      END IF
    END IF
  END IF
END DO

RETURN
END

!===================================================================

INTEGER FUNCTION newtonRaphson( r,s2,initX0,min_dFx,solution ) RESULT( irv )

!--- Newton Raphson method algorithm
!--- for equation C4*x^4+C3*x^3+C4*x^4+C1*x+C0
!--- where Ci are defined by r and s2
!--- finds solution given initial guess initX0

USE srfevap_fi
USE srfevap2D_fd

IMPLICIT NONE

REAL,    INTENT( IN )   :: r        ! = awetRatio=awet(diam)/awet
REAL,    INTENT( IN )   :: s2       !=d2Ratio2 or s^2 = average(diam^2)/ (average(diam)^2)
REAL,    INTENT( IN )   :: initX0   !initial guess for x
REAL,    INTENT( IN )   :: min_dFx  !min function derivative
REAL,    INTENT( OUT )  :: solution !solution

REAL    C0, C1, C2, C3, C4  !equation coefficients
REAL    Fx, dFx             !function and it's derivative
REAL    x                   !current solutions
INTEGER i                   !loop integer

!---- getCoeff

C0 = r*s2*(s2 - 1.)
C1 = 2.*r - r*s2 - s2*s2
C2 = 3.*(s2 - r)
C3 = 3.*r - s2 - 2.
C4 = 1. - r

x = initX0

DO i = 1, NR_MAXITER, 1

  Fx = C4*x*x*x*x + C3*x*x*x + C2*x*x + C1*x + C0
  dFx = 4.*C4*x*x*x + 3.*C3*x*x + 2.*C2*x + C1

!---- dFx is too small

  IF( ABS(dFx) < TINY(0.0) )THEN
    solution = x
    irv = NR_TOO_SMALL_SLOPE
    GOTO 9999
  END IF

  solution = x - Fx/dFx

!---- DOES_NOT_CONVERGE

  IF( x == solution )THEN
    irv = NR_SUCCESS  !NR_DOES_NOT_CONVERGE
    GOTO 9999
  END IF

  IF( ABS(solution-x) < ABS(NR_PRECISION*solution) )THEN !Solution found
    irv = NR_SUCCESS
    GOTO 9999
  END IF

  x = solution

END DO

irv = NR_SUCCESS            !in case max iteration reached

9999 CONTINUE

RETURN
END
