MODULE utilsrf

USE error_fi
USE scnRel_fi
USE matl_fi
USE surface_fi
USE sagdef_fd
USE srfparam_fd
USE srfaux_fi
USE constants_fd
USE grdlock_fi
USE step_p_fi, ONLY: myTId

IMPLICIT NONE

CONTAINS

!===============================================================================

SUBROUTINE get_delmin( srf,dels,mxlev )

!---- Finds max refinement level and minimum grid size for SAG grid

TYPE( SAGgrid_str ), POINTER       :: srf
REAL,                INTENT( OUT ) :: dels
INTEGER,             INTENT( OUT ) :: mxlev

REAL xx, yy, xmap, ymap

CALL get_max_lev( srf,mxlev )

xx = srf%xmin + 0.5*FLOAT(srf%nx-1)*srf%dx
yy = srf%ymin + 0.5*FLOAT(srf%ny-1)*srf%dy
CALL mapfac( xx,yy,xmap,ymap )

dels = MIN(srf%dx/xmap,srf%dy/ymap)*(0.5**mxlev)

RETURN
END SUBROUTINE get_delmin

!===============================================================================

SUBROUTINE get_max_lev( srf,mxlev )

TYPE( SAGgrid_str ), POINTER       :: srf
INTEGER,             INTENT( OUT ) :: mxlev

INTEGER ix, iy, icell0, mlevi, nrfm

SELECT CASE( srf%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
  CASE DEFAULT
    nrfm = 1
END SELECT

mxlev = 0

DO ix = 1,srf%nx
  DO iy = 1,srf%ny

    icell0 = (iy-1)*srf%nx + ix
    mlevi  = 0
    CALL limget( srf%ipgrd,nrfm,icell0,mlevi )
    mxlev = MAX(mxlev,mlevi)

  END DO
END DO

RETURN
END SUBROUTINE get_max_lev

!===============================================================================

RECURSIVE SUBROUTINE limget( irg,nrfm,icell0,mlev0 )

INTEGER, DIMENSION(:), POINTER         :: irg
INTEGER,               INTENT( IN )    :: nrfm
INTEGER,               INTENT( IN )    :: icell0
INTEGER,               INTENT( INOUT ) :: mlev0

INTEGER mlevx, mlev, i, icell

IF( irg(icell0) /= 0 )THEN
  mlevx = mlev0 + 1
  DO i = 0,nrfm
    icell = irg(icell0) + i
    mlev  = mlevx
    CALL limget( irg,nrfm,icell,mlev )
    mlev0 = MAX(mlev,mlev0)
  END DO
END IF

RETURN
END SUBROUTINE limget

!=======================================================================

INTEGER FUNCTION MaxNumCells( n )

INTEGER, INTENT( IN ) :: n

MaxNumCells = n + 2 - MOD(n,2)

RETURN
END FUNCTION MaxNumCells

!=======================================================================

SUBROUTINE set_sdep( sdat,c,rat,ratt,si,cmax,ratb,rattb,tscale )

REAL, INTENT( IN ) :: c
REAL, INTENT( IN ) :: rat
REAL, INTENT( IN ) :: ratt
REAL, INTENT( IN ) :: si
REAL, INTENT( IN ) :: cmax
REAL, INTENT( IN ) :: ratb
REAL, INTENT( IN ) :: rattb
REAL, INTENT( IN ) :: tscale

REAL, DIMENSION(ISRF_TOT), INTENT( OUT ) :: sdat

sdat(ISRF_C   ) = c
sdat(ISRF_CC  ) = c*rat
sdat(ISRF_CCT ) = c*ratt
sdat(ISRF_SL  ) = si
sdat(ISRF_C0  ) = cmax
sdat(ISRF_CCB ) = c*ratb
sdat(ISRF_CCTB) = c*rattb
sdat(ISRF_TSCL) = tscale

RETURN
END SUBROUTINE set_sdep

!=======================================================================

SUBROUTINE AllocSrfAux( srf_aux,mxgrd )

TYPE( SAGfield_aux ), INTENT( INOUT ) :: srf_aux
INTEGER,              INTENT( IN    ) :: mxgrd

INTEGER i, ios

ALLOCATE( srf_aux%srf_data(mxgrd),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Error allocating surface auxiliary array'
  eRoutine = 'AllocSrfAux'
  GOTO 9999
END IF

DO i = 1,mxgrd
  NULLIFY( srf_aux%srf_data(i)%data )
END DO

srf_aux%alloc = .TRUE.

9999 CONTINUE

RETURN
END SUBROUTINE AllocSrfAux

!==============================================================================

RECURSIVE SUBROUTINE processSrfAux( srf,ib,fac,icell,block )

TYPE( SAGgrid_str ),  POINTER      :: srf
INTEGER,              INTENT( IN ) :: ib     !Aux block number
REAL,                 INTENT( IN ) :: fac    !Gaussian factor
INTEGER,              INTENT( IN ) :: icell  !Cell number
TYPE( sfield_block ), INTENT( IN ) :: block

9999 CONTINUE

RETURN
END SUBROUTINE processSrfAux
END MODULE utilsrf

