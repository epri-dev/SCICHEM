INTEGER FUNCTION SWIMInitMG( gridp )

!------ Setup multi-grid field for McWIF based on finer "gridp" (parent)

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), POINTER :: gridp

INTEGER irv, alloc_stat, i
INTEGER nxb, nyb, nzb, nzt, nzm1, nxy, nxp, nyp
REAL    xfac, yfac

TYPE( MetGrid ), POINTER :: grid

INTERFACE

  INTEGER FUNCTION SetTerrainGrad2( gridx,gridp )
    USE SWIM_fi
    TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
    TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp
  END FUNCTION SetTerrainGrad2

  INTEGER FUNCTION McWIFsetSmooth2DMG( grid,gridp )
    USE SWIM_fI
    TYPE( MetGrid ), POINTER :: grid
    TYPE( MetGrid ), POINTER :: gridp
  END FUNCTION McWIFsetSmooth2DMG

  SUBROUTINE McWIFsmooth2dMG( grid,fldp,fld )
    USE SWIM_fi
    TYPE( MetGrid ),    POINTER :: grid
    REAL, DIMENSION(:), POINTER :: fldp
    REAL, DIMENSION(:), POINTER :: fld
  END SUBROUTINE McWIFsmooth2dMG

  SUBROUTINE McWIFsmooth3d( grid,fldp,fld,UorV )
    USE SWIM_fi
    TYPE( MetGrid ),        POINTER      :: grid
    REAL, DIMENSION(:),     POINTER      :: fldp
    REAL, DIMENSION(:),     POINTER      :: fld
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE McWIFsmooth3d

  SUBROUTINE McWIFunSmooth3d( grid,fld,fldp,UorV )
    USE SWIM_fi
    TYPE( MetGrid ),        POINTER      :: grid
    REAL, DIMENSION(:),     POINTER      :: fld
    REAL, DIMENSION(:),     POINTER      :: fldp
    CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV
  END SUBROUTINE McWIFunSmooth3d

END INTERFACE

INTEGER, EXTERNAL :: initFFTMcWIF

!------ Initialize

SWIMInitMG = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMInitMG'
error%Message = 'Error allocating Multi-grid McWIF arrays'

grid            => gridp%MGgrid
grid%prevMGgrid => gridp

grid%coord = gridp%coord
grid%type  = gridp%type
grid%Ztop  = gridp%Ztop
grid%nz    = gridp%nz

!------ Define grid (halve resolution)

grid%nX = (gridp%nX+1)/2 + 1;
grid%nY = (gridp%nY+1)/2 + 1;

grid%nXY = grid%nX*grid%nY

xfac = FLOAT(gridp%nX-2)/FLOAT(grid%nX-2)
yfac = FLOAT(gridp%nY-2)/FLOAT(grid%nY-2)
grid%dX = gridp%dX*xfac
grid%dY = gridp%dY*yfac

grid%McWIF%dxi = gridp%McWIF%dxi/xfac
grid%McWIF%dyi = gridp%McWIF%dyi/yfac

grid%Xmin = gridp%Xmin + 0.5*(gridp%dX-grid%dX)  !Keep location of flux boundary fixed
grid%Ymin = gridp%Ymin + 0.5*(gridp%dY-grid%dY)

grid%Xmax = grid%Xmin + FLOAT(grid%nX-1)*grid%dX
grid%Ymax = grid%Ymin + FLOAT(grid%nY-1)*grid%dY

!------ Save parent grid size

nxp = gridp%nx
nyp = gridp%ny

!------ Set vertical grid

nzb = grid%nZ
nzt = nzb + 1

ALLOCATE( grid%Z(nzb),grid%Zw(nzt),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

ALLOCATE( grid%McWIF%zt(nzt),grid%McWIF%z(nzt),grid%McWIF%alphaFFT(nzt), &
          grid%McWIF%dzi(nzt),grid%McWIF%dzti(nzt), &
          grid%McWIF%cza(nzt),grid%McWIF%czb(nzt),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO i = 1,nzb
  grid%Z(i) = gridp%Z(i)
END DO

DO i = 1,nzt
  grid%Zw(i)             = gridp%Zw(i)
  grid%McWIF%zt(i)       = gridp%McWIF%zt(i)
  grid%McWIF%z(i)        = gridp%McWIF%z(i)
  grid%McWIF%alphaFFT(i) = gridp%McWIF%alphaFFT(i)
  grid%McWIF%dzi(i)      = gridp%McWIF%dzi(i)
  grid%McWIF%dzti(i)     = gridp%McWIF%dzti(i)
  grid%McWIF%cza(i)      = gridp%McWIF%cza(i)
  grid%McWIF%czb(i)      = gridp%McWIF%czb(i)
END DO

nxy = grid%nXY

!------ Terrain arrays

ALLOCATE( grid%terrain%H(nxy),grid%terrain%Hx(nxy),grid%terrain%Hy(nxy), &
          grid%terrain%D(nxy),grid%terrain%Du(nxy),grid%terrain%Dv(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

!------ Allocate horizontal weight arrays

ALLOCATE( grid%McWIF%alphaU(nzt*grid%nXY),grid%McWIF%alphaV(nzt*grid%nXY),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO i = 1,nzt*grid%nXY
  grid%McWIF%alphaU(i) = 1.
  grid%McWIF%alphaV(i) = 1.
END DO

nxb  = grid%nX
nyb  = grid%nY
nzm1 = nzt-1

!------ Setup filter structures

irv = McWIFsetSmooth2DMG( grid,gridp )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Filter terrain arrays

CALL McWIFsmooth2dMG( grid,gridp%terrain%H,grid%terrain%H )

irv = SetTerrainGrad2( gridp=grid )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Set vertical partition parameter

grid%McWif%alphaMin = gridp%McWif%alphaMin
grid%McWif%alphaMax = gridp%McWif%alphaMax

!------ Setup FFT arrays

ALLOCATE( grid%McWIF%xevec(2*nxb+15),grid%McWIF%xsn(nxb), &
          grid%McWIF%xcs(nxb),grid%McWIF%xeval(nxb),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

ALLOCATE( grid%McWIF%yevec(2*nyb+15),grid%McWIF%ysn(nyb), &
          grid%McWIF%ycs(nyb),grid%McWIF%yeval(nyb),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

irv = initFFTMcWIF( nxb,nyb,grid%McWIF )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Find max slope

grid%McWIF%MaxSlope = -HUGE(0.)

DO i = 1,grid%nXY
  grid%McWIF%MaxSlope = MAX( grid%McWIF%MaxSlope,     &
                             ABS(grid%terrain%Hx(i)), &
                             ABS(grid%terrain%Hy(i)) )
END DO

grid%McWIF%Hscale = gridp%McWIF%Hscale  !Characteristic length scale from finer grid

SWIMInitMG = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

LOGICAL FUNCTION SWIMDoMG( grid ) RESULT( DoMG )

USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid

INTEGER, PARAMETER :: MG_MIN = 16

INTEGER nxc, nyc

!------ First check for special values

IF( Prj%MC%MaxIterRelax == 9999 .OR. Prj%MC%MaxIterFFT == 9999 )THEN

  DoMG = .FALSE.

ELSE

!------ Check for adequate grid size

  nxc = grid%nx/2+1
  nyc = grid%ny/2+1

  DoMG = (nxc >= MG_MIN .AND. nyc >= MG_MIN)

END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION McWIFsetSmooth2DMG( grid,gridp )

USE FilterDefinitionMG
USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ), POINTER :: grid
TYPE( MetGrid ), POINTER :: gridp

TYPE( SmoothGrid ), POINTER:: smooth

INTEGER alloc_stat, nFil, ns
INTEGER i, j, ic, im, ip, jc, jm, jp, ii, jj
REAL    rx, ry, xp, yp                              ,sumw
LOGICAL xOdd, yOdd

REAL, DIMENSION(:), ALLOCATABLE :: wtFil, wtFilStg

McWIFsetSmooth2DMG = SWIMfailure

!------ Point to smooth structure on coarse grid

smooth => grid%smooth

NULLIFY( smooth%xSmooth,smooth%ySmooth,smooth%xSmoothU,smooth%ySmoothV )

!------ Setup arrays for U and V grid locations

ALLOCATE( smooth%xSmoothU(grid%nX),smooth%ySmoothV(grid%nY),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%number   = UK_ERROR
  error%routine  = 'SetSmooth2DMG'
  error%message = 'Error allocating interpolation arrays for MG fields'
  GOTO 9999
END IF

!------ Get filter parameters

CALL GetFilterLengthMG( nFil )
ALLOCATE( wtFil(nFil),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%number  = UK_ERROR
  error%routine = 'SetSmooth2DMGMG'
  error%message = 'Error allocating interpolation arrays for MG fields'
  GOTO 9999
END IF

CALL GetFilterWeightsMG( wtFil )

ns = (nFil-1)/2

xOdd = grid%nX /= gridp%nX/2+1
yOdd = grid%nY /= gridp%nY/2+1

smooth%nXp = gridp%nX

!------ X-grid

IF( .NOT.xOdd )THEN
  smooth%xNsmthU = nFil
ELSE
  smooth%xNsmthU = nFil + 1
END IF

DO i = 1,grid%nx

  ALLOCATE( smooth%xSmoothU(i)%ip(smooth%xNsmthU), &
            smooth%xSmoothU(i)%wt(smooth%xNsmthU),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Error allocating interpolation point arrays for MG fields'
    GOTO 9999
  END IF

  IF( .NOT.xOdd )THEN
    ic = 2*i - 1
    DO ii = 1,nFil
      smooth%xSmoothU(i)%ip(ii) = ilimit( ic-ns+ii-1,1,gridp%nx )
      smooth%xSmoothU(i)%wt(ii) = wtFil(ii)
    END DO
  ELSE
    rx = FLOAT(i-1)*grid%dx/gridp%dx
    ic = INT(rx) + 1
    im = ic - ns
    ip = ic + ns + 1
    j  = 0
    DO ii = im,ip
      j = j + 1
      xp = FLOAT(ii) - 1.
      ic = ilimit( ii,1,gridp%nx )
      smooth%xSmoothU(i)%ip(j) = ic
      smooth%xSmoothU(i)%wt(j) = FilterFacMG( rx,xp )
    END DO
  END IF
  sumw = SUM(smooth%xSmoothU(i)%wt)
  IF( ABS(sumw-1.) > 0.001 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'U-location x-weights do not sum to 1'
    GOTO 9999
  END IF

END DO

!------ Y-grid

IF( .NOT.yOdd )THEN
  smooth%yNsmthV = nFil
ELSE
  smooth%yNsmthV = nFil + 1
END IF

DO j = 1,grid%ny

  ALLOCATE( smooth%ySmoothV(j)%ip(smooth%yNsmthV), &
            smooth%ySmoothV(j)%wt(smooth%yNsmthV),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Error allocating interpolation point arrays for MG fields'
    GOTO 9999
  END IF

  IF( .NOT.yOdd )THEN
    jc = 2*j - 1
    DO jj = 1,nFil
      smooth%ySmoothV(j)%ip(jj) = ilimit( jc-ns+jj-1,1,gridp%ny )
      smooth%ySmoothV(j)%wt(jj) = wtFil(jj)
    END DO
  ELSE
    ry = FLOAT(j-1)*grid%dy/gridp%dy
    jc = INT(ry) + 1
    jm = jc - ns
    jp = jc + ns + 1
    i  = 0
    sumw = 0.
    DO jj = jm,jp
      i = i + 1
      yp = FLOAT(jj) - 1.
      smooth%ySmoothV(j)%ip(i) = ilimit( jj,1,gridp%ny )
      smooth%ySmoothV(j)%wt(i) = FilterFacMG( ry,yp )
      sumw = sumw + smooth%ySmoothV(j)%wt(i)
    END DO
  END IF
  sumw = SUM(smooth%ySmoothV(j)%wt)
  IF( ABS(sumw-1.) > 0.001 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'V-location y-weights do not sum to 1'
    GOTO 9999
  END IF

END DO

!------ Setup arrays for center points

ALLOCATE( smooth%xSmooth(grid%nx),smooth%ySmooth(grid%ny),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%number  = UK_ERROR
  error%routine = 'SetSmooth2DMG'
  error%message = 'Error allocating interpolation arrays for staggered MG fields'
  GOTO 9999
END IF

ALLOCATE( wtFilStg(nFil+1),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%number  = UK_ERROR
  error%routine = 'SetSmooth2DMG'
  error%message = 'Error allocating filter weights array for staggered MG grid'
  GOTO 9999
END IF

CALL GetFilterWeightsStgMG( wtFilStg )

ns = ns + 1

!------ X-grid

IF( .NOT.xOdd )THEN
  smooth%xNsmth = nfil+1
ELSE
  smooth%xNsmth = nfil+2
END IF

DO i = 1,grid%nx

  ALLOCATE( smooth%xSmooth(i)%ip(smooth%xNsmth), &
            smooth%xSmooth(i)%wt(smooth%xNsmth),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Error allocating interpolation point arrays for x-staggered MG fields'
    GOTO 9999
  END IF

  IF( .NOT.xOdd )THEN
    DO ii = 1,nFil+1
      smooth%xSmooth(i)%ip(ii) = ilimit( 2*i-2-ns+ii,1,gridp%nx )
      smooth%xSmooth(i)%wt(ii) = wtFilStg(ii)
    END DO
  ELSE
    rx = (FLOAT(i)-1.5)*grid%dx/gridp%dx + 1.5  !Fractional index, i.e., at i=1, x=-dx/2
    ic = INT(rx+0.5)
    im = ic - ns
    ip = ic + ns
    j  = 0
    DO ii = im,ip
      j = j + 1
      xp = FLOAT(ii)
      ic = ilimit( ii,1,gridp%nx )
      smooth%xSmooth(i)%ip(j) = ic
      smooth%xSmooth(i)%wt(j) = FilterFacStgMG( rx,xp )
    END DO
  END IF
  sumw = SUM(smooth%xSmooth(i)%wt)
  IF( ABS(sumw-1.) > 0.001 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Cell center x-weights do not sum to 1'
    GOTO 9999
  END IF

END DO

!------ Y-grid

IF( .NOT.yOdd )THEN
  smooth%yNsmth = nfil + 1
ELSE
  smooth%yNsmth = nfil + 2
END IF

DO j = 1,grid%ny

  ALLOCATE( smooth%ySmooth(j)%ip(smooth%yNsmth), &
            smooth%ySmooth(j)%wt(smooth%yNsmth),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Error allocating interpolation point arrays for y-staggered MG fields'
    GOTO 9999
  END IF

  IF( .NOT.yOdd )THEN
    DO ii = 1,nFil+1
      smooth%ySmooth(j)%ip(ii) = ilimit(2*j-2-ns+ii,1,gridp%ny)
      smooth%ySmooth(j)%wt(ii) = wtFilStg(ii)
    END DO
  ELSE
    ry = (FLOAT(j)-1.5)*grid%dy/gridp%dy + 1.5  !fractional "j" index, i.e., at j=1, y=-dy/2
    jc = INT(ry+0.5)
    jm = jc - ns
    jp = jc + ns
    i  = 0
    sumw = 0.
    DO jj = jm,jp
      i = i + 1
      yp = FLOAT(jj)
      jc = ilimit( jj,1,gridp%ny )
      smooth%ySmooth(j)%ip(i) = jc
      smooth%ySmooth(j)%wt(i) = FilterFacStgMG( ry,yp )
      sumw = sumw + smooth%ySmooth(j)%wt(i)
    END DO
  END IF
  sumw = SUM(smooth%ySmooth(j)%wt)
  IF( ABS(sumw-1.) > 0.001 )THEN
    error%number  = UK_ERROR
    error%routine = 'SetSmooth2DMG'
    error%message = 'Cell center y-weights do not sum to 1'
    GOTO 9999
  END IF

END DO

McWIFsetSmooth2DMG = SWIMsuccess

9999 CONTINUE

IF( ALLOCATED(wtFil)    )DEALLOCATE( wtFil,STAT=alloc_stat )
IF( ALLOCATED(wtFilStg) )DEALLOCATE( wtFilStg,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE McWIFsmooth2dMG( grid,fldp,fld )

!------ Perform horizontal smoothing on cell-center locations

USE FilterDefinitionMG
USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ),    POINTER :: grid
REAL, DIMENSION(:), POINTER :: fldp    !Finer   (input)
REAL, DIMENSION(:), POINTER :: fld     !Coarser (output)

TYPE( SmoothGrid ), POINTER :: smooth

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

INTEGER nxs, nys
INTEGER i, j, ii, jj, ip, jp, i0, j0, is, isp
REAL    fmin, fmax

smooth => grid%smooth

nxs = smooth%xNsmth; xSmooth => smooth%xSmooth
nys = smooth%yNsmth; ySmooth => smooth%ySmooth

DO j = 1,grid%nY
  i0 = (j-1)*grid%nX
  DO i = 1,grid%nX
    is = i0 + i
    fmin =  HUGE(fmin)
    fmax = -fmin

    fld(is) = 0.
    DO jj = 1,nys
      jp  = ySmooth(j)%ip(jj)
      j0  = (jp-1)*smooth%nXp
      DO ii = 1,nxs
        ip  = xSmooth(i)%ip(ii)
        isp = j0 + ip
        fld(is) = fld(is) + ySmooth(j)%wt(jj)*xSmooth(i)%wt(ii)*fldp(isp)
        fmin = MIN(fmin,fldp(isp))
        fmax = MAX(fmax,fldp(isp))
      END DO
    END DO
    fld(is) = rlimit( fld(is),fmin,fmax )

  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE McWIFsmooth3d( grid,fldp,fld,UorV )

!------ Perform horizontal smoothing on 3d fields

USE FilterDefinitionMG
USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ),        POINTER      :: grid    !Coarse grid
REAL, DIMENSION(:),     POINTER      :: fldp    !Finer   (input)
REAL, DIMENSION(:),     POINTER      :: fld     !Coarser (output)
CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV

TYPE( SmoothGrid ),  POINTER :: smooth

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

INTEGER nxs, nys, nzt
INTEGER i, j, ii, jj, ip, jp, i0, j0, is, isp, k, i3d, i3p, nxyp

smooth => grid%smooth

nxs = smooth%xNsmth; xSmooth => smooth%xSmooth
nys = smooth%yNsmth; ySmooth => smooth%ySmooth

nzt = grid%nZ+1

nxyp = grid%prevMGgrid%nXY

IF( PRESENT(UorV) )THEN
  SELECT CASE( UorV(1:1) )
    CASE( 'U','u' )
      nxs = smooth%xNsmthU; xSmooth => smooth%xSmoothU
    CASE( 'V','v' )
      nys = smooth%yNsmthV; ySmooth => smooth%ySmoothV
  END SELECT
END IF

DO i = 1,grid%nXY*nzt
  fld(i) = 0.
END DO

DO j = 1,grid%ny
  i0 = (j-1)*grid%nX

  DO i = 1,grid%nx
    is = i0 + i

    DO jj = 1,nys
      jp = ySmooth(j)%ip(jj)
      j0 = (jp-1)*smooth%nXp
      DO ii = 1,nxs
        ip  = xSmooth(i)%ip(ii)
        isp = j0 + ip
        DO k = 1,nzt
          i3d = (k-1)*grid%nXY + is
          i3p = (k-1)*nxyp + isp
          fld(i3d) = fld(i3d) + ySmooth(j)%wt(jj)*xSmooth(i)%wt(ii)*fldp(i3p)
        END DO
      END DO
    END DO

  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE McWIFunSmooth3d( grid,fld,fldp,UorV )

!------ Perform horizontal "inverse" smoothing on 3d fields

USE FilterDefinitionMG
USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ),        POINTER      :: grid     !Coarse grid
REAL, DIMENSION(:),     POINTER      :: fld      !Input  field (coarse)
REAL, DIMENSION(:),     POINTER      :: fldp     !Output field (fine)
CHARACTER(*), OPTIONAL, INTENT( IN ) :: UorV

TYPE( MetGrid    ), POINTER :: gridp    !Fine grid
TYPE( SmoothGrid ), POINTER :: smooth

TYPE( SmoothPt ), DIMENSION(:), POINTER :: xSmooth, ySmooth

REAL, DIMENSION(:), ALLOCATABLE :: sum_wt

INTEGER nxp, nxyp, nx, ny, nxs, nys, nzt
INTEGER i, j, ii, jj, ip, jp, i0, j0, is, isp, k, i3d, i3p

gridp => grid%prevMGgrid

nxp = gridp%nX; nxyp = gridp%nXY
nx  = grid%nX;  ny  = grid%nY

nzt = grid%nZ + 1

ALLOCATE( sum_wt(nxyp),STAT=i )
IF( i /= 0 )RETURN

smooth => grid%smooth

nxs = smooth%xNsmth; xSmooth => smooth%xSmooth
nys = smooth%yNsmth; ySmooth => smooth%ySmooth

IF( PRESENT(UorV) )THEN
  SELECT CASE( UorV(1:1) )
    CASE( 'U','u' )
      nxs = smooth%xNsmthU; xSmooth => smooth%xSmoothU
    CASE( 'V','v' )
      nys = smooth%yNsmthV; ySmooth => smooth%ySmoothV
  END SELECT
END IF

DO is = 1,nxyp
  sum_wt(is) = 0.
END DO

DO i = 1,nxyp*nzt
  fldp(i) = 0.
END DO

DO j = 1,ny
  i0 = (j-1)*nx

  DO i = 1,nx
    is = i0 + i

    DO jj = 1,nys
      jp = ySmooth(j)%ip(jj)
      j0 = (jp-1)*nxp
      DO ii = 1,nxs
        ip  = xSmooth(i)%ip(ii)
        isp = j0 + ip
        DO k = 1,nzt
          i3d = (k-1)*grid%nXY + is
          i3p = (k-1)*nxyp + isp
          fldp(i3p) = fldp(i3p) + ySmooth(j)%wt(jj)*xSmooth(i)%wt(ii)*fld(i3d)
        END DO
        sum_wt(isp) = sum_wt(isp) + ySmooth(j)%wt(jj)*xSmooth(i)%wt(ii)
      END DO
    END DO

  END DO
END DO

DO k = 1,nzt
  i0 = (k-1)*nxyp
  DO i = 1,nxyp
    fldp(i0+i) = fldp(i0+i) / sum_wt(i)
  END DO
END DO

9999 CONTINUE

IF( ALLOCATED(sum_wt) )DEALLOCATE( sum_wt,STAT=i )

RETURN
END
