SUBROUTINE InitGaussVert( srf,p,slice,sdat,s )

USE scipuff_fi
USE srfdos_fd
USE srfparam_fd
USE sagstr_fd
USE slice_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER       :: srf
TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( slice_str ),     INTENT( IN  ) :: slice
REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
TYPE( srf_gauss_str ), INTENT( OUT ) :: s

TYPE( puff_totalcc ) :: pt
REAL xlen, ylen, delx, dely, axx, axy, axz, ayy, ayz, azz, xmap, ymap
REAL dist, sdist, cdist, ex, ey, r, xp, yp, zp, arg, c0, vol, tem

!------ Initialize

sdat(ISRF_C   ) = 0.
sdat(ISRF_CC  ) = 0.
sdat(ISRF_CCT ) = 0.
sdat(ISRF_SL  ) = 0.
sdat(ISRF_C0  ) = NOT_SET_R
sdat(ISRF_CCB ) = NOT_SET_R
sdat(ISRF_CCTB) = NOT_SET_R
sdat(ISRF_TSCL) = NOT_SET_R

!------ Define map factors based on puff location

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

!------ Scale puff moment tensor by map factors

axx = p%axx / (xmap*xmap)       ! axx in inverse prj coord. squared
axy = p%axy / (xmap*ymap)
ayy = p%ayy / (ymap*ymap)
axz = p%axz / xmap
ayz = p%ayz / ymap
azz = p%azz

!------ Define unit vectors in prj coord. (slice is a straight line in
!                                          prj coord., not cartesian)

xlen = slice%xmax - slice%xmin
ylen = slice%ymax - slice%ymin

dist = SQRT( xlen*xlen + ylen*ylen ) ! prj coord.
ex   = xlen / dist
ey   = ylen / dist

!------ Find location of max. concentration (min. arg) along slice

delx = slice%xmin - SNGL(p%xbar)      !Prj coord.
dely = slice%ymin - SNGL(p%ybar)

tem = (axz*ex + ayz*ey) / azz

r  = -( axx*ex*delx + axy*(ex*dely+ey*delx) + ayy*ey*dely - tem*(delx*axz+dely*ayz) ) / &
       (axx*ex*ex + 2.*axy*ex*ey + ayy*ey*ey - tem*tem*azz )   ! non-dimensional

zp = -( axz*(delx+r*ex) + ayz*(dely+r*ey) ) / azz
xp = slice%xmin + r*ex          !Prj coord.
yp = slice%ymin + r*ey

!------ Check distance from slice

delx = xp - SNGL(p%xbar)              !Prj coord.
dely = yp - SNGL(p%ybar)

arg = axx*delx*delx + ayy*dely*dely + 2.*axy*delx*dely  &
                                    + 2.*axz*delx*zp + 2.*ayz*dely*zp + azz*zp*zp

IF( arg > ARGMAX )RETURN

!------ Compute factors at this location on slice

c0  = EXP(-arg)
vol = PI3*SQRT(p%det)

sdat(ISRF_C ) = c0*p%c/vol
sdat(ISRF_CC) = c0*MAX((p%cc-p%ccb)/vol,0.)
sdat(ISRF_SL) = p%si

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc(p,pt)
  sdat(ISRF_CCT) = c0*MAX((pt%cct-pt%cctb)/vol,0.)
END IF

!------ Set 2d Gaussian parameters

xlen  = xlen/xmap; ylen = ylen/ymap   !Meters
cdist = SQRT( xlen*xlen + ylen*ylen ) !Meters

ex = xlen / cdist ; ey = ylen / cdist

s%axx = ex*ex*p%axx + 2.*ex*ey*p%axy + ey*ey*p%ayy !Rotate into slice coord.
s%axy = ex*p%axz + ey*p%ayz
s%ayy = p%azz

sdist  = FLOAT(srf%nx)*srf%dx      !Slice length in plot coord.
s%xbar = srf%xmin + sdist*r / dist
s%ybar = p%zbar + zp

s%xmap = sdist / cdist
s%ymap = 1.

s%voli = 1.
s%facv = 0.

s%argmax = ARGMAX

!------ Save slice horizontal coordinates

s%xp = slice%xmin; s%yp = slice%ymin

s%hx = slice%xmax - slice%xmin
s%hy = slice%ymax - slice%ymin

RETURN
END

!==============================================================================

SUBROUTINE SliceGaussVert( srf,p,s,ng,ig,cfac )

!------ Calculate puff contribution at grid locations and increment fields

USE scipuff_fi
USE met_fi
USE sagstr_fd
USE srfdos_fd
USE utilsrf
USE accumsrf

IMPLICIT NONE

TYPE( SAGgrid_str   ), POINTER      :: srf
TYPE( puff_str ),      INTENT( IN ) :: p
TYPE( srf_gauss_str ), INTENT( IN ) :: s
INTEGER,               INTENT( IN ) :: ng
INTEGER, DIMENSION(*), INTENT( IN ) :: ig
REAL,    DIMENSION(*), INTENT( IN ) :: cfac

REAL, DIMENSION(:), ALLOCATABLE :: ccell

INTEGER ios, j1, j2, m1, mlev, i, j, i1, i2, k, icell
REAL    dxs, dys, xmins, ymins, xfac, yfac, dfac
REAL    xminus, xplus, cxy, h, hx, hy, xmap, ymap
REAL    x, xs, xp, y, ys, yp, xprj, yprj, arg
REAL    xm, ym, zm, facr, facs, faci, fac, zr
REAL    xr(3), xnrm(3), znrm, deth, rat, zp, zfac, hp
REAL    xbar, ybar
REAL    del

INTEGER, EXTERNAL :: getPuffifld

INTERFACE
  SUBROUTINE SetSliceVSweep( s,srf,hmin,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
    USE sagstr_fd
    USE srfdos_fd
    TYPE( srf_gauss_str ), INTENT( IN  ) :: s
    TYPE( SAGgrid_str   ), POINTER       :: srf
    REAL,                  INTENT( IN  ) :: hmin
    INTEGER,               INTENT( OUT ) :: j1, j2, m1, mlev
    REAL,                  INTENT( OUT ) :: dfac, xminus, xplus, cxy
  END SUBROUTINE SetSliceVSweep
END INTERFACE

!------ Allocate

ALLOCATE( ccell(ng),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SliceGaussHoriz'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ Setup grid sweep parameters

xbar = SNGL(p%xbar)
ybar = SNGL(p%ybar)

dxs   = srf%dx
dys   = srf%dy
xmins = srf%xmin
ymins = srf%ymin - hmin

xfac = s%hx/FLOAT(srf%nx)
yfac = s%hy/FLOAT(srf%nx)

CALL SetSliceVSweep( s,srf,hmin,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
IF( j2 < j1 )GOTO 9999

!------ Compute factors for reflections

deth = p%axx*p%ayy - p%axy**2
rat  = 0.5/(p%det*deth)

IF( lter )THEN

  CALL mapfac( xbar,ybar,xmap,ymap )

  CALL get_topogIn( xbar,ybar,hp,hx,hy,getPuffifld(p) )

  zp = p%zbar - hp
  CALL puff_grnd_reflect( zp,p,hx,hy,xr,xnrm,deth,znrm )
  zfac = 0.5*znrm/(p%det*deth)

  jLoopTer : DO j = j1,j2

    y  = FLOAT(j)-0.5
    ys = ymins + y*dys
    yp = (ys-s%ybar)/s%ymap

    x  = -cxy * yp * s%xmap

    i1 = INT(xminus + x)
    i2 = INT(xplus  + x) + 1

    IF( .NOT.global_lon )THEN
      i1 = MAX( i1,1 )
      i2 = MIN( i2,m1)
    END IF

    zm = ys - p%zbar

    iLoopTer : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap

      xprj = s%xp + x*xfac
      yprj = s%yp + x*yfac

      IF( global_lon )THEN
        IF( yprj < xmins .OR. yprj > xmins + FLOAT(srf%nx)*dxs )CYCLE
        CALL SetGlobalLon( xprj )
      END IF

      CALL get_topogIn( xprj,yprj,h,hx,hy,getPuffifld(p) )

      IF( ys < h )CYCLE iLoopTer ! check for height below ground

      arg  = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( arg > s%argmax )CYCLE iLoopTer  ! check for significant contribution

      CALL zi_reflect( p%zbar,p%zc,p%zc-hp+h,ys,rat,faci ) !Move cap with terrain as in SCIP 3.2
      faci = 1. + faci

      xm = (xprj-xbar)/xmap
      ym = (yprj-ybar)/ymap

      zr = xnrm(1)*(xm-xr(1)) + xnrm(2)*(ym-xr(2)) + &
                                          xnrm(3)*(zm-xr(3))
      zr  = MAX(zr,0.)
      fac = EXP(-arg) * (1. + EXP(zfac*zr))*faci
      DO k = 1,ng
        ccell(k) = fac*cfac(k)
      END DO

      icell = 0
      CALL accum_surfv( srf,x,y,mlev,ig,ng,ccell,icell )
      IF( nError /= NO_ERROR )GOTO 9998

    END DO iLoopTer

  END DO jLoopTer

ELSE  !No terrain

  jLoop : DO j = j1,j2

    y  = FLOAT(j)-0.5
    ys = ymins + y*dys

    IF( ys < 0.0 )CYCLE jLoop ! check for height below ground

    yp = (ys-s%ybar)/s%ymap

    x  = -cxy * yp * s%xmap
    i1 = MAX( INT(xminus + x)    , 1 )
    i2 = MIN( INT(xplus  + x) + 1, m1)

    CALL zi_reflect( p%zbar,p%zc,p%zc,ys,rat,faci )
    arg = ys*p%zbar*rat
    facs = EXP(-arg)
    facr = (1.+facs)*(1.+faci)

    iLoop : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap

      arg  = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( arg > s%argmax )CYCLE iLoop ! Check for significant contribution

      fac  = EXP(-arg) * facr
      DO k = 1,ng
        ccell(k) = fac*cfac(k)
      END DO

      icell = 0
      CALL accum_surfv( srf,x,y,mlev,ig,ng,ccell,icell )
      IF( nError /= NO_ERROR )GOTO 9998

    END DO iLoop

  END DO jLoop

END IF

9999 CONTINUE

IF( ALLOCATED(ccell) )DEALLOCATE( ccell,STAT=ios )

RETURN

9998 CONTINUE

eMessage = 'Insufficient surface cells available'
CALL get_delmin( srf,del,mlev )
WRITE(eInform,*) 'Current refinement level is ',mlev
eAction  = 'Try decreasing the refinement in OPTIONS'
GOTO 9999

END

!==============================================================================

SUBROUTINE SetSliceVSweep( s,srf,hmin,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )

!------ Set grid-sweep parameters for vertical slices

USE sagstr_fd
USE srfdos_fd
USE constants_fd

IMPLICIT NONE

TYPE( srf_gauss_str ), INTENT( IN  ) :: s
TYPE( SAGgrid_str   ), POINTER       :: srf
REAL,                  INTENT( IN  ) :: hmin
INTEGER,               INTENT( OUT ) :: j1, j2, m1, mlev
REAL,                  INTENT( OUT ) :: dfac, xminus, xplus, cxy

REAL    del, dely, delx, xfac, xp, yp, hfac
INTEGER nlev, n1

hfac = MIN( MAX(FLOAT(srf%ny-5)/25.,1.) , 4. )

del  = 0.25/s%axx/(hfac**2)

mlev = MAX(0,INT(0.5*LOG(del/(srf%dx/s%xmap)**2)*RLOGR2))
IF( mlev > ABS(srf%maxlev) )THEN
  srf%maxlev = ABS(srf%maxlev)
  mlev       = srf%maxlev
  srf%delmin = srf%dx/s%xmap*(0.5**(mlev))
END IF

nlev = 2**mlev
dfac = 1.0/FLOAT(nlev)
delx = srf%dx*dfac
dely = srf%dy

xfac = 2.*s%argmax / (1.0 - s%axy*s%axy/s%axx/s%ayy)
yp   = s%ymap*SQRT(xfac/s%ayy)

m1 = srf%nx*nlev
n1 = srf%ny

j1 = INT((s%ybar - yp - (srf%ymin-hmin))/dely)
j2 = INT((s%ybar + yp - (srf%ymin-hmin))/dely) + 1

j1 = MAX(j1,1)
j2 = MIN(j2,n1)

IF( j2 >= j1 )THEN

  xp     = SQRT(s%argmax/s%axx) * s%xmap
  xplus  = (s%xbar + xp - srf%xmin) / delx
  xminus = (s%xbar - xp - srf%xmin) / delx
  cxy    = s%axy/s%axx / delx

END IF

RETURN
END

!==============================================================================

SUBROUTINE vertical_topography_spv( slice,srf,hmin )

USE field_fd
USE sagstr_fd
USE sagdef_fd
USE slice_fd

IMPLICIT NONE

TYPE( slice_str ),   INTENT( IN ) :: slice ! slice definition structure
TYPE( SAGgrid_str ), POINTER      :: srf
REAL,                INTENT( IN ) :: hmin  !reference height

INTEGER ix, iy, icell0, nrfm, i0, iv
REAL    x, y, dxs, dys, h, hx, hy, rfx, rfy, xt, yt, dxt, dyt

INTERFACE
  RECURSIVE SUBROUTINE setspv_vertical( icell0,x0,y0,dx0,dy0, &
                                            nrfm,rfx,rfy,slice,srf,spv )
    USE sagstr_fd
    USE slice_fd
    INTEGER,             INTENT( IN ) :: icell0
    REAL,                INTENT( IN ) :: x0, y0
    REAL,                INTENT( IN ) :: dx0,dy0
    INTEGER,             INTENT( IN ) :: nrfm
    REAL,                INTENT( IN ) :: rfx, rfy
    TYPE( slice_str ),   INTENT( IN ) :: slice ! slice definition structure
    TYPE( SAGgrid_str ), POINTER      :: srf
    REAL,                INTENT( IN ) :: spv     !Special value
  END SUBROUTINE setspv_vertical
END INTERFACE

!------ Set toplevel grid spacing

dxs = srf%dx
dys = srf%dy

dxt = slice%xmax - slice%xmin
dyt = slice%ymax - slice%ymin

!------ Set refinement parameter

SELECT CASE( srf%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
    rfx  = 0.5
    rfy  = 0.5
  CASE( SAG_GRID_HORZ )
    nrfm = 1
    rfx  = 0.5
    rfy  = 1.0
  CASE( SAG_GRID_VERT )
    nrfm = 1
    rfx  = 1.0
    rfy  = 0.5
  CASE( SAG_GRID_NONE )
    nrfm = -1
    rfx  = 1.0
    rfy  = 1.0
  CASE DEFAULT
    GOTO 9999
END SELECT

!------ Loop over top level cells

DO ix = 1,srf%nx

  x = srf%xmin + (FLOAT(ix-1)+0.5)*dxs

  xt = slice%xmin + x*dxt
  yt = slice%ymin + x*dyt
  CALL get_topogPrj( xt,yt,h,hx,hy )

  DO iy = 1,srf%ny

    y = srf%ymin + (FLOAT(iy-1)+0.5)*dys - hmin

    icell0 = (iy-1)*srf%nx + ix

    IF( h > y )THEN
      DO iv = 1,srf%mxfld
        i0 = (iv-1)*srf%mxgrd
        srf%ipdat(i0+icell0) = HP_SPV
      END DO
    END IF

    IF( srf%ipgrd(icell0) /= 0 )THEN
      CALL setspv_vertical( icell0,x,y,dxs,dys,nrfm,rfx,rfy,slice,srf,HP_SPV )
    END IF

  END DO
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE setspv_vertical( icell0,x0,y0,dx0,dy0, &
                                      nrfm,rfx,rfy,slice,srf,spv )

USE sagstr_fd
USE slice_fd

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: icell0
REAL,                INTENT( IN ) :: x0, y0
REAL,                INTENT( IN ) :: dx0,dy0
INTEGER,             INTENT( IN ) :: nrfm
REAL,                INTENT( IN ) :: rfx, rfy
TYPE( slice_str ),   INTENT( IN ) :: slice ! slice definition structure
TYPE( SAGgrid_str ), POINTER      :: srf
REAL,                INTENT( IN ) :: spv     !Special value

INTEGER ipt, ix, iy, icell, iv, i0
REAL    xx, yy, x ,y ,dxs ,dys, h, hx, hy, xt, yt, dxt, dyt

dxt = slice%xmax - slice%xmin
dyt = slice%ymax - slice%ymin

DO ipt = 0,nrfm

  iy = ipt/2
  ix = ipt - 2*iy
  xx = (FLOAT(ix)-0.5)
  yy = (FLOAT(iy)-0.5)

  dxs = rfx*dx0
  dys = rfy*dy0

  x = x0 + xx*(dx0-dxs)
  y = y0 + yy*(dy0-dys)

  icell = srf%ipgrd(icell0) + ipt

  xt = slice%xmin + x*dxt
  yt = slice%ymin + x*dyt
  CALL get_topogPrj( xt,yt,h,hx,hy )
  IF( h > y )THEN
    DO iv = 1,srf%mxfld
      i0 = (iv-1)*srf%mxgrd
      srf%ipdat(i0+icell) = spv
    END DO
  END IF

  IF( srf%ipgrd(icell) /= 0 )THEN
    CALL setspv_vertical( icell,x,y,dxs,dys,nrfm,rfx,rfy,slice,srf,spv )
  END IF

END DO

RETURN
END

!==============================================================================

SUBROUTINE InitGaussHint( srf,p,slice,sdat,s )

USE scipuff_fi
USE srfdos_fd
USE srfparam_fd
USE sagstr_fd
USE slice_fd

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER       :: srf
TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( slice_str ),     INTENT( IN  ) :: slice
REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
TYPE( srf_gauss_str ), INTENT( OUT ) :: s

TYPE( puff_totalcc ) :: pt
REAL xlen, ylen, delx, dely, axx, axy, axz, ayy, ayz, azz, xmap, ymap
REAL dist, sdist, cdist, ex, ey, r, xp, yp, vol, ra33

!------ Initialize

sdat(ISRF_C   ) = 0.
sdat(ISRF_CC  ) = 0.
sdat(ISRF_CCT ) = 0.
sdat(ISRF_SL  ) = 0.
sdat(ISRF_C0  ) = NOT_SET_R
sdat(ISRF_CCB ) = NOT_SET_R
sdat(ISRF_CCTB) = NOT_SET_R
sdat(ISRF_TSCL) = NOT_SET_R

!------ Define map factors based on puff location

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

!------ Scale puff moment tensor by map factors

axx = p%axx / (xmap*xmap)       ! axx in inverse prj coord. squared
axy = p%axy / (xmap*ymap)
ayy = p%ayy / (ymap*ymap)
axz = p%axz / xmap
ayz = p%ayz / ymap
azz = p%azz

!------ Define unit vectors in prj coord. (slice is a straight line in
!                                          prj coord., not cartesian)

xlen = slice%xmax - slice%xmin
ylen = slice%ymax - slice%ymin

dist = SQRT( xlen*xlen + ylen*ylen ) ! prj coord.
ex   = xlen / dist
ey   = ylen / dist

!------ Find location of max. concentration (min. arg) along slice

delx = SNGL(p%xbar) - slice%xmin     !Prj coord.
dely = SNGL(p%ybar) - slice%ymin
r    = ex*delx + ey*dely

xp = slice%xmin + r*ex          !Prj coord.
yp = slice%ymin + r*ey

!------ Set 2d Gaussian parameters

xlen  = xlen/xmap; ylen = ylen/ymap   !Meters
cdist = SQRT( xlen*xlen + ylen*ylen ) !Meters

ex = xlen / cdist ; ey = ylen / cdist

s%axx = ex*ex*p%axx + 2.*ex*ey*p%axy + ey*ey*p%ayy !Rotate into slice coord.
s%axy = ex*p%axz + ey*p%ayz
s%ayy = p%azz

sdist  = FLOAT(srf%nx)*srf%dx      !Slice length in plot coord.
s%xbar = srf%xmin + sdist*r / dist
s%ybar = p%zbar

s%xmap = sdist / cdist
s%ymap = 1.

s%voli = 1.
s%facv = 0.

s%argmax = ARGMAX

!------ Compute factors for slice

vol  = PI3*SQRT(p%det)
azz  = ey*ey*p%axx - 2.*ex*ey*p%axy + ex*ex*p%ayy !Rotate into slice coord.
ra33 = SQRT(PI/azz)

sdat(ISRF_SL) = 2.*p%si
sdat(ISRF_C ) = ra33*p%c/vol
sdat(ISRF_CC) = ra33*MAX((p%cc-p%ccb)/vol,0.)*sdat(ISRF_SL)

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc( p,pt )
  sdat(ISRF_CCT) = ra33*MAX((pt%cct-pt%cctb)/vol,0.)*sdat(ISRF_SL)
END IF

!------ Save slice horizontal coordinates

s%xp = slice%xmin; s%yp = slice%ymin

s%hx = slice%xmax - slice%xmin
s%hy = slice%ymax - slice%ymin

RETURN
END

!==============================================================================

SUBROUTINE SliceGaussHint( srf,p,s,ng,ig,cfac )

!------ Calculate puff contribution at grid locations and increment fields

USE scipuff_fi
USE met_fi
USE sagstr_fd
USE srfdos_fd
USE utilsrf
USE accumsrf

IMPLICIT NONE

TYPE( SAGgrid_str   ), POINTER      :: srf
TYPE( puff_str ),      INTENT( IN ) :: p
TYPE( srf_gauss_str ), INTENT( IN ) :: s
INTEGER,               INTENT( IN ) :: ng
INTEGER, DIMENSION(*), INTENT( IN ) :: ig
REAL,    DIMENSION(*), INTENT( IN ) :: cfac

REAL, DIMENSION(:), ALLOCATABLE :: ccell

INTEGER ios, j1, j2, m1, mlev, i, j, i1, i2, k, icell
REAL    dxs, dys, xmins, ymins, xfac, yfac, dfac, del
REAL    xminus, xplus, cxy, h, hx, hy, xmap, ymap
REAL    x, xs, xp, y, ys, yp, xprj, yprj, arg
REAL    xm, ym, zm, facr, facs, faci, fac, zr
REAL    xr(3), xnrm(3), znrm, deth, rat, zp, zfac, hp
REAL    xbar, ybar

INTEGER, EXTERNAL :: getPuffifld

INTERFACE
  SUBROUTINE SetSliceVSweep( s,srf,hmin,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
    USE sagstr_fd
    USE srfdos_fd
    TYPE( srf_gauss_str ), INTENT( IN  ) :: s
    TYPE( SAGgrid_str   ), POINTER       :: srf
    REAL,                  INTENT( IN  ) :: hmin
    INTEGER,               INTENT( OUT ) :: j1, j2, m1, mlev
    REAL,                  INTENT( OUT ) :: dfac, xminus, xplus, cxy
  END SUBROUTINE SetSliceVSweep
END INTERFACE

!------ Allocate

ALLOCATE( ccell(ng),STAT=ios )
IF( ios /= 0 )THEN
  nError = UK_ERROR
  eRoutine = 'SliceGaussHoriz'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ Setup grid sweep parameters

xbar = SNGL(p%xbar)
ybar = SNGL(p%ybar)

dxs   = srf%dx
dys   = srf%dy
xmins = srf%xmin
ymins = srf%ymin - hmin

xfac = s%hx/FLOAT(srf%nx)
yfac = s%hy/FLOAT(srf%nx)

CALL SetSliceVSweep( s,srf,hmin,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
IF( j2 < j1 )GOTO 9999

!------ Compute factors for reflections

deth = p%axx*p%ayy - p%axy**2
rat  = 0.5/(p%det*deth)

IF( lter )THEN

  CALL mapfac( xbar,ybar,xmap,ymap )

  CALL get_topogIn( xbar,ybar,hp,hx,hy,getPuffifld(p) )

  zp = p%zbar - hp
  CALL puff_grnd_reflect( zp,p,hx,hy,xr,xnrm,deth,znrm )
  zfac = 0.5*znrm/(p%det*deth)

  jLoopTer : DO j = j1,j2

    y  = FLOAT(j)-0.5
    ys = ymins + y*dys
    yp = (ys-s%ybar)/s%ymap

    x  = -cxy * yp * s%xmap

    i1 = INT(xminus + x)
    i2 = INT(xplus  + x) + 1

    IF( .NOT.global_lon )THEN
      i1 = MAX( i1,1 )
      i2 = MIN( i2,m1)
    END IF

    zm = ys - p%zbar

    iLoopTer : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap

      xprj = s%xp + x*xfac
      yprj = s%yp + x*yfac

      IF( global_lon )THEN
        IF( yprj < xmins .OR. yprj > xmins + FLOAT(srf%nx)*dxs )CYCLE
        CALL SetGlobalLon( xprj )
      END IF

      CALL get_topogIn( xprj,yprj,h,hx,hy,getPuffifld(p) )

      IF( ys < h )CYCLE iLoopTer ! check for height below ground

      arg  = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( arg > s%argmax )CYCLE iLoopTer  ! check for significant contribution

      CALL zi_reflect( p%zbar,p%zc,p%zc-hp+h,ys,rat,faci ) !Move cap with terrain as in SCIP 3.2
      faci = 1. + faci

      xm = (xprj-xbar)/xmap
      ym = (yprj-ybar)/ymap

      zr = xnrm(1)*(xm-xr(1)) + xnrm(2)*(ym-xr(2)) + &
                                          xnrm(3)*(zm-xr(3))
      zr  = MAX(zr,0.)
      fac = EXP(-arg) * (1. + EXP(zfac*zr))*faci
      DO k = 1,ng
        ccell(k) = fac*cfac(k)
      END DO

      icell = 0
      CALL accum_surfv( srf,x,y,mlev,ig,ng,ccell,icell )
      IF( nError /= NO_ERROR )GOTO 9998

    END DO iLoopTer

  END DO jLoopTer

ELSE  !No terrain

  jLoop : DO j = j1,j2

    y  = FLOAT(j)-0.5
    ys = ymins + y*dys

    IF( ys < 0.0 )CYCLE jLoop ! check for height below ground

    yp = (ys-s%ybar)/s%ymap

    x  = -cxy * yp * s%xmap
    i1 = MAX( INT(xminus + x)    , 1 )
    i2 = MIN( INT(xplus  + x) + 1, m1)

    CALL zi_reflect( p%zbar,p%zc,p%zc,ys,rat,faci )
    arg = ys*p%zbar*rat
    facs = EXP(-arg)
    facr = (1.+facs)*(1.+faci)

    iLoop : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap

      arg  = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( arg > s%argmax )CYCLE iLoop ! Check for significant contribution

      fac  = EXP(-arg) * facr
      DO k = 1,ng
        ccell(k) = fac*cfac(k)
      END DO

      icell = 0
      CALL accum_surfv( srf,x,y,mlev,ig,ng,ccell,icell )
      IF( nError /= NO_ERROR )GOTO 9998

    END DO iLoop

  END DO jLoop

END IF

9999 CONTINUE

IF( ALLOCATED(ccell) )DEALLOCATE( ccell,STAT=ios )

RETURN

9998 CONTINUE

eMessage = 'Insufficient surface cells available'
CALL get_delmin( srf,del,mlev )
WRITE(eInform,*) 'Current refinement level is ',mlev
eAction  = 'Try decreasing the refinement in OPTIONS'
GOTO 9999

END
