SUBROUTINE InitGaussHoriz( p,zslice,sdat,s )

USE scipuff_fi
USE srfdos_fd
USE srfparam_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN  ) :: p
REAL,                  INTENT( IN  ) :: zslice
REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
TYPE( srf_gauss_str ), INTENT( OUT ) :: s

TYPE( puff_totalcc ) :: pt
REAL xbar, ybar, zbar, deth, xp, yp, zp, zfac
REAL facs, faci, args, arg, c0, vol, rat

!------ Initialize

sdat(ISRF_C   ) = 0.
sdat(ISRF_CC  ) = 0.
sdat(ISRF_CCT ) = 0.
sdat(ISRF_SL  ) = 0.
sdat(ISRF_C0  ) = NOT_SET_R
sdat(ISRF_CCB ) = NOT_SET_R
sdat(ISRF_CCTB) = NOT_SET_R
sdat(ISRF_TSCL) = NOT_SET_R

!------ Check distance from slice

xbar = SNGL(p%xbar)
ybar = SNGL(p%ybar)
zbar = p%zbar
zp   = zslice - p%zbar

IF( 0.5*zp**2/p%szz > ARGMAX )RETURN

!------ Check for Inversion location

IF( zbar < p%zc .AND. zslice > p%zc )RETURN

!------ If Puff contributes - compute peak concentration at zslice

deth = p%axx*p%ayy - p%axy**2
zfac = (p%axz*p%ayy - p%ayz*p%axy)/deth
xp   = - zp*zfac
zfac = (p%ayz*p%axx - p%axz*p%axy)/deth
yp   = - zp*zfac
rat  = 0.5/(p%det*deth)

IF( lter )THEN
  facs = 0.
ELSE
  args = zslice*zbar*rat
  facs = EXP(-args)
END IF

CALL zi_reflect( zbar,p%zc,p%zc,zslice,rat,faci )

arg  = p%axx*xp*xp+2.*p%axy*xp*yp+2.*p%axz*xp*zp &
     + p%ayy*yp*yp+2.*p%ayz*yp*zp+p%azz*zp*zp

c0 = EXP(-arg)*(1. + faci)*(1. + facs)

vol = PI3*SQRT(p%det)

sdat(ISRF_C ) = c0*p%c/vol
sdat(ISRF_CC) = c0*MAX((p%cc-p%ccb)/vol,0.)
sdat(ISRF_SL) = p%si

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc( p,pt )
  sdat(ISRF_CCT) = c0*MAX((pt%cct-pt%cctb)/vol,0.)
END IF

!------ Set 2d Gaussian parameters

s%axx = p%axx; s%axy = p%axy; s%ayy = p%ayy

CALL mapfac( xbar,ybar,s%xmap,s%ymap )
s%xbar  = xbar + xp*s%xmap
s%ybar  = ybar + yp*s%ymap

s%zp = zp

s%voli = 1.
s%facv = 0.
s%argmax = ARGMAX

IF( BTEST(run_mode,REVERSE_MODE) )s%argmax = 0.4*s%argmax

RETURN
END

!==============================================================================

SUBROUTINE SliceGaussHoriz( srf,p,s,ng,ig,cfac,zslice )

!------ Calculate puff contribution at grid locations and increment fields

USE scipuff_fi
USE sagstr_fd
USE srfdos_fd
USE accumsrf
USE utilsrf

IMPLICIT NONE

TYPE( SAGgrid_str   ), POINTER         :: srf
TYPE( puff_str ),      INTENT( IN    ) :: p
TYPE( srf_gauss_str ), INTENT( INOUT ) :: s
INTEGER,               INTENT( IN    ) :: ng
INTEGER, DIMENSION(*), INTENT( IN    ) :: ig
REAL,    DIMENSION(*), INTENT( IN    ) :: cfac
REAL,                  INTENT( IN    ) :: zslice

REAL, DIMENSION(:), ALLOCATABLE :: ccell

INTEGER mlev, m1, j1, j2, i1, i2, i, j, icell, ios, k
INTEGER i1S, i2S
REAL    zr
REAL    xp, yp, xplus, xminus, cxy, y, ys, x, xs
REAL    arg, fac, dfac, dxs, dys, xmins, ymins, h, hx, hy
REAL    xr(3), xnrm(3), znrm, deth, zfac
REAL    del

INTEGER, EXTERNAL :: getPuffifld

INTERFACE
  SUBROUTINE SetSliceSweep( s,srf,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
    USE sagstr_fd
    USE srfdos_fd
    TYPE( SAGgrid_str   ), POINTER       :: srf
    TYPE( srf_gauss_str ), INTENT( IN  ) :: s
    INTEGER,               INTENT( OUT ) :: j1, j2, m1, mlev
    REAL,                  INTENT( OUT ) :: dfac, xminus, xplus, cxy
  END SUBROUTINE SetSliceSweep
END INTERFACE

!------ Allocate

ALLOCATE( ccell(ng),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'SliceGaussHoriz'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ Setup grid sweep parameters

dxs   = srf%dx
dys   = srf%dy
xmins = srf%xmin
ymins = srf%ymin

CALL SetSliceSweep( s,srf,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )
IF( j2 < j1 )GOTO 9999

IF( s%xminS /= NOT_SET_R )THEN
  i1S = MAX(INT((s%xminS - srf%xmin)/(srf%dx*dfac)),1)
ELSE
  i1S = 1
END IF
IF( s%xmaxS /= NOT_SET_R )THEN
  i2S = MIN(INT((s%xmaxS - srf%xmin)/(srf%dx*dfac)+1),m1)
ELSE
  i2S = m1
END IF

IF( lter )THEN

  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),s%h,s%hx,s%hy,getPuffifld(p) )

  CALL puff_grnd_reflect( p%zbar-s%h,p,s%hx,s%hy,xr,xnrm,deth,znrm )
  zfac = 0.5*znrm/(p%det*deth)

  jLoopTer : DO j = j1,j2

    y  = (FLOAT(j)-0.5)*dfac
    ys = ymins + y*dys
    yp = (ys-s%ybar)/s%ymap
    x  = -cxy * yp * s%xmap

    i1 = INT(xminus + x)
    i2 = INT(xplus  + x) + 1

    IF( BTEST(run_mode,REVERSE_MODE) )THEN
      i1 = MAX(i1,i1S)
      i2 = MIN(i2,i2S)
    END IF

    IF( .NOT.global_lon )THEN
      i1 = MAX(i1,1 )
      i2 = MIN(i2,m1)
    END IF

    iLoopTer : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap

      IF( global_lon )THEN
        CALL SetGlobalGrid( x,srf%nx )
        xs = xmins + x*dxs
      END IF

      CALL get_topogIn( xs,ys,h,hx,hy,getPuffifld(p) )

      IF( h <= zslice )THEN

        arg = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

        IF( arg < s%argmax )THEN

          zr = xnrm(1)*(xp-xr(1)) + xnrm(2)*(yp-xr(2)) + &
                                    xnrm(3)*(s%zp-xr(3))
          zr  = MAX(zr,0.)
          fac = EXP(-arg) * (1. + EXP(zfac*zr))
          DO k = 1,ng
            ccell(k) = fac*cfac(k)
          END DO

          icell = 0
          CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell )
          IF( nError /= NO_ERROR )GOTO 9998

      END IF

    END IF

    END DO iLoopTer

  END DO jLoopTer

ELSE  !No terrain

  jLoop : DO j = j1,j2

    y  = (FLOAT(j)-0.5)*dfac
    ys = ymins + y*dys
    yp = (ys-s%ybar)/s%ymap
    x  = -cxy * yp * s%xmap

    i1 = INT(xminus + x)
    i2 = INT(xplus  + x) + 1

    IF( BTEST(run_mode,REVERSE_MODE) )THEN
      i1 = MAX(i1,i1S)
      i2 = MIN(i2,i2S)
    END IF

    IF( .NOT.global_lon )THEN
      i1 = MAX(i1, 1)
      i2 = MIN(i2,m1)
    END IF

    iLoop : DO i = i1,i2

      x  = (FLOAT(i)-0.5)*dfac
      xs = xmins + x*dxs
      xp = (xs-s%xbar)/s%xmap
      arg  = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp

      IF( global_lon )CALL SetGlobalGrid( x,srf%nx )

      IF( arg < s%argmax )THEN

        fac = EXP(-arg)
        DO k = 1,ng
          ccell(k) = fac*cfac(k)
        END DO

        icell = 0
        CALL accum_surf( srf,x,y,mlev,ig,ng,ccell,icell )
        IF( nError /= NO_ERROR )GOTO 9998

      END IF

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

SUBROUTINE SetSliceSweep( s,srf,j1,j2,m1,dfac,xminus,xplus,cxy,mlev )

!------ Set grid-sweep parameters for horizontal slices

USE sagstr_fd
USE srfdos_fd
USE constants_fd
USE default_fd
USE scipuff_fi, ONLY: REVERSE_MODE,run_mode

IMPLICIT NONE

TYPE( SAGgrid_str   ), POINTER       :: srf
TYPE( srf_gauss_str ), INTENT( IN  ) :: s
INTEGER,               INTENT( OUT ) :: j1, j2, m1, mlev
REAL,                  INTENT( OUT ) :: dfac, xminus, xplus, cxy

REAL    xlam, del, dely, delx, xfac, xp, yp
REAL    y1, y2
INTEGER nlev, n1

xlam = 0.5*(s%axx + s%ayy + SQRT((s%axx-s%ayy)**2+4.*s%axy**2))
del  = SQRT(0.25/xlam)
IF( BTEST(run_mode,REVERSE_MODE) )del  = 0.5*del
mlev = MAX(0,INT(LOG(del/MAX(srf%dx/s%xmap,srf%dy/s%ymap))*RLOGR2))
IF( mlev > ABS(srf%maxlev) )THEN
  srf%maxlev = ABS(srf%maxlev)
  mlev       = srf%maxlev
  srf%delmin = MIN(srf%dx/s%xmap,srf%dy/s%ymap)*(0.5**(mlev))
END IF

nlev = 2**mlev
dfac = 1.0/FLOAT(nlev)
delx = srf%dx*dfac
dely = srf%dy*dfac

xfac = 2.*s%argmax / (1.0 - s%axy*s%axy/s%axx/s%ayy)
yp   = s%ymap*SQRT(xfac/s%ayy)

m1 = srf%nx*nlev
n1 = srf%ny*nlev
y1 = s%ybar - yp
IF( s%yminS /= NOT_SET_R )y1 = MAX(y1,s%yminS)
y2 = s%ybar + yp
IF( s%ymaxS /= NOT_SET_R )y2 = MIN(y2,s%ymaxS)
j1 = INT((y1 - srf%ymin)/dely)
j2 = INT((y2 - srf%ymin)/dely) + 1

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

SUBROUTINE horizontal_topography_spv( zslice,srf )

USE field_fd
USE sagstr_fd
USE sagdef_fd

IMPLICIT NONE

REAL,                INTENT( IN ) :: zslice  !Slice relative to hmin
TYPE( SAGgrid_str ), POINTER      :: srf

INTEGER ix, iy, icell0, nrfm, i0, iv
REAL    x, y, dxs, dys, h, hx, hy, rfx, rfy

INTERFACE
  RECURSIVE SUBROUTINE setspv_horizontal( icell0,x0,y0,dx0,dy0, &
                                          nrfm,rfx,rfy,zslice,srf,spv )
    USE sagstr_fd
    INTEGER,             INTENT( IN ) :: icell0
    REAL,                INTENT( IN ) :: x0, y0
    REAL,                INTENT( IN ) :: dx0,dy0
    INTEGER,             INTENT( IN ) :: nrfm
    REAL,                INTENT( IN ) :: rfx, rfy
    REAL,                INTENT( IN ) :: zslice  !Slice relative to hmin
    TYPE( SAGgrid_str ), POINTER      :: srf
    REAL,                INTENT( IN ) :: spv     !Special value
  END SUBROUTINE setspv_horizontal
END INTERFACE

!------ Set toplevel grid spacing

dxs = srf%dx
dys = srf%dy

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
  DO iy = 1,srf%ny

    x = srf%xmin + (FLOAT(ix-1)+0.5)*dxs
    y = srf%ymin + (FLOAT(iy-1)+0.5)*dys

    icell0 = (iy-1)*srf%nx + ix

    CALL get_topogPrj( x,y,h,hx,hy )

    IF( h > zslice )THEN
      DO iv = 1,srf%mxfld
        i0 = (iv-1)*srf%mxgrd
        srf%ipdat(i0+icell0) = HP_SPV
      END DO
    END IF

    IF( srf%ipgrd(icell0) /= 0 )THEN
      CALL setspv_horizontal( icell0,x,y,dxs,dys,nrfm,rfx,rfy,zslice,&
                                                          srf,HP_SPV )
    END IF

  END DO
END DO

9999 CONTINUE

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE setspv_horizontal( icell0,x0,y0,dx0,dy0, &
                                        nrfm,rfx,rfy,zslice,srf,spv )

USE sagstr_fd

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: icell0
REAL,                INTENT( IN ) :: x0, y0
REAL,                INTENT( IN ) :: dx0,dy0
INTEGER,             INTENT( IN ) :: nrfm
REAL,                INTENT( IN ) :: rfx, rfy
REAL,                INTENT( IN ) :: zslice  !Slice relative to hmin
TYPE( SAGgrid_str ), POINTER      :: srf
REAL,                INTENT( IN ) :: spv     !Special value

INTEGER ipt, ix, iy, icell, iv, i0
REAL    xx, yy, x ,y ,dxs ,dys, h, hx, hy

DO ipt = 0,nrfm

  iy = ipt/2
  ix = ipt - 2*iy
  xx = (FLOAT(ix)-0.5)
  yy = (FLOAT(iy)-0.5)

  dxs = rfx*dx0
  dys = rfy*dy0

  x = x0 + xx*dxs
  y = y0 + yy*dys

  icell = srf%ipgrd(icell0) + ipt

  CALL get_topogPrj( x,y,h,hx,hy )
  IF( h > zslice )THEN
    DO iv = 1,srf%mxfld
      i0 = (iv-1)*srf%mxgrd
      srf%ipdat(i0+icell) = spv
    END DO
  END IF

  IF( srf%ipgrd(icell) /= 0 )THEN
    CALL setspv_horizontal( icell,x,y,dxs,dys,nrfm,rfx,rfy,&
                                             zslice,srf,spv )
  END IF

END DO

RETURN
END

