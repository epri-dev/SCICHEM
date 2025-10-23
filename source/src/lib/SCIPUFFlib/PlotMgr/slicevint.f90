SUBROUTINE InitGaussVint( p,sdat,s )

USE scipuff_fi
USE srfdos_fd
USE srfparam_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN  ) :: p
REAL, DIMENSION(*),    INTENT( OUT ) :: sdat
TYPE( srf_gauss_str ), INTENT( OUT ) :: s

TYPE( puff_totalcc ) pt

REAL vol, ra33

!------ Set peak vertically-integrated concentration

vol  = PI3*SQRT(p%det)
ra33 = SQRT(PI/p%azz)

sdat(ISRF_SL) = 2.*p%sv
sdat(ISRF_C ) = ra33*p%c/vol
sdat(ISRF_CC) = ra33*MAX((p%cc-p%ccb)/vol,0.)*sdat(ISRF_SL)

IF( typeID(p%ityp)%ltot )THEN
  CALL get_totalcc( p,pt )
  sdat(ISRF_CCT) = ra33*MAX((pt%cct-pt%cctb)/vol,0.)*sdat(ISRF_SL)
END IF

!------ Set 2d Gaussian parameters

s%axx = p%axx - p%axz*p%axz/p%azz
s%ayy = p%ayy - p%ayz*p%ayz/p%azz
s%axy = p%axy - p%axz*p%ayz/p%azz

s%xbar = SNGL(p%xbar)
s%ybar = SNGL(p%ybar)

CALL mapfac( s%xbar,s%ybar,s%xmap,s%ymap )

s%xp = 0.; s%yp = 0.; s%zp = 0.

s%voli = 1.
s%facv = 0.

s%argmax = ARGMAX

RETURN
END

!==============================================================================

SUBROUTINE SliceGaussVint( srf,s,ng,ig,cfac )

!------ Calculate puff contribution at grid locations and increment fields

USE scipuff_fi
USE sagstr_fd
USE srfdos_fd
USE accumsrf
USE utilsrf
USE slice_fd

IMPLICIT NONE

TYPE( SAGgrid_str   ), POINTER      :: srf
TYPE( srf_gauss_str ), INTENT( IN ) :: s
INTEGER,               INTENT( IN ) :: ng
INTEGER, DIMENSION(*), INTENT( IN ) :: ig
REAL,    DIMENSION(*), INTENT( IN ) :: cfac

REAL, DIMENSION(:), ALLOCATABLE :: ccell

INTEGER mlev, m1, j1, j2, i1, i2, i, j, icell, ios, k
INTEGER i1S, i2S
REAL    xp, yp, xplus, xminus, cxy, y, ys, x, xs
REAL    arg, fac, dfac, dxs, dys, xmins, ymins
REAL    del

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
  eRoutine = 'SliceGaussVint'
  eMessage = 'Error allocating slice stuff'
  GOTO 9999
END IF

!------ save grid parameters

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

    IF( global_lon )CALL SetGlobalGrid( x,srf%nx )

    arg = s%axx*xp*xp + 2.*s%axy*xp*yp + s%ayy*yp*yp
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
