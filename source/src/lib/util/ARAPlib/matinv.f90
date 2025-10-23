RECURSIVE SUBROUTINE matinv( amat,bmat,cmat,ss,ff,nz )

!  Tri-diagonal matrix inversion
!
!  Solves  amat*ff(i-1) + bmat*ff(i) + cmat*ff(i+1) = ss

IMPLICIT NONE

INTEGER,             INTENT( IN )    :: nz
REAL, DIMENSION(nz), INTENT( IN )    :: amat, bmat, cmat
REAL, DIMENSION(nz), INTENT( INOUT ) :: ss
REAL, DIMENSION(nz), INTENT( OUT )   :: ff

INTEGER nzm1, kk, km, kp
REAL    den, tem

nzm1 = nz - 1

!------ Bottom boundary condition

ff(1) = -cmat(1)/bmat(1)
ss(1) =    ss(1)/bmat(1)

DO kk = 2,nzm1
  km = kk - 1
  den    =  bmat(kk) + amat(kk)*ff(km)
  ff(kk) = -cmat(kk) / den
  ss(kk) = (ss(kk) - ss(km)*amat(kk))/den
END DO

!------ Top boundary condition

kk = nz
km = nzm1
tem = ss(kk) - ss(km)*amat(kk)
den = bmat(kk) + ff(km)*amat(kk)
ff(kk) = tem/den

!------ Downsweep solution

DO kk = nzm1,1,-1
  kp = kk + 1
  ff(kk) = ss(kk) + ff(kp)*ff(kk)
END DO

RETURN
END
