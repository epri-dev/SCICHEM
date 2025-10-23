MODULE SWIMinterpPointer

USE SWIMinterp_fd
USE SWIMparam_fd
USE SWIMmetField_fd

IMPLICIT NONE

CONTAINS

!==============================================================================

RECURSIVE SUBROUTINE IntX( m,var,var0 )

!----- Interpolate horizontal field

TYPE( met1dh ),     INTENT( IN  ) :: m
REAL, DIMENSION(:), POINTER       :: var
REAL,               INTENT( OUT ) :: var0

IF( m%dxr == 0.0 )THEN
  var0 = var(1)
ELSE
  var0 = m%rm1*var(m%i) + m%rat*var(m%i+1)
END IF

RETURN
END SUBROUTINE IntX

!==============================================================================

RECURSIVE SUBROUTINE IntXY( mh,var,var0 )

!----- Interpolate horizontal field

TYPE( meth ),       INTENT( IN  ) :: mh
REAL, DIMENSION(:), POINTER       :: var
REAL,               INTENT( OUT ) :: var0

INTEGER nxi, ij

ij = mh%ij

IF( mh%cc2 + mh%cc3 + mh%cc4 /= 0. )THEN

  nxi = mh%nxi

  var0 = mh%cc1*var( ij      ) + &
         mh%cc2*var( ij+nxi  ) + &
         mh%cc3*var( ij+nxi+1) + &
         mh%cc4*var( ij+1    )

ELSE

  var0 = var(ij)

END IF

RETURN
END SUBROUTINE IntXY

!==============================================================================

RECURSIVE SUBROUTINE nIntXY( mh,var,var0 )

! ----- Get nearest point from horizontal field

TYPE( meth ),       INTENT( IN  ) :: mh
REAL, DIMENSION(:), POINTER       :: var
INTEGER,            INTENT( OUT ) :: var0

INTEGER nxi, ij
REAL    cc

IF( mh%cc2 + mh%cc3 + mh%cc4 /= 0. )THEN

  nxi = mh%nxi
  ij  = mh%ij
  cc  = mh%cc1

  IF( mh%cc2 > cc )THEN
    ij = mh%ij + nxi
    cc = mh%cc2
  END IF

  IF( mh%cc3 > cc )THEN
    ij = mh%ij + nxi + 1
    cc = mh%cc3
  END IF

  IF( mh%cc4 > cc )THEN
    ij = mh%ij + 1
    cc = mh%cc4
  END IF

ELSE

  ij = mh%ij

END IF

var0 = NINT(var(ij))

RETURN
END SUBROUTINE nIntXY

!==============================================================================

RECURSIVE SUBROUTINE mIntXY( mh,ivar,ivar0 )

! ----- Get nearest point from horizontal field

TYPE( meth ),          INTENT( IN  ) :: mh
INTEGER, DIMENSION(:), POINTER       :: ivar
INTEGER,               INTENT( OUT ) :: ivar0

INTEGER nxi, ij
REAL    cc

IF( mh%cc2 + mh%cc3 + mh%cc4 /= 0. )THEN

  nxi = mh%nxi
  ij  = mh%ij
  cc  = mh%cc1

  IF( mh%cc2 > cc )THEN
    ij = mh%ij + nxi
    cc = mh%cc2
  END IF

  IF( mh%cc3 > cc )THEN
    ij = mh%ij + nxi + 1
    cc = mh%cc3
  END IF

  IF( mh%cc4 > cc )THEN
    ij = mh%ij + 1
    cc = mh%cc4
  END IF

ELSE

  ij = mh%ij

END IF

ivar0 = ivar(ij)

RETURN
END SUBROUTINE mIntXY

!==============================================================================

RECURSIVE SUBROUTINE IntXYZ( mh,mv,f,fb,dfdx,dfdy,dfdz,dp,gx,gy,iflag,lstagger )

!------ Interpolate from a packed 3d field

IMPLICIT NONE

TYPE( meth ),            INTENT( IN ) :: mh         !Horiz. interp.
TYPE( metv ),            INTENT( IN ) :: mv         !Vert. interp.
REAL, DIMENSION(:),      POINTER      :: f          !Input array (packed 3d)
INTEGER,                 INTENT( IN ) :: iflag      !Gradient flag
REAL,                    INTENT( IN ) :: dp, gx, gy !Terrain factors
LOGICAL,                 INTENT( IN ) :: lstagger   !Staggered grid flag

REAL,                    INTENT( OUT ) :: fb               !Output value
REAL,                    INTENT( OUT ) :: dfdx, dfdy, dfdz !Gradients

INTEGER kp, nxi, km, ij, kk

REAL v1, v2, v3, v4, v5, v6, v7, v8, vm, vp
REAL rat, ratm

km = MAX(1,ABS(mv%km))
IF( lstagger .AND. iflag /= 3 )km = km + 1

!------ check if horizontal grid is 2D

IF( mh%nxyi > 1 )THEN

  ij  = mh%ij
  nxi = mh%nxi
  kk  = (km-1)*mh%nxyi

  v5 = f( ij      + kk )
  v6 = f( ij+nxi  + kk )
  v7 = f( ij+nxi+1+ kk )
  v8 = f( ij+1    + kk )
  IF( mv%facm /= 0.0 )THEN
    kk = kk + mh%nxyi
    ratm = mv%facm
    rat  = mv%f2m
    v5 = rat*v5 + ratm*f( ij      + kk )
    v6 = rat*v6 + ratm*f( ij+nxi  + kk )
    v7 = rat*v7 + ratm*f( ij+nxi+1+ kk )
    v8 = rat*v8 + ratm*f( ij+1    + kk )
  END IF
  vm  = mh%cc1*v5 + mh%cc2*v6 + mh%cc3*v7 + mh%cc4*v8

!------ check vertical grid; compute mean and vertical gradient

  IF( mv%km > 0 )THEN

    kp = mv%kp
    IF( lstagger .AND. iflag /= 3 )kp = kp + 1
    kk  = (kp-1)*mh%nxyi

    v1 = f( ij      + kk )
    v2 = f( ij+nxi  + kk )
    v3 = f( ij+nxi+1+ kk )
    v4 = f( ij+1    + kk )
    IF( mv%facp /= 0.0 )THEN
      kk = kk - mh%nxyi
      ratm = mv%facp
      rat  = mv%f2p
      v1 = rat*v1 + ratm*f( ij      + kk )
      v2 = rat*v2 + ratm*f( ij+nxi  + kk )
      v3 = rat*v3 + ratm*f( ij+nxi+1+ kk )
      v4 = rat*v4 + ratm*f( ij+1    + kk )
    END IF
    vp = mh%cc1*v1 + mh%cc2*v2 + mh%cc3*v3 + mh%cc4*v4

    fb   = mv%rzm1*vm + mv%ratz*vp
    dfdz = (vp-vm)*mv%dzr / dp

  ELSE

    v1 = v5
    v2 = v6
    v3 = v7
    v4 = v8

    fb   = vm*mv%rzm1
    dfdz = 0.

  END IF

  IF( iflag > 0 )THEN

!------ compute horizontal gradients

    vm = mv%ratz*(mh%rym1*v1 + mh%raty*v2) &
       + mv%rzm1*(mh%rym1*v5 + mh%raty*v6)
    vp = mv%ratz*(mh%raty*v3 + mh%rym1*v4) &
       + mv%rzm1*(mh%raty*v7 + mh%rym1*v8)

    dfdx = (vp-vm)*mh%dxr

    vm = mv%ratz*(mh%rxm1*v1 + mh%ratx*v4) &
       + mv%rzm1*(mh%rxm1*v5 + mh%ratx*v8)
    vp = mv%ratz*(mh%rxm1*v2 + mh%ratx*v3) &
       + mv%rzm1*(mh%rxm1*v6 + mh%ratx*v7)

    dfdy = (vp-vm)*mh%dyr

!------ Compute cartesian gradients in terrain-following coordinates

    IF( mh%lter )THEN
      dfdx = dfdx + gx*dfdz
      dfdy = dfdy + gy*dfdz
    END IF

  ELSE

    dfdx = 0.
    dfdy = 0.

  END IF ! if (iflag > 0)

ELSE

!------ compute for single horizontal location

  vm = f(km)

  IF( mv%km > 0 )THEN

    IF( mv%facm /= 0.0 )THEN
      vm = mv%f2m*vm + mv%facm*f(km+1)
    END IF
    vp = f(mv%kp)
    IF( mv%facp /= 0.0 )THEN
      vp = mv%f2p*vp + mv%facp*f(mv%kp-1)
    END IF
    fb   = mv%rzm1*vm + mv%ratz*vp
    dfdz = (vp-vm)*mv%dzr

  ELSE

    fb   = vm*mv%rzm1
    dfdz = 0.

  END IF

  dfdx = 0.
  dfdy = 0.

END IF

RETURN
END SUBROUTINE IntXYZ

!==============================================================================

SUBROUTINE IntdXYZ( mh,mv,dtdz,aa,bb,diff,dddz,dp,lstagger )

IMPLICIT NONE

! ----- Interpolates equilibrium vertical diffusivity

TYPE( meth ),            INTENT( IN  ) :: mh       !Horizontal interpolation structure
TYPE( metv ),            INTENT( IN  ) :: mv       !Verical interpolation structure
REAL,                    INTENT( IN  ) :: dtdz     !Pot'l temperature gradient
REAL, DIMENSION(:),      POINTER       :: aa, bb   !Met diffusivity arrays
REAL,                    INTENT( OUT ) :: diff     !Diffusivity
REAL,                    INTENT( OUT ) :: dddz     !Diffusivity gradient
REAL,                    INTENT( IN  ) :: dp       !Terrain factor
LOGICAL,                 INTENT( IN  ) :: lstagger !Staggered grid flag

INTEGER kp, km, kk, nxi, ij
REAL    a1, a2, a3, a4, a5, a6, a7, a8, am, ap
REAL    b1, b2, b3, b4, b5, b6, b7, b8, bm, bp
REAL    diffm, diffp, ratm, rat

km = MAX(ABS(mv%km),1)
IF( lstagger )km = km + 1

!------ check if horizontal grid is 2D

IF( mh%nxyi > 1 )THEN

  ij  = mh%ij
  nxi = mh%nxi
  kk  = (km-1)*mh%nxyi

  a5 = aa( ij      + kk )
  a6 = aa( ij+nxi  + kk )
  a7 = aa( ij+nxi+1+ kk )
  a8 = aa( ij+1    + kk )

  b5 = bb( ij      + kk )
  b6 = bb( ij+nxi  + kk )
  b7 = bb( ij+nxi+1+ kk )
  b8 = bb( ij+1    + kk )

  IF( mv%facm /= 0.0 )THEN
    kk = kk + mh%nxyi
    ratm = mv%facm
    rat  = 1.0 - ratm
    a5 = rat*a5 + ratm*aa( ij      + kk )
    a6 = rat*a6 + ratm*aa( ij+nxi  + kk )
    a7 = rat*a7 + ratm*aa( ij+nxi+1+ kk )
    a8 = rat*a8 + ratm*aa( ij+1    + kk )
    b5 = rat*b5 + ratm*bb( ij      + kk )
    b6 = rat*b6 + ratm*bb( ij+nxi  + kk )
    b7 = rat*b7 + ratm*bb( ij+nxi+1+ kk )
    b8 = rat*b8 + ratm*bb( ij+1    + kk )
  END IF

  am = mh%cc1*a5 + mh%cc2*a6 + mh%cc3*a7 + mh%cc4*a8
  bm = mh%cc1*b5 + mh%cc2*b6 + mh%cc3*b7 + mh%cc4*b8

  diffm = am/(1.0 + bm*dtdz)

!------ check vertical grid; compute mean and vertical gradient

  IF( mv%km > 0 )THEN

    kp = mv%kp
    IF( lstagger )kp = kp + 1
    kk  = (kp-1)*mh%nxyi

    a1 = aa( ij      + kk )
    a2 = aa( ij+nxi  + kk )
    a3 = aa( ij+nxi+1+ kk )
    a4 = aa( ij+1    + kk )

    b1 = bb( ij      + kk )
    b2 = bb( ij+nxi  + kk )
    b3 = bb( ij+nxi+1+ kk )
    b4 = bb( ij+1    + kk )

    IF( mv%facp /= 0.0 )THEN
      kk = kk - mh%nxyi
      ratm = mv%facp
      rat  = 1.0 - ratm
      a1 = rat*a1 + ratm*aa( ij      + kk )
      a2 = rat*a2 + ratm*aa( ij+nxi  + kk )
      a3 = rat*a3 + ratm*aa( ij+nxi+1+ kk )
      a4 = rat*a4 + ratm*aa( ij+1    + kk )
      b1 = rat*b1 + ratm*bb( ij      + kk )
      b2 = rat*b2 + ratm*bb( ij+nxi  + kk )
      b3 = rat*b3 + ratm*bb( ij+nxi+1+ kk )
      b4 = rat*b4 + ratm*bb( ij+1    + kk )
    END IF

    ap = mh%cc1*a1 + mh%cc2*a2 + mh%cc3*a3 + mh%cc4*a4
    bp = mh%cc1*b1 + mh%cc2*b2 + mh%cc3*b3 + mh%cc4*b4

    diffp = ap/(1.0 + bp*dtdz)
    diff  = mv%rzm1*diffm + mv%ratz*diffp
    dddz  = (diffp-diffm)*mv%dzr / dp

  ELSE

    diff = diffm
    dddz = 0.

  END IF

ELSE

!------ compute for single horizontal location

  am = aa(km)
  bm = bb(km)
  IF( mv%facm /= 0.0 )THEN
    ratm = mv%facm
    rat  = 1.0 - ratm
    am   = rat*am + ratm*aa(km+1)
    bm   = rat*bm + ratm*bb(km+1)
  END IF
  diffm = am/MIN(1.0 + bm*dtdz,7.)

  IF( mv%km > 0 )THEN
    kp = mv%kp
    ap = aa(kp)
    bp = bb(kp)
    IF( mv%facp /= 0.0 )THEN
      ratm = mv%facp
      rat  = 1.0 - ratm
      ap   = rat*ap + ratm*aa(kp-1)
      bp   = rat*bp + ratm*bb(kp-1)
    END IF
    diffp = ap/MIN(1.0 + bp*dtdz,7.)

    diff = mv%rzm1*diffm + mv%ratz*diffp
    dddz = (diffp-diffm)*mv%dzr

  ELSE

    diff = diffm
    dddz = 0.

  END IF

END IF

RETURN
END SUBROUTINE IntdXYZ

!==============================================================================

RECURSIVE SUBROUTINE SetZfac( zm,z,nz,mv,sz,zcap )

IMPLICIT NONE

REAL,            INTENT( IN  ) :: zm, sz, zcap
REAL, DIMENSION(:), POINTER    :: z
INTEGER,         INTENT( IN  ) :: nz
TYPE( metv    ), INTENT( OUT ) :: mv

INTEGER km, kp, k0
REAL    zplus, zminus, zl, zu

mv%km   = 0
mv%kp   = 0
mv%zm   = 0
mv%zp   = 0
mv%dzr  = 0.
mv%ratz = 0.
mv%facm = 0.
mv%facp = 0.
mv%rzm1 = 1.0
mv%f2m  = 1.0
mv%f2p  = 1.0

k0 = 1
DO WHILE( z(k0) < 0. )
  k0 = k0 + 1
  IF( k0 == nz+1 )EXIT
END DO

IF( k0 < nz )THEN

  zl = MAX(zm - sz,0.5*sz)

  IF( zcap > 0.0 )THEN
    zu = MIN(zm + sz, zcap)
  ELSE
    zu = zm + sz
  END IF

  IF( zu <= z(k0) )THEN
    mv%km   = -k0
    mv%dzr  = 1.0/(z(k0+1) - z(k0))
  ELSE IF( zl > z(nz) )THEN
    mv%km   = -nz
    mv%dzr  = 1.0/(z(nz) - z(nz-1))
  ELSE
    km = k0+1
    DO WHILE( z(km) < zl )
      km = km + 1
    END DO
    km = km - 1
    kp = km
    DO WHILE( z(kp) < zu .AND. kp < nz )
      kp = kp + 1
    END DO

! ----- Use met points if adjacent
    IF( kp - km == 1 )THEN
      zplus  = z(kp)
      zminus = z(km)
    ELSE
      mv%facp = (z(kp)-zu)/(z(kp)-z(kp-1))
      zplus   = zu
      mv%facm = (zl-z(km))/(z(km+1)-z(km))
      zminus  = zl
    END IF

! ----- Use top point but assign zu-height
    IF( zu >= z(kp) )THEN
      mv%facp = 0.0
      zplus   = zu
    END IF
! ----- Use bottom point but don't assign zl-height
    IF( zl <= z(k0) )THEN
      mv%facm = 0.0
      zminus  = z(k0)
    END IF
    mv%dzr  = 1.0/(zplus - zminus)
    mv%km   = km
    mv%kp   = kp
    mv%zm   = zminus
    mv%zp   = zplus
    mv%ratz = (zm - zminus)*mv%dzr
    mv%ratz = MAX(0.,MIN(mv%ratz,1.0))

  END IF

ELSE IF( k0 == nz )THEN

  mv%km   = -nz
  mv%kp   = 0
  mv%zm   = 0
  mv%zp   = 0
  IF( nz > 1 )mv%dzr  = 1.0/(z(nz) - z(nz-1))

END IF

mv%rzm1 = 1.0 - mv%ratz
mv%f2m  = 1.0 - mv%facm
mv%f2p  = 1.0 - mv%facp

RETURN
END SUBROUTINE SetZfac

!==============================================================================

RECURSIVE SUBROUTINE SetZfacSL( zm,z,nz,mv,sz,zcap,zlim )

IMPLICIT NONE

REAL,            INTENT( IN  ) :: zm, sz, zcap, zlim
REAL, DIMENSION(:), POINTER    :: z
INTEGER,         INTENT( IN  ) :: nz
TYPE( metv    ), INTENT( OUT ) :: mv

INTEGER km, kp, k0
REAL    zplus, zminus, zl, zu

mv%km   = 0
mv%kp   = 0
mv%zm   = 0
mv%zp   = 0
mv%dzr  = 0.
mv%ratz = 0.
mv%facm = 0.
mv%facp = 0.
mv%rzm1 = 1.0
mv%f2m  = 1.0
mv%f2p  = 1.0


k0 = 1
DO WHILE( z(k0) < zlim )   !Don't use levels below ZLIM
  k0 = k0 + 1
  IF( k0 == nz+1 )EXIT
END DO

IF( k0 < nz )THEN

  zl = MAX(zm - sz,0.5*sz)

  IF( zcap > 0.0 )THEN
    zu = MIN(zm + sz, zcap)
  ELSE
    zu = zm + sz
  END IF

  IF( zu <= z(k0) )THEN
    mv%km   = -k0
    mv%dzr  = 1.0/(z(k0+1) - z(k0))
  ELSE IF( zl > z(nz) )THEN
    mv%km   = -nz
    mv%dzr  = 1.0/(z(nz) - z(nz-1))
  ELSE
    km = k0+1
    DO WHILE( z(km) < zl )
      km = km + 1
    END DO
    km = km - 1
    kp = km
    DO WHILE( z(kp) < zu .AND. kp < nz )
      kp = kp + 1
    END DO

! ----- Use met points if adjacent
    IF( kp - km == 1 )THEN
      zplus  = z(kp)
      zminus = z(km)
    ELSE
      mv%facp = (z(kp)-zu)/(z(kp)-z(kp-1))
      zplus   = zu
      mv%facm = (zl-z(km))/(z(km+1)-z(km))
      zminus  = zl
    END IF

! ----- Use top point but assign zu-height
    IF( zu >= z(kp) )THEN
      mv%facp = 0.0
      zplus   = zu
    END IF
! ----- Use bottom point but don't assign zl-height
    IF( zl <= z(k0) )THEN
      mv%facm = 0.0
      zminus  = z(k0)
    END IF
    mv%dzr  = 1.0/(zplus - zminus)
    mv%km   = km
    mv%kp   = kp
    mv%zm   = zminus
    mv%zp   = zplus
    mv%ratz = (zm - zminus)*mv%dzr
    mv%ratz = MAX(0.,MIN(mv%ratz,1.0))

  END IF

ELSE IF( k0 == nz )THEN

  mv%km   = -nz
  mv%kp   = 0
  mv%zm   = 0
  mv%zp   = 0
  IF( nz > 1 )mv%dzr  = 1.0/(z(nz) - z(nz-1))

END IF

mv%rzm1 = 1.0 - mv%ratz
mv%f2m  = 1.0 - mv%facm
mv%f2p  = 1.0 - mv%facp

RETURN
END SUBROUTINE SetZfacSL

!===============================================================================

RECURSIVE SUBROUTINE set_mv( zb,nzb,mz,mv,zh,dp,zsl,hc,alphac,zruf,xml )

IMPLICIT NONE

INTEGER,              INTENT( IN  ) :: nzb
REAL, DIMENSION(:),   POINTER       :: zb
TYPE( metv ),         INTENT( IN  ) :: mz
TYPE( metv ),         INTENT( OUT ) :: mv
REAL,                 INTENT( IN  ) :: zh,dp
REAL,                 INTENT( IN  ) :: zsl,hc,alphac,zruf,xml

INTEGER km
REAL    fsl, fz, fm, fp, fzm, fzp

!------ check if within surface layer

mv = mz
km = MAX(ABS(mz%km),1)

IF( MIN(zb(km)*dp,zh) < zsl )THEN

  fsl = -1.

  CALL set_fsl( zh,zsl,hc,alphac,zruf,xml,fz,fsl )

  IF( nzb > 1 .AND. mz%km > 0 )THEN

    IF(  zb(mz%km)*dp >= 2.*hc )THEN  !Make sure lowest interpolation level is well above the canopy

      CALL set_fsl( mv%zm*dp,zsl,hc,alphac,zruf,xml,fm,fsl )
      CALL set_fsl( mv%zp*dp,zsl,hc,alphac,zruf,xml,fp,fsl )

      mv%ratz = mv%ratz*fz/fp
      mv%rzm1 = mv%rzm1*fz/fm

      IF( mz%facm > 0. )THEN

        CALL set_fsl( mv%zm*dp,      zsl,hc,alphac,zruf,xml,fzm,fsl )
        CALL set_fsl( zb(mv%km)*dp,  zsl,hc,alphac,zruf,xml,fm,fsl )
        CALL set_fsl( zb(mv%km+1)*dp,zsl,hc,alphac,zruf,xml,fp,fsl )

        mv%facm = mv%facm*fzm/fp
        mv%f2m  = mv%f2m*fzm/fm

        CALL set_fsl( mv%zp*dp,      zsl,hc,alphac,zruf,xml,fzp,fsl )
        CALL set_fsl( zb(mv%kp-1)*dp,zsl,hc,alphac,zruf,xml,fm,fsl )
        CALL set_fsl( zb(mv%kp)*dp,  zsl,hc,alphac,zruf,xml,fp,fsl )

        mv%facp = mv%facp*fzp/fm
        mv%f2p  = mv%f2p*fzp/fp

      END IF

    END IF

  ELSE IF( zb(km) > zruf )THEN

    CALL set_fsl( zb(km)*dp,zsl,hc,alphac,zruf,xml,fm,fsl )

    mv%rzm1 = mv%rzm1*fz/fm

  END IF

END IF

RETURN
END SUBROUTINE set_mv

!===============================================================================

SUBROUTINE Int1D(xi,ni,vi,xo,no,vo)

IMPLICIT NONE

!------ 1-d linear interpolation routine - no extrapolation
!       N.B. copied from ARAPlib interp for POINTER arguments

REAL, DIMENSION(:), POINTER :: xi, vi, xo, vo
INTEGER,       INTENT( IN ) :: ni, no

INTEGER i, ii, j, jj
REAL    rate

!------ points below xi(1)

DO i = 1,no
  IF( xo(i) >= xi(1) )GOTO 10
END DO

DO i = 1,no
  vo(i) = vi(1)
END DO

RETURN

10 CONTINUE
IF( i > 1 )THEN
  ii = i - 1
  DO i = 1,ii
    vo(i) = vi(1)
  END DO
ELSE
  ii = 0
END IF

!------ set internal points

jj = 2

DO i = ii+1,no

  DO j = jj,ni
    IF( xi(j) >= xo(i) )GOTO 20
  END DO

  GOTO 30

20 CONTINUE
  rate = (xo(i) - xi(j-1))/(xi(j) - xi(j-1))
  vo(i) = vi(j-1) + rate*(vi(j)- vi(j-1))
  jj = j

END DO

RETURN

!------ points above xi(ni)

30 CONTINUE
ii = i
DO i = ii,no
  vo(i) = vi(ni)
END DO

RETURN
END SUBROUTINE Int1D

END MODULE SWIMinterpPointer
