SUBROUTINE ProfileBL( fld )

!------ Set small scale turbulence in boundary layer using detailed profiles

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE SWIMinterpPointer

IMPLICIT NONE

TYPE( MetField ), INTENT( INOUT ) :: fld

INTEGER nz, nxy, izmax, is, i, k, ip
REAL    zinv, hflux, soq, tem, wwzi, denom, cd, tref, gt, zref
REAL    wtmin, L

REAL, DIMENSION(:), POINTER :: zb, d, zruf
REAL, DIMENSION(:), POINTER :: wspd2, ustr2, wstr2, zi, hflx, invL
REAL, DIMENSION(:), POINTER :: zsl, usl, vsl, ubl, vbl
REAL, DIMENSION(:), POINTER :: qq, aa, bb
REAL, DIMENSION(:), POINTER :: uu, vv, ww, wt, sl, sz
REAL, DIMENSION(:), POINTER :: tpot

REAL, EXTERNAL :: ulog, InvLfromL

!------ Set locals

zb => fld%grid%Z

IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
  i = 1 + fld%grid%nXY
ELSE
  i = 1
END IF

uu => fld%BLprof%UU(i:)
vv => fld%BLprof%VV(i:)
ww => fld%BLprof%WW(i:)
wt => fld%BLprof%WT(i:)
sl => fld%BLprof%SL(i:)
sz => fld%BLprof%SZ(i:)

wspd2 => fld%BLaux%wspd2
ustr2 => fld%BLaux%ustr2
wstr2 => fld%BLaux%wstr2
zi    => fld%BL%zi
hflx  => fld%BL%HeatFlux
invL  => fld%BL%invMOL

zsl => fld%BLaux%zsl
usl => fld%BLaux%usl
vsl => fld%BLaux%vsl
ubl => fld%BLaux%ubl
vbl => fld%BLaux%vbl

aa => fld%BLaux%aDiff(i:)
bb => fld%BLaux%bDiff(i:)
qq => fld%BLaux%qq(i:)

tpot => fld%Field%Tpot(i:)

d     => fld%grid%terrain%D
zruf  => fld%grid%landcover%roughness

nz  = fld%grid%nZ
nxy = fld%grid%nXY

!------ Loop over horizontal grid points

DO is = 1,nxy

!------ Calculate inversion height for each vertical profile

  IF( wt(is) > 0. )THEN  !Height of minimum heat flux for unstable BL

    ip = is
    wtmin = wt(is)
    zinv  = zb(1)
    DO k = 2,nz
      ip = ip + nxy
      IF( wt(ip) < wtmin )THEN
        wtmin = wt(ip)
        zinv  = zb(k)
      END IF
    END DO

  ELSE                   !Height where WW falls to 10% of maximum values for neutral/stable

    ip    = is
    wwzi  = ww(is)
    izmax = 1
    DO k = 2,nz
      ip = ip + nxy
      IF( ww(ip) > wwzi )THEN
        wwzi  = ww(ip)
        izmax = k
      END IF
    END DO

    k = izmax + 1
    i = (k-1)*nxy + is
    wwzi = 0.1*wwzi
    DO WHILE( k <= nz )
      IF( ww(i) <= wwzi )EXIT
      k = k + 1; i = i + nxy
    END DO

    IF( k > nz )THEN
      zinv = zb(nz)
    ELSE
      denom = ww(i) - ww(i-nxy)
      IF( ABS(denom) > SMALL )THEN
        zinv = zb(k-1) + (zb(k)-zb(k-1))*(wwzi-ww(i-nxy))/denom
      ELSE
        zinv = zb(k)
      END IF
    END IF

  END IF

  zinv = zinv*d(is)

  IF( BTEST(fld%type,FTB_T) )THEN
    IF( BTEST(fld%grid%type,GTB_STAGGER) )THEN
      tref = tpot(is+nxy)
    ELSE
      tref = tpot(is)
    END IF
  ELSE
    tref = TSURF
  END IF

  gt    = G0/tref
  zref  = zb(1)*d(is)
  cd    = (VONK/ulog( 0.,0.,zref,zruf(is),-999.) )**2
  hflux = wt(is)

  IF( hflux > 0. )THEN
    wstr2(is) = (gt*hflux*zinv)**TwoThirds
  ELSE
    wstr2(is) = 0.0
  END IF

  hflx(is)  = hflux
  zi(is)    = zinv
  ustr2(is) = cd*wspd2(is) !wspd2 set to U*U in calling routine

!------ Vertical profiles of qq and diffusivity coefficients

  DO k = 1,nz
    i = (k-1)*nxy + is
    qq(i) = 2.*vv(i)*(sz(i)/sl(i))**TwoThirds + 2.*uu(i) + ww(i)
    IF( qq(i) > 0.0 )THEN
      soq = sz(i)/SQRT(qq(i))
    ELSE
      soq = 1.E36
    END IF
    tem   = soq/a*ww(i)
    bb(i) = EQF*soq**2
    aa(i) = tem + MAX(-0.5*tem,bb(i)*wt(i))
  END DO

!------ Set surface layer variables for log profile below first point

  usl(is) = ubl(is)
  vsl(is) = vbl(is)
  zsl(is) = zb(1)*d(is)

  IF( ABS(hflux) <= TINY(0.) )THEN
    invL(is) = 1.E-4
  ELSE
    L = -SQRT(ustr2(is))**3 / (VONK*gt*hflux)
    invL(is) = InvLfromL( L )
  END IF

!------ Set bottom slice for staggered grids

  IF( BTEST(fld%grid%type,GTB_STAGGERZ) )THEN
    fld%BLprof%UU(is)   = uu(is)
    fld%BLprof%VV(is)   = vv(is)
    fld%BLprof%WW(is)   = ww(is)
    fld%BLprof%WT(is)   = wt(is)
    fld%BLprof%SL(is)   = sl(is)
    fld%BLprof%SZ(is)   = sz(is)
    fld%BLaux%aDiff(is) = aa(is)
    fld%BLaux%bDiff(is) = bb(is)
    fld%BLaux%qq(is)    = qq(is)
  END IF

END DO

RETURN
END
