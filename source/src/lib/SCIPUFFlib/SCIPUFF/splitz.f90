SUBROUTINE split_zi( p,ipuf,zinv )

USE scipuff_fi
USE step_p_fi

IMPLICIT NONE

REAL, PARAMETER :: ZFAC = 3.0           !Match value in step_p
REAL, PARAMETER :: MXZ2 = 1.E5          !sxx/szz
REAL, PARAMETER :: ZIZ2 = 1.E-2         !zi^2*azz
REAL, PARAMETER :: ZIX2 = 1.E-4         !zi^2*axx
REAL, PARAMETER :: SZI  = 0.1           !SZI*sz tolerance about zinv

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: ipuf
REAL,             INTENT( INOUT ) :: zinv


REAL,    DIMENSION(3)   :: normal
REAL(8), DIMENSION(3,3) :: amat, amat_t

REAL    dzz, zrfl, fac, zi2
LOGICAL lrfl_top, lrfl_bot, lrotate, lNoSplit

LOGICAL, EXTERNAL :: check_slope

!------ check space for new puff

CALL check_newpuff()
IF( nError /= NO_ERROR )GOTO 9999

!---- Rotate puff into local terrain coordinate

IF( check_slope(hx,hy) )THEN
  fac = 1.0/SQRT(1.0+hx*hx+hy*hy)
  normal(1) = -fac*hx
  normal(2) = -fac*hy
  normal(3) =  fac
  CALL set_rot_norm( normal,amat ) !Rotate into coord. aligned with terrain
  amat_t = TRANSPOSE( amat )
  CALL apply_rot_norm( p,amat,amat_t )
  sz = SQRT(p%szz)
  lrotate = .TRUE.
ELSE
  lrotate = .FALSE.
END IF

CALL siginv( p )  !Needs to be called even w/o rotation

!------ Determine vertical range for newly created puffs

zbar = p%zbar
dzz  = ZFAC*sz
zrfl = p%zc

!------ Top-most extent

IF( zrfl >= zbar + dzz .OR. zrfl == 0. )THEN
  ztop = zbar + dzz
  lrfl_top = .FALSE.
ELSE
  ztop = zrfl
  lrfl_top = .TRUE.
END IF

!------ Do not split puffs with very large aspect ratio, or
!       if zinv is above vertical extent of puff (+0.1*sz), or
!       or puff bottom is very close to zinv, or
!       if zinv is relatively small; rotate back and set cap if so.

zi2 = (zinv - hp)**2
fac = SZI*sz

lNoSplit = MAX(p%sxx,p%syy)*p%azz > MXZ2 .OR. &
                       ztop < zinv+fac   .OR. &
           MAX(hp,zbar-dzz) > zinv-fac   .OR. &
           zi2*p%azz            < ZIZ2   .OR. &
           zi2*MIN(p%axx,p%ayy) < ZIX2

IF( lNoSplit )THEN
  IF( lrotate )THEN
    CALL apply_rot_norm( p,amat_t,amat )
    CALL siginv( p )
  END IF
  p%zi = zinv
  p%zc = p%zi
  GOTO 9999
END IF

!------ Bottom reflection condition

lrfl_bot = (zbar - hp) < dzz

!------ Create new puff above zi with remainder below (in original puff)

CALL chop_puff_zi( p,ipuf,zinv,ztop,zrfl,hp,lrfl_top,lrfl_bot,lrotate,amat,amat_t )

CALL siginv( p )

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE chop_puff_zi( p,ipuf,zinv,ztop,zrfl,hp,lrfl_top,lrfl_bot,lrotate,amat,amat_t )

USE scipuff_fi
USE step_p_fi, ONLY: hx, hy
USE files_fi
USE step_p_fi, ONLY: myTId, PuffSplit, PuffTemp, npufTemp, nDimTemp, &
                     useTemp

IMPLICIT NONE

TYPE( puff_str ),        INTENT( INOUT ) :: p
INTEGER,                 INTENT( IN    ) :: ipuf
REAL,                    INTENT( IN    ) :: zinv
REAL,                    INTENT( IN    ) :: ztop
REAL,                    INTENT( IN    ) :: zrfl
REAL,                    INTENT( IN    ) :: hp
LOGICAL,                 INTENT( IN    ) :: lrfl_top, lrfl_bot, lrotate
REAL(8), DIMENSION(3,3), INTENT( IN    ) :: amat, amat_t

INTEGER next
REAL    zlow, frac, frac0
REAL    szz, delz, sxz, syz
REAL    delx, dely, sxx, syy, sxy, xmap, ymap
REAL    zbar, h1, hx1, hy1

TYPE( puff_str ), DIMENSION(:), POINTER :: ptem

INTEGER, EXTERNAL :: next_puff, getPuffifld

!------ Define map factors based on original puff

CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

!----- set puff above zi

zlow = zinv

CALL chop_puff( p,zlow,ztop,zrfl,hp,lrfl_top,lrfl_bot,frac, &
                      delz,delx,dely,sxx,sxy,sxz,syy,syz,szz )

IF( frac == 0. )THEN
  IF( lrotate )CALL apply_rot_norm( p,amat_t,amat )
  p%zi = zinv
  p%zc = p%zi
  GOTO 9999
END IF

frac0 = frac

IF( lrotate )CALL RotateDel( delx,dely,delz,amat_t )

IF( useTemp )THEN
  IF( puffSplit(ipuf,2) == 0 )puffSplit(ipuf,3) = npufTemp(myTId)
  puffSplit(ipuf,2) = puffSplit(ipuf,2) + 1
  npufTemp(myTId)   = npufTemp(myTId)   + 1
  IF( npufTemp(myTId) > nDimTemp(myTid) )THEN
    CALL ExpandPuffTemp( myTId )
    IF( nError /= NO_ERROR )GOTO 9999
  END IF

  ptem => puffTemp(myTId)%p
  next =  npufTemp(myTId)
  NULLIFY( ptem(next)%aux )

  CALL zero_puff( ptem(next) )

  CALL set_puff_xc( ptem(next),p,frac )

  ptem(next)%xbar = p%xbar + DBLE(delx*xmap)
  ptem(next)%ybar = p%ybar + DBLE(dely*ymap)
  ptem(next)%zbar = p%zbar + delz

  IF( global_lon )CALL SetGlobalLonD( ptem(next)%xbar )

  CALL get_topogIn( SNGL(ptem(next)%xbar),SNGL(ptem(next)%ybar),h1,hx1,hy1,getPuffifld(p) )

  ptem(next)%sxx = sxx
  ptem(next)%sxy = sxy
  ptem(next)%syy = syy
  ptem(next)%sxz = sxz
  ptem(next)%syz = syz
  ptem(next)%szz = szz

  IF( lrotate )CALL apply_rot_norm( ptem(next),amat_t,amat )

  IF( p%zc > 0. )THEN
    ptem(next)%zc = p%zc - hp + h1
  ELSE
    ptem(next)%zc = 0.
  END IF
  ptem(next)%zi   = zinv - hp + h1
  ptem(next)%zbar = MAX(ptem(next)%zbar,ptem(next)%zi + 1.)

  ptem(next)%ityp = p%ityp
  ptem(next)%inxt = 0
  ptem(next)%iprv = 0
  ptem(next)%idtl = p%idtl
  ptem(next)%idtn = p%idtn
  ptem(next)%ipgd = p%ipgd

  CALL siginv( ptem(next) )

ELSE

next = next_puff()

CALL zero_puff( puff(next) )

CALL set_puff_xc( puff(next),p,frac )

puff(next)%xbar = p%xbar + DBLE(delx*xmap)
puff(next)%ybar = p%ybar + DBLE(dely*ymap)
puff(next)%zbar = p%zbar + delz

IF( global_lon )CALL SetGlobalLonD( puff(next)%xbar )

CALL get_topogIn( SNGL(puff(next)%xbar),SNGL(puff(next)%ybar),h1,hx1,hy1,getPuffifld(p) )

puff(next)%sxx = sxx
puff(next)%sxy = sxy
puff(next)%syy = syy
puff(next)%sxz = sxz
puff(next)%syz = syz
puff(next)%szz = szz

IF( lrotate )CALL apply_rot_norm( puff(next),amat_t,amat )

IF( p%zc > 0. )THEN
  puff(next)%zc = p%zc - hp + h1
ELSE
  puff(next)%zc = 0.
END IF
puff(next)%zi   = zinv - hp + h1
puff(next)%zbar = MAX(puff(next)%zbar,puff(next)%zi + 1.)

puff(next)%ityp = p%ityp
puff(next)%inxt = 0
puff(next)%iprv = 0
puff(next)%idtl = p%idtl
puff(next)%idtn = p%idtn
puff(next)%ipgd = p%ipgd

CALL siginv( puff(next) )

END IF


!------ Define puff below inversion

CALL chop_puff( p,hp,zlow,zrfl,hp,lrfl_top,lrfl_bot,frac, &
                    delz,delx,dely,sxx,sxy,sxz,syy,syz,szz )

frac = 1. - frac0 !Ensure all mass is included

IF( lrotate )CALL RotateDel( delx,dely,delz,amat_t )

p%xbar = p%xbar + DBLE(delx*xmap)
p%ybar = p%ybar + DBLE(dely*ymap)
zbar   = p%zbar + delz

IF( global_lon )CALL SetGlobalLonD( p%xbar )

CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h1,hx1,hy1,getPuffifld(p) )

p%zbar = MAX(zbar,h1+0.1)

!------ Put moments in original puff

p%sxx = sxx
p%sxy = sxy
p%syy = syy
p%sxz = sxz
p%syz = syz
p%szz = szz

CALL scale_puff( p,frac )

CALL siginv( p )

IF( lrotate )THEN
  CALL apply_rot_norm( p,amat_t,amat )
  CALL siginv( p )
END IF

p%zi = zinv - hp + h1
p%zc = p%zi

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE set_puff_xc( p1,p2,frac )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str), INTENT( INOUT ) :: p1
TYPE( puff_str), INTENT( IN    ) :: p2
REAL,            INTENT( IN    ) :: frac

TYPE( puff_str_xc) p1_xc, p2_xc

p1_xc = TRANSFER(p1,p1_xc)
p2_xc = TRANSFER(p2,p2_xc)

p1_xc%psum = frac*p2_xc%psum

p1_xc%prat = p2_xc%prat

p1 = TRANSFER(p1_xc,p1)

CALL copy_scale_paux( p1,p2,frac )

RETURN
END
