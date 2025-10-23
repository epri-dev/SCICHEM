RECURSIVE SUBROUTINE split( ipuf,lsplit )

USE scipuff_fi
USE files_fi
USE step_p_fi, ONLY: lblcap, myTId, puffSplit, puffTemp, npufTemp, nDimTemp
USE error_fi,  ONLY: multiThread, singleThread

!------ split if puffs get too big in the vertical and/or horizontal

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ipuf   ! puff number
LOGICAL, INTENT( INOUT ) :: lsplit ! split flag

INTEGER jpuf, j, naux, ityp, ios, ifld, next, mpuf, mchk
REAL    conc_min, chkx, chky, chkz
REAL    hp, hx, hy, fac, snn
LOGICAL lrotate, ltest, need_rot
LOGICAL ldense

REAL, DIMENSION(3)   :: normal
REAL(8), DIMENSION(3,3) :: amat, amat_t
INTEGER, DIMENSION(8) :: npList

TYPE( puff_dynamics ) pd

TYPE( puff_str ), DIMENSION(:), POINTER :: ptem

LOGICAL, EXTERNAL :: chk_cc
LOGICAL, EXTERNAL :: check_slope
LOGICAL, EXTERNAL :: dense_effect
INTEGER, EXTERNAL :: getPuffifld
REAL,    EXTERNAL :: DzSplit, DxSplit
INTEGER, EXTERNAL :: next_puff

!------ Skip small puffs

IF( puff(ipuf)%c <= cmin )RETURN

!------ Initialize for splitting checks

ityp = puff(ipuf)%ityp
ifld = getPuffifld( puff(ipuf) )

!------ Don't split puffs in polar caps

IF( ifld == polefld_n .OR. ifld == polefld_s )RETURN

naux = typeID(ityp)%npaux

IF( BTEST(run_mode,REVERSE_MODE) )THEN
  conc_min = 0.0
ELSE
  conc_min = material(typeID(ityp)%imat)%prop(3)
END IF

mpuf         = 1
npList(mpuf) = ipuf
npList(2:)   = 0

ldense   = .FALSE.
need_rot = .FALSE.
lrotate  = .FALSE.
ltest    = .FALSE.
snn      = puff(ipuf)%szz

IF( lter )THEN
  IF( puff(ipuf)%zbar < MAX(puff(ipuf)%zi,puff(ipuf)%zc) )THEN
    CALL get_topogIn( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),hp,hx,hy,ifld )
    IF( check_slope(hx,hy) )THEN
      fac = 1.0/SQRT(1.0+hx*hx+hy*hy)
      normal(1) = -fac*hx
      normal(2) = -fac*hy
      normal(3) =  fac
      snn = normal(1)*normal(1)*puff(ipuf)%sxx + &
            normal(2)*normal(2)*puff(ipuf)%syy + &
            normal(3)*normal(3)*puff(ipuf)%szz + &
        2.0*normal(1)*normal(2)*puff(ipuf)%sxy + &
        2.0*normal(1)*normal(3)*puff(ipuf)%sxz + &
        2.0*normal(2)*normal(3)*puff(ipuf)%syz
      IF( (snn < 0.5*puff(ipuf)%szz) .OR. (puff(ipuf)%zbar-hp < 3.*puff(ipuf)%sv) )THEN
        need_rot = .TRUE.
      ELSE
        snn = puff(ipuf)%szz
      END IF
    END IF
  ELSE
    hp = 0.0
  END IF
  IF( dense_gas )THEN
    CALL get_dynamics( puff(ipuf),pd )
    ldense = ((pd%dudx+pd%dvdy)*puff(ipuf)%c*(puff(ipuf)%sv**2) > puff(ipuf)%zwc) &
         .AND. dense_effect( puff(ipuf)%zbar-hp,puff(ipuf)%sv,pd%wcp,puff(ipuf)%c )
  END IF

END IF

!----- Vertical split check

chkz = DzSplit( puff(ipuf),ifld,.TRUE.)  !facv*delz2

IF( multiThread )ptem => puffTemp(myTId)%p

IF( snn > chkz .AND. chk_cc(puff(ipuf),conc_min) )THEN
  IF( .NOT.(lsplitz .AND. lblcap .AND. (puff(ipuf)%zbar < puff(ipuf)%zi)) )THEN
 IF( multiThread )THEN
    IF( puffSplit(ipuf,2) == 0 )puffSplit(ipuf,3) = npufTemp(myTId)
      puffSplit(ipuf,2) = puffSplit(ipuf,2) + 1
      npufTemp(myTId)   = npufTemp(myTId)   + 1
      IF( npufTemp(myTId) > nDimTemp(myTid) )THEN
        CALL ExpandPuffTemp( myTId )
        IF( nError /= NO_ERROR )GOTO 9999
        ptem => puffTemp(myTId)%p
      END IF
      next         = npufTemp(myTId)
      mpuf         = mpuf + 1
      npList(mpuf) = npufTemp(myTId)
    ELSE
    CALL check_splitpuff()
    IF( nError == SZ_ERROR )GOTO 9998
    IF( nError /= NO_ERROR )GOTO 9999
    next = next_puff()
    mpuf         = mpuf + 1
    npList(mpuf) = next
    ENDIF
    IF( need_rot .AND. .NOT.lrotate )THEN
      CALL set_rot_norm( normal,amat ) !Rotate into coord. aligned with terrain
      amat_t = TRANSPOSE( amat )
      CALL apply_rot_norm( puff(ipuf),amat,amat_t )
      lrotate = .TRUE.
    END IF
    IF( multiThread )THEN
      NULLIFY( ptem(next)%aux )
      CALL zsplit( puff(ipuf),ptem(next),ldense,lrotate,amat_t )
    ELSE
    CALL zsplit( puff(ipuf),puff(next),ldense,lrotate,amat_t )
    END IF
  END IF
END IF

chkx = DxSplit( puff(ipuf),ifld,ldense )
IF( chkx < 0. )GOTO 1000

chky = chkx

!------ x-direction split

mchk = mpuf
DO j = 1,mchk
  jpuf = npList(j)
  IF( j == 1 .OR. singleThread )THEN
    ltest = puff(jpuf)%sxx > chkx .AND. chk_cc(puff(jpuf),conc_min)
  ELSE
    ltest = ptem(jpuf)%sxx > chkx .AND. chk_cc(ptem(jpuf),conc_min)
  END IF
  IF( ltest )THEN
    IF( multiThread )THEN
      IF( puffSplit(ipuf,2) == 0 )puffSplit(ipuf,3) = npufTemp(myTId)
      puffSplit(ipuf,2) = puffSplit(ipuf,2) + 1
      npufTemp(myTId)   = npufTemp(myTId)   + 1
      IF( npufTemp(myTId) > nDimTemp(myTid) )THEN
        CALL ExpandPuffTemp( myTId )
        IF( nError /= NO_ERROR )GOTO 9999
        ptem => puffTemp(myTId)%p
      END IF
      next         = npufTemp(myTId)
      mpuf         = mpuf + 1
      npList(mpuf) = npufTemp(myTId)
    ELSE
      CALL check_splitpuff()
      IF( nError == SZ_ERROR )GOTO 9998
      IF( nError /= NO_ERROR )GOTO 9999
      next = next_puff()
      mpuf         = mpuf + 1
      npList(mpuf) = next
    END IF
    IF( need_rot .AND. .NOT.lrotate )THEN
      CALL set_rot_norm( normal,amat ) !Rotate into coord. aligned with terrain
      amat_t = TRANSPOSE( amat )
      IF( j > 1 .AND. multiThread )THEN
        CALL apply_rot_norm( ptem(jpuf),amat_t,amat ) !Rotate back
      ELSE
      CALL apply_rot_norm( puff(jpuf),amat,amat_t )
      END IF
      lrotate = .TRUE.
    END IF
    IF( multiThread )THEN
      NULLIFY( ptem(next)%aux )
      IF( j > 1 )THEN
        CALL xsplit( ptem(jpuf),ptem(next),ldense,lrotate,amat_t )
      ELSE
        CALL xsplit( puff(jpuf),ptem(next),ldense,lrotate,amat_t )
      END IF
    ELSE
    CALL xsplit( puff(jpuf),puff(next),ldense,lrotate,amat_t )
    END IF
  END IF
END DO

!------ y-direction split

mchk = mpuf
DO j = 1,mpuf
  jpuf = npList(j)
  IF( j == 1 .OR. singleThread )THEN
    ltest = puff(jpuf)%syy > chkx .AND. chk_cc(puff(jpuf),conc_min)
  ELSE
    ltest = ptem(jpuf)%syy > chkx .AND. chk_cc(ptem(jpuf),conc_min)
  END IF
  IF( ltest )THEN
    IF( multiThread )THEN
      IF( puffSplit(ipuf,2) == 0 )puffSplit(ipuf,3) = npufTemp(myTId)
      puffSplit(ipuf,2) = puffSplit(ipuf,2) + 1
      npufTemp(myTId)   = npufTemp(myTId)   + 1
      IF( npufTemp(myTId) > nDimTemp(myTid) )THEN
        CALL ExpandPuffTemp( myTId )
        IF( nError /= NO_ERROR )GOTO 9999
        ptem => puffTemp(myTId)%p
      END IF
      next         = npufTemp(myTId)
      mpuf         = mpuf + 1
      npList(mpuf) = npufTemp(myTId)
    ELSE
      CALL check_splitpuff()
      IF( nError == SZ_ERROR )GOTO 9998
      IF( nError /= NO_ERROR )GOTO 9999
      next = next_puff()
      mpuf         = mpuf + 1
      npList(mpuf) = next
    END IF
    IF( need_rot .AND. .NOT.lrotate )THEN
      CALL set_rot_norm( normal,amat ) !Rotate into coord. aligned with terrain
      amat_t = TRANSPOSE( amat )
      IF( j > 1 .AND. multiThread )THEN
        CALL apply_rot_norm( ptem(jpuf),amat_t,amat ) !Rotate back
      ELSE
      CALL apply_rot_norm( puff(jpuf),amat,amat_t )
      END IF
      lrotate = .TRUE.
    END IF
    IF( multiThread )THEN
      NULLIFY( ptem(next)%aux )
      IF( j > 1 )THEN
        CALL ysplit( ptem(jpuf),ptem(next),ldense,lrotate,amat_t )
      ELSE
        CALL ysplit( puff(jpuf),ptem(next),ldense,lrotate,amat_t )
      END IF
    ELSE
    CALL ysplit( puff(jpuf),puff(next),ldense,lrotate,amat_t )
    END IF
  END IF
END DO

1000 CONTINUE

IF( lrotate )THEN
  DO j = 1,mpuf
    jpuf = npList(j)
    IF( j > 1 .AND. multiThread )THEN
      CALL apply_rot_norm( ptem(jpuf),amat_t,amat ) !Rotate back
    ELSE
    CALL apply_rot_norm( puff(jpuf),amat_t,amat ) !Rotate back
    END IF
  END DO
END IF

9999 CONTINUE

RETURN

9998 CONTINUE

!-----  Stop further splitting for the rest of the time step

lsplit = .FALSE.

IF( lsplit_report )THEN
  WRITE(lun_log,'(A)',IOSTAT=ios)'********** WARNING **********'
  WRITE(lun_log,*,IOSTAT=ios)'Time = ',t,'( ',t/3600.,' )'
  WRITE(lun_log,*,IOSTAT=ios)'Splitting halted temporarily'
  WRITE(lun_log,*,IOSTAT=ios)'Too many puffs'
  WRITE(lun_log,*,IOSTAT=ios)'Resolution may be compromised'
  WRITE(lun_log,'(A)',IOSTAT=ios)'*****************************'
  lsplit_report = .FALSE.
END IF

nStopSplit = nStopSplit + 1

CALL init_error()
GOTO 9999

END

!==============================================================================
RECURSIVE REAL FUNCTION DzSplit( p,ifld,chkBL ) RESULT( chkz )

USE scipuff_fi

IMPLICIT NONE

TYPE ( puff_str ), INTENT( IN ) :: p
INTEGER,           INTENT( IN ) :: ifld
LOGICAL,           INTENT( IN ) :: chkBL

REAL, PARAMETER :: FACV = 1.0

REAL hc, hp, hx, hy, zi

!------ Basic vertical split length

chkz = FACV*delz2

IF( lter )THEN
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,ifld )
ELSE
  hp = 0.0
END IF

!----- Improve vertical resolution in canopy

CALL get_canopyIn( SNGL(p%xbar),SNGL(p%ybar),hc,ifld )

IF( p%zbar-hp < 2.*hc .AND. p%sv < hc .AND. dzg > 0.5*hc )THEN

  chkz = chkz * (0.5*hc/dzg)**2

ELSE IF( p%zbar < p%zi .AND. chkBL )THEN

!----- Otherwise improve vertical resolution in "shallow" BL

  zi = p%zi - hp

  IF( p%sv < zi .AND. dzg > 0.25*zi )chkz = chkz * (0.25*zi/dzg)**2

END IF

RETURN
END

!==============================================================================

REAL FUNCTION DxSplit( p,ifld,ldense ) RESULT( chkx )

USE scipuff_fi
USE met_fi

IMPLICIT NONE

TYPE ( puff_str ), INTENT( IN ) :: p
INTEGER,           INTENT( IN ) :: ifld
LOGICAL,           INTENT( IN ) :: ldense

REAL, PARAMETER :: SHR_DIFF = 0.5
REAL, PARAMETER :: SHR_CURV = 0.2**2
REAL, PARAMETER :: TER_CURV = 0.1**2

REAL    facs, hp, hx, hy, shr, dif, L2_shr, L2_dif
REAL    hx1, hx2, hy1, hy2, hz1, hz2, d2h, kappa2, del2
REAL    hxmx, hxmn, hymx, hymn
REAL    shh, shu, xmap, ymap, sx, sy, delx, dely
LOGICAL DoSplit, CheckTer

LOGICAL, EXTERNAL :: IsNearGround

!------ Set horizontal factor

facs = 1.0

!------ Check if splitting can be avoided based on diffusion & curvature

IF( mgrd >= 0 )THEN

  IF( MAX(p%sxx,p%syy) < facs*MetGrid(ifld)%delx2 )THEN
    chkx = -1.
    RETURN
  END IF

  DoSplit  = .TRUE.
  CheckTer = .FALSE.

  shh = 0.5*(p%sxx + p%syy)
  shr = 2.*dudx**2+2.*dvdy**2+(dudy+dvdx)**2
  IF( shh*su2 < SHR_CURV*shr )DoSplit = .FALSE.

  IF( DoSplit )THEN

    shu = SQRT(su2 * shh**3)
    dif = ( p%xuc + p%yvc + 2.*(p%yvsc+p%yvbc) ) / p%c
    IF( shu < SHR_DIFF*dif )DoSplit = .FALSE.

    IF( DoSplit )THEN
      L2_shr = SHR_CURV*shr/(su2+SMALL)
      L2_dif = (SHR_DIFF*dif/SQRT(su2+SMALL))**TwoThirds
      chkx   = facs*MAX(L2_shr,L2_dif,MetGrid(ifld)%delx2)
      CheckTer = chkx > facs*MetGrid(ifld)%delx2
    END IF

  END IF

 CheckTer = CheckTer .OR. .NOT.DoSplit

!------ Check for splitting based on terrain curvature

  IF( CheckTer )THEN
    IF( lter )THEN
      IF( IsNearGround(p) )THEN

        CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,ifld )
        CALL mapfac( SNGL(p%xbar),SNGL(p%ybar),xmap,ymap )

        sx   = SQRT(p%sxx)          !Check along x-split direction
        delx = 2.*sx * xmap
        dely = 2.*p%sxy/sx * ymap
        CALL get_topogIn( SNGL(p%xbar)+delx,SNGL(p%ybar)+dely,hz1,hx1,hy1,ifld )
        CALL get_topogIn( SNGL(p%xbar)-delx,SNGL(p%ybar)-dely,hz2,hx2,hy2,ifld )
        IF( ldense )THEN
          hxmx = MAX(hx1,hx2,hx)
          hxmn = MIN(hx1,hx2,hx)
          hymx = MAX(hy1,hy2,hy)
          hymn = MIN(hy1,hy2,hy)
          IF( MAX(hxmx-hxmn,hymx-hymn)*SQRT(shh) > 0.5*p%sv )DoSplit = .TRUE.
       END IF
        IF( .NOT.DoSplit )THEN
          del2 = (delx/xmap)**2 + (dely/ymap)**2
          d2h  = (hz2+hz1-2.*hp)/del2
          kappa2 = (d2h*d2h)/(1.0+0.25*(hz2-hz1)**2/del2)**3
          IF( kappa2*shh > TER_CURV )DoSplit = .TRUE.
        END IF

        IF( .NOT.DoSplit )THEN
          sy   = SQRT(p%syy)        !Check along y-split direction
          dely = 2.*sy * ymap
          delx = 2.*p%sxy/sy * xmap
          CALL get_topogIn( SNGL(p%xbar)+delx,SNGL(p%ybar)+dely,hz1,hx1,hy1,ifld )
          CALL get_topogIn( SNGL(p%xbar)-delx,SNGL(p%ybar)-dely,hz2,hx2,hy2,ifld )
          IF( ldense )THEN
            hxmx = MAX(hx1,hx2,hx)
            hxmn = MIN(hx1,hx2,hx)
            hymx = MAX(hy1,hy2,hy)
            hymn = MIN(hy1,hy2,hy)
            IF( MAX(hxmx-hxmn,hymx-hymn)*SQRT(shh) > 0.5*p%sv )DoSplit = .TRUE.
          END IF
          IF( .NOT.DoSplit )THEN
            del2 = (delx/xmap)**2 + (dely/ymap)**2
            d2h  = (hz2+hz1-2.*hp)/del2
            kappa2 = (d2h*d2h)/(1.0+0.25*(hz2-hz1)**2/del2)**3
            IF( kappa2*shh > TER_CURV )DoSplit = .TRUE.
          END IF

        END IF

        IF( DoSplit )chkx = facs*MetGrid(ifld)%delx2

      END IF
    END IF
  END IF

  IF( .NOT.DoSplit )chkx = -1.

ELSE

  chkx = facs*MetGrid(ifld)%delx2

END IF

RETURN
END

!==============================================================================

LOGICAL FUNCTION chk_cc( p,conc_min )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p
REAL,             INTENT( IN ) :: conc_min

REAL, PARAMETER :: CC_MAX = 100.0

chk_cc = MIN(p%cc,CC_MAX*p%ccb) > 2.*conc_min*p%c

RETURN
END

!==============================================================================

SUBROUTINE zsplit( p1,p2,ldense,lrotate,amat )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),        INTENT( INOUT ) :: p1
TYPE( puff_str ),        INTENT( OUT   ) :: p2
LOGICAL,                 INTENT( IN    ) :: ldense
LOGICAL,                 INTENT( IN    ) :: lrotate
REAL(8), DIMENSION(3,3), INTENT( IN    ) :: amat

REAL sz, delx, dely, delz
REAL aspltz, aspltzc

CALL zero_puff( p2 )

aspltz  = ASPLT_ZFAC * asplt
aspltzc = 1.0 - aspltz*aspltz

sz   = SQRT(p1%szz)
delz = aspltz*sz
delx = aspltz*p1%sxz/sz
dely = aspltz*p1%syz/sz

p2%sxx = p1%sxx - delx*delx
p2%syy = p1%syy - dely*dely
p2%sxy = p1%sxy - delx*dely

p2%szz = p1%szz*aspltzc
p2%sxz = p1%sxz*aspltzc
p2%syz = p1%syz*aspltzc

!--- Rotate displacement back into Cartesian

IF( lrotate )CALL RotateDel( delx,dely,delz,amat )

CALL csplit( p1,p2,delx,dely,delz,ldense )

RETURN
END

!==============================================================================

SUBROUTINE xsplit( p1,p2,ldense,lrotate,amat )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),        INTENT( INOUT ) :: p1
TYPE( puff_str ),        INTENT( OUT   ) :: p2
LOGICAL,                 INTENT( IN    ) :: ldense
LOGICAL,                 INTENT( IN    ) :: lrotate
REAL(8), DIMENSION(3,3), INTENT( IN    ) :: amat

REAL sx, delx, dely, delz

CALL zero_puff( p2 )

sx   = SQRT(p1%sxx)
delx = asplt*sx
dely = asplt*p1%sxy/sx
delz = asplt*p1%sxz/sx

p2%syy = p1%syy - dely*dely
p2%szz = p1%szz - delz*delz
p2%syz = p1%syz - dely*delz

p2%sxx = p1%sxx*aspltc
p2%sxy = p1%sxy*aspltc
p2%sxz = p1%sxz*aspltc

!--- Rotate displacement back into Cartesian

IF( lrotate )CALL RotateDel( delx,dely,delz,amat )

CALL csplit( p1,p2,delx,dely,delz,ldense )

RETURN
END

!==============================================================================

SUBROUTINE ysplit( p1,p2,ldense,lrotate,amat )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),        INTENT( INOUT ) :: p1
TYPE( puff_str ),        INTENT( OUT   ) :: p2
LOGICAL,                 INTENT( IN    ) :: ldense
LOGICAL,                 INTENT( IN    ) :: lrotate
REAL(8), DIMENSION(3,3), INTENT( IN    ) :: amat

REAL sy, delx, dely, delz

CALL zero_puff( p2 )

sy   = SQRT(p1%syy)
dely = asplt*sy
delz = asplt*p1%syz/sy
delx = asplt*p1%sxy/sy

p2%szz = p1%szz - delz*delz
p2%sxx = p1%sxx - delx*delx
p2%sxz = p1%sxz - delx*delz

p2%syy = p1%syy*aspltc
p2%syz = p1%syz*aspltc
p2%sxy = p1%sxy*aspltc

!--- Rotate displacement back into Cartesian

IF( lrotate )CALL RotateDel( delx,dely,delz,amat )

CALL csplit( p1,p2,delx,dely,delz,ldense )

RETURN
END

!===============================================================================

SUBROUTINE csplit( p1,p2,delx,dely,delz,ldense )

USE scipuff_fi
USE met_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p1, p2
REAL,             INTENT( INOUT ) :: delx, dely, delz
LOGICAL,          INTENT( IN    ) :: ldense

REAL    h0, hx0, hy0, xmap, ymap, dxx, dyy
REAL(8) xsav, ysav, x0, y0
REAL    zsav
REAL    z0
REAL    h, hx, hy
REAL    zrefl, rat, zreflx
INTEGER ifld
LOGICAL lcap, lzinv, lrefl, lblSplit

LOGICAL, EXTERNAL :: SWIMcappedBL
INTEGER, EXTERNAL :: getPuffifld

CALL init_csplit( p1,p2 )

p2%ityp = p1%ityp
p2%inxt = 0
p2%iprv = 0
p2%idtl = p1%idtl
p2%idtn = 0
p2%ipgd = p1%ipgd

x0 = p1%xbar
y0 = p1%ybar
z0 = p1%zbar

CALL mapfac( SNGL(x0),SNGL(y0),xmap,ymap )

delx = delx*xmap
dely = dely*ymap

ifld = getPuffifld( p1 )

IF( lter )THEN
  CALL get_topogIn( SNGL(x0),SNGL(y0),h0,hx0,hy0,ifld )
ELSE
  h0 = 0.
END IF

lzinv = p1%zbar <= p1%zi .AND. lbl
lcap  = p1%zc > 0.
IF( lzinv )THEN
  IF( SWIMcappedBL(ifld,SNGL(x0),SNGL(y0)) )THEN
    lrefl = .TRUE.
    zrefl = p1%zi
  ELSE
    lrefl = lcap
    zrefl = p1%zc
  END IF
ELSE
  lrefl = lcap
  zrefl = p1%zc
END IF

!------ Set partial reflection factor for split outside stable BL

rat = 1.0
IF( p1%zbar < p1%zi .AND. .NOT.lrefl )THEN
  IF( p1%zbar + ABS(delz) > p1%zi )THEN
    CALL get_met( SNGL(x0),SNGL(y0),z0,0.0,0.0,0,inField=ifld )
    IF( difb*p1%c < p1%zwc )rat = difb*p1%c/p1%zwc
  END IF
END IF

p2%xbar = p1%xbar + DBLE(delx)
p1%xbar = p1%xbar - DBLE(delx)
p2%ybar = p1%ybar + DBLE(dely)
p1%ybar = p1%ybar - DBLE(dely)
p2%zbar = p1%zbar + delz
p1%zbar = p1%zbar - delz

IF( global_lon )THEN
  IF( p1%ybar > 90.D0 .OR. p1%ybar < -90.D0 )THEN
    p1%xbar = p1%xbar + 180.D0
    IF( p1%ybar > 90.D0 )THEN
      p1%ybar = 180.D0 - p1%ybar
    ELSE
      p1%ybar = -180.D0 - p1%ybar
    END IF
  END IF
  IF( p2%ybar > 90.D0 .OR. p2%ybar < -90.D0 )THEN
    p2%xbar = p2%xbar + 180.D0
    IF( p2%ybar > 90.D0 )THEN
      p2%ybar = 180.D0 - p2%ybar
    ELSE
      p2%ybar = -180.D0 - p2%ybar
    END IF
  END IF
  CALL SetGlobalLonD( p1%xbar )
  CALL SetGlobalLonD( p2%xbar )
END IF

!------ check for splitting below the surface

IF( lter )THEN
  lblSplit = (lzinv.OR.lcap) .AND. MAX(p1%sxx,p1%syy)>(MAX(p1%zi,p1%zc)-h0)**2
  CALL split_ter_chk( x0,y0,z0,h0,hx0,hy0,p1,ldense )
  CALL split_ter_chk( x0,y0,z0,h0,hx0,hy0,p2,ldense )
ELSE ! --- Modify reflection to be along current puff direction
  lblSplit = .FALSE.
  dxx = xmap*delz*p1%sxz/p1%sxx
  dyy = ymap*delz*p1%syz/p1%syy
  CALL split_chk( dxx, dyy,delz,p1 )
  CALL split_chk(-dxx,-dyy,delz,p2 )
END IF

!------ check for splitting across the cap

IF( lrefl )THEN
  zreflx = zrefl
  IF( lter )THEN
    CALL get_topogIn( SNGL(p1%xbar),SNGL(p1%ybar),h,hx,hy,ifld )
    zreflx = zrefl + h - h0
  ELSE
    h = 0.
  END IF
  p1%zc = zreflx
  IF( p1%zbar > zreflx )THEN
    p1%zbar = 2.*zreflx - p1%zbar
    IF( p1%zbar < h )p1%zbar = 0.5*(zreflx+h)
  END IF
  IF( lter )THEN
    CALL get_topogIn( SNGL(p2%xbar),SNGL(p2%ybar),h,hx,hy,ifld )
    zreflx = zrefl + h - h0
  END IF
  p2%zc = zreflx
  IF( p2%zbar > zreflx )THEN
    p2%zbar = 2.*zreflx - p2%zbar
    IF( p2%zbar < h )p2%zbar = 0.5*(zreflx+h)
  END IF
END IF

IF( lblSplit )THEN
  CALL get_topogIn( SNGL(p1%xbar),SNGL(p1%ybar),h,hx,hy,ifld )
  CALL dense_rot_norm( hx0,hy0,hx,hy,p1 )
  CALL get_topogIn( SNGL(p2%xbar),SNGL(p2%ybar),h,hx,hy,ifld )
  CALL dense_rot_norm( hx0,hy0,hx,hy,p2 )
END IF

!------ Auxiliary arrays

CALL scale_paux( p1,0.5 )
CALL copyPuffAux( p1,p2 )
IF( nError /= NO_ERROR )GOTO 9999

!------ Set puff-1 masses

CALL sum_csplit( p1,p2 )

!------ Rescale for partial reflection

IF( rat /= 1.0 )THEN
  IF( p2%zbar > p2%zi )THEN
    CALL scale_puff( p2,rat )
    CALL scale_puff( p1,2.0-rat )
  ELSE IF( p1%zbar > p1%zi )THEN
    CALL scale_puff( p1,rat )
    CALL scale_puff( p2,2.0-rat )
  END IF
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE init_csplit( p1,p2 )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p1, p2

TYPE( puff_str_xc ) p1_xc,p2_xc

p1_xc = TRANSFER(p1,p1_xc)
p2_xc = TRANSFER(p2,p2_xc)

p1_xc%sig  = p2_xc%sig

p2_xc%psum = 0.5*p1_xc%psum

p2_xc%prat = p1_xc%prat

p1 = TRANSFER(p1_xc,p1)
p2 = TRANSFER(p2_xc,p2)

RETURN
END

!===============================================================================

SUBROUTINE sum_csplit( p1,p2 )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p1
TYPE( puff_str ), INTENT( IN    ) :: p2

TYPE( puff_str_xc ) p1_xc,p2_xc

p1_xc = TRANSFER(p1,p1_xc)
p2_xc = TRANSFER(p2,p2_xc)

p1_xc%psum = p2_xc%psum

p1 = TRANSFER(p1_xc,p1)

RETURN
END

!===============================================================================

SUBROUTINE split_ter_chk( x0,y0,z0,h0,hx0,hy0,p,ldense )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL(8),          INTENT( IN    ) :: x0, y0
REAL,             INTENT( IN    ) :: z0
REAL,             INTENT( IN    ) :: h0, hx0, hy0
LOGICAL,          INTENT( IN    ) :: ldense

REAL hp, hx, hy

INTEGER, EXTERNAL :: getPuffifld
INTEGER, EXTERNAL :: getPuffiskew

CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,getPuffifld(p) )

IF( p%zc > 0.0 )p%zc = p%zc + hp - h0
p%zi = p%zi + hp - h0

IF( p%zbar < hp .AND. getPuffiskew(p) == SKEW_DOWN )CALL setPuffiskew( p,SKEW_UP )

IF( ldense )THEN
  CALL dense_rot_norm( hx0,hy0,hx,hy,p )
  p%zbar = hp + z0 - h0
ELSE
  IF( p%zbar < hp )THEN
    CALL split_ter_refl( x0,y0,z0,h0,hx0,hy0,hp,hx,hy,p )
  END IF
END IF

RETURN
END

!===============================================================================

SUBROUTINE split_ter_refl( x0,y0,z0,h0,hx0,hy0,hp,hx,hy,p )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL(8),          INTENT( IN    ) :: x0, y0
REAL,             INTENT( IN    ) :: z0
REAL,             INTENT( IN    ) :: h0, hx0, hy0
REAL,             INTENT( INOUT ) :: hp, hx, hy

REAL del0, del1, rat, rm1, hp0

INTEGER, EXTERNAL :: getPuffifld

del0 = z0 - h0
del1 = hp - p%zbar
rat  = (del0-del1) / (del0+del1)
rm1  = 1.0 - rat

p%xbar = DBLE(rm1)*x0 + DBLE(rat)*p%xbar
p%ybar = DBLE(rm1)*y0 + DBLE(rat)*p%ybar
p%zbar = rm1*z0 + rat*p%zbar

hp0 = hp

CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),hp,hx,hy,getPuffifld(p) )

p%zi = p%zi + hp - hp0
IF( p%zc > 0.0 )p%zc = p%zc + hp - hp0

IF( p%zbar < hp )THEN
  IF( p%zc > 0.0 )p%zc = p%zc + h0 - hp
  p%zi   = p%zi + h0 - hp
  p%xbar = x0
  p%ybar = y0
  p%zbar = z0
  hp     = h0
  hx     = hx0
  hy     = hy0
ELSE IF( p%zc > 0. .AND. p%zbar > p%zc )THEN
  p%zc = p%zbar + 1.0
END IF

RETURN
END

!===============================================================================

SUBROUTINE split_chk( delx,dely,delz,p )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
REAL,             INTENT( IN    ) :: delx, dely, delz

REAL fac

INTEGER, EXTERNAL :: getPuffiskew

IF( p%zbar < 0.0 )THEN
  fac = 2.*ABS(p%zbar/delz)
  p%zbar = -p%zbar
  p%xbar =  p%xbar + DBLE(fac*delx)
  p%ybar =  p%ybar + DBLE(fac*dely)
  IF( getPuffiskew(p) == SKEW_DOWN )CALL setPuffiskew( p,SKEW_UP )
END IF

RETURN
END

!==============================================================================

LOGICAL FUNCTION IsNearGround( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

REAL sx, sy, h, hx, hy, ztest

INTEGER, EXTERNAL :: getPuffifld

IF( lter )THEN
  sx = SQRT(p%sxx)
  sy = SQRT(p%syy)
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h,hx,hy,getPuffifld(p) )
  ztest = MAX(p%zbar-h-fac_rfl*(ABS(hx)*sx + ABS(hy)*sy),0.)
ELSE
  ztest = p%zbar
END IF

IsNearGround = ztest*ztest < fac_rfl*fac_rfl*p%szz

RETURN
END
