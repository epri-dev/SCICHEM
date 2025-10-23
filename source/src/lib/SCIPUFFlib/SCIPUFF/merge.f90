SUBROUTINE merge( ilev,jlev )

USE scipuff_fi

!------ merge puffs, setup ipgrd and linked lists

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ilev, jlev

INTEGER lev, ipuf, igrd, k, ip, jpuf
INTEGER k0, ip0, jpuf0, nk, kl, ku, in, jn, id, jd, i, j
REAL    xmap, ymap, fmass, rxx, ryy, xx, yy, xc, yc, dfac

TYPE( puff_liquid ) pq

LOGICAL, EXTERNAL :: IsLiquid
LOGICAL, EXTERNAL :: chkgrd
INTEGER, EXTERNAL :: PuffGridLevel

!------ set inverse moments and remove puffs from ip list

DO lev = jlev,ilev,-1

  ipuf = itfrst(lev)

  DO WHILE( ipuf > 0 )
    CALL remove_ipgrd( ipuf )
    ipuf = puff(ipuf)%idtn
  END DO

END DO

!------ loop over puffs

TimeLevel: DO lev = ilev,jlev

  ipuf = itfrst(lev)

  PuffLoop: DO WHILE( ipuf > 0 )

    CALL mapfac( SNGL(puff(ipuf)%xbar),SNGL(puff(ipuf)%ybar),xmap,ymap )

    fmass = puff(ipuf)%c
    IF( IsLiquid(typeID(puff(ipuf)%ityp)%icls) )THEN
      CALL get_liquid( puff(ipuf),pq )
      fmass = MAX(fmass,pq%ccs)
    END IF

!------ find appropriate ipgrd location

    igrd = PuffGridLevel( puff(ipuf)%sxx,puff(ipuf)%syy,xmap,ymap )
    IF( nError /= NO_ERROR )THEN
      eRoutine = 'merge'
      CALL dump_puff( ipuf,puff(ipuf) )
      GOTO 9999
    END IF

!------ Set grid locations for puff

    CALL puff_grid( puff(ipuf),xx,yy,k0 )

    rxx = puff(ipuf)%sxx*xmap*xmap/(dxg*dxg)
    ryy = puff(ipuf)%syy*ymap*ymap/(dyg*dyg)

!------- check for other puffs in grid

    ActivePuff: IF( chkgrd(xx,yy,rxx,ryy) .AND. fmass > cmin )THEN

      CALL siginv( puff(ipuf) )

!----- Set up grid search list

      IF( puff(ipuf)%szz > delz2 )THEN
        nk = MAX(INT(2.*SQRT(puff(ipuf)%szz)/dzg),1)
        ku = MIN(k0+nk,nz)
        kl = MAX(k0-nk,1)
      ELSE
        ku = k0
        kl = k0
      END IF

      dfac = 0.5**igrd

      xc = xx/dfac
      xc = xc - INT(xc)
      IF( xc > 0.5 )THEN
        IF( xx+dfac > FLOAT(nx) )THEN
          in = 0
        ELSE
          in = 1
        END IF
        id = 1
      ELSE
        IF( xx-dfac < 0.0 )THEN
          in = 0
        ELSE
          in = -1
        END IF
        id = -1
      END IF

      yc = yy/dfac
      yc = yc - INT(yc)
      IF( yc > 0.5 )THEN
        IF( yy+dfac > FLOAT(ny) )THEN
          jn = 0
        ELSE
          jn = 1
        END IF
        jd = 1
      ELSE
        IF( yy-dfac < 0.0 )THEN
          jn = 0
        ELSE
          jn = -1
        END IF
        jd = -1
      END IF

!----- Loops to search in x,y,z directions

      Xloop: DO i = 0,in,id
        xc = xx + FLOAT(i)*dfac

        Yloop: DO j = 0,jn,jd
          yc = yy + FLOAT(j)*dfac

          Zloop: DO k = kl,ku

            CALL find_grid_cell( xc,yc,k,igrd,ip )

            IF( ip > 0 )THEN
              CALL get_ipgrd( ip,2,k,jpuf )
            ELSE
              jpuf = 0
            END IF

!------- check other puffs in box

            IF( jpuf > 0 )THEN
              CALL search_and_merge( ipuf,jpuf,xmap,ymap,ilev,k,igrd,ip )
              IF( nError /= NO_ERROR )GOTO 9999
              IF( jpuf == 0 )THEN
                jpuf0 = -1
                EXIT Xloop
              END IF
            END IF

            IF( k==k0 .AND. i==0 .AND. j==0 )THEN   !Save local cell info
              jpuf0 = jpuf
              ip0   = ip
            END IF

          END DO Zloop

        END DO Yloop

      END DO Xloop

!----- End search loop, deal with ipuf puff

      IF( jpuf0 == 0 )THEN   ! First puff in box

        IF( ip0 == 0 )THEN
          CALL get_grid_cell( xx,yy,k0,igrd,ip0 )
          IF( nError /= NO_ERROR )GOTO 9999
        END IF

        CALL set_ipgrd( ip0,2,k0,ipuf )

        puff(ipuf)%inxt = 0
        puff(ipuf)%iprv = -(1000*ip0 + k0)
        CALL setPuffipgrd( puff(ipuf),igrd )

      ELSE IF( jpuf0 > 0 )THEN ! No merge - add to list

        puff(jpuf0)%inxt = ipuf
        puff(ipuf)%inxt  = 0
        puff(ipuf)%iprv  = jpuf0
        CALL setPuffipgrd( puff(ipuf),igrd )

      ELSE ! merged - mark for removal

        puff(ipuf)%idtl = I_REMOVE

      END IF

    ELSE ! failed chkgrd - remove

      puff(ipuf)%idtl = I_REMOVE

    END IF ActivePuff

    ipuf = puff(ipuf)%idtn

    CALL step_clock()

  END DO PuffLoop

END DO TimeLevel

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE search_and_merge( ipuf,jpuf,xmap,ymap,ilev,k,igrd,ip )

USE scipuff_fi

!------ search linked list, merge if deemed appropriate

IMPLICIT NONE

INTEGER, INTENT( IN    ) :: ipuf       !Puff to be merged
INTEGER, INTENT( INOUT ) :: jpuf       !Starting puff for search on input,
                                       !Zero on return if successfull merge
REAL,    INTENT( IN    ) :: xmap, ymap !Map factors
INTEGER, INTENT( IN    ) :: ilev       !Lowest time level to consider
INTEGER, INTENT( IN    ) :: k          !Vertical grid level of search cell
INTEGER, INTENT( INOUT ) :: igrd       !Grid size level of search cell
INTEGER, INTENT( IN    ) :: ip         !Search cell ID

INTEGER ityp, jtyp, jpufn, iprv, inxt, ipn, kn, irel
INTEGER iSkew
REAL    s1, s2, arg, xn, yn
REAL(8) betx, bety, betz
LOGICAL l1, l2
LOGICAL lwet

REAL,    EXTERNAL :: overlp_merge
INTEGER, EXTERNAL :: getPuffirel
INTEGER, EXTERNAL :: getPuffiskew
LOGICAL, EXTERNAL :: check_wetpart_merge, IsWetParticle

jpufn = jpuf

!------ do not merge static puffs but still go to end of list

IF( puff(ipuf)%idtl < ilev )THEN
  DO WHILE( jpufn /= 0 )
    jpuf  = jpufn
    jpufn = puff(jpuf)%inxt
  END DO
  RETURN
END IF

irel  = getPuffirel( puff(ipuf) )
iSkew = getPuffiskew( puff(ipuf) )

ityp = puff(ipuf)%ityp
lwet = IsWetParticle( typeID(ityp)%icls )

! --- Reset type for new secondary evap puffs

IF( puff(ipuf)%cc < 0.0 )ityp = -ityp

! --- Check puffs in linked list for merging with ipuf

SearchList: DO WHILE( jpufn /= 0 )

  jpuf  = jpufn

  jpufn = puff(jpuf)%inxt

  IF( puff(jpuf)%idtl < ilev )CYCLE

!------ check type

  jtyp = puff(jpuf)%ityp
  IF( puff(jpuf)%cc < 0.0 )jtyp = -jtyp

  IF( ityp == jtyp .AND. irel == getPuffirel(puff(jpuf)) &
                  .AND. iSkew == getPuffiskew(puff(jpuf)) )THEN

!------ check for wet particles

    IF( lwet )THEN
      IF( .NOT.check_wetpart_merge(puff(ipuf),puff(jpuf)) )CYCLE Searchlist
    END IF

!------ check scale

    s1 = puff(ipuf)%si
    s2 = puff(jpuf)%si
    IF( s1 <= simrge*s2 .AND. s2 <= simrge*s1 )THEN

!------ check for location relative to inversion height

      s1 = puff(ipuf)%zbar - puff(ipuf)%zi
      s2 = puff(jpuf)%zbar - puff(jpuf)%zi

      l1 = puff(ipuf)%zc <= simrge*puff(jpuf)%zc
      l2 = puff(jpuf)%zc <= simrge*puff(ipuf)%zc

      IF( s1*s2 >= 0.0 .AND. l1 .AND. l2 )THEN

!------ check overlap distance

        betx = (puff(jpuf)%xbar - puff(ipuf)%xbar)/DBLE(xmap)
        bety = (puff(jpuf)%ybar - puff(ipuf)%ybar)/DBLE(ymap)
        betz =  DBLE(puff(jpuf)%zbar) - DBLE(puff(ipuf)%zbar)
        arg = overlp_merge( puff(ipuf),puff(jpuf),betx,bety,betz )

        IF( arg <= rrmrge )THEN

!------ passed all tests; merge and return

          CALL pmerge( puff(ipuf),puff(jpuf),xmap,ymap )

          CALL puff_grid( puff(jpuf),xn,yn,kn )
          CALL get_grid_cell( xn,yn,kn,igrd,ipn )
          IF( nError /= NO_ERROR )GOTO 9999

          IF( ip /= ipn .OR. k /= kn )THEN

!------ remove jpuf from current cell list

            iprv = puff(jpuf)%iprv
            inxt = puff(jpuf)%inxt
            IF( iprv < 0 )THEN
              CALL set_ipgrd( -iprv/1000,2,k,inxt )
            ELSE
              puff(iprv)%inxt = inxt
            END IF
            IF( inxt > 0 )puff(inxt)%iprv = iprv

!------ add jpuf to new cell list

            CALL get_ipgrd( ipn,2,kn,jpufn )
            CALL set_ipgrd( ipn,2,kn,jpuf  )

            puff(jpuf)%iprv = -(1000*ipn + kn)
            puff(jpuf)%inxt = jpufn

            IF( jpufn > 0 )puff(jpufn)%iprv = jpuf

            CALL setPuffipgrd( puff(jpuf),igrd )

          END IF

          jpuf = 0; RETURN

        END IF

      END IF

    END IF ! different scale

  END IF ! different type

!------ unable to merge with jpuf; check next guy

END DO SearchList

!------ no mergable puffs found; return

9999 CONTINUE

RETURN
END

!===============================================================================

REAL FUNCTION overlp_merge( p1,p2,betxs,betys,betzs )

USE inter_fi
USE basic_fi
USE struct_fd


IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p1, p2
REAL(8),          INTENT( IN ) :: betxs, betys, betzs

REAL(8) betzz

REAL(8) det_8, x1_8, y1_8, z1_8, rdet
REAL(8) d11, d12, d13, d22, d23, d33_8
REAL(8) a0, a1, a2, a3
REAL(8) a11, a12, a13, a22, a23, a33

REAL(8), DIMENSION(7) :: asig1, asig2

betzz = betzs / DBLE(ASPLT_ZFAC)

CALL get_asig( p1,asig1 )
CALL get_asig( p2,asig2 )

a11 = asig1(1) + asig2(1)
a12 = asig1(2) + asig2(2)
a13 = asig1(3) + asig2(3)
a22 = asig1(4) + asig2(4)
a23 = asig1(5) + asig2(5)
a33 = asig1(6) + asig2(6)

IF( asig1(7) > asig2(7) )THEN
  a1 = (asig1(1)*betxs + asig1(2)*betys + asig1(3)*betzz)
  a2 = (asig1(2)*betxs + asig1(4)*betys + asig1(5)*betzz)
  a3 = (asig1(3)*betxs + asig1(5)*betys + asig1(6)*betzz)
  a0 = (a1*betxs + a2*betys + a3*betzz)
  iovlp = 2
ELSE
  a1 = -(asig2(1)*betxs + asig2(2)*betys + asig2(3)*betzz)
  a2 = -(asig2(2)*betxs + asig2(4)*betys + asig2(5)*betzz)
  a3 = -(asig2(3)*betxs + asig2(5)*betys + asig2(6)*betzz)
  a0 = -(a1*betxs + a2*betys + a3*betzz)
  iovlp = 1
END IF

d11 = a22*a33 - a23*a23
d12 = a12*a33 - a13*a23
d13 = a12*a23 - a13*a22
d22 = a11*a33 - a13*a13
d23 = a11*a23 - a13*a12

d33_8 = a11*a22 - a12*a12
det_8 = a11*d11 - a12*d12 + a13*d13

rdet = 1.0/det_8

x1_8 = -(d11*a1 - d12*a2 + d13*a3)*rdet
y1_8 =  (d12*a1 - d22*a2 + d23*a3)*rdet
z1_8 = -(d13*a1 - d23*a2 + d33_8*a3)*rdet

overlp_merge = SNGL(a0 - a11*x1_8*x1_8 - 2.*a12*x1_8*y1_8 - 2.*a13*x1_8*z1_8 &
                       - a22*y1_8*y1_8 - 2.*a23*y1_8*z1_8 - a33*z1_8*z1_8)

RETURN
END

!===============================================================================

LOGICAL FUNCTION chkgrd( x,y,rxx,ryy )

!------ Checks if puff is still affecting the domain (within sqrt(RFAC)*sigma)

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: x, y         !Centroid location
REAL, INTENT( IN ) :: rxx, ryy     !X and Y sigmas (divided by grid size)

REAL, PARAMETER :: RFAC = 9.0

LOGICAL chkx, chky
REAL    xfac, yfac

chkx = (x >= 0.) .AND. (x <= FLOAT(nx))
chky = ((y >= 0.).OR.polarcap_s) .AND. ((y <= FLOAT(ny)).OR.polarcap_n)

chkgrd = chkx .AND. chky

IF( .NOT.chkgrd )THEN

  IF( .NOT.chkx )THEN
    IF( (x < -1.) .OR. (x > FLOAT(nx+1)) )RETURN
    xfac = (MIN(ABS(x-FLOAT(nx)),ABS(x)))**2 / rxx
    chkx = xfac <= RFAC
  END IF

  IF( .NOT.chky )THEN
    IF( (y < -1.) .OR. (y > FLOAT(ny+1)) )RETURN
    yfac = (MIN(ABS(y-FLOAT(ny)),ABS(y)))**2 / ryy
    chky = yfac <= RFAC
  END IF

  chkgrd = chkx .AND. chky

END IF

RETURN
END

!===============================================================================

INTEGER FUNCTION PuffGridLevel( sxx,syy,xmap,ymap ) RESULT( grid )

!------ Find grid level for puff based on sigmas

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: sxx, syy, xmap, ymap

INTEGER, PARAMETER :: MAX_N = 100
REAL,    PARAMETER :: FAC   = 4.0

REAL    dxp, dxx, xx, yy
INTEGER n

INTEGER, EXTERNAL :: get_mxgrd

xx  = FAC*SQRT(sxx)
yy  = FAC*SQRT(syy)
dxp = MAX( xx*xmap/dxg,yy*ymap/dyg )

dxx = 1.0
n   = 0

DO WHILE( dxp <= dxx .AND. n < MAX_N )
  n   = n + 1
  dxx = 0.5*dxx
END DO

IF( n >= MAX_N )THEN
  nError   = IV_ERROR
  eMessage = 'Program Error'
  eInform  = 'Invalid puff size'
  eAction  = 'Please file a Bug report'
END IF

grid = MIN( n,get_mxgrd() )

RETURN
END

!===============================================================================

SUBROUTINE pmerge( p1,p2,xmap,ymap )

!------ Merge puffs

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p1
TYPE( puff_str ), INTENT( INOUT ) :: p2
REAL,             INTENT( IN    ) :: xmap, ymap

TYPE( puff_dynamics ) pd1, pd2

REAL(8) xbar, ybar
REAL    tot, r1, r2, zbar
REAL    ddx1, ddx2, ddy1, ddy2, ddz1, ddz2, zc
REAL    h0, h1, h2, hx0, hx1, hx2, hy0, hy1, hy2, h, hx, hy
INTEGER ifld, jfld
LOGICAL lcap
LOGICAL ldense

LOGICAL, EXTERNAL :: dense_effect
INTEGER, EXTERNAL :: getPuffifld
INTEGER, EXTERNAL :: reallocatePuffAux

tot = p1%c + p2%c
IF( tot > SMALL )THEN
  r1 = p1%c/tot
  r2 = p2%c/tot
ELSE
  r1 = 0.5
  r2 = 0.5
END IF

xbar = DBLE(r1)*p1%xbar + DBLE(r2)*p2%xbar
ybar = DBLE(r1)*p1%ybar + DBLE(r2)*p2%ybar
zbar = r1*p1%zbar + r2*p2%zbar

IF( lter )THEN

  lcap = p1%zc > 0.0

  ifld = getPuffifld( p1 )
  jfld = getPuffifld( p2 )
  CALL get_topogIn( SNGL(xbar),   SNGL(ybar)   ,h0,hx0,hy0,jfld )
  CALL get_topogIn( SNGL(p1%xbar),SNGL(p1%ybar),h1,hx1,hy1,ifld )
  CALL get_topogIn( SNGL(p2%xbar),SNGL(p2%ybar),h2,hx2,hy2,jfld )

  IF( lcap )zc = r1*(p1%zc - h1) + r2*(p2%zc - h2)

  IF( ifld /= jfld )THEN
    CALL get_topogIn( SNGL(p1%xbar),SNGL(p1%ybar),h,hx,hy,jfld )
    p1%zbar = p1%zbar + (h-h1)
    p1%zi   = p1%zi   + (h-h1)
    IF( lcap )p1%zc = p1%zc + (h-h1)
    zbar = zbar + r1*(h-h1)
  END IF

  IF( zbar < h0 )THEN
    p1%zbar = p1%zbar + (h0-zbar)
    p2%zbar = p2%zbar + (h0-zbar)
    zbar = h0
  END IF

  IF( dense_gas )THEN
    CALL get_dynamics( p1,pd1 )
    CALL get_dynamics( p2,pd2 )
    CALL get_topogIn( SNGL(p1%xbar),SNGL(p1%ybar),h1,hx1,hy1,ifld )
    ldense = dense_effect( p1%zbar-h1,p1%sv,pd1%wcp,p1%c ) &
        .OR. dense_effect( p2%zbar-h2,p2%sv,pd2%wcp,p2%c )
    IF( ldense )THEN
      CALL dense_rot_norm( hx1,hy1,hx0,hy0,p1 )
      CALL dense_rot_norm( hx2,hy2,hx0,hy0,p2 )
    END IF
  END IF


ELSE
  lcap = .FALSE.
END IF

ddx1 = SNGL(p1%xbar - xbar)/xmap
ddx2 = SNGL(p2%xbar - xbar)/xmap
ddy1 = SNGL(p1%ybar - ybar)/ymap
ddy2 = SNGL(p2%ybar - ybar)/ymap
ddz1 =  p1%zbar - zbar
ddz2 =  p2%zbar - zbar

p2%sxx = r1*(p1%sxx + ddx1*ddx1) + r2*(p2%sxx + ddx2*ddx2)
p2%syy = r1*(p1%syy + ddy1*ddy1) + r2*(p2%syy + ddy2*ddy2)
p2%szz = r1*(p1%szz + ddz1*ddz1) + r2*(p2%szz + ddz2*ddz2)
p2%sxy = r1*(p1%sxy + ddx1*ddy1) + r2*(p2%sxy + ddx2*ddy2)
p2%syz = r1*(p1%syz + ddy1*ddz1) + r2*(p2%syz + ddy2*ddz2)
p2%sxz = r1*(p1%sxz + ddx1*ddz1) + r2*(p2%sxz + ddx2*ddz2)

CALL pmerge_psum( p1,p2,r1,r2 )

p2%idtl = MAX(p1%idtl,p2%idtl)

p2%xbar = xbar
p2%ybar = ybar
p2%zbar = zbar

IF( lcap )p2%zc = MAX(p2%zc,zc+h0)

CALL siginv( p2 )

!----- Puff auxiliary space

CALL sum_paux( p1,p2 )

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE pmerge_psum( p1,p2,r1,r2 )

!------ Merge basic puff structures

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN    ) :: p1
TYPE( puff_str ), INTENT( INOUT ) :: p2
REAL,             INTENT( IN    ) :: r1, r2   !Mass fraction ratios

TYPE( puff_str_xc ) p1_xc,p2_xc

p1_xc = TRANSFER(p1,p1_xc)
p2_xc = TRANSFER(p2,p2_xc)

p2_xc%psum =    p1_xc%psum +    p2_xc%psum
p2_xc%prat = r1*p1_xc%prat + r2*p2_xc%prat

p2 = TRANSFER(p2_xc,p2)

RETURN
END

!===============================================================================

SUBROUTINE puff_grid( p,xx,yy,k )

!------ Set grid locations for puff

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN )  :: p        !Puff
REAL,             INTENT( OUT ) :: xx, yy   !Horizontal grid coordinate
INTEGER,          INTENT( OUT ) :: k        !Vertical grid level

xx = (SNGL(p%xbar)-xmin)/dxg
yy = (SNGL(p%ybar)-ymin)/dyg

IF( p%zbar <= p%zi .AND. lsplitz )THEN
  k = 1
ELSE IF( p%zbar > FLOAT(nz-1)*dzg )THEN
  k = nz
ELSE
  k = INT(p%zbar/dzg) + 1
  k = MIN(k,nz)
END IF

RETURN
END

!===============================================================================

LOGICAL FUNCTION check_wetpart_merge( p1,p2 )

!------ Set grid locations for puff

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN )  :: p1, p2        !Wet Particle Puffs

TYPE( puff_liquid )   :: pq1, pq2
TYPE( puff_material ) :: pmatIn
TYPE( part_material ) :: pmat

REAL dbin

! --- Get liquid drop data

CALL get_liquid( p1,pq1 )
CALL get_liquid( p2,pq2 )

! --- Check drop diameter difference cf particle bin size

CALL get_puff_material( p1%ityp,pmatIn )

pmat = TRANSFER(pmatIn,pmat)

dbin = (pmat%dbar - pmat%dmin)*pq1%d/pmat%dbar

check_wetpart_merge = ABS(pq1%d-pq2%d) < 2.0*dbin

RETURN
END

!===============================================================================

SUBROUTINE compress_puff_list()

USE scipuff_fi

!------ Remove dead puffs and fix IPGRD linked list

IMPLICIT NONE

INTEGER ipuf, inxt, iprv, iout
INTEGER ip, jprv, k, ios
INTEGER, EXTERNAL :: deallocatePuffAux

iout  = 0

DO ipuf = 1,npuf

  ActivePuff: IF( puff(ipuf)%idtl /= I_REMOVE )THEN

    iout = iout + 1

    MovePuff: IF( ipuf /= iout )THEN

      CALL move_puff( puff(ipuf),puff(iout) )

      iprv = puff(iout)%iprv
      inxt = puff(iout)%inxt

      IF( inxt > 0 )THEN
        puff(inxt)%iprv = iout
      END IF

      IF( iprv > 0 )THEN
        puff(iprv)%inxt = iout
      ELSE
        jprv = IABS(iprv)
        ip   = jprv/1000
        k    = jprv - 1000*ip
        CALL set_ipgrd( ip,2,k,iout )
      END IF

      IF( puff(iout)%idtl == I_STATIC )THEN
        CALL update_static_pointers( iout )
      END IF

    END IF MovePuff
  ELSE ActivePuff
    ios = deallocatePuffAux( puff(ipuf) )
  END IF ActivePuff

END DO

npuf = iout

RETURN
END
