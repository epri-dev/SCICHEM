RECURSIVE SUBROUTINE test_refl( p,lrfl,zp,h,hx,hy )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: p
LOGICAL,          INTENT( OUT ) :: lrfl
REAL,             INTENT( OUT ) :: zp, h, hx, hy

REAL sx, sy, ztest

INTEGER, EXTERNAL :: getPuffifld

IF( lter )THEN
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h,hx,hy,getPuffifld(p) )
  sx    = SQRT(p%sxx)
  sy    = SQRT(p%syy)
  zp    = p%zbar - h
  ztest = zp - fac_rfl*(ABS(hx)*sx + ABS(hy)*sy)
ELSE
  h     = 0.
  zp    = p%zbar
  ztest = zp
END IF

lrfl = ztest < fac_rfl*SQRT(p%szz)

RETURN
END

!============================================================================

SUBROUTINE limint( x,d,i1,i2,nx )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: nx
INTEGER, INTENT( OUT ) :: i1, i2
REAL,    INTENT( IN  ) :: x, d

IF( x-d < 0.0 )THEN
  i1 = 0
ELSE
  i1 = -1
END IF

IF( x+d > FLOAT(nx) )THEN
  i2 = 0
ELSE
  i2 = 1
END IF

RETURN
END

!=============================================================================

SUBROUTINE matrot( asig,a,bsig )

IMPLICIT NONE

REAL(8), DIMENSION(7),   INTENT( IN  ) :: asig
REAL(8), DIMENSION(3,3), INTENT( IN  ) :: a
REAL(8), DIMENSION(7),   INTENT( OUT ) :: bsig

REAL(8), DIMENSION(3,3) :: at, b, c

b(1,1) = asig(1)
b(1,2) = asig(2)
b(1,3) = asig(3)
b(2,2) = asig(4)
b(2,3) = asig(5)
b(3,3) = asig(6)
b(2,1) = b(1,2)
b(3,1) = b(1,3)
b(3,2) = b(2,3)

c  = MATMUL( a,b )
at = TRANSPOSE( a )
b  = MATMUL( c,at )

bsig(1) = b(1,1)
bsig(2) = b(1,2)
bsig(3) = b(1,3)
bsig(4) = b(2,2)
bsig(5) = b(2,3)
bsig(6) = b(3,3)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_asig( p )

USE scipuff_fi
USE inter_fi
USE refl_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

REAL dum1, dum2

REAL(8), DIMENSION(7) :: p_asig

CALL get_asig( p,p_asig )

IF( lter )THEN
  CALL reflect( zp,zp,p_asig,hx,hy,.FALSE.,xr,dum1,dum2 )
  IF( nError /= NO_ERROR )GOTO 9999
  asig(1) = b_rfl(1,1)
  asig(2) = b_rfl(1,2)
  asig(3) = b_rfl(1,3)
  asig(4) = b_rfl(2,2)
  asig(5) = b_rfl(2,3)
  asig(6) = b_rfl(3,3)
  asig(7) = DBLE(p%det)
  zp1     = zp*SNGL(a_rfl(3,3))
  r_ipuf  = 0.5*zp1/(p%det*SNGL(deth))
ELSE
  a_rfl(3,3) = 1.D0
  asig   = p_asig
  deth   = asig(1)*asig(4) - asig(2)*asig(2)
  r_ipuf = 0.5*zp/(p%det*SNGL(deth))
  zp1    = zp
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_bsig( p )

USE scipuff_fi
USE inter_fi
USE refl_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

REAL(8), DIMENSION(7)   :: p_asig
REAL(8)                 :: delxd,delyd,delzd

CALL get_asig( p,p_asig )

IF( lter )THEN
  delxd = DBLE(delx)
  delyd = DBLE(dely)
  delzd = DBLE(delz)
  CALL matrot( p_asig,a_rfl,bsig )
  bsig(7) = DBLE(p%det)
  betx    = a_rfl(1,1)*delxd + a_rfl(1,2)*delyd + a_rfl(1,3)*delzd
  bety    = a_rfl(2,1)*delxd + a_rfl(2,2)*delyd + a_rfl(2,3)*delzd
  betz    = a_rfl(3,1)*delxd + a_rfl(3,2)*delyd + a_rfl(3,3)*delzd
  zp2     = zp*SNGL(a_rfl(3,3))
  deth    = bsig(1)*bsig(4) - bsig(2)*bsig(2)
  r_jpuf  = 0.5*zp2/(p%det*SNGL(deth))
ELSE
  bsig   = p_asig
  betx   = DBLE(delx)
  bety   = DBLE(dely)
  betz   = DBLE(delz)
  zp2    = zp
  deth   = bsig(1)*bsig(4) - bsig(2)*bsig(2)
  r_jpuf = 0.5*zp2/(p%det*SNGL(deth))
END IF

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_facr( p1,p2 )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p1, p2

REAL rat, b33, s33

REAL, EXTERNAL :: overlp, efun

fac = overlp( asig,bsig,betx,bety,betz )

IF( fac < -1.0E-6 )THEN
  CALL inter_error( p1,p2,'inter_facr' )
  GOTO 9999
END IF

IF( fac < 10. )THEN

  b33 = det/d33
  s33 = -SQRT(b33)

  IF( iovlp == 1 )THEN
    z1 = zp1 + z1
  ELSE
    z1 = zp2 + z1
  END IF

  g_ipuf = -r_ipuf*(z1 - 0.25*r_ipuf/b33)
  g_jpuf = -r_jpuf*(z1 - 0.25*r_jpuf/b33)
  z_ipuf = z1 - 0.5*r_ipuf/b33
  z_jpuf = z1 - 0.5*r_jpuf/b33

  facn = efun( g_ipuf,s33*z_ipuf ) + efun( g_jpuf,s33*z_jpuf )
  den  = PI3*SQRT(8.*det*p1%det*p2%det)
  fac  = 0.5*EXP(-fac)/den
  IF( lter )THEN
    rat  = hx*hx + hy*hy
    rat  = (rat-1.0)/(rat+1.0)
    facw = fac*(2.+rat*facn)
  ELSE
    facw = fac*(2.-facn)
  END IF
  fac = fac*(2.+facn)

ELSE

  fac  = 0.
  facw = 0.

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_facr_self( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

REAL rat, b33, s33

REAL, EXTERNAL :: efun

d33 = SNGL(4.D0*(asig(1)*asig(4) - asig(2)*asig(2)))

b33 = 1./p%det/d33
s33 = -SQRT(b33)

g_ipuf = -r_ipuf*(zp1 - 0.25*r_ipuf/b33)
z_ipuf = zp1 - 0.5*r_ipuf/b33

facn = efun( g_ipuf,s33*z_ipuf )
den  = PI3*SQRT(8.*p%det)
fac  = 1./den
IF( lter )THEN
  rat  = hx*hx + hy*hy
  rat  = (rat-1.0)/(rat+1.0)
  facw = fac*(1.+rat*facn)
ELSE
  facw = fac*(1.-facn)
END IF
fac = fac*(1.+facn)

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_faci( p1,p2 )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p1, p2

REAL    del, faci, r_inv, g_inv, z_inv, b33, s33
LOGICAL lset

REAL, EXTERNAL :: efun

faci = 1.
lset = .FALSE.

IF( p1%zc > 0. )THEN

  del = MAX(p1%zc - p1%zbar,0.0)

  IF( del < fac_rfl*SQRT(p1%szz) )THEN

    lset = .TRUE.

    b33 = det/d33
    s33 = -SQRT(b33)

    IF( p1%zc >= z1 )THEN
      r_inv = 2.*del/p1%szz
      g_inv = -r_inv*(p1%zc - z1 - 0.25*r_inv/b33)
      z_inv = p1%zc - z1 - 0.5*r_inv/b33

      faci = faci + 0.5*efun( g_inv,s33*z_inv )
    END IF

  END IF

END IF

IF( p2%zc > 0. )THEN

  del = MAX(p2%zc - p2%zbar,0.0)

  IF( del < fac_rfl*SQRT(p2%szz) )THEN

    IF( .NOT.lset )THEN

      b33 = det/d33
      s33 = -SQRT(b33)

    END IF

    IF( p2%zc >= z1 )THEN
      r_inv = 2.*del/p2%szz
      g_inv = -r_inv*(p2%zc - z1 - 0.25*r_inv/b33)
      z_inv = p2%zc - z1 - 0.5*r_inv/b33

      faci = faci + 0.5*efun( g_inv,s33*z_inv )
    END IF

  END IF

END IF

fac = fac*faci

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_faci_self( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

REAL del, faci, b33, s33, r_inv, g_inv, z_inv

REAL, EXTERNAL :: efun

faci = 1.

IF( p%zc > 0. )THEN

  del = MAX(p%zc - p%zbar,0.0)

  IF( del < fac_rfl*SQRT(p%szz) )THEN

    d33 = 4.*(p%axx*p%ayy - p%axy*p%axy)

    b33 = 1./p%det/d33
    s33 = -SQRT(b33)

    r_inv = 2.*del/p%szz
    g_inv = -r_inv*(del - 0.25*r_inv/b33)
    z_inv = del - 0.5*r_inv/b33

    faci = faci + efun( g_inv,s33*z_inv )

  END IF

END IF

fac = fac*faci

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_fac( p1,p2 )
!DIR$ OPTIMIZE:1

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p1, p2

REAL(8), DIMENSION(7) :: asig1, asig2

REAL, EXTERNAL :: overlp

betx = DBLE(delx)
bety = DBLE(dely)
betz = DBLE(p2%zbar) - DBLE(p1%zbar)

CALL get_asig( p1,asig1 )
CALL get_asig( p2,asig2 )

fac = overlp( asig1,asig2,betx,bety,betz )

IF( fac < -1.0E-6 )THEN
  CALL inter_error( p1,p2,'inter_fac' )
  GOTO 9999
END IF

IF( fac < 10. )THEN
  den = PI3*SQRT(8.*det*p1%det*p2%det)
  fac = EXP(-fac)/den
ELSE
  fac = 0.
END IF

facw = fac

!-- Reset z1 to be relative to hmin for inter_faci

IF( iovlp == 1 )THEN
  z1 = p1%zbar + z1
ELSE
  z1 = p2%zbar + z1
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE inter_fac_self( p )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

den = PI3*SQRT(8.*p%det)
fac = 1./den

facw = fac

RETURN
END

!=======================================================================

RECURSIVE SUBROUTINE inter_self( p,ccs )

USE scipuff_fi
USE inter_fi

!---- calculates the self overlap integral

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: p
REAL,             INTENT( OUT ) :: ccs

REAL b33, s33

REAL, EXTERNAL :: efun

!------ check ground proximity for ipuf

CALL test_refl( p,lrfl_ipuf,zp,h,hx,hy )

fac = 1./(PI3*SQRT(8.*p%det))

IF( lrfl_ipuf )THEN

  CALL inter_asig( p )
  IF( nError /= NO_ERROR )GOTO 9999

  b33 = p%det*SNGL((4.D0*(asig(1)*asig(4) - asig(2)**2)))
  s33 = -SQRT(1./b33)

  z_ipuf = zp1 - 0.50*r_ipuf*b33
  z_jpuf = zp1 - 0.25*r_ipuf*b33

  facn = efun( -r_ipuf*z_jpuf,s33*z_ipuf )
  fac  = fac*(1.0+facn)

END IF

!------ interaction with self

ccs = fac*(p%c**2)

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION efun( arg1,arg )

IMPLICIT NONE

REAL, INTENT( IN ) :: arg1,arg

REAL, PARAMETER :: p  =  0.3275911
REAL, PARAMETER :: a1 =  0.254829592
REAL, PARAMETER :: a2 = -0.284496736
REAL, PARAMETER :: a3 =  1.421413741
REAL, PARAMETER :: a4 = -1.453152027
REAL, PARAMETER :: a5 =  1.061405429

REAL t

t = 1./(1.+p*ABS(arg))

efun = t*(a1+t*(a2+t*(a3+t*(a4+t*a5))))*EXP(arg1-arg*arg)

IF( arg < 0.0 )THEN
  efun = 2.*EXP(arg1) - efun
END IF

RETURN
END

!===============================================================================

SUBROUTINE add_remove_list( ipuf )

USE scipuff_fi
USE inter_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf

nrlist = nrlist + 1

IF( irfrst == 0 )THEN
  irfrst = ipuf
  irlast = ipuf
ELSE
  puff(irlast)%cfo = FLOAT(ipuf)
  irlast           = ipuf
END IF

puff(ipuf)%cfo = 0.0
puff(ipuf)%sr  = FLOAT(iprv)

RETURN
END

!===============================================================================

SUBROUTINE inter_error( p1,p2,routine )

USE scipuff_fi
USE inter_fi
USE files_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p1, p2
CHARACTER(*),     INTENT( IN ) :: routine

INTEGER ios

nError   = UK_ERROR
eRoutine = routine
eMessage = 'Interaction problem'

!$OMP CRITICAL (LOG)
WRITE(lun_log,*,IOSTAT=ios)'Interaction problem in '//routine
WRITE(lun_log,*,IOSTAT=ios)'fac,betx,bety,betz:'
WRITE(lun_log,*,IOSTAT=ios)fac,betx,bety,betz
WRITE(lun_log,*,IOSTAT=ios)'p1:'
CALL dump_puff( 0,p1 )
WRITE(lun_log,*,IOSTAT=ios)'p2:'
CALL dump_puff( 0,p2 )
!$OMP END CRITICAL (LOG)

RETURN
END

!===============================================================================

RECURSIVE REAL FUNCTION overlp( asigs,bsigs,betxs,betys,betzs )

USE inter_fi

IMPLICIT NONE

REAL(8), DIMENSION(7), INTENT( IN ) :: asigs, bsigs
REAL(8),               INTENT( IN ) :: betxs, betys, betzs

DOUBLE PRECISION det_8, x1_8, y1_8, z1_8, rdet
DOUBLE PRECISION d11, d12, d13, d22, d23, d33_8
DOUBLE PRECISION a0, a1, a2, a3
DOUBLE PRECISION a11, a12, a13, a22, a23, a33

a11 = asigs(1) + bsigs(1)
a12 = asigs(2) + bsigs(2)
a13 = asigs(3) + bsigs(3)
a22 = asigs(4) + bsigs(4)
a23 = asigs(5) + bsigs(5)
a33 = asigs(6) + bsigs(6)

IF( asigs(7) > bsigs(7) )THEN
  a1 = asigs(1)*betxs + asigs(2)*betys + asigs(3)*betzs
  a2 = asigs(2)*betxs + asigs(4)*betys + asigs(5)*betzs
  a3 = asigs(3)*betxs + asigs(5)*betys + asigs(6)*betzs
  a0 = a1*betxs + a2*betys + a3*betzs
  iovlp = 2
ELSE
  a1 = -(bsigs(1)*betxs + bsigs(2)*betys + bsigs(3)*betzs)
  a2 = -(bsigs(2)*betxs + bsigs(4)*betys + bsigs(5)*betzs)
  a3 = -(bsigs(3)*betxs + bsigs(5)*betys + bsigs(6)*betzs)
  a0 = -(a1*betxs + a2*betys + a3*betzs)
  iovlp = 1
END IF

d11   = a22*a33 - a23*a23
d12   = a12*a33 - a13*a23
d13   = a12*a23 - a13*a22
d22   = a11*a33 - a13*a13
d23   = a11*a23 - a13*a12
d33_8 = a11*a22 - a12*a12
det_8 = a11*d11 - a12*d12 + a13*d13

rdet = 1.0D0/det_8

x1_8 = -(d11*a1 - d12*a2 + d13*a3)*rdet
y1_8 =  (d12*a1 - d22*a2 + d23*a3)*rdet
z1_8 = -(d13*a1 - d23*a2 + d33_8*a3)*rdet

overlp = SNGL(a0 - a11*x1_8*x1_8 - 2.D0*a12*x1_8*y1_8 - 2.D0*a13*x1_8*z1_8 &
                 - a22*y1_8*y1_8 - 2.D0*a23*y1_8*z1_8 - a33*z1_8*z1_8)

det = SNGL(det_8)
d33 = SNGL(d33_8)
x1  = SNGL(x1_8)
y1  = SNGL(y1_8)
z1  = SNGL(z1_8)

RETURN
END

!===============================================================================

SUBROUTINE get_asig( p,asig )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN  ) :: p
REAL(8), DIMENSION(7), INTENT( OUT ) :: asig

asig(1) = DBLE(p%axx)
asig(2) = DBLE(p%axy)
asig(3) = DBLE(p%axz)
asig(4) = DBLE(p%ayy)
asig(5) = DBLE(p%ayz)
asig(6) = DBLE(p%azz)
asig(7) = DBLE(p%det)

RETURN
END

