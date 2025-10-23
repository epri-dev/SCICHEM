RECURSIVE SUBROUTINE settle( p,zsettle,hc,sr,zrfac,zexp )

USE scipuff_fi
USE refl_fi

!------ compute surface integral for deposition and dosage calculations

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: p          !Puff structure
REAL,             INTENT( IN  ) :: zsettle    !Height for integral
REAL,             INTENT( IN  ) :: hc         !Canopy height
REAL,             INTENT( OUT ) :: sr         !Surface integral
REAL,             INTENT( IN  ) :: zrfac      !Reflection limit
REAL,             INTENT( OUT ) :: zexp       !Exp factor

REAL zdos, sx, sy, sz, zrefl, zp, ztest, rat, faci, cmax
REAL vfac, xs, ys, zs, arg, h, hx, hy, facs, conc_min

INTEGER ityp

REAL, DIMENSION(3) :: xr

INTEGER, EXTERNAL :: getPuffifld

ityp     = p%ityp
cmax     = p%cc/p%c
zexp     = 0.
conc_min = 0.0

IF( cmax < conc_min )THEN
  sr = 0.0; RETURN
END IF

IF( lter )THEN
  sx = SQRT(p%sxx)
  sy = SQRT(p%syy)
  CALL get_topogIn( SNGL(p%xbar),SNGL(p%ybar),h,hx,hy,getPuffifld(p) )
  IF( zsettle < 0. )THEN
    zdos = MIN(hc,p%zbar-h)
  ELSE
    zdos = zsettle
  END IF
  zp    = p%zbar - (h+zdos)
  ztest = zp - zrfac*(ABS(hx)*sx + ABS(hy)*sy)
ELSE
  IF( zsettle < 0. )THEN
    zdos = MIN(hc,p%zbar)
  ELSE
    zdos = zsettle
  END IF
  zp    = p%zbar - zdos
  ztest = zp
  h     = 0.
END IF

!------ if capped, check for dose level above cap

IF( p%zbar <= p%zc .AND. p%zc > 0.0 .AND. zdos+h > p%zc )THEN
  sr = 0.0; RETURN
END IF

!------ if in canopy, set volumetric deposition factor

sz = SQRT(p%szz)

IF( zdos < hc .AND. sz < hc )THEN
  sr = p%c/hc; RETURN
END IF

IF( ztest < zrfac*sz )THEN

  IF( lter )THEN
    zs = p%zbar - h
    CALL puff_reflect( zs,zp,p,hx,hy,.TRUE.,xr,vfac,zexp )
    IF( nError /= NO_ERROR )THEN
      CALL dump_puff(0,p)
      GOTO 9999
    END IF
    vfac = PI/(PI3*SQRT(p%det)) * vfac
  ELSE
    deth = DBLE(p%axx)*DBLE(p%ayy) - DBLE(p%axy)**2
    zs   = -zp
    xs   = -zs*(p%axz*p%ayy-p%ayz*p%axy)/SNGL(deth)
    ys   = -zs*(p%ayz*p%axx-p%axz*p%axy)/SNGL(deth)
    arg  = -(p%azz*zs*zs-p%axx*xs*xs-p%ayy*ys*ys -2.*p%axy*xs*ys)
    IF( arg > -30.0 )THEN
      zexp = EXP(arg)
      IF( zexp*cmax < 0.25*conc_min )THEN ! allow for reflections
        sr = 0.0; RETURN
      END IF
    ELSE
      zexp = 0.0
    END IF
    vfac = zexp*PI/(PI3*SQRT(p%det*SNGL(deth))) !Integral at z=zdos
    IF( zdos == 0.0 )THEN
      facs = 1.
    ELSE
      zs   = -p%zbar
      facs = EXP(0.5*zs*zdos/(p%det*SNGL(deth))) !Surface reflection factor
    END IF
    vfac = vfac * (1.+facs)
  END IF

!------ compute inversion reflection factor

  zrefl = p%zc
  IF( p%zbar > p%zc .OR. p%zc == 0.0 )THEN
    faci = 0.
  ELSE
    rat  = 0.5/(p%det*SNGL(deth))
    arg  = (zrefl-(h+zdos))*(zrefl-p%zbar)*rat
    faci = EXP(-arg)
  END IF
  vfac = vfac * (1.+faci)

!------ set centerline value

  sr = vfac*p%c

ELSE

  sr = 0.0

END IF

9999 CONTINUE

RETURN
END
