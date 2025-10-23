!===============================================================================

RECURSIVE SUBROUTINE reflect( zsrf,zp,asig,hx,hy,lsrf,xr,vfac,zexp )

USE refl_fi
USE error_fi
USE files_fi

!------ Double precision version of reflect
!       rotate moment tensor into local coordinate system defined by
!       the surface normal and tangent plane; find position of reflected
!       puff in this system; rotate back into original coordinate system;
!       find surface integral factor

IMPLICIT NONE

REAL,                  INTENT( IN )  :: zsrf, zp
REAL(8), DIMENSION(7), INTENT( IN )  :: asig
REAL,                  INTENT( IN )  :: hx, hy
LOGICAL,               INTENT( IN )  :: lsrf
REAL,    DIMENSION(3), INTENT( OUT ) :: xr
REAL,                  INTENT( OUT ) :: vfac, zexp

REAL(8) denr, xs, ys, zs, arg, facs

REAL(8), DIMENSION(3)   :: x
REAL(8), DIMENSION(3)   :: dxr
REAL(8), DIMENSION(3,3) :: c

REAL(8) dvfac, dzexp

LOGICAL, EXTERNAL :: check_slope

b_rfl(1,1) = asig(1)
b_rfl(1,2) = asig(2)
b_rfl(1,3) = asig(3)
b_rfl(2,2) = asig(4)
b_rfl(2,3) = asig(5)
b_rfl(3,3) = asig(6)
b_rfl(2,1) = b_rfl(1,2)
b_rfl(3,1) = b_rfl(1,3)
b_rfl(3,2) = b_rfl(2,3)

!------ rotate into local coordinates

IF( check_slope(hx,hy) )THEN

  CALL set_matrot( hx,hy,a_rfl )

  c =  MATMUL( a_rfl,b_rfl )
  at_rfl = TRANSPOSE( a_rfl )
  b_rfl =  MATMUL( c,at_rfl )

ELSE

  a_rfl = RESHAPE( (/ 1.D0, 0.D0, 0.D0, &
                       0.D0, 1.D0, 0.D0, &
                       0.D0, 0.D0, 1.D0 /),(/ 3,3 /) )

  at_rfl = a_rfl

END IF

denr = at_rfl(3,3)

!------ Find reflected puff position in local coordinates

zs = -zp*denr

deth = b_rfl(1,1)*b_rfl(2,2) - b_rfl(1,2)*b_rfl(1,2)
xs   = -zs*(b_rfl(1,3)*b_rfl(2,2)-b_rfl(2,3)*b_rfl(1,2))/deth
ys   = -zs*(b_rfl(2,3)*b_rfl(1,1)-b_rfl(1,3)*b_rfl(1,2))/deth

x(1) = 2.*xs
x(2) = 2.*ys
x(3) = 2.*zs

!------ Compute surface integral factor

IF( lsrf )THEN

  arg = -(b_rfl(3,3)*zs*zs - b_rfl(1,1)*xs*xs - b_rfl(2,2)*ys*ys &
          -2.*b_rfl(1,2)*xs*ys)
  IF( arg > 0.0 )THEN
!$OMP CRITICAL (LOG)
    nPuffReflect = nPuffReflect + 1
    WRITE(lun_log,*)'Puff reflection error arg > 0.0'
    WRITE(lun_log,*)'a_rfl=',a_rfl
    WRITE(lun_log,*)'b_rfl=',b_rfl
    WRITE(lun_log,*)'x=',x
    WRITE(lun_log,*)'deth=',deth
    WRITE(lun_log,*)'arg=',arg
!$OMP END CRITICAL (LOG)
    arg = 0.0
  END IF
  IF( arg > -30.0 )THEN
    dzexp = DEXP(arg)
  ELSE
    dzexp = 0.0
  END IF

  dvfac = dzexp/DSQRT(deth)

!------ Add surface reflection

  IF( zsrf == zp )THEN
    facs = 1.D0
  ELSE
    zs   = -DBLE(zsrf)*denr
    facs = DEXP(0.5D0*zs*DBLE(zsrf-zp)/(asig(7)*deth*denr))
  END IF

  dvfac = dvfac * (1.D0+facs)

  zexp = SNGL(dzexp)
  vfac = SNGL(dvfac)

ELSE

  zexp = 0.0
  vfac = 0.0

END IF

!------ Rotate back into global coordinates

dxr = MATMUL( at_rfl,x )

!------ Set single precision results

xr(1) = SNGL(dxr(1))
xr(2) = SNGL(dxr(2))
xr(3) = SNGL(dxr(3))

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE zi_reflect( zbar,zcap,zrefl,zs,rat,faci )

!------ Inversion reflection factor

IMPLICIT NONE

REAL, INTENT( IN  ) :: zbar  !PUFF Position
REAL, INTENT( IN  ) :: zcap  !Capping height
REAL, INTENT( IN  ) :: zrefl !Reflection height
REAL, INTENT( IN  ) :: zs    !SLICE Location
REAL, INTENT( IN  ) :: rat   !Gaussian factor
REAL, INTENT( OUT ) :: faci  !Reflection factor

REAL arg

!------ Inversion reflection factor

IF( zcap == 0. .OR. zbar > zcap )THEN  !PUFF above capping height
  faci = 0.
ELSE
  IF( zs > zrefl )THEN !PUFF Below - SLICE Above
    faci = -1.
  ELSE                 !PUFF Below - SLICE Below
    arg  = (zrefl-zs)*(zrefl-zbar)*rat
    faci = EXP(-arg)
  END IF
END IF

RETURN
END

!==============================================================================

SUBROUTINE grnd_reflect( zp,asig,hx,hy,xr,x,deth,zs )

!------ Ground reflections

IMPLICIT NONE

REAL,                  INTENT( IN  ) :: zp   !PUFF Height above terrain
REAL(8), DIMENSION(7), INTENT( IN  ) :: asig !PUFF sigma
REAL,                  INTENT( IN  ) :: hx   !Terrain x-gradient
REAL,                  INTENT( IN  ) :: hy   !Terrain y-gradient
REAL,    DIMENSION(3), INTENT( OUT ) :: xr   !Reflected position
REAL,    DIMENSION(3), INTENT( OUT ) :: x    !Ground normals
REAL,                  INTENT( OUT ) :: deth !Horizontal determinate
REAL,                  INTENT( OUT ) :: zs   !Z normal

REAL(8), DIMENSION(3,3) :: a, b, at, c
REAL(8), DIMENSION(3)   :: xd, xrd

REAL(8) xs,ys,zsd,dethd

LOGICAL, EXTERNAL :: check_slope

!------ Ground reflection

b(1,1) = asig(1); b(1,2) = asig(2); b(1,3) = asig(3)
b(2,1) = b(1,2);  b(2,2) = asig(4); b(2,3) = asig(5)
b(3,1) = b(1,3);  b(3,2) = b(2,3);  b(3,3) = asig(6)

!------ Rotate into local coordinates

IF( check_slope(hx,hy) )THEN

  CALL set_matrot( hx,hy,a )

  c = MATMUL( a,b )
  at = TRANSPOSE( a )
  b = MATMUL( c,at )

ELSE

  a = RESHAPE( (/ 1.D0, 0.D0, 0.D0,   &
                  0.D0, 1.D0, 0.D0,   &
                  0.D0, 0.D0, 1.D0 /),&
      (/ 3,3 /) )

  at = a

END IF

!------ Find reflected puff position in local coordinates

zsd = -DBLE(zp)*at(3,3)

dethd = b(1,1)*b(2,2) - b(1,2)*b(1,2)
xs    = -zsd*(b(1,3)*b(2,2)-b(2,3)*b(1,2))/dethd
ys    = -zsd*(b(2,3)*b(1,1)-b(1,3)*b(1,2))/dethd

xd(1) = xs
xd(2) = ys
xd(3) = zsd

!------ Rotate back into global coordinates; save surface normal vector

xrd = MATMUL( at,xd )

zs = SNGL(zsd)

deth = SNGL(dethd)

x(1) = SNGL(a(3,1))
x(2) = SNGL(a(3,2))
x(3) = SNGL(a(3,3))

xr(1) = SNGL(xrd(1))
xr(2) = SNGL(xrd(2))
xr(3) = SNGL(xrd(3))

RETURN
END

!=======================================================================

SUBROUTINE set_matrot( hx,hy,amat )

IMPLICIT NONE

REAL,                    INTENT( IN  ) :: hx, hy
REAL(8), DIMENSION(3,3), INTENT( OUT ) :: amat

REAL(8) denr, denx, dhx, dhy

dhx = DBLE(hx)
dhy = DBLE(hy)

denr = 1.D0/DSQRT(1.D0 + dhx*dhx + dhy*dhy)
denx = 1.D0/DSQRT(1.D0 + dhx*dhx)

amat = RESHAPE( (/ (denx),    -(dhx*dhy*denr*denx), -(dhx*denr), &
                    0.D0,      (denr/denx),         -(dhy*denr), &
                   (dhx*denx), (dhy*denr*denx),      (denr)  /),(/3,3/) )
RETURN
END

!=======================================================================

SUBROUTINE puff_grnd_reflect( zp,p,hx,hy,xr,xnrm,deth,znrm )

USE struct_fd

IMPLICIT NONE

REAL,               INTENT( IN  ) :: zp   !PUFF Height above terrain
TYPE( puff_str ),   INTENT( IN  ) :: p    !PUFF structure
REAL,               INTENT( IN  ) :: hx   !Terrain x-gradient
REAL,               INTENT( IN  ) :: hy   !Terrain y-gradient
REAL, DIMENSION(3), INTENT( OUT ) :: xr   !Reflected position
REAL, DIMENSION(3), INTENT( OUT ) :: xnrm !Ground normals
REAL,               INTENT( OUT ) :: deth !Horizontal determinate
REAL,               INTENT( OUT ) :: znrm !Z normal

REAL(8), DIMENSION(7) :: asig

CALL get_asig( p,asig )

CALL grnd_reflect( zp,asig,hx,hy,xr,xnrm,deth,znrm )

RETURN
END


!===============================================================================

SUBROUTINE puff_reflect( zs,zp,p,hx,hy,flag,xr,vfac,zexp )

USE struct_fd

IMPLICIT NONE

REAL,    INTENT( IN  ) :: zs, zp, hx, hy
REAL,    INTENT( OUT ) :: xr(3)
REAL,    INTENT( OUT ) :: vfac, zexp
LOGICAL, INTENT( IN  ) :: flag

TYPE( puff_str ) p

REAL(8), DIMENSION(7) :: asig

CALL get_asig( p,asig )

CALL reflect( zs,zp,asig,hx,hy,flag,xr,vfac,zexp )

RETURN
END

