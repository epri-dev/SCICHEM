 INTEGER FUNCTION SWIMshearGrad( fld,grid )

!------ Compute measure of velocity field second-derivatives.
!       Used by puffs to determine if splitting is necessary
!       to resolve wind field inhomogeneity

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: fld
TYPE( MetGrid   ), INTENT( IN    ) :: grid

INTEGER, EXTERNAL :: ShearGradTer, ShearGradTerStg, ShearGradFlat, ShearGradFlatStg

!------ With terrain

IF( BTEST(grid%type,GTB_TERRAIN) .OR. BTEST(grid%type,GTB_Z3D) )THEN

  IF( BTEST(grid%type,GTB_STAGGER) )THEN !Check for staggered grid
    SWIMshearGrad = ShearGradTerStg( fld,grid )
  ELSE
    SWIMshearGrad = ShearGradTer( fld,grid )
  END IF

ELSE

!------ Flat

  IF( BTEST(grid%type,GTB_STAGGER) )THEN !Check for staggered grid
    SWIMshearGrad = ShearGradFlatStg( fld,grid )
  ELSE
    SWIMshearGrad = ShearGradFlat( fld,grid )
  END IF

END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION ShearGradFlat( fld,grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: fld
TYPE( MetGrid   ), INTENT( IN    ) :: grid

INTEGER nx, ny, nz, nxy, i, j, k
INTEGER k0, j0, jm0, jp0, im, ip, is, jm, jp, ijm, ijp, imjp, ipjm

REAL x, y, xmap, ymap, dx, dy, dxr, dyr
REAL uxxp, vxxp, uyyp, vyyp, uxyp, vxyp

REAL, DIMENSION(:), POINTER :: u, v, s

ShearGradFlat = SWIMfailure

!------ Set locals

nx = grid%nX; ny = grid%nY; nz = grid%nZ; nxy = nx*ny
dx = grid%dX; dy = grid%dY

DO k = 1,nz
  k0 = (k-1)*nxy + 1
  u => fld%U(k0:)
  v => fld%V(k0:)
  s => fld%dU2(k0:)

!$OMP PARALLEL SHARED(grid,u,v,s,nx,ny,dx,dy)
!$OMP DO PRIVATE(j,j0,jm0,jp0,i,im,ip,is,jm,jp,ijp,ijm,ipjm,imjp,x,y,xmap,ymap,dxr,dyr,uxxp,vxxp,uyyp,vyyp,uxyp,vxyp)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    jp0 = MIN(j0+nx,(ny-1)*nx)
    y   = grid%Ymin + FLOAT(j-1)*dy

    DO i = 1,nx
      im   = j0 + MAX(i-1,1)
      ip   = j0 + MIN(i+1,nx)
      is   = j0 + i
      jm   = jm0 + i
      jp   = jp0 + i
      ijp  = jp0 + MIN(i+1,nx)
      ijm  = jm0 + MAX(i-1,1)
      ipjm = jm0 + MIN(i+1,nx)
      imjp = jp0 + MAX(i-1,1)
      x    = grid%Xmin + FLOAT(i-1)*dx

      CALL SWIMmapfac( grid%coord,x,y,xmap,ymap )
      dxr = xmap/dx
      dyr = ymap/dy

      uxxp = u(ip)-2.*u(is)+u(im)
      vxxp = v(ip)-2.*v(is)+v(im)
      uyyp = u(jp)-2.*u(is)+u(jm)
      vyyp = v(jp)-2.*v(is)+v(jm)
      uxyp = 0.25*(u(ijp)+u(ijm)-u(imjp)-u(ipjm))
      vxyp = 0.25*(v(ijp)+v(ijm)-v(imjp)-v(ipjm))

      s(is) = (uxxp**2 + vxxp**2)*dxr**4 + (uyyp**2 + vyyp**2)*dyr**4 + &
              2.*(uxyp**2 + vxyp**2)*(dxr*dyr)**2

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Set outer rows

  j0 = (ny-1)*nx
  DO i = 1,nx
    s(i)    = s(i+nx)
    s(j0+i) = s(j0+i-nx)
  END DO

  DO j = 1,ny
    j0 = (j-1)*nx
    s(j0+1)  = s(j0+2)
    s(j0+nx) = s(j0+nx-1)
  END DO

END DO

ShearGradFlat = SWIMresult

RETURN
END

!==============================================================================

INTEGER FUNCTION ShearGradFlatStg( fld,grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: fld
TYPE( MetGrid   ), INTENT( IN    ) :: grid

INTEGER alloc_stat, nx, ny, nz, nxy, i, j, k
INTEGER k0, j0, jm0, jp0, im, ip, is, jm, jp, imjp, ipjm, ijm

REAL x, y, xmap, ymap, dx, dy, dxr, dyr

REAL, DIMENSION(:), POINTER :: u, v, s
REAL, DIMENSION(:), POINTER :: uxx, uxy, uyy, vxx, vxy, vyy

ShearGradFlatStg = SWIMfailure

!------ Set locals

nx = grid%nX; ny = grid%nY; nz = grid%nZ; nxy = nx*ny
dx = grid%dX; dy = grid%dY

ALLOCATE( uxx(nxy),uxy(nxy),uyy(nxy),vxx(nxy),vxy(nxy),vyy(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  ShearGradFlatStg = SWIMfailure
  error%Number  = UK_ERROR
  error%Routine = 'ShearGradFlatStg'
  error%Message = 'Error allocating 2d gradient arrays'
  GOTO 9999
END IF

DO k = 1,nz+1
  k0 = (k-1)*nxy + 1
  u => fld%U(k0:)
  v => fld%V(k0:)
  s => fld%dU2(k0:)

!$OMP PARALLEL SHARED(grid,u,v,nx,ny,dx,dy,uxx,vxx,uyy,vyy,uxy,vxy)
!$OMP DO PRIVATE(j,j0,jm0,jp0,i,im,ip,is,jm,jp,ipjm,imjp,ijm,x,y,xmap,ymap,dxr,dyr)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    jp0 = MIN(j0+nx,(ny-1)*nx)
    y   = grid%Ymin + FLOAT(j-1)*dy

    DO i = 1,nx
      im   = j0 + MAX(i-1,1)
      ip   = j0 + MIN(i+1,nx)
      is   = j0 + i
      jm   = jm0 + i
      jp   = jp0 + i
      ipjm = jm0 + MIN(i+1,nx)
      imjp = jp0 + MAX(i-1,1)
      x    = grid%Xmin + FLOAT(i-1)*dx

      CALL SWIMmapfac( grid%coord,x,y,xmap,ymap )
      dxr = xmap/dx
      dyr = ymap/dy

      uxx(is) = ((u(ip)-2.*u(is)+u(im))*dxr*dxr)**2
      vxx(is) = ((v(ip)-2.*v(is)+v(im))*dxr*dxr)**2
      uyy(is) = ((u(jp)-2.*u(is)+u(jm))*dyr*dyr)**2
      vyy(is) = ((v(jp)-2.*v(is)+v(jm))*dyr*dyr)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        ijm = jm0 + MAX(i-1,1)
        uxy(is) = ((u(is)+u(ijm)-u(im)-u(jm))*dxr*dyr)**2
        vxy(is) = ((v(is)+v(ijm)-v(im)-v(jm))*dxr*dyr)**2
      ELSE
        uxy(is) = ((u(jp)+u(im)-u(is)-u(imjp))*dxr*dyr)**2
        vxy(is) = ((v(ip)+v(jm)-v(is)-v(ipjm))*dxr*dyr)**2
      END IF

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

  IF( BTEST(grid%type,GTB_STAGGERB) )THEN

!$OMP PARALLEL SHARED(s,nx,ny,uxx,vxx,uyy,vyy,uxy,vxy)
!$OMP DO PRIVATE(j,j0,jm0,i,im,jm,is,ijm)
    DO j = 1,ny
      j0  = (j-1)*nx
      jm0 = MAX(j0-nx,0)
      DO i = 1,nx
        is  = j0 + i
        im  = j0 + MAX(i-1,1)
        jm  = jm0 + i
        ijm = jm0 + MAX(i-1,1)
        s(is) = 0.25*(uxx(is)+uxx(im)+uxx(jm)+uxx(ijm)  + &
                      vxx(is)+vxx(im)+vxx(jm)+vxx(ijm)  + &
                      uyy(is)+uyy(im)+uyy(jm)+uyy(ijm)  + &
                      vyy(is)+vyy(im)+vyy(jm)+vyy(ijm)) + &
                      uxy(is)+vxy(is)
      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

  ELSE

!$OMP PARALLEL SHARED(s,nx,ny,uxx,vxx,uyy,vyy,uxy,vxy)
!$OMP DO PRIVATE(j,j0,jm0,i,im,jm,is)
    DO j = 1,ny
      j0  = (j-1)*nx
      jm0 = MAX(j0-nx,0)
      DO i = 1,nx
        is = j0 + i
        im = j0 + MAX(i-1,1)
        jm = jm0 +i
        s(is) = 0.5*(uxx(is)+uxx(im)+uyy(is)+uyy(im)  + &
                     vyy(is)+vyy(jm)+vxx(is)+vxx(jm)) + &
                     vxy(is)+vxy(im)+uxy(is)+uxy(jm)
      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

  END IF

!------ Set outer rows

  j0 = (ny-1)*nx
  DO i = 1,nx
    s(i)    = s(i+nx)
    s(j0+i) = s(j0+i-nx)
  END DO

  DO j = 1,ny
    j0 = (j-1)*nx
    s(j0+1)  = s(j0+2)
    s(j0+nx) = s(j0+nx-1)
  END DO

END DO

IF( ASSOCIATED(uxx) )DEALLOCATE( uxx,STAT=alloc_stat )
IF( ASSOCIATED(uxy) )DEALLOCATE( uxy,STAT=alloc_stat )
IF( ASSOCIATED(uyy) )DEALLOCATE( uyy,STAT=alloc_stat )
IF( ASSOCIATED(vxx) )DEALLOCATE( vxx,STAT=alloc_stat )
IF( ASSOCIATED(vxy) )DEALLOCATE( vxy,STAT=alloc_stat )
IF( ASSOCIATED(vyy) )DEALLOCATE( vyy,STAT=alloc_stat )

ShearGradFlatStg = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ShearGradTerStg( fld,grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: fld
TYPE( MetGrid   ), INTENT( IN    ) :: grid

INTEGER alloc_stat, nx, ny, nz, nxy, i, j, k
INTEGER k0, j0, jm0, jp0, im, ip, is, jm, jp, ijm, imjp, ipjm, ijp

REAL x, y, zbtop, dx, dy, dxr, dyr, dzr
REAL tem1, tem2, tem3, tem4, temp, temm

LOGICAL lZ3d

REAL, DIMENSION(:), POINTER     :: u, v, s, z
REAL, DIMENSION(:), POINTER     :: hx, hy, du, dv, zb, zbw
REAL, DIMENSION(:), ALLOCATABLE :: uxx, uxy, uyy, vxx, vxy, vyy
REAL, DIMENSION(:), ALLOCATABLE :: uz, vz, uxz, uyz, vxz, vyz
REAL, DIMENSION(:), ALLOCATABLE :: uzm, vzm, uxzm, uyzm, vxzm, vyzm
REAL, DIMENSION(:), ALLOCATABLE :: gx, gy, gxx, gyy, gxy
REAL, DIMENSION(:), ALLOCATABLE :: gxm, gym, gxxm, gyym, gxym
REAL, DIMENSION(:), ALLOCATABLE :: xmap, ymap
REAL, DIMENSION(:), ALLOCATABLE :: zx, zy, zxp, zyp

ShearGradTerStg = SWIMfailure


!------ Set locals

nx = grid%nX; ny = grid%nY; nz = grid%nZ; nxy = grid%nXY
dx = grid%dX; dy = grid%dY

IF( BTEST(grid%type,GTB_SIGMA) )THEN
  hx => grid%sigma%Px
  hy => grid%sigma%Py
  zbtop = 1.0
ELSE
  hx => grid%terrain%Hx
  hy => grid%terrain%Hy
  zbtop = grid%Ztop
END IF

du => grid%terrain%Du
dv => grid%terrain%Dv

zb  => grid%Z
zbw => grid%Zw

ALLOCATE( uxx(nxy),uxy(nxy),uyy(nxy),vxx(nxy),vxy(nxy),vyy(nxy), &
          uz(nxy),vz(nxy),uxz(nxy),uyz(nxy),vxz(nxy),vyz(nxy), &
          uzm(nxy),vzm(nxy),uxzm(nxy),uyzm(nxy),vxzm(nxy),vyzm(nxy), &
          gx(nxy),gy(nxy),gxx(nxy),gyy(nxy),gxy(nxy), &
          gxm(nxy),gym(nxy),gxxm(nxy),gyym(nxy),gxym(nxy), &
          xmap(nxy),ymap(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  ShearGradTerStg = SWIMfailure
  error%Number    = UK_ERROR
  error%Routine   = 'ShearGradTerStg'
  error%Message   = 'Error allocating 2d gradient arrays'
  GOTO 9999
END IF

IF( ASSOCIATED(fld%Z) )THEN  !Allocate arrays if using 3d height field
  lZ3d = .TRUE.
  ALLOCATE( zx(nxy),zy(nxy),zxp(nxy),zyp(nxy),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    ShearGradTerStg = SWIMfailure
    error%Number    = UK_ERROR
    error%Routine   = 'ShearGradTerStg'
    error%Message   = 'Error allocating gradient arrays used with 3d height field'
    GOTO 9999
  END IF
ELSE
  lZ3d = .FALSE.
END IF

!------ Pre-compute map factors

DO j = 1,ny
  j0  = (j-1)*nx
  y   = grid%Ymin + FLOAT(j-1)*dy

  DO i = 1,nx
    is   = j0 + i
    x    = grid%Xmin + FLOAT(i-1)*dx

    CALL SWIMmapfac( grid%coord,x,y,xmap(is),ymap(is) )

  END DO
END DO

!------ Initialize work arrays

DO i = 1,nxy
  uzm(i)  = 0.; vzm(i)  = 0.
  uxzm(i) = 0.; vxzm(i) = 0.
  uyzm(i) = 0.; vyzm(i) = 0.

  gxm(i)  = -hx(i)
  gym(i)  = -hy(i)
  gxxm(i) = 0.   !Multiplies uzm,vzm=0
  gyym(i) = 0.
  gxym(i) = 0.
END DO

!------ Initialize z-height gradients if using 3d height field

IF( lZ3d )THEN
  IF( .NOT.BTEST(grid%type,GTB_Z3DW) )THEN
    z => fld%Z(nxy+1:)
    DO j = 1,ny
      j0  = (j-1)*nx
      jp0 = MIN(j0+nx,(ny-1)*nx)
      DO i = 1,nx
        is   = j0 + i
        ip   = j0 + MIN(i+1,nx)
        jp   = jp0 + i
        dxr  = xmap(is)/dx
        dyr  = ymap(is)/dy
        zxp(is) = (z(ip)-z(is))*dxr
        zyp(is) = (z(jp)-z(is))*dyr
      END DO
    END DO
  END IF
END IF

kLoop : DO k = 2,nz
  k0 = (k-1)*nxy + 1

  u => fld%U(k0:)
  v => fld%V(k0:)
  s => fld%dU2(k0:)

!----- Compute gradient factors and du/dz, dv/zdz

  IF( lZ3d )THEN

    IF( BTEST(grid%type,GTB_Z3DW) )THEN

      z => fld%Z(k0-nxy:)
!$OMP PARALLEL SHARED(grid,u,v,z,gx,gy,hx,hy,uz,vz,nx,ny,nxy,xmap,ymap,dx,dy)
!$OMP DO PRIVATE(j,j0,jp0,i,is,ip,jp,ijp,dxr,dyr,dzr,tem1,tem2)
      DO j = 1,ny
        j0  = (j-1)*nx
        jp0 = MIN(j0+nx,(ny-1)*nx)

        DO i = 1,nx
          is   = j0 + i
          ip   = j0 + MIN(i+1,nx)
          jp   = jp0 + i
          dxr  = xmap(is)/dx
          dyr  = ymap(is)/dy

          gx(is) = -(z(ip+nxy)-z(is+nxy))*dxr - hx(is)
          gy(is) = -(z(jp+nxy)-z(is+nxy))*dyr - hy(is)

          IF( BTEST(grid%type,GTB_STAGGERB) )THEN
            ijp   = jp0 + MIN(i+1,nx)
            tem1   = 0.25*(z(is)+z(ip)+z(jp)+z(ijp))
            tem2   = 0.25*(z(is+2*nxy)+z(ip+2*nxy)+z(jp+2*nxy)+z(ijp+2*nxy))
            dzr    = 2./(tem2-tem1)
            uz(is) = (u(nxy+is)-u(is))*dzr
            vz(is) = (v(nxy+is)-v(is))*dzr
          ELSE
            tem1   = z(is)+z(ip)
            tem2   = z(is+2*nxy)+z(ip+2*nxy)
            dzr    = 4./(tem2-tem1)
            uz(is) = (u(nxy+is)-u(is))*dzr
            tem1   = z(is)+z(jp)
            tem2   = z(is+2*nxy)+z(jp+2*nxy)
            dzr    = 4./(tem2-tem1)
            vz(is) = (v(nxy+is)-v(is))*dzr
          END IF

        END DO

      END DO
!$OMP END DO
!$OMP END PARALLEL

    ELSE

      z => fld%Z(k0:)
!$OMP PARALLEL SHARED(grid,u,v,z,zx,zy,zxp,zyp,gx,gy,hx,hy,uz,vz,nx,ny,xmap,ymap,dx,dy)
!$OMP DO PRIVATE(j,j0,jp0,i,is,ip,jp,ijp,dxr,dyr,dzr)
      DO j = 1,ny
        j0  = (j-1)*nx
        jp0 = MIN(j0+nx,(ny-1)*nx)

        DO i = 1,nx
          is   = j0 + i
          ip   = j0 + MIN(i+1,nx)
          jp   = jp0 + i
          dxr  = xmap(is)/dx
          dyr  = ymap(is)/dy

          zx(is)  = zxp(is)
          zy(is)  = zyp(is)
          zxp(is) = (z(ip+nxy)-z(is+nxy))*dxr
          zyp(is) = (z(jp+nxy)-z(is+nxy))*dyr

          gx(is) = -0.5*(zx(is)+zxp(is)) - hx(is)
          gy(is) = -0.5*(zy(is)+zyp(is)) - hy(is)

          IF( BTEST(grid%type,GTB_STAGGERB) )THEN
            ijp   = jp0 + MIN(i+1,nx)
            dzr   = 4./(z(is+nxy)+z(ip+nxy)+z(jp+nxy)+z(ijp+nxy) - &
                       (z(is    )+z(ip    )+z(jp    )+z(ijp    )))
            uz(is) = (u(nxy+is)-u(is))*dzr
            vz(is) = (v(nxy+is)-v(is))*dzr
          ELSE
            dzr    = 2./(z(is+nxy)+z(ip+nxy)-z(is)-z(ip))
            uz(is) = (u(nxy+is)-u(is))*dzr
            dzr    = 2./(z(is+nxy)+z(jp+nxy)-z(is)-z(jp))
            vz(is) = (v(nxy+is)-v(is))*dzr
          END IF

        END DO

      END DO
!$OMP END DO
!$OMP END PARALLEL

    END IF

  ELSE

    dzr = 1./(zb(k)-zb(k-1))

!$OMP PARALLEL SHARED(u,v,gx,gy,hx,hy,du,dv,uz,vz,nxy,k,zbw,zbtop,dzr)
!$OMP DO PRIVATE(i)
    DO i = 1,nxy

      gx(i) = (zbw(k)/zbtop-1.)*hx(i) / du(i)
      gy(i) = (zbw(k)/zbtop-1.)*hy(i) / dv(i)

      uz(i) = (u(nxy+i)-u(i))*dzr
      vz(i) = (v(nxy+i)-v(i))*dzr

    END DO
!$OMP END DO
!$OMP END PARALLEL

  END IF

!------ Compute second derivatives in transformed coordinates

!$OMP PARALLEL SHARED(grid,u,v,uz,vz,uxx,vxx,uyy,vyy,uxy,vxy,uxz,uyz,vxz,vyz,gx,gy,gxx,gyy,gxy,nx,ny,xmap,ymap,dx,dy)
!$OMP DO PRIVATE(j,j0,jm0,jp0,i,is,im,ip,jm,jp,ijm,ipjm,imjp,dxr,dyr)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    jp0 = MIN(j0+nx,(ny-1)*nx)

    DO i = 1,nx
      is   = j0 + i
      im   = j0 + MAX(i-1,1)
      ip   = j0 + MIN(i+1,nx)
      jm   = jm0 + i
      jp   = jp0 + i
      ijm  = jm0 + MAX(i-1,1)
      ipjm = jm0 + MIN(i+1,nx)
      imjp = jp0 + MAX(i-1,1)
      dxr  = xmap(is)/dx
      dyr  = ymap(is)/dy

      uxx(is) = (u(ip)-2.*u(is)+u(im))*dxr*dxr
      vxx(is) = (v(ip)-2.*v(is)+v(im))*dxr*dxr
      uyy(is) = (u(jp)-2.*u(is)+u(jm))*dyr*dyr
      vyy(is) = (v(jp)-2.*v(is)+v(jm))*dyr*dyr

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        ijm = jm0 + MAX(i-1,1)
        uxy(is) = ((u(is)+u(ijm)-u(im)-u(jm))*dxr*dyr)**2
        vxy(is) = ((v(is)+v(ijm)-v(im)-v(jm))*dxr*dyr)**2
        uxz(is) = (uz(is)-uz(im))*dxr
        uyz(is) = (uz(is)-uz(jm))*dyr
        vxz(is) = (vz(is)-vz(im))*dxr
        vyz(is) = (vz(is)-vz(jm))*dyr
      ELSE
        uxy(is) = (u(jp)+u(im)-u(is)-u(imjp))*dxr*dyr
        vxy(is) = (v(ip)+v(jm)-v(is)-v(ipjm))*dxr*dyr
        uxz(is) = (uz(is)-uz(im))*dxr
        uyz(is) = (uz(jp)-uz(is))*dyr
        vxz(is) = (vz(ip)-vz(is))*dxr
        vyz(is) = (vz(is)-vz(jm))*dyr
      END IF

      gxx(is) = (gx(is)-gx(im))*dxr
      gyy(is) = (gy(is)-gy(jm))*dyr
      gxy(is) = 0.5*((gx(jp)-gx(is))*dyr + (gy(ip)-gy(is))*dxr)

   END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Terrain terms

!$OMP PARALLEL SHARED(grid,s,nx,ny,uz,vz,uxx,vxx,uyy,vyy,uxy,vxy,uxz,uyz,vxz,&
!$OMP                 vyz,uzm,vzm,uxzm,uyzm,vxzm,vyzm,gx,gy,gxx,gyy,gxy,gxm,gym,gxxm,gyym,gxym)
!$OMP DO PRIVATE(j,j0,jm0,i,is,im,jm,ijm,tem1,tem2,tem3,tem4,temp,temm)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    DO i = 1,nx
      is  = j0 + i
      im  = j0 + MAX(i-1,1)
      jm  = jm0 + i
      ijm = jm0 + MAX(i-1,1)

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        tem1 = 0.5*(uyz (is)+uyz (im))
        tem2 = 0.5*(uyzm(is)+uyzm(im))
        tem3 = 0.5*(vxz (is)+vxz (jm))
        tem4 = 0.5*(vxzm(is)+vxzm(jm))
      ELSE
        tem1 = 0.25*(uyz (is)+uyz (im)+uyz (jm)+uyz (ijm))
        tem2 = 0.25*(uyzm(is)+uyzm(im)+uyzm(jm)+uyzm(ijm))
        tem3 = 0.25*(vxz (is)+vxz (im)+vxz (jm)+vxz (ijm))
        tem4 = 0.25*(vxzm(is)+vxzm(im)+vxzm(jm)+vxzm(ijm))
      END IF

!------ (du2/dx2)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = (gx (is)+gx (im))*(uxz (is)+uxz (jm))            !2*Gx|x * uxz
        temm = (gxm(is)+gxm(im))*(uxzm(is)+uxzm(jm))

        temp = temp + 0.5*gxx (is)*(uz (is)+uz (im)+uz (jm)+uz (ijm)) !Gxx * uz|x
        temm = temm + 0.5*gxxm(is)*(uzm(is)+uzm(im)+uzm(jm)+uzm(ijm))

        temp = uxx(is)**2 + uxx(im)**2 + uxx(jm)**2 + uxx(ijm)**2 + temp**2 + temm**2 + &
               0.5*(uxx(is)+uxx(im)+uxx(jm)+uxx(ijm))*(temp+temm)
        temp = 0.5*temp
      ELSE
        temp = (gx (is)+gx (im))*uxz (is)            !2*Gx|x * uxz
        temm = (gxm(is)+gxm(im))*uxzm(is)

        temp = temp + 0.5*gxx (is)*(uz (is)+uz (im)) !Gxx * uz|x
        temm = temm + 0.5*gxxm(is)*(uzm(is)+uzm(im))

        temp = uxx(is)**2 + uxx(im)**2 + temp**2 + temm**2 + &
               (uxx(is)+uxx(im))*(temp+temm)
      END IF

      s(is) = MAX(temp,0.)

!------ (du2/dy2)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = tem1*(gy (is)+gy (jm))
        temm = tem2*(gym(is)+gym(jm))

        temp = temp + 0.25*gyy (is)*(uz (is)+uz (im)+uz (jm)+uz (ijm))
        temm = temm + 0.25*gyym(is)*(uzm(is)+uzm(im)+uzm(jm)+uzm(ijm))

        temp = uyy(is)**2 + uyy(im)**2 + uyy(jm)**2 + uyy(ijm)**2 + temp**2 + temm**2 + &
               0.5*(uyy(is)+uyy(im)+uyy(jm)+uyy(ijm))*(temp+temm)
        temp = 0.5*temp
      ELSE
        temp = tem1*(gy (is)+gy (jm))                !2*Gy|y * uyz|xy
        temm = tem2*(gym(is)+gym(jm))

        temp = temp + 0.5*gyy (is)*(uz (is)+uz (im)) !Gyy * uz|x
        temm = temm + 0.5*gyym(is)*(uzm(is)+uzm(im))

        temp = uyy(is)**2 + uyy(im)**2 + temp**2 + temm**2 + &
               (uyy(is)+uyy(im))*(temp+temm)
      END IF

      s(is) = s(is) + MAX(temp,0.)

!------ 2*(du2/dxdy)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = 0.5*(gx (is)*uyz (is)+gx (im)*uyz (im))
        temm = 0.5*(gxm(is)*uyzm(is)+gxm(im)*uyzm(im))

        temp = temp + 0.5*(gy (is)*uxz (is)+gy (jm)*uxz (jm))
        temm = temm + 0.5*(gym(is)*uxzm(is)+gym(jm)*uxzm(jm))

        temp = temp + 0.25*(gxy (is)*uz (is)+gxy (jm )*uz (jm) + &
                            gxy (im)*uz (im)+gxy (ijm)*uz (ijm))
        temm = temm + 0.25*(gxym(is)*uzm(is)+gxym(jm )*uzm(jm) + &
                            gxym(im)*uzm(im)+gxym(ijm)*uzm(ijm))
      ELSE
        temp = 0.5*(gx (is)+gx (im))*tem1            !Gx|x * uyz|xy
        temm = 0.5*(gxm(is)+gxm(im))*tem2

        temp = temp + 0.5*(gy (is)+gy (jm))*uxz (is) !Gy|y * uxz
        temm = temm + 0.5*(gym(is)+gym(jm))*uxzm(is)

        temp = temp + 0.25*((gxy(is)+gxy(jm ))*uz(is) + &
                            (gxy(im)+gxy(ijm))*uz(im))     !(Gxy|y * uz)|x
        temm = temm + 0.25*((gxym(is)+gxym(jm ))*uzm(is) + &
                            (gxym(im)+gxym(ijm))*uzm(im))

        temp = 2.*(uxy(is)**2 + uxy(jm)**2 + temp**2 + temm**2 + &
                  (uxy(is)+uxy(jm))*(temp+temm))
      END IF

      s(is) = s(is) + MAX(temp,0.)

!------ (dv2/dx2)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = (gx (is)+gx (im))*(vxz (is)+vxz (jm))
        temm = (gxm(is)+gxm(im))*(vxzm(is)+vxzm(jm))

        temp = temp + 0.5*gxx (is)*(vz (is)+vz (im)+vz (jm)+vz (ijm))
        temm = temm + 0.5*gxxm(is)*(vzm(is)+vzm(im)+vzm(jm)+vzm(ijm))

        temp = vxx(is)**2 + vxx(im)**2 + vxx(jm)**2 + vxx(ijm)**2 + temp**2 + temm**2 + &
               0.5*(vxx(is)+vxx(im)+vxx(jm)+vxx(ijm))*(temp+temm)
        temp = 0.5*temp
      ELSE
        temp = tem3*(gx (is)+gx (im))                !2*Gx|x * vxz|xy
        temm = tem4*(gxm(is)+gxm(im))

        temp = temp + 0.5*gxx (is)*(vz (is)+vz (jm)) !Gxx * vz|y
        temm = temm + 0.5*gxxm(is)*(vzm(is)+vzm(jm))

        temp = vxx(is)**2 + vxx(jm)**2 + temp**2 + temm**2 + &
               (vxx(is)+vxx(jm))*(temp+temm)
      END IF

      s(is) = s(is) + MAX(temp,0.)

!------ (dv2/dy2)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = tem3*(gy (is)+gy (jm))
        temm = tem4*(gym(is)+gym(jm))

        temp = temp + 0.25*gyy (is)*(vz (is)+vz (im)+vz (jm)+vz (ijm))
        temm = temm + 0.25*gyym(is)*(vzm(is)+vzm(im)+vzm(jm)+vzm(ijm))

        temp = vyy(is)**2 + vyy(im)**2 + vyy(jm)**2 + vyy(ijm)**2 + temp**2 + temm**2 + &
               0.5*(vyy(is)+vyy(im)+vyy(jm)+vyy(ijm))*(temp+temm)
        temp = 0.5*temp
      ELSE
        temp = (gy (is)+gy (jm))*vyz (is)            !2*Gy|y * vyz
        temm = (gym(is)+gxm(jm))*vyzm(is)

        temp = temp + 0.5*gyy (is)*(vz (is)+vz (jm)) !Gyy * vz|y
        temm = temm + 0.5*gyym(is)*(vzm(is)+vzm(jm))

        temp = vyy(is)**2 + vyy(jm)**2 + temp**2 + temm**2 + &
               (vyy(is)+vyy(jm))*(temp+temm)
      END IF

      s(is) = s(is) + MAX(temp,0.)

!------ 2*(dv2/dxdy)**2

      IF( BTEST(grid%type,GTB_STAGGERB) )THEN
        temp = 0.5*(gx (is)*vyz (is)+gx (im)*vyz (im))
        temm = 0.5*(gxm(is)*vyzm(is)+gxm(im)*vyzm(im))

        temp = temp + 0.5*(gy (is)*vxz (is)+gy (jm)*vxz (jm))
        temm = temm + 0.5*(gym(is)*vxzm(is)+gym(jm)*vxzm(jm))

        temp = temp + 0.25*(gxy (is)*vz (is)+gxy (jm )*vz (jm) + &
                            gxy (im)*vz (im)+gxy (ijm)*vz (ijm))
        temm = temm + 0.25*(gxym(is)*vzm(is)+gxym(jm )*vzm(jm) + &
                            gxym(im)*vzm(im)+gxym(ijm)*vzm(ijm))

        temp = 2.*(2.*vxy(is)**2 + temp**2 + temm**2 + 2.*vxy(is)*(temp+temm))
      ELSE
        temp = 0.5*(gy (is)+gy (jm))*tem3                  !Gy|y * vxz|xy
        temm = 0.5*(gym(is)+gym(jm))*tem4

        temp = temp + 0.5*(gx (is)+gx (im))*vyz (is)       !Gx|x * vyz
        temm = temm + 0.5*(gxm(is)+gxm(im))*vyzm(is)

        temp = temp + 0.25*((gxy(is)+gxy(im ))*vz(is) + &  !(Gxy|x * vz)|y
                            (gxy(jm)+gxy(ijm))*vz(jm))
        temm = temm + 0.25*((gxym(is)+gxym(im ))*vzm(is) + &
                            (gxym(jm)+gxym(ijm))*vzm(jm))

        temp = 2.*(vxy(is)**2 + vxy(im)**2 + temp**2 + temm**2 + &
                  (vxy(is)+vxy(im))*(temp+temm))
      END IF

      s(is) = s(is) + MAX(temp,0.)

      s(is) = 0.5*s(is)

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Shuffle uz, gx, etc. to lower slice

!$OMP PARALLEL SHARED(nxy,uz,uxz,uyz,vz,vxz,vyz,gx,gy,gxx,gyy,gxy,uzm,uxzm,uyzm,vzm,vxzm,vyzm,gxm,gym,gxxm,gyym,gxym)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy
    uzm(i)  = uz(i) ; uxzm(i) = uxz(i); uyzm(i) = uyz(i)
    vzm(i)  = vz(i) ; vxzm(i) = vxz(i); vyzm(i) = vyz(i)
    gxm(i)  = gx(i) ; gym(i)  = gy(i)
    gxxm(i) = gxx(i); gyym(i) = gyy(i); gxym(i) = gxy(i)
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Set outer rows

  j0 = (ny-1)*nx
  DO i = 1,nx
    s(i)    = s(i+nx)
    s(j0+i) = s(j0+i-nx)
  END DO

  DO j = 1,ny
    j0 = (j-1)*nx
    s(j0+1)  = s(j0+2)
    s(j0+nx) = s(j0+nx-1)
  END DO

!------ Set top/bottom slices

  IF( k == 2 )THEN
    DO is = 1,nxy
      fld%du2(is) = s(is)
    END DO
  ELSE IF( k == nz )THEN
    j0 = nz*nxy
    DO is = 1,nxy
      fld%du2(j0+is) = s(is)
    END DO
  END IF

END DO kLoop

IF( ALLOCATED(uxx) )DEALLOCATE( uxx,STAT=alloc_stat )
IF( ALLOCATED(uxy) )DEALLOCATE( uxy,STAT=alloc_stat )
IF( ALLOCATED(uyy) )DEALLOCATE( uyy,STAT=alloc_stat )
IF( ALLOCATED(vxx) )DEALLOCATE( vxx,STAT=alloc_stat )
IF( ALLOCATED(vxy) )DEALLOCATE( vxy,STAT=alloc_stat )
IF( ALLOCATED(vyy) )DEALLOCATE( vyy,STAT=alloc_stat )

IF( ALLOCATED(uz)  )DEALLOCATE( uz,STAT=alloc_stat )
IF( ALLOCATED(vz)  )DEALLOCATE( vz,STAT=alloc_stat )
IF( ALLOCATED(uxz) )DEALLOCATE( uxz,STAT=alloc_stat )
IF( ALLOCATED(uyz) )DEALLOCATE( uyz,STAT=alloc_stat )
IF( ALLOCATED(vxz) )DEALLOCATE( vxz,STAT=alloc_stat )
IF( ALLOCATED(vyz) )DEALLOCATE( vyz,STAT=alloc_stat )

IF( ALLOCATED(uzm)  )DEALLOCATE( uzm,STAT=alloc_stat )
IF( ALLOCATED(vzm)  )DEALLOCATE( vzm,STAT=alloc_stat )
IF( ALLOCATED(uxzm) )DEALLOCATE( uxzm,STAT=alloc_stat )
IF( ALLOCATED(uyzm) )DEALLOCATE( uyzm,STAT=alloc_stat )
IF( ALLOCATED(vxzm) )DEALLOCATE( vxzm,STAT=alloc_stat )
IF( ALLOCATED(vyzm) )DEALLOCATE( vyzm,STAT=alloc_stat )

IF( ALLOCATED(gx)  )DEALLOCATE( gx,STAT=alloc_stat )
IF( ALLOCATED(gy)  )DEALLOCATE( gy,STAT=alloc_stat )
IF( ALLOCATED(gxx) )DEALLOCATE( gxx,STAT=alloc_stat )
IF( ALLOCATED(gyy) )DEALLOCATE( gyy,STAT=alloc_stat )
IF( ALLOCATED(gxy) )DEALLOCATE( gxy,STAT=alloc_stat )

IF( ALLOCATED(gxm)  )DEALLOCATE( gxm,STAT=alloc_stat )
IF( ALLOCATED(gym)  )DEALLOCATE( gym,STAT=alloc_stat )
IF( ALLOCATED(gxxm) )DEALLOCATE( gxxm,STAT=alloc_stat )
IF( ALLOCATED(gyym) )DEALLOCATE( gyym,STAT=alloc_stat )
IF( ALLOCATED(gxym) )DEALLOCATE( gxym,STAT=alloc_stat )

IF( ALLOCATED(zx)  )DEALLOCATE( zx,STAT=alloc_stat )
IF( ALLOCATED(zy)  )DEALLOCATE( zy,STAT=alloc_stat )
IF( ALLOCATED(zxp) )DEALLOCATE( zxp,STAT=alloc_stat )
IF( ALLOCATED(zyp) )DEALLOCATE( zyp,STAT=alloc_stat )

IF( ALLOCATED(xmap) )DEALLOCATE( xmap,STAT=alloc_stat )
IF( ALLOCATED(ymap) )DEALLOCATE( ymap,STAT=alloc_stat )

ShearGradTerStg = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ShearGradTer( fld,grid )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetMean3D ), INTENT( INOUT ) :: fld
TYPE( MetGrid   ), INTENT( IN    ) :: grid

INTEGER alloc_stat, nx, ny, nz, nxy, i, j, k
INTEGER k0, j0, jm0, jp0, im, ip, is, jm, jp, ijm, ijp, imjp, ipjm

REAL x, y, zbtop, dx, dy, dxr, dyr, dzr
REAL uxxp, vxxp, uyyp, vyyp, uxyp, vxyp
REAL tem1, tem2, tem3, tem4, temp, temm
REAL tem5, tem6, tem7, tem8

LOGICAL lZ3d

REAL, DIMENSION(:), POINTER     :: u, v, s, z
REAL, DIMENSION(:), POINTER     :: hx, hy, d, zb
REAL, DIMENSION(:), ALLOCATABLE :: uz, vz, uxz, uyz, vxz, vyz
REAL, DIMENSION(:), ALLOCATABLE :: uzm, vzm, uxzm, uyzm, vxzm, vyzm
REAL, DIMENSION(:), ALLOCATABLE :: gx, gy, gxx, gyy, gxy
REAL, DIMENSION(:), ALLOCATABLE :: gxm, gym, gxxm, gyym, gxym
REAL, DIMENSION(:), ALLOCATABLE :: xmap, ymap

ShearGradTer = SWIMfailure

!------ Set locals

nx = grid%nX; ny = grid%nY; nz = grid%nZ; nxy = grid%nXY
dx = grid%dX; dy = grid%dY

IF( BTEST(grid%type,GTB_SIGMA) )THEN
  hx => grid%sigma%Px
  hy => grid%sigma%Py
  zbtop = 1.0
ELSE
  hx => grid%terrain%Hx
  hy => grid%terrain%Hy
  zbtop = grid%Ztop
END IF

d  => grid%terrain%Du
zb => grid%Z

ALLOCATE( uz(nxy),vz(nxy),uzm(nxy),vzm(nxy), &
          uxz(nxy),uyz(nxy),vxz(nxy),vyz(nxy), &
          uxzm(nxy),uyzm(nxy),vxzm(nxy),vyzm(nxy), &
          gx(nxy),gy(nxy),gxx(nxy),gyy(nxy),gxy(nxy), &
          gxm(nxy),gym(nxy),gxxm(nxy),gyym(nxy),gxym(nxy), &
          xmap(nxy),ymap(nxy),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  ShearGradTer = SWIMfailure
  error%Number  = UK_ERROR
  error%Routine = 'ShearGradTer'
  error%Message = 'Error allocating 2d gradient arrays'
  GOTO 9999
END IF

lZ3d = ASSOCIATED(fld%Z)

!------ Pre-compute map factors

DO j = 1,ny
  j0  = (j-1)*nx
  y   = grid%Ymin + FLOAT(j-1)*dy

  DO i = 1,nx
    is   = j0 + i
    x    = grid%Xmin + FLOAT(i-1)*dx

    CALL SWIMmapfac( grid%coord,x,y,xmap(is),ymap(is) )

  END DO
END DO

DO i = 1,nxy
  uzm(i)  = 0.; vzm(i)  = 0.
  uxzm(i) = 0.; vxzm(i) = 0.
  uyzm(i) = 0.; vyzm(i) = 0.

  gxm(i)  = -hx(i)
  gym(i)  = -hy(i)
  gxxm(i) = 0.   !Multiplies uzm,vzm=0
  gyym(i) = 0.
  gxym(i) = 0.
END DO

kLoop : DO k = 1,nz-1
  k0 = (k-1)*nxy + 1
  u => fld%U(k0:)
  v => fld%V(k0:)
  s => fld%dU2(k0:)

  IF( lZ3d )THEN

    z => fld%Z(k0:)

!$OMP PARALLEL SHARED(nx,ny,nxy,z,xmap,ymap,dx,dy,hx,hy,u,v,uz,vz,gx,gy)
!$OMP DO PRIVATE(j,j0,jp0,i,is,ip,jp,dxr,dyr,dzr)
    DO j = 1,ny
      j0  = (j-1)*nx
      jp0 = MIN(j0+nx,(ny-1)*nx)

      DO i = 1,nx
        is   = j0 + i
        ip   = j0 + MIN(i+1,nx)
        jp   = jp0 + i
        dxr  = xmap(is)/dx
        dyr  = ymap(is)/dy

        gx(is) = -(z(ip)+z(is))*dxr - hx(is)
        gy(is) = -(z(jp)+z(is))*dyr - hy(is)

        dzr    = 1./(z(is+nxy)-z(is))
        uz(is) = (u(nxy+is)-u(is))*dzr
        vz(is) = (v(nxy+is)-v(is))*dzr

      END DO

    END DO
!$OMP END DO
!$OMP END PARALLEL

  ELSE

    dzr  = 1./(zb(k+1)-zb(k))
    tem1 = 0.5*(zb(k)+zb(MIN(k+1,nz)))/zbtop-1.
!$OMP PARALLEL SHARED(nx,ny,tem1,hx,hy,d,gx,gy)
!$OMP DO PRIVATE(j,j0,jp0,i,is,ip,jp)
    DO j = 1,ny
      j0 = (j-1)*nx
      jp0 = MIN(j0+nx,(ny-1)*nx)
      DO i = 1,nx
        is = j0 + i
        ip = j0  + MIN(i+1,nx)
        jp = jp0 + i
        gx(is) = 2.*tem1*hx(is) / (d(is)+d(ip))
        gy(is) = 2.*tem1*hy(is) / (d(is)+d(jp))
      END DO
    END DO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL SHARED(nxy,dzr,u,v,uz,vz)
!$OMP DO PRIVATE(i)
    DO i = 1,nxy
      uz(i) = (u(nxy+i)-u(i))*dzr
      vz(i) = (v(nxy+i)-v(i))*dzr
    END DO
!$OMP END DO
!$OMP END PARALLEL

  END IF

!$OMP PARALLEL SHARED(nx,ny,xmap,ymap,dx,dy,uz,vz,uxz,uyz,vxz,vyz,gx,gy,gxx,gyy,gxy)
!$OMP DO PRIVATE(j,j0,jm0,jp0,i,im,ip,is,jm,jp,dxr,dyr)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    jp0 = MIN(j0+nx,(ny-1)*nx)

    DO i = 1,nx
      im = j0 + MAX(i-1,1)
      ip = j0 + MIN(i+1,nx)
      is = j0 + i
      jm = jm0 + i
      jp = jp0 + i
      dxr = xmap(is)/dx
      dyr = ymap(is)/dy

      uxz(is) = (uz(ip)-uz(is))*dxr; vxz(is) = (vz(ip)-vz(is))*dxr
      uyz(is) = (uz(jp)-uz(is))*dyr; vyz(is) = (vz(jp)-vz(is))*dyr

      gxx(is) = (gx(is)-gx(im))*dxr ; gyy(is) = (gy(is)-gy(jm))*dyr
      gxy(is) = 0.5*( (gy(ip)-gy(is))*dxr + (gx(jp)-gx(is))*dyr )

    END DO

  END DO
!$OMP END DO
!$OMP END PARALLEL

!$OMP PARALLEL SHARED(nx,ny,xmap,ymap,dx,dy,u,v,s,uz,vz,uxz,uyz,vxz,vyz,&
!$OMP                 uzm,vzm,uxzm,uyzm,vxzm,vyzm,gx,gy,gxx,gyy,gxy,gxm,gym,gxxm,gyym,gxym)
!$OMP DO PRIVATE(j,j0,jm0,jp0,i,im,ip,is,jm,jp,ijm,ijp,imjp,ipjm,dxr,dyr,uxxp,vxxp,&
!$OMP            uyyp,vyyp,uxyp,vxyp,tem1,tem2,tem3,tem4,tem5,tem6,tem7,tem8,temm,temp)
  DO j = 1,ny
    j0  = (j-1)*nx
    jm0 = MAX(j0-nx,0)
    jp0 = MIN(j0+nx,(ny-1)*nx)

    DO i = 1,nx
      im   = j0 + MAX(i-1,1)
      ip   = j0 + MIN(i+1,nx)
      is   = j0 + i
      jm   = jm0 + i
      jp   = jp0 + i
      ijm  = jm0 + MAX(i-1,1)
      ijp  = jp0 + MIN(i+1,nx)
      imjp = jp0 + MAX(i-1,1)
      ipjm = jm0 + MIN(i+1,nx)
      dxr  = xmap(is)/dx
      dyr  = ymap(is)/dy

      uxxp = (u(ip)-2.*u(is)+u(im))*dxr*dxr
      vxxp = (v(ip)-2.*v(is)+v(im))*dxr*dxr
      uyyp = (u(jp)-2.*u(is)+u(jm))*dyr*dyr
      vyyp = (v(jp)-2.*v(is)+v(jm))*dyr*dyr
      uxyp = 0.25*(u(ijp)+u(ijm)-u(imjp)-u(ipjm))*dxr*dyr
      vxyp = 0.25*(v(ijp)+v(ijm)-v(imjp)-v(ipjm))*dxr*dyr

!------ Cartesian terms

      s(is) = uxxp**2 + vxxp**2 + uyyp**2 + vyyp**2 + 2.*(uxyp**2 + vxyp**2)

!------ Non-Cartesian terms

!------ (du2/dx2)**2

      tem1 = 2.*gx(is)*uxz(is); tem3 = 2.*gxm(is)*uxzm(is)             !2*Gx*uxz (offset in x & z)
      tem2 = 2.*gx(im)*uxz(im); tem4 = 2.*gxm(im)*uxzm(im)

      temp = gxx(is)*uz(is); temm = gxxm(is)*uzm(is)                   !Gxx*uz (offset in z)

      temp = 0.25*(tem1**2+tem2**2+tem3**2+tem4**2) + 0.5*(temp**2+temm**2) + &
             uxxp*(temp+temm+0.5*(tem1+tem2+tem3+tem4)) + &
             0.5*((tem1+tem2)*temp + (tem2+tem3)*temm)

      s(is) = s(is) + MAX(temp,0.)

!------ (du2/dy2)**2

      tem1 = 2.*gy(is)*uyz(is); tem3 = 2.*gym(is)*uyzm(is)             !2*Gy*uyz
      tem2 = 2.*gy(jm)*uyz(jm); tem4 = 2.*gym(jm)*uyzm(jm)

      temp = gyy(is)*uz(is); temm = gyym(is)*uzm(is)                   !Gyy*uz

      temp = 0.25*(tem1**2+tem2**2+tem3**2+tem4**2) + 0.5*(temp**2+temm**2) + &
             uyyp*(temp+temm+0.5*(tem1+tem2+tem3+tem4)) + &
             0.5*((tem1+tem2)*temp + (tem2+tem3)*temm)

      s(is) = s(is) + MAX(temp,0.)

!------ 2*(du2/dxdy)**2

      tem1 = 0.25*(gx(is)+gx(im )+gx(jp)+gx(imjp))*uyz(is)             !Gx|xy * uyz (offset in y & z)
      tem2 = 0.25*(gx(jm)+gx(ijm)+gx(is)+gx(im  ))*uyz(jm)

      tem3 = 0.25*(gxm(is)+gxm(im )+gxm(jp)+gxm(imjp))*uyzm(is)
      tem4 = 0.25*(gxm(jm)+gxm(ijm)+gxm(is)+gxm(im  ))*uyzm(jm)

      tem5 = 0.25*(gy(is)+gy(jm )+gy(ip)+gy(ipjm))*uxz(is)             !Gy|xy * uxz (offset in x & z)
      tem6 = 0.25*(gy(im)+gy(ijm)+gy(is)+gy(jm  ))*uxz(im)

      tem7 = 0.25*(gym(is)+gym(jm )+gym(ip)+gym(ipjm))*uxzm(is)
      tem8 = 0.25*(gym(im)+gym(ijm)+gym(is)+gym(jm  ))*uxzm(im)

      temp = 0.25*(gxy (is)+gxy (im )+gxy (jp)+gxy (imjp))*uz (is)      !gxyy|xy * uz (offset in z)
      temm = 0.25*(gxym(is)+gxym(im )+gxym(jp)+gxym(imjp))*uzm(is)

      temp = 2.*(0.125*(tem1**2+tem2**2+tem3**2+tem4**2 + &
                        tem5**2+tem6**2+tem7**2+tem8**2) + &
             0.5*(temp**2 + temm**2) + uxyp*(temp+temm + &
             0.25*(tem1+tem2+tem3+tem4+tem5+tem6+tem7+tem8)) + &
             0.25*(tem1+tem2+tem5+tem6)*temp + &
             0.25*(tem3+tem4+tem7+tem8)*temm)

      s(is) = s(is) + MAX(temp,0.)

!------ (dv2/dx2)**2

      tem1 = 2.*gx(is)*vxz(is); tem3 = 2.*gxm(is)*vxzm(is)             !2*Gx*vxz
      tem2 = 2.*gx(im)*vxz(im); tem4 = 2.*gxm(im)*vxzm(im)

      temp = gxx(is)*vz(is); temm = gxxm(is)*vzm(is)                   !Gxx*vz

      temp = 0.25*(tem1**2+tem2**2+tem3**2+tem4**2) + 0.5*(temp**2+temm**2) + &
             vxxp*(temp+temm+0.5*(tem1+tem2+tem3+tem4)) + &
             0.5*((tem1+tem2)*temp + (tem2+tem3)*temm)

      s(is) = s(is) + MAX(temp,0.)

!------ (dv2/dy2)**2

      tem1 = 2.*gy(is)*vyz(is); tem3 = 2.*gym(is)*vyzm(is)             !2*Gy*vyz
      tem2 = 2.*gy(jm)*vyz(jm); tem4 = 2.*gym(jm)*vyzm(jm)

      temp = gyy(is)*vz(is); temm = gyym(is)*vzm(is)                   !Gyy*vz

      temp = 0.25*(tem1**2+tem2**2+tem3**2+tem4**2) + 0.5*(temp**2+temm**2) + &
             vyyp*(temp+temm+0.5*(tem1+tem2+tem3+tem4)) + &
             0.5*((tem1+tem2)*temp + (tem2+tem3)*temm)

      s(is) = s(is) + MAX(temp,0.)

!------ (dv2/dxdy)**2

      tem1 = 0.25*(gx(is)+gx(im )+gx(jp)+gx(imjp))*vyz(is)             !Gx|xy * vyz
      tem2 = 0.25*(gx(jm)+gx(ijm)+gx(is)+gx(im  ))*vyz(jm)

      tem3 = 0.25*(gxm(is)+gxm(im )+gxm(jp)+gxm(imjp))*vyzm(is)
      tem4 = 0.25*(gxm(jm)+gxm(ijm)+gxm(is)+gxm(im  ))*vyzm(jm)

      tem5 = 0.25*(gy(is)+gy(jm )+gy(ip)+gy(ipjm))*vxz(is)             !Gy|xy * vxz
      tem6 = 0.25*(gy(im)+gy(ijm)+gy(is)+gy(jm  ))*vxz(im)

      tem7 = 0.25*(gym(is)+gym(jm )+gym(ip)+gym(ipjm))*vxzm(is)
      tem8 = 0.25*(gym(im)+gym(ijm)+gym(is)+gym(jm  ))*vxzm(im)

      temp = 0.25*(gxy (is)+gxy (im )+gxy (jp)+gxy (imjp))*vz (is)      !gxyy|xy * vz
      temm = 0.25*(gxym(is)+gxym(im )+gxym(jp)+gxym(imjp))*vzm(is)

      temp = 2.*(0.125*(tem1**2+tem2**2+tem3**2+tem4**2 + &
                        tem5**2+tem6**2+tem7**2+tem8**2) + &
                 0.5*(temp**2 + temm**2) + vxyp*(temp+temm + &
                 0.25*(tem1+tem2+tem3+tem4+tem5+tem6+tem7+tem8)) + &
                 0.25*(tem1+tem2+tem5+tem6)*temp + &
                 0.25*(tem3+tem4+tem7+tem8)*temm)

      s(is) = s(is) + MAX(temp,0.)

    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Shuffle uz, gx, etc. to lower slice

!$OMP PARALLEL SHARED(nxy,uz,uxz,uyz,vz,vxz,vyz,gx,gy,gxx,gyy,gxy,uzm,uxzm,uyzm,vzm,vxzm,vyzm,gxm,gym,gxxm,gyym,gxym)
!$OMP DO PRIVATE(i)
  DO i = 1,nxy
    uzm(i)  = uz(i) ; uxzm(i) = uxz(i); uyzm(i) = uyz(i)
    vzm(i)  = vz(i) ; vxzm(i) = vxz(i); vyzm(i) = vyz(i)
    gxm(i)  = gx(i) ; gym(i)  = gy(i)
    gxxm(i) = gxx(i); gyym(i) = gyy(i); gxym(i) = gxy(i)
  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Set outer rows

  j0 = (ny-1)*nx
  DO i = 1,nx
    s(i)    = s(i+nx)
    s(j0+i) = s(j0+i-nx)
  END DO

  DO j = 1,ny
    j0 = (j-1)*nx
    s(j0+1)  = s(j0+2)
    s(j0+nx) = s(j0+nx-1)
  END DO

  IF( k == nz-1 )THEN  !Set top slice
    j0 = (nz-1)*nxy
    DO is = 1,nxy
      fld%du2(j0+is) = s(is)
    END DO
  END IF

END DO kLoop

IF( ALLOCATED(uz)  )DEALLOCATE( uz,STAT=alloc_stat )
IF( ALLOCATED(vz)  )DEALLOCATE( vz,STAT=alloc_stat )
IF( ALLOCATED(uxz) )DEALLOCATE( uxz,STAT=alloc_stat )
IF( ALLOCATED(uyz) )DEALLOCATE( uyz,STAT=alloc_stat )
IF( ALLOCATED(vxz) )DEALLOCATE( vxz,STAT=alloc_stat )
IF( ALLOCATED(vyz) )DEALLOCATE( vyz,STAT=alloc_stat )

IF( ALLOCATED(uzm)  )DEALLOCATE( uzm,STAT=alloc_stat )
IF( ALLOCATED(vzm)  )DEALLOCATE( vzm,STAT=alloc_stat )
IF( ALLOCATED(uxzm) )DEALLOCATE( uxzm,STAT=alloc_stat )
IF( ALLOCATED(uyzm) )DEALLOCATE( uyzm,STAT=alloc_stat )
IF( ALLOCATED(vxzm) )DEALLOCATE( vxzm,STAT=alloc_stat )
IF( ALLOCATED(vyzm) )DEALLOCATE( vyzm,STAT=alloc_stat )

IF( ALLOCATED(gx)  )DEALLOCATE( gx,STAT=alloc_stat )
IF( ALLOCATED(gy)  )DEALLOCATE( gy,STAT=alloc_stat )
IF( ALLOCATED(gxx) )DEALLOCATE( gxx,STAT=alloc_stat )
IF( ALLOCATED(gyy) )DEALLOCATE( gyy,STAT=alloc_stat )
IF( ALLOCATED(gxy) )DEALLOCATE( gxy,STAT=alloc_stat )

IF( ALLOCATED(gxm)  )DEALLOCATE( gxm,STAT=alloc_stat )
IF( ALLOCATED(gym)  )DEALLOCATE( gym,STAT=alloc_stat )
IF( ALLOCATED(gxxm) )DEALLOCATE( gxxm,STAT=alloc_stat )
IF( ALLOCATED(gyym) )DEALLOCATE( gyym,STAT=alloc_stat )
IF( ALLOCATED(gxym) )DEALLOCATE( gxym,STAT=alloc_stat )

IF( ALLOCATED(xmap) )DEALLOCATE( xmap,STAT=alloc_stat )
IF( ALLOCATED(ymap) )DEALLOCATE( ymap,STAT=alloc_stat )

ShearGradTer = SWIMresult

9999 CONTINUE

RETURN
END

