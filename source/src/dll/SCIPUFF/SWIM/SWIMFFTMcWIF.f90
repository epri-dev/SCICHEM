INTEGER FUNCTION FFTMcWIF( grid,u,v,w,p,alpha,psrce )

!------ Use FFT method

USE SWIM_fi
USE SWIMparam_fd
USE McWIFsubIntrf

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:),   POINTER :: u, v, w, p, alpha, psrce
REAL, DIMENSION(:),   POINTER :: pm

INTEGER irv, i, k, nxy, i0

INTEGER, EXTERNAL :: PostProgressMessage, PostCheckMessage

FFTMcWIF = SWIMfailure

IF( Prj%MC%MaxIterFFT <= 0 )THEN
  FFTMcWIF = SWIMresult
  RETURN
END IF

message%bString  = 'FFT method'
message%cString  = ' '

irv = PostProgressMessage( message )

!------ Compute pressure

CALL SetFFTsrce( grid,u,v,w,p,psrce )

irv = PostCheckMessage( )

pm => alpha

irv = IterFFT( grid,p,pm,psrce )
IF( irv /= SWIMsuccess )GOTO 9999

irv = PostCheckMessage( )

!------ Advance velocities and set bc's

nxy = grid%nXY

DO k = 1,grid%nZ+1
  i0 = (k-1)*nxy
  DO i = 1,nxy
    alpha(i0+i) = grid%McWIF%alphaFFT(k)
  END DO
END DO

CALL StepU( grid,u,v,w,p,alpha )
CALL SetBC( grid,u,v,w )

irv = PostCheckMessage()

FFTMcWIF = SWIMresult

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE SetFFTsrce( grid,u,v,w,p,psrce )

USE SWIM_fi

!------ Calculate pressure source terms

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:),   POINTER :: u, v, w, p, psrce

INTEGER j, k, i0, is, ix, ip
INTEGER nxb, nyb, nxyb, nxm1, nym1, nzm1

REAL wwtem, wwnew, temu, temv, cz, zz
REAL dxi, dyi, zbtop

REAL, DIMENSION(:), POINTER :: dzi, z
REAL, DIMENSION(:), POINTER :: Hx, Hy, D, Du, Dv

!------ Define locals

nxb  = grid%nX
nyb  = grid%nY
nxyb = nxb*nyb
nxm1 = nxb-1
nym1 = nyb-1
nzm1 = grid%nZ

dxi = grid%McWIF%dxi
dyi = grid%McWIF%dyi

zbtop = grid%Ztop

dzi  => grid%McWIF%dzi
z    => grid%McWIF%z

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy
D  => grid%terrain%D
Du => grid%terrain%Du
Dv => grid%terrain%Dv

DO is = 1,SIZE(p)
  p(is) = 0.
END DO

!------ Compute Poisson source = divergence

!$OMP PARALLEL SHARED(nym1,nxm1,nzm1,nxb,nxyb,dzi,u,v,w,Du,Dv,D,psrce,dxi,dyi)
!$OMP DO PRIVATE(j,i0,ix,is,wwtem,k,ip,cz,zz,temu,temv,wwnew)
DO j = 2,nym1
  i0 = (j-1)*nxb

  DO ix = 2,nxm1
    is = i0 + ix

    wwtem = 0.0

    DO k = 2,nzm1
      ip = (k-1)*nxyb + is

      cz   = dzi(k)
      zz   = 0.25*(1.0 - z(k)/zbtop)

      temu = Hx(is    )*Du(is    )*(u(ip    )+u(ip    +nxyb)) &
           + Hx(is-1  )*Du(is-1  )*(u(ip - 1)+u(ip-1  +nxyb))
      temv = Hy(is    )*Dv(is    )*(v(ip    )+v(ip    +nxyb)) &
           + Hy(is-nxb)*Dv(is-nxb)*(v(ip-nxb)+v(ip-nxb+nxyb))

      wwnew = w(ip) - zz*(temu+temv)/D(is)

      psrce(ip) = ( (wwnew-wwtem)*cz &
                  + (Dv(is)*v(ip)-Dv(is-nxb)*v(ip-nxb))*dyi &
                  + (Du(is)*u(ip)-Du(is-  1)*u(ip-  1))*dxi )
      wwtem  = wwnew

    END DO
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END

!==============================================================================

INTEGER FUNCTION IterFFT( grid,p,pm,psrce )

!------ Solve Poisson equation for pressure
!       PSRCE contains the explicit pressure source
!       P     contains the final pressure
!       PM    is used as local storage of old pressure field

USE SWIM_fi
USE FFTmod
USE SWIMparam_fd
USE SWIMutilArrayPtr

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:),   POINTER :: p, pm, psrce

INTEGER iter, ix, i, j, k, i0, ip, is, j0, j1, k0, ios, irv
INTEGER nyz, nzm2, n
REAL    dxi, dyi
REAL    dxxi, dyyi, tem, cz, pmax, dpmax, alp

REAL, DIMENSION(:), ALLOCATABLE :: wrk, wrk1, wrk2
REAL, DIMENSION(:,:), ALLOCATABLE :: wrk3, wrk4
REAL, DIMENSION(:), ALLOCATABLE :: gx, gxb, gy

REAL, DIMENSION(:), POINTER :: dzi
REAL, DIMENSION(:), POINTER :: Hx, Hy, D, Du, Dv

INTEGER, EXTERNAL :: PostProgressMessage
INTEGER, EXTERNAL :: PostCheckMessage

IterFFT = SWIMfailure
ResetButtons = .TRUE.

!------ Define locals

nxb  = grid%nX
nyb  = grid%nY
nxyb = nxb*nyb
nxm1 = nxb-1
nym1 = nyb-1
nxm2 = nxm1-1
nym2 = nym1-1
nzm1 = grid%nZ
nzm2 = nzm1-1
nzt  = nzm1+1
nyz  = nyb*nzt

dxi  = grid%McWIF%dxi
dyi  = grid%McWIF%dyi
dxxi = dxi*dxi
dyyi = dyi*dyi

dzi  => grid%McWIF%dzi

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy
D  => grid%terrain%D
Du => grid%terrain%Du
Dv => grid%terrain%Dv

ALLOCATE( gx(nyb*nzt),gxb(nyb*nzt),gy(nyb*nzt),wrk(nyb*nzt), &
          wrk1(nyb*nzt),wrk2(nyb*nzt),wrk3(nyb*nzt,nxb),wrk4(nyb*nzt,nxb),STAT=ios )
IF( ios /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'IterFFT'
  error%Message = 'Error allocating work arrays'
  GOTO 9999
END IF

n = SIZE(p)

alp = 1./(1.+grid%McWIF%MaxSlope**2)

!------ Initialize to avoid copying undefined numbers

DO i = 1,nyb*nzt
  wrk (i) = 1.E+36
  wrk1(i) = 1.E+36
  wrk2(i) = 1.E+36
END DO

DO ix = 1,nxb
    DO i = 1,nyb*nzt
      wrk3(i,ix) = 1.E+36
      wrk4(i,ix) = 1.E+36
    END DO
END DO

!------ Iteration

iter = 0

CALL enableSWIMhalt( StopMode )
SWIMiteration = .TRUE.

Iteration : DO

  irv = PostCheckMessage( )

  iter = iter + 1
  IF( iter > Prj%MC%MaxIterFFT )EXIT

!-----  Save old pressure in pm

  CALL CopyArray( pm,p,n )

  irv = PostCheckMessage( )

!-----  Save old value for WRK4=d/dz(gx*p|xz)

  ix = 1
  CALL coeff( grid,ix,gx,gxb,gy )

  irv = PostCheckMessage( )

  DO k = 1,nzm1
    k0 = (k-1)*nxyb
    DO j = 2,nym1
      i = (k-1)*nyb + j
      ip = (j-1)*nxb + ix + k0
      tem = p(ip) + p(ip+1) + p(ip+nxyb) + p(ip+1+nxyb)
      wrk2(i) = 0.25*gx(i)*tem
    END DO
  END DO

  DO k = 2,nzm1
    cz = dzi(k)
    DO j = 2,nym1
      i = (k-1)*nyb + j
      wrk4(i,ix) = (wrk2(i) - wrk2(i-nyb)) * cz
    END DO
  END DO
!----- b.c.'s
  j0 = nzm1*nyb
  j1 = nzm2*nyb
  DO j = 2,nym1
    wrk4(j,ix)    = 0.0
    wrk4(j+j0,ix) = wrk4(j+j1,ix)
  END DO

!-----  Save old value for WRK3=gx*(d/dx( p*d ))|z

  DO k = 1,nzt
    i0 = (k-1)*nyb
    k0 = (k-1)*nxyb
    DO j = 2,nym1
      i  = i0 + j
      is = (j-1)*nxb + ix
      ip = is + k0
      wrk1(i) = dxi*(p(ip+1)*D(is+1)-p(ip)*D(is))
    END DO
  END DO

!-----  gx*(d/dx( p*d ))|z

  DO k = 2,nzm1
    i0 = (k-1)*nyb
    DO j = 2,nym1
      i  = i0 + j
      wrk3(i,ix) = 0.5*gx(i)*(wrk1(i) + wrk1(i+nyb))
    END DO
  END DO
!----- b.c.'s
  DO j = 2,nym1
    wrk3(j,ix) = 0.0
  END DO

!-----  Loop over x-slices

  irv = PostCheckMessage()

!-----  First define d/dx work arrays

!$OMP PARALLEL SHARED(nxm1,nzm1,nym1,nxb,nyb,grid,wrk3,wrk4,p,dzi)
!$OMP DO PRIVATE(ix,gx,gxb,gy,wrk1,wrk2,k,k0,j,i,ip,tem,cz,j0,j1,i0,is)
  DO ix = 2,nxm1

    CALL coeff( grid,ix,gx,gxb,gy )

!-----  Non-Cartesian pressure terms for iteration

!-----  WRK1=d/dz(gx*p|xz)

    DO k = 1,nzm1
      k0 = (k-1)*nxyb
      DO j = 2,nym1
        i = (k-1)*nyb + j
        ip = (j-1)*nxb + ix + k0
        tem = p(ip) + p(ip+1) + p(ip+nxyb) + p(ip+1+nxyb)
        wrk2(i) = 0.25*gx(i)*tem
      END DO
    END DO

    DO k = 2,nzm1
      cz = dzi(k)
      DO j = 2,nym1
        i = (k-1)*nyb + j
        wrk4(i,ix) = (wrk2(i) - wrk2(i-nyb)) * cz
      END DO
    END DO
!----- b.c.'s
    j0 = nzm1*nyb
    j1 = nzm2*nyb
    DO j = 2,nym1
      wrk4(j,ix)    = 0.0
      wrk4(j+j0,ix) = wrk4(j+j1,ix)
    END DO

!-----  d/dx( p*d )

    DO k = 1,nzt
      k0 = (k-1)*nxyb
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        is = (j-1)*nxb + ix
        ip = is + k0
        wrk1(i) = dxi*(p(ip+1)*D(is+1)-p(ip)*D(is))
      END DO
    END DO

!-----  gx*(d/dx( p*d ))|z

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        wrk3(i,ix) = 0.5*gx(i)*(wrk1(i) + wrk1(i+nyb))
      END DO
    END DO
!----- b.c.'s
    DO j = 2,nym1
      wrk3(j,ix) = 0.0
    END DO

  END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Loop again

!$OMP PARALLEL SHARED(nxm1,nzm1,nym1,nxb,nyb,grid,dzi,dxi,dyi,dxxi,dyyi,wrk3,wrk4,p,D,psrce,pm)
!$OMP DO PRIVATE(ix,gx,gxb,gy,wrk,wrk1,wrk2,k,k0,j,i,ip,tem,cz,j0,j1,i0,is)
  DO ix = 2,nxm1

    CALL coeff( grid,ix,gx,gxb,gy )

!-----  Non-Cartesian pressure terms for iteration

!-----  Add d/dx( d/dz(gx*p|xz) ) * d

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        is = (j-1)*nxb + ix
        wrk(i) = dxi*(wrk4(i,ix)-wrk4(i,ix-1))*D(is)
      END DO
    END DO

!-----  WRK2 = (gx* (d/dz(gx*p|xz))|z )|x

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        wrk2(i) = 0.25*(gx(i) *(wrk4(i,ix)+wrk4(i+nyb,ix)) &
                + gxb(i)*(wrk4(i,ix-1)+wrk4(i+nyb,ix-1)))
      END DO
    END DO
    DO j = 2,nym1
      wrk2(j)    = 0.0
    END DO

!-----  Add d/dz( (gx* (d/dz(gx*p|xz))|z )|x)

    DO k = 2,nzm1
      cz = dzi(k)
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        wrk(i) = wrk(i) + (wrk2(i)-wrk2(i-nyb))*cz
      END DO
    END DO

!-----  WRK1=d/dz(gy*p|yz)

    DO k = 1,nzm1
      k0 = (k-1)*nxyb
      DO j = 1,nym1
        i = (k-1)*nyb + j
        ip = (j-1)*nxb + ix + k0
        tem = p(ip) + p(ip+nxb) + p(ip+nxyb) + p(ip+nxb+nxyb)
        wrk2(i) = 0.25*gy(i)*tem
      END DO
    END DO

    DO k = 2,nzm1
      cz = dzi(k)
      DO j = 1,nym1
        i = (k-1)*nyb + j
        wrk1(i) = (wrk2(i) - wrk2(i-nyb)) * cz
      END DO
    END DO
!----- b.c.'s
    j0 = nzm1*nyb
    j1 = nzm2*nyb
    DO j = 1,nym1
      wrk1(j)    = 0.0
      wrk1(j+j0) = wrk1(j+j1)
    END DO

!-----  Add d/dy( d/dz(gy*p|yz) ) * d

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        is = (j-1)*nxb + ix
        wrk(i) = wrk(i) + dyi*(wrk1(i)-wrk1(i-1))*D(is)
      END DO
    END DO

!-----  WRK2 = (gy* (d/dz(gy*p|yz))|z )|y)

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        wrk2(i) = 0.25*(gy(i) *(wrk1(i)+wrk1(i+nyb)) &
                + gy(i-1)*(wrk1(i-1)+wrk1(i+nyb-1)))
      END DO
    END DO
    DO j = 2,nym1
      wrk2(j) = 0.0
    END DO

!-----  Add d/dz( (gy* (d/dz(gy*p|yz))|z )|y)

    DO k = 2,nzm1
      cz = dzi(k)
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i = i0 + j
        wrk(i) = wrk(i) + (wrk2(i)-wrk2(i-nyb))*cz
      END DO
    END DO

!-----  ( gx*(d/dx( p*d ))|z )|x

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        wrk1(i) = 0.5*(wrk3(i,ix)+wrk3(i,ix-1))
      END DO
    END DO
    DO j = 2,nym1
      wrk1(j) = 0.0
    END DO

!-----  Add d/dz( ( gx*(d/dx( p*d ))|z )|x )

    DO k = 2,nzm1
      cz = dzi(k)
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        wrk(i) = wrk(i) + cz*(wrk1(i)-wrk1(i-nyb))
      END DO
    END DO

!-----  d/dy( p*d )

    DO k = 1,nzt
      k0 = (k-1)*nxyb
      i0 = (k-1)*nyb
      DO j = 1,nym1
        i  = i0 + j
        is = (j-1)*nxb + ix
        ip = is + k0
        wrk1(i) = dyi*(p(ip+nxb)*D(is+nxb)-p(ip)*D(is))
      END DO
    END DO

!-----  ( gy*(d/dy( p*d ))|z )|y

    DO k = 2,nzm1
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        wrk2(i) = 0.25*(gy(i  )*(wrk1(i)   + wrk1(i+nyb)) &
                + gy(i-1)*(wrk1(i-1) + wrk1(i+nyb-1)) )
      END DO
    END DO
    DO j = 2,nym1
      wrk2(j) = 0.0
    END DO

!-----  Add d/dz( ( gy*(d/dy( p*d ))|z )|y )

    DO k = 2,nzm1
      cz = dzi(k)
      i0 = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        wrk(i) = wrk(i) + cz*(wrk2(i)-wrk2(i-nyb))
      END DO
    END DO

!----- Add the explicit PSRCE & multiply by D

    DO k = 2,nzm1
      k0 = (k-1)*nxyb
      i0  = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        is = (j-1)*nxb + ix
        ip = k0 + is
        wrk(i) = ( psrce(ip)*D(is) - wrk(i) ) * D(is)
      END DO
    END DO

!----- Horizontal Cartesian divergence term - NB use pm

    DO k = 2,nzm1
      k0 = (k-1)*nxyb
      i0  = (k-1)*nyb
      DO j = 2,nym1
        i  = i0 + j
        is = (j-1)*nxb + ix
        ip = is + k0
        tem = dxxi*(pm(ip+1)*D(is+1)-2.*pm(ip)*D(is) &
                            +pm(ip-1)*D(is-1)) &
            + dyyi*(pm(ip+nxb)*D(is+nxb)-2.*pm(ip)*D(is) &
                            +pm(ip-nxb)*D(is-nxb))
        p(ip) = wrk(i) + (1.0-D(is)**2)*tem
      END DO
    END DO

  END DO
!$OMP END DO
!$OMP END PARALLEL

!----- Solve for new pressure

  irv = PostCheckMessage( )

  irv = pressureFFT( grid,p )
  IF( irv /= SWIMsuccess )GOTO 9999

!----- Check for convergence

  pmax  = 0.
  dpmax = 0.
  DO k = 1,nzt
    k0 = (k-1)*nxyb
    DO i = 1,nxyb
      p(k0+i) = p(k0+i) / D(i)
      p(k0+i) = alp*p(k0+i) + (1.-alp)*pm(k0+i)
      pmax  = MAX(pmax,ABS(p(k0+i)))
      dpmax = MAX(dpmax,ABS(p(k0+i)-pm(k0+i)))
    END DO
  END DO

  WRITE(message%cString,'(I5,A,1PE10.2)') iter, &
                ' iterations: residual = ',dpmax/MAX(pmax,TINY(pmax))

  irv = PostProgressMessage( message )

!------ Termination criteria

  irv = PostCheckMessage( )

  IF( pmax < TINY(pmax)   )EXIT
  IF( dpmax/pmax <= Prj%MC%epsFFT )EXIT
  IF( StopMode == SWIM_HALT )THEN
    StopMode = SWIM_ENABLE
    EXIT
  END IF
  IF( SWIMresult /= SWIMsuccess )EXIT

END DO Iteration

SWIMiteration = .FALSE.
CALL enableSWIMhalt( SWIM_HALT ) !Show abort button only

IterFFT = SWIMresult

9999 CONTINUE

IF( ALLOCATED(gx)   )DEALLOCATE( gx,STAT=ios )
IF( ALLOCATED(gxb)  )DEALLOCATE( gxb,STAT=ios )
IF( ALLOCATED(gy)   )DEALLOCATE( gy,STAT=ios )
IF( ALLOCATED(wrk)  )DEALLOCATE( wrk,STAT=ios )
IF( ALLOCATED(wrk1) )DEALLOCATE( wrk1,STAT=ios )
IF( ALLOCATED(wrk2) )DEALLOCATE( wrk2,STAT=ios )
IF( ALLOCATED(wrk3) )DEALLOCATE( wrk3,STAT=ios )
IF( ALLOCATED(wrk4) )DEALLOCATE( wrk4,STAT=ios )

RETURN
END
