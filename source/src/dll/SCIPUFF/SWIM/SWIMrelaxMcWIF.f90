INTEGER FUNCTION RelaxMcWIF( grid,u,v,w,p,alpha,csq,DivScale,iMG )

USE SWIM_fi
USE SWIMparam_fd
USE McWIFsubIntrf

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN ) :: grid
REAL, DIMENSION(:),   POINTER :: u, v, w, p, alpha, csq
REAL,            INTENT( IN ) :: DivScale
INTEGER,         INTENT( IN ) :: iMG

INTEGER iter, irv
INTEGER i, n, ios
REAL    divmax

CHARACTER(128) string

REAL, DIMENSION(:), POINTER :: pSum

CHARACTER(128), EXTERNAL :: ArraySizeStr

INTEGER, EXTERNAL :: SWIMaddLogMessage
INTEGER, EXTERNAL :: PostProgressMessage
INTEGER, EXTERNAL :: PostCheckMessage

ResetButtons = .TRUE.
WRITE(message%bString,'(A,I2)') 'Point relaxation method: Multi-grid ',iMG
message%cString = ''
irv = PostProgressMessage( message )

RelaxMcWIF = SWIMfailure

iter = 0

WRITE(string,FMT='(A,I3,A,I5)',IOSTAT=ios) 'Point relaxation: Multi-grid ',iMG
irv = SWIMaddLogMessage( TRIM(string) )
IF( irv /= SWIMsuccess )GOTO 9999

CALL enableSWIMhalt( StopMode )
SWIMiteration = .TRUE.

divmax = NOT_SET_R

n = grid%nXY*(grid%nZ+1)

ALLOCATE( pSum(n),STAT=irv )
IF( irv /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'RelaxMcWIF'
  error%Message = 'Error allocating 3d pressure summation array'
  error%Inform  =  ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF

!------ Advance velocities with initial pressure

CALL StepU( grid,u,v,w,p,alpha )
CALL SetBC( grid,u,v,w )

DO i = 1,n
  psum(i) = p(i)
  p(i)    = 0.
END DO

DO

  irv = PostCheckMessage( )

  iter = iter + 1
  IF( iter > Prj%MC%MaxIterRelax )THEN
    WRITE(string,FMT='(A,I5)',IOSTAT=ios) 'Number of iterations exceeded ',Prj%MC%MaxIterRelax
    irv = SWIMaddLogMessage( TRIM(string) ); IF( irv /= SWIMsuccess )GOTO 9999
    WRITE(string,FMT='(A,2ES12.4)',IOSTAT=ios) 'Max Divergence =',divmax,divmax/DivScale
    irv = SWIMaddLogMessage( TRIM(string) ); IF( irv /= SWIMsuccess )GOTO 9999
    EXIT
  END IF

!------ Advance pressure

  CALL StepRelaxP( grid,u,v,w,p,alpha,csq,divmax )

  irv = PostCheckMessage( )

!------ Advance velocities and set bc's

  CALL StepU( grid,u,v,w,p,alpha )
  CALL SetBC( grid,u,v,w )

  DO i = 1,n
    pSum(i) = pSum(i) + p(i)
  END DO

  irv = PostCheckMessage( )

  WRITE(message%cString,'(I5,A,1PE10.2)') iter, &
                ' iterations: residual = ',divmax/DivScale

  irv = PostProgressMessage( message )

  IF( divmax <= Prj%MC%epsRelax*DivScale )THEN
    WRITE(string,FMT='(A,I5)',IOSTAT=ios) 'Converged: number of iterations =',iter
    irv = SWIMaddLogMessage( TRIM(string) ); IF( irv /= SWIMsuccess )GOTO 9999
    WRITE(string,FMT='(A,2ES12.4)',IOSTAT=ios) 'Max Divergence =',divmax,divmax/DivScale
    irv = SWIMaddLogMessage( TRIM(string) ); IF( irv /= SWIMsuccess )GOTO 9999
    EXIT
  END IF
  IF( StopMode == SWIM_HALT )THEN
    StopMode = SWIM_ENABLE
    EXIT
  END IF
  IF( SWIMresult /= SWIMsuccess )EXIT

END DO

DO i = 1,n
  p(i) = pSum(i)
END DO

SWIMiteration = .FALSE.
CALL enableSWIMhalt( SWIM_HALT ) !Show abort button only

RelaxMcWIF = SWIMresult

9999 CONTINUE

IF( ASSOCIATED(pSum) )DEALLOCATE( pSum,STAT=irv )

RETURN
END

!==============================================================================

SUBROUTINE StepRelaxP( grid,u,v,w,p,alpha,csq,divmax )

USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ),    INTENT( IN  ) :: grid
REAL, DIMENSION(:), POINTER       :: u, v, w, p, alpha, csq
REAL,               INTENT( OUT ) :: divmax

REAL, PARAMETER :: DAMP = 0.15

INTEGER i, j, i0, ix, is, k, ip, k0
INTEGER nxb, nyb, nxm1, nym1, nzm1, nzt, nxyb
REAL    wwtem, wwnew, cz, temu, temv, div, fac
REAL    dxi, dyi, zbtop

REAL, DIMENSION(grid%nZ+1) :: wrk, wrk1, wrk2, wrk3, wrk4

REAL, DIMENSION(:), POINTER :: dzi, dzti, z
REAL, DIMENSION(:), POINTER :: Hx, Hy, D, Du, Dv

!------ Define locals

nxb  = grid%nX
nyb  = grid%nY
nxyb = nxb*nyb
nxm1 = nxb-1
nym1 = nyb-1
nzm1 = grid%nZ
nzt  = nzm1+1

dxi = grid%McWIF%dxi
dyi = grid%McWIF%dyi

zbtop = grid%Ztop

dzi  => grid%McWIF%dzi
dzti => grid%McWIF%dzti
z    => grid%McWIF%z

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy
D  => grid%terrain%D
Du => grid%terrain%Du
Dv => grid%terrain%Dv

!------ Loop horizontally, compute divergence and pressure on vertical column

divmax = 0.

!$OMP PARALLEL SHARED(nxm1,nym1,nzm1,nxb,nxyb,nzt,dzi,dzti,zbtop,Hx,Hy,u,v,w,z,p,csq,D,Du,Dv,dxi,dyi,alpha)
!$OMP DO PRIVATE(j,i0,ix,is,k,ip,cz,temu,temv,wwnew,wwtem,div,wrk,wrk1,wrk2,wrk3,wrk4,fac) REDUCTION(MAX:divmax)
DO j = 2,nym1
  i0 = (j-1)*nxb

  DO ix = 2,nxm1
    is = i0 + ix

    wwtem = 0.0

!------ Compute divergence

    DO k = 2,nzm1
      ip = (k-1)*nxyb + is
      cz  = dzi(k)

      temu  = Hx(is    )*(u(ip    ) + u(ip+nxyb    )) &
            + Hx(is-1  )*(u(ip-1  ) + u(ip-1+nxyb  ))
      temv  = Hy(is    )*(v(ip    ) + v(ip+nxyb    )) &
            + Hy(is-nxb)*(v(ip-nxb) + v(ip-nxb+nxyb))

      wwnew = w(ip) - 0.25*(zbtop-z(k))*(temu+temv)/zbtop

      div = (wwnew-wwtem)*cz &
          + (Du(is)*u(ip)-Du(is-1  )*u(ip-1  ))*dxi &
          + (Dv(is)*v(ip)-Dv(is-nxb)*v(ip-nxb))*dyi

      divmax = MAX(divmax,ABS(div))

      wrk4(k) = p(ip) - csq(ip)*div
      wwtem   = wwnew

    END DO

!------ set coefficients for tri-diagonal system

    DO k = 2,nzm1
      ip = (k-1)*nxyb + is
      fac     = csq(ip)*dzi(k)/d(is)
      wrk1(k) = -fac*alpha(ip-nxyb)*dzti(k)
      wrk3(k) = -fac*alpha(ip     )*dzti(k+1)
      wrk2(k) = 1. + DAMP - wrk1(k) - wrk3(k)
    END DO

!------ Zero slope bc's

    wrk2(1)   =  1.
    wrk3(1)   = -1.
    wrk4(1)   =  0.
    wrk1(nzt) = -1.
    wrk2(nzt) =  1.
    wrk4(nzt) =  0.

!------ Invert to solve for pressure

    CALL matinv( wrk1,wrk2,wrk3,wrk4,wrk,nzt )

    DO k = 2,nzm1
      p((k-1)*nxyb+is) = wrk(k)
    END DO

  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

!------ BC's

!$OMP PARALLEL SHARED(nzm1,nxyb,nyb,nxb,nym1,p)
!$OMP DO PRIVATE(k,k0,j,i0,i,ip)
DO k = 2,nzm1
  k0 = (k-1)*nxyb

  DO j = 1,nyb
    i0 = (j-1)*nxb + k0
    p(i0+1)   = -p(i0+2)
    p(i0+nxb) = -p(i0+nxm1)
  END DO

  i0 = nym1*nxb
  DO i = 1,nxb
    ip = i + k0
    p(ip)    = -p(ip+nxb)
    p(i0+ip) = -p(i0+ip-nxb)
  END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

k0 = (nzt-1)*nxyb
!$OMP PARALLEL SHARED(k0,nxyb,p)
!$OMP DO PRIVATE(i)
DO i = 1,nxyb
  p(i)    = p(i+nxyb)
  p(k0+i) = p(k0+i-nxyb)
END DO
!$OMP END DO
!$OMP END PARALLEL

RETURN
END
