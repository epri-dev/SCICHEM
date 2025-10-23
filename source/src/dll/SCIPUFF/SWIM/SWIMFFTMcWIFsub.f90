!==============================================================================
!
!  SWIM McWIF FFT subroutine module
!
!==============================================================================

MODULE FFTmod

USE SWIM_fi

IMPLICIT NONE

SAVE

INTEGER nxb, nyb, nxyb, nzt
INTEGER nxm1, nym1, nzm1, nxm2, nym2

REAL, DIMENSION(:), POINTER :: cza, czb, xeval, yeval
REAL, DIMENSION(:), POINTER :: xcs, ycs, xsn, ysn

REAL, DIMENSION(:), ALLOCATABLE :: xevec, yevec

CONTAINS

!==============================================================================

INTEGER FUNCTION pressureFFT( grid,p )

!----- Solves Poisson equation for pressure

USE SWIMparam_fd

TYPE( MetGrid ),    INTENT( IN ) :: grid
REAL, DIMENSION(:), POINTER      :: p

INTEGER i, j, k, i0, i1, i2, k0, ip, alloc_stat

REAL, DIMENSION(:), ALLOCATABLE :: atem, r1, q1, czab
REAL, DIMENSION(:,:), ALLOCATABLE :: wrk1, wrk2

pressureFFT = SWIMfailure

!------ Define locals

cza   => grid%McWIF%cza
czb   => grid%McWIF%czb
xeval => grid%McWIF%xeval
yeval => grid%McWIF%yeval
xcs   => grid%McWIF%xcs
ycs   => grid%McWIF%ycs
xsn   => grid%McWIF%xsn
ysn   => grid%McWIF%ysn

ALLOCATE( atem(nzt),r1(nzt),q1(nzt),czab(nzt),STAT=alloc_stat )

i = 2*MAX(nxm2,nym2)
IF( alloc_stat == 0 )ALLOCATE( wrk1(i,nzt), wrk2(i,nzt),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( xevec(SIZE(grid%McWIF%xevec)),STAT=alloc_stat )
IF( alloc_stat == 0 )ALLOCATE( yevec(SIZE(grid%McWIF%yevec)),STAT=alloc_stat )

IF( alloc_stat /= 0 )THEN
  error%Number =  UK_ERROR
  error%Routine = 'pressureFFT'
  error%Message = 'Error allocating work arrays'
  GOTO 9999
END IF

xevec = grid%McWIF%xevec
yevec = grid%McWIF%yevec

czab = cza + czb

!------ Transverse analysis

!$OMP PARALLEL SHARED(nzm1,nxyb,p,wrk1,wrk2)
!$OMP DO PRIVATE(k,k0)
DO k = 2,nzm1
  k0 = (k-1)*nxyb
  CALL tanal( k0,p,wrk1(1,k),wrk2(1,k) )
END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Solve z-direction

!$OMP PARALLEL SHARED(nxm1,nym1,nzm1,nxb,nxyb,xeval,yeval,czab,cza,czb,p)
!$OMP DO PRIVATE(i,j,k,i1,ip,r1,q1,atem)
DO i = 2,nxm1

!-------- Select coefficients

  DO k = 1,nzm1
    r1(k) = xeval(i) - czab(k)
  END DO

  DO j = 2,nym1
    i1=(j-1)*nxb+i

!-------- Select elements

    DO k = 1,nzm1
      ip = (k-1)*nxyb + i1
      q1(k) = yeval(j) + r1(k)
      atem(k) = p(ip)
    END DO

    CALL sweep( atem,cza,czb,q1 )

!-------- Replace in array

    DO k = 1,nzm1
      ip = (k-1)*nxyb + i1
      p(ip) = atem(k)
    END DO

  END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

!------ Transverse synthesis

i1 = nym2*nxb
i2 = nym1*nxb

!$OMP PARALLEL SHARED(nzm1,nxb,nyb,nxyb,p,wrk1,wrk2)
!$OMP DO PRIVATE(k,k0,j,i0)
DO k = 2,nzm1
  k0 = (k-1)*nxyb

  CALL tsyn( k0,p,wrk1(1,k),wrk2(1,k) )

  DO j = 1,nyb
    i0 = (j-1)*nxb + k0
    p(i0+1)   = -p(i0+2)
    p(i0+nxb) = -p(i0+nxm1)
  END DO

  DO i = 1,nxb
    p(i+k0)    = -p(i+nxb+k0)
    p(i+i2+k0) = -p(i+i1+k0)
  END DO

END DO
!$OMP END DO
!$OMP END PARALLEL

k0 = (nzt-1)*nxyb
DO i = 1,nxyb
  p(i)    = p(i+nxyb)
  p(k0+i) = p(k0-nxyb+i)
END DO

pressureFFT = SWIMresult

9999 CONTINUE

IF( ALLOCATED(atem)  )DEALLOCATE( atem,STAT=alloc_stat )
IF( ALLOCATED(r1)    )DEALLOCATE( r1,STAT=alloc_stat )
IF( ALLOCATED(q1)    )DEALLOCATE( q1,STAT=alloc_stat )
IF( ALLOCATED(czab)  )DEALLOCATE( czab,STAT=alloc_stat )
IF( ALLOCATED(wrk1)  )DEALLOCATE( wrk1,STAT=alloc_stat )
IF( ALLOCATED(wrk2)  )DEALLOCATE( wrk2,STAT=alloc_stat )
IF( ALLOCATED(xevec) )DEALLOCATE( xevec,STAT=alloc_stat )
IF( ALLOCATED(yevec) )DEALLOCATE( yevec,STAT=alloc_stat )

RETURN
END FUNCTION pressureFFT

!==============================================================================

RECURSIVE SUBROUTINE tanal( k0,stem,ctem,atem )

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: k0
REAL, DIMENSION(:), POINTER         :: stem
REAL, DIMENSION(*), INTENT( INOUT ) :: atem, ctem

INTEGER nper, nper2, i, j, i0, ix

!------ x-direction

nper  = 2*nxm2
nper2 = nxm2

DO j = 2,nym1
  i0 = (j-1)*nxb + 1 + k0

  DO i = 1,nper2
    ctem(i)        =  stem(i0+i)
    ctem(nper-i+1) = -ctem(i)
  END DO

  CALL rfftf( nper,ctem,atem,xevec )

  DO i = 1,nper2-1
    stem(i0+i) = ctem(2*i+1)/xcs(i+1)
  END DO
  stem(i0+nper2) = ctem(nper)

END DO

!------ y-direction

nper  = 2*nym2
nper2 = nym2

DO ix = 2,nxm1

  DO j = 1,nper2
    i = j*nxb + ix + k0
    ctem(j)        =  stem(i)
    ctem(nper-j+1) = -ctem(j)
  END DO

  CALL rfftf( nper,ctem,atem,yevec )

  DO j = 1,nper2-1
    i = j*nxb + ix + k0
    stem(i) = ctem(2*j+1)/ycs(j+1)
  END DO
  stem(i+nxb) = ctem(nper)

END DO

RETURN
END SUBROUTINE tanal

!============================================================================

RECURSIVE SUBROUTINE tsyn( k0,stem,ctem,atem )

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: k0
REAL, DIMENSION(:), POINTER         :: stem
REAL, DIMENSION(*), INTENT( INOUT ) :: atem, ctem

INTEGER nper, nper2, i, j, i0
REAL    fac

!------ Normalization factor

fac  = 1./FLOAT(4*nxm2*nym2)

!------ y-direction

nper  = 2*nym2
nper2 = nym2

DO i = 2,nxm1

  DO j = 2,nper2
    i0 = (j-1)*nxb + k0
    ctem(2*j-2) = -stem(i0+i)*ysn(j)
    ctem(2*j-1) =  stem(i0+i)*ycs(j)
  END DO
  ctem(1)    = 0.
  ctem(nper) = stem(i0+nxb+i)

  CALL rfftb( nper,ctem,atem,yevec )

  DO j = 1,nper2
    i0 = j*nxb + k0
    stem(i0+i) = ctem(j)
  END DO

END DO

!------ x-direction

nper  = 2*nxm2
nper2 = nxm2

DO j = 2,nym1
  i0 = (j-1)*nxb + k0

  DO i = 2,nper2
    ctem(2*i-2) = -stem(i0+i)*xsn(i)
    ctem(2*i-1) =  stem(i0+i)*xcs(i)
  END DO
  ctem(1)    = 0.
  ctem(nper) = stem(i0+nxm1)

  CALL rfftb( nper,ctem,atem,xevec )

  DO i = 1,nper2
    stem(i0+i+1) = ctem(i)*fac
  END DO

END DO

RETURN
END SUBROUTINE tsyn

!=============================================================================

RECURSIVE SUBROUTINE sweep( dtem,ctem,atem,btem )

IMPLICIT NONE

REAL, DIMENSION(nzt), INTENT( IN    ) :: atem, btem, ctem
REAL, DIMENSION(nzt), INTENT( INOUT ) :: dtem

INTEGER i, k
REAL    dena

REAL, DIMENSION(nzt) :: alp

dena    = 1./(btem(2)+ctem(2))
alp(2)  = dena*atem(2)
dtem(2) = dtem(2)*dena

DO k = 3,nzm1
  dena    = 1./(btem(k)-ctem(k)*alp(k-1))
  alp(k)  = dena*atem(k)
  dtem(k) = (dtem(k)-ctem(k)*dtem(k-1))*dena
END DO

dtem(nzm1) = dtem(nzm1)/(1.+alp(nzm1))

DO k = 2,nzm1-1
  i = nzt - k
  dtem(i) = dtem(i)-alp(i)*dtem(i+1)
END DO

RETURN
END SUBROUTINE sweep

END MODULE FFTmod
