INTEGER FUNCTION SWIMMcWIF( grid,meanField )

USE SWIM_fi
USE SWIMparam_fd
USE McWIFintrf

IMPLICIT NONE

TYPE( MetGrid   ), TARGET, INTENT( INOUT ) :: grid
TYPE( MetMean3D ),         INTENT( INOUT ) :: meanField

INTEGER alloc_stat, n, irv
INTEGER MCWIFmethod, nMG, iMG, i
LOGICAL lTpot
REAL    DivScale0
REAL    DivScale

REAL, DIMENSION(:), POINTER :: p, alpha, csq
REAL, DIMENSION(:), POINTER :: u, v, w, t
REAL, DIMENSION(:), POINTER :: u1, v1, w1, t1, p1
REAL, DIMENSION(:), POINTER :: alpha0,csq0, ptem, p0

TYPE( MetMean3D ), DIMENSION(:), ALLOCATABLE :: MGfield

TYPE( MetMean3D ):: meanFieldSave

TYPE( MetGrid ), POINTER :: MGgrid

INTEGER, EXTERNAL :: PostProgressMessage, SWIMaddLogMessage

CHARACTER(128), EXTERNAL :: ArraySizeStr

SWIMMcWIF = SWIMfailure

message%bString  = 'Generating mass-consistent winds'
message%cString  = ' '

irv = PostProgressMessage( message )

!------ Allocate work space

n = grid%nXY * (grid%nZ+1)

ALLOCATE( ptem(n),alpha0(n),csq0(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'SWIMMcWIF'
  error%Message = 'Error allocating mass-consistent work arrays'
  error%Inform  =  ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF
DO i = 1,n
  ptem(i) = 0.
END DO

!------ Compute alpha and determine appropriate method required

MCWIFmethod = SetMcWIF( grid,meanField%U,meanField%V,meanField%W,meanField%Tpot,ptem, &
                        alpha0,csq0,DivScale0 )
IF( error%Number /= NO_ERROR )GOTO 9999

SELECT CASE( MCWIFmethod )

  CASE( MCWIF_RELAX )

    irv = SWIMaddLogMessage( 'Using point relaxation wind field adjustment' )
    IF( irv /= SWIMsuccess )GOTO 9999

    nMG = grid%numMG

  CASE( MCWIF_FFT )

    irv = SWIMaddLogMessage( 'Using FFT wind field adjustment' )
    IF( irv /= SWIMsuccess )GOTO 9999

    nMG = 0 !Multi-grid not used with FFT

  CASE( MCWIF_WZERO )  !Grid cells too anisotropic for stable calculation. Set W=0

    message%bString  = 'Grid too anisotropic for stable calculation. W set to zero'
    message%cString  = 'No horizontal wind field adjustment'

    irv = PostProgressMessage( message )

    irv = SWIMaddLogMessage( message%bString )
    IF( irv /= SWIMsuccess )GOTO 9999
    irv = SWIMaddLogMessage( message%cString )
    IF( irv /= SWIMsuccess )GOTO 9999

    n = grid%nXY*(grid%nZ+1)
    DO i = 1,n
      meanField%W(i) = 0.
    END DO

    SWIMMcWIF = SWIMresult
    GOTO 9999

  CASE( MCWIF_NOADJ )  !No adjustment: zero velocity or max iterations is zero

    IF( DivScale0 == NOT_SET_R )THEN
      message%bString  = 'Max number of iterations set to zero'
    ELSE
      message%bString  = 'Zero winds'
    END IF
    message%cString  = 'No wind field adjustment required'

    irv = PostProgressMessage( message )

    irv = SWIMaddLogMessage( message%bString ); IF( irv /= SWIMsuccess )GOTO 9999
    irv = SWIMaddLogMessage( message%cString ); IF( irv /= SWIMsuccess )GOTO 9999

    SWIMMcWIF = SWIMresult
    GOTO 9999

END SELECT

!------ Allocate MG fields (if needed)

MGgrid => grid

IF( nMG > 0 )THEN

  ALLOCATE( MGfield(nMG),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    error%Number  = UK_ERROR
    error%Routine = 'SWIMMcWIF'
    error%Message = 'Error allocating multi-grid mean field structures'
    error%Inform  =  ArraySizeStr( 1,(/nMG/) )
    GOTO 9999
  END IF

  lTpot = ASSOCIATED( meanField%Tpot )

  CALL nullifyMetMean3D( meanFieldSave )
  n = grid%nXY*(grid%nZ+1)
  irv = allocateMGfield( n,lTpot,meanFieldSave )
  IF( irv /= SWIMsuccess )GOTO 9999

  DO iMG = 1,nMG

    CALL nullifyMetMean3D( MGfield(iMG) )

    n = MGgrid%nXY*(MGgrid%nZ+1)
    irv = allocateMGfield( n,lTpot,MGfield(iMG) )
    IF( irv /= SWIMsuccess )GOTO 9999

    MGgrid => MGgrid%MGgrid  !Point to next MG

  END DO

  MGgrid => grid  !Point back to finest grid

!------ Setup multi-grid fields by smoothing onto coarser grids

  n = grid%nXY*(grid%nZ+1)

!------ Smooth field u1 from u, etc.

  DO iMG = 1,nMG

    MGgrid => MGgrid%MGgrid

    IF( iMG == 1 )THEN
      u => meanField%U
      v => meanField%V
      w => meanField%W
      t => meanField%Tpot
    ELSE
      u => MGfield(iMG-1)%U
      v => MGfield(iMG-1)%V
      w => MGfield(iMG-1)%W    ; !ww => ww1
      t => MGfield(iMG-1)%Tpot
    END IF

    u1 => MGfield(iMG)%U
    v1 => MGfield(iMG)%V
    w1 => MGfield(iMG)%W
    t1 => MGfield(iMG)%Tpot

    n = MGgrid%nXY*(MGgrid%nZ+1)

    CALL McWIFsmooth3D( MGgrid,u,u1,'U' )
    CALL McWIFsmooth3D( MGgrid,v,v1,'V' )
    CALL McWIFsmooth3D( MGgrid,w,w1 )

    IF( ltpot )CALL McWIFsmooth3D( MGgrid,t,t1 )

!------ Smooth spatially-varying weights (for observations)

    CALL McWIFsmooth3D( MGgrid,MGgrid%prevMGgrid%McWif%alphaU,MGgrid%McWif%alphaU,'U' )
    CALL McWIFsmooth3D( MGgrid,MGgrid%prevMGgrid%McWif%alphaV,MGgrid%McWif%alphaV,'V' )

!------- Initialize pressure to zero

    n = MGgrid%nXY*(MGgrid%nZ+1)

    DO i = 1,n
      MGfield(iMG)%Press(i) = 0.
    END DO

  END DO

END IF

!------ Loop over multi-grids, starting with coarsest grid
!       N.B. No multi-grid correponds to one loop with iMG=0

MG_Loop : DO iMG = nMG,0,-1

!------ Assign local pointers

  IF( iMG == 0 )THEN

    u => meanField%U
    v => meanField%V
    w => meanField%W
    t => meanField%Tpot
    p => ptem

    alpha => alpha0      !From initial call to SetMcWif
    csq   => csq0
    DivScale = DivScale0

  ELSE

    u => MGfield(iMG)%U
    v => MGfield(iMG)%V
    w => MGfield(iMG)%W
    t => MGfield(iMG)%Tpot
    p => MGfield(iMG)%Press

    n = MGgrid%nXY*(MGgrid%nZ+1)
    ALLOCATE( alpha(n),csq(n),p0(n),STAT=alloc_stat )
    IF( alloc_stat /= 0 )THEN
      error%Number  = UK_ERROR
      error%Routine = 'SWIMMcWIF'
      error%Message = 'Error allocating mass-consistent arrays'
      error%Inform  =  ArraySizeStr( 1,(/n/) )
      GOTO 9999
    END IF

!------ Compute parameters for point-relaxation
!       N.B. assumes MGgrid is pointing to coarsest grid (from previous iMG loop)

    irv = SetMcWIF( MGgrid,u,v,w,t,p,alpha,csq,DivScale,MCWIFmethod )
    IF( error%Number /= NO_ERROR )GOTO 9999

    IF( iMG == nMG )THEN
      DEALLOCATE( p0,STAT=alloc_stat )  !Not used on coarsest grid
    ELSE
      DO i = 1,n
        p0(i) = p(i)                    !Save initial P for extrapolation
      END DO
    END IF

  END IF

!------ Use appropriate method to solve for P and divergence-free velocity field

  IF( MCWIFmethod == MCWIF_RELAX )THEN
    irv = RelaxMcWIF( MGgrid,u,v,w,p,alpha,csq,DivScale,iMG )
  ELSE
    irv = FFTMcWIF( MGgrid,u,v,w,p,alpha,csq)
  END IF
  IF( irv /= SWIMsuccess )GOTO 9999

!------ "Prolong" pressure field onto finer grid

  IF( iMG == 0 )THEN
    EXIT
  ELSE IF( iMG == 1 )THEN
    p1 => ptem                     !Finest grid
  ELSE
    p1 => MGfield(iMG-1)%Press     !Next finer grid
  END IF

  IF( iMG > 0 .AND. iMG < nMG )THEN
    DO i = 1,n
      p(i) = (4.*p(i) - p0(i))/3.  !Extrapolate
    END DO
    DEALLOCATE( p0,STAT=alloc_stat )
  END IF

  CALL McWIFunSmooth3D( MGgrid,p,p1 )

  DEALLOCATE( alpha,csq,STAT=alloc_stat )

  MGgrid => MGgrid%prevMGgrid      !Point to next (finer) grid

END DO MG_Loop

message%bString  = 'Completed wind field adjustment'
message%cString  = ' '

irv = PostProgressMessage( message )

!------ Another SWIMming success

SWIMMcWIF = SWIMresult

9999 CONTINUE

IF( ALLOCATED(MGfield) )THEN
  DO iMG = 1,nMG
    irv = deallocateMGfield( MGfield(iMG) )
  END DO
  DEALLOCATE( MGfield,STAT=alloc_stat )
END IF

IF( ASSOCIATED(ptem)   )DEALLOCATE( ptem,  STAT=alloc_stat )
IF( ASSOCIATED(alpha0) )DEALLOCATE( alpha0,STAT=alloc_stat )
IF( ASSOCIATED(csq0)   )DEALLOCATE( csq0,  STAT=alloc_stat )

RETURN
END

!===============================================================================

INTEGER FUNCTION SetMcWIF( grid,u,v,w,th,p,alpha,csq,DivScale,Method )

!------ Compute vertical velocity factor (alpha), setup artificial "sound speed" if required
!       or vertical differencing arrays for FFT method

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd
USE McWIFsubIntrf

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN  ) :: grid
REAL, DIMENSION(:),    POINTER :: u, v, w, th, p, alpha, csq
REAL,            INTENT( OUT ) :: DivScale
INTEGER, OPTIONAL, INTENT( IN ) :: Method

REAL, PARAMETER :: CSQ_FAC  = 0.9 !1.0 was original value. N.B. 1.1 is unstable
REAL, PARAMETER :: CMIN_FAC = 10.
REAL, PARAMETER :: FR_FAC   = 1.
REAL, PARAMETER :: FFT_LIM  = 1.4
REAL, PARAMETER :: DIV_FAC  = 0.1
REAL, PARAMETER :: AR2      = 1.E5

INTEGER, PARAMETER :: NMAX_FFT = 360 !Max 1d length for FFT (empirically determined)

CHARACTER(128) string

INTEGER ios, irv
INTEGER i, k, is, km, j, jm, im, i0, j0, ism, jsm, ip, k0
INTEGER nxb, nyb, nxyb, nzm1, nzt
REAL    dx2, czt, dum1, dum2, fac
REAL    bv, fr2, ubar, vbar, dtdz, zp
REAL    xfac, yfac, zzfac, cx2, cy2
REAL    alp_min, sumu, sumt, umax, vmax, csq0
LOGICAL tflag
LOGICAL lPointRelax

REAL, DIMENSION(grid%nZ+1) :: csqmin

REAL, DIMENSION(:), POINTER :: z, cza, czb, dzi, dzti, alphaFFT
REAL, DIMENSION(:), POINTER :: Hx, Hy, H, D
REAL, DIMENSION(:), POINTER :: alphaU, alphaV

INTEGER, EXTERNAL :: SWIMaddLogMessage
REAL,    EXTERNAL :: SWIMrlimit

!------ Define locals

tflag = ASSOCIATED( th )
nxb   = grid%nX
nyb   = grid%nY
nxyb  = grid%nXY
nzm1  = grid%nZ
nzt   = nzm1+1

z    => grid%McWIF%z
cza  => grid%McWIF%cza
czb  => grid%McWIF%czb
dzi  => grid%McWIF%dzi
dzti => grid%McWIF%dzti

alphaFFT => grid%McWIF%alphaFFT

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy
H  => grid%terrain%H
D  => grid%terrain%D
alphaU => grid%McWIF%alphaU
alphaV => grid%McWIF%alphaV

lPointRelax = (Prj%MC%MaxIterFFT == 0)

!------ Bottom BC

DO i = 1,nxyb
  u(i)     = u(i+nxyb)
  v(i)     = v(i+nxyb)
  alpha(i) = 0.
END DO

IF( tflag )THEN
  DO i = 1,nxyb
    th(i) = th(i+nxyb)
  END DO
END IF

!------ Check if no adjustment requested

IF( Prj%MC%MaxIterRelax == 0 .AND. Prj%MC%MaxIterFFT == 0 )THEN
  DivScale = NOT_SET_R
  SetMcWIF = MCWIF_NOADJ
  GOTO 9999
END IF

!------ Adjust velocities on boundaries for zero net flux

irv = ZeroNetFlux( grid,u,v )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Compute Froude no. parameter at each grid cell

dx2     = grid%McWIF%Hscale**2
alp_min = 1.E+20

umax = 0.
vmax = 0.

DO k = 2,nzm1
  k0 = (k-1)*nxyb
  czt = dzti(k+1)

  sumu = 0.
  sumt = 0.

  DO j = 2,nyb
    i0 = (j-1)*nxb

    DO i = 2,nxb
      is = i0 + i
      ip = is + k0

      IF( tflag )THEN
        dtdz = MAX((th(ip+nxyb)-th(ip))*czt,0.)
      ELSE
        zp = D(is)*z(k) + H(is) + grid%Hmin
        CALL stnd_atmos( zp,dum1,dum2,dtdz,SA_TA ) !dtdz always potential temp gradient
      END IF
      bv   = G0/300.*dtdz
      ubar = 0.25*(u(ip)+u(ip-1)+u(ip+nxyb)+u(ip-1+nxyb))
      vbar = 0.25*(v(ip)+v(ip-nxb)+v(ip+nxyb)+v(ip-nxb+nxyb))
      fr2  = bv*dx2/(ubar**2+vbar**2+1.E-6)
      alpha(ip) = 1./(1. + FR_FAC*fr2)

      alpha(ip) = SWIMrlimit( alpha(ip),grid%McWIF%alphaMin,grid%McWIF%alphaMax )

      sumu = sumu + ubar**2 + vbar**2
      sumt = sumt + dtdz

      umax = MAX(umax,ABS(u(ip)))
      vmax = MAX(vmax,ABS(v(ip)))

      IF( alphaU(ip) /= 1.0 .OR. alphaV(ip) /= 1.0 )lPointRelax = .TRUE.

    END DO
  END DO

  DO i = 1,nxb
    ip = k0 + i
    alpha(ip) = alpha(ip+nxb)
    ip = k0 + (nyb-1)*nxb + i
    alpha(ip) = alpha(ip-nxb)
  END DO
  DO i = 1,nyb
    ip = k0 + (i-1)*nxb + 1
    alpha(ip) = alpha(ip+1)
    ip = k0 + (i-1)*nxb + nxb
    alpha(ip) = alpha(ip-1)
  END DO

  fr2         = G0/300.*sumt*dx2/(sumu+1.E-6)
  alphaFFT(k) = 1./(1. + FR_FAC*fr2)
  alphaFFT(k) = SWIMrlimit( alphaFFT(k),grid%McWIF%alphaMin,grid%McWIF%alphaMax )
  alp_min     = MIN(alp_min,alphaFFT(k))

END DO

k0 = (nzt-1)*nxyb
DO is = 1,nxyb
  alpha(is)    = alpha(is+nxyb)
  alpha(k0+is) = alpha(k0+is-nxyb)
END DO

alphaFFT(1)      = alphaFFT(2)
alphaFFT(nzm1+1) = alphaFFT(nzm1)

WRITE(string,"('Max. slope, min. alpha : ',2ES12.4)",IOSTAT=ios) grid%McWIF%MaxSlope,alp_min
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Determine if artificial compressibility method is required:
!       FFT can be used if (max slope)/(min alpha) < FFT_LIM

IF( PRESENT(Method) )lPointRelax = lPointRelax .OR. (Method == MCWIF_RELAX)

lPointRelax = lPointRelax .OR. ((nxb > NMAX_FFT .OR. nyb > NMAX_FFT) .AND. grid%McWIF%MaxSlope >= 0.6*FFT_LIM*alp_min) !No FFT with large grids & significant slope

IF( lPointRelax .OR. grid%McWIF%MaxSlope > FFT_LIM*alp_min .OR. nxb < 4 .OR. nyb < 4 )THEN

!------ Determine "sound speed" based on grid length
!       (accounting for surface slope and aspect ratio)

  csq0   = 1.E+20
  csqmin = 1.E+20
  fac    = 0.

  jm = 1

  DO j = 1,nyb
    i0 = (j-1)*nxb
    j0 = (jm-1)*nxb
    im = 1

    DO i = 1,nxb
      is  = i0 + i
      ism = i0 + im
      jsm = j0 + i

      xfac = (MAX(ABS(Hx(is)),ABS(Hx(ism)))/grid%Ztop)**2
      yfac = (MAX(ABS(Hy(is)),ABS(Hy(jsm)))/grid%Ztop)**2

      DO k = 2,nzm1
        ip = (k-1)*nxyb + is
        zzfac     = ((grid%Ztop-z(k))*dzi(k))**2
        cx2       = MAX(grid%McWIF%dxi**2,xfac*zzfac)
        cy2       = MAX(grid%McWIF%dyi**2,yfac*zzfac)
        csq(ip)   = CSQ_FAC/(cx2+cy2)
        zzfac     = 0.25*(alphaU(ip)+alphaU(ism+(k-1)*nxyb)+alphaV(ip)+alphaV(jsm+(k-1)*nxyb))
        csq(ip)   = csq(ip) / zzfac
        csqmin(k) = MIN(csqmin(k),csq(ip))
      END DO

      im = i
    END DO

   jm = j
  END DO

  DO k = 2,nzm1
    k0 = (k-1)*nxyb
    csq0 = MIN(csq0,csqmin(k))
    DO is = 1,nxyb
      ip = k0 + is
      csq(ip) = MIN(csq(ip),CMIN_FAC*csqmin(k))
      fac     = MAX(fac,csq(ip)*alpha(ip)*dzi(k)**2/D(is))
    END DO
  END DO

  IF( fac >= AR2 )THEN

    SetMcWIF = MCWIF_WZERO !Grid is too anistropic for stable calculation

  ELSE

    SetMcWIF = MCWIF_RELAX !Set return value indicating relaxation method

  END IF

ELSE

!------ FFT will be used; setup vertical differencing arrays

  DO k = 2,nzt
    km = k - 1
    cza(k)  = alphaFFT(km)*dzi(k) *dzti(k)
    czb(km) = alphaFFT(km)*dzti(k)*dzi(km)
  END DO

  cza(1)   = 0.  !Avoid "undefined" issues
  czb(nzt) = 0.

  SetMcWIF = MCWIF_FFT !Set return value indicating FFT method

END IF

!---- Divergence scale for convergence criterion

DivScale = DIV_FAC*MAX(umax*grid%McWIF%dxi,vmax*grid%McWIF%dyi)

!------ No adjustment required for zero velocity

IF( DivScale == 0.0 )SetMcWIF = MCWIF_NOADJ

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE StepU( grid,u,v,w,p,alpha )

!------ Advance momentum equations using pressure gradients

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ),    INTENT( IN  ) :: grid
REAL, DIMENSION(:), POINTER       :: u, v, w, p, alpha

INTEGER ix, i, j, k, i0, is, ip, k0, alloc_stat
INTEGER nxb, nyb, nxyb, nxm1, nym1, nzm1, nzm2, nzt
REAL    dxi, dyi, tem, cz

REAL, DIMENSION(:), ALLOCATABLE :: gx, gxb, gy, wrk1, wrk2

REAL, DIMENSION(:), POINTER :: dzi, dzti
REAL, DIMENSION(:), POINTER :: Hx, Hy, D, Du, Dv
REAL, DIMENSION(:), POINTER :: alphaU, alphaV

!------ Define locals

nxb  = grid%nX
nyb  = grid%nY
nxyb = nxb*nyb
nxm1 = nxb-1
nym1 = nyb-1
nzm1 = grid%nZ
nzm2 = nzm1-1
nzt  = nzm1+1

dxi = grid%McWIF%dxi
dyi = grid%McWIF%dyi

dzi  => grid%McWIF%dzi
dzti => grid%McWIF%dzti

alphaU => grid%McWIF%alphaU
alphaV => grid%McWIF%alphaV

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy
D  => grid%terrain%D
Du => grid%terrain%Du
Dv => grid%terrain%Dv

ALLOCATE( gx(nyb*nzt),gxb(nyb*nzt),gy(nyb*nzt), &
          wrk1(nyb*nzt),wrk2(nyb*nzt),STAT=alloc_stat )

!------ Sweep along x-axis

!$OMP PARALLEL SHARED(nxm1,nym1,nzm1,nzm2,nxb,nyb,nxyb,nzt,grid,dzi,dzti,u,v,w,p,D,Du,Dv,dxi,dyi,alpha)
!$OMP DO PRIVATE(ix,gx,gxb,gy,k,k0,cz,j,is,ip,i0,i,tem,wrk1,wrk2)
Xsweep : DO ix = 1,nxm1

  CALL coeff( grid,ix,gx,gxb,gy )

!------ W-equation

  DO k = 2,nzm2
    k0 = (k-1)*nxyb
    cz = dzti(k+1)
    DO j = 2,nym1
      is = (j-1)*nxb + ix
      ip = k0 + is
      w(ip) = w(ip) + alpha(ip)*(p(ip)-p(ip+nxyb))*cz/D(is)
    END DO
  END DO

!------ wrk1 = gx * p|xz, wrk2 = gy * p|yz

  DO k = 1,nzm1
    i0 = (k-1)*nyb
    k0 = (k-1)*nxyb

    DO j = 1,nym1
      is = (j-1)*nxb + ix
      ip = k0 + is
      i  = i0 + j

      tem = 0.25*(p(ip)+p(ip+1)+p(ip+nxyb)+p(ip+1+nxyb))
      wrk1(i) = gx(i)*tem
      tem = 0.25*(p(ip)+p(ip+nxb)+p(ip+nxyb)+p(ip+nxb+nxyb))
      wrk2(i) = gy(i)*tem

    END DO
  END DO

  DO k = 2,nzm1
    i0 = (k-1)*nyb
    k0 = (k-1)*nxyb
    cz = dzi(k)

    DO j = 1,nym1
      i = i0 + j
      is = (j-1)*nxb + ix
      ip = k0 + is

!------ u-equation

      tem   = dxi*(p(ip)*D(is)-p(ip+1)*D(is+1)) + cz*(wrk1(i-nyb)-wrk1(i))
      u(ip) = u(ip) + tem/Du(is) * alphaU(ip)

!------ v-equation

      tem   = dyi*(p(ip)*D(is)-p(ip+nxb)*D(is+nxb)) + cz*(wrk2(i-nyb)-wrk2(i))
      v(ip) = v(ip) + tem/Dv(is) * alphaV(ip)

    END DO
  END DO

END DO Xsweep
!$OMP END DO
!$OMP END PARALLEL

IF( ALLOCATED(gx)   )DEALLOCATE( gx,STAT=alloc_stat )
IF( ALLOCATED(gxb)  )DEALLOCATE( gxb,STAT=alloc_stat )
IF( ALLOCATED(gy)   )DEALLOCATE( gy,STAT=alloc_stat )
IF( ALLOCATED(wrk1) )DEALLOCATE( wrk1,STAT=alloc_stat )
IF( ALLOCATED(wrk2) )DEALLOCATE( wrk2,STAT=alloc_stat )

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE Coeff( grid,ix,gx,gxb,gy )

!------ Compute transformation coefficients

USE SWIMmetField_fd

IMPLICIT NONE

TYPE( MetGrid ),    INTENT( IN  ) :: grid
INTEGER,            INTENT( IN  ) :: ix
REAL, DIMENSION(*), INTENT( OUT ) :: gx, gxb, gy

INTEGER k, i0, j, is, i, ixm, ism
INTEGER nxb, nyb, nzt
REAL    fac

REAL, DIMENSION(:), POINTER :: Hx, Hy

!------ Define locals

nyb = grid%nY
nxb = grid%nX
nzt = grid%nZ + 1

Hx => grid%terrain%Hx
Hy => grid%terrain%Hy

ixm = MAX(1,ix-1)

!----- Loop over y-z plane

DO k = 1,nzt
  fac = 1.0 - grid%McWIF%z(k)/grid%Ztop
  i0 = (k-1)*nyb
  DO j = 1,nyb
    i   = i0 + j
    is  = (j-1)*nxb + ix
    ism = (j-1)*nxb + ixm
    gx(i)  = -Hx(is )*fac
    gxb(i) = -Hx(ism)*fac
    gy(i)  = -Hy(is )*fac
  END DO
END DO

RETURN
END

!==============================================================================

SUBROUTINE SetBC( grid,u,v,w )

!------ Set velocity boundary conditions

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN  ) :: grid
REAL, DIMENSION(:),    POINTER :: u, v, w

INTEGER i0, k, i, j, j0, ip, k0
INTEGER nxb, nyb, nxyb, nzt, nzm1, nym1, nxm1

!------ Define locals

nxb  = grid%nX
nyb  = grid%nY
nxyb = nxb*nyb
nzm1 = grid%nZ
nym1 = nyb - 1
nxm1 = nxb - 1
nzt  = nzm1 + 1

!------ Set lateral boundary conditions

i0 = nym1*nxb

DO k = 2,nzm1
  k0 = (k-1)*nxyb

  DO i = 1,nxb
    ip = k0 + i
    u(ip)    = u(ip+nxb)
    w(ip)    = w(ip+nxb)
    u(i0+ip) = u(i0+ip-nxb)
    v(i0+ip) = v(i0+ip-nxb)
    w(i0+ip) = w(i0+ip-nxb)
  END DO

  DO j = 1,nyb
    j0 = (j-1)*nxb + k0
    v(j0+1)   = v(j0+2)
    w(j0+1)   = w(j0+2)
    u(j0+nxb) = u(j0+nxm1)
    v(j0+nxb) = v(j0+nxm1)
    w(j0+nxb) = w(j0+nxm1)
  END DO

END DO

!------ Set bottom and top bc's

k0 = (nzt-1)*nxyb
k  = k0 - nxyb

DO i = 1,nxyb
  u(i) = -u(i+nxyb)
  v(i) = -v(i+nxyb)
  w(i) = 0.

  u(k0+i) = u(k+i)
  v(k0+i) = v(k+i)
  w(k0+i) = 0.
  w(k +i) = 0.
END DO

RETURN
END

!==============================================================================

INTEGER FUNCTION ZeroNetFlux( grid,u,v )

USE SWIM_fi
USE SWIMparam_fd

IMPLICIT NONE

TYPE( MetGrid ), INTENT( IN  ) :: grid
REAL, DIMENSION(:),    POINTER :: u, v

REAL, DIMENSION(:,:), ALLOCATABLE :: ubc_e, ubc_w, vbc_s, vbc_n

INTEGER alloc_stat, nx, ny, nz, nxy
INTEGER i, j, k, i0, k0, ip

CHARACTER(128), EXTERNAL :: ArraySizeStr

ZeroNetFlux = SWIMfailure

!------ Allocate inflow/outflow arrays

nx = grid%nX; ny = grid%nY; nz = grid%nZ
nxy = nx*ny

ALLOCATE( ubc_e(ny,nz),ubc_w(ny,nz), &
          vbc_s(nx,nz),vbc_n(nx,nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  error%Number = UK_ERROR
  error%Routine = 'ZeroNetFlux'
  error%Message = 'Error allocating BC arrays'
  error%Inform  =  ArraySizeStr( 2,(/MAX(nx,ny),nz/) )
  GOTO 9999
END IF

!------ Set 2d inflow/outflow arrays

DO k = 1,nz
  k0 = (k-1)*nxy

!------ East/West faces

  DO j = 1,ny
    i0 = (j-1)*nx

    ip = k0 + i0 + 1
    ubc_w(j,k)  = u(ip)

    ip = k0 + i0 + nx - 1
    ubc_e(j,k)  = u(ip)

  END DO

!------ North/South faces

  DO i = 1,nx

    ip = k0 + i
    vbc_s(i,k)  = v(ip)

    ip = ip + (ny-2)*nx
    vbc_n(i,k)  = v(ip)

  END DO

END DO

!------ Adjust for zero net flux

CALL adjbc( grid,ubc_e,ubc_w,vbc_s,vbc_n )

!------ Put back in 3d arrays

DO k = 1,nz
  k0 = (k-1)*nxy

!------ East/West faces

  DO j = 1,ny
    i0 = (j-1)*nx

    ip = k0 + i0 + 1
    u(ip) = ubc_w(j,k)

    ip = k0 + i0 + nx - 1
    u(ip)   = ubc_e(j,k)
    u(ip+1) = ubc_e(j,k)

  END DO

!------ North/South faces

  DO i = 1,nx

    ip = k0 + i
    v(ip) = vbc_s(i,k)

    ip = ip + (ny-2)*nx
    v(ip)    = vbc_n(i,k)
    v(ip+nx) = vbc_n(i,k)

  END DO

END DO

ZeroNetFlux = SWIMresult

9999 CONTINUE

IF( ALLOCATED(ubc_e) )DEALLOCATE( ubc_e,STAT=alloc_stat )
IF( ALLOCATED(ubc_w) )DEALLOCATE( ubc_w,STAT=alloc_stat )
IF( ALLOCATED(vbc_s) )DEALLOCATE( vbc_s,STAT=alloc_stat )
IF( ALLOCATED(vbc_n) )DEALLOCATE( vbc_n,STAT=alloc_stat )

RETURN
END

!==============================================================================

SUBROUTINE adjbc( grid,ue,uw,vs,vn )

!------ Adjust velocity boundary conditions for mass consistency

USE constants_fd
USE SWIM_fi

IMPLICIT NONE

TYPE( MetGrid ),                  INTENT( IN    ) :: grid
REAL, DIMENSION(grid%nY,grid%nZ), INTENT( INOUT ) :: ue, uw
REAL, DIMENSION(grid%nX,grid%nZ), INTENT( INOUT ) :: vs, vn

INTEGER i, k, nx, ny, nz, nxm1, nym1, nzm1
REAL    xc, yc, xmap, ymap

REAL(8) fluxIn, fluxOut, delx, dely, eps, facIn, facOut, facz
!REAL(8) fluxc

REAL, DIMENSION(:), POINTER :: z

xc = 0.5*(grid%Xmin+grid%Xmax)
yc = 0.5*(grid%Ymin+grid%Ymax)

CALL SWIMmapfac( grid%coord,xc,yc,xmap,ymap )

delx = grid%dX / xmap
dely = grid%dY / ymap

z => grid%McWIF%z

nx = grid%nX; ny = grid%nY; nz = grid%nZ

nxm1 = nx-1
nym1 = ny-1
nzm1 = nz-1

fluxIn  = 0.D0
fluxOut = 0.D0

DO k = 2,nzm1

  facz = (z(k) - z(k-1))*dely
  DO i = 2,nym1
    fluxIn  = fluxIn  + (MAX(uw(i,k),0.) + MAX(-ue(i,k),0.))*facz
    fluxOut = fluxOut + (MAX(ue(i,k),0.) + MAX(-uw(i,k),0.))*facz
  END DO

  facz = (z(k) - z(k-1))*delx
  DO i = 2,nxm1
    fluxIn  = fluxIn  + (MAX(vs(i,k),0.) + MAX(-vn(i,k),0.))*facz
    fluxOut = fluxOut + (MAX(vn(i,k),0.) + MAX(-vs(i,k),0.))*facz
  END DO

END DO

IF( ABS(fluxIn+fluxOut) > DBLE(SMALL) )THEN
  eps = -(fluxIn - fluxOut)/(fluxIn+fluxOut)
ELSE
  eps = 0.
END IF

facOut = 1.D0 - eps
facIn  = 1.D0 + eps

fluxIn  = 0.D0
fluxOut = 0.D0

DO k = 2,nzm1

  facz = (z(k) - z(k-1))*dely
  DO i = 2,nym1
    IF( ue(i,k) > 0. )THEN
      ue(i,k) = ue(i,k)*facOut
    ELSE
      ue(i,k) = ue(i,k)*facIn
    END IF
    IF( uw(i,k) < 0. )THEN
      uw(i,k) = uw(i,k)*facOut
    ELSE
      uw(i,k) = uw(i,k)*facIn
    END IF
    fluxIn  = fluxIn  + (MAX(uw(i,k),0.) + MAX(-ue(i,k),0.))*facz
    fluxOut = fluxOut + (MAX(ue(i,k),0.) + MAX(-uw(i,k),0.))*facz
  END DO

  facz = (z(k) - z(k-1))*delx
  DO i = 2,nxm1
    IF( vn(i,k) > 0. )THEN
      vn(i,k) = vn(i,k)*facOut
    ELSE
      vn(i,k) = vn(i,k)*facIn
    END IF
    IF( vs(i,k) < 0. )THEN
      vs(i,k) = vs(i,k)*facOut
    ELSE
      vs(i,k) = vs(i,k)*facIn
    END IF
    fluxIn  = fluxIn  + (MAX(vs(i,k),0.) + MAX(-vn(i,k),0.))*facz
    fluxOut = fluxOut + (MAX(vn(i,k),0.) + MAX(-vs(i,k),0.))*facz
  END DO

END DO

!fluxc = fluxIn - fluxOut

RETURN
END

!==============================================================================

INTEGER FUNCTION allocateMGfield( n,lTpot,MGfield ) RESULT( irv )

USE SWIM_fi
USE SWIMparam_fd
USE MGfield_fd

INTEGER,           INTENT( IN    ) :: n
LOGICAL,           INTENT( IN    ) :: lTpot
TYPE( MetMean3D ), INTENT( INOUT ) :: MGfield

INTEGER alloc_stat

CHARACTER(128), EXTERNAL :: ArraySizeStr

irv = SWIMfailure

ALLOCATE( MGfield%U(n),MGfield%V(n),MGfield%W(n),MGfield%Press(n),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

IF( lTpot )THEN
  ALLOCATE( MGfield%Tpot(n),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9999
END IF

irv = SWIMresult

9999 CONTINUE

IF( alloc_stat /= 0 )THEN
  error%Number  = UK_ERROR
  error%Routine = 'allocateMGfield'
  error%Message = 'Error allocating multi-grid 3d field arrays for mass-consisten calculation'
  error%Inform  =  ArraySizeStr( 1,(/n/) )
  GOTO 9999
END IF

RETURN
END

!==============================================================================

INTEGER FUNCTION deallocateMGfield( MGfield ) RESULT( irv )

USE SWIM_fi
USE SWIMparam_fd
USE MGfield_fd

TYPE( MetMean3D ), INTENT( INOUT ) :: MGfield

INTEGER alloc_stat

IF( ASSOCIATED(MGfield%U)     )DEALLOCATE(MGfield%U,    STAT=alloc_stat)
IF( ASSOCIATED(MGfield%V)     )DEALLOCATE(MGfield%V,    STAT=alloc_stat)
IF( ASSOCIATED(MGfield%W)     )DEALLOCATE(MGfield%W,    STAT=alloc_stat)
IF( ASSOCIATED(MGfield%Press) )DEALLOCATE(MGfield%Press,STAT=alloc_stat)
IF( ASSOCIATED(MGfield%Tpot)  )DEALLOCATE(MGfield%Tpot, STAT=alloc_stat)

irv = SWIMresult

RETURN
END

