INTEGER FUNCTION SWIMinitMcWIF( zMC,nzMC,grid )

USE SWIM_fi
USE SWIMparam_fd
USE SWIMinit_fd
USE constants_fd

IMPLICIT NONE

REAL, DIMENSION(:),      POINTER         :: zMC
INTEGER,                 INTENT( IN    ) :: nzMC
TYPE( MetGrid ), TARGET, INTENT( INOUT ) :: grid

REAL, DIMENSION(:), POINTER :: zb, z, zt, zbw, dzi, dzti

INTEGER ios, irv, nzb, nzt, nxb, nyb, nzm1, i, j, i0
REAL    xmap, ymap
LOGICAL lAssim

TYPE( MetGrid ), POINTER :: gridp

INTEGER k
REAL    sum_var, sum_svar, sx, sy, hv

CHARACTER(128) string

REAL, DIMENSION(:), ALLOCATABLE :: hp

INTEGER, EXTERNAL :: SWIMaddLogMessage
INTEGER, EXTERNAL :: initFFTMcWIF, PostProgressMessage, SetTerrainGrad
REAL,    EXTERNAL :: SWIMsetHmin
LOGICAL, EXTERNAL :: SWIMDoMG

INTERFACE

  INTEGER FUNCTION SWIMInitMG( gridp )
    USE SWIM_fi
    TYPE( MetGrid ), POINTER :: gridp
  END FUNCTION SWIMInitMG

  SUBROUTINE nullifyMetGrid2( gridx,gridp )
    USE SWIMmetField_fd
    TYPE( MetGrid ), TARGET, INTENT( INOUT ), OPTIONAL :: gridx
    TYPE( MetGrid ),         POINTER,         OPTIONAL :: gridp
  END SUBROUTINE nullifyMetGrid2

END INTERFACE

message%bString  = 'SCIPUFF Mass-consistent initialization'

irv = PostProgressMessage( message )

SWIMinitMcWIF = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'SWIMinitMcWIF'
error%Message = 'Error allocating McWIF arrays'

irv = SWIMaddLogMessage('Setup for mass-consistent adjustment of wind fields')
IF( irv /= SWIMsuccess )GOTO 9999

!------ Check input grid
!       N.B. Check for assimlating obs into existing gridded field
!       IF so, basic grid is unchanged.

IF( nzMC < 0 )THEN
  lAssim = .TRUE.

ELSE
  lAssim = .FALSE.

  IF( nzMC < 1 .OR. .NOT.ASSOCIATED(zMC) )THEN
    error%Message = 'Undefined vertical grid for mass-consistent calculation'
    GOTO 9999
  END IF

  IF( zMC(1) <= 0. )THEN
    error%Message = 'Lowest level of vertical grid must be greater than zero'
    GOTO 9999
  END IF

  DO i = 2,nzMC
    IF( zMC(i) <= zMC(i-1) )THEN
      error%Message = 'Vertical grid not monotonic'
      GOTO 9999
    END IF
  END DO

END IF

IF( MIN(grid%nX,grid%nY) < 3 )THEN
  error%Number  = IV_ERROR
  error%Message = 'Horizontal grid too small for mass-consistent calculation'
  error%Inform  = 'Must be at least 3x3'
  GOTO 9999
END IF

!------ Set vertical grid

IF( lAssim )THEN
  nzb = grid%nZ
  nzt = nzb + 1

ELSE
  nzb = nzMC
  nzt = nzb + 1
  grid%nZ = nzb
  ALLOCATE( grid%Z(nzb),grid%Zw(nzt),STAT=ios )
  IF( ios /= 0 )GOTO 9999
  DO i = 1,nzMC
    grid%Z(i) = zMC(i)
  END DO

END IF

ALLOCATE( grid%McWIF%zt(nzt),grid%McWIF%z(nzt),grid%McWIF%alphaFFT(nzt), &
          grid%McWIF%dzi(nzt),grid%McWIF%dzti(nzt), &
          grid%McWIF%cza(nzt),grid%McWIF%czb(nzt),STAT=ios )
IF( ios /= 0 )GOTO 9999

!------ Allocate horizontal weight arrays

ALLOCATE( grid%McWIF%alphaU(nzt*grid%nXY),grid%McWIF%alphaV(nzt*grid%nXY),STAT=ios )
IF( ios /= 0 )GOTO 9999

DO i = 1,nzt*grid%nXY
  grid%McWIF%alphaU(i) = 1.
  grid%McWIF%alphaV(i) = 1.
END DO

!------ Assign local pointers

zb   => grid%Z
zbw  => grid%Zw
z    => grid%McWIF%z
zt   => grid%McWIF%zt
dzi  => grid%McWIF%dzi
dzti => grid%McWIF%dzti

nxb  = grid%nX
nyb  = grid%nY
nzm1 = nzt-1

!------ Define vertical grid

zt(1) = -zb(1)
DO i = 1,nzb
  zt(i+1) = zb(i)
END DO

z(1)  = 0.5*(zt(1)+zt(2))
DO i = 2,nzm1
  z(i) = 0.5*(zt(i)+zt(i+1))
END DO
z(nzt) = 1.5*zt(nzt) - 0.5*zt(nzm1)

IF( .NOT.lAssim )THEN
  DO i = 1,nzt
    zbw(i) = z(i)
  END DO
  grid%Ztop = z(nzm1)
END IF

DO i = 2,nzt
  dzi(i)  = 1./(z(i) - z(i-1))
  dzti(i) = 1./(zt(i) - zt(i-1))
END DO
dzi(1)  = dzi(2)
dzti(1) = dzti(2)

!------ Set staggered grid type bits
!       N.B. Applies even if assimilating directly into gridded met
!       (which is checked in SWIMinitObsAssim)

grid%type = IBSET(grid%type,GTB_STAGGER)
grid%type = IBSET(grid%type,GTB_STAGGERZ)

IF( .NOT.lAssim )THEN

!------ Zero slope terrain boundaries

  DO i = 1,grid%nXY  !First add back Hmin in case it' on a boundary
    grid%terrain%H(i) = grid%terrain%H(i) + grid%Hmin
  END DO

  i0 = (nyb-1)*nxb
  DO i = 1,nxb
    grid%terrain%H(i)    = grid%terrain%H(i+nxb)
    grid%terrain%H(i0+i) = grid%terrain%H(i0+i-nxb)
  END DO

  DO j = 1,nyb
    i0 = (j-1)*nxb
    grid%terrain%H(i0+1)   = grid%terrain%H(i0+2)
    grid%terrain%H(i0+nxb) = grid%terrain%H(i0+nxb-1)
  END DO

  CALL SetHmin( grid ) !Reset Hmin

  Prj%Hmin = SWIMsetHmin()

!------ Define relative depth arrays and gradients

  irv = SetTerrainGrad( grid )
  IF( irv /= SWIMsuccess )GOTO 9999

END IF

!------ Scale dx and dy by mean map factors

CALL SWIMmapfac( grid%coord,0.5*(grid%Xmin+grid%Xmax), &
                            0.5*(grid%Ymin+grid%Ymax),xmap,ymap )

grid%McWIF%dxi = xmap/grid%dX
grid%McWIF%dyi = ymap/grid%dY

!------ Set vertical partition parameter limits

grid%McWif%alphaMin = Prj%MC%alphaMin
grid%McWif%alphaMax = Prj%MC%alphaMax

!------ Setup FFT arrays

ALLOCATE( grid%McWIF%xevec(2*nxb+15),grid%McWIF%xsn(nxb), &
          grid%McWIF%xcs(nxb),grid%McWIF%xeval(nxb),STAT=ios )
IF( ios /= 0 )GOTO 9999

ALLOCATE( grid%McWIF%yevec(2*nyb+15),grid%McWIF%ysn(nyb), &
          grid%McWIF%ycs(nyb),grid%McWIF%yeval(nyb),STAT=ios )
IF( ios /= 0 )GOTO 9999

irv = initFFTMcWIF( nxb,nyb,grid%McWIF )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Find max slope

grid%McWIF%MaxSlope = -HUGE(0.)
DO i = 1,grid%nXY
  grid%McWIF%MaxSlope = MAX( grid%McWIF%MaxSlope,     &
                             ABS(grid%terrain%Hx(i)), &
                             ABS(grid%terrain%Hy(i)) )
END DO

!------ Find characteristic length scale
!       First look at x-direction

ALLOCATE( hp(nxb),STAT=ios )
IF( ios /= 0 )GOTO 9999

sum_var  = 0.
sum_svar = 0.

DO j = 1,nyb
  i0 = (j-1)*nxb

  hp = grid%terrain%H(i0+1:i0+nxb)

  CALL SetHscale( nxb,hp,k,hv )

  sum_var  = sum_var + hv
  sum_svar = sum_svar + FLOAT(k)*hv

END DO

IF( sum_var > SMALL )THEN
  sx = sum_svar / sum_var
  sx = MIN(sx,FLOAT(nxb-1)*0.5)
ELSE
  sx = FLOAT(nxb-1)*0.5
END IF
sx = sx / grid%McWIF%dxi

DEALLOCATE( hp,STAT=ios )

ALLOCATE( hp(nyb),STAT=ios )
IF( ios /= 0 )GOTO 9999

sum_var  = 0.
sum_svar = 0.

DO j = 1,nxb

  hp = grid%terrain%H(j:nxb*nyb:nxb)

  CALL SetHscale( nyb,hp,k,hv )

  sum_var  = sum_var + hv
  sum_svar = sum_svar + FLOAT(k)*hv

END DO

IF( sum_var > SMALL )THEN
  sy = sum_svar / sum_var
  sy = MIN(sy,FLOAT(nyb-1)*0.5)
ELSE
  sy = FLOAT(nyb-1)*0.5
END IF
sy = sy / grid%McWIF%dyi

DEALLOCATE( hp,STAT=ios )

grid%McWIF%Hscale = MAX(sx,sy) * 2.

grid%McWIF%Hscale = MIN(grid%McWIF%Hscale, &
                        MAX(FLOAT(nxb-1)/grid%McWIF%dxi,FLOAT(nyb-1)/grid%McWIF%dyi))

WRITE(string,"('SCIPUFF Mass-consistent Hscale : ',ES12.4)",IOSTAT=ios) grid%McWIF%Hscale
irv = SWIMaddLogMessage( string )
IF( irv /= SWIMsuccess )GOTO 9999

!------ Setup coarser grids for multi-grid solution

IF( .NOT.Prj%Create )THEN

  gridp => grid

  DO WHILE( SWIMDoMG(gridp) )

    ALLOCATE( gridp%MGgrid,STAT=ios )
    IF( ios /= 0 )GOTO 9999

    grid%numMG = grid%numMG + 1

    CALL nullifyMetGrid2( gridp=gridp%MGgrid )

    irv =  SWIMInitMG( gridp )
    IF( irv /= SWIMsuccess )GOTO 9999

    gridp => gridp%MGgrid

  END DO

END IF

!------ Another SWIMming success

SWIMinitMcWIF = SWIMresult

error%Number = NO_ERROR
error%Routine = ' '
error%Message = ' '

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetHscale( N,hp,k,hv )

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: N
REAL, DIMENSION(N), INTENT( INOUT ) :: hp
INTEGER,            INTENT( OUT   ) :: k
REAL,               INTENT( OUT   ) :: hv

REAL, PARAMETER :: RHO_LIM = 0.5

REAL fac, hb, sum0, sum1, sum2, rho

fac = 1./FLOAT(N)

hb = SUM(hp)*fac
hp = hp - hb           !Perturbation height
hv = SUM(hp**2)*fac    !Variance

DO k = 1,N/2

  sum0 = SUM(hp(1:N-k)*hp(k+1:N))
  sum1 = SUM(hp(1:N-k)**2)
  sum2 = SUM(hp(k+1:N)**2)

  rho = sum0/SQRT(sum1*sum2)

  IF( rho < RHO_LIM )EXIT

END DO

RETURN
END

!===============================================================================

INTEGER FUNCTION initFFTMcWIF( nx,ny,mc )

USE SWIM_fi
USE SWIMparam_fd
USE constants_fd

!------ Poisson solver constants

IMPLICIT NONE

INTEGER,           INTENT( IN    ) :: nx, ny
TYPE( McWIFdata ), INTENT( INOUT ) :: mc

INTEGER nper, i, alloc_stat
REAL    cx2, cy2

REAL, DIMENSION(:), ALLOCATABLE :: work

initFFTMcWIF = SWIMfailure

error%Number  = UK_ERROR
error%Routine = 'initFFTMcWIF'
error%Message = 'Error allocating FFT work arrays'

ALLOCATE( work(SIZE(mc%xevec)),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO i = 1,SIZE(work)
  work(i) = 1.E+36
END DO

nper = 2*(nx-2)
CALL rffti( nper,work ); mc%xevec = work; DEALLOCATE(work,STAT=alloc_stat)

DO i = 1,nx-2
  mc%xcs(i) = COS(FLOAT(i-1)*PI/FLOAT(nper))
  mc%xsn(i) = SIN(FLOAT(i-1)*PI/FLOAT(nper))
END DO

ALLOCATE( work(SIZE(mc%yevec)),STAT=alloc_stat )
IF( alloc_stat /= 0 )GOTO 9999

DO i = 1,SIZE(work)
  work(i) = 1.E+36
END DO

nper = 2*(ny-2)
CALL rffti( nper,work ); mc%yevec = work; DEALLOCATE(work,STAT=alloc_stat)

DO i = 1,ny-2
  mc%ycs(i) = COS(FLOAT(i-1)*PI/FLOAT(nper))
  mc%ysn(i) = SIN(FLOAT(i-1)*PI/FLOAT(nper))
END DO

cx2 = mc%dxi*mc%dxi
DO i = 1,nx
  mc%xeval(i) = 2.0*(COS(PI*FLOAT(i-1)/FLOAT(nx-2))-1.0)*cx2
END DO

cy2 = mc%dyi*mc%dyi
DO i = 1,ny
  mc%yeval(i) = 2.0*(COS(PI*FLOAT(i-1)/FLOAT(ny-2))-1.0)*cy2
END DO

initFFTMcWIF = SWIMresult

error%Number  = NO_ERROR
error%Routine = ' '
error%Message = ' '

9999 CONTINUE

RETURN
END
