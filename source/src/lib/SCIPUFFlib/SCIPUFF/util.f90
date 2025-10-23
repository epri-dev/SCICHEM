SUBROUTINE init_param()

USE scipuff_fi

IMPLICIT NONE

!------ code version

iversion = iversion_code

!------ puff variable names

names(1)  = 'X   '
names(2)  = 'Y   '
names(3)  = 'Z   '
names(4)  = 'SXX '
names(5)  = 'SXY '
names(6)  = 'SXZ '
names(7)  = 'SYY '
names(8)  = 'SYZ '
names(9)  = 'SZZ '
names(10) = 'AXX '
names(11) = 'AXY '
names(12) = 'AXZ '
names(13) = 'AYY '
names(14) = 'AYZ '
names(15) = 'AZZ '
names(16) = 'DET '
names(17) = 'C   '
names(18) = 'CC  '
names(19) = 'XUC '
names(20) = 'XVC '
names(21) = 'YVC '
names(22) = 'YVSC'
names(23) = 'YVBC'
names(24) = 'ZWC '
names(25) = 'WC  '
names(26) = 'CCB '
names(27) = 'SI  '
names(28) = 'SI2 '
names(29) = 'SV  '
names(30) = 'SR  '
names(31) = 'CFO '
names(32) = 'ZI  '
names(33) = 'ZC  '
names(34) = 'U   '
names(35) = 'V   '
names(36) = 'W   '
names(37) = 'TYPE'
names(38) = 'NEXT'
names(39) = 'PREV'
names(40) = 'GRID'
names(41) = 'TLEV'
names(42) = 'NXTL'
names(43) = 'IAUX'

!------ constants

g       = G0
f0      = 4.*PI/(24.*3600.)

!------ turbulence parameters

!a         = 0.75
!b         = 0.125
!bs        = 1.8*b
!cvrtx     = 3.0
!cqb       = 0.4
!csi1      = 0.25
!csi2      = 2.0*bs/4.6
!vonk      = 0.40
sle_fac   = 1.4

!------ puff parameters

simrge    = 1.25
rrmrge    = 0.75
asplt     = 0.75 ! must be < 0.999
asplt2    = asplt*asplt
aspltc    = 1.0 - asplt2
dxsplt    = 0.5
dzsplt    = 0.5
fac_rfl   = 3.0

!------ default parameter values

gt      = 0.0327
!eqf     = gt/(2.0*a*bs)
rmuair  = 1.6E-5
rhoair  = 1.2
rnu     = rmuair/rhoair
cmin    = 0.
delmin  = DEF_VAL_R
z_dosage = 0.
title   ='SCIPUFF'
lmap    = I_LATLON
create    = .FALSE.
dynamic   = .FALSE.
dense_gas = .FALSE.
buoy_gas  = .FALSE.
lsplitz   = .FALSE.
static    = .TRUE.
run_mode= 0
t_avg   = DEF_VAL_R
mgrd    = 2
hres    = DEF_VAL_R
vres    = DEF_VAL_R
xmin    = DEF_VAL_R
xmax    = DEF_VAL_R
ymin    = DEF_VAL_R
ymax    = DEF_VAL_R
zmax    = DEF_VAL_R
utm_zone= NOT_SET_I
dxg     = 0.
dyg     = 0.
delx2   = NOT_SET_R
xref    = 0.
yref    = 0.
lon0    = NOT_SET_R
lat0    = NOT_SET_R
epstrop = 4.0E-4
sltrop  = 10.
wwtrop  = 0.01
audit_class   ='Not specified'
audit_analyst ='Anonymous'
audit_space = ' '
smpfile = ' '
dt_smp  = DEF_VAL_R
lwash   = .TRUE.
substrate_type = 0
nsrc_prime = 0
CALL init_stack_rel_prime()

!------ material definitions

CALL init_matl_param()

RETURN
END

!===============================================================================

SUBROUTINE init_matl_param()

USE scipuff_fi

IMPLICIT NONE

INTEGER i

DO i = 1,MAXCLS !namec no longer used
  namec(i) =' '
END DO

ntypm   = 0
ntypp   = 0
ntyps   = 0
ntypd   = 0
nmaux   = 0

RETURN
END

!===============================================================================

RECURSIVE INTEGER FUNCTION time_level( dtp )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

REAL, INTENT( IN ) :: dtp

REAL    del
INTEGER ilev

!----- Find time level through number of doublings

del  = dtp
ilev = 0
DO WHILE( del < delt .AND. ilev <= MAXTLV )
  ilev = ilev + 1
  del  = del + del
END DO

IF( ilev > MAXTLV )THEN
  nError   = SZ_ERROR
  eRoutine = 'time_level'
  del      = dtp * (2.0**MAXTLV)
  eMessage = 'Puff timestep too small'
  WRITE(eInform,'(A,1P,E9.1,A)')'Max Timestep must be less than ',del,' sec'
  IF( del/delt > .001 )THEN
    eAction= 'Edit project to reduce Maximum Timestep'
  ELSE
    eAction= 'Please submit a problem report about this project'
  END IF
  WRITE(lun_log,*,ERR=9999)'********* Time level error: dtp=',dtp,' : del=',del
  GOTO 9999
END IF

9999 CONTINUE

time_level = MIN(ilev,MAXTLV)

RETURN
END

!==============================================================================

SUBROUTINE frac( x,x0,dx,nx,rat,i )

IMPLICIT NONE

REAL,    INTENT( IN  ) :: x
REAL,    INTENT( IN  ) :: x0
REAL,    INTENT( IN  ) :: dx
INTEGER, INTENT( IN  ) :: nx
REAL,    INTENT( OUT ) :: rat
INTEGER, INTENT( OUT ) :: i

REAL xfrac

xfrac = (x-x0)/dx
xfrac = MAX(xfrac,0.0)
xfrac = MIN(xfrac,FLOAT(nx-1))

i = MIN(INT(xfrac) + 1,nx-1)

rat = xfrac - FLOAT(i-1)
rat = MAX(0.,MIN(rat,1.))

RETURN
END

!=======================================================================

SUBROUTINE ludcmp( a,n,np,indx,d,vv )

USE error_fi

!====   Linear solver - decomposition

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: n, np
INTEGER,  DIMENSION(n), INTENT( OUT   ) :: indx
REAL,                   INTENT( OUT   ) :: d
REAL, DIMENSION(np,np), INTENT( INOUT ) :: a
REAL, DIMENSION(np),    INTENT( OUT   ) :: vv

INTEGER i, imax, j, k
REAL    aamax, dum, sum

!====   Get implicit scaling and check for singular matrix

d = 1.

DO i = 1, n
  aamax=0.
  DO j = 1,n
    aamax = MAX(aamax,ABS(a(i,j)))
  END DO
  IF( aamax == 0. )THEN
    nError   = IV_ERROR
    eRoutine = 'ludcmp'
    eMessage = 'Singular matrix in ludcmp'
    GOTO 9999
  END IF
  vv(i) = 1./aamax
END DO

!====   Loop over columns of Crout's method

DO j = 1,n

!====== Step 1

  DO i = 1,j-1
    sum = a(i,j)
    DO k=1,i-1
      sum = sum - a(i,k)*a(k,j)
    END DO
    a(i,j) = sum
  END DO

!====== Step 2 + find largets pivotal element

  aamax = 0.
  DO i = j,n
    sum = a(i,j)
    DO k = 1,j-1
      sum = sum - a(i,k)*a(k,j)
    END DO
    a(i,j) = sum
    dum = vv(i)*ABS(sum)
    IF( dum >= aamax )THEN
      imax  = i
      aamax = dum
    END IF
  END DO

!====== Step 3 - interchange rows if necessary

  IF( j /= imax )THEN
    DO k = 1,n
      dum = a(imax,k)
      a(imax,k)=a(j,k)
      a(j,k) = dum
    END DO
    d = -d
    vv(imax) = vv(j)
  END IF

!====== Step 4 - divide by pivotal element

  indx(j) = imax
  IF( a(j,j) == 0. )a(j,j) = TINY(1.0)
  IF( j /= n )THEN
    dum = 1./a(j, j)
    DO i = j+1,n
      a(i,j) = a(i,j)*dum
    END DO
  END IF
END DO

9999 CONTINUE

RETURN
END

!=======================================================================

SUBROUTINE lubksb( a,n,np,indx,b )

!====   Linear solver - generate the solution vector

IMPLICIT NONE

INTEGER,                INTENT( IN    ) :: n, np
INTEGER,  DIMENSION(n), INTENT( IN    ) :: indx
REAL, DIMENSION(np,np), INTENT( IN    ) :: a
REAL, DIMENSION(n),     INTENT( INOUT ) :: b

INTEGER i, ii, j, ll
REAL    sum

!====   Forward substitution

ii = 0
DO i = 1,n
  ll = indx(i)
  sum = b(ll)
  b(ll) = b(i)
  IF( ii /= 0 )THEN
    DO j = ii,i-1
      sum = sum - a(i,j)*b(j)
    END DO
  ELSE IF( sum /= 0. )THEN
    ii = i
  END IF
  b(i) = sum
END DO

!====   backward substitution

DO i = n,1,-1
  sum = b(i)
  DO j = i+1,n
    sum = sum - a(i,j)*b(j)
  END DO
  b(i) = sum/a(i,i)
END DO

RETURN
END

!===============================================================================

INTEGER FUNCTION limit( i,imin,imax )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i, imin, imax

limit = MAX( i,imin )
limit = MIN( limit,imax )

RETURN
END

!===============================================================================

REAL FUNCTION rlimit( r,rmin,rmax )

IMPLICIT NONE

REAL, INTENT( IN ) :: r, rmin, rmax

rlimit = MAX( r,rmin )
rlimit = MIN( rlimit,rmax )

RETURN
END

!===============================================================================

SUBROUTINE check_lon( x,xmin,xmax )

USE error_fi
USE default_fd

IMPLICIT NONE

REAL, INTENT( INOUT ) :: x
REAL, INTENT( IN    ) :: xmin, xmax

INTEGER, PARAMETER :: MAX = 3

INTEGER icount

CHARACTER(80) xsav

IF( x == DEFERRED_R )GOTO 9999

icount = 0
WRITE(xsav,*) x

IF( x < xmin )THEN
  DO WHILE( xmin > x .AND. icount < MAX )
    x = x + 360.
    icount = icount + 1
  END DO
ELSE IF( x > xmax )THEN
  DO WHILE( x > xmax .AND. icount < MAX )
    x = x - 360.
    icount = icount + 1
  END DO
END IF

IF( icount >= MAX )THEN
  nError = UK_ERROR
  eRoutine = 'check_lon'
  eMessage = 'Invalid longitude='//TRIM(xsav)
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE check_lonD( x,xmin,xmax )

USE error_fi
USE default_fd

IMPLICIT NONE

REAL(8), INTENT( INOUT ) :: x
REAL,    INTENT( IN    ) :: xmin, xmax

INTEGER, PARAMETER :: MAX = 3

INTEGER icount

REAL(8) dmax, dmin

CHARACTER(80) xsav

IF( x == DEFERRED_D )GOTO 9999

icount = 0
WRITE(xsav,*) x

dmin = DBLE(xmin)
dmax = DBLE(xmax)

IF( x < dmin )THEN
  DO WHILE( dmin > x .AND. icount < MAX )
    x = x + 360.D0
    icount = icount + 1
  END DO
ELSE IF( x > dmax )THEN
  DO WHILE( x > dmax .AND. icount < MAX )
    x = x - 360.D0
    icount = icount + 1
  END DO
END IF

IF( icount >= MAX )THEN
  nError = UK_ERROR
  eRoutine = 'check_lon'
  eMessage = 'Invalid longitude='//TRIM(xsav)
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE SetGlobalLon( x )

USE scipuff_fi

IMPLICIT NONE

REAL, INTENT( INOUT ) :: x

DO WHILE( x < xmin )
  x = x + 360.
END DO
DO WHILE( x > xmax )
  x = x - 360.
END DO

RETURN
END

!===============================================================================

SUBROUTINE SetGlobalLonD( x )

USE scipuff_fi

IMPLICIT NONE

REAL(8), INTENT( INOUT ) :: x

REAL(8) dmax, dmin

dmin = DBLE(xmin)
dmax = DBLE(xmax)

DO WHILE( x < dmin )
  x = x + 360.D0
END DO
DO WHILE( x > dmax )
  x = x - 360.D0
END DO

RETURN
END

!===============================================================================

SUBROUTINE SetGlobalGrid( x,n )

IMPLICIT NONE

REAL,    INTENT( INOUT ) :: x
INTEGER, INTENT( IN    ) :: n

DO WHILE( x < 0. )
  x = x + FLOAT(n)
END DO
DO WHILE( x > FLOAT(n) )
  x = x - FLOAT(n)
END DO

RETURN
END

!==============================================================================

SUBROUTINE SetDataRelID( nRel,SigR,p )

USE scipuff_fi

IMPLICIT NONE

INTEGER,          INTENT( IN ) :: nRel
REAL,             INTENT( IN ) :: SigR
TYPE( puff_str ), INTENT( IN ) :: p

INTEGER alloc_stat

TYPE( Data_relID ), DIMENSION(:), ALLOCATABLE :: tem

IF( numRelID == 0 )THEN

  ALLOCATE( releaseID(10),STAT=alloc_stat )
  IF( alloc_stat /= 0 )GOTO 9998

ELSE

  IF( numRelID+1 > SIZE(releaseID) )THEN
    ALLOCATE( tem(numRelID),STAT=alloc_stat )
    IF( alloc_stat /= 0 )GOTO 9998

    tem = releaseID

    DEALLOCATE( releaseID,STAT=alloc_stat )
    ALLOCATE( releaseID(numRelID+10),STAT=alloc_stat )
    IF( alloc_stat /= 0 )GOTO 9998

    releaseID(1:numRelID) = tem

    DEALLOCATE( tem,STAT=alloc_stat )
  END IF

END IF

numRelID = numRelID + 1

releaseID(numRelID)%SigRel   = SQRT(SigR*SigR + 0.5*(p%sxx + p%syy))
releaseID(numRelID)%SigMerge = 2.0*SigR/SQRT(FLOAT(nRel))

9999 CONTINUE

IF( ALLOCATED(tem) )DEALLOCATE( tem,STAT=alloc_stat )

RETURN

9998 CONTINUE

nError = UK_ERROR
eRoutine = 'SetRelNWPN'
eMessage = 'Unable to allocate NWPN release array'
GOTO 9999

END

!===============================================================================

SUBROUTINE siginv( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p

REAL(8) d11, d12, d13, d22, d23, d33, dalp, ralp
REAL(8) dxx, dxy, dxz, dyy, dyz, dzz

dxx = DBLE(p%sxx)
dxy = DBLE(p%sxy)
dxz = DBLE(p%sxz)
dyy = DBLE(p%syy)
dyz = DBLE(p%syz)
dzz = DBLE(p%szz)

d11 = dyy*dzz - dyz*dyz
d12 = dxy*dzz - dxz*dyz
d13 = dxy*dyz - dxz*dyy
d22 = dxx*dzz - dxz*dxz
d23 = dxx*dyz - dxz*dxy
d33 = dxx*dyy - dxy*dxy

dalp = dxx*d11 - dxy*d12 + dxz*d13
ralp = 0.5D0/dalp

p%axx =  SNGL(d11*ralp)
p%axy = -SNGL(d12*ralp)
p%axz =  SNGL(d13*ralp)
p%ayy =  SNGL(d22*ralp)
p%ayz = -SNGL(d23*ralp)
p%azz =  SNGL(d33*ralp)

p%det = SNGL(dalp)

RETURN
END

!===============================================================================

LOGICAL FUNCTION PuffRealizable(  sxx,sxy,sxz,syy,syz,szz )

IMPLICIT NONE

REAL sxx,sxy,sxz,syy,syz,szz

REAL d11,d12,d13,dalp

PuffRealizable = .FALSE.

IF( sxy*sxy >= sxx*syy )GOTO 9999
IF( sxz*sxz >= sxx*szz )GOTO 9999
IF( syz*syz >= syy*szz )GOTO 9999

d11 = syy*szz - syz*syz
d12 = sxy*szz - sxz*syz
d13 = sxy*syz - sxz*syy

dalp = sxx*d11 - sxy*d12 + sxz*d13

IF( dalp <= 0.0 )GOTO 9999

PuffRealizable = .TRUE.

9999 CONTINUE

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE zero_puff( p )

USE struct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( OUT ) :: p

TYPE( puff_str_ri ) p_ri

INTEGER ios

IF( ASSOCIATED(p%aux) )DEALLOCATE(p%aux,STAT=ios)

p_ri%p_real = 0.0
p_ri%p_int  = 0
NULLIFY(p_ri%aux)

p = TRANSFER(p_ri,p)

RETURN
END

