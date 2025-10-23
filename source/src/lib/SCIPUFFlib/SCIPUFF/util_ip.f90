!===============================================================================
!     ipgrd utility routines
!===============================================================================

SUBROUTINE allocate_ip( nz )

USE ipgrd_fi
USE error_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nz

INTEGER alloc_stat

ALLOCATE( ipgrd(MAXG,2,nz),npgrd(nz),mxlev_grd(nz),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'allocate_ip'
  eMessage = 'Error allocating puff list arrays'
  WRITE(eInform,'(A,I12)') 'Size requested=',MAXG*2*nz
  eAction  = 'Try reducing vertical resolution'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE zero_ip()

USE scipuff_fi
USE ipgrd_fi

IMPLICIT NONE

INTEGER i, k

DO k = 1,nz
  CALL set_npgrd( k,nx*ny )
  CALL set_mxlev( k,0 )
  DO i = 1,MAXG !nx*ny
    CALL clr_ipgrd( i,k )
  END DO
END DO

RETURN
END

!========================================================================

SUBROUTINE find_cell( x,y,k,mlev,m0,n0,jpuf )

IMPLICIT NONE

REAL, PARAMETER :: EPS = 1.E-6

REAL,    INTENT( IN  ) :: x, y
INTEGER, INTENT( IN  ) :: k
INTEGER, INTENT( IN  ) :: mlev, m0, n0
INTEGER, INTENT( OUT ) :: jpuf

INTEGER icell, ix, iy, ilev, jcell
REAL    xc, yc

xc = MAX(0.,MIN(FLOAT(m0)-EPS,x))
yc = MAX(0.,MIN(FLOAT(n0)-EPS,y))

ix = INT(xc)
iy = INT(yc)
icell = iy*m0 + ix + 1

IF( mlev > 0 )THEN

  DO ilev = 1,mlev

    CALL get_ipgrd( icell,1,k,jcell )

    IF( jcell == 0 )THEN
      jpuf = 0
      RETURN
    END IF

    xc = xc - FLOAT(ix)
    yc = yc - FLOAT(iy)

    xc = xc + xc
    yc = yc + yc

    ix = INT(xc)
    iy = INT(yc)

    icell = jcell + iy + iy + ix

  END DO

END IF

CALL get_ipgrd( icell,2,k,jpuf )

RETURN
END

!========================================================================

LOGICAL FUNCTION find_cell_samp( x,y,k,mlev,m0,n0,jpuf )

IMPLICIT NONE

REAL, PARAMETER :: EPS = 1.E-6

REAL,    INTENT( IN  ) :: x, y
INTEGER, INTENT( IN  ) :: k
INTEGER, INTENT( IN  ) :: mlev, m0, n0
INTEGER, INTENT( OUT ) :: jpuf

INTEGER icell, ix, iy, ilev, jcell
REAL    xc, yc

xc = MAX(0.,MIN(FLOAT(m0)-EPS,x))
yc = MAX(0.,MIN(FLOAT(n0)-EPS,y))

ix = INT(xc)
iy = INT(yc)
icell = iy*m0 + ix + 1

IF( mlev > 0 )THEN

  DO ilev = 1,mlev

    CALL get_ipgrd( icell,1,k,jcell )

    IF( jcell == 0 )THEN
      jpuf = 0
      find_cell_samp = .FALSE.
      RETURN
    END IF

    xc = xc - FLOAT(ix)
    yc = yc - FLOAT(iy)

    xc = xc + xc
    yc = yc + yc

    ix = INT(xc)
    iy = INT(yc)

    icell = jcell + iy + iy + ix

  END DO

END IF

CALL get_ipgrd( icell,2,k,jpuf )

find_cell_samp = .TRUE.

RETURN
END

!========================================================================

SUBROUTINE find_grid_cell( xx,yy,k,igrd,ip )

USE scipuff_fi

IMPLICIT NONE

REAL, PARAMETER :: EPS = 1.E-6

REAL,    INTENT( IN  ) :: xx, yy
INTEGER, INTENT( IN  ) :: k
INTEGER, INTENT( IN  ) :: igrd
INTEGER, INTENT( OUT ) :: ip

INTEGER ix, iy, ilev, jcell
REAL    xc, yc

xc = MAX(0.,MIN(FLOAT(nx)-EPS,xx))
yc = MAX(0.,MIN(FLOAT(ny)-EPS,yy))

ix = INT(xc)
iy = INT(yc)
ip = iy*nx + ix + 1

IF( igrd > 0 )THEN

  DO ilev = 1,igrd

    CALL get_ipgrd( ip,1,k,jcell )

    IF( jcell == 0 )THEN
      ip = 0
      RETURN
    END IF

    xc = xc - FLOAT(ix)
    yc = yc - FLOAT(iy)

    xc = xc + xc
    yc = yc + yc

    ix = INT(xc)
    iy = INT(yc)

    ip = jcell + iy + iy + ix

  END DO

END IF

RETURN
END

!===============================================================================

SUBROUTINE get_grid_cell( xx,yy,k,igrd,ip )

USE scipuff_fi

IMPLICIT NONE

REAL,    INTENT( IN    ) :: xx, yy
INTEGER, INTENT( IN    ) :: k
INTEGER, INTENT( INOUT ) :: igrd
INTEGER, INTENT( OUT   ) :: ip

LOGICAL lerr

INTEGER, EXTERNAL :: get_mxgrd, get_mxlev

CALL set_mxlev( k,MAX(get_mxlev(k),igrd) )

CALL get_cell( xx,yy,k,igrd,ip,nx,ny,lerr )

DO WHILE( lerr )
  CALL dezone_grid( k )
  IF( nError /= NO_ERROR )GOTO 9999
  igrd = MIN(igrd,get_mxgrd())
  CALL get_cell( xx,yy,k,igrd,ip,nx,ny,lerr )
END DO

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE get_cell( x,y,k,mlev,icell,m0,n0,lerr )

IMPLICIT NONE

REAL, PARAMETER :: EPS = 1.E-6

REAL,    INTENT( IN  ) :: x, y
INTEGER, INTENT( IN  ) :: k
INTEGER, INTENT( IN  ) :: mlev, m0, n0
INTEGER, INTENT( OUT ) :: icell
LOGICAL, INTENT( OUT ) :: lerr

INTEGER ix, iy, ilev, jcell
REAL    xc, yc

LOGICAL, EXTERNAL :: check_npgrd

lerr = .TRUE.

xc = MAX(0.,MIN(FLOAT(m0)-EPS,x))
yc = MAX(0.,MIN(FLOAT(n0)-EPS,y))

ix = INT(xc)
iy = INT(yc)
icell = iy*m0 + ix + 1

DO ilev = 1,mlev

  CALL get_ipgrd( icell,1,k,jcell )

  IF( jcell == 0 )THEN
    IF( check_npgrd(k) )GOTO 9999
    CALL build_cell( icell,k )
    CALL get_ipgrd( icell,1,k,jcell )
  END IF

  xc = xc - FLOAT(ix)
  yc = yc - FLOAT(iy)

  xc = xc + xc
  yc = yc + yc

  ix = INT(xc)
  iy = INT(yc)

  icell = jcell + iy + iy + ix

END DO

lerr = .FALSE.

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE build_cell( icell,k )

!  Refine cell number ICELL of the adaptive grid pointer list LGRID
!  NCELL is the total number of cells in the grid

IMPLICIT NONE

INTEGER, INTENT( IN ) :: icell, k

INTEGER ncell, ii

INTEGER, EXTERNAL :: get_npgrd

ncell = get_npgrd( k )

CALL set_ipgrd( icell,1,k,ncell+1 )

DO ii = 1,4
  CALL clr_ipgrd( ncell+ii,k )
END DO

CALL set_npgrd( k,ncell+4 )

RETURN
END

!===============================================================================

SUBROUTINE dezone_grid( k )

USE scipuff_fi
USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k

INTEGER jcell, jpuf, ncell, alloc_stat
INTEGER mxy, i, ix, iy, ip, icell0, nstart, iout, mlev, j, ipo

INTEGER, DIMENSION(:), ALLOCATABLE :: lgridp

INTEGER, EXTERNAL :: get_npgrd, get_mxlev

ALLOCATE( lgridp(MAXG/4),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  nError   = IV_ERROR
  eRoutine = 'dezone_grid'
  eMessage = 'Error allocating puff list arrays'
  WRITE(eInform,'(A,I12)') 'Size requested=',MAXG/4
  eAction  = 'Try reducing vertical resolution'
  GOTO 9999
END IF

ncell = get_npgrd( k )

!------ Set backward pointers

mxy = nx*ny - 3

DO i = 1,ncell
  CALL get_ipgrd( i,1,k,jcell )
  IF( jcell > 0 )THEN
    ip = (jcell-mxy)/4
    lgridp(ip) = i
  END IF
END DO

!------ Start on level-0 grid

DO ix = 1,nx
  DO iy = 1,ny

!------ Locate position on level-0

    icell0 = (iy-1)*nx + ix
    mlev   = 0

!------ Check for dezone

    CALL get_ipgrd( icell0,1,k,jcell )

    IF( jcell /= 0 )CALL check_grid_cell( icell0,k,lgridp,mxy,mlev )

  END DO
END DO

!------ Dezone grid

nstart = nx*ny + 1
iout   = nstart

DO i = nstart,ncell,4

  ip = (i - mxy)/4

  IF( lgridp(ip) /= 0 )THEN

    CALL set_ipgrd( lgridp(ip),1,k,iout )

    DO j = 0,3
      CALL mov_ipgrd( i+j,iout,k )
      IF( i+j > iout )THEN
        CALL set_ipgrd( i+j,2,k,0 )
        CALL get_ipgrd( iout,2,k,jpuf )
        IF( jpuf /= 0 )puff(jpuf)%iprv = -(1000*iout + k)
      END IF
      CALL get_ipgrd( iout,1,k,jcell )
      IF( jcell /= 0 )THEN
        ipo         = (jcell-mxy)/4
        lgridp(ipo) = iout
      END IF
      iout = iout + 1
    END DO

  END IF

END DO

CALL set_npgrd( k,iout-1 )
CALL set_mxlev( k,get_mxlev(k)-1 )
CALL set_mxgrd( get_mxlev(k) )

9999 CONTINUE

DEALLOCATE( lgridp,STAT=alloc_stat )

RETURN
END

!===============================================================================

RECURSIVE SUBROUTINE check_grid_cell( icell0,k,lgridp,mn0,mlev )

USE scipuff_fi

IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: icell0
INTEGER,               INTENT( IN    ) :: k
INTEGER,               INTENT( IN    ) :: mn0
INTEGER,               INTENT( IN    ) :: mlev
INTEGER, DIMENSION(*), INTENT( INOUT ) :: lgridp

INTEGER maxlev, i0, i, icell, ip, mlevn
INTEGER ipuf, jpuf, jpufn, jcell, kpuf

INTEGER, EXTERNAL :: get_mxlev, getPuffipgrd

!------ Initialize

maxlev = get_mxlev( k )
CALL get_ipgrd( icell0,1,k,i0 )

!------ Check the four sub-cells

mlevn = mlev + 1

DO i = 0,3

  icell = i0 + i
  CALL get_ipgrd( icell,1,k,jcell )

  IF( jcell /= 0 )CALL check_grid_cell( icell,k,lgridp,mn0,mlevn )

END DO

!------ If at level max lev, remove 4 cells; otherwise return

IF( mlevn < maxlev )RETURN

CALL set_ipgrd( icell0,1,k,0 )
ip         = (i0 - mn0)/4
lgridp(ip) = 0

CALL get_ipgrd( icell0,2,k,jpufn )
DO WHILE( jpufn /= 0 )
  jpuf  = jpufn
  jpufn = puff(jpuf)%inxt
END DO

DO i = 0,3

  icell = i0 + i
  CALL get_ipgrd( icell,2,k,ipuf )

  IF( ipuf /= 0 )THEN
    CALL set_ipgrd( icell,2,k,0 )
    CALL get_ipgrd( icell0,2,k,kpuf )
    IF( kpuf == 0 )THEN
      CALL set_ipgrd( icell0,2,k,ipuf )
      puff(ipuf)%iprv = -1000*(icell0-icell) + puff(ipuf)%iprv
    ELSE
      puff(jpuf)%inxt = ipuf
      puff(ipuf)%iprv = jpuf
    END IF

    jpufn = ipuf
    DO WHILE( jpufn /= 0 )
      CALL setPuffipgrd( puff(jpufn),getPuffipgrd(puff(jpufn))-1 )
      jpuf = jpufn
      jpufn = puff(jpuf)%inxt
    END DO

  END IF

END DO

RETURN
END

!===============================================================================

SUBROUTINE remove_ipgrd( ipuf )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ipuf

INTEGER iprv, inxt, jprv, ip, k

iprv = puff(ipuf)%iprv
inxt = puff(ipuf)%inxt

IF( iprv > 0 )THEN
  puff(iprv)%inxt = inxt
ELSE IF( iprv < 0 )THEN
  jprv = ABS(iprv)
  ip   = jprv/1000
  k    = jprv - 1000*ip
  CALL set_ipgrd( ip,2,k,inxt )
END IF

IF( inxt > 0 )puff(inxt)%iprv = iprv

RETURN
END

!===============================================================================

SUBROUTINE find_mxgrd( nz )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nz

INTEGER mxlev,mxcell,k

INTEGER, EXTERNAL :: get_mxlev, get_npgrd, get_mxgrd, max_grd

mxlev  = 0
mxcell = 0

DO k = 1,nz
  mxlev  = MAX(get_mxlev(k),mxlev)
  mxcell = MAX(get_npgrd(k),mxcell)
END DO

IF( mxcell < max_grd()/4 )CALL set_mxgrd( MAX(get_mxgrd(),mxlev+1) )

RETURN
END

!===============================================================================

INTEGER FUNCTION grid_mxlev( nz )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nz

INTEGER mxlev, k

INTEGER, EXTERNAL :: get_mxlev

mxlev = 0

DO k = 1,nz
  mxlev = MAX(get_mxlev(k),mxlev)
END DO

grid_mxlev = mxlev

RETURN
END

!===============================================================================

INTEGER FUNCTION max_vres()

USE ipgrd_fi

IMPLICIT NONE

max_vres = MAXGZ

RETURN
END

!===============================================================================

INTEGER FUNCTION max_grd()

USE ipgrd_fi

IMPLICIT NONE

max_grd = MAXG

RETURN
END

!===============================================================================

SUBROUTINE set_mxgrd( ival )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: ival

mxgrd = ival

RETURN
END

!===============================================================================

INTEGER FUNCTION get_mxgrd()

USE ipgrd_fi

IMPLICIT NONE

get_mxgrd = mxgrd

RETURN
END

!===============================================================================

INTEGER FUNCTION get_mxlev( k )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k

get_mxlev = mxlev_grd( k )

RETURN
END

!===============================================================================

SUBROUTINE set_mxlev( k,mxlev )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k,mxlev

mxlev_grd(k) = mxlev

RETURN
END

!===============================================================================

INTEGER FUNCTION get_npgrd( k )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k

get_npgrd = npgrd(k)

RETURN
END

!===============================================================================

SUBROUTINE set_npgrd( k,igrd )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k,igrd

npgrd(k) = igrd

RETURN
END

!===============================================================================

SUBROUTINE clr_ipgrd( i,k )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i,k

ipgrd(i,1,k) = 0
ipgrd(i,2,k) = 0

RETURN
END

!===============================================================================

SUBROUTINE mov_ipgrd( i,j,k )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i,j,k

ipgrd(j,1,k) = ipgrd(i,1,k)
ipgrd(j,2,k) = ipgrd(i,2,k)

RETURN
END

!===============================================================================

SUBROUTINE set_ipgrd( i,j,k,ival )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: i,j,k,ival

ipgrd(i,j,k) = ival

RETURN
END

!===============================================================================

SUBROUTINE get_ipgrd( i,j,k,ival )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: i,j,k
INTEGER, INTENT( OUT ) :: ival

ival = ipgrd(i,j,k)

RETURN
END

!===============================================================================

LOGICAL FUNCTION check_npgrd( k )

USE ipgrd_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: k

check_npgrd =  npgrd(k)+4 > MAXG

RETURN
END

!===============================================================================

INTEGER FUNCTION getPuffiskew( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

getPuffiskew = IBITS(p%ipgd,POS_IPSKEW,LEN_IPSKEW)

RETURN
END

!===============================================================================

SUBROUTINE setPuffiskew( p,i )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_IPSKEW,p%ipgd,POS_IPSKEW)

RETURN
END

!===============================================================================

INTEGER FUNCTION getPuffipgrd( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

getPuffipgrd = IBITS(p%ipgd,POS_IPGRID,LEN_IPGRID)

RETURN
END

!===============================================================================

SUBROUTINE setPuffipgrd( p,i )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_IPGRID,p%ipgd,POS_IPGRID)

RETURN
END

!===============================================================================

INTEGER FUNCTION getPuffifld( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

getPuffifld = IBITS(p%ipgd,POS_IFIELD,LEN_IFIELD)

RETURN
END

!===============================================================================

SUBROUTINE setPuffifld( p,i )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_IFIELD,p%ipgd,POS_IFIELD)

RETURN
END

!===============================================================================

INTEGER FUNCTION getPuffifld_2000( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

getPuffifld_2000 = IBITS(p%ipgd,16,16)

RETURN
END

!===============================================================================

INTEGER FUNCTION getPuffirel( p )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN ) :: p

getPuffirel = IBITS(p%ipgd,POS_IDREL,LEN_IDREL)

RETURN
END

!===============================================================================

SUBROUTINE setPuffirel( p,i )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: p
INTEGER,          INTENT( IN    ) :: i

CALL MVBITS(i,0,LEN_IDREL,p%ipgd,POS_IDREL)

RETURN
END
