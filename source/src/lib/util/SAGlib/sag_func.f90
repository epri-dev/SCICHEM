!*******************************************************************************
!               SAG_BottomFunctionID
!*******************************************************************************
INTEGER FUNCTION SAG_BottomFunctionID( grdI,UserFunction,nfld,ifld,restore,lThreadsafe )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: grdI
INTEGER,               EXTERNAL     :: UserFunction
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
LOGICAL,               INTENT( IN ) :: restore
LOGICAL,               INTENT( IN ) :: lThreadSafe !Should be FALSE if UserFunction is not Threadsafe

TYPE ( SAGgrid_str ), POINTER :: grd

INTERFACE
  INTEGER FUNCTION SAG_BottomFunction( grd,UserFunction,nfld,ifld,restore,lThreadSafe )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
    INTEGER,               EXTERNAL     :: UserFunction
    INTEGER,               INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
    LOGICAL,               INTENT( IN ) :: restore
    LOGICAL,               INTENT( IN ) :: lThreadSafe !Should be FALSE if UserFunction is not Threadsafe
  END FUNCTION
END INTERFACE

!==== Initialize result

SAG_BottomFunctionID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED( grd ))THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_BottomFunctionID = SAG_BottomFunction( grd,UserFunction,nfld,ifld,restore,lThreadSafe )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_BottomFunction( grd,UserFunction,nfld,ifld,restore,lThreadSafe )
!DIR$ OPTIMIZE:0
USE sagdef_fd
USE sagstr_fd
USE sagcel_fd
USE sagpop_fi
USE omp_lib

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER      :: grd
INTEGER,               EXTERNAL     :: UserFunction
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
LOGICAL,               INTENT( IN ) :: restore
LOGICAL,               INTENT( IN ) :: lThreadSafe !Indicates if UserFunction is Threadsafe

TYPE( SAGcell_str ) p0

INTEGER i,j,ix,iy,icell,jfld
INTEGER ompError

LOGICAL doThreads

INTERFACE
  INTEGER FUNCTION SAG_BackUpData( grd,nfld,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ),   POINTER      :: grd
    INTEGER,               INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
  END FUNCTION SAG_BackUpData

  INTEGER FUNCTION SAG_RestoreData( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_RestoreData

  INTEGER FUNCTION SAG_BottomValue( grd,ifld )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    INTEGER,             INTENT( IN ) :: ifld
  END FUNCTION SAG_BottomValue

  RECURSIVE INTEGER FUNCTION SAG_BottomWalk( grd,p0,UserFunction )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    TYPE( SAGcell_str ), INTENT( IN ) :: p0
    INTEGER, EXTERNAL                 :: UserFunction
  END FUNCTION SAG_BottomWalk
END INTERFACE

!==== Initialize result

SAG_BottomFunction = SAG_ERROR

!==== Back Up data if requested

IF( restore )THEN
  i = SAG_BackUpData( grd,nfld,ifld )
  IF( i /= SAG_OK )THEN
    SAG_BottomFunction = i
    GOTO 9999
  END IF
END IF

!==== Push data to the bottom grid

DO j = 1,nfld
  jfld = ifld(j)
  i = SAG_BottomValue( grd,jfld )
  IF( i /= SAG_OK )THEN
    SAG_BottomFunction = i
    GOTO 9998
  END IF
END DO

!==== Walk the grid

doThreads = lThreadSafe .AND. OMP_GET_MAX_THREADS() > 1

ompError = SAG_OK
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ix,iy,icell,p0) IF(doThreads)
!$OMP SINGLE
DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell = (iy-1)*grd%nx + ix

    p0%id  = icell
    p0%x   = FLOAT(ix) - 0.5
    p0%y   = FLOAT(iy) - 0.5
    p0%hx  = 1.0
    p0%hy  = 1.0
    p0%lev = 0

!$OMP TASK DEFAULT(SHARED) FIRSTPRIVATE(p0) PRIVATE(i)
    IF( ompError == SAG_OK )THEN
    threadID = OMP_GET_THREAD_NUM()
    i = SAG_BottomWalk( grd,p0,UserFunction )
    IF( i /= SAG_OK )THEN
!$OMP ATOMIC WRITE
      ompError = i
    END IF
    END IF
!$OMP END TASK

  END DO
END DO
!$OMP END SINGLE
!$OMP END PARALLEL
IF( ompError /= SAG_OK )THEN
  SAG_BottomFunction = ompError
  GOTO 9998
END IF

!==== Set Return value

SAG_BottomFunction = SAG_OK

!==== Restore data if requested

9998 CONTINUE

IF( restore )THEN
  i = SAG_RestoreData( grd )
  IF( i /= SAG_OK )THEN
    SAG_BottomFunction = i
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_BottomWalk
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_BottomWalk( grd,p0,UserFunction ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
TYPE( SAGcell_str ), INTENT( IN ) :: p0
INTERFACE
  INTEGER FUNCTION UserFunction( dat,mx,p )
    USE sagcel_fd
    INTEGER,             INTENT( IN ) :: mx
    REAL, DIMENSION(:),  POINTER      :: dat
    TYPE( SAGcell_str ), INTENT( IN ) :: p
  END FUNCTION UserFunction
END INTERFACE

TYPE( SAGcell_str ) p
INTEGER ipt,nrfm,ix,iy
REAL    rfx,rfy,xx,yy

INTEGER, POINTER, DIMENSION(:) :: igrd

irv = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
    rfx  = 0.5
    rfy  = 0.5
  CASE( SAG_GRID_HORZ )
    nrfm = 1
    rfx  = 0.5
    rfy  = 1.0
  CASE( SAG_GRID_VERT )
    nrfm = 1
    rfx  = 1.0
    rfy  = 0.5
  CASE( SAG_GRID_NONE )
    nrfm = -1
    rfx  = 1.0
    rfy  = 1.0
  CASE DEFAULT
    GOTO 9999
END SELECT

IF( igrd(p0%id) /= 0 )THEN
  DO ipt = 0,nrfm

    iy = ipt/NINT(1./rfx)
    ix = ipt - 2*iy
    xx = (FLOAT(ix)-0.5)*(1.-rfx)*p0%hx
    yy = (FLOAT(iy)-0.5)*(1.-rfy)*p0%hy

    p%id  = igrd(p0%id) + ipt
    p%x   = p0%x + xx
    p%y   = p0%y + yy
    p%hx  = rfx*p0%hx
    p%hy  = rfy*p0%hy
    p%lev = p0%lev + 1

    irv = SAG_BottomWalk( grd,p,UserFunction )
    IF( irv /= SAG_OK )GOTO 9999
  END DO
ELSE
  irv = UserFunction( grd%ipdat,grd%mxgrd,p0 ) !UserFunction(grd,p0)
  IF( irv /= SAG_OK )LastError = SAG_ERR_USER
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_MaxValue
!*******************************************************************************
INTEGER FUNCTION SAG_MaxValue( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE saggrd_fi
USE sagmaxval_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

REAL value

value = dat((ifld_maxval-1)*mxgrd + p0%id)

IF( UseSpecial )THEN
  IF( value /= Special )THEN
!$OMP ATOMIC UPDATE
    out_maxval = MAX(out_maxval,value)
!$OMP ATOMIC UPDATE
    out_minval = MIN(out_minval,value)
  END IF
ELSE
!$OMP ATOMIC UPDATE
  out_maxval = MAX(out_maxval,value)
!$OMP ATOMIC UPDATE
  out_minval = MIN(out_minval,value)
END IF

SAG_MaxValue = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_Add
!*******************************************************************************
INTEGER FUNCTION SAG_Add( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE sagadd_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

INTEGER i, j

i = (ifld_add-1)*mxgrd + p0%id
j = (jfld_add-1)*mxgrd + p0%id

dat(i) = dat(i) + scale_add*dat(j)

SAG_Add = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_AddVariance
!*******************************************************************************
INTEGER FUNCTION SAG_AddVariance( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE sagadd_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

INTEGER i, j

i = (ifld_add-1)*mxgrd + p0%id
j = (jfld_add-1)*mxgrd + p0%id

dat(i) = (SQRT(dat(i)) + SIGN(1.,scale_add)*SQRT(ABS(scale_add)*dat(j)))**2

SAG_AddVariance = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_AddLimit
!*******************************************************************************
INTEGER FUNCTION SAG_AddLimit( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE sagadd_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

INTEGER i, j

i = (ifld_add-1)*mxgrd + p0%id
j = (jfld_add-1)*mxgrd + p0%id

dat(i) = dat(i) + scale_add*dat(j)

IF( min_value /= NOT_USED )dat(i) = MAX(dat(i),min_value)
IF( max_value /= NOT_USED )dat(i) = MIN(dat(i),max_value)

SAG_AddLimit = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_AddVariance
!*******************************************************************************
INTEGER FUNCTION SAG_AddVarianceLimit( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE sagadd_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

INTEGER i, j

i = (ifld_add-1)*mxgrd + p0%id
j = (jfld_add-1)*mxgrd + p0%id

dat(i) = (SQRT(dat(i)) + SIGN(1.,scale_add)*SQRT(ABS(scale_add)*dat(j)))**2

IF( min_value /= NOT_USED )dat(i) = MAX(dat(i),min_value)
IF( max_value /= NOT_USED )dat(i) = MIN(dat(i),max_value)

SAG_AddVarianceLimit = SAG_OK

RETURN
END

!*******************************************************************************
!               SAG_Copy
!*******************************************************************************
INTEGER FUNCTION SAG_Copy( dat,mxgrd,p0 )

USE sagdef_fd
USE sagcel_fd
USE sagadd_usr

IMPLICIT NONE

REAL,   DIMENSION(:), POINTER      :: dat
INTEGER,              INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ),  INTENT( IN ) :: p0

INTEGER i, j

i = (ifld_add-1)*mxgrd + p0%id
j = (jfld_add-1)*mxgrd + p0%id

dat(i) = dat(j)

SAG_Copy = SAG_OK

RETURN
END
!*******************************************************************************
!               SAG_CellFunction
!*******************************************************************************
INTEGER FUNCTION SAG_CellFunction( grd,UserFunction )

USE sagdef_fd
USE sagstr_fd
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd
INTERFACE
  INTEGER FUNCTION UserFunction( dat,mx,p )
    USE sagcel_fd
    INTEGER,             INTENT( IN ) :: mx
    REAL, DIMENSION(:),  POINTER      :: dat
    TYPE( SAGcell_str ), INTENT( IN ) :: p
  END FUNCTION UserFunction
END INTERFACE

TYPE( SAGcell_str ) p0

INTEGER icell

!==== Initialize result

SAG_CellFunction = SAG_ERROR

!==== Loop over all cells

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(icell,p0)
DO icell = 1,grd%ncells
  p0%id = icell
  IF( UserFunction(grd%ipdat,grd%mxgrd,p0) /= SAG_OK )CYCLE
END DO
!$OMP END PARALLEL DO

!==== Set Return value

SAG_CellFunction = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_TriangleFunction
!*******************************************************************************
INTEGER FUNCTION SAG_TriangleFunction( triT,UserFunction,UserDisplay,grd,use8 )

USE sagdef_fd
USE sagerr_fd
USE sagnod_fd
USE sagtri_fd
USE sagerr_fi
USE sagstr_fd

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT
INTERFACE
  INTEGER FUNCTION UserFunction( nid,node,dat,nfld,UserDisplay,grd,use8 )
    USE sagnod_fd
    USE sagstr_fd
    INTEGER,                           INTENT( IN ) :: nfld
    INTEGER,             DIMENSION(3), INTENT( IN ) :: nid
    TYPE( SAGnode_str ), DIMENSION(:), POINTER      :: node
    REAL,                DIMENSION(:), POINTER      :: dat
    INTEGER, EXTERNAL :: UserDisplay
    TYPE( SAGgrid_str ), POINTER  :: grd
    LOGICAL, INTENT( IN ) :: use8
 END FUNCTION UserFunction
END INTERFACE
INTEGER, EXTERNAL :: UserDisplay
 LOGICAL, INTENT( IN ) :: use8

TYPE( SAGgrid_str ),                   POINTER :: grd
TYPE( SAGnode_str ),     DIMENSION(:), POINTER :: node
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri
REAL,                    DIMENSION(:), POINTER :: dat

INTEGER i,nfld,irv

!==== Initialize result

SAG_TriangleFunction = SAG_ERROR

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipnode) )THEN
  LastError = SAG_ERR_IPNODE
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipdata) )THEN
  LastError = SAG_ERR_IPDATA
  LastParm  = 0
  GOTO 9999
END IF

IF( triT%ntri <= 0 )THEN
  LastError = SAG_ERR_NOTRI
  LastParm  = 0
  GOTO 9998
END IF

IF( triT%nodeT%mxdata <= 0 )THEN
  LastError = SAG_ERR_NODATA
  LastParm  = 0
  GOTO 9998
END IF

!==== Loop over all triangles

nfld =  triT%nodeT%mxdata
node => triT%nodeT%ipnode
dat  => triT%nodeT%ipdata
tri  => triT%iptri

DO i = 1,triT%ntri
  irv = UserFunction(tri(i)%nid,node,dat,nfld,UserDisplay,grd,use8)
  IF( irv /= SAG_OK )GOTO 9999
END DO

!==== Set Return value

9998 CONTINUE

SAG_TriangleFunction = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_GridFunction
!*******************************************************************************
INTEGER FUNCTION SAG_GridFunction( grd,UserFunction,UserDisplay )

USE sagdef_fd
USE sagstr_fd
USE sagcel_fd

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER :: grd
INTEGER,            EXTERNAL :: UserFunction
INTEGER,            EXTERNAL :: UserDisplay

TYPE( SAGcell_str ) p0

INTEGER i,ix,iy,icell

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_GridWalk( grd,p0,UserFunction,UserDisplay )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    TYPE( SAGcell_str ), INTENT( IN ) :: p0
    INTEGER,             EXTERNAL     :: UserFunction
    INTEGER,             EXTERNAL     :: UserDisplay
  END FUNCTION SAG_GridWalk
END INTERFACE

!==== Initialize result

SAG_GridFunction = SAG_ERROR

!==== Walk the grid

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    icell = (iy-1)*grd%nx + ix

    p0%id  = icell
    p0%x   = FLOAT(ix) - 0.5
    p0%y   = FLOAT(iy) - 0.5
    p0%hx  = 1.0
    p0%hy  = 1.0
    p0%lev = 0

    i = SAG_GridWalk( grd,p0,UserFunction,UserDisplay )
    IF( i /= SAG_OK )THEN
      SAG_GridFunction = i
      GOTO 9999
    END IF

  END DO
END DO

!==== Set Return value

SAG_GridFunction = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_GridWalk
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_GridWalk( grd,p0,UserFunction,UserDisplay ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
TYPE( SAGcell_str ), INTENT( IN ) :: p0

INTERFACE
  INTEGER FUNCTION UserFunction( grd,p0,UserDraw )
    USE sagstr_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ), POINTER      :: grd
    TYPE( SAGcell_str ), INTENT( IN ) :: p0
    INTEGER,             EXTERNAL     :: UserDraw
  END FUNCTION UserFunction
END INTERFACE

INTEGER, EXTERNAL :: UserDisplay

TYPE( SAGcell_str ) p

INTEGER ipt,nrfm,ix,iy
REAL    rfx,rfy,xx,yy

INTEGER, DIMENSION(:), POINTER :: igrd

irv = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    nrfm = 3
    rfx  = 0.5
    rfy  = 0.5
  CASE( SAG_GRID_HORZ )
    nrfm = 1
    rfx  = 0.5
    rfy  = 1.0
  CASE( SAG_GRID_VERT )
    nrfm = 1
    rfx  = 1.0
    rfy  = 0.5
  CASE( SAG_GRID_NONE )
    nrfm = -1
    rfx  = 1.0
    rfy  = 1.0
  CASE DEFAULT
    GOTO 9999
END SELECT

irv = UserFunction( grd,p0,UserDisplay )
IF( irv /= SAG_OK )THEN
  LastError = SAG_ERR_USER
  GOTO 9999
END IF

IF( igrd(p0%id) /= 0 )THEN
  DO ipt = 0,nrfm

    iy = ipt/NINT(1./rfx)
    ix = ipt - 2*iy
    xx = (FLOAT(ix)-0.5)*(1.-rfx)*p0%hx
    yy = (FLOAT(iy)-0.5)*(1.-rfy)*p0%hy

    p%id  = igrd(p0%id) + ipt
    p%x   = p0%x + xx
    p%y   = p0%y + yy
    p%hx  = rfx*p0%hx
    p%hy  = rfy*p0%hy
    p%lev = p0%lev + 1

    irv = SAG_GridWalk( grd,p,UserFunction,UserDisplay )
    IF( irv /= SAG_OK )GOTO 9999
  END DO
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_CellDraw
!*******************************************************************************
INTEGER FUNCTION SAG_CellDraw( grd,p0,UserDraw )

USE sagdef_fd
USE sagstr_fd
USE sagcel_fd
USE sagerr_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ), POINTER      :: grd
TYPE( SAGcell_str ), INTENT( IN ) :: p0
INTEGER,             EXTERNAL     :: UserDraw

INTEGER  npoly,irv
REAL, DIMENSION(5) :: xpoly,ypoly

!==== Initialize result

SAG_CellDraw = SAG_ERROR

npoly = 5

xpoly(1) = grd%xmin + grd%dx*(p0%x - 0.5*p0%hx)
ypoly(1) = grd%ymin + grd%dy*(p0%y - 0.5*p0%hy)

xpoly(2) = xpoly(1)
ypoly(2) = grd%ymin + grd%dy*(p0%y + 0.5*p0%hy)

xpoly(3) = grd%xmin + grd%dx*(p0%x + 0.5*p0%hx)
ypoly(3) = ypoly(2)

xpoly(4) = xpoly(3)
ypoly(4) = ypoly(1)

xpoly(5) = xpoly(1)
ypoly(5) = ypoly(1)

irv = UserDraw( npoly,xpoly,ypoly,0 )
IF( irv /= SAG_OK )THEN
  LastError = SAG_ERR_USER
  GOTO 9999
END IF

SAG_CellDraw = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_TriangleDraw
!*******************************************************************************
INTEGER FUNCTION SAG_TriangleDraw( id,tri_node,tri_data,nfld,UserDraw,grd )

USE sagdef_fd
USE sagnod_fd
USE sagfil_fd
USE sagerr_fd
USE sagerr_fi
USE sagstr_fd

!     Fill a triangle

IMPLICIT NONE

INTEGER,             DIMENSION(3), INTENT( IN ) :: id
TYPE( SAGnode_str ), DIMENSION(:), POINTER      :: tri_node
REAL ,               DIMENSION(:), POINTER      :: tri_data
INTEGER,                           INTENT( IN ) :: nfld
INTEGER,                           EXTERNAL     :: UserDraw
TYPE ( SAGgrid_str ),              POINTER      :: grd

INTEGER npoly,irv
REAL, DIMENSION(4) :: xpoly,ypoly

!==== Initialize

SAG_TriangleDraw = SAG_ERROR

!==== Set Polygon to entire triangle

xpoly(1) = grd%xmin + grd%dx*tri_node(id(1))%x
ypoly(1) = grd%ymin + grd%dy*tri_node(id(1))%y
xpoly(2) = grd%xmin + grd%dx*tri_node(id(2))%x
ypoly(2) = grd%ymin + grd%dy*tri_node(id(2))%y
xpoly(3) = grd%xmin + grd%dx*tri_node(id(3))%x
ypoly(3) = grd%ymin + grd%dy*tri_node(id(3))%y
xpoly(4) = xpoly(1)
ypoly(4) = ypoly(1)
npoly = 4

irv = UserDraw( npoly,xpoly,ypoly,0 )
IF( irv /= SAG_OK )THEN
  LastError = SAG_ERR_USER
  GOTO 9999
END IF

SAG_TriangleDraw = SAG_OK

9999 CONTINUE

RETURN
END
