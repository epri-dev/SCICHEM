!*******************************************************************************
!                SAG_InitContour
!*******************************************************************************
SUBROUTINE SAG_InitContour( contour,ifld,contour_value,log_interp,lclose )

USE sagcnt_fd

!     Initialize a contour structure

IMPLICIT NONE

TYPE( SAGcontour_str ), INTENT( INOUT ) :: contour
INTEGER,                INTENT( IN    ) :: ifld
REAL,                   INTENT( IN    ) :: contour_value
LOGICAL,                INTENT( IN    ) :: log_interp
LOGICAL,                INTENT( IN    ) :: lclose

contour%ifld       = ifld
contour%value      = contour_value
contour%log_interp = log_interp
contour%lclose     = lclose
contour%start      = .FALSE.

contour%npts = 0
contour%nlns = 0

contour%start_node = -1
contour%last_node  = -1
contour%last_tri   = -1

CALL SAG_FreeContour( contour )

RETURN
END
!*******************************************************************************
!                SAG_InitContourDraw
!*******************************************************************************
SUBROUTINE SAG_InitContourDraw( contour,draw,trifld,ilevel )

USE sagcnt_fd
USE sagdrw_fd

!     Initialize a contour structure from a draw structure

IMPLICIT NONE

TYPE( SAGcontour_str ), INTENT( INOUT ) :: contour
TYPE( SAGdrawfld_str ), INTENT( IN    ) :: draw
INTEGER,                INTENT( IN    ) :: trifld
INTEGER,                INTENT( IN    ) :: ilevel

REAL, POINTER, DIMENSION(:) :: level

level => draw%iplev

contour%ifld       = trifld
contour%value      = level(ilevel)
contour%level      = draw%nlev - ilevel + 1
contour%log_interp = draw%log_interp
contour%lclose     = draw%lclose
contour%start      = .FALSE.

contour%npts = 0
contour%nlns = 0

contour%start_node = -1
contour%last_node  = -1
contour%last_tri   = -1

CALL SAG_FreeContour( contour )

RETURN
END
!*******************************************************************************
!                SAG_InitContourWrite
!*******************************************************************************
SUBROUTINE SAG_InitContourWrite( contour,uwrite,trifld,ilevel )

USE sagcnt_fd
USE sagwrt_fd

!     Initialize a contour structure from a write structure

IMPLICIT NONE

TYPE( SAGcontour_str ),  INTENT( INOUT ) :: contour
TYPE( SAGwritefld_str ), INTENT( IN    ) :: uwrite
INTEGER,                 INTENT( IN    ) :: trifld
INTEGER,                 INTENT( IN    ) :: ilevel

REAL, POINTER, DIMENSION(:) :: level

level => uwrite%iplev

contour%ifld       = trifld
contour%level      = uwrite%nlev - ilevel + 1
contour%value      = level(ilevel)
contour%log_interp = uwrite%log_interp
contour%lclose     = uwrite%lclose
contour%start      = .FALSE.

contour%npts = 0
contour%nlns = 0

contour%start_node = -1
contour%last_node  = -1
contour%last_tri   = -1

CALL SAG_FreeContour( contour )

RETURN
END
!*******************************************************************************
!                SAG_FreeContour
!*******************************************************************************
SUBROUTINE SAG_FreeContour( contour )

USE sagcnt_fd

!     Free data storage space in a contour structure

IMPLICIT NONE

TYPE( SAGcontour_str ), INTENT( INOUT ) :: contour

INTEGER alloc_stat

IF( ASSOCIATED(contour%ippts) )DEALLOCATE( contour%ippts,STAT=alloc_stat )
IF( ASSOCIATED(contour%iplns) )DEALLOCATE( contour%iplns,STAT=alloc_stat )

NULLIFY( contour%ippts )
NULLIFY( contour%iplns )

contour%mxpts = 0
contour%mxlns = 0

RETURN
END
!*******************************************************************************
!                SAG_AllocateContour
!*******************************************************************************
INTEGER FUNCTION SAG_AllocateContour( contour,mxpts,mxlns )

USE sagdef_fd
USE sagerr_fd
USE sagcnt_fd
USE sagerr_fi

!     Free data storage space in a contour structure

IMPLICIT NONE

TYPE( SAGcontour_str ), INTENT( INOUT ) :: contour
INTEGER,                INTENT( IN    ) :: mxpts,mxlns

INTEGER ios

SAG_AllocateContour = SAG_ERROR

CALL SAG_FreeContour( contour )

IF( mxpts > 0 )THEN
  ALLOCATE( contour%ippts(mxpts),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxpts
    GOTO 9999
  END IF
END IF

IF( mxlns > 0 )THEN
  ALLOCATE( contour%iplns(mxlns),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxlns
    IF( ASSOCIATED(contour%ippts) )DEALLOCATE( contour%ippts,STAT=ios )
    GOTO 9999
  END IF
END IF

contour%mxpts = mxpts
contour%mxlns = mxlns

SAG_AllocateContour = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_BuildContourID
!*******************************************************************************
INTEGER FUNCTION SAG_BuildContourID( grdI,contourI )

USE PtrGrdStrItf
USE sagtri_fd
USE sagcnt_fd
USE sagerr_fd
USE sagdef_fd
USE sagerr_fi

IMPLICIT NONE

INTEGER,                INTENT( IN )    :: grdI
TYPE( SAGcontour_str ), INTENT( INOUT ) :: contourI

TYPE( SAGTriangleT_str ), POINTER :: tri

INTERFACE
  INTEGER FUNCTION SAG_BuildContour( triT,contourI )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ), POINTER         :: triT
    TYPE( SAGcontour_str ), TARGET,   INTENT( INOUT ) :: contourI
  END FUNCTION SAG_BuildContour
END INTERFACE

SAG_BuildContourID = SAG_ERROR

!==== Point to structure

tri => SAG_PtrTriStr( grdI )
IF( .NOT.ASSOCIATED(tri) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_BuildContourID = SAG_BuildContour( tri,contourI )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_BuildContour
!*******************************************************************************
INTEGER FUNCTION SAG_BuildContour( triT,contourI )

USE sagdef_fd
USE sagtri_fd
USE sagcnt_fd
USE sagcnt_usr

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGtriangleT_str ),       POINTER         :: triT
TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI

INTEGER mxpts,mxlns,j

INTEGER, EXTERNAL :: SAG_AllocateContour
INTEGER, EXTERNAL :: SAG_ContourAddPoint

INTERFACE
  INTEGER FUNCTION SAG_CountContour( triT,contourI )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ),       POINTER         :: triT
    TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI
  END FUNCTION SAG_CountContour

  INTEGER FUNCTION SAG_TraceContour( triT,contour,UserFunction )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ), POINTER         :: triT
    TYPE( SAGcontour_str ),   INTENT( INOUT ) :: contour
    INTEGER, EXTERNAL                         :: UserFunction
  END FUNCTION SAG_TraceContour
END INTERFACE

SAG_BuildContour = SAG_ERROR

contour => contourI

IF( .NOT.ASSOCIATED(contour%ippts) .OR. .NOT.ASSOCIATED(contour%iplns) )THEN
  IF( contour%npts == 0 .OR. contour%nlns == 0 )THEN
    j = SAG_CountContour( triT,contourI )
    IF( j /= SAG_OK )GOTO 9999
  END IF
  mxpts = contour%npts
  mxlns = contour%nlns
  j = SAG_AllocateContour( contourI,mxpts,mxlns )
  IF( j /= SAG_OK )GOTO 9999
END IF

contour%npts = 0
contour%nlns = 0

SAG_BuildContour = SAG_TraceContour( triT,contourI,SAG_ContourAddPoint )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CountContourID
!*******************************************************************************
INTEGER FUNCTION SAG_CountContourID( grdI,contourI )

USE PtrGrdStrItf
USE sagtri_fd
USE sagcnt_fd
USE sagerr_fd
USE sagerr_fi
USE sagdef_fd

IMPLICIT NONE

INTEGER,                INTENT( IN )    :: grdI
TYPE( SAGcontour_str ), INTENT( INOUT ) :: contourI

TYPE( SAGTriangleT_str ), POINTER :: tri

INTERFACE
  INTEGER FUNCTION SAG_CountContour( triT,contourI )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ),       POINTER         :: triT
    TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI
  END FUNCTION SAG_CountContour
END INTERFACE

SAG_CountContourID = SAG_ERROR

!==== Point to structure

tri => SAG_PtrTriStr( grdI )
IF( .NOT.ASSOCIATED(tri) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_CountContourID = SAG_CountContour( tri,contourI )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CountContour
!*******************************************************************************
INTEGER FUNCTION SAG_CountContour( triT,contourI )

USE sagdef_fd
USE sagtri_fd
USE sagcnt_fd
USE sagcnt_usr

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGtriangleT_str ),       POINTER         :: triT
TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI

INTEGER, EXTERNAL :: SAG_ContourCount

INTERFACE
  INTEGER FUNCTION SAG_TraceContour( triT,contour,UserFunction )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ), POINTER         :: triT
    TYPE( SAGcontour_str ),   INTENT( INOUT ) :: contour
    INTEGER, EXTERNAL                         :: UserFunction
  END FUNCTION SAG_TraceContour
END INTERFACE

contour => contourI

contour%npts = 0
contour%nlns = 0

SAG_CountContour = SAG_TraceContour( triT,contourI,SAG_ContourCount )

IF( SAG_CountContour /= SAG_OK )THEN
  contour%npts = 0
  contour%nlns = 0
END IF

RETURN
END
!*******************************************************************************
!                SAG_TraceContour
!*******************************************************************************
INTEGER FUNCTION SAG_TraceContour( triT,contour,UserFunction )

USE sagdef_fd
USE sagtri_fd
USE sagerr_fd
USE sagcnt_fd
USE sagerr_fi
USE saggrd_fi

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER         :: triT
TYPE( SAGcontour_str ),   INTENT( INOUT ) :: contour
INTEGER, EXTERNAL                         :: UserFunction

REAL,                    POINTER, DIMENSION(:) :: dat
TYPE( SAGnode_str ),     POINTER, DIMENSION(:) :: node
TYPE( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri
TYPE( SAGedgenode_str ), POINTER, DIMENSION(:) :: enode

LOGICAL, ALLOCATABLE, DIMENSION(:) :: test_edge
LOGICAL, ALLOCATABLE, DIMENSION(:) :: test_tri

INTEGER i,nedge,ii,nfld,is,ie,inc,je,jt,j,ios
REAL    vmax,vmin
LOGICAL ledge,found,first_edge,has_edge

INTERFACE
  LOGICAL FUNCTION SAG_HasContour(contour,nid,dat,nfld)
    USE sagcnt_fd
    TYPE( SAGcontour_str ), INTENT( IN ) :: contour
    INTEGER, DIMENSION(3),  INTENT( IN ) :: nid
    INTEGER,                INTENT( IN ) :: nfld
    REAL, DIMENSION(:),     POINTER      :: dat
  END FUNCTION SAG_HasContour

  INTEGER FUNCTION SAG_Contour( triT,contour,test_tri,jt0,sid,ledge,UserFunction )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ), POINTER         :: triT
    TYPE( SAGcontour_str ),   INTENT( INOUT ) :: contour
    LOGICAL, DIMENSION(*),    INTENT( INOUT ) :: test_tri
    INTEGER,                  INTENT( IN    ) :: jt0
    INTEGER,                  INTENT( IN    ) :: sid
    LOGICAL,                  INTENT( INOUT ) :: ledge
    INTEGER, EXTERNAL                         :: UserFunction
  END FUNCTION SAG_Contour
END INTERFACE

SAG_TraceContour = SAG_ERROR

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

IF( contour%ifld <= 0 .OR. contour%ifld > triT%nodeT%mxdata )THEN
  LastError = SAG_ERR_IFLD
  LastParm  = contour%ifld
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%ipedge) )THEN
  nedge  = 0
ELSE
  nedge = triT%nedge
  IF( .NOT.ASSOCIATED(triT%ipenod) )THEN
    LastError = SAG_ERR_IPENOD
    LastParm  = 0
    GOTO 9999
  END IF
  enode => triT%ipenod
  ALLOCATE( test_edge(triT%nedge),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = triT%nedge
    GOTO 9999
  END IF
END IF

tri => triT%iptri

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

ALLOCATE( test_tri(triT%ntri),STAT=ios )
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = triT%ntri
  GOTO 9998
END IF

nfld =  triT%nodeT%mxdata
node => triT%nodeT%ipnode
dat  => triT%nodeT%ipdata

contour%last_tri   = -1
contour%start_node = -1
contour%last_node  = -1

vmin =  1.E+30
vmax = -1.E+30
DO i = 1,triT%nodeT%nnode
  ii = (i-1)*nfld + contour%ifld
  IF( .NOT.UseSpecial .OR. dat(ii) /= Special )THEN
    vmin = MIN(vmin,dat(ii))
    vmax = MAX(vmax,dat(ii))
  END IF
END DO

IF( vmin >= contour%value .OR. vmax < contour%value )GOTO 9996

DO i = 1,triT%ntri
  test_tri(i) = .TRUE.
END DO

DO i = 1,nedge
  test_edge(i) = .TRUE.
END DO

first_edge = .TRUE.
has_edge   = .FALSE.
DO je = 1,nedge
  contour%start_node = -1
  contour%last_node = -1
  jt = enode(je)%tid
  IF( test_edge(je) )THEN
    IF( SAG_HasContour( contour,tri(jt)%nid,dat,nfld ) )THEN
      ledge    = .TRUE.
      j = SAG_Contour( triT,contour,test_tri,jt,enode(je)%sid,ledge,UserFunction )
      IF( j /= SAG_OK )GOTO 9997
      IF( ledge )THEN ! .and. contour%lclose)then
        first_edge = .FALSE.
        has_edge = .TRUE.
1000    CONTINUE
        found = .FALSE.
        i = 1
        DO WHILE( .NOT.found .AND. i <= nedge )
          found = contour%last_node == enode(i)%nid
          IF( found )THEN
            is = i
            ie = is + 1
            IF( ie > nedge )ie = ie - nedge
            IF( contour%last_tri == enode(is)%tid )THEN
              IF( contour%last_tri == enode(ie)%tid )THEN !Corner
                IF( dat( (enode(ie)%nid-1)*nfld + contour%ifld ) > contour%value )THEN
                  test_edge(is) = .FALSE.
                  ie = je + nedge
                  inc = 1
                ELSE
                  test_edge(ie) = .FALSE.
                  is = ie
                  ie = je
                  inc = -1
                END IF
              ELSE
                test_edge(is) = .FALSE.
                ie = je + nedge
                inc = 1
              END IF
            ELSE IF( contour%last_tri == enode(ie)%tid )THEN
              test_edge(ie) = .FALSE.
              is = ie
              ie = je
              inc = -1
            ELSE
              is = 1000
              ie = -1
              inc = 1
            END IF
          ELSE
            i = i + 1
          END IF
        END DO
        IF( found )THEN
          IF( contour%lclose )THEN
            DO i = is,ie,inc
              IF( i > nedge )THEN
                ii = i - nedge
              ELSE
                ii = i
              END IF
              jt = enode(ii)%tid
              IF( test_edge(ii) )THEN
                test_edge(ii) = .FALSE.
                IF( SAG_HasContour(contour,tri(jt)%nid,dat,nfld) )THEN
                  test_tri(jt) = .FALSE.
                  ledge = .TRUE.
                  j = SAG_Contour( triT,contour,test_tri,jt,enode(ii)%sid, &
                                                        ledge,UserFunction )
                  IF( j /= SAG_OK )GOTO 9997
                  IF( ledge )THEN
                    IF( contour%last_node > 0 )GOTO 1000
                  ELSE
                    contour%last_node = enode(ii)%nid
                    contour%last_tri  = enode(ii)%tid
                    j = UserFunction( node(enode(ii)%nid)%x,node(enode(ii)%nid)%y )
                    IF( j /= SAG_OK )GOTO 9000
                  END IF
                ELSE
                  contour%last_node = enode(ii)%nid
                  contour%last_tri  = enode(ii)%tid
                  j = UserFunction( node(enode(ii)%nid)%x,node(enode(ii)%nid)%y )
                  IF( j /= SAG_OK )GOTO 9000
                END IF
              END IF
            END DO
          ELSE IF( ie > 0 )THEN
            IF( is > nedge )is = is - nedge
            IF( ie > nedge )ie = ie - nedge
            test_edge(is) = .FALSE.
            test_edge(ie) = .FALSE.
          END IF
        END IF
      ELSE
        test_edge(je) = .FALSE.
      END IF
    ELSE
      test_edge(je) = first_edge !Only turn off after starting a trace
      test_tri(jt)  = .FALSE.
    END IF
  END IF
END DO

!No edge contour but edge values are above contour => trace out boundary
! (SFP - 05/07/01)
IF( .NOT.has_edge .AND. dat( (enode(1)%nid-1)*nfld + contour%ifld ) > contour%value )THEN
  contour%start = .TRUE.
  DO je = 1,nedge
    j = UserFunction( node(enode(je)%nid)%x,node(enode(je)%nid)%y )
    IF( j /= SAG_OK )GOTO 9000
  END DO
  j = UserFunction( node(enode(1)%nid)%x,node(enode(1)%nid)%y )
  IF( j /= SAG_OK )GOTO 9000
END IF

ledge = .FALSE.
DO jt = 1,triT%ntri
  IF( test_tri(jt) )THEN
    IF( SAG_HasContour(contour,tri(jt)%nid,dat,nfld) )THEN
      contour%start_node = -1
      contour%last_node = -1
      j = SAG_Contour( triT,contour,test_tri,jt,0,ledge,UserFunction )
      IF( j /= SAG_OK )GOTO 9997
    ELSE
      test_tri(jt) = .FALSE.
    END IF
  END IF
END DO

9996 CONTINUE
SAG_TraceContour = SAG_OK

9997 CONTINUE
IF( ALLOCATED(test_tri) )DEALLOCATE( test_tri,STAT=ios )

9998 CONTINUE
IF( ALLOCATED(test_edge) )DEALLOCATE( test_edge,STAT=ios )

9999 CONTINUE

RETURN

9000 CONTINUE
LastError = SAG_ERR_USER
GOTO 9997

END
!*******************************************************************************
!                SAG_HasContour
!*******************************************************************************
LOGICAL FUNCTION SAG_HasContour( contour,nid,dat,nfld )

USE saggrd_fi
USE sagcnt_fd

!     Check a triangle for existance of contour

IMPLICIT NONE

TYPE( SAGcontour_str ), INTENT( IN ) :: contour
INTEGER, DIMENSION(3),  INTENT( IN ) :: nid
INTEGER,                INTENT( IN ) :: nfld
REAL, DIMENSION(:),     POINTER      :: dat

INTEGER i
REAL    value(3),cmin,cmax

DO i = 1,3
  value(i) = dat(contour%ifld+(nid(i)-1)*nfld)
END DO

IF( UseSpecial )THEN
  DO i = 1,3
    IF( value(i) == Special )THEN
      SAG_HasContour = .FALSE.
      GOTO 9999
    END IF
  END DO
END IF

cmin = MIN(value(1),value(2),value(3))
cmax = MAX(value(1),value(2),value(3))

SAG_HasContour = cmin < contour%value .AND. cmax >= contour%value

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_HighNode
!*******************************************************************************
SUBROUTINE SAG_HighNode( dat,nfld,contour,nid,sid,hi_node )

USE sagcnt_fd

!     Check a triangle for existence of a contour

IMPLICIT NONE

REAL, DIMENSION(:),     POINTER       :: dat
INTEGER,                INTENT( IN  ) :: nfld
TYPE( SAGcontour_str ), INTENT( IN  ) :: contour
INTEGER, DIMENSION(3),  INTENT( IN  ) :: nid
INTEGER,                INTENT( IN  ) :: sid
INTEGER,                INTENT( OUT ) :: hi_node

REAL    x1,x2
INTEGER node1,node2,osid

osid = sid + 1
IF( osid > 3 )osid = 1

node1 = nid(sid)
node2 = nid(osid)

x1 = dat(contour%ifld+(node1-1)*nfld)
x2 = dat(contour%ifld+(node2-1)*nfld)

IF( x1 > x2 )THEN
  hi_node = node1
ELSE
  hi_node = node2
END IF

RETURN
END
!*******************************************************************************
!                SAG_Contour
!*******************************************************************************
INTEGER FUNCTION SAG_Contour( triT,contour,test_tri,jt0,sid,ledge,UserFunction )

USE sagdef_fd
USE sagnod_fd
USE sagtri_fd
USE sagerr_fd
USE sagcnt_fd
USE sagerr_fi

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER         :: triT
TYPE( SAGcontour_str ),   INTENT( INOUT ) :: contour
LOGICAL, DIMENSION(*),    INTENT( INOUT ) :: test_tri
INTEGER,                  INTENT( IN    ) :: jt0
INTEGER,                  INTENT( IN    ) :: sid
LOGICAL,                  INTENT( INOUT ) :: ledge
INTEGER, EXTERNAL                         :: UserFunction

REAL,                    POINTER, DIMENSION(:) :: dat
TYPE( SAGnode_str ),     POINTER, DIMENSION(:) :: node
TYPE( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri
TYPE( SAGconnect_str ),  POINTER, DIMENSION(:) :: conn

TYPE( SAGendpoint_str ) endpt

INTEGER jt,j,hi_node,nfld,jt2,sidx,sids

LOGICAL endpt1_first

INTERFACE
  INTEGER FUNCTION SAG_EndPoint( contour,tri,node,dat,nfld,endpt )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGcontour_str ),      INTENT( IN  ) :: contour
    TYPE( SAGtriangle_str ),     INTENT( IN  ) :: tri
    TYPE( SAGnode_str ), DIMENSION(:), POINTER :: node
    REAL, DIMENSION(:),                POINTER :: dat
    INTEGER,                     INTENT( IN  ) :: nfld
    TYPE( SAGendpoint_str ),     INTENT( OUT ) :: endpt
  END FUNCTION SAG_EndPoint

  SUBROUTINE SAG_HighNode( dat,nfld,contour,nid,sid,hi_node )
    USE sagcnt_fd
    REAL, DIMENSION(:),     POINTER       :: dat
    INTEGER,                INTENT( IN  ) :: nfld
    TYPE( SAGcontour_str ), INTENT( IN  ) :: contour
    INTEGER, DIMENSION(3),  INTENT( IN  ) :: nid
    INTEGER,                INTENT( IN  ) :: sid
    INTEGER,                INTENT( OUT ) :: hi_node
  END SUBROUTINE SAG_HighNode
END INTERFACE

SAG_Contour = SAG_ERROR

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%ipconn) )THEN
  LastError = SAG_ERR_IPCONN
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

tri  => triT%iptri
conn => triT%ipconn

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

nfld =  triT%nodeT%mxdata
node => triT%nodeT%ipnode
dat  => triT%nodeT%ipdata

jt = jt0

j = SAG_EndPoint( contour,tri(jt),node,dat,nfld,endpt )
IF( j /= SAG_OK )THEN
  test_tri(jt) = .FALSE.
  GOTO 9998
END IF

IF( ledge .AND. contour%lclose )THEN
  IF( conn(jt)%tid(endpt%sid(2)) <= 0 .AND. endpt%sid(2) == sid )THEN
    CALL SAG_HighNode( dat,nfld,contour,tri(jt)%nid,endpt%sid(2),hi_node )
  ELSE IF( conn(jt)%tid(endpt%sid(1)) <= 0 .AND. endpt%sid(1) == sid )THEN
    CALL SAG_HighNode( dat,nfld,contour,tri(jt)%nid,endpt%sid(1),hi_node )
  ELSE
    ledge = .FALSE.
    GOTO 9998
  END IF
  IF( contour%start_node < 0 )THEN
    contour%start_node = hi_node
    contour%start      = .TRUE.
  ELSE
    IF( hi_node /= contour%last_node )THEN
      j = UserFunction( node(hi_node)%x,node(hi_node)%y )
      contour%start = .FALSE.
    END IF
    IF( j /= SAG_OK )GOTO 9000
    IF( hi_node == contour%start_node )THEN
      IF( conn(jt)%tid(endpt%sid(2)) <= 0 )THEN
        j = UserFunction( endpt%px2,endpt%py2 )
        IF( j /= SAG_OK )GOTO 9000
      ELSE
        j = UserFunction( endpt%px1,endpt%py1 )
        IF( j /= SAG_OK )GOTO 9000
      END IF
      test_tri(jt) = .FALSE.
      contour%last_node = -1
      contour%last_tri  = -1
      GOTO 9998
    END IF
  END IF
ELSE
  contour%start = .TRUE.
END IF

IF( conn(jt)%tid(endpt%sid(1)) <= 0 )THEN
  IF( conn(jt)%tid(endpt%sid(2)) <= 0 )THEN
    endpt1_first = ( endpt%sid(1) == sid )
  ELSE
    endpt1_first = .TRUE.
  END IF
ELSE
  IF( conn(jt)%tid(endpt%sid(2)) <= 0 )THEN
    endpt1_first = .FALSE.
  ELSE
    endpt1_first = .TRUE.
  END IF
END IF

IF( endpt1_first )THEN
  sids = endpt%sid(1)
  sidx = endpt%sid(2)
  CALL SAG_HighNode( dat,nfld,contour,tri(jt)%nid,sidx,contour%last_node )
  contour%last_tri = jt
  j = UserFunction( endpt%px1,endpt%py1 )
  IF( j /= SAG_OK )GOTO 9000
  j = UserFunction( endpt%px2,endpt%py2 )
  IF( j /= SAG_OK )GOTO 9000
ELSE
  sids = endpt%sid(2)
  sidx = endpt%sid(1)
  CALL SAG_HighNode( dat,nfld,contour,tri(jt)%nid,sidx,contour%last_node )
  contour%last_tri = jt
  j = UserFunction( endpt%px2,endpt%py2 )
  IF( j /= SAG_OK )GOTO 9000
  j = UserFunction( endpt%px1,endpt%py1 )
  IF( j /= SAG_OK )GOTO 9000
END IF

test_tri(jt) = .FALSE.
jt2 = conn(jt)%tid(sidx)

DO WHILE( jt2 > 0 )

  IF( conn(jt2)%tid(1) == jt )THEN
    sids = 1
  ELSE IF( conn(jt2)%tid(2) == jt )THEN
    sids = 2
  ELSE
    sids = 3
  END IF
  j = SAG_EndPoint( contour,tri(jt2),node,dat,nfld,endpt )
  IF( j /= SAG_OK )THEN
    test_tri(jt2) = .FALSE.
    jt2 = 0
  ELSE
    IF( endpt%sid(1) == sids )THEN
      sidx = endpt%sid(2)
      CALL SAG_HighNode( dat,nfld,contour,tri(jt2)%nid,sidx,contour%last_node )
      contour%last_tri = jt2
      j = UserFunction(endpt%px2,endpt%py2)
      IF( j /= SAG_OK )GOTO 9000
    ELSE
      sidx = endpt%sid(1)
      CALL SAG_HighNode( dat,nfld,contour,tri(jt2)%nid,sidx,contour%last_node )
      contour%last_tri = jt2
      j = UserFunction( endpt%px1,endpt%py1 )
      IF( j /= SAG_OK )GOTO 9000
    END IF

    jt = jt2
    test_tri(jt) = .FALSE.
    jt2 = conn(jt)%tid(sidx)
    IF( jt2 == jt0)jt2 = 0
  END IF

END DO

IF( ledge .AND. contour%lclose )THEN
  j = UserFunction( node(contour%last_node)%x,node(contour%last_node)%y )
  IF( j /= SAG_OK )GOTO 9000
END IF

9998 CONTINUE
SAG_Contour = SAG_OK

9999 CONTINUE

RETURN

9000 CONTINUE
LastError = SAG_ERR_USER
GOTO 9999

END
!*******************************************************************************
!                SAG_EndPoint
!*******************************************************************************
INTEGER FUNCTION SAG_EndPoint( contour,tri,node,dat,nfld,endpt )

USE sagdef_fd
USE sagtri_fd
USE saggrd_fi
USE sagcnt_fd

!     Find endpoints of a contour

IMPLICIT NONE

TYPE( SAGcontour_str ),      INTENT( IN  ) :: contour
TYPE( SAGtriangle_str ),     INTENT( IN  ) :: tri
TYPE( SAGnode_str ), DIMENSION(:), POINTER :: node
REAL,                DIMENSION(:), POINTER :: dat
INTEGER,                     INTENT( IN  ) :: nfld
TYPE( SAGendpoint_str ),     INTENT( OUT ) :: endpt

REAL    x1,x2,x3,y1,y2,y3,v1,v2,v3,rat,dval
INTEGER l1,l2,l3

REAL, EXTERNAL :: SAG_DrawRatio

SAG_EndPoint = SAG_ERROR

x1 = node(tri%nid(1))%x
x2 = node(tri%nid(2))%x
x3 = node(tri%nid(3))%x

y1 = node(tri%nid(1))%y
y2 = node(tri%nid(2))%y
y3 = node(tri%nid(3))%y

l1 = node(tri%nid(1))%lev
l2 = node(tri%nid(2))%lev
l3 = node(tri%nid(3))%lev

v1 = dat(contour%ifld+(tri%nid(1)-1)*nfld)
v2 = dat(contour%ifld+(tri%nid(2)-1)*nfld)
v3 = dat(contour%ifld+(tri%nid(3)-1)*nfld)

IF( UseSpecial )THEN
  IF( v1 == Special .OR. v2 == Special .OR. v3 == Special )THEN
    GOTO 9999
  END IF
END IF

IF( contour%log_interp )THEN
  v1   = LOG(MAX(v1,1.E-30))
  v2   = LOG(MAX(v2,1.E-30))
  v3   = LOG(MAX(v3,1.E-30))
  dval = LOG(MAX(contour%value,1.E-30))
ELSE
  dval = contour%value
END IF

IF( v1 < dval )THEN
  IF( v2 >= dval )THEN
    rat = SAG_DrawRatio( v1,l1,v2,l2,dval )
    endpt%px1 = x1 + rat*(x2-x1)
    endpt%py1 = y1 + rat*(y2-y1)
    endpt%sid(1) = 1
    IF( v3 >= dval )THEN
      rat = SAG_DrawRatio( v1,l1,v3,l3,dval )
      endpt%px2 = x1 + rat*(x3-x1)
      endpt%py2 = y1 + rat*(y3-y1)
      endpt%sid(2) = 3
    ELSE
      rat = SAG_DrawRatio( v3,l3,v2,l2,dval )
      endpt%px2 = x3 + rat*(x2-x3)
      endpt%py2 = y3 + rat*(y2-y3)
      endpt%sid(2) = 2
    END IF
  ELSE
    IF( v3 < dval )THEN
      GOTO 9999
    ELSE
      rat = SAG_DrawRatio( v1,l1,v3,l3,dval )
      endpt%px1 = x1 + rat*(x3-x1)
      endpt%py1 = y1 + rat*(y3-y1)
      rat = SAG_DrawRatio( v3,l3,v2,l2,dval )
      endpt%px2 = x3 + rat*(x2-x3)
      endpt%py2 = y3 + rat*(y2-y3)
      endpt%sid(1) = 3
      endpt%sid(2) = 2
    END IF
  END IF
ELSE
  IF( v2 < dval )THEN
    rat = SAG_DrawRatio( v1,l1,v2,l2,dval )
    endpt%px1 = x1 + rat*(x2-x1)
    endpt%py1 = y1 + rat*(y2-y1)
    endpt%sid(1) = 1
    IF( v3 < dval )THEN
      rat = SAG_DrawRatio( v1,l1,v3,l3,dval )
      endpt%px2 = x1 + rat*(x3-x1)
      endpt%py2 = y1 + rat*(y3-y1)
      endpt%sid(2) = 3
    ELSE
      rat = SAG_DrawRatio( v3,l3,v2,l2,dval )
      endpt%px2 = x3 + rat*(x2-x3)
      endpt%py2 = y3 + rat*(y2-y3)
      endpt%sid(2) = 2
    END IF
  ELSE
    IF( v3 >= dval )THEN
      GOTO 9999
    ELSE
      rat = SAG_DrawRatio( v1,l1,v3,l3,dval )
      endpt%px1 = x1 + rat*(x3-x1)
      endpt%py1 = y1 + rat*(y3-y1)
      rat = SAG_DrawRatio( v3,l3,v2,l2,dval )
      endpt%px2 = x3 + rat*(x2-x3)
      endpt%py2 = y3 + rat*(y2-y3)
      endpt%sid(1) = 3
      endpt%sid(2) = 2
    END IF
  END IF
END IF

SAG_EndPoint = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_DrawRatio
!*******************************************************************************
REAL FUNCTION SAG_DrawRatio( v1,l1,v2,l2,v )

!     Set interpolation value

IMPLICIT NONE

REAL,    INTENT( IN ) :: v1,v2,v
INTEGER, INTENT( IN ) :: l1,l2

REAL    fac
INTEGER lev

SAG_DrawRatio = (v-v1)/(v2-v1)

lev = ABS(l1-l2)
IF( lev >= 2 )THEN
  fac = 4./(1.+ 2.**lev)
  IF( l1 > l2 )THEN
    SAG_DrawRatio = fac*SAG_DrawRatio
  ELSE
    SAG_DrawRatio = 1.0 - fac*(1.0-SAG_DrawRatio)
  END IF
END IF

RETURN
END
!*******************************************************************************
!                SAG_ContourCount
!*******************************************************************************
INTEGER FUNCTION SAG_ContourCount( x,y )

USE sagdef_fd
USE sagcnt_fd
USE sagcnt_usr

!     count contour points

IMPLICIT NONE

REAL, INTENT( IN ) :: x,y

contour%npts = contour%npts + 1

IF( contour%start )THEN
  contour%start = .FALSE.
  contour%nlns  = contour%nlns + 1
END IF

SAG_ContourCount = SAG_OK

RETURN
END
!*******************************************************************************
!                SAG_ContourAddPoint
!*******************************************************************************
INTEGER FUNCTION SAG_ContourAddPoint( x,y )

USE sagdef_fd
USE sagerr_fd
USE sagcnt_fd
USE sagerr_fi
USE sagcnt_usr

!     add a contour point to the array

IMPLICIT NONE

REAL, INTENT( IN ) :: x,y

TYPE( SAGpoint_str ), POINTER, DIMENSION(:) :: points
INTEGER,              POINTER, DIMENSION(:) :: lines

SAG_ContourAddPoint = SAG_ERROR

IF( .NOT.ASSOCIATED(contour%ippts) )THEN
  LastError = SAG_ERR_IPPTS
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(contour%iplns) )THEN
  LastError = SAG_ERR_IPLNS
  LastParm  = 0
  GOTO 9999
END IF

points => contour%ippts
lines  => contour%iplns

contour%npts = contour%npts + 1
IF( contour%npts > contour%mxpts )THEN
  LastError = SAG_ERR_MXPTS
  LastParm  = 0
  GOTO 9999
END IF

points(contour%npts)%x = x
points(contour%npts)%y = y

IF( contour%start )THEN
  contour%start = .FALSE.
  contour%nlns = contour%nlns + 1
  IF( contour%nlns > contour%mxlns )THEN
    LastError = SAG_ERR_MXLNS
    LastParm  = 0
    GOTO 9999
  END IF
  lines(contour%nlns) = contour%npts
END IF

SAG_ContourAddPoint = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_UserContour
!*******************************************************************************
INTEGER FUNCTION SAG_UserContour( contour,UserFunction,grd )

USE sagdef_fd
USE sagerr_fd
USE sagcnt_fd
USE sagerr_fi
USE sagstr_fd

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGcontour_str ),   INTENT( IN ) :: contour
INTEGER, EXTERNAL                      :: UserFunction
TYPE ( SAGgrid_str ),     POINTER      :: grd

REAL, ALLOCATABLE, DIMENSION(:) :: xpoly
REAL, ALLOCATABLE, DIMENSION(:) :: ypoly

TYPE( SAGpoint_str ), POINTER, DIMENSION(:) :: points
INTEGER,              POINTER, DIMENSION(:) :: lines

INTEGER i,ifrst,ilast,npoly
INTEGER j,irv,level,ios

!==== Initialize

SAG_UserContour = SAG_ERROR

!==== See if there is anything to do

IF( contour%npts == 0 )THEN
  SAG_UserContour = SAG_OK
  GOTO 9999
END IF

IF( contour%nlns == 0 )THEN
  SAG_UserContour = SAG_OK
  GOTO 9999
END IF

!==== Error Check

IF( .NOT.ASSOCIATED(contour%ippts) )THEN
  LastError = SAG_ERR_IPPTS
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(contour%iplns) )THEN
  LastError = SAG_ERR_IPLNS
  LastParm  = 0
  GOTO 9999
END IF

points => contour%ippts
lines  => contour%iplns

level = contour%level

!==== Loop over lines

DO i = 1,contour%nlns

!====== Determine number of points for this line

  ifrst = lines(i)
  IF( i == contour%nlns )THEN
    ilast = contour%npts
  ELSE
    ilast = lines(i+1) - 1
  END IF
  npoly = ilast - ifrst + 1

!====== Allocate point arrays

  IF( npoly > 0 )THEN

    ALLOCATE( xpoly(npoly),ypoly(npoly),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = npoly
      GOTO 9998
    END IF

!======== Set points

    DO j = 1,npoly
      xpoly(j) = grd%xmin + grd%dx*points(j+ifrst-1)%x
      ypoly(j) = grd%ymin + grd%dy*points(j+ifrst-1)%y
    END DO

!======== User

    irv = UserFunction( npoly,xpoly,ypoly,level )
    IF( irv /= SAG_OK )THEN
      LastError = SAG_ERR_USER
      GOTO 9998
    END IF

!======== Free points

    IF( ALLOCATED(xpoly) )DEALLOCATE( xpoly,STAT=ios )
    IF( ALLOCATED(ypoly) )DEALLOCATE( ypoly,STAT=ios )

  END IF

END DO

SAG_UserContour = SAG_OK

9999 CONTINUE

RETURN

9998 CONTINUE
IF( ALLOCATED(ypoly) )DEALLOCATE( ypoly,STAT=ios )
IF( ALLOCATED(xpoly) )DEALLOCATE( xpoly,STAT=ios )
GOTO 9999

END
