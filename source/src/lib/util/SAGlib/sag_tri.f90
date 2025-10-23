!*******************************************************************************
!                SAG_InitTriangle
!*******************************************************************************
INTEGER FUNCTION SAG_InitTriangle( mxtri,connect,mxedge,triT )

USE sagdef_fd
USE sagtri_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG triangle array structure

IMPLICIT NONE

INTEGER,                  INTENT (IN ) :: mxtri
INTEGER,                  INTENT (IN ) :: mxedge
LOGICAL,                  INTENT (IN ) :: connect
TYPE( SAGtriangleT_str ), POINTER      :: triT

INTEGER i, j, ios

SAG_InitTriangle = SAG_ERROR

IF( mxtri > 0 )THEN

  ALLOCATE( triT%iptri(mxtri),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxtri
    GOTO 9999
  END IF

  IF( connect )THEN
    ALLOCATE( triT%ipconn(mxtri),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxtri
      GOTO 9998
    END IF
  END IF

END IF

IF( mxedge > 0 )THEN
  ALLOCATE( triT%ipedge(mxedge),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxedge
    GOTO 9997
  END IF

  ALLOCATE( triT%ipenod(mxedge),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxedge
    GOTO 9996
  END IF

END IF

triT%mxtri  = mxtri
triT%mxedge = mxedge

triT%ntri  = 0
triT%nedge = 0
triT%nenod = 0

triT%build = 0

IF( mxtri > 0 )THEN
  DO i = 1,mxtri
    DO j = 1,3
      triT%iptri(i)%nid(j) = 0
    END DO
  END DO
  IF( connect )THEN
    DO i = 1,mxtri
      DO j = 1,3
        triT%ipconn(i)%tid(j) = 0
      END DO
    END DO
  END IF
END IF

IF( mxedge > 0 )THEN
  DO i = 1,mxedge
    DO j = 1,2
      triT%ipedge(i)%nid(j) = 0
    END DO
    triT%ipedge(i)%tid = 0
    triT%ipedge(i)%sid = 0
    triT%ipenod(i)%nid = 0
    triT%ipenod(i)%tid = 0
    triT%ipenod(i)%sid = 0
  END DO
END IF

SAG_InitTriangle = SAG_OK

9999 CONTINUE

RETURN

9996  IF( ASSOCIATED(triT%ipedge) )DEALLOCATE( triT%ipedge,STAT=ios )
9997  IF( ASSOCIATED(triT%ipconn) )DEALLOCATE( triT%ipconn,STAT=ios )
9998  IF( ASSOCIATED(triT%iptri ) )DEALLOCATE( triT%iptri,STAT=ios  )
GOTO 9999

END
!*******************************************************************************
!                SAG_FreeTriangleID
!*******************************************************************************
SUBROUTINE SAG_FreeTriangleID( grdI )

USE saglst_fd
USE PtrGrdStrItf

!  Deallocate memory for a SAG grid structure

IMPLICIT NONE

INTEGER grdI

TYPE( SAGTriangleT_str ), POINTER :: triT

INTERFACE
  SUBROUTINE SAG_FreeTriangle( triT )
    USE sagtri_fd
    TYPE( SAGTriangleT_str ), POINTER :: triT
  END SUBROUTINE SAG_FreeTriangle
END INTERFACE

triT => SAG_PtrTriStr( grdI )

IF( ASSOCIATED(triT) )CALL SAG_FreeTriangle( triT )

RETURN
END
!*******************************************************************************
!                SAG_FreeTriangle
!*******************************************************************************
SUBROUTINE SAG_FreeTriangle( triT )

USE sagtri_fd

!     Frees memory used by a SAG triangle array structure

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT

INTEGER ios

CALL SAG_FreeNode(triT%nodeT)

IF( ASSOCIATED(triT%iptri ) )DEALLOCATE( triT%iptri ,STAT=ios )
IF( ASSOCIATED(triT%ipconn) )DEALLOCATE( triT%ipconn,STAT=ios )
IF( ASSOCIATED(triT%ipedge) )DEALLOCATE( triT%ipedge,STAT=ios )
IF( ASSOCIATED(triT%ipenod) )DEALLOCATE( triT%ipenod,STAT=ios )

NULLIFY( triT%iptri  )
NULLIFY( triT%ipconn )
NULLIFY( triT%ipedge )
NULLIFY( triT%ipenod )

RETURN
END
!*******************************************************************************
!                SAG_InitEdges
!*******************************************************************************
INTEGER FUNCTION SAG_InitEdges( mxedge,triT )

USE sagdef_fd
USE sagtri_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG triangle edge array structure

IMPLICIT NONE

INTEGER, INTENT( IN )             :: mxedge
TYPE( SAGtriangleT_str ), POINTER :: triT


TYPE( SAGedge_str ),     DIMENSION(:), POINTER :: edge
TYPE( SAGedgenode_str ), DIMENSION(:), POINTER :: node

INTEGER i, j, ios

SAG_InitEdges = SAG_ERROR

IF( mxedge > 0 )THEN
  ALLOCATE( edge(mxedge),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxedge
    GOTO 9999
  END IF

  ALLOCATE( node(mxedge),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxedge
    GOTO 9998
  END IF

END IF

triT%ipedge => edge
triT%ipenod => node

triT%mxedge = mxedge

triT%nedge = 0
triT%nenod = 0

IF( mxedge > 0 )THEN
  DO i = 1,mxedge
    DO j = 1,2
      edge(i)%nid(j) = 0
    END DO
    edge(i)%tid = 0
    edge(i)%sid = 0
    node(i)%nid = 0
    node(i)%tid = 0
    node(i)%sid = 0
  END DO
END IF

SAG_InitEdges = SAG_OK

9999 CONTINUE

RETURN

9998  DEALLOCATE( edge )
GOTO 9999

END
!*******************************************************************************
!               SAG_Triangles
!*******************************************************************************
INTEGER FUNCTION SAG_TrianglesID( grdI,nfld,ifld,connect )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER,               INTENT( IN ) :: grdI
INTEGER,               INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld
LOGICAL,               INTENT( IN ) :: connect

TYPE ( SAGgrid_str ),     POINTER :: grd
TYPE( SAGtriangleT_str ), POINTER :: triT

INTERFACE
  INTEGER FUNCTION SAG_Triangles( grd,nfld,ifld,connect,triT )
    USE sagstr_fd
    USE sagtri_fd
    TYPE ( SAGgrid_str ),     POINTER      :: grd
    INTEGER,                  INTENT( IN ) :: nfld
    INTEGER, DIMENSION(*),    INTENT( IN ) :: ifld
    LOGICAL,                  INTENT( IN ) :: connect
    TYPE( SAGtriangleT_str ), POINTER      :: triT
  END FUNCTION SAG_Triangles
END INTERFACE

SAG_TrianglesID = SAG_ERROR

!==== Point to grid structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

triT => SAG_PtrTriStr( grdI )
IF( .NOT.ASSOCIATED(triT) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

SAG_TrianglesID =  SAG_Triangles( grd,nfld,ifld,connect,triT )

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION SAG_Triangles( grd,nfld,ifld,connect,triT )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE ( SAGgrid_str ),     POINTER      :: grd
INTEGER,                  INTENT( IN ) :: nfld
INTEGER, DIMENSION(*),    INTENT( IN ) :: ifld
LOGICAL,                  INTENT( IN ) :: connect
TYPE( SAGtriangleT_str ), POINTER      :: triT

INTEGER j,nnode,nedge
INTEGER ix,iy

TYPE( SAGcell_str ) p,pr,pt,prt

TYPE( SAGnodeT_str ), POINTER :: node

INTEGER, EXTERNAL :: SAG_InitNode

INTERFACE
  INTEGER FUNCTION SAG_Nodes( grd,nfld,ifld,nodeI )
    USE sagstr_fd
    USE sagnod_fd
    TYPE( SAGgrid_str ),  POINTER :: grd
    INTEGER                       :: nfld
    INTEGER, DIMENSION(*), TARGET :: ifld
    TYPE( SAGnodeT_str ),  TARGET :: nodeI
  END FUNCTION SAG_Nodes

  INTEGER FUNCTION SAG_InitTriangle( mxtri,connect,mxedge,triT )
    USE sagtri_fd
    INTEGER, INTENT (IN ) :: mxtri
    INTEGER, INTENT (IN ) :: mxedge
    LOGICAL, INTENT (IN ) :: connect
    TYPE( SAGtriangleT_str ), POINTER :: triT
  END FUNCTION SAG_InitTriangle

  INTEGER FUNCTION SAG_InitEdges( mxedge,triT )
    USE sagtri_fd
    INTEGER, INTENT( IN ) :: mxedge
    TYPE( SAGtriangleT_str ), POINTER :: triT
  END FUNCTION SAG_InitEdges

  INTEGER FUNCTION SAG_BuildConnect( triT )
    USE sagtri_fd
    TYPE( SAGtriangleT_str ), POINTER :: triT
  END FUNCTION SAG_BuildConnect

  INTEGER FUNCTION SAG_CountEdges( triT )
    USE sagtri_fd
    TYPE( SAGtriangleT_str ), POINTER :: triT
  END FUNCTION SAG_CountEdges

  RECURSIVE INTEGER FUNCTION SAG_StartTriangle( grd, triT, p1 , pr , pt , prt )
    USE sagstr_fd
    USE sagtri_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: p1
    TYPE( SAGcell_str ),      INTENT( IN ) :: pr
    TYPE( SAGcell_str ),      INTENT( IN ) :: pt
    TYPE( SAGcell_str ),      INTENT( IN ) :: prt
  END FUNCTION SAG_StartTriangle

  INTEGER FUNCTION SAG_BottomCount( grd )
    USE sagstr_fd
    TYPE( SAGgrid_str ), POINTER :: grd
  END FUNCTION SAG_BottomCount

  INTEGER FUNCTION SAG_BuildEdges( triT )
    USE sagtri_fd
    TYPE( SAGtriangleT_str ), POINTER :: triT
  END FUNCTION SAG_BuildEdges
END INTERFACE

SAG_Triangles = SAG_ERROR

nnode = SAG_BottomCount( grd )

IF( nnode <= 0 )THEN
  LastError = SAG_ERR_NONODE
  LastParm  = 0
  GOTO 9999
END IF

CALL SAG_InitCell( p )
CALL SAG_InitCell( pr )
CALL SAG_InitCell( pt )
CALL SAG_InitCell( prt )

j = SAG_InitNode( grd%ncells,nnode,nfld,triT%nodeT )
IF( j /= SAG_OK )GOTO 9999

node => triT%nodeT
j = SAG_Nodes( grd,nfld,ifld,node )
IF( j /= SAG_OK )GOTO 9999

j = SAG_InitTriangle( 2*nnode,connect,0,triT )
IF( j /= SAG_OK )GOTO 9999

!==== Set Triangles

DO ix = 1,grd%nx
  DO iy = 1,grd%ny

    p%id = (iy-1)*grd%nx + ix

    IF( ix < grd%nx )THEN
      pr%id = p%id + 1
    ELSE
      pr%id = 0
    END IF

    IF( iy < grd%ny )THEN
      pt%id = p%id + grd%nx
    ELSE
      pt%id = 0
    END IF

    IF( ix < grd%nx .AND. iy < grd%ny )THEN
      prt%id = p%id + grd%nx + 1
    ELSE
      prt%id = 0
    END IF

    j = SAG_StartTriangle( grd,triT,p,pr,pt,prt )
    IF( j /= SAG_OK )GOTO 9999

  END DO
END DO

!==== Set Connections

IF( connect )THEN

  j = SAG_BuildConnect( triT )
  IF( j /= SAG_OK )GOTO 9999

!====== Set Edges

  nedge = SAG_CountEdges( triT )
  IF( nedge <= 0 )THEN
    LastError = SAG_ERR_NOEDGE
    LastParm  = 0
    GOTO 9999
  END IF
  j = SAG_InitEdges( nedge,triT )
  IF( j /= SAG_OK )GOTO 9999

  j = SAG_BuildEdges( triT )
  IF( j /= SAG_OK )GOTO 9999

END IF

SAG_Triangles = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_StartTriangle
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_StartTriangle( grd,triT,p1,pr,pt,prt ) RESULT ( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),      POINTER      :: grd
TYPE( SAGtriangleT_str ), POINTER      :: triT
TYPE( SAGcell_str ),      INTENT( IN ) :: p1,pr,pt,prt

INTEGER,                 DIMENSION(:), POINTER :: igrd
INTEGER,                 DIMENSION(:), POINTER :: node
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri

INTEGER j

INTERFACE
  RECURSIVE INTEGER FUNCTION SAG_RightNeighbor( grd,triT,pr )
    USE sagstr_fd
    USE sagcel_fd
    USE sagtri_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: pr
  END FUNCTION SAG_RightNeighbor

  RECURSIVE INTEGER FUNCTION SAG_TopNeighbor( grd,triT,pt )
    USE sagstr_fd
    USE sagcel_fd
    USE sagtri_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: pt
  END FUNCTION SAG_TopNeighbor

  RECURSIVE INTEGER FUNCTION SAG_RightTopNeighbor( grd,triT,prt )
    USE sagstr_fd
    USE sagcel_fd
    USE sagtri_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: prt
  END FUNCTION SAG_RightTopNeighbor

  RECURSIVE INTEGER FUNCTION SAG_RefineCell( grd,triT,p1,pr,pt,prt )
    USE sagstr_fd
    USE sagtri_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: p1
    TYPE( SAGcell_str ),      INTENT( IN ) :: pr
    TYPE( SAGcell_str ),      INTENT( IN ) :: pt
    TYPE( SAGcell_str ),      INTENT( IN ) :: prt
  END FUNCTION SAG_RefineCell
END INTERFACE

irv = SAG_ERROR

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipcell) )THEN
  LastError = SAG_ERR_IPCELL
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd
tri  => triT%iptri
node => triT%nodeT%ipcell

IF( igrd(p1%id) == 0 )THEN

  IF( triT%ntri+1 > triT%mxtri )THEN
    LastError = SAG_ERR_MXTRI
    LastParm = 0
    GOTO 9999
  END IF
  triT%build = 0
  tri(triT%ntri+1)%nid(1) = node(p1%id)

  j = SAG_RightNeighbor( grd,triT,pr  )
  IF( j /= SAG_OK )GOTO 9999

  j = SAG_RightTopNeighbor( grd,triT,prt )
  IF( j /= SAG_OK )GOTO 9999

  j = SAG_TopNeighbor( grd,triT,pt  )
  IF( j /= SAG_OK )GOTO 9999

ELSE

  j = SAG_RefineCell( grd,triT,p1,pr,pt,prt )
  IF( j /= SAG_OK )GOTO 9999

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_RefineCell
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_RefineCell( grd,triT,p1,pr,pt,prt ) RESULT ( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),      POINTER      :: grd
TYPE( SAGtriangleT_str ), POINTER      :: triT
TYPE( SAGcell_str ),      INTENT( IN ) :: p1,pr,pt,prt

TYPE( SAGcell_str ) q1,qr,qt,qrt
TYPE( SAGcell_str ) c1,cr,ct,crt

INTEGER, DIMENSION(:), POINTER :: igrd

INTEGER j

INTERFACE
  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID

  LOGICAL FUNCTION SAG_IsRefined( id,igrd )
    INTEGER,               INTENT( IN ) :: id
    INTEGER, DIMENSION(:), POINTER      :: igrd
  END FUNCTION SAG_IsRefined

  RECURSIVE INTEGER FUNCTION SAG_StartTriangle( grd,triT,p1,pr,pt,prt )
    USE sagstr_fd
    USE sagtri_fd
    USE sagcel_fd
    TYPE( SAGgrid_str ),      POINTER      :: grd
    TYPE( SAGtriangleT_str ), POINTER      :: triT
    TYPE( SAGcell_str ),      INTENT( IN ) :: p1,pr,pt,prt
  END FUNCTION SAG_StartTriangle
END INTERFACE

irv = SAG_ERROR

IF( .NOT.ASSOCIATED( grd%ipgrd ) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

CALL SAG_InitCell( q1 )
CALL SAG_InitCell( qr )
CALL SAG_InitCell( qt )
CALL SAG_InitCell( qrt )

igrd  => grd%ipgrd

SELECT CASE( grd%type )
  CASE( SAG_GRID_BOTH )
    CALL SAG_GetID( grd%ipgrd,p1,0,q1 )
    CALL SAG_GetID( grd%ipgrd,p1,1,qr )
    CALL SAG_GetID( grd%ipgrd,p1,2,qt )
    CALL SAG_GetID( grd%ipgrd,p1,3,qrt )

    c1  = q1
    cr  = qr
    ct  = qt
    crt = qrt
    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

    c1 = qr
    ct = qrt

    IF( SAG_IsRefined(pr%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pr,0,cr )
      CALL SAG_GetID( grd%ipgrd,pr,2,crt )
    ELSE
      cr = pr
      crt%id = 0
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

    c1 = qt
    cr = qrt

    IF( SAG_IsRefined(pt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pt,0,ct )
      CALL SAG_GetID( grd%ipgrd,pt,1,crt )
    ELSE
      ct = pt
      crt%id = 0
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

    c1  = qrt
    crt = prt

    IF( SAG_IsRefined(pr%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pr,2,cr )
    ELSE
      cr = pr
    END IF

    IF( SAG_IsRefined(pt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pt,1,ct )
    ELSE
      ct = pt
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

  CASE( SAG_GRID_HORZ )
    CALL SAG_GetID( grd%ipgrd,p1,0,q1 )
    CALL SAG_GetID( grd%ipgrd,p1,1,qr )

    c1 = q1
    cr = qr

    IF( SAG_IsRefined(pt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pt,0,ct )
      CALL SAG_GetID( grd%ipgrd,pt,1,crt )
    ELSE
      ct = pt
      crt%id = 0
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

    c1 = qr

    IF( SAG_IsRefined(pr%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pr,0,cr )
    ELSE
      cr = pr
    END IF

    IF( SAG_IsRefined(pt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pt,1,ct )
    ELSE
      ct = pt
    END IF

    IF( SAG_IsRefined(prt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,prt,0,crt )
    ELSE
      crt = prt
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

  CASE( SAG_GRID_VERT )
    CALL SAG_GetID( grd%ipgrd,p1,0,q1 )
    CALL SAG_GetID( grd%ipgrd,p1,1,qt )

    c1 = q1
    ct = qt

    IF( SAG_IsRefined(pr%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pr,0,cr )
      CALL SAG_GetID( grd%ipgrd,pr,1,crt )
    ELSE
      cr = pr
      crt%id = 0
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

    c1 = qt

    IF( SAG_IsRefined(pr%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pr,1,cr )
    ELSE
      cr = pr
    END IF

    IF( SAG_IsRefined(pt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,pt,0,ct )
    ELSE
      ct = pt
    END IF

    IF( SAG_IsRefined(prt%id,igrd) )THEN
      CALL SAG_GetID( grd%ipgrd,prt,0,crt )
    ELSE
      crt = prt
    END IF

    j = SAG_StartTriangle( grd,triT,c1,cr,ct,crt )
    IF( j /= SAG_OK )GOTO 9999

  CASE( SAG_GRID_NONE )
    LastError = SAG_ERR_TYPE
    LastParm = grd%type
    GOTO 9999

  CASE DEFAULT
    LastError = SAG_ERR_TYPE
    LastParm = grd%type
    GOTO 9999

END SELECT

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_RightNeighbor
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_RightNeighbor( grd,triT,pr ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),      POINTER      :: grd
TYPE( SAGtriangleT_str ), POINTER      :: triT
TYPE( SAGcell_str ),      INTENT( IN ) :: pr

INTERFACE
  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

TYPE( SAGcell_str ) qr

INTEGER,                 DIMENSION(:), POINTER :: igrd
INTEGER,                 DIMENSION(:), POINTER :: node
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri

INTEGER j

irv = SAG_ERROR

IF( pr%id == 0 )THEN
  irv = SAG_OK
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipcell) )THEN
  LastError = SAG_ERR_IPCELL
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd
tri  => triT%iptri
node => triT%nodeT%ipcell

IF( igrd(pr%id) == 0 )THEN

  IF( triT%build == 0 )THEN
    tri(triT%ntri+1)%nid(2) = node(pr%id)
    triT%build = 1
  ELSE
    tri(triT%ntri+1)%nid(3) = node(pr%id)
    triT%ntri = triT%ntri+1
    IF( triT%ntri+1 > triT%mxtri )THEN
      LastError = SAG_ERR_MXTRI
      LastParm = 0
      GOTO 9999
    END IF
    tri(triT%ntri+1)%nid(1) = tri(triT%ntri)%nid(1)
    tri(triT%ntri+1)%nid(2) = tri(triT%ntri)%nid(3)
  END IF

ELSE

  CALL SAG_GetID( grd%ipgrd,pr,0,qr )

  j = SAG_RightNeighbor( grd,triT,qr )
  IF( j /= SAG_OK )GOTO 9999

  SELECT CASE( grd%type )
    CASE( SAG_GRID_BOTH )
      CALL SAG_GetID( grd%ipgrd,pr,2,qr )
      j = SAG_RightNeighbor( grd,triT,qr )
      IF( j /= SAG_OK )GOTO 9999

    CASE( SAG_GRID_VERT )
      CALL SAG_GetID( grd%ipgrd,pr,1,qr )
      j = SAG_RightNeighbor( grd,triT,qr )
      IF( j /= SAG_OK )GOTO 9999

    CASE( SAG_GRID_HORZ,SAG_GRID_NONE )

    CASE DEFAULT
      LastError = SAG_ERR_TYPE
      LastParm = grd%type
      GOTO 9999

  END SELECT

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_TopNeighbor
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_TopNeighbor( grd,triT,pt ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),      POINTER      :: grd
TYPE( SAGtriangleT_str ), POINTER      :: triT
TYPE( SAGcell_str ),      INTENT( IN ) :: pt

TYPE( SAGcell_str ) qt

INTEGER,                 DIMENSION(:), POINTER :: igrd
INTEGER,                 DIMENSION(:), POINTER :: node
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri

INTEGER j

INTERFACE
  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

irv = SAG_ERROR

IF( pt%id == 0 )THEN
  irv = SAG_OK
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipcell) )THEN
  LastError = SAG_ERR_IPCELL
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd
tri  => triT%iptri
node => triT%nodeT%ipcell

IF( igrd(pt%id) == 0 )THEN

  IF( triT%build == 0 )THEN
    tri(triT%ntri+1)%nid(2) = node(pt%id)
    triT%build = 1
  ELSE
    tri(triT%ntri+1)%nid(3) = node(pt%id)
    triT%ntri = triT%ntri+1
    IF( triT%ntri+1 > triT%mxtri )THEN
      LastError = SAG_ERR_MXTRI
      LastParm = 0
      GOTO 9999
    END IF
    tri(triT%ntri+1)%nid(1) = tri(triT%ntri)%nid(1)
    tri(triT%ntri+1)%nid(2) = tri(triT%ntri)%nid(3)
  END IF

ELSE

  SELECT CASE( grd%type )
    CASE( SAG_GRID_BOTH,SAG_GRID_HORZ )
      CALL SAG_GetID( grd%ipgrd,pt,1,qt )
      j = SAG_TopNeighbor( grd,triT,qt )
      IF( j /= SAG_OK )GOTO 9999

    CASE( SAG_GRID_VERT,SAG_GRID_NONE )

    CASE DEFAULT
      LastError = SAG_ERR_TYPE
      LastParm = grd%type
      GOTO 9999

  END SELECT

  CALL SAG_GetID( grd%ipgrd,pt,0,qt )

  j = SAG_TopNeighbor( grd,triT,qt )
  IF( j /= SAG_OK )GOTO 9999

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_RightTopNeighbor
!*******************************************************************************
RECURSIVE INTEGER FUNCTION SAG_RightTopNeighbor( grd,triT,prt ) RESULT( irv )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGgrid_str ),      POINTER      :: grd
TYPE( SAGtriangleT_str ), POINTER      :: triT
TYPE( SAGcell_str ),      INTENT( IN ) :: prt

TYPE( SAGcell_str ) qrt

INTEGER,                 POINTER, DIMENSION(:) :: igrd
INTEGER,                 POINTER, DIMENSION(:) :: node
TYPE( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri

INTEGER j

INTERFACE
  SUBROUTINE SAG_GetID( igrd,p,ipt,q )
    USE sagcel_fd
    INTEGER, DIMENSION(:), POINTER         :: igrd
    TYPE( SAGcell_str ),   INTENT( IN    ) :: p
    INTEGER,               INTENT( IN    ) :: ipt
    TYPE( SAGcell_str ),   INTENT( INOUT ) :: q
  END SUBROUTINE SAG_GetID
END INTERFACE

irv = SAG_ERROR

IF( prt%id == 0 )THEN
  irv = SAG_OK
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(grd%ipgrd) )THEN
  LastError = SAG_ERR_IPGRD
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  LastError = SAG_ERR_IPTRI
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%nodeT%ipcell) )THEN
  LastError = SAG_ERR_IPCELL
  LastParm  = 0
  GOTO 9999
END IF

igrd => grd%ipgrd
tri  => triT%iptri
node => triT%nodeT%ipcell

IF( igrd(prt%id) == 0 )THEN

  IF( triT%build == 0 )THEN
    tri(triT%ntri+1)%nid(2) = node(prt%id)
    triT%build = 1
  ELSE
    tri(triT%ntri+1)%nid(3) = node(prt%id)
    triT%ntri = triT%ntri+1
    IF( triT%ntri+1 > triT%mxtri )THEN
      LastError = SAG_ERR_MXTRI
      LastParm = 0
      GOTO 9999
    END IF
    tri(triT%ntri+1)%nid(1) = tri(triT%ntri)%nid(1)
    tri(triT%ntri+1)%nid(2) = tri(triT%ntri)%nid(3)
  END IF

ELSE

  CALL SAG_GetID( grd%ipgrd,prt,0,qrt )

  j = SAG_RightTopNeighbor( grd,triT,qrt )
  IF( j /= SAG_OK )GOTO 9999

END IF

irv = SAG_OK

9999 CONTINUE

RETURN
END
