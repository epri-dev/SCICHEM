!*******************************************************************************
!                SAG_BuildConnect
!*******************************************************************************
INTEGER FUNCTION SAG_BuildConnect( triT )

USE sagdef_fd
USE sagtri_fd
USE sagerr_fd
USE sagerr_fi

!     Build connections from triangles

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT

TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri
TYPE( SAGconnect_str ),  DIMENSION(:), POINTER :: conn

INTEGER i

LOGICAL, EXTERNAL :: SAG_CheckConnect

INTERFACE
  SUBROUTINE SAG_SearchBuild( ntri,i,conn,tri )
    USE sagtri_fd
    INTEGER,                               INTENT( IN ) :: i
    INTEGER,                               INTENT( IN ) :: ntri
    TYPE( SAGconnect_str ),  DIMENSION(:), POINTER      :: conn
    TYPE( SAGtriangle_str ), DIMENSION(:), POINTER      :: tri
  END SUBROUTINE SAG_SearchBuild
END INTERFACE

SAG_BuildConnect = SAG_ERROR

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

IF( triT%ntri == 0 )THEN
  LastError = SAG_ERR_NOTRI
  LastParm  = 0
  GOTO 9999
END IF

tri  => triT%iptri
conn => triT%ipconn

DO i = 1,triT%ntri
  IF( SAG_CheckConnect(conn(i)) )CALL SAG_SearchBuild( triT%ntri,i,conn,tri )
END DO

SAG_BuildConnect = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_CheckConnect
!*******************************************************************************
LOGICAL FUNCTION SAG_CheckConnect( conn )

USE sagtri_fd

!     Check to see if any connection needs to be set

IMPLICIT NONE

TYPE( SAGconnect_str ), INTENT( IN ) :: conn

LOGICAL, EXTERNAL :: SAG_CheckSide

SAG_CheckConnect = SAG_CheckSide( conn,1 ) .OR. &
                   SAG_CheckSide( conn,2 ) .OR. &
                   SAG_CheckSide( conn,3 )

RETURN
END
!*******************************************************************************
!                SAG_CheckSide
!*******************************************************************************
LOGICAL FUNCTION SAG_CheckSide( conn,i )

USE sagtri_fd

!     Check to see if this side needs to be set

IMPLICIT NONE

TYPE( SAGconnect_str ), INTENT( IN ) :: conn
INTEGER,                INTENT( IN ) :: i

SAG_CheckSide = ( conn%tid(i) == 0 )

RETURN
END
!*******************************************************************************
!                SAG_SearchBuild
!*******************************************************************************
SUBROUTINE SAG_SearchBuild( ntri,i,conn,tri )

USE sagtri_fd

!     Build connections from triangles

IMPLICIT NONE

INTEGER,                               INTENT( IN ) :: i
INTEGER,                               INTENT( IN ) :: ntri
TYPE( SAGconnect_str ),  DIMENSION(:), POINTER      :: conn
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER      :: tri

INTEGER ns,n,ii

LOGICAL check1,check2,check3,check

LOGICAL, EXTERNAL :: SAG_CheckSide

INTERFACE
  SUBROUTINE SAG_SetConnection( i,ii,conn,tri,check1,check2,check3 )
    USE sagtri_fd
    INTEGER,                               INTENT( IN    ) :: ii
    INTEGER,                               INTENT( IN    ) :: i
    TYPE( SAGtriangle_str ), DIMENSION(:), POINTER         :: tri
    TYPE( SAGconnect_str ),  DIMENSION(:), POINTER         :: conn
    LOGICAL,                               INTENT( INOUT ) :: check1
    LOGICAL,                               INTENT( INOUT ) :: check2
    LOGICAL,                               INTENT( INOUT ) :: check3
  END SUBROUTINE SAG_SetConnection
END INTERFACE

ns = MAX(ntri-i,i-1)

check1 = SAG_CheckSide( conn(i),1 )
check2 = SAG_CheckSide( conn(i),2 )
check3 = SAG_CheckSide( conn(i),3 )
check  = check1 .OR. check2 .OR. check3

n = 0

DO WHILE( check .AND. (n <= ns) )

  n = n + 1

  ii = i - n
  IF( ii > 0 )CALL SAG_SetConnection( i,ii,conn,tri,check1,check2,check3 )

  ii = i + n
  IF( ii <= ntri )CALL SAG_SetConnection( i,ii,conn,tri,check1,check2,check3 )

  check  = check1 .OR. check2 .OR. check3

END DO

IF( check1 )conn(i)%tid(1) = -1
IF( check2 )conn(i)%tid(2) = -1
IF( check3 )conn(i)%tid(3) = -1

RETURN
END
!*******************************************************************************
!                SAG_SetConnection
!*******************************************************************************
SUBROUTINE SAG_SetConnection( i,ii,conn,tri,check1,check2,check3 )

USE sagtri_fd

!     Build connections from a triangle

IMPLICIT NONE

INTEGER,                               INTENT( IN    ) :: ii
INTEGER,                               INTENT( IN    ) :: i
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER         :: tri
TYPE( SAGconnect_str ),  DIMENSION(:), POINTER         :: conn
LOGICAL,                               INTENT( INOUT ) :: check1
LOGICAL,                               INTENT( INOUT ) :: check2
LOGICAL,                               INTENT( INOUT ) :: check3

INTERFACE
  SUBROUTINE SAG_TestSide( i,ii,conn,tri,n1,n2,check )
    USE sagtri_fd
    INTEGER,                               INTENT( IN    ) :: i
    INTEGER,                               INTENT( IN    ) :: ii
    TYPE( SAGtriangle_str ), DIMENSION(:), POINTER         :: tri
    TYPE( SAGconnect_str ),  DIMENSION(:), POINTER         :: conn
    INTEGER,                               INTENT( IN    ) :: n1
    INTEGER,                               INTENT( IN    ) :: n2
    LOGICAL,                               INTENT( INOUT ) :: check
  END SUBROUTINE SAG_TestSide
END INTERFACE

!------ check side 1

IF( check1 )THEN
  CALL SAG_TestSide( i,ii,conn,tri,tri(i)%nid(1),tri(i)%nid(2),check1 )
  IF( .NOT.check1 )conn(i)%tid(1) = ii
END IF

!------ check side 2

IF( check2 )THEN
  CALL SAG_TestSide( i,ii,conn,tri,tri(i)%nid(3),tri(i)%nid(2),check2 )
  IF( .NOT.check2 )conn(i)%tid(2) = ii
END IF

!------ check side 3

IF( check3 )THEN
  CALL SAG_TestSide( i,ii,conn,tri,tri(i)%nid(1),tri(i)%nid(3),check3 )
  IF( .NOT.check3 )conn(i)%tid(3) = ii
END IF

RETURN
END
!*******************************************************************************
!                SAG_TestSide
!*******************************************************************************
SUBROUTINE SAG_TestSide( i,ii,conn,tri,n1,n2,check )

USE sagtri_fd

!     Build connections from a triangle

IMPLICIT NONE

INTEGER,                               INTENT( IN    ) :: i
INTEGER,                               INTENT( IN    ) :: ii
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER         :: tri
TYPE( SAGconnect_str ),  DIMENSION(:), POINTER         :: conn
INTEGER,                               INTENT( IN    ) :: n1
INTEGER,                               INTENT( IN    ) :: n2
LOGICAL,                               INTENT( INOUT ) :: check

IF( tri(ii)%nid(1) == n1 )THEN
  IF( tri(ii)%nid(2) == n2 )THEN
    check = .FALSE.
    conn(ii)%tid(1) = i
  ELSE IF( tri(ii)%nid(3) == n2 )THEN
    check = .FALSE.
    conn(ii)%tid(3) = i
  END IF

ELSE IF( tri(ii)%nid(1) == n2 )THEN
  IF( tri(ii)%nid(2) == n1 )THEN
    check = .FALSE.
    conn(ii)%tid(1) = i
  ELSE IF( tri(ii)%nid(3) == n1 )THEN
    check = .FALSE.
    conn(ii)%tid(3) = i
  END IF

ELSE IF( tri(ii)%nid(2) == n1 )THEN
  IF( tri(ii)%nid(3) == n2 )THEN
    check = .FALSE.
    conn(ii)%tid(2) = i
  END IF

ELSE IF( tri(ii)%nid(2) == n2 )THEN
  IF( tri(ii)%nid(3) == n1 )THEN
    check = .FALSE.
    conn(ii)%tid(2) = i
  END IF

END IF

RETURN
END
!*******************************************************************************
!                SAG_CountEdges
!*******************************************************************************
INTEGER FUNCTION SAG_CountEdges( triT )

USE sagerr_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT

TYPE( SAGconnect_str ), POINTER, DIMENSION(:) :: conn

INTEGER nze,i

INTEGER, EXTERNAL :: SAG_NumberEdges

SAG_CountEdges = 0

IF( .NOT.ASSOCIATED( triT%ipconn ) )THEN
  LastError = SAG_ERR_IPCONN
  LastParm  = 0
  GOTO 9999
END IF

conn => triT%ipconn

nze = 0

DO i = 1,triT%ntri
  nze = nze + SAG_NumberEdges( conn(i) )
END DO

SAG_CountEdges = nze

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_NumberEdges
!*******************************************************************************
INTEGER FUNCTION SAG_NumberEdges( conn ) RESULT( nze )

USE sagtri_fd

IMPLICIT NONE

TYPE( SAGconnect_str ), INTENT( IN ) :: conn

INTEGER i

LOGICAL, EXTERNAL :: SAG_IsEdge

nze = 0

DO i = 1,3
  IF( SAG_IsEdge(conn%tid(i)) )nze = nze + 1
END DO

RETURN
END
!*******************************************************************************
!                SAG_CheckEdge
!*******************************************************************************
LOGICAL FUNCTION SAG_CheckEdge( conn )

USE sagtri_fd

!     Check to see if this connection contains an edge

IMPLICIT NONE

TYPE( SAGconnect_str )  conn

LOGICAL, EXTERNAL :: SAG_IsEdge

SAG_CheckEdge = SAG_IsEdge( conn%tid(1) ) .OR. &
                SAG_IsEdge( conn%tid(2) ) .OR. &
                SAG_IsEdge( conn%tid(3) )

RETURN
END
!*******************************************************************************
!                SAG_IsEdge
!*******************************************************************************
LOGICAL FUNCTION SAG_IsEdge( tid )

!     Check to see if this connection element is an edge

IMPLICIT NONE

INTEGER tid

SAG_IsEdge = tid < 0

RETURN
END
!*******************************************************************************
!                SAG_BuildEdges
!*******************************************************************************
INTEGER FUNCTION SAG_BuildEdges( triT )

USE sagdef_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT

TYPE( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri
TYPE( SAGconnect_str ),  POINTER, DIMENSION(:) :: conn
TYPE( SAGedge_str ),     POINTER, DIMENSION(:) :: edge
TYPE( SAGedgenode_str ), POINTER, DIMENSION(:) :: node

LOGICAL, ALLOCATABLE, DIMENSION(:) :: test_edge

INTEGER i,ii,nze,alloc_stat

LOGICAL found

LOGICAL, EXTERNAL :: SAG_CheckEdge,SAG_IsEdge

SAG_BuildEdges = SAG_ERROR

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

IF( .NOT.ASSOCIATED(triT%ipedge) )THEN
  LastError = SAG_ERR_IPEDGE
  LastParm  = 0
  GOTO 9999
END IF

IF( .NOT.ASSOCIATED(triT%ipenod) )THEN
  LastError = SAG_ERR_IPENOD
  LastParm  = 0
  GOTO 9999
END IF

tri  => triT%iptri
conn => triT%ipconn
edge => triT%ipedge
node => triT%ipenod

IF( triT%ntri <= 0 )THEN
  LastError = SAG_ERR_NOTRI
  LastParm  = 0
  GOTO 9999
END IF

nze = 0
DO i = 1,triT%ntri
  IF( SAG_CheckEdge(conn(i)) )THEN
    DO ii = 1,3
      IF( SAG_IsEdge(conn(i)%tid(ii)) )THEN
        nze = nze + 1
        edge(nze)%tid = i
        edge(nze)%sid = ii
        SELECT CASE( ii )
          CASE( 1 )
            edge(nze)%nid(1) = tri(i)%nid(1)
            edge(nze)%nid(2) = tri(i)%nid(2)
          CASE( 2 )
            edge(nze)%nid(1) = tri(i)%nid(2)
            edge(nze)%nid(2) = tri(i)%nid(3)
          CASE( 3 )
            edge(nze)%nid(1) = tri(i)%nid(3)
            edge(nze)%nid(2) = tri(i)%nid(1)
          CASE DEFAULT
        END SELECT
      END IF
    END DO
  END IF
END DO

triT%nedge = nze

ALLOCATE( test_edge(triT%nedge),STAT=alloc_stat )
IF( alloc_stat /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastParm  = triT%nedge
  GOTO 9999
END IF

DO i = 1,triT%nedge
  test_edge(i) = .TRUE.
END DO

nze = 1
node(nze)%nid = edge(1)%nid(1)
node(nze)%tid = edge(1)%tid
node(nze)%sid = edge(1)%sid
test_edge(nze) = .FALSE.
DO WHILE( nze < triT%nedge )
  found = .FALSE.
  DO ii = 1,triT%nedge
    IF( test_edge(ii) )THEN
      IF( edge(ii)%nid(1) == node(nze)%nid )THEN
        nze = nze + 1
        node(nze)%nid = edge(ii)%nid(2)
        node(nze)%tid = edge(ii)%tid
        node(nze)%sid = edge(ii)%sid
        test_edge(ii) = .FALSE.
        found    = .TRUE.
      ELSE IF( edge(ii)%nid(2) == node(nze)%nid )THEN
        nze = nze + 1
        node(nze)%nid = edge(ii)%nid(1)
        node(nze)%tid = edge(ii)%tid
        node(nze)%sid = edge(ii)%sid
        test_edge(ii) = .FALSE.
        found    = .TRUE.
      END IF
    END IF
  END DO
  IF( .NOT.found )THEN
    LastError = SAG_ERR_MIBE
    LastParm  = 0
    GOTO 9998
  END IF
END DO

SAG_BuildEdges = SAG_OK

9998 CONTINUE
DEALLOCATE( test_edge,STAT=alloc_stat )

9999 CONTINUE

RETURN
END
