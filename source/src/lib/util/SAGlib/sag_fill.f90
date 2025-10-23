!*******************************************************************************
!                SAG_TriangleFill
!*******************************************************************************
INTEGER FUNCTION SAG_TriangleFill( id,tri_node,tri_data,nfld,UserFill,grd,use8 )

USE sagdef_fd
USE sagnod_fd
USE sagerr_fd
USE sagdrw_fd
USE sagfil_fd
USE saggrd_fi
USE sagerr_fi
USE sagdrw_usr
USE sagstr_fd

!     Fill a triangle

IMPLICIT NONE

INTEGER,                      INTENT( IN ) :: nfld
INTEGER,        DIMENSION(3), INTENT( IN ) :: id
TYPE( SAGnode_str ), POINTER, DIMENSION(:) :: tri_node
REAL,                POINTER, DIMENSION(:) :: tri_data
TYPE( SAGgrid_str ), POINTER               :: grd
LOGICAL,                      INTENT( IN ) :: use8

INTEGER, EXTERNAL :: UserFill

REAL, POINTER, DIMENSION(:)       :: level
REAL,          DIMENSION(MAXPOLY) :: xpoly
REAL,          DIMENSION(MAXPOLY) :: ypoly
REAL,          DIMENSION(MAXPOLY) :: xxpoly
REAL,          DIMENSION(MAXPOLY) :: yypoly
REAL(8),       DIMENSION(MAXPOLY) :: xdpoly
REAL(8),       DIMENSION(MAXPOLY) :: ydpoly

REAL    v1,v2,v3,fill_value
REAL    node_min,node_max
INTEGER min_level,max_level,i,fill_level,irv
INTEGER npoly,color_index,color_min,color_max

TYPE( SAGfillnode_str ) node(3)
TYPE( SAGfillside_str ) side(3)
TYPE( SAGnode_str     ) point(2)

INTEGER, EXTERNAL :: SAG_PolyFill
INTEGER, EXTERNAL :: SAG_PolyFill8

!==== Initialize

SAG_TriangleFill = SAG_ERROR

IF( .NOT.ASSOCIATED(draw%iplev) )THEN
  LastError = SAG_ERR_IPLEV
  LastParm  = 0
  GOTO 9999
END IF

level => draw%iplev

IF( draw%nlev <= 0 )THEN
  LastError = SAG_ERR_NOLEV
  LastParm  = 0
  GOTO 9999
END IF

!==== Check for special values - if any vertex value is special return

v1 = tri_data(id(1))
v2 = tri_data(id(2))
v3 = tri_data(id(3))

IF( UseSpecial )THEN
  IF( v1 == Special .OR. v2 == Special .OR. v3 == Special )THEN
    SAG_TriangleFill = SAG_OK
    GOTO 9999
  END IF
END IF

!==== Set Polygon to entire triangle

xpoly(1) = tri_node(id(1))%x
ypoly(1) = tri_node(id(1))%y
xpoly(2) = tri_node(id(2))%x
ypoly(2) = tri_node(id(2))%y
xpoly(3) = tri_node(id(3))%x
ypoly(3) = tri_node(id(3))%y
npoly = 3

IF( use8 )THEN
  DO i = 1,npoly
    xdpoly(i) = DBLE(grd%xmin) + DBLE(grd%dx)*DBLE(xpoly(i))
    ydpoly(i) = DBLE(grd%ymin) + DBLE(grd%dy)*DBLE(ypoly(i))
  END DO
ELSE
  DO i = 1,npoly
    xxpoly(i) = grd%xmin + grd%dx*xpoly(i)
    yypoly(i) = grd%ymin + grd%dy*ypoly(i)
  END DO
END IF

!==== Use UserFill to test entire triangle for visibility

color_index = -1

IF( use8 )THEN
  irv = SAG_PolyFill8( npoly,xdpoly,ydpoly,color_index,UserFill )
ELSE
  irv = SAG_PolyFill( npoly,xxpoly,yypoly,color_index,UserFill )
END IF
IF( irv /= SAG_OK )GOTO 9999

!==== Not visible - return

IF( color_index <= 0 )THEN
  SAG_TriangleFill = SAG_OK
  GOTO 9999
END IF

!==== Find Triangle min and max values to test for single color

node_min = MIN(v1,v2,v3)
node_max = MAX(v1,v2,v3)

!==== If max is below the first level - fill and return

fill_value = level(draw%start)

IF( node_max <= fill_value )THEN
  IF( draw%fill_lo )THEN
    color_index = (draw%nlev+1) - draw%start + 1
    IF( use8 )THEN
      irv = SAG_PolyFill8( npoly,xdpoly,ydpoly,color_index,UserFill )
    ELSE
      irv = SAG_PolyFill( npoly,xxpoly,yypoly,color_index,UserFill )
    END IF
    IF( irv /= SAG_OK )GOTO 9999
  END IF
  SAG_TriangleFill = SAG_OK
  GOTO 9999
END IF

!==== If min is above the last level - fill and return

fill_value = level(draw%stop)

IF( node_min >= fill_value )THEN
  IF( draw%fill_hi )THEN
    color_index = (draw%nlev+1) - (draw%stop+1) + 1
    IF( use8 )THEN
      irv = SAG_PolyFill8( npoly,xdpoly,ydpoly,color_index,UserFill )
    ELSE
      irv = SAG_PolyFill( npoly,xxpoly,yypoly,color_index,UserFill )
    END IF
    IF( irv /= SAG_OK )GOTO 9999
  END IF
  SAG_TriangleFill = SAG_OK
  GOTO 9999
END IF

!==== Find range of levels covered by the data
!     min_level = largest  level below node_min
!     max_level = smallest level above node_max

min_level = draw%start - 1
max_level = draw%start

DO i = draw%start,draw%stop
  fill_value = level(i)
  IF( fill_value <= node_min )min_level = i
  IF( fill_value <  node_max )max_level = i + 1
END DO

!==== Check for single color triangle - Fill and return

IF( max_level-min_level <= 1 )THEN
  IF( min_level < draw%start )THEN
    IF( draw%fill_lo )THEN
      color_index = (draw%nlev+1) - draw%start + 1
    ELSE
      color_index = -1
    END IF
  ELSE IF( max_level > draw%stop )THEN
    IF( draw%fill_hi )THEN
      color_index = (draw%nlev+1) - (draw%stop+1) + 1
    ELSE
      color_index = -1
    END IF
  ELSE
    color_index = (draw%nlev + 1) - max_level + 1
  END IF
  IF( use8 )THEN
    irv = SAG_PolyFill8( npoly,xdpoly,ydpoly,color_index,UserFill )
  ELSE
    irv = SAG_PolyFill( npoly,xxpoly,yypoly,color_index,UserFill )
  END IF
  IF( irv /= SAG_OK )GOTO 9999
  SAG_TriangleFill = SAG_OK
  GOTO 9999
END IF

!==== Multiple level triangle - Loop over levels and fill below level
!       If not filling above highest level then can stop at level=nlev
!       Assume level(nlev+1) = MAXFAC*node_max
!       Fill area is always composed of two points and either a node or a side
!       Nodes are represented by either a single triangle vertex or a pair of points
!       Sides are represented by either a pair of nodes or a pair of points
!       Interpolations are always done between triangle vertices
!
!             n1                      n1(H)                       n1(L)
!             /\                       /\                          **
!            /  \                     /  \                        ****
!           /    \                   /    \ p1                   ****** p2
!        s3/      \s2               /     /*                    ******/\
!         /        \               /   n3/***                  ******/  \
!        /          \             /     /*****                ******/s3  \
!     n2/____________\n3    n2(H)/_____/*******n3(L)    n2(L)******/______n3(H)
!             s1                      p2                          p1
!         Notation             Example - Hi-Hi-Low         Example - Low-Low-Hi
!                                  Node moves                  Side moves
!                                 Fill=P1-P2-N3               Fill=P1-P2-S3(N1-N2)
!
!==== Set Nodes and Sides

node(1)%node = tri_node(id(1))
node(2)%node = tri_node(id(2))
node(3)%node = tri_node(id(3))

IF( draw%log_interp )THEN
  v1 = LOG(MAX(v1,LOGMIN))
  v2 = LOG(MAX(v2,LOGMIN))
  v3 = LOG(MAX(v3,LOGMIN))
  node_max = LOG(MAX(node_max,LOGMIN))
  node_min = LOG(MAX(node_min,LOGMIN))
END IF

node(1)%value = v1
node(2)%value = v2
node(3)%value = v3

node(1)%UseNode = .TRUE.
node(2)%UseNode = .TRUE.
node(3)%UseNode = .TRUE.

side(1)%node_id(1) = 2
side(1)%node_id(2) = 3
side(2)%node_id(1) = 3
side(2)%node_id(2) = 1
side(3)%node_id(1) = 1
side(3)%node_id(2) = 2

side(1)%UseNode = .TRUE.
side(2)%UseNode = .TRUE.
side(3)%UseNode = .TRUE.

!==== Loop over levels

IF( draw%fill_hi )THEN
  color_min = (draw%nlev + 1) - (draw%stop + 1) + 1
ELSE
  color_min = (draw%nlev + 1) - (draw%stop    ) + 1
END IF

IF( draw%fill_lo )THEN
  color_max = (draw%nlev + 1) - (draw%start    ) + 1
ELSE
  color_max = (draw%nlev + 1) - (draw%start + 1) + 1
END IF

IF( .NOT.draw%fill_hi)max_level = MIN0(max_level,draw%nlev)

DO fill_level = min_level+1,max_level

!====== Set level value

  IF( fill_level > draw%stop )THEN
    IF( draw%log_interp )THEN
      fill_value = LOG(MAXFAC) + node_max
    ELSE
      fill_value = MAXFAC*node_max
    END IF
  ELSE
    IF( draw%log_interp )THEN
      fill_value = LOG(MAX(level(fill_level),LOGMIN))
    ELSE
      fill_value = level(fill_level)
    END IF
  END IF

!====== Set color index value

  color_index = (draw%nlev + 1) - fill_level + 1

!====== Zero out fill Polygon

  npoly = 0

!====== Check node values to determine how to build polygon

!====== N1 Low

  IF( node(1)%value < fill_value )THEN

!====== N1 Low : N2 High -> N1 < P1 < N2

    IF( node(2)%value >= fill_value )THEN

      CALL SAG_FillInterpPoint( node(1),node(2),fill_value,point(1) )
      CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(1) )

!====== N1 Low : N2 High : N3 High -> N1 < P2 < N3 : Close via N1

      IF( node(3)%value >= fill_value )THEN

        CALL SAG_FillInterpPoint( node(1),node(3),fill_value,point(2) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )
        CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(1) )

        CALL SAG_FillMoveNode( node(1),point(2),point(1) )

!====== N1 Low : N2 High : N3 Low -> N3 < P2 < N2 : Close via S2

      ELSE

        CALL SAG_FillInterpPoint( node(3),node(2),fill_value,point(2) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )
        CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(2),node )

        CALL SAG_FillMoveSide( side(2),point(2),point(1) )

      END IF

!====== N1 Low : N2 Low

    ELSE

!====== N1 Low : N2 Low : N3 Low : Fill whats left : Close via the node which is still
!                                                    a triangle vertex and its opposide side
!                                                    Only reached when fill_level=max_level

      IF( node(3)%value < fill_value )THEN

         IF( node(1)%UseNode )THEN
           CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(1) )
           CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(1),node )
         ELSE IF( node(2)%UseNode )THEN
           CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(2) )
           CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(2),node )
         ELSE IF( node(3)%UseNode )THEN
           CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(3) )
           CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(3),node )
         END IF

!====== N1 Low : N2 Low : N3 High -> N2 < P1 < N3 & N1 < P2 < N3 : Close via S3

      ELSE

        CALL SAG_FillInterpPoint( node(2),node(3),fill_value,point(1) )
        CALL SAG_FillInterpPoint( node(1),node(3),fill_value,point(2) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(1) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )
        CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(3),node )

        CALL SAG_FillMoveSide( side(3),point(2),point(1) )

      END IF

    END IF

!====== N1 High

  ELSE

!====== N1 High : N2 Low -> N2 < P1 < N1

    IF( node(2)%value < fill_value )THEN

      CALL SAG_FillInterpPoint( node(2),node(1),fill_value,point(1) )
      CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(1) )

!====== N1 High : N2 Low : N3 Low -> N3 < P2 < N1 : Close via S1

      IF( node(3)%value < fill_value )THEN

        CALL SAG_FillInterpPoint( node(3),node(1),fill_value,point(2) )
        CALL SAG_FillAddPolySide( npoly,xpoly,ypoly,side(1),node )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )

        CALL SAG_FillMoveSide( side(1),point(1),point(2) )

!====== N1 High : N2 Low : N3 High -> N2 < P2 < N3 : Close via N2

      ELSE

        CALL SAG_FillInterpPoint( node(2),node(3),fill_value,point(2) )
        CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(2) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )

        CALL SAG_FillMoveNode( node(2),point(1),point(2) )

      END IF

!====== N1 High : N2 High

    ELSE

!====== N1 High : N2 High : N3 High -> OOPS

      IF( node(3)%value >= fill_value )THEN

!====== N1 High : N2 High : N3 Low -> N3 < P1 < N2 & N3 < P2 < N1 : Close via N3

      ELSE

        CALL SAG_FillInterpPoint( node(3),node(2),fill_value,point(1) )
        CALL SAG_FillInterpPoint( node(3),node(1),fill_value,point(2) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(1) )
        CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(3) )
        CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point(2) )

        CALL SAG_FillMoveNode( node(3),point(1),point(2) )

      END IF

    END IF

  END IF

!====== Fill Polygon if necessary

  IF( npoly > 0 )THEN
    IF( color_index <= color_max .AND. color_index >= color_min )THEN
      IF( use8 )THEN
        DO i = 1,npoly
          xdpoly(i) = DBLE(grd%xmin) + DBLE(grd%dx)*DBLE(xpoly(i))
          ydpoly(i) = DBLE(grd%ymin) + DBLE(grd%dy)*DBLE(ypoly(i))
        END DO
        irv = SAG_PolyFill8( npoly,xdpoly,ydpoly,color_index,UserFill )
      ELSE
        DO i = 1,npoly
          xxpoly(i) = grd%xmin + grd%dx*xpoly(i)
          yypoly(i) = grd%ymin + grd%dy*ypoly(i)
        END DO
        irv = SAG_PolyFill( npoly,xxpoly,yypoly,color_index,UserFill )
      END IF
      IF( irv /= SAG_OK )GOTO 9999
    END IF
  END IF

END DO

SAG_TriangleFill = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_PolyFill
!*******************************************************************************
INTEGER FUNCTION SAG_PolyFill( npoly,xpoly,ypoly,color_index,FillFunction )

USE sagfil_fd

!     Call User Fill Function

IMPLICIT NONE

INTEGER,                    INTENT( IN    ) :: npoly
INTEGER,                    INTENT( IN    ) :: color_index
REAL,   DIMENSION(MAXPOLY), INTENT( INOUT ) :: xpoly
REAL,   DIMENSION(MAXPOLY), INTENT( INOUT ) :: ypoly
INTEGER,                    EXTERNAL        :: FillFunction

INTEGER nn

!==== Close polygon

nn = npoly + 1
xpoly(nn) = xpoly(1)
ypoly(nn) = ypoly(1)

!==== Call User specified fill routine

SAG_PolyFill = FillFunction( nn,xpoly,ypoly,color_index )

RETURN
END
!*******************************************************************************
!                SAG_PolyFill8
!*******************************************************************************
INTEGER FUNCTION SAG_PolyFill8( npoly,xpoly,ypoly,color_index,FillFunction )

USE sagfil_fd

!     Call User Fill Function

IMPLICIT NONE

INTEGER,                     INTENT( IN    ) :: npoly
INTEGER,                     INTENT( IN    ) :: color_index
REAL(8), DIMENSION(MAXPOLY), INTENT( INOUT ) :: xpoly
REAL(8), DIMENSION(MAXPOLY), INTENT( INOUT ) :: ypoly
INTEGER,                     EXTERNAL        :: FillFunction

INTEGER nn, i

!==== Close polygon

nn = npoly + 1
xpoly(nn) = xpoly(1)
ypoly(nn) = ypoly(1)

!==== Call User specified fill routine

SAG_PolyFill8 = FillFunction( nn,xpoly,ypoly,color_index )

RETURN
END
!*******************************************************************************
!                SAG_FillAddPolyNode
!*******************************************************************************
SUBROUTINE SAG_FillAddPolyNode( npoly,xpoly,ypoly,node )

USE sagfil_fd

!     Add a node to the polygon list

IMPLICIT NONE

INTEGER,                    INTENT( INOUT ) :: npoly
REAL,   DIMENSION(MAXPOLY), INTENT( INOUT ) :: xpoly
REAL,   DIMENSION(MAXPOLY), INTENT( INOUT ) :: ypoly
TYPE( SAGfillnode_str ),    INTENT( IN    ) :: node

IF( node%UseNode )THEN

!====== Node is single triangle vertex

  CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,node%node )

ELSE

!====== Node is a pair of points

  CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,node%point(1) )
  CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,node%point(2) )

END IF

RETURN
END
!*******************************************************************************
!                SAG_FillAddPolySide
!*******************************************************************************
SUBROUTINE SAG_FillAddPolySide( npoly,xpoly,ypoly,side,node )

USE sagfil_fd

!     Add a side to the polygon list

IMPLICIT NONE

INTEGER,                               INTENT( INOUT ) :: npoly
REAL,   DIMENSION(MAXPOLY),            INTENT( INOUT ) :: xpoly
REAL,   DIMENSION(MAXPOLY),            INTENT( INOUT ) :: ypoly
TYPE( SAGfillnode_str ), DIMENSION(3), INTENT( INOUT ) :: node
TYPE( SAGfillside_str ),               INTENT( IN    ) :: side

IF( side%UseNode )THEN

!====== Side marked by nodes
!====== Add nodes

  CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(side%node_id(1)) )
  CALL SAG_FillAddPolyNode( npoly,xpoly,ypoly,node(side%node_id(2)) )

!====== Disable node node useage

  node(side%node_id(1))%UseNode = .FALSE.
  node(side%node_id(2))%UseNode = .FALSE.

ELSE

!====== Side marked by points
!====== Add points

  CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,side%point(1) )
  CALL SAG_FillAddPolyPoint( npoly,xpoly,ypoly,side%point(2) )

END IF

RETURN
END
!*******************************************************************************
!                SAG_FillAddPolyPoint
!*******************************************************************************
SUBROUTINE SAG_FillAddPolyPoint( npoly,xpoly,ypoly,point )

USE sagnod_fd
USE sagfil_fd

!     Add a point to the polygon list

IMPLICIT NONE

INTEGER,                     INTENT( INOUT ) :: npoly
REAL,   DIMENSION(MAXPOLY),  INTENT( INOUT ) :: xpoly
REAL,   DIMENSION(MAXPOLY),  INTENT( INOUT ) :: ypoly
TYPE( SAGnode_str ),         INTENT( IN    ) :: point

npoly = npoly + 1
xpoly(npoly) = point%x
ypoly(npoly) = point%y

RETURN
END
!*******************************************************************************
!                SAG_FillInterpPoint
!*******************************************************************************
SUBROUTINE SAG_FillInterpPoint( node1,node2,value,point )

USE sagnod_fd
USE sagfil_fd

!     Interpolate point between two triangle vertices

IMPLICIT NONE

TYPE( SAGfillnode_str ), INTENT( IN  ) :: node1
TYPE( SAGfillnode_str ), INTENT( IN  ) :: node2
REAL,                    INTENT( IN  ) :: value
TYPE( SAGnode_str ),     INTENT( OUT ) :: point

REAL rat

REAL, EXTERNAL :: SAG_DrawRatio

rat = SAG_DrawRatio( node1%value,node1%node%lev,node2%value,node2%node%lev,value )
point%x = node1%node%x + rat*(node2%node%x - node1%node%x)
point%y = node1%node%y + rat*(node2%node%y - node1%node%y)

RETURN
END
!*******************************************************************************
!                SAG_FillMoveNode
!*******************************************************************************
SUBROUTINE SAG_FillMoveNode( node,point1,point2 )

USE sagnod_fd
USE sagfil_fd

!     Move a node from a triangle vertex to a pair of points

IMPLICIT NONE

TYPE( SAGfillnode_str ), INTENT( INOUT ) :: node
TYPE( SAGnode_str ),     INTENT( IN    ) :: point1,point2

node%UseNode = .FALSE.
node%point(1) = point1
node%point(2) = point2

RETURN
END
!*******************************************************************************
!                SAG_FillMoveSide
!*******************************************************************************
SUBROUTINE SAG_FillMoveSide( side,point1,point2 )

USE sagnod_fd
USE sagfil_fd

!     Move a side from a pair of nodes to a paitr of points

IMPLICIT NONE

TYPE( SAGfillside_str ), INTENT( INOUT ) :: side
TYPE( SAGnode_str ),     INTENT( IN    ) :: point1,point2

side%UseNode = .FALSE.
side%point(1) = point1
side%point(2) = point2

RETURN
END
