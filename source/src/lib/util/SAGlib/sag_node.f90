!*******************************************************************************
!               SAG_NodesID
!*******************************************************************************
INTEGER FUNCTION SAG_NodesID( grdI,nfld,ifld )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi
USE sagnod_usr
USE PtrGrdStrItf

IMPLICIT NONE

INTEGER              , INTENT( IN ) :: grdI
INTEGER              , INTENT( IN ) :: nfld
INTEGER, DIMENSION(*), INTENT( IN ) :: ifld

TYPE( SAGgrid_str ),      POINTER :: grd
TYPE( SAGnodeT_str ),     POINTER :: node
TYPE( SAGTriangleT_str ), POINTER :: tri

INTERFACE
  INTEGER FUNCTION SAG_Nodes( grd,nfld,ifld,nodeI )
    USE sagstr_fd
    USE sagnod_fd
    TYPE( SAGgrid_str ),  POINTER :: grd
    INTEGER                       :: nfld
    INTEGER, DIMENSION(*), TARGET :: ifld
    TYPE( SAGnodeT_str ),  TARGET :: nodeI
  END FUNCTION SAG_Nodes
END INTERFACE

SAG_NodesID = SAG_ERROR

!==== Point to structure

grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

tri => SAG_PtrTriStr( grdI )
IF( .NOT.ASSOCIATED(tri) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

node => tri%nodeT

SAG_NodesID = SAG_Nodes( grd,nfld,ifld,node )

9999 CONTINUE

RETURN
END

!*******************************************************************************
!               SAG_Nodes
!*******************************************************************************
INTEGER FUNCTION SAG_Nodes( grd,nfld,ifld,nodeI )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagnod_fd
USE sagerr_fi
USE sagnod_usr

IMPLICIT NONE

TYPE( SAGgrid_str ),   POINTER :: grd
INTEGER                        :: nfld
INTEGER, DIMENSION(*),  TARGET :: ifld
TYPE( SAGnodeT_str ),   TARGET :: nodeI

LOGICAL restore
INTEGER i

INTEGER, EXTERNAL :: SAG_SetNodeValue
INTEGER, EXTERNAL :: SAG_ResetNodeValue

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

!==== Initialize call to SAG_BottomFunction

SAG_Nodes = SAG_ERROR

nfld_nod =  nfld
ifld_nod => ifld(1:nfld)
nodeT    => nodeI

IF( .NOT.ASSOCIATED(nodeT%ipnode) )THEN
  LastError = SAG_ERR_IPNODE
  LastParm  = 0
  GOTO 9999
END IF

IF( nfld > 0 )THEN
  IF( .NOT.ASSOCIATED(nodeT%ipdata) )THEN
    LastError = SAG_ERR_IPDATA
    LastParm  = 0
    GOTO 9999
  END IF

  IF( nfld > nodeT%mxdata )THEN
    LastError = SAG_ERR_MXDATA
    LastParm  = nfld
    GOTO 9999
  END IF

  DO i = 1,nfld
    IF( (ifld_nod(i) <= 0) .OR. (ifld_nod(i) > grd%mxfld) )THEN
      LastError = SAG_ERR_IFLD
      LastParm  = ifld_nod(i)
      GOTO 9999
    END IF
  END DO
END IF

restore = .TRUE.

grdN => grd ! for use in SAG_SetNodeValue

IF( nodeT%nnode == 0 )THEN
  SAG_Nodes = SAG_BottomFunction( grd,SAG_SetNodeValue,nfld,ifld_nod,restore,.TRUE. )    !Build Node list
ELSE
  SAG_Nodes = SAG_BottomFunction( grd,SAG_ResetNodeValue,nfld,ifld_nod,restore,.TRUE. )  !Just reset Node data values
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_SetNodeValue
!*******************************************************************************
INTEGER FUNCTION SAG_SetNodeValue( datg,mxgrd,p0 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagnod_fd
USE sagerr_fi
USE sagnod_usr

IMPLICIT NONE

REAL, DIMENSION(:),  POINTER      :: datg
INTEGER,             INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGnode_str ), POINTER, DIMENSION(:) :: node

REAL,    POINTER, DIMENSION(:) :: datn
INTEGER, POINTER, DIMENSION(:) :: cell

INTEGER i,in,ig,nn

!==== Initialize result

SAG_SetNodeValue = SAG_ERROR

IF( .NOT.ASSOCIATED(nodeT%ipnode) )THEN
  LastError = SAG_ERR_IPNODE
  LastParm  = 0
  GOTO 9999
END IF

IF( nfld_nod > 0 )THEN

  IF( .NOT.ASSOCIATED(nodeT%ipdata) )THEN
    LastError = SAG_ERR_IPDAT
    LastParm  = 0
    GOTO 9999
  END IF

END IF

node => nodeT%ipnode
datn => nodeT%ipdata
cell => nodeT%ipcell

!$OMP ATOMIC CAPTURE
nodeT%nnode = nodeT%nnode + 1
nn = nodeT%nnode
!$OMP END ATOMIC
IF( nn > nodeT%mxnode )THEN
  LastError = SAG_ERR_MXNODE
  LastParm  = 0
  GOTO 9999
END IF

node(nn)%id = p0%id
node(nn)%lev= p0%lev
node(nn)%x  = p0%x
node(nn)%y  = p0%y
node(nn)%hx = p0%hx
node(nn)%hy = p0%hy

IF( ASSOCIATED(nodeT%ipcell) )cell(p0%id) = nn

IF( nfld_nod > 0 )THEN
  DO i = 1,nfld_nod
    IF( ifld_nod(i) <= 0 .OR. ifld_nod(i) > grdN%mxfld )THEN
      LastError = SAG_ERR_IFLD
      LastParm  = ifld_nod(i)
      GOTO 9999
    END IF
    ig = (ifld_nod(i)-1)*mxgrd + p0%id
    in = (nn-1)*nodeT%mxdata + i
    datn(in) = datg(ig)
  END DO
END IF

SAG_SetNodeValue = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!               SAG_ResetNodeValue
!*******************************************************************************
INTEGER FUNCTION SAG_ResetNodeValue( datg,mxgrd,p0 )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagcel_fd
USE sagnod_fd
USE sagerr_fi
USE sagnod_usr

IMPLICIT NONE

REAL, DIMENSION(:),  POINTER      :: datg
INTEGER,             INTENT( IN ) :: mxgrd
TYPE( SAGcell_str ), INTENT( IN ) :: p0

TYPE( SAGnode_str ), POINTER, DIMENSION(:) :: node

REAL,    POINTER, DIMENSION(:) :: datn
INTEGER, POINTER, DIMENSION(:) :: cell

INTEGER i,in,ig,nn

!==== Initialize result

SAG_ResetNodeValue = SAG_ERROR

IF( .NOT.ASSOCIATED(nodeT%ipnode) )THEN
  LastError = SAG_ERR_IPNODE
  LastParm  = 0
  GOTO 9999
END IF

IF( nfld_nod > 0 )THEN

  IF( .NOT.ASSOCIATED(nodeT%ipdata) )THEN
    LastError = SAG_ERR_IPDAT
    LastParm  = 0
    GOTO 9999
  END IF

END IF

node => nodeT%ipnode
datn => nodeT%ipdata
cell => nodeT%ipcell

IF( ASSOCIATED(nodeT%ipcell) )THEN
  nn = cell(p0%id)

  IF( nfld_nod > 0 )THEN
    DO i = 1,nfld_nod
      IF( ifld_nod(i) <= 0 .OR. ifld_nod(i) > grdN%mxfld )THEN
        LastError = SAG_ERR_IFLD
        LastParm  = ifld_nod(i)
        GOTO 9999
      END IF
      ig = (ifld_nod(i)-1)*mxgrd + p0%id
      in = (nn-1)*nodeT%mxdata + i
      datn(in) = datg(ig)
    END DO
  END IF

END IF

SAG_ResetNodeValue = SAG_OK

9999 CONTINUE

RETURN
END

!*******************************************************************************
!                SAG_InitNode
!*******************************************************************************
INTEGER FUNCTION SAG_InitNode( mxcell,mxnode,mxdata,nodeT )

USE sagdef_fd
USE sagnod_fd
USE sagerr_fd
USE sagerr_fi

!     Initialize a SAG node array structure

IMPLICIT NONE

INTEGER,              INTENT( IN  ) :: mxcell
INTEGER,              INTENT( IN  ) :: mxnode
INTEGER,              INTENT( IN  ) :: mxdata
TYPE( SAGnodeT_str ), INTENT( OUT ) ::  nodeT

INTEGER i,ios

SAG_InitNode = SAG_ERROR

IF( mxnode > 0 )THEN

  ALLOCATE( nodeT%ipnode(mxnode),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxnode
    GOTO 9999
  END IF

  IF( mxdata > 0 )THEN
    ALLOCATE( nodeT%ipdata(mxdata*mxnode),STAT=ios )
    IF( ios /= 0 )THEN
      LastError = SAG_ERR_MALLOC
      LastParm  = mxdata*mxnode
      GOTO 9998
    END IF
  END IF

END IF

IF( mxcell > 0 )THEN
  ALLOCATE( nodeT%ipcell(mxcell),STAT=ios )
  IF( ios /= 0 )THEN
    LastError = SAG_ERR_MALLOC
    LastParm  = mxcell
    GOTO 9997
  END IF
END IF

nodeT%mxnode = mxnode
nodeT%mxdata = mxdata
nodeT%mxcell = mxcell

nodeT%nnode = 0
nodeT%ncell = 0

IF( mxnode > 0 )THEN
  DO i = 1,mxnode
    nodeT%ipnode(i)%id  = 0
    nodeT%ipnode(i)%lev = 0
    nodeT%ipnode(i)%x   = 0.
    nodeT%ipnode(i)%y   = 0.
    nodeT%ipnode(i)%hx  = 0.
    nodeT%ipnode(i)%hy  = 0.
  END DO
  IF( mxdata > 0 )THEN
    DO i = 1,mxnode*mxdata
      nodeT%ipdata(i) = 0.0
    END DO
  END IF
END IF

IF( mxcell > 0 )THEN
  DO i = 1,mxcell
    nodeT%ipcell(i) = 0
  END DO
END IF

SAG_InitNode = SAG_OK

9999 CONTINUE

RETURN

9997  IF( ASSOCIATED(nodeT%ipdata) )DEALLOCATE( nodeT%ipdata,STAT=ios )
9998  IF( ASSOCIATED(nodeT%ipnode) )DEALLOCATE( nodeT%ipnode,STAT=ios )
GOTO 9999

END
!*******************************************************************************
!                SAG_FreeNode
!*******************************************************************************
SUBROUTINE SAG_FreeNode( nodeT )

USE sagnod_fd

!     Frees memory used by a SAG node array structure

IMPLICIT NONE

INTEGER ios

TYPE( SAGnodeT_str ), INTENT( INOUT ) :: nodeT

IF( ASSOCIATED(nodeT%ipnode) )DEALLOCATE( nodeT%ipnode,STAT=ios )
IF( ASSOCIATED(nodeT%ipdata) )DEALLOCATE( nodeT%ipdata,STAT=ios )
IF( ASSOCIATED(nodeT%ipcell) )DEALLOCATE( nodeT%ipcell,STAT=ios )
NULLIFY( nodeT%ipnode )
NULLIFY( nodeT%ipdata )
NULLIFY( nodeT%ipcell )

RETURN
END
