!*******************************************************************************
!                SAG_InitWriteField
!*******************************************************************************
INTEGER FUNCTION SAG_InitWriteField( grdI,        & !SAG grid ID
                                     ifld,        & !Field number
                                     lunit,       & !Logical Unit number for writing
                                     log_interp,  & !logarithmic interolation
                                     nlev,        & !Number of contour levels
                                     level,       & !Array of contour levels
                                     istart,      & !Start contour level
                                     istop,       & !Stop  contour level
                                     nheader,     & !Contour header/contour
                                     header,      & !Array of contour headers
                                     lContour,    & !Contour vs Nodes flag
                                     lclose       ) !Close contours flag

USE sagdef_fd
USE sagerr_fd
USE sagstr_fd
USE sagerr_fi
USE sagwrt_usr
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                                                  INTENT( IN ) :: grdI        !SAG grid ID
INTEGER,                                                  INTENT( IN ) :: ifld        !Field number
INTEGER,                                                  INTENT( IN ) :: lunit       !Logical Unit number for writing
LOGICAL,                                                  INTENT( IN ) :: log_interp  !logarithmic interolation
INTEGER,                                                  INTENT( IN ) :: nlev        !Number of contour levels
REAL,        DIMENSION(nlev),                     TARGET, INTENT( IN ) :: level       !Array of contour levels
INTEGER,                                                  INTENT( IN ) :: istart      !Start contour level
INTEGER,                                                  INTENT( IN ) :: istop       !Stop  contour level
INTEGER,                                                  INTENT( IN ) :: nheader     !Contour headers/contour
CHARACTER(*),DIMENSION(MAX(nlev*ABS(nheader),1)), TARGET, INTENT( IN ) :: header      !Array of contour levels
LOGICAL,                                                  INTENT( IN ) :: lContour    !Contour vs Nodes flag
LOGICAL,                                                  INTENT( IN ) :: lclose      !Close contours flag

!==============================================================================
! Local Variables
!==============================================================================
TYPE( SAGgrid_str ), POINTER :: grd

!==============================================================================
!==============================================================================
! Initialize return value
!==============================================================================
SAG_InitWriteField = SAG_ERROR

!==============================================================================
! Clear internal SAG draw structure in sagdrw_usr module
!==============================================================================
CALL SAG_FreeWriteField()

!==============================================================================
! Load draw instructions into internal SAG draw structure in sagdrw_usr module
!==============================================================================
uwrite%lun        = lunit
uwrite%ifld       = ifld
uwrite%log_interp = log_interp

uwrite%lcontour = lcontour
uwrite%lclose   = lclose
IF( lcontour )THEN
  uwrite%nlev   =  nlev
  uwrite%start  =  istart
  uwrite%stop   =  istop
  uwrite%iplev  => level
ELSE
  uwrite%nlev   =  0
  uwrite%start  =  0
  uwrite%stop   =  0
  NULLIFY( uwrite%iplev )
END IF

IF( nheader /= 0 )THEN
  uwrite%nheader = nheader
  uwrite%iphdr   => header
ELSE
  uwrite%nheader = 0
  NULLIFY( uwrite%iphdr )
END IF

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Set return value
!==============================================================================
SAG_InitWriteField = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_FreeWriteField
!*******************************************************************************
SUBROUTINE SAG_FreeWriteField()

USE sagwrt_usr

IMPLICIT NONE

!==============================================================================
! Disassociate pointers to contour levels and contour header arrays
!    NOTE - Does not Deallocate the space - calling functions responsibility
!==============================================================================
IF( ASSOCIATED(uwrite%iplev) )NULLIFY( uwrite%iplev )
IF( ASSOCIATED(uwrite%iphdr) )NULLIFY( uwrite%iphdr )

!==============================================================================
! Zero number of levels
!==============================================================================
uwrite%nlev    = 0
uwrite%nheader = 0

RETURN
END
!*******************************************************************************
!                SAG_WriteFieldID
!*******************************************************************************
INTEGER FUNCTION SAG_WriteFieldID( grdI,UserWrite )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagerr_fi
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER, INTENT( IN ) :: grdI      !SAG Grid ID
INTEGER, EXTERNAL     :: UserWrite !User Write function

!==============================================================================
! Local variables
!==============================================================================
TYPE ( SAGgrid_str )     , POINTER :: grd
TYPE ( SAGTriangleT_str ), POINTER :: triT

!==============================================================================
! Function calls
!==============================================================================
INTERFACE
  INTEGER FUNCTION SAG_WriteField( grd,triT,UserWrite )
    USE sagstr_fd
    USE sagtri_fd
    TYPE ( SAGgrid_str ),      POINTER :: grd       !SAG grid structure
    TYPE ( SAGtriangleT_str ), POINTER :: triT      !SAG triangle structure
    INTEGER, EXTERNAL                  :: UserWrite !User Write function
  END FUNCTION SAG_WriteField
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_WriteFieldID = SAG_ERROR

!==============================================================================
! Get associated SAG grid structure
!==============================================================================
grd => SAG_PtrGrdStr( grdI )
IF( .NOT.ASSOCIATED(grd) )THEN
  LastError = SAG_ERR_INVALID
  LastParm  = grdI
  GOTO 9999
END IF

!==============================================================================
! Get associated SAG triangle structure
!==============================================================================
triT => SAG_PtrTriStr( grdI )

!==============================================================================
! Call SAG_WriteField
!==============================================================================
SAG_WriteFieldID = SAG_WriteField( grd,triT,UserWrite )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_WriteField
!*******************************************************************************
INTEGER FUNCTION SAG_WriteField( grd,triT,UserWrite )

USE sagdef_fd
USE sagstr_fd
USE sagerr_fd
USE sagtri_fd
USE sagcnt_fd
USE sagerr_fi
USE sagwrt_usr

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
TYPE ( SAGgrid_str ),      POINTER :: grd       !SAG grid structure
TYPE ( SAGtriangleT_str ), POINTER :: triT      !SAG triangle structure
INTEGER, EXTERNAL                  :: UserWrite !User Write function

!==============================================================================
! Local vaiables
!==============================================================================
INTEGER                              :: ios
INTEGER                              :: irv
INTEGER                              :: ic
INTEGER                              :: ih
INTEGER                              :: nfld
INTEGER,       DIMENSION(2)          :: ifld
TYPE( SAGcontour_str )               :: contour
CHARACTER(80), DIMENSION(:), POINTER :: header

!==============================================================================
! Function calls
!==============================================================================
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

  INTEGER FUNCTION SAG_BuildContour( triT,contourI )
    USE sagtri_fd
    USE sagcnt_fd
    TYPE( SAGtriangleT_str ),       POINTER         :: triT
    TYPE( SAGcontour_str ), TARGET, INTENT( INOUT ) :: contourI
  END FUNCTION SAG_BuildContour

  INTEGER FUNCTION SAG_UserField( triT,UserFunction,grd )
    USE sagtri_fd
    USE sagstr_fd
    TYPE( SAGtriangleT_str ), POINTER  :: triT
    INTERFACE
      INTEGER FUNCTION UserFunction( ntri,tri,nnode,node,mxdata,dat )
        USE sagtri_fd
        INTEGER,                           INTENT( IN ) :: ntri
        INTEGER,                           INTENT( IN ) :: nnode
        INTEGER,                           INTENT( IN ) :: mxdata
        TYPE ( SAGtriangle_str ), DIMENSION(:), POINTER :: tri
        TYPE ( SAGnode_str ),     DIMENSION(:), POINTER :: node
        REAL,                     DIMENSION(:), POINTER :: dat
      END FUNCTION UserFunction
    END INTERFACE
    TYPE ( SAGgrid_str ),     POINTER  :: grd
  END FUNCTION SAG_UserField

  INTEGER FUNCTION SAG_UserContour( contour,UserFunction,grd )
    USE sagcnt_fd
    USE sagstr_fd
    TYPE( SAGcontour_str ), INTENT( IN ) :: contour
    INTEGER, EXTERNAL                    :: UserFunction
    TYPE ( SAGgrid_str ),     POINTER    :: grd
  END FUNCTION SAG_UserContour
END INTERFACE

!==============================================================================
! Initialize return value
!==============================================================================
SAG_WriteField = SAG_ERROR

!==============================================================================
! Build triangles if not already available
!==============================================================================
IF( .NOT.ASSOCIATED(triT%iptri) )THEN
  nfld    = 1
  ifld(1) = uwrite%ifld
  irv     = SAG_Triangles( grd,nfld,ifld,.TRUE.,triT )
  IF( irv /= SAG_OK )GOTO 9999
END IF

!==============================================================================
! Write Contours
!==============================================================================
IF( uwrite%lcontour )THEN

!==============================================================================
! Check for contour definitions
!==============================================================================
  IF( uwrite%nlev == 0 .OR. .NOT.ASSOCIATED(uwrite%iplev) )THEN
    LastError = SAG_ERR_NOLEV
    GOTO 9999
  END IF

  IF( uwrite%nheader > 0 )THEN
    IF( .NOT.ASSOCIATED(uwrite%iphdr) )THEN
      LastError = SAG_ERR_NOLEV
      GOTO 9999
    ELSE
      header => uwrite%iphdr
    END IF
  END If

  contour%mxpts = 0
  contour%mxlns = 0
  NULLIFY(contour%ippts)
  NULLIFY(contour%iplns)

  DO ic = uwrite%start,uwrite%stop

    IF( uwrite%nheader > 0 )THEN
      DO ih = 1,uwrite%nheader
        WRITE(uwrite%lun,'(A)',IOSTAT=ios)TRIM(header((ic-1)*uwrite%nheader+ih))
      END DO
    END IF

    CALL SAG_InitContourWrite( contour,uwrite,1,ic )

    irv = SAG_BuildContour( triT,contour )
    IF( irv /= SAG_OK )GOTO 9998

    irv = SAG_UserContour( contour,UserWrite,grd )
    IF( irv /= SAG_OK )GOTO 9998

  END DO

!==============================================================================
! Write Nodes
!==============================================================================
ELSE

  irv = SAG_UserField( triT,UserWrite,grd )
  IF(irv /= SAG_OK)GOTO 9998

END IF

CALL FLUSH(uwrite%lun)

!==============================================================================
! Set return value
!==============================================================================
SAG_WriteField = SAG_OK

!==============================================================================
! Clean up - Free contour space
!==============================================================================
9998 CONTINUE
IF( uwrite%lcontour )CALL SAG_FreeContour( contour )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!                SAG_UserField
!*******************************************************************************
INTEGER FUNCTION SAG_UserField( triT,UserFunction,grd )

USE sagdef_fd
USE sagtri_fd
USE sagerr_fd
USE sagerr_fi
USE sagstr_fd

!     Trace out a contour

IMPLICIT NONE

TYPE( SAGtriangleT_str ), POINTER :: triT
TYPE ( SAGgrid_str ),     POINTER :: grd
INTERFACE
  INTEGER FUNCTION UserFunction( ntri,tri,nnode,node,mxdata,dat )
    USE sagtri_fd
    INTEGER,                           INTENT( IN ) :: ntri
    INTEGER,                           INTENT( IN ) :: nnode
    INTEGER,                           INTENT( IN ) :: mxdata
    TYPE ( SAGtriangle_str ), DIMENSION(:), POINTER :: tri
    TYPE ( SAGnode_str ),     DIMENSION(:), POINTER :: node
    REAL,                     DIMENSION(:), POINTER :: dat
  END FUNCTION UserFunction
END INTERFACE

INTEGER  irv, i, ios

TYPE( SAGnode_str ),     DIMENSION(:), POINTER :: node
REAL,                    DIMENSION(:), POINTER :: datt
TYPE( SAGtriangle_str ), DIMENSION(:), POINTER :: tri

SAG_UserField = SAG_ERROR

NULLIFY(node)

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

tri  => triT%iptri
datt => triT%nodeT%ipdata

!==============
!Convert node locations to real coordinates if necessary
!==============
ALLOCATE(node(triT%nodeT%nnode),STAT=ios)
IF( ios /= 0 )THEN
  LastError = SAG_ERR_MALLOC
  LastError = triT%nodeT%nnode
  GOTO 9999
END IF
DO i = 1,triT%nodeT%nnode
  node(i) = triT%nodeT%ipnode(i)
  node(i)%x = grd%xmin + grd%dx*node(i)%x
  node(i)%y = grd%ymin + grd%dy*node(i)%y
  node(i)%hx = grd%dx*node(i)%hx
  node(i)%hy = grd%dy*node(i)%hy
END DO

irv = UserFunction( triT%ntri,tri,triT%nodeT%nnode,node,triT%nodeT%mxdata,datt )
IF( irv /= SAG_OK )THEN
  LastError = SAG_ERR_USER
  GOTO 9999
END IF

SAG_UserField = SAG_OK

9999 CONTINUE

IF( ASSOCIATED(node) )DEALLOCATE(node,STAT=ios)

RETURN
END
