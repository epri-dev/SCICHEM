!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildHeaderAVS
!*******************************************************************************
INTEGER FUNCTION BuildHeaderAVS( grdI, Field, nComment, Comment, Max, string )

USE charT_fd
USE field_fd
USE sagdef_fd
USE sagstr_fd
USE PtrGrdStrItf

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                             INTENT( IN )  :: grdI     !SAG grid ID
TYPE( SCIPPlotFieldT ),              INTENT( IN )  :: Field    !Field definition
INTEGER,                             INTENT( IN )  :: nComment !User supplied comments
TYPE( char128T ),DIMENSION(nComment),INTENT( IN )  :: Comment  !User supplied comments
INTEGER,                             INTENT( IN  ) :: Max      !Maximum number of strings
CHARACTER(*),DIMENSION(Max),         INTENT( OUT ) :: string   !Footer strings

!==============================================================================
! Local variables
!==============================================================================
INTEGER              ios
INTEGER              MaxS
INTEGER              iss
REAL,DIMENSION(2) :: fldmx
CHARACTER( 1) header
CHARACTER(24) filetype
CHARACTER(48) tmpstring
TYPE ( SAGgrid_str )     , POINTER :: grd

!==============================================================================
! Finction calls
!==============================================================================
INTEGER, EXTERNAL :: BuildHeaderStandard
INTEGER, EXTERNAL :: SAG_BottomMinMaxID
INTEGER, EXTERNAL :: SAG_BottomLevelID

!==============================================================================
! Initialize
!==============================================================================
BuildHeaderAVS = 0

header   = '#'
filetype = 'AVS ASCII UCD'

!==============================================================================
! Standard Header
!==============================================================================
MaxS = Max - BuildHeaderAVS
iss  = BuildHeaderAVS + 1
BuildHeaderAVS = BuildHeaderAVS + &
                 BuildHeaderStandard( header,filetype,grdI,Field, &
                                      nComment,Comment,MaxS,string(iss) )

!==============================================================================
! AVS Header
!==============================================================================
IF( BuildHeaderAVS < Max )THEN
  string(BuildHeaderAVS+1) = TRIM(header)//' Field   Units    : '//TRIM(Field%units)
  BuildHeaderAVS = BuildHeaderAVS + 1
END IF
IF( BuildHeaderAVS < Max )THEN
  ios = SAG_BottomMinMaxID( grdI,3,.TRUE.,fldmx )
  IF( ios == SAG_OK )THEN
    WRITE(tmpstring,'(1PE12.5)',IOSTAT=ios)fldmx(2)
    IF( ios == 0 )THEN
      string(BuildHeaderAVS+1) = TRIM(header)//' Field   MaxValue : '//TRIM(ADJUSTL(tmpstring))
      BuildHeaderAVS = BuildHeaderAVS + 1
    END IF
  END IF
END IF
IF( BuildHeaderAVS < Max )THEN
  SELECT CASE( ABS(Field%coordinate%mode) )
    CASE( HD_UTM )
      WRITE(tmpstring,'(a,i3)',IOSTAT=ios)'Modified UTM (km) : zone=',Field%coordinate%UTMZone
      IF( ios /= 0 )THEN
        tmpstring = 'Modified UTM (km) : zone= **ERROR**'
      END IF
    CASE( HD_CARTESIAN )
      tmpstring = 'Cartesian (km)'
    CASE( HD_LATLON )
      tmpstring = 'Longitude/Latitude (degrees North/East)'
    CASE DEFAULT
  END SELECT
  string(BuildHeaderAVS+1) = TRIM(header)//' Field   CoordType: '//TRIM(ADJUSTL(tmpstring))
  BuildHeaderAVS = BuildHeaderAVS + 1
END IF
IF( BuildHeaderAVS < Max )THEN
  string(BuildHeaderAVS+1) = TRIM(header)//' Field   Domain   : '
  grd => SAG_PtrGrdStr( grdI )
  IF( ASSOCIATED(grd) )THEN
    WRITE(tmpstring,*)grd%xmin
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' ('//TRIM(ADJUSTL(tmpstring))
    WRITE(tmpstring,*)grd%ymin
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//','//TRIM(ADJUSTL(tmpstring))
    WRITE(tmpstring,*)grd%xmin + FLOAT(grd%nx)*grd%dx
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//') ('//TRIM(ADJUSTL(tmpstring))
    WRITE(tmpstring,*)grd%ymin + FLOAT(grd%ny)*grd%dy
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//','//TRIM(ADJUSTL(tmpstring))//')'
  ELSE
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' **ERROR**'
  END IF
  BuildHeaderAVS = BuildHeaderAVS + 1
END IF
IF( BuildHeaderAVS < Max )THEN
  string(BuildHeaderAVS+1) = TRIM(header)//' Field   Grid     : Type=Adaptive : Base='
  grd => SAG_PtrGrdStr( grdI )
  IF( ASSOCIATED(grd) )THEN
    WRITE(tmpstring,*)grd%nx
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' '//TRIM(ADJUSTL(tmpstring))
    WRITE(tmpstring,*)grd%ny
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//'x'//TRIM(ADJUSTL(tmpstring))
    WRITE(tmpstring,*)grd%ncells
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' : Cells='//TRIM(ADJUSTL(tmpstring))
  ELSE
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' **ERROR**'
  END IF
  BuildHeaderAVS = BuildHeaderAVS + 1
END IF
IF( BuildHeaderAVS < Max )THEN
  string(BuildHeaderAVS+1) = TRIM(header)//' Field  Resolution:'
  IF( ASSOCIATED(grd) )THEN
    WRITE(tmpstring,*)SAG_BottomLevelID(grdI)
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' '//TRIM(ADJUSTL(tmpstring))
    IF( grd%maxlev >= 0 )THEN
      tmpstring = '(limited)'
      string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' '//TRIM(tmpstring)
    END IF
  ELSE
    string(BuildHeaderAVS+1) = TRIM(string(BuildHeaderAVS+1))//' **ERROR**'
  END IF
  BuildHeaderAVS = BuildHeaderAVS + 1
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyAVS
!*******************************************************************************
INTEGER FUNCTION WriteBodyAVS( ntri,tri,nnode,node,mxdata,dat )

USE sagdef_fd
USE sagwrt_usr
USE sagnod_fd
USE sagtri_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                           INTENT( IN ) :: ntri   !Number of triangles
INTEGER,                           INTENT( IN ) :: nnode  !Number of nodes
INTEGER,                           INTENT( IN ) :: mxdata !Max number of data fields
TYPE ( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri    !Trangle array
TYPE ( SAGnode_str ),     POINTER, DIMENSION(:) :: node   !Node array
REAL,                     POINTER, DIMENSION(:) :: dat    !data array

!==============================================================================
! Local variables
!==============================================================================
INTEGER lun
INTEGER ios
INTEGER i
INTEGER j
INTEGER ncn
INTEGER ncc
INTEGER ncm
REAL    z
CHARACTER(80), DIMENSION(:), POINTER :: header
CHARACTER(32) label

!==============================================================================
! Initialize
!==============================================================================
WriteBodyAVS = SAG_ERROR

lun = uwrite%lun

!==============================================================================
! Write header
!==============================================================================
ncn = 1
ncc = 0
ncm = 0
WRITE(lun,*,IOSTAT=ios)nnode,ntri,ncn,ncc,ncm
IF( ios /= 0 )GOTO 9999

!==============================================================================
! Write nodes
!==============================================================================
z = 0.
DO i = 1,nnode
  WRITE(lun,*,IOSTAT=ios)i,node(i)%x,node(i)%y,z
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Write triangles
!==============================================================================
DO i = 1,ntri
  WRITE(lun,*,IOSTAT=ios)i,'   1 tri',(tri(i)%nid(j),j=1,3)
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Write first data
!==============================================================================
IF( uwrite%nheader > 0 )THEN
  IF( .NOT.ASSOCIATED( uwrite%iphdr ) )THEN
    label = 'C,   **ERROR**'
  ELSE
    header => uwrite%iphdr
    label = TRIM(header(1))
  END IF
ELSE
  label = 'C'
END IF

WRITE(lun,'(A)')'   1   1'
WRITE(lun,'(A)')TRIM(label)
DO i = 1,nnode
  WRITE(lun,*,IOSTAT=ios)i,dat((i-1)*mxdata+1)
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyAVS = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyNone
!*******************************************************************************
INTEGER FUNCTION WriteBodyNone( ntri,tri,nnode,node,mxdata,dat )

USE sagdef_fd
USE sagnod_fd
USE sagtri_fd
USE write_noFile
USE abort

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                           INTENT( IN ) :: ntri   !Number of triangles
INTEGER,                           INTENT( IN ) :: nnode  !Number of nodes
INTEGER,                           INTENT( IN ) :: mxdata !Max number of data fields
TYPE ( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri    !Trangle array
TYPE ( SAGnode_str ),     POINTER, DIMENSION(:) :: node   !Node array
REAL,                     POINTER, DIMENSION(:) :: dat    !data array

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios
INTEGER i

!==============================================================================
! Initialize
!==============================================================================
WriteBodyNone = SAG_ERROR

!==============================================================================
! Allocate space
!==============================================================================
IF( ALLOCATED(nfNode) )DEALLOCATE( nfNode,STAT=ios )
IF( ALLOCATED(nfTri)  )DEALLOCATE( nfTri, STAT=ios )

ios = 0
IF( nnode > 0 )ALLOCATE( nfNode(nnode),STAT=ios )
IF( ios /= 0 )GOTO 9999

IF( ntri > 0 )ALLOCATE( nfTri(ntri), STAT=ios )
IF( ios /= 0 )GOTO 9999

numNodes = nnode
numTriangles = ntri

IF( Aborted() ) GOTO 9999

!==============================================================================
! Set nodes
!==============================================================================
DO i = 1,nnode
  nfNode(i)%id = i
  nfNode(i)%x  = node(i)%x
  nfNode(i)%y  = node(i)%y
  nfNode(i)%z  = 0.
  nfNode(i)%hx = node(i)%hx
  nfNode(i)%hy = node(i)%hy
  nfNode(i)%v  = dat((i-1)*mxdata+1)
END DO

IF( Aborted() ) GOTO 9999

!==============================================================================
! Set triangles
!==============================================================================
DO i = 1,ntri
  nfTri(i)%id = i
  nfTri(i)%nidA = tri(i)%nid(1)
  nfTri(i)%nidB = tri(i)%nid(2)
  nfTri(i)%nidC = tri(i)%nid(3)
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyNone = SAG_OK

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyCount
!*******************************************************************************
INTEGER FUNCTION WriteBodyCount( ntri,tri,nnode,node,mxdata,dat )

USE sagdef_fd
USE sagnod_fd
USE sagtri_fd
USE write_noFile

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                           INTENT( IN ) :: ntri   !Number of triangles
INTEGER,                           INTENT( IN ) :: nnode  !Number of nodes
INTEGER,                           INTENT( IN ) :: mxdata !Max number of data fields
TYPE ( SAGtriangle_str ), POINTER, DIMENSION(:) :: tri    !Trangle array
TYPE ( SAGnode_str ),     POINTER, DIMENSION(:) :: node   !Node array
REAL,                     POINTER, DIMENSION(:) :: dat    !data array

!==============================================================================
! Local variables
!==============================================================================

!==============================================================================
! Initialize
!==============================================================================
!==============================================================================
! Allocate space
!==============================================================================

numNodes = nnode
numTriangles = ntri

!==============================================================================
! Set return value
!==============================================================================
WriteBodyCount = SAG_OK

RETURN
END
