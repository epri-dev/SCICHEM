!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildHeaderStandard
!*******************************************************************************
INTEGER FUNCTION BuildHeaderStandard( header,filetype,grdI,Field, &
                                      nComment,Comment,Max,string )
USE project_fi
USE sagstr_fd
USE field_fd
USE PtrGrdStrItf
USE charT_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
CHARACTER(*),                        INTENT( IN  ) :: header   !Comment indicator
CHARACTER(*),                        INTENT( IN  ) :: filetype !File type string
INTEGER,                             INTENT( IN )  :: grdI     !SAG grid ID
TYPE( SCIPPlotFieldT ),              INTENT( IN )  :: Field    !Field definition
INTEGER,                             INTENT( IN )  :: nComment !User supplied comments
TYPE( char128T ),DIMENSION(nComment),INTENT( IN )  :: Comment  !User supplied comments
INTEGER,                             INTENT( IN  ) :: Max      !Maximum number of strings
CHARACTER(*),DIMENSION(Max),         INTENT( OUT ) :: string   !Footer strings

!==============================================================================
! Local variables
!==============================================================================
INTEGER       ios
INTEGER       i
LOGICAL       lexist
CHARACTER(32)             version
CHARACTER(PATH_MAXLENGTH) filestr
TYPE ( char128T    ) charStruct
TYPE ( SAGgrid_str ), POINTER :: grd

!==============================================================================
! Function calls
!==============================================================================
CHARACTER(24),             EXTERNAL :: sysGetDate
INTEGER,                   EXTERNAL :: GetVersionString
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==============================================================================
! Initialize
!==============================================================================
BuildHeaderStandard = 0

!==============================================================================
! Standard Header
!==============================================================================
! File Information
!==============================================================================
IF( BuildHeaderStandard < Max )THEN
  string(BuildHeaderStandard+1) = TRIM(header)//' File    Type     : '//TRIM(filetype)
  BuildHeaderStandard = BuildHeaderStandard + 1
END IF

IF( BuildHeaderStandard < Max )THEN
  string(BuildHeaderStandard+1) = TRIM(header)//' File    Creation : '//TRIM(sysGetDate())
  BuildHeaderStandard = BuildHeaderStandard + 1
END IF

!==============================================================================
! Creator Information
!==============================================================================
IF( BuildHeaderStandard < Max )THEN
  ios = GetVersionString( 2,charStruct )
  version = TRIM(charStruct%string)
  string(BuildHeaderStandard+1) = TRIM(header)//' Creator          : SCIP Tool'
  BuildHeaderStandard = BuildHeaderStandard + 1
END IF

IF( BuildHeaderStandard < Max )THEN
  ios = GetVersionString( 2,charStruct )
  version = TRIM(charStruct%string)
  string(BuildHeaderStandard+1) = TRIM(header)//' Creator Version  : '//TRIM(version)
  BuildHeaderStandard = BuildHeaderStandard + 1
END IF

!==============================================================================
! Project Information (if associated with a project)
!==============================================================================
IF( LEN_TRIM(Field%project) > 0 )THEN
  filestr = TRIM(Field%project)
  CALL AddPath( filestr,TRIM(Field%path) )
  filestr = TRIM(AddExtension( filestr,'inp' ))
  INQUIRE( FILE=filestr,EXIST=lexist )
  IF( lexist )THEN
    CALL SetFileNamesT( filestr )
    CALL SetFileUnitsT()
    CALL set_version( iversion_code )
    CALL read_prj()
    CALL init_error()          !Clear version error since ok for plotting
    IF( BuildHeaderStandard < Max )THEN
      string(BuildHeaderStandard+1) = TRIM(header)//' Project Name     : '//TRIM(Field%project)
      BuildHeaderStandard = BuildHeaderStandard + 1
    END IF
    IF( BuildHeaderStandard < Max )THEN
      string(BuildHeaderStandard+1) = TRIM(header)//' Project Path     : '//TRIM(Field%path)
      BuildHeaderStandard = BuildHeaderStandard + 1
    END IF
    IF( BuildHeaderStandard < Max )THEN
      string(BuildHeaderStandard+1) = TRIM(header)//' Project Version  : '//TRIM(audit_version)
      BuildHeaderStandard = BuildHeaderStandard + 1
    END IF
    IF( BuildHeaderStandard < Max )THEN
      string(BuildHeaderStandard+1) = TRIM(header)//' Project Creation : '//TRIM(audit_date)
      BuildHeaderStandard = BuildHeaderStandard + 1
    END IF
    CALL deallocate_read_prj()
  END IF

!==============================================================================
! Grid information (if only association through grid file)
!==============================================================================
ELSE
  grd => SAG_PtrGrdStr( grdI )
  IF( .NOT.ASSOCIATED(grd) )GOTO 9999

  IF( LEN_TRIM(grd%file) > 0 )THEN
    INQUIRE( FILE=grd%file,EXIST=lexist )
    IF( lexist )THEN
      filestr = grd%file
      IF( BuildHeaderStandard < Max )THEN
        string(BuildHeaderStandard+1) = TRIM(header)//' Field   File     : '//TRIM(grd%file)
        BuildHeaderStandard = BuildHeaderStandard + 1
      END IF
    END IF
  END IF
END IF

!==============================================================================
! User Supplied comments
!==============================================================================
DO i = 1,nComment
  IF( BuildHeaderStandard < Max )THEN
    string(BuildHeaderStandard+1) = TRIM(header)//' '//TRIM(Comment(i)%string)
    BuildHeaderStandard = BuildHeaderStandard + 1
  ELSE
    EXIT
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildHeaderOVL
!*******************************************************************************
INTEGER FUNCTION BuildHeaderOVL( grdI,Field,nComment,Comment,Max,string )

USE charT_fd
USE sagdef_fd
USE write_fi

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

INTEGER,PARAMETER :: CURRENT_FILE_VERSION = 1000

INTEGER              ios
INTEGER              MaxS
INTEGER              iss
INTEGER              iNeed
REAL,DIMENSION(2) :: fldmx
CHARACTER( 1) header
CHARACTER( 1) command
CHARACTER(24) filetype
CHARACTER(80) tmpstring
CHARACTER(24) numstring

!==============================================================================
! Function calls
!==============================================================================
INTEGER, EXTERNAL :: BuildHeaderStandard
INTEGER, EXTERNAL :: SAG_BottomMinMaxID

!==============================================================================
! Initialize
!==============================================================================
BuildHeaderOVL = 0

command  = '!'
header   = '#'
filetype = 'SCIP Overlay'

iNeed = 7
IF( Field%coordinate%mode < 0 )iNeed = iNeed + 1

!==============================================================================
! Standard Header
!==============================================================================
MaxS = Max - BuildHeaderOVL - iNeed
iss  = BuildHeaderOVL + 1
BuildHeaderOVL = BuildHeaderOVL + &
                 BuildHeaderStandard( header,filetype,grdI,Field, &
                                      nComment,Comment,MaxS,string(iss) )

!==============================================================================
! SCIP Overlay Header
!==============================================================================
IF( BuildHeaderOVL < Max )THEN
  string(BuildHeaderOVL+1) = TRIM(header)//' Field   Units    : '//TRIM(Field%units)
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( BuildHeaderOVL < Max )THEN
  ios = SAG_BottomMinMaxID(grdI,3,.TRUE.,fldmx)
  IF( ios == SAG_OK )THEN
    WRITE(tmpstring,'(1PE12.5)',IOSTAT=ios)fldmx(2)
    IF( ios == 0 )THEN
      string(BuildHeaderOVL+1) = TRIM(header)//' Field   MaxValue : '//TRIM(ADJUSTL(tmpstring))
      BuildHeaderOVL = BuildHeaderOVL + 1
    END IF
  END IF
END IF

IF( Field%coordinate%mode >= 0 )THEN
  IF( BuildHeaderOVL < Max )THEN
    WRITE(numstring,*,IOSTAT=ios)Field%coordinate%horzSlice%height
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(ADJUSTL(numstring))//' ('
    IF( Field%coordinate%horzSlice%mode == 0 )THEN
      tmpstring = TRIM(tmpstring)//' AGL )'
    ELSE
      tmpstring = TRIM(tmpstring)//' MSL )'
    END IF
    string(BuildHeaderOVL+1) = TRIM(header)//' Field     Height : '//TRIM(tmpstring)
    BuildHeaderOVL = BuildHeaderOVL + 1
  END IF
END IF


IF( BuildHeaderOVL < Max )THEN
  IF( WriteMode == I_LATLON )THEN
    tmpstring = 'LATLON'
  ELSE
    SELECT CASE( ABS(Field%coordinate%mode) )
      CASE( HD_UTM )
        WRITE(tmpstring,'(A,I3)',IOSTAT=ios)'UTM : ZONE=',Field%coordinate%UTMZone
        IF( ios /= 0 )THEN
          tmpstring = 'UTM : ZONE= **ERROR**'
        END IF
      CASE( HD_CARTESIAN )
        tmpstring = 'CARTESIAN : '
        WRITE(numstring,*,IOSTAT=ios)Field%coordinate%reference%x
        IF( ios /= 0 )THEN
          numstring = '**ERROR**'
        END IF
        tmpstring = TRIM(tmpstring)//' ('//TRIM(ADJUSTL(numstring))//','
        WRITE(numstring,*,IOSTAT=ios)Field%coordinate%reference%y
        IF( ios /= 0 )THEN
          numstring = '**ERROR**'
        END IF
        tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//') => ( '
        WRITE(numstring,*,IOSTAT=ios)Field%coordinate%reference%lat
        IF( ios /= 0 )THEN
          numstring = '**ERROR**'
        END IF
        tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//'N,'
        WRITE(numstring,*,IOSTAT=ios)Field%coordinate%reference%lon
        IF( ios /= 0 )THEN
          numstring = '**ERROR**'
        END IF
        tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//'E)'
      CASE( HD_LATLON )
        tmpstring = 'LATLON'
      CASE DEFAULT
    END SELECT
  END IF
  string(BuildHeaderOVL+1) = TRIM(command)//'COORD  '//TRIM(tmpstring)
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( Field%coordinate%mode < 0 )THEN
  IF( BuildHeaderOVL < Max )THEN
    WRITE(numstring,'(I3)',IOSTAT=ios)Field%coordinate%vertSlice%resolution
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(numstring)//' ('
    WRITE(numstring,*,IOSTAT=ios)Field%coordinate%vertSlice%startPt%x
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//','
    WRITE(numstring,*,IOSTAT=ios)Field%coordinate%vertSlice%startPt%y
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//') ('
    WRITE(numstring,*,IOSTAT=ios)Field%coordinate%vertSlice%endPt%x
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//','
    WRITE(numstring,*,IOSTAT=ios)Field%coordinate%vertSlice%endPt%y
    IF( ios /= 0 )THEN
      numstring = '** ERROR **'
    END IF
    tmpstring = TRIM(tmpstring)//TRIM(ADJUSTL(numstring))//')'
    string(BuildHeaderOVL+1) = TRIM(command)//'RESOL  '//TRIM(tmpstring)
    BuildHeaderOVL = BuildHeaderOVL + 1
  END IF
END IF
IF( BuildHeaderOVL < Max )THEN
  string(BuildHeaderOVL+1) = TRIM(command)//'SIZE   1.0'
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( BuildHeaderOVL < Max )THEN
  string(BuildHeaderOVL+1) = TRIM(command)//'COLOR  1'
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( BuildHeaderOVL < Max )THEN
  string(BuildHeaderOVL+1) = TRIM(command)//'FILL   1'
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( BuildHeaderOVL < Max )THEN
  string(BuildHeaderOVL+1) = TRIM(command)//'SYMBOL 1'
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF
IF( BuildHeaderOVL < Max )THEN
  WRITE(numstring,*,IOSTAT=ios)CURRENT_FILE_VERSION
  IF( ios /= 0 )THEN
    numstring = '**ERROR**'
  END IF
  string(BuildHeaderOVL+1) = TRIM(command)//'FVERS '//TRIM(ADJUSTL(numstring))
  BuildHeaderOVL = BuildHeaderOVL + 1
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                ContourHeaderOVL
!*******************************************************************************
INTEGER FUNCTION ContourHeaderOVL( indx,contour,max,header )

USE field_fd
USE sagdef_fd
USE contourlist_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                        INTENT( IN  ) :: indx     !contour index
TYPE( SCIPContourElementLIST ), INTENT( IN  ) :: contour  !contour
INTEGER,                        INTENT( IN  ) :: Max      !Maximum number of strings
CHARACTER(*),DIMENSION(Max),    INTENT( OUT ) :: header   !header strings

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios
CHARACTER(24) tmpstr

!==============================================================================
! Initialize
!==============================================================================
!==============================================================================
! Header String
!==============================================================================
header(1) = '#CONTOUR'
WRITE(tmpstr,*,IOSTAT=ios)contour%listPtr(indx)%contour
IF( ios /= 0 )THEN
  tmpstr = '**ERROR**'
END IF

IF( contour%listHdr%labelMode == PLOT_ON )THEN
  header(1) = TRIM(header(1))//' '//TRIM(contour%listPtr(indx)%label)//' ('//TRIM(ADJUSTL(tmpstr))//')'
ELSE
  header(1) = TRIM(header(1))//' '//TRIM(ADJUSTL(tmpstr))
END IF

IF( contour%listHdr%scale /= 1.0 )THEN
  WRITE(tmpstr,*,IOSTAT=ios)contour%listHdr%scale
  IF( ios /= 0 )THEN
    tmpstr = '**ERROR**'
  END IF
  header(1) = TRIM(header(1))//' : SCALE='//TRIM(ADJUSTL(tmpstr))
END IF

IF( LEN_TRIM(contour%listHdr%unit) > 0 .and. TRIM(contour%listHdr%unit) /= 'default' )THEN
  header(1) = TRIM(header(1))//' : UNIT='//TRIM(contour%listHdr%unit)
END IF

!==============================================================================
! Set Return value
!==============================================================================
ContourHeaderOVL = SAG_OK

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyOVL
!*******************************************************************************
INTEGER FUNCTION WriteBodyOVL( npoly,xpoly,ypoly,nlev )

USE sagdef_fd
USE sagwrt_usr
USE write_fi
USE error_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                INTENT( IN  ) :: npoly  !number of points
REAL, DIMENSION(npoly), INTENT( IN  ) :: xpoly  !x coordinate arrray
REAL, DIMENSION(npoly), INTENT( IN  ) :: ypoly  !y coordinate arrray
INTEGER,                INTENT( IN  ) :: nlev   !Contour index

!==============================================================================
! Local variables
!==============================================================================
INTEGER lun
INTEGER ios
INTEGER i
TYPE( SCIPFieldCoordinateT ) TransStruct
INTEGER outMode
REAL    swap
REAL, DIMENSION(:), ALLOCATABLE :: xp         !X work array
REAL, DIMENSION(:), ALLOCATABLE :: yp         !Y work array
!==============================================================================
! Initialize
!==============================================================================
WriteBodyOVL = SAG_ERROR

lun = uwrite%lun

!==============================================================================
! Write Line header
!==============================================================================
WRITE(lun,'(A)',IOSTAT=ios)'!LINE'
IF( ios /= 0 )GOTO 9999

!==============================================================================
! Allocate work space
!==============================================================================
ALLOCATE( xp(npoly),yp(npoly),STAT=ios )
IF( ios /= 0 )GOTO 9999

DO i = 1,npoly
  xp(i) = xpoly(i)
  yp(i) = ypoly(i)
END DO

!==============================================================================
! Transform data
!==============================================================================
IF( WriteMode == I_LATLON )THEN
  TransStruct = Coordinate
  TransStruct%mode = I_LATLON
  CALL Transform( Coordinate,TransStruct,npoly,xp,yp )
  IF( nError /= NO_ERROR )GOTO 9999
  outMode = TransStruct%mode
ELSE
  outMode = Coordinate%mode
END IF

!==============================================================================
! Swap Lat/Lon
!==============================================================================
IF( outMode == I_LATLON )THEN
  DO i = 1,npoly
    swap = xp(i)
    xp(i) = yp(i)
    yp(i) = swap
  END DO
END IF

!==============================================================================
! Write Line
!==============================================================================
DO i = 1,npoly
  WRITE(lun,'(1P2E15.7)',IOSTAT=ios)xp(i),yp(i)
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyOVL = SAG_OK

9999 CONTINUE
!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(xp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
