!*******************************************************************************
!                OIL Definition module
!*******************************************************************************
MODULE oilfile_fi

  INTEGER, PARAMETER ::  MAX_OILCOLOR    =  11
  CHARACTER(12), DIMENSION(MAX_OILCOLOR), PARAMETER :: OIL_COLOR = (/&
      'CREAM       ', &
      'RED         ', &
      'ORANGE      ', &
      'YELLOW      ', &
      'GREEN       ', &
      'MINT_GREEN  ', &
      'CYAN        ', &
      'LT_BLUE     ', &
      'PURPLE      ', &
      'MAGENTA     ', &
      'GRAY        ' /)

END MODULE oilfile_fi
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildHeaderOIL
!*******************************************************************************
INTEGER FUNCTION BuildHeaderOIL( grdI,Field,nComment,Comment,Max,string )

USE charT_fd
USE field_fd

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
INTEGER       MaxS
INTEGER       iss
CHARACTER( 1) header
CHARACTER(24) filetype

!==============================================================================
! Finction calls
!==============================================================================
INTEGER, EXTERNAL :: BuildHeaderStandard

!==============================================================================
! Initialize
!==============================================================================
BuildHeaderOIL = 0

header   = 'c'
filetype = 'OILSTOCK Overlay'

!==============================================================================
! Standard Header
!==============================================================================
MaxS = Max - BuildHeaderOIL - 2
iss  = BuildHeaderOIL + 1
BuildHeaderOIL = BuildHeaderOIL + &
                 BuildHeaderStandard( header,filetype,grdI,Field, &
                                      nComment,Comment,MaxS,string(iss) )

!==============================================================================
! OILSTOCK input
!==============================================================================
IF( BuildHeaderOIL < Max )THEN
  string(BuildHeaderOIL+1) = 'll 0 1'
  BuildHeaderOIL = BuildHeaderOIL + 1
END IF
IF( BuildHeaderOIL < Max )THEN
  string(BuildHeaderOIL+1) = 'lf 0 0'
  BuildHeaderOIL = BuildHeaderOIL + 1
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                ContourHeaderOIL
!*******************************************************************************
INTEGER FUNCTION ContourHeaderOIL( indx,contour,max,header )

USE sagdef_fd
USE contourlist_fd
USE oilfile_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                        INTENT( IN  )     :: indx     !contour index
TYPE( SCIPContourElementLIST ), INTENT( IN  )     :: contour  !contour
INTEGER,                        INTENT( IN  )     :: Max      !Maximum number of strings
CHARACTER(*),DIMENSION(Max),    INTENT( OUT )     :: header   !header strings

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios
INTEGER icolor
CHARACTER(24) tmpstr

!==============================================================================
! Initialize
!==============================================================================
icolor = contour%listHdr%number - indx + 1
DO WHILE( icolor > MAX_OILCOLOR )
  icolor = icolor - MAX_OILCOLOR
END DO

!==============================================================================
! Header String
!==============================================================================
header(1) = 'c CONTOUR'
WRITE(tmpstr,'(1PE12.5)',IOSTAT=ios)contour%listPtr(indx)%contour
IF( ios /= 0 )THEN
  tmpstr = '**ERROR**'
END IF
header(1) = TRIM(header(1))//' '//TRIM(tmpstr)

header(2) = 'h '//TRIM(OIL_COLOR(icolor))

!==============================================================================
! Set Return value
!==============================================================================
ContourHeaderOIL = SAG_OK

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyOIL
!*******************************************************************************
INTEGER FUNCTION WriteBodyOIL( npoly,xpoly,ypoly,nlev )

USE sagdef_fd
USE sagwrt_usr
USE write_fi
USE charT_fd
USE field_fd
USE error_fi

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                INTENT( IN ) :: npoly  !number of points
REAL, DIMENSION(npoly), INTENT( IN ) :: xpoly  !x coordinate arrray
REAL, DIMENSION(npoly), INTENT( IN ) :: ypoly  !y coordinate arrray
INTEGER,                INTENT( IN ) :: nlev   !Contour index

!==============================================================================
! Local variables
!==============================================================================
INTEGER lun
INTEGER ios
INTEGER i
TYPE( SCIPFieldCoordinateT ) :: TransStruct
INTEGER outMode
REAL    swap
REAL,DIMENSION(:),ALLOCATABLE :: xp
REAL,DIMENSION(:),ALLOCATABLE :: yp
LOGICAL neg
INTEGER deg
INTEGER min
INTEGER sec
INTEGER hsec
INTEGER nch
CHARACTER(128) line
CHARACTER(2)   header
CHARACTER(6), PARAMETER :: tail = ' 0 1.0'

!==============================================================================
! Formats
!==============================================================================
100 FORMAT(3I2.2)
200 FORMAT(I3.3,2I2.2)

!==============================================================================
! Initialize
!==============================================================================
WriteBodyOIL = SAG_ERROR

lun = uwrite%lun

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
IF( outMode /= I_LATLON )GOTO 9999

DO i = 1,npoly
  swap = xp(i)
  xp(i) = yp(i)
  yp(i) = swap
END DO

!==============================================================================
! Write Line
!==============================================================================
DO i = 1,npoly

  nch = 1

  CALL DegMinSec( xp(i),neg,deg,min,sec,hsec )

  WRITE(line(nch:),100)deg,min,sec
  nch = nch + 6
  line(nch:nch) = '.'
  nch = nch + 1
  WRITE(line(nch:),100)hsec
  nch = nch + 2
  IF(neg)THEN
    line(nch:nch) = 'S '
  ELSE
    line(nch:nch) = 'N '
  END IF
  nch = nch + 2

  CALL DegMinSec( yp(i),neg,deg,min,sec,hsec )

  WRITE(line(nch:),200)deg,min,sec
  nch = nch + 7
  line(nch:nch) = '.'
  nch = nch + 1
  WRITE(line(nch:),100)hsec
  nch = nch + 2
  IF(neg)THEN
    line(nch:nch) = 'W'
  ELSE
    line(nch:nch) = 'E'
  END IF
  nch = nch + 1

  IF( i == 1 )THEN
    IF( uwrite%lclose )THEN
      header = 'f '
    ELSE
      header = 's '
    END IF
  ELSE
    header = 'd '
  END IF

  WRITE(lun,'(A)',IOSTAT=ios)header//TRIM(line)//tail
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyOIL = SAG_OK

9999 CONTINUE
!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(xp) )DEALLOCATE( yp,STAT=ios )

RETURN

END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                DegMinSec
!*******************************************************************************
SUBROUTINE DegMinSec( x,Neg,Deg,Min,Sec,Hsec )

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
REAL,    INTENT( IN  )  :: x    !Decimal degrees
LOGICAL, INTENT( OUT )  :: Neg  !Positive/Negative flag
INTEGER, INTENT( OUT )  :: Deg  !Integer Degrees
INTEGER, INTENT( OUT )  :: Min  !Integer Minutes
INTEGER, INTENT( OUT )  :: Sec  !Integer Seconds
INTEGER, INTENT( OUT )  :: Hsec !Integer Hundredths of a second

!==============================================================================
! Local variables
!==============================================================================
REAL val

!==============================================================================
! Set Negative/Positive flag
!==============================================================================
Neg = x < 0

!==============================================================================
! Integer Degrees
!==============================================================================
val = ABS(x)
Deg = INT(val)

!==============================================================================
! Integer Minutes
!==============================================================================
Min = INT((val - FLOAT(Deg))*60.)

!==============================================================================
! Integer Seconds
!==============================================================================
Sec = INT(((val - FLOAT(Deg))*60. - FLOAT(Min))*60.)

!==============================================================================
! Hundredths of Seconds
!==============================================================================
Hsec = NINT((((val - FLOAT(Deg))*60. - FLOAT(Min))*60. - FLOAT(Sec))*100.)

IF( Hsec >= 100 )THEN

  Hsec = Hsec - 100
  Sec = Sec + 1

  IF( Sec >= 60 )THEN

    Sec = Sec - 60
    Min = Min + 1

    IF( Min >= 60 )THEN
      Min = Min - 60
      Deg = Deg + 1
    END IF

  END IF

END IF

RETURN
END
