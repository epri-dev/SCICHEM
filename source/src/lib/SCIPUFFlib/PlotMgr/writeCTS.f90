!*******************************************************************************
!                CTS Definition module
!*******************************************************************************
MODULE CTSfile_fd

  INTEGER, PARAMETER ::  CTS_CLOSE       =  8900
  INTEGER, PARAMETER ::  CTS_OPEN        =  8000
  INTEGER, PARAMETER ::  MAX_CTSCOLOR    =  6
  CHARACTER(1), DIMENSION(MAX_CTSCOLOR), PARAMETER :: CTS_COLOR = (/&
      'M', &
      'R', &
      'G', &
      'B', &
      'C', &
      'Y' /)

END MODULE CTSfile_fd
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                BuildFooterCTS
!*******************************************************************************
INTEGER FUNCTION BuildFooterCTS( Max,string )

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                        INTENT( IN  )     :: Max      !Maximum number of strings
CHARACTER(*),DIMENSION(Max),    INTENT( OUT )     :: string   !Footer strings

INTEGER,PARAMETER :: BACK_SLASH = 92

!==============================================================================
! Local variables
!==============================================================================
INTEGER ios

!==============================================================================
! Initialize
!==============================================================================
BuildFooterCTS = 0

!==============================================================================
! Footer String
!==============================================================================
IF( BuildFooterCTS < Max )THEN
  WRITE(string(BuildFooterCTS+1),'(a)',IOSTAT=ios)CHAR(BACK_SLASH)
  IF( ios /= 0 )GOTO 9999
  BuildFooterCTS = BuildFooterCTS + 1
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyCTS
!*******************************************************************************
INTEGER FUNCTION WriteBodyCTS( npoly,xpoly,ypoly,nlev )

USE sagdef_fd
USE sagwrt_usr
USE write_fi
USE error_fi
USE CTSfile_fd

IMPLICIT NONE

!==============================================================================
! Function Arguments
!==============================================================================
INTEGER,                INTENT( IN  ) :: npoly  !number of points
REAL, DIMENSION(npoly), INTENT( IN  ) :: xpoly  !x coordinate arrray
REAL, DIMENSION(npoly), INTENT( IN  ) :: ypoly  !y coordinate arrray
INTEGER,                INTENT( IN  ) :: nlev   !Contour index

INTEGER,PARAMETER :: BACK_SLASH = 92

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
INTEGER jcol
INTEGER ityp
INTEGER jtyp

!==============================================================================
! Formats
!==============================================================================
100 FORMAT(3I2.2)
200 FORMAT(I3.3,2I2.2)
102 FORMAT(/,I6.6,' ',A1,' S TYPE=',I1)

!==============================================================================
! Initialize
!==============================================================================
WriteBodyCTS = SAG_ERROR

lun = uwrite%lun

!==============================================================================
! Write Line header
!==============================================================================
jcol = MOD(nlev,MAX_CTSCOLOR) + 1
jtyp = (nlev-1)/MAX_CTSCOLOR + 1
IF( uwrite%lclose )THEN
  ityp = CTS_CLOSE
ELSE
  ityp = CTS_OPEN
END IF
WRITE(lun,102)ityp,CTS_COLOR(jcol),jtyp

!==============================================================================
! Allocate work space
!==============================================================================
ALLOCATE( xp(npoly),STAT=ios )
IF( ios /= 0 )GOTO 9999

ALLOCATE( yp(npoly),STAT=ios )
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
  CALL Transform(Coordinate,TransStruct,npoly,xp,yp)
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

  IF( neg )THEN
    line(nch:nch) = '-'
    nch = nch + 1
  END IF

  WRITE(line(nch:),100)deg,min,sec
  nch = nch + 6
  line(nch:nch) = '.'
  nch = nch + 1
  WRITE(line(nch:),100)hsec
  nch = nch + 2

  line(nch:nch) = 'N '
  nch = nch + 2

  CALL DegMinSec( yp(i),neg,deg,min,sec,hsec )

  IF( neg )THEN
    line(nch:nch) = '-'
    nch = nch + 1
  END IF

  WRITE(line(nch:),200)deg,min,sec
  nch = nch + 7
  line(nch:nch) = '.'
  nch = nch + 1
  WRITE(line(nch:),100)hsec
  nch = nch + 2

  line(nch:nch) = 'E'
  nch = nch + 1

  IF( i == npoly )THEN
    line(nch:nch) = CHAR(BACK_SLASH)
  END IF

  WRITE(lun,'(A)',IOSTAT=ios)TRIM(line)
  IF( ios /= 0 )GOTO 9999
END DO

!==============================================================================
! Set return value
!==============================================================================
WriteBodyCTS = SAG_OK

9999 CONTINUE
!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(xp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
