!*******************************************************************************
!                USA Definition module
!*******************************************************************************
MODULE USAfile_fi

  INTEGER, PARAMETER ::  MAX_USACOLOR    =  5
  INTEGER, DIMENSION(MAX_USACOLOR), PARAMETER :: USA_COLOR  = (/ 1, 4, 2, 0, 3/)
  INTEGER, DIMENSION(MAX_USACOLOR), PARAMETER :: USA_SIZE   = (/ 0, 0, 1, 1, 1/)
  INTEGER, DIMENSION(MAX_USACOLOR), PARAMETER :: USA_SYMBOL = (/12,46,16,51,50/)

END MODULE USAfile_fi
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                ContourHeaderUSA
!*******************************************************************************
INTEGER FUNCTION ContourHeaderUSA( indx,contour,max,header )

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
CALL c_format( contour%listPtr(indx)%contour,ios,tmpstr )

IF( contour%listHdr%labelMode == PLOT_ON )THEN
  header(1) = TRIM(contour%listPtr(indx)%label)
ELSE
  header(1) = TRIM(tmpstr)
END IF

!==============================================================================
! Set Return value
!==============================================================================
ContourHeaderUSA = SAG_OK

RETURN
END
!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!                WriteBodyUSA
!*******************************************************************************
INTEGER FUNCTION WriteBodyUSA( npoly,xpoly,ypoly,nlev )

USE sagdef_fd
USE sagwrt_usr
USE write_fi
USE USAfile_fi
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
TYPE( SCIPFieldCoordinateT ) :: TransStruct
INTEGER outMode
REAL    swap
REAL,DIMENSION(:),ALLOCATABLE :: xp
REAL,DIMENSION(:),ALLOCATABLE :: yp
INTEGER jlev
INTEGER icol
INTEGER isiz
INTEGER isymbol
INTEGER ilabel
CHARACTER(32) label
CHARACTER(80), DIMENSION(:), POINTER :: header

!==============================================================================
! Formats
!==============================================================================
200 FORMAT(F13.8,',',F13.8,',')
202 FORMAT(F13.8,',',F13.8,',,',3(',',I2))
203 FORMAT(F13.8,',',F13.8,',',A,',')

!==============================================================================
! Initialize
!==============================================================================
WriteBodyUSA = SAG_ERROR

lun = uwrite%lun

jlev    = uwrite%nlev - nlev
icol    = USA_COLOR (MOD(jlev,MAX_USACOLOR) + 1)
isiz    = USA_SIZE  (MIN(jlev+1,MAX_USACOLOR))
isymbol = USA_SYMBOL(MOD(jlev/MAX_USACOLOR,MAX_USACOLOR) + 1)

IF( uwrite%nheader < 0 )THEN
  ilabel  = MIN(MOD(jlev,MAX_USACOLOR)*MAX(npoly/MAX_USACOLOR,1) + 2,npoly-1)
  IF( .NOT.ASSOCIATED( uwrite%iphdr ) )THEN
    label = '**ERROR**'
  ELSE
    header => uwrite%iphdr
    label = TRIM(header(nlev))
  END IF
ELSE
  ilabel = npoly+1
END IF

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
WRITE(lun,202,IOSTAT=ios)xp(1),yp(1),isymbol,isiz,icol
IF( ios /= 0 )GOTO 9999

DO i = 2,npoly-1

  IF(i == ilabel)THEN
    WRITE(lun,203,IOSTAT=ios)xp(i),yp(i),TRIM(label)
  ELSE
    WRITE(lun,200,IOSTAT=ios)xp(i),yp(i)
  END IF
  IF( ios /= 0 )GOTO 9999

END DO

WRITE(lun,200,IOSTAT=ios)xp(npoly),yp(npoly)
IF( ios /= 0 )GOTO 9999

!==============================================================================
! Set return value
!==============================================================================
WriteBodyUSA = SAG_OK

9999 CONTINUE
!==============================================================================
! Deallocate work space
!==============================================================================
IF( ALLOCATED(xp) )DEALLOCATE( xp,STAT=ios )
IF( ALLOCATED(xp) )DEALLOCATE( yp,STAT=ios )

RETURN
END
