MODULE datums

IMPLICIT NONE

INTERFACE

!==============================================================================
! DtmsExtendedZone
!==============================================================================
  INTEGER(C_LONG) FUNCTION DtmsExtendedZone(rlamda,rphi,izone,xdecal,xeast,xnorth,idir,ierr,lipsoid) &
    BIND(C,NAME='DtmsExtendedZone')

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_FLOAT
    IMPLICIT NONE

    REAL(C_FLOAT)   :: rlamda
    REAL(C_FLOAT)   :: rphi
    INTEGER(C_LONG) :: izone
    REAL(C_FLOAT)   :: xdecal
    REAL(C_FLOAT)   :: xeast
    REAL(C_FLOAT)   :: xnorth
    INTEGER(C_LONG) :: idir
    INTEGER(C_LONG) :: ierr
    INTEGER(C_LONG) :: lipsoid
  END FUNCTION DtmsExtendedZone

  INTEGER(C_LONG) FUNCTION DtmsExtendedZone8(rlamda,rphi,izone,xdecal,xeast,xnorth,idir,ierr,lipsoid) &
    BIND(C,NAME='DtmsExtendedZone8')

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_DOUBLE
    IMPLICIT NONE

    REAL(C_DOUBLE)  :: rlamda
    REAL(C_DOUBLE)  :: rphi
    INTEGER(C_LONG) :: izone
    REAL(C_DOUBLE)  :: xdecal
    REAL(C_DOUBLE)  :: xeast
    REAL(C_DOUBLE)  :: xnorth
    INTEGER(C_LONG) :: idir
    INTEGER(C_LONG) :: ierr
    INTEGER(C_LONG) :: lipsoid
  END FUNCTION DtmsExtendedZone8

!==============================================================================
! DtmsUTMfromLLAdomain
!==============================================================================
  INTEGER(C_LONG) FUNCTION DtmsUTMfromLLAdomain(Wlla,Ella,Nlla,Slla,Zone,Wutm,Eutm,Nutm,Sutm,Rcode) &
    BIND(C,NAME='DtmsUTMfromLLAdomain')

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_FLOAT
    IMPLICIT NONE

    REAL(C_FLOAT)   :: Wlla
    REAL(C_FLOAT)   :: Ella
    REAL(C_FLOAT)   :: Nlla
    REAL(C_FLOAT)   :: Slla
    REAL(C_FLOAT)   :: Wutm
    REAL(C_FLOAT)   :: Eutm
    REAL(C_FLOAT)   :: Nutm
    REAL(C_FLOAT)   :: Sutm
    INTEGER(C_LONG) :: Zone
    INTEGER(C_LONG) :: Rcode
  END FUNCTION DtmsUTMfromLLAdomain

!==============================================================================
! DtmsInitDatums
!==============================================================================
  INTEGER(C_LONG) FUNCTION DtmsInitDatums( pFile )  RESULT( irv ) BIND(C,NAME='DtmsInitDatums')
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_CHAR
    USE DefSize_fd
    IMPLICIT NONE

    CHARACTER(KIND=C_CHAR), DIMENSION(PATH_MAXLENGTH) :: pFile
  END FUNCTION DtmsInitDatums

END INTERFACE

!==============================================================================

CONTAINS

!===============================================================================

LOGICAL FUNCTION InitUTM( INIfile )

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_NULL_CHAR
USE DefSize_fd

!------ Checks to see if DATUMS dll is installed OK

CHARACTER(*), INTENT( IN ) :: INIfile

InitUTM = DtmsInitDatums( TRIM(INIfile)//C_NULL_CHAR ) == 1

RETURN
END FUNCTION InitUTM

!==============================================================================

INTEGER FUNCTION UTM2LL( zone,x,y,lat,lon ) RESULT( ierr )

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

!------ Convert from UTM to geodetic location

INTEGER, INTENT( IN  ) :: zone
REAL,    INTENT( IN  ) :: x,y
REAL,    INTENT( OUT ) :: lat,lon

! Need to match external datums library variable sizes to avoid Linux problems
INTEGER(C_LONG) irv, izone, idir, lipsoid, ierrl
REAL    xdecal, xeast, xnorth, ya

idir    = -1
xdecal  = 0.
ierr    = 0
lipsoid = 0

xeast  = x
xnorth = y

IF( xnorth < 0 )THEN
  xnorth = 10000. + xnorth
  izone  = -zone
ELSE
  izone = zone
END IF

!$OMP CRITICAL (DATUMS)
irv = DtmsExtendedZone( lon,lat,izone,xdecal,xeast,xnorth, &
                        idir,ierrl,lipsoid )
ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux
!$OMP END CRITICAL (DATUMS)
!------ If error, limit (x,y) to valid locations

IF( ierr /= 0 )THEN

  ya = MIN(10000.,ABS(y))
  IF( ABS(ya) <= 2230. )THEN
    xeast = MIN(MAX(x,-524.),1524.)
  ELSE IF( ABS(ya) <= 4460. )THEN
    xeast = MIN(MAX(x,-462.),1462.)
  ELSE IF( ABS(ya) <= 6680. )THEN
    xeast = MIN(MAX(x,-283.),1283.)
  ELSE IF( ABS(ya) <= 8890. )THEN
    xeast = MIN(MAX(x, -10.),1010.)
  ELSE
    xeast = MIN(MAX(x,323.),677.)
  END IF
  xnorth = SIGN(ya,y)

  IF( xnorth < 0 )THEN
    xnorth = 10000. + xnorth
  END IF

!$OMP CRITICAL (DATUMS)
  irv = DtmsExtendedZone( lon,lat,izone,xdecal,xeast,xnorth, &
                          idir,ierrl,lipsoid )
  ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux
!$OMP END CRITICAL (DATUMS)

END IF

RETURN
END FUNCTION UTM2LL

!==============================================================================

INTEGER FUNCTION UTM2LLD( zone,x,y,lat,lon ) RESULT( ierr )

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

!------ Convert from UTM to geodetic location

INTEGER, INTENT( IN  ) :: zone
REAL(8), INTENT( IN  ) :: x,y
REAL(8), INTENT( OUT ) :: lat,lon

! Need to match external datums library variable sizes to avoid Linux problems
INTEGER(C_LONG) irv, izone, idir, lipsoid, ierrl
REAL(8) xdecal, xeast, xnorth, ya

idir    = -1
xdecal  = 0.0D0
ierr    = 0
lipsoid = 0

xeast  = x
xnorth = y

IF( xnorth < 0.0D0 )THEN
  xnorth = 10000.0D0 + xnorth
  izone  = -zone
ELSE
  izone = zone
END IF

!$OMP CRITICAL (DATUMS)
irv = DtmsExtendedZone8( lon,lat,izone,xdecal,xeast,xnorth, &
                        idir,ierrl,lipsoid )
ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux
!$OMP END CRITICAL (DATUMS)

!------ If error, limit (x,y) to valid locations

IF( ierr /= 0 )THEN

  ya = MIN(10000.0D0,ABS(y))
  IF( ABS(ya) <= 2230.0D0 )THEN
    xeast = MIN(MAX(x,-524.0D0),1524.0D0)
  ELSE IF( ABS(ya) <= 4460.0D0 )THEN
    xeast = MIN(MAX(x,-462.0D0),1462.0D0)
  ELSE IF( ABS(ya) <= 6680.0D0 )THEN
    xeast = MIN(MAX(x,-283.0D0),1283.0D0)
  ELSE IF( ABS(ya) <= 8890.0D0 )THEN
    xeast = MIN(MAX(x, -10.0D0),1010.0D0)
  ELSE
    xeast = MIN(MAX(x,323.0D0),677.0D0)
  END IF
  xnorth = SIGN(ya,y)

  IF( xnorth < 0.0D0 )THEN
    xnorth = 10000.0D0 + xnorth
  END IF

!$OMP CRITICAL (DATUMS)
  irv = DtmsExtendedZone8( lon,lat,izone,xdecal,xeast,xnorth, &
                          idir,ierrl,lipsoid )
  ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux
!$OMP END CRITICAL (DATUMS)

END IF

RETURN
END FUNCTION UTM2LLD

!==============================================================================

INTEGER FUNCTION LL2UTM( lat,lon,zone,x,y ) RESULT( ierr )

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

!------ Convert from geodetic to UTM location

INTEGER, INTENT( INOUT ) :: zone
REAL,    INTENT( OUT   ) :: x,y
REAL,    INTENT( IN    ) :: lat,lon

! Need to match external datums library variable sizes to avoid Linux problems
INTEGER(C_LONG) irv, izone, idir, lipsoid, izone0, ierrl
REAL    rlamda, rphi, xdecal

idir    = 1
xdecal  = 0.
ierr    = 0
lipsoid = 0

rlamda = lon
rphi   = lat
izone  = zone

IF( izone == 0 )THEN
  IF( rlamda < -180. )THEN
    rlamda = rlamda + 360.
  ELSE IF( rlamda > 180. )THEN
    rlamda = rlamda - 360.
  END IF
!------ Handle longitude outside -/+180
!       Move to nearby zone at same distance from central meridian
ELSE IF( lon < -180. )THEN
  izone0 = ABS(izone)
  irv    = SIGN(1,izone)
  DO izone = izone0+1,60
    rlamda = rlamda + 6.
    IF( rlamda >= -180. )EXIT
  END DO
  izone = SIGN(izone,irv)
ELSE IF( lon > 180. )THEN
  izone0 = ABS(izone)
  irv    = SIGN(1,izone)
  DO izone = izone0-1,1,-1
    rlamda = rlamda - 6.
    IF( rlamda <= 180. )EXIT
  END DO
  izone = SIGN(izone,irv)
END IF

irv = DtmsExtendedZone( rlamda,rphi,izone,xdecal,x,y, &
                        idir,ierrl,lipsoid )
ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux

!------ If error, limit to +/-9 deg. of center meridian

IF( ierr /= 0 )THEN
  rphi   = MIN(MAX(lat,-89.),89.)
  rlamda = -183. + izone*6.
  rlamda = MIN(MAX(lon,rlamda-9.),rlamda+9.)
  irv = DtmsExtendedZone( rlamda,rphi,izone,xdecal,x,y, &
                          idir,ierrl,lipsoid )
  ierr = INT(ierrl, KIND(ierr)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux
END IF

IF( zone == 0 )zone = izone

RETURN
END FUNCTION LL2UTM

!==============================================================================

INTEGER FUNCTION UTMdomain( Wlla,Ella,Nlla,Slla,Wutm,Eutm,Nutm,Sutm )

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG

!------ Get bounding UTM domain for rectangular LL domain

REAL, INTENT( IN  ) :: Wlla,Ella,Nlla,Slla
REAL, INTENT( OUT ) :: Wutm,Eutm,Nutm,Sutm

LOGICAL lok
! Need to match external datums library variable sizes to avoid Linux problems
INTEGER(C_LONG) irv, zone, rcode

irv = DtmsUTMfromLLAdomain( Wlla,Ella,Nlla,Slla,zone,Wutm,Eutm,Nutm,Sutm,rcode )
lok = (irv == 1) .AND. (rcode == 1)

IF( .NOT.lok )THEN
  UTMdomain = rcode
ELSE
  UTMdomain = 0
END IF

RETURN
END FUNCTION UTMdomain

END MODULE datums
