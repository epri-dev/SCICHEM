INTEGER(C_LONG) FUNCTION DtmsInitDatums( pFile ) RESULT( irv ) &
    BIND(C,Name='DtmsInitDatums')

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_CHAR
USE fdatums_fi

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT::DtmsInitDatums

INTEGER, PARAMETER :: PATH_MAXLENGTH = 256
CHARACTER(KIND=C_CHAR), DIMENSION(PATH_MAXLENGTH) :: pFile !Dummy file name

!------ For the NAD83/WGS84  Datum

er = 6378137.D0
rf = 298.257222101D0

!------ Pre-compute constants

fe = 500000.0D0
sf = 0.9996D0
or = 0.0D0

CALL tconst( er,rf,sf,or,esq,eps,r,a,b,c,u,v,w,so )
CALL tconpc( v0,v2,v4,v6,er,esq,rf )

irv = 1 !Success

END FUNCTION DtmsInitDatums

!==============================================================================

INTEGER(C_LONG) FUNCTION DtmsExtendedZone( lon,lat,ilzone,xdecal,xeast,xnorth, &
    ildir,ilerr,lipsoid) RESULT( irv ) BIND(C,Name='DtmsExtendedZone')

!DEC$ ATTRIBUTES DLLEXPORT::DtmsExtendedZone

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_FLOAT

IMPLICIT NONE

REAL(C_FLOAT)   :: lon
REAL(C_FLOAT)   :: lat
INTEGER(C_LONG) :: ilzone
REAL(C_FLOAT)   :: xdecal
REAL(C_FLOAT)   :: xeast
REAL(C_FLOAT)   :: xnorth
INTEGER(C_LONG) :: ildir
INTEGER(C_LONG) :: ilerr
INTEGER(C_LONG) :: lipsoid

INTEGER :: izone

izone = INT(ilzone, KIND(izone)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux

IF( ildir > 0 )THEN

  CALL drgput( lon,lat,izone,xeast,xnorth )

ELSE IF( ildir < 0 )THEN

  CALL drutgp( xeast,xnorth,izone,lon,lat )

END IF

ilzone = izone

ilerr = 0
irv   = 1

END FUNCTION DtmsExtendedZone

!==============================================================================

INTEGER FUNCTION DtmsUTMfromLLAdomain( Wlla,Ella,Nlla,Slla,zone, &
    Wutm,Eutm,Nutm,Sutm,rcode ) RESULT( irv ) BIND(C,Name='DtmsUTMfromLLAdomain')

!DEC$ ATTRIBUTES DLLEXPORT::DtmsUTMfromLLAdomain

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

REAL    lat, lon, lonW, lonE
REAL    x, y
INTEGER izone

lat = 0.5*(Nlla+Slla)
lonE = Ella
lonW = Wlla
IF( Ella - Wlla < 0. )lonE = 360. + lonE
lon = 0.5*(lonE+lonW)

izone = 0
CALL drgput( lon,lat,izone,x,y )

CALL drgput( Wlla,Slla,izone,x,y )
Wutm = x
Sutm = y

CALL drgput( Wlla,Nlla,izone,x,y )
Wutm = MAX(Wutm,x)
Nutm = y

CALL drgput( Ella,Nlla,izone,x,y )
Eutm = x
Nutm = MIN(Nutm,y)

CALL drgput( Ella,Slla,izone,x,y )
Eutm = MIN(Eutm,x)
Sutm = MAX(Sutm,y)

rcode = 1
irv   = 1

RETURN

END FUNCTION DtmsUTMfromLLAdomain

!************************************************************************
       SUBROUTINE drgput( lon,lat,izone,xeast,xnorth )
!**********************************************************************

!      COMPUTE UTM NORTHINGS AND EASTINGS
!
!      VARIABLES
!      ER = EQUATORIAL RADIUS OF THE ELLIPSOID (SEMI-MAJOR AXIS)
!      RF = RECIPROCAL OF FLATTING OF THE ELLIPSOD
!      ESQ= E SQUARED
!      RAD = RADIAN CONVERSION FACTOR
!      CM = CENTRAL MERIDIAN ( COMPUTED USEING THE LONGITUDE)
!      SF = SCALE FACTOR OF CENTRAL MERIDIAN ( ALWAYS .9996 FOR UTM)
!      OR = SOUTHERNMOST PARALLEL OF LATITUDE ( ALWAYS ZERO FOR UTM)
!      R, A, B, C, U, V, W = ELLIPSOID CONSTANTS USED FOR COMPUTING
!                            MERIDIONAL DISTANCE FROM LATITUDE
!      SO = MERIDIONAL DISTANCE (MULTIPLIED BY SCALE FACTOR )
!           FROM THE EQUATOR TO THE SOUTHERNMOST PARALLEL OF LATITUDE
!           ( ALWAYS ZERO FOR UTM)

USE fdatums_fi

IMPLICIT NONE

REAL(4), INTENT( IN    ) :: lon, lat
INTEGER, INTENT( INOUT ) :: izone
REAL(4), INTENT( OUT   ) :: xeast, xnorth

INTEGER lod, icm, iz
REAL(8) fi, lam, cm, fn, east, north, conv, kp

fi = DBLE(lat) / rad
iz = ABS(izone)

IF( lon >= 0. )THEN
  lam = (360.D0-DBLE(lon)) / rad
  lod = INT(360.D0-DBLE(lon))
ELSE
  lam = -DBLE(lon) / rad
  lod = -INT(lon)
END IF

IF( iz == 0 )THEN

  IF( lod < 180 )THEN
    iz  = lod/6
    iz  = 30 - iz
    icm = (183-(6*iz))
    cm  = DBLE(FLOAT(icm))/rad
  ELSE
    iz  = lod/6
    iz  = 90 - iz
    icm = (543 - (6*iz))
    cm  =  DBLE(FLOAT(icm))/rad
  ENDIF

ELSE

  IF( iz <= 30 )THEN
    icm = (183-(6*iz))
    cm  = DBLE(FLOAT(icm))/rad
  ELSE
    icm = (543 - (6*iz))
    cm  =  DBLE(FLOAT(icm))/rad
  ENDIF

  IF( ABS(lod-icm) > 180 )THEN
    IF( icm > 180 )THEN
      lam = lam + 360.D0 / rad
    ELSE
      lam = lam - 360.D0 / rad
    END IF
  END IF

END IF

IF( lat < 0. ) THEN
 fn = 10000000.D0
 fi = -fi
 IF( izone == 0 )izone = -iz
ELSE
 fn = 0.D0
 IF( izone == 0 )izone = iz
ENDIF

CALL tmgrid( fi,lam,north,east,conv,kp,er,esq,eps,cm,fe,fn,sf,so,r,a,b,c,u,v,w )

xeast  = SNGL(east)  * 1.E-3
xnorth = SNGL(north) * 1.E-3

RETURN
END

!************************************************************************
       SUBROUTINE drutgp( xeast,xnorth,izone,lon,lat )
!**********************************************************************

USE fdatums_fi

IMPLICIT NONE

REAL(4), INTENT( IN  ) :: xeast, xnorth
INTEGER, INTENT( IN  ) :: izone
REAL(4), INTENT( OUT ) :: lon, lat

REAL(8) n, e, rlat, rlon, cm, fn, conv, kp
INTEGER iz, icm

iz = ABS(izone)

IF( iz < 30 )THEN
   icm = 183-(6*iz)
   cm  = DBLE(FLOAT(icm)) / rad
ELSE
   icm = 543 - (6*iz)
   cm  = DBLE(FLOAT(icm)) / rad
END IF

IF( izone < 0 )THEN
  fn = 10000000.0d0
ELSE
  fn = 0.d0
END IF

n = DBLE(xnorth)*1.D03
e = DBLE(xeast)*1.D03

CALL tmgeod( n,e,rlat,rlon,eps,cm,fe,sf,so,r,v0,v2,v4,v6,fn,er,esq,conv,kp )

lat = SNGL(rlat*rad)
lon = 360. - SNGL(rlon*rad)
IF( lon > 180. ) lon = lon - 360.

RETURN
END

!==============================================================================

INTEGER(C_LONG) FUNCTION DtmsExtendedZone8( lon,lat,ilzone,xdecal,xeast,xnorth,&
    ildir,ilerr,lipsoid) RESULT( irv ) BIND(C,Name='DtmsExtendedZone8')

!DEC$ ATTRIBUTES DLLEXPORT::DtmsExtendedZone8

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LONG, C_DOUBLE

IMPLICIT NONE

REAL(C_DOUBLE)  :: lon
REAL(C_DOUBLE)  :: lat
INTEGER(C_LONG) :: ilzone
REAL(C_DOUBLE)  :: xdecal
REAL(C_DOUBLE)  :: xeast
REAL(C_DOUBLE)  :: xnorth
INTEGER(C_LONG) :: ildir
INTEGER(C_LONG) :: ilerr
INTEGER(C_LONG) :: lipsoid

INTEGER :: izone

izone = INT(ilzone, KIND(izone)) ! be careful, INTEGER(C_LONG) to INTEGER on Linux

IF( ildir > 0 )THEN

  CALL drgput8( lon,lat,izone,xeast,xnorth )

ELSE IF( ildir < 0 )THEN

  CALL drutgp8( xeast,xnorth,izone,lon,lat )

END IF

ilzone = izone

irv  = 1
ilerr = 0

END FUNCTION DtmsExtendedZone8

!************************************************************************
       SUBROUTINE drgput8( lon,lat,izone,xeast,xnorth )
!**********************************************************************

!      COMPUTE UTM NORTHINGS AND EASTINGS
!
!      VARIABLES
!      ER = EQUATORIAL RADIUS OF THE ELLIPSOID (SEMI-MAJOR AXIS)
!      RF = RECIPROCAL OF FLATTING OF THE ELLIPSOD
!      ESQ= E SQUARED
!      RAD = RADIAN CONVERSION FACTOR
!      CM = CENTRAL MERIDIAN ( COMPUTED USEING THE LONGITUDE)
!      SF = SCALE FACTOR OF CENTRAL MERIDIAN ( ALWAYS .9996 FOR UTM)
!      OR = SOUTHERNMOST PARALLEL OF LATITUDE ( ALWAYS ZERO FOR UTM)
!      R, A, B, C, U, V, W = ELLIPSOID CONSTANTS USED FOR COMPUTING
!                            MERIDIONAL DISTANCE FROM LATITUDE
!      SO = MERIDIONAL DISTANCE (MULTIPLIED BY SCALE FACTOR )
!           FROM THE EQUATOR TO THE SOUTHERNMOST PARALLEL OF LATITUDE
!           ( ALWAYS ZERO FOR UTM)

USE fdatums_fi

IMPLICIT NONE

REAL(8), INTENT( IN    ) :: lon, lat
INTEGER, INTENT( INOUT ) :: izone
REAL(8), INTENT( OUT   ) :: xeast, xnorth

INTEGER lod, icm, iz
REAL(8) fi, lam, cm, fn, east, north, conv, kp

fi = lat / rad
iz = ABS(izone)

IF( lon >= 0. )THEN
  lam = (360.D0-lon) / rad
  lod = INT(360.D0-lon)
ELSE
  lam = -lon / rad
  lod = -INT(lon)
END IF

IF( iz == 0 )THEN

  IF( lod < 180 )THEN
    iz  = lod/6
    iz  = 30 - iz
    icm = (183-(6*iz))
    cm  = DBLE(FLOAT(icm))/rad
  ELSE
    iz  = lod/6
    iz  = 90 - iz
    icm = (543 - (6*iz))
    cm  =  DBLE(FLOAT(icm))/rad
  ENDIF

ELSE

  IF( iz <= 30 )THEN
    icm = (183-(6*iz))
    cm  = DBLE(FLOAT(icm))/rad
  ELSE
    icm = (543 - (6*iz))
    cm  =  DBLE(FLOAT(icm))/rad
  ENDIF

  IF( ABS(lod-icm) > 180 )THEN
    IF( icm > 180 )THEN
      lam = lam + 360.D0 / rad
    ELSE
      lam = lam - 360.D0 / rad
    END IF
  END IF

END IF

IF( lat < 0. ) THEN
 fn = 10000000.D0
 fi = -fi
 IF( izone == 0 )izone = -iz
ELSE
 fn = 0.D0
 IF( izone == 0 )izone = iz
ENDIF

CALL tmgrid( fi,lam,north,east,conv,kp,er,esq,eps,cm,fe,fn,sf,so,r,a,b,c,u,v,w )

xeast  = east  * 1.E-3
xnorth = north * 1.E-3

RETURN
END

!************************************************************************
       SUBROUTINE drutgp8( xeast,xnorth,izone,lon,lat )
!**********************************************************************

USE fdatums_fi

IMPLICIT NONE

REAL(8), INTENT( IN  ) :: xeast, xnorth
INTEGER, INTENT( IN  ) :: izone
REAL(8), INTENT( OUT ) :: lon, lat

REAL(8) n, e, rlat, rlon, cm, fn, conv, kp
INTEGER iz, icm

iz = ABS(izone)

IF( iz < 30 )THEN
   icm = 183-(6*iz)
   cm  = DBLE(FLOAT(icm)) / rad
ELSE
   icm = 543 - (6*iz)
   cm  = DBLE(FLOAT(icm)) / rad
END IF

IF( izone < 0 )THEN
  fn = 10000000.0d0
ELSE
  fn = 0.d0
END IF

n = xnorth*1.D03
e = xeast *1.D03

CALL tmgeod( n,e,rlat,rlon,eps,cm,fe,sf,so,r,v0,v2,v4,v6,fn,er,esq,conv,kp )

lat = rlat * rad
lon = 360. - rlon * rad
IF( lon > 180. ) lon = lon - 360.

RETURN
END
