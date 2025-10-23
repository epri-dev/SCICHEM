!===============================================================================

RECURSIVE INTEGER FUNCTION SWIMcnvCoord( x,y,coordI,xo,yo,coordO )

!------ Convert location (x,y) in coordI to coordinates (xo,yo) defined by coordO
!
!       N.B. Assumes that coordinates of the same type match, e.g., UTM in/out zones
!       are the same, or Cartesian lat/lon reference locations are the same, or Lambert
!       "true" latitutudes are the same, etc.
!
!       N.B. Currently does not handle conversions between different conformal mappings,
!       e.g., Polar to Lambert, but that could be changed.

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMcnvCoord

USE SWIMparam_fd
USE constants_fd
USE datums
USE SWIM_fi

IMPLICIT NONE

REAL,             INTENT( IN    ) :: x, y
TYPE( MapCoord ), INTENT( IN    ) :: coordI
REAL,             INTENT( OUT   ) :: xo, yo
TYPE( MapCoord ), INTENT( INOUT ) :: coordO

INTEGER irv
INTEGER zone
REAL    lat, lon, xmap, ymap, lat0, lon0

INTEGER, EXTERNAL:: LL2Lambert,  Lambert2LL
INTEGER, EXTERNAL:: LL2Polar,    Polar2LL
INTEGER, EXTERNAL:: LL2Mercator, Mercator2LL
INTEGER, EXTERNAL:: LL2RotPolar, RotPolar2LL
INTEGER, EXTERNAL:: LL2Rotll,    RotLL2LL
LOGICAL, EXTERNAL:: CheckRef

SWIMcnvCoord = SWIMfailure

!------ No transformation required for same map types

IF( coordI%type == coordO%type )THEN

  xo = x
  yo = y

  IF( coordO%type == I_UTM .AND. coordO%zone == 0 )coordO%zone = coordI%zone

ELSE IF( coordI%type == I_CARTESIAN .AND. coordO%type == I_METERS )THEN

  xo = x * 1.E3
  yo = y * 1.E3

ELSE IF( coordI%type == I_METERS .AND. coordO%type == I_CARTESIAN )THEN

  xo = x * 1.E-3
  yo = y * 1.E-3

ELSEIF( (coordI%type == I_METERS .OR. coordI%type == I_CARTESIAN) .AND. coordO%type == I_UTM )THEN

  IF( .NOT.CheckRef( coordI%reference ) )GOTO 9999

  zone = coordO%zone
  irv = LL2UTM( coordI%reference%lat,coordI%reference%lon,zone,xo,yo )
  IF( irv /= 0 )GOTO 9999

  IF( coordI%type == I_CARTESIAN )THEN
    xo = (x-coordI%reference%x) + xo
    yo = (y-coordI%reference%y) + yo
  ELSE
    xo = (x-coordI%reference%x)*1.E-3 + xo
    yo = (y-coordI%reference%y)*1.E-3 + yo
  END IF

  IF( coordO%zone == 0 )coordO%zone = zone

ELSEIF( coordI%type == I_UTM  .AND. (coordO%type == I_METERS .OR. coordO%type == I_CARTESIAN) )THEN

  IF( .NOT.CheckRef( coordO%reference ) )GOTO 9999

  zone = coordI%zone
  irv = LL2UTM( coordO%reference%lat,coordO%reference%lon,zone,xo,yo )
  IF( irv /= 0 )GOTO 9999

  IF( coordO%type == I_CARTESIAN )THEN
    xo = x - xo + coordO%reference%x
    yo = y - yo + coordO%reference%y
  ELSE
    xo = (x - xo)*1.E+3 + coordO%reference%x
    yo = (y - yo)*1.E+3 + coordO%reference%y
  END IF

ELSE

!------ Let's not deal with exotic "native to native" conversions

  IF( ANY(coordI%type==(/I_LAMBERT,I_POLAR,I_MERCATOR,I_RPOLAR,I_ROTLL/)) .AND. &
      ANY(coordO%type==(/I_LAMBERT,I_POLAR,I_MERCATOR,I_RPOLAR,I_ROTLL/)) )THEN
    error%Message = 'Coordinate conversion not supported'
    GOTO 9999
  END IF

!----- The rest of the combinations require lat/lon, perhaps as an intermediate result

  SELECT CASE( coordI%type )

    CASE( I_LATLON )
      lat = y
      lon = x

    CASE( I_UTM )
      irv = UTM2LL( coordI%zone,x,y,lat,lon )
      IF( irv /= 0 )THEN
        CALL SWIMsetUTMerror( 'UTM2LL',irv )
        GOTO 9999
      END IF

    CASE( I_CARTESIAN,I_METERS )
      IF( .NOT.CheckRef( coordI%reference ) )GOTO 9999
      xmap = 1.E3
      ymap = SPHFACR * xmap
      xmap = ymap/COS( coordI%reference%lat*PI180 )
      IF( coordI%type == I_METERS )THEN
        xmap = xmap * 1.E-3
        ymap = ymap * 1.E-3
      END IF
      lon = coordI%reference%lon + (x - coordI%reference%x)*xmap
      lat = coordI%reference%lat + (y - coordI%reference%y)*ymap

    CASE( I_LAMBERT )
      irv = Lambert2LL( x,y,coordI%Lon0,coordI%n,coordI%f,coordI%y0,lat,lon )
      IF( irv /= 1 )GOTO 9999

    CASE( I_POLAR )
      irv = Polar2LL( x,y,coordI%Lon0,coordI%n,coordI%f,coordI%y0,lat,lon )
      IF( irv /= 1 )GOTO 9999

    CASE( I_RPOLAR )
      irv = RotPolar2LL( x,y,coordI%Rearth,coordI%sp0,coordI%cp0,coordI%sl0,coordI%cl0, &
                         coordI%cc0,coordI%sc0,coordI%cs0,coordI%ss0,lat,lon )
      IF( irv /= 1 )GOTO 9999

    CASE( I_ROTLL )
      irv = RotLL2LL( x,y,coordI%Lon0,coordI%sp0,coordI%cp0,lat,lon )
      IF( irv /= 1 )GOTO 9999

    CASE( I_MERCATOR )
      irv = Mercator2LL( x,y,coordI%Lon0,coordI%f,lat,lon )
      IF( irv /= 1 )GOTO 9999

  END SELECT

!------ Convert from lat/lon to output coordinates

  SELECT CASE( coordO%type )

    CASE( I_LATLON )
      xo = lon
      yo = lat

    CASE( I_UTM )
      irv = LL2UTM( lat,lon,coordO%zone,xo,yo )
      IF( irv == 14 .OR. irv == 18 )THEN        !Handle cases out of (extended) zone
        IF( CheckRef( coordO%reference ) )THEN
          lat0 = coordO%reference%lat
          lon0 = coordO%reference%lon
          xo   = coordO%reference%x
          yo   = coordO%reference%y
        ELSE
          lat0 = lat
          lon0 = -183. + coordO%zone*6.
          irv = LL2UTM( lat0,lon0,coordO%zone,xo,yo )
          IF( irv /= 0 )GOTO 9999
        END IF
        ymap = SPHFAC * 1.E-3
        xmap = ymap*COS( lat0*PI180 )
        xo   = xo + (lon - lon0)*xmap
        yo   = yo + (lat - lat0)*ymap
      ELSE IF( irv /= 0 )THEN
        GOTO 9999
      END IF

    CASE( I_CARTESIAN,I_METERS )
      IF( .NOT.CheckRef( coordO%reference ) )GOTO 9999
      ymap = SPHFAC * 1.E-3
      xmap = ymap*COS( coordO%reference%lat*PI180 )
      IF( coordO%type == I_METERS )THEN
        xmap = xmap * 1.E+3
        ymap = ymap * 1.E+3
      END IF
      xo = coordO%reference%x + (lon - coordO%reference%lon)*xmap
      yo = coordO%reference%y + (lat - coordO%reference%lat)*ymap

    CASE( I_LAMBERT )
      irv = LL2Lambert( lat,lon,coordO%Lat0,coordO%Lon0,coordO%n,coordO%f,coordO%y0,xo,yo )
      IF( irv /= 1 )GOTO 9999

    CASE( I_POLAR )
      irv = LL2Polar( lat,lon,coordO%Lat0,coordO%Lon0,coordO%n,coordO%f,coordO%y0,xo,yo )
      IF( irv /= 1 )GOTO 9999

    CASE( I_RPOLAR )
      irv = LL2RotPolar( lat,lon,coordO%Lon0,coordO%Rearth,coordO%sp0,coordO%cp0,xo,yo )

    CASE( I_ROTLL )
      irv = LL2RotLL( lat,lon,coordO%Lon0,coordO%sp0,coordO%cp0,xo,yo )

    CASE( I_MERCATOR )
      irv = LL2Mercator( lat,lon,coordO%Lon0,coordO%f,xo,yo )
      IF( irv /= 1 )GOTO 9999

  END SELECT

END IF

9999 CONTINUE

SWIMcnvCoord = SWIMsuccess  !Currently no error checking

RETURN
END

!==============================================================================

RECURSIVE SUBROUTINE SWIMmapfac( coord,x,y,xmap,ymap )

!------ Horizontal coordinate transform function
!       (x,y) are grid coordinates
!       If dx is in meters then dx*xmap is in grid coords

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMmapfac

USE SWIMparam_fd
USE SWIM_fi
USE constants_fd

IMPLICIT NONE

TYPE( MapCoord ), INTENT( IN  ) :: coord
REAL,             INTENT( IN  ) :: x, y
REAL,             INTENT( OUT ) :: xmap, ymap

REAL ytem, r, t

SELECT CASE( coord%type )
  CASE( I_LATLON,I_ROTLL )
    ytem = MIN( ABS(y),POLARCAP_LAT )
    xmap = SPHFACR/COS(ytem*PI180)
    ymap = SPHFACR

  CASE( I_METERS )
    xmap = 1.
    ymap = 1.

  CASE( I_LAMBERT )
    r    = SQRT(x**2+(y-coord%y0)**2) * SIGN(1.,coord%n)
    r    = r/coord%f
    t    = r**(1./coord%n)
    xmap = 0.5 * coord%m0 * r * (t+1./t) * 1.0E-3
    ymap = xmap

  CASE( I_POLAR )
    r    = SQRT(x**2+(y-coord%y0)**2)
    r    = r/coord%f
    xmap = 0.5 * coord%m0 * (r*r+1.) * 1.0E-3
    ymap = xmap

  CASE( I_RPOLAR )
    t    = (x**2 + y**2)/(2.*coord%Rearth)**2
    xmap = (1. + t) * 1.0E-3
    ymap = xmap

  CASE( I_MERCATOR )
    t    = EXP(y/coord%f)
    xmap = 0.5 * coord%m0 * (t+1./t) * 1.0E-3
    ymap = xmap

  CASE DEFAULT !is cartesian km
    xmap = 1.0E-3
    ymap = 1.0E-3

END SELECT

RETURN
END

!===============================================================================

REAL FUNCTION SWIMrot2LL( x,y,coordI ) RESULT( rot )

!------ Get angle at (x,y) in coordI for rotation into lat/lon coordinates

!DEC$ ATTRIBUTES DLLEXPORT :: SWIMrot2LL

USE SWIMparam_fd
USE constants_fd
USE datums
USE SWIM_fi

IMPLICIT NONE

REAL,             INTENT( IN ) :: x, y    !In coordinate system defined by coordI
TYPE( MapCoord ), INTENT( IN ) :: coordI

INTEGER irv
REAL    lat, lon, c, s

TYPE( MapCoord ) :: coordO

INTEGER, EXTERNAL :: SWIMcnvCoord
REAL,    EXTERNAL :: SIND, COSD

SELECT CASE( coordI%type )

  CASE( I_LAMBERT,I_POLAR,I_RPOLAR,I_ROTLL )

    IF( coordI%type == I_ROTLL )THEN
      lon = x; lat = y !Use rotated lat/lon for wind rotation
    ELSE
      coordO%type = I_LATLON
      irv = SWIMcnvCoord( x,y,coordI,lon,lat,coordO )
    END IF

    IF( coordI%type == I_RPOLAR )THEN
      rot = SIND(lat)
      c   = 1. + rot**2
      s   = (c*coordI%sp0 + coordI%cp0**2*rot)*SIND(lon-coordI%Lon0)
      c   = c*COSD(lon-coordI%Lon0) + coordI%cp0*COSD(lat)
      rot = ATAN2(s,c)
    ELSE IF( coordI%type == I_ROTLL )THEN
      c   = coordI%cp0*COSD(lat) - coordI%sp0*SIND(lat)*COSD(lon) !N.B. lat,lon on rotated sphere
      s   = coordI%sp0*SIND(lon)
      rot = ATAN2(s,c)
    ELSE
      rot = (lon - coordI%Lon0)*PI180 * coordI%n
    END IF

  CASE DEFAULT

    rot = 0.

END SELECT

RETURN
END

!===============================================================================

LOGICAL FUNCTION CheckRef( reference )

USE domain_fd
USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

TYPE( referenceT ), INTENT( IN ) :: reference

CheckRef = reference%x   /= NOT_SET_R .AND. &
           reference%y   /= NOT_SET_R .AND. &
           reference%lat /= NOT_SET_R .AND. &
           reference%lon /= NOT_SET_R

RETURN
END

!==============================================================================

SUBROUTINE AdjustLongitude( lon )

!------ Add/subtract 360 so lon has the range [-180,180)

REAL, INTENT( INOUT ) :: lon

DO WHILE( lon >= 180. )
  lon = lon - 360.
END DO

DO WHILE( lon < -180. )
  lon = lon + 360.
END DO

RETURN
END

!==============================================================================

SUBROUTINE AdjustLongitudeDom( lon,dlon )

!------ Add/subtract 360 to maximize domain extent with respect to project

USE SWIM_fi
USE coordinate_fd
USE checkErr_fd

REAL, INTENT( INOUT ) :: lon
REAL, INTENT( IN    ) :: dlon

INTEGER mode
REAL    lon1, x1, x2, lon0
REAL    dx0, dxm, dxp
LOGICAL lTest
INTEGER, DIMENSION(1) :: i

LOGICAL, EXTERNAL :: SpecialValueReal, HasPrjReference

!------ Only adjust if project domain is set

mode = IBSET(0,DEFAULT_BIT); mode = IBSET(mode,NOTSET_BIT); mode = IBSET(mode,DEFER_BIT)
IF( .NOT.(SpecialValueReal(mode,prj%Xmin) .OR. SpecialValueReal(mode,prj%Xmax) .OR. &
          SpecialValueReal(mode,prj%Ymin) .OR. SpecialValueReal(mode,prj%Ymax)) )THEN

!------ Base on distance from project reference longitude if no extent is given

  IF( dlon == NOT_SET_R )THEN

    IF( HasPrjReference() )THEN
      lon0 = Prj%Lon0
    ELSE IF( Prj%coord == I_LATLON )THEN
      lon0 = 0.5*(prj%Xmin+prj%Xmax)
    ELSE
      RETURN !Don't do anything if there's no project longitude
    END IF

    IF( lon-lon0 > 180. )THEN
      lon = lon - 360.
    ELSE IF( lon-lon0 < -180. )THEN
      lon = lon + 360.
    END IF

!------ Define origin that results in largest (positive) extent

  ELSE IF( Prj%coord == I_LATLON )THEN

    lon1 = lon + dlon
    dx0  = MIN(lon1,prj%Xmax) - MAX(lon,prj%Xmin)

    IF( dx0 < 0.999*dlon )THEN

      dxp = MIN(lon1+360.,prj%Xmax) - MAX(lon+360.,prj%Xmin)
      dxm = MIN(lon1-360.,prj%Xmax) - MAX(lon-360.,prj%Xmin)

      i =  MAXLOC((/dxm,dx0,dxp/))
      SELECT CASE( i(1) )
        CASE( 1 )
          lon = lon - 360.
        CASE( 3 )
          lon = lon + 360.
      END SELECT

    END IF

  ELSE  !UTM or Cartesian

    IF( Prj%coord == I_UTM .AND. (Prj%UTMZone >= 1 .AND. Prj%UTMZone <= 60) )THEN
      x1 = -183. + FLOAT(Prj%UTMZone)*6.  !Central longitude of zone
      lTest = .TRUE.
    ELSE IF( HasPrjReference() )THEN
      x1 = Prj%Lon0
      lTest = .TRUE.
    ELSE
      lTest = .FALSE.
    END IF

    IF( lTest )THEN
      x2   = x1 + 9.    !Test with "extended zone" of -/+9 deg.
      x1   = x1 - 9.
      lon1 = lon + dlon
      dx0  = MIN(lon1     ,x2) - MAX(lon     ,x1)
      dxp  = MIN(lon1+360.,x2) - MAX(lon+360.,x1)
      dxm  = MIN(lon1-360.,x2) - MAX(lon-360.,x1)

      i =  MAXLOC((/dxm,dx0,dxp/))
      SELECT CASE( i(1) )
        CASE( 1 )
          lon = lon - 360.
        CASE( 3 )
          lon = lon + 360.
      END SELECT
    END IF

  END IF

END IF

RETURN
END

!==============================================================================

SUBROUTINE SWIMsetUTMerror( subr_name,ierr )

USE SWIMparam_fd
USE SWIM_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: subr_name
INTEGER,      INTENT( IN ) :: ierr

CHARACTER(20) string

WRITE(string,*) ierr

error%Number  = IV_ERROR
error%Routine = TRIM(subr_name)
error%Message = 'Error from DtmsExtendedZone'
error%Inform  = 'Error = '//TRIM(ADJUSTL(string))

SELECT CASE( ierr )
  CASE( 3 )
    error%Action = 'Longitude not in range [-180,+180]'
  CASE( 4 )
    error%Action = 'Latitude not in range [-80,+84]'
  CASE( 12 )
    error%Action = 'Invalid zone number'
  CASE( 13 )
    error%Action = 'Northing not in range [-1E5,+1E5] km'
  CASE( 14 )
    error%Action = 'Easting more than 9 deg from central meridian'
  CASE( 18 )
    error%Action = 'More that 9.17 deg from central meridian'
  CASE( 20 )
    error%Action = 'Error initializing DATUMS'
  CASE DEFAULT
    error%Action = 'Unexpected error condition'
END SELECT

RETURN
END
