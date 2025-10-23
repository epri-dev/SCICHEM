MODULE MapConstants_fd

  REAL, PARAMETER :: PI2 = 1.5707963
  REAL, PARAMETER :: D2R = PI2/90.
  REAL, PARAMETER :: R2D = 90./PI2

END MODULE MapConstants_fd

!==============================================================================

INTEGER FUNCTION InitLambert( lat0,lat1,lat2,a,n,f,m0,y0 ) RESULT( irv )

!------ Pre-compute factors for Lambert Conformal Projection

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat0        !Reference (central) latitude
REAL, INTENT( IN  ) :: lat1, lat2  !Standard (true) latitudes
REAL, INTENT( IN  ) :: a           !Radius of sphere
REAL, INTENT( OUT ) :: n           !Exponent
REAL, INTENT( OUT ) :: f
REAL, INTENT( OUT ) :: m0          !for map factor
REAL, INTENT( OUT ) :: y0          !Latitude offset

REAL clat0, clat1, clat2
REAL t0, t1, t2, s1, s2

!------ Check input

IF( ABS(lat0) > 89. .OR. &  !Error if latitudes are above 89 degrees
    ABS(lat1) > 89. .OR. &
    ABS(lat2) > 89. )THEN
  irv = -1
  RETURN
END IF

IF( lat1 > lat2 )THEN       !Swap latitudes if not in proper order
  clat1  = lat2
  clat2  = lat1
ELSE
  clat1  = lat1
  clat2  = lat2
END IF

IF( lat1 * lat2 <= 0. )THEN !Error if standard latitudes bracket (or include) equator
  irv = -3
  RETURN
END IF

!------ Define colatitudes

clat0 = PI2 - lat0*D2R
clat1 = PI2 - clat1*D2R
clat2 = PI2 - clat2*D2R

!------ Factors for converting location and map factor

t0 = TAN(clat0/2.)
t1 = TAN(clat1/2.)
t2 = TAN(clat2/2.)
s1 = SIN(clat1)
s2 = SIN(clat2)

IF( ABS(lat2-lat1) < 0.1 )THEN
  n = COS(clat1)
ELSE
  n = LOG(s2/s1) / LOG(t2/t1)
END IF
m0 = s1 / t1**n
f  = a * m0 / n
y0 = f * t0**n

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION LL2Lambert( lat,lon,lat0,lon0,n,f,y0,x,y ) RESULT( irv )

!------ Compute (x,y) of the Lambert Conformal Projections
!       Assumes InitLambert has been called to generate n,f,y0

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat,  lon   !Input lat/lon
REAL, INTENT( IN  ) :: lat0, lon0  !Reference lat/lon
REAL, INTENT( IN  ) :: n,f,y0      !Pre-computed projection parameters
REAL, INTENT( OUT ) :: x, y        !Projection coordinates (km)

REAL t, r, arg

IF( ABS(lat ) > 89. .OR. & !Error if latitudes are above 89 degrees
    ABS(lat0) > 89. )THEN
  irv = -1
  RETURN
END IF

t = PI2 - lat*D2R
t = TAN(0.5*t)
r = f * t**n

arg = lon-lon0
IF( arg > 180. )THEN
  arg = arg - 360.
ELSE IF( arg < -180. )THEN
  arg = arg + 360.
END IF
arg = n*arg*D2R

x = r*SIN(arg)
y = y0 - r*COS(arg)

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION Lambert2LL( x,y,lon0,n,f,y0,lat,lon ) RESULT( irv )

!------ Compute (lat,lon) corresponding to location (x,y) in Lambert Conformal Projection
!       Assumes InitLambert has been called to generate n,f,y0

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y      !Map coordinates (km)
REAL, INTENT( IN  ) :: lon0      !Reference longitude
REAL, INTENT( IN  ) :: n,f,y0    !Pre-computed projection parameters
REAL, INTENT( OUT ) :: lat, lon  !Output lat/lon

REAL r, t, arg

t   = ATAN(x/(y0-y))
r   = SQRT(x*x + (y-y0)**2) * SIGN(1.,n)
arg = (r/f)**(1./n)

lon = lon0 + t/n * R2D
lat = (PI2 - 2.*ATAN(arg)) * R2D

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION InitPolar( lat0,lat1,a,n,f,m0,y0 ) RESULT( irv )

!------ Pre-compute factors for Polar Stereographic Projection

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat0  !Reference (central) latitude
REAL, INTENT( IN  ) :: lat1  !Standard (true) latitudes
REAL, INTENT( IN  ) :: a     !Radius of sphere
REAL, INTENT( OUT ) :: n, f  !Pre-computed factors
REAL, INTENT( OUT ) :: m0    !for map factor
REAL, INTENT( OUT ) :: y0    !Latitude offset

REAL clat0, clat1
REAL t0, t1, s1

!------ Check input

IF( ABS(lat0) > 90. .OR. & !Error if latitudes are above 89 degrees
    ABS(lat1) > 90. .OR. &
    lat0*lat1 <  0. )THEN  !Error if lat0 and lat1 are in different hemispheres
  irv = -1
  RETURN
END IF

!------ Define colatitudes

IF( lat1 >= 0. )THEN
  n = 1.0
ELSE
  n = -1.0
END IF
clat0 = PI2 - lat0*D2R
clat1 = PI2 - lat1*D2R

!------ Factors for converting location and map factor

t0 = TAN(clat0/2.)
t1 = TAN(clat1/2.)
s1 = SIN(clat1)

m0 = s1 / t1**n
f  = a * m0 / n
y0 = f * t0**n

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION LL2Polar( lat,lon,lat0,lon0,n,f,y0,x,y ) RESULT( irv )

!------ Compute (x,y) of Polar Stereographic Projections
!       Assumes InitPolar has been called to generate f,y0

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat,  lon   !Input lat/lon
REAL, INTENT( IN  ) :: lat0, lon0  !Reference lat/lon
REAL, INTENT( IN  ) :: n, f, y0    !Pre-computed projection parameters
REAL, INTENT( OUT ) :: x, y        !Projection coordinates (km)

REAL t, r, arg

IF( ABS(lat ) > 90. .OR. & !Error if latitudes are above 89 degrees
    ABS(lat0) > 90. .OR. &
    lat0*lat  < 0.  )THEN
  irv = -1
  RETURN
END IF

t = PI2 - lat*D2R
t = TAN(0.5*t)
r = f * t**n

arg = lon-lon0
IF( arg > 180. )THEN
  arg = arg - 360.
ELSE IF( arg < -180. )THEN
  arg = arg + 360.
END IF
arg = n*arg*D2R

x = r*SIN(arg)
y = y0 - r*COS(arg)

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION Polar2LL( x,y,lon0,n,f,y0,lat,lon ) RESULT( irv )

!------ Compute (lat,lon) corresponding to location (x,y) in Polar Stereographic Projection
!       Assumes InitPolar has been called to generate f,y0

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y      !Map coordinates (km)
REAL, INTENT( IN  ) :: lon0      !Reference longitude
REAL, INTENT( IN  ) :: n, f, y0  !Pre-computed projection parameters
REAL, INTENT( OUT ) :: lat, lon  !Output lat/lon

REAL r, t, arg

t   = ATAN(x/(y0-y))
r   = SQRT(x*x + (y-y0)**2) * SIGN(1.,n)
arg = (r/f)**(1./n)

lon = lon0 + t/n * R2D
lat = (PI2 - 2.*ATAN(arg)) * R2D

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION InitRotPolar( lat0,lon0,sp0,cp0,sl0,cl0,cc0,sc0,cs0,ss0 ) RESULT( irv )

!------ Pre-compute factors for Polar Stereographic Projection

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat0              !Rotated pole latitude
REAL, INTENT( IN  ) :: lon0              !Rotated pole longitude
REAL, INTENT( OUT ) :: sp0, cp0          !Sin,Cos of latitude
REAL, INTENT( OUT ) :: sl0, cl0          !Sin,Cos of longitude
REAL, INTENT( OUT ) :: cc0,sc0, cs0, ss0 !Products (lat,lon), e.g., sc0 = sp0*cl0

!------ Check input

IF( ABS(lat0) > 90. )THEN
  irv = -1
  RETURN
END IF

!------ Save sines, cosines and products

sp0 = SIN(lat0*D2R)
cp0 = COS(lat0*D2R)

sl0 = SIN(lon0*D2R)
cl0 = COS(lon0*D2R)

cc0 = cp0*cl0
sc0 = sp0*cl0
cs0 = cp0*sl0
ss0 = sp0*sl0

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION LL2RotPolar( lat,lon,lon0,a,sp0,cp0,x,y ) RESULT( irv )

!------ Compute (x,y) of Rotated Polar Stereographic Projections
!       Assumes InitRotPolar has been called to generate sines and cosine terms

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat,  lon !Input lat/lon
REAL, INTENT( IN  ) :: lon0      !Reference lon
REAL, INTENT( IN  ) :: a         !Earth radius
REAL, INTENT( IN  ) :: sp0,cp0   !Pre-computed sin and cos(lat0)
REAL, INTENT( OUT ) :: x, y      !Projection coordinates (km)

REAL t, sl, cl, sp, cp

sl = SIN((lon-lon0) * D2R)
cl = COS((lon-lon0) * D2R)

sp = SIN(lat * D2R)
cp = COS(lat * D2R)

t = 2.*a / (1.+sp0*sp+cp0*cp*cl )
!t = 2.*a / (1.+sp*sp+cp0*cp*cl )

x = cp*sl
y = cp0*sp - sp0*cp*cl

x = x * t
y = y * t

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION RotPolar2LL( x,y,a,sp0,cp0,sl0,cl0,cc0,sc0,cs0,ss0,lat,lon ) RESULT( irv )

!------ Compute (lat,lon) corresponding to location (x,y) in Rotated Polar Stereographic Projection
!       Assumes InitRotPolar has been called to generate sines and cosine terms

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y               !Map coordinates (km)
REAL, INTENT( IN  ) :: a                  !Earth radius
REAL, INTENT( IN  ) :: sp0,cp0,sl0,cl0, & !Pre-computed sines and cosines
                       cc0,sc0,cs0,ss0
REAL, INTENT( OUT ) :: lat, lon           !Output lat/lon

REAL xp, yp, r2

REAL, DIMENSION(3) :: n, xr, u, e1, e2

e1 = (/-sl0, cl0,0. /)
e2 = (/-sc0,-ss0,cp0/)
n  = (/ cc0, cs0,sp0/)

xp = x/a; yp = y/a
r2 = xp*xp + yp*yp
xr = xp*e1 + yp*e2
u  = ((4.-r2)*n + 4.*xr) / (4.+r2)

lat = ASIN( u(3) ) * R2D
lon = ATAN2(u(2),u(1)) * R2D

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION InitRotLL( lat0,sp0,cp0 ) RESULT( irv )

!------ Pre-compute factors for Rotated Lat/Lon coordinates

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat0              !Rotation in latitude
REAL, INTENT( OUT ) :: sp0, cp0          !Sin,Cos of latitude

!------ Check input

IF( ABS(lat0) > 90. )THEN
  irv = -1
  RETURN
END IF

!------ Save sine and cosine

sp0 = SIN(lat0*D2R)
cp0 = COS(lat0*D2R)

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION LL2RotLL( lat,lon,lon0,sp0,cp0,x,y ) RESULT( irv )

!------ Compute (x,y) [lon,lat] of Rotated Lat/Lon coordinates
!       Assumes InitRotLL has been called to generate sine and cosine terms

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat,  lon !Input lat/lon (degrees)
REAL, INTENT( IN  ) :: lon0      !Rotation in longitude
REAL, INTENT( IN  ) :: sp0,cp0   !Pre-computed sin and cos(lat0)
REAL, INTENT( OUT ) :: x, y      !Rotated coordinates (LON,LAT in degrees)

REAL sl, cl, sp, cp

sl = SIN((lon-lon0) * D2R)
cl = COS((lon-lon0) * D2R)

sp = SIN(lat * D2R)
cp = COS(lat * D2R)

x = ATAN2( cp*sl,cp0*cp*cl+sp0*sp ) * R2D
y = ASIN( cp0*sp - sp0*cp*cl ) * R2D

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION RotLL2LL( x,y,lon0,sp0,cp0,lat,lon ) RESULT( irv )

!------ Compute (lat,lon) corresponding to location (x,y) [lon,lat] in Rotated Lat/Lon coordinates.
!       Assumes InitRotLL has been called to generate sine and cosine terms

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y               !Model coordinates (rotated LON,LAT in degrees)
REAL, INTENT( IN  ) :: lon0               !Rotation in longitude
REAL, INTENT( IN  ) :: sp0,cp0            !Pre-computed sine and cosine
REAL, INTENT( OUT ) :: lat, lon           !Output lat/lon (degrees)

REAL sp, cp, sl, cl, denom

sp = SIN(y * D2R)
cp = COS(y * D2R)
sl = SIN(x * D2R)
cl = COS(x * D2R)

lat = cp0*sp + sp0*cp*cl

denom = (cp*cl-sp0*lat)/cp0
lon = ATAN2( cp*sl,denom ) * R2D + lon0
lat = ASIN( lat ) * R2D

IF( lon > 180. )lon = lon - 360.
IF( lon <-180. )lon = lon + 360.

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION InitMercator( lat1,a,f,m0 ) RESULT( irv )

!------ Pre-compute factors for Mercator Projection

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat1  !Standard (true) latitude
REAL, INTENT( IN  ) :: a     !Radius of sphere
REAL, INTENT( OUT ) :: f
REAL, INTENT( OUT ) :: m0    !for map factor

IF( ABS(lat1) > 89. )THEN
  irv = -1
  RETURN
END IF

m0 = COS(lat1*D2R)
f  = m0 * a

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION LL2Mercator( lat,lon,lon0,f,x,y ) RESULT( irv )

!------ Compute (x,y) of Mercator Projections (
!       Assumes InitMercator has been called to generate f

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: lat,  lon   !Input lat/lon
REAL, INTENT( IN  ) :: lon0        !Reference lon (ref lat = 0)
REAL, INTENT( IN  ) :: f           !Pre-computed projection parameter
REAL, INTENT( OUT ) :: x, y        !Projection coordinates (km)

REAL t

IF( ABS(lat ) > 89. )THEN
  irv = -1
  RETURN
END IF

t = (45. + 0.5*lat)*D2R
t = TAN(t)

x = f*(lon-lon0)*D2R
y = f*LOG(t)

irv = 1

RETURN
END

!==============================================================================

INTEGER FUNCTION Mercator2LL( x,y,lon0,f,lat,lon ) RESULT( irv )

!------ Compute (lat,lon) corresponding to location (x,y) in Mercator Projection
!       Assumes InitMercator has been called to generate f

USE MapConstants_fd

IMPLICIT NONE

REAL, INTENT( IN  ) :: x, y        !Map coordinates (km)
REAL, INTENT( IN  ) :: lon0        !Reference longitude
REAL, INTENT( IN  ) :: f           !Pre-computed projection parameter
REAL, INTENT( OUT ) :: lat,  lon   !Output lat/lon

REAL arg

arg = EXP(y/f)

lon = lon0 + x/f * R2D
lat = (-PI2 + 2.*ATAN(arg)) * R2D

irv = 1

RETURN
END
