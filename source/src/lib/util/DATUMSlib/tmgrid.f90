SUBROUTINE tmgrid( FI,LAM,NORTH,EAST,CONV,KP,ER,ESQ,EPS,CM, &
                      FE,FN,SF,SO,R,A,B,C,U,V,W )

IMPLICIT DOUBLE PRECISION(A-H,K-Z)

!*****  TRANSVERSE MERCATOR PROJECTION
!       CONVERSION OF GEODETIC COORDINATES TO GRID COORDINATES
!*****  Programmed by T. Vincenty, NGS, in July 1984.
!*****************  SYMBOLS AND DEFINITIONS *************************
!   Latitude positive north, longitude positive west.  All angles are
!     in radian measure.
!   N, E are northing and easting coordinates respectively.
!   LAT, LON are latitude and longitude respectively.
!   CONV is convergence.
!   KP is point scale factor.
!   ER is equatorial radius of the ellipsoid (= major semiaxis).
!   ESQ is the square of first eccentricity of the ellipsoid.
!   EPS is the square of second eccentricity of the ellipsoid.
!   CM is the central meridian of the projection zone.
!   FE is false easting value at the central meridian.
!   FN is "false northing" at the southernmost latitude, usually zero.
!   SF is scale factor at the central meridian.
!   SO is meridional distance (multiplied by the scale factor) from
!     the equator to the southernmost parallel of latitude for the zone.
!   R is the radius of the rectifying sphere (used for computing
!     meridional distance from latitude and vice versa).
!   A, B, C, U, V, W are other precomputed constants for determination
!     of meridional distance from latitude and vice versa.
!
!   The formula used in this subroutine gives geodetic accuracy within
!   zones of 7 degrees in east-west extent.  Within State transverse
!   Mercator projection zones, several minor terms of the equations
!   may be omitted (see a separate NGS publication).  If programmed
!   in full, the subroutine can be used for computations in surveys
!   extending over two zones.
!
!*********************************************************************

OM=FI + A*SIN(2.*FI) + B*SIN(4.*FI) + C*SIN(6.*FI)
S=R*OM*SF
SINFI=SIN(FI)
COSFI=COS(FI)
TN=SINFI/COSFI
TS=TN**2
ETS=EPS*COSFI**2
L=(LAM-CM)*COSFI
LS=L*L
RN=SF*ER/SQRT(1.-ESQ*SINFI**2)

A2=RN*TN/2.
A4=(5.-TS+ETS*(9.+4.*ETS))/12.
A6=(61.+TS*(TS-58.)+ETS*(270.-330.*TS))/360.
A1=-RN
A3=(1.-TS+ETS)/6.
A5=(5.+TS*(TS-18.)+ETS*(14.-58.*TS))/120.
A7=(61.-479.*TS+179.*TS**2-TS**3)/5040.
NORTH=S-SO + A2*LS*(1.+LS*(A4+A6*LS)) - FN
EAST=FE + A1*L*(1.+ LS*(A3+LS*(A5+A7*LS)))

IF(NORTH < 0.0) THEN
 NORTH = NORTH * (-1.0D0)
ENDIF

!*** CONVERGENCE
C1=-TN
C3=(1.+3.*ETS+2.*ETS**2)/3.
C5=(2.-TS)/15.
CONV=C1*L*(1.+LS*(C3+C5*LS))

!*** POINT SCALE FACTOR
F2=(1.+ETS)/2.
F4=(5.-4.*TS+ETS*( 9.-24.*TS))/12.
KP=SF*(1.+F2*LS*(1.+F4*LS))

RETURN
END

