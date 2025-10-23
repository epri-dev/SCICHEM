SUBROUTINE mapfac( x,y,xmap,ymap )

USE scipuff_fi
USE SWIMparam_fd

!      Horizontal coordinate transform function
!               (x,y) are puff coordinates
!               If dx is in meters then  dx*xmap is in puff coords

IMPLICIT NONE

REAL, INTENT( IN )  :: x, y
REAL, INTENT( OUT ) :: xmap, ymap

REAL ytem

SELECT CASE( lmap )

  CASE( I_LATLON )

     ytem = MIN( ABS(y),POLARCAP_LAT )

     xmap = SPHFACR/COS(ytem*PI180)
     ymap = SPHFACR

  CASE( I_METERS)

     xmap = 1.
     ymap = 1.

  CASE DEFAULT ! (I_CARTESIAN)

     xmap = 1.0E-3
     ymap = 1.0E-3

END SELECT

RETURN
END
