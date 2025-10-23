!*******************************************************************************
!            UnloadDomain
!*******************************************************************************
SUBROUTINE UnloadDomain( spatial )

USE domain_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP Domain structure

IMPLICIT NONE

TYPE( spatialT ), INTENT( IN ) :: spatial

!==== Unload

SELECT CASE( spatial%domain%coord )
  CASE( HD_LATLON )
    lmap = I_LATLON
  CASE( HD_CARTESIAN )
    lmap = I_CARTESIAN
  CASE( HD_UTM )
    lmap = I_UTM
  CASE DEFAULT
    lmap = spatial%domain%coord
END SELECT

utm_zone = spatial%domain%zoneUTM

xmax     = spatial%domain%xMax
xmin     = spatial%domain%xMin
ymax     = spatial%domain%yMax
ymin     = spatial%domain%yMin
zmax     = spatial%domain%zMax

hres     = spatial%domain%hRes
vres     = spatial%domain%vRes

xref     = spatial%reference%x
yref     = spatial%reference%y
lon0     = spatial%reference%lon
lat0     = spatial%reference%lat


RETURN
END
!*******************************************************************************
!            LoadDomain
!*******************************************************************************
SUBROUTINE LoadDomain( spatial )

USE domain_fd
USE scipuff_fi

!     Load an SCIP Domain structure from SCIPUFF commons

IMPLICIT NONE

TYPE( spatialT ), INTENT( OUT ) :: spatial

!==== Load

SELECT CASE( lmap )
  CASE( I_LATLON )
    spatial%domain%coord = HD_LATLON
  CASE( I_CARTESIAN )
    spatial%domain%coord = HD_CARTESIAN
  CASE( I_UTM )
    spatial%domain%coord = HD_UTM
  CASE DEFAULT
    spatial%domain%coord = lmap
END SELECT

spatial%domain%zoneUTM = utm_zone

spatial%domain%xMax    = xmax
spatial%domain%xMin    = xmin
spatial%domain%yMax    = ymax
spatial%domain%yMin    = ymin
spatial%domain%zMax    = zmax

spatial%domain%hRes    = hres
spatial%domain%vRes    = vres

spatial%reference%x    = xref
spatial%reference%y    = yref
spatial%reference%lon  = lon0
spatial%reference%lat  = lat0


RETURN
END
