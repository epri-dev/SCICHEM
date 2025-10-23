MODULE MapCoord_fd

  USE domain_fd

!==== MapCoord ==============================================================

  TYPE MapCoord

    SEQUENCE

    INTEGER            :: type
    INTEGER            :: zone       !UTM zone
    TYPE( referenceT ) :: reference
    REAL               :: Lat0, Lon0 !Lat0 = 0 for Mercator
    REAL               :: Lat1, Lat2 !Lat2 required for Lambert Conformal only
    REAL               :: Rearth     !KM
    REAL               :: n          !Lambert Conformal only
    REAL               :: f
    REAL               :: m0
    REAL               :: y0
    REAL               :: sp0, cp0           !sin(Lat0), cos(Lat0)  For Rotated Polar & LL
    REAL               :: sl0, cl0           !sin(Lon0), cos(Lon0)
    REAL               :: cc0, sc0, cs0, ss0 !cp0*cl0, sp0*cl0, cp0*sl0, sp0*sl0

  END TYPE MapCoord

!------ Set size for transfer in read/wrt_prj for basic parameters (others are computed as needed)

  INTEGER, PARAMETER :: SIZE_MapCoordPrjTransfer = SIZE_referenceT + 2*KIND(1) + 5*KIND(1.)
  INTEGER, PARAMETER :: SIZE_MapCoord            = SIZE_MapCoordPrjTransfer + 12*KIND(1.)

END MODULE MapCoord_fd
