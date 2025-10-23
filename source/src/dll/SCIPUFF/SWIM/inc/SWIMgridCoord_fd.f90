MODULE GridCoord_fd

  TYPE GridCoord
    INTEGER :: UTMzone
    INTEGER :: coord
    REAL    :: Lat0, Lon0
    REAL    :: X0, Y0
    REAL    :: dX, dY
    INTEGER :: nX, nY
  END TYPE GridCoord

END MODULE GridCoord_fd
