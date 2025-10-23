MODULE SWIMgridStr_fd

  USE MapCoord_fd

  TYPE SWIMgridStr

    SEQUENCE

    INTEGER :: nx,   ny
    REAL    :: xmin, ymin
    REAL    :: dx,   dy
    LOGICAL :: lter
    REAL    :: Hmin
    REAL    :: delx2
    INTEGER :: basic      ! > 0 indicates parent of smoothed field

    REAL    :: xminPrj, yminPrj
    REAL    :: xmaxPrj, ymaxPrj

    TYPE( MapCoord ) :: coord
    REAL, DIMENSION(:), POINTER :: H, Hx, Hy
    REAL, DIMENSION(:), POINTER :: zi

  END TYPE SWIMgridStr

END MODULE SWIMgridStr_fd
