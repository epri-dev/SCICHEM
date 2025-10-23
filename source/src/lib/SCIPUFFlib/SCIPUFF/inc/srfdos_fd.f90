MODULE srfdos_fd

  REAL, PARAMETER :: ARGMAX = 20.0

  TYPE srf_gauss_str

    SEQUENCE
    REAL    axx, axy, ayy
    REAL    xbar, ybar
    REAL    xmap, ymap
    REAL    argmax
    REAL    conc_min
    REAL    pmass
    REAL    cmax
    REAL    xp, yp, zp
    REAL    voli
    REAL    facv
    REAL    h, hx, hy
    REAL    tscale
    REAL    xminS
    REAL    yminS
    REAL    xmaxS
    REAL    ymaxS
    INTEGER gridlev

  END TYPE srf_gauss_str

END MODULE srfdos_fd
