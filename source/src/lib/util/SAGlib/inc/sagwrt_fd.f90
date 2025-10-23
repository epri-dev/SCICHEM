MODULE sagwrt_fd

  TYPE  SAGwritefld_str
    SEQUENCE
    INTEGER start
    INTEGER stop
    INTEGER nlev
    INTEGER ifld
    LOGICAL log_interp
    LOGICAL lclose
    LOGICAL lcontour
    LOGICAL lLatLon
    INTEGER lun
    INTEGER nheader
    REAL,          DIMENSION(:), POINTER :: iplev !contour levels
    CHARACTER(80), DIMENSION(:), POINTER :: iphdr !contour level headers
  END TYPE  SAGwritefld_str

END MODULE sagwrt_fd
