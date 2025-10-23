MODULE sagdrw_fd

  TYPE  SAGdrawfld_str
    SEQUENCE
    INTEGER nlev
    INTEGER start
    INTEGER stop
    INTEGER ifld
    LOGICAL log_interp
    LOGICAL lclose
    LOGICAL FILL_lo
    LOGICAL FILL_hi
    LOGICAL DrawContour
    LOGICAL FILLContour
    LOGICAL lAreaCell
    LOGICAL lAreaTri
    LOGICAL lDoThreaded
    REAL,   DIMENSION(:), POINTER :: iplev !contour levels
  END TYPE  SAGdrawfld_str

END MODULE sagdrw_fd
