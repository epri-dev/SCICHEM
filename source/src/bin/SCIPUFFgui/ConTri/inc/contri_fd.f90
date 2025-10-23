MODULE contri_fd

  INTEGER, PARAMETER :: MAXCNT = 40
  INTEGER, PARAMETER :: MAXFUN = 10

  TYPE  cell_str
    SEQUENCE
	  INTEGER id
	  REAL    x, y, hx, hy
	  REAL	  d, drt, drb, dlt, dlb
	  REAL    f, frt, frb, flt, flb
  END TYPE  cell_str

END MODULE contri_fd
