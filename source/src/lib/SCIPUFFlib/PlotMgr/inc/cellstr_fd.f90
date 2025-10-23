MODULE cellstr_fd

  REAL, PARAMETER :: EPS = 1.0E-4

  TYPE  getp_cell_str
    SEQUENCE
    INTEGER id, lev
    REAL    x, y, hx, hy
    REAL, DIMENSION(:), POINTER :: f
  END TYPE  getp_cell_str

END MODULE cellstr_fd
