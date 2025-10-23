MODULE sagcnt_fd

  TYPE  SAGpoint_str
    SEQUENCE
    REAL    x,y
  END TYPE  SAGpoint_str

  TYPE  SAGcontour_str
    SEQUENCE
    INTEGER mxpts
    INTEGER mxlns
    TYPE( SAGpoint_str ), POINTER, DIMENSION(:) :: ippts
    INTEGER,              POINTER, DIMENSION(:) :: iplns
    INTEGER npts
    INTEGER nlns
    INTEGER start_node
    INTEGER last_node
    INTEGER last_tri
    INTEGER ifld
    INTEGER level
    REAL    value
    LOGICAL log_interp
    LOGICAL lclose
    LOGICAL start
  END TYPE  SAGcontour_str

  TYPE  SAGendpoint_str
    SEQUENCE
    INTEGER, DIMENSION(2) :: sid
    REAL    px1,px2,py1,py2
  END TYPE  SAGendpoint_str

END MODULE sagcnt_fd
