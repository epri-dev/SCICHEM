MODULE sagnod_fd

  TYPE  SAGnode_str
    SEQUENCE
    INTEGER id, lev
    REAL    x, y, hx, hy
  END TYPE  SAGnode_str

  TYPE  SAGnodeT_str
    SEQUENCE
    INTEGER mxcell
    INTEGER mxnode
    INTEGER mxdata
    INTEGER nnode
    INTEGER ncell
    INTEGER,             POINTER, DIMENSION(:) :: ipcell
    TYPE( SAGnode_str ), POINTER, DIMENSION(:) :: ipnode
    REAL,                POINTER, DIMENSION(:) :: ipdata
  END TYPE  SAGnodeT_str

END MODULE sagnod_fd

