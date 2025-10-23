MODULE sagtri_fd

  USE sagnod_fd

  TYPE  SAGtriangle_str
    SEQUENCE
    INTEGER nid(3)
  END TYPE  SAGtriangle_str

  TYPE  SAGconnect_str
    SEQUENCE
    INTEGER tid(3)
  END TYPE  SAGconnect_str

  TYPE  SAGedge_str
    SEQUENCE
    INTEGER nid(2)
    INTEGER tid
    INTEGER sid
  END TYPE  SAGedge_str

  TYPE  SAGedgenode_str
    SEQUENCE
    INTEGER nid
    INTEGER tid
    INTEGER sid
  END TYPE  SAGedgenode_str

  TYPE  SAGtriangleT_str
    SEQUENCE
    INTEGER mxtri
    INTEGER mxedge
    TYPE ( SAGtriangle_str ), POINTER, DIMENSION(:) :: iptri
    TYPE ( SAGconnect_str  ), POINTER, DIMENSION(:) :: ipconn
    TYPE ( SAGedge_str ),     POINTER, DIMENSION(:) :: ipedge
    TYPE ( SAGedgenode_str ), POINTER, DIMENSION(:) :: ipenod
    INTEGER ntri
    INTEGER nedge
    INTEGER nenod
    TYPE ( SAGnodeT_str ) nodeT
    INTEGER build
  END TYPE  SAGtriangleT_str

END MODULE sagtri_fd

