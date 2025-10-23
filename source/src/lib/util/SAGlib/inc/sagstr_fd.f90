MODULE sagstr_fd

  USE DefSize_fd
  INTEGER, PARAMETER :: VERSION_DELMIN = 651

  INTEGER, PARAMETER :: SAG_RECL = 128

  INTEGER, PARAMETER :: SAG_HWRDS     = 8
  INTEGER, PARAMETER :: SAG_HEAD_TIME = 1
  INTEGER, PARAMETER :: SAG_HEAD_CELL = 2
  INTEGER, PARAMETER :: SAG_HEAD_NX   = 3
  INTEGER, PARAMETER :: SAG_HEAD_NY   = 4
  INTEGER, PARAMETER :: SAG_HEAD_XMIN = 5
  INTEGER, PARAMETER :: SAG_HEAD_YMIN = 6
  INTEGER, PARAMETER :: SAG_HEAD_DX   = 7
  INTEGER, PARAMETER :: SAG_HEAD_DY   = 8

  TYPE SAGblock_str
    SEQUENCE
    INTEGER ifld          !Starting field number
    INTEGER nfld          !Number of fields in block
    INTEGER type          !Block type
    INTEGER iaux          !Aux data pointer
    CHARACTER(64) name    !Block name
    CHARACTER(4), DIMENSION(:), POINTER :: fldnam !variable name data
  END TYPE SAGblock_str

  TYPE  SAGfield_aux_data  !Surface cell auxiliary data structure
    SEQUENCE
    REAL, DIMENSION(:), POINTER :: data
  END TYPE  SAGfield_aux_data

  TYPE  SAGfield_aux  !Surface block auxiliary data structure
    SEQUENCE
    LOGICAL alloc   !Flag indicating aux data is allocated
    TYPE( SAGfield_aux_data ), DIMENSION(:), POINTER :: srf_data
  END TYPE  SAGfield_aux

  TYPE SAGgrid_str
    SEQUENCE

    INTEGER id               !grid list id
    INTEGER nunit            !Unit number
    INTEGER record           !Current record pointer
    INTEGER status           !Current record type (header,grid,data)
    INTEGER type             !Adaptive grid type (horiz,vert,both)
    INTEGER ftype            !Adaptive grid file version (0 for old, nblk for block version)
    INTEGER pushtype         !Flag for multi-distribution (1 or 2) bottom push

    REAL    time             !Time
    INTEGER ncells           !No. cells
    INTEGER nx               !Primary grid
    INTEGER ny               !Primary grid
    REAL    xmin             !grid origin
    REAL    ymin             !grid origin
    REAL    dx               !grid size
    REAL    dy               !grid size
    INTEGER nblk             !No. data blocks
    INTEGER naux             !No. data blocks with auxiliary data
    INTEGER nvart            !Number of data fields
    INTEGER version          !file version
    INTEGER maxlev           !Max refinement level
    REAL    delmin           !minimum grid size

    INTEGER mxgrd            !Maximum number of grid points allowed
    INTEGER mxfld            !Maximum number of fields allowed
    INTEGER mxnam            !Maximum number of names allowed

    INTEGER PlotType         !Plot field type for plot request - avoid recalculating plot field
    REAL    PlotData         !Plot request data

    INTEGER,              DIMENSION(:), POINTER :: ipgrd !grid data
    REAL,                 DIMENSION(:), POINTER :: ipdat !surface data
    CHARACTER(4),         DIMENSION(:), POINTER :: ipnam !variable name data
    INTEGER,              DIMENSION(:), POINTER :: ipflg !internal flags array
    TYPE( SAGblock_str ), DIMENSION(:), POINTER :: ipblk !internal block data
                                                         !for block versions
    TYPE( SAGfield_aux ), DIMENSION(:), POINTER :: aux   !Auxiliary data

    CHARACTER(PATH_MAXLENGTH)  file      !Associated filename
  END TYPE SAGgrid_str

END MODULE sagstr_fd
