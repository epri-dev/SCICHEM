MODULE sagfil_fd

  USE sagnod_fd

  REAL, PARAMETER :: LOGMIN = 1.E-30
  REAL, PARAMETER :: MAXFAC = 10.

  INTEGER, PARAMETER :: MAXPOLY = 6

  TYPE  SAGfillnode_str
    SEQUENCE
    LOGICAL UseNode
    REAL    value
    TYPE( SAGnode_str ) node
    TYPE( SAGnode_str ), DIMENSION(2) :: point
  END TYPE  SAGfillnode_str

  TYPE  SAGfillside_str
    SEQUENCE
    LOGICAL UseNode
    INTEGER, DIMENSION(2) :: node_id
    TYPE( SAGnode_str ), DIMENSION(2) :: point
  END TYPE  SAGfillside_str

END MODULE sagfil_fd
