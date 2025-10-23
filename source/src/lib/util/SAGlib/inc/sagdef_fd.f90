MODULE sagdef_fd

!==== SAG return codes

INTEGER, PARAMETER :: SAG_EOF    = -1
INTEGER, PARAMETER :: SAG_OK     =  0
INTEGER, PARAMETER :: SAG_ERROR  =  1

!==== SAG grid types

INTEGER, PARAMETER :: SAG_GRID_BOTH =  0
INTEGER, PARAMETER :: SAG_GRID_HORZ =  1
INTEGER, PARAMETER :: SAG_GRID_VERT =  2
INTEGER, PARAMETER :: SAG_GRID_NONE =  3

!==== SAG grid status

INTEGER, PARAMETER :: SAG_GRID_FULL =  0
INTEGER, PARAMETER :: SAG_GRID_BOTM =  1

!==== SAG copy flag

INTEGER, PARAMETER :: SAG_COPY_NULL  =  0 !Header data only
INTEGER, PARAMETER :: SAG_COPY_FULL  =  1 !Exact copy including allocated sizes, names and blocks
INTEGER, PARAMETER :: SAG_COPY_GRID  =  2 !Header data and grid
INTEGER, PARAMETER :: SAG_COPY_DATA  =  3 !Header data, grid and all fields

INTEGER, PARAMETER :: SAG_COPY_FIELD =  10000 !Individual fields

INTEGER, PARAMETER :: SAG_TO_FIELD   =  1000

END MODULE sagdef_fd

