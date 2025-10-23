MODULE terrain_fd

  USE charT_fd

!==== SCIP mass-consistency types =============================================

  INTEGER, PARAMETER :: HTB_SCIPUFF  = 1
  INTEGER, PARAMETER :: HTB_AVAIL_T  =  9
  INTEGER, PARAMETER :: HTB_USE_T    = 10
  INTEGER, PARAMETER :: HTB_AVAIL_L  = 13
  INTEGER, PARAMETER :: HTB_USE_L    = 14
  INTEGER, PARAMETER :: HTB_CATEGORY = 15
  INTEGER, PARAMETER :: HT_NONE      = 0
  INTEGER, PARAMETER :: HT_SCIPUFF   = 2**HTB_SCIPUFF
  INTEGER, PARAMETER :: HT_AVAIL_T   = 2**HTB_AVAIL_T
  INTEGER, PARAMETER :: HT_USE_T     = 2**HTB_USE_T
  INTEGER, PARAMETER :: HT_AVAIL_L   = 2**HTB_AVAIL_L
  INTEGER, PARAMETER :: HT_USE_L     = 2**HTB_USE_L
  INTEGER, PARAMETER :: HT_CATEGORY  = 2**HTB_CATEGORY

!==== SCIP met output types ===================================================

  INTEGER, PARAMETER :: HOB_OUTPUT = 0
  INTEGER, PARAMETER :: HOB_OUTMET = 1
  INTEGER, PARAMETER :: HOB_ASCII  = 2
  INTEGER, PARAMETER :: HOB_2D     = 3
  INTEGER, PARAMETER :: HOB_3D     = 4
  INTEGER, PARAMETER :: HO_OFF    = 0
  INTEGER, PARAMETER :: HO_OUTPUT = 2**HOB_OUTPUT
  INTEGER, PARAMETER :: HO_OUTMET = 2**HOB_OUTMET
  INTEGER, PARAMETER :: HO_ASCII  = 2**HOB_ASCII
  INTEGER, PARAMETER :: HO_2D     = 2**HOB_2D
  INTEGER, PARAMETER :: HO_3D     = 2**HOB_3D

!==== terrainMCT ==============================================================

  INTEGER, PARAMETER :: HS_MAXZB = 50

  TYPE terrainMCT
    SEQUENCE
    REAL                      :: notUsed
    INTEGER, DIMENSION(2)     :: maxIter
    REAL,    DIMENSION(2)     :: eps
    REAL,    DIMENSION(2)     :: alpha
    INTEGER                   :: nz
    REAL, DIMENSION(HS_MAXZB) :: z
  END TYPE terrainMCT

  INTEGER, PARAMETER :: SIZE_terrainMCT = 3*KIND(1) + (5+HS_MAXZB)*KIND(1.)

!==== terrainT

  TYPE terrainT
    SEQUENCE
    INTEGER                    type
    TYPE( terrainMCT )         mc
    CHARACTER(PATH_MAXLENGTH)  file
  END TYPE terrainT

  INTEGER, PARAMETER :: SIZE_terrainT = KIND(1) + SIZE_terrainMCT + PATH_MAXLENGTH

END MODULE terrain_fd
