MODULE slice_fd

  INTEGER, PARAMETER :: SD_HEIGHT   = 1
  INTEGER, PARAMETER :: SD_ZMIN     = 1
  INTEGER, PARAMETER :: SD_ZMAX     = 2
  INTEGER, PARAMETER :: SD_ZRES     = 3
  INTEGER, PARAMETER :: SD_BASEGRID = 4

  TYPE slice_str
    SEQUENCE
    INTEGER :: cat              ! plot category
    INTEGER :: nblk             ! number of blocks
    INTEGER :: nfld             ! number of fields
    REAL    :: time             ! plot timebreak
    REAL    :: xmin, ymin       ! slice corner point (lower left)
    REAL    :: xmax, ymax       !  "       "     "   (upper right)
    INTEGER :: maxlev           ! max. refinement level
    INTEGER :: maxcell          ! max. number of cells
    REAL    :: xminS, yminS     ! slice corner points for mask subdomain (lower left)
    REAL    :: xmaxS, ymaxS     !  "       "     "     "    "       "     (upper right)
    REAL, DIMENSION(10) :: data ! supplementary slice data
  END TYPE slice_str

END MODULE slice_fd

