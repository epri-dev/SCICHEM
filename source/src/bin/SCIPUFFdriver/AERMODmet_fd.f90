MODULE AERMODmet_fd

  INTEGER, PARAMETER :: MAXMETINP = 1000

  TYPE AERMODmet
    CHARACTER(200) :: file
    INTEGER        :: type
    INTEGER        :: staNum
    CHARACTER(64)  :: name
    INTEGER        :: year
    REAL           :: x, y
    REAL           :: baseElev
  END TYPE AERMODmet

END MODULE AERMODmet_fd
