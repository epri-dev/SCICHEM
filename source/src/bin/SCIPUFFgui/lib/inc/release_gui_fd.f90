MODULE release_gui_fd

  USE create_fd
  USE DefSize_fd

  INTEGER, PARAMETER :: NUM_DYNAMIC = 4

  TYPE  release_str
    SEQUENCE
    LOGICAL     defName
    INTEGER     spec
    INTEGER     indx
    INTEGER     distrib
    REAL        time
    REAL        dur
    REAL        rate
    REAL(8)     xRel
    REAL(8)     yRel
    REAL        zRel
    REAL        sig(3)
    REAL        vel(3)
    REAL        param(MAXRELPARAM)
    REAL        dynam(NUM_DYNAMIC)
    REAL, DIMENSION(:), POINTER :: mc
    CHARACTER(4)   type
    CHARACTER(16)  matl
    CHARACTER(128) matlfile
    CHARACTER(128) matlpath
    CHARACTER(128) string
    CHARACTER(PATH_MAXLENGTH) file
    CHARACTER(PATH_MAXLENGTH) path

  END TYPE  release_str


  TYPE GUI_dryFrac
    INTEGER id
    REAL    dry
  END TYPE

END MODULE release_gui_fd
