MODULE options_fd

  USE charT_fd

!==== optionsT ================================================================

  TYPE  optionsT
    SEQUENCE
    INTEGER nzBL
    INTEGER mGrd
    INTEGER substrate
    REAL    timeAvg
    REAL    massMin
    REAL    delMin
    REAL    wwTrop
    REAL    epsTrop
    REAL    slTrop
    REAL    uuCalm
    REAL    slCalm
    REAL    zDosage
    REAL    dtSampler
    LOGICAL lOutputVariance
    CHARACTER(PATH_MAXLENGTH) samplerFile
  END TYPE  optionsT

  INTEGER, PARAMETER :: SIZE_optionsT = 3*KIND(1) + 10*KIND(1.) + PATH_MAXLENGTH + KIND(.TRUE.)

END MODULE options_fd
