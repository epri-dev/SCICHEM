!==== charXXT =================================================================

MODULE charT_fd

  USE DefSize_fd

  TYPE fileNameT
    SEQUENCE
    CHARACTER(PATH_MAXLENGTH) string
  END TYPE fileNameT

  TYPE char16T
    SEQUENCE
    CHARACTER(16) string
  END TYPE char16T

  TYPE char32T
    SEQUENCE
    CHARACTER(32) string
  END TYPE char32T

  TYPE char64T
    SEQUENCE
    CHARACTER(64) string
  END TYPE char64T

  TYPE char128T
    SEQUENCE
    CHARACTER(128) string
  END TYPE char128T

END MODULE charT_fd
