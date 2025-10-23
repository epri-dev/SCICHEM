MODULE prjstruct_fd

  USE DefSize_fd

!==== projectIDT ==============================================================

  TYPE  projectIDT
    SEQUENCE
    INTEGER                   ID
    INTEGER                   version
    CHARACTER(PATH_MAXLENGTH) name
    CHARACTER(PATH_MAXLENGTH) path
  END TYPE  projectIDT

  INTEGER, PARAMETER :: SIZE_projectIDT = 2*KIND(1) + 2*PATH_MAXLENGTH

END MODULE prjstruct_fd
