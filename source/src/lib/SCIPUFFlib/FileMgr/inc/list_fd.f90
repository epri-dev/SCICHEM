MODULE list_fd

!==== listT ===================================================================

  TYPE listHeadT
    SEQUENCE
    INTEGER  max
    INTEGER  number
  END TYPE listHeadT

  INTEGER, PARAMETER :: SIZE_listHeadT = 2*KIND(1)

END MODULE list_fd
