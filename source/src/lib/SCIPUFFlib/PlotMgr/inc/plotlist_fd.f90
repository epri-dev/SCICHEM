MODULE plotlist_fd

  USE time_fd

!==== SCIPClassChoiceT ========================================================

  TYPE SCIPClassChoiceT
    INTEGER available
    INTEGER kind              ! kind subplots available
    INTEGER ikind             ! points to kind string list
    INTEGER nkind             ! no. of kinds available
    INTEGER itime             ! time list id
    INTEGER usertime          ! user-specified time available
    CHARACTER(16) units       ! units string
  END TYPE SCIPClassChoiceT

!==== SCIPCategoryClassT ======================================================

  TYPE SCIPCategoryClassT
    INTEGER available
    INTEGER type              ! type subplot available
  END TYPE SCIPCategoryClassT

!==== SCIPTimeT ===============================================================

  TYPE SCIPTimeT
    SEQUENCE
    TYPE( TimeT ) time
    INTEGER       nItems
    CHARACTER(24) string
  END TYPE SCIPTimeT

END MODULE plotlist_fd
