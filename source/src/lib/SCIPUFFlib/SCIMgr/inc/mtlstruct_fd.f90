MODULE mtlstruct_fd

  USE material_fd     !Basic material structures
  USE prjstruct_fd    !Basic project structures
  USE list_fd         !Basic list structures

  IMPLICIT NONE

!==== mtlControlT =============================================================

  TYPE mtlControlT
    SEQUENCE
    INTEGER       mode
    CHARACTER(8)  fileExtension
    CHARACTER(16) searchName
  END TYPE mtlControlT

  INTEGER, PARAMETER :: SIZE_mtlControlT = KIND(1) + 24

!==== pmaterialT ==============================================================

  TYPE pmaterialT
    SEQUENCE
    TYPE( projectIDT  ) project
    TYPE( listHeadT   ) mtlHead
    TYPE( mtlControlT ) control
  END TYPE pmaterialT

  INTEGER, PARAMETER :: SIZE_pmaterialT = SIZE_projectIDT + SIZE_listHeadT  + &
                                          SIZE_mtlControlT

END MODULE mtlstruct_fd
