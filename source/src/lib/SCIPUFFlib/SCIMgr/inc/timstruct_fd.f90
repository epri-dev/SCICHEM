MODULE timstruct_fd

  USE time_fd         !Basic time structures
  USE prjstruct_fd    !Basic project structures

!==== pstartT =================================================================

  TYPE  pstartT
    SEQUENCE
    TYPE( projectIDT ) project
    TYPE( startT     ) start
  END TYPE  pstartT

  INTEGER, PARAMETER :: SIZE_pstartT = SIZE_projectIDT + SIZE_startT

!==== pendT ===================================================================

  TYPE  pendT
    SEQUENCE
    TYPE( projectIDT ) project
    TYPE( endT       ) end
  END TYPE  pendT

  INTEGER, PARAMETER :: SIZE_pendT = SIZE_projectIDT + SIZE_endT

!==== pctrlT ==================================================================

  TYPE  pctrlT
    SEQUENCE
    TYPE( projectIDT ) project
    TYPE( ctrlT      ) ctrl
  END TYPE  pctrlT

  INTEGER, PARAMETER :: SIZE_pctrlT = SIZE_projectIDT + SIZE_ctrlT

!==== ptemporalT ==============================================================

  TYPE  ptemporalT
    SEQUENCE
    TYPE( projectIDT ) project
    TYPE( temporalT  ) time
  END TYPE  ptemporalT

  INTEGER, PARAMETER :: SIZE_ptemporalT = SIZE_projectIDT + SIZE_temporalT

END MODULE timstruct_fd
