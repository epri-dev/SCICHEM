MODULE inpstruct_fd

  USE flags_fd        !Basic flags structures
  USE options_fd      !Basic options structures
  USE list_fd         !Basic list structures
  USE prjstruct_fd    !Basic project structures

!==== pauditT ==================================================================

  TYPE pauditT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( auditT     ) audit
  END TYPE  pauditT

!==== pflagT ==================================================================

  TYPE pflagsT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( flagsT     ) flags
  END TYPE  pflagsT

  INTEGER, PARAMETER :: SIZE_pflagsT = SIZE_projectIDT + SIZE_flagsT

!==== poptionsT ===============================================================

  TYPE poptionsT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( optionsT   ) option
  END TYPE  poptionsT

  INTEGER, PARAMETER :: SIZE_poptionsT = SIZE_projectIDT + SIZE_optionsT

END MODULE inpstruct_fd
