MODULE structure_fd

  USE prjstruct_fd    !Basic project structures
  USE message_fd      !Message/Error structures
  USE timstruct_fd    !Time structures
  USE domstruct_fd    !Domain structures
  USE mtlstruct_fd    !Material structures
  USE inpstruct_fd    !Other input structures
  USE metstruct_fd    !Weather structures
  USE relstruct_fd    !Release structures
  USE update_fd       !Update release and compute effects structures
  USE spcstruct_fd    !Special structures - may be elimiated/expanded in the future
  USE charT_fd
  USE limitT_fd

!==== inputT ==================================================================

  TYPE  inputT
    SEQUENCE
    TYPE ( ctrlT     ) ctrl
    TYPE ( temporalT ) time
    TYPE ( flagsT    ) flags
    TYPE ( spatialT  ) domain
    TYPE ( optionsT  ) option
    TYPE ( listHeadT ) mtlHead
  END TYPE  inputT

  INTEGER, PARAMETER :: SIZE_inputT = SIZE_ctrlT + SIZE_temporalT + SIZE_flagsT + &
                                      SIZE_spatialT + SIZE_optionsT + SIZE_listHeadT


!==== pinputT =================================================================

  TYPE  pinputT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( inputT     ) input
  END TYPE  pinputT

  INTEGER, PARAMETER :: SIZE_pinputT = SIZE_projectIDT + SIZE_inputT

!==== standardInputT ==========================================================

  TYPE  standardInputT
    SEQUENCE
    TYPE ( temporalT ) time
    TYPE ( flagsT    ) flags
    TYPE ( spatialT  ) domain
    TYPE ( optionsT  ) option
    TYPE ( listHeadT ) mtlHead
  END TYPE  standardInputT

!==== pstandardInputT =========================================================

  TYPE  pstandardInputT
    SEQUENCE
    TYPE ( projectIDT     ) project
    TYPE ( standardInputT ) input
  END TYPE  pstandardInputT

!==== restartInputT ===========================================================

  TYPE  restartInputT
    SEQUENCE
    TYPE ( ctrlT     ) ctrl
    TYPE ( endT      ) end
    TYPE ( auditT    ) audit
    TYPE ( spatialT  ) domain
    TYPE ( optionsT  ) option
  END TYPE  restartInputT

!==== prestartInputT ==========================================================

  TYPE  prestartInputT
    SEQUENCE
    TYPE ( projectIDT    ) project
    TYPE ( restartInputT ) input
  END TYPE  prestartInputT

!==== createNewT ==============================================================

  TYPE  createNewT
    SEQUENCE
    TYPE ( projectIDT     ) project
    TYPE ( standardInputT ) input
    TYPE ( weatherT       ) weather
    TYPE ( listHeadT      ) scnHead
  END TYPE  createNewT

!==== createRstT ==============================================================

  TYPE  createRstT
    SEQUENCE
    TYPE ( projectIDT    ) project
    TYPE ( restartInputT ) input
    TYPE ( weatherT      ) weather
  END TYPE  createRstT

!==== projectT ================================================================

  TYPE  projectT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( inputT     ) input
    TYPE ( weatherT   ) weather
    TYPE ( listHeadT  ) scnHead
    TYPE ( statusT    ) current
  END TYPE  projectT

END MODULE structure_fd
