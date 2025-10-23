MODULE update_fd

  USE release_fd
  USE environment_fd  !Enviroment structures
  USE basic_fd
!==== updateRelT ==============================================================

  TYPE  updateRelT
    SEQUENCE
    INTEGER               mode
    REAL                  currentTime
    REAL                  nextUpdate
    TYPE ( releaseT     ) release
    TYPE ( environmentT ) environment
  END TYPE  updateRelT

!==== updateRelMCT ==============================================================

  TYPE  updateRelMCT
    SEQUENCE
    INTEGER               mode
    REAL                  currentTime
    REAL                  nextUpdate
    TYPE ( releaseT     ) release
    TYPE ( environmentT ) environment
    CHARACTER(PATH_MAXLENGTH) updateSCN
  END TYPE  updateRelMCT

!==== computeEffT ==============================================================

  TYPE  computeEffT
    SEQUENCE
    INTEGER               incidentID
    INTEGER               effectID
    INTEGER               request
    INTEGER(LEN_ADDRESS)  addressIn
    INTEGER(LEN_ADDRESS)  addressOut
  END TYPE  computeEffT

END MODULE update_fd
