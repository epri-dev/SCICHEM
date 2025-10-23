MODULE dialog_fi

  USE GUIparam_fd
  USE dialog_fd
  USE plotlist_fd
  USE options_fd

  SAVE

  TYPE ( optionsT ),    DIMENSION(-1:2) :: dlgOptions
  TYPE ( TIME_DLG ),    DIMENSION(-1:2) :: dlgTime
  TYPE ( RunAnimateT ), DIMENSION(-1:2) :: rundef
  TYPE ( DOMAIN_DLG ),  DIMENSION(-1:2) :: dlgDomain
  TYPE ( METDEF_DLG ),  DIMENSION(-1:2) :: metdef

  TYPE(SCIPTimeT), DIMENSION(:),     ALLOCATABLE :: timeRestart

  INTEGER nTimeRestart

  INTEGER met_types, met_offset
  INTEGER lsv_types, lsv_offset
  INTEGER bl_types,  bl_offset

END MODULE dialog_fi

MODULE randef
  SAVE
  INTEGER, DIMENSION(4) :: save_flag
END MODULE randef
