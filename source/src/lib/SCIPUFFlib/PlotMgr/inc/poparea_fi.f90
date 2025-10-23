MODULE poparea_fi

  USE poparea_fd
  USE field_fd
  USE param_fd

  SAVE

  INTEGER nlev

  REAL(8), DIMENSION(:), ALLOCATABLE :: PopArea
  REAL,    DIMENSION(:), ALLOCATABLE :: level
  REAL popScale

  TYPE( GridDefinitionPopInCell ) :: gridDef

END MODULE poparea_fi


