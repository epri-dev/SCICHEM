MODULE plotmet_fi

  USE plotmet_fd

  SAVE

  REAL, PARAMETER :: SCALE_HEIGHT = 8000.  !Used to convert sigma-pressure to height

  LOGICAL lformat
  INTEGER n2Dchoice

  INTEGER, DIMENSION(:), ALLOCATABLE :: nMet2D, nMet3D

  CHARACTER(8), DIMENSION(:,:), ALLOCATABLE :: MetVar2D, MetVar3D
  CHARACTER(8), DIMENSION(:,:), ALLOCATABLE :: MetUnit2D, MetUnit3D

  TYPE( metGridT ), DIMENSION(:), POINTER :: pMetGrid

  INTEGER, DIMENSION(:), ALLOCATABLE :: istgMet

END MODULE plotmet_fi
