MODULE localpuf

  USE tooluser_fd
  USE GetTimes_fi
  USE mcstruct_fd
  USE multcomp_fd

  IMPLICIT NONE

  SAVE

  TYPE( puffT ),     DIMENSION(:), ALLOCATABLE :: puffs
  TYPE( puffTypeT ), DIMENSION(:), ALLOCATABLE :: puffType
  REAL,              DIMENSION(:), ALLOCATABLE :: puffAux
  TYPE( material_MClist ) mcList
  LOGICAL                                          :: hasChem
  TYPE( ChemMC_out    ), DIMENSION(:), ALLOCATABLE :: chemOut

END MODULE localpuf
