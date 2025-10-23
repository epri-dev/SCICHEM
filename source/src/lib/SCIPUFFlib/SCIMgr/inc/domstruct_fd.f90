MODULE domstruct_fd

  USE domain_fd       !Basic domain structures
  USE prjstruct_fd    !Basic project structures

!==== pspatialT ===============================================================

  TYPE pspatialT
    SEQUENCE
    TYPE ( projectIDT ) project
    TYPE ( spatialT   ) spatial
  END TYPE  pspatialT

  INTEGER, PARAMETER :: SIZE_pspatialT = SIZE_projectIDT + SIZE_spatialT

END MODULE domstruct_fd
