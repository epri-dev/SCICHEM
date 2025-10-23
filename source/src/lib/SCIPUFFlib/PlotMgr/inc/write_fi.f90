MODULE write_fi

  USE write_fd
  USE field_fd
  USE param_fd

  SAVE

  INTEGER WriteMode
  TYPE( SCIPFieldCoordinateT )  Coordinate

END MODULE write_fi

MODULE write_noFile

  USE field_fd

  SAVE

  INTEGER :: numNodes
  INTEGER :: numTriangles
  TYPE( SCIPPlotFieldNodeT ),     DIMENSION(:), ALLOCATABLE :: nfNode  !Plot definition
  TYPE( SCIPPlotFieldTriangleT ), DIMENSION(:), ALLOCATABLE :: nfTri   !Plot definition

END MODULE write_noFile


