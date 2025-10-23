MODULE saggrd_fi

!==== Gradient commons

  SAVE

  INTEGER, DIMENSION(:), POINTER :: pgrd               !grid data
  REAL,    DIMENSION(:), POINTER :: grt, grb, glt, glb !gradient data
  REAL,    DIMENSION(:), POINTER :: pdat               !grid data

  REAL,    DIMENSION(:),   POINTER :: pdat2                  !grid data for field-2 (variance)
  REAL,    DIMENSION(:,:), POINTER :: grt2, grb2, glt2, glb2 !gradient data for 2-component push

  INTEGER nx
  INTEGER ny

  LOGICAL UseSpecial

  REAL    Special
  REAL    Defval

END MODULE saggrd_fi
