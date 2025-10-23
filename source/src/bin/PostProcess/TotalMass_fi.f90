MODULE TotalMass_fi

USE tooluser_fd

SAVE

INTEGER                                       :: nMatl
INTEGER                                       :: iMatl
TYPE( materialT  ), DIMENSION(:), ALLOCATABLE :: materials
REAL,               DIMENSION(:), ALLOCATABLE :: airMass
REAL,               DIMENSION(:), ALLOCATABLE :: srfMass

END MODULE TotalMass_fi
