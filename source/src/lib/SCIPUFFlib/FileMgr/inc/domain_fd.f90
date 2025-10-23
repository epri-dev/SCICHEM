MODULE domain_fd

  USE domainCoord_fd

!==== referenceT ==============================================================

  TYPE  referenceT
    SEQUENCE
    REAL  x
    REAL  y
    REAL  lat
    REAL  lon
  END TYPE  referenceT

  TYPE  referenceD
    SEQUENCE
    REAL(8)  x
    REAL(8)  y
    REAL(8)  lat
    REAL(8)  lon
  END TYPE  referenceD

  INTEGER, PARAMETER :: SIZE_referenceT = 4*KIND(1.)

!==== domainT =================================================================

  TYPE  domainT
    SEQUENCE
    INTEGER coord
    INTEGER zoneUTM
    REAL    xMax
    REAL    xMin
    REAL    yMax
    REAL    yMin
    REAL    zMax
    REAL    hRes
    REAL    vRes
  END TYPE  domainT

  INTEGER, PARAMETER :: SIZE_domainT = 2*KIND(1) + 7*KIND(1.)

!==== spatialT ================================================================

  TYPE  spatialT
    SEQUENCE
    TYPE( domainT    ) domain
    TYPE( referenceT ) reference
  END TYPE  spatialT

  INTEGER, PARAMETER :: SIZE_spatialT = SIZE_domainT + SIZE_referenceT

END MODULE domain_fd
