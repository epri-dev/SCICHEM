MODULE poparea_fd

  USE domain_fd

  INTEGER, PARAMETER :: POP_BON     = 0
  INTEGER, PARAMETER :: POP_BAREA   = 1
  INTEGER, PARAMETER :: POP_BEXPECT = 2
  INTEGER, PARAMETER :: POP_OFF        = 0
  INTEGER, PARAMETER :: POP_ON         = 2**POP_BON      ! (1)
  INTEGER, PARAMETER :: POP_AREA       = 2**POP_BAREA    ! (2)
  INTEGER, PARAMETER :: POP_EXPECT     = 2**POP_BEXPECT  ! (4)

  TYPE GridDefinitionPopInCell
    SEQUENCE
    !Grid information
    REAL(8) xOrigin
    REAL(8) yOrigin
    REAL(8) dx
    REAL(8) dy
    !Coordinate information
    INTEGER coord
    !UTM
    INTEGER zone
    !Cartesion
    TYPE ( referenceD ) reference
    REAL(8) xfac
    REAL(8) yfac
    !flag for area/population
    LOGICAL doPop
  END TYPE GridDefinitionPopInCell

END MODULE poparea_fd

