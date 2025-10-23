!=======================================================================
!    Search - SCIP 4.0 multiple directory search
!=======================================================================
MODULE search_fd

  INTEGER,PARAMETER :: LOCAL_DRIVE   =  0
  INTEGER,PARAMETER :: NETWORK_DRIVE =  1
  INTEGER,PARAMETER :: CDROM_DRIVE   = -1


  TYPE SearchPathT
    SEQUENCE
    CHARACTER(128) path
    INTEGER        driveType
    LOGICAL        valid
  END TYPE SearchPathT

END MODULE search_fd
