MODULE UpdateRelList_fd

  USE DefSize_fd

  TYPE UpdateRelList
    CHARACTER(64) ID
    INTEGER       nData
    REAL          t, tNext
    REAL          rateNext, rate
    REAL          buoy, buoyNext
    REAL          wmom, umom, vmom
    REAL          wmomNext, umomNext, vmomNext
    REAL          rateScale
    REAL          buoyScale
    REAL          wmomScale, umomScale, vmomScale
    INTEGER       fileUnit
    CHARACTER(PATH_MAXLENGTH) :: name
    TYPE( UpdateRelList ), POINTER :: next
  END TYPE UpdateRelList

  TYPE FirstRel
    TYPE( UpdateRelList), POINTER :: Rel
  END TYPE FirstRel

END MODULE UpdateRelList_fd
