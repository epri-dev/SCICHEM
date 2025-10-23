MODULE combine_fd

  TYPE timeStruct
    INTEGER nTimes
    REAL, DIMENSION(:), POINTER :: times
  END TYPE

  TYPE typStruct
    INTEGER nTypes
    INTEGER nDep
    INTEGER, DIMENSION(:), POINTER :: types
  END TYPE

END MODULE combine_fd
