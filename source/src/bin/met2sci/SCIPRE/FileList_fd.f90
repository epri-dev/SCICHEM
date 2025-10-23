MODULE fileList_fd

  USE MaxChar_fd

  TYPE fileList

    CHARACTER(MAXC) :: name
    CHARACTER(8)    :: type
    CHARACTER(16)   :: ID
    CHARACTER(16)   :: landuse
    CHARACTER(8)    :: clat
    CHARACTER(8)    :: clon
    REAL            :: lat
    REAL            :: lon
    REAL            :: zref
    REAL            :: elev
    INTEGER         :: tAdj

    TYPE( fileList ), POINTER :: next

  END TYPE fileList

  TYPE firstFile

    INTEGER nFile

    TYPE( fileList ), POINTER :: first

  END TYPE firstFile

END MODULE fileList_fd
  