MODULE Char8Array_fd

  TYPE Char8Array
    INTEGER                             :: n
    CHARACTER(8), DIMENSION(:), POINTER :: string
    CHARACTER(8), DIMENSION(:), POINTER :: unit
  END TYPE Char8Array

END MODULE Char8Array_fd
