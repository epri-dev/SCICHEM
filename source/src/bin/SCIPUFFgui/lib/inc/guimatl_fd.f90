MODULE GUImatl_fd

  USE struct_fd

  TYPE  matMC_str
    SEQUENCE
    CHARACTER(16), DIMENSION(:), POINTER :: name
    CHARACTER(16) units
  END TYPE  matMC_str

  TYPE  matdef_str
    SEQUENCE
    INTEGER nmatl
    INTEGER nmaux
    REAL , DIMENSION(:), POINTER ::  mat_aux
    TYPE ( material_str ), DIMENSION(:), POINTER :: material
    TYPE ( matMC_str ), DIMENSION(:), POINTER :: materialMC
  END TYPE  matdef_str

END MODULE GUImatl_fd
