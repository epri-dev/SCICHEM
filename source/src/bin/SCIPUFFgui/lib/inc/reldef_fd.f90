MODULE reldef_fd

  USE create_fd
  USE release_gui_fd

  TYPE  reldef_str
    SEQUENCE
    INTEGER nrel
    TYPE( release_str ), DIMENSION(:), POINTER :: release
  END TYPE  reldef_str

END MODULE reldef_fd
