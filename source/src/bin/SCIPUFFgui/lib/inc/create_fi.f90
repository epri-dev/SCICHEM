MODULE create_fi
!     FORTRAN Include file - Create Project commons

  USE release_gui_fd
  USE reldef_fd
  USE winAPI_fd, ONLY: POINTER_LEN

  SAVE

  INTEGER MAXREL
  INTEGER met_flags
  INTEGER(POINTER_LEN) iwnd_dbr
  INTEGER id_rel,idr_level
  LOGICAL lcreate

  LOGICAL,            DIMENSION(3)    :: lcopy
  CHARACTER(128),     DIMENSION(0:21) :: loadfile
 TYPE( reldef_str ), DIMENSION(-1:2) :: scenario

  TYPE( release_str ) cur_release

END MODULE create_fi
