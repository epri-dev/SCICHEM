MODULE animate_fi

  USE DefSize_fd
  USE winAPI_fd, ONLY: POINTER_LEN

  INTEGER, PARAMETER :: MAX_PAUSE  = 10000 ! 10 sec pause between frames
  INTEGER, PARAMETER :: PAGE_PAUSE = 1000  ! 1 sec jump per scroll page
  INTEGER, PARAMETER :: LINE_PAUSE = 100   ! 1/10 sec jump per scroll line
  INTEGER, PARAMETER :: INIT_PAUSE = 100   ! 1/10 sec pause between frames

  INTEGER, PARAMETER :: MAX_SIZE = 960

  SAVE

  LOGICAL lanimate,lanim_cont,lanim_file
  INTEGER start_frame,end_frame,current_frame,inc_frame
  CHARACTER(PATH_MAXLENGTH) file_animate,path_animate
  INTEGER(POINTER_LEN) AnimDIB,AnimBmap,AnimPal
  INTEGER bitmap_type

END MODULE animate_fi
