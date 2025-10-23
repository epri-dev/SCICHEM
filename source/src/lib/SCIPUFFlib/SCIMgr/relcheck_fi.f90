MODULE ReleaseCheck_fi

! Used to check release structures after updates or from interactive

  USE material_fd
  USE list_fd
  SAVE
  TYPE ( listHeadT ) mtlHead
  TYPE ( materialT ),DIMENSION(:),ALLOCATABLE :: mtlList

END MODULE ReleaseCheck_fi
