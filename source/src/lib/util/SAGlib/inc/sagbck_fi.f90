MODULE sagbck_fi

  SAVE

  INTEGER nBackUp                               !no. of backup fields
  REAL,    DIMENSION(:,:), ALLOCATABLE :: Bdata !backup data
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: Bflag !backup flags array

END MODULE sagbck_fi
