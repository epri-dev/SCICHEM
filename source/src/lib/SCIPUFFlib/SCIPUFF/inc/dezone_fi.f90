MODULE dezone_fi

  SAVE
  INTEGER, POINTER, DIMENSION(:) :: dezone_type
  INTEGER, POINTER, DIMENSION(:) :: dezone_auxID
  INTEGER                        :: dezone_nvar

  COMMON /dezone_fi_cmn/dezone_type,dezone_auxID,dezone_nvar

  !$OMP THREADPRIVATE(/dezone_fi_cmn/)


END MODULE dezone_fi

