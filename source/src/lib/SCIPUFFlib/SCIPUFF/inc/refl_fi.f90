MODULE refl_fi

!------ surface reflection common

  SAVE

  REAL(8), DIMENSION(3,3) :: a_rfl, b_rfl, at_rfl

  REAL(8) deth

  COMMON /refl_fi_cmn/ a_rfl, b_rfl, at_rfl, deth
!$OMP THREADPRIVATE(/refl_fi_cmn/)

END MODULE refl_fi
