MODULE pointval_fi

  SAVE

  REAL x0, y0
  REAL xR, yT
  REAL xL, yB
  REAL xp, yp

  COMMON / common_pointval_fi / x0, y0
  COMMON / common_pointval_fi / xR, yT
  COMMON / common_pointval_fi / xL, yB
  COMMON / common_pointval_fi / xp, yp
!$OMP THREADPRIVATE(/common_pointval_fi/)

END MODULE pointval_fi
