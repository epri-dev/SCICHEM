MODULE srfevap_fi

  USE struct_fd

! --- Local variables for SRF_EVAP routines

  SAVE

  INTEGER, PARAMETER :: MAXLEV_EVAP = 30

  REAL    xmap_evap, ymap_evap, ustr, ubsrf, vbsrf, tsrf, zisrf, hflxsrf, z0
  REAL    evap_min, del_evap, del_haz, facu_evap, facu_haz, dx_evap

  REAL    dfac, rhod, difd, cs, darea
  REAL    rate, rate_a, rate_d, rate_s

  INTEGER ityp_evap


  INTEGER, DIMENSION(0:MAXLEV_EVAP) :: lev_evap

  LOGICAL found_subcell, srf_met

  TYPE( liquid_material ) pmatl

  COMMON /srfevap_fi_cmn/ xmap_evap, ymap_evap, ustr, ubsrf, vbsrf, tsrf, zisrf, hflxsrf, z0
  COMMON /srfevap_fi_cmn/ dfac, rhod, difd, cs, darea
  COMMON /srfevap_fi_cmn/ rate, rate_a, rate_d, rate_s
  COMMON /srfevap_fi_cmn/ found_subcell, srf_met

!$OMP THREADPRIVATE(/srfevap_fi_cmn/)

END MODULE srfevap_fi

MODULE srfevap2D_fd

!---- Split ratio settings

  REAL, PARAMETER       :: awetRatioMax    = 0.95
  REAL, PARAMETER       :: mwDiam2RatioMin = 1.0001

!---- NewtonRaphson solution interval

  REAL,    PARAMETER    :: NR_X_FROM      = 0.0
  REAL,    PARAMETER    :: NR_X_TO        = 1.0
  REAL,    PARAMETER    :: NR_STEP        = 0.1
  INTEGER, PARAMETER    :: NR_MAXITER     = 1000
  REAL,    PARAMETER    :: NR_PRECISION   = 0.001

!---- Newton Raphson results

  INTEGER, PARAMETER    :: NR_SUCCESS           = 1
  INTEGER, PARAMETER    :: NR_TOO_SMALL_SLOPE   = 2
  INTEGER, PARAMETER    :: NR_DOES_NOT_CONVERGE = 3

  TYPE var2Distrib
    SEQUENCE
    LOGICAL is2D                               !true if conditions are met, default false
    REAL awetRatio                             !ratio r=awet(diam)/awet
    REAL d2Ratio2                              !ratio s^2 = average(diam^2)/ (average(diam)^2)
    REAL alpha, beta, gamma                    !distribution coefficients
    REAL dmass1, dmass2                        !liquid mass
    REAL diam1, diam2                          !surf drop diameter
    REAL awet1, awet2                          !wetted area
    REAL surf_mass1, surf_mass2                !surface mass
    REAL rate1, rate2, rate_a1, rate_a2        !rates
    REAL rate_s1, rate_s2, rate_d1, rate_d2    !rates
    REAL depth1, depth2						             !frontal depth - void above the drop in substrate
    REAL dropH, dropH1, dropH2			        	 !distance from bottom of the drop to the surface
  END TYPE var2Distrib

   END MODULE srfevap2D_fd

MODULE substrate_fi

! --- Surface absorption/desorption variables

  SAVE

  REAL tortuosity, porosity, grain_size
  REAL k_substrate

END MODULE substrate_fi

