MODULE aerosol_fi

  USE constants_fd

  SAVE

! --- Local variables for aerosol routines

  REAL, PARAMETER :: CPAIR = CP
  REAL, PARAMETER :: CPWL  = 4200.
  REAL, PARAMETER :: CPWV  = 2000.

  REAL, PARAMETER :: A_WATER  = 11.344
  REAL, PARAMETER :: B_WATER  = 3720.34
  REAL, PARAMETER :: C_WATER  = -45.00
  REAL, PARAMETER :: TB_WATER = 373.15
  REAL, PARAMETER :: LWATER   = 2.50E6

  REAL, PARAMETER :: TZERO = -ABSZERO

  REAL mc, mcl, mcv, mw, mwl, mwv, mda
  REAL cbar, cbarn, cpl, cpv
  REAL Lc, Lw, Lct, Lwt, MWc, Ac, Bc, Cc, Tcb, Twb, Tc_min, Tw_min
  REAL temp, pamb

  COMMON /aerosol_fi_cmn/ mc, mcl, mcv, mw, mwl, mwv, mda
  COMMON /aerosol_fi_cmn/ cbar, cbarn, cpl, cpv
  COMMON /aerosol_fi_cmn/ Lc, Lw, Lct, Lwt, MWc, Ac, Bc, Cc, Tcb, Twb, Tc_min, Tw_min
  COMMON /aerosol_fi_cmn/ temp, pamb

!$OMP THREADPRIVATE(/aerosol_fi_cmn/)


END MODULE aerosol_fi
