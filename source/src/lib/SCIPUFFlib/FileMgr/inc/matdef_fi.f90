MODULE matdef_fi

  USE param_fd

  SAVE

!===============================================================================
!     MATDEF material definition namelist components
!===============================================================================
!
!     NOTE - any changes here MUST be reflected in subroutine ReadNamelistMatdef_v02
!            gas_deposition
!
!===============================================================================

  REAL, DIMENSION(3)        :: antoine
  REAL                      :: conc_min
  REAL                      :: decay_amp
  REAL                      :: decay_min
  REAL                      :: density
  REAL                      :: evap_min
  REAL                      :: gas_deposition
  REAL, DIMENSION(2)        :: liquid_density
  REAL                      :: mweight
  REAL, DIMENSION(MAXSGP)   :: binSize
  REAL, DIMENSION(MAXSGP+1) :: binBounds
  REAL                      :: specific_heat_liq
  REAL                      :: specific_heat_vap
  REAL                      :: spread_factor
  REAL                      :: surf_tension
  REAL                      :: viscosity

  CHARACTER(16) class
  CHARACTER(16) mname
  CHARACTER(16) units
  CHARACTER(64) file_name
  CHARACTER(64) file_path

  LOGICAL group_deposition
  LOGICAL group_dose
  LOGICAL multi_comp
  LOGICAL total_deposition
  LOGICAL total_dose

  INTEGER nsg

END MODULE matdef_fi
