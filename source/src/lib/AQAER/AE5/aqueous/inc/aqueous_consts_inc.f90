!----------------------------------------------------------------------
! CMAQ 4.7.1 version, PK, ENVIRON, February 2012
!----------------------------------------------------------------------

module aqueous_consts_inc

use aero_consts_inc

!
! Define additional constants for aqueous-phase module
!
!
! Molar volume at STP [ L/mol ] Non MKS units
      REAL, PARAMETER :: MOLVOL = 22.41410

! Standard Temperature [ K ]
      REAL, PARAMETER :: STDTEMP = 273.15

! density of water at 20 C and 1 ATM (kg/m3)
      REAL, PARAMETER :: H2ODENS = 1000.0

! minimum and maximum pH
      REAL, PARAMETER :: PHMIN = 0.0001
      REAL, PARAMETER :: PHMAX = 10.0

end module aqueous_consts_inc
