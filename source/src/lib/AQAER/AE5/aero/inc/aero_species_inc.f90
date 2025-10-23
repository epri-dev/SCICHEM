!----------------------------------------------------------------------
! CMAQ 4.7.1 Compatible version
! PK, ENVIRON, Feb 2012
!----------------------------------------------------------------------

module aero_species_inc

use aero_consts_inc

implicit none

save

! number of species (gas + aerosol) includes 2nd, 3rd Moments & 5 gases
! (from aero_driver.F)
INTEGER, PARAMETER :: N_AE_SPC = 53 ! For AE5 module
INTEGER, PARAMETER :: N_AE_SPCD = N_AE_SPC

! number of deposited aerosol species
INTEGER, PARAMETER :: N_AE_DEPV = N_AE_SPC
INTEGER, PARAMETER :: N_AE_DEPVD = N_AE_DEPV

! array of minimum concentrations
! set in aerosol_chem
REAL, DIMENSION( N_AE_SPCD ) :: AECONMIN

! pointer to deposition velocity surrogates
! set in aerosol_chem
INTEGER, DIMENSION( N_AE_DEPVD ) :: DEPV_SUR

INTEGER, PARAMETER :: NSPCSDA = N_AE_SPC + 37 ! N2O5 added - FSB
                                              ! HCl added - US
                                              ! 12 SVOCs added - PVB
                                              ! NO2 and HONO added - GS
                                              ! 6 species added - JTK
                                              ! 3 amine species added - PK
REAL, DIMENSION ( NSPCSDA ) :: CBLK, CBLKA, CBLKP

! pointers for aerosol module
! *** pointers to gas (vapor) phase species and production rates
INTEGER, PARAMETER :: NINORG = 7 ! For AE5 module
INTEGER :: LSULF = 0, LHNO3 = 0, LNH3 = 0, LHCL = 0, LN2O5 = 0, &
           LNO2 = 0,  LHONO = 0

INTEGER, PARAMETER :: NAMINE = 3 ! 3 amines: MEA, AMP, PZ
INTEGER :: LMEA = 0, LAMP = 0, LPZ = 0

INTEGER, PARAMETER :: NVAP = 12 ! For AE5 module
INTEGER :: LVALK  = 0, LVXYL1 = 0, LVXYL2 = 0, LVTOL1 = 0, LVTOL2 = 0, &
           LVBNZ1 = 0, LVBNZ2 = 0, LVTRP1 = 0, LVTRP2 = 0, LVISO1 = 0, &
           LVISO2 = 0, LVSQT = 0

! Reaction products: note for AE5 with CB05, there is no rxn product from ALK
! LALKRXN kept here for consistency with CMAQ
INTEGER, PARAMETER :: NPROD = 10 ! For AE5 module
INTEGER :: LSULFP = 0
INTEGER :: LALKRXN  = 0, LXYLNRXN = 0, LXYLHRXN = 0, LTOLNRXN = 0, &
           LTOLHRXN = 0, LBNZNRXN = 0, LBNZHRXN = 0, LTRPRXN  = 0, &
           LISOPRXN = 0, LSESQRXN = 0

! secondary organic aerosols precursors
INTEGER, PARAMETER :: NPSPCS = 10
REAL, DIMENSION ( NPSPCS ) :: ORGPROD  ! SOA precursor counter species
!     ORGPROD( 1) -> "long" alkanes  (ALKRXN)
!     ORGPROD( 2) -> low-yield aromatics, hi-NOx pathway (XYLNRXN)
!     ORGPROD( 3) -> low-yield aromatics, lo-NOx pathway (XYLHRXN)
!     ORGPROD( 4) -> high-yield aromatics, hi-NOx pathway (TOLNRXN)
!     ORGPROD( 5) -> high-yield aromatics, lo-NOx pathway (TOLHRXN)
!     ORGPROD( 6) -> benzene, hi-NOx pathway (BNZNRXN)
!     ORGPROD( 7) -> benzene, lo-NOx pathway (BNZHRXN)
!     ORGPROD( 8) -> monoterpenes (TRPRXN)
!     ORGPROD( 9) -> isoprene (ISOPRXN)
!     ORGPROD(10) -> sesquiterpenes (SESQRXN)

end module aero_species_inc
