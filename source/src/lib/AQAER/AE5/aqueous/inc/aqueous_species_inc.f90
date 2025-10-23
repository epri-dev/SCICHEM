!----------------------------------------------------------------------
! Version 1, February 2005, PK, AER, Inc
!
! Updated for CMAQ 4.7.1 AE5 version, PK, ENVIRON, Feb 2012
!----------------------------------------------------------------------

module aqueous_species_inc

use cmaq_species_inc

implicit none

SAVE

INTEGER, DIMENSION(NGAS) :: NSRGGAS   ! # surrogates in CONC for each gas
INTEGER, DIMENSION(NAER) :: NSRGAER   ! # surrogates in CONC for each aerosol

! # of species in aqueous chemistry module
INTEGER, PARAMETER :: N_CONC2AQ = N_GC_G2AQ + N_AE_A2AQ + N_NR_N2AQ
CHARACTER*16  CONC2AQ( N_CONC2AQ )     ! CONC species used in AQCHEM
INTEGER       CONC2AQ_MAP( N_CONC2AQ ) ! CONC map to aqueous species

! # of scavenged species
INTEGER, PARAMETER :: N_CONC_SCAV = N_GC_SCAV + N_AE_SCAV + N_NR_SCAV
CHARACTER*16  CONC_SCAV( N_CONC_SCAV )     ! Scavenged CONC species
INTEGER       CONC_SCAV_MAP( N_CONC_SCAV ) ! CONC map to scavenged species
REAL          CONC_SCAV_FAC( N_CONC_SCAV ) ! Sscavenged species factors

! additional pointers for aqueous module surrogate species
INTEGER       LSRG( NSPCS )          ! temporary pointer array
INTEGER       LSRGGAS( NGAS, NSPCS ) ! pointers in CONC to each gas
INTEGER       LSRGAER( NAER, NSPCS ) ! pointers in CONC to each aerosol

! pointers to aitken aerosol #, aitken aerosol species, aitken aerosol surface
INTEGER, DIMENSION( NSPCS ) :: L_NUMAKN, L_MASAKN, L_SRFAKN

end module aqueous_species_inc
