!----------------------------------------------------------------------
! Version 1, February 2012, PK, ENVIRON
!
! Include files for CMAQ species for CB05-AE5 (CMAQ 4.7.1)
!----------------------------------------------------------------------

      module cmaq_species_inc

      implicit none

      include 'GC_SPC.EXT'
      include 'AE_SPC.EXT'
      include 'NR_SPC.EXT'
      include 'GC_G2AQ.EXT'
      include 'AE_A2AQ.EXT'
      include 'NR_N2AQ.EXT'
      include 'GC_SCAV.EXT'
      include 'AE_SCAV.EXT'
      include 'NR_SCAV.EXT'

      include 'AQ_PARAMS.EXT'      ! aqueous chemistry shared parameters

      INTEGER, PARAMETER :: NSPCS  = N_GC_SPC + N_AE_SPC + N_NR_SPC

      end module cmaq_species_inc
