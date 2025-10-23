!***********************************************************************
! Constants for CMAQ 4.7.1 AE5 aerosol chemistry modules               *
!                                                                      *
! Version 1 by Prakash Karamchandani, February 2012                    *
! ENVIRON, 773 San Marin Drive, Suite 2115, Novato, CA, 94998          *
!***********************************************************************

module aero_consts_inc

implicit none

save

! --- CONSTANTS
! gas constant in L-atm/mol-K
REAL, PARAMETER :: RGAS = 0.0820567

! Molecular weights of SVOCs
REAL, PARAMETER :: MWVALK   = 150.0
REAL, PARAMETER :: MWVXYL   = 192.0
REAL, PARAMETER :: MWVTOL   = 168.0
REAL, PARAMETER :: MWVBNZ   = 144.0
REAL, PARAMETER :: MWVTRP   = 168.0
REAL, PARAMETER :: MWVISO   = 96.0
REAL, PARAMETER :: MWVSQT   = 378.0

! Molecular weights of SVOC precursors
REAL, PARAMETER :: MWALKRXN = 114.0
REAL, PARAMETER :: MWXYLRXN = 106.0
REAL, PARAMETER :: MWTOLRXN = 92.0
REAL, PARAMETER :: MWBNZRXN = 78.0
REAL, PARAMETER :: MWTRPRXN = 136.0
REAL, PARAMETER :: MWISORXN = 68.0
REAL, PARAMETER :: MWSQTRXN = 204.0

! Molecular weight of oligomerized SOA
REAL, PARAMETER :: MWOLGA = 176.4
REAL, PARAMETER :: MWOLGB = 252.0

! Stoichiometric coefficients of SVOCs from precursor reactions
REAL, PARAMETER :: SCALK  = 0.0718
REAL, PARAMETER :: SCXYL1 = 0.0386
REAL, PARAMETER :: SCXYL2 = 0.1119
REAL, PARAMETER :: SCTOL1 = 0.0758
REAL, PARAMETER :: SCTOL2 = 0.1477
REAL, PARAMETER :: SCBNZ1 = 0.0942
REAL, PARAMETER :: SCBNZ2 = 1.162
REAL, PARAMETER :: SCTRP1 = 0.1123
REAL, PARAMETER :: SCTRP2 = 0.5013
REAL, PARAMETER :: SCISO1 = 0.232
REAL, PARAMETER :: SCISO2 = 0.0288
REAL, PARAMETER :: SCSQT  = 1.3

REAL, PARAMETER :: SCXYL3 = 0.373
REAL, PARAMETER :: SCTOL3 = 0.471
REAL, PARAMETER :: SCBNZ3 = 0.484

! Carbon numbers
REAL, PARAMETER :: NCALK = 8.0
REAL, PARAMETER :: NCXYL = 8.0
REAL, PARAMETER :: NCTOL = 7.0
REAL, PARAMETER :: NCBNZ = 6.0
REAL, PARAMETER :: NCTRP = 10.0
REAL, PARAMETER :: NCISO = 5.0
REAL, PARAMETER :: NCSQT = 15.0
REAL, PARAMETER :: NCOLG = 12.0

! SOA/SOC ratio of oligomer products
REAL, PARAMETER :: OLGRAT = 2.1

! minimum values
REAL, PARAMETER :: CONMIN = 1.0E-30 ! concentration lower limit

! minimum aerosol sulfate concentration for acccumulation mode
REAL, PARAMETER :: AEROCONCMIN_AC = 1.0E-6 ! 1 pg
                                     ! [ ug/m**3 ] ! changed 12/13/99 by FSB
! *** This value is smaller than any reported tropospheric concentrations.

! minimum aerosol sulfate concentration for the Aitken mode
REAL, PARAMETER :: AEROCONCMIN_AT = 1.0E-6 * AEROCONCMIN_AC

! minimum coarse mode concentration. Set to give NUMMIN_CO = 1.0
REAL, PARAMETER :: AEROCONCMIN_CO = 1.889544E-05

! number of surrogates for aerosol dry deposition velocities
INTEGER, PARAMETER :: N_AE_DEP_SPC = 9

! based on order in aero_depv and getdep_v (in CMAQ)
INTEGER, PARAMETER :: VDNATK = 1  ! Aitken mode number
INTEGER, PARAMETER :: VDNACC = 2  ! accumulation mode number
INTEGER, PARAMETER :: VDNCOR = 3  ! coarse mode number
INTEGER, PARAMETER :: VDMATK = 4  ! Aitken mode mass
INTEGER, PARAMETER :: VDMACC = 5  ! accumulation mode mass
INTEGER, PARAMETER :: VDMCOR = 6  ! coarse mode mass
INTEGER, PARAMETER :: VDSATK = 7  ! Aitken mode surface area
INTEGER, PARAMETER :: VDSACC = 8  ! accumulation mode surface area
INTEGER, PARAMETER :: VDSCOR = 9  ! coarse mode surface area

end module aero_consts_inc
