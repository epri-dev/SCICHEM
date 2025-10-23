!***********************************************************************
! SCICHEM driver routine for the inorganic aerosol equilibrium         *
! This code has been developed for EPRI, Palo Alto, CA 94304           *
! Contract 00-10003332                                                 *
!                                                                      *
! Developed by Prakash Karamchandani                                   *
! Ramboll Environ, Novato, CA 94998                                    *
!***********************************************************************
subroutine inorg_aero(cso4,cno3,cnh4,chno3,cnh3,tb,rh)
!******************************************************************************
!
! FUNCTION:  Driver for inorganic aerosol equilibrium calculations
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!               isoropia
!
! REVISION HISTORY:
!      Version 1.0, by Prakash Karamchandani, May 2015
!      ENVIRON
!      773 San Marin Drive, Suite 2115, Novato, CA 94998
!
! REVISION HISTORY:
!
!******************************************************************************

! --- MODULES

USE AERO_INFO      ! from CMAQ
!USE mpi_fi, ONLY: myid

implicit none

! --- ARGUMENTS

! Inorganic aerosol species concs
REAL, INTENT( INOUT ) :: cso4,cno3,cnh4,chno3,cnh3
REAL, INTENT( IN    ) :: tb, rh  ! temperature and relative humidity

! factors for converting from moles/m3 to ug/m3
REAL, PARAMETER :: HNO3CONV  = 1.0E6 * MWHNO3
REAL, PARAMETER :: NH3CONV   = 1.0E6 * MWNH3
REAL, PARAMETER :: HCLCONV   = 1.0E6 * MWHCL
REAL, PARAMETER :: SO4CONV   = 1.0E6 * MWSO4
REAL, PARAMETER :: NO3CONV   = 1.0E6 * MWNO3
REAL, PARAMETER :: NH4CONV   = 1.0E6 * MWNH4

! factors for converting from ug/m3 to moles/m3
REAL, PARAMETER :: HNO3CONV1  = 1.0 / HNO3CONV
REAL, PARAMETER :: NH3CONV1   = 1.0 / NH3CONV
REAL, PARAMETER :: HCLCONV1   = 1.0 / HCLCONV
REAL, PARAMETER :: SO4CONV1   = 1.0 / SO4CONV
REAL, PARAMETER :: NO3CONV1   = 1.0 / NO3CONV
REAL, PARAMETER :: NH4CONV1   = 1.0 / NH4CONV

! *** ISORROPIA input variables

REAL( 8 ) :: WI( 8 )              ! species array
REAL( 8 ) :: RHI                  ! relative humidity
REAL( 8 ) :: TEMPI                ! temperature
REAL( 8 ) :: CNTRL( 2 )           ! control parameters

! *** ISORROPIA output variables

REAL( 8 ) :: WT( 8 )              ! species output array
REAL( 8 ) :: GAS( 3 )             ! gas-phase   "     "
REAL( 8 ) :: AERLIQ( 15 )         ! liq aerosol "     "
REAL( 8 ) :: AERSLD( 19 )         ! solid "     "     "
REAL( 8 ) :: OTHER( 9 )           ! supplmentary output array
CHARACTER( 15 ) :: SCASI          ! subcase number output

! *** double precision vars for ISORROPIA (mole/m3)
! *** assume Na and Cl are negligible
WI( 1 ) = 1.D-30
WI( 2 ) = cso4 * SO4CONV1
WI( 3 ) = cnh4 * NH4CONV1 + cnh3 * NH3CONV1
WI( 4 ) = cno3 * NO3CONV1 + chno3 * HNO3CONV1
WI( 5 ) = 1.D-30
!temporary set Ca, K, Mg to zero
WI( 6 ) = 1.D-30 !Ca
WI( 7 ) = 1.D-30 !K
WI( 8 ) = 1.D-30 !Mg

TEMPI = tb
RHI   = rh

CNTRL( 1 ) = 0.0D0   ! forward problem
CNTRL( 2 ) = 1.0D0   ! aerosol in metastable state

CALL ISOROPIA( WI, RHI, TEMPI, CNTRL, WT, GAS, AERLIQ, &
               AERSLD, SCASI, OTHER )

! *** update gas-phase concentrations
cnh3  = GAS( 1 ) * NH3CONV
chno3 = GAS( 2 ) * HNO3CONV

! *** update particle-phase concentrations (no change in so4, only nh4 and no3)
cnh4  = (WI(3) - GAS( 1 )) * NH4CONV
cno3  = (WI(4) - GAS( 2 )) * NO3CONV

return
end
