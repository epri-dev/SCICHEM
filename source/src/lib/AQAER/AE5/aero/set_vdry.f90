subroutine set_vdry(conc)
!----------------------------------------------------------------------
! aerosol dry deposition routine, based on CMAQ 4.7.1 aero_depv.F
!
! PK, ENVIRON, February 2012
!----------------------------------------------------------------------

use AqAer_fi
USE AERO_INFO      ! from CMAQ

implicit none

! Arguments
! Inputs:
real conc(*)          ! array of species concentrations

! Local variables:

REAL, PARAMETER :: DGMIN = 1.0E-09   ! minimum particle diameter ( m )
REAL, PARAMETER :: DENSMIN = 1.0E03  ! minimum particle density [kg/m**3]

! ranges for acceptable values of LOG( sigma_g).
REAL, PARAMETER :: MINL2SG = 2.38048048E-3 ! minimum value of L2SG
                                           ! minimum sigma_g = 1.05
REAL, PARAMETER :: MAXL2SG = 8.39588705E-1 ! maximum value of L2SG
                                           ! maximum sigma_g = 2.5

! Boundary layer variables
real blkta(1)
real ra(1)  ! Aerodynamic resistance
real ustar(1)
real wstar(1)

real pres_pa  ! Pressure in pascals
real airdens(1)  ! Air density (kg/m3)

integer   v, spc                   ! species loop counters

! Variables for getting size distribution information

! *** modal mass concentrations [ ug m**3 ]

real pmassat         ! mass concentration in Aitken mode
real pmassac         ! mass concentration in accumulation mode
real pmassco         ! mass concentration in coarse mode

! *** average modal particle densities  [ kg/m**3 ]

real pdensat(1)         ! average particle density in Aitken mode
real pdensac(1)         ! average particle density in accumulation mode
real pdensco(1)         ! average particle density in coarse mode

real xlm(1)     ! atmospheric mean free path [ m]
real amu(1)     ! atmospheric dynamic viscosity ! [ kg/(m s) ]

! *** modal diameters: [ m ]

real dgatk(1)           ! Aitken mode geometric mean diameter  [ m ]
real dgacc(1)           ! accumulation geometric mean diameter [ m ]
real dgcor(1)           ! coarse mode geometric mean diameter  [ m ]

! *** log of modal geometric standard deviation

real xxlsgat(1)      ! Aitken mode
real xxlsgac(1)      ! accumulation mode
real xxlsgco(1)      ! coarse mode

! Square root of the log of geometric standard deviation
real l2sgat, l2sgac, l2sgco
real esat36, esac36  ! see usage
real esco36

REAL, PARAMETER :: RADYN_MAX = 1.0E30 ! max. aerodynamic resistance

! *** Variables for adjustment of THIRD AND SECOND moments

real old_m3i, new_m3i ! aitken mode
real old_m2i, new_m2i

real old_m3j, new_m3j ! accumulation mode
real old_m2j, new_m2j

real old_m3k, new_m3k ! coarse mode
real old_m2k, new_m2k !

REAL VDEP( N_AE_DEP_SPC )

! dummy variable for consistency with CMAQ routines
integer numcells

! Begin calculations
blkta(1) = tk
ra(1)    = MIN( radyn, RADYN_MAX )
ustar(1) = SQRT( us2 )
wstar(1) = SQRT( ws2 )

! Load aerosol species concentrations

! *** initialize CBLK

cblk = 0.0

! --- Fill CBLK array with aerosol species concentrations
do spc = 1, naerp
   cblk(spc) = MAX( conc(cblk_map(spc)), AECONMIN( spc ) )
end do

! *** Transform surface area to 2nd moment by dividing by PI

cblk( VAT2 ) = cblk( VSURFAT ) / PI
cblk( VAC2 ) = cblk( VSURFAC ) / PI
cblk( VCO2 ) = cblk( VSURFCO ) / PI

! *** code from getpar

! *** set up aerosol 3rd moment, mass, density [ mom/ m**3 s ]

! *** Adjust the 3rd and 2nd moments for H2O and SOA. ***
!     The transport codes post-AE2 consider the aerosol to be "dry",
!     that is not to have H2O in the 2nd moment. The approach here
!     is to calculate the 3rd moments without H2O (OLD_M3I and OLD_M3J),
!     then add the contribution of water (NEW_M3I AND NEW_M3J), and then
!     adjust the 2nd moments holding the geometric standard deviations
!     constant. To preserve the standard deviations, the ratio of
!     the NEW to OLD 2nd moments is equal to the ratio of NEW to OLD
!     3rd moments raised to the two thirds power.  The reason for this
!     adjustment is that aerosols are deposited at ambient conditions,
!     which includes particle-bound H2O.
! *** 12/17/03 revision --S.Roselle
!     SOA was removed from 2nd moment for transport (i.e., "dry"
!     is now defined as containing neither H2O nor SOA).  Therefore,
!     SOA must be added back for deposition in a manner analogous to
!     the treatment of H2O.
! *** 01/30/08 revision --S.Napelenok & P.Bhave
!       Modified definition of "dry" aerosol to include the newly added
!     nonvolatile SOA species (AE5 only)

! *** Calculate third moments without contribution of H2O and semi-vol SOA

old_m3i = MAX( CONMIN, ( SO4FAC  * cblk( VSO4AI  ) + &
                         NH4FAC  * cblk( VNH4AI  ) + &
                         NO3FAC  * cblk( VNO3AI  ) + &
                         ORGFAC  * cblk( VORGPAI ) + &
                         ORGFAC  * cblk( VPAMINEI) + &
                         ANTHFAC * cblk( VP25AI  ) + &
                         ANTHFAC * cblk( VECI    ) + &
                         SEASFAC * cblk( VNAI    ) + &
                         SEASFAC * cblk( VCLI    ) ) )

old_m3j = MAX( CONMIN, ( SO4FAC  * cblk( VSO4AJ  ) + &
                         NH4FAC  * cblk( VNH4AJ  ) + &
                         NO3FAC  * cblk( VNO3AJ  ) + &
                         ORGFAC  * cblk( VORGPAJ ) + &
                         ORGFAC  * cblk( VPAMINEJ) + &
                         ORGFAC  * cblk( VXYL3J  ) + &
                         ORGFAC  * cblk( VTOL3J  ) + &
                         ORGFAC  * cblk( VBNZ3J  ) + &
                         ORGFAC  * cblk( VISO3J  ) + &
                         ORGFAC  * cblk( VOLGAJ  ) + &
                         ORGFAC  * cblk( VOLGBJ  ) + &
                         ORGFAC  * cblk( VORGCJ  ) + &
                         ANTHFAC * cblk( VP25AJ  ) + &
                         ANTHFAC * cblk( VECJ    ) + &
                         SEASFAC * cblk( VNAJ    ) + &
                         SEASFAC * cblk( VCLJ    ) ) )

old_m3k = MAX( CONMIN, ( SO4FAC  * cblk( VSO4K   ) + &
                         NH4FAC  * cblk( VNH4K   ) + &
                         NO3FAC  * cblk( VNO3K   ) + &
                         SOILFAC * cblk( VSOILA  ) + &
                         ANTHFAC * cblk( VANTHA  ) + &
                         SEASFAC * cblk( VNAK    ) + &
                         SEASFAC * cblk( VCLK    ) ) )

! *** add contribution of water and semi-volatile SOA to third moment

new_m3i = old_m3i + H2OFAC  * cblk( VH2OAI )
cblk( VAT3 ) = new_m3i

new_m3j = old_m3j + H2OFAC  * cblk( VH2OAJ ) &
                  + ORGFAC  * cblk( VALKJ  ) &
                  + ORGFAC  * cblk( VXYL1J ) &
                  + ORGFAC  * cblk( VXYL2J ) &
                  + ORGFAC  * cblk( VTOL1J ) &
                  + ORGFAC  * cblk( VTOL2J ) &
                  + ORGFAC  * cblk( VBNZ1J ) &
                  + ORGFAC  * cblk( VBNZ2J ) &
                  + ORGFAC  * cblk( VTRP1J ) &
                  + ORGFAC  * cblk( VTRP2J ) &
                  + ORGFAC  * cblk( VISO1J ) &
                  + ORGFAC  * cblk( VISO2J ) &
                  + ORGFAC  * cblk( VSQTJ  )
cblk( VAC3 ) = new_m3j

new_m3k = old_m3k + H2OFAC  * cblk( VH2OK )
cblk( VCO3 ) = new_m3k

! *** fetch second moment for "dry" particles

old_m2i = cblk( VAT2 )
old_m2j = cblk( VAC2 )
old_m2k = cblk( VCO2 )

! *** adjust second moment for "wet" particles

new_m2i = old_m2i * ( new_m3i / old_m3i ) ** TWO3
cblk( VAT2 ) = new_m2i

new_m2j = old_m2j * ( new_m3j / old_m3j ) ** TWO3
cblk( VAC2 ) = new_m2j

new_m2k = old_m2k * ( new_m3k / old_m3k ) ** TWO3
cblk( VCO2 ) = new_m2k

! *** now get particle mass [ ug/m**3 ] ( including water & semi-vol SOA )

! *** Aitken-mode:

pmassat = MAX( CONMIN, ( cblk( VSO4AI  ) + cblk( VNH4AI  ) + &
                         cblk( VNO3AI  ) + cblk( VORGPAI ) + &
                         cblk( VPAMINEI ) + &
                         cblk( VP25AI  ) + cblk( VECI    ) + &
                         cblk( VNAI    ) + cblk( VCLI    ) + &
                         cblk( VH2OAI  ) ) )

! *** Accumulation-mode:

pmassac = MAX( CONMIN, ( cblk( VSO4AJ  ) + cblk( VNH4AJ  ) + &
                         cblk( VNO3AJ  ) + cblk( VALKJ   ) + &
                         cblk( VXYL1J  ) + cblk( VXYL2J  ) + &
                         cblk( VXYL3J  ) + cblk( VTOL1J  ) + &
                         cblk( VTOL2J  ) + cblk( VTOL3J  ) + &
                         cblk( VBNZ1J  ) + cblk( VBNZ2J  ) + &
                         cblk( VBNZ3J  ) + cblk( VTRP1J  ) + &
                         cblk( VTRP2J  ) + cblk( VISO1J  ) + &
                         cblk( VISO2J  ) + cblk( VISO3J  ) + &
                         cblk( VSQTJ   ) + cblk( VOLGAJ  ) + &
                         cblk( VOLGBJ )  + cblk( VORGCJ  ) + &
                         cblk( VORGPAJ ) + cblk( VP25AJ  ) + &
                         cblk( VPAMINEJ) + &
                         cblk( VECJ    ) + cblk( VNAJ    ) + &
                         cblk( VCLJ    ) + cblk( VH2OAJ  ) ) )

! *** Coarse mode:

pmassco = MAX( CONMIN, cblk( VSOILA ) + cblk( VANTHA ) + &
                       cblk( VNAK   ) + cblk( VCLK   ) + &
                       cblk( VSO4K  ) + cblk( VNO3K  ) + &
                       cblk( VNH4K  ) + cblk( VH2OK  ) )

! *** now get particle density, mean free path, and dynamic viscosity

! *** density in [ kg m**-3 ]

pdensat = MAX( DENSMIN, ( F6DPIM9 * pmassat / cblk( VAT3 ) ) )
pdensac = MAX( DENSMIN, ( F6DPIM9 * pmassac / cblk( VAC3 ) ) )
pdensco = MAX( DENSMIN, ( F6DPIM9 * pmassco / cblk( VCO3 ) ) )

! *** Calculate mean free path [ m ]:

! --- pressure in pascals
pres_pa = patm * STDATMPA

xlm = 6.6328E-8 * P0 * blkta(1) / ( T0 * pres_pa )

! *** 6.6328E-8 is the sea level value given in Table I.2.8
! *** on page 10 of U.S. Standard Atmosphere 1962

! *** Calculate dynamic viscosity [ kg m**-1 s**-1 ]:

! *** U.S. Standard Atmosphere 1962 page 14 expression
!     for dynamic viscosity is:
!     dynamic viscosity =  beta * T * sqrt(T) / ( T + S)
!     where beta = 1.458e-6 [ kg sec^-1 K**-0.5 ], s = 110.4 [ K ].

amu = 1.458E-6 * blkta(1) * SQRT( blkta(1) ) / ( blkta(1) + 110.4 )

! *** Calculate geometric standard deviations and geometric mean diameters
!     in Aitken and accumulation modes

! *** geometric standard deviations
! *** Aitken Mode:

l2sgat = ONE3 * LOG( cblk( VAT0 ) ) + &
         TWO3 * LOG( cblk( VAT3 ) ) - &
     &          LOG( cblk( VAT2 ) )

l2sgat = MAX( MINL2SG, l2sgat )
l2sgat = MIN( MAXL2SG, l2sgat )
xxlsgat = SQRT( l2sgat )
esat36 = EXP( 4.5 * l2sgat )

! *** accumulation mode:

l2sgac = ONE3 * LOG( cblk( VAC0 ) ) + &
         TWO3 * LOG( cblk( VAC3 ) ) - &
     &          LOG( cblk( VAC2 ) )

l2sgac = MAX( MINL2SG, l2sgac )
l2sgac = MIN( MAXL2SG, l2sgac )

xxlsgac = SQRT( l2sgac )
esac36 = EXP( 4.5 * l2sgac )

! *** accumulation mode:

l2sgco = ONE3 * LOG( cblk( VCO0 ) ) + &
         TWO3 * LOG( cblk( VCO3 ) ) - &
                LOG( cblk( VCO2 ) )

l2sgco = MAX( MINL2SG, l2sgco )
l2sgco = MIN( MAXL2SG, l2sgco )

xxlsgco = SQRT( l2sgco )
esco36 = EXP( 4.5 * l2sgco )

! *** Calculate geometric mean diameters [ m ]

dgatk = MAX( DGMIN, ( cblk( VAT3  ) / cblk( VAT0 ) * esat36 ) ** ONE3 )
dgacc = MAX( DGMIN, ( cblk( VAC3  ) / cblk( VAC0 ) * esac36 ) ** ONE3 )
dgcor = MAX( DGMIN, ( cblk( VCO3  ) / cblk( VCO0 ) * esco36 ) ** ONE3 )

! *** end of code from getpar

! *** now get dry deposition velocities:

! Calculate air density (kg/m3)
airdens = ( patm * MWAIR ) / ( blkta(1) * RGAS )

! Assign dummy variable (not used) for consistency with CMAQ subroutine
numcells = 1

call getdep_v ( numcells, N_AE_DEP_SPC, blkta, airdens, xlm, amu, &
                wstar, ustar, ra, dgatk, dgacc, dgcor, &
                xxlsgat, xxlsgac, xxlsgco, &
                pdensat, pdensac, pdensco, &
                vdep )

! Return dry deposition velocities for aerosols.
vdry = 0.
do v = 1, N_AE_DEPV
  if ( DEPV_SUR( v ) > 0 ) THEN
    vdry( v ) = vdep( DEPV_SUR( v ) )
  end if
end do

return
end
