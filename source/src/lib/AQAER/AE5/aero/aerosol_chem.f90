!***********************************************************************
! SCICHEM driver routine for the CMAQ-AE5 aerosol module               *
! This code has been developed for EPRI, Palo Alto, CA 94304           *
! Contract EP-P41335/C18216                                            *
!                                                                      *
! Developed by Prakash Karamchandani                                   *
! ENVIRON, Novato, CA 94998                                            *
!***********************************************************************
subroutine step_aerosol_chem(dt,concamb,conc)
!******************************************************************************
!
! FUNCTION:  Driver for aerosol routines
!
! PRECONDITIONS REQUIRED:
!
! SUBROUTINES AND FUNCTIONS CALLED:
!               sat_humid            aeroproc              getpar
!
! REVISION HISTORY:
!      Version 2.0, by Prakash Karamchandani, November 2013
!      Based on CMAQ-AE5 aerosol module in CMAQ 4.7.1
!      ENVIRON
!      773 San Marin Drive, Suite 2115, Novato, CA 94998
!
! REVISION HISTORY:
! Additional updates for SCICHEM Beta-2 release, Nov 2013, PK, ENVIRON *
! Additional updates for Amine chemistry, Sep 2023, PK, Ramboll
!******************************************************************************

! --- MODULES

USE AqAer_fi
USE AERO_INFO      ! from CMAQ
USE mpi_fi, ONLY: myid

implicit none

! --- ARGUMENTS

real dt              ! time step (seconds)
real conc(*)         ! array of species puff concentrations
real concamb(*)      ! array of species ambient concentrations

! --- LOCALS

! --- Total concs (plume + ambient)
REAL, DIMENSION(MAX_SP) :: conctot     ! array of species total concentrations

REAL GAMMA_N2O5       ! N2O5 heterogeneous reaction probability [ ]

! factors for converting from moles/m3 to ug/m3
REAL, PARAMETER :: H2SO4CONV = 1.0E6 * MWH2SO4
REAL, PARAMETER :: SO4CONV   = 1.0E6 * MWSO4
REAL, PARAMETER :: HNO3CONV  = 1.0E6 * MWHNO3
REAL, PARAMETER :: NH3CONV   = 1.0E6 * MWNH3
REAL, PARAMETER :: N2O5CONV  = 1.0E6 * MWN2O5
REAL, PARAMETER :: HCLCONV   = 1.0E6 * MWHCL
REAL, PARAMETER :: NO2CONV   = 1.0E6 * MWNO2
REAL, PARAMETER :: HONOCONV  = 1.0E6 * MWHONO

! Amine conversion factors
REAL, PARAMETER :: MEACONV   = 1.0E6 * MWMEA
REAL, PARAMETER :: AMPCONV   = 1.0E6 * MWAMP
REAL, PARAMETER :: PZCONV    = 1.0E6 * MWPZ

REAL, PARAMETER :: VALKCONV  = 1.0E6 * MWVALK
REAL, PARAMETER :: VXYLCONV  = 1.0E6 * MWVXYL
REAL, PARAMETER :: VTOLCONV  = 1.0E6 * MWVTOL
REAL, PARAMETER :: VBNZCONV  = 1.0E6 * MWVBNZ
REAL, PARAMETER :: VTRPCONV  = 1.0E6 * MWVTRP
REAL, PARAMETER :: VISOCONV  = 1.0E6 * MWVISO
REAL, PARAMETER :: VSQTCONV  = 1.0E6 * MWVSQT

! factors for converting from ug/m3 to moles/m3
REAL, PARAMETER :: H2SO4CONV1 = 1.0 / H2SO4CONV
REAL, PARAMETER :: HNO3CONV1  = 1.0 / HNO3CONV
REAL, PARAMETER :: NH3CONV1   = 1.0 / NH3CONV
REAL, PARAMETER :: N2O5CONV1  = 1.0 / N2O5CONV
REAL, PARAMETER :: HCLCONV1   = 1.0 / HCLCONV
REAL, PARAMETER :: NO2CONV1   = 1.0 / NO2CONV
REAL, PARAMETER :: HONOCONV1  = 1.0 / HONOCONV

! Amine conversion factors
REAL, PARAMETER :: MEACONV1  = 1.0 / MEACONV
REAL, PARAMETER :: AMPCONV1  = 1.0 / AMPCONV
REAL, PARAMETER :: PZCONV1   = 1.0 / PZCONV

REAL, PARAMETER :: VALKCONV1  = 1.0 / VALKCONV
REAL, PARAMETER :: VXYLCONV1  = 1.0 / VXYLCONV
REAL, PARAMETER :: VTOLCONV1  = 1.0 / VTOLCONV
REAL, PARAMETER :: VBNZCONV1  = 1.0 / VBNZCONV
REAL, PARAMETER :: VTRPCONV1  = 1.0 / VTRPCONV
REAL, PARAMETER :: VISOCONV1  = 1.0 / VISOCONV
REAL, PARAMETER :: VSQTCONV1  = 1.0 / VSQTCONV

! Minimum time step (for equilibrium only calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

! Molecular weight ratios
REAL, PARAMETER :: NH3RAT  = MWNH3  / MWNH4, &
                   NO3RAT = MWHNO3 / MWNO3, &
                   HCLRAT  = MWHCL  / MWCL, &
                   H2SO4RAT = MWH2SO4 / MWSO4, &
                   NO2RAT = MWHNO3 / MWNO2, &
                   HONORAT = MWHNO3 / MWHONO, &
                   N2O5RAT = MWHNO3 / MWN2O5

! *** factors to set minimum value for number concentrations
REAL :: NUMMIN_AT   ! Aitken mode
REAL :: NUMMIN_AC   ! accumulation mode
REAL :: NUMMIN_CO   ! coarse mode

! *** factors to set minimum value for second moment
REAL :: M2MIN_AT    ! Aitken  mode
REAL :: M2MIN_AC    ! accumulation mode
REAL :: M2MIN_CO    ! coarse mode

! *** chemical production rates: [ ug / m**3 s ]
real so4rate      ! sulfate gas-phase production rate

! *** modal diameters [ m ]

real dgatk           ! Aitken mode geometric mean diameter  [ m ]
real dgacc           ! accumulation geometric mean diameter [ m ]
real dgcor           ! coarse mode geometric mean diameter  [ m ]

! *** log of modal geometric standard deviation

real xxlsgat         ! Aitken mode
real xxlsgac         ! accumulation mode
real xxlsgco         ! coarse mode

REAL      ESC36      ! exp( 4.5 * log^2 (sigmag) )

! *** aerosol properties:

! *** modal mass concentrations [ ug m**3 ]

real pmassat         ! mass concentration in Aitken mode
real pmassac         ! mass concentration in accumulation mode
real pmassco         ! mass concentration in coarse mode

! *** average modal particle densities  [ kg/m**3 ]

real pdensat         ! average particle density in Aitken mode
real pdensac         ! average particle density in accumulation mode
real pdensco         ! average particle density in coarse mode

! *** variables to set up for "dry transport "
real m3_wet, m3_dry   ! third moment with and without water
real m2_wet, m2_dry   ! second moment with and without water
real m3subt           ! temp variable for dry 3rd moment calcs

! flag to include water in the 3rd moment calculation
LOGICAL, PARAMETER :: M3_WET_FLAG = .FALSE.

! *** if LIMIT_Sg = T, atkn & accum std. dev. are not changed by GETPAR

LOGICAL, PARAMETER :: LIMIT_Sg = .FALSE.

integer spc, vv   ! loop variables
integer i, j, ii
real hsx, pmb, rh, factor, factor1, pres_pa

! dummy variables for consistency with CMAQ routines
integer col, row, layer
real airdens, xlm, amu

! plume to total concentration ratios
REAL RNOY, OMRNOY, RSO4, OMRSO4
REAL RNA, OMRNA, RCL, OMRCL
REAL PLUMECONC, TOTALCONC

! Variables for amine nucleation
REAL amine_conc, h2so4_conc
REAL dmea, damp, dpz, rmea, ramp, rpz

LOGICAL, SAVE :: FIRSTTIME = .TRUE.

if (FIRSTTIME) then

   FIRSTTIME = .FALSE.

! Species mapping for particle species
   call init_aero_map(aero_names,naerp,cblk_map,depv_sur)
   IF( error%nError /= NO_ERROR )return
! set internal species pointers for non-AE species
! inorganic gases:
   do spc = naerp+1, naero
      if (TRIM(aero_names(spc)) == 'SULF') then
         LSULF = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HNO3') then
         LHNO3 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'N2O5') then
         LN2O5 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'NO2') then
         LNO2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HONO') then
         LHONO = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'HCL') then
         LHCL = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'NH3') then
         LNH3 = spc
         CYCLE
      end if

! reaction products
      if (TRIM(aero_names(spc)) == 'ALKRXN') then
         LALKRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'XYLNRXN') then
         LXYLNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'XYLHRXN') then
         LXYLHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TOLNRXN') then
         LTOLNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TOLHRXN') then
         LTOLHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'BNZNRXN') then
         LBNZNRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'BNZHRXN') then
         LBNZHRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'TRPRXN') then
         LTRPRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'ISOPRXN') then
         LISOPRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SESQRXN') then
         LSESQRXN = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SULRXN') then
         LSULFP = spc
         CYCLE
      end if

! *** Set pointers for vapor-phase semi-volatile organic compounds
      if (TRIM(aero_names(spc)) == 'SV_ALK') then
         LVALK = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_XYL1') then
         LVXYL1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_XYL2') then
         LVXYL2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TOL1') then
         LVTOL1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TOL2') then
         LVTOL2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_BNZ1') then
         LVBNZ1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_BNZ2') then
         LVBNZ2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TRP1') then
         LVTRP1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_TRP2') then
         LVTRP2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_ISO1') then
         LVISO1 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_ISO2') then
         LVISO2 = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'SV_SQT') then
         LVSQT = spc
         CYCLE
      end if

! Amines
      if (TRIM(aero_names(spc)) == 'MEA') then
         LMEA = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'AMP') then
         LAMP = spc
         CYCLE
      end if
      if (TRIM(aero_names(spc)) == 'PZ') then
         LPZ = spc
         CYCLE
      end if
   end do
!debug
   !WRITE(99+myid,*)'lsulf,lhno3,lnh3,ln2o5,lhcl,lno2,lhono: ', &
              !lsulf,lhno3,lnh3,ln2o5,lhcl,lno2,lhono
   !WRITE(99+myid,*)'lsulfp,lalkrxn,lisoprxn,ltrprxn,ltolnrxn,ltolhrxn: ', &
              !lsulfp,lalkrxn,lisoprxn,ltrprxn,ltolnrxn,ltolhrxn
   !WRITE(99+myid,*)'lxylnrxn,lxylhrxn,lbnznrxn,lbnzhrxn,lsesqrxn: ', &
              !lxylnrxn,lxylhrxn,lbnznrxn,lbnzhrxn,lsesqrxn
   !WRITE(99+myid,*)'lvalk,lvxyl1,lvxyl2,lvtol1,lvtol2,lvbnz1: ', &
              !lvalk,lvxyl1,lvxyl2,lvtol1,lvtol2,lvbnz1
   !WRITE(99+myid,*)'lvbnz2,lvtrp1,lvtrp2,lviso1,lviso2,lvsqt: ', &
              !lvbnz2,lvtrp1,lvtrp2,lviso1,lviso2,lvsqt
!debug

! Check for required species
   if (lsulf == 0 .or. lhno3 == 0 .or. ln2o5 == 0 .or. lno2 == 0 .or. &
       lhono == 0 .or. lhcl  == 0 .or. lnh3 ==  0 .or. lsulfp == 0 ) then
       error%nError   = IV_ERROR
       error%eRoutine = 'step_aerosol_chem'
       error%eMessage = 'Necessary inorganic gaseous species for aerosol module' &
                   // '  not found in species list'
       return
   end if

   if (lvalk  == 0 .or. lvxyl1 == 0 .or. lvxyl2 == 0 .or. lvtol1 == 0 .or. &
       lvtol2 == 0 .or. lvbnz1 == 0 .or. lvbnz2 == 0 .or. lvtrp1 == 0 .or. &
       lvtrp2 == 0 .or. lviso1 == 0 .or. lviso2 == 0 .or. lvsqt  == 0) then
       error%nError   = IV_ERROR
       error%eRoutine = 'step_aerosol_chem'
       error%eMessage = 'Necessary semi-volatile organic species for ' &
                   // ' aerosol module not found in species list'
       return
   end if

   if (lmea == 0 .or. lamp == 0 .or. lpz == 0 ) then
       error%nError   = IV_ERROR
       error%eRoutine = 'step_aerosol_chem'
       error%eMessage = 'Necessary amine species for aerosol module' &
                   // '  not found in species list'
       return
   end if

! *** calculate minimum values for number and 2nd moment.

  xxlsgat = LOG( SGINIAT ); xxlsgac = LOG( SGINIAC ); xxlsgco = LOG( SGINICO )
  ESC36   = EXP( 4.5 * xxlsgco * xxlsgco )

  NUMMIN_AT = SO4FAC * AEROCONCMIN_AT &
              / ( DGINIAT ** 3 * EXP( 4.5 * xxlsgat * xxlsgat ) )
  NUMMIN_AC = SO4FAC * AEROCONCMIN_AC &
              / ( DGINIAC ** 3 * EXP( 4.5 * xxlsgac * xxlsgac ) )
  NUMMIN_CO = ANTHFAC * AEROCONCMIN_CO / ( DGINICO**3 * ESC36 )

  M2MIN_AT = NUMMIN_AT * DGINIAT ** 2 * EXP( 2.0 * xxlsgat * xxlsgat )
  M2MIN_AC = NUMMIN_AC * DGINIAC ** 2 * EXP( 2.0 * xxlsgac * xxlsgac )
  M2MIN_CO = NUMMIN_CO * DGINICO ** 2 * EXP( 2.0 * xxlsgco * xxlsgco )

  AECONMIN = CONMIN

  AECONMIN( VAT0 ) = NUMMIN_AT
  AECONMIN( VAC0 ) = NUMMIN_AC
  AECONMIN( VCO0 ) = NUMMIN_CO

  AECONMIN( VSURFAT ) = M2MIN_AT
  AECONMIN( VSURFAC ) = M2MIN_AC
  AECONMIN( VSURFCO ) = M2MIN_CO

  AECONMIN( VSO4AI ) = AEROCONCMIN_AT
  AECONMIN( VSO4AJ ) = AEROCONCMIN_AC
  AECONMIN( VANTHA ) = AEROCONCMIN_CO

!debug
  !WRITE(99+myid,*)'AECONMIN: ',AECONMIN
!debug

end if  ! If first time

! --- calculate humidity mixing ratio at saturation
pmb = patm*1013.25
call sat_humid(tk,pmb,hsx)
rh = hb/hsx
rh = MIN(rh,1.0)
rh = MAX(rh,0.01)

! --- pressure in pascals
pres_pa = patm * STDATMPA

! --- Set conversion factors for concentrations
! --- ppm or molecules/cc to moles/m3
if (ic_units == UNIT_PPM) then
 factor = 1.E-3/(298.*RGAS)
!   factor = patm*1.E-3/(tk*RGAS)
else if (ic_units == UNIT_MOLECULE) then
 factor = 1.3634E-19/(RGAS)
end if

! *** initialize CBLK

cblka = 0.0  ! Ambient concs
cblkp = 0.0  ! Plume concs
cblk  = 0.0  ! Total concs

! --- Fill CBLK array
! --- First fill aerosol species
do spc = 1, naerp
   cblka(spc) = concamb(cblk_map(spc))
   cblkp(spc) = conc(cblk_map(spc))
   if ( spc == VALKJ  .or. &
      spc == VXYL1J .or. &
      spc == VXYL2J .or. &
      spc == VXYL3J .or. &
      spc == VTOL1J .or. &
      spc == VTOL2J .or. &
      spc == VTOL3J .or. &
      spc == VBNZ1J .or. &
      spc == VBNZ2J .or. &
      spc == VBNZ3J .or. &
      spc == VTRP1J .or. &
      spc == VTRP2J .or. &
      spc == VISO1J .or. &
      spc == VISO2J .or. &
      spc == VISO3J .or. &
      spc == VSQTJ  .or. &
      spc == VOLGAJ .or. &
      spc == VOLGBJ ) then
         cblk(spc)  = MAX( AECONMIN(spc), cblkp(spc) )
   else
         cblk(spc)  = MAX( AECONMIN(spc), cblka(spc) + cblkp(spc) )
   end if
end do

! *** Add gas and vapor phase species to CBLK

! First do nucleation of amines on sulfuric acid

! Total amine in plume
amine_conc = MAX( CONMIN, conc( LMEA ) + conc( LAMP ) + conc( LPZ ) )
rmea = conc( LMEA ) / amine_conc
ramp = conc( LAMP ) / amine_conc
rpz  = conc( LPZ )  / amine_conc

! Plume sulfuric acid
h2so4_conc = conc( LSULFP )
if ( amine_conc > h2so4_conc ) then
   conc( LSULFP ) = 0.
   dmea = h2so4_conc * rmea
   conc( LMEA ) = MAX( CONMIN, conc( LMEA ) - dmea )
   damp = h2so4_conc * ramp
   conc( LAMP ) = MAX( CONMIN, conc( LAMP ) - damp )
   dpz = h2so4_conc * rpz
   conc( LPZ  ) = MAX( CONMIN, conc( LPZ  ) - dpz )
   cblkp( VPAMINEI ) = cblkp( VPAMINEI ) + factor * ( MEACONV * dmea + &
                         AMPCONV * damp + PZCONV * dpz )
   conc( LSULFP ) = 0.
   cblkp( VSO4AI ) = cblkp( VSO4AI ) + factor * SO4CONV * h2so4_conc
else
   dmea = amine_conc * rmea
   conc( LMEA ) = MAX( CONMIN, conc( LMEA ) - dmea )
   damp = amine_conc * ramp
   conc( LAMP ) = MAX( CONMIN, conc( LAMP ) - damp )
   dpz  = amine_conc * rpz
   conc( LPZ  ) = conc( LPZ  ) - dpz
   conc( LPZ  ) = MAX( CONMIN, conc( LPZ  ) - dpz )
   cblkp( VPAMINEI ) = cblkp( VPAMINEI ) + factor * ( MEACONV * dmea + &
                         AMPCONV * damp + PZCONV * dpz )
   conc( LSULFP ) = MAX( CONMIN, conc( LSULFP ) - amine_conc )
   cblkp( VSO4AI ) = cblkp( VSO4AI ) + factor * SO4CONV * amine_conc
endif

cblka( VSULF ) = concamb( LSULF ) * factor * H2SO4CONV
cblkp( VSULF ) = conc( LSULF ) * factor * H2SO4CONV
cblk ( VSULF ) = MAX( CONMIN, cblka(VSULF) + cblkp(VSULF) )

cblka( VHNO3 ) = concamb( LHNO3 ) * factor * HNO3CONV
cblkp( VHNO3 ) = conc( LHNO3 ) * factor * HNO3CONV
cblk ( VHNO3 ) = MAX( CONMIN, cblka(VHNO3) + cblkp(VHNO3) )

cblka( VNH3  ) = concamb( LNH3  ) * factor * NH3CONV
cblkp( VNH3  ) = conc( LNH3  ) * factor * NH3CONV
cblk ( VNH3  ) = MAX( CONMIN, cblka(VNH3) + cblkp(VNH3) )

cblka( VHCL  ) = concamb( LHCL  ) * factor * HCLCONV
cblkp( VHCL  ) = conc( LHCL  ) * factor * HCLCONV
cblk ( VHCL  ) = MAX( CONMIN, cblka(VHCL) + cblkp(VHCL) )

cblka( VN2O5 ) = concamb( LN2O5 ) * factor * N2O5CONV
cblkp( VN2O5 ) = conc( LN2O5 ) * factor * N2O5CONV
cblk ( VN2O5 ) = MAX( CONMIN, cblka(VN2O5) + cblkp(VN2O5) )

cblka( VNO2  ) = concamb( LNO2  ) * factor * NO2CONV
cblkp( VNO2  ) = conc( LNO2  ) * factor * NO2CONV
cblk ( VNO2  ) = MAX( CONMIN, cblka(VNO2) + cblkp(VNO2) )

cblka( VHONO ) = concamb( LHONO ) * factor * HONOCONV
cblkp( VHONO ) = conc( LHONO ) * factor * HONOCONV
cblk ( VHONO ) = MAX( CONMIN, cblka(VHONO) + cblkp(VHONO) )

! SOA

cblkp( VVALK  ) = conc( LVALK  ) * factor * VALKCONV
cblk ( VVALK  ) = MAX( CONMIN, cblkp(VVALK))
cblkp( VVXYL1 ) = conc( LVXYL1  ) * factor * VXYLCONV
cblk ( VVXYL1 ) = MAX( CONMIN, cblkp(VVXYL1) )
cblkp( VVXYL2 ) = conc( LVXYL2  ) * factor * VXYLCONV
cblk ( VVXYL2 ) = MAX( CONMIN, cblkp(VVXYL2) )
cblkp( VVTOL1 ) = conc( LVTOL1  ) * factor * VTOLCONV
cblk ( VVTOL1 ) = MAX( CONMIN, cblkp(VVTOL1) )
cblkp( VVTOL2 ) = conc( LVTOL2  ) * factor * VTOLCONV
cblk ( VVTOL2 ) = MAX( CONMIN, cblkp(VVTOL2) )
cblkp( VVBNZ1 ) = conc( LVBNZ1  ) * factor * VBNZCONV
cblk ( VVBNZ1 ) = MAX( CONMIN, cblkp(VVBNZ1) )
cblkp( VVBNZ2 ) = conc( LVBNZ2  ) * factor * VBNZCONV
cblk ( VVBNZ2 ) = MAX( CONMIN, cblkp(VVBNZ2) )
cblkp( VVTRP1 ) = conc( LVTRP1  ) * factor * VTRPCONV
cblk ( VVTRP1 ) = MAX( CONMIN, cblkp(VVTRP1) )
cblkp( VVTRP2 ) = conc( LVTRP2  ) * factor * VTRPCONV
cblk ( VVTRP2 ) = MAX( CONMIN, cblkp(VVTRP2) )
cblkp( VVISO1 ) = conc( LVISO1  ) * factor * VISOCONV
cblk ( VVISO1 ) = MAX( CONMIN, cblkp(VVISO1) )
cblkp( VVISO2 ) = conc( LVISO2  ) * factor * VISOCONV
cblk ( VVISO2 ) = MAX( CONMIN, cblkp(VVISO2) )
cblkp( VVSQT  ) = conc( LVSQT  ) * factor * VSQTCONV
cblk ( VVSQT  ) = MAX( CONMIN, cblkp(VVSQT) )

! *** Fetch gas-phase production rates.

! *** Initialize secondary organics products
orgprod = 0.0

if ( dt > DTMIN ) then
! *** sulfate
! *** sulfate (ug/m3/s)
  so4rate = H2SO4CONV * factor * conc( LSULFP ) / dt

! *** Secondary organic products
  if ( LALKRXN  > 0 ) orgprod( 1  ) = conc( LALKRXN  )
  if ( LXYLNRXN > 0 ) orgprod( 2  ) = conc( LXYLNRXN )
  if ( LXYLHRXN > 0 ) orgprod( 3  ) = conc( LXYLHRXN )
  if ( LTOLNRXN > 0 ) orgprod( 4  ) = conc( LTOLNRXN )
  if ( LTOLHRXN > 0 ) orgprod( 5  ) = conc( LTOLHRXN )
  if ( LBNZNRXN > 0 ) orgprod( 6  ) = conc( LBNZNRXN )
  if ( LBNZHRXN > 0 ) orgprod( 7  ) = conc( LBNZHRXN )
  if ( LTRPRXN  > 0 ) orgprod( 8  ) = conc( LTRPRXN  )
  if ( LISOPRXN > 0 ) orgprod( 9  ) = conc( LISOPRXN )
  if ( LSESQRXN > 0 ) orgprod( 10 ) = conc( LSESQRXN )

else

  so4rate = 0.

end if

dt = MAX( DTMIN, dt )

! Calculate plume/total ratios before aerosol module. Account for
! molecular weights since concs in cblk are in mass units.

! inorganic species
! Total NOy
! All NOy species expressed as HNO3
plumeconc = cblkp( VNO2 ) * NO2RAT + cblkp( VHONO ) * HONORAT + &
            2.*cblkp( VN2O5 ) * N2O5RAT + cblkp( VHNO3 ) + &
            ( cblkp( VNO3AJ ) + cblkp( VNO3AI ) ) * NO3RAT
totalconc = cblk( VNO2 ) * NO2RAT + cblk( VHONO ) * HONORAT + &
            2.*cblk( VN2O5 ) * N2O5RAT + cblk( VHNO3 ) + &
            ( cblk( VNO3AJ ) + cblk( VNO3AI ) )* NO3RAT
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rnoy = plumeconc/totalconc
omrnoy = 1. - rnoy

rna = (cblkp(VNAJ) + cblkp(VNAI)) / (cblk(VNAJ) + cblk(VNAI))
rna = MAX( 0., rna )
omrna = 1. - rna

rcl   = (cblkp( VHCL ) + (cblkp( VCLJ ) + cblkp( VCLI ))*HCLRAT) / &
        (cblk( VHCL ) + (cblk( VCLJ ) + cblk( VCLI ))*HCLRAT)
rcl = MAX( 0., rcl )
omrcl = 1. - rcl

! For sulfate, also account for H2SO4 production in plume
plumeconc = cblkp( VSULF ) + so4rate*dt + &
           (cblkp( VSO4AJ ) + cblkp( VSO4AI ))* H2SO4RAT
totalconc = cblk( VSULF ) + so4rate*dt + &
           (cblk( VSO4AJ ) + cblk( VSO4AI ))* H2SO4RAT
plumeconc = MAX( 0., plumeconc )
totalconc = MAX( CONMIN, totalconc )
rso4 = plumeconc/totalconc
omrso4 = 1. - rso4

! number concentrations and surface area
! better to do this in post-processor if number concs required
!rnumat = cblkp(VAT0)/cblk(VAT0)
!rnumac = cblkp(VAC0)/cblk(VAC0)
!rnumco = cblkp(VCO0)/cblk(VCO0)
!rsurfat = cblkp(VSURFAT)/cblk(VSURFAT)
!rsurfac = cblkp(VSURFAC)/cblk(VSURFAC)
!rsurfco = cblkp(VSURFCO)/cblk(VSURFCO)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     call aerosol process routines

! Assign dummy variables (not used) for consistency with CMAQ aeroproc
col = 1
row = 1
layer = 1
airdens = ( patm * MWAIR ) / ( tk * RGAS )
xlm = 6.6328E-8 * tk  / ( T0 * patm )
amu = 1.458e-6 * tk * SQRT( tk ) / ( tk + 110.4 )

call aeroproc( NSPCSDA, cblk, dt, col, row, layer, &
               tk, pres_pa, airdens, rh, &
               so4rate, orgprod, NPSPCS, &
               xlm, amu, &
               dgatk, dgacc, dgcor, &
               xxlsgat, xxlsgac, xxlsgco, &
               pmassat, pmassac, pmassco, &
               pdensat, pdensac, pdensco, GAMMA_N2O5, lun_log )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! *** Calculate 2nd and 3rd moments of the "dry" aerosol distribution
!     NOTE! "dry" aerosol excludes both H2O and SOA  (January 2004 --SJR)
!     EXCEPT!  nonvolatile SOA is part of dry aerosol (Oct 2007 --PVB)

! Aitken mode.
m3_wet = MAX( CONMIN, cblk( VAT3 ) )
m3subt = H2OFAC * cblk( VH2OAI )
m3_dry = MAX( CONMIN, m3_wet - m3subt )

m2_wet = cblk( VAT2 )
m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VAT3 ) = MAX( CONMIN, m3_dry )
cblk( VAT2 ) = MAX( CONMIN, m2_dry )

! accumulation mode.
m3_wet = MAX( CONMIN, cblk( VAC3 ) )
m3subt = H2OFAC * cblk( VH2OAJ ) &
       + ORGFAC * cblk( VALKJ  ) &
       + ORGFAC * cblk( VXYL1J ) &
       + ORGFAC * cblk( VXYL2J ) &
       + ORGFAC * cblk( VTOL1J ) &
       + ORGFAC * cblk( VTOL2J ) &
       + ORGFAC * cblk( VBNZ1J ) &
       + ORGFAC * cblk( VBNZ2J ) &
       + ORGFAC * cblk( VTRP1J ) &
       + ORGFAC * cblk( VTRP2J ) &
       + ORGFAC * cblk( VISO1J ) &
       + ORGFAC * cblk( VISO2J ) &
       + ORGFAC * cblk( VSQTJ  )
m3_dry = MAX( CONMIN, m3_wet - m3subt )

m2_wet = cblk( VAC2 )
m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VAC3 ) = MAX( CONMIN, m3_dry )
cblk( VAC2 ) = MAX( CONMIN, m2_dry )

!     coarse mode
m3_wet = MAX( CONMIN, cblk( VCO3 ) )
m3subt = h2ofac * cblk( VH2OK )
m3_dry = MAX( CONMIN, m3_wet - m3subt )
m2_wet = cblk( VCO2 )

m2_dry = m2_wet * ( m3_dry / m3_wet ) ** TWO3

cblk( VCO3 ) = MAX( CONMIN, m3_dry )
cblk( VCO2 ) = MAX( CONMIN, m2_dry )

! *** Calculate geometric mean diameters and standard deviations of the
!     "dry" size distribution

call getpar( NSPCSDA, cblk, pmassat, pmassac, pmassco, &
             pdensat, pdensac, pdensco, &
             dgatk, dgacc, dgcor, &
             xxlsgat, xxlsgac, xxlsgco, &
             M3_WET_FLAG, LIMIT_Sg )

! *** Calculate aerosol surface area from the dry 2nd moment.  Dry value is
!     used in transport routines.
cblk( VSURFAT ) = PI * cblk( VAT2 )
cblk( VSURFAC ) = PI * cblk( VAC2 )
cblk( VSURFCO ) = PI * cblk( VCO2 )

! *** Transfer new gas and aerosol information from CBLK to CONC
! Inorganic Species
cblkp( VNO2 ) = rnoy * cblk( VNO2 )
cblka( VNO2 ) = omrnoy * cblk( VNO2 )

cblkp( VN2O5 ) = rnoy * cblk( VN2O5 )
cblka( VN2O5 ) = omrnoy * cblk( VN2O5 )

cblkp( VHONO ) = rnoy * cblk( VHONO )
cblka( VHONO ) = omrnoy * cblk( VHONO )

cblkp( VHNO3 ) = rnoy * cblk( VHNO3 )
cblka( VHNO3 ) = omrnoy * cblk( VHNO3 )

cblkp( VNO3AJ ) = rnoy * cblk( VNO3AJ )
cblka( VNO3AJ ) = omrnoy * cblk( VNO3AJ )
cblkp( VNO3AI ) = rnoy * cblk( VNO3AI )
cblka( VNO3AI ) = omrnoy * cblk( VNO3AI )

cblkp( VSULF ) = rso4 * cblk( VSULF )
cblka( VSULF ) = omrso4 * cblk( VSULF )

cblkp( VSO4AJ ) = rso4 * cblk( VSO4AJ )
cblka( VSO4AJ ) = omrso4 * cblk( VSO4AJ )
cblkp( VSO4AI ) = rso4 * cblk( VSO4AI )
cblka( VSO4AI ) = omrso4 * cblk( VSO4AI )

cblkp( VNAJ ) = rna * cblk( VNAJ )
cblka( VNAJ ) = omrna * cblk( VNAJ )
cblkp( VNAI ) = rna * cblk( VNAI )
cblka( VNAI ) = omrna * cblk( VNAI )

cblkp( VHCL ) = rcl * cblk( VHCL )
cblka( VHCL ) = omrcl * cblk( VHCL )

cblkp( VCLJ ) = rcl * cblk( VCLJ )
cblka( VCLJ ) = omrcl * cblk( VCLJ )
cblkp( VCLI ) = rcl * cblk( VCLI )
cblka( VCLI ) = omrcl * cblk( VCLI )

! Organic Species
cblkp( VVALK ) = cblk( VVALK )
cblkp( VALKJ ) = cblk( VALKJ )

cblkp( VVXYL1 ) = cblk( VVXYL1 )
cblkp( VXYL1J ) = cblk( VXYL1J )

cblkp( VVXYL2 ) = cblk( VVXYL2 )
cblkp( VXYL2J ) = cblk( VXYL2J )

cblkp( VXYL3J ) = cblk( VXYL3J )

cblkp( VVTOL1 ) = cblk( VVTOL1 )
cblkp( VTOL1J ) = cblk( VTOL1J )

cblkp( VVTOL2 ) = cblk( VVTOL2 )
cblkp( VTOL2J ) = cblk( VTOL2J )

cblkp( VTOL3J ) = cblk( VTOL3J )

cblkp( VVBNZ1 ) = cblk( VVBNZ1 )
cblkp( VBNZ1J ) = cblk( VBNZ1J )

cblkp( VVBNZ2 ) = cblk( VVBNZ2 )
cblkp( VBNZ2J ) = cblk( VBNZ2J )

cblkp( VBNZ3J ) = cblk( VBNZ3J )

cblkp( VVISO1 ) = cblk( VVISO1 )
cblkp( VISO1J ) = cblk( VISO1J )

cblkp( VVISO2 ) = cblk( VVISO2 )
cblkp( VISO2J ) = cblk( VISO2J )

cblkp( VISO3J ) = cblk( VISO3J )

cblkp( VVSQT )  = cblk( VVSQT )
cblkp( VSQTJ )  = cblk( VSQTJ )

cblkp( VOLGAJ ) = cblk( VOLGAJ )

cblkp( VOLGBJ ) = cblk( VOLGBJ )

do spc = 1, naerp
   concamb(cblk_map(spc)) = MAX( cblka(spc), AECONMIN(spc) )
   conc(cblk_map(spc)) = MAX( cblkp(spc), -concamb(spc) )
end do

! *** Transfer new gas/vapor concentrations from CBLK to CONC
factor1 = 1. / factor

concamb( LSULF ) = MAX( CONMIN, cblka( VSULF ) * factor1 * H2SO4CONV1 )
conc( LSULF ) = MAX( cblkp( VSULF ) * factor1 * H2SO4CONV1, -concamb(LSULF) )

concamb( LHNO3 ) = MAX( CONMIN, cblka( VHNO3 ) * factor1 * HNO3CONV1  )
conc( LHNO3 ) = MAX( cblkp( VHNO3 ) * factor1 * HNO3CONV1, -concamb(LHNO3) )

concamb( LNH3 ) = MAX( CONMIN, cblka( VNH3 ) * factor1 * NH3CONV1  )
conc( LNH3 ) = MAX( cblkp( VNH3 ) * factor1 * NH3CONV1, -concamb(LNH3) )

concamb( LHCL ) = MAX( CONMIN, cblka( VHCL ) * factor1 * HCLCONV1  )
conc( LHCL ) = MAX( cblkp( VHCL ) * factor1 * HCLCONV1, -concamb(LHCL) )

concamb( LN2O5 ) = MAX( CONMIN, cblka( VN2O5 ) * factor1 * N2O5CONV1  )
conc( LN2O5 ) = MAX( cblkp( VN2O5 ) * factor1 * N2O5CONV1, -concamb(LN2O5) )

concamb( LNO2 ) = MAX( CONMIN, cblka( VNO2 ) * factor1 * NO2CONV1  )
conc( LNO2 ) = MAX( cblkp( VNO2 ) * factor1 * NO2CONV1, -concamb(LNO2) )

concamb( LHONO ) = MAX( CONMIN, cblka( VHONO ) * factor1 * HONOCONV1  )
conc( LHONO ) = MAX( cblkp( VHONO ) * factor1 * HONOCONV1, -concamb(LHONO) )

conc( LVALK ) = MAX( cblkp( VVALK ) * factor1 * VALKCONV1, -concamb(LVALK) )
conc( LVXYL1 ) = MAX( cblkp( VVXYL1 ) * factor1 * VXYLCONV1, -concamb(LVXYL1) )
conc( LVXYL2 ) = MAX( cblkp( VVXYL2 ) * factor1 * VXYLCONV1, -concamb(LVXYL2) )
conc( LVTOL1 ) = MAX( cblkp( VVTOL1 ) * factor1 * VTOLCONV1, -concamb(LVTOL1) )
conc( LVTOL2 ) = MAX( cblkp( VVTOL2 ) * factor1 * VTOLCONV1, -concamb(LVTOL2) )
conc( LVBNZ1 ) = MAX( cblkp( VVBNZ1 ) * factor1 * VBNZCONV1, -concamb(LVBNZ1) )
conc( LVBNZ2 ) = MAX( cblkp( VVBNZ2 ) * factor1 * VBNZCONV1, -concamb(LVBNZ2) )
conc( LVTRP1 ) = MAX( cblkp( VVTRP1 ) * factor1 * VTRPCONV1, -concamb(LVTRP1) )
conc( LVTRP2 ) = MAX( cblkp( VVTRP2 ) * factor1 * VTRPCONV1, -concamb(LVTRP2) )
conc( LVISO1 ) = MAX( cblkp( VVISO1 ) * factor1 * VISOCONV1, -concamb(LVISO1) )
conc( LVISO2 ) = MAX( cblkp( VVISO2 ) * factor1 * VISOCONV1, -concamb(LVISO2) )
conc( LVSQT ) = MAX( cblkp( VVSQT ) * factor1 * VSQTCONV1, -concamb(LVSQT) )

! *** Zero out species representing the contributions to production
!     of aerosols.
concamb( LSULFP )  = 0.0
conc( LSULFP )  = 0.0

if( LALKRXN  > 0 ) conc( LALKRXN  ) = 0.
if( LXYLNRXN > 0 ) conc( LXYLNRXN ) = 0.
if( LXYLHRXN > 0 ) conc( LXYLHRXN ) = 0.
if( LTOLNRXN > 0 ) conc( LTOLNRXN ) = 0.
if( LTOLHRXN > 0 ) conc( LTOLHRXN ) = 0.
if( LBNZNRXN > 0 ) conc( LBNZNRXN ) = 0.
if( LBNZHRXN > 0 ) conc( LTRPRXN  ) = 0.
if( LISOPRXN > 0 ) conc( LISOPRXN ) = 0.
if( LSESQRXN > 0 ) conc( LSESQRXN ) = 0.

return
end
