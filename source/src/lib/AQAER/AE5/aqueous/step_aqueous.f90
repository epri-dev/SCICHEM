!***********************************************************************
! This is the SCICHEM driver routine for the RADM aqueous-phase        *
! chemistry module in CMAQ (October 2004 version)                      *
! This code has been developed for Southern Company Services under     *
! subcontract to EPRI, 3412 Hillview Ave., Palo Alto, CA 94304         *
! Contract EP-P14638/C7185                                             *
!                                                                      *
! Developed by Prakash Karamchandani                                   *
! Atmospheric and Environmental Research, Inc., 2682 Bishop Drive,     *
! Suite 120, San Ramon, CA 94583                                       *
!                                                                      *
! Updated Feb 2008 to include scavenging of species that do not        *
! participate in aqueous chemistry (for consistency with CMAQ call to  *
! SCAVWDEP, but done here without a separate call); PK, AER            *
!                                                                      *!
!                                                                      *
! CMAQ 4.6 compatibility updates, Feb 2008, PK, AER                    *
! CMAQ 4.7.1 compatibility updates, Feb 2012, PK, ENVIRON              *
! Additional updates for SCICHEM Beta-2 release, Nov 2013, PK, ENVIRON *
!***********************************************************************
subroutine step_aqueous(dt,concamb,conc,lscale,scav)

use aqueous_consts_inc
use aqueous_species_inc
use AqAer_fi, LNH3P => LNH3, LHNO3P => LHNO3, LN2O5P => LN2O5
USE AERO_INFO, ONLY: MWAIR, STDATMPA
!USE mpi_fi, ONLY: myid

implicit none

! --- ARGUMENTS

real dt              ! time step (seconds)
real conc(*)         ! array of species concentrations
real concamb(*)      ! array of species ambient concentrations

! --- length scale (for scavenging coefficient calculations) (m)
real lscale

real scav(*)         ! scavenging coefficients

! --- Local variables
real fpn             ! fraction of precip. that is non-convective

! --- Minimum cloud water content for aqueous calculations
REAL, PARAMETER :: LWCMIN = 0.00001   ! kg/m3

! --- Minimum time step (for scavenging coefficient calculations)
REAL, PARAMETER :: DTMIN = 1.E-20

! --- Minimum cloud cover fraction when non-negligible cloud water is present
REAL, PARAMETER :: FCCMIN = 0.05

integer i,j, spc, var  ! loop variables

real factorg, factorp, factorn
real lwckg

real          alfa                ! scavenging coefficient (1/s)
real          kh                  ! Henry's law constant (mol/l/atm)

real alfa0       ! scavenging coefficient for number [ 1/s ]
real alfa2       ! scavenging coefficient for surface area [ 1/s ]
real alfa3       ! scavenging coefficient for mass [1/s]

real rhoairkg    ! Air density (kg/m3)

real pbar        ! Pressure in Pa

integer, save :: n_numakn            ! # aitken aerosol number species
integer, save :: n_masakn            ! # aitken aerosol mass species
integer, save :: n_srfakn            ! # aitken aerosol sfc area species

real          numakn              ! Aitken mode aerosol # (#/m3)
real          masakn              ! Total Aitken mode mass (ug/m3)
real          srfakn              ! Aitken mode total surface area

Integer :: ios
CHARACTER( 16 ) :: spname
integer       iaer                ! aerosol loop counter
integer       igas                ! gas loop counter
integer       isrg                ! surrogate loop counter
integer       pntr                ! relative pointer variable

real  :: gas    ( NGAS )          ! gas phase conc (mol/mol)
real  :: aerosol( NAER )          ! aerosol conc (mol/mol)

! Weights for surrogate species
REAL          WSRGGAS( NGAS, NSPCS )  ! weights for surrogate gases
REAL          WSRGAER( NAER, NSPCS )  ! weights for surrogate aerosols

real          rtch          ! chemical gas const times temp (liter atm/mol)
real          twash         ! washout time for clouds (sec) with low liq wat content
real          one_over_twash      ! 1 / twash
real          twf                 ! washout scaling factor (mol/l/atm)

real,   parameter :: HPLUS = 1.0E-4   ! typical value hydrogen ion concentration [mol/l]

real :: hplusaq  ! Actual hydrogen ion conc from aqchem [mol/l]

CHARACTER*16  BKGROUND            ! temporary background var string
CHARACTER*16  BKUNITS             ! temporary background units string

real :: chno3p, chno3a, cn2o5p, cn2o5a, cso2p, cso2a, co3p, co3a, ch2o2p, &
        ch2o2a, cmhpp, cmhpa, cfoap, cfoaa, cpaap, cpaaa, cmglyp, cmglya, &
        csulfp, csulfa, chno3, cn2o5, cso2, co3, ch2o2, cmhp, cfoa, cpaa, &
        cmgly, csulf
real :: cano3ip, cano3ia, cano3jp, cano3ja, caorgcjp, &
        caso4ip, caso4ia, caso4jp, caso4ja, caorgcja, &
        cano3i, cano3j, caso4i, caso4j, caorgcj
real :: rnoy, rsox, ro3, rh2o2, rmhp, rfoa, rpaa, rorgc
real :: omrnoy, omrsox, omrorgc
real :: omfcc

integer :: vhno3 = 0, vn2o5 = 0, vso2 = 0, vo3 = 0, vh2o2 = 0, &
           vmhp = 0, vfoa = 0, vpaa = 0, vmgly = 0, vsulf = 0
integer :: vaso4i = 0, vaso4j = 0, vano3i = 0, vano3j = 0, vaorgcj = 0

logical, save :: FIRSTTIME = .TRUE.

!...........External Functions:

INTEGER, EXTERNAL :: AqAerWarningMessage
INTEGER, EXTERNAL :: INDEXN
REAL,    EXTERNAL :: HLCONST

if (FIRSTTIME) then

  FIRSTTIME = .FALSE.

! Set species mapping to aqueous chemistry module species

  conc2aq_map = 0
! Reactive gases
  spc = 0
outer1: do i = 1, N_GC_G2AQ
    spc = spc + 1
    spname = GC_SPC(GC_G2AQ_MAP(i))
    conc2aq(spc) = GC_G2AQ(i)
! Find matching species
    do j = 1, naqueous
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        conc2aq_map(spc) = j
        CYCLE outer1  ! go to next aqueous chemistry species
      end if
    end do
    error%nerror = UK_ERROR
    error%eRoutine = 'step_aqueous'
    error%eMessage = 'No match for ' // spname
    return
  end do outer1

! Aerosols
outer2: do i = 1, N_AE_A2AQ
    spc = spc + 1
    spname = AE_SPC(AE_A2AQ_MAP(i))
    conc2aq(spc) = AE_A2AQ(i)
! Find matching species
    do j = 1, naqueous
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        conc2aq_map(spc) = j
        CYCLE outer2  ! go to next aqueous chemistry species
      end if
    end do
    error%nerror = UK_ERROR
    error%eRoutine = 'step_aqueous'
    error%eMessage = 'No match for ' // spname
    return
  end do outer2

! Non-reactives
outer3: do i = 1, N_NR_N2AQ
    spc = spc + 1
    spname = NR_SPC(NR_N2AQ_MAP(i))
    conc2aq(spc) = NR_N2AQ(i)
! Find matching species
    do j = 1, naqueous
      if (TRIM(aqueous_names(j)) == TRIM(spname)) then
        conc2aq_map(spc) = j
        CYCLE outer3  ! go to next aqueous chemistry species
      end if
    end do
    error%nerror = UK_ERROR
    error%eRoutine = 'step_aqueous'
    error%eMessage = 'No match for ' // spname
    return
  end do outer3

  !do i = 1, N_CONC2AQ
  !  WRITE(99+myid,*)'map for aqueous species',i,' = ',conc2aq_map(i)
  !end do
!debug

! pointers for reacting species
  do j = 1, naqueous
    SELECT CASE(TRIM(aqueous_names(j)))

      CASE('HNO3')
        vhno3 = j
      CASE('N2O5')
        vn2o5 = j
      CASE('SO2')
        vso2 = j
      CASE('O3')
        vo3 = j
      CASE('H2O2')
        vh2o2 = j
      CASE('MEPX')
        vmhp = j
      CASE('FACD')
        vfoa = j
      CASE('PACD')
        vpaa = j
      CASE('MGLY')
        vmgly = j
      CASE('SULF')
        vsulf = j
      CASE('ASO4J')
        vaso4j = j
      CASE('ASO4I')
        vaso4i = j
      CASE('ANO3J')
        vano3j = j
      CASE('ANO3I')
        vano3i = j
      CASE('AORGCJ')
        vaorgcj = j
      CASE DEFAULT
    END SELECT
  end do
! for debugging only
! for debugging only
! Make sure all pointers are set
  if (vhno3 == 0 .or. vn2o5 == 0 .or. vso2 == 0 .or. vo3 == 0 .or. &
      vh2o2 == 0 .or. vmhp == 0 .or. vfoa == 0 .or. vpaa == 0 .or. &
      vmgly == 0 .or. vsulf == 0 .or. vaso4j == 0 .or. vaso4i == 0 .or. &
      vano3j == 0 .or. vano3i == 0) then
    error%nerror = UK_ERROR
    error%eRoutine = 'step_aqueous'
    error%eMessage = 'Required species missing '
    return
  end if

! Now do mapping for scavenged species
  if ( N_CONC_SCAV <= 0 ) then
    error%nError = WN_ERROR
    error%eRoutine = 'step_aqueous'
    error%eMessage = 'No species were specified for scavenging by cloud ' // &
                'or rain water...SCAVENGING WILL NOT BE PERFORMED!'

    error%eAction  = CHAR(0)
    ios = AqAerWarningMessage('')
  else
!... load the CONC to scavenged species pointers
    conc_scav_map = 0
    conc_scav_fac = 0
! Reactive gases
    spc = 0
outer4:   do i = 1, N_GC_SCAV
      spc = spc + 1
      spname = GC_SPC(GC_SCAV_MAP(i))
      conc_scav(spc) = GC_SCAV(i)
      conc_scav_fac(spc) = GC_SCAV_FAC(i)
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          conc_scav_map(spc) = j
          CYCLE outer4  ! go to next scavenged species
        end if
      end do
      error%nerror = UK_ERROR
      error%eRoutine = 'step_aqueous'
      error%eMessage = 'No match for ' // spname
      return
    end do outer4

! Aerosols
outer5:   do i = 1, N_AE_SCAV
      spc = spc + 1
      spname = AE_SPC(AE_SCAV_MAP(i))
      conc_scav(spc) = AE_SCAV(i)
      conc_scav_fac(spc) = AE_SCAV_FAC(i)
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          conc_scav_map(spc) = j
          CYCLE outer5  ! go to next scavenged species
        end if
      end do
      error%nerror = UK_ERROR
      error%eRoutine = 'step_aqueous'
      error%eMessage = 'No match for ' // spname
      return
    end do outer5

! Non-reactives
outer6:   do i = 1, N_NR_SCAV
      spc = spc + 1
      conc_scav(spc) = NR_SCAV(i)
      conc_scav_fac(spc) = NR_SCAV_FAC(i)
      spname = NR_SPC(NR_SCAV_MAP(i))
! Find matching species
      do j = 1, naqueous
        if (TRIM(aqueous_names(j)) == TRIM(spname)) then
          conc_scav_map(spc) = j
          CYCLE outer6  ! go to next scavenged species
        end if
      end do
      error%nerror = UK_ERROR
      error%eRoutine = 'step_aqueous'
      error%eMessage = 'No match for ' // spname
      return
    end do outer6

!...create the pointers from CONC to the gaseous species needed by AQCHEM
    do igas = 1, NGAS
      nsrggas( igas ) = INDEXN( sgrgas( igas ), N_CONC2AQ, &
                                conc2aq, lsrg )
      if ( nsrggas( igas ) == 0 ) then
        write ( BKGROUND, '(1PE8.2)' ) bgndgas( igas )
        bkunits = buntsgas( igas )
        error%nError = WN_ERROR
        error%emessage = 'No surrogates specified for aqueous species ' // &
                    TRIM( sgrgas( igas ) ) // &
                   '...Using background value of ' // &
                    TRIM( bkground ) // ' ' // &
                    TRIM( bkunits )
        error%eAction  = CHAR(0)
        ios = AqAerWarningMessage('')
      else
        do isrg = 1, nsrggas( igas )
          lsrggas( igas, isrg ) = lsrg( isrg )
        end do
      end if
    end do

!...create the pointers from CONC to the aerosol species needed by AQCHEM
    do iaer = 1, NAER
      nsrgaer( iaer ) = INDEXN( sgraer( iaer ), N_CONC2AQ, &
                                conc2aq, lsrg )
      if ( nsrgaer( iaer ) == 0 ) then
        write ( BKGROUND, '(1PE8.2)' ) bgndaer( iaer )
        bkunits = buntsaer( iaer )
        error%emessage = 'No surrogates specified for aqueous species ' // &
                    TRIM( sgraer(iaer) ) // &
                   '...Using background value of ' // &
                    TRIM( bkground ) // ' ' // &
                    TRIM( bkunits  )
        error%eAction  = CHAR(0)
        ios = AqAerWarningMessage('')
      else
        do isrg = 1, nsrgaer( iaer )
          lsrgaer( iaer, isrg ) = lsrg( isrg )
        end do
      end if
    end do

    n_numakn = INDEXN( 'NUM_AITKEN', N_CONC_SCAV, conc_scav, l_numakn )
    n_masakn = INDEXN( 'AITKEN', N_CONC_SCAV, conc_scav, l_masakn )
    n_srfakn = INDEXN( 'SRF_AITKEN', N_CONC_SCAV, conc_scav, l_srfakn )

  end if

end if  !FIRSTTIME

!...for subsequent calls, check to make sure some species are specified
!...for aqueous chemistry or scavenging

if (N_CONC2AQ == 0 .and. N_CONC_SCAV == 0) then
  return
end if

lwckg = lwc * 1.E-3  ! g/m3 water to kg/m3

! --- Scavenging coefficient calculations for Aitken mode variables
! --- Aitken mode number concentration (#/m3)
numakn = 0.0
do i = 1, n_numakn
  pntr = conc_scav_map(l_numakn(i))
  numakn = numakn + conc(pntr) + concamb(pntr)
end do

! --- Aitken mode surface area (m2/m3)
srfakn = 0.0
do i = 1, n_srfakn
  pntr = conc_scav_map(l_srfakn(i))
  srfakn = srfakn + conc(pntr) + concamb(pntr)
end do

! --- Total Aitken mode mass (ug/m3)
masakn = 0.0
do i = 1, n_masakn
  pntr = conc_scav_map(l_masakn(i))
  if ( (index(conc_scav(l_masakn(i)), 'NUM') == 0 ) .and. &
       (index(conc_scav(l_masakn(i)), 'SRF') == 0 ) .and. &
       (index(conc_scav(l_masakn(i)), 'H2O') == 0 ) ) then
    masakn = masakn + conc(pntr) + concamb(pntr)
  end if
end do

rhoair = patm * 1.E3 / (RGAS * tk)   ! Air density in moles/m3
rhoairkg = rhoair * MWAIR * 1.E-3    ! Air density in kg/m3
pbar = patm * STDATMPA               ! Pressure in Pa

! --- calculate in-cloud scavenging coefficients
call getalpha (numakn, masakn, srfakn, lwckg, tk, pbar, &
               rhoairkg, alfa0, alfa2, alfa3 )

! Aqueous-phase chemistry
if (lwckg > LWCMIN) then

!
! --- Set conversion factors for gas-phase species concentrations
! --- (convert to moles/mole of air)
  if (ic_units == UNIT_PPM) then
    factorg = 1.0E-06 * tk / ( 298. * patm )
  else if (ic_units == UNIT_MOLECULE) then
    factorg = 1.3634E-22 * tk / patm
  end if

! --- Set conversion factors for particle-phase species concentrations
! --- (convert to moles/mole of air)
  factorp = 1.E-6 / rhoair

! --- Set conversion factors for particle number and surface area
! --- concentrations (convert to #/mole of air and m2/mole of air)
  factorn = 1. / rhoair

!...load gas-phase concentrations (mol/mol air)
! ... First calculate plume and ambient concs of reactive species
  chno3p = conc(vhno3)*factorg
  chno3a = concamb(vhno3)*factorg
  chno3 = MAX(CONMIN,chno3p+chno3a)

  cn2o5p = conc(vn2o5)*factorg
  cn2o5a = concamb(vn2o5)*factorg
  cn2o5 = MAX(CONMIN,cn2o5p+cn2o5a)

  cso2p = conc(vso2)*factorg
  cso2a = concamb(vso2)*factorg
  cso2 = MAX(CONMIN,cso2p+cso2a)

  co3p = conc(vo3)*factorg
  co3a = concamb(vo3)*factorg
  co3 = MAX(CONMIN,co3p+co3a)

  ch2o2p = conc(vh2o2)*factorg
  ch2o2a = concamb(vh2o2)*factorg
  ch2o2 = MAX(CONMIN,ch2o2p+ch2o2a)

  cmhpp = conc(vmhp)*factorg
  cmhpa = concamb(vmhp)*factorg
  cmhp = MAX(CONMIN,cmhpp+cmhpa)

  cfoap = conc(vfoa)*factorg
  cfoaa = concamb(vfoa)*factorg
  cfoa = MAX(CONMIN,cfoap+cfoaa)

  cpaap = conc(vpaa)*factorg
  cpaaa = concamb(vpaa)*factorg
  cpaa = MAX(CONMIN,cpaap+cpaaa)

  cmglyp = conc(vmgly)*factorg
  cmglya = concamb(vmgly)*factorg
  cmgly = MAX(CONMIN,cmglyp+cmglya)

  csulfp = conc(vsulf)*factorg
  csulfa = concamb(vsulf)*factorg
  csulf = MAX(CONMIN,csulfp+csulfa)

  gas = 0.0
  wsrggas = 0.0
  do igas = 1, NGAS

    do isrg = 1, nsrggas(igas)
      pntr = conc2aq_map(lsrggas(igas,isrg))
      gas(igas) = gas(igas) + MAX((conc(pntr)+concamb(pntr))*factorg,CONMIN)
    end do
    if (gas(igas) > 0.0) then
      do isrg = 1, nsrggas(igas)
        pntr = conc2aq_map(lsrggas(igas,isrg))
        wsrggas(igas,isrg) = MAX((conc(pntr)+concamb(pntr))*factorg,CONMIN) / gas(igas)
      end do
    else
      do isrg = 1, nsrggas(igas)
        wsrggas(igas,isrg) = 1.0 / FLOAT(nsrggas(igas))
      end do
    end if

!...set background values for gases if no surrogates were specified

    if (nsrggas(igas) == 0) then
      gas(igas) = bgndgas(igas) * 1.0e-6 ! assumes background is in ppm
    end if

  end do

!...load aerosol concentrations
! ... First calculate plume and ambient concs of reactive species
  cano3ip = conc(vano3i)*factorp / SGRAERMW( LNO3AKN  )
  cano3ia = concamb(vano3i)*factorp / SGRAERMW( LNO3AKN  )
  cano3i  = MAX(CONMIN,cano3ip+cano3ia)

  cano3jp = conc(vano3j)*factorp / SGRAERMW( LNO3ACC  )
  cano3ja = concamb(vano3j)*factorp / SGRAERMW( LNO3ACC  )
  cano3j  = MAX(CONMIN,cano3jp+cano3ja)

  caso4ip = conc(vaso4i)*factorp / SGRAERMW( LSO4AKN  )
  caso4ia = concamb(vaso4i)*factorp / SGRAERMW( LSO4AKN  )
  caso4i  = MAX(CONMIN,caso4ip+caso4ia)

  caso4jp = conc(vaso4j)*factorp / SGRAERMW( LSO4ACC  )
  caso4ja = concamb(vaso4j)*factorp / SGRAERMW( LSO4ACC  )
  caso4j  = MAX(CONMIN,caso4jp+caso4ja)

  caorgcjp = conc(vaorgcj)*factorp / SGRAERMW( LORGCACC  )
  caorgcja = concamb(vaorgcj)*factorp / SGRAERMW( LORGCACC  )
  caorgcj  = MAX(CONMIN,caorgcjp+caorgcja)

! -- plume to total ratios
  rnoy = (chno3p + 2.*cn2o5p + cano3ip + cano3jp) / &
         (chno3  + 2.*cn2o5  + cano3i  + cano3j )
  rnoy = MAX(rnoy,0.)

  rsox = (cso2p + csulfp + caso4ip + caso4jp) / &
         (cso2  + csulf  + caso4i  + caso4j )
  rsox = MAX(rsox,0.)
  omrsox = 1. - rsox

  ro3 = co3p / co3
  rh2o2 = ch2o2p / ch2o2
  rmhp = cmhpp / cmhp
  rfoa = cfoap / cfoa
  rpaa = cpaap / cpaa

  rorgc = (cmglyp*0.04 + caorgcjp) / (cmgly*0.04 + caorgcj)
  rorgc = MAX(rorgc,0.)
  omrorgc = 1. - rorgc

  aerosol = 0.0
  wsrgaer = 0.0

  do iaer = 1, NAER
    do isrg = 1, nsrgaer(iaer)
      pntr = conc2aq_map(lsrgaer(iaer,isrg))
      if (sgraer(iaer)(1:3) /= 'NUM' .AND. &
          sgraer(iaer)(1:3) /= 'SRF' ) then
!...aerosol mass concentrations (mol/mol air)
        aerosol(iaer) = aerosol(iaer) + &
                 MAX((conc(pntr)+concamb(pntr))*factorp/sgraermw(iaer),CONMIN)
      else
!...aerosol no. concentrations and surface area (#/mol air and m2/mol of air)
        aerosol(iaer) = aerosol(iaer) + &
                 MAX((conc(pntr)+concamb(pntr))*factorn,CONMIN)
      end if
    end do
    if (aerosol(iaer) > 0.0) then
      do isrg = 1, nsrgaer(iaer)
        pntr = conc2aq_map(lsrgaer(iaer,isrg))
        if (sgraer(iaer)(1:3) /= 'NUM' .AND. &
            sgraer(iaer)(1:3) /= 'SRF' ) then
           wsrgaer(iaer,isrg ) = MAX((conc(pntr)+concamb(pntr))*factorp / &
                    sgraermw(iaer),CONMIN) / aerosol(iaer)
        else
           wsrgaer(iaer,isrg ) = MAX((conc(pntr)+concamb(pntr))*factorn, &
                                      CONMIN) / aerosol(iaer)
        end if
      end do
    else
      do isrg = 1, nsrgaer(iaer)
        wsrgaer(iaer,isrg) = 1.0 / nsrgaer(iaer)
      end do
    end if

!...set background values for aerosols if no surrogates were specified

    if (nsrgaer(iaer) == 0) then
      if (sgraer(iaer)(1:3) /= 'NUM' .AND. &
          sgraer(iaer)(1:3) /= 'SRF' ) then
        aerosol(iaer) = bgndaer(iaer) * factorp / sgraermw(iaer)
      else
        aerosol(iaer) = bgndaer(iaer) * factorn
      end if
    end if
  end do

  CALL aqradm(tk, patm, dt, lwckg, &
              alfa0, alfa2, alfa3, &
              gas, aerosol, rhoair, hplusaq )

  if (error%nError /= NO_ERROR) go to 9999
!
! --- Assign aqueous-species concentrations back to main concentration array
! --- Gases
!...since the aqueous chemistry routine doesn't know which surrogate
!...  species were specified by the user, several checks must be made
!...  on the surrogate species to make sure that all of the mass is
!...  conserved and doesn't fall through the cracks.

  do igas = 1, NGAS

!...check for the special case for species H2SO4

    if ( igas == LH2SO4 ) then

!...  load sulfate accumulation aerosol mass into the slot for
!...    sulfuric acid gas if there is no slot for it

      if ( nsrgaer( LSO4ACC ) == 0 ) THEN
        gas    ( igas    ) = gas    ( igas ) + aerosol( LSO4ACC )
        aerosol( LSO4ACC ) = 0.0

        if ( nsrgaer( LSO4COR ) == 0 ) then
          gas    ( igas    ) = gas    ( igas ) + aerosol( LSO4COR )
          aerosol( LSO4COR ) = 0.0

          if ( nsrgaer( LSO4AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LSO4AKN )
            aerosol( LSO4AKN ) = 0.0
          end if

        else

          if ( nsrgaer( LSO4AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LSO4AKN )
            aerosol( LSO4AKN ) = 0.0
          end if

        end if
      end if

!..check for the special case for species NH3

    else if ( igas == LNH3 ) then

!...  load ammonium accumulation aerosol mass into the slot for
!...   ammonia gas if there is no slot for it

      if ( nsrgaer( LNH4ACC ) == 0 ) then
        gas    ( igas    ) = gas    ( igas ) + aerosol( LNH4ACC )
        aerosol( LNH4ACC ) = 0.0

!...  load ammonium coarse aerosol mass into the slot for
!...   ammonia gas if there is no slot for it

        if ( nsrgaer( LNH4COR ) == 0 ) then
          gas    ( igas    ) = gas    ( igas ) + aerosol( LNH4COR )
          aerosol( LNH4COR ) = 0.0

!...  load ammonium aitken aerosol mass into the slot for
!...   ammonia gas if there is no slot for it

          if ( nsrgaer( LNH4AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LNH4AKN )
            aerosol( LNH4AKN ) = 0.0
          end if

        else

          if ( nsrgaer( LNH4AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LNH4AKN )
            aerosol( LNH4AKN ) = 0.0
          end if
        end if
      end if

!...check for the special case for species HNO3

    else if ( igas == LHNO3 ) then

!...  add-on any NITRATE mass output by AQCHEM which has no slot in array

      if ( nsrgaer( LNO3ACC ) == 0 ) THEN
        gas    ( igas    ) = gas    ( igas ) + aerosol( LNO3ACC )
        aerosol( LNO3ACC ) = 0.0

        if ( nsrgaer( LNO3COR ) == 0 ) THEN
          gas    ( igas    ) = gas    ( igas ) + aerosol( LNO3COR )
          aerosol( LNO3COR ) = 0.0

          if ( nsrgaer( LNO3AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LNO3AKN )
            aerosol( LNO3AKN ) = 0.0
          end if

        else

          if ( nsrgaer( LNO3AKN ) == 0 ) then
            gas    ( igas    ) = gas    ( igas ) + aerosol( LNO3AKN )
            aerosol( LNO3AKN ) = 0.0
          end if

        end if

      end if

    end if

  end do

  do iaer = 1, NAER

    if ( iaer == LSO4AKN ) then

!...  load sulfuric acid gas and sulfate accumulation aerosol mass into
!...    the slot for sulfate aitken aerosol if there are no slots
!...    for either of those species

      if ( ( nsrggas( LH2SO4  ) == 0 ) .AND. &
           ( nsrgaer( LSO4ACC ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LH2SO4  ) &
                        + aerosol( LSO4ACC )
        gas    ( LH2SO4  ) = 0.0
        aerosol( LSO4ACC ) = 0.0

        if ( nsrgaer( LSO4COR ) == 0 ) then
          aerosol( iaer    ) = aerosol( iaer ) + aerosol( LSO4COR )
          aerosol( LSO4COR ) = 0.0
        end if

      end if

    else if ( iaer == LSO4ACC ) then

!...  load sulfuric acid gas mass into the slot for sulfate accumulation
!...    aerosol if there is no slot for it

      if ( nsrggas( LH2SO4 ) == 0 ) then
        aerosol( iaer   ) = aerosol( iaer ) + gas    ( LH2SO4 )
        gas    ( LH2SO4 ) = 0.0
      end if

!...  load sulfate aitken aerosol mass into the slot for
!...   sulfate accumulation aerosol if there is no slot for it

      if ( nsrgaer( LSO4AKN ) == 0 ) then
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LSO4AKN )
        aerosol( LSO4AKN ) = 0.0
      end if

!...  load sulfate coarse aerosol mass into the slot for
!...   sulfate accumulation aerosol if there is no slot for it

      if ( nsrgaer( LSO4COR ) == 0 ) then
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LSO4COR )
        aerosol( LSO4COR ) = 0.0
      end if

    else if ( iaer == LSO4COR ) then

!...  check for H2SO4, SO4ACC, and SO4AKN in conc...and as
!...    a last resort, put all of the sulfate mass into SO4COR

      if ( ( nsrggas( LH2SO4  ) == 0 ) .AND. &
           ( nsrgaer( LSO4ACC ) == 0 ) .AND. &
           ( nsrgaer( LSO4AKN ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LH2SO4  ) &
                        + aerosol( LSO4AKN ) &
                        + aerosol( LSO4ACC )
        gas    ( LH2SO4  ) = 0.0
        aerosol( LSO4AKN ) = 0.0
        aerosol( LSO4ACC ) = 0.0
      end if

    else if ( iaer == LNH4AKN ) then

!...  load ammonia gas and ammonium accumulation aerosol mass into
!...    the slot for ammonium aitken aerosol if there are no slots
!...    for either of those species in CEND

      if ( ( nsrggas( LNH3    ) == 0 ) .AND. &
           ( nsrgaer( LNH4ACC ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LNH3    ) &
                        + aerosol( LNH4ACC )
        gas    ( LNH3    ) = 0.0
        aerosol( lnh4acc ) = 0.0

        if ( nsrgaer( LNH4COR ) == 0 ) THEN
          aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNH4COR )
          aerosol( LNH4COR ) = 0.0
        end if

      end if

    else if ( iaer == LNH4ACC ) then

!...  load ammonia gas mass into the slot for ammonium accumulation
!...    aerosol if there is no slot for it

      if ( nsrggas( LNH3 ) == 0 ) then
        aerosol( iaer ) = aerosol( iaer ) + gas    ( LNH3 )
        gas    ( LNH3 ) = 0.0
      end if

!...  load ammonium aitken aerosol mass into the slot for
!...   ammonium accumulation aerosol if there is no slot for it

      if ( nsrgaer( LNH4AKN ) == 0 ) THEN
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNH4AKN )
        aerosol( LNH4AKN ) = 0.0
      end if

!...  load ammonium coarse aerosol mass into the slot for
!..   ammonium accumulation aerosol if there is no slot for it

      if ( nsrgaer( LNH4COR ) == 0 ) then
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNH4COR )
        aerosol( LNH4COR ) = 0.0
      end if

    else if ( iaer == LNH4COR ) then

!..  check for NH3, NH4ACC, and NH4AKN in conc...and as
!..    a last resort, put all of the ammonium mass into NH4COR

      if ( ( nsrggas( LNH3   ) == 0) .AND. &
           ( nsrgaer( LNH4ACC ) == 0 ) .AND. &
           ( nsrgaer( LNH4AKN ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LNH3    ) &
                        + aerosol( LNH4AKN ) &
                        + aerosol( LNH4ACC )
        gas    ( LNH3   ) = 0.0
        aerosol( LNH4AKN ) = 0.0
        aerosol( LNH4ACC ) = 0.0

      end if

    else if ( iaer == LNO3AKN ) then

!...  check for HNO3 in conc...and if there then add it mass to HNO3
      if ( ( nsrggas( LHNO3   ) == 0 ) .AND. &
           ( nsrgaer( LNO3ACC ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LHNO3   ) &
                        + aerosol( LNO3ACC )
        gas    ( LHNO3    ) = 0.0
        aerosol( LNO3ACC  ) = 0.0

        if ( nsrgaer( LNO3COR ) == 0 ) then
          aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNO3COR )
          aerosol( LNO3COR ) = 0.0
        end if

      end if

    else if ( iaer == LNO3ACC ) then

!...  check for hno3 in conc...and if there then add it mass to hno3

      if ( nsrggas( LHNO3 ) == 0 ) then
        aerosol( iaer  ) = aerosol( iaer ) + gas    ( LHNO3 )
        gas    ( LHNO3 ) = 0.0
      end if

!...  check for NO3AKN in conc...and if there then add it mass to HNO3

      if ( nsrgaer( LNO3AKN ) == 0 ) then
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNO3AKN )
        aerosol( LNO3AKN ) = 0.0
      end if

!...  check for NO3COR in conc...and if there then add it mass to HNO3

      if ( nsrgaer( LNO3COR ) == 0 ) THEN
        aerosol( iaer    ) = aerosol( iaer ) + aerosol( LNO3COR )
        aerosol( LNO3COR ) = 0.0
      end if

    else if ( iaer == LNO3COR ) then

!...  check for HNO3, NO3ACC, and NO3AKN in conc...and as
!...    a last resort, put all of the nitrate mass into NO3COR

      if ( ( nsrggas( LHNO3   ) == 0 ) .AND. &
           ( nsrgaer( LNO3ACC ) == 0 ) .AND. &
           ( nsrgaer( LNO3AKN ) == 0 ) ) then
        aerosol( iaer ) = aerosol( iaer    ) &
                        + gas    ( LHNO3   ) &
                        + aerosol( LNO3AKN ) &
                        + aerosol( LNO3ACC )
        gas    ( LHNO3   ) = 0.0
        aerosol( LNO3AKN ) = 0.0
        aerosol( LNO3ACC ) = 0.0

      end if

    end if

  end do

!...Now, re-apportion mass back into main array
  factorg = 1. / factorg
  factorp = 1. / factorp
  factorn = 1. / factorn

  omrnoy = 1. - rnoy

! --- Set min. fractional cloud cover when non-negligible cloud water is present
  fcc = MAX(fcc,FCCMIN)
  omfcc = 1. - fcc

  conc(vhno3) = omfcc * conc(vhno3) + fcc * rnoy * gas(LHNO3) * factorg
  concamb(vhno3) = omfcc * concamb(vhno3) + fcc * omrnoy * gas(LHNO3) * factorg

  conc(vn2o5) = omfcc * conc(vn2o5) + fcc * rnoy * gas(LN2O5) * factorg
  concamb(vn2o5) = omfcc * concamb(vn2o5) + fcc * omrnoy * gas(LN2O5) * factorg

  conc(vano3i) = omfcc * conc(vano3i) + fcc * rnoy * aerosol(LNO3AKN) * factorp * SGRAERMW( LNO3AKN  )
  concamb(vano3i) = omfcc * concamb(vano3i) + fcc * omrnoy * aerosol(LNO3AKN) * factorp * SGRAERMW( LNO3AKN  )

  conc(vano3j) = omfcc * conc(vano3j) + fcc * rnoy * aerosol(LNO3ACC) * factorp * SGRAERMW( LNO3ACC  )
  concamb(vano3j) = omfcc * concamb(vano3j) + fcc * omrnoy * aerosol(LNO3ACC) * factorp * SGRAERMW( LNO3ACC  )

  conc(vso2) = omfcc * conc(vso2) + fcc * rsox * gas(LSO2) * factorg
  concamb(vso2) = omfcc * concamb(vso2) + fcc * omrsox * gas(LSO2) * factorg

  conc(vsulf) = omfcc * conc(vsulf) + fcc * rsox * gas(LH2SO4) * factorg
  concamb(vsulf) = omfcc * concamb(vsulf) + fcc * omrsox * gas(LH2SO4) * factorg

  conc(vaso4i) = omfcc * conc(vaso4i) + fcc * rsox * aerosol(LSO4AKN) * factorp * SGRAERMW( LSO4AKN  )
  concamb(vaso4i) = omfcc * concamb(vaso4i) + fcc * omrsox * aerosol(LSO4AKN) * factorp * SGRAERMW( LSO4AKN  )

  conc(vaso4j) = omfcc * conc(vaso4j) + fcc * rsox * aerosol(LSO4ACC) * factorp * SGRAERMW( LSO4ACC  )
  concamb(vaso4j) = omfcc * concamb(vaso4j) + fcc * omrsox * aerosol(LSO4ACC) * factorp * SGRAERMW( LSO4ACC  )

  conc(vo3) = omfcc * conc(vo3) + fcc * ro3 * gas(LO3) * factorg
  concamb(vo3) = omfcc * concamb(vo3) + fcc * (1. - ro3) * gas(LO3) * factorg

  conc(vh2o2) = omfcc * conc(vh2o2) + fcc * rh2o2 * gas(LH2O2) * factorg
  concamb(vh2o2) = omfcc * concamb(vh2o2) + fcc * (1. - rh2o2) * gas(LH2O2) * factorg

  conc(vmhp) = omfcc * conc(vmhp) + fcc * rmhp * gas(LMHP) * factorg
  concamb(vmhp) = omfcc * concamb(vmhp) + fcc * (1. - rmhp) * gas(LMHP) * factorg

  conc(vfoa) = omfcc * conc(vfoa) + fcc * rfoa * gas(LFOA) * factorg
  concamb(vfoa) = omfcc * concamb(vfoa) + fcc * (1. - rfoa) * gas(LFOA) * factorg

  conc(vpaa) = omfcc * conc(vpaa) + fcc * rpaa * gas(LPAA) * factorg
  concamb(vpaa) = omfcc * concamb(vpaa) + fcc * (1. - rpaa) * gas(LPAA) * factorg

  conc(vmgly) = omfcc * conc(vmgly) + fcc * rorgc * gas(LMGLY) * factorg
  concamb(vmgly) = omfcc * concamb(vmgly) + fcc * omrorgc * gas(LMGLY) * factorg

  conc(vaorgcj) = omfcc * conc(vaorgcj) + fcc * rorgc * aerosol(LORGCACC) * factorp * SGRAERMW(LORGCACC)
  concamb(vaorgcj) = omfcc * concamb(vaorgcj) + fcc * omrorgc * aerosol(LORGCACC) * factorp * SGRAERMW(LORGCACC)

!  do igas = 1, NGAS
!    do isrg = 1, nsrggas(igas)
!      pntr = conc2aq_map(lsrggas(igas,isrg))
!      conc(pntr) = gas(igas) * wsrggas(igas,isrg) * factorg
!    end do
!  end do

! --- Particles

!  do iaer = 1, NAER
!    do isrg = 1, nsrgaer(iaer)
!      pntr = conc2aq_map(lsrgaer(iaer,isrg))
!      if (sgraer(iaer)(1:3) /= 'NUM' .AND. &
!          sgraer(iaer)(1:3) /= 'SRF' ) then

!...aerosol mass concentrations (ug/m3 air)
!        conc(pntr) = aerosol(iaer) * wsrgaer(iaer,isrg) * &
!                     factorp * sgraermw(iaer)
!      else
!...aerosol no. concentrations and surface area (#/mol air and m2/mol of air)
!        conc(pntr) = aerosol(iaer) * wsrgaer(iaer,isrg) * &
!                     factorn
!      end if
!    end do
!  end do

else

  hplusaq = hplus

end if

! --- Scavenging coefficients
do i = 1, naqueous
  scav(i) = 0.
end do

if (prate <= 0.0 .OR. lwckg/rhoairkg <= 0.00005 ) return

rtch = (MOLVOL / STDTEMP) * tk
twash = lwckg*1000.0*lscale*3600.0 / ( H2ODENS*MAX( 1.0E-20, prate ) )
twash = MAX( twash, dt )
one_over_twash = 1.0 / twash
twf = H2ODENS / ( lwckg * rtch )

!...gas scavenging

spc = 0

do var = 1, N_GC_SCAV
  spc = spc + 1
  pntr = conc_scav_map( spc )
  kh = HLCONST( conc_scav( spc ), tk, .true., hplusaq )
  if ( kh > 0.0 ) then
    alfa = conc_scav_fac( spc ) * one_over_twash / ( 1.0 + twf / kh )
  else
    alfa = 0.0
  end if
  scav(pntr) = alfa
end do

!...aerosol scavenging
do var = 1, N_AE_SCAV
  spc = spc + 1
  pntr = conc_scav_map( spc )

  if ( INDEX( conc_scav(spc), 'AITKEN' ) > 0 ) then
    if ( INDEX( conc_scav(spc), 'NUM' )  > 0 ) then
      alfa = conc_scav_fac(spc) * alfa0
    else if ( INDEX( conc_scav(spc), 'SRF' ) > 0 ) then
      alfa = conc_scav_fac(spc) * alfa2
    else
      alfa = conc_scav_fac(spc) * alfa3
    end if
  else
   alfa = conc_scav_fac(spc) * one_over_twash
  end if
  scav(pntr) = alfa
end do

!...non-reactive scavenging
do var = 1, N_NR_SCAV
  spc = spc + 1
  pntr = conc_scav_map( spc )
  kh = HLCONST( conc_scav( spc ), tk, .true., hplusaq )
  if ( kh > 0.0 ) then
    alfa = conc_scav_fac( spc ) * one_over_twash / ( 1.0 + twf / kh )
  else
    alfa = 0.0
  end if
  scav(pntr) = alfa
end do

! --- Adjust scavenging coefficents for convective precipitation
fpn = 1. - fpc
do i = 1, naqueous
  scav(i) = ( fpn + fcc * fpc ) * scav(i)
  scav(i) = MIN(scav(i),1.00E-3) ! cap based on Loosmore and Cederwall (2004)
end do

9999  return
end
