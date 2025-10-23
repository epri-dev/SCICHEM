      subroutine aqradm ( temp, pres_atm, taucld, wcavg,
     &                    alfa0, alfa2, alfa3,
     &                    gas, aerosol, rhoair, hplus )

C-----------------------------------------------------------------------
C
C  DESCRIPTION:
C    Compute concentration changes in cloud due to aqueous chemistry
C    Adapted from RADM Cloud implementation in CMAQ for SCICHEM
C    by PK, AER, Feb 2005 for Southern Company and EPRI,
C    EPRI Agreement EP-P14638/C7185
C
C  Reference:
C     Walcek & Taylor, 1986, A theoretical Method for computing
C      vertical distributions of acidity and sulfate within cumulus
C      clouds, J. Atmos Sci.,  Vol. 43, no. 4 pp 339 - 355
C     Carlton, A.G., B.J. Turpin, K.E. Altieri, S.P. Seitzinger, R. Mathur,
C        S.J. Roselle, and R.J. Weber, CMAQ Model Performance Enhanced When
C        In-Cloud Secondary Organic Aerosol is Included:  Comparison of Organic
C        Carbon Predictions with Measurements, Environ. Sci. Technol., 42(23),
C        8798-8802, 2008.
C     Si, L., P.A. Ariya, Reduction of oxidized mercury species by dicarboxylic
C        acids (C2-C4): kinetic and product studies, Environ. Sci. Technol., 42
C        5150-5155
C
C  Called by:  STEP_AQUEOUS
C
C
C  Calls the following functions:  HLCONST
C
C  ARGUMENTS     TYPE      I/O       DESCRIPTION
C  ---------     ----  ------------  --------------------------------
C  GAS(ngas)     real  input&output  Concentration for species i=1,15
C
C  AEROSOL(naer) real input&output   Concentration for species i=1,52
C
C Updated to CMAQ 4.7.1, PK, ENVIRON, Feb 2012
C-----------------------------------------------------------------------
      use error_aqaer_fi
      use aqueous_consts_inc
      use aqueous_species_inc
      use mpi_fi, only: myid

      implicit none

C...........PARAMETERS

      INTEGER, PARAMETER :: NUMOX =  5   ! number of oxidizing reactions

! minimum concentration
      REAL, PARAMETER :: CONCMIN = 1.0E-30

      REAL, PARAMETER :: ONETHIRD = 1.0 / 3.0
      REAL, PARAMETER :: TWOTHIRDS = 2.0 / 3.0

C...........ARGUMENTS and their descriptions

      REAL,      INTENT( IN ) ::  TEMP         ! temperature (K)
      REAL,      INTENT( IN ) ::  PRES_ATM     ! pressure (atm)
      REAL,      INTENT( IN ) ::  TAUCLD       ! timestep for cloud (s)
      REAL,      INTENT( IN ) ::  WCAVG        ! liquid water content (kg/m3)
      REAL,      INTENT( IN ) ::  ALFA0        ! scav coef for aitken aerosol number
      REAL,      INTENT( IN ) ::  ALFA2        ! scav coef for aitken aerosol sfc area
      REAL,      INTENT( IN ) ::  ALFA3        ! scav coef for aitken aerosol mass

      REAL, INTENT( INOUT ) :: GAS    ( NGAS )         ! gas phase concentrations (mol/molV)
      REAL, INTENT( INOUT ) :: aerosol( NAER ) ! aerosol concentrations (mol/molV)

      real,      INTENT( IN )  ::  rhoair  ! air density, moles/m3
      real,      INTENT( OUT ) ::  hplus   ! hydrogen ion concentration (mol/l)

C...........LOCAL VARIABLES and their descriptions:
      LOGICAL, SAVE :: FIRSTIME = .TRUE. ! flag for first pass thru

! Henry's law surrogate for MGLY
      CHARACTER(16), SAVE :: MGLYSUR = 'METHYL_GLYOXAL  '

      INTEGER      nError          ! local error value
      INTEGER      ISPC            ! loop counter for species
      INTEGER      I20C            ! loop counter for do loop 20
      INTEGER      I30C            ! loop counter for do loop 30
      INTEGER      ITERAT          ! # iterations of aqueous chemistry solver
      INTEGER      I7777C          ! aqueous chem iteration counter
      INTEGER      ICNTAQ          ! aqueous chem iteration counter
      INTEGER      LIQ             ! loop counter for liquid species
      INTEGER      IGAS            ! loop counter for gas species
      INTEGER      IOX             ! index over oxidation reactions

      REAL         A               ! iron's anion concentration
      REAL         AC              ! H+ concentration in cloudwater (mol/liter)
      REAL         ACT1            ! activity correction factor!single ions
      REAL         ACT2            ! activity factor correction!double ions
      REAL         ACTB            !
      REAL         AE              ! guess for H+ conc in cloudwater (mol/l)

      REAL         B               ! manganese's anion concentration
      REAL         BB              ! lower limit guess of cloudwater pH
      REAL         CA              ! Calcium conc in cloudwater (mol/liter)
      REAL         CAA             ! initial Calcium in cloudwater (mol/liter)
      REAL         CL              ! total Cl-  conc in cloudwater (mol/liter)
C
      REAL         CLACC           ! fine Cl- in cloudwater (mol/liter)
      REAL         CLACCA          ! initial fine Cl in cloudwater (mol/liter)
      REAL         CLAKNA          ! initial interstitial aero Cl (mol/liter)
      REAL         CLCOR           ! coarse Cl-  conc in cloudwater (mol/liter)
      REAL         CO2H            ! Henry's Law constant for CO2
      REAL         CO21            ! First dissociation constant for CO2
      REAL         CO22            ! Second dissociation constant for CO2
      REAL         CO212           ! CO21*CO22
      REAL         CO212H          ! CO2H*CO21*CO22
      REAL         CO21H           ! CO2H*CO21
      REAL         CO2L            ! CO2 conc in cloudwater (mol/liter)
      REAL         CO3             ! CO3= conc in cloudwater (mol/liter)
      REAL         CO3A            ! initial CO3 in cloudwater (mol/liter)
      REAL         DSIV_SCALE      ! mass conservation scale factor for S(IV)
      REAL         DTS6            !

      REAL(8)      DGLYDT          ! change in GLY (mol/liter/sec)
      REAL(8)      DMGLYDT         ! change in MGLY (mol/liter/sec)
      REAL(8)      DGLY1           ! change due to Rxn. in GLY for DTW(0) time step
      REAL(8)      DMGLY1          ! change due to Rxn. in MGLY for DTW(0) time step
      REAL(8)      DORGC           ! change in ORGC for DTW(0) time step

      REAL         EALFA0T         ! EXP( -ALFA0 * TAUCLD )
      REAL         EALFA2T         ! EXP( -ALFA2 * TAUCLD )
      REAL         EALFA3T         ! EXP( -ALFA3 * TAUCLD )
      REAL         OMEALFAT        ! 1 - e(-alfa*t)

      REAL         EC              ! elemental carbon acc+akn aerosol in
                                   ! cloudwater (mol/liter)
      REAL         ECACCA          ! init EC ACC aerosol in
                                   ! cloudwater (mol/liter)
      REAL         ECAKNA          ! init EC AKN aerosol in cloudwater (mol/liter)
      REAL         FA              ! functional value ??
      REAL         FB              ! functional value ??
      REAL         FE              ! Fe+++ conc in cloudwater (mol/liter)
      REAL         FEA             ! initial Fe in cloudwater (mol/liter)

      REAL         FNH3            ! frac weight of NH3 to total ammonia
      REAL         FNH4ACC         ! frac weight of NH4 acc to total ammonia
      REAL         FHNO3           ! frac weight of HNO3 to total NO3
      REAL         FNO3ACC         ! frac weight of NO3 acc to total NO3

      REAL         FOA1            ! First dissociation constant for FOA
      REAL         FOAH            ! Henry's Law constant for FOA
      REAL         FOA1H           ! FOAH*FOA1
      REAL         FOAL            ! FOA conc in cloudwater (mol/liter)
      REAL         FTST            !
      REAL         GLYH            ! Henry's Law constant for glyoxal
      REAL(8)      GLYL            ! glyoxal conc in cloud water (mol/liter)

      REAL         GM              !
      REAL         GM1             !
      REAL         GM1LOG          !
      REAL         GM2             ! activity correction factor
      REAL         GM2LOG          !

      REAL         HA              !
      REAL         HB              !
      REAL         H2OW            !
      REAL         H2O2H           ! Henry's Law Constant for H2O2
      REAL         H2O2L           ! H2O2 conc in cloudwater (mol/liter)
      REAL         HCLH            ! Henry's Law constant for HCL
      REAL         HCL1            ! First dissociation constant for HCL
      REAL         HCL1H           ! HCLH*HCL1
      REAL         HCLL            ! HCL conc in cloudwater (mol/liter)
      REAL         HCO2            ! HCO2 conc in cloudwater (mol/liter)
      REAL         HCO3            ! HCO3 conc in cloudwater (mol/liter)
      REAL         HNO3H           ! Henry's Law Constant for HNO3
      REAL         HNO31           ! First dissociation constant for HNO3
      REAL         HNO31H          ! HNO31*HNO3H
      REAL         HNO3L           ! HNO3 conc in cloudwater (mol/liter)
C
      REAL         HOH             ! Henry's Law Constant for HO
      REAL         HSO3            ! HSO3 conc in cloudwater (mol/liter)
      REAL         HSO4            ! HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4ACC         ! accumulation mode HSO4 concn in cloudwater (mol/liter)
      REAL         HSO4COR         ! coarse HSO4 concn in cloudwater (mol/liter)

      REAL         HTST            !

      REAL         K               ! K conc in cloudwater (mol/liter)
      REAL         KA              ! initial K in cloudwater (mol/liter)
      REAL         M3NEW           ! accumulation mode mass at time t
      REAL         M3NEWCOR        ! coarse mode mass at time t
      REAL         M3OLD           ! accumulation mode mass at time 0
      REAL         M3OLDCOR        ! coarse mode mass at time 0
      REAL         MG              ! Mg conc in cloudwater (mol/liter)
      REAL         MGA             ! initial Mg in cloudwater (mol/liter)
      REAL         MGLYH           ! Henry's Law Constant for MGLY (M/atm)
      REAL(8)      MGLYL           ! MGLY conc in cloud water (mol/liter)
      REAL         MHPH            ! Henry's Law Constant for MHP
      REAL         MHPL            ! MHP conc in cloudwater (mol/liter)
      REAL         MN              ! Mn++ conc in cloudwater (mol/liter)
      REAL         MNA             ! initial Mn in cloudwater (mol/liter)
      REAL         NA              ! Na conc in cloudwater (mol/liter)
      REAL         NAACC           ! Na in cloudwater (mol/liter)
      REAL         NAACCA          ! initial Na in cloudwater (mol/liter)
      REAL         NAAKNA          ! init Aitken mode aer conc (mol/liter)
      REAL         NACOR           ! coarse Na in cloudwater (mol/liter)
      REAL         NH31            ! First dissociation constant for NH3
      REAL         NH3H            ! Henry's Law Constant for NH3
      REAL         NH3DH2O         !
      REAL         NH31HDH         !
      REAL         NH3L            ! NH3 conc in cloudwater (mol/liter)
      REAL         NH4             ! NH4+ conc in cloudwater (mol/liter)
      REAL         NH4AKNA         ! init NH4 akn conc in cloudwater (mol/liter)
      REAL         NH4ACC          ! NH4 acc conc in cloudwater (mol/liter)
      REAL         NH4ACCA         ! init NH4 acc conc in cloudwater (mol/liter)
      REAL         NH4COR          ! NH4 coarse conc in cloudwater (mol/liter)
      REAL         NITAER          ! total aerosol nitrate
      REAL         NO3             ! NO3- conc in cloudwater (mol/liter)
      REAL         NO3ACC          ! NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3ACCA         ! init NO3 acc conc in cloudwater (mol/liter)
      REAL         NO3AKNA         ! init NO3 akn conc in cloudwater (mol/liter)
      REAL         NO3COR          ! NO3 coarse conc in cloudwater (mol/liter)
      REAL         O3H             ! Henry's Law Constant for O3
      REAL         O3L             ! O3 conc in cloudwater (mol/liter)
      REAL         OH              ! OH- conc in cloudwater (mol/liter)
      REAL         OHL             ! OH radical conc in cloudwater (mol/liter)

      REAL         ORGC            ! cloud-produced SOA in cloudwater (treated as primary) (mol/liter)
      REAL         ORGCACCA        ! init in-cloud SOA (mol/liter)
      REAL         ORGP            ! primary ORGANIC aerosol in cloudwater (mol/liter)
      REAL         ORGPACCA        ! init primary ORG ACC aerosol in cloudwater (mol/liter)
      REAL         ORGPAKNA        ! init primary ORG AKN aerosol in cloudwater (mol/liter)
      REAL         PAAH            ! Henry's Law Constant for PAA
      REAL         PAAL            ! PAA conc in cloudwater (mol/liter)
C
      REAL         PCO20           ! total CO2 partial pressure (atm)
      REAL         PCO2F           ! gas only CO2 partial pressure (atm)
      REAL         PFOA0           ! total ORGANIC acid partial pressure (atm)
      REAL         PFOAF           ! gas only ORGANIC ACID partial press (atm)
      REAL(8)      PGLY0           ! total glyoxal partial pressure (atm)
      REAL(8)      PGLYF           ! gas only GLY partial pressure (atm)
      REAL         PH2O20          ! total H2O2 partial pressure (atm)
      REAL         PH2O2F          ! gas only H2O2 partial pressure (atm)
      REAL         PHCL0           ! total HCL partial pressure (atm)
      REAL         PHCLF           ! gas only HCL partial pressure (atm)
C
      REAL         PHNO30          ! total HNO3 partial pressure (atm)
      REAL         PHNO3F          ! gas only HNO3 partial pressure (atm)
      REAL(8)      PHO0            ! total HO partial Pressure (atm)
      REAL(8)      PHOF            ! gas only HO partial pressure (atm)
      REAL(8)      PMGLY0          ! total MGLY partial pressure (atm)
      REAL(8)      PMGLYF          ! gas only MGLY parital pressure (atm)
      REAL         PMHP0           ! total MHP partial pressure (atm)
      REAL         PMHPF           ! gas only MHP partial pressure (atm)
      REAL         PNH30           ! total NH3 partial pressure (atm)
      REAL         PNH3F           ! gas only NH3 partial pressure (atm)
      REAL         PO30            ! total O3 partial pressure (atm)
      REAL         PO3F            ! gas only O3 partial pressure (atm)
      REAL         PPAA0           ! total PAA partial pressure (atm)
      REAL         PPAAF           ! gas only PAA partial pressure (atm)
      REAL         PRIM            ! PRIMARY acc+akn aerosol in cloudwater (mol/liter)
      REAL         PRIACCA         ! init PRI ACC aerosol in cloudwater (mol/liter)
      REAL         PRIAKNA         ! init PRI AKN aerosol in cloudwater (mol/liter)
      REAL         PSO20           ! total SO2 partial pressure (atm)
      REAL         PSO2F           ! gas only SO2 partial pressure (atm)

      REAL         RECIPA1         !
      REAL         RECIPA2         !
      REAL         RECIPAP1        ! one over pressure (/atm)

      REAL(8)      RGLY3           ! liter/(mol sec)
      REAL         RH2O2           !
      REAL(8)      RMGLY3          ! liter/(mol sec)
      REAL         RMHP            !
      REAL         RPAA            !

      REAL         RT              ! gas const * temperature (liter atm/mol)

      REAL         SIV             ! dissolved so2 in cloudwater (mol/liter)
      REAL         SK6             !
      REAL         SK6TS6          !
      REAL         SO21            ! First dissociation constant for SO2
      REAL         SO22            ! Second dissociation constant for SO2
      REAL         SO2H            ! Henry's Law Constant for SO2
      REAL         SO212           ! SO21*SO22
      REAL         SO212H          ! SO21*SO22*SO2H
      REAL         SO21H           ! SO21*SO2H
      REAL         SO2L            ! SO2 conc in cloudwater (mol/liter)
      REAL         SO3             ! SO3= conc in cloudwater (mol/liter)
      REAL         SO4             ! SO4= conc in cloudwater (mol/liter)
      REAL         SO4ACC          ! accumulation mode SO4= conc in cloudwater (mol/liter)
      REAL         SO4COR          ! coarse SO4= conc in cloudwater (mol/liter)

      REAL         STION           ! ionic strength
      REAL         TAC             !
      REAL         TEMP1           ! (1/T) - (1/298) (1/K)
      REAL         TIMEW           ! cloud chemistry clock (sec)

      REAL(8)      TGLY            ! total glyoxal available for oxidation
      REAL(8)      TMGLY           ! total methylglyoxal available for oxidation
      REAL         TOTOX           !
      REAL         TH2O2
      REAL         TO3
      REAL         TMHP
      REAL         TPAA
      REAL         TOTAMM          ! total ammonium
      REAL         TOTNIT          ! total nitrate (excluding coarse mode)
      REAL         TS6             ! SO4 conc in cloudwater (mol/liter)
      REAL         TS6AKNA         ! init SO4 akn conc in cloudwater (mol/liter)
      REAL         TS6ACC          ! SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6ACCA         ! init SO4 acc conc in cloudwater (mol/liter)
      REAL         TS6COR          ! coarse SO4 conc in cloudwater   (mol/liter)
      REAL         TSIV            ! total S(iv) available for oxidation

      REAL         TST             !

      REAL(8)      XL              ! conversion factor (liter-atm/mol)
      REAL(8)      ONE_OVER_XL     ! 1.0 / XL
      REAL         PRES_ATM_OVER_XL     ! PRES_ATM / XL

      REAL         XLCO2           !
      REAL         XLH2O2          !
      REAL         XLHCL           ! const in calc of HCL final partial pres
      REAL         XLHNO3          !
      REAL         XLMHP           !
      REAL         XLNH3           !
      REAL         XLO3            !
      REAL         XLPAA           !
      REAL         XLSO2           !

C...........Local Variables (arrays):

      REAL      :: LIQUIDS( NLIQS )  ! wet deposition array (mm mol/liter)
      REAL      :: DSIVDT( 0:NUMOX ) ! rate of so2 oxid incloud (mol/liter/sec)
      REAL      :: DS4   ( 0:NUMOX ) ! S(IV) oxidized over timestep DTW(0)
      REAL      :: DTW   ( 0:NUMOX ) ! cloud chemistry timestep (sec)

      REAL      :: ONE_OVER_TEMP     ! 1.0 / TEMP

C...........EXTERNAL FUNCTIONS and their descriptions:

      REAL, EXTERNAL    :: HLCONST
      INTEGER, EXTERNAL :: AqAerWarningMessage

C*********************************************************************
C     begin body of subroutine AQRADM

C...Initialization

      IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

C...special treatment of MGLY for CB05 mechanism:
C...  use Henry's law constant for glyoxal as a surrogate for methyl glyoxal

        MGLYSUR = 'GLYOXAL         '

      END IF    ! FIRSTIME

C...Check for bad temperature or pressure

      if ( temp <= 0.0 .or. pres_atm <= 0.0 ) then
         error%nError   = IV_ERROR
         error%eRoutine = 'aqradm'
         error%eMessage = 'Invalid temp and/or pressure'
         error%eInform  = 'Need positive values'
         error%eAction  = 'Check met file'
         nError = AqAerWarningMessage("")
         return
      end if

      one_over_temp = 1.0 / temp

C...Initialize counters and Compute several conversion factors

      icntaq = 0
      iterat = 0
      DSIV_SCALE = 1.0
      rt = ( MOLVOL / STDTEMP ) * temp    ! r * t (liter atm / mol)
      xl   = wcavg * rt / H2ODENS         ! conversion factor (l-atm/mol)
      ONE_OVER_XL = 1.0d0 / XL
      pres_atm_over_xl = pres_atm / xl
      tst  = 0.999
      act1 = 1.0
      act2 = 1.0
      gm2  = 1.0
      timew = 0.0
      recipap1 = 1.0 / pres_atm

C...set equilibrium constants as a function of temperature
C...Henry's law constants

      so2h  = HLCONST( 'SO2             ', temp, .FALSE., 0.0 )
      co2h  = HLCONST( 'CO2             ', temp, .FALSE., 0.0 )
      nh3h  = HLCONST( 'NH3             ', temp, .FALSE., 0.0 )
      h2o2h = HLCONST( 'H2O2            ', temp, .FALSE., 0.0 )
      o3h   = HLCONST( 'O3              ', temp, .FALSE., 0.0 )
      hclh  = HLCONST( 'HCL             ', temp, .false., 0.0 )
      hno3h = HLCONST( 'HNO3            ', temp, .FALSE., 0.0 )
      mhph  = HLCONST( 'METHYLHYDROPEROX', temp, .FALSE., 0.0 )
      paah  = HLCONST( 'PEROXYACETIC_ACI', temp, .FALSE., 0.0 )
      foah  = HLCONST( 'FORMIC_ACID     ', temp, .FALSE., 0.0 )
      glyh  = HLCONST( 'GLYOXAL         ', temp, .FALSE., 0.0 )
      mglyh = HLCONST( MGLYSUR,            temp, .FALSE., 0.0 )
      hoh   = HLCONST( 'OH              ', temp, .false., 0.0 )

C...Dissociation constants

      temp1 = one_over_temp - 1.0 / 298.0

      foa1  = 1.80e-04 * EXP( -2.00e+01 * temp1 )    ! martell and smith (1977)
      sk6   = 1.02e-02 * exp(  2.72e+03 * temp1 )    ! smith and martell (1976)
      so21  = 1.30e-02 * exp(  1.96e+03 * temp1 )    ! smith and martell (1976)
      so22  = 6.60e-08 * exp(  1.50e+03 * temp1 )    ! smith and martell (1976)
      co21  = 4.30e-07 * exp( -1.00e+03 * temp1 )    ! smith and martell (1976)
      co22  = 4.68e-11 * exp( -1.76e+03 * temp1 )    ! smith and martell (1976)
      h2ow  = 1.00e-14 * exp( -6.71e+03 * temp1 )    ! smith and martell (1976)
      nh31  = 1.70e-05 * exp( -4.50e+02 * temp1 )    ! smith and martell (1976)
      hcl1  = 1.74e+06 * exp(  6.90e+03 * temp1 )    ! marsh & mcelroy (1985)
      hno31 = 1.54e+01 * exp(  8.70e+03 * temp1 )    ! schwartz (1984)

c...kinetic oxidation rates
c...   from chamedies (1982)

      rh2o2 = 8.0e+04 * exp( -3650.0 * temp1 )

c...from kok

      rmhp = 1.75e+07 * exp( -3801.0 * temp1 )
      rpaa = 3.64e+07 * exp( -3994.0 * temp1 )

c...from carlton et al. (2007)

      rgly3  = 3.0d+10   ! rate constant measured at 298k
      rmgly3 = 3.0d+10   ! assumed to be the same as gly

c...make initializations

      dsivdt = 0.0
      dtw    = 0.0
      ds4    = 0.0

      dgly1  = 0.0d0
      dmgly1 = 0.0d0
      dorgc  = 0.0d0

!...compute the initial accumulation aerosol 3rd moment
!...  secondary organic aerosol and water are not included

      m3old = ( aerosol( LSO4ACC  ) * SGRAERMW( LSO4ACC  ) / 1.8e6
     &      +   aerosol( LNH4ACC  ) * SGRAERMW( LNH4ACC  ) / 1.8e6
     &      +   aerosol( LNO3ACC  ) * SGRAERMW( LNO3ACC  ) / 1.8e6
     &      +   aerosol( LXYL3ACC ) * SGRAERMW( LXYL3ACC ) / 2.0e6
     &      +   aerosol( LTOL3ACC ) * SGRAERMW( LTOL3ACC ) / 2.0e6
     &      +   aerosol( LBNZ3ACC ) * SGRAERMW( LBNZ3ACC ) / 2.0e6
     &      +   aerosol( LORGCACC ) * SGRAERMW( LORGCACC ) / 2.0e6
     &      +   aerosol( LISO3ACC ) * SGRAERMW( LISO3ACC ) / 2.0e6
     &      +   aerosol( LOLGAACC ) * SGRAERMW( LOLGAACC ) / 2.0e6
     &      +   aerosol( LOLGBACC ) * SGRAERMW( LOLGBACC ) / 2.0e6
     &      +   aerosol( LORGPACC ) * SGRAERMW( LORGPACC ) / 2.0e6
     &      +   aerosol( LECACC   ) * SGRAERMW( LECACC   ) / 2.2e6
     &      +   aerosol( LPRIACC  ) * SGRAERMW( LPRIACC  ) / 2.2e6
     &      +   aerosol( LNAACC   ) * SGRAERMW( LNAACC   ) / 2.2e6
     &      +   aerosol( LCLACC   ) * SGRAERMW( LCLACC   ) / 2.2e6 )
!!!  &      * 6.0 / PI    ! cancels out in division at end of subroutine

!...compute the initial coarse aerosol 3rd moment
!...  secondary organic aerosol and water are not included

      m3oldcor = ( aerosol( LSO4COR  ) * SGRAERMW( LSO4COR  ) / 1.8e6
     &         +   aerosol( LNH4COR  ) * SGRAERMW( LNH4COR  ) / 1.8e6
     &         +   aerosol( LNO3COR  ) * SGRAERMW( LNO3COR  ) / 1.8e6
     &         +   aerosol( LPRICOR  ) * SGRAERMW( LPRICOR  ) / 2.2e6
     &         +   aerosol( LNACOR   ) * SGRAERMW( LNACOR   ) / 2.2e6
     &         +   aerosol( LCLCOR   ) * SGRAERMW( LCLCOR   ) / 2.2e6 )

C...compute fractional weights for several species

      totnit = gas( LHNO3 ) + aerosol( LNO3ACC )
      if ( totnit > 0.0 ) then
        fhno3   = gas( LHNO3 ) / totnit
        fno3acc = 1. - fhno3
      else
        fhno3   = 1.0
        fno3acc = 0.0
      end if

      totamm = gas( LNH3 ) + aerosol( LNH4ACC )
      if ( totamm > 0.0 ) then
        fnh3    = gas( lnh3 ) / totamm
        fnh4acc = 1. - fnh3
      else
        fnh3    = 1.0
        fnh4acc = 0.0
      end if

C...initial concentration from accumulation-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the accumulation-mode
C...  aerosol mass is incorporated into the cloud droplets
      ts6acca  = ( aerosol( LSO4ACC )
     &         +   gas    ( LH2SO4  ) )  * pres_atm_over_xl
      no3acca  =   aerosol( LNO3ACC   )  * pres_atm_over_xl
      nh4acca  =   aerosol( LNH4ACC   )  * pres_atm_over_xl

      orgcacca =  aerosol( LORGCACC )  * pres_atm_over_xl
      orgpacca =  aerosol( LORGPACC )  * pres_atm_over_xl
      ecacca  =   aerosol( LECACC  )   * pres_atm_over_xl
      priacca =   aerosol( LPRIACC )   * pres_atm_over_xl
      naacca  =   aerosol( LNAACC  )   * pres_atm_over_xl
      clacca  =   aerosol( LCLACC  )   * pres_atm_over_xl

C...initial concentration from coarse-mode aerosol loading (mol/liter)
C...  an assumption is made that all of the coarse-mode
C...  aerosol mass is incorporated into the cloud droplets
      ts6cor   =   aerosol( LSO4COR )   * pres_atm_over_xl
      no3cor   =   aerosol( LNO3COR )   * pres_atm_over_xl
      nh4cor   =   aerosol( LNH4COR )   * pres_atm_over_xl
      clcor    =   aerosol( LCLCOR  )   * pres_atm_over_xl
      nacor    =   aerosol( LNACOR  )   * pres_atm_over_xl
      ka       =   aerosol( LK      )   * pres_atm_over_xl
      caa      =   aerosol( LCACO3  )   * pres_atm_over_xl
      mga      =   aerosol( LMGCO3  )   * pres_atm_over_xl
      fea      =   aerosol( LA3FE   )   * pres_atm_over_xl
      mna      =   aerosol( LB2MN   )   * pres_atm_over_xl
      co3a     = ( aerosol( LCACO3  )
     &         +   aerosol( LMGCO3  ) ) * pres_atm_over_xl

C...Set constant factors that will be used in later multiplications (moles/atm)

      xlh2o2  = h2o2h * xl
      xlo3    = o3h   * xl
      xlmhp   = mhph  * xl
      xlpaa   = paah  * xl
      xlso2   = so2h  * xl
      xlnh3   = nh3h  * xl
      xlhcl   = hclh  * xl
      xlhno3  = hno3h * xl
      xlco2   = co2h  * xl
C
      so212   = so21  * so22
      so21h   = so21  * so2h
      so212h  = so212 * so2h
      co212   = co21  * co22
      co21h   = co21  * co2h
      co212h  = co22  * co21h
      nh3dh2o = nh31  / h2ow
      nh31hdh = nh3h  * nh3dh2o
      foa1h   = foa1  * foah
      hcl1h   = hcl1  * hclh
      hno31h  = hno31 * hno3h

C...loop if kinetic calculations are made, return to this point

      DO I20C = 1, 10001

        if ( i20c >= 10000 ) then
!!          error%nError   = AB_ERROR
          error%nError   = NO_ERROR
          error%eRoutine = 'aqradm'
          error%eMessage = 'Excessive looping at I20C'
!pkdebug
!          write(*,*)error%eMessage
!pkdebug
          error%eInform  = ''
          error%eAction  = ''
!!          error%nError = AqAerWarningMessage('')
          return
        end if

C...set aitken-mode aerosol loading (mol/liter)

        omealfat = 1. - EXP( -alfa3 * timew )
        no3akna  = aerosol( LNO3AKN  ) * pres_atm_over_xl * omealfat
        nh4akna  = aerosol( LNH4AKN  ) * pres_atm_over_xl * omealfat
        ts6akna  = aerosol( LSO4AKN  ) * pres_atm_over_xl * omealfat
        orgpakna = aerosol( LORGPAKN ) * pres_atm_over_xl * omealfat
        ecakna   = aerosol( LECAKN   ) * pres_atm_over_xl * omealfat
        priakna  = aerosol( LPRIAKN  ) * pres_atm_over_xl * omealfat
        naakna   = aerosol( LNAAKN   ) * pres_atm_over_xl * omealfat
        clakna  =  aerosol( LCLAKN   ) * pres_atm_over_xl * omealfat

c...Initial gas phase partial pressures (atm)
        pso20  = gas( LSO2  ) * pres_atm + ds4( 0 ) * xl
        pnh30  = gas( LNH3  ) * pres_atm
     &         + ( nh4acca + nh4cor + nh4akna ) * xl
        phno30 = ( gas( LHNO3 ) + 2.0 * gas( LN2O5 ) ) * pres_atm
     &         + ( no3acca + no3cor + no3akna ) * xl
        phcl0  = gas( LHCL ) * pres_atm
     &         + ( clacca + clcor + clakna ) * xl  ! new for sea salt
        ph2o20 = gas( LH2O2 ) * pres_atm
        po30   = gas( LO3   ) * pres_atm
        pfoa0  = gas( LFOA  ) * pres_atm
        pmhp0  = gas( lmhp  ) * pres_atm
        ppaa0  = gas( lpaa  ) * pres_atm
        pco20  = gas( lco2  ) * pres_atm
     &         + co3a * xl
        pgly0  = gas( lgly  ) * pres_atm + dgly1 * xl
        pmgly0 = gas( lmgly ) * pres_atm + dmgly1 * xl
        pho0   = gas( lho   ) * pres_atm

C...Don't allow gas concentrations to go below zero
        pso20  = MAX( pso20,  0.0 )
        pnh30  = MAX( pnh30,  0.0 )
        ph2o20 = MAX( ph2o20, 0.0 )
        po30   = MAX( po30,   0.0 )
        pfoa0  = MAX( pfoa0,  0.0 )
        pmhp0  = MAX( pmhp0,  0.0 )
        ppaa0  = MAX( ppaa0,  0.0 )
        pco20  = MAX( pco20,  0.0 )
        phno30 = MAX( phno30, 0.0 )
        pgly0  = MAX( pgly0,  0.0d0 )
        pmgly0 = MAX( pmgly0, 0.0d0 )
        pho0   = MAX( pho0,   0.0d0 )

C...Molar concentrations of soluble aerosols
        ts6cor  = MAX( ts6cor, 0.0 )
        no3cor  = MAX( no3cor, 0.0 )
        nacor   = MAX( nacor, 0.0 )
        clcor   = MAX( clcor, 0.0 )
        nh4cor  = MAX( nh4cor, 0.0 )

        ts6     = ts6acca  + ts6akna  + ts6cor - ds4( 0 )
        na      = naacca   + naakna  + nacor
        ca      = caa
        mg      = mga
        k       = ka
        fe      = fea
        mn      = mna
        orgc    = orgcacca  + dorgc             ! new in-cloud organic
        orgp    = orgpacca + orgpakna
        ec      = ecacca + ecakna
        prim    = priacca + priakna

        a       = 3.0 * fe
        b       = 2.0 * mn

C...Don't allow aerosol concentrations to go below zero

        ts6     = MAX( ts6,     0.0 )
        na      = MAX( na,      0.0 )
        ca      = MAX( ca,      0.0 )
        mg      = MAX( mg,      0.0 )
        k       = MAX( k,       0.0 )
        fe      = MAX( fe,      0.0 )
        mn      = MAX( mn,      0.0 )
        orgc    = MAX( orgc,    0.0 )
        orgp    = MAX( orgp,    0.0 )
        ec      = MAX( ec,      0.0 )
        prim    = MAX( prim,    0.0 )
        a       = MAX( a,       0.0 )
        b       = MAX( b,       0.0 )

        sk6ts6 = sk6 * ts6

C...Find solution of the equation using a method of reiterative
C...bisections Make initial guesses for pH:   between .01  to  10.

        ha = PHMIN
        hb = PHMAX

        DO I7777C = 1, 10001

          if ( i7777c >= 10000 ) then
            error%nError   = NO_ERROR
!!            error%nError   = AB_ERROR
            error%eRoutine = 'aqradm'
            error%eMessage = 'Excessive looping at I7777C'
            error%eInform  = 'Using last pH value'
            error%eAction  = ''
!!            error%nError = AqAerWarningMessage('')
            icntaq = 0
!pkdebug
!            write(*,*)'ha,hb: ',ha,hb
!pkdebug
            EXIT    ! exit loop I7777C
          end if

!     ha = MAX( ha - 0.8, 0.1 )
!     hb = MIN( hb + 0.8, 9.9 )
          ha = MAX( ha - 0.8, PHMIN )
          hb = MIN( hb + 0.8, PHMAX )
          ae = 10.0**( -ha )

          recipa1 = 1.0 / ( ae * act1 )
          recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3
C...HCOOH, and CO2 (atm)

          pso2f = pso20 / ( 1.0 + xlso2 * ( 1.0 + so21 * recipa1
     &          + so212 * recipa2 ) )
          pnh3f = pnh30 / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )
          phclf = phcl0 / ( 1.0 + xlhcl *  ( 1.0 + hcl1 * recipa1 ) )

          pfoaf = pfoa0 / ( 1.0 + xl * ( foah + foa1h * recipa1 ) )

          phno3f = phno30 / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

          pco2f = pco20 / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &          + co212 * recipa2 ) )

C...Calculate liquid phase concentrations (moles/liter)

          so4  = sk6ts6 / ( ae * gm2 + sk6 )
          hso4 = ts6 - so4
          so3  = so212h  * pso2f  * recipa2
          hso3 = so21h   * pso2f  * recipa1
          co3  = co212h  * pco2f  * recipa2
          hco3 = co21h   * pco2f  * recipa1
          oh   = h2ow    * recipa1
          nh4  = nh31hdh * pnh3f  * ae
          hco2 = foa1h   * pfoaf  * recipa1
          no3  = hno31h  * phno3f * recipa1
          cl   = hcl1h   * phclf  * recipa1 ! new for sea salt

C...Compute functional value

          fa = ae + nh4 + na + k + 2.0 * ( ca + mg - co3 - so3 - so4 )  ! SLN 16March2011
     &       - oh - hco3 - hso3 - no3 - hso4 - hco2 - cl

C...Start iteration and bisection ****************<<<<<<<
          DO I30C = 1, 10000
            if ( i30c >= 10000 ) then
              error%nError   = NO_ERROR
              error%eRoutine = 'aqradm'
              error%eMessage = 'Excessive looping at I30C'
!pkdebug
!          write(*,*)error%eMessage
!          write(*,*)'Using last pH value'
!pkdebug
              error%eInform  = 'Using last pH value'
              error%eAction  = ''
!!              error%nError = AqAerWarningMessage('')
              icntaq = 0
!pkdebug
!            write(*,*)'ha,hb: ',ha,hb
!pkdebug
              EXIT    ! exit loop I30C
            end if

!04102018            bb = 0.5 * ( ha + hb )
            bb = -LOG10( 0.5 * ( 10.0 **( -ha ) + 10.0 ** (-hb ) ) )
            ae = 10.0**( -bb )

! --- don't solve for H+ if fa < 0 at first try
            if ( i7777c == 1 .and. fa < 0. ) then

              bb = ha
              hb = ha
              ae = 10.0**( -bb )

            end if

            recipa1 = 1.0 / ( ae * act1 )
            recipa2 = 1.0 / ( ae * ae * act2 )

C...Calculate final gas phase partial pressure of SO2, NH3, HNO3
C...HCOOH, CO2, HCL, HO2, HNO2, and HCHO (atm)

            pso2f = pso20 / ( 1.0 + xlso2
     &            * ( 1.0 + so21 * recipa1 + so212 * recipa2 ) )

            pnh3f = pnh30 / ( 1.0 + xlnh3 * ( 1.0 + nh3dh2o * ae ) )

            phclf = phcl0 / ( 1.0 + xlhcl *  ( 1.0 + hcl1 * recipa1 ) )

            phno3f = phno30 / ( 1.0 + xlhno3 * ( 1.0 + hno31 * recipa1 ) )

            pfoaf = pfoa0 / ( 1.0 + xl * ( foah + foa1h * recipa1 ) )

            pco2f = pco20 / ( 1.0 + xlco2 * ( 1.0 + co21 * recipa1
     &            + co212 * recipa2 ) )

C...Calculate liquid phase concentrations (moles/liter)

            so4  = sk6ts6 / ( ae * gm2 + sk6 )
            hso4 = ts6 - so4
            so3  = so212h  * pso2f  * recipa2
            hso3 = so21h   * pso2f  * recipa1
            co3  = co212h  * pco2f  * recipa2
            hco3 = co21h   * pco2f  * recipa1
            oh   = h2ow    * recipa1
            nh4  = nh31hdh * pnh3f  * ae
            hco2 = foa1h   * pfoaf  * recipa1
            no3  = hno31h  * phno3f * recipa1
            cl   = hcl1h   * phclf  * recipa1 ! new for sea salt

C...compute functional value

            fb = ae + nh4 + na + k + 2.0 * ( ca + mg - co3 - so3 - so4 )  ! SLN 16March2011
     &         - oh - hco3 - hso3 - no3 - hso4 - hco2 - cl

C...Calculate and check the sign of the product of the two functional values

            ftst = fa * fb
            if ( ftst <= 0.0 ) then
              hb = bb
            else
              ha = bb
              fa = fb
            end if

C...Check convergence of solutions

!04102018            htst = ha / hb
!04102018            if ( htst <= tst ) EXIT  ! exit loop I30C
            htst = ABS(10.0**(-hb)-10.0**(-ha))/10.0**(-hb)
            if ( htst < 1.0E-3 ) EXIT    ! exit loop I30C
          end do   ! I30C

C...end of zero-finding routine ****************<<<<<<<<<<<<

C...compute Ionic strength and activity coefficient by the Davies equation

          stion = 0.5
     &          * ( ae + nh4 + oh + hco3 + hso3
     &            + 4.0 * ( so4 + co3 + so3 + ca + mg + mn )
     &            + no3 + hso4 + 9.0 * fe + na + k + cl + a + b + hco2 )
          gm1log = -0.509 * ( SQRT( stion )
     &           / ( 1.0 + SQRT( stion ) ) - 0.2 * stion )
          gm2log = gm1log * 4.0
          gm1  = 10.0**gm1log
          gm2  = MAX( 10.0**gm2log, 1.0e-30 )
          actb = act1
          act1 = MAX( gm1 * gm1, 1.0e-30 )
          act2 = MAX( gm1 * gm1 * gm2, 1.0e-30 )

C...check for convergence and possibly go to I7777C, to recompute
C...  Gas and liquid phase concentrations

! --- don't solve for H+ if fa < 0 at first try
          if ( i7777c == 1 .and. fa < 0. ) then
            actb = act1
          end if

          tac = ABS( actb - act1 ) / actb
          if ( tac < 1.0D-2 ) EXIT    ! exit loop I7777C

          icntaq = icntaq + 1
          if ( icntaq >= 60000 ) then
            error%nError   = NO_ERROR
            error%eRoutine = 'aqradm'
            error%eMessage = 'Maximum iterations for pH calculation exceeded'
            error%eInform  = 'Using last pH value'
!pkdebug
!          write(*,*)error%eMessage
!          write(*,*)'Using last pH value'
!            write(*,*)'ha,hb: ',ha,hb
!pkdebug
            error%eAction  = ''
            icntaq = 0
!!            error%nError = AqAerWarningMessage('')
            EXIT    ! exit loop I7777C
          end if
        end do     ! end of do loop I7777C

C...return an error if the pH is not in range

ccc      if ( ( ha .lt. 0.02 ) .or. ( ha .gt. 9.49 ) ) then
        if ( ( ha < PHMIN ) .or. ( ha > PHMAX ) ) then
          print *, ha
          error%nError   = NO_ERROR
          error%eRoutine = 'aqradm'
          error%eMessage = 'pH value out of range'
!pkdebug
!          write(*,*)error%eMessage
!pkdebug
          error%eInform  = ''
          error%eAction  = ''
!!          error%nError = AqAerWarningMessage('')
          return
        end if

C...Make those concentration calculations which can be made outside
C...  of the function.

        so2l = so2h * pso2f
        ac = 10.0**( -bb )
        siv = so3 + hso3 + so2l

C...Calculate final gas phase concentrations of oxidants (atm)

        ph2o2f = ( ph2o20 + xl * ds4( 1 ) ) / ( 1.0 + xlh2o2 )
        po3f   = ( po30   + xl * ds4( 2 ) ) / ( 1.0 + xlo3   )
        pmhpf  = ( pmhp0  + xl * ds4( 4 ) ) / ( 1.0 + xlmhp  )
        ppaaf  = ( ppaa0  + xl * ds4( 5 ) ) / ( 1.0 + xlpaa  )
        pglyf  = ( pgly0                  ) / ( 1.0 + glyh * xl )
        pmglyf = ( pmgly0                 ) / ( 1.0 + mglyh * xl )
        phof   = ( pho0                   ) / ( 1.0 + hoh * xl)

        ph2o2f = MAX( ph2o2f, 0.0 )
        po3f   = MAX( po3f,   0.0 )
        pmhpf  = MAX( pmhpf,  0.0 )
        ppaaf  = MAX( ppaaf,  0.0 )

C...Calculate liquid phase concentrations of oxidants (moles/liter)

        h2o2l = ph2o2f * h2o2h
        o3l   = po3f   * o3h
        mhpl  = pmhpf  * mhph
        paal  = ppaaf  * paah
        foal  = pfoaf  * foah
        nh3l  = pnh3f  * nh3h
        co2l  = pco2f  * co2h
        hcll  = phclf  * hclh
        hno3l = phno3f * hno3h
        glyl  = pglyf  * glyh
        mglyl = pmglyf * mglyh
        ohl   = phof   * hoh

C...compute modal concentrations

        so4cor  = sk6 * ts6cor / ( ae * gm2 + sk6 )
        hso4cor = MAX( ts6cor - so4cor, 0.0 )

        ts6acc  = MAX( ts6  - ts6cor,   0.0 )
        so4acc  = MAX( so4  - so4cor,   0.0 )
        hso4acc = MAX( hso4 - hso4cor,  0.0 )
        no3acc  = MAX( no3 - no3cor,   0.0 )
        naacc   = MAX( na   - nacor,    0.0 )
        clacc   = MAX( cl   - clcor,    0.0 )
        nh4acc  = MAX( nh4  - nh4cor,   0.0 )

C...load the liquid concentration array with current values

        liquids( lacl      ) = ac

C...if the maximum cloud lifetime has not been reached, then compute
C...the next timestep.

        if ( timew >= taucld ) EXIT   ! exit 20 loop

C...make kinetics calculations
C...  note: DS4(i) and DSIV(I) are negative numbers!

        iterat = iterat + 1

C...Define the total S(iv) available for oxidation

        tsiv = pso20 * one_over_xl

C...Calculate sulfur iv oxidation rate due to H2O2

        dsivdt( 1 ) = -rh2o2 * h2o2l * so2l / ( 0.1 + ac )
        th2o2 = ph2o20 * one_over_xl
        if ( ( dsivdt( 1 ) == 0.0 ) .or.
     &       ( tsiv  <= CONCMIN ) .or.
     &       ( th2o2 <= CONCMIN ) ) then
          dtw(1) = taucld
        else
          dtw( 1 ) = -0.05 * MIN( th2o2, tsiv ) / dsivdt( 1 )
        end if

C...Calculate sulfur iv oxidation rate due to O3

        if ( bb >= 2.7 ) then
          dsivdt( 2 ) = -4.19e5 * ( 1.0 + 2.39e-4 / ac ) * o3l * siv
        else
          dsivdt( 2 ) = -1.9e4 * siv * o3l / SQRT( ac )
        end if

        to3 = po30 * one_over_xl
        if ( ( dsivdt( 2 ) == 0.0 ) .or.
     &       ( tsiv  <= CONCMIN ) .or.
     &       ( to3 <= CONCMIN ) ) then
          dtw( 2 ) = taucld
        else
          dtw( 2 ) = -0.01 * MIN( to3, tsiv ) / dsivdt( 2 )
        end if

C...Calculate sulfur iv oxidation rate due to O2 catalyzed by Mn++
C...  and Fe+++  See Table IV Walcek & Taylor ( 1986)

        if ( bb >= 4.0 )  then  ! 4.0  < ph

          if ( siv <= 1.0e-5 ) then
            dsivdt( 3 ) = -5000.0 * mn * hso3
          else if ( siv > 1.0e-5 ) then
            dsivdt( 3 ) = -( 4.7 * mn * mn / ac
     &                  + 1.0e7 * fe * siv * siv )
          end if  ! end of first pass through siv conc.

        else          ! ph < 4.0

	  if ( siv <= 1.0e-5 ) then
            dsivdt( 3 ) = -3.0 * ( 5000.0 * mn * hso3
     &                  + 0.82 * fe * siv / ac )
          else
            dsivdt( 3 ) = -( 4.7 * mn * mn / ac
     &                  + ( 0.82 * fe * siv / ac )
     &                  * ( 1.0 + 1.7e3 * mn**1.5 / ( 6.3e-6 + fe ) ) )
          end if ! end of second pass through siv conc.
        end if  ! end of pass through ph

        if ( ( dsivdt( 3 ) == 0.0 ) .or. ( tsiv <= CONCMIN ) ) then
          dtw( 3 ) = taucld
        else
          dtw( 3 ) = -0.1 * tsiv / dsivdt( 3 )
        end if

C...Calculate sulfur oxidation rate due to MHP

        dsivdt( 4 ) = -rmhp * ac * mhpl * hso3
        tmhp = pmhp0 * one_over_xl
        if ( ( dsivdt( 4 ) == 0.0 ) .or.
     &       ( tsiv  <= CONCMIN ) .or.
     &       ( tmhp <= CONCMIN ) ) then
          dtw( 4 ) = taucld
        else
          dtw( 4 ) = -0.1 * MIN( tmhp, tsiv ) / dsivdt( 4 )
        end if

C...Calculate sulfur oxidation due to PAA

        dsivdt( 5 ) = -rpaa * hso3 * paal * ( ac + 1.65e-5 )
        tpaa = ppaa0 * one_over_xl
        if ( ( dsivdt( 5 ) == 0.0 ) .or.
     &       ( tsiv  <= CONCMIN ) .or.
     &       ( tpaa <= CONCMIN ) ) then
          dtw( 5 ) = taucld
        else
          dtw( 5 ) = -0.1 * MIN( tpaa, tsiv ) / dsivdt( 5 )
        end if

C...Calculate total sulfur iv oxidation rate

        dsivdt( 0 ) = 0.0
        do iox = 1, NUMOX
          dsivdt( 0 ) = dsivdt( 0 ) + dsivdt( iox )
        end do

C...Calculate a minimum time step required

        dtw( 0 ) = MIN( dtw( 1 ), dtw( 2 ), dtw( 3 ),
     &                  dtw( 4 ), dtw( 5 ) )

C...check for large time step

        if ( dtw( 0 ) > 8.0e+37 ) then
          WRITE(99+myid,1001) dsivdt(0), ts6, dtw(0)
        else

C...CALCULATE IN-CLOUD SOA PRODUCTION
C...  Reference:  Carlton, A.G., B.J. Turpin, K.E. Altieri, A. Reff,
C...  S. Seitzinger, H.J. Lim, and B. Ervens (2007), Atmospheric Oxalic
C...  Acid and SOA Production from Glyoxal: Results of Aqueous
C...  Photooxidation Experiments, Atmos. Environ., 41(35), 7588-7602.

C...Define the total glyoxal available for oxidation

          tgly = pgly0 * one_over_xl

C...Calculate GLY oxidation due to OH

          dglydt = -rgly3 * glyl * ohl

C...Define the total methylglyoxal available for oxidation

          tmgly = pmgly0 * one_over_xl

C...Calculate MGLY oxidation due to OH

          dmglydt = -rmgly3 * mglyl * ohl

!ccC...Define the total OH available for oxidation
!cc
!cc          THO = PHO0 * ONE_OVER_XL

C...Calculate OH consumption

!steadystate          DOHDT = -( RGLY3 * GLYL + RMGLY3 * MGLYL ) * OHL

C...calculate the change in sulfur iv for this time step

60        CONTINUE
          dts6 = ABS( dtw( 0 ) * ( -dsivdt( 0 ) ) )

C...If DSIV(0), sulfur iv oxidized during this time step would be
C...less than 5% of sulfur oxidized since time 0, then double DT

          if ( dtw( 0 ) <= taucld ) then
            if ( dts6 < 0.05 * ts6 ) then
              dtw( 0 ) = dtw( 0 ) * 2.0
	      go to 60
            end if
          end if
        end if
        dtw( 0 ) = MIN( dtw( 0 ), taucld )

C...Limit the timestep to prevent negative SO2 concentrations and mass creation
C...  for sulfate (suggested by Bonyoung Koo)

        if ( dsivdt( 0 ) < 0.0 ) then
!         DTW( 0 ) = MIN( DTW( 0 ), -TSIV * 1.00001 / DSIVDT( 0 ) )
          dtw( 0 ) = MIN( dtw( 0 ), -tsiv / dsivdt( 0 ) )
        end if

C...If the total time after this time increment will be greater than
C...  TAUCLD sec., then set DTW(0) so that total time will be TAUCLD

        if ( timew + dtw( 0 ) > taucld ) dtw( 0 ) = taucld - timew
!       if ( ts6 < 1.0e-11 ) dtw( 0 ) = taucld - timew
!       if ( iterat > 100 ) dtw( 0 ) = taucld - timew
        if ( iterat > 100 ) dtw( 0 ) = MAX( 1.0, dtw( 0 ) )

C...force mass balance for the specified timestep
C...  for GLY and MGLY, assume that OH is in steady state

        dglydt  = MAX( dglydt,  -tgly  / dtw( 0 ) )
        dmglydt = MAX( dmglydt, -tmgly / dtw( 0 ) )

C...  for S(IV), also limit by oxidants (except assume O2 in steady state)

        dsivdt( 1 ) = MAX( dsivdt( 1 ), -MIN( tsiv, th2o2 ) / dtw( 0 ) )
        dsivdt( 2 ) = MAX( dsivdt( 2 ), -MIN( tsiv, to3   ) / dtw( 0 ) )
        dsivdt( 3 ) = MAX( dsivdt( 3 ), -tsiv / dtw( 0 ) )
        dsivdt( 4 ) = MAX( dsivdt( 4 ), -MIN( tsiv, tmhp  ) / dtw( 0 ) )
        dsivdt( 5 ) = MAX( dsivdt( 5 ), -MIN( tsiv, tpaa  ) / dtw( 0 ) )

C...  recalculate the total S(iv) oxidation rate

        dsivdt( 0 ) = 0.0
        do iox = 1, NUMOX
          dsivdt( 0 ) = dsivdt( 0 ) + dsivdt( iox )
        end do

C...  if the total S(iv) oxidized over the timestep exceeds the amount of
C...    S(iv) available then scale the rates to conserve mass

        if (-dsivdt( 0 ) * dtw( 0 ) > tsiv ) then
          dsiv_scale = tsiv / ( -dsivdt( 0 ) * dtw( 0 ) )
          dsivdt( 0 ) = dsivdt( 0 ) * dsiv_scale
          dsivdt( 1 ) = dsivdt( 1 ) * dsiv_scale
          dsivdt( 2 ) = dsivdt( 2 ) * dsiv_scale
          dsivdt( 3 ) = dsivdt( 3 ) * dsiv_scale
          dsivdt( 4 ) = dsivdt( 4 ) * dsiv_scale
          dsivdt( 5 ) = dsivdt( 5 ) * dsiv_scale
        end if

C...Set DSIV(I), I = 0,NUMOX, the amount of S(IV) oxidized by each
C... individual oxidizing agent, as well as the total.

        do iox = 0, NUMOX
          ds4( iox ) = ds4( iox ) + dtw( 0 ) * dsivdt( iox )
        end do

        dgly1  = dgly1  + dtw( 0 ) * dglydt

        dmgly1 = dmgly1 + dtw( 0 ) * dmglydt

csteadystate        DOH1   = DOH1   + DTW( 0 ) * DOHDT

C...Calculate AORGC Production:  4% SOAcld (ORGC) yield from glyoxal
C...  and methylglyoxal is assumed

        dorgc = dorgc - ( 0.04 * ( dglydt + dmglydt ) * dtw( 0 ) )

        timew = timew + dtw( 0 )

      end do     ! I20C loop

C...Compute the output concentrations

C...gas concentrations (mol/molV)

      totamm = ( pnh3f  + ( nh4acc + nh3l  ) * xl ) * recipap1
      totnit = ( phno3f + ( no3acc + hno3l ) * xl ) * recipap1

      gas( LSO2   ) = ( pso2f   + xl *  siv )   * recipap1
      gas( LH2O2  ) = ( ph2o2f  + xl *  h2o2l ) * recipap1
      gas( LO3    ) = ( po3f    + xl *  o3l )   * recipap1
      gas( LCO2   ) = ( pco2f   + xl * co2l )  * recipap1
      gas( LFOA   ) = ( pfoaf  + xl * ( foal + hco2 ) ) * recipap1
      gas( LMHP   ) = ( pmhpf   + xl *  mhpl )  * recipap1
      gas( LPAA   ) = ( ppaaf   + xl *  paal )  * recipap1
      gas( LHCL   ) = ( phclf   + xl *  hcll )  * recipap1
      gas( LGLY   ) = ( pglyf   + xl *  glyl )   * recipap1
      gas( LMGLY  ) = ( pmglyf  + xl *  mglyl)   * recipap1
!     gas( LHO    ) = ( phof   + xl *  ohl  )  * recipap1

      gas( LNH3   ) = fnh3  * totamm
      gas( LHNO3  ) = fhno3 * totnit
      gas( LN2O5  ) = 0.0 ! assume all into aerosol
      gas( LH2SO4 ) = 0.0 ! assume all into aerosol

C...aerosol concentrations (mol/molV)
      ealfa0t   = EXP( -alfa0 * taucld )
      ealfa2t   = EXP( -alfa2 * taucld )
      ealfa3t   = EXP( -alfa3 * taucld )

      aerosol( LSO4AKN ) = aerosol( LSO4AKN ) * ealfa3t
      aerosol( LNH4AKN ) = aerosol( LNH4AKN ) * ealfa3t
      aerosol( LNO3AKN ) = aerosol( LNO3AKN ) * ealfa3t
      aerosol( LECAKN  ) = aerosol( LECAKN  ) * ealfa3t
      aerosol( LPRIAKN ) = aerosol( LPRIAKN ) * ealfa3t

      aerosol( LORGPAKN ) = aerosol( LORGPAKN ) * ealfa3t

      aerosol( LSO4ACC ) = ts6acc * xl * recipap1
      aerosol( LECACC  ) = ec     * xl * recipap1
      aerosol( LPRIACC ) = prim   * xl * recipap1
      aerosol( LORGCACC ) = ORGC    * xl * recipap1
      aerosol( LORGPACC ) = ORGP    * xl * recipap1

      aerosol( LNH4ACC ) = fnh4acc * totamm
      aerosol( LNO3ACC ) = fno3acc * totnit

      aerosol( LSO4COR ) = ts6cor * xl * recipap1
C
      aerosol( LNAAKN  ) = aerosol( LNAAKN ) * ealfa3t
      aerosol( LCLAKN  ) = aerosol( LCLAKN ) * ealfa3t
      aerosol( LNAACC  ) = naacc * xl * recipap1
      aerosol( LCLACC  ) = clacc * xl * recipap1
      aerosol( LNACOR  ) = nacor * xl * recipap1
      aerosol( LCLCOR  ) = clcor * xl * recipap1

      aerosol( LNUMAKN ) = aerosol( LNUMAKN ) * ealfa0t

C...compute the final accumulation aerosol 3rd moment

      m3new = ( aerosol( LSO4ACC  ) * SGRAERMW( LSO4ACC  ) / 1.8e6
     &      +   aerosol( LNH4ACC  ) * SGRAERMW( LNH4ACC  ) / 1.8e6
     &      +   aerosol( LNO3ACC  ) * SGRAERMW( LNO3ACC  ) / 1.8e6
     &      +   aerosol( LXYL3ACC ) * SGRAERMW( LXYL3ACC ) / 2.0e6
     &      +   aerosol( LTOL3ACC ) * SGRAERMW( LTOL3ACC ) / 2.0e6
     &      +   aerosol( LBNZ3ACC ) * SGRAERMW( LBNZ3ACC ) / 2.0e6
     &      +   aerosol( LORGCACC ) * SGRAERMW( LORGCACC ) / 2.0e6
     &      +   aerosol( LISO3ACC ) * SGRAERMW( LISO3ACC ) / 2.0e6
     &      +   aerosol( LOLGAACC ) * SGRAERMW( LOLGAACC ) / 2.0e6
     &      +   aerosol( LOLGBACC ) * SGRAERMW( LOLGBACC ) / 2.0e6
     &      +   aerosol( LORGPACC ) * SGRAERMW( LORGPACC ) / 2.0e6
     &      +   aerosol( LECACC   ) * SGRAERMW( LECACC   ) / 2.2e6
     &      +   aerosol( LPRIACC  ) * SGRAERMW( LPRIACC  ) / 2.2e6
     &      +   aerosol( LNAACC   ) * SGRAERMW( LNAACC   ) / 2.2e6
     &      +   aerosol( LCLACC   ) * SGRAERMW( LCLACC   ) / 2.2e6 )
CCC     &      * 6.0 / PI      ! cancels out in division below

      m3newcor = ( aerosol( LSO4COR  ) * SGRAERMW( LSO4COR  ) / 1.8e6
     &         +   aerosol( LNH4COR  ) * SGRAERMW( LNH4COR  ) / 1.8e6
     &         +   aerosol( LNO3COR  ) * SGRAERMW( LNO3COR  ) / 1.8e6
     &         +   aerosol( LPRICOR  ) * SGRAERMW( LPRICOR  ) / 2.2e6
     &         +   aerosol( LNACOR   ) * SGRAERMW( LNACOR   ) / 2.2e6
     &         +   aerosol( LCLCOR   ) * SGRAERMW( LCLCOR   ) / 2.2e6 )

      aerosol( LSRFAKN ) = aerosol( LSRFAKN ) * ealfa2t
      aerosol( LSRFACC ) = aerosol( LSRFACC )
     &                   * ( m3new / MAX( m3old, CONCMIN) ) ** TWOTHIRDS
      aerosol( LSRFCOR ) = aerosol( LSRFCOR )
     &        * ( ( m3newcor / MAX( m3oldcor, CONCMIN ) ) ** TWOTHIRDS )

! hydrogen ion concentrations (for scavenging coefficient calculations in
! driver routine)

      hplus = liquids( lacl      )

      return

C...formats

1001  format (1X,'DSIVDT(0) =', F10.5,
     &       'TS6=', F10.5, 'DTW(0)=', F10.5)

      end
