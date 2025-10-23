!=======================================================================
!    SCIPUFF definitions - multicomponent
!=======================================================================
MODULE multcomp_fd

  USE DefSize_fd
  USE MapCoord_fd

!------ List of multi-component type-ID's

  INTEGER, PARAMETER :: MC_CHEM = 1           !reactive chemistry

!------ List of CHEM multi-component control bits

  INTEGER, PARAMETER :: CCB_UTC   = 0         !Ambient file time convention

!------ Set up parameters for staged chemistry

  INTEGER, PARAMETER :: MAX_STAGE = 3  ! Maximum number of stages
  INTEGER, PARAMETER :: NCRITERIA = 6  ! No. of criteria for staged chemistry
  INTEGER, PARAMETER :: NKEY_SPEC = 10 ! No. of key species for staged chemistry
  INTEGER, PARAMETER :: NKEY_RXNS = 5  ! No. of key reactions for staged chemistry

!------ Pointers for key reactions in staged chemistry

  INTEGER, PARAMETER :: IPHNO3  = 1 ! Production of HNO3 by NO and OH
  INTEGER, PARAMETER :: IPNO3   = 2 ! Production of NO3 by NO2 and O3
  INTEGER, PARAMETER :: INOO3   = 3 ! NO + O3 --> NO2
  INTEGER, PARAMETER :: INO3NO2 = 4 ! NO3 + NO2 --> N2O5
  INTEGER, PARAMETER :: INOHO2  = 5 ! Reaction of NO with HO2

!------ Pointers for key species in staged chemistry

  INTEGER, PARAMETER :: sNO   = 1
  INTEGER, PARAMETER :: sNO2  = 2
  INTEGER, PARAMETER :: sO3   = 3
  INTEGER, PARAMETER :: sOH   = 4
  INTEGER, PARAMETER :: sHO2  = 5
  INTEGER, PARAMETER :: sNO3  = 6
  INTEGER, PARAMETER :: sHNO3 = 7
  INTEGER, PARAMETER :: sN2O5 = 8
  INTEGER, PARAMETER :: sO1D  = 9
  INTEGER, PARAMETER :: sC2O3 = 10

!------ List of CHEM species types

  INTEGER, PARAMETER :: ID_SPECIES_FAST        = 1           !Fast species
  INTEGER, PARAMETER :: ID_SPECIES_SLOW        = 2           !Slow species
  INTEGER, PARAMETER :: ID_SPECIES_EQUILIBRIUM = 3           !Equilibrium species
  INTEGER, PARAMETER :: ID_SPECIES_AMBIENT     = 4           !Ambient species
  INTEGER, PARAMETER :: ID_SPECIES_PARTICLE    = 5           !Aerosol particle species class identifier

!------ List of CHEM reaction type bits (can set multiple bits)

  INTEGER, PARAMETER :: ID_REACT_FAST        = 1           !Fast reaction
  INTEGER, PARAMETER :: ID_REACT_SLOW        = 2           !Slow reaction
  INTEGER, PARAMETER :: ID_REACT_EQUILIBRIUM = 3           !Equilibrium reaction
  INTEGER, PARAMETER :: ID_REACT_LINEAR      = 4           !Linear reaction
  INTEGER, PARAMETER :: ID_REACT_AMBIENT     = 5           !Ambient reaction
  INTEGER, PARAMETER :: ID_REACT_THERMAL     = 6           !Exo/endothermic reaction

!------ Solution types for equilibrium species

  INTEGER, PARAMETER :: ISOLVE_EQ =  0   !fully coupled solution for equilibrium
  INTEGER, PARAMETER :: IDIR_EQ   = -1   !direct solution for equilibrium
  INTEGER, PARAMETER :: ISUBS_EQ  = -2   !final substitution for equilibrium

!------ List of CHEM unit conversion types

  INTEGER, PARAMETER :: NO_CONVERSION = 0 !Rate/Species in same units
  INTEGER, PARAMETER :: MOLECULE_PPM  = 1 !molecule/cm3 <-> PPM conversion
  INTEGER, PARAMETER :: G_PPM         = 2 !grams <-> PPM conversion
  INTEGER, PARAMETER :: G_MOLECULE    = 3 !grams <-> molecule/cm3 conversion

  INTEGER, PARAMETER :: UNIT_PPM      = 0 !PPM concentration units
  INTEGER, PARAMETER :: UNIT_MOLECULE = 1 !molecule/cm3 concentration units
  INTEGER, PARAMETER :: UNIT_G        = 2 !g/s emission units
  INTEGER, PARAMETER :: UNIT_UGM3     = 3 !ug/m3 output concentration units


  INTEGER, PARAMETER :: ATB_MEDOC      = 0 !MEDOC ambient file
  INTEGER, PARAMETER :: ATB_BINARY     = 4 !Binary file


TYPE ChemSpecies_str
  SEQUENCE
  CHARACTER(16) :: name
  CHARACTER(16) :: classAux        !Character string for class aux type and units
  INTEGER       :: class
  INTEGER       :: ID            !Species number
  INTEGER       :: eqID          !ID for equilibrium species solver
  LOGICAL       :: lstar         !Flag for overlap concentration
  REAL          :: ambient       !Ambient concentration
  REAL          :: tol           !Absolute tolerance
  LOGICAL       :: ldos          !Dosage flag
  LOGICAL       :: ldep          !Deposition flag
  REAL          :: vdep          !Deposition velocity
  REAL          :: scav          !Scavenging coefficient
  REAL          :: mw            !Molecular weight
  REAL          :: taudry         !
  REAL          :: tauwet         !
  REAL          :: nit            ! Number of nitrogen molecules for balance
  REAL          :: lim            ! Volume limit for species
  REAL          :: Henry0  !EPRI model Henry's law constant for species at 298 K
  REAL          :: TpFac   !EPRI model temperature factor for Henry's law constant
  REAL          :: RxFac   !EPRI model reactivity factor
  REAL          :: SrfRFac !EPRI surface resistance scaling factor
  LOGICAL       :: ldrydep !EPRI dry deposition flag

!---- Working storage
  REAL    mass
  REAL    conc
  REAL    amb
  REAL    msav
  REAL    csav
  REAL    csav_noy
  REAL    mddp
  REAL    mwdp
  REAL    mass2     !Used for overlap (jpuf)
  REAL    conc2
END TYPE ChemSpecies_str

TYPE ChemSpecies_ptr
  SEQUENCE
  TYPE( ChemSpecies_str ), POINTER :: s
END TYPE ChemSpecies_ptr

TYPE ChemReact_str
  SEQUENCE
  INTEGER       :: class                !Reaction class

  INTEGER                        :: type  !Reaction type
  DOUBLE PRECISION, DIMENSION(:), &
                         POINTER :: data  !Rate coefficient data
  REAL                           :: k     !current rate coefficient (set and used during calculation)

  REAL                           :: H     !heat of reaction (J/kg) * conc units conversion factor

  INTEGER       :: A                    !ID of species A
  INTEGER       :: B                    !ID of species B (0 for linear reaction)
  REAL          :: fB                   !Reactant stoichiometric factor
  INTEGER       :: nP                   !No. of product species
  INTEGER, DIMENSION(:), POINTER :: P   !List of product ID's
  REAL,    DIMENSION(:), POINTER :: fP  !List of product stoichiometric factors
END TYPE ChemReact_str

TYPE ChemReaction_ptr
  SEQUENCE
  TYPE( ChemReact_str ), POINTER :: r
END TYPE ChemReaction_ptr

TYPE ChemGroup_str
  SEQUENCE
  CHARACTER(16) :: Name
  INTEGER       :: nComp
  INTEGER, DIMENSION(:), POINTER :: iComp
  REAL,    DIMENSION(:), POINTER :: fComp
END TYPE ChemGroup_str

TYPE ChemAmbient_str
  SEQUENCE
  INTEGER  type
  INTEGER  n2d                            !No. of 2d fields in file
  INTEGER  n3d                            !No. of 3d fields in file
  INTEGER  nskip                          !No. of header records between time record and 3d data

  LOGICAL  InterpAmb                      !Interpolate instead of stepping ambient concentration

  INTEGER, DIMENSION(:), POINTER :: ID    !Index for each species into ambient file, 0=>missing
  LOGICAL, DIMENSION(:), POINTER :: read  !Flag for reading/skipping each species on ambient file

  INTEGER  nx                             !No. of points in x-direction
  INTEGER  ny                             !No. of points in y-direction
  INTEGER  nz                             !No. of points in z-direction

  REAL                           :: x0, dx, x1
  REAL                           :: y0, dy, y1
  REAL,    DIMENSION(:), POINTER :: zGrid !Vertical grid levels

  INTEGER  terrain                         !Flag for terrain= -1(no terrain); 0(terrain already read); otherwise, read terrain
  REAL,    DIMENSION(:),   POINTER :: ht   !terrain elevation
  REAL,    DIMENSION(:,:), POINTER :: zMid !3d height field (cell center)

  INTEGER it1
  REAL    time1
  REAL    time2
  REAL    tStepAmb                           !time for stepped ambient
  REAL, DIMENSION(:,:,:), POINTER :: amb     !ambient (x+y+z,species,t) data
  REAL, DIMENSION(:,:,:),   POINTER :: stepAmb !stepped ambient (x+y,z,species) data
  REAL, DIMENSION(:,:,:,:), POINTER :: HrAmb   !Hourly ambient (x+y,z,species,hr) data for each month

  TYPE( MapCoord) :: coord                        !Map coordinate info

END TYPE ChemAmbient_str

TYPE ChemSFlux_str
  SEQUENCE
  INTEGER  type
  INTEGER  n2d                             !No. of 2d fields in file
  INTEGER  n3d                             !No. of 3d fields in file
  INTEGER  nskip                           !No. of header records between time record and 3d data
  INTEGER, DIMENSION(:), POINTER  :: sID    !Index for each species into ambient file, 0=>missing
  LOGICAL, DIMENSION(:), POINTER  :: sRead  !Flag for reading/skipping each species on ambient file

  INTEGER  nx                              !No. of points in x-direction
  INTEGER  ny                              !No. of points in y-direction
  INTEGER  nz                              !No. of points in z-direction

  REAL                            :: x0, dx, x1
  REAL                            :: y0, dy, y1

  INTEGER it1
  REAL    time1
  REAL    time2
  REAL, DIMENSION(:,:,:), POINTER :: sflx    !surface flux (x+y,species,t) data
END TYPE ChemSFlux_str

!------ Main Chemistry Structure ---------------------------------------------------

TYPE ChemMC_str

  SEQUENCE

  INTEGER  flag         !Control flag

  INTEGER  cUnits       !Concentration units type
  INTEGER  eUnits       !Emission units type
  INTEGER  kUnits       !Reaction rate concentration units type
  INTEGER  oUnits       !Concentration output units for gas species
  INTEGER  pTypeN       ! Number of particle types
  CHARACTER(6), DIMENSION(:), POINTER :: pTypes   !Particle types
  CHARACTER(6), DIMENSION(:), POINTER :: pUnits   !Particle type units

  INTEGER  kConv        !Reaction rate conversion factor type
  INTEGER  eConvType    !Emissions conversion factor type
  REAL     eConv        !Emissions units conversion factor
  REAL     tConv        !Reaction time units conversion factor

  REAL     rtol         !Relative tolerance

  LOGICAL  thermal      !Flag for exo/endothermic reactions

!---- Multicomponent species

  INTEGER nSpecies     !Total No. of component species
  TYPE( ChemSpecies_str ), DIMENSION(:), POINTER :: species

!---- Multicomponent reactions

  INTEGER nReactions   !No. of reactions
  INTEGER nZenith      !No. of zenith angles for photochemical rate data
  TYPE( ChemReact_str ), DIMENSION(:), POINTER :: reaction
  REAL,                  DIMENSION(:), POINTER :: zenith

!---- Pointers for "star" species

  INTEGER  nStar        !No. of non-linear overlap species
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: star

!---- Pointers for fast/slow species

  INTEGER nFast         !No. of fast species
  INTEGER nSlow         !No. of slow species
  INTEGER nGaseous      !No. of gaseous species
  INTEGER nParticle     !No. of particle species
  INTEGER nEquilibrium  !No. of equilibrium species
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: fast
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: slow
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: gaseous
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: particle
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: equil

!---- Pointers for fast/slow reactions

  INTEGER nFastReact   !No. of fast reactions
  INTEGER nSlowReact   !No. of slow reactions
  INTEGER nEquilReact  !No. of equilibrium reactions
  INTEGER nPhotoReact  !No. of photochemical reactions
  TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: fReact
  TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: sReact
  TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: eReact
  TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: pReact

!---- Equilibrium solver arrays

  INTEGER  nSolveEq     !No. of species for nonlinear equilibrium solver
  INTEGER  nLinearEq    !No. of species that can be eliminated
  INTEGER  nDirectEq    !No. of species that can be solved directly
  INTEGER  nSubstEq     !No. of species that can be substituted
  INTEGER, DIMENSION(:), POINTER :: IndexEq   !List of equilibrium species in the order that they must be solved
  INTEGER, DIMENSION(:), POINTER :: TypeEq    !List of solution types
  INTEGER, DIMENSION(:), POINTER :: RowEq     !List of row numbers

  INTEGER                       :: imax   !Offset pointer for puff list

!---- Staged chemistry flag

  LOGICAL                   lStage
  INTEGER                   nStage

!---- Species balance flag

  LOGICAL                   lBalance

!---- Step ambient flag

  LOGICAL                   lStepAmb  !Step the ambient concentration

!---- Add ambient concentration flag

  LOGICAL                   laddAmb   !Add the ambient concentration

!---- Ambient concentation surface flux

  LOGICAL                   lSfcFlx   !Use surface flux data
  INTEGER                   lunSfcFlx
  CHARACTER(PATH_MAXLENGTH) sFlxFile
  TYPE( ChemSFlux_str )     sFlux

  INTEGER nOutGroup
  TYPE( ChemGroup_str ), DIMENSION(:), POINTER :: OutGroup

!---- Ambient concentration from file

  LOGICAL                   useHetChem
  LOGICAL                   lAmbFile
  INTEGER                   lunAmb
  CHARACTER(PATH_MAXLENGTH) ambFile
  TYPE( ChemAmbient_str )   Ambient
  TYPE( ChemAmbient_str ), DIMENSION(12) :: MonthlyAmb ! Ambient for each month

  CHARACTER(PATH_MAXLENGTH) FileNotUsed

END TYPE ChemMC_str

TYPE ChemSpecies_out
  CHARACTER(16) :: name
  LOGICAL       :: lstar         !Flag for overlap concentration
  REAL    mass
  REAL    conc
  REAL    amb
END TYPE ChemSpecies_out
TYPE ChemMC_out
  INTEGER nFast         !No. of fast species
  INTEGER nSlow         !No. of slow species
  INTEGER nParticle     !No. of particle species
  INTEGER nEquilibrium  !No. of equilibrium species
  INTEGER nStar         !No. of non-linear overlap species
  INTEGER nSpecies      !Total No. of component species
  TYPE( ChemSpecies_out ), DIMENSION(:), POINTER :: species
END TYPE ChemMC_out
END MODULE multcomp_fd
