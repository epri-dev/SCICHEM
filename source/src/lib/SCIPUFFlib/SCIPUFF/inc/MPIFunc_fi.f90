MODULE mpi_fi

  USE localmpi, ONLY: MPI_ADDRESS_KIND,MPI_STATUS_SIZE

  SAVE

  TYPE statType
    INTEGER nStat
    CHARACTER(80)  sRoutine
    CHARACTER(128) sMessage
    CHARACTER(128) sInform
    CHARACTER(128) sAction
  END TYPE statType

  TYPE( statType )                                           :: Status

  TYPE AqAerInpT
    INTEGER nError
    CHARACTER(128) prjName
    CHARACTER(128) mcFile
  END TYPE AqAerInpT

  TYPE( AqAerInpT )                                          :: AqAerInp

  REAL,    PARAMETER                                         :: MP_BUFFER_MEM = 1e6 ! Max MPI send/recv buffer in bytes
  INTEGER, PARAMETER                                         :: eOPEN  = -1
  INTEGER, PARAMETER                                         :: eALLOC = -2
  INTEGER, PARAMETER                                         :: eMPI   = -3
  INTEGER, PARAMETER                                         :: sDone  = 1

  INTEGER, PARAMETER                                         :: tag_chunk = 1
  INTEGER, PARAMETER                                         :: tag_putmc = 2
  INTEGER, PARAMETER                                         :: tag_putsp = 3
  INTEGER, PARAMETER                                         :: tag_getmc = 4
  INTEGER, PARAMETER                                         :: tag_getsp = 5

  INTEGER                                                    :: Status_MPI
  INTEGER                                                    :: ChemMCHead_MPI
  INTEGER                                                    :: ChemAqArHd_MPI
  INTEGER                                                    :: ChemSpecies_MPI
  INTEGER                                                    :: ChemReact_MPI
  INTEGER                                                    :: StepMCdat_MPI
  INTEGER                                                    :: PuffStruc_MPI
  INTEGER                                                    :: PuffMet_MPI
  INTEGER                                                    :: PuffSpec_MPI

  INTEGER                                                    :: myid = 0
  INTEGER                                                    :: numprocs = 1
  INTEGER                                                    :: ierr
  INTEGER                                                    :: source, dest
  INTEGER                                                    :: npuf_chunk
  INTEGER                                                    :: max_chunk
  INTEGER                                                    :: pufNo

  INTEGER                                                    :: n_int, n_real
  INTEGER                                                    :: n_char, n_lgl

  INTEGER, DIMENSION(MPI_STATUS_SIZE)                        :: stat

  INTEGER, DIMENSION(:), ALLOCATABLE                         :: nchunk, ipuf_start, ipuf_end
  INTEGER, DIMENSION(:), ALLOCATABLE                         :: block_lengths
  INTEGER, DIMENSION(:), ALLOCATABLE                         :: typelist

  INTEGER (KIND=MPI_ADDRESS_KIND), DIMENSION(:), ALLOCATABLE :: disp
  INTEGER (KIND=MPI_ADDRESS_KIND)                            :: base

  CHARACTER, DIMENSION(:), ALLOCATABLE                       :: MPI_MCHEAD

  LOGICAL                                                    :: useMPI   = .FALSE.
  LOGICAL                                                    :: isSerial = .TRUE.
END MODULE mpi_fi

!=======================================================================

MODULE chem_mpi_fi

  SAVE

  INTEGER, PARAMETER :: ID_K_RAD      = 0 !Radiation dependent
  INTEGER, PARAMETER :: UNIT_PPM      = 0 !PPM concentration units
  INTEGER, PARAMETER :: UNIT_MOLECULE = 1 !molecule/cm3 concentration units

  ! Use to transfer initial values and dimensions of chemMC components
  TYPE ChemMCHead_mpi_str
    SEQUENCE
    LOGICAL  ldyn         !Dynamics flag
    LOGICAL  lwash        !Wash flag
    INTEGER  flag         !Control flag
    INTEGER  cUnits       !Concentration units type
    INTEGER  eUnits       !Emission units type
    INTEGER  kUnits       !Reaction rate concentration units type
    INTEGER  kConv        !Reaction rate conversion factor type
    INTEGER  eConvType    !Emissions conversion factor type
    REAL     eConv        !Emissions units conversion factor
    REAL     tConv        !Reaction time units conversion factor
    REAL     rtol         !Relative tolerance
    LOGICAL  thermal      !Flag for exo/endothermic reactions
    !---- Multicomponent species
    INTEGER nSpecies      !Total No. of component species
    INTEGER nReactions    !No. of reactions
    INTEGER nZenith       !No. of zenith angles for photochemical rate data
    INTEGER  nStar        !No. of non-linear overlap species
    !---- Pointers for fast/slow species
    INTEGER nFast         !No. of fast species
    INTEGER nSlow         !No. of slow species
    INTEGER nParticle     !No. of particle species
    INTEGER nEquilibrium  !No. of equilibrium species
    !---- Pointers for fast/slow reactions
    INTEGER nFastReact   !No. of fast reactions
    INTEGER nSlowReact   !No. of slow reactions
    INTEGER nEquilReact  !No. of equilibrium reactions
    INTEGER nPhotoReact  !No. of photochemical reactions
    !---- Equilibrium solver arrays
    INTEGER  nSolveEq     !No. of species for nonlinear equilibrium solver
    INTEGER  nLinearEq    !No. of species that can be eliminated
    INTEGER  nDirectEq    !No. of species that can be solved directly
    INTEGER  nSubstEq     !No. of species that can be substituted
    INTEGER  imax         !Offset pointer for puff list
    LOGICAL  lBalance
    INTEGER  nRxnData
    INTEGER  nRxnNp
    INTEGER  nQLine
  END TYPE ChemMCHead_mpi_str

  TYPE ChemAqArHd_mpi_str
    SEQUENCE
    LOGICAL        :: laero
    INTEGER        :: naerp
    INTEGER        :: nsec_aer
    CHARACTER(128) :: file_imc
  END TYPE ChemAqArHd_mpi_str

  TYPE ChemReact_mpi_str
    SEQUENCE
    INTEGER       :: class  !Reaction class
    INTEGER       :: type   !Reaction type
    REAL          :: k      !current rate coefficient (set and used during calculation)
    INTEGER       :: A      !ID of species A
    INTEGER       :: B      !ID of species B (0 for linear reaction)
    REAL          :: fB     !Reactant stoichiometric factor
    INTEGER       :: nP     !No. of product species
    INTEGER       :: nData  !No. of data values
  END TYPE ChemReact_mpi_str

  TYPE puff_mpi_str
    SEQUENCE
    REAL(8) xbar,ybar
    REAL    zbar,det
    REAL    sxx, sxy, sxz, syy, syz, szz
    REAL    axx, axy, axz, ayy, ayz, azz
    REAL    c,cc,ccb,csav
    REAL    sr
    REAL    vol,pres,temp
    REAL    tdyn,vself
    INTEGER pno,ityp,idtl,naux
  END TYPE  puff_mpi_str

  TYPE met_mpi_str
    SEQUENCE
    REAL    tb,pb,hb
    REAL    cw,cc,ctop,cbot,sfac
    REAL    prate,prbl
    REAL    zinv,hp
    REAL    us2, ws2, xml
    REAL    zb1, zruf
    REAL    ustdep, tdep
    REAL    zenith
    REAL    lat, lon
    INTEGER lucat, jday
  END TYPE  met_mpi_str

  TYPE SplitMetT
    SEQUENCE
    REAL    su2, dudx, dudy, dvdx, dvdy
  END TYPE  SplitMetT

 !Declared in chem_fi
 !TYPE( ChemMCHead_mpi_str )                          :: chemMCHead
 !TYPE( ChemAqArHd_mpi_str )                          :: ChemAqArHd
 !TYPE( ChemReact_mpi_str ),DIMENSION(:),ALLOCATABLE  :: ChemRxn


END MODULE chem_mpi_fi

!=======================================================================

MODULE StepMC_mpi_fi

  USE chem_mpi_fi

  SAVE

  INTEGER, PARAMETER :: iDDep  = 1
  INTEGER, PARAMETER :: iWDep  = 2
  INTEGER, PARAMETER :: iMass  = 3
  INTEGER, PARAMETER :: iConc  = 4
  INTEGER, PARAMETER :: iAmb   = 5
  INTEGER, PARAMETER :: nVarSp = 5

  ! Input chem%species variables. Dimension is (nPuf,nSpecies,nVarSp)
  REAL, DIMENSION(:,:,:),ALLOCATABLE :: StepMcSp  !Chem species variable

  TYPE  StepMCdataT
    SEQUENCE
    INTEGER  ngd                    !Number of good steps
    INTEGER  nbd                    !Number of bad steps
    INTEGER  ipuf
    REAL t                          !Current time
    REAL dt                         !Time step (sec)
    TYPE ( puff_mpi_str    ) :: pf  !Puff structure
    TYPE ( met_mpi_str     ) :: pm  !Met at the puff loc
  END TYPE  StepMCdataT

  TYPE  SplitDataT
    SEQUENCE
    INTEGER  ipuf
    INTEGER  npuf
    REAL     dt
    REAL     frac
  END TYPE  SplitDataT

  TYPE  SplitZiT
    SEQUENCE
    LOGICAL lszi
    REAL    sz, zinv
    REAL    hp, hx, hy
  END TYPE  SplitZiT

  TYPE (StepMCdataT), DIMENSION(:), ALLOCATABLE, TARGET :: StepMCdat
  TYPE (StepMCdataT), POINTER                           :: pStepMCdat
  TYPE (SplitDataT) , DIMENSION(:), ALLOCATABLE         :: puffNos
  TYPE (SplitMetT)  , DIMENSION(:), ALLOCATABLE         :: puffSplitMet
  TYPE (SplitZiT)   , DIMENSION(:), ALLOCATABLE         :: puffSplitZi

  INTEGER iStepMC

END MODULE StepMC_mpi_fi
