!=======================================================================
!    SCIPUFF data - chemistry multicomponent
!=======================================================================
MODULE chem_fi

  USE multcomp_fd
  USE sagstr_fd
  USE multcomp_aqaer_fd, ONLY: ChemMC_aqaer_str
  USE chem_mpi_fi, ONLY: ChemMCHead_mpi_str,ChemAqArHd_mpi_str,ChemReact_mpi_str
  USE sagstr_fd

  SAVE

  TYPE( ChemMC_str ), DIMENSION(:,:), ALLOCATABLE, TARGET :: chemMC
  TYPE( ChemMC_str ), POINTER :: chem

!---- Pointers to fixed data in main structure


  TYPE( ChemMCHead_mpi_str )                            :: chemMCHead
  TYPE( ChemAqArHd_mpi_str )                            :: ChemAqArHd
  TYPE( ChemReact_mpi_str  ),DIMENSION(:),ALLOCATABLE   :: ChemRxn
  TYPE( ChemMC_aqaer_str   )                            :: chem_aqaer

  TYPE( ChemSpecies_str ), DIMENSION(:), POINTER :: species

  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: fast
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: slow
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: gaseous
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: particle
  TYPE( ChemSpecies_ptr ), DIMENSION(:), POINTER :: equil

!---- Local variables and movable pointers

  TYPE( ChemReaction_ptr ), DIMENSION(:), POINTER :: reaction

  INTEGER, DIMENSION(:), POINTER :: indx_eq
  INTEGER, DIMENSION(:), POINTER :: itype_eq
  INTEGER, DIMENSION(:), POINTER :: irow_eq

  INTEGER nfast, nslow, nequil, nambient, nspecies, nspectot
  INTEGER ngaseous, nparticle
  ! For useMPI
  INTEGER ngd_chem_sav, nbad_chem_sav
  INTEGER chemID
  LOGICAL lscav
  INTEGER ndir_eq, nlin_eq, nsolve_eq, nsubs_eq
  INTEGER numode, ngd_chem, nbad_chem, tot_bad

  INTEGER nrxn   !No.of reactions in current list pointed to by "reaction"
  INTEGER H2O    !index for water in species list
  INTEGER iprtyp !precipitation type

  REAL    rtol
  REAL    vol, vself, pres, temp, zen_ang
  REAL    cld_att, zlev
  REAL    vol2            !Used for overlap calculation (jpuf)
  REAL    volx, vselfx    !Used for effective volume (vol is inverse volume)
  REAL    dTdt
  LOGICAL ResetEq
  LOGICAL :: lDiagno     = .FALSE.   ! Logical to print diagnostics
  LOGICAL :: lDepBin     = .TRUE.    ! Logical to print binary dgn
  LOGICAL :: lMonthAmb   = .TRUE.    ! Logical to use monthly ambient
  LOGICAL :: DoChemistry = .TRUE.    ! Logical to use step chemistry (w/ IMC file)

  INTEGER, DIMENSION(2,2) :: MonthDay  ! MonthDay,UpdatedRadical(0-False,1-True) value for chem%Ambient%amb

  REAL, DIMENSION(:),   POINTER :: pdnrate
  REAL, DIMENSION(:),   POINTER :: ydots
  REAL, DIMENSION(:,:), POINTER :: pdm
  REAL, DIMENSION(:),   ALLOCATABLE :: pratebl !Used for scavenging

  COMMON /chem_fi_cmn/ vol, vself, pres, temp, zen_ang
  COMMON /chem_fi_cmn/ cld_att, zlev
  COMMON /chem_fi_cmn/ pdnrate, ydots, pdm
  COMMON /chem_fi_cmn/ nfast, nslow, nequil, nambient, nspecies, nspectot
  COMMON /chem_fi_cmn/ ngaseous, nparticle, chem_aqaer
  COMMON /chem_fi_cmn/ chemID
  COMMON /chem_fi_cmn/ lscav
  COMMON /chem_fi_cmn/ ndir_eq, nlin_eq, nsolve_eq, nsubs_eq
  COMMON /chem_fi_cmn/ nrxn, numode
  COMMON /chem_fi_cmn/ iprtyp
  COMMON /chem_fi_cmn/ rtol
  COMMON /chem_fi_cmn/ vol2
  COMMON /chem_fi_cmn/ volx, vselfx
  COMMON /chem_fi_cmn/ dTdt
  COMMON /chem_fi_cmn/ ResetEq
  COMMON /chem_fi_cmn/ chem
  COMMON /chem_fi_cmn/ species
  COMMON /chem_fi_cmn/ fast
  COMMON /chem_fi_cmn/ slow
  COMMON /chem_fi_cmn/ gaseous
  COMMON /chem_fi_cmn/ particle
  COMMON /chem_fi_cmn/ equil
  COMMON /chem_fi_cmn/ reaction
  COMMON /chem_fi_cmn/ indx_eq
  COMMON /chem_fi_cmn/ itype_eq
  COMMON /chem_fi_cmn/ irow_eq
!$OMP THREADPRIVATE(/chem_fi_cmn/)

  REAL(8), DIMENSION(:,:), ALLOCATABLE :: star8
  REAL(8), DIMENSION(:,:), ALLOCATABLE :: cmax8
  REAL(8), DIMENSION(:),   ALLOCATABLE :: vol8
  REAL(8), DIMENSION(:),   ALLOCATABLE :: vself8


  LOGICAL lChemAmbDosRes
  REAL    xChemAmbDosRes
  TYPE( SAGgrid_str ),   POINTER     :: grdC
  INTEGER iMult, nMult

END MODULE chem_fi

MODULE diagnostics_fi

  SAVE

  LOGICAL :: DgnDbg = .FALSE.

  INTEGER, PARAMETER :: NDIAG = 10

  CHARACTER(1), DIMENSION(NDIAG), PARAMETER :: SFX  = (/ 'e','s','b','t','d',&
                                                         'w','c','a','r','p' /)
  INTEGER ndvar, ndump

  CHARACTER(18), DIMENSION(:), ALLOCATABLE :: dgname

  REAL(8), DIMENSION(:), ALLOCATABLE :: emission, statics, boundary, transport
  REAL(8), DIMENSION(:), ALLOCATABLE :: ddeposition, wdeposition, chemistry
  REAL(8), DIMENSION(:), ALLOCATABLE :: active, removed, ppmfac

END MODULE diagnostics_fi
