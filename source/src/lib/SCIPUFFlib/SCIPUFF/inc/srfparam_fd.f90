MODULE srfparam_fd

!----- Surface field pointers / parameters

! ---  Field variable pointers

  INTEGER, PARAMETER :: ISRF_C    = 1 !Mean conc. pointer
  INTEGER, PARAMETER :: ISRF_CC   = 2 !Group variance pointer
  INTEGER, PARAMETER :: ISRF_SL   = 3 !Scale (*variance) pointer
  INTEGER, PARAMETER :: ISRF_CCT  = 4 !Total variance pointer
  INTEGER, PARAMETER :: ISRF_C0   = 5 !Max conc
  INTEGER, PARAMETER :: ISRF_CCB  = 6 !Mean square conc.
  INTEGER, PARAMETER :: ISRF_CCTB = 7 !Mean square total conc.
  INTEGER, PARAMETER :: ISRF_TSCL = 8 !Puff timescale

  INTEGER, PARAMETER :: ISRF_TOT  = 8 !Total no. of field pointers

! ---  Field counters

  INTEGER, PARAMETER :: NV_BASIC    = 3 !Standard (Mean,var,scale)
  INTEGER, PARAMETER :: NV_EVAP     = 2 !Extra sfc evap (liquid/dep)

! ---  Surface Field flag bits

  INTEGER, PARAMETER :: SFLAG_TOT = 0 ! Total field
  INTEGER, PARAMETER :: SFLAG_VAP = 2 ! Vapor group fields for liquid material
  INTEGER, PARAMETER :: SFLAG_LIQ = 3 ! Vapor group fields for liquid material

! ---  Surface Block types

  INTEGER, PARAMETER :: SBLK_PLOT_ADJ =-3 ! Adjoint Plot block - all 3 scales
  INTEGER, PARAMETER :: SBLK_PLOT_MC  =-2 ! Multicomponent Plot block - Mean and Var only
  INTEGER, PARAMETER :: SBLK_PLOT     =-1 ! Plot block - Mean and Var only
  INTEGER, PARAMETER :: SBLK_EFFECTS  = 0 ! Surface effects block
  INTEGER, PARAMETER :: SBLK_STD      = 1 ! Std Mean,Var,Scale
  INTEGER, PARAMETER :: SBLK_EVAP     = 4 ! Liquid evaporation Mean,Var,Scale,Ddrop,Warea
  INTEGER, PARAMETER :: SBLK_MULTI_DEP = 8 ! Multi-component deposition
  INTEGER, PARAMETER :: SBLK_MULTI_DOS = 9 ! Multi-component dose

! ---  Effects Block types


! ---  Dezone Field types

  INTEGER, PARAMETER :: DEZONE_AVG   = 0 ! Simple dezone field
  INTEGER, PARAMETER :: DEZONE_MEAN  = 1 ! Mean dezone field
  INTEGER, PARAMETER :: DEZONE_VAR   = 2 ! Variance dezone field
  INTEGER, PARAMETER :: DEZONE_SCALE = 3 ! Scale dezone field

! ---  Surface file type

  CHARACTER(13), PARAMETER :: SURFACE_FILE_TYPE ='BLOCK_VERSION'

END MODULE srfparam_fd
