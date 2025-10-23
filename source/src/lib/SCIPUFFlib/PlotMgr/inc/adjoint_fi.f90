MODULE adjoint_fi
  USE sagstr_fd
  USE plotlist_fd

  SAVE

  INTEGER, DIMENSION(5), PARAMETER :: I_FIELD = (/ 1,2,3,4,5 /)

  TYPE Adjoint_str
    SEQUENCE
    REAL      :: mean
    REAL      :: sig
    REAL      :: min
    REAL      :: scaleS
    REAL      :: scaleT
    REAL      :: scaleV
    LOGICAL   :: trigger
  END TYPE Adjoint_str

  TYPE Adjoint_ptr
    SEQUENCE
    INTEGER                      :: imat
    TYPE( Adjoint_str ), POINTER :: m
  END TYPE Adjoint_ptr

  TYPE AdjointMat_str
    SEQUENCE
    REAL      :: xrel
    REAL      :: yrel
    REAL      :: zrel
    REAL      :: trel
    REAL      :: tdur
    REAL      :: mass
    REAL      :: umet
    REAL      :: vmet
  END TYPE AdjointMat_str

  TYPE AdjAxisVal
    SEQUENCE
    REAL, DIMENSION(:), POINTER :: val
  END TYPE AdjAxisVal

  TYPE AdjointMaxLocTime_str
    SEQUENCE
    INTEGER                        :: ID           !Field ID
    INTEGER                        :: nLoc         !# of values
    INTEGER                        :: nDim         !# of extra dimensions
    INTEGER, DIMENSION(:), POINTER :: DimX         !# of values in extra dimensions
    REAL,    DIMENSION(:), POINTER :: tLoc         !Time (relative to Max)
    REAL,    DIMENSION(:), POINTER :: fLoc         !LocMax value
    REAL,    DIMENSION(:), POINTER :: fMass        !Mass Estimate value
    CHARACTER(64)                  :: xLab         !x-axis label (t-tMax plus units)
    CHARACTER(64)                  :: MassUnit     !mass units
    CHARACTER(64),      DIMENSION(:), POINTER :: titleX !Plot titles for extra dimensions
    CHARACTER(64),      DIMENSION(:), POINTER :: xLabX  !x-axis labels for extra dimensions
    TYPE( AdjAxisVal ), DIMENSION(:), POINTER :: axisX  !x-axis valuse for extra dimensions
  END TYPE AdjointMaxLocTime_str

  TYPE SAGgrid_ptr
    TYPE( SAGgrid_str ), POINTER :: grd
  END TYPE SAGgrid_ptr

  TYPE ( SAGgrid_ptr ), DIMENSION(:), ALLOCATABLE :: AdjGrd

!---- Plot field data

  LOGICAL                                     :: Probabilistic
  INTEGER                                     :: nScale
  INTEGER                                     :: adjChoice, adjClass
  INTEGER                                     :: nTrigger, nNull, nSat, nHit
  INTEGER, DIMENSION(:),   ALLOCATABLE        :: adjI
  REAL,    DIMENSION(:), POINTER              :: wt
  REAL,    DIMENSION(:), POINTER              :: gamT
  REAL                                        :: tm, xpt, ypt
  REAL                                        :: special
  TYPE( Adjoint_str ), DIMENSION(:), POINTER  :: Amat
  TYPE( Adjoint_ptr ), DIMENSION(:), POINTER  :: Nmat, Tmat, Smat


!---- Adjoint material/release data

  TYPE( AdjointMat_str ), DIMENSION(:), ALLOCATABLE  :: AdjMat

  REAL tFirstTrigger

  REAL, DIMENSION(:,:), ALLOCATABLE :: AdjDistS, AdjHitDistS
  REAL, DIMENSION(:,:), ALLOCATABLE :: AdjDistT, AdjHitDistT
  REAL, DIMENSION(:,:), ALLOCATABLE :: AdjDistV

!---- Associated LocMax data

  INTEGER nLocMax     !Number of active time plot data sets
  INTEGER nActive     !Number of active cells

  TYPE( AdjointMaxLocTime_str ), DIMENSION(:), ALLOCATABLE, TARGET :: AdjLocMax

  REAL MaxAdjVal         !Max adjoint conc value for mask area
  REAL xminMask, yminMask, xmaxMask, ymaxMask

  INTEGER nTime, nDim, mlev, iMaxLoc
  INTEGER, DIMENSION(1) :: DimX

!---- Local pointers

  TYPE ( SAGgrid_str ), POINTER  :: grd
  TYPE ( SAGgrid_str ), POINTER  :: srf

  TYPE( SCIPTimeT ), DIMENSION(:), POINTER :: adjTime

  LOGICAL                        :: FastSearch
  LOGICAL                        :: FillGrid

  TYPE AdjMaxSearchStr
    INTEGER                      :: TimeID
    INTEGER                      :: DurID
    REAL                         :: SrcFunc
    REAL                         :: Mass
    REAL                         :: xMax
    REAL                         :: yMax
    REAL, DIMENSION(2)           :: xMask
    REAL, DIMENSION(2)           :: yMask
  END TYPE

  TYPE( AdjMaxSearchStr )        :: AdjMaxSearch
  REAL, DIMENSION(4)             :: Px, Py, Pv
  REAL                           :: xcMask, ycMask, xdMask, ydMask

!---- Simplex search data

  INTEGER nSearchVar, iter
  REAL    rad, pMass

  REAL, DIMENSION(:),   ALLOCATABLE :: pCost
  REAL, DIMENSION(:,:), ALLOCATABLE :: pSimplex

END MODULE adjoint_fi

