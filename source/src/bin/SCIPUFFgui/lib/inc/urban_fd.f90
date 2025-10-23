MODULE urban_fd

  USE DefSize_fd

  INTEGER, PARAMETER :: UDM_OFF = 0
  INTEGER, PARAMETER :: UDM_ON  = 1

  INTEGER, PARAMETER :: UDM_LOW = 0
  INTEGER, PARAMETER :: UDM_MED = 1
  INTEGER, PARAMETER :: UDM_HI  = 2

  INTEGER, PARAMETER :: UDM_BULKONLY   = 1
  INTEGER, PARAMETER :: UDM_INDIVIDUAL = 2

  INTEGER, PARAMETER :: MSS_OFF = 0
  INTEGER, PARAMETER :: MSS_ON  = 1

  INTEGER, PARAMETER :: MSS_LOW = 0
  INTEGER, PARAMETER :: MSS_MED = 1
  INTEGER, PARAMETER :: MSS_HI  = 2

  INTEGER, PARAMETER :: MSS_SMALL  = 0
  INTEGER, PARAMETER :: MSS_MEDIUM = 1
  INTEGER, PARAMETER :: MSS_LARGE  = 2

  INTEGER, PARAMETER :: MSS_AVGHGT = 0
  INTEGER, PARAMETER :: MSS_USER   = 1

  INTEGER, PARAMETER :: UWM_OFF = 2
  INTEGER, PARAMETER :: UWM_ON  = 1

  INTEGER, PARAMETER :: UWM_COARSE = 1
  INTEGER, PARAMETER :: UWM_MEDIUM = 2
  INTEGER, PARAMETER :: UWM_FINE   = 3

  INTEGER, PARAMETER :: UWM_AUTO = 1
  INTEGER, PARAMETER :: UWM_USER = 2

  INTEGER, PARAMETER :: UWM_UTM = 1
  INTEGER, PARAMETER :: UWM_LLA = 2

  TYPE UDMSettings
    SEQUENCE
    LOGICAL               :: On
    INTEGER               :: ChannnelMode
    INTEGER, DIMENSION(3) :: I_reserve
    REAL(4)               :: MaxSrcHgt
    REAL(4), DIMENSION(5) :: F_reserve
    INTEGER               :: BuildingInter
    INTEGER               :: PuffSplit
    INTEGER               :: PuffMerge
    INTEGER               :: J_reserve
  END TYPE  UDMSettings

  TYPE MSSSettings
    SEQUENCE
    LOGICAL    :: On
    INTEGER    :: HGrid
    INTEGER    :: HDomain
    INTEGER    :: VGrid
    INTEGER    :: VCluster
    REAL(4)    :: ResolHgt
    INTEGER    :: PTimeStep
    INTEGER    :: PSyncFac
    INTEGER    :: NumPart
    INTEGER    :: ConcTime
  END TYPE MSSSettings

  TYPE UWMSettings
    SEQUENCE
    LOGICAL    :: On
    INTEGER    :: Grid
    INTEGER    :: Domain
    REAL(4)    :: dsmin  !Auto grid
    REAL(4)    :: dmax   !Auto grid
    REAL(4)    :: dmin   !Auto grid
    INTEGER    :: coor   !User grid
    INTEGER    :: zone   !User grid
    REAL(4)    :: xBL    !User grid
    REAL(4)    :: yBL    !User grid
    REAL(4)    :: xTR    !User grid
    REAL(4)    :: yTR    !User grid
  END TYPE UWMSettings

  TYPE UrbanSettings
    SEQUENCE
    TYPE( UDMSettings )       :: udm
    TYPE( MSSSettings )       :: mss
    TYPE( UWMSettings )       :: uwm
    CHARACTER(PATH_MAXLENGTH) :: DBName
    CHARACTER(PATH_MAXLENGTH) :: DBPath
    CHARACTER(PATH_MAXLENGTH) :: TerPath
    CHARACTER(PATH_MAXLENGTH) :: LUPath
    CHARACTER(PATH_MAXLENGTH) :: PopPath
  END TYPE UrbanSettings

END MODULE urban_fd
