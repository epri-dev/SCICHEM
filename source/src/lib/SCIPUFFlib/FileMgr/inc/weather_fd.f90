MODULE weather_fd

  USE version_fd    !Basic version parameters
  USE terrain_fd    !Basic terrain structures
  USE param_fd
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL

!==== SCIP Weather types ======================================================

  INTEGER, PARAMETER :: HWB_METFIX   = 0
  INTEGER, PARAMETER :: HWB_METSRF   = 1
  INTEGER, PARAMETER :: HWB_METPRF   = 2
  INTEGER, PARAMETER :: HWB_METMRF   = 3
  INTEGER, PARAMETER :: HWB_METMED   = 4
  INTEGER, PARAMETER :: HWB_METASSIM = 12
  INTEGER, PARAMETER :: HWB_METWRF   = 13
  INTEGER, PARAMETER :: HWB_METMEDLS = 15
  INTEGER, PARAMETER :: HWB_METOPER  = 7
  INTEGER, PARAMETER :: HWB_METFCST  = 9
  INTEGER, PARAMETER :: HWB_METCURR  = 10
  INTEGER, PARAMETER :: HW_METNONE   = 0
  INTEGER, PARAMETER :: HW_METFIX    = 2**HWB_METFIX
  INTEGER, PARAMETER :: HW_METSRF    = 2**HWB_METSRF
  INTEGER, PARAMETER :: HW_METPRF    = 2**HWB_METPRF
  INTEGER, PARAMETER :: HW_METMRF    = 2**HWB_METMRF
  INTEGER, PARAMETER :: HW_METMED    = 2**HWB_METMED
  INTEGER, PARAMETER :: HW_METWRF    = 2**HWB_METWRF
  INTEGER, PARAMETER :: HW_METASSIM  = 2**HWB_METASSIM
  INTEGER, PARAMETER :: HW_METMEDLS  = 2**HWB_METMEDLS
  INTEGER, PARAMETER :: HW_METOPER   = 2**HWB_METOPER
  INTEGER, PARAMETER :: HW_METFCST   = 2**HWB_METFCST
  INTEGER, PARAMETER :: HW_METCURR   = 2**HWB_METCURR

!==== SCIP Precipitation types ================================================

  INTEGER, PARAMETER :: HPB_PRCPMET  = 0
  INTEGER, PARAMETER :: HPB_RAIN     = 1
  INTEGER, PARAMETER :: HPB_SNOW     = 2
  INTEGER, PARAMETER :: HPB_LIGHT    = 3
  INTEGER, PARAMETER :: HPB_MODERATE = 4
  INTEGER, PARAMETER :: HP_NONE      = 0
  INTEGER, PARAMETER :: HP_PRCPINP   = HP_NONE
  INTEGER, PARAMETER :: HP_PRCPMET   = 2**HPB_PRCPMET
  INTEGER, PARAMETER :: HP_RAIN      = 2**HPB_RAIN
  INTEGER, PARAMETER :: HP_SNOW      = 2**HPB_SNOW
  INTEGER, PARAMETER :: HP_LIGHT     = 2**HPB_LIGHT
  INTEGER, PARAMETER :: HP_MODERATE  = 2**HPB_MODERATE
  INTEGER, PARAMETER :: HP_HEAVY     = HP_MODERATE + HP_LIGHT

!==== SCIP Boundary Layer types ===============================================

  INTEGER, PARAMETER :: HBB_BLSIMPLE = 0
  INTEGER, PARAMETER :: HBB_BLCALC   = 1
  INTEGER, PARAMETER :: HBB_BLPRF    = 2
  INTEGER, PARAMETER :: HBB_BLOBS    = 3
  INTEGER, PARAMETER :: HBB_BLMED    = 4
  INTEGER, PARAMETER :: HBB_BLOPER   = 7
  INTEGER, PARAMETER :: HB_NONE     = 0
  INTEGER, PARAMETER :: HB_BLSIMPLE = 2**HBB_BLSIMPLE
  INTEGER, PARAMETER :: HB_BLCALC   = 2**HBB_BLCALC
  INTEGER, PARAMETER :: HB_BLPRF    = 2**HBB_BLPRF
  INTEGER, PARAMETER :: HB_BLOBS    = 2**HBB_BLOBS
  INTEGER, PARAMETER :: HB_BLMED    = 2**HBB_BLMED
  INTEGER, PARAMETER :: HB_BLOPER   = 2**HBB_BLOPER

!==== SCIP Surface Moisture types ===============================================

  INTEGER, PARAMETER :: HM_MSTDRY  = 1
  INTEGER, PARAMETER :: HM_MSTNORM = 2
  INTEGER, PARAMETER :: HM_MSTWET  = 3

!==== SCIP LSV types ==========================================================

  INTEGER, PARAMETER :: HLB_LSVINP  = 0
  INTEGER, PARAMETER :: HLB_LSVMOD  = 1
  INTEGER, PARAMETER :: HLB_LSVOBS  = 3
  INTEGER, PARAMETER :: HLB_LSVOPER = 7
  INTEGER, PARAMETER :: HLB_LSVOPER_20 = HLB_LSVOPER +  HVB_VERSION20
  INTEGER, PARAMETER :: HL_NONE     = 0
  INTEGER, PARAMETER :: HL_LSVINP   = 2**HLB_LSVINP
  INTEGER, PARAMETER :: HL_LSVMOD   = 2**HLB_LSVMOD
  INTEGER, PARAMETER :: HL_LSVOBS   = 2**HLB_LSVOBS
  INTEGER, PARAMETER :: HL_LSVOPER  = 2**HLB_LSVOPER
  INTEGER, PARAMETER :: HL_LSVOPER_20  = 2**HLB_LSVOPER_20

!==== SCIP Units ==============================================================

  INTEGER, PARAMETER :: HU_DEG  = 1
  INTEGER, PARAMETER :: HU_DMS  = 2
  INTEGER, PARAMETER :: HU_MPS  = 1
  INTEGER, PARAMETER :: HU_KTS  = 2
  INTEGER, PARAMETER :: HU_MPH  = 3
  INTEGER, PARAMETER :: HU_KPH  = 4
  INTEGER, PARAMETER :: HU_FPS  = 5

!==== metFlagsT ===============================================================

  TYPE metFlagsT
    SEQUENCE
    INTEGER  reference
    INTEGER  doMC
    INTEGER  doOutput
    INTEGER  notUsed
    REAL     tOutput
    REAL     slHazard
  END TYPE metFlagsT

  INTEGER, PARAMETER :: SIZE_metFlagsT = 4*KIND(1) + 2*KIND(1.)

!==== metMetT =================================================================

  INTEGER, PARAMETER :: HS_MAXMETINP = 20

  TYPE metMetT
    SEQUENCE
    INTEGER        type
    INTEGER        nnPrf
    INTEGER        nnSfc
    REAL           timeBin
    INTEGER        nStations
    INTEGER        monthDay
    INTEGER        unitSpd
    INTEGER        unitDir
    REAL           speed
    REAL           direction
    INTEGER        notUsed
    LOGICAL        Use_TKE
    CHARACTER(PATH_MAXLENGTH), DIMENSION(HS_MAXMETINP) :: input
  END TYPE metMetT

  INTEGER, PARAMETER :: SIZE_metMetT = 8*KIND(1) + 3*KIND(1.) + KIND(.TRUE.) + HS_MAXMETINP*PATH_MAXLENGTH

!==== metBLT ==================================================================

  TYPE metBLT
    SEQUENCE
    INTEGER type
    INTEGER wetness
    REAL    ziDay
    REAL    ziNight
    REAL    hfluxDay
    REAL    hfluxNight
    REAL    canopy
    REAL    roughness
    REAL    albedo
    REAL    bowen
    REAL    cloud
    REAL    canopyParam
    CHARACTER(64) landuse
  END TYPE metBLT

  INTEGER, PARAMETER :: SIZE_metBLT = 2*KIND(1) + 10*KIND(1.) + 64

!==== metLSVT =================================================================

  TYPE metLSVT
    SEQUENCE
    INTEGER type
    REAL    uu
    REAL    sl
  END TYPE metLSVT

  INTEGER, PARAMETER :: SIZE_metLSVT = KIND(1) + 2*KIND(1.)

!==== metPrecipT ==============================================================

  TYPE metPrecipT
    SEQUENCE
    INTEGER type
    INTEGER class
  END TYPE metPrecipT

  INTEGER, PARAMETER :: SIZE_metPrecipT = 2*KIND(1)

!==== weatherT ================================================================

  TYPE weatherT
    SEQUENCE
    TYPE( metFlagsT   ) flags
    TYPE( metMetT     ) met
    TYPE( metBLT      ) bl
    TYPE( metLSVT     ) lsv
    TYPE( metPrecipT  ) precip
    TYPE( terrainT    ) terrain
  END TYPE weatherT

  INTEGER, PARAMETER :: SIZE_weatherT = SIZE_metFlagsT + SIZE_metMetT + SIZE_metBLT + &
                                        SIZE_metLSVT + SIZE_metPrecipT + SIZE_terrainT

END MODULE weather_fd
