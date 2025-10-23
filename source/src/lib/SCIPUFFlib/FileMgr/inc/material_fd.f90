MODULE material_fd

!==== SCIP Material types =====================================================

  INTEGER,PARAMETER :: HMB_GAS          = 0
  INTEGER,PARAMETER :: HMB_PARTICLE     = 1
  INTEGER,PARAMETER :: HMB_LIQUID       = 2
  INTEGER,PARAMETER :: HMB_AEROSOL      = 3
  INTEGER,PARAMETER :: HMB_WETPARTICLE  = 4
  INTEGER,PARAMETER :: HMB_2NDEVAP      = 20
  INTEGER,PARAMETER :: HMB_MULTICOMP    = 24
  INTEGER,PARAMETER :: HMB_NULLSENSOR   = 19
  INTEGER,PARAMETER :: HMB_SATSENSOR    = 18

  INTEGER,PARAMETER :: HMB_BASIC        = 10

  INTEGER,PARAMETER :: HM_GAS         = 2**HMB_GAS
  INTEGER,PARAMETER :: HM_PARTICLE    = 2**HMB_PARTICLE
  INTEGER,PARAMETER :: HM_LIQUID      = 2**HMB_LIQUID
  INTEGER,PARAMETER :: HM_AEROSOL     = 2**HMB_AEROSOL
  INTEGER,PARAMETER :: HM_WETPARTICLE = 2**HMB_WETPARTICLE
  INTEGER,PARAMETER :: HM_2NDEVAP     = 2**HMB_2NDEVAP
  INTEGER,PARAMETER :: HM_MULTICOMP   = 2**HMB_MULTICOMP
  INTEGER,PARAMETER :: HM_NULLSENSOR  = 2**HMB_NULLSENSOR
  INTEGER,PARAMETER :: HM_SATSENSOR   = 2**HMB_SATSENSOR

!==== SCIP Surface file modes =================================================

  INTEGER,PARAMETER :: HSB_GROUPDEP  = 0
  INTEGER,PARAMETER :: HSB_GROUPDOS  = 1
  INTEGER,PARAMETER :: HSB_TOTALDEP  = 2
  INTEGER,PARAMETER :: HSB_TOTALDOS  = 3
  INTEGER,PARAMETER :: HS_NONE       = 0
  INTEGER,PARAMETER :: HS_GROUPDEP   = 2**HSB_GROUPDEP
  INTEGER,PARAMETER :: HS_GROUPDOS   = 2**HSB_GROUPDOS
  INTEGER,PARAMETER :: HS_TOTALDEP   = 2**HSB_TOTALDEP
  INTEGER,PARAMETER :: HS_TOTALDOS   = 2**HSB_TOTALDOS

!==== parameters ==============================================================

  INTEGER,PARAMETER :: HS_MAXMTLBINSIZE  = 50
  INTEGER,PARAMETER :: HS_MAXMTLBASIC    = 19
  INTEGER,PARAMETER :: HS_MAXMTLGAS      =  7
  INTEGER,PARAMETER :: HS_MAXMTLAEROSOL  = 13
  INTEGER,PARAMETER :: HS_MAXMTLLIQUID   = 19
  INTEGER,PARAMETER :: HS_MAXMTLPARTICLE =  7
  INTEGER,PARAMETER :: HS_PADMTLGEN       = HS_MAXMTLBASIC &
                                          + 2*HS_MAXMTLBINSIZE + 1
  INTEGER,PARAMETER :: HS_PADMTLGAS       = HS_MAXMTLBASIC - HS_MAXMTLGAS &
                                          + 2*HS_MAXMTLBINSIZE + 1
  INTEGER,PARAMETER :: HS_PADMTLAEROSOL   = HS_MAXMTLBASIC - HS_MAXMTLAEROSOL &
                                          + 2*HS_MAXMTLBINSIZE + 1
  INTEGER,PARAMETER :: HS_PADMTLPARTICLE  = HS_MAXMTLBASIC - HS_MAXMTLPARTICLE

!==== matGenT =================================================================

  TYPE  matGenT
    SEQUENCE
    INTEGER padding(HS_PADMTLGEN)
  END TYPE  matGenT

  INTEGER, PARAMETER :: SIZE_matGenT = HS_PADMTLGEN*KIND(1)

!==== matGasT =================================================================

  TYPE  matGasT
    SEQUENCE
    REAL    minConcentration
    REAL    decayAmp
    REAL    decayMin
    REAL    rNotUsed
    INTEGER save
    REAL    gasDensity
    REAL    gasDeposition
    INTEGER padding(HS_PADMTLGAS)
  END TYPE  matGasT

!==== matAerosolT =============================================================

  TYPE  matAerosolT
    SEQUENCE
    REAL    minConcentration
    REAL    decayAmp
    REAL    decayMin
    REAL    rNotUsed
    INTEGER save
    REAL    gasDeposition
    REAL    liquidDensity
    REAL    antoine(3)
    REAL    molWeight
    REAL    liqSpecificHeat
    REAL    gasSpecificHeat
    INTEGER padding(HS_PADMTLAEROSOL)
  END TYPE  matAerosolT

!==== matLiquidT ==============================================================

  TYPE  matLiquidT
    SEQUENCE
    REAL    minConcentration
    REAL    decayAmp
    REAL    decayMin
    REAL    rNotUsed
    INTEGER save
    REAL    gasDensity
    REAL    gasDeposition
    REAL    liquidDensity(2)
    REAL    antoine(3)
    REAL    molWeight
    REAL    liqSpecificHeat
    REAL    gasSpecificHeat
    REAL    srfTension
    REAL    spreadFactor
    REAL    viscosity
    INTEGER nSizeBins
    REAL    binSize(HS_MAXMTLBINSIZE)
    REAL    binBounds(HS_MAXMTLBINSIZE+1)
  END TYPE  matLiquidT

!==== matParticleT ============================================================

  TYPE  matParticleT
    SEQUENCE
    REAL    minConcentration
    REAL    decayAmp
    REAL    decayMin
    REAL    rNotUsed
    INTEGER save
    REAL    density
    INTEGER nSizeBins
    REAL    binSize(HS_MAXMTLBINSIZE)
    REAL    binBounds(HS_MAXMTLBINSIZE+1)
    INTEGER padding(HS_PADMTLPARTICLE)
  END TYPE  matParticleT

!==== materialT ===============================================================

  TYPE  materialT
    SEQUENCE
    INTEGER            type        !class->matl.ccls
    INTEGER            puffIndex   !ioffp
    INTEGER, DIMENSION(2) :: iNotUsed
    TYPE ( matGenT )   matData     !Class specific parameters
    CHARACTER(16 )     name        !mname->matl.cmat
    CHARACTER(16 )     units       !units->matl.unit
    CHARACTER(64 )     file        !file_name->matl.file
    CHARACTER(128)     path        !path_name->matl.path
  END TYPE  materialT

  INTEGER, PARAMETER :: SIZE_materialT = 4*KIND(1) + SIZE_matGenT + 2*16 + 64 + 128

END MODULE material_fd
