MODULE release_fd

  USE DefSize_fd

!==== SCIP Release types ======================================================

!--- basic release type bits

  INTEGER, PARAMETER :: HRB_INST  = 0
  INTEGER, PARAMETER :: HRB_CONT  = 1

!--- composite release type bits

  INTEGER, PARAMETER :: HRB_RAN3  = 19
  INTEGER, PARAMETER :: HRB_FILE  = 20
  INTEGER, PARAMETER :: HRB_MOVE  = 21
  INTEGER, PARAMETER :: HRB_POOL  = 22
  INTEGER, PARAMETER :: HRB_STACK = 23
  INTEGER, PARAMETER :: HRB_PUFF  = 24
  INTEGER, PARAMETER :: HRB_FLASH = 25
  INTEGER, PARAMETER :: HRB_PRIME = 26
  INTEGER, PARAMETER :: HRB_OFFDIAG = 27
  INTEGER, PARAMETER :: HRB_STACK3  = 30

!---- basic release types

  INTEGER, PARAMETER :: HR_INST   = 2**HRB_INST
  INTEGER, PARAMETER :: HR_CONT   = 2**HRB_CONT

!--- composite release types

  INTEGER, PARAMETER :: HR_FILE   = 2**HRB_FILE  + HR_INST
  INTEGER, PARAMETER :: HR_MOVE   = 2**HRB_MOVE  + HR_CONT
  INTEGER, PARAMETER :: HR_POOL   = 2**HRB_POOL  + HR_CONT
  INTEGER, PARAMETER :: HR_PUFF   = 2**HRB_PUFF  + HR_INST
  INTEGER, PARAMETER :: HR_STACK  = 2**HRB_STACK + HR_CONT
  INTEGER, PARAMETER :: HR_AREA   = 2**HRB_OFFDIAG + HR_STACK
  INTEGER, PARAMETER :: HR_PRIME  = 2**HRB_PRIME + HR_STACK
  INTEGER, PARAMETER :: HR_FINST  = 2**HRB_FLASH + HR_INST
  INTEGER, PARAMETER :: HR_XINST  = 2**HRB_OFFDIAG + HR_INST
  INTEGER, PARAMETER :: HR_XINST3 = 2**HRB_OFFDIAG + 2**HRB_RAN3 + HR_INST
  INTEGER, PARAMETER :: HR_STACK3 = 2**HRB_STACK3 + HR_STACK
  INTEGER, PARAMETER :: HR_CONTF   = HR_CONT   + 2**HRB_FILE
  INTEGER, PARAMETER :: HR_STACKF  = HR_STACK  + 2**HRB_FILE
  INTEGER, PARAMETER :: HR_STACK3F = HR_STACK3 + 2**HRB_FILE
  INTEGER, PARAMETER :: HR_STACKFP = HR_STACKF + 2**HRB_PRIME
  INTEGER, PARAMETER :: HR_AREAF   = HR_AREA   + 2**HRB_FILE

!==== SCIP Release distribution types =========================================

  INTEGER, PARAMETER :: HDB_LOGNORM = 0
  INTEGER, PARAMETER :: HD_LOGNORM  = -2**HDB_LOGNORM

!==== parameters ==============================================================

  INTEGER, PARAMETER :: HS_MAXRELRESERVED =  1

  INTEGER, PARAMETER :: HS_MAXRELCONTSIZE  = 12
  INTEGER, PARAMETER :: HS_MAXRELCMOVSIZE  = 15
  INTEGER, PARAMETER :: HS_MAXRELCSTK3SIZE = 13
  INTEGER, PARAMETER :: HS_MAXRELCSTKSIZE  = 11
  INTEGER, PARAMETER :: HS_MAXRELFILESIZE = 3 + PATH_MAXLENGTH/4
  INTEGER, PARAMETER :: HS_MAXRELINSTSIZE = 14
  INTEGER, PARAMETER :: HS_MAXRELPOOLSIZE =  4
  INTEGER, PARAMETER :: HS_MAXRELIPUFSIZE  = 26
  INTEGER, PARAMETER :: HS_MAXRELXINSTSIZE  = 17
  INTEGER, PARAMETER :: HS_MAXRELXINST3SIZE = 20

  INTEGER, PARAMETER :: HS_MAXRELMAXSIZE  = MAX(26,HS_MAXRELFILESIZE)

  INTEGER, PARAMETER :: HS_PADRELGEN      = HS_MAXRELMAXSIZE + HS_MAXRELRESERVED
  INTEGER, PARAMETER :: HS_PADRELCONT     = HS_PADRELGEN - HS_MAXRELCONTSIZE
  INTEGER, PARAMETER :: HS_PADRELFILE     = HS_PADRELGEN - HS_MAXRELFILESIZE
  INTEGER, PARAMETER :: HS_PADRELINST     = HS_PADRELGEN - HS_MAXRELINSTSIZE
  INTEGER, PARAMETER :: HS_PADRELCMOV     = HS_PADRELGEN - HS_MAXRELCMOVSIZE
  INTEGER, PARAMETER :: HS_PADRELPOOL     = HS_PADRELGEN - HS_MAXRELPOOLSIZE
  INTEGER, PARAMETER :: HS_PADRELIPUF     = HS_PADRELGEN - HS_MAXRELIPUFSIZE
  INTEGER, PARAMETER :: HS_PADRELCSTK     = HS_PADRELGEN - HS_MAXRELCSTKSIZE
  INTEGER, PARAMETER :: HS_PADRELCSTK3    = HS_PADRELGEN - HS_MAXRELCSTK3SIZE
  INTEGER, PARAMETER :: HS_PADRELXINST    = HS_PADRELGEN - HS_MAXRELXINSTSIZE
  INTEGER, PARAMETER :: HS_PADRELXINST3   = HS_PADRELGEN - HS_MAXRELXINST3SIZE

!==== relGenT =================================================================

  TYPE  relGenT
    SEQUENCE
    INTEGER padding(HS_PADRELGEN)
  END TYPE  relGenT

  INTEGER, PARAMETER :: SIZE_relGenT = HS_PADRELGEN*KIND(1)

!==== relContT ================================================================

  TYPE  relContT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    INTEGER padding(HS_PADRELCONT)
  END TYPE  relContT

!==== relFileT ================================================================

  TYPE  relFileT
    SEQUENCE
    INTEGER                   nRandom   !rel_param(REL_RAND_INDX)=number_random
    INTEGER                   ranSeed   !rel_param(REL_SEED_INDX)=random_seed
    REAL                      ranSpread !rel_param(REL_SPREAD_INDX)=random_spread
    CHARACTER(PATH_MAXLENGTH) relFile
    INTEGER padding(HS_PADRELFILE)
  END TYPE  relFileT

!==== relContFileT ================================================================

  TYPE  relContFileT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    CHARACTER(PATH_MAXLENGTH-32) relFile     !Keep release same size as relFileT
    INTEGER padding(HS_PADRELFILE)
  END TYPE  relContFileT

!==== relInstT ================================================================

  TYPE  relInstT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    mass         !cmass
    REAL    sigX         !sigx
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    INTEGER nRandom      !rel_param(REL_RAND_INDX)=number_random
    INTEGER ranSeed      !rel_param(REL_SEED_INDX)=random_seed
    REAL    ranSpread    !rel_param(REL_SPREAD_INDX)=random_spread
    REAL    activeFrac   !active Fraction
    INTEGER padding(HS_PADRELINST)
  END TYPE  relInstT

!==== relXInstT ================================================================

  TYPE  relXInstT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    mass         !cmass
    REAL    sigX         !sigx
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    sigRxy       !x-y correlation coefficient
    REAL    sigRxz       !x-z correlation coefficient
    REAL    sigRyz       !y-z correlation coefficient
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    INTEGER nRandom      !rel_param(REL_RAND_INDX)=number_random
    INTEGER ranSeed      !rel_param(REL_SEED_INDX)=random_seed
    REAL    ranSpread    !rel_param(REL_SPREAD_INDX)=random_spread
    REAL    activeFrac   !active Fraction
    INTEGER padding(HS_PADRELXINST)
  END TYPE  relXInstT

!==== relXInst3T ================================================================

  TYPE  relXInst3T
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    mass         !cmass
    REAL    sigX         !sigx
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    sigRxy       !x-y correlation coefficient
    REAL    sigRxz       !x-z correlation coefficient
    REAL    sigRyz       !y-z correlation coefficient
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    INTEGER nRandom      !rel_param(REL_RAND_INDX)=number_random
    INTEGER ranSeed      !rel_param(REL_SEED_INDX)=random_seed
    REAL    ranSpreadA   !rel_param(REL_SPREAD_INDX)=random_spread along direction
    REAL    ranSpreadT   !rel_param(REL_SPREADT_INDX)=random_spread transverse direction
    REAL    ranSpreadV   !rel_param(REL_SPREADV_INDX)=random_spread vertical direction
    REAL    ranDir       !rel_param(REL_RANDIR_INDX)=random_spread direction heading
    REAL    activeFrac   !active Fraction
    INTEGER padding(HS_PADRELXINST3)
  END TYPE  relXInst3T

!==== relMoveT ================================================================

  TYPE  relMoveT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    sigY         !sigy
    REAL    sigZ         !sigz
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    momentum     !wmom
    REAL    buoyancy     !buoy
    REAL    dryFrac      !mass Fraction : Liquid, Wet Particle release only
    REAL    velX         !urel
    REAL    velY         !vrel
    REAL    velZ         !wrel
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    INTEGER padding(HS_PADRELCMOV)
  END TYPE  relMoveT

!==== relPoolT ================================================================

  TYPE  relPoolT
    SEQUENCE
    REAL    mass  !cmass
    REAL    sizeX !sigx
    REAL    sizeY !sigy
    REAL    activeFrac   !active Fraction
    INTEGER padding(HS_PADRELPOOL)
  END TYPE  relPoolT
!==== relPuffT ===============================================================

  TYPE  relPuffT
    SEQUENCE
    INTEGER subgroup     !subgroup
    REAL    mass         !cmass (c)
    REAL    sxx          !Puff moments
    REAL    sxy          !
    REAL    sxz          !
    REAL    syy          !
    REAL    syz          !
    REAL    szz          !
    REAL    difhShear    !Small scale diffusivity   (yvsc/c)
    REAL    difhBuoy     !BL scale diffusivity      (yvbc/c)
    REAL    difhLSVxx    !Large scale diffusivities (xuc/c)
    REAL    difhLSVxy    !                          (xvc/c)
    REAL    difhLSVyy    !                          (yvc/c)
    REAL    difVert      !Vertical diffusivity      (zwc/c)
    REAL    activeFrac   !Active fraction           (cfo)
    REAL    sigRatio     !SigmaC / Cbar
    REAL    scaleLateral !Lateral scale             (si)
    REAL    scaleStream  !Streamwise scale          (si2)
    REAL    scaleVert    !Vertical scale            (sv)
    REAL    dropDiam     !Droplet diameter
    REAL    dropSigD     !Droplet diameter spread
    REAL    dropTemp     !Droplet temperature
    REAL    tAge         !Puff age since release for RAD materials (s)
    REAL    wDynamic     !Dynamic vertical velocity (m/s)
    REAL    tDynamic     !Dynamic temperature excess (C)
    REAL    liquidFraction     !Aerosol liquid mass fraction
    INTEGER padding(HS_PADRELIPUF)
  END TYPE  relPuffT

!==== relStackT ===============================================================

  TYPE  relStackT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    diameter     !size
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel      !wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    INTEGER padding(HS_PADRELCSTK)
  END TYPE  relStackT

!==== relStack3T ==============================================================

  TYPE  relStack3T
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    diameter     !size
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel(3)   !umom,vmom,wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    INTEGER padding(HS_PADRELCSTK3)
  END TYPE  relStack3T

!==== relStackFileT ===============================================================

  TYPE  relStackFileT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    diameter     !size
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel      !wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    CHARACTER(PATH_MAXLENGTH-28) relFile     !Keep release same size as relFileT
    INTEGER padding(HS_PADRELFILE)
  END TYPE  relStackFileT

!==== relStack3FileT ==============================================================

  TYPE  relStack3FileT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    diameter     !size
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel(3)   !umom,vmom,wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    CHARACTER(PATH_MAXLENGTH-36) relFile     !Keep release same size as relFileT
    INTEGER padding(HS_PADRELFILE)
  END TYPE  relStack3FileT

!==== relAreaT =================================================================

  TYPE  relAreaT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    xlen         !length in nominal x-direction
    REAL    ylen         !length "     "    y   "
    REAL    ang          !orientation (deg. CCW from x-axis)
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel      !wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    REAL    nextUpdtTime !update time
    INTEGER padding(HS_PADRELCSTK-12)
  END TYPE  relAreaT

!==== relAreaFileT ===============================================================

  TYPE  relAreaFileT
    SEQUENCE
    INTEGER distribution !subgroup
    REAL    rate         !cmass
    REAL    duration     !tdur/3600.
    REAL    xlen         !length in nominal x-direction
    REAL    ylen         !length "     "    y   "
    REAL    ang          !orientation (deg. CCW from x-axis)
    REAL    MMD          !rel_param(REL_MMD_INDX)=lognorm_mmd
    REAL    sigma        !rel_param(REL_SIGMA_INDX)=lognorm_sigma
    REAL    exitVel      !wmom
    REAL    exitTemp     !buoy
    REAL    dryFrac      !mass Fraction : Liquid release only
    REAL    activeFrac   !active Fraction
    CHARACTER(PATH_MAXLENGTH-48) relFile     !Keep release same size as relFileT
    INTEGER padding(HS_PADRELFILE)
  END TYPE  relAreaFileT

!==== releaseT ================================================================

  TYPE  releaseT
    SEQUENCE
    INTEGER         padding     !For alignment
    INTEGER         type        !reltyp
    INTEGER         status      !SCIP 4.0 - Needs update flag
    REAL            tRel        !trel/3600.
    REAL(8)         xRel        !xrel
    REAL(8)         yRel        !yrel
    REAL            zRel        !zrel
    REAL            notUsedA    !Space holder
    REAL            notUsedB    !Space holder
    REAL            notUsed     !Space holder
    TYPE( relGenT ) relData     !Type dependent data
    CHARACTER(16)   material    !relmat
    CHARACTER(32)   relName     !Release Identifier
    CHARACTER(192)  relDisplay  !Release display string
  END TYPE  releaseT

  INTEGER, PARAMETER :: SIZE_releaseT = 3*KIND(1) + 2*KIND(1.D0) + 5*KIND(1.) + SIZE_relGenT + 16 + 32 + 192

!==== releaseMCT ================================================================

  TYPE  releaseMCT
    SEQUENCE
    INTEGER         relID        !release number
    CHARACTER(16)   MCname       !multicomponent name
    REAL            MCmass       !component release mass
  END TYPE  releaseMCT

!------ Multi-component data structures

TYPE MCrelData
  SEQUENCE
  CHARACTER(16)              :: MCname
  REAL                       :: MCmass
  TYPE( MCrelData ), POINTER :: next
END TYPE MCrelData

TYPE MCrelList
  SEQUENCE
  INTEGER nList
  TYPE( MCrelData ), POINTER :: firstMCRel
END TYPE MCrelList

!==== releaseSpecT ================================================================

  TYPE  releaseSpecT
    SEQUENCE
    TYPE( ReleaseT ) release
    TYPE( MCrelList ) MClist
    INTEGER ityp
    INTEGER distrib
  END TYPE releaseSpecT
!==== StoreReleaseT ================================================================

  TYPE  StoreReleaseT
    SEQUENCE
    TYPE( releaseSpecT ) relSpec
    TYPE( StoreReleaseT ), POINTER :: NextRelease
  END TYPE  StoreReleaseT

END MODULE release_fd
