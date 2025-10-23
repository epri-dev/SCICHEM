MODULE sampler_fi

  USE param_fd
  USE sampler_fd

  SAVE

  INTEGER, PARAMETER :: MAXSMP = 5000
  INTEGER, PARAMETER :: SamplerFileVersion = 0002
  INTEGER nvarAlloc, nsmp0
  INTEGER nmc0
  INTEGER nsmp, isg, itys, itye, nvarsmp, ndepblk_smp, ndosblk_smp
  INTEGER nsmpT, nbinOut
  INTEGER sampMap, sampZone
  INTEGER istepP, mstepP
  REAL    dtsP
  REAL    t_smp, zi_smp, tolSmpOut
  REAL    dtSmpOut, tSmpOut, tStartSamp, tNextSamp
  REAL    tEndSamp
  REAL    UTMfac
  REAL    cartLat0, cartLon0, cartX0, cartY0
  LOGICAL lWrap, lSmpOut, lSmpOutList, lGridOut, lCartOrig
  LOGICAL lReadSum, lUpdateSum, lSmpCnv, lOutToSmp
  LOGICAL lAvg
  LOGICAL lBinOut, lDosSmp, lLOSSmp, lDepS

  CHARACTER(256) SampTitle

  TYPE( SampTime ), TARGET  :: FirstSampTimeList
  TYPE( SampTime ), POINTER :: SampTimeList

  CHARACTER(16) matname

  CHARACTER(5),   DIMENSION(:), ALLOCATABLE :: smp_vname
  CHARACTER(5),   DIMENSION(:), ALLOCATABLE :: asmp_vname
  CHARACTER(64),  DIMENSION(:), ALLOCATABLE :: compName

  CHARACTER(16),   DIMENSION(:), ALLOCATABLE :: smp_units

  TYPE( sensor ), DIMENSION(:), ALLOCATABLE, TARGET :: smp

  TYPE srfblk_smp_fd
    INTEGER :: iblk
    INTEGER :: grdI
    INTEGER :: nfld
    INTEGER, DIMENSION(:), POINTER :: ifld
  END TYPE srfblk_smp_fd

  TYPE( srfblk_smp_fd ), DIMENSION(:), ALLOCATABLE :: depblk_smp
  TYPE( srfblk_smp_fd ), DIMENSION(:), ALLOCATABLE :: dosblk_smp

  INTEGER, DIMENSION(:), POINTER :: nsmp_dep, nsmp_dos

  TYPE( SampClassT ), POINTER :: FirstSampClass
  INTEGER :: nSampClass

  REAL uubl_S, vvbl_S, wwbl_S, wwbh_S, uub_S, vvb_S, sbl_S, sbls_S, sby_S

  INTEGER(KIND=8) :: intSPSTimeCountFilePosition, isps
  INTEGER(KIND=4) :: intSPSMaterialCount, intSPSMaterial, intSPSLoop
  INTEGER(KIND=4) :: intSPSDumpCount,  irec0SPS
  INTEGER(KIND=8) :: intSPSDataLength, nrecSPST

  LOGICAL lIsSPSOpened, lKgToUg, lOutputVariance

  INTEGER, ALLOCATABLE, DIMENSION(:) :: intSPSMaterials


END MODULE sampler_fi
