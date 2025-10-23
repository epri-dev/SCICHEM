!==============================================================================
!
!  SWIM (SCIPUFF Weather Input Module) structure definitions
!
!  16 Apr 2001 - Initial definition
!  13 Aug 2001 - Initial CVS repository
!
!==============================================================================

MODULE SWIMmetField_fd

  USE SWIMinterp_fd
  USE MapCoord_fd
  USE DefSize_fd

  IMPLICIT NONE

!------ Gridded+Obs assimilation limits

  INTEGER, PARAMETER :: MAXOBSASSM  = 21  !11  !Max number of obs sources associated with gridded source
  INTEGER, PARAMETER :: MAXTERASSM  = 10  !Max number of terrain files

  TYPE MetMean3D

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: U
    REAL, DIMENSION(:), POINTER :: V
    REAL, DIMENSION(:), POINTER :: W
    REAL, DIMENSION(:), POINTER :: Tpot
    REAL, DIMENSION(:), POINTER :: Humid
    REAL, DIMENSION(:), POINTER :: Press
    REAL, DIMENSION(:), POINTER :: Z
    REAL, DIMENSION(:), POINTER :: dU2
    REAL, DIMENSION(:), POINTER :: Qcloud

  END TYPE MetMean3D

!==============================================================================

  TYPE MetVariance

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: UU
    REAL, DIMENSION(:), POINTER :: VV
    REAL, DIMENSION(:), POINTER :: UV
    REAL, DIMENSION(:), POINTER :: SL

  END TYPE MetVariance

!==============================================================================

  TYPE MetBLprof

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: UU
    REAL, DIMENSION(:), POINTER :: VV
    REAL, DIMENSION(:), POINTER :: WW
    REAL, DIMENSION(:), POINTER :: WT
    REAL, DIMENSION(:), POINTER :: SL
    REAL, DIMENSION(:), POINTER :: SZ

  END TYPE MetBLprof

!==============================================================================

  TYPE MetQLprof

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: QQ
    REAL, DIMENSION(:), POINTER :: SL
    REAL, DIMENSION(:), POINTER :: Tpot
    REAL, DIMENSION(:), POINTER :: Press

  END TYPE MetQLprof

!==============================================================================

  TYPE MetBLparam

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: zi
    REAL, DIMENSION(:), POINTER :: HeatFlux
    REAL, DIMENSION(:), POINTER :: invMOL
    REAL, DIMENSION(:), POINTER :: prcp
    REAL, DIMENSION(:), POINTER :: rainprb
    REAL, DIMENSION(:), POINTER :: cc
    REAL, DIMENSION(:), POINTER :: zruf
    REAL, DIMENSION(:), POINTER :: ustr
    REAL, DIMENSION(:), POINTER :: LAI

  END TYPE MetBLparam

!==============================================================================

  TYPE MetBLaux

    SEQUENCE

    INTEGER                     :: JulianDay
    INTEGER                     :: nz
    REAL, DIMENSION(:), POINTER :: zsl
    REAL, DIMENSION(:), POINTER :: usl, vsl
    REAL, DIMENSION(:), POINTER :: ubl, vbl
    REAL, DIMENSION(:), POINTER :: tbl
    REAL, DIMENSION(:), POINTER :: pbl
    REAL, DIMENSION(:), POINTER :: wspd2
    REAL, DIMENSION(:), POINTER :: ustr2
    REAL, DIMENSION(:), POINTER :: wstr2
    REAL, DIMENSION(:), POINTER :: qq
    REAL, DIMENSION(:), POINTER :: aDiff, bDiff
    REAL, DIMENSION(:), POINTER :: uu_sh, uu_buoy, sbl

  END TYPE MetBLaux

!==============================================================================

  TYPE MetLandcover

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: roughness
    REAL, DIMENSION(:), POINTER :: canopyHt
    REAL, DIMENSION(:), POINTER :: alpha
    REAL, DIMENSION(:), POINTER :: Bowen
    REAL, DIMENSION(:), POINTER :: albedo

    INTEGER, DIMENSION(:), POINTER :: kbl
    INTEGER, DIMENSION(:), POINTER :: LandUse
    INTEGER julLU
  END TYPE MetLandcover

!==============================================================================

  TYPE MetTerrain

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: H, Hx, Hy
    REAL, DIMENSION(:), POINTER :: D, Du, Dv

  END TYPE MetTerrain

!==============================================================================

  TYPE McWIFdata

    SEQUENCE

    REAL :: dxi, dyi
    REAL :: MaxSlope
    REAL :: alphaMin, alphaMax
    REAL :: Hscale

    REAL, DIMENSION(:), POINTER :: z, zt
    REAL, DIMENSION(:), POINTER :: dzi, dzti
    REAL, DIMENSION(:), POINTER :: alphaFFT
    REAL, DIMENSION(:), POINTER :: cza ,czb
    REAL, DIMENSION(:), POINTER :: xevec, xsn, xcs, xeval
    REAL, DIMENSION(:), POINTER :: yevec, ysn, ycs, yeval
    REAL, DIMENSION(:), POINTER :: alphaU, alphaV

  END TYPE McWIFdata

!==============================================================================

  TYPE GridIntrp

    SEQUENCE

    LOGICAL :: Stagger

    TYPE( meth ), DIMENSION(:), POINTER :: mh, mhu, mhv
    TYPE( metv ), DIMENSION(:), POINTER :: mz, mzu, mzv, mzw
    LOGICAL     , DIMENSION(:), POINTER :: DiffLandU, DiffLandV
    REAL        , DIMENSION(:), POINTER :: zbluS, zblvS
    REAL        , DIMENSION(:), POINTER :: zblu,  zblv

  END TYPE GridIntrp

!==============================================================================

  TYPE SmoothPt

    SEQUENCE

    INTEGER, DIMENSION(:), POINTER :: ip
    REAL,    DIMENSION(:), POINTER :: wt

  END TYPE SmoothPt

!==============================================================================

  TYPE SmoothGrid

    SEQUENCE

    INTEGER xNsmth,  yNsmth
    INTEGER xNsmthU, yNsmthV
    INTEGER nXp
    TYPE( SmoothPt), DIMENSION(:), POINTER :: xSmooth
    TYPE( SmoothPt), DIMENSION(:), POINTER :: ySmooth
    TYPE( SmoothPt), DIMENSION(:), POINTER :: xSmoothU
    TYPE( SmoothPt), DIMENSION(:), POINTER :: ySmoothV

  END TYPE SmoothGrid

!==============================================================================

  TYPE SigmaCoord
    SEQUENCE
    REAL, DIMENSION(:), POINTER :: Z, Zw
    REAL, DIMENSION(:), POINTER :: Psrf
    REAL, DIMENSION(:), POINTER :: Px, Py
    REAL                        :: Ptop, P00
    REAL                        :: a1, a2, aiso
  END TYPE SigmaCoord

!==============================================================================

  TYPE MetGrid

    SEQUENCE

    INTEGER                     :: type
    REAL                        :: Xmin, Xmax, dX
    REAL                        :: Ymin, Ymax, dY
    REAL                        :: Ztop
    REAL                        :: Hmin, Hmax
    REAL, DIMENSION(:), POINTER :: Z
    REAL, DIMENSION(:), POINTER :: Zw
    REAL, DIMENSION(:), POINTER :: lat, lon
    REAL, DIMENSION(:), POINTER :: sunrise, sunset
    REAL, DIMENSION(:), POINTER :: sunfac
    INTEGER                     :: AlignmentPadding
    INTEGER                     :: nX, nY, nZ, nXY
    TYPE( MetTerrain   )        :: terrain
    TYPE( MetLandcover )        :: landcover
    TYPE( McWIFdata    )        :: McWIF
    TYPE( SmoothGrid   )        :: Smooth
    TYPE( MapCoord     )        :: coord
    TYPE( SigmaCoord   )        :: sigma

    INTEGER                  :: numMG
    TYPE( MetGrid ), POINTER :: MGgrid
    TYPE( MetGrid ), POINTER :: prevMGgrid

  END TYPE MetGrid

!==============================================================================

  TYPE GridSrc

    SEQUENCE

    !With Type as INTEGER(8) need to keep structure size a multiple of 8 bytes
    !with large elements on 8 byte boundaries
    !Note: POINTER size is compiler dependent.

    INTEGER(KIND=8)                       :: type
    INTEGER                               :: nSource
    INTEGER                               :: unit
    INTEGER                               :: nX, nY, nZ, nXY
    INTEGER                               :: iStart, jStart, kStart
    INTEGER                               :: iEnd  , jEnd
    INTEGER                               :: iSkip,  jSkip
    INTEGER                               :: UTMzone
    INTEGER                               :: nVar2d
    INTEGER                               :: nVar3d
    INTEGER                               :: nStg3D, nStg2D
    INTEGER                               :: nSkipBreak
    INTEGER                               :: nBreak
    INTEGER                               :: nrecH
    LOGICAL                               :: lBuoy
    LOGICAL                               :: lPrecipAux
    INTEGER                               :: LUmodel
    LOGICAL                               :: lVertFlip
    REAL                                  :: X0, dX
    REAL                                  :: Y0, dY
    REAL                                  :: timeOffset
    REAL                                  :: tFirst
    REAL                                  :: tFcst
    REAL                                  :: Ztop
    REAL                                  :: Lat0, Lon0
    REAL                                  :: Lat1, Lat2  !Ref. lat/lon are in Lat0,Lon0
    REAL                                  :: TsrfRef, lapseRef, Tiso
    REAL                                  :: Ptop, P00
    TYPE( MetMean3D  )                    :: ReadField
    TYPE( MetBLparam )                    :: ReadBL
    TYPE( MetVariance )                   :: ReadLSV
    TYPE( GridIntrp )                     :: IntrpFac
    CHARACTER(PATH_MAXLENGTH), &
                    DIMENSION(:), POINTER :: Source
    CHARACTER(16),  DIMENSION(:), POINTER :: Var2dName
    CHARACTER(16),  DIMENSION(:), POINTER :: Var3dName
    INTEGER,        DIMENSION(:), POINTER :: Var2dID
    INTEGER,        DIMENSION(:), POINTER :: Var3dID
    INTEGER,      DIMENSION(:,:), POINTER :: iStg3D,     iStg2D
    INTEGER,      DIMENSION(:,:), POINTER :: Var3dShift, Var2dShift
    REAL,           DIMENSION(:), POINTER :: Conv2d
    REAL,           DIMENSION(:), POINTER :: Conv3d
    REAL,           DIMENSION(:), POINTER :: Z, Zw
    REAL,           DIMENSION(:), POINTER :: P
  END TYPE GridSrc

!==============================================================================

  TYPE MetObsWt

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: su
    REAL, DIMENSION(:), POINTER :: sv
    REAL, DIMENSION(:), POINTER :: st
    REAL, DIMENSION(:), POINTER :: sh
    REAL, DIMENSION(:), POINTER :: sp
    REAL, DIMENSION(:), POINTER :: sqc
    REAL, DIMENSION(:), POINTER :: szi
    REAL, DIMENSION(:), POINTER :: shflx
    REAL, DIMENSION(:), POINTER :: sustr
    REAL, DIMENSION(:), POINTER :: smol
    REAL, DIMENSION(:), POINTER :: sprcp
    REAL, DIMENSION(:), POINTER :: scc
    REAL, DIMENSION(:), POINTER :: slai

  END TYPE MetObsWt

!==============================================================================

  TYPE MetField

    SEQUENCE

    !With type as INTEGER(8) need to keep structure size a multiple of 8 bytes
    !with large elements on 8 byte boundaries
    !Note: POINTER size is compiler dependent.

    INTEGER(KIND=8)       :: type

    CHARACTER(PATH_MAXLENGTH) :: fileOut

    INTEGER               :: index
    INTEGER               :: status
    INTEGER               :: BLtype
    INTEGER               :: LSVtype
    INTEGER               :: unitOut
    REAL                  :: t
    REAL                  :: tNext
    REAL                  :: tBL

    TYPE( MetObsWt )      :: obsWt       !Located here for alignment issues
    TYPE( MetBLaux )      :: BLaux

    TYPE( GridSrc )       :: gridSource
    TYPE( MetGrid )       :: grid

    TYPE( MetMean3D  )    :: Field
    TYPE( MetBLparam )    :: BL
    TYPE( MetBLprof  )    :: BLprof
    TYPE( MetQLprof  )    :: QLprof

    TYPE( MetMean3D  )    :: NextField
    TYPE( MetBLparam )    :: NextBL
    TYPE( MetBLprof  )    :: NextBLprof
    TYPE( MetQLprof  )    :: NextQLprof

    TYPE( MetVariance )   :: LSV
    TYPE( MetVariance )   :: NextLSV

    REAL, DIMENSION(:), &
                  POINTER :: NextZ
    INTEGER               :: nObsSource
    INTEGER,                               &
    DIMENSION(MAXOBSASSM) :: IObsSource
    REAL                  :: t1
    REAL                  :: t2
    TYPE( MetMean3D  )    :: Field1
    TYPE( MetBLparam )    :: BL1
    TYPE( MetVariance )   :: LSV1
    TYPE( MetMean3D  )    :: Field2
    TYPE( MetBLparam )    :: BL2
    TYPE( MetVariance )   :: LSV2

  END TYPE MetField

END MODULE SWIMmetField_fd

!==============================================================================
!==============================================================================
!==============================================================================

MODULE SWIMobs_fd

  USE DefSize_fd
  USE SWIMinterp_fd

  IMPLICIT NONE

!==============================================================================

  TYPE ObsVariance

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: uu
    REAL, DIMENSION(:), POINTER :: vv
    REAL, DIMENSION(:), POINTER :: uv
    REAL, DIMENSION(:), POINTER :: sl

  END TYPE ObsVariance

!==============================================================================

  TYPE ObsBLPrf

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: uu
    REAL, DIMENSION(:), POINTER :: vv
    REAL, DIMENSION(:), POINTER :: ww
    REAL, DIMENSION(:), POINTER :: sl
    REAL, DIMENSION(:), POINTER :: sz
    REAL, DIMENSION(:), POINTER :: wt

  END TYPE ObsBLPrf

!==============================================================================

  TYPE ObsVelPrf

    SEQUENCE

    INTEGER                     :: nz
    REAL, DIMENSION(:), POINTER :: z
    REAL, DIMENSION(:), POINTER :: u
    REAL, DIMENSION(:), POINTER :: v
    TYPE( ObsVariance )         :: LSV
    TYPE( ObsBLPrf    )         :: BLprof

  END TYPE ObsVelPrf

!==============================================================================

  TYPE ObsPrf

    SEQUENCE

    INTEGER                     :: nz
    REAL, DIMENSION(:), POINTER :: z
    REAL, DIMENSION(:), POINTER :: obs

  END TYPE ObsPrf


!==============================================================================

  TYPE ObsSrf

    SEQUENCE

    REAL :: zi
    REAL :: hflux
    REAL :: ustr
    REAL :: invL
    REAL :: prcp
    REAL :: cloudcover
    REAL :: LAI

  END TYPE ObsSrf

!==============================================================================

  TYPE ObsTimeID

    SEQUENCE

    INTEGER :: type
    INTEGER :: year
    INTEGER :: month
    INTEGER :: day
    INTEGER :: hour
    INTEGER :: day_start
    INTEGER :: year_start

  END TYPE ObsTimeID

!==============================================================================

  TYPE ObsIndex

    SEQUENCE

    INTEGER           :: ID
    INTEGER           :: z
    TYPE( ObsTimeID ) :: TimeID

  END TYPE ObsIndex

!==============================================================================

  TYPE ObsLandcover

    SEQUENCE

    REAL :: zruf
    REAL :: hc
    REAL :: alpha
    REAL :: Bowen
    REAL :: albedo
    REAL :: adens

  END TYPE ObsLandcover

!==============================================================================

  TYPE ObsBLparam

    SEQUENCE

    REAL                 :: zsl
    REAL                 :: usl, vsl
    REAL                 :: uslx, vslx
    REAL                 :: L
    REAL                 :: hflux     !From surface layer calculations
    REAL                 :: zi        !if not available as observation
    REAL                 :: tref, pref
    REAL                 :: cc
    REAL                 :: h
    REAL                 :: LAI
    TYPE( ObsLandcover ) :: surface


  END TYPE ObsBLparam

!==============================================================================

  TYPE ObsStndAtmos

    SEQUENCE

    REAL :: Tbot, Ttop, LogPtop, LogPbot
    REAL :: RHbot, RHtop, PHbot, PHtop, extrapRHslope

  END TYPE ObsStndAtmos

!==============================================================================

  TYPE ObsSrfPrf !Surface layer profile used for interpolation

    SEQUENCE

    REAL, DIMENSION(:), POINTER :: z
    REAL, DIMENSION(:), POINTER :: fsl

  END TYPE ObsSrfPrf

!==============================================================================

  TYPE ObsMet

    SEQUENCE

    CHARACTER(64)           :: id
    CHARACTER(256)          :: string
    REAL                    :: x, y
    REAL                    :: xfac, yfac
    REAL                    :: a2
    REAL                    :: vscaleTop
    REAL                    :: vscaleBot
    TYPE( ObsVelPrf   )     :: Vel
    TYPE( ObsPrf      )     :: Tpot
    TYPE( ObsPrf      )     :: Humid
    TYPE( ObsPrf      )     :: Press
    TYPE( ObsPrf      )     :: Qcloud
    TYPE( ObsSrf      )     :: varSrf
    TYPE( ObsBLparam  )     :: BL
    TYPE( ObsStndAtmos )    :: Stnd
    TYPE( ObsSrfPrf   )     :: SrfPrf
    TYPE( ObsMet ), POINTER :: nextObs
    TYPE( ObsMet ), POINTER :: prevObs
    TYPE( ObsVelPrf   )     :: VelObs
    TYPE( ObsPrf      )     :: TpotObs, TpotStA
    TYPE( ObsPrf      )     :: PressObs
    TYPE( ObsPrf      )     :: HumidObs
    TYPE( ObsPrf      )     :: QcloudObs

    TYPE( meth )            :: mxy, mxyu, mxyv
    TYPE( metv ), &
      DIMENSION(:), POINTER :: mzVel, mzTpot, mzPress, mzHumid
    TYPE( metv ), &
      DIMENSION(:), POINTER :: mzQcloud

  END TYPE ObsMet

!==============================================================================

  TYPE ObsGridList

    SEQUENCE

    TYPE( ObsMet ),      POINTER :: Obs
    TYPE( ObsGridList ), POINTER :: Next

  END TYPE ObsGridList

!==============================================================================

  TYPE FirstObsGridList

    SEQUENCE

    INTEGER                                    :: Nx, Ny
    REAL                                       :: Xmin, Xmax
    REAL                                       :: Ymin, Ymax
    TYPE( ObsGridList ), DIMENSION(:), POINTER :: GridList
    INTEGER, DIMENSION(:), POINTER             :: NumObsCell
    LOGICAL                                    :: lInterpVel, lInterpT, lInterpP, lInterpH
    LOGICAL                                    :: lInterpQcld
    LOGICAL                                    :: lInterpZi, lInterpHf, lInterpUs, lInterpL, lInterpCC, lInterpPr

  END TYPE FirstObsGridList

!==============================================================================

  TYPE FirstObs

    SEQUENCE

    !With type as INTEGER(8) need to keep structure size a multiple of 8 bytes
    !with large elements on 8 byte boundaries
    !Note: POINTER size is compiler dependent.

    CHARACTER(PATH_MAXLENGTH)      :: Source
    INTEGER(KIND=8)                :: type
    INTEGER                        :: unit
    INTEGER                        :: nVar
    CHARACTER(8)                   :: BadString
    INTEGER, DIMENSION(:), POINTER :: VarID
    REAL,    DIMENSION(:), POINTER :: Conv
    INTEGER                        :: nVarFixed
    TYPE( ObsIndex )               :: index
    REAL                           :: zref
    INTEGER                        :: nz
    REAL, DIMENSION(:), POINTER    :: z
    REAL                           :: timeBin
    REAL                           :: timeOffset
    LOGICAL                        :: local
    REAL                           :: tFcst
    REAL                           :: time
    INTEGER                        :: numObs
    INTEGER                        :: numObsDom
    REAL                           :: Zmax
    TYPE( ObsMet ), POINTER        :: Obs
    TYPE( FirstObsGridList ), &
                        POINTER    :: GridList
    REAL                           :: PrevTime
    INTEGER                        :: PrevNumObs
    INTEGER                        :: PrevNumObsDom
    REAL                           :: PrevZmax
    TYPE( ObsMet ), POINTER        :: PrevObs
    TYPE( FirstObsGridList ), &
                        POINTER    :: PrevGridList
    REAL, DIMENSION(:), POINTER    :: ObsArray
    LOGICAL                        :: lend
    LOGICAL                        :: lWarnTooMuchObsDAta
    LOGICAL                        :: lAERMET
    CHARACTER(64)                  :: AERid, AER_UAid
    REAL                           :: Lat, Lon
    REAL                           :: BaseElev
    LOGICAL                        :: lASOS1, lASOSthermo
    INTEGER                        :: hourOffset
    INTEGER                        :: nASOS1repeat
    CHARACTER(PATH_MAXLENGTH)      :: errorLine1, errorLine2
    CHARACTER(PATH_MAXLENGTH)      :: AuxSource

  END TYPE FirstObs

!==============================================================================

  TYPE ObsList
    REAL                    :: r2, wt
    TYPE( ObsMet ), POINTER :: Obs
  END TYPE ObsList

  TYPE NearestObs
    INTEGER                                :: numInterp
    INTEGER                                :: numObs
    TYPE( ObsList ), DIMENSION(:), POINTER :: Obs
  END TYPE NearestObs

END MODULE SWIMobs_fd

!==============================================================================
!==============================================================================
!==============================================================================

MODULE SWIMerr_fd

  IMPLICIT NONE

  TYPE SWIMerror

    SEQUENCE

    INTEGER        :: Number
    CHARACTER(80)  :: Routine
    CHARACTER(128) :: Message
    CHARACTER(128) :: Inform
    CHARACTER(128) :: Action

  END TYPE SWIMerror

END MODULE SWIMerr_fd

!==============================================================================
!==============================================================================
!==============================================================================

MODULE SWIMrect_fd

  IMPLICIT NONE

  INTEGER SWIMrectNotUsed

END MODULE SWIMrect_fd

