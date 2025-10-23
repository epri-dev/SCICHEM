!==============================================================================
!
!  SWIM (SCIPUFF Weather Input Modules) puff interface definitions
!
!  May 8, 2001 - Initial definition
!==============================================================================

MODULE SWIMpuff_fd

!------ Puff request structure

  TYPE PuffMetRequest

    SEQUENCE

    INTEGER :: iField
    INTEGER :: type
    REAL    :: X, Y, Z
    REAL    :: Shh
    REAL    :: SigZ
    REAL    :: Zcap

  END TYPE PuffMetRequest

!------ Mean velocity gradients

  TYPE PuffVelGrad

    SEQUENCE

    REAL :: Ux, Uy, Uz
    REAL :: Vx, Vy, Vz
    REAL :: Wx, Wy, Wz

  END TYPE PuffVelGrad

!------ Mean met

  TYPE PuffMetMean

    SEQUENCE

    REAL                :: U
    REAL                :: V
    REAL                :: W
    REAL                :: Tpot
    REAL                :: TpotZ
    REAL                :: T
    REAL                :: Humid
    REAL                :: Qcloud
    REAL                :: cldTop, cldBot
    REAL                :: cldAvg
    REAL                :: Press
    TYPE( PuffVelGrad ) :: Ugrad
    REAL                :: dU2

  END TYPE PuffMetMean

!------ Boundary layer parameters

  TYPE PuffMetBLparam

    SEQUENCE

    REAL    :: MixingHt
    REAL    :: Zsl
    REAL    :: dTdz_mh
    REAL    :: HeatFlux
    REAL    :: L
    REAL    :: TerElev
    REAL    :: Hmin
    REAL    :: zruf
    REAL    :: CanopyHt
    REAL    :: Alpha
    REAL    :: Ustar2
    REAL    :: Wstar2
    REAL    :: UstarDep
    REAL    :: sunfac
    REAL    :: prate
    REAL    :: cc
    REAL    :: LAI
    INTEGER :: PrecipType

  END TYPE PuffMetBLparam

!------ Turbulence parameters

  TYPE PuffMetTurb

    SEQUENCE

    REAL    :: UUshear
    REAL    :: UUbuoy
    REAL    :: WW
    REAL    :: WT
    REAL    :: QQshear
    REAL    :: LscaleShear
    REAL    :: LscaleBuoy
    REAL    :: Diff
    REAL    :: DiffGrad
    REAL    :: DiffGradX
    REAL    :: DiffGradY
    REAL    :: WWgrad

  END TYPE PuffMetTurb

  TYPE PuffMetVariance

    SEQUENCE

    REAL    :: UU
    REAL    :: VV
    REAL    :: UV
    REAL    :: UUz
    REAL    :: VVz
    REAL    :: UVz
    REAL    :: Lscale

  END TYPE PuffMetVariance

!------ Puff met

  TYPE PuffMet

    SEQUENCE

    INTEGER                 :: iField
    REAL                    :: dElev
    TYPE( PuffMetMean     ) :: Mean
    TYPE( PuffMetBLparam  ) :: BL
    TYPE( PuffMetTurb     ) :: Turb
    TYPE( PuffMetVariance ) :: LSV

  END TYPE PuffMet

END MODULE SWIMpuff_fd
