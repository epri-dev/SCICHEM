MODULE SWIMobsInterp_fd

  USE SWIMmetField_fd

  TYPE ObsInterpPrf

    REAL, DIMENSION(:), POINTER :: wt
    REAL, DIMENSION(:), POINTER :: obs

  END TYPE ObsInterpPrf

  TYPE ObsInterpSrf

    REAL :: wt
    REAL :: obs

  END TYPE ObsInterpSrf

  TYPE ObsInterp

    INTEGER                       :: type
    REAL                          :: x,  y
    REAL                          :: xu, yv
    REAL                          :: xfac, yfac
    REAL                          :: a2
    INTEGER                       :: nz
    REAL, DIMENSION(:), POINTER   :: z
    TYPE( ObsInterpPrf )          :: U, V
    TYPE( ObsInterpPrf )          :: Tpot
    TYPE( ObsInterpPrf )          :: TpotStA
    TYPE( ObsInterpPrf )          :: Humid
    TYPE( ObsInterpPrf )          :: Press
    TYPE( ObsInterpPrf )          :: Qcloud
    TYPE( ObsInterpPrf )          :: UUL, VVL, UVL, SHL
    TYPE( ObsInterpPrf )          :: UU, VV, WW, WT, SL, SZ
    TYPE( ObsInterpSrf )          :: Zi
    TYPE( ObsInterpSrf )          :: Hflux
    TYPE( ObsInterpSrf )          :: Ust
    TYPE( ObsInterpSrf )          :: invL
    TYPE( ObsInterpSrf )          :: Prcp
    TYPE( ObsInterpSrf )          :: CldCv
    TYPE( ObsInterpSrf )          :: Ustar
    TYPE( ObsInterpSrf )          :: LAI
    REAL                          :: zruf, hc, alpha
    REAL                          :: h, hu, hv
    REAL                          :: d, du, dv
    REAL                          :: zmax
    REAL                          :: Hmin
    LOGICAL                       :: lExtrap

  END TYPE ObsInterp

END MODULE SWIMobsInterp_fd
