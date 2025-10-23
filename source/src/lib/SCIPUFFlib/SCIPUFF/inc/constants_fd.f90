MODULE constants_fd

!------ Mathematical constants

  REAL, PARAMETER :: PI        = 3.141592653
  REAL, PARAMETER :: PI2       = 2.*PI
  REAL, PARAMETER :: PI3       = 15.74961    !SQRT(2*PI)**3
  REAL, PARAMETER :: PI180     = PI/180.
  REAL, PARAMETER :: SQRTPI    = 1.77245385
  REAL, PARAMETER :: EULER     = 0.577215664
  REAL, PARAMETER :: LOG2      = 0.693147180 !LN(2)
  REAL, PARAMETER :: LOGR2     = -LOG2       !LN(0.5)
  REAL, PARAMETER :: RLOGR2    = 1./LOGR2    !1/LN(0.5)
  REAL, PARAMETER :: SQRT2     = 1.41421356
  REAL, PARAMETER :: SQRT3     = 1.73205081
  REAL, PARAMETER :: TwoThirds = 0.6666667
  REAL, PARAMETER :: SMALL     = 1.E-20

  INTEGER(8), PARAMETER :: IZERO8 = 0

!------ Earth constants

  REAL, PARAMETER :: Rearth  = 6371.E3           !m - Mean radius
  REAL, PARAMETER :: Reqtr   = 6378.E3           !m - Radius at equator
  REAL, PARAMETER :: EC2     = 6.69438E-3        !Eccentricity^2
  REAL, PARAMETER :: SPHFAC  = PI180*Rearth      !m/deg
  REAL, PARAMETER :: SPHFACR = 1.0/SPHFAC
  REAL, PARAMETER :: G0      = 9.81              !m/s/s
  REAL, PARAMETER :: FCOR0   = 4.*PI/(24.*3600.) !1/s

!------ Properties of Air & Atmosphere

  REAL, PARAMETER :: ABSZERO = -273.15       !deg-K
  REAL, PARAMETER :: RGAS    = 287.04        !Gas constant for dry air (J/kg/K)
  REAL, PARAMETER :: KAPPA   = 2./7.         !(Cp-Cv)/Cp for diatomic molecule (dry air)
  REAL, PARAMETER :: KAPPAC  = 1.- KAPPA
  REAL, PARAMETER :: CP      = 1004.         !
  REAL, PARAMETER :: MWAIR   = 29.0          !Mol. wt of air
  REAL, PARAMETER :: MW_WATER= 18.0          !Mol. wt of water
  REAL, PARAMETER :: MR      = 0.62198       !MW_WATER / MWAIR (WMO value for rel. humid.)
  REAL, PARAMETER :: PSURF   = 1013.25       !mb (Standard Atmosphere)
  REAL, PARAMETER :: TSURF   = 288.15        !K  (Standard Atmosphere)
  REAL, PARAMETER :: GAMMA0  = G0*KAPPA/RGAS !Adiabatic lapse rate
  REAL, PARAMETER :: RHOCP   = 1206.         !rho*Cp for air
  REAL, PARAMETER :: RHO_WATER = 1000.0      !Water density (Kg/m^3)
  REAL, PARAMETER :: SIG_SB  = 5.67E-8       !Stefan-Boltzman constant (W/m^2/K)
  REAL, PARAMETER :: ST_WATER= 0.0728        !Surface tension of water, N/m
  REAL, PARAMETER :: RHSLOPE = 7.8571E-2     !d(RH)/dP (percent/mb) from Manabe & Wetherald (1967)
  REAL, PARAMETER :: RHSRF   = 77.           !Surface relative humidity (percent) (Manabe & Wetherald)
  REAL, PARAMETER :: PSTRAT  = 200.          !Pressure (mb) at bottom of reference stratosphere
  REAL, PARAMETER :: RHSTRAT = 14.14         !Relative humidity (percent) at PSTRAT (Manabe & Wetherald)
  REAL, PARAMETER :: HSTRAT  = 3.E-6         !Stratosphere (minimum) mixing ratio
  REAL, PARAMETER :: GAMMA_EPA = 5.E-3       !Minimum overlying pot. temp. gradient (EPA)
  REAL, PARAMETER :: PPM2UGM3  = &
                     PSURF*750.06/(62.4*298) !Factor for converting ppm to ug/m3 using EPA STP values

!------ Turbulence Model Constants

  REAL, PARAMETER :: VONK  = 0.4             !von Karman constant
  REAL, PARAMETER :: A     = 0.75
  REAL, PARAMETER :: B     = 0.125
  REAL, PARAMETER :: BS    = 1.8*B
  REAL, PARAMETER :: CVRTX = 3.0
  REAL, PARAMETER :: CQB   = 0.4
  REAL, PARAMETER :: CSI1  = 0.25            !Internal scale growth: BL (3d)
  REAL, PARAMETER :: CSI2  = 0.30            !Internal scale growth: Large-scale (2D)
  REAL, PARAMETER :: EQF   = G0/300./(2.0*A*BS)
  REAL, PARAMETER :: AREA_DENSITY_TO_ALPHA = 10.8 !Conversion factor for area density
                                                  !to velocity attenuation parameter
END MODULE constants_fd

