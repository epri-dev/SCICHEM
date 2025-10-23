!==============================================================================
!
!  SWIM (SCIPUFF Weather Input Modules) parameter definitions
!
!  April 16, 2001 - Initial definition
!==============================================================================

MODULE SWIMparam_fd

  USE errorParam_fd
  USE coordinate_fd

!------ Success/failure

  INTEGER, PARAMETER :: SWIMsuccess = 1
  INTEGER, PARAMETER :: SWIMnull    = 0
  INTEGER, PARAMETER :: SWIMfailure = -1

!------ Halt

  INTEGER, PARAMETER :: SWIM_ENABLE  = 0
  INTEGER, PARAMETER :: SWIM_HALT    = 2
  INTEGER, PARAMETER :: SWIM_DISABLE = -999

!------ Met source type

  INTEGER, PARAMETER :: SWIMobs    = 0
  INTEGER, PARAMETER :: SWIMfixed  = 1
  INTEGER, PARAMETER :: SWIMgrid   = 2 !Generic gridded (type TBD)
  INTEGER, PARAMETER :: SWIMMEDOC  = 3
  INTEGER, PARAMETER :: SWIMSCIP   = 4
  INTEGER, PARAMETER :: SWIMnest   = 7
  INTEGER, PARAMETER :: SWIMassim   = 9
  INTEGER, PARAMETER :: SWIMterAssm = 10
  INTEGER, PARAMETER :: SWIMvrtGrid = 11
  INTEGER, PARAMETER :: SWIMAERsfc  = 12
  INTEGER, PARAMETER :: SWIMAERpfl  = 13
  INTEGER, PARAMETER :: SWIMWRF     = 14
  INTEGER, PARAMETER :: SWIMMEDLIST = 22
  INTEGER, PARAMETER :: SWIMASOS1M  = 23
  INTEGER, PARAMETER :: ASSIM_R_OBS       = 16  !These parameters must be distinct from met source types
  INTEGER, PARAMETER :: ASSIM_E2BO        = 17
  INTEGER, PARAMETER :: ASSIM_DU2EPS      = 18
  INTEGER, PARAMETER :: ASSIM_SCM_MAXITER = 19
  INTEGER, PARAMETER :: ASSIM_MCWIFWtFac  = 20

!------ Overall SWIM status bits

  INTEGER, PARAMETER :: SSB_NEWMET = 0
  INTEGER, PARAMETER :: SSB_UPDATE = 1
  INTEGER, PARAMETER :: SSB_EXPAND = 2

!------ File unit numbers

  INTEGER, PARAMETER :: DbgUnit   = 111  !For debug output
  INTEGER, PARAMETER :: SWIMunit  = 100  !For temporary unit number; also offset for 'permanent' file units


!------ Mass-Consistent type bits (input) - Consistent with SCIPtool

  INTEGER, PARAMETER :: MCB_SWIFT    = 0  !Use SWIFT (if possible)
  INTEGER, PARAMETER :: MCB_MCWIF    = 1  !Use McWif
  INTEGER, PARAMETER :: MCB_GRIDNEST = 3  !Nest w/ gridded (MEDOC) parent
  INTEGER, PARAMETER :: MCB_TER      = 10 !Terrain elevation
  INTEGER, PARAMETER :: MCB_LC       = 14 !Land cover

!------ Field status bits

  INTEGER, PARAMETER :: FSB_DOINIT      = 0
  INTEGER, PARAMETER :: FSB_UPDATE      = 1
  INTEGER, PARAMETER :: FSB_FIRST       = 3
  INTEGER, PARAMETER :: FSB_DOINITBL    = 5
  INTEGER, PARAMETER :: FSB_TERRAIN     = 6
  INTEGER, PARAMETER :: FSB_LSV         = 7
  INTEGER, PARAMETER :: FSB_DONESMOOTH  = 9
  INTEGER, PARAMETER :: FSB_EXPAND      = 10
  INTEGER, PARAMETER :: FSB_FIRSTASSM   = 15
  INTEGER, PARAMETER :: FSB_CANOPYWARN  = 16
  INTEGER, PARAMETER :: FSB_SECOND      = 17
  INTEGER, PARAMETER :: FSB_INITSMOOTH  = 18
  INTEGER, PARAMETER :: FSB_DOINITBL2   = 19

!------ Field type bits

  INTEGER, PARAMETER :: FTB_OBS   = 0
  INTEGER, PARAMETER :: FTB_MEDOC = 1
  INTEGER, PARAMETER :: FTB_SCIP  = 2
  INTEGER, PARAMETER :: FTB_MCWIF = 6
  INTEGER, PARAMETER :: FTB_INTRP  = 9
  INTEGER, PARAMETER :: FTB_NEST   = 10
  INTEGER, PARAMETER :: FTB_SMOOTH = 11
  INTEGER, PARAMETER :: FTB_NPOLE  = 12
  INTEGER, PARAMETER :: FTB_SPOLE  = 13

  INTEGER, PARAMETER :: FTB_W     = 14
  INTEGER, PARAMETER :: FTB_T     = 15
  INTEGER, PARAMETER :: FTB_P     = 16
  INTEGER, PARAMETER :: FTB_H     = 17
  INTEGER, PARAMETER :: FTB_ZI    = 18
  INTEGER, PARAMETER :: FTB_HFLX  = 19
  INTEGER, PARAMETER :: FTB_MOL   = 20
  INTEGER, PARAMETER :: FTB_PRCP  = 21
  INTEGER, PARAMETER :: FTB_CLDCV = 22
  INTEGER, PARAMETER :: FTB_LSV   = 23
  INTEGER, PARAMETER :: FTB_LSVL  = 24
  INTEGER, PARAMETER :: FTB_UU    = 27
  INTEGER, PARAMETER :: FTB_DU2   = 28
  INTEGER, PARAMETER :: FTB_ZRUF  = 29
  INTEGER, PARAMETER :: FTB_PRATE = 30
  INTEGER, PARAMETER :: FTB_Z     = 31
  INTEGER, PARAMETER :: FTB_UST   = 33
  INTEGER, PARAMETER :: FTB_WRF   = 34
  INTEGER, PARAMETER :: FTB_QCLD  = 36
  INTEGER, PARAMETER :: FTB_ACCPR = 39
  INTEGER, PARAMETER :: FTB_LAI   = 42

!----- The index element of the met field structure is used to save both the
!      index of the field and the index of its smoothed met ancestor. The
!      following parameters give the corresponding bit start positions and lengths

  INTEGER, PARAMETER :: POS_INDEX = 0   !Start of field index
  INTEGER, PARAMETER :: LEN_INDEX = 16  !Length of field index

  INTEGER, PARAMETER :: POS_SMOOTH = 16 !Start of ancestor met field index
  INTEGER, PARAMETER :: LEN_SMOOTH = 16 !Length of ancestor met field index

  INTEGER, PARAMETER :: NMIN_SMTH = 6    !Minium grid size after smoothing

!------ Field grid type bits

  INTEGER, PARAMETER :: GTB_STAGGER   = 0
  INTEGER, PARAMETER :: GTB_LATLON    = 1
  INTEGER, PARAMETER :: GTB_UTM       = 2
  INTEGER, PARAMETER :: GTB_CARTESIAN = 3
  INTEGER, PARAMETER :: GTB_METERS    = 4
  INTEGER, PARAMETER :: GTB_NPOLE     = 5
  INTEGER, PARAMETER :: GTB_SPOLE     = 6
  INTEGER, PARAMETER :: GTB_PERIODIC  = 7

  INTEGER, PARAMETER :: GTB_TERRAIN   = 10
  INTEGER, PARAMETER :: GTB_ZRUF      = 11
  INTEGER, PARAMETER :: GTB_HCNP      = 12
  INTEGER, PARAMETER :: GTB_ALPHA     = 13
  INTEGER, PARAMETER :: GTB_ALBEDO    = 14
  INTEGER, PARAMETER :: GTB_BOWEN     = 15
  INTEGER, PARAMETER :: GTB_LANDUSE   = 16
  INTEGER, PARAMETER :: GTB_LAMBERT   = 17
  INTEGER, PARAMETER :: GTB_POLAR     = 18
  INTEGER, PARAMETER :: GTB_RPOLAR    = 19
  INTEGER, PARAMETER :: GTB_MERCATOR  = 20
  INTEGER, PARAMETER :: GTB_ROTLL     = 21
  INTEGER, PARAMETER :: GTB_SIGMA     = 22
  INTEGER, PARAMETER :: GTB_STAGGERZ  = 23 !W is shifted
  INTEGER, PARAMETER :: GTB_STAGGERB  = 24 !Arakawa B
  INTEGER, PARAMETER :: GTB_Z3D       = 25 !3D height array
  INTEGER, PARAMETER :: GTB_Z3DW      = 26 !Height defined on W-point
  INTEGER, PARAMETER :: GTB_NEST      = 27 !Nested domain

!------ North/South pole parameters

  INTEGER, PARAMETER :: NOTAPOLE  = 0
  INTEGER, PARAMETER :: NORTHPOLE = 1
  INTEGER, PARAMETER :: SOUTHPOLE = 2

!------ Boundary layer type parameters

  INTEGER, PARAMETER :: BLP_NONE   = 0
  INTEGER, PARAMETER :: BLP_SBL    = 1
  INTEGER, PARAMETER :: BLP_MET    = 2
  INTEGER, PARAMETER :: BLP_NEST   = 3
  INTEGER, PARAMETER :: BLP_CALC   = 4
  INTEGER, PARAMETER :: BLP_PROF   = 5
  INTEGER, PARAMETER :: BLP_OPER   = 6

!------ Large-scale variability parameters

  INTEGER, PARAMETER :: LVP_OFF   = 0
  INTEGER, PARAMETER :: LVP_INPUT = 1
  INTEGER, PARAMETER :: LVP_MODEL = 2
  INTEGER, PARAMETER :: LVP_MET   = 3
  INTEGER, PARAMETER :: LVP_OPER  = 4

!------ Gridded source bits

  INTEGER, PARAMETER :: GSB_MEDOC      = 0
  INTEGER, PARAMETER :: GSB_SCIP       = 1
  INTEGER, PARAMETER :: GSB_NEST       = 8
  INTEGER, PARAMETER :: GSB_SMOOTH     = 9

  INTEGER, PARAMETER :: GSB_BINARY     = 10

  INTEGER, PARAMETER :: GSB_LATLON     = 11
  INTEGER, PARAMETER :: GSB_UTM        = 12
  INTEGER, PARAMETER :: GSB_CARTESIAN  = 13
  INTEGER, PARAMETER :: GSB_STAGGER    = 14
  INTEGER, PARAMETER :: GSB_MCWIF      = 16    !N.B. Not used in any test (could be removed)
  INTEGER, PARAMETER :: GSB_PERIODIC   = 17    !Requires additional point for periodic BC
  INTEGER, PARAMETER :: GSB_ABSTEMP    = 18
  INTEGER, PARAMETER :: GSB_SPD        = 19
  INTEGER, PARAMETER :: GSB_ABSHUMID   = 20
  INTEGER, PARAMETER :: GSB_NOMAP      = 21
  INTEGER, PARAMETER :: GSB_LAMBERT    = 22
  INTEGER, PARAMETER :: GSB_POLAR      = 23
  INTEGER, PARAMETER :: GSB_RPOLAR     = 24
  INTEGER, PARAMETER :: GSB_ROTLL      = 25
  INTEGER, PARAMETER :: GSB_MERCATOR   = 26
  INTEGER, PARAMETER :: GSB_SIGMAF     = 27
  INTEGER, PARAMETER :: GSB_ZMSL       = 28
  INTEGER, PARAMETER :: GSB_ZAGL       = 29
  INTEGER, PARAMETER :: GSB_SIGMAZ     = 30
  INTEGER, PARAMETER :: GSB_SIGMAZM    = 31
  INTEGER, PARAMETER :: GSB_WRF        = 32
  INTEGER, PARAMETER :: GSB_INITWRF    = 33
  INTEGER, PARAMETER :: GSB_QCLDMIX    = 37
  INTEGER, PARAMETER :: GSB_ZSTAGGER   = 38
  INTEGER, PARAMETER :: GSB_STAGGERB   = 39
  INTEGER, PARAMETER :: GSB_MEDOCLIST  = 40  !From MEDOC list
  INTEGER, PARAMETER :: GSB_INITMEDOC  = 41
  INTEGER, PARAMETER :: GSB_WRFINP     = 42
  INTEGER, PARAMETER :: GSB_OLDMMIF    = 46

!------ NOT_SET values for MEDOC input/output

  REAL,    PARAMETER :: NOT_SET_MED_R = -999999.
  INTEGER, PARAMETER :: NOT_SET_MED_I = -999999

!------ Recognized gridded file variable parameters

  INTEGER, PARAMETER :: GVP_NONE = 0
  INTEGER, PARAMETER :: GVP_U    = 1
  INTEGER, PARAMETER :: GVP_V    = 2
  INTEGER, PARAMETER :: GVP_SPD  = 3
  INTEGER, PARAMETER :: GVP_DIR  = 4
  INTEGER, PARAMETER :: GVP_W    = 5
  INTEGER, PARAMETER :: GVP_T    = 6
  INTEGER, PARAMETER :: GVP_H    = 7
  INTEGER, PARAMETER :: GVP_P    = 8
  INTEGER, PARAMETER :: GVP_UUL  = 9
  INTEGER, PARAMETER :: GVP_VVL  = 10
  INTEGER, PARAMETER :: GVP_UVL  = 11
  INTEGER, PARAMETER :: GVP_SHL  = 12
  INTEGER, PARAMETER :: GVP_TKE  = 17
  INTEGER, PARAMETER :: GVP_TERRAIN = 20
  INTEGER, PARAMETER :: GVP_ZRUF    = 21
  INTEGER, PARAMETER :: GVP_HCNP    = 22
  INTEGER, PARAMETER :: GVP_ALPHA   = 23
  INTEGER, PARAMETER :: GVP_ALBEDO  = 24
  INTEGER, PARAMETER :: GVP_BOWEN   = 25
  INTEGER, PARAMETER :: GVP_ZI      = 26
  INTEGER, PARAMETER :: GVP_HFLX    = 27
  INTEGER, PARAMETER :: GVP_CC      = 28
  INTEGER, PARAMETER :: GVP_ZRUFT   = 29
  INTEGER, PARAMETER :: GVP_PRATE   = 30
  INTEGER, PARAMETER :: GVP_LANDUSE = 31
  INTEGER, PARAMETER :: GVP_Z       = 32
  INTEGER, PARAMETER :: GVP_UST     = 33
  INTEGER, PARAMETER :: GVP_QCLD   = 36
  INTEGER, PARAMETER :: GVP_ACCPR = 45 !Accumulated precip.
  INTEGER, PARAMETER :: GVP_LAI = 46 !Leaf area index

!------ Obs type bits

  INTEGER, PARAMETER :: OTB_SRF  = 0
  INTEGER, PARAMETER :: OTB_PRF  = 1

  INTEGER, PARAMETER :: OTB_FIXED = 2
  INTEGER, PARAMETER :: OTB_FCST  = 3
  INTEGER, PARAMETER :: OTB_ANLY  = 4
  INTEGER, PARAMETER :: OTB_CLI   = 5
  INTEGER, PARAMETER :: OTB_T    = 6
  INTEGER, PARAMETER :: OTB_H    = 7
  INTEGER, PARAMETER :: OTB_P    = 8
  INTEGER, PARAMETER :: OTB_ZI   = 9
  INTEGER, PARAMETER :: OTB_HFLX = 10
  INTEGER, PARAMETER :: OTB_MOL  = 11
  INTEGER, PARAMETER :: OTB_PRCP = 12
  INTEGER, PARAMETER :: OTB_CC   = 13
  INTEGER, PARAMETER :: OTB_BLV  = 14
  INTEGER, PARAMETER :: OTB_XY   = 15
  INTEGER, PARAMETER :: OTB_LL   = 16
  INTEGER, PARAMETER :: OTB_UV   = 17
  INTEGER, PARAMETER :: OTB_SPD  = 18
  INTEGER, PARAMETER :: OTB_TIME = 19
  INTEGER, PARAMETER :: OTB_YMD  = 20
  INTEGER, PARAMETER :: OTB_YR   = 21
  INTEGER, PARAMETER :: OTB_JDAY = 22
  INTEGER, PARAMETER :: OTB_LSV  = 25
  INTEGER, PARAMETER :: OTB_LSVL = 26
  INTEGER, PARAMETER :: OTB_UU   = 27
  INTEGER, PARAMETER :: OTB_TCNV = 28
  INTEGER, PARAMETER :: OTB_HCNV = 29
  INTEGER, PARAMETER :: OTB_Z    = 30
  INTEGER, PARAMETER :: OTB_RIFL = 31
  INTEGER, PARAMETER :: OTB_UST  = 32
  INTEGER, PARAMETER :: OTB_ZRUF = 33
  INTEGER, PARAMETER :: OTB_HCNP = 34
  INTEGER, PARAMETER :: OTB_ACNP = 35
  INTEGER, PARAMETER :: OTB_ALPH = 36
  INTEGER, PARAMETER :: OTB_QCLD  = 37
  INTEGER, PARAMETER :: OTB_QCNV  = 38
  INTEGER, PARAMETER :: OTB_PRATE = 39
  INTEGER, PARAMETER :: OTB_TSTA  = 40
  INTEGER, PARAMETER :: OTB_LAI   = 41

!------ Recognized Obs variables

  INTEGER, PARAMETER :: OVP_NONE = 0
  INTEGER, PARAMETER :: OVP_ID   = 1
  INTEGER, PARAMETER :: OVP_TIME = 2
  INTEGER, PARAMETER :: OVP_YR   = 3
  INTEGER, PARAMETER :: OVP_MO   = 4
  INTEGER, PARAMETER :: OVP_DAY  = 5
  INTEGER, PARAMETER :: OVP_HR   = 6
  INTEGER, PARAMETER :: OVP_YMD  = 7
  INTEGER, PARAMETER :: OVP_JDAY = 8
  INTEGER, PARAMETER :: OVP_X    = 9
  INTEGER, PARAMETER :: OVP_Y    = 10
  INTEGER, PARAMETER :: OVP_LAT  = 11
  INTEGER, PARAMETER :: OVP_LON  = 12
  INTEGER, PARAMETER :: OVP_Z    = 13
  INTEGER, PARAMETER :: OVP_U    = 14
  INTEGER, PARAMETER :: OVP_V    = 15
  INTEGER, PARAMETER :: OVP_SPD  = 16
  INTEGER, PARAMETER :: OVP_DIR  = 17
  INTEGER, PARAMETER :: OVP_T    = 18
  INTEGER, PARAMETER :: OVP_P    = 19
  INTEGER, PARAMETER :: OVP_H    = 20
  INTEGER, PARAMETER :: OVP_UL   = 28
  INTEGER, PARAMETER :: OVP_VL   = 29
  INTEGER, PARAMETER :: OVP_UUL  = 30
  INTEGER, PARAMETER :: OVP_VVL  = 31
  INTEGER, PARAMETER :: OVP_UVL  = 32
  INTEGER, PARAMETER :: OVP_SHL  = 33
  INTEGER, PARAMETER :: OVP_UU   = 34
  INTEGER, PARAMETER :: OVP_VV   = 35
  INTEGER, PARAMETER :: OVP_WW   = 36
  INTEGER, PARAMETER :: OVP_WT   = 37
  INTEGER, PARAMETER :: OVP_SL   = 38
  INTEGER, PARAMETER :: OVP_SZ   = 39
  INTEGER, PARAMETER :: OVP_ZI   = 40
  INTEGER, PARAMETER :: OVP_HFLX = 41
  INTEGER, PARAMETER :: OVP_PGT  = 42
  INTEGER, PARAMETER :: OVP_MOL  = 43
  INTEGER, PARAMETER :: OVP_CC   = 44
  INTEGER, PARAMETER :: OVP_PRCP = 45
  INTEGER, PARAMETER :: OVP_FPC0 = 46
  INTEGER, PARAMETER :: OVP_FPC1 = 47
  INTEGER, PARAMETER :: OVP_FPC2 = 48
  INTEGER, PARAMETER :: OVP_FPC3 = 49
  INTEGER, PARAMETER :: OVP_FPC4 = 50
  INTEGER, PARAMETER :: OVP_FPC5 = 51
  INTEGER, PARAMETER :: OVP_FPC6 = 52
  INTEGER, PARAMETER :: OVP_RIFL = 53
  INTEGER, PARAMETER :: OVP_UST  = 55
  INTEGER, PARAMETER :: OVP_ZRUF = 56
  INTEGER, PARAMETER :: OVP_HCNP = 57
  INTEGER, PARAMETER :: OVP_ACNP = 58
  INTEGER, PARAMETER :: OVP_ALPH = 59
  INTEGER, PARAMETER :: OVP_QCLD = 60
  INTEGER, PARAMETER :: OVP_LAI  = 70

!------ Obs time parameters

  INTEGER, PARAMETER :: OBP_NODATE = 0
  INTEGER, PARAMETER :: OBP_YJDAY  = 1
  INTEGER, PARAMETER :: OBP_YDATE  = 2
  INTEGER, PARAMETER :: OBP_YMD    = 3
  INTEGER, PARAMETER :: OBP_JDAY   = 4

!------ Obs temperature parameters

  INTEGER, PARAMETER :: OBP_CELSIUS    = 0
  INTEGER, PARAMETER :: OBP_KELVIN     = 1
  INTEGER, PARAMETER :: OBP_POTENTIAL  = 2
  INTEGER, PARAMETER :: OBP_FAHRENHEIT = 3

!------ Bad/missing data parameters

  REAL,         PARAMETER :: OBP_NODATA    = -1.E+36
  REAL(8),      PARAMETER :: OBP_BADDATA   = -1.D+36
  CHARACTER(7), PARAMETER :: OBP_BADSTRING = '-1.D+36'

!------ Obs interpolation type bits

  INTEGER, PARAMETER :: OIB_UV      = 0
  INTEGER, PARAMETER :: OIB_LSV     = 1
  INTEGER, PARAMETER :: OIB_UU      = 3
  INTEGER, PARAMETER :: OIB_STAGGER = 4
  INTEGER, PARAMETER :: OIB_RIFL    = 5

!------ Temperature interpolation parameters

  REAL, PARAMETER :: ZLIM1 = 200.
  REAL, PARAMETER :: ZLIM2 = 500.

!------ US Standard Atmosphere temperature output parameters

  INTEGER, PARAMETER :: SA_TPOT = 0  !Potential
  INTEGER, PARAMETER :: SA_TA   = 1  !Actual

!------ 3D climatology parameters

  INTEGER, PARAMETER :: I3DC_ANLY = 0
  INTEGER, PARAMETER :: I3DC_SUMM = 1

  REAL,    PARAMETER :: RH_MISS = 0.3

  INTEGER, PARAMETER :: MAX3DC = 300
  INTEGER, PARAMETER :: MAXV   = 7

  INTEGER, PARAMETER :: NRECL_A = 18 !Record length - Analysis file
  INTEGER, PARAMETER :: NRECZ_A = 10 !No. of records for zlev
  INTEGER, PARAMETER :: NRECH_A = 3  !No. of header records per pt.

  INTEGER, PARAMETER :: NRECL_S = 30 !Record length - Summarized file
  INTEGER, PARAMETER :: NRECZ_S = 6  !No. of records for zlev
  INTEGER, PARAMETER :: NRECH_S = 6  !No. of header records per pt.

!------ Met grid latitude limit parameter

  REAL, PARAMETER :: POLARCAP_LAT = 86.0      !Latitude limit for polar cap
  REAL, PARAMETER :: POLARLIM_LAT = 89.0      !Latitude limit error check

!------ Max BL depth parameters

  REAL, PARAMETER :: MAXZI_HILAT  = 1000., HILAT  = 80.
  REAL, PARAMETER :: MAXZI_MIDLAT = 3000., MIDLAT = 40.
  REAL, PARAMETER :: DZIDLAT = (MAXZI_HILAT-MAXZI_MIDLAT)/(HILAT-MIDLAT)

!------ Limits on met input
!       N.B. Checks made only for heights below ZMAX_CHECK

  REAL, PARAMETER :: ZMAX_CHECK  = 30.E3
  REAL, PARAMETER :: MAXLIM_VEL  = 300.
  REAL, PARAMETER :: MAXLIM_VEL2 = MAXLIM_VEL*MAXLIM_VEL
  REAL, PARAMETER :: MINLIM_TEMP = 50.
  REAL, PARAMETER :: MAXLIM_TEMP = 400.
  REAL, PARAMETER :: MAXLIM_TPOT = 4.*MAXLIM_TEMP

!------ MCWIF parameters

  INTEGER, PARAMETER :: MCWIF_NOADJ = 0  !No mass-consistent adjustment required
  INTEGER, PARAMETER :: MCWIF_FFT   = 1  !Use FFT method
  INTEGER, PARAMETER :: MCWIF_RELAX = 2  !Use point relaxation method
  INTEGER, PARAMETER :: MCWIF_WZERO = 3  !Grid too anisotropic. Set W=0

!------ Met request type bits

  INTEGER, PARAMETER :: RTB_AGL    = 0  !Request height is above ground level
  INTEGER, PARAMETER :: RTB_FLD    = 2  !Use specified met field
  INTEGER, PARAMETER :: RTB_SMOOTH = 3  !Add smooth field, if appropriate
  INTEGER, PARAMETER :: RTB_ADDMET = 4  !New met field has been added

  INTEGER, PARAMETER :: DIM_X    = 1      !netCDF  parameters
  INTEGER, PARAMETER :: DIM_Y    = 2
  INTEGER, PARAMETER :: DIM_Z    = 3
  INTEGER, PARAMETER :: DIM_TIME = 4

  INTEGER, PARAMETER :: LU_NONE       = 0
  INTEGER, PARAMETER :: LU_NLCD40     = 1
  INTEGER, PARAMETER :: LU_USGS24     = 2
  INTEGER, PARAMETER :: LU_USGS33     = 3
  INTEGER, PARAMETER :: LU_MODIS      = 4
  INTEGER, PARAMETER :: LU_MODIS_NOAH = 5

!----- Old (pre-WRF3.8) default USGS roughness (m) (as defined in SWIM)

  REAL, DIMENSION(24) :: ROUGHNESS_USGS24 = (/ &
    0.80, &
    0.05, &
    0.02, &
    0.05, &
    0.05, &
    0.20, &
    0.10, &
    0.01, &
    0.01, &
    0.15, &
    0.50, &
    0.50, &
    0.50, &
    0.50, &
    0.20, &
    1E-4, &
    0.20, &
    0.40, &
    0.01, &
    0.10, &
    0.30, &
    0.15, &
    0.05, &
    0.05 /)

!----- USGS Summer/winter surface roughness (cm) from WRFv3.5, see run/LANDUSE.TBL.
!      Taken from MMIF 3.4.1

  REAL, DIMENSION(33,2) :: ROUGHNESS_USGS33 =  RESHAPE( (/ &
                                 80.0,  15.0,  10.0,  15.0,  14.0,  20.0,   &
                                 12.0,   5.0,   6.0,  15.0,  50.0,  50.0,   &
                                 50.0,  50.0,  50.0,   0.1,  20.0,  40.0,   &
                                  1.0,  10.0,  30.0,  15.0,  10.0,   5.0,   &
                                  1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0, &
                                 80.0,   5.0,   2.0,   5.0,   5.0,  20.0,   &
                                 10.0,   1.0,   1.0,  15.0,  50.0,  50.0,   &
                                 50.0,  50.0,  20.0,   0.1,  20.0,  40.0,   &
                                  1.0,  10.0,  30.0,  15.0,   5.0,   5.0,   &
                                  1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /), (/ 33,2 /) )


!----- NLCD Summer/winter surface roughness (cm) from WRFv3.5, see run/LANDUSE.TBL
!      Taken from MMIF 3.4.1

  REAL, DIMENSION(40,2) :: ROUGHNESS_NLCD40 =  RESHAPE( (/ &
                                100.0, 100.0, 100.0, 100.0, 100.0,  15.0,   &
                                 15.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
                                 80.0,  30.0,   1.2,   5.0,  0.01,  0.01,   &
                                 0.01,  0.01,  0.01,   1.2,  30.0,  40.0,   &
                                 60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
                                 10.0,  15.0,   7.0,   7.0,   5.0,   5.0,   &
                                 7.0,   10.0,  55.0,  11.0, &
                                100.0, 100.0, 100.0, 100.0, 100.0,  15.0,   &
                                 15.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
                                 80.0,  30.0,   1.2,   5.0,  0.01,  0.01,   &
                                 0.01,  0.01,  0.01,   1.2,  30.0,  40.0,   &
                                 60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
                                 10.0,  15.0,   7.0,   7.0,   5.0,   5.0,   &
                                 7.0,   10.0,  55.0,  11.0 /), (/ 40,2 /) )

!----- MODIS Summer/winter surface roughness (cm) from WRFv3.5, see run/LANDUSE.TBL
!      Taken from MMIF 3.4.1

  REAL, DIMENSION(33,2) :: ROUGHNESS_MODIS =  RESHAPE( (/ &
                                 50.0,  50.0,  50.0,  50.0,  50.0,  10.0,   &
                                 10.0,  15.0,  15.0,   7.5,  30.0,   7.5,   &
                                 50.0,   6.5,   1.0,   6.5,  0.01,  15.0,   &
                                 10.0,   6.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0, &
                                 50.0,  50.0,  50.0,  50.0,  50.0,  10.0,   &
                                 10.0,  15.0,  15.0,   7.5,  30.0,   7.5,   &
                                 50.0,   6.5,   1.0,   6.5,  0.01,  15.0,   &
                                 10.0,   6.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /), (/ 33,2 /) )

!-----IGBP_MODIS Summer/winter surface roughness (cm) from WRFv3.5, see run/LANDUSE.TBL
!      Taken from MMIF 3.4.1

  REAL, DIMENSION(33,2) :: ROUGHNESS_IGBP_MODIS =  RESHAPE( (/ &
                                 50.0,  50.0,  50.0,  50.0,  50.0,   5.0,   &
                                  6.0,   5.0,  15.0,  12.0,  30.0,  15.0,   &
                                 80.0,  14.0,   0.1,   0.1,  0.01,  30.0,   &
                                 15.0,  10.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0, &
                                 50.0,  50.0,  50.0,  50.0,  20.0,   1.0,   &
                                  1.0,   1.0,  15.0,  10.0,  30.0,   5.0,   &
                                 80.0,   5.0,   0.1,   1.0,  0.01,  30.0,   &
                                 15.0,   5.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /), (/ 33,2 /) )

!----- Mapping from WRF landuse categories to SCIPUFF categories
!      Based on WRF file runs/landuse.tbl

!----- USGS 24 categories
!      (First 24 of 33 USGS categories in landuse.tbl)

  INTEGER, DIMENSION(24), PARAMETER :: cat_USGS24 = (/ &
    1, 2, 3, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 /)

!----- USGS 33 categories

  INTEGER, DIMENSION(33), PARAMETER :: cat_USGS33 = (/ &
    1, 2, 3, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, &
    19, 19,19, 26, 26, 26, 25, 1, 1 /)

!----- NLCD 40 categories

  INTEGER, DIMENSION(40), PARAMETER :: cat_NCLD40 = (/ &
    14, 13, 12, 11, 15, &
     8,  8, 10, 10,  7, &
    18,  2,  1,  5, 24, &
    19, 16, 28, 28, 28, &
    16, 24, 25, 25,1001,&
     1, 19, 11, 13, 15, &
     7,  9,  9,  9, 19, &
    19,  2,  3, 18, 20 /)

!----- IGBP_MODIS 33 categories

  INTEGER, DIMENSION(33), PARAMETER :: cat_IGBP_MODIS = (/ &
    14, 13, 12, 11, 15, &
     8,  8, 10, 10,  7, &
    18,  2,  1,  5, 24, &
    19, 16, 21, 22, 23, &
    26, 26, 26, 26, 26, &
    26, 26, 26, 26, 26, &
    25,  1,  1 /)

!----- MODIS 33 categories

  INTEGER, DIMENSION(33), PARAMETER :: cat_MODIS = (/ &
    14, 13, 12, 11, 15, &
     8,  8, 10, 10,  7, &
    18,  2,  1,  5, 24, &
    19, 16, 21, 22, 23, &
    16, 26, 26, 26, 26, &
    26, 26, 26, 26, 26, &
    25,  1,  1 /)

END MODULE SWIMparam_fd

