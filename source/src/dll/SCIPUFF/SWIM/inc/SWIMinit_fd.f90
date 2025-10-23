!------ SCIPUFF / SWIM initialization interface

MODULE SWIMinit_fd

  USE DefSize_fd
  IMPLICIT NONE

  TYPE BLinput

    SEQUENCE

    INTEGER :: type
    INTEGER :: nzbl
    REAL    :: UUcalm, SLcalm
    REAL    :: HFLXmin, HFLXmax
    REAL    :: ZImin,   ZImax
    REAL    :: WWtrop, SLtrop
    REAL    :: Atrop, Btrop
    REAL    :: zruf
    REAL    :: hc
    REAL    :: Bowen
    REAL    :: alpha
    REAL    :: albedo
    REAL    :: cc
    REAL    :: pr_type
    INTEGER :: i_wet
    INTEGER :: i_cat

  END TYPE BLinput

!==============================================================================

  TYPE MCinput

    SEQUENCE

    INTEGER                     :: type
    CHARACTER(PATH_MAXLENGTH)   :: TerFile
    CHARACTER(PATH_MAXLENGTH)   :: LandUseFile
    INTEGER                     :: MaxIterRelax
    INTEGER                     :: MaxIterFFT
    REAL                        :: alphaMax
    REAL                        :: alphaMin
    REAL                        :: epsRelax
    REAL                        :: epsFFT
    REAL                        :: dtSWIFT
    INTEGER                     :: nz
    REAL, DIMENSION(:), POINTER :: z

  END TYPE MCinput

!==============================================================================

  TYPE PrjInput

    SEQUENCE

    INTEGER :: coord
    REAL    :: Xmin, Xmax
    REAL    :: Ymin, Ymax
    REAL    :: Zmax
    REAL    :: Hres

    REAL    :: Xref, Yref
    REAL    :: Lon0, Lat0
    INTEGER :: UTMzone

    REAL    :: Hmin

    LOGICAL :: local
    REAL    :: timeZone

    REAL    :: time
    REAL    :: timeEnd

    REAL    :: hourStart
    INTEGER :: dayStart
    INTEGER :: monthStart
    INTEGER :: yearStart
    INTEGER :: julStart

    REAL    :: hourEnd
    INTEGER :: dayEnd
    INTEGER :: monthEnd
    INTEGER :: yearEnd


    INTEGER :: iUnused1
    REAL    :: rUnused1

    INTEGER :: LSVType
    REAL    :: LSVuu
    REAL    :: LSVscale
    REAL    :: epstrop

    TYPE( BLinput ) :: BL
    TYPE( MCinput ) :: MC

    INTEGER :: MAX1D_MET
    INTEGER :: maxObs

    LOGICAL :: create
    LOGICAL :: decay

    REAL    :: timeBin
    LOGICAL :: localMet

    LOGICAL :: lOut3D
    LOGICAL :: lOut2D
    LOGICAL :: lCreateOut
    LOGICAL :: lFormat

    LOGICAL :: Use_TKE

    CHARACTER(PATH_MAXLENGTH) :: OutFile
    CHARACTER(PATH_MAXLENGTH) :: prjName

  END TYPE PrjInput

!==============================================================================

  TYPE SWIMinitmet

    SEQUENCE

    INTEGER                               :: nMetSrc
    CHARACTER(PATH_MAXLENGTH), &
                    DIMENSION(:), POINTER :: MetSrc
    INTEGER,        DIMENSION(:), POINTER :: type
    TYPE( PrjInput )                      :: prj

  END TYPE SWIMinitmet

END MODULE SWIMinit_fd

