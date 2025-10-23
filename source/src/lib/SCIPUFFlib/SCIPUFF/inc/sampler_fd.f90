
! Line-of-sight structure

MODULE los_fd

  TYPE los_str
    SEQUENCE
    REAL x, y, z          !starting location
    REAL lx, ly, lz       !direction cosines
    REAL r                !length (1.e+36 --> infinite)
  END TYPE los_str

END MODULE los_fd

!------ Sampler definitions

MODULE sampler_fd

 USE los_fd

  INTEGER, PARAMETER :: IS_CONC = 1
  INTEGER, PARAMETER :: IS_CORR = 4
  INTEGER, PARAMETER :: IS_DEP  = 5

  INTEGER, PARAMETER :: STB_AGL       = 0
  INTEGER, PARAMETER :: STB_MOVING    = 1
  INTEGER, PARAMETER :: STB_LOS       = 2
  INTEGER, PARAMETER :: STB_INTEGRATE = 3
  INTEGER, PARAMETER :: STB_MET       = 5
  INTEGER, PARAMETER :: STB_TURB      = 6
  INTEGER, PARAMETER :: STB_ENSM      = 7
  INTEGER, PARAMETER :: STB_TOTALVAR  = 8
  INTEGER, PARAMETER :: STB_OUTLOC    = 9  !For moving sensor since STB_MOVING is cleared after last waypt
  INTEGER, PARAMETER :: STB_CORR      = 10
  INTEGER, PARAMETER :: STB_WETP      = 13
  INTEGER, PARAMETER :: STB_PART      = 14
  INTEGER, PARAMETER :: STB_MULT      = 15
  INTEGER, PARAMETER :: STB_DEP       = 16
  INTEGER, PARAMETER :: STB_NODECAY   = 17
  INTEGER, PARAMETER :: STB_FXGRID    = 18
  INTEGER, PARAMETER :: STB_AUTOGRID  = 19
  INTEGER, PARAMETER :: STB_INTGRID   = 20

  INTEGER, PARAMETER :: SOB_INTONLY   = 0
  INTEGER, PARAMETER :: SOB_LARGEDELT = 1
  INTEGER, PARAMETER :: SOB_NOPUFF    = 2

  TYPE WayPoint
    SEQUENCE
    REAL :: time                      !Time in hours
    REAL :: x, y, z                   !Location in project coord.
    REAL :: az, el, dist              !LOS orientation
    TYPE( WayPoint ), POINTER :: next, prev
  END TYPE WayPoint

  TYPE SampTime
    SEQUENCE
    REAL :: tStart                  !Time to start new period (since start of run)
    REAL :: dtSamp                  !Increment time (sec)
    TYPE( SampTime ), POINTER :: next
  END TYPE SampTime

  TYPE sensor
    SEQUENCE
    REAL          :: time
    REAL          :: x,  y,  z
    REAL          :: az, el, dist
    REAL          :: lx, ly, lz
    REAL          :: h, zh
    REAL          :: dx, dy, rot, autoLev
    INTEGER       :: nx, ny, npts
    REAL          :: x0, y0                  !Coordinates used in input file
    REAL          :: dxs, dys
    CHARACTER(64) :: type
    CHARACTER(500):: var
    CHARACTER(64) :: name
    INTEGER       :: is, ie
    INTEGER       :: nvar                    !No. of sensor outputs
    INTEGER       :: stype
    INTEGER       :: nbin
    REAL          :: conv
    REAL          :: csum
    INTEGER       :: nmc
    INTEGER, DIMENSION(:), POINTER :: mcID
    TYPE( WayPoint ),      POINTER :: nextpoint
    REAL                           :: speed, tprev
    TYPE( los_str )                :: mvlos
    REAL,    DIMENSION(:), POINTER :: dsmp            !Sensor output
    REAL,    DIMENSION(:), POINTER :: asmp            !Sensor output for ambient values
    REAL,    DIMENSION(:), POINTER :: bin

    REAL,      DIMENSION(:,:,:), POINTER :: gsmp       !Gridded output
    CHARACTER(64), DIMENSION(:), POINTER :: FieldName  !SensorData description strings
    CHARACTER(64), DIMENSION(:), POINTER :: FieldUnits !for gridded output
    CHARACTER(64), DIMENSION(:), POINTER :: SizeBin

  END TYPE sensor

  TYPE SampClassT
    SEQUENCE
    CHARACTER(64)  :: type
    CHARACTER(500) :: var
    TYPE( SampClassT ), POINTER :: next
  END TYPE SampClassT

END MODULE sampler_fd
