!=======================================================================
!    SCIPUFF definitions - run mode
!=======================================================================
MODULE mode_fd

  INTEGER, PARAMETER :: FAST_MODE = 0
  INTEGER, PARAMETER :: REVERSE_MODE   = 7   !Reverse time calculation
  INTEGER, PARAMETER :: DINCRMNT       = 12  !Dep & Dos files store increments from previous output time
  INTEGER, PARAMETER :: EVAP2D         = 14   !2 distribution surface evaporation

END MODULE mode_fd
!=======================================================================
!    SCIPUFF definitions - precipitation
!=======================================================================
MODULE precip_fd

  INTEGER, PARAMETER :: NRAIN    =           3 !Maximum Number of Rain groups
  INTEGER, PARAMETER :: NSNOW    =           3 !Maximum Number of Snow groups
  INTEGER, PARAMETER :: NWASH    = NRAIN+NSNOW !Maximum Number of Precip. groups

!-----  Define Rain Precipitation Groups (mm/hr): Rain + Snow

  REAL, DIMENSION(NRAIN+NSNOW), PARAMETER :: PWASH = (/ 0.5,3.5,25.0,5.,20.,100. /)

END MODULE precip_fd
!=======================================================================
!    SCIPUFF definitions - puff and puff auxiliary
!=======================================================================
MODULE puff_fd

  INTEGER, PARAMETER :: NP_SUM  = 10                          !Puff variables - merged by summing
  INTEGER, PARAMETER :: NP_RAT  = 10                          !Puff variables - merged by ratio
  INTEGER, PARAMETER :: NP_INT  =  7                          !Puff variables - integers
  INTEGER, PARAMETER :: NP_DBLE =  2                          !Puff variables - double precision
  INTEGER, PARAMETER :: NP_REAL = 14 + NP_SUM + NP_RAT        !Puff variables - floating point
  INTEGER, PARAMETER :: NP_ALL  = NP_DBLE + NP_REAL + NP_INT  !Puff variables - total

  INTEGER, PARAMETER :: MAXPMAUX  =    5 !Particle material parameters
  INTEGER, PARAMETER :: MAXPMAUXX =    3
  INTEGER, PARAMETER :: PMAUX_BOUNDS = 2
  INTEGER, PARAMETER :: PMAUX_MEAN   = 3
  INTEGER, PARAMETER :: MAXGMAUX =     2 !Gas material parameters
  INTEGER, PARAMETER :: MAXLMAUX =     4 !Liquid mat'l size group data
  INTEGER, PARAMETER :: MAXLMAUXP =   12 !Liquid mat'l physical constants
  INTEGER, PARAMETER :: MAXLMAUXX =    3
  INTEGER, PARAMETER :: LMAUX_BOUNDS = 2
  INTEGER, PARAMETER :: LMAUX_MEAN   = 3
  INTEGER, PARAMETER :: MAXAMAUXP =   12 !Aerosol mat'l physical constants

  INTEGER, PARAMETER :: MAXSGP = 50 !Maximum Number of Groups/Type

  INTEGER, PARAMETER :: NAUX_DYNAMICS_GAS  =  12
  INTEGER, PARAMETER :: NAUX_DYNAMICS_PART =   8
  INTEGER, PARAMETER :: NAUX_DYNAMICS_GAS_OLD  =  6
  INTEGER, PARAMETER :: NAUX_DYNAMICS_PART_OLD =  4
  INTEGER, PARAMETER :: NAUX_DENSE_PART    =  6   !Dense gas dynamic variables
  INTEGER, PARAMETER :: NAUX_DENSE_GAS     = 11   !Dense gas dynamic variables
  INTEGER, PARAMETER :: NAUX_BUOY          =  2   !Buoyant gas dynamic variables
  INTEGER, PARAMETER :: NAUX_DYNAMICS      = NAUX_DYNAMICS_GAS
  INTEGER, PARAMETER :: NAUX_DENSE         = NAUX_DENSE_GAS

  INTEGER, PARAMETER :: NAUX_TOTALCC       = 2
  INTEGER, PARAMETER :: NAUX_LIQUID        = 6
  INTEGER, PARAMETER :: NAUX_AEROSOL       = 4
  INTEGER, PARAMETER :: NAUX_STATIC        = 3

  INTEGER, PARAMETER :: I_REMOVE = -1 !Remove puff
  INTEGER, PARAMETER :: I_STATIC = -2 !Static puff

!----- The ipgd element of puff_str is used for the puff grid level,
!      the nested met field ID, and an optional release ID.
!      The following parameters give the bit
!      start positions and bit lengths for the components

  INTEGER, PARAMETER :: SKEW_NONE = 0    !Skew turbulence flag : none
  INTEGER, PARAMETER :: SKEW_DOWN = 1    !Skew turbulence flag : downdraft
  INTEGER, PARAMETER :: SKEW_UP   = 2    !Skew turbulence flag : updraft

  INTEGER, PARAMETER :: POS_IPSKEW = 0   !Start of puff skew turbulence  : 0=none
  INTEGER, PARAMETER :: LEN_IPSKEW = 2   !Length of puff skew turbulence : 1=down, 2=up

  INTEGER, PARAMETER :: POS_IPGRID = 2   !Start of puff grid level
  INTEGER, PARAMETER :: LEN_IPGRID = 6   !Length of puff grid level

  INTEGER, PARAMETER :: POS_IFIELD = 8   !Start of met field ID
  INTEGER, PARAMETER :: LEN_IFIELD = 8   !Length of met field ID

  INTEGER, PARAMETER :: POS_IDREL  = 16  !Start release ID
  INTEGER, PARAMETER :: LEN_IDREL  = 16  !Length of release ID

END MODULE puff_fd
!=======================================================================
!    SCIPUFF definitions - operational release status
!=======================================================================
MODULE opRel_fd

  INTEGER, PARAMETER :: SCIPUFF_STATUS_ARRAY_SIZE = 60
  INTEGER, PARAMETER :: OPREADY_INDEX             = 2
  INTEGER, PARAMETER :: DOMAIN_STATUS_ARRAY_SIZE  = 20
  INTEGER, PARAMETER :: TIME_STATUS_ARRAY_SIZE    = 5

END MODULE opRel_fd
!=======================================================================
!    SCIPUFF definitions - obsolete
!=======================================================================
MODULE obsolete_fd

  INTEGER, PARAMETER :: MAXCLS = 3 !Maximum Number of Classes
  INTEGER, PARAMETER :: MAXSF  = 3

END MODULE obsolete_fd
!=======================================================================
!    SCIPUFF definitions - max field sizes
!=======================================================================
MODULE DefSize_fd

  INTEGER, PARAMETER :: MAXSG_DEF  = 25000 !Maximum Number of Surface cells
  INTEGER, PARAMETER :: MAXPUF_DEF = 20000 !Maximum Number of Puffs

  INTEGER, PARAMETER :: PATH_MAXLENGTH = 256 !Maximum no of characters in a pathname

END MODULE DefSize_fd
!=======================================================================
!    SCIPUFF definitions - miscellaneous
!=======================================================================
MODULE scipuff_fd

  INTEGER, PARAMETER :: MAXTLV   = 25  !Maximum Number of Time levels
  INTEGER, PARAMETER :: SCIPUFF_ENABLE  = 0
  INTEGER, PARAMETER :: SCIPUFF_HALT    = 1
  INTEGER, PARAMETER :: SCIPUFF_STOP    = 2
  INTEGER, PARAMETER :: SCIPUFF_DISABLE = -999

  INTEGER, PARAMETER :: SCIPUFF_MAXRELPARAM  = 25
  INTEGER, PARAMETER :: SCIPUFF_POOLSRCPARAM = 207

END MODULE scipuff_fd
!=======================================================================
!    SCIPUFF definitions - All
!=======================================================================
MODULE param_fd
  USE coordinate_fd
  USE mode_fd
  USE precip_fd
  USE puff_fd
  USE obsolete_fd
  USE opRel_fd
  USE DefSize_fd
  USE scipuff_fd
END MODULE param_fd
