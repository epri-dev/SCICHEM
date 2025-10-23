MODULE cont_rel_fd
  USE puffstruct_fd
  USE param_fd
  USE release_fd

  IMPLICIT NONE

  INTEGER, PARAMETER :: CRMODE_START   = 0
  INTEGER, PARAMETER :: CRMODE_RESTART = 1
  INTEGER, PARAMETER :: CRMODE_UPDATE  = 2

  INTEGER, PARAMETER :: POS_IDDEF  =  0   !Start of release definition ID
  INTEGER, PARAMETER :: LEN_IDDEF  = 16   !Length of release definition ID : Max value = 65535

  INTEGER, PARAMETER :: POS_IDSET  = 16   !Start of release set ID
  INTEGER, PARAMETER :: LEN_IDSET  = 10   !Length of release set ID : Max value = 1023

  INTEGER, PARAMETER :: POS_IDPUFF = 26   !Start of release puff ID
  INTEGER, PARAMETER :: LEN_IDPUFF =  4   !Length of release puff ID : Max value = 15

  INTEGER, PARAMETER :: CR_ACTIVE   =  1
  INTEGER, PARAMETER :: CR_READY    =  0
  INTEGER, PARAMETER :: CR_EMPTY    = -1

  TYPE cont_release_id
    INTEGER                    :: idef
    INTEGER                    :: irel
    INTEGER                    :: ipuf
  END TYPE cont_release_id

  TYPE cont_release_triad_R8
    REAL(8)                    :: x
    REAL(8)                    :: y
    REAL(8)                    :: z
  END TYPE cont_release_triad_R8

  TYPE cont_release_triad_R4
    REAL                       :: x
    REAL                       :: y
    REAL                       :: z
  END TYPE cont_release_triad_R4

  TYPE static_puff
    TYPE( cont_release_id )       :: rid   !def/rel/puff triad
    INTEGER                       :: ipuf  !puff number, Index into puff array
    TYPE( cont_release_triad_R4 ) :: vel   !velocity triad
    REAL                          :: qqx   !qqb*dt0*dt0
  END TYPE static_puff

  TYPE static_release
    TYPE( static_puff )         :: sp
    TYPE( static_puff )         :: vp
    LOGICAL                     :: liquid
  END TYPE static_release

  TYPE static_release_list
    TYPE( static_release ),      POINTER :: stat
    TYPE( static_release_list ), POINTER :: next
    TYPE( static_release_list ), POINTER :: prev
  END TYPE static_release_list

  TYPE cont_release_static
    REAL                       :: del
    REAL                       :: dur
    REAL                       :: step
    REAL                       :: tstat
    INTEGER                    :: ilev        !Initial time level
    INTEGER                    :: tlev        !Stepping time level
    LOGICAL                    :: doStatics
    LOGICAL                    :: hasStatics
  END TYPE cont_release_static

  TYPE cont_release_mass
    INTEGER                    :: num
    INTEGER, DIMENSION(MAXSGP) :: ityp
    REAL,    DIMENSION(MAXSGP) :: mass
  END TYPE cont_release_mass

  TYPE cont_release_met
    LOGICAL                 :: init
    LOGICAL                 :: doSrf
    INTEGER                 :: ifld
    REAL                    :: zbar
    REAL                    :: szz
    REAL                    :: shh
  END TYPE cont_release_met

  TYPE cont_release_rel
    REAL                                    :: time
    REAL                                    :: plen
    REAL                                    :: fracUP
    INTEGER                                 :: naux
    INTEGER                                 :: nPuff
    TYPE( cont_release_triad_R8 )           :: loc
    TYPE( puff_str )                        :: basePuff
    LOGICAL                                 :: isStatic
    REAL,             DIMENSION(:), POINTER :: saux
    TYPE( puff_str ), DIMENSION(:), POINTER :: relPuff
    TYPE( puff_str ), DIMENSION(:), POINTER :: vapPuff
  END TYPE cont_release_rel

  TYPE cont_release_set
    INTEGER                                         :: tlev
    INTEGER                                         :: nrel
    TYPE( cont_release_rel ), DIMENSION(:), POINTER :: rels
  END TYPE cont_release_set

  TYPE cont_release_def
    INTEGER                                          :: ID    !Definition ID (needed for statics)
    INTEGER                                          :: cID   !Collection ID
    INTEGER                                          :: mID   !Material ID
    INTEGER                                          :: state
    LOGICAL                                          :: isPool
    LOGICAL                                          :: isDynamic
    LOGICAL                                          :: isMoving
    REAL                                             :: time
    REAL                                             :: end
    REAL                                             :: dur
    REAL                                             :: dtr
    TYPE( cont_release_triad_R8 )                    :: loc
    TYPE( cont_release_triad_R4 )                    :: vel
    TYPE( cont_release_set )                         :: rSet
    TYPE( cont_release_def ),                POINTER :: nextDef
    TYPE( cont_release_def ),                POINTER :: prevDef
    LOGICAL                                          :: update
    LOGICAL                                          :: extraUpdate  !nextUpdtTime (if defined) /= DEF_VAL_R
    TYPE( releaseSpecT )                             :: relSpec
  END TYPE cont_release_def

  TYPE cont_release_col
    LOGICAL                                          :: isActive
    LOGICAL                                          :: isPool
    LOGICAL                                          :: isDynamic
    LOGICAL                                          :: update_cc
    TYPE( cont_release_static )                      :: rStat
    TYPE( cont_release_def ),                POINTER :: firstDef
    TYPE( cont_release_def ),                POINTER :: lastDef
  END TYPE cont_release_col

END MODULE cont_rel_fd
MODULE cont_rel_fi

  USE cont_rel_fd

  IMPLICIT NONE

  SAVE

  LOGICAL                                                    :: runUpdates
  LOGICAL                                                    :: initStatics
  INTEGER                                                    :: maxCollection
  INTEGER                                                    :: numCollection
  INTEGER                                                    :: maxDefinition
  INTEGER                                                    :: numDefinition
  TYPE( cont_release_col), DIMENSION(:), ALLOCATABLE, TARGET :: cCollection
  TYPE( cont_release_def), DIMENSION(:), ALLOCATABLE, TARGET :: cDefinition

END MODULE cont_rel_fi
