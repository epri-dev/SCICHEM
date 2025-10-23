MODULE AdjointFilter_fi

  IMPLICIT NONE
  SAVE

  INTEGER, PARAMETER                   :: uHIT = 1, uNULL = 3, uNBR = 2

  INTEGER                              :: nhit_target       ! Target number of hits, default is 30
  INTEGER                              :: itsta, itend
  INTEGER                              :: p1, p2, irel
  INTEGER                              :: nsmp, nedge, ntri, nsrc
  INTEGER                              :: nt, ntsmp, nrd, nrm
  REAL                                 :: dx, dy
  LOGICAL                              :: UseMax
  INTEGER, DIMENSION(:), ALLOCATABLE   :: indx, irt
  REAL,    DIMENSION(:), ALLOCATABLE   :: fca, fch, fs

  INTEGER, DIMENSION(:,:), ALLOCATABLE :: indr

  TYPE  releaseP
    SEQUENCE
    INTEGER is
    INTEGER it
    INTEGER imat
    INTEGER icls
    LOGICAL linc
  END TYPE

  TYPE ker_str
    SEQUENCE
    INTEGER it, id
    REAL    dc, ks, kc
  END TYPE

  TYPE ker_ptr
    SEQUENCE
    TYPE( ker_str ), POINTER :: p
  END TYPE

  TYPE edge_str
    SEQUENCE
    INTEGER id
    INTEGER con
    REAL    dx, dy
  END TYPE edge_str

  TYPE sloc_str
    SEQUENCE
    INTEGER                                   :: id
    INTEGER                                   :: nbr, nedge
    INTEGER, DIMENSION(:), POINTER            :: nid
    REAL                                      :: x, y, z
    REAL                                      :: xd, yd, zd
    TYPE( edge_str ), DIMENSION(:), POINTER   :: edge
  END TYPE sloc_str

  TYPE stim_str
    SEQUENCE
    INTEGER                                   :: nt
    REAL                                      :: t, td
    REAL, DIMENSION(:), POINTER               :: dt
  END TYPE stim_str

! iu status of the sampler: null(uNULL), hit(uHIT), null neighbour for hits(uNBR), reject (< 0)
  TYPE dat_str
    SEQUENCE
    INTEGER                                   :: it, id, iu
    REAL                                      :: c, cd
    TYPE( ker_ptr ), DIMENSION(:,:), POINTER  :: ker
  END TYPE dat_str

  TYPE dat_ptr
    SEQUENCE
    TYPE( dat_str ), POINTER :: p
  END TYPE dat_ptr

  TYPE( releaseP ), DIMENSION(:), ALLOCATABLE :: relp

  TYPE( stim_str ), DIMENSION(:), ALLOCATABLE            :: tim
  TYPE( ker_str  ), DIMENSION(:), ALLOCATABLE, TARGET    :: ker

  TYPE( dat_str  ), DIMENSION(:,:), ALLOCATABLE          :: sall
  TYPE( dat_str  ), DIMENSION(:,:), ALLOCATABLE, TARGET  :: wrka

END MODULE AdjointFilter_fi

