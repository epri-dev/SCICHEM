MODULE AERMODreceptor_fd

  INTEGER, PARAMETER :: MAXNUMRE = 20 !Max number of receptor networks

  TYPE DiscRE
    CHARACTER(16) :: id
    REAL          :: x, y
    REAL          :: Zelev, Zhill, Zflag
    TYPE( DiscRE), POINTER :: next
  END TYPE DiscRE

  TYPE NetRE
    CHARACTER(8) :: id
    CHARACTER(8) :: type
    CHARACTER(12):: srcid
    REAL         :: x0, y0
    INTEGER      :: nx, ny
    REAL, DIMENSION(:), POINTER :: x, y
  END TYPE NetRE

  TYPE AERMODreceptorInfo

    LOGICAL :: lAvg
    LOGICAL :: lConc
    LOGICAL :: lDepTot
    LOGICAL :: lDepDry
    LOGICAL :: lDepWet
    LOGICAL :: lFlagPole
    REAL    :: Flagdf
    REAL    :: elevCnv
    REAL    :: dtSampler

  END TYPE AERMODreceptorInfo

END MODULE AERMODreceptor_fd
