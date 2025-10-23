MODULE plotlist_fi

  USE plotlist_fd

  SAVE

  INTEGER nPcat, nPclass, nPchoice, nPkind
  INTEGER nPclassT, nPchoiceT, nPkindT
  INTEGER nPclassEff,nPchoiceEff,nPkindEff
  INTEGER adj_kind
  INTEGER nPchoiceMC
  INTEGER srfDepClass, srfDosClass

  INTEGER, ALLOCATABLE, DIMENSION(:) :: is_kind
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ClassID
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ClassOrder
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ChoiceOrder
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ClassInterp
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: ChoiceMCID
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: is_kindMC
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: n_kindMC

  CHARACTER(64), ALLOCATABLE, DIMENSION(:) :: ClassString
  CHARACTER(64), ALLOCATABLE, DIMENSION(:) :: ChoiceString
  CHARACTER(64), ALLOCATABLE, DIMENSION(:) :: KindString

  TYPE( SCIPCategoryClassT ), ALLOCATABLE, DIMENSION(:,:) :: CatClassComb
  TYPE( SCIPClassChoiceT   ), ALLOCATABLE, DIMENSION(:,:) :: ClassChoiceComb

  INTEGER nPuffTime
  INTEGER nSrfTime
  INTEGER nRadTime
  INTEGER nMetTime

  LOGICAL hasPlotTimes(5) ! Indexed to timeID + 1

  INTEGER nRow
  INTEGER nCol
  INTEGER nTable

  LOGICAL MemoryField
  LOGICAL UsePlotCN2

  REAL    CasualtyCutOff

  TYPE( SCIPTimeT ), ALLOCATABLE, DIMENSION(:), TARGET :: PuffTime, SrfTime, MetTime

END MODULE plotlist_fi
