MODULE Zsort

  TYPE SortNode
    REAL                   :: z
    INTEGER                :: n
    TYPE( SortNode ), POINTER :: Greater, Lesser
  END TYPE SortNode

  TYPE Ordered
    REAL                   :: z
    INTEGER                :: n
  END TYPE Ordered

END MODULE Zsort

