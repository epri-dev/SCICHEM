
MODULE simple_fi

  USE simple_fd
  USE combine_fd
  USE DefSize_fd

  CHARACTER(PATH_MAXLENGTH) primaryFile, secondaryFile, processedFile, backupFile

  INTEGER maxCells
  INTEGER nCombine
  INTEGER Pid, Sid, Fid
  LOGICAL doCombine
  LOGICAL doDep
  LOGICAL doFilter
  LOGICAL doMaximize
  REAL    eps
  REAL    filterTime
  REAL    pExceed

  REAL, DIMENSION(:), ALLOCATABLE :: filterTimes

  TYPE( CombineRecord ), DIMENSION(:), ALLOCATABLE :: combineRec
  TYPE( CombineBlock  ), DIMENSION(:), ALLOCATABLE :: combineBlk

  TYPE ( timeStruct ), TARGET :: primaryTimes
  TYPE ( timeStruct ), TARGET :: secondaryTimes
  TYPE ( typStruct  ), TARGET :: primaryTypes
  TYPE ( typStruct  ), TARGET :: secondaryTypes

END MODULE simple_fi
