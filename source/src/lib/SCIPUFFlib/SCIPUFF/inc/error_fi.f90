!=======================================================================
!    ERROR common
!=======================================================================
MODULE error_fi

  USE errorParam_fd

  SAVE

  CHARACTER(80)  eRoutine
  CHARACTER(128) eMessage
  CHARACTER(128) eInform
  CHARACTER(128) eAction

  INTEGER nError

  ! Caution counters

  INTEGER nRelOutsideDomain, nCRelOutsideDomain
  INTEGER nRelBeforeStart
  INTEGER nStopSplit
  INTEGER nDezoneSrf
  INTEGER nPuffAboveTop
  INTEGER nPuffReflect
  INTEGER nRelInterIter

  COMMON / CommonErrorFi / eRoutine, eMessage, eInform, eAction, nError
!$OMP THREADPRIVATE(/CommonErrorFi/)

  LOGICAL multiThread, singleThread
  INTEGER nthreads

  TYPE omp_err_fi
    SEQUENCE
    INTEGER        nError
    CHARACTER(80)  eRoutine
    CHARACTER(128) eMessage
    CHARACTER(128) eInform
    CHARACTER(128) eAction
  END TYPE

  TYPE( omp_err_fi ), DIMENSION(:), ALLOCATABLE :: omp_error

END MODULE error_fi
