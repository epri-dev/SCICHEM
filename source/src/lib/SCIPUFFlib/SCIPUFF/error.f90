!***********************************************************************
!               InitError
!***********************************************************************
SUBROUTINE init_error()

USE error_fi

IMPLICIT NONE

INTEGER i

nError = NO_ERROR

eMessage = CHAR(0)
eInform  = CHAR(0)
eAction  = CHAR(0)
eRoutine = CHAR(0)

IF( ALLOCATED(omp_error) )THEN
  DO i = 1,nthreads
    omp_error(i)%nError   = nError
    omp_error(i)%eRoutine = eRoutine
    omp_error(i)%eMessage = eMessage
    omp_error(i)%eInform  = eInform
    omp_error(i)%eAction  = eAction
  END DO
END IF

RETURN
END
!***********************************************************************
!               InitCaution
!***********************************************************************
SUBROUTINE initCaution()

USE error_fi

IMPLICIT NONE

nRelOutsideDomain  = 0
nCRelOutsideDomain = 0
nRelBeforeStart    = 0
nStopSplit         = 0
nDezoneSrf         = 0
nPuffAboveTop      = 0
nPuffReflect       = 0
nRelInterIter      = 0

RETURN
  END
!***********************************************************************
!               InitOMPError
!***********************************************************************
SUBROUTINE InitOMPError()

USE error_fi

IMPLICIT NONE

INTEGER alloc_stat, i

IF( .NOT.ALLOCATED(omp_error) )THEN
  ALLOCATE( omp_error(nthreads),STAT=alloc_stat )
  IF( alloc_stat /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'InitTool'
    eMessage = 'Unable to allocate OpenMP Thread error structures'
    GOTO 9999
  END IF
END IF

DO i = 1,nthreads
  omp_error(i)%nError   = NO_ERROR
  omp_error(i)%eRoutine = ' '
  omp_error(i)%eMessage = ' '
  omp_error(i)%eInform  = ' '
  omp_error(i)%eAction  = ' '
END DO

9999 CONTINUE
RETURN
END

