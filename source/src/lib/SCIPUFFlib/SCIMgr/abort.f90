!=======================================================================
!    abort common
!=======================================================================
MODULE abort

  SAVE

  INTEGER, PARAMETER :: MAX_CHECKCOUNT = 100

  LOGICAL lAbort

  INTEGER abortCheckCount

CONTAINS

!*******************************************************************************
!            AbortInit
!*******************************************************************************
SUBROUTINE AbortInit()

IMPLICIT NONE

lAbort = .FALSE.
abortCheckCount = 0

RETURN

END SUBROUTINE AbortInit
!*******************************************************************************
!            AbortSet
!*******************************************************************************
SUBROUTINE AbortSet( userID )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN  ) :: userID      !USER ID Tag

!==== Set abort message

nError   = AB_ERROR
eMessage ='User requested Abort of SCIPtool output detected'
eInform  ='UserID:'//TRIM(ADJUSTL(userID))
eAction  =' '
eRoutine ='SCIPAbortOutput'

!==== Set abort flag

lAbort = .TRUE.

RETURN

END SUBROUTINE AbortSet
!*******************************************************************************
!            AbortClear
!*******************************************************************************
SUBROUTINE AbortClear()

USE error_fi

IMPLICIT NONE

!==== Reset abort error if aborting and message was somehow cleared

IF( lAbort .AND. nError /= AB_ERROR )THEN
  CALL AbortSet( "AbortClear" )
END IF

CALL AbortInit()

RETURN

END SUBROUTINE AbortClear
!*******************************************************************************
!            Aborted
!*******************************************************************************
LOGICAL FUNCTION Aborted()

USE files_fi

IMPLICIT NONE

LOGICAL switch0
INTEGER irv

INTEGER, EXTERNAL  :: sysDeleteFile

!==== Checks for recent abort calls

CALL check_progress()

INQUIRE(FILE=file_abort,EXIST=switch0)
IF( switch0 )THEN
  CALL abortSet( 'Aborted' )
  irv = sysDeleteFile( file_abort )
END IF

Aborted = lAbort

RETURN

END FUNCTION Aborted
!*******************************************************************************
!            InitAbortedCount
!*******************************************************************************
SUBROUTINE InitAbortedCount()

IMPLICIT NONE

abortCheckCount = 0

RETURN

END SUBROUTINE InitAbortedCount
!*******************************************************************************
!            AbortedCount
!*******************************************************************************
LOGICAL FUNCTION AbortedCount()

IMPLICIT NONE

!$OMP ATOMIC UPDATE
abortCheckCount = abortCheckCount + 1
IF( abortCheckCount >= MAX_CHECKCOUNT )THEN
!$OMP CRITICAL (ABORT_CHECK)
  IF( abortCheckCount >= MAX_CHECKCOUNT )THEN
    abortCheckCount = 0
    AbortedCount = Aborted()
  ELSE
     AbortedCount = .FALSE.
  END IF
!$OMP END CRITICAL (ABORT_CHECK)
ELSE
  AbortedCount = .FALSE.
END IF

RETURN

END FUNCTION AbortedCount

END MODULE abort
