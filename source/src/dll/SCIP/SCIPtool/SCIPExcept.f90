MODULE SCIPException
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INTPTR_T

  TYPE T_EXCEPTION_POINTERS
     SEQUENCE
     INTEGER(C_INTPTR_T) SIGINFO_PTR
     INTEGER(C_INTPTR_T) SIGCONTEXT_PTR
  END TYPE
END MODULE SCIPException

!*******************************************************************************
!            Fortran Traceback from C/C++
!*******************************************************************************
INTEGER(4) FUNCTION SCIPTraceException ( ExceptionNumber, ExceptionInfo )
!DEC$ ATTRIBUTES DLLEXPORT :: SCIPTraceException
!DEC$ ATTRIBUTES REFERENCE :: ExceptionInfo

USE SCIMgr_fd
USE SCIPException


INTEGER(4),                 INTENT(IN) ::  ExceptionNumber
TYPE(T_EXCEPTION_POINTERS), INTENT(IN) ::  ExceptionInfo

INTEGER(4)        exitCode, sts
CHARACTER(LEN=80) title             !Suggested maxiumum

INTEGER(LEN_ADDRESS), EXTERNAL :: ADDRESSOF
#if defined(_WIN64)

INTERFACE
  SUBROUTINE TRACEBACKQQ(STRING, USER_EXIT_CODE, STATUS, EPTR)
    USE SCIPException
    !DEC$ ATTRIBUTES DEFAULT :: TRACEBACKQQ
    !DEC$ ATTRIBUTES DECORATE, ALIAS : 'TRACEBACKQQ' :: TRACEBACKQQ
    CHARACTER*(*), INTENT(IN ), OPTIONAL :: STRING
    INTEGER*4,     INTENT(IN ), OPTIONAL :: USER_EXIT_CODE
    INTEGER*4,     INTENT(OUT), OPTIONAL :: STATUS
    OPTIONAL EPTR
    TYPE(T_EXCEPTION_POINTERS) :: EBASE
    POINTER(EPTR,EBASE)
  END SUBROUTINE
END INTERFACE
#endif

SCIPTraceException = SCIPFailure

exitCode = -1

WRITE(title,'(A,Z10)')'SCIPtool Fortran Exception Traceback for exception ',ExceptionNumber

#if defined(_WIN64)
IF( exceptionNumber /= 999 )THEN
  WRITE(*,*)'Calling TraceBackQQ with '//TRIM(title)
  CALL TRACEBACKQQ( TRIM(title), exitCode, sts, ADDRESSOF(ExceptionInfo) )
ELSE
  WRITE(*,*)'Testing TraceBackQQ with '//TRIM(title)
END IF

#endif
SCIPTraceException = SCIPSuccess

RETURN
END
