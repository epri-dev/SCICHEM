!******************************************************************************
!******************************************************************************

SUBROUTINE SetCheckErrorMessage( routine,name,message )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
CHARACTER(*), INTENT( IN ) :: message

nError = IV_ERROR
eRoutine = TRIM(routine)
eMessage = 'Invalid '//TRIM(name)//' value'
eInform  = TRIM(message)

RETURN
END

!******************************************************************************

SUBROUTINE SetMessageHandlerError( routine )

USE SCIMgr_fi
USE SCIMgrparam_fd
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine

TYPE( messageT ) error

CALL GetMessageHandlerError( error )

nError   = error%iParm
eRoutine = TRIM(routine)//':'//TRIM(error%routine)
eMessage = TRIM(error%aString)
eInform  = TRIM(error%bString)
LastError = CALLBACK_ERROR

RETURN
END

!******************************************************************************

SUBROUTINE SetCheckErrorSpecial( routine,name,mode )

USE error_fi
USE checkErr_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
INTEGER,      INTENT( IN ) :: mode

CHARACTER(40) :: string

string = ' '
IF( BTEST(mode,DEFAULT_BIT) )string = TRIM(string)//',Default'
IF( BTEST(mode,NOTSET_BIT)  )string = TRIM(string)//',NotSet'
IF( BTEST(mode,DEFER_BIT)   )string = TRIM(string)//',Deferred'

string = TRIM(string(2:))

nError = IV_ERROR
eRoutine = TRIM(routine)
eMessage = 'Invalid '//TRIM(name)//' value'
eInform  = 'must not be a special value ('//TRIM(string)//')'
IF( LEN_TRIM(name)+LEN_TRIM(eInform) < LEN(eInform) )THEN
  eInform = TRIM(name)//' '//TRIM(eInform)
END IF

RETURN
END

!******************************************************************************

SUBROUTINE SetCheckErrorReal( routine,name,min,max,value )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
REAL,         INTENT( IN ) :: min
REAL,         INTENT( IN ) :: max
REAL,         INTENT( IN ) :: value

nError   = IV_ERROR
eRoutine = TRIM(routine)
WRITE(eMessage,*)value
eMessage = TRIM(name)//' value out of range : '//ADJUSTL(eMessage)
IF( max == HUGE(max) )THEN
  IF( min == -HUGE(max) )THEN
  ELSE IF( min == TINY(min) )THEN
    WRITE(eInform,*)'must be greater than zero'
  ELSE
    WRITE(eInform,*)'must be greater than or equal to',min
  END IF
ELSE IF( min == -HUGE(min) )THEN
  IF( max == -TINY(max) )THEN
    WRITE(eInform,*)'must be less than zero'
  ELSE
    WRITE(eInform,*)'must be less than or equal to',max
  END IF
ELSE
  WRITE(eInform,*)'Min =',min,'  :  Max =',max
END IF

eInform = ADJUSTL(eInform)
IF( LEN_TRIM(name)+LEN_TRIM(eInform) < LEN(eInform) )THEN
  eInform = TRIM(name)//' '//TRIM(eInform)
END IF

RETURN
END

!******************************************************************************

SUBROUTINE SetCheckErrorDouble( routine,name,min,max,value )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
REAL(8),      INTENT( IN ) :: min
REAL(8),      INTENT( IN ) :: max
REAL(8),      INTENT( IN ) :: value

nError   = IV_ERROR
eRoutine = TRIM(routine)
WRITE(eMessage,*)value
eMessage = TRIM(name)//' value out of range : '//ADJUSTL(eMessage)
IF( max == HUGE(max) )THEN
  IF( min == -HUGE(max) )THEN
  ELSE IF( min == TINY(min) )THEN
    WRITE(eInform,*)'must be greater than zero'
  ELSE
    WRITE(eInform,*)'must be greater than or equal to',min
  END IF
ELSE IF( min == -HUGE(min) )THEN
  IF( max == -TINY(max) )THEN
    WRITE(eInform,*)'must be less than zero'
  ELSE
    WRITE(eInform,*)'must be less than or equal to',max
  END IF
ELSE
  WRITE(eInform,*)'Min =',min,'  :  Max =',max
END IF

eInform = ADJUSTL(eInform)
IF( LEN_TRIM(name)+LEN_TRIM(eInform) < LEN(eInform) )THEN
  eInform = TRIM(name)//' '//TRIM(eInform)
END IF

RETURN
END

!******************************************************************************

SUBROUTINE SetCheckErrorInteger( routine,name,min,max,value )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
INTEGER,      INTENT( IN ) :: min
INTEGER,      INTENT( IN ) :: max
INTEGER,      INTENT( IN ) :: value

nError = IV_ERROR
eRoutine = TRIM(routine)

WRITE(eMessage,*)value
eMessage = TRIM(name)//' value out of range : '//ADJUSTL(eMessage)
IF( max == HUGE(max) )THEN
  IF( min == -HUGE(min) )THEN
  ELSE IF( min == 1 )THEN
    WRITE(eInform,*)'must be greater than zero'
  ELSE
    WRITE(eInform,*)'must be greater than or equal to',min
  END IF
ELSE IF( min == -HUGE(min) )THEN
  IF( max == -1 )THEN
    WRITE(eInform,*)'must be less than zero'
  ELSE
    WRITE(eInform,*)'must be less than or equal to',max
  END IF
ELSE
  WRITE(eInform,*)'Min =',min,'  :  Max =',max
END IF

eInform = ADJUSTL(eInform)
IF( LEN_TRIM(name)+LEN_TRIM(eInform) < LEN(eInform) )THEN
  eInform = TRIM(name)//' '//TRIM(eInform)
END IF

RETURN
END

!******************************************************************************

LOGICAL FUNCTION BadValueReal( routine,name,mode,min,max,value )

USE checkErr_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
INTEGER,      INTENT( IN ) :: mode
REAL,         INTENT( IN ) :: min
REAL,         INTENT( IN ) :: max
REAL,         INTENT( IN ) :: value

LOGICAL, EXTERNAL :: SpecialValueReal
LOGICAL, EXTERNAL :: OutOfRangeReal

BadValueReal = .TRUE.

IF( .NOT.SpecialValueReal(mode,value) )THEN
  IF( SpecialValueReal(SPECIAL_VALUE-mode,value) )THEN
    CALL SetCheckErrorSpecial( routine,name,SPECIAL_VALUE-mode )
    GOTO 9999
  ELSE
    IF( OutOfRangeReal(min,max,value) )THEN
      CALL SetCheckErrorReal( routine,name,min,max,value )
      GOTO 9999
    END IF
  END IF
END IF

BadValueReal = .FALSE.

9999 CONTINUE

RETURN
END

!******************************************************************************

LOGICAL FUNCTION BadValueDouble( routine,name,mode,min,max,value )

USE checkErr_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
INTEGER,      INTENT( IN ) :: mode
REAL(8),      INTENT( IN ) :: min
REAL(8),      INTENT( IN ) :: max
REAL(8),      INTENT( IN ) :: value

LOGICAL, EXTERNAL :: SpecialValueDouble
LOGICAL, EXTERNAL :: OutOfRangeDouble

BadValueDouble = .TRUE.

IF( .NOT.SpecialValueDouble(mode,value) )THEN
  IF( SpecialValueDouble(SPECIAL_VALUE-mode,value) )THEN
    CALL SetCheckErrorSpecial( routine,name,SPECIAL_VALUE-mode )
    GOTO 9999
  ELSE
    IF( OutOfRangeDouble(min,max,value) )THEN
      CALL SetCheckErrorDouble( routine,name,min,max,value )
      GOTO 9999
    END IF
  END IF
END IF

BadValueDouble = .FALSE.

9999 CONTINUE

RETURN
END

!******************************************************************************

LOGICAL FUNCTION BadValueInteger( routine,name,mode,min,max,value )

USE checkErr_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: name
INTEGER,      INTENT( IN ) :: mode
INTEGER,      INTENT( IN ) :: min
INTEGER,      INTENT( IN ) :: max
INTEGER,      INTENT( IN ) :: value

LOGICAL, EXTERNAL :: SpecialValueInteger
LOGICAL, EXTERNAL :: OutOfRangeInteger

BadValueInteger = .TRUE.

IF( .NOT.SpecialValueInteger(mode,value) )THEN
  IF( SpecialValueInteger(SPECIAL_VALUE-mode,value) )THEN
    CALL SetCheckErrorSpecial( routine,name,SPECIAL_VALUE-mode )
    GOTO 9999
  ELSE
    IF( OutOfRangeInteger(min,max,value) )THEN
      CALL SetCheckErrorInteger( routine,name,min,max,value )
      GOTO 9999
    END IF
  END IF
END IF

BadValueInteger = .FALSE.

9999 CONTINUE

RETURN
END

!******************************************************************************

LOGICAL FUNCTION OutOfRangeReal( min,max,value )

IMPLICIT NONE

REAL, INTENT( IN ) :: min
REAL, INTENT( IN ) :: max
REAL, INTENT( IN ) :: value

OutOfRangeReal = value < min .OR. value > max

RETURN
END

!******************************************************************************

LOGICAL FUNCTION OutOfRangeDouble( min,max,value )

IMPLICIT NONE

REAL(8), INTENT( IN ) :: min
REAL(8), INTENT( IN ) :: max
REAL(8), INTENT( IN ) :: value

OutOfRangeDouble = value < min .OR. value > max

RETURN
END

!******************************************************************************

LOGICAL FUNCTION OutOfRangeInteger( min,max,value )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: min
INTEGER, INTENT( IN ) :: max
INTEGER, INTENT( IN ) :: value

OutOfRangeInteger = value < min .OR. value > max

RETURN
END

!******************************************************************************

LOGICAL FUNCTION SpecialValueReal( mode,value )

USE checkErr_fd
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode
REAL   , INTENT( IN ) :: value

SpecialValueReal = .FALSE.

IF( BTEST(mode,DEFAULT_BIT) )SpecialValueReal = SpecialValueReal .OR. value == DEF_VAL_R
IF( BTEST(mode,NOTSET_BIT)  )SpecialValueReal = SpecialValueReal .OR. value == NOT_SET_R
IF( BTEST(mode,DEFER_BIT)   )SpecialValueReal = SpecialValueReal .OR. value == DEFERRED_R

RETURN
END

!******************************************************************************

LOGICAL FUNCTION SpecialValueDouble( mode,value )

USE checkErr_fd
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode
REAL(8), INTENT( IN ) :: value

SpecialValueDouble = .FALSE.

IF( BTEST(mode,DEFAULT_BIT) )SpecialValueDouble = SpecialValueDouble .OR. value == DEF_VAL_D
IF( BTEST(mode,NOTSET_BIT)  )SpecialValueDouble = SpecialValueDouble .OR. value == NOT_SET_D
IF( BTEST(mode,DEFER_BIT)   )SpecialValueDouble = SpecialValueDouble .OR. value == DEFERRED_D

RETURN
END

!******************************************************************************

LOGICAL FUNCTION SpecialValueInteger( mode,value )

USE checkErr_fd
USE default_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mode
INTEGER, INTENT( IN ) :: value

SpecialValueInteger = .FALSE.

IF( BTEST(mode,DEFAULT_BIT) )THEN
  SpecialValueInteger = SpecialValueInteger .OR. value == DEF_VAL_I
END IF

IF( BTEST(mode,NOTSET_BIT) )THEN
  SpecialValueInteger = SpecialValueInteger .OR. value == NOT_SET_I
END IF

IF( BTEST(mode,DEFER_BIT) )THEN
  SpecialValueInteger = SpecialValueInteger .OR. value == DEFERRED_I
END IF

RETURN
END
