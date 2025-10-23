!*******************************************************************************
!            Strip NULL CHARACTER to END of a string
!*******************************************************************************
CHARACTER(*) FUNCTION StripNullT( string )

!DEC$ ATTRIBUTES DLLEXPORT :: StripNullT

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

INTEGER nch

CHARACTER(MAX_LARGEF) tempString

tempString = string

nch = INDEX(tempString,CHAR(0)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN(nch,LEN(tempString),LEN(StripNullT))

StripNullT = ' '
IF( nch > 0 )StripNullT = tempString(1:nch)

#if defined(LINUX)
!--- Remove DOS carriage return (CR)
tempString = StripNullT

nch = INDEX(tempString,CHAR(13)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN( nch,LEN(tempString),LEN(StripNullT) )

StripNullT = ' '
IF( nch > 0 )StripNullT = tempString(1:nch)
#endif

RETURN
END

!*******************************************************************************
!            Strip NULL CHARACTER to END of a string
!*******************************************************************************
CHARACTER(*) FUNCTION StripNull( string )

!DEC$ ATTRIBUTES DLLEXPORT :: StripNull

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

INTEGER nch

CHARACTER(MAX_FILEF) tempString

tempString = string

nch = INDEX(tempString,CHAR(0)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN( nch,LEN(tempString),LEN(StripNull) )

StripNull = ' '
IF( nch > 0 )StripNull = tempString(1:nch)

#if defined(LINUX)
!--- Remove DOS carriage return (CR)
tempString = StripNull

nch = INDEX(tempString,CHAR(13)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN( nch,LEN(tempString),LEN(StripNull) )

StripNull = ' '
IF( nch > 0 )StripNull = tempString(1:nch)
#endif

RETURN
END

!*******************************************************************************
!            Add NULL CHARACTER to END of a string
!*******************************************************************************
CHARACTER(*) FUNCTION AddNull( string )

!DEC$ ATTRIBUTES DLLEXPORT :: AddNull

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

CHARACTER(MAX_FILEC) tempString

INTEGER nch

tempString = string

nch = INDEX(tempString,CHAR(0)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN(nch,LEN(tempString)-1,LEN(AddNull)-1)

tempString(nch+1:nch+1) = CHAR(0)

AddNull = tempString(1:nch+1)
IF( nch+1 < LEN(AddNull) )AddNull(nch+2:) = ' '

RETURN
END
!*******************************************************************************
!            Add NULL CHARACTER to END of a string
!*******************************************************************************
CHARACTER(*) FUNCTION AddNullT( string )

!DEC$ ATTRIBUTES DLLEXPORT :: AddNullT

USE MaxLength_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: string

CHARACTER(MAX_LARGEF) tempString

INTEGER nch

tempString = string

nch = INDEX(tempString,CHAR(0)) - 1
IF( nch < 0 )nch = LEN( TRIM(tempString) )

nch = MIN(nch,LEN(tempString)-1,LEN(AddNullT)-1)

tempString(nch+1:nch+1) = CHAR(0)

AddNullT = tempString(1:nch+1)
IF( nch+1 < LEN(AddNullT) )AddNullT(nch+2:) = ' '

RETURN
END


MODULE string_utils

  CONTAINS
  !*******************************************************************************
  ! Converts a C-style char* string of length <= 255 to a Fortran character array
  ! of the same length-1 (removes null terminator). Use the C_PTR type from the
  ! ISO_C_BINDING module in the Fortran interface to declare the C string return
  ! type. Returns the string 'NULL' if the C string pointer is null.
  !*******************************************************************************
  FUNCTION StringCToF( c_str_ptr ) RESULT( f_str )

    !DEC$ ATTRIBUTES DLLEXPORT :: StringCToF

    USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR, C_F_POINTER
    IMPLICIT NONE

    TYPE(C_PTR), INTENT(IN) :: c_str_ptr

    INTEGER, PARAMETER :: max_str_len = 255
    INTEGER :: i, str_len

    CHARACTER(LEN=:), ALLOCATABLE :: f_str
    CHARACTER(LEN=max_str_len) :: tmp_str
    CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: char_arr_ptr => NULL()

    CALL C_F_POINTER( c_str_ptr, char_arr_ptr, [max_str_len] )

    IF( .NOT. ASSOCIATED(char_arr_ptr) ) THEN
      ALLOCATE( CHARACTER(LEN=4) :: f_str )
      f_str = 'NULL'
      RETURN
    END IF

    tmp_str = ' '

    ! Read C-string until null terminator found and record length
    DO i = 1, max_str_len
      IF( char_arr_ptr(i) == C_NULL_CHAR ) THEN
        str_len = i - 1
        EXIT
      END IF

      tmp_str(i:i) = char_arr_ptr(i)
    END DO

    ALLOCATE( CHARACTER(LEN=str_len) :: f_str )
    f_str = tmp_str(1:str_len)

  END FUNCTION StringCToF

END MODULE string_utils
