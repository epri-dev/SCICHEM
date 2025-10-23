!===============================================================================
!     CheckRangeReal
!===============================================================================
SUBROUTINE CheckRangeReal( x,xmin,xmax,string )

USE default_fd
USE error_fi

IMPLICIT NONE

REAL,         INTENT( IN ) :: x, xmin, xmax
CHARACTER(*), INTENT( IN ) :: string

IF( x /= DEF_VAL_R .AND. x /= NOT_SET_R .AND. x /= DEFERRED_R )THEN

  IF( x < xmin .OR. x > xmax )THEN
    nError = IV_ERROR
    eMessage = TRIM(string)//' outside allowable range'
    WRITE(eInform,*)'Range = ',xmin,' to',xmax
  END IF

END IF

RETURN
END
!===============================================================================
!     IncrementList
!===============================================================================
LOGICAL FUNCTION IncrementList( List )

USE list_fd
USE error_fi

IMPLICIT NONE

TYPE( listHeadT ), INTENT( INOUT ) :: List

IncrementList = .false.

IF( List%number >= List%max )THEN
  nError   = SZ_ERROR
  eRoutine = 'IncrementList'
  eMessage = 'List is full'
  WRITE(eInform,'(A,I6)')'List maximum = ',List%max
ELSE
  List%number   = List%number + 1
  IncrementList = .TRUE.
END IF

RETURN
END
!===============================================================================
!     FindNML
!===============================================================================
INTEGER FUNCTION FindNML( lun,nml )

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: nml

CHARACTER(128) line, nml1, nml2, nml1U, nml2U
INTEGER        ios

! build namelist strings for search

nml1  = '&'//TRIM( nml ); nml2   = '$'//TRIM( nml )
nml1U = nml1            ; nml2U = nml2
CALL cupper( nml1U )    ; CALL cupper( nml2U )

DO !read until '&nml' or '$nml' found

  READ(UNIT=lun,FMT='(A)',IOSTAT=ios) line

  IF( ios /= 0 )THEN
    FindNML = ios
    RETURN
  END IF

  line = ADJUSTL( line )  !remove leading blanks

  IF( INDEX(line,TRIM(nml2U)) == 1 )EXIT
  IF( INDEX(line,TRIM(nml1U)) == 1 )EXIT
  IF( INDEX(line,TRIM(nml2) ) == 1 )EXIT
  IF( INDEX(line,TRIM(nml1) ) == 1 )EXIT

END DO

BACKSPACE( UNIT=lun,IOSTAT=ios )

FindNML = ios

RETURN
END
