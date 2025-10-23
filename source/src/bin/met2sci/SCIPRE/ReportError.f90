SUBROUTINE ReportError()

!------ Write error messages to met2sci.err

USE met2sci_fi

IMPLICIT NONE

INTEGER i

OPEN(UNIT=lunErr,FILE='met2sci.err',STATUS='UNKNOWN',ACTION='WRITE',IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(*,*) 'Error opening error file'
  GOTO 9999
END IF

IF( error%Number /= NO_ERROR )THEN

  WRITE(lunErr,'(A)',IOSTAT=ios) '********************************'
  WRITE(lunErr,'(A)',IOSTAT=ios) '*         MET2SC Error         *'
  WRITE(lunErr,'(A)',IOSTAT=ios) '********************************'
  
  i = LEN_TRIM(error%Routine); IF( i > 0 )WRITE(lunErr,'(A)',IOSTAT=ios) 'Routine='//ADJUSTL(Error%Routine(1:i))
  i = LEN_TRIM(error%aString); IF( i > 0 )WRITE(lunErr,'(A)',IOSTAT=ios) ADJUSTL(Error%aString(1:i))
  i = LEN_TRIM(error%bString); IF( i > 0 )WRITE(lunErr,'(A)',IOSTAT=ios) ADJUSTL(Error%bString(1:i))
  i = LEN_TRIM(error%cString); IF( i > 0 )WRITE(lunErr,'(A)',IOSTAT=ios) ADJUSTL(Error%cString(1:i))

END IF

CLOSE(lunErr,IOSTAT=ios)

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE ClearError()

USE met2sci_fi

IMPLICIT NONE

error%Routine = ''
error%Number  = NO_ERROR
error%aString = ''
error%bString = ''
error%cString = ''

RETURN
END
