!===============================================================================
!     ReadCtrl
!===============================================================================
SUBROUTINE ReadCtrl( file,lunit )

USE scipuff_fi


!     Reads the SCIPUFF CTRL namelist from the input file (*.INP,*.RST)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER     , INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadCtrl'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistCtrl( lunit )
IF( nError /= NO_ERROR )GOTO 9998

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE
CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     WriteCtrl
!===============================================================================
SUBROUTINE WriteCtrl( file,lunit,extFlag )

USE scipuff_fi
USE basic_fd

!     Reads the SCIPUFF CTRL namelist to the input file (*.INP,*.RST)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER     , INTENT( IN ) :: lunit
INTEGER     , INTENT( IN ) :: extFlag

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteCtrl'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

IF( extFlag == FALSE )THEN
  CALL WriteNamelistCtrlRst( lunit )
ELSE
  CALL WriteNamelistCtrl( lunit )
END IF
IF( nError /= NO_ERROR )GOTO 9998

!==== Close the file and return

9999 CONTINUE

CLOSE( UNIT=lunit,IOSTAT=ios )

RETURN

9998 CONTINUE

CALL ReportFileName( eInform,'File=',file )
GOTO 9999

END
!===============================================================================
!     ReadNamelistCtrl
!===============================================================================
SUBROUTINE ReadNamelistCtrl( iunit )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER, EXTERNAL :: FindNML

NAMELIST / ctrl  / restart, file_rst, path_rst, time_rst

file_rst = '<empty>'
path_rst = ' '
time_rst = DEF_VAL_R


ios = FindNML( iunit,'ctrl' )

IF( ios == 0 )THEN

  READ(UNIT=iunit,NML=ctrl,IOSTAT=ios)

END IF

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistCtrl'
  eMessage = 'Error reading CTRL namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistCtrl'
  eMessage = 'EOF reading CTRL namelist'
  GOTO 9999
END IF


IF( file_rst == '<empty>' )THEN
  file_rst = ' '
  path_rst = ' '
  time_rst = 0.0
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistCtrlRst
!===============================================================================
SUBROUTINE WriteNamelistCtrlRst( iunit )

USE scipuff_fi
USE files_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / ctrl  / restart, file_rst, path_rst, time_rst

restart = .FALSE.

WRITE(UNIT=iunit,NML=ctrl,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistCtrl'
  eMessage = 'Error writing CTRL/restart namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistCtrl
!===============================================================================
SUBROUTINE WriteNamelistCtrl( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / ctrl  / restart

restart = .TRUE.

WRITE(UNIT=iunit,NML=ctrl,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistCtrl'
  eMessage = 'Error writing CTRL namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
