!===============================================================================
!     ReadStart
!===============================================================================
SUBROUTINE ReadStart( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadStart'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistTime1( lunit )
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
!     WriteStart
!===============================================================================
SUBROUTINE WriteStart( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteStart'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistTime1( lunit )
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
!     ReadNamelistTime1
!===============================================================================
SUBROUTINE ReadNamelistTime1( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER, EXTERNAL :: FindNML

INTEGER, DIMENSION(TIME_STATUS_ARRAY_SIZE) :: time_status

NAMELIST / time1 / year_start,month_start,day_start,tstart &
                  ,tzone, local, time_status

ios = FindNML( iunit,'time1' )

IF( ios == 0 )READ(UNIT=iunit,NML=time1,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistTime1'
  eMessage = 'Error reading TIME1 namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistTime1'
  eMessage = 'EOF reading TIME1 namelist'
  GOTO 9999
END IF

IF( tzone /= DEF_VAL_R .AND. tzone /= NOT_SET_R )THEN
  CALL CheckRangeReal( tzone,-24.0,24.0,'Time zone' )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistTime1
!===============================================================================
SUBROUTINE WriteNamelistTime1( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / time1 / year_start,month_start,day_start,tstart &
                  ,tzone, local

WRITE(UNIT=iunit,NML=time1,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistTime1'
  eMessage = 'Error writing TIME1 namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
