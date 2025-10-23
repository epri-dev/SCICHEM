!===============================================================================
!     ReadEnd
!===============================================================================
SUBROUTINE ReadEnd( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME2 namelist from the input file (*.INP,*.RST)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER     , INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION='READ',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadEnd'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistTime2( lunit )
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
!     WriteEnd
!===============================================================================
SUBROUTINE WriteEnd( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME2 namelist to the input file (*.INP,*.RST)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER     , INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteEnd'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistTime2( lunit )
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
!     ReadNamelistTime2
!===============================================================================
SUBROUTINE ReadNamelistTime2( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER, EXTERNAL :: FindNML

NAMELIST / time2 / year_end,month_end,day_end,tend,tend_hr &
                  ,delt,dt_save

ios = FindNML( iunit,'time2')

IF( ios == 0 )READ(UNIT=iunit,NML=time2,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistTime2'
  eMessage = 'Error reading TIME2 namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistTime2'
  eMessage = 'EOF reading TIME2 namelist'
  GOTO 9999
END IF

CALL CheckRangeReal( delt,1.E-30,1.E30,'Time step' )
IF( nError /= NO_ERROR )GOTO 9999

CALL CheckRangeReal( dt_save,delt,1.E30,'Output interval' )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistTime2
!===============================================================================
SUBROUTINE WriteNamelistTime2( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / time2 / year_end,month_end,day_end,tend,tend_hr &
                  ,delt,dt_save

WRITE(UNIT=iunit,NML=time2,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistTime2'
  eMessage = 'Error writing TIME2 namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
