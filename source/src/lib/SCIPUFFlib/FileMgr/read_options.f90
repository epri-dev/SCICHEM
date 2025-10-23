!===============================================================================
!     ReadOptions
!===============================================================================
SUBROUTINE ReadOptions( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadOptions'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistOptions( lunit )
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
!     WriteOptions
!===============================================================================
SUBROUTINE WriteOptions( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF TIME1 namelist to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteOptions'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistOptions( lunit )
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
!     ReadNamelistOptions
!===============================================================================
SUBROUTINE ReadNamelistOptions( iunit )

USE met_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

REAL    PopSpeed, PopRes
INTEGER ios

INTEGER, EXTERNAL :: FindNML

!--- Eliminate grdmin. Leave in namelist for reading old files
REAL grdmin

NAMELIST / OPTIONS / t_avg,cmin,lsplitz,delmin,wwtrop,epstrop,sltrop &
                    ,uu_calm,sl_calm,nzbl,mgrd,z_dosage,smpfile,dt_smp &
                    ,substrate_type &
                    ,grdmin, PopSpeed, PopRes

ios = FindNML( iunit,'options' )

IF( ios == 0 )READ(UNIT=iunit,NML=OPTIONS,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistOptions'
  eMessage = 'Error reading OPTIONS namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistOptions'
  eMessage = 'EOF reading OPTIONS namelist'
  GOTO 9999
END IF

!--- Eliminate lsplitz from interface for now. Leave in namelist for reading old files
!--- Just always set to FALSE
lsplitz = .FALSE.

CALL CheckRangeReal( t_avg  ,0.0    ,1.E30,'Averaging time' )
CALL CheckRangeReal( cmin   ,0.0    ,1.E30,'Minimum puff mass' )
CALL CheckRangeReal( delmin ,0.0    ,1.E30,'Minimum surface grid' )
CALL CheckRangeReal( wwtrop ,0.0    ,1.E30,'wwtrop' )
CALL CheckRangeReal( sltrop ,1.0E-30,1.E30,'sltrop' )
CALL CheckRangeReal( epstrop,0.0    ,1.E30,'epstrop' )
CALL CheckRangeReal( uu_calm,0.0    ,1.E30,'uu_calm' )
CALL CheckRangeReal( sl_calm,1.0E-30,1.E30,'sl_calm' )
IF( nError /= NO_ERROR )GOTO 9999

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistOptions
!===============================================================================
SUBROUTINE WriteNamelistOptions( iunit )

USE met_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

NAMELIST / OPTIONS / t_avg,cmin,delmin,wwtrop,epstrop,sltrop &
                    ,uu_calm,sl_calm,nzbl,mgrd,z_dosage,smpfile &
                    ,dt_smp,substrate_type

WRITE(UNIT=iunit,NML=OPTIONS,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = WR_ERROR
  eRoutine = 'WriteNamelistOptions'
  eMessage = 'Error writing OPTIONS namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END

