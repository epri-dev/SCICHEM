!===============================================================================
!     ReadFlags
!===============================================================================
SUBROUTINE ReadFlags( file,lunit )

USE scipuff_fi

!     Reads the SCIPUFF FLAGS namelist from the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='OLD',ACTION='READ',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'ReadFlags'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== Read the namelist

CALL ReadNamelistFlags( lunit )
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
!     WriteFlags
!===============================================================================
SUBROUTINE WriteFlags( file,lunit )

USE scipuff_fi

!     Writes the SCIPUFF FLAGS namelist to the input file (*.INP)

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: lunit

INTEGER ios

!==== Open the file

OPEN( UNIT=lunit,FILE=file,STATUS='UNKNOWN',POSITION='APPEND',IOSTAT=ios,DELIM='APOSTROPHE' )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'WriteFlags'
  eMessage = 'Error opening SCIPUFF input file'
  GOTO 9998
END IF

!==== write the namelist

CALL WriteNamelistFlags( lunit )
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
!     ReadNamelistFlags
!===============================================================================
SUBROUTINE ReadNamelistFlags( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios

INTEGER relEffects          !This is for compatability with early SCIP 4.0 projects

CHARACTER(8) hazarea

INTEGER hazard     !For compatibility with HAZARD versions

INTEGER, EXTERNAL :: FindNML

LOGICAL hascal     !For compatibility with NFAC versions

NAMELIST / flags / title,create,hazard,audit_class,audit_analyst &
                  ,dynamic,dense_gas,static,multicomp,hazarea &
                  ,run_mode,hascal,prjEffects,relEffects

relEffects = NOT_SET_I

ios = FindNML( iunit,'flags' )

IF( ios == 0 )READ(UNIT=iunit,NML=flags,IOSTAT=ios)

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'ReadNamelistFlags'
  eMessage = 'Error reading FLAGS namelist'
  GOTO 9999
ELSE IF( ios < 0 )THEN
  nError   = EOF_ERROR
  eRoutine = 'ReadNamelistFlags'
  eMessage = 'EOF reading FLAGS namelist'
  GOTO 9999
END IF

!----- Transfer old relEfects top new prjEffects

IF( relEffects /= NOT_SET_I )prjEffects = relEffects

!----- Set Hazard area

9999 CONTINUE

RETURN
END
!===============================================================================
!     WriteNamelistFlags
!===============================================================================
SUBROUTINE WriteNamelistFlags( iunit )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iunit

INTEGER ios, i, j, ncht

CHARACTER(128) ctmp

NAMELIST / flags / title,create,audit_class,audit_analyst &
                  ,dynamic,dense_gas,static &
                  ,run_mode,prjEffects

! ----- Replace quotes in title with double

i = INDEX(title,'''')
j = 1
DO WHILE( i /= 0 )
  ncht = LEN_TRIM(title)
  i = i + j - 1
  IF( i < ncht )THEN
    ctmp = title(1:i)//''''//title(i+1:ncht)
  ELSE
    ctmp = title(1:i)//''''
  END IF
    title = TRIM(ctmp)
  j = i + 2
  i = INDEX(title(j:),'''')
END DO

WRITE(UNIT=iunit,NML=flags,IOSTAT=ios)
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'WriteNamelistFlags'
  eMessage = 'Error writing FLAGS namelist'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
