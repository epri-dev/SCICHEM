!===============================================================================
!     CountRelease
!===============================================================================
INTEGER FUNCTION CountRelease( UserID,file,nRel,nMCrel )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIMgrState

!Counts the SCIPUFF SCN namelists from the input file

IMPLICIT NONE

INTEGER,           INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file   !filename
INTEGER,           INTENT( OUT ) :: nRel   !number of releases in file
INTEGER,           INTENT( OUT ) :: nMCrel !number of multicomponent records in file

INTEGER ios
INTEGER lun

INTEGER currentState,irv
LOGICAL lopen

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Initialize

CountRelease = SCIPFailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState(HS_BUSY)
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

filename = StripNull( TRIM(file%string) )

!==== Check to see if file is already opened

INQUIRE( FILE=filename,OPENED=lopen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'CountRelease'
  eMessage = 'Error checking open status of SCIPUFF input file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

!==== If Open - determine unit number

IF( lopen )THEN
  INQUIRE( FILE=filename,NUMBER=lun,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'CountRelease'
    eMessage = 'Error determining logical unit number of SCIPUFF input file'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF

!==== If not - Open the file

ELSE
  lun = lun_tmp
  OPEN( UNIT=lun,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'CountRelease'
    eMessage = 'Error opening SCIPUFF input file'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF
END IF

!==== Read the namelists

nRel   = 0
nMCrel = 0
nError = NO_ERROR

DO WHILE( nError == NO_ERROR )
  CALL CountNamelistScn( lun,nRel,nMCrel )
END DO

!==== Do some error checking

IF( nError == EOF_ERROR )THEN
  CALL ModuleInitError()
ELSE IF( nError /= NO_ERROR )THEN
  GOTO 9999
END IF

CountRelease = SCIPSuccess

!==== Close the file and return

9999 CONTINUE

IF( .NOT.lopen )CLOSE(UNIT=lun,IOSTAT=ios)

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
!===============================================================================
!     CountMaterial
!===============================================================================
INTEGER FUNCTION CountMaterial( UserID,file,nMtl )

USE SCIMgr_fd
USE files_fi
USE error_fi
USE SCIMgrState

!Counts the SCIPUFF MATDEF namelists from the input file

IMPLICIT NONE

INTEGER,           INTENT( IN  ) :: userID !USER ID tag
TYPE( fileNameT ), INTENT( IN  ) :: file !filename
INTEGER,           INTENT( OUT ) :: nMtl !number of materials in file

INTEGER ios
INTEGER lun
INTEGER nCount, currentState, irv
LOGICAL lopen, read_v02

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Initialize

CountMaterial = SCIPFailure

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

filename = StripNull( TRIM(file%string) )

!==== Check to see if file is already opened

INQUIRE( FILE=filename,OPENED=lopen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'CountMaterial'
  eMessage = 'Error checking open status of SCIPUFF input file'
  CALL ReportFileName( eInform,'File=',filename )
  GOTO 9999
END IF

!==== If Open - determine unit number

IF( lopen )THEN
  INQUIRE( FILE=filename,NUMBER=lun,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'CountMaterial'
    eMessage = 'Error determining logical unit number of SCIPUFF input file'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF

!==== If not - Open the file

ELSE
  lun = lun_tmp
  OPEN( UNIT=lun,FILE=filename,STATUS='OLD',ACTION="READ",IOSTAT=ios,DELIM='APOSTROPHE' )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'CountMaterial'
    eMessage = 'Error opening SCIPUFF input file'
    CALL ReportFileName( eInform,'File=',filename )
    GOTO 9999
  END IF
END IF

!==== Read the namelists

nCount = 0
read_v02 = .FALSE.
nError = NO_ERROR

DO WHILE( nError == NO_ERROR )

  IF( read_v02 )THEN
    CALL ReadNamelistMatdef_v02( lun )
  ELSE
    CALL ReadNamelistMatdef( lun )
    IF( nError == RD_ERROR )THEN
      IF( nCount == 0 )THEN
        read_v02 = .TRUE.
        CALL ModuleInitError()
        REWIND(UNIT=lun,IOSTAT=ios)
        CYCLE
      END IF
    END IF
  END IF
  IF( nError == NO_ERROR )nCount = nCount + 1

END DO

!==== Do some error checking

IF( nError == EOF_ERROR )THEN
  CALL ModuleInitError()
ELSE IF( nError /= NO_ERROR )THEN
  GOTO 9999
END IF

nMtl = nCount
CountMaterial = SCIPSuccess

!==== Close the file and return

9999 CONTINUE

IF( .NOT.lopen )CLOSE(UNIT=lun,IOSTAT=ios)

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END
