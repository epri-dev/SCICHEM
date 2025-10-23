!*******************************************************************************
!*******************************************************************************
!            Get project version number
!*******************************************************************************
INTEGER FUNCTION ExtracProjectVersion()

USE scipuff_fi

IMPLICIT NONE

ExtracProjectVersion = iversion

RETURN
END
!***********************************************************************
!               SetUTMerror
!***********************************************************************
SUBROUTINE SetUTMerror( subr_name,ierr )

USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: subr_name
INTEGER,      INTENT( IN ) :: ierr

CHARACTER(20) string

WRITE(string,*) ierr

nError   = IV_ERROR
eRoutine = TRIM(subr_name)
eMessage = 'Error from DtmsExtendedZone'
eInform  = 'Error = '//TRIM(ADJUSTL(string))

SELECT CASE( ierr )
  CASE( 3 )
    eAction = 'Longitude not in range [-180,+180]'
  CASE( 4 )
    eAction = 'Latitude not in range [-80,+84]'
  CASE( 12 )
    eAction = 'Invalid zone number'
  CASE( 13 )
    eAction = 'Northing not in range [-1E5,+1E5] km'
  CASE( 14 )
    eAction = 'Easting more than 9 deg from central meridian'
  CASE( 18 )
    eAction = 'More that 9.17 deg from central meridian'
  CASE( 20 )
    eAction = 'Error initializing DATUMS'
  CASE DEFAULT
    eAction = 'Unexpected error condition'
END SELECT

RETURN
END
!*******************************************************************************
!                CheckFileT
!*******************************************************************************
LOGICAL FUNCTION CheckFileT( file )

USE SCIMgr_fd
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file

INTEGER irv

LOGICAL, EXTERNAL :: CheckPathT
INTEGER, EXTERNAL :: sysCheckFile

CheckFileT = .FALSE.

DO

  irv = sysCheckFile( file )
  IF( irv == SCIPfailure )THEN
    nError   = NF_ERROR
    eRoutine = 'CheckFileT'
    eMessage = 'File not Found'
    eInform  = TRIM(file)
    eAction  = 'Check file and/or INI file definitions'
  ELSE IF( irv == SCIPnull )THEN
    IF( CheckPathT(file) )CYCLE
  ELSE
    CheckFileT = .TRUE.
  END IF

  EXIT

END DO

RETURN
END
!*******************************************************************************
!                CheckFileT_NoError
!*******************************************************************************
LOGICAL FUNCTION CheckFileT_NoError( file )

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: file

INQUIRE( FILE=file,EXIST=CheckFileT_NoError )

RETURN
END
!*******************************************************************************
!                CheckPathT
!*******************************************************************************
LOGICAL FUNCTION CheckPathT( path )

USE SCIMgr_fd
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

INTEGER irv

LOGICAL, EXTERNAL :: CheckDriveT
INTEGER, EXTERNAL :: sysCheckPath

CheckPathT = .FALSE.

irv = sysCheckPath( path )
IF( irv == SCIPfailure )THEN
  CheckPathT = CheckDriveT( path )
ELSE IF( irv == SCIPnull )THEN
  CheckPathT = .FALSE.
ELSE
  CheckPathT = .TRUE.
END IF

IF( .NOT.CheckPathT )THEN
  nError   = NF_ERROR
  eRoutine = 'CheckPath'
  eMessage = 'Path not Found'
  eInform  = TRIM(path)
  eAction  = 'Check path and/or INI file definitions'
END IF

RETURN
END
!*******************************************************************************
!                CheckPathT_NoError
!*******************************************************************************
LOGICAL FUNCTION CheckPathT_NoError( path )

USE SCIMgr_fd
USE search_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: path

INTEGER irv

INTEGER, EXTERNAL :: sysCheckPath
INTEGER, EXTERNAL :: ToolgetDriveType

CheckPathT_NoError = .FALSE.

irv = sysCheckPath( path )
IF( irv == SCIPfailure )THEN
  CheckPathT_NoError = ToolgetDriveType(path) /= LOCAL_DRIVE
ELSE IF( irv == SCIPnull )THEN
  CheckPathT_NoError = .FALSE.
ELSE
  CheckPathT_NoError = .TRUE.
END IF

RETURN
END
!*******************************************************************************
!                CheckDriveT
!*******************************************************************************
LOGICAL FUNCTION CheckDriveT( file )

USE SCIMgr_fd
USE error_fi

IMPLICIT NONE

INTEGER, PARAMETER :: DRIVE_REMOTE = 4
INTEGER, PARAMETER :: DRIVE_CDROM  = 5

CHARACTER(*), INTENT( IN ) :: file

INTEGER drive,irv

CHARACTER(PATH_MAXLENGTH) check

INTEGER,                   EXTERNAL :: sysCheckDrive
INTEGER,                   EXTERNAL :: sysDriveType
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

IF( file(2:2) == ':' )THEN
  check = AddNull( file(1:2) )
ELSE
  check = AddNull( TRIM(file) )
END IF

drive = sysDriveType( check )

DO

  CheckDriveT   = .FALSE.

  IF( drive == DRIVE_CDROM )THEN
    eRoutine = 'CheckDriveT'
    nError   = WN_ERROR
    eMessage = 'CDROM path not found : '//TRIM(file)
    eInform  = 'Please verify that the correct CD is in the CDROM drive'
    eAction  = 'Click YES to Retry : NO to Ignore'
    CALL WarningMessage( .FALSE. )
    CheckDriveT = nError == NO_ERROR
  ELSE IF( drive == DRIVE_REMOTE )THEN
    eRoutine = 'CheckDriveT'
    nError   = WN_ERROR
    eMessage = 'Remote(Network) path not found : '//TRIM(file)
    eInform  = 'Please verify that you are connected to the Network Drive'
    eAction  = 'Click YES to Retry : NO to Ignore'
    CALL WarningMessage( .FALSE. )
    CheckDriveT = nError == NO_ERROR
  END IF

  IF( CheckDriveT )THEN
    irv = sysCheckDrive( file )
    IF( irv == SCIPfailure )CYCLE
  END IF

  EXIT

END DO

RETURN
END
!*******************************************************************************
!                ToolGetDriveType
!*******************************************************************************
INTEGER FUNCTION ToolGetDriveType( file )

USE search_fd
USE DefSize_fd

IMPLICIT NONE

INTEGER, PARAMETER :: DRIVE_REMOTE = 4
INTEGER, PARAMETER :: DRIVE_CDROM  = 5

CHARACTER(*), INTENT( IN ) :: file

INTEGER drive

CHARACTER(PATH_MAXLENGTH) check

INTEGER,                   EXTERNAL :: sysDriveType
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

IF( file(2:2) == ':' )THEN
  check = AddNull( file(1:2) )
ELSE
  check = AddNull( TRIM(file) )
END IF

drive = sysDriveType( check )

IF( drive == DRIVE_CDROM )THEN
  ToolGetDriveType = CDROM_DRIVE
ELSE IF( drive == DRIVE_REMOTE )THEN
  ToolGetDriveType = NETWORK_DRIVE
ELSE
  ToolGetDriveType = LOCAL_DRIVE
END IF

RETURN
END
