MODULE SwinAPI
  USE winAPI_fd
  USE MaxLength_fd
#if defined(LINUX)
  CONTAINS
!==============================================================================
! GetCurrentDirectory
!==============================================================================
  INTEGER FUNCTION GetCurrentDirectory( nBufferLength ,lpBuffer )

  USE winAPI_fd
  USE MaxLength_fd
  IMPLICIT NONE

  INTEGER, INTENT(IN)                   :: nBufferLength
  CHARACTER(nBufferLength), INTENT(OUT) :: lpBuffer

  INTEGER(4)     irv, getcwd, nch
  CHARACTER(MAX_PATHF) DirName

  irv = -1
  GetCurrentDirectory = NULL
  lpBuffer = ''

  irv = getcwd(DirName)
  IF( irv == 0 )THEN
    nch = LEN_TRIM(DirName)
    IF( nch < nBufferLength )THEN
      lpBuffer = DirName(1:nch)
      GetCurrentDirectory = nch
    ELSE
      WRITE(*,*)' Error in GetCurrentDirectory nch > MAXPATHC',nch,nBufferLength
    END IF
  ELSE
    WRITE(*,*)' Error in GetCurrentDirectory for getcwd'
  END IF

  RETURN
  END FUNCTION GetCurrentDirectory

!==============================================================================
! SetCurrentDirectory
!==============================================================================
  INTEGER FUNCTION SetCurrentDirectory( path_tmp )

  USE winAPI_fd
  USE MaxLength_fd
  IMPLICIT NONE

  CHARACTER*(*)  path_tmp

  CHARACTER(MAX_PATHF) path

  INTEGER  irv, chdir

  CHARACTER(MAX_FILEF), EXTERNAL :: STRIPNULL

  path = STRIPNULL(path_tmp)

  irv = -1
  irv = chdir(TRIM(path))
  IF (irv == 0) THEN
    SetCurrentDirectory  = TRUE
  ELSE
    WRITE(*,100)TRIM(path)
    SetCurrentDirectory  = FALSE
  END IF

  100 FORMAT('Error in SetCurrentDirectory for "',(A),'"')

  RETURN
  END FUNCTION SetCurrentDirectory
!==============================================================================
! CreateDirectory
!==============================================================================
  INTEGER FUNCTION CreateDirectory (path_tmp,sa)

  USE winAPI_fd
  USE MaxLength_fd
  IMPLICIT NONE

  CHARACTER*(*)  path_tmp

  CHARACTER(MAX_FILEF) path
  CHARACTER(MAX_LARGEF) cmd

  INTEGER  ios, SYSTEM

  TYPE ( T_SECURITY_ATTRIBUTES ) sa

  CHARACTER(MAX_PATHF), EXTERNAL :: STRIPNULL

  path = STRIPNULL(path_tmp)
  cmd = 'mkdir  '//TRIM(path)
  ios = SYSTEM(TRIM(cmd))
  IF (ios == 0) THEN
    CreateDirectory = TRUE
  ELSE
    CreateDirectory = FALSE
    WRITE(*,*)' Error in CreateDirectory for ',TRIM(path)
  END IF

  RETURN
  END FUNCTION CreateDirectory

!==============================================================================
! RemoveDirectory
!==============================================================================
  INTEGER FUNCTION RemoveDirectory( path_tmp )

  USE winAPI_fd
  USE MaxLength_fd
  IMPLICIT NONE

  CHARACTER*(*) path_tmp
  CHARACTER(MAX_FILEF) path
  CHARACTER(MAX_LARGEF) cmd

  INTEGER  ios, SYSTEM

  CHARACTER(MAX_FILEF), EXTERNAL :: STRIPNULL

  path = STRIPNULL(path_tmp)

  cmd = 'rmdir  '//TRIM(path)
  ios = SYSTEM(TRIM(cmd))
  IF (ios == 0) THEN
    RemoveDirectory = TRUE
  ELSE
    RemoveDirectory = FALSE
    WRITE(*,*)' Error in RemoveDirectory for ',TRIM(path)
  END IF

  RETURN
  END FUNCTION RemoveDirectory

!==============================================================================
! DeleteFile
!==============================================================================
  INTEGER FUNCTION DeleteFile( lpFileName )

  USE winAPI_fd
  USE MaxLength_fd
  IMPLICIT NONE

  CHARACTER*(*)   lpFileName

  CHARACTER(MAX_FILEF) FileName
  CHARACTER(MAX_LARGEF) cmd

  INTEGER  ios, SYSTEM

  CHARACTER(MAX_FILEF), EXTERNAL :: STRIPNULL

  FileName = STRIPNULL(lpFileName)

  cmd = 'rm -f '//TRIM(FileName)
  ios = SYSTEM(TRIM(cmd))
  IF (ios == 0) THEN
    DeleteFile = TRUE
  ELSE
    DeleteFile = FALSE
    WRITE(*,*)'Error in DeleteFile for ',TRIM(FileName)
  END IF

  RETURN
  END FUNCTION DeleteFile

!==============================================================================
! CopyFile
!==============================================================================
  INTEGER FUNCTION CopyFile( lpExistingFileName,lpNewFileName,bFailIfExists )

  USE MaxLength_fd
  USE winAPI_fd

  IMPLICIT NONE

  CHARACTER(*)      lpExistingFileName
  CHARACTER(*)      lpNewFileName
  INTEGER           bFailIfExists

  CHARACTER(MAX_FILEF) ExistingFileName,NewFileName
  CHARACTER(MAX_LARGEF) cmd
  INTEGER           ios, SYSTEM

  CHARACTER(MAX_FILEF), EXTERNAL :: STRIPNULL

  ExistingFileName = STRIPNULL(lpExistingFileName)
  NewFileName      = STRIPNULL(lpNewFileName)

  cmd = 'cp -p '//TRIM(ExistingFileName)//' '//TRIM(NewFileName)
  ios = SYSTEM(TRIM(cmd))
  IF (ios == 0) THEN
    CopyFile = TRUE
  ELSE
    CopyFile = FALSE
    WRITE(*,*)'Error in CopyFile for ',TRIM(ExistingFileName)
  END IF

  RETURN
  END FUNCTION CopyFile

!==============================================================================
! GetLocalTime
!==============================================================================
  SUBROUTINE GetLocalTime( lpSystemTime )

  USE winAPI_fd
  IMPLICIT NONE

  TYPE( T_SYSTEMTIME )    lpSystemTime


  CHARACTER*12 :: sysGetTime
  CHARACTER*10 :: date,clock,time,zone
  INTEGER      :: values(8)

  CALL date_and_time( date,clock,zone,values )

  !values (1)  The 4-digit year
  !values (2)  The month of the year
  !values (3)  The day of the month
  !values (4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes
  !values (5)  The hour of the day (range 0 to 23) - local time
  !values (6)  The minutes of the hour (range 0 to 59) - local time
  !values (7)  The seconds of the minute (range 0 to 59) - local time
  !values (8)  The milliseconds of the second (range 0 to 999) - local time

  lpSystemTime%wYear    = values(1)
  lpSystemTime%wMonth   = values(2)
  lpSystemTime%wDayOfWeek     = 1
  lpSystemTime%wDay     = values(3)
  lpSystemTime%wHour    = values(5)
  lpSystemTime%wMinute  = values(6)
  lpSystemTime%wSecond  = values(7)
  lpSystemTime%wMilliseconds  = values(8)

  RETURN
  END SUBROUTINE GetLocalTime

!==============================================================================
! Sleep
!==============================================================================
  SUBROUTINE Sleep( Msec )

  USE MaxLength_fd
  IMPLICIT NONE

  INTEGER Msec
  CHARACTER( MAX_LARGEF ) :: cmd
  INTEGER :: ios, SYSTEM

  WRITE(cmd,*)'sleep ',Msec*1.e-3
  ios = SYSTEM( TRIM(cmd) )
  IF (ios /= 0) THEN
    WRITE(*,*)'Error in Sleep '
  END IF

  RETURN
  END SUBROUTINE Sleep

!==============================================================================
! Empty functions - just return success
!==============================================================================
!==============================================================================
! GetDriveType
!==============================================================================
  INTEGER FUNCTION GetDriveType( lpRootPathName )

  USE search_fd
  USE winAPI_fd
  IMPLICIT NONE

  CHARACTER*(*)   lpRootPathName

  GetDriveType = LOCAL_DRIVE    ! Dummy function to always return local drive

  RETURN
  END FUNCTION GetDriveType

!==============================================================================
! GetLastError
!==============================================================================
  INTEGER FUNCTION GetLastError()
   GetLastError = 1
  END FUNCTION GetLastError

!==============================================================================
! LoadLibrary
!==============================================================================
  FUNCTION LoadLibrary( lpLibFileName )
  USE winAPI_fd
  IMPLICIT NONE

  INTEGER(HANDLE) :: LoadLibrary
  CHARACTER*(*)   lpLibFileName

  INTEGER(HANDLE)     dlopen

  LoadLibrary = 1

  RETURN
  END FUNCTION LoadLibrary

!==============================================================================
! FreeLibrary
!==============================================================================
  FUNCTION FreeLibrary( hLibModule )
  USE winAPI_fd
  IMPLICIT NONE

  INTEGER(BOOL)   :: FreeLibrary
  INTEGER(HANDLE)    hLibModule

  INTEGER irv

  INTEGER(BOOL)     dlclose

  irv = 1

  IF( irv == FALSE )THEN
    FreeLibrary = FALSE
  ELSE
    FreeLibrary = TRUE
  END IF

  RETURN
  END FUNCTION FreeLibrary

!==============================================================================
! GetProcAddress
!==============================================================================
  FUNCTION GetProcAddress( hModule,lpProcName )

  USE winAPI_fd
  IMPLICIT NONE

  INTEGER(HANDLE) :: GetProcAddress
  INTEGER(HANDLE)    hModule
  CHARACTER*(*)      lpProcName

  INTEGER(HANDLE)     dlsym

  GetProcAddress = 1 !dlsym( hModule,lpProcName )

  RETURN
  END FUNCTION GetProcAddress

!==============================================================================
! GetTickCount
!==============================================================================
  INTEGER FUNCTION GetTickCount()
   GetTickCount = 0
  RETURN
  END FUNCTION GetTickCount

#else
!==============================================================================
!==============================================================================
!Use Intel WINDOWs function defintions (Intel and not Linux)
!==============================================================================
!==============================================================================
  USE IFWINA, HIDEX_TRUE => TRUE, HIDEX_FALSE => FALSE, HIDEX_NULL=>NULL, HIDEX_NO_ERROR => NO_ERROR &
            , HIDEX_ERROR => ERROR
#endif
END MODULE SwinAPI
