!***********************************************************************
!               InitReleaseCheck
!***********************************************************************
LOGICAL FUNCTION InitReleaseCheck( nMtl )

USE error_fi
USE ReleaseCheck_fi

! This sets up materials for checking interactive sources

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nMtl

INTEGER ios

InitReleaseCheck = .FALSE.

IF( ALLOCATED(mtlList) )THEN
  IF( SIZE(mtlList) /= nMtl )THEN
    nError    = IV_ERROR
    eRoutine  = 'InitReleaseCheck'
    eMessage  = 'Error preparing to check releases'
    eInform   = 'Current number of materials does not match allocated array size'
    GOTO 9999
  END IF
ELSE
  ALLOCATE( mtlList(nMtl), STAT=ios )
  IF( ios /= 0 )THEN
    nError   = IV_ERROR
    eRoutine = 'InitReleaseCheck'
    eMessage = 'Error preparing to check releases'
    eInform  = 'Unable to allocate space for material list'
    GOTO 9999
  END IF
  mtlHead%max = nMtl
  CALL LoadMaterial( mtlHead,mtlList )
  IF( nError /= NO_ERROR )THEN
    nError   = IV_ERROR
    eRoutine = 'InitReleaseCheck'
    eMessage = 'Error preparing to check releases'
    eInform  = 'Unable to load material list'
    GOTO 9999
  END IF

END IF

InitReleaseCheck = .TRUE.

9999 CONTINUE

RETURN
END
!***********************************************************************
!               ReleaseCheck
!***********************************************************************
LOGICAL FUNCTION ReleaseCheck( release )

USE ReleaseCheck_fi
USE release_fd
USE SCIPresults_fd

! This sets up an interactive source at time=t

IMPLICIT NONE

TYPE( releaseT ), INTENT( IN ) :: release

INTEGER, EXTERNAL :: Check_Release, SCIPUFFNumMtl
LOGICAL, EXTERNAL :: InitReleaseCheck

INTEGER iMat

ReleaseCheck = InitReleaseCheck( SCIPUFFNumMtl() )

IF( ReleaseCheck )THEN

  IF( release%type == HR_FILE )THEN
    iMat = 1
  ELSE
    DO iMat = 1,mtlHead%number
      IF( TRIM(release%material) == TRIM(mtlList(iMat)%name) )THEN
        EXIT
      END IF
    END DO
    iMat = MIN(iMat,mtlHead%number)
  END IF

  ReleaseCheck = Check_Release(release,mtlList(iMat)) == SCIPsuccess

END IF

RETURN
END
!***********************************************************************
!               SkipRelease
!***********************************************************************
LOGICAL FUNCTION SkipRelease( release )

USE release_fd
USE default_fd
USE error_fi
USE files_fi

IMPLICIT NONE

TYPE( releaseT ), INTENT( IN ) :: release

TYPE( relContT  ) contData
TYPE( relFileT  ) fileData
TYPE( relInstT  ) instData
TYPE( relXInstT ) XinstData
TYPE( relMoveT  ) moveData
TYPE( relContFileT ) contDataF
TYPE( relAreaT     ) areaData
TYPE( relPoolT  ) poolData
TYPE( relPuffT  ) puffData
TYPE( relStackT ) stckData
TYPE( relStack3T ) stck3Data

INTEGER ios

REAL, EXTERNAL :: GetSCIPUFFtime

SkipRelease = .FALSE.

SELECT CASE( release%type )
  CASE( HR_INST )
    instData = TRANSFER(release%relData,instData)
    SkipRelease = instData%mass == NOT_SET_R
  CASE( HR_XINST )
    XinstData = TRANSFER(release%relData,XinstData)
    SkipRelease = XinstData%mass == NOT_SET_R
  CASE( HR_FINST )
    instData = TRANSFER(release%relData,instData)
    SkipRelease = instData%mass == NOT_SET_R
  CASE( HR_FILE )
    fileData = TRANSFER(release%relData,fileData)
    SkipRelease = TRIM(fileData%relFile) == TRIM(NOT_SET_C)
  CASE( HR_PUFF )
    puffData = TRANSFER(release%relData,puffData)
    SkipRelease = puffData%mass == NOT_SET_R
  CASE( HR_CONT )
    contData = TRANSFER(release%relData,contData)
    SkipRelease = contData%rate == NOT_SET_R
  CASE( HR_CONTF )
    contDataF = TRANSFER(release%relData,contDataF)
    SkipRelease = contDataF%rate == NOT_SET_R .OR. &
                  TRIM(fileData%relFile) == TRIM(NOT_SET_C)
  CASE( HR_STACKF,HR_STACKFP )
    stckData = TRANSFER(release%relData,stckData)
    SkipRelease = stckData%rate == NOT_SET_R .OR. &
                  TRIM(fileData%relFile) == TRIM(NOT_SET_C)
  CASE( HR_STACK3F )
    stck3Data = TRANSFER(release%relData,stck3Data)
    SkipRelease = stck3Data%rate == NOT_SET_R .OR. &
                  TRIM(fileData%relFile) == TRIM(NOT_SET_C)
  CASE( HR_AREA )
    areaData = TRANSFER(release%relData,areaData)
    SkipRelease = areaData%rate == NOT_SET_R
  CASE( HR_AREAF )
    areaData = TRANSFER(release%relData,areaData)
    SkipRelease = areaData%rate == NOT_SET_R .OR. &
                  TRIM(fileData%relFile) == TRIM(NOT_SET_C)
CASE( HR_MOVE )
    moveData = TRANSFER(release%relData,moveData)
    SkipRelease = moveData%rate == NOT_SET_R
  CASE( HR_STACK,HR_PRIME )
    stckData = TRANSFER(release%relData,stckData)
    SkipRelease = stckData%rate == NOT_SET_R
  CASE( HR_STACK3 )
    stck3Data = TRANSFER(release%relData,stck3Data)
    SkipRelease = stck3Data%rate == NOT_SET_R
  CASE( HR_POOL )
    poolData = TRANSFER(release%relData,poolData)
    SkipRelease = poolData%mass == NOT_SET_R
  CASE DEFAULT
    nError = IV_ERROR
    eRoutine = 'SkipRelease'
    eMessage = 'Invalid release type'
    WRITE(eInform,'(A,I8)')'type =',release%type
END SELECT

IF( SkipRelease )THEN
  nError   = SKP_ERROR
  eRoutine = 'SkipRelease'
  eMessage = 'Release to be skipped'
  eInform  = 'Updated release indicates that it is valid but to be skipped'
  WRITE(lun_log,*,IOSTAT=ios)'********** Skipping Release at T=', &
                              GetSCIPUFFtime(),' rel='//TRIM(release%relDisplay)
END IF

RETURN
END
