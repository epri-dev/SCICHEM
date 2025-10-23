MODULE FINI
  CONTAINS
!==============================================================================
!==============================================================================
!Use Fortan emulatons of INI dll
!==============================================================================
!==============================================================================
!==============================================================================
! get_private_profile_string
!==============================================================================
  INTEGER FUNCTION get_private_profile_string ( lpAppName ,lpKeyName ,lpDefault ,&
                                               lpReturnedString ,nSize ,lpFileName )
  USE MaxLength_fd
  IMPLICIT NONE

  CHARACTER(*)            :: lpAppName,lpKeyName,lpDefault
  INTEGER                 :: nSize
  CHARACTER(*)            :: lpReturnedString,lpFileName

  INTEGER, PARAMETER      :: LUIN = 77
  CHARACTER(MAX_FILEF)    :: AppName, KeyName, FileName
  CHARACTER(MAX_FILEF)    :: Line,Buffer,SectionName
  INTEGER                 :: ios,nstart,nend,ncomment
  LOGICAL                 :: lrv, InSection

  CHARACTER(MAX_FILEF), EXTERNAL :: ADDNULL, STRIPNULL

  get_private_profile_string = nSize-1
  lpReturnedString = lpDefault


  FileName = STRIPNULL(lpFileName)
  AppName  = STRIPNULL(lpAppName)
  KeyName  = STRIPNULL(lpKeyName)

  CALL CUPPER( AppName )
  CALL CUPPER( KeyName )

  OPEN(UNIT=LUIN,FILE=TRIM(FileName),STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    WRITE(6,*)'Error opening '//TRIM(FileName)
    RETURN
  END IF

  ios         = 0
  InSection   = .false.
  SectionName = ' '
  DO WHILE ( ios == 0 )
    !==============================================================================
    ! Read a line
    !==============================================================================
    READ(UNIT=LUIN,IOSTAT=ios,FMT='(A)')Line
    IF( ios /= 0 )EXIT
    Buffer = Line
    CALL CUPPER( Buffer )
    lrv = Is_Section(Buffer, SectionName)
    IF (lrv) THEN
      IF (TRIM(SectionName) == TRIM(AppName)) THEN
        InSection = .true.
      ELSE
        InSection = .false.
      END IF
    END IF
    IF (InSection) THEN
      ncomment = INDEX(Buffer,';')
      IF (ncomment /=1) THEN
        nstart = INDEX(Buffer,'=')
        IF ((nstart > 1).and.(nstart>ncomment)) THEN
          IF (Buffer(1:nstart-1) == TRIM(KeyName)) THEN
            nend = LEN_TRIM(Buffer)
            IF ( nend > nstart) THEN
              lpReturnedString = ADDNULL(TRIM(Line(nstart+1:nend)))
              get_private_profile_string = LEN_TRIM(lpReturnedString)
              GO TO 999
            END IF
          END IF
        END IF
      END IF ! ncomment
    END IF ! InSection
  END DO

  lpReturnedString           = lpDefault
  get_private_profile_string = LEN_TRIM(lpDefault)

  999   CLOSE(LUIN)
  RETURN
  END

!==============================================================================
! get_private_profile_int
!==============================================================================
  INTEGER FUNCTION get_private_profile_int ( lpAppName ,lpKeyName ,nDefault ,lpFileName )

  USE MaxLength_fd

  IMPLICIT NONE

  CHARACTER(*)         :: lpAppName
  CHARACTER(*)         :: lpKeyName
  INTEGER              :: nDefault
  CHARACTER(*)         :: lpFileName
  CHARACTER(MAX_FILEF) :: lpDefault
  CHARACTER(MAX_FILEF) :: lpReturnedString
  INTEGER              :: nSize
  INTEGER              :: i
  CHARACTER(MAX_FILEF) :: StripNull, AddNull
  EXTERNAL             :: StripNull, AddNull

  WRITE(lpDefault,*) nDefault
  lpDefault = AddNull( TRIM(lpDefault) )
  nSize     = LEN(lpDefault)
  i = get_private_profile_string (lpAppName ,lpKeyName ,lpDefault ,&
                                  lpReturnedString ,nSize ,lpFileName  )
  lpReturnedString = ADJUSTL(StripNull(lpReturnedString))

  get_private_profile_int = 0
  DO i = 1,LEN_TRIM(lpReturnedString)
    IF( lpReturnedString(i:i) < '0' )EXIT
    IF( lpReturnedString(i:i) > '9' )EXIT
    get_private_profile_int = get_private_profile_int*10 &
                              + ICHAR(lpReturnedString(i:i))-48
  END DO

  RETURN
  END

!==============================================================================
! write_private_profile_string
!==============================================================================
  INTEGER FUNCTION write_private_profile_string ( lpAppName ,lpKeyName ,lpString ,lpFileName )

  USE winAPI_FD

  IMPLICIT NONE

  CHARACTER(*)   lpAppName
  CHARACTER(*)   lpKeyName
  CHARACTER(*)   lpString
  CHARACTER(*)   lpFileName

  write_private_profile_string = TRUE

  END

!==============================================================================
! delete_private_profile_string
!==============================================================================
  INTEGER FUNCTION delete_private_profile_string (lpAppName ,lpKeyName ,lpString ,lpFileName   )

  USE winAPI_FD

  IMPLICIT NONE

  CHARACTER(*)   lpAppName
  CHARACTER(*)   lpKeyName
  INTEGER        lpString
  CHARACTER(*)   lpFileName

  delete_private_profile_string = TRUE

  END

!==============================================================================
! delete_private_profile_section
!==============================================================================
  INTEGER FUNCTION delete_private_profile_section (lpAppName ,lpKeyName ,lpString ,lpFileName   )

  USE winAPI_FD

  IMPLICIT NONE

  CHARACTER(*)   lpAppName
  INTEGER        lpKeyName
  INTEGER        lpString
  CHARACTER(*)   lpFileName

  delete_private_profile_section = TRUE

  END
!==============================================================================
! Is_Section - support function
!==============================================================================
  LOGICAL FUNCTION Is_Section (Buffer, SectionName)

  USE MaxLength_fd
  CHARACTER(MAX_FILEF) :: Buffer, SectionName
  INTEGER              :: nstart, nend, ncomment

  Is_Section = .false.
  ncomment = INDEX(Buffer,';')
  IF (ncomment == 1) RETURN
  nstart = INDEX(Buffer,'[')
  IF ((nstart > 0).and.(nstart>ncomment)) THEN
    nend = INDEX(Buffer,']')
    IF (nend > nstart+1) THEN
      SectionName = TRIM(Buffer(nstart+1:nend-1))
      Is_Section  = .true.
    END IF
  END IF
  END

  END MODULE FINI

