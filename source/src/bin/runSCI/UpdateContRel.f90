INTEGER FUNCTION UpdateContRel( update )

USE SCIPresults_fd
USE default_fd
USE update_fd
USE constants_fd
USE UpdateRelList_fi

IMPLICIT NONE

TYPE( updateRelT ), INTENT( INOUT ) :: update

TYPE( relContFileT   ) :: relData1
TYPE( relStackFileT  ) :: relData2
TYPE( relStack3FileT ) :: relData3

TYPE( UpdateRelList ), POINTER :: relList

INTEGER ios, irv
REAL    t, tx

REAL :: eps = 0.001
LOGICAL lNew

CHARACTER(PATH_MAXLENGTH) path, file

REAL, DIMENSION(5) :: relData

CHARACTER(PATH_MAXLENGTH) relFile

INTERFACE

  INTEGER FUNCTION ReadNextRel( t,relList )
    USE UpdateRelList_fi
    REAL, INTENT( IN ) :: t
    TYPE( UpdateRelList ), POINTER :: relList
  END FUNCTION ReadNextRel

  SUBROUTINE FindRel( nameRel,lNew,relList )
    USE UpdateRelList_fi
    CHARACTER(*), INTENT( IN  )    :: nameRel
    LOGICAL,      INTENT( OUT )    :: lNew
    TYPE( UpdateRelList ), POINTER :: relList
  END SUBROUTINE FindRel

END INTERFACE

UpdateContRel = SCIPFAILURE

IF( LEN_TRIM(update%release%relName) == 0 )THEN
  UpdateContRel = SCIPSUCCESS  !Don't update unnamed releases
  GOTO 9999
END IF

IF( update%release%type == HR_CONTF )THEN
  relData1 = TRANSFER(update%release%relData,relData1)
  relData  = (/relData1%rate,relData1%buoyancy,NOT_SET_R,NOT_SET_R,relData1%momentum/)
  relFile  = TRIM(relData1%relFile)
ELSE IF( update%release%type == HR_STACKF .OR. update%release%type == HR_STACKFP )THEN
  relData2 = TRANSFER(update%release%relData,relData2)
  relData  = (/relData2%rate,relData2%exitTemp,NOT_SET_R,NOT_SET_R,relData2%exitVel/)
  relFile  = TRIM(relData2%relFile)
ELSE IF( update%release%type == HR_STACK3F )THEN
  relData3 = TRANSFER(update%release%relData,relData3)
  relData  = (/relData3%rate,relData3%exitTemp,relData3%exitVel(1:3)/)
  relFile  = TRIM(relData3%relFile)
END IF

CALL FindRel( update%release%relName,lNew,relList )
IF( .NOT.ASSOCIATED(relList) )THEN
  update%release%relDisplay = 'Release not found'
  GOTO 9999
END IF

IF( update%release%type == HR_STACK3F )THEN
  relList%nData = 5
ELSE
  relList%nData = 3
END IF

IF( lNew )THEN

  relList%rateScale = relData(1)
  relList%buoyScale = relData(2)
  relList%umomScale = relData(3)
  relList%vmomScale = relData(4)
  relList%wmomScale = relData(5)

  relList%rateNext = relData(1)
  relList%buoyNext = relData(2)
  relList%umomNext = relData(3)
  relList%vmomNext = relData(4)
  relList%wmomNext = relData(5)

  relList%tNext = -HUGE(0.)
  relList%name  = TRIM(relFile)

  OPEN(UNIT=relList%fileUnit,FILE=TRIM(relFile),STATUS='OLD',ACTION='READ',IOSTAT=ios)
  IF( ios /= 0 )THEN
    UpdateContRel = SCIPFAILURE
    CALL SplitName( relList%name,file,path )
    update%release%relDisplay = 'Error opening '//TRIM(file)
    GOTO 9999
  END IF

END IF

t = update%currentTime  !Relative to release time
tx = t
IF( t > 0. )tx = t + eps

IF( tx > relList%tNext .AND. relList%fileUnit > 0 )THEN

  irv = ReadNextRel( tx,relList )  !Moves "tNext" values to current "t"
  IF( irv < 0 )THEN
    CALL SplitName( relList%name,file,path )
    update%release%relDisplay = 'Error reading file '//TRIM(file)
    GOTO 9999
  END IF

  IF( lNew )THEN

    IF( relList%tNext /= 0. )THEN
      CALL SplitName( relList%name,file,path )
      update%release%relDisplay = 'Missing data at t = 0 in '//TRIM(file)
      GOTO 9999
    END IF

    relList%t    = relList%tNext
    relList%rate = relList%rateNext
    relList%buoy = relList%buoyNext
    relList%umom = relList%umomNext
    relList%vmom = relList%vmomNext
    relList%wmom = relList%wmomNext

    irv = ReadNextRel( t+SMALL,relList )

  END IF

END IF

relData(1) = relList%rate
relData(2) = relList%buoy
relData(3) = relList%umom
relData(4) = relList%vmom
relData(5) = relList%wmom

IF( update%nextUpdate > relList%tNext )THEN
  tx = relList%tNext  !Only set tNext if next SCIPUFF update time is beyond next list time
ELSE
  tx = DEF_VAL_R
END IF

IF( update%release%type == HR_CONTF )THEN
  relData1%rate = relData(1); relData1%buoyancy = relData(2); relData1%momentum = relData(5)
  relData1%nextUpdtTime = tx
  update%release%relData = TRANSFER(relData1,update%release%relData)
ELSE IF( update%release%type == HR_STACKF .OR. update%release%type == HR_STACKFP )THEN
  relData2%rate = relData(1); relData2%exitTemp = relData(2); relData2%exitVel = relData(5)
  relData2%nextUpdtTime = tx
  update%release%relData = TRANSFER(relData2,update%release%relData)
ELSE IF( update%release%type == HR_STACK3F )THEN
  relData3%rate = relData(1); relData3%exitTemp = relData(2); relData3%exitVel(1:3) = relData(3:5)
  relData3%nextUpdtTime = tx
  update%release%relData = TRANSFER(relData3,update%release%relData)
END IF

UpdateContRel = SCIPSUCCESS

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION ReadNextRel( t,relList )

USE UpdateRelList_fi
USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: t
TYPE( UpdateRelList ), POINTER :: relList

INTEGER ios, lun

ReadNextRel = -1

lun = relList%FileUnit

!------ Read down until release time is beyond current time
!       N.B. Assumes time in hours

DO WHILE( relList%tNext < t )

  line = ' '
  READ(lun,'(A)',IOSTAT=ios) line
  IF( ios > 0 )THEN
    GOTO 9999
  ELSE IF( ios < 0 )THEN     !EOF: close file

    relList%FileUnit = -1
    ReadNextRel      = 0
    relList%tNext    = HUGE(0.)
    CLOSE( lun,IOSTAT=ios )

    relList%t    = relList%tNext
    relList%rate = relList%rateNext
    relList%buoy = relList%buoyNext
    relList%umom = relList%umomNext
    relList%vmom = relList%vmomNext
    relList%wmom = relList%wmomNext
    GOTO 9999

  END IF
  CALL get_next_data( 0,line,nch,kwrd,narg,carg,MAXN,lerr )
  IF( lerr )GOTO 9999

  IF( narg == 0 )CYCLE  !Skip blank lines

!------ Skip comments

  SELECT CASE( carg(1)(1:1) )
    CASE( '#','!','%','$','&','*' )
      CYCLE
  END SELECT

!------ Check if line is constructed properly:
!       Require time and mass rate;
!       All buoyancy & momenta must be given or not at all;
!       Do not allow extra input.

  IF( narg /= 2 .AND. narg /= relList%nData+1 )GOTO 9999

  relList%t    = relList%tNext
  relList%rate = relList%rateNext
  relList%buoy = relList%buoyNext
  relList%umom = relList%umomNext
  relList%vmom = relList%vmomNext
  relList%wmom = relList%wmomNext

!------ Read time, rate, etc.
!       N.B. Momenta and buoyancy held constant if not given here.

  READ(carg(1),*,IOSTAT=ios,ERR=9999) relList%tNext
  READ(carg(2),*,IOSTAT=ios,ERR=9999) relList%rateNext
  relList%rateNext = relList%rateNext * relList%rateScale
  IF( narg > 2 )THEN
    READ(carg(3),*,IOSTAT=ios,ERR=9999) relList%buoyNext
    relList%buoyNext = relList%buoyNext * relList%buoyScale
    IF( narg > 4 )THEN
      READ(carg(4),*,IOSTAT=ios,ERR=9999) relList%umomNext
      READ(carg(5),*,IOSTAT=ios,ERR=9999) relList%vmomNext
      READ(carg(6),*,IOSTAT=ios,ERR=9999) relList%wmomNext
      relList%umomNext = relList%umomNext * relList%umomScale
      relList%vmomNext = relList%vmomNext * relList%vmomScale
    ELSE
      READ(carg(4),*,IOSTAT=ios,ERR=9999) relList%wmomNext
    END IF
    relList%wmomNext = relList%wmomNext * relList%wmomScale
  END IF

END DO

ReadNextRel = 1

9999 CONTINUE

RETURN
END

!==============================================================================

SUBROUTINE FindRel( nameRel,lNew,relList )

USE UpdateRelList_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN  )    :: nameRel
LOGICAL,      INTENT( OUT )    :: lNew
TYPE( UpdateRelList ), POINTER :: relList

TYPE( UpdateRelList ), POINTER :: relListPrev

INTEGER, EXTERNAL :: GetNextFileUnit

lNew = .FALSE.

IF( .NOT.ASSOCIATED(FirstUpdateRel%Rel) )THEN

  lNew = .TRUE.
  ALLOCATE( FirstUpdateRel%Rel )

  relList => FirstUpdateRel%Rel

  NULLIFY( relList%next )

  relList%id = TRIM(nameRel)

  relList%tNext = -HUGE(0.)
  relList%fileUnit = GetNextFileUnit( lun_rel0 )
  IF( relList%fileUnit < 0 )THEN
    NULLIFY( relList )
    GOTO 9999
  END IF

ELSE

  relList => FirstUpdateRel%Rel
  relListPrev => relList
  DO WHILE( ASSOCIATED(relList) )
    IF( TRIM(relList%id) == TRIM(nameRel) )EXIT
    relListPrev => relList
    relList     => relList%next
  END DO
  IF( .NOT.ASSOCIATED(relList) )THEN
    lNew = .TRUE.
    ALLOCATE( relListPrev%next )
    relList    => relListPrev%next
    relList%id = TRIM(nameRel)
    NULLIFY(relList%next)
    relList%fileUnit = GetNextFileUnit( relListPrev%fileUnit+1 )
    IF( relList%fileUnit < 0 )THEN
      NULLIFY( relList )
      GOTO 9999
    END IF
  END IF

END IF

9999 CONTINUE

RETURN
END

!==============================================================================

INTEGER FUNCTION GetNextFileUnit( iun0 ) RESULT( iun )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iun0

INTEGER ios
LOGICAL lOpen

lOpen = .TRUE.
iun   = iun0-1
DO WHILE( lOpen )
  iun = iun + 1
  INQUIRE(UNIT=iun,OPENED=lOpen,IOSTAT=ios)
  IF( ios /= 0 )THEN
    iun = -1
    EXIT
  END IF
  IF( iun > 10000 )THEN
    iun = -1
    ELSE
  END IF
END DO

RETURN
END

!==============================================================================

SUBROUTINE ClearRelList()

USE UpdateRelList_fi

IMPLICIT NONE

INTEGER ios

TYPE( UpdateRelList ), POINTER :: rel, next

rel => FirstUpdateRel%Rel
DO WHILE( ASSOCIATED(rel) )
  next => rel%next
  IF( rel%fileUnit > 0 )CLOSE(UNIT=rel%fileUnit,IOSTAT=ios)
  DEALLOCATE( rel,STAT=ios )
  rel => next
END DO

RETURN
END
