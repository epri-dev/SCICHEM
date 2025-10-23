!***********************************************************************
!               LoadSen
!***********************************************************************
MODULE LoadSen_fi

  USE material_fd
  USE release_fd

  SAVE

  TYPE( materialT ),DIMENSION(:),ALLOCATABLE :: matList !Material list
  TYPE( releaseT  ),DIMENSION(:),ALLOCATABLE :: rlsList !Release list

  INTEGER nNull

END MODULE

!------------------------------------------------------------------------

INTEGER FUNCTION LoadSen( nRel,nMtl,senfile,RunTime )
! This function reduces the number of releases for a reverse project
USE files_fi
USE default_fd
USE SCIMgr_fd
USE LoadSen_fi

IMPLICIT NONE

INTEGER, INTENT( INOUT )                   :: nRel, nMtl
CHARACTER(256)                             :: senfile
TYPE( temporalT )                          :: RunTime

!--- Local list
INTEGER, PARAMETER                 :: lun_sen=12, NullType = 0
REAL,    PARAMETER                 :: OneSec = 1./3600.

INTEGER                            :: ios, alloc_stat
INTEGER                            :: indx, i, j, k
INTEGER                            :: iyr,imon,iday
INTEGER                            :: ihr,imin,isec
INTEGER                            :: jul_end, jul_start
REAL                               :: sig, cmass
REAL                               :: relsec, tstart, tend
CHARACTER(2)                       :: sat, mattyp
CHARACTER(14)                      :: TimeString
CHARACTER(256)                     :: line, string

INTEGER, DIMENSION(:), ALLOCATABLE :: iTimes
REAL,    DIMENSION(:), ALLOCATABLE :: Times

TYPE( messageT  )                  :: error
TYPE( matGasT   )                  :: mtlData
TYPE( matGenT )                    :: mtlDataGen
TYPE( relContT  )                  :: relDataC
TYPE( materialT )                  :: TmpMat
TYPE( releaseT  )                  :: Tmprel


INTEGER,EXTERNAL                   :: julian_day

LoadSen = SCIPfailure

! open sensor file
OPEN(lun_sen,FILE=TRIM(senfile),IOSTAT=ios)
IF( ios /= 0 )THEN
  error%iParm   = 999
  error%routine = 'LoadSen'
  error%aString = 'Error opening sensor file '//TRIM(senfile)
  GOTO 9999
END IF

nMtl  = 0
nRel  = 0
nNull = 0

DO

  READ(lun_sen,'(A)',IOSTAT=ios )line
  IF( ios < 0 )THEN
    EXIT
  ELSE IF( ios > 0 )THEN
    error%iParm   = 999
    error%routine = 'LoadSen'
    error%aString = 'Error reading sensor file'//TRIM(senfile)
    GOTO 9999
  END IF

  IF( LEN_TRIM(line) > 0. )THEN

    indx = INDEX(TRIM(line),'Type2Sensor')
    IF( indx > 0 )THEN
      READ(lun_sen,'(A)',IOSTAT=ios )line
      IF( ios < 0 )THEN
        EXIT
      ELSE IF( ios > 0 )THEN
        error%iParm   = 999
        error%routine = 'LoadSen'
        error%aString = 'Error reading sensor file'//TRIM(senfile)
        GOTO 9999
      END IF
      nMtl = nMtl + 1
    END IF

  END IF

END DO

nRel = nMtl

ALLOCATE( matList(nMtl),STAT=ios )
IF( ios /= 0 )THEN
  error%iParm   = 999
  error%routine = 'LoadSen'
  error%aString = 'Failed to allocate matList'
  GOTO 9999
END IF

mtlData%minConcentration = NOT_SET_R
mtlData%decayAmp         = 0.0
mtlData%decayMin         = 0.0
mtlData%rNotUsed         =  0.0
mtlData%save             = HS_GROUPDOS
mtlData%gasDensity       = 1.2
mtlData%gasDeposition    = 0.0
mtlData%padding          = 0

mtlDataGen = TRANSFER(mtlData,mtlDataGen)

DO i = 1,nMtl
  matList(i)%type          = NullType
  matList(i)%type          = IBSET(matList(i)%type,HMB_GAS)
  matList(i)%puffIndex     = NOT_SET_I
  matList(i)%iNotUsed(1)   = NOT_SET_I
  matList(i)%iNotUsed(2)   = NOT_SET_I
  matList(i)%matData       = mtlDataGen
  matList(i)%name          = ' '
  matList(i)%units         = 'kg'
  matList(i)%file          = ' '
  matList(i)%path          = ' '
END DO

ALLOCATE( rlsList(nRel),STAT=ios )
IF( ios /= 0 )THEN
  error%iParm   = 999
  error%routine = 'LoadSen'
  error%aString = 'Failed to allocate relList'
  GOTO 9999
END IF

relDataC%distribution = 1 ! subgroup
relDataC%rate         = 0.
relDataC%duration     = 0.
relDataC%sigY         = 0.
relDataC%sigZ         = 0.
relDataC%MMD          = NOT_SET_R
relDataC%sigma        = NOT_SET_R
relDataC%momentum     = 0.
relDataC%buoyancy     = 0.
relDataC%dryFrac      = NOT_SET_R
relDataC%activeFrac   = 1.
relDataC%nextUpdtTime = NOT_SET_R
relDataC%padding      = NOT_SET_I

DO i = 1,nRel
    rlsList(i)%type      = NOT_SET_I
    rlsList(i)%status    = HS_VALID
    rlsList(i)%tRel      = NOT_SET_R
    rlsList(i)%xRel      = NOT_SET_D
    rlsList(i)%yRel      = NOT_SET_D
    rlsList(i)%zRel      = NOT_SET_R
    rlsList(i)%notUsedA  = NOT_SET_R
    rlsList(i)%notUsedB  = NOT_SET_R
    rlsList(i)%notUsed   = NOT_SET_R
    rlsList(i)%relData    = TRANSFER(relDataC,rlsList(i)%relData)
    rlsList(i)%material   = ' '
    rlsList(i)%relName    = '<empty>'
    rlsList(i)%relDisplay = '<empty>'
END DO

ALLOCATE( Times(nMtl),STAT=ios )
IF( ios /= 0 )THEN
  error%iParm   = 999
  error%routine = 'SCIPStub'
  error%aString = 'Failed to allocate Times'
  GOTO 9999
END IF

ALLOCATE( iTimes(nMtl),STAT=ios )
IF( ios /= 0 )THEN
  error%iParm   = 999
  error%routine = 'SCIPStub'
  error%aString = 'Failed to allocate iTimes'
  GOTO 9999
END IF

REWIND(lun_sen)

i = 0

DO

  READ(lun_sen,'(A)',IOSTAT=ios )line
  IF( ios < 0 )THEN
    EXIT
  ELSE IF( ios > 0 )THEN
    error%iParm   = 999
    error%routine = 'LoadSen'
    error%aString = 'Error reading sensor file'//TRIM(senfile)
    GOTO 9999
  END IF

  IF( LEN_TRIM(line) > 0. )THEN

    indx = INDEX(TRIM(line),'Type2Sensor')
    IF( indx > 0 )THEN
      READ(lun_sen,'(A)',IOSTAT=ios )line
      IF( ios < 0 )THEN
        EXIT
      ELSE IF( ios > 0 )THEN
        error%iParm   = 999
        error%routine = 'LoadSen'
        error%aString = 'Error reading sensor file'//TRIM(senfile)
        GOTO 9999
      END IF
      i = i + 1
      mtlData  = TRANSFER(matList(i)%matData,mtlData)
      relDataC = TRANSFER(rlsList(i)%relData,relDataC)
      rlsList(i)%type = HR_CONT
      !-- relmat
      indx = INDEX (TRIM(line), ';')
      string = ADJUSTL(line(1:indx-1))
      READ(string,'(A)',IOSTAT=ios)rlsList(i)%material
      IF( ios /= 0 )EXIT
      matList(i)%name = rlsList(i)%material
      line = line(indx+1:)
      !-- xrel
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)rlsList(i)%xRel
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- yrel
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)rlsList(i)%yRel
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- zrel
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)rlsList(i)%zRel
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- TimeString
      indx = INDEX (TRIM(line), ';')
      string = ADJUSTL(line(1:indx-1))
      TimeString = string(1:14)
      IF( ios /= 0 )EXIT
      READ(TimeString(1:4),'(I4)')iyr
      READ(TimeString(5:6),'(I2)')imon
      READ(TimeString(7:8),'(I2)')iday
      READ(TimeString(9:10),'(I2)')ihr
      READ(TimeString(11:12),'(I2)')imin
      READ(TimeString(13:14),'(I2)')isec
      relsec    = (ihr*60. + imin)*60. + isec
      Times(i)  = -24.*FLOAT(julian_day( imon,iday,iyr )) - relsec/3600. ! Hrs
      iTimes(i) = i
      line = line(indx+1:)
      !-- Material type
      indx = INDEX (TRIM(line), ';')
      string = ADJUSTL(line(1:indx-1))
      READ(string,'(A)',IOSTAT=ios)MatTyp
      IF( ios /= 0 )EXIT
      IF( MatTyp(1:1) == 'N' .OR. MatTyp(1:1) == 'n' )nNull = nNull + 1
      line = line(indx+1:)
      !-- Conc Min
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)mtlData%minConcentration
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- Cmass
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)cmass
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- Sig
      indx = INDEX (TRIM(line), ';')
      READ(line(1:indx-1),*,IOSTAT=ios)sig
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- Saturation type
      indx = INDEX (TRIM(line), ';')
      string = ADJUSTL(line(1:indx-1))
      READ(string,'(A)',IOSTAT=ios)sat
      IF( ios /= 0 )EXIT
      line = line(indx+1:)
      !-- Tau
      READ(line,*,IOSTAT=ios)relDataC%duration
      IF( ios /= 0 )EXIT
      relDataC%duration = relDataC%duration/3600.

      IF( MatTyp(1:1)  == 'N' )THEN
        matList(i)%type = IBSET(matList(i)%type,HMB_NULLSENSOR)
        cmass           = 1.
      ELSE
        matList(i)%type = IBCLR(matList(i)%type,HMB_NULLSENSOR)
      END IF

      IF( sat(1:1)  == 'S' )THEN
        matList(i)%type = IBSET(matList(i)%type,HMB_SATSENSOR)
      ELSE
        matList(i)%type = IBCLR(matList(i)%type,HMB_SATSENSOR)
      END IF
      matList(i)%matData = TRANSFER(mtlData,matList(i)%matData)
      relDataC%rate      = 1./(cmass*relDataC%duration*3600.)
      relDataC%sigY      = sig
      relDataC%sigZ      = sig
      rlsList(i)%relData = TRANSFER(relDataC,rlsList(i)%relData)

    END IF

  END IF

END DO

IF( ios > 0 )THEN
  error%iParm   = 999
  error%routine = 'LoadSen'
  error%aString = 'Error reading sensor file'//TRIM(senfile)
  GOTO 9999
END IF

CALL vsorti(Times,iTimes,nMtl)

DO i = 1,nMtl
  j = iTimes(i)
  TmpMat = matList(i)
  TmpRel = rlsList(i)
  matList(i) = matList(j)
  rlsList(i) = rlsList(j)
  matList(j) = TmpMat
  rlsList(j) = TmpRel
  DO k = i+1,nMtl
    IF( iTimes(k) == i )THEN
      iTimes(k) = j
      EXIT
    END IF
  END DO
  IF( (Times(i) - Times(1)) < OneSec )THEN
    rlsList(i)%tRel = 0.
  ELSE
    rlsList(i)%tRel = Times(i) - Times(1)  ! + OneSec
  END IF
END DO

jul_start = IFIX(-1.*Times(1))/24.
CALL julian_ymd(jul_start,iyr,imon,iday)
tstart = (IFIX(Times(1)/24.) - Times(1)/24.)*24.

RunTime%START%TIME%YEAR  = iyr
RunTime%START%TIME%MONTH = imon
RunTime%START%TIME%DAY   = iday
RunTime%START%TIME%HOUR  = tstart

jul_end = jul_start + IFIX((tstart + RunTime%END%TIME%RUNTIME)/24.)
CALL julian_ymd(jul_end,iyr,imon,iday)
tend = MOD(tstart + RunTime%END%TIME%RUNTIME,24.)

RunTime%END%TIME%YEAR  = iyr
RunTime%END%TIME%MONTH = imon
RunTime%END%TIME%DAY   = iday
RunTime%END%TIME%HOUR  = tend

LoadSen = SCIPsuccess

GO TO 9998
9999 CONTINUE
LoadSen = SCIPfailure

IF( ALLOCATED(matList) )DEALLOCATE( matList,STAT=alloc_stat )
IF( ALLOCATED(rlsList) )DEALLOCATE( rlsList,STAT=alloc_stat )

9998 CONTINUE

IF( ALLOCATED(Times)  )DEALLOCATE( Times,STAT=alloc_stat )
IF( ALLOCATED(iTimes) )DEALLOCATE( iTimes,STAT=alloc_stat )

RETURN
END

