INTEGER FUNCTION GetDomRefFromWeather()

!------ Get local Cartesian reference location from met station

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER ios, i, j
LOGICAL lLat, lLon

CHARACTER(8), DIMENSION(2) :: string

GetDomRefFromWeather = FAILURE

IF( nMet == 0 )THEN
  WRITE(*,'(A)') 'No met input files defined'
  GOTO 9999
END IF

!------ Use first SFC file

DO i = 1,nMet

  IF( metInp(i)%type == MET_AERSRF )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_met,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

!------ Read lat/lon from first record

    READ(lun_met,FMT='(2(2X,A8))',IOSTAT=ios,ERR=9999) string
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error reading lat/lon from '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    CALL cupper( string(1) )
    CALL cupper( string(2) )

    j = LEN_TRIM(string(1))
    READ(string(1)(1:j-1),*,IOSTAT=ios,ERR=9999) new%input%domain%reference%lat
    IF( string(1)(j:j) == 'W' )new%input%domain%reference%lat = -new%input%domain%reference%lat

    j = LEN_TRIM(string(2))
    READ(string(2)(1:j-1),*,IOSTAT=ios,ERR=9999) new%input%domain%reference%lon
    IF( string(2)(j:j) == 'W' )new%input%domain%reference%lon = -new%input%domain%reference%lon

    lDomRef = .TRUE.

  ELSE IF( metInp(i)%type == MET_SCISRF )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    lLat = .TRUE.
    lLon = .TRUE.

    DO
      CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,MAXN,lerr )
      IF( carg(1)(1:1) /= '#' )EXIT
      IF( narg < 3 )CYCLE
      IF( carg(2)(1:8) == 'LATITUDE' )THEN
        READ(carg(3),*,IOSTAT=ios,ERR=9999) new%input%domain%reference%lat
        lLat = .TRUE.
      ELSE IF( carg(2)(1:9) == 'LONGITUDE' )THEN
        READ(carg(3),*,IOSTAT=ios,ERR=9999) new%input%domain%reference%lon
        lLon = .TRUE.
      END IF
    END DO

    lDomRef = lLat .AND. lLon

  END IF

END DO

IF( .NOT.lDomRef )THEN
  WRITE(*,'(A)') 'Cannot set domain reference lat/lon'
  WRITE(*,'(A)') 'Set projection in CO pathway'
  GOTO 9999
END IF

GetDomRefFromWeather = SUCCESS

9999 CONTINUE

CLOSE(UNIT=lun_met,IOSTAT=ios)

RETURN
END
