INTEGER FUNCTION GenSCIPUFFWeather()

!------ Generate SCIPUFF weather input, i.e. a list of AERMET files

USE SCIPUFFdriver_fi

IMPLICIT NONE

CHARACTER(1), PARAMETER :: QQ = CHAR(34)  !Double quote

INTEGER ios, irv, i
LOGICAL lTer, lexist

INTEGER, EXTERNAL :: GetZrufFromWeather, SYSCOPYFILE

GenSCIPUFFWeather = FAILURE

IF( nMet == 0 )THEN
  WRITE(*,'(A)') 'No met input files defined '
  GOTO 9999
END IF

!------ Default w/o terrain

lTer = .FALSE.

!------ Setup list input

new%weather%met%type = HW_METASSIM

new%weather%met%input(1) = TRIM(new%project%name)//'.lis'

OPEN(FILE=TRIM(new%weather%met%input(1)),UNIT=lun_met,STATUS='UNKNOWN',ACTION='WRITE', &
                                                                             IOSTAT=ios)
IF( ios /= 0 )THEN
  WRITE(*,'(A)') 'Error opening SCIPUFF met input file'
  WRITE(*,'(A)') 'File name = '//TRIM(new%weather%met%input(1))
  GOTO 9999
END IF

!------ First check for "list" input which supersedes all others

DO i = 1,nMet
  IF( metInp(i)%type == MET_SCILIS )THEN

    INQUIRE( FILE=TRIM(metInp(i)%file), EXIST=lexist )
    IF( lexist )THEN
      CLOSE(lun_met,IOSTAT=ios)
      irv = SYSCOPYFILE( TRIM(metInp(i)%file),TRIM(new%weather%met%input(1)) )
      IF( irv /= SUCCESS )THEN
        WRITE(*,'(A)') 'Error copying SCIPUFF met list file '//TRIM(metInp(i)%file)
        GOTO 9999
      END IF
    END IF

    GOTO 1000 !Skip reading other met input (if any)

  END IF
END DO

!------ Write header

WRITE(lun_met,FMT='(A)',IOSTAT=ios) 'SCIPUFF_LIST' !Synonym for ASSIM

!------ Loop over met files

DO i = 1,nMet

  SELECT CASE( metInp(i)%type )
    CASE( MET_AERSRF )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'AERSFC '
    CASE( MET_AERPRF )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'AERPFL '
    CASE( MET_SCISRF )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'SFC '
    CASE( MET_SCIPRF )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'PRF '
    CASE( MET_SCIFIX )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'FIXED '
    CASE( MET_SCIGRD )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'MEDOC '
    CASE( MET_MEDLIS )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'MEDOC_LIST '
    CASE( MET_ASOS1M )
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'ASOS1MIN '
    CASE( MET_SCITER )
      lTer = .TRUE.
      WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) 'TER '
  END SELECT

  WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) QQ//TRIM(metInp(i)%file)//QQ

  IF( metInp(i)%type == MET_AERSRF .OR. metInp(i)%type == MET_AERPRF )THEN
    IF( metInp(i)%staNum /= NOT_SET_I )THEN
      WRITE(lun_met,FMT='(2I8)',ADVANCE='NO',IOSTAT=ios) metInp(i)%staNum, metInp(i)%year
      IF( TRIM(metInp(i)%name) /= 'NOT_SET' ) &
          WRITE(lun_met,FMT='(A)',ADVANCE='NO',IOSTAT=ios) ' '//TRIM(metInp(i)%name)
      IF( metInp(i)%x /= NOT_SET_R ) &
          WRITE(lun_met,FMT='(2ES14.5)',ADVANCE='NO',IOSTAT=ios) metInp(i)%x, metInp(i)%y  !lon,lat for such projects  ** but ignored **
      IF( metInp(i)%baseElev /= NOT_SET_R ) &
          WRITE(lun_met,FMT='(A,ES14.5)',ADVANCE='NO',IOSTAT=ios) ' BASELEV ', metInp(i)%baseElev
    END IF
  ELSE IF( metInp(i)%type == MET_ASOS1M )THEN
    IF( metInp(i)%x /= NOT_SET_R .AND. metInp(i)%y /= NOT_SET_R )THEN
      WRITE(lun_met,FMT='(2ES14.5)',ADVANCE='NO',IOSTAT=ios) metInp(i)%y, metInp(i)%x  !lat, lon
    END IF
  END IF

  WRITE(lun_met,'()',ADVANCE='YES')

END DO

IF( .NOT.lZruf )THEN
  irv = GetZrufFromWeather() !Get roughness
  IF( irv /= SUCCESS )GOTO 9999
END IF

IF( zruf == NOT_SET_R )THEN
  new%weather%bl%landuse = TRIM(landuse)
ELSE
  new%weather%bl%roughness = zruf
END IF
new%weather%bl%canopy = NOT_SET_R

!------ Check for terrain

IF( lTer )THEN

  IF( nvertlev < 1 )THEN  !Set default levels

    new%weather%terrain%mc%nz = 23

    new%weather%terrain%mc%z(1:23) = (/ &
      5.00000E+01,    1.50000E+02,    2.61430E+02,    3.85414E+02,    5.23165E+02, &
      6.75995E+02,    8.45317E+02,    1.03265E+03,    1.23962E+03,    1.46799E+03, &
      1.71960E+03,    1.99644E+03,    2.30062E+03,    2.63436E+03,    3.00000E+03, &
      3.40000E+03,    3.89262E+03,    4.50354E+03,    5.26670E+03,    6.22712E+03, &
      7.44483E+03,    9.00000E+03,    1.10000E+04 /)

  ELSE

    new%weather%terrain%mc%nz = nvertlev

  END IF

  new%weather%terrain%mc%alpha(1)   = 0.
  new%weather%terrain%mc%maxIter(1) = 300

END IF

1000 CONTINUE

GenSCIPUFFWeather = SUCCESS

9999 CONTINUE

CLOSE(UNIT=lun_met,IOSTAT=ios)

RETURN
END
