INTEGER FUNCTION GetZrufFromWeather()

!------ Get surface roughness from AERMET files

USE SCIPUFFdriver_fi
USE default_fd

IMPLICIT NONE

INTEGER ios, i, n
INTEGER year, month, day, jday
REAL    sumLog, hour, H, ust, wstr, vptg, pbl, sbl, L

GetZrufFromWeather = FAILURE

sumLog = 0.
n = 0
landuse = 'NOT SET'

!------ Use first SFC file

DO i = 1,nMet

  IF( metInp(i)%type == MET_AERSRF )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    READ(lun_tmp,*,IOSTAT=ios)
    IF( ios /= 0 )EXIT
    DO                  !Average over entire file
      READ(lun_tmp,*,IOSTAT=ios) year,month,day,jday,hour,H,ust,wstr,vptg,pbl,sbl,L,zruf
      IF( ios > 0 )THEN
        WRITE(*,'(A)') 'Error reading met file '//TRIM(metInp(i)%file)
        GOTO 9999
      ELSE IF( ios < 0 )THEN
        EXIT
      END IF
      IF( zruf < 0. )THEN
        CYCLE !Assumed missing value
      ELSE IF( zruf == 0. )THEN
        WRITE(*,'(A)') 'Invalid surface roughness in met file '//TRIM(metInp(i)%file)
        WRITE(*,'(A)') 'Must be greater than zero or missing value'
        GOTO 9999
      END IF
      n = n + 1
      sumLog = sumLog + LOG(zruf)
    END DO

  ELSE IF( metInp(i)%type == MET_SCISRF )THEN

    OPEN(FILE=TRIM(metInp(i)%file),UNIT=lun_tmp,STATUS='OLD',ACTION='READ',IOSTAT=ios)
    IF( ios /= 0 )THEN
      WRITE(*,'(A)') 'Error opening met file '//TRIM(metInp(i)%file)
      GOTO 9999
    END IF

    DO
      CALL get_next_data( lun_tmp,line,nch,kwrd,narg,carg,MAXN,lerr )
      IF( carg(1)(1:1) /= '#' )EXIT
      IF( narg < 3 )CYCLE
      IF( carg(2)(1:7) == 'LANDUSE' )THEN
        landuse = TRIM(carg(3))
        EXIT
      END IF
    END DO

  END IF

END DO

IF( n == 0 )THEN
  IF( TRIM(landuse) == 'NOT SET' )THEN
    zruf = 0.1 !Default value
  ELSE
    zruf = NOT_SET_R
  END IF
ELSE
  zruf  = EXP(sumLog/FLOAT(n))
  lZruf = .TRUE.
END IF

GetZrufFromWeather = SUCCESS

9999 CONTINUE

CLOSE(UNIT=lun_tmp,IOSTAT=ios)

RETURN
END
