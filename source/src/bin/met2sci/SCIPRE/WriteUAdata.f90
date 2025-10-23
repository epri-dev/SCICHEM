INTEGER FUNCTION WriteUAdata() RESULT( irv )

!------ Read data in temporary file and output SCICHEM UA file

USE met2sci_fi
USE BLOCK2_IRND

IMPLICIT NONE

INTEGER nt, i
INTEGER iyr, imo, iday, ihr, nlev, jday, ih
INTEGER p, temp, wspd, wdir, z, dwpt
REAL    lat, lon, v, d, rh

INTEGER, DIMENSION(22) :: sfobs

CHARACTER(1) c

INTEGER, EXTERNAL :: JULIAN

irv = FAILURE

!------ Initialize for writing error

error%Number  = WR_ERROR
error%Routine = 'WriteUAdata'
error%aString = 'Error writing UA data'

!------ Get id, lat, lon (for the only station)

currentFile => UAlist%First

carg(1) = TRIM(currentfile%ID)

lat = currentFile%lat
lon = currentFile%lon

!------ Skip over header

DO
  READ(lunScratch,'(A)',IOSTAT=ios) carg(2)
  IF( ios /= 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'WriteUAdata'
    error%aString = 'Error skipping UA header section'
    error%bString = 'File=UA scratch'
    GOTO 9999
  END IF
  IF( carg(2)(1:1) /= '*' )THEN
    BACKSPACE(lunScratch,IOSTAT=ios)
    EXIT
  END IF
END DO

!------ Loop over data and set for SCICHEM format
!       Write data on line sequentially to handle possible missing values


DO 

  READ(lunScratch,2050,IOSTAT=ios) iyr,imo,iday,ihr,nlev
  IF( ios < 0 )THEN
    EXIT
  ELSE IF( ios > 0 )THEN
    error%Number  = RD_ERROR
    error%Routine = 'WriteUAdata'
    error%aString = 'Error reading UA scratch file'
    error%bString = 'Date record'
    GOTO 9999
  END IF

  WRITE(lunOutUA,2000,IOSTAT=ios,ERR=9999) 'ID:'//TRIM(carg(1)),lat,lon,iyr,imo,iday,ihr
  
  DO i = 1,nlev

    READ(lunScratch,2100,IOSTAT=ios) p,z,temp,dwpt,wdir,wspd
    IF( ios /= 0 )THEN
      error%Number  = WR_ERROR
      error%Routine = 'WriteUAdata'
      error%aString = 'Error reading FSL scratch file'
      error%bString = 'Data level record'
      GOTO 9999
    END IF

!------ Height and pressure are never missing

    WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) FLOAT(z)
    WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) FLOAT(p)/10.

!------ Wind speed & direction
!       N.B. Set zero if one or the other is missing

    IF( wdir /= 999 .AND. wspd /= 9990 )THEN
      d = FLOAT(wdir)
      v = FLOAT(wspd) / 10.
      IF( lRandUA )THEN
        jday = JULIAN(iyr,imo,iday )
        ih = ihr + 1; IF( ih > 24 )ih = ih-24
        d = d + (IRND(ih,jday) - 4.0)*dRanUA
        IF( d > 360.0) THEN
         d = d - 360.0
        ELSEIF (d < 0.0) THEN
          d = d + 360.0
        ENDIF
      END IF
    ELSE IF( wdir == 999 .AND. wspd == 9990 )THEN
      d = MISSING_R
      v = MISSING_R
    ELSE
      d = 0.
      v = 0.
    END IF

    IF( d /= MISSING_R )THEN
      WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) d
      WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
    ELSE
      WRITE(lunOutUA,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
      WRITE(lunOutUA,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
    END IF

!------ Temperature

    IF( temp /= 9990 )THEN
      v = FLOAT(temp) / 10.
      WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
    ELSE
      WRITE(lunOutUA,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
    END IF

!------ Relative humidity (use dewpoint)

    IF( temp /= 9990 .AND. dwpt /= -9990 )THEN
!    IF( -1000 < temp .AND. temp < 1000  .AND. &
!        -1000 < dwpt .AND. dwpt < 1000 )THEN
      v = FLOAT(temp) / 10.
      d = FLOAT(dwpt) / 10.
      CALL AERMET_HUMID( v,d,rh )
      WRITE(lunOutUA,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) rh
    ELSE
      WRITE(lunOutUA,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
    END IF

!------ End-of-line

   WRITE(lunOutUA,ADVANCE='YES',FMT='()',IOSTAT=ios)

  END DO
  
END DO

2000 FORMAT(A,1X,2F8.2,1X,3I2.2,1X,I3)
2001 FORMAT(1X,F8.1)
2002 FORMAT(4X,A)
2050 FORMAT( 1X,4I2.2,I5 )
2100 FORMAT( 6(1X,I6) )

error%Number = NO_ERROR
error%Routine = ''
error%aString = ''

irv = SUCCESS

9999 CONTINUE

RETURN
END


