INTEGER FUNCTION WriteSFCdata() RESULT( irv )

!------ Read data in temporary file and output SCICHEM SFC file

USE met2sci_fi
USE BLOCK2_IRND

IMPLICIT NONE

INTEGER nt, i
INTEGER iyr, imo, iday, ihr, jday, ih
INTEGER itotal, iopaq
REAL    lat, lon, z, elev
REAL    v, d !prate, p, cc, temp, rh, wspd, wdir

INTEGER, DIMENSION(22) :: sfobs

CHARACTER(1) c

INTEGER, EXTERNAL :: JULIAN

irv = FAILURE

!------ Setup for writing error

error%Number  = WR_ERROR
error%Routine = 'WriteSFCdata'
error%aString = 'Error writing SFC data'

!------ Get id, lat, lon (for the only station)

currentFile => SFClist%First

carg(1) = TRIM(currentfile%ID)

lat  = currentFile%lat
lon  = currentFile%lon
z    = currentFile%zref
elev = currentFile%elev

!------ Loop over data and set for SCICHEM format
!       Write data on line sequentially to handle possible missing values

DO 

  READ(lunScratch,2110,IOSTAT=ios) iyr,imo,iday,ihr,(sfobs(i),i=1,22),c
  IF( ios < 0 )THEN
    EXIT
  ELSE IF( ios > 0 )THEN
    error%Number = RD_ERROR
    error%Routine = 'WriteSFCdata'
    error%aString = 'Error reading temporary file with ISHD data'
    GOTO 9999
  END IF

!------ Write variables that are never 'missing'

  WRITE(lunOutSFC,2000,ADVANCE='NO',IOSTAT=ios,ERR=9999) TRIM(carg(1)),iyr,imo,iday,ihr,lat,lon,z
  IF( ios /= 0 )THEN
    error%Number  = WR_ERROR
    error%Routine = 'WriteSFCdata'
    error%aString = 'Error writing SFC data'
    GOTO 9999
  END IF

!------ Pressure

  IF( sfobs(3) /= 99999 )THEN
    v = FLOAT(sfobs(3))/10.
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Cloud cover; check opaque cover first and use if it's not missing

  itotal = sfobs(5) / 100
  iopaq  = sfobs(5) - itotal * 100

  IF( iopaq /= 99 )THEN                  !Use opaque
     v = iopaq / 10.
  ELSE IF( itotal /= 99 )THEN            !Use total
     v = itotal / 10.
  ELSE IF( sfobs(14) /= 99 )THEN         !Use ASOS
     v = sfobs(14) / 10.
  ELSE
    v = MISSING_R
  END IF

  IF( v /= MISSING_R )THEN
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Precipitation rate

  IF( sfobs(1) /= -9 )THEN
    v = FLOAT(sfobs(1)) / 100. !mm/hr ?
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Temperature

  IF( sfobs(17) /= 9999 )THEN
    v = FLOAT(sfobs(17)) / 10.
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Relative humidity

  IF( sfobs(20) /= 999 )THEN
    v = FLOAT(sfobs(20))
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Wind speed & direction
!       N.B. Set zero if one or the other is missing

  IF( sfobs(21) /= 999 .AND. sfobs(22) /= 999 )THEN
    d = FLOAT(sfobs(21)) * 10.
    v = FLOAT(sfobs(22)) / 10.
    IF( lRandSFC )THEN
      jday = JULIAN(iyr,imo,iday )
      ih = ihr + 1; IF( ih > 24 )ih = ih-24
      d = d + (IRND(ih,jday) - 4.0)*dRanSFC
      IF( d > 360.0) THEN
       d = d - 360.0
      ELSEIF (d < 0.0) THEN
        d = d + 360.0
      ENDIF
    END IF
   ELSE IF( sfobs(21) == 999 .AND. sfobs(22) == 999 )THEN
    d = MISSING_R
    v = MISSING_R
  ELSE
    d = 0.
    v = 0.
  END IF

  IF( d /= MISSING_R )THEN
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) d
    WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
  ELSE
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
    WRITE(lunOutSFC,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
  END IF

!------ Elevation

  IF( elev /= -9999. )WRITE(lunOutSFC,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) elev

!------ End-of-line

   WRITE(lunOutSFC,ADVANCE='YES',FMT='()',IOSTAT=ios)

END DO

!2000 FORMAT(A7,1X,3I2.2,1X,I3,1X,2F8.2,F8.2,F8.1,F8.1,F8.1,F8.1,F8.1,F8.1,F8.1)
2000 FORMAT(A7,1X,3I2.2,1X,I3,1X,2F8.2,F8.2,F8.1)
2001 FORMAT(1X,F8.1)
2002 FORMAT(4X,A)
2110 FORMAT(1X,4I2.2,4(1X,I5),6(1X,I5.5),/8X,5(1X,I5.5),7(1X,I5),2X,A1)    !Taken from RDISHD.FOR

error%Number = NO_ERROR
error%Routine = ''
error%aString = ''

irv = SUCCESS

9999 CONTINUE

RETURN
END


