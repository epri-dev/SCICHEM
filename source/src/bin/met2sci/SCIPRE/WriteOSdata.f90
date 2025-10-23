INTEGER FUNCTION WriteOSdata() RESULT( irv )

!------ Output ONSITE data in SCICHEM profile file
!       Assumes file is positioned after header section
!       Get values from AERMET arrays (accounting for missing data)

USE met2sci_fi
USE BLOCK2_IRND

IMPLICIT NONE

REAL, PARAMETER :: PSURF = 1013.25       !mb (Standard Atmosphere)

INTEGER nt, i, k, ih
INTEGER iyr, imo, iday, ihr, imn , nlev, jday, tzone
REAL    lat, lon, elev, v, p, p0, pr, pr0, z, t, tz
LOGICAL lExtract

INTEGER, EXTERNAL :: JULIAN

irv = FAILURE

!------ Check if within start/end dates; exit if not

CALL CheckDate( lExtract )
IF( .NOT.lExtract )THEN
  irv = SUCCESS
  GOTO 9999
END IF

!------ Initialize for writing error

error%Number  = WR_ERROR
error%Routine = 'WriteOSdata'
error%aString = 'Error writing ONSITE data'

!------ Get id, lat, lon (for the only station)

currentFile => OSlist%First

carg(1) = TRIM(currentfile%ID)

lat = currentFile%lat
lon = currentFile%lon

!------ Get date & time

CALL GetTzone( tzone ) 
CALL GetDate( iyr,imo,iday )
CALL GetHour( ihr,imn )
CALL GMTLST( iyr,imo,iday,ihr,tzone )

!------ Write fixed data

IF( lOSHGT )THEN
  WRITE(lunOutOS,2000,ADVANCE='NO',IOSTAT=ios,ERR=9999) 'ID:'//TRIM(carg(1)),lat,lon,iyr,imo,iday,FLOAT(ihr)+FLOAT(imn)/60.
ELSE
  WRITE(lunOutOS,2000,ADVANCE='NO',IOSTAT=ios,ERR=9999) TRIM(carg(1)),lat,lon,iyr,imo,iday,FLOAT(ihr)+FLOAT(imn)/60.
END IF
IF( ios /= 0 )THEN
  error%Number  = WR_ERROR
  error%Routine = 'WriteSFCdata'
  error%aString = 'Error writing SFC data'
  GOTO 9999
END IF

!------ Write surface data

ihr = 1  !OSSWAP is always called with hour 1 in ExtractOSdata

IF( nvars > 0 )THEN

  DO i = 1,nvars
    SELECT CASE( OSstype(i) )
      CASE( OSHFLX )
        CALL GetSurfVar( 'HFLX',ihr,v )
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      CASE( OSUSTR )
        CALL GetSurfVar( 'USTR',ihr,v )
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      CASE( OSMHGT )
        CALL GetSurfVar( 'MHGT',ihr,v )
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      CASE( OSZ0HT )
        CALL GetSurfVar( 'Z0HT',ihr,v )
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      CASE( OSPRES )
      CASE( OSSKY )
        CALL GetSurfVar( 'SKY',ihr,v )
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) MIN(v,1.)
    END SELECT
  END DO

END IF

IF( lOSHGT )WRITE(lunOutOS,ADVANCE='YES',FMT='()',IOSTAT=ios)

IF( iPress > 0 )THEN
  elev = currentFile%elev
  CALL GetSurfVar( 'PRES',ihr,p )
  IF( lOSHGT )THEN
    CALL GetPrfVar( 'HGT',1,ihr,z )
    IF( iPressSL > 0 )THEN
      CALL GetSurfVar( 'SLVP',ihr,p0 )
    ELSE
      p0 = NOT_SET_R
    END IF
    IF( p0 == NOT_SET_R )p0 = PSURF
    CALL StandardAtmosphere( elev+z,t,tz,pr )
    pr0 = (p/p0) / pr
  END IF
END IF

!------ Write profile data

IF( nvarp > 0 )THEN

  CALL GetNlev( nlev )
  DO k = 1,nlev

    IF( lOSHGT .AND. .NOT.lOSHTX )THEN
      CALL GetPrfVar( 'HGT',k,ihr,v )
      z = v        
      WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
    END IF

    DO i = 1,nvarp
      SELECT CASE( OSptype(i) )
        CASE( OSHGT )
          CALL GetPrfVar( 'HGT',k,ihr,v )
          z = v        
        CASE( OSTEMP )
          CALL GetPrfVar( 'TEMP',k,ihr,v )
        CASE( OSWDIR )
          CALL GetPrfVar( 'WDIR',k,ihr,v )
          IF( lRandOS .AND. v /= NOT_SET_R )THEN
            jday = JULIAN(iyr,imo,iday )
            ih = ihr + 1; IF( ih > 24 )ih = ih-24
            v = v + (IRND(ih,jday) - 4.0)*dRanOS
            IF( v > 360.0) THEN
             v = v - 360.0
            ELSEIF (v < 0.0) THEN
              v = v + 360.0
            ENDIF
          END IF
        CASE( OSWSPD )
          CALL GetPrfVar( 'WSPD',k,ihr,v )
        CASE( OSRH )
          CALL GetPrfVar( 'RH',k,ihr,v )
        CASE DEFAULT
          CYCLE
      END SELECT
      IF( v == NOT_SET_R )THEN
        WRITE(lunOutOS,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
      ELSE
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      END IF
    END DO

    IF( iPress > 0 )THEN
      IF( p == NOT_SET_R )THEN
        WRITE(lunOutOS,2002,ADVANCE='NO',IOSTAT=ios,ERR=9999) MISSING_C
      ELSE IF( lOSHGT )THEN
        CALL StandardAtmosphere( elev+z,t,tz,pr )
        v = pr*pr0*p0
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) v
      ELSE
        WRITE(lunOutOS,2001,ADVANCE='NO',IOSTAT=ios,ERR=9999) P
      END IF
    END IF

    WRITE(lunOutOS,ADVANCE='YES',FMT='()',IOSTAT=ios)

  END DO

END IF

2000 FORMAT(A,1X,2F8.2,1X,3I2.2,1X,F8.4)
2001 FORMAT(1X,ES12.4)
2002 FORMAT(1X,A12)

CALL ClearError()

irv = SUCCESS

9999 CONTINUE

RETURN
END
