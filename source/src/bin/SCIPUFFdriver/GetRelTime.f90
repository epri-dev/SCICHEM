REAL FUNCTION GetRelTime( iday,imonth,iyear,hour ) RESULT( t )

!------ Convert release date/hour to time-from-start (hours)

USE SCIPUFFdriver_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iday, imonth, iyear
REAL,    INTENT( IN ) :: hour

INTEGER iyy, jul, jyy

INTEGER, EXTERNAL :: julian_day, days_in_year

iyy = iyear; CALL SetYear( iyy )

jul  = julian_day( imonth,iday,iyy )

IF( iyy > year0 )THEN
  DO jyy = year0,iyy-1
    jul = jul + days_in_year( jyy )
  END DO
ELSE IF( iyy < year0 )THEN
  DO jyy = year0-1,iyy,-1
    jul = jul - days_in_year( jyy )
  END DO
END IF


t = FLOAT(jul - jul0)*24 + hour - hour0

RETURN
END

