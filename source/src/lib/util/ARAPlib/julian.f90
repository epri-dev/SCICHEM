INTEGER FUNCTION julian_day( imonth,iday,iyear )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: imonth,iday,iyear

!  this function converts year, month and day to julian date

INTEGER jday,kday

INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
                      (/  0,31,59,90,120,151,181,212,243,273,304,334 /)
INTEGER, DIMENSION(12), PARAMETER :: MDAY = &
                      (/ 31,28,31,30, 31, 30, 31, 31, 30, 31, 30, 31 /)

LOGICAL, EXTERNAL :: leap_year

IF( (imonth <= 0) .OR. (imonth >= 13) )THEN

  jday = -999

ELSE

  jday = NDAY(imonth) + iday
  kday = MDAY(imonth)

  IF( leap_year(iyear) )THEN
    IF( imonth > 2 )THEN
      jday = jday + 1
    ELSE IF( imonth == 2 )THEN
      kday = kday + 1
    END IF
  END IF

  IF( iday > kday )jday = -999

END IF

julian_day = jday

RETURN
END

!=============================================================================

LOGICAL FUNCTION leap_year( iyear )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: iyear

IF( MOD(iyear,4) /= 0 )THEN
  leap_year = .FALSE.
ELSE
  IF( MOD(iyear,100) == 0 )THEN
    leap_year = MOD(iyear,400) == 0
  ELSE
    leap_year = .TRUE.
  END IF
END IF

RETURN
END

!=============================================================================

SUBROUTINE julian_ymd( jday,yr,mnth,day )

IMPLICIT NONE

INTEGER, INTENT( INOUT ) :: jday, yr
INTEGER, INTENT( OUT   ) :: mnth ,day

INTEGER, PARAMETER :: DAYS_YEAR = 365
INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
                      (/ 0,31,59,90,120,151,181,212,243,273,304,334 /)

LOGICAL leap
INTEGER days

LOGICAL, EXTERNAL :: leap_year

leap = leap_year( yr )
days = DAYS_YEAR
IF( leap )days = days + 1

DO WHILE( jday > days )
  jday = jday - days
  yr   = yr + 1
  leap = leap_year( yr )
  days = DAYS_YEAR
  IF( leap )days = days + 1
END DO

DO WHILE( jday <= 0 )
  yr   = yr - 1
  leap = leap_year( yr )
  days = DAYS_YEAR
  IF( leap )days = days + 1
  jday = jday + days
END DO

DO mnth = 12,1,-1
  IF( mnth >= 3 .AND. leap )THEN
    days = NDAY(mnth) + 1
  ELSE
    days = NDAY(mnth)
  END IF
  IF( jday > days )EXIT
END DO

day = jday - days

RETURN
END

!===============================================================================

INTEGER FUNCTION days_in_year( yr )

IMPLICIT NONE

INTEGER, INTENT( IN ) :: yr

LOGICAL, EXTERNAL :: leap_year

IF( leap_year(yr) )THEN
  days_in_year = 366
ELSE
  days_in_year = 365
END IF

RETURN
END
!=============================================================================

SUBROUTINE DateTimeShift( tshift,ti,yr,mnth,day,to,yro,mntho,dayo )

IMPLICIT NONE

INTEGER, INTENT( IN  ) :: yr ,mnth ,day
REAL,    INTENT( IN  ) :: tshift, ti
INTEGER, INTENT( OUT ) :: yro,mntho,dayo
REAL,    INTENT( OUT ) :: to

INTEGER days, iday, jul, julo
REAL    t
LOGICAL leap

INTEGER, EXTERNAL :: julian_day
LOGICAL, EXTERNAL :: leap_year

INTEGER, DIMENSION(12), PARAMETER :: NDAY = &
           (/0,31,59,90,120,151,181,212,243,273,304,334/)

t = ti + tshift

iday = INT(t/24.)
to   = t - 24.*FLOAT(iday)
IF( to < 0. )THEN
  iday = iday - 1
  to = to + 24.
END IF

jul  = julian_day( mnth,day,yr )
julo = jul + iday

yro = yr

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo <= 0 )
  yro  = yro - 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
  julo = julo + days
END DO

leap = leap_year( yro )
IF( leap )THEN
  days = 366
ELSE
  days = 365
END IF

DO WHILE( julo > days )
  julo = julo - days
  yro  = yro + 1
  leap = leap_year( yro )
  IF( leap )THEN
    days = 366
  ELSE
    days = 365
  END IF
END DO

DO mntho = 12,1,-1
  IF( mntho >= 3 .AND. leap )THEN
    days = NDAY(mntho) + 1
  ELSE
    days = NDAY(mntho)
  END IF
  IF( julo > days )EXIT
END DO

dayo = julo - days

RETURN
END
