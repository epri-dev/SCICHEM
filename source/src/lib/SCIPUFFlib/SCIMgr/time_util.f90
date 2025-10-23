!***********************************************************************
!               UTMtoLocal
!***********************************************************************
SUBROUTINE UTCtoLocal( Time,zone )

USE SCIMgr_fd
USE default_fd

!     converts timeT structures from UTC to local times

IMPLICIT NONE

TYPE( timeT ), INTENT( INOUT ) :: Time
REAL,          INTENT( IN    ) :: zone

INTEGER iday
REAL    tz, hour

INTEGER, EXTERNAL :: julian_day

IF( MIN(Time%month,Time%day,Time%year)> 0 )THEN

  iday = julian_day( Time%month,Time%day,Time%year )
  tz   = zone
  CALL fix_tzone( tz )
  hour = Time%hour + tz
  IF( hour > 24.0 )THEN
    hour = hour - 24.
    iday = iday + 1
  END IF
  IF( hour < 0.0 )THEN
    hour = hour + 24.
    iday = iday - 1
  END IF
  Time%hour = hour
  CALL julian_ymd( iday,Time%year,Time%month,Time%day )

ELSE

  iday = Time%day
  tz   = zone
  CALL fix_tzone( tz )
  hour = Time%hour + tz
  IF( hour < 0.0 .AND. iday /= NOT_SET_I )THEN
    hour = hour + 24.
    iday = iday - 1
  END IF
  IF( hour > 24.0 .AND. iday /= NOT_SET_I )THEN
    hour = hour - 24.
    iday = iday + 1
  END IF
  Time%day  = iday
  Time%hour = hour

END IF

RETURN
END
!***********************************************************************
!               LocaltoUTC
!***********************************************************************
SUBROUTINE LocaltoUTC( Time,zone )

USE SCIMgr_fd
USE default_fd

!     converts timeT structures from local to UTC times

IMPLICIT NONE

TYPE( timeT ), INTENT( INOUT ) :: Time
REAL,          INTENT( IN    ) :: zone

INTEGER iday
REAL    tz, hour

INTEGER, EXTERNAL :: julian_day

IF( MIN(Time%month,Time%day,Time%year)> 0 )THEN

  iday = julian_day( Time%month,Time%day,Time%year )
  tz   = zone
  CALL fix_tzone( tz )
  hour = Time%hour - tz
  IF( hour > 24.0 )THEN
    hour = hour - 24.
    iday = iday + 1
  END IF
  IF( hour < 0.0 )THEN
    hour = hour + 24.
    iday = iday - 1
  END IF
  Time%hour = hour
  CALL julian_ymd( iday,Time%year,Time%month,Time%day )

ELSE

  iday = Time%day
  tz   = zone
  CALL fix_tzone( tz )
  hour = Time%hour - tz
  IF( hour < 0.0 .AND. iday /= NOT_SET_I )THEN
    hour = hour + 24.
    iday = iday - 1
  END IF
  IF( hour > 24.0 .AND. iday /= NOT_SET_I )THEN
    hour = hour - 24.
    iday = iday + 1
  END IF
  Time%day  = iday
  Time%hour = hour

END IF

RETURN
END
!***********************************************************************
!               ComputeTime
!***********************************************************************
INTEGER FUNCTION ComputeTime( start,runtime,time )

USE SCIMgr_fd
USE default_fd

!     sets a timeT structure from a run time

IMPLICIT NONE

TYPE( startT ), INTENT( IN    ) :: start
TYPE( timeT ),  INTENT( INOUT ) :: time
REAL,           INTENT( IN    ) :: runtime

LOGICAL ymd
REAL    rtime

LOGICAL, EXTERNAL :: IsReverseMode

ComputeTime = SCIPfailure

time = start%time
time%runTime = runtime

ymd = MIN(time%year,time%month,time%day) > 0

rtime = runtime
IF( IsReverseMode() )rtime = -rtime

IF( ymd )THEN

  CALL year_month_day( rtime + start%time%hour,  &
                               start%time%year,  &
                               start%time%month, &
                               start%time%day,   &
                               time%hour,        &
                               time%year,        &
                               time%month,       &
                               time%day )

ELSE

  time%year  = NOT_SET_I
  time%month = NOT_SET_I
  time%day   = 0
  time%hour  = rtime + start%time%hour
  DO WHILE( time%hour >= 24.0 )
    time%hour = time%hour - 24.
    time%day  = time%day + 1
  END DO

END IF

IF( time%day <= 0 )time%day = NOT_SET_I

ComputeTime = SCIPsuccess

RETURN
END
!***********************************************************************
!               TimeToString
!***********************************************************************
INTEGER FUNCTION TimeToString( time,string )

USE SCIMgr_fd
USE default_fd

!     sets a timeT structure from a run time

IMPLICIT NONE

TYPE( timeT ), INTENT( IN  ) :: time
CHARACTER(*),  INTENT( OUT ) :: string

INTEGER ih, im, is, iyx, i, j

CHARACTER(1) tail

CHARACTER(36), PARAMETER :: MONTHS = 'JanFebMarAprMayJunJulAugSepOctNovDec'

TimeToString = SCIPfailure

IF( time%hour == NOT_SET_R )THEN
  string = 'Unspecified'
  RETURN
END IF

IF( time%reference == HT_LOCAL )THEN
  tail ='L'
ELSE
  tail ='Z'
END IF

ih = INT(time%hour)
IF( time%runTime > 1000.0 )THEN              !Set to 1000. to effectively remove
  im = NINT( 60.*(time%hour - FLOAT(ih)) )
  is = -999
  IF( im >= 60 )THEN
    ih = ih + 1
    im = im - 60
  END IF
ELSE
  im = INT( 60.*(time%hour - FLOAT(ih)) )
  is = NINT( 60.*(60.*(time%hour - FLOAT(ih)) - FLOAT(im)) )
  IF( is >= 60 )THEN
    im = im + 1
    is = is - 60
  END IF
  IF( im >= 60 )THEN
    ih = ih + 1
    im = im - 60
  END IF
END IF

IF( MIN(time%year,time%month,time%day) > 0 )THEN
  iyx = time%year - 100*(time%year/100)
  i = (time%month-1)*3 + 1
  j = i + 2
  IF( is >= 0 )THEN
    WRITE(string,'(I2.2,''-'',A,''-'',I2.2,'' '',I2.2,2('':'',I2.2),A)') &
         time%day,MONTHS(i:j),iyx,ih,im,is,tail
  ELSE
    WRITE(string,'(I2.2,''-'',A,''-'',I2.2,'' '',I2.2,('':'',I2.2),A)') &
         time%day,MONTHS(i:j),iyx,ih,im,tail
  END IF
ELSE
  IF( time%day >= 0 )THEN
    iyx = time%day
  ELSE
    iyx = 0
  END IF
  DO WHILE( ih >= 24 )
    iyx = iyx + 1
    ih = ih - 24
  END DO
  IF( is >= 0 )THEN
    WRITE(string,'(''Day '',I2.2,'' '',I2.2,2('':'',I2.2))') &
          iyx,ih,im,is
  ELSE
    WRITE(string,'(''Day '',I2.2,'' '',I2.2,('':'',I2.2))') &
          iyx,ih,im
  END IF
END IF

TimeToString = SCIPsuccess

RETURN
END
