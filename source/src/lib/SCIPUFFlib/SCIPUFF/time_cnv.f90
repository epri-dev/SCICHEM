SUBROUTINE time_cnv( ti,local_out,lymd_out,hour,min,sec,year,month,day,string )

USE scipuff_fi

!------ convert "time since start" to local or UTC
!       give year, month, day, hour, min, sec if appropriate

IMPLICIT NONE

REAL,         INTENT(  IN ) :: ti
LOGICAL,      INTENT(  IN ) :: local_out, lymd_out
INTEGER,      INTENT( OUT ) :: hour, min, sec, year, month, day
CHARACTER(*), INTENT( OUT ) :: string

CHARACTER(3), DIMENSION(12), PARAMETER ::  name_month = (/ &
                                            'JAN','FEB','MAR','APR','MAY','JUN', &
                                            'JUL','AUG','SEP','OCT','NOV','DEC' /)

CHARACTER(1) symbol, day_thing

REAL tt, tx, rmin

LOGICAL, EXTERNAL :: IsReverseMode

IF( IsReverseMode() )THEN
  tt = -ti + 0.5  !Add 1/2 second for rounding
ELSE
  tt = ti + 0.5
END IF

IF( local_out )THEN
  IF( local )THEN
    tt = tt/3600. + tstart
  ELSE
    tt = tt/3600. + tstart + tzone
  END IF
  symbol = 'L'
ELSE
  IF( local )THEN
    tt = tt/3600. + tstart - tzone
  ELSE
    tt = tt/3600. + tstart
  END IF
  symbol = 'Z'
END IF

IF( lymd_out )THEN

  IF( year_start == NOT_SET_I .OR. month_start == NOT_SET_I .OR. &
                          day_start == NOT_SET_I )THEN
    nError   = UK_ERROR
    eRoutine ='time_cnv'
    eMessage ='Cannot output time in year-month-day format'
    eInform  ='Start time must be set in year-month-day format'
    GOTO 9999
  END IF

  CALL year_month_day( tt,year_start,month_start,day_start,tx,year,month,day )

  IF( year >= 2000 )THEN
    year = year - 2000
  ELSE IF( year >= 1900 )THEN
    year = year - 1900
  END IF

  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  sec  = INT(60.*(rmin-FLOAT(min)))

  WRITE(string,100) day,name_month(month),year,hour,min,sec,symbol
100 FORMAT(I2.2,'-',A3,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2,A1)

ELSE

  year  = NOT_SET_I
  month = NOT_SET_I

  day  = INT(tt/24.)
  tx   = tt - FLOAT(day)*24.
  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  sec  = INT(60.*(rmin-FLOAT(min)))

  IF( day == 0 )THEN
    day_thing ='-'
  ELSE
    day_thing ='+'
  END IF

  IF( day < 10 )THEN
    WRITE(string,101) day_thing,day,hour,min,sec,symbol
  ELSE IF( day < 100 )THEN
    WRITE(string,102) day_thing,day,hour,min,sec,symbol
  ELSE IF( day < 1000 )THEN
    WRITE(string,103) day_thing,day,hour,min,sec,symbol
  END IF

101 FORMAT('DAY',A1,I1.1,' ',I2.2,':',I2.2,':',I2.2,A1)
102 FORMAT('DAY',A1,I2.2,' ',I2.2,':',I2.2,':',I2.2,A1)
103 FORMAT('DAY',A1,I3.3,' ',I2.2,':',I2.2,':',I2.2,A1)

END IF

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE time_cnv_sampler( ti,local_out,lymd_out,hour,min,sec,year,month,day,string )

USE scipuff_fi

!------ convert "time since start" to local or UTC
!       give year, month, day, hour, min, sec if appropriate
!       **** Special version for EPRI sampler file ****

IMPLICIT NONE

REAL,         INTENT(  IN ) :: ti
LOGICAL,      INTENT(  IN ) :: local_out, lymd_out
INTEGER,      INTENT( OUT ) :: hour, min, sec, year, month, day
CHARACTER(*), INTENT( OUT ) :: string

CHARACTER(7) symbol
INTEGER      ihr, imin

REAL tt, tx, rmin, tzonx

LOGICAL, EXTERNAL :: IsReverseMode

IF( IsReverseMode() )THEN
  tt = -ti + 0.5  !Add 1/2 second for rounding
ELSE
  tt = ti + 0.5
END IF

IF( tzone == NOT_SET_R .OR. tzone == DEF_VAL_R )THEN
  tzonx = 0.
ELSE
  tzonx = tzone
END IF

ihr = INT(tzonx)
imin = (tzonx - FLOAT(ihr))*60

IF( local_out )THEN
  IF( local )THEN
    tt = tt/3600. + tstart
  ELSE
    tt = tt/3600. + tstart + tzonx
  END IF
  symbol(1:1) = 'L'
ELSE
  IF( local )THEN
    tt = tt/3600. + tstart - tzonx
  ELSE
    tt = tt/3600. + tstart
  END IF
  IF( tzone == NOT_SET_R .OR. tzone == DEF_VAL_R )THEN
    symbol(1:1) = 'L'
  ELSE
    symbol(1:1) = 'Z'
  END IF
END IF
IF( ihr < 0 )THEN
  symbol(2:2) = '-'
  ihr = -ihr
ELSE
  symbol(2:2) = '+'
END IF
WRITE(symbol(3:7),'(I2.2,A1,I2.2)') ihr,':',imin

IF( lymd_out )THEN

  IF( year_start == NOT_SET_I .OR. month_start == NOT_SET_I .OR. &
                          day_start == NOT_SET_I )THEN
    nError   = UK_ERROR
    eRoutine ='time_cnv_sampler'
    eMessage ='Cannot output time in year-month-day format'
    eInform  ='Start time must be set in year-month-day format'
    GOTO 9999
  END IF

  CALL year_month_day( tt,year_start,month_start,day_start,tx,year,month,day )

  IF( year < 50 )THEN
    year = year + 2000
  ELSE IF( year < 100 )THEN
    year = year + 1900
  END IF

  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  sec  = INT(60.*(rmin-FLOAT(min)))

ELSE

  year  = 00
  month = 00

  day  = INT(tt/24.)
  tx   = tt - FLOAT(day)*24.
  hour = INT(tx)
  rmin = 60.*(tx-FLOAT(hour))
  min  = INT(rmin)
  sec  = INT(60.*(rmin-FLOAT(min)))

END IF

WRITE(string,100) year,month,day,hour,min,sec,symbol
100 FORMAT(I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2,A7)

9999 CONTINUE

RETURN
END

!===============================================================================

SUBROUTINE time_message( cmsg,tx )

USE scipuff_fi

IMPLICIT NONE

REAL,          INTENT( IN  ) :: tx
CHARACTER(80), INTENT( OUT ) :: cmsg

CHARACTER(32) ctem

LOGICAL local_out, lymd_out

INTEGER hour, min, sec, year, month, day
INTEGER nch, nch1

local_out = local
lymd_out  = lymd
CALL time_cnv( tx,local_out,lymd_out,hour,min,sec,year,month,day,ctem )

cmsg = ctem
nch1 = LEN_TRIM(cmsg)

IF( tx == 0. .OR. ABS(tx) > 1800. )THEN
  CALL c_format( tx/3600.,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' hr)'
ELSE IF( ABS(tx) > 60. )THEN
  CALL c_format( tx/60.,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' min)'
ELSE
  CALL c_format( tx,nch,ctem )
  WRITE(cmsg,'(A)') cmsg(1:nch1)//' ('//ctem(1:nch)//' sec)'
END IF

RETURN
END
