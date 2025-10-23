SUBROUTINE ComputeDurationAERMOD( start,end )

USE tooluser_fd
USE default_fd
USE param_fd

IMPLICIT NONE

TYPE( timeT ), INTENT( IN    ) :: start
TYPE( timeT ), INTENT( INOUT ) :: end

INTEGER i
REAL    tim

INTEGER, EXTERNAL :: julian_day
LOGICAL, EXTERNAL :: check_YMD

end%runTime = NOT_SET_R

IF( start%hour == NOT_SET_R .OR. end%hour == NOT_SET_R )GOTO 9999

IF( check_YMD(start) )THEN

  IF( check_YMD(end) )THEN
    IF( start%year > end%year .OR. julian_day(start%month,start%day,start%year) == -999 .OR. &
                                   julian_day(end%month,end%day,end%year) == -999 )THEN
      GOTO 9999
    ELSE IF( start%year == end%year )THEN
      tim = FLOAT(julian_day(  end%month,  end%day,  end%year) - &
            julian_day(start%month,start%day,start%year))*24.
    ELSE
      tim = FLOAT(julian_day(         12,       31,start%year) - &
                  julian_day(start%month,start%day,start%year) + &
                  julian_day(  end%month,  end%day,  end%year))*24.
      DO i = start%year+1,end%year-1
        tim = tim + FLOAT(julian_day(12,31,i))*24.
      END DO
    END IF
  ELSE
    tim = 0.
    IF( end%day > 0 )THEN
      tim = tim + FLOAT(end%day)*24.
    END IF
  END IF

ELSE

  IF( check_YMD(end) )THEN
    tim = NOT_SET_R
  ELSE
    tim = 0.
    IF( start%day > 0 )THEN
      tim = tim - FLOAT(start%day)*24.
    END IF
    IF( end%day > 0 )THEN
      tim = tim + FLOAT(end%day)*24.
    END IF
  END IF
END IF

IF( tim /= NOT_SET_R )THEN
  IF( start%hour /= DEF_VAL_R )THEN
    tim = tim - start%hour
  END IF
  IF( end%hour /= DEF_VAL_R )THEN
    tim = tim + end%hour
  END IF
END IF

end%runTime = tim

9999 CONTINUE

RETURN
END

!==============================================================================

LOGICAL FUNCTION check_YMD( time )

USE tooluser_fd

IMPLICIT NONE

TYPE( timeT ) time

check_YMD = MIN(time%year,time%month,time%day) > 0 !N.B. Assumes NOT_SET_R is negative

RETURN
END
