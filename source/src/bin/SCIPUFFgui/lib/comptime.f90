!***********************************************************************
!               ComputeEndTime
!***********************************************************************
SUBROUTINE ComputeEndTime(start,end,lok)

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE default_fd

IMPLICIT NONE

TYPE( timeT ) start
TYPE( timeT ) end
LOGICAL lok,check_YMD

REAL tim

lok = start%hour /= DEF_VAL_R
IF( .NOT.lok)RETURN

IF( check_YMD(start) )THEN
  tim = start%hour + end%runTime
  CALL year_month_day(tim,start%year,start%month,start%day, &
          end%hour,  end%year,  end%month,  end%day)
ELSE
  end%year  = NOT_SET_I
  end%month = NOT_SET_I
  end%day   = 0
  end%hour  = end%runTime + start%hour
  DO WHILE( end%hour >= 24.0)
    end%hour = end%hour - 24.
    end%day = end%day + 1
  END DO
END IF
IF( end%day <= 0)end%day = NOT_SET_I

RETURN
END
!***********************************************************************
!               ComputeDurationGUI
!***********************************************************************
SUBROUTINE ComputeDurationGUI(start,end,lok)

USE resource_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE default_fd

IMPLICIT NONE

TYPE( timeT ) start
TYPE( timeT ) end
LOGICAL lok,check_YMD

REAL tim
INTEGER julian_day,i

end%runTime = NOT_SET_R
lok = .FALSE.

IF( start%hour == NOT_SET_R .OR. end%hour == NOT_SET_R )THEN
  GOTO 9999
END IF

IF( check_YMD(start) )THEN
  IF( check_YMD(end) )THEN
    IF( start%year > end%year )THEN
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

lok = tim > 0.
IF( lok )THEN
  end%runTime = tim
END IF

9999  RETURN

END
!***********************************************************************
!               CheckYMD
!***********************************************************************
LOGICAL FUNCTION check_YMD(time)
USE resource_fd
USE pcscipuf_fi
USE dialog_fi
USE default_fd

IMPLICIT NONE

TYPE( timeT ) time

check_YMD = MIN(time%year,time%month,time%day) > 0

RETURN
END
