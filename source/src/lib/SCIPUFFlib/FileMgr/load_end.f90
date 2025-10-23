!*******************************************************************************
!            UnloadEnd
!*******************************************************************************
SUBROUTINE UnloadEnd( end )

USE convert_fd
USE time_fd
USE scipuff_fi

!     Load SCIPUFF commons from an SCIP End structure

IMPLICIT NONE

TYPE( endT ), INTENT( IN ) :: end

REAL, EXTERNAL :: ScaleReal

!==== Unload

!==== Steps

delt    = end%step%max
dt_save = ScaleReal( end%step%output,HCF_HOUR2SEC )

!==== End time

local     = end%time%reference == HT_LOCAL
year_end  = end%time%year
month_end = end%time%month
day_end   = end%time%day
tend      = end%time%hour

tend_hr   = end%time%runTime

RETURN
END
!*******************************************************************************
!            LoadEnd
!*******************************************************************************
SUBROUTINE LoadEnd( end )

USE convert_fd
USE time_fd
USE scipuff_fi

!     Load an SCIP End structure from SCIPUFF commons

IMPLICIT NONE

TYPE( endT ), INTENT( OUT ) :: end

REAL, EXTERNAL :: ScaleReal

!==== Load

!==== Steps

end%step%max    = delt
end%step%output = ScaleReal( dt_save,HCF_SEC2HOUR )

!==== End time

IF( local )THEN
 end%time%reference = HT_LOCAL
ELSE
 end%time%reference = HT_UTC
END IF
end%time%year  = year_end
end%time%month = month_end
end%time%day   = day_end
end%time%hour  = tend

end%time%runTime = tend_hr

RETURN
END
