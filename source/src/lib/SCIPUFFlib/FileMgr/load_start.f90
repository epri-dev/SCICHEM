!*******************************************************************************
!            UnloadStart
!*******************************************************************************
SUBROUTINE UnloadStart( start )

USE time_fd
USE scipuff_fi
!     Load SCIPUFF commons from an SCIP Start structure

IMPLICIT NONE

TYPE( startT ), INTENT( IN ) :: start

!==== Unload

!==== Start time

local       = start%time%reference == HT_LOCAL
year_start  = start%time%year
month_start = start%time%month
day_start   = start%time%day
tstart      = start%time%hour

!==== Time zone

tzone  = start%zone

RETURN
END
!*******************************************************************************
!            LoadStart
!*******************************************************************************
SUBROUTINE LoadStart( start )

USE time_fd
USE scipuff_fi

!     Load an SCIP Start structure from SCIPUFF commons

IMPLICIT NONE

TYPE( startT ), INTENT( OUT ) :: start

!==== Load

!==== Start time

IF( local )THEN
 start%time%reference = HT_LOCAL
ELSE
 start%time%reference = HT_UTC
END IF
start%time%year  = year_start
start%time%month = month_start
start%time%day   = day_start
start%time%hour  = tstart

start%time%runTime = 0.0

!==== Time zone

IF( tzone >= 0.0 )THEN
  start%zone  = tzone
ELSE
  start%zone  = tzone + 24.0
END IF

RETURN
END
