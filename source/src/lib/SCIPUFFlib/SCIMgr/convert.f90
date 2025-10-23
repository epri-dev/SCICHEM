!***********************************************************************
!               ConvertTime
!***********************************************************************
INTEGER FUNCTION ConvertTime( timeIn,timeOut )

USE SCIMgr_fd
USE error_fi

!     converts temporalT structures from UTC to local times and vice versa

IMPLICIT NONE

TYPE( temporalT ), INTENT( IN    ) :: timeIn
TYPE( temporalT ), INTENT( INOUT ) :: timeOut

INTEGER out

!==== Initialize

ConvertTime = SCIPfailure

!==== Convert start time

out = timeOut%start%time%reference
timeOut%start = timeIn%start
timeOut%start%time%reference = out

IF( timeIn%start%time%reference /= timeOut%start%time%reference )THEN

  SELECT CASE( timeIn%start%time%reference )
    CASE( HT_UTC )
      IF( timeOut%start%time%reference == HT_LOCAL )THEN
        CALL UTCtoLocal(timeOut%start%time,timeOut%start%zone)
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        GOTO 9998
      END IF

    CASE( HT_LOCAL )
      IF( timeOut%start%time%reference == HT_UTC )THEN
        CALL LocaltoUTC(timeOut%start%time,timeOut%start%zone)
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        GOTO 9998
      END IF

    CASE DEFAULT
      GOTO 9997

  END SELECT

END IF

!==== Convert end time

out = timeOut%end%time%reference
timeOut%end = timeIn%end
timeOut%end%time%reference = out

IF( timeIn%end%time%reference /= timeOut%end%time%reference )THEN

  SELECT CASE( timeIn%end%time%reference )
    CASE( HT_UTC )
      IF( timeOut%end%time%reference == HT_LOCAL )THEN
        CALL UTCtoLocal(timeOut%end%time,timeOut%start%zone)
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        GOTO 9998
     END IF

    CASE( HT_LOCAL )
      IF( timeOut%end%time%reference == HT_UTC )THEN
        CALL LocaltoUTC(timeOut%end%time,timeOut%start%zone)
        IF( nError /= NO_ERROR )GOTO 9999
      ELSE
        GOTO 9998
      END IF

    CASE DEFAULT
      GOTO 9997

  END SELECT

END IF

ConvertTime = SCIPsuccess

9999 CONTINUE

RETURN

9998 CONTINUE
nError = UK_ERROR
eMessage = 'Conversion not performed : Invalid output format'
WRITE(eInform,*)'Output format =',out
GOTO 9999

9997 CONTINUE
nError = UK_ERROR
eMessage = 'Conversion not performed : Invalid input format'
WRITE(eInform,*)'Input format =',out
GOTO 9999

END
