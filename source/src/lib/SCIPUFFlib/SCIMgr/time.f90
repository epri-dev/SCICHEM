!*******************************************************************************
!            Load Project Time Input
!*******************************************************************************
INTEGER FUNCTION Load_Time( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( ptemporalT ), INTENT( INOUT ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Load_Time = SCIPfailure

!==== Set namelist default values (SCIPUFF memory) from input data

CALL UnloadStart( tool%time%start )

CALL UnloadEnd( tool%time%end )

!==== Read namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )

CALL ReadStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL ReadEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Set input data from nalelist results (SCIPUFF memory)

CALL LoadStart( tool%time%start )

CALL LoadEnd( tool%time%end )

!==== Return

Load_Time = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Write Project Time Input
!*******************************************************************************
INTEGER FUNCTION Write_Time( tool )

USE SCIMgr_fd
USE files_fi
USE error_fi

IMPLICIT NONE

TYPE( ptemporalT ), INTENT( IN ) :: tool

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

Write_Time = SCIPfailure

!==== Set namelist values (SCIPUFF memory) from input data

CALL UnloadStart( tool%time%start )

CALL UnloadEnd( tool%time%end )

!==== Write namelist

filename = TRIM(AddExtension( tool%project%name,'inp' ))
CALL AddPath( filename,tool%project%path )

CALL WriteStart( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

CALL WriteEnd( filename,lun_tmp )
IF( nError /= NO_ERROR )GOTO 9999

!==== Return

Write_Time = SCIPsuccess

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Default Project Time Input
!*******************************************************************************
INTEGER FUNCTION Default_Time( tool )

USE SCIMgr_fd
USE SCIMgr_fi
USE error_fi

IMPLICIT NONE

TYPE( temporalT ), INTENT( INOUT ) ::  tool

TYPE( startT ) start
TYPE( endT   ) end

INTEGER irv

INTEGER, EXTERNAL :: Default_Start
INTEGER, EXTERNAL :: Default_End

Default_Time = SCIPfailure

irv = Default_Start( start )

IF( irv == SCIPsuccess )THEN

  tool%start = start

  irv = Default_End( end )

  IF( irv == SCIPsuccess )THEN
    tool%end = end
  ELSE
    LastError = UK_ERROR
    nError    = UK_ERROR
    eRoutine  = 'Default_End'
    eMessage  = 'Error setting default end time data'
  END IF

ELSE

  LastError = UK_ERROR
  nError    = UK_ERROR
  eRoutine  = 'Default_End'
  eMessage  = 'Error setting default start time data'

END IF

Default_Time = irv

RETURN
END
!***********************************************************************
!               ComputeDuration
!***********************************************************************
INTEGER FUNCTION ComputeDuration( start,end,dur )

USE SCIMgr_fd
USE default_fd
USE checkErr

IMPLICIT NONE

TYPE( timeT ), INTENT( IN  ) :: start
TYPE( timeT ), INTENT( IN  ) :: end
REAL,          INTENT( OUT ) :: dur

LOGICAL ymd
INTEGER i

INTEGER, EXTERNAL :: julian_day

ComputeDuration = SCIPfailure

dur = NOT_SET_R

IF( start%hour == NOT_SET_R .OR. end%hour == NOT_SET_R )GOTO 9999

ymd = .NOT.SpecialValue(SPECIAL_VALUE,start%year) .OR. &
      .NOT.SpecialValue(SPECIAL_VALUE,start%month)

IF( ymd )THEN
  ymd = .NOT.SpecialValue(SPECIAL_VALUE,end%year) .OR. &
        .NOT.SpecialValue(SPECIAL_VALUE,end%month)
  IF( ymd )THEN
    IF( start%year > end%year )THEN
      GOTO 9999
    ELSE IF( start%year == end%year )THEN
      dur = FLOAT(julian_day(   end%month,  end%day,  end%year ) - &
                  julian_day( start%month,start%day,start%year ))*24.
    ELSE
      dur = FLOAT(julian_day(          12,       31,start%year ) - &
                  julian_day( start%month,start%day,start%year ) + &
                  julian_day(   end%month,  end%day,  end%year ))*24.
      DO i = start%year+1,end%year-1
        dur = dur + FLOAT(julian_day( 12,31,i ))*24.
      END DO
    END IF
  ELSE
    dur = 0.
    IF( end%day > 0 )dur = dur + FLOAT(end%day)*24.
  END IF
ELSE
  ymd = .NOT.SpecialValue(SPECIAL_VALUE,end%year) .OR. &
        .NOT.SpecialValue(SPECIAL_VALUE,end%month)
  IF( ymd )THEN
    dur = NOT_SET_R
  ELSE
    dur = 0.
    IF( start%day > 0 )dur = dur - FLOAT(start%day)*24.
    IF( end%day   > 0 )dur = dur + FLOAT(end%day)*24.
  END IF
END IF

IF( dur /= NOT_SET_R )THEN
  IF( start%hour /= DEF_VAL_R )dur = dur - start%hour
  IF( end%hour   /= DEF_VAL_R )dur = dur + end%hour
END IF

ComputeDuration = SCIPsuccess

9999 CONTINUE

RETURN
END
