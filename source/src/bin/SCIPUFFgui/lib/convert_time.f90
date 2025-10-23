!***********************************************************************
!               GUI_SCIP_start
!***********************************************************************
SUBROUTINE GUI_SCIP_start(prjdlg,timdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( pstartT ) tool
TYPE( TIME_DLG ) timdlg
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== Start Time

tool%start = timdlg%time%start

RETURN
END
!***********************************************************************
!               SCIP_GUI_start
!***********************************************************************
SUBROUTINE SCIP_GUI_start(timdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( pstartT ) tool
TYPE( TIME_DLG ) timdlg

LOGICAL check_YMD

!==== Start time

timdlg%time%start = tool%start

!==== Extra

IF( .NOT.check_YMD(timdlg%time%start%time) )THEN
  timdlg%time%start%time%year  = NOT_SET_I
  timdlg%time%start%time%month = NOT_SET_I
END IF
IF( timdlg%time%start%time%day <= 0 )THEN
  timdlg%time%start%time%day  = NOT_SET_I
END IF

CALL time_string(timdlg%time%start%time,timdlg%startString)

RETURN
END
!***********************************************************************
!               GUI_SCIP_end
!***********************************************************************
SUBROUTINE GUI_SCIP_end(prjdlg,timdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( pendT ) tool
TYPE( TIME_DLG ) timdlg
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== End Time

tool%end = timdlg%time%end

RETURN
END
!***********************************************************************
!               SCIP_GUI_end
!***********************************************************************
SUBROUTINE SCIP_GUI_end(timdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

LOGICAL check_YMD

TYPE( pendT ) tool
TYPE( TIME_DLG ) timdlg

!==== End time

timdlg%time%end = tool%end

!==== Extra

IF( .NOT.check_YMD(timdlg%time%end%time) )THEN
  timdlg%time%end%time%year  = NOT_SET_I
  timdlg%time%end%time%month = NOT_SET_I
END IF
IF( timdlg%time%end%time%day <= 0 )THEN
  timdlg%time%end%time%day  = NOT_SET_I
END IF

CALL time_string(timdlg%time%end%time,timdlg%endString)

RETURN
END
!***********************************************************************
!               GUI_SCIP_time
!***********************************************************************
SUBROUTINE GUI_SCIP_time(prjdlg,timdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( ptemporalT ) tool
TYPE( TIME_DLG ) timdlg
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== Start/End Time

tool%time = timdlg%time

IF(  tool%time%start%time%reference == HT_UTC )tool%time%start%zone=DEF_VAL_R

RETURN
END
!***********************************************************************
!               SCIP_GUI_time
!***********************************************************************
SUBROUTINE SCIP_GUI_time(timdlg,tool)

USE tooluser_fd
USE dialog_fi
USE default_fd

IMPLICIT NONE

TYPE( ptemporalT ) tool
TYPE( TIME_DLG ) timdlg

LOGICAL lok,check_YMD

REAL x

!==== Start/End time

timdlg%time = tool%time

!==== Extra

IF( .NOT.check_YMD(timdlg%time%start%time) )THEN
  timdlg%time%start%time%year  = NOT_SET_I
  timdlg%time%start%time%month = NOT_SET_I
END IF
IF( timdlg%time%start%time%day <= 0 )THEN
  timdlg%time%start%time%day  = NOT_SET_I
END IF

CALL time_string(timdlg%time%start%time,timdlg%startString)

IF( .NOT.check_YMD(timdlg%time%end%time) )THEN
  timdlg%time%end%time%year  = NOT_SET_I
  timdlg%time%end%time%month = NOT_SET_I
END IF

IF( timdlg%time%end%time%runTime == NOT_SET_R )THEN
  IF( check_YMD(timdlg%time%start%time) .AND. .NOT.check_YMD(timdlg%time%end%time) )THEN
    timdlg%time%end%time%runTime = timdlg%time%end%time%hour - timdlg%time%start%time%hour
    CALL ComputeEndTime(timdlg%time%start%time,timdlg%time%end%time,lok)
  ELSE
    CALL ComputeDurationGUI(timdlg%time%start%time,timdlg%time%end%time,lok)
  END IF
ELSE
  CALL ComputeEndTime(timdlg%time%start%time,timdlg%time%end%time,lok)
END IF
IF( timdlg%time%end%time%day <= 0 )THEN
  timdlg%time%end%time%day  = NOT_SET_I
END IF

CALL time_string(timdlg%time%end%time,timdlg%endString)

CALL set_default_save(timdlg%time%end%time%runTime,-1,dlgTime(DEFAULT_LEVEL)%time%end%step%output,x)
timdlg%default_save = timdlg%time%end%step%output == x

RETURN
END
