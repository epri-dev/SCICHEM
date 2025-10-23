MODULE SCIMgr_fi

  USE basic_fd
  USE message_fd
  USE prjstruct_fd

  SAVE

!***********************************************************************
!               SCIP Tool Commons
!***********************************************************************

!==== Error

  INTEGER LastError

!==== Size

!==== Clock

  INTEGER clockDelay
  INTEGER clockUpdate
  INTEGER prevClockDelay
  INTEGER prevClockUpdate
!  INTEGER clockStart
!  INTEGER clockDelta

!==== Availability of DLLs

  LOGICAL AvailUTM

!==== State

  INTEGER toolState
  INTEGER SingleStepState
  LOGICAL SCIPinProgress

!==== CallBack

  INTEGER prevCallerID

  INTEGER(LEN_ADDRESS) prevCallBack
  INTEGER(LEN_ADDRESS) SCIPCallBack
  INTEGER(LEN_ADDRESS) ExternalCallBack

  TYPE ( messageT ) message
  TYPE ( messageT ) last_message

!==== Project

  TYPE( projectIDT ) ProjectID

END MODULE SCIMgr_fi

MODULE InitTool_fi

  USE charT_fd

  CHARACTER(PATH_MAXLENGTH) datapath,binpath,temppath,scipath
  CHARACTER(PATH_MAXLENGTH) INIfile

END MODULE InitTool_fi
