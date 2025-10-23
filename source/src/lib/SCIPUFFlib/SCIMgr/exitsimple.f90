!*******************************************************************************
!            Exit SCIP Tool
!*******************************************************************************
INTEGER FUNCTION SimpleExitTool()

USE SCIMgr_fd
USE files_fi
USE SCIMgr_fi
USE scipuff_fi
USE search_fd
USE SCIMgrState
USE met_fi

IMPLICIT NONE

INTEGER irv
INTEGER currentState
INTEGER alloc_stat


SimpleExitTool = SCIPfailure

IF( SCIMgrCheckState(HS_IDLESTATE) )THEN !Available only while idle
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

!==== Exit UTM

IF( AvailUTM )THEN
END IF

!==== deallocate materials

IF( ALLOCATED(material)  )DEALLOCATE( material,STAT=alloc_stat )
IF( ASSOCIATED(mat_aux)  )DEALLOCATE( mat_aux,STAT=alloc_stat ); NULLIFY( mat_aux )
IF( ALLOCATED(typeID)    )DEALLOCATE( typeID,STAT=alloc_stat )

!----- McWif vertical grid

IF( ALLOCATED(zMC) )DEALLOCATE( zMC,STAT=alloc_stat )


!==== Set return value

SimpleExitTool = SCIPsuccess

irv = SCIMgrSetState( currentState )

RETURN
END
