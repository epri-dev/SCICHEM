!***********************************************************************
!               GUI_SCIP_flags
!***********************************************************************
SUBROUTINE GUI_SCIP_flags( prjdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd

IMPLICIT NONE

TYPE( pflagsT ) tool
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== Method flags

IF( prjdlg%Dynamic )THEN
   tool%flags%method = IBSET(tool%flags%method,HFB_DYNAMIC)
ELSE
   tool%flags%method = IBCLR(tool%flags%method,HFB_DYNAMIC)
END IF

IF( prjdlg%DenseGas )THEN
   tool%flags%method = IBSET(tool%flags%method,HFB_DENSE)
ELSE
   tool%flags%method = IBCLR(tool%flags%method,HFB_DENSE)
END IF

IF( prjdlg%StaticPuffs )THEN
   tool%flags%method = IBSET(tool%flags%method,HFB_STATIC)
ELSE
   tool%flags%method = IBCLR(tool%flags%method,HFB_STATIC)
END IF

!==== Mode flags

IF( BTEST(prjdlg%Mode,FAST_MODE) )THEN
   tool%flags%mode = IBSET(tool%flags%mode,HFB_FAST)
ELSE
   tool%flags%mode = IBCLR(tool%flags%mode,HFB_FAST)
END IF

IF( BTEST(prjdlg%Mode,REVERSE_MODE) )THEN
   tool%flags%mode = IBSET(tool%flags%mode,HFB_REVERSE)
ELSE
   tool%flags%mode = IBCLR(tool%flags%mode,HFB_REVERSE)
END IF

!==== Start flags

IF( prjdlg%Restart )THEN
  tool%flags%start = IBSET(tool%flags%start,HFB_RESTART)
ELSE
  tool%flags%start = IBCLR(tool%flags%start,HFB_RESTART)
END IF

!==== Audit

tool%flags%audit%title   = prjdlg%Title
tool%flags%audit%analyst = prjdlg%audit%Analyst
tool%flags%audit%class   = prjdlg%audit%Classification

RETURN
END
!***********************************************************************
!               SCIP_GUI_flags
!***********************************************************************
SUBROUTINE SCIP_GUI_flags( prjdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE errorParam_fd

IMPLICIT NONE

TYPE( pflagsT ) tool
TYPE( ProjectStructure ) prjdlg

!==== Clear

!==== Method flags

prjdlg%Dynamic     = BTEST(tool%flags%method,HFB_DYNAMIC)
prjdlg%DenseGas    = BTEST(tool%flags%method,HFB_DENSE)
prjdlg%StaticPuffs = BTEST(tool%flags%method,HFB_STATIC)

!==== Mode flags

prjdlg%Mode = 0
IF( BTEST(tool%flags%mode,HFB_FAST) )prjdlg%Mode = IBSET(prjdlg%Mode,FAST_MODE)
IF( BTEST(tool%flags%mode,HFB_REVERSE) )prjdlg%Mode = IBSET(prjdlg%Mode,REVERSE_MODE)

!==== Start flags

prjdlg%Restart = BTEST(tool%flags%start,HFB_RESTART)

!==== Audit

prjdlg%Title                = tool%flags%audit%title
prjdlg%audit%Analyst        = tool%flags%audit%analyst
prjdlg%audit%Classification = tool%flags%audit%class
prjdlg%audit%Version        = tool%flags%audit%version
prjdlg%audit%CreateDate     = tool%flags%audit%date

RETURN
END
!***********************************************************************
!               GUI_SCIP_options
!***********************************************************************
SUBROUTINE GUI_SCIP_options( prjdlg,optdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( poptionsT ) tool
TYPE( optionsT ) optdlg
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

tool%option  = optdlg

RETURN
END
!***********************************************************************
!               SCIP_GUI_options
!***********************************************************************
SUBROUTINE SCIP_GUI_options( optdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( poptionsT ) tool
TYPE( optionsT ) optdlg

optdlg = tool%option

RETURN
END
!***********************************************************************
!               GUI_SCIP_ctrl
!***********************************************************************
SUBROUTINE GUI_SCIP_ctrl( prjdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi

IMPLICIT NONE

TYPE( pctrlT  ) tool
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== Restart

IF( prjdlg%Restart )THEN
  CALL SplitName( prjdlg%RestartFile,tool%ctrl%name,tool%ctrl%path )
  CALL RemoveExtension( tool%ctrl%name )
  tool%ctrl%runTime = timeRestart(prjdlg%RestartTimeIndx)%time%runTime
ELSE
  tool%ctrl%name = ' '
  tool%ctrl%path = ' '
  tool%ctrl%runTime = 0.0
END IF

RETURN
END
!***********************************************************************
!               SCIP_GUI_ctrl
!***********************************************************************
SUBROUTINE SCIP_GUI_ctrl( prjdlg,tool )

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE errorParam_fd

IMPLICIT NONE

TYPE( pctrlT ) tool
TYPE( ProjectStructure ) prjdlg

INTEGER ios

!==== Start flags

IF( prjdlg%Restart )THEN
  prjdlg%RestartFile = TRIM(tool%ctrl%name)
  CALL AddPath( prjdlg%RestartFile,tool%ctrl%path )
  prjdlg%RestartFile = TRIM(prjdlg%RestartFile)//'.puf'
  prjdlg%RestartTimeIndx = 1
  nTimeRestart = 1
  IF( ALLOCATED(timeRestart) )THEN
    DEALLOCATE( timeRestart,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    CALL SetError( UK_ERROR, &
	                  'Failed to deallocate restart time array', &
	                  'Deallocate error = '//TRIM(ADJUSTL(string1)),' ', &
	                  'SCIPtoGUI_ctrl' )
	    GOTO 9999
	  END IF
	END IF
  ALLOCATE( timeRestart(nTimeRestart),STAT=ios )
  IF( ios /= 0 )THEN
    WRITE(string1,*)ios
	  CALL SetError( UK_ERROR, &
	                'Failed to allocate restart time array', &
	                'Allocate error = '//TRIM(ADJUSTL(string1)),' ', &
	                'SCIPtoGUI_ctrl' )
    GOTO 9999
  END IF
  timeRestart(1)%time%runTime = tool%ctrl%runTime
ELSE
  prjdlg%RestartFile = ' '
  prjdlg%RestartTimeIndx = NOT_SET_I
END IF

9999 CONTINUE

RETURN
END
