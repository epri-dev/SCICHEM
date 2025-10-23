!*******************************************************************************
!                     OpenProject
!*******************************************************************************
SUBROUTINE OpenProject( iwnd )

USE resource_fd
USE SCIAPIversion_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE GUImatl_fi
USE create_fi
USE dialog_fi
USE mettype_fd
USE SwinAPI
USE pltchoice_fi

IMPLICIT NONE

INTEGER  NWPN_VERSION
PARAMETER (NWPN_VERSION=500)

INTEGER  WXS_VERSION
PARAMETER (WXS_VERSION=500)

INTEGER   WXS_SURF
INTEGER   WXS_PROF

PARAMETER ( WXS_SURF   = 1 )
PARAMETER ( WXS_PROF   = 2 )

INTEGER(POINTER_LEN)     iwnd !Window handle

INTEGER irv,jversion
LOGICAL   dose,surface

LOGICAL, EXTERNAL :: hasError

!---- Read Project file

irv = SetCursor(hcur_wait) !  Set Arrow

project(BASE_LEVEL)%OK = .TRUE.

CALL GetMatInfo(iwnd,dose,surface)
IF( hasError() )GOTO 9999

CALL read_version(file_prj,jversion)

!---- Read PUFF File

IF( nTimePuff > 0 )THEN
  PlotDef(DEFAULT_LEVEL)%Field%UserTime = timePuff(nTimePuff)%time%runTime + 30.*24.
  IF( PlotDef(BASE_LEVEL)%Field%UserTime == NOT_SET_R .OR. &
    (PlotDef(BASE_LEVEL)%Field%UserTime < timePuff(nTimePuff)%time%runTime) )THEN
    PlotDef(BASE_LEVEL)%Field%UserTime = PlotDef(DEFAULT_LEVEL)%Field%UserTime
  END IF
ELSE
  PlotDef(DEFAULT_LEVEL)%Field%UserTime = 30.*24.
END IF
IF( RadTimeMax /= DEF_VAL_R ) &
PlotDef(DEFAULT_LEVEL)%Field%UserTime = MIN(PlotDef(DEFAULT_LEVEL)%Field%UserTime,RadTimeMax)

!---- Set Return value

project(BASE_LEVEL)%Puff = nTimePuff > 0
project(BASE_LEVEL)%Grid = nTimeSrf > 0
IF( project(BASE_LEVEL)%Restart )THEN
  project(BASE_LEVEL)%Edit = nTimeSrf == 1 !Restart starts
ELSE
  project(BASE_LEVEL)%Edit = .NOT. project(BASE_LEVEL)%Plot !New starts
END IF

project(BASE_LEVEL)%Weather = 0
IF( jversion >= WXS_VERSION )THEN
  SELECT CASE (metdef(BASE_LEVEL)%met)
    CASE (MET_OBS)
      project(BASE_LEVEL)%Weather = IBSET(project(BASE_LEVEL)%Weather,WXS_SURF)
      project(BASE_LEVEL)%Weather = IBSET(project(BASE_LEVEL)%Weather,WXS_PROF)
    CASE (MET_SRF)
      project(BASE_LEVEL)%Weather = IBSET(project(BASE_LEVEL)%Weather,WXS_SURF)
    CASE (MET_UAIR)
      project(BASE_LEVEL)%Weather = IBSET(project(BASE_LEVEL)%Weather,WXS_PROF)
    CASE DEFAULT
  END SELECT
END IF

9999  project(BASE_LEVEL)%OK = project(BASE_LEVEL)%OK .AND. .NOT.hasError()

!---- Set READ Flag - data to be read before doing a plot

!---- PCSCIPUF - Post any Errors

irv = SetCursor(hcur_arrow) !  Set Arrow
IF( hasError() )THEN
  CALL ShowErrorMessage( hwnd_mw )
END IF

RETURN
END

