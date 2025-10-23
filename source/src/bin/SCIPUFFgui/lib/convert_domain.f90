!***********************************************************************
!               GUI_SCIP_domain
!***********************************************************************
SUBROUTINE GUI_SCIP_domain(prjdlg,domdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd

IMPLICIT NONE

TYPE( pspatialT ) tool
TYPE( DOMAIN_DLG ) domdlg
TYPE( ProjectStructure ) prjdlg

!==== Project id member

tool%project = prjdlg%ID

!==== domain

tool%spatial = domdlg%spatial

RETURN
END
!***********************************************************************
!               SCIP_GUI_domain
!***********************************************************************
SUBROUTINE SCIP_GUI_domain(domdlg,tool)

USE resource_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE param_fd
USE errorParam_fd

IMPLICIT NONE

TYPE( pspatialT ) tool
TYPE( DOMAIN_DLG ) domdlg

!==== Domain

domdlg%spatial = tool%spatial

IF( domdlg%spatial%domain%coord == I_LATLON .OR. domdlg%spatial%domain%coord == I_UTM )THEN
  domdlg%hasReference = .FALSE.
ELSE
  domdlg%hasReference = MIN(domdlg%spatial%reference%lat,domdlg%spatial%reference%lon, &
                            domdlg%spatial%reference%x,domdlg%spatial%reference%y) /= NOT_SET_R
END IF

RETURN
END
