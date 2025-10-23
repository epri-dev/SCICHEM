!*******************************************************************************
!            UnloadCtrl
!*******************************************************************************
SUBROUTINE UnloadCtrl( ctrl )

USE convert_fd
USE time_fd
USE scipuff_fi
USE files_fi

!     Load SCIPUFF commons from an SCIP Ctrl structure

IMPLICIT NONE

TYPE( ctrlT ), INTENT( IN ) :: ctrl

REAL, EXTERNAL :: ScaleReal

!==== Unload

file_rst = ctrl%name
path_rst = ctrl%path
time_rst = ScaleReal( ctrl%runTime,HCF_HOUR2SEC )

RETURN
END
!*******************************************************************************
!            LoadCtrl
!*******************************************************************************
SUBROUTINE LoadCtrl( ctrl )

USE convert_fd
USE time_fd
USE scipuff_fi
USE files_fi

!     Load an SCIP Ctrl structure from SCIPUFF commons

IMPLICIT NONE

TYPE( ctrlT ), INTENT( OUT ) :: ctrl

REAL, EXTERNAL :: ScaleReal

!==== Load

ctrl%name    = file_rst
ctrl%path    = path_rst
ctrl%runTime = ScaleReal( time_rst,HCF_SEC2HOUR )

RETURN
END
