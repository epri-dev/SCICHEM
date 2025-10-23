!*******************************************************************************
!            Run a Project
!*******************************************************************************
INTEGER FUNCTION SCIPRunProject( UserID,run )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPRunProject

INTEGER,       INTENT( IN  ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN  ) :: run    !Run input

INTEGER, EXTERNAL :: RunProject

SCIPRunProject = RunProject( UserID,run )

RETURN
END
!*******************************************************************************
!            Initialize a Project to run in single step mode
!*******************************************************************************
INTEGER FUNCTION SCIPInitProjectSS( UserID,run )

USE timstruct_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPInitProjectSS

INTEGER,       INTENT( IN  ) :: UserID !USER ID Tag
TYPE( pendT ), INTENT( IN  ) :: run    !Run input

INTEGER, EXTERNAL :: InitProjectSS

SCIPInitProjectSS = InitProjectSS( UserID,run )

RETURN
END
!*******************************************************************************
!            Run a Project for single step
!*******************************************************************************
INTEGER FUNCTION SCIPRunProjectSS( t )

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPRunProjectSS

REAL, INTENT( OUT ) :: t

INTEGER, EXTERNAL :: RunProjectSS

SCIPRunProjectSS = RunProjectSS( t )

RETURN
END
!*******************************************************************************
!            Exit Project from single step mode
!*******************************************************************************
INTEGER FUNCTION SCIPExitProjectSS()

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT :: SCIPExitProjectSS

INTEGER, EXTERNAL :: ExitProjectSS

SCIPExitProjectSS = ExitProjectSS()

RETURN
END

