!******************************************************************************
! ClippedNormal Gausian parameters
!******************************************************************************
! ClippedNormalParm
!   Purpose : compute the clipped normal parameters
!   Input   : ratio  - cSig/cBar
!   Output  : gBar   - clipped normal gaussian mean
!           : gSig   - clipped normal gaussian sigma
!           : gamma  - clipped normal gamma
!   Return  : TRUE/FALSE
!******************************************************************************
INTEGER FUNCTION ClippedNormalParm( ratio,gBar,gSig,gamma )

USE basic_fd

IMPLICIT NONE

!DEC$ ATTRIBUTES DLLEXPORT, DECORATE, ALIAS : 'ClippedNormalParm' :: ClippedNormalParm

REAL, PARAMETER :: CCOC_MIN  = 1.e-3
REAL, PARAMETER :: GAMMA_MIN = 1.e-4
REAL, PARAMETER :: SQRT2     = 1.414214

REAL, INTENT ( IN  ) :: ratio
REAL, INTENT ( OUT ) :: gBar
REAL, INTENT ( OUT ) :: gSig
REAL, INTENT ( OUT ) :: gamma

REAL ccoc

REAL, EXTERNAL :: erfc

ccoc = MAX( CCOC_MIN,ratio )

CALL clnpar( ccoc,gBar,gSig )

gamma = MAX(0.5*erfc( -gBar/(SQRT2*gSig) ),GAMMA_MIN)

ClippedNormalParm = TRUE

RETURN
END
