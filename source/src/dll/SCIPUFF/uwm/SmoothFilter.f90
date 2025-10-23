!==============================================================================

MODULE FilterDefinitionMG

IMPLICIT NONE

INTEGER, PARAMETER :: FILTER_LENGTH = 3   !Number of non-zero coefficients
INTEGER, PARAMETER :: HALF_LENGTH   = (FILTER_LENGTH+1)/2

REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH), PARAMETER :: FILTER_WT = (/ 0.,.25,.5,.25,0. /)

REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH+1), PARAMETER :: FILTER_WT_STG = &
                                                                    (/ 0.,.125,.375,.375,.125,0. /)
CONTAINS

!------------------------------------------------------------------------------

INTEGER FUNCTION ilimit( i,imin,imax )

!DEC$ ATTRIBUTES DLLEXPORT :: ilimit
IMPLICIT NONE

INTEGER, INTENT( IN ) :: i, imin, imax

ilimit = MAX( i,imin )
ilimit = MIN( ilimit,imax )

RETURN
END FUNCTION ilimit

!------------------------------------------------------------------------------

REAL FUNCTION rlimit( r,rmin,rmax )

!DEC$ ATTRIBUTES DLLEXPORT :: rlimit
IMPLICIT NONE

REAL, INTENT( IN ) :: r, rmin, rmax

rlimit = MAX( r,rmin )
rlimit = MIN( rlimit,rmax )

RETURN
END FUNCTION rlimit

!------------------------------------------------------------------------------

SUBROUTINE GetFilterLengthMG( nFil )

!DEC$ ATTRIBUTES DLLEXPORT :: GetFilterLengthMG
IMPLICIT NONE

INTEGER, INTENT( OUT ) :: nFil

nFil = FILTER_LENGTH

RETURN
END SUBROUTINE GetFilterLengthMG

!------------------------------------------------------------------------------

SUBROUTINE GetFilterWeightsMG( wt )

!DEC$ ATTRIBUTES DLLEXPORT :: GetFilterWeightsMG
IMPLICIT NONE

REAL, DIMENSION(*), INTENT( OUT ) :: wt

!------ Set weights

wt(1:FILTER_LENGTH) = FILTER_WT(-HALF_LENGTH+1:HALF_LENGTH-1)  !Only non-zero coefficients

!------ Normalize (should already be done)

wt(1:FILTER_LENGTH) = wt(1:FILTER_LENGTH) / SUM(wt(1:FILTER_LENGTH))

RETURN
END SUBROUTINE GetFilterWeightsMG

!------------------------------------------------------------------------------

SUBROUTINE GetFilterWeightsStgMG( wt )

!DEC$ ATTRIBUTES DLLEXPORT :: GetFilterWeightsStgMG
IMPLICIT NONE

REAL, DIMENSION(*), INTENT( OUT ) :: wt

!------ Set weights for staggered grid

wt(1:FILTER_LENGTH+1) = FILTER_WT_STG(-HALF_LENGTH+1:HALF_LENGTH)  !Only non-zero coefficients

!------ Normalize

wt(1:FILTER_LENGTH+1) = wt(1:FILTER_LENGTH+1) / SUM(wt(1:FILTER_LENGTH+1))

RETURN
END SUBROUTINE GetFilterWeightsStgMG

!------------------------------------------------------------------------------

REAL FUNCTION FilterFacMG( tc,tp ) RESULT( f )

!DEC$ ATTRIBUTES DLLEXPORT :: FilterFacMG
IMPLICIT NONE

!------ Filter function for non-integer locations

REAL, INTENT( IN ) :: tc  !Center location of filter (normalized)
REAL, INTENT( IN ) :: tp  !Location to evaluate filter weight (normalized)

INTEGER i, ip
REAL    t, r

t  = rlimit(tp-tc,-FLOAT(HALF_LENGTH),FLOAT(HALF_LENGTH))
i  = FLOOR(t)
ip = MIN(i+1,HALF_LENGTH)
r  = t - FLOAT(i)
f = (1.-r)*FILTER_WT(i) + r*FILTER_WT(ip)

RETURN
END FUNCTION FilterFacMG

!------------------------------------------------------------------------------

REAL FUNCTION FilterFacStgMG( tc,tp ) RESULT( f )

!DEC$ ATTRIBUTES DLLEXPORT :: FilterFacStgMG
IMPLICIT NONE

!------ Filter function for velocity locations on a staggered grid

REAL, INTENT( IN ) :: tc  !Center location of filter (normalized)
REAL, INTENT( IN ) :: tp  !Location to evaluate filter weight (normalized)

INTEGER i, ip
REAL    t, r

t  = rlimit(tp-tc,-FLOAT(HALF_LENGTH)-0.5,FLOAT(HALF_LENGTH)+0.5)
i  = FLOOR(t+0.5)
ip = MIN(i+1,HALF_LENGTH+1)
r  = t - (FLOAT(i)-0.5)

f = (1.-r)*FILTER_WT_STG(i) + r*FILTER_WT_STG(ip)

RETURN
END FUNCTION FilterFacStgMG

END MODULE FilterDefinitionMG

!==============================================================================

