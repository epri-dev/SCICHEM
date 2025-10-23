!==============================================================================

MODULE FilterDefinition

IMPLICIT NONE

INTEGER, PARAMETER :: FILTER_LENGTH = 3   !Number of non-zero coefficients (must be 3 or 5)
INTEGER, PARAMETER :: HALF_LENGTH   = (FILTER_LENGTH+1)/2

REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH), PARAMETER :: FILTER_WT = (/ 0.,.25,.5,.25,0. /)

REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH+1), PARAMETER :: FILTER_WT_STG = (/ &
                                                                           0., &
                               FILTER_WT(0)*(-1.) + FILTER_WT(1)*( 1.) + 0.25, &
                               FILTER_WT(0)*( 1.) + FILTER_WT(1)*(-1.) + 0.25, &
                               FILTER_WT(0)*( 1.) + FILTER_WT(1)*(-1.) + 0.25, &
                               FILTER_WT(0)*(-1.) + FILTER_WT(1)*( 1.) + 0.25, &
                                                                           0.  /)

!REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH), PARAMETER :: FILTER_WT = (/ 0.,-1.,2.,6.,2.,-1.,0. /) / 8.
!REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH), PARAMETER :: FILTER_WT = (/ 0.,1.,4.,6.,4.,1.,0. /) / 16.
!REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH), PARAMETER :: FILTER_WT = (/ 0.,0.,1.,2.,1.,0.,0. /) / 4.

!REAL, DIMENSION(-HALF_LENGTH:HALF_LENGTH+1), PARAMETER :: FILTER_WT_STG = (/      &
!                                                                                0., &
!    (2./3.)*(FILTER_WT(0)*( 1.) + FILTER_WT(1)*(-3.) + FILTER_WT(2)*( 2.)) + 1./6., &
!    (2./3.)*(FILTER_WT(0)*(-2.) + FILTER_WT(1)*( 3.) + FILTER_WT(2)*(-1.)) + 1./6., &
!    (2./3.)*(FILTER_WT(0)*( 1.)                      + FILTER_WT(2)*(-1.)) + 1./6., &
!    (2./3.)*(FILTER_WT(0)*( 1.)                      + FILTER_WT(2)*(-1.)) + 1./6., &
!    (2./3.)*(FILTER_WT(0)*(-2.) + FILTER_WT(1)*( 3.) + FILTER_WT(2)*(-1.)) + 1./6., &
!    (2./3.)*(FILTER_WT(0)*( 1.) + FILTER_WT(1)*(-3.) + FILTER_WT(2)*( 2.)) + 1./6., &
!                                                                                0.  /)
REAL, EXTERNAL :: SWIMrlimit

CONTAINS

!------------------------------------------------------------------------------

SUBROUTINE GetFilterLength( nFil )

IMPLICIT NONE

INTEGER, INTENT( OUT ) :: nFil

nFil = FILTER_LENGTH

RETURN
END SUBROUTINE GetFilterLength

!------------------------------------------------------------------------------

SUBROUTINE GetFilterWeights( wt )

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( OUT ) :: wt

!------ Set weights

wt(1:FILTER_LENGTH) = FILTER_WT(-HALF_LENGTH+1:HALF_LENGTH-1)  !Only non-zero coefficients

!------ Normalize (should already be done)

wt(1:FILTER_LENGTH) = wt(1:FILTER_LENGTH) / SUM(wt(1:FILTER_LENGTH))

RETURN
END SUBROUTINE GetFilterWeights

!------------------------------------------------------------------------------

SUBROUTINE GetFilterWeightsStg( wt )

IMPLICIT NONE

REAL, DIMENSION(*), INTENT( OUT ) :: wt

!------ Set weights for staggered grid

wt(1:FILTER_LENGTH+1) = FILTER_WT_STG(-HALF_LENGTH+1:HALF_LENGTH)  !Only non-zero coefficients

!------ Normalize

wt(1:FILTER_LENGTH+1) = wt(1:FILTER_LENGTH+1) / SUM(wt(1:FILTER_LENGTH+1))

RETURN
END SUBROUTINE GetFilterWeightsStg

!------------------------------------------------------------------------------

REAL FUNCTION FilterFac( tc,tp ) RESULT( f )

IMPLICIT NONE

!------ Filter function for non-integer locations

REAL, INTENT( IN ) :: tc  !Center location of filter (normalized)
REAL, INTENT( IN ) :: tp  !Location to evaluate filter weight (normalized)

INTEGER i, ip
REAL    t, r

!t = MIN(ABS(tp-tc),2.0)
!f = 0.25*(2.-t)

t  = SWIMrlimit(tp-tc,-FLOAT(HALF_LENGTH),FLOAT(HALF_LENGTH))
i  = FLOOR(t)
ip = MIN(i+1,HALF_LENGTH)
r  = t - FLOAT(i)
f = (1.-r)*FILTER_WT(i) + r*FILTER_WT(ip)

RETURN
END FUNCTION FilterFac

!------------------------------------------------------------------------------

REAL FUNCTION FilterFacStg( tc,tp ) RESULT( f )

IMPLICIT NONE

!------ Filter function for velocity locations on a staggered grid

REAL, INTENT( IN ) :: tc  !Center location of filter (normalized)
REAL, INTENT( IN ) :: tp  !Location to evaluate filter weight (normalized)

INTEGER i, ip
REAL    t, r

!t = MIN(ABS(tp-tc),1.5)
!f = 0.5*MIN(1.,1.5-t)

t  = SWIMrlimit(tp-tc,-FLOAT(HALF_LENGTH)-0.5,FLOAT(HALF_LENGTH)+0.5)
i  = FLOOR(t+0.5)
ip = MIN(i+1,HALF_LENGTH+1)
r  = t - (FLOAT(i)-0.5)

f = (1.-r)*FILTER_WT_STG(i) + r*FILTER_WT_STG(ip)

RETURN
END FUNCTION FilterFacStg

END MODULE FilterDefinition

!==============================================================================

