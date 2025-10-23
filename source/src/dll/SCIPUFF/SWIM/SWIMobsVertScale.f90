SUBROUTINE SetObsVertScale( Obs,NearObs )

!------ Adjust vertical scale for interpolation if there are other obs
!       nearby and have appropriate levels
!       N.B. use only velocity levels

USE SWIMobs_fd
USE SWIMObsSort

IMPLICIT NONE

TYPE( ObsMet    ), POINTER      :: Obs       !Adjust the vertical scale for THIS obs
TYPE( NearestObs), INTENT( IN ) :: NearObs   !Linked-list of nearest obs

REAL, PARAMETER :: VscaleFac = 0.1
REAL, PARAMETER :: CLOSE_ENOUGH = 100.**2    !(m^2) Close enough to be considered co-located

TYPE( ObsMet ), POINTER :: pObs

INTEGER i, k
REAL    zBot, zTop, s, f, z, s0, sz

REAL, EXTERNAL :: VertWtDistFunc, VertInflScale

IF( Obs%Vel%nz == 0 )RETURN

!------ Loop over nearest neighbors

StaLoop : DO i = 1,MIN(NearObs%numInterp,NearObs%numObs)

  pObs => NearObs%obs(i)%obs

  IF( pObs%Vel%nz == 0 )CYCLE StaLoop

  IF( pObs%Vel%nz == 1 .AND. Obs%Vel%nz == 1 )CYCLE StaLoop

!------ Check distance

  s = 1./NearObs%obs(i)%wt - 1. !Squared actual distance (since "normalized" by unit influence area)
  s = s/CLOSE_ENOUGH            !Check if close enough to be co-located
  IF( s >= 2. )EXIT

!------ Set top & bottom heights relative to pObs terrain

  zBot = Obs%Vel%z(1) - Obs%BL%h + pObs%BL%h
  zTop = Obs%Vel%z(Obs%Vel%nz) - Obs%BL%h + pObs%BL%h

!------- Set scale for upward extrapolation

  IF( pObs%Vel%z(pObs%Vel%nz) > zTop )THEN

    k = pObs%Vel%nz
    IF( k > 1 )THEN
      DO WHILE( pObs%Vel%z(k-1) > zTop )
        k = k - 1
        IF( k == 1 )EXIT
      END DO
    END IF

    f = VertWtDistFunc( s )

    sz = VscaleFac*(pObs%Vel%z(k)-zTop)
    z  = pObs%Vel%z(k) - pObs%BL%h
    s0 = VertInflScale( z )
    s  = f*sz + (1.-f)*s0
    s  = MAX(s,2.)  !Don't let get smaller than 2m (arbitrary)
    Obs%vscaleTop = MIN(Obs%vscaleTop,s)

  END IF

!------- Set scale for downward extrapolation

  IF( pObs%Vel%z(1) < zBot )THEN

    k = 1
    IF( pObs%Vel%nz > 1 )THEN
      DO WHILE( pObs%Vel%z(k+1) < zBot )
        k = k + 1
        IF( k == pObs%Vel%nz )EXIT
      END DO
    END IF

    f = VertWtDistFunc( s )

    sz = VscaleFac*(zBot-pObs%Vel%z(k))
    z  = pObs%Vel%z(k) - pObs%BL%h
    s0 = VertInflScale( z )
    s  = f*sz + (1.-f)*s0
    s  = MAX(s,2.)  !Don't let get smaller than 2m (arbitrary)
    Obs%vscaleBot = MIN(Obs%vscaleBot,s)

  END IF

END DO StaLoop

RETURN
END

!==============================================================================

REAL FUNCTION VertWtDistFunc( s2 ) RESULT( f )

IMPLICIT NONE

REAL, INTENT( IN ) :: s2      !Normalized distance squared

REAL, PARAMETER :: ss = 4.    !Lengthscale factor (and limit) squared

REAL t

IF( s2 > ss )THEN
  f = 0.
ELSE
  t = (s2/ss)**2
  f = 1. - t*(3*t - 2*t*t);
END IF

RETURN
END

!==============================================================================

REAL FUNCTION VertInflScale( z ) RESULT( f )

USE SWIMobs_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: z    !Height AGL

REAL, PARAMETER :: RZ_MIN = 100.                        !Min range above typical BL depth
REAL, PARAMETER :: RZ_MAX = 500.                        !Max range for near surface
REAL, PARAMETER :: ZBL1   = 500., ZBL2 = 1500.
REAL, PARAMETER :: RZ_FAC = (RZ_MAX-RZ_MIN)/(ZBL2-ZBL1)


IF( z < ZBL1 )THEN
  f = RZ_MAX
ELSE IF( z < ZBL2 )THEN
  f = RZ_MAX - (z-ZBL1)*RZ_FAC
ELSE
  f = RZ_MIN
END IF

RETURN
END

