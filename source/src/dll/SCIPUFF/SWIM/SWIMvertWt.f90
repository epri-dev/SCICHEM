MODULE SWIMvertWt

USE SWIMparam_fd

CONTAINS

RECURSIVE SUBROUTINE CalcVertWt( nzp,zp,hp,nzObs,zObs,hObs,vscaleTop,vscaleBot,VertWt,k1,k2,fac,zmax )

!------ Array of vertical weights; also interpolation arrays k1, k2 & fac

IMPLICIT NONE

INTEGER, INTENT( IN ) :: nzp       !No. of interpolation heights
REAL,    INTENT( IN ) :: hp        !Terrain height at interpolation point
INTEGER, INTENT( IN ) :: nzObs     !No. of obs heights
REAL,    INTENT( IN ) :: hObs      !Terrain height at observation location
REAL,    INTENT( IN ) :: vscaleTop !Scale for upward extrapolation
REAL,    INTENT( IN ) :: vscaleBot !Scale for downward extrapolation
REAL,    INTENT( IN ) :: zmax      !Highest obs level (for all obs)

REAL, DIMENSION(nzp), INTENT( IN ) :: zp   !Interpolation heights (AGL)
REAL, DIMENSION(:),   POINTER      :: zObs !Observation heights (above reference)

REAL,    DIMENSION(nzp), INTENT( OUT ) :: VertWt !Vertical interpolation weight
REAL,    DIMENSION(nzp), INTENT( OUT ) :: fac    !Linear interpolation factor for observation at k2
INTEGER, DIMENSION(nzp), INTENT( OUT ) :: k1, k2 !Observation levels bracketing interpolation pt

INTEGER k, km, kp
REAL    zpx, zh, delz, dh, dz2, b2

REAL, EXTERNAL :: VertInflScale

!------ Interpolation height is grid level (AGL) plus greater of
!       terrain elevation at grid point or observation location.
!       (Note that observation heights are above reference level, not ground level)

km = 1
kp = 1

DO k = 1,nzp

  zpx = zp(k) + MAX( hp,hObs )

!------ Find bracketing obs levels

  DO

    IF( kp == nzObs )EXIT     !Exit if at top of obs array

    IF( zpx > zObs(kp) )THEN  !If interpolation height is greater than
      km = kp                 !obs level, increment to next level
      kp = kp + 1
    ELSE                      !Otherwise, kp is first obs level above
      EXIT                    !interpolation height
    END IF

  END DO

  k1(k) = km                  !Save bracketing levels for linear interpolation
  k2(k) = kp

!------ Set interpolation factors and vertical distance for weights

!------ Below (or equal) lowest obs level

  IF( zpx <= zObs(km) )THEN

    delz   = zpx - zObs(km)
    fac(k) = 1.
    zh     = zObs(km) - hObs
    zh     = MAX( zh,zp(k) )
    k2(k)  = k1(k)

    IF( ANY(vscaleBot == (/-OBP_NODATA,0./)) )THEN
      b2 = VertInflScale( zp(k) )
    ELSE
      b2 = vscaleBot
    END IF

!------ Above (or equal) highest obs level

  ELSE IF( zpx >= zObs(kp) )THEN

    delz   = zpx - zObs(kp)
    fac(k) = 0.
    zh     = zObs(kp) - hObs
    zh     = MAX( zh,zp(k) )
    k1(k)  = k2(k)

    IF( ANY(vscaleTop == (/-OBP_NODATA,0./)) )THEN
      b2 = VertInflScale( zp(k) )
    ELSE
      b2 = vscaleTop
    END IF

!------ Set linear interpolating factor and distance from nearest level

  ELSE

    delz   = MIN( zObs(kp)- zpx , zpx - zObs(km) )
    fac(k) = (zObs(kp) - zpx) / (zObs(kp) - zObs(km))
    zh     = zpx - hObs
    zh     = MAX( zh,zp(k) )
    b2     = VertInflScale( zh )

  END IF

  b2 = 1./b2**2

!------ Define an effective vertical distance accounting for differences in terrain elevation

  dh = ABS( hp-hObs )
  dh = dh*dh*dh/(dh*dh+zh*zh+1.E-10)

  IF( zmax > 0. .AND. zp(k)+hp > zmax )THEN
    dz2 = zmax-zObs(nzObs) + MAX(hp,hObs) + dh
    dz2 = dz2*dz2
    VertWt(k)  = 1. / ((1. + dz2*b2 )*(1. + (zp(k)+hp-zmax)**2*b2))
  ELSE
    dz2 = delz + dh
    dz2 = dz2*dz2
    VertWt(k)  = 1. / ( 1. + dz2*b2 )
  END IF

END DO

RETURN

END SUBROUTINE CalcVertWt

END MODULE SWIMvertWt
