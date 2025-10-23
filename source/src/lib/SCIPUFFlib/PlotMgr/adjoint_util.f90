REAL FUNCTION RatioSetSourceLocProb( flag,mass )

USE adjoint_fi
USE scipuff_fi
USE error_fi

IMPLICIT NONE

LOGICAL, INTENT( IN    ) :: flag
REAL,    INTENT( INOUT ) :: mass

REAL, PARAMETER :: GAMMA_MIN = 1.0E-07
REAL, PARAMETER :: CUTOFF    = 1.0E-20

LOGICAL lSatI, lSatJ
INTEGER i, j, nwt, ntot, imat, jmat
REAL    rat, prb, gam, dist, dcrit, cstarI, cstarJ, rmax
REAL    wtt, sumwt, wtmxp, wtmax

REAL, DIMENSION(nHit)  :: wtp
REAL, DIMENSION(nNull) :: prbX

REAL, EXTERNAL :: ProbSrc

RatioSetSourceLocProb = 0.0

mass  = 0.0
sumwt = 0.0
wtmax = 0.0
wtmxp = 0.0
wtp   = 0.0

DO i = 1,nHit

  IF( Tmat(i)%m%mean <= CUTOFF )CYCLE
  gam = ProbSrc( Tmat(i)%m%mean,Tmat(i)%m%sig,0.0 )
  IF( gam <= GAMMA_MIN )THEN
    Tmat(i)%m%mean = 0.0
    CYCLE
  END IF
  imat  = Tmat(i)%imat
  wtp(i) = MIN(1.0,gam/gamT(imat))
  wt(i)  = 1.0
  DO j = 1,nNull
    prbX(j) = 1.0
    IF( Nmat(j)%m%mean <= MAX(CUTOFF,0.005*Nmat(j)%m%sig) )CYCLE
    prb = ProbSrc( Nmat(j)%m%mean,Nmat(j)%m%sig,Nmat(j)%m%min*Tmat(i)%m%mean/gamT(imat) )
    IF( Probabilistic )THEN
      jmat = Nmat(j)%imat
      dist = (AdjDistS(imat,jmat)/MAX(Amat(imat)%ScaleS,Amat(jmat)%ScaleS))**2 &
           + (AdjDistT(imat,jmat)/MAX(Amat(imat)%ScaleT,Amat(jmat)%ScaleT))**2 &
           + (AdjDistV(imat,jmat)/MAX(Amat(imat)%ScaleV,Amat(jmat)%ScaleV))**2
      IF( dist < 1.0 )prb = prb*(dist+(1.0-dist)/ProbSrc( Nmat(j)%m%mean,Nmat(j)%m%sig,0.0 ))
    END IF
    prbX(j) = 1.0 - prb
  END DO

  CALL usort( prbX,nNull )
  DO j = 1,MIN(nNull,nHit/2)
    wt(i) = wt(i)*prbX(j)
  END DO

  IF( flag .AND. gam > 1.0E-4 )THEN
    wtt   = wtp(i)*gamT(imat)
    mass  = mass + wtt*gam/Tmat(i)%m%mean
    sumwt = sumwt + wtt
  END IF

  wt(i) = SQRT(wt(i))
  wtmax = MAX(wtmax,wt(i))
  wtmxp = MAX(wtmxp,wtp(i))

END DO

nwt = 0
DO i = 1,nHit-1
  IF( Tmat(i)%m%mean <= CUTOFF )EXIT
  IF( wt(i) < 0.01*wtmax )CYCLE
  IF( nwt == 0 .AND. i > nHit/2 )EXIT
  imat = Tmat(i)%imat
  cstarI = Tmat(i)%m%mean/gamT(imat)
  lSatI = i>nTrigger
  DO j = i+1,nHit
    IF( Tmat(j)%m%mean <= CUTOFF )CYCLE
    wtt = wt(j)*wt(i)*MAX(wtp(i),wtp(j))
    IF( wtt < 0.01*wtmxp*wtmax )CYCLE
    jmat = Tmat(j)%imat
    cstarJ = Tmat(j)%m%mean/gamT(jmat)
    lSatJ = j>nTrigger
    IF( cstarJ > cstarI )THEN
      IF( lSatJ )THEN
        rat = 1.0
      ELSE
        rat = cstarI/cstarJ
      END IF
      gam = gamT(jmat)
    ELSE
      IF( lSatI )THEN
        rat = 1.0
      ELSE
        rat = cstarJ/cstarI
      END IF
      gam = gamT(imat)
    END IF
    IF( Probabilistic )THEN
      dist = (AdjHitDistS(imat,jmat)/MAX(Amat(imat)%ScaleS,Amat(jmat)%ScaleS))**2 &
           + (AdjHitDistT(imat,jmat)/MAX(Amat(imat)%ScaleT,Amat(jmat)%ScaleT))**2 &
           + (   AdjDistV(imat,jmat)/MAX(Amat(imat)%ScaleV,Amat(jmat)%ScaleV))**2
      dcrit = MIN(3.,MAX(1.0,2.0*ABS(LOG(rat))))
      dist  = dist/dcrit
      dcrit = dcrit*gam
      rmax  = (rat*dcrit + 1.0)/(1.0+dcrit)
      IF( dist > 1.0 )THEN
        IF( dist > 1.5 )THEN
          rat = rmax / (dist-0.5)**4
        ELSE
          rat = rmax
        END IF
      ELSE
        dist = dist**4
        rat  = rmax*dist + rat*(1.0-dist)
      END IF
    END IF
    RatioSetSourceLocProb = RatioSetSourceLocProb + rat*wtt
    nwt = nwt + 1
  END DO
END DO

ntot = nHit*(nHit-1)/2
RatioSetSourceLocProb = RatioSetSourceLocProb*FLOAT(nwt)/FLOAT(ntot*ntot)

IF( flag )THEN
  IF( sumwt > 0.0 )THEN
    mass = mass/sumwt
  ELSE
    mass = special
  END IF
END IF
RETURN
END

!------------------------------------------------------------------------------

REAL FUNCTION ProbSrc( cbar,csig,cmin )

!--- Calculates probability of exceeding threshold based on clipped normal

USE constants_fd

IMPLICIT NONE

REAL, INTENT( IN ) :: cbar   !Mean value
REAL, INTENT( IN ) :: csig   !Std. deviation
REAL, INTENT( IN ) :: cmin   !Threshold value

REAL soc, gbar, sigg, arg

REAL, EXTERNAL :: erfc

soc = MAX(0.001,csig/cbar)

CALL clnpar( soc,gbar,sigg )

arg = (cmin/cbar - gbar)/sigg

!---- find probability that conc is greater than cmin

IF( arg > 5.0 )THEN
  ProbSrc = 0.
ELSE IF( arg < -5.0 )THEN
  ProbSrc = 1.
ELSE
  ProbSrc = 0.5*erfc( arg/SQRT2 )
END IF

RETURN
END
