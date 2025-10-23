REAL FUNCTION xfun( v1,v2,ifun,val,small,spv )

IMPLICIT NONE

REAL, PARAMETER :: GAMMA_MIN = 1.e-4

REAL v1
REAL v2
REAL val
REAL small
REAL spv
INTEGER ifun

REAL sig, soc, prb, arg, gbar, sigg, gamma, exv

REAL, EXTERNAL :: erfc, erfci

!------ Check for ifun=0 -> no functional operation

IF( ifun == 0 )THEN
  xfun = MAX(v1,small)
  RETURN
END IF

!------ Check for special values

IF( v1 == spv .OR. v2 == spv )THEN
  xfun = spv
  RETURN
END IF

!------Choose the specified functional operation

SELECT CASE( ifun)

  CASE( 1 )     !------ Add

    xfun = MAX( v1 + v2 , small )

  CASE( 2 )     !------ Multiply

  	xfun = MAX( v1 * v2 , small )

  CASE( 3 )     !------ Divide

    IF( v2 > small )THEN
      xfun = MAX( v1/v2 , small )
    ELSE
      xfun = spv
    END IF

  CASE( 4 )     !------ Invert

    IF( v1 <= small )THEN
      xfun = spv
    ELSE
      xfun = MAX( 1./v1 , small )
    END IF

  CASE( 5 )     !------ Square root

    IF( v1 >= 0. )THEN
      xfun = MAX(SQRT(v1),small)
    ELSE
      xfun = spv
    END IF

  CASE( 6 )     !------ Sigma/mean

    sig = SQRT(MAX(v2,0.0))
    IF( ABS(v1) > small )THEN
      xfun = MAX(0.0,sig/v1)
    ELSE
      xfun = spv
    END IF

  CASE( 7 )     !------ Probability of Exceeding - Clipped-normal

    sig = SQRT(MAX(v2,0.0))
    IF( v1 > small )THEN
      soc = MAX(0.001,sig/v1)
      CALL clnpar(soc,gbar,sigg)
      arg = (val/v1 - gbar)/sigg
      IF( arg > 1.E6 )THEN
        prb = 0.
      ELSE IF( arg < -1.E6 )THEN
        prb = 1.
      ELSE
        prb = 0.5*erfc(arg/SQRT(2.))
      END IF
    ELSE
      prb = 0.0
    END IF
    xfun = MAX(prb,small)

  CASE( 8 )     !------ Probability of Exceeding - lognormal

    sig = SQRT(MAX(v2,0.0))
    IF( v1 > small )THEN
      soc = sig/v1
      gbar = -0.5*LOG(1.+soc**2)
      sigg = SQRT(-2.*gbar)
      arg = (LOG(val/v1) - gbar)/sigg
      IF( arg > 1.E6 )THEN
        prb = 0.
      ELSE IF( arg < -1.E6 )THEN
        prb = 1.
      ELSE
        prb = 0.5*erfc(arg/SQRT(2.))
      END IF
    ELSE
      prb = 0.0
    END IF
    xfun = MAX(prb,small)

  CASE( 9 )     !------ Conditional Probability of Exceeding - Clipped-normal

    sig = SQRT(MAX(v2,0.0))
    IF( v1 > small )THEN
      soc = MAX(0.001,sig/v1)
      CALL clnpar(soc,gbar,sigg)
      arg = (val/v1 - gbar)/sigg
      IF( arg > 1.E6 )THEN
        prb = 0.
      ELSE IF( arg < -1.E6 )THEN
        prb = 1.
      ELSE
        prb = 0.5*erfc(arg/SQRT(2.))
        gamma = MAX(0.5*erfc(-(gbar/sigg)/SQRT(2.)),GAMMA_MIN)
        IF( gamma > 0.0 )prb = prb/gamma
      END IF
    ELSE
      prb = 0.0
    END IF
    xfun = MAX(prb,small)

  CASE( 10 )   !------ Inverse Conditional Probability of Exceeding - Clipped Normal

    sig = SQRT(MAX(v2,0.0))
    IF( v1 > small )THEN
      soc = MAX(0.001,sig/v1)
      CALL clnpar(soc,gbar,sigg)
      gamma = MAX(0.5*erfc(-(gbar/sigg)/SQRT(2.)),GAMMA_MIN)
      exv = v1*(erfci(val*gamma)*sigg + gbar)
    ELSE
      exv = 0.0
    END IF
    xfun = MAX(exv,small)

END SELECT

!----- Finished

RETURN
END
