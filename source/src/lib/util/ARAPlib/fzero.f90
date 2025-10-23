RECURSIVE SUBROUTINE fzero( Func,b,c,re,ae,iflag )

!***DATE WRITTEN   700901   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***REVISION DATE  020410   (YYMMDD) [CONVERSION TO Fortran 90]
!***CATEGORY NO.  F1B
!***KEYWORDS  LIBRARY=SLATEC,TYPE=SINGLE PRECISION(FZERO-S DFZERO-D),
!             BISECTION,NONLINEAR,NONLINEAR EQUATIONS,ROOTS,ZEROES,
!             ZEROS
!***AUTHOR  SHAMPINE,L.F.,SNLA
!           WATTS,H.A.,SNLA
!***PURPOSE  FZERO searches for a zero of a function Func(X) in a given
!            interval (B,C).  It is designed primarily for problems
!            where Func(B) and Func(C) have opposite signs.
!***DESCRIPTION
!
!     Based on a method by T J Dekker
!     written by L F Shampine and H A Watts
!
!            FZERO searches for a zero of a function Func(X) between
!            the given values B and C until the width of the interval
!            (B,C) has collapsed to within a tolerance specified by
!            the stopping criterion, ABS(B-C) <= 2.*(RW*ABS(B)+AE).
!            The method used is an efficient combination of bisection
!            and the secant rule.
!
!     Description Of Arguments
!
!     Func,B,C,R,RE and AE are input parameters
!     B,C and IFLAG are output parameters (flagged by an * below)
!
!        Func  - Name of the real valued external function.  This name
!                must be in an EXTERNAL statement in the calling
!                program.  F must be a function of one real argument.
!
!       *B     - One end of the interval (B,C).  The value returned for
!                B usually is the better approximation to a zero of F.
!
!       *C     - The other end of the interval (B,C)
!
!        R     - A (better) guess of a zero of Func which could help in  **** Removed ***
!                speeding up convergence.  If Func(B) and Func(R) have
!                opposite signs, a root will be found in the interval
!                (B,R); if not, but Func(R) and Func(C) have opposite
!                signs, a root will be found in the interval (R,C);
!                otherwise, the interval (B,C) will be searched for a
!                possible root.  When no better guess is known, it is
!                recommended that r be set to B or C; because if R is
!                not interior to the interval (B,C), it will be ignored.
!
!        RE    - Relative error used for RW in the stopping criterion.
!                If the requested RE is less than machine precision,
!                then RW is set to approximately machine precision.
!
!        AE    - Absolute error used in the stopping criterion.  If the
!                given interval (B,C) contains the origin, then a
!                nonzero value should be chosen for AE.
!
!       *IFLAG - A status code.  User must check IFLAG after each call.
!                Control returns to the user from FZERO in all cases.
!                XERROR does not process diagnostics in these cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   Func(X) decreased in magnitude as (B,C) collapsed.
!
!                2  Func(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of Func(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   Func(X) increased in magnitude as (B,C) collapsed,i.e.
!                     abs(Func(B out)) > max(abs(Func(B in)),abs(Func(C in)))
!
!                4  No change in sign of Func(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of Func(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many function evaluations used.
!
!***REFERENCES  L. F. SHAMPINE AND H. A. WATTS, *FZERO, A ROOT-SOLVING
!                 CODE*, SC-TM-70-631, SEPTEMBER 1970.
!               T. J. DEKKER, *FINDING A ZERO BY MEANS OF SUCCESSIVE
!                 LINEAR INTERPOLATION*, 'CONSTRUCTIVE ASPECTS OF THE
!                 FUNDAMENTAL THEOREM OF ALGEBRA', EDITED BY B. DEJON
!                 P. HENRICI, 1969.

!------ Modifed by D. Henn
!       1) Fortran 90 style
!       2) Set immediate error if bracketing values Func(B) and Func(C) are not
!          opposite signs

IMPLICIT NONE

REAL,    INTENT( INOUT ) :: b, c
REAL,    INTENT( IN    ) :: re, ae
INTEGER, INTENT( OUT   ) :: iflag

INTEGER, PARAMETER :: MAX_ITER = 100

REAL    a, acbs, acmb, aw, cmb, er, p, q
REAL    fa, fb, fc, fx
REAL    rw, tol
INTEGER ic, count

REAL, EXTERNAL :: Func

!------ Set error tolerances

er = 2.0 * TINY(1.0)
rw = MAX(re,er)
aw = MAX(ae,0.0)

count = 0 !Iteration count
ic    = 0

!------ Get function at bracketing values

fc = Func( c )
fb = Func( b )

!------ Check if sign changes in initial interval [b,c]; if not set error

IF( fc*fb > 0. )THEN
  iflag = 4
  GOTO 9999
END IF

a    = c
fa   = fc
acbs = ABS(b-c)
fx   = MAX(ABS(fb),ABS(fc))

!------ Iteration loop

DO

!------ Interchange such that |Func(b)| < |Func(c)|

  IF( ABS(fc) < ABS(fb) )THEN
    a  = b
    fa = fb
    b  = c
    fb = fc
    c  = a
    fc = fa
  END IF

  cmb  = 0.5*(c-b)
  acmb = ABS(cmb)
  tol  = rw*ABS(b) + aw

!------ Test stopping criteria; set iflag appropriately

  IF( acmb <= tol )THEN
    IF( fb*fc > 0. )THEN
      iflag = 4
    ELSE IF( ABS(fb) > fx )THEN
      iflag = 3
    ELSE
      iflag = 1
    END IF
    IF( ABS(fb) > ABS(fc) )THEN  !Make sure last call is for smallest function value
      fc = Func( c )
    ELSE
      fb = Func( b )
    END IF
    EXIT

  ELSE IF( fb == 0.0 )THEN
   iflag = 2;  EXIT

  ELSE IF( count >= MAX_ITER )THEN
   iflag = 5; EXIT

  END IF

!------ Calculate new iterate implicitly as b+p/q; arrange such that p > 0.

  p = (b-a)*fb
  q = fa-fb
  IF( p < 0. )THEN
    p = -p
    q = -q
  END IF

!------ Update a and check for satisfactory reduction in the size of the
!       bracketing interval every four increments; if not, perform bisection.

  a  = b
  fa = fb

  ic = ic + 1

  IF( ic >= 4 )THEN

    IF( 8.*acmb >= acbs )THEN
      p = HUGE(1.) !To force bisection
    ELSE
      ic = 0
      acbs = acmb
    END IF

  END IF

!------ Get new iterate

  IF( p < ABS(q)*tol )THEN !If too small, increment by tolerance
    b = b + SIGN(tol,cmb)
  ELSE IF( p < cmb*q )THEN !Use secant rule if root is between b and (c+b)/2
    b = b + p/q
  ELSE                     !Othewise, use bisection
    b = 0.5*(c+b)
  END IF

!------ Evaluate function at new iterate b

  fb = Func( b )

!------ Decide whether next step is interpolation or extrapolation

  IF( fb*fc > 0. )THEN
    c  = a
    fc = fa
  END IF

  count = count + 1

END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
! Double Precision version of fzero
!*******************************************************************************
RECURSIVE SUBROUTINE dzero( Func,b,c,re,ae,iflag )

!***DATE WRITTEN   700901   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***REVISION DATE  020410   (YYMMDD) [CONVERSION TO Fortran 90]
!***CATEGORY NO.  F1B
!***KEYWORDS  LIBRARY=SLATEC,TYPE=SINGLE PRECISION(FZERO-S DFZERO-D),
!             BISECTION,NONLINEAR,NONLINEAR EQUATIONS,ROOTS,ZEROES,
!             ZEROS
!***AUTHOR  SHAMPINE,L.F.,SNLA
!           WATTS,H.A.,SNLA
!***PURPOSE  FZERO searches for a zero of a function Func(X) in a given
!            interval (B,C).  It is designed primarily for problems
!            where Func(B) and Func(C) have opposite signs.
!***DESCRIPTION
!
!     Based on a method by T J Dekker
!     written by L F Shampine and H A Watts
!
!            FZERO searches for a zero of a function Func(X) between
!            the given values B and C until the width of the interval
!            (B,C) has collapsed to within a tolerance specified by
!            the stopping criterion, ABS(B-C) <= 2.*(RW*ABS(B)+AE).
!            The method used is an efficient combination of bisection
!            and the secant rule.
!
!     Description Of Arguments
!
!     Func,B,C,R,RE and AE are input parameters
!     B,C and IFLAG are output parameters (flagged by an * below)
!
!        Func  - Name of the real valued external function.  This name
!                must be in an EXTERNAL statement in the calling
!                program.  F must be a function of one real argument.
!
!       *B     - One end of the interval (B,C).  The value returned for
!                B usually is the better approximation to a zero of F.
!
!       *C     - The other end of the interval (B,C)
!
!        R     - A (better) guess of a zero of Func which could help in  **** Removed ***
!                speeding up convergence.  If Func(B) and Func(R) have
!                opposite signs, a root will be found in the interval
!                (B,R); if not, but Func(R) and Func(C) have opposite
!                signs, a root will be found in the interval (R,C);
!                otherwise, the interval (B,C) will be searched for a
!                possible root.  When no better guess is known, it is
!                recommended that r be set to B or C; because if R is
!                not interior to the interval (B,C), it will be ignored.
!
!        RE    - Relative error used for RW in the stopping criterion.
!                If the requested RE is less than machine precision,
!                then RW is set to approximately machine precision.
!
!        AE    - Absolute error used in the stopping criterion.  If the
!                given interval (B,C) contains the origin, then a
!                nonzero value should be chosen for AE.
!
!       *IFLAG - A status code.  User must check IFLAG after each call.
!                Control returns to the user from FZERO in all cases.
!                XERROR does not process diagnostics in these cases.
!
!                1  B is within the requested tolerance of a zero.
!                   The interval (B,C) collapsed to the requested
!                   tolerance, the function changes sign in (B,C), and
!                   Func(X) decreased in magnitude as (B,C) collapsed.
!
!                2  Func(B) = 0.  However, the interval (B,C) may not have
!                   collapsed to the requested tolerance.
!
!                3  B may be near a singular point of Func(X).
!                   The interval (B,C) collapsed to the requested tol-
!                   erance and the function changes sign in (B,C), but
!                   Func(X) increased in magnitude as (B,C) collapsed,i.e.
!                     abs(Func(B out)) > max(abs(Func(B in)),abs(Func(C in)))
!
!                4  No change in sign of Func(X) was found although the
!                   interval (B,C) collapsed to the requested tolerance.
!                   The user must examine this case and decide whether
!                   B is near a local minimum of Func(X), or B is near a
!                   zero of even multiplicity, or neither of these.
!
!                5  Too many function evaluations used.
!
!***REFERENCES  L. F. SHAMPINE AND H. A. WATTS, *FZERO, A ROOT-SOLVING
!                 CODE*, SC-TM-70-631, SEPTEMBER 1970.
!               T. J. DEKKER, *FINDING A ZERO BY MEANS OF SUCCESSIVE
!                 LINEAR INTERPOLATION*, 'CONSTRUCTIVE ASPECTS OF THE
!                 FUNDAMENTAL THEOREM OF ALGEBRA', EDITED BY B. DEJON
!                 P. HENRICI, 1969.

!------ Modifed by D. Henn
!       1) Fortran 90 style
!       2) Set immediate error if bracketing values Func(B) and Func(C) are not
!          opposite signs

IMPLICIT NONE

REAL(8),    INTENT( INOUT ) :: b, c
REAL(8),    INTENT( IN    ) :: re, ae
INTEGER, INTENT( OUT   ) :: iflag

INTEGER, PARAMETER :: MAX_ITER = 100

REAL(8)    a, acbs, acmb, aw, cmb, er, p, q
REAL(8)    fa, fb, fc, fx
REAL(8)    rw, tol
INTEGER ic, count

REAL(8), EXTERNAL :: Func

!------ Set error tolerances

er = 2.0D0 * TINY(1.0D0)
rw = MAX(re,er)
aw = MAX(ae,0.0D0)

count = 0 !Iteration count
ic    = 0

!------ Get function at bracketing values

fc = Func( c )
fb = Func( b )

!------ Check if sign changes in initial interval [b,c]; if not set error

IF( fc*fb > 0.D0 )THEN
  iflag = 4
  GOTO 9999
END IF

a    = c
fa   = fc
acbs = ABS(b-c)
fx   = MAX(ABS(fb),ABS(fc))

!------ Iteration loop

DO

!------ Interchange such that |Func(b)| < |Func(c)|

  IF( ABS(fc) < ABS(fb) )THEN
    a  = b
    fa = fb
    b  = c
    fb = fc
    c  = a
    fc = fa
  END IF

  cmb  = 0.5D0*(c-b)
  acmb = ABS(cmb)
  tol  = rw*ABS(b) + aw

!------ Test stopping criteria; set iflag appropriately

  IF( acmb <= tol )THEN
    IF( fb*fc > 0.D0 )THEN
      iflag = 4
    ELSE IF( ABS(fb) > fx )THEN
      iflag = 3
    ELSE
      iflag = 1
    END IF
    IF( ABS(fb) > ABS(fc) )THEN  !Make sure last call is for smallest function value
      fc = Func( c )
    ELSE
      fb = Func( b )
    END IF
    EXIT

  ELSE IF( fb == 0.0D0 )THEN
   iflag = 2;  EXIT

  ELSE IF( count >= MAX_ITER )THEN
   iflag = 5; EXIT

  END IF

!------ Calculate new iterate implicitly as b+p/q; arrange such that p > 0.

  p = (b-a)*fb
  q = fa-fb
  IF( p < 0.D0 )THEN
    p = -p
    q = -q
  END IF

!------ Update a and check for satisfactory reduction in the size of the
!       bracketing interval every four increments; if not, perform bisection.

  a  = b
  fa = fb

  ic = ic + 1

  IF( ic >= 4 )THEN

    IF( 8.D0*acmb >= acbs )THEN
      p = HUGE(1.D0) !To force bisection
    ELSE
      ic = 0
      acbs = acmb
    END IF

  END IF

!------ Get new iterate

  IF( p < ABS(q)*tol )THEN !If too small, increment by tolerance
    b = b + SIGN(tol,cmb)
  ELSE IF( p < cmb*q )THEN !Use secant rule if root is between b and (c+b)/2
    b = b + p/q
  ELSE                     !Othewise, use bisection
    b = 0.5D0*(c+b)
  END IF

!------ Evaluate function at new iterate b

  fb = Func( b )

!------ Decide whether next step is interpolation or extrapolation

  IF( fb*fc > 0.D0 )THEN
    c  = a
    fc = fa
  END IF

  count = count + 1

END DO

9999 CONTINUE

RETURN
END
