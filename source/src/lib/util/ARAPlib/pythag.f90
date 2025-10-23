REAL FUNCTION PYTHAG(A,B)
!***BEGIN PROLOGUE  PYTHAG
!***REFER TO  EISDOC
!
!     Finds sqrt(A**2+B**2) without overflow or destructive underflow
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  PYTHAG
REAL A,B
!
REAL P,Q,R,S,T
!***FIRST EXECUTABLE STATEMENT  PYTHAG
P = MAX(ABS(A),ABS(B))
Q = MIN(ABS(A),ABS(B))

IF( Q /= 0.0E0 )THEN
  DO
    R = (Q/P)**2
    T = 4.0E0 + R
    IF( T == 4.0E0 )EXIT
    S = R/T
    P = P + 2.0E0*P*S
    Q = Q*S
  END DO
END IF

PYTHAG = P

RETURN
END
