SUBROUTINE TQLRAT(N,D,E2,IERR)
!***BEGIN PROLOGUE  TQLRAT
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  D4A5,D4C2A
!***KEYWORDS  LIBRARY=SLATEC(EISPACK),TYPE=SINGLE PRECISION(TQLRAT-S),
!             EIGENVALUES,EIGENVECTORS
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Computes eigenvalues of symmetric tridiagonal matrix
!            a rational variant of the QL method.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TQLRAT,
!     ALGORITHM 464, COMM. ACM 16, 689(1973) by Reinsch.
!
!     This subroutine finds the eigenvalues of a SYMMETRIC
!     TRIDIAGONAL matrix by the rational QL method.
!
!     On Input
!
!        N is the order of the matrix.
!
!        D contains the diagonal elements of the input matrix.
!
!        E2 contains the squares of the subdiagonal elements of the
!          input matrix in its last N-1 positions.  E2(1) is arbitrary.
!
!      On Output
!
!        D contains the eigenvalues in ascending order.  If an
!          error exit is made, the eigenvalues are correct and
!          ordered for indices 1,2,...IERR-1, but may not be
!          the smallest eigenvalues.
!
!        E2 has been destroyed.
!
!        IERR is set to
!          Zero       for normal return,
!          J          if the J-th eigenvalue has not been
!                     determined after 30 iterations.
!
!     Calls PYTHAG(A,B) for sqrt(A**2 + B**2).
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  PYTHAG
!***END PROLOGUE  TQLRAT

IMPLICIT NONE

INTEGER I,J,L,M,N,II,L1,MML,IERR
REAL D(N),E2(N)
REAL B,C,F,G,H,P,R,S,MACHEP
REAL PYTHAG

SAVE MACHEP
DATA MACHEP/ 1.0 /

!***FIRST EXECUTABLE STATEMENT  TQLRAT

IF( MACHEP == 1.0 )THEN
  DO
    MACHEP = 0.5E0*MACHEP
    IF( 1.0 + MACHEP <= 1.0 )CYCLE
  END DO
  MACHEP = 2.0E0*MACHEP
END IF

IERR = 0

IF (N == 1) RETURN

DO I = 2, N
  E2(I-1) = E2(I)
END DO
!
F = 0.0E0
B = 0.0E0
E2(N) = 0.0E0

DO L = 1, N
  J = 0
  H = MACHEP * (ABS(D(L)) + SQRT(E2(L)))
  IF (B <= H) THEN
    B = H
    C = B * B
  END IF
!     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  DO M = L, N
    IF (E2(M) <= C) EXIT
!     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
  END DO

  IF (M /= L) THEN
    DO
      IF (J == 30) GO TO 1000
      J = J + 1
!     .......... FORM SHIFT ..........
      L1 = L + 1
      S = SQRT(E2(L))
      G = D(L)
      P = (D(L1) - G) / (2.0E0 * S)
      R = PYTHAG(P,1.0E0)
      D(L) = S / (P + SIGN(R,P))
      H = G - D(L)

      DO I = L1, N
        D(I) = D(I) - H
      END DO

      F = F + H
!     .......... RATIONAL QL TRANSFORMATION ..........
      G = D(M)
      IF (G == 0.0E0) G = B
      H = G
      S = 0.0E0
      MML = M - L
!. FOR I=M-1 STEP -1 UNTIL L DO -- ..........
      DO II = 1, MML
        I = M - II
        P = G * H
        R = P + E2(I)
        E2(I+1) = S * R
        S = E2(I) / R
        D(I+1) = H + S * (H + D(I))
        G = D(I) - E2(I) / G
        IF (G == 0.0E0) G = B
        H = G * P / R
      END DO

      E2(L) = S * G
      D(L) = H
!. GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
      IF (H == 0.0E0) EXIT
      IF (ABS(E2(L)) <= ABS(C/H)) EXIT
      E2(L) = H * E2(L)
      IF (E2(L) == 0.0E0) EXIT

    END DO

  END IF

  P = D(L) + F
!.. ORDER EIGENVALUES ..........
  IF (L /= 1) THEN
!.. FOR I=L STEP -1 UNTIL 2 DO -- ..........
    DO II = 2, L
      I = L + 2 - II
      IF (P >= D(I-1)) GO TO 270
      D(I) = D(I-1)
    END DO
  END IF
  I = 1

270 CONTINUE
D(I) = P

END DO

RETURN

!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 CONTINUE
IERR = L
RETURN

END
