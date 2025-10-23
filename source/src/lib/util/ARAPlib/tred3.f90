SUBROUTINE TRED3(N,NV,A,D,E,E2)
!***BEGIN PROLOGUE  TRED3
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  D4C1B1
!***KEYWORDS  LIBRARY=SLATEC(EISPACK),TYPE=SINGLE PRECISION(TRED3-S),
!             EIGENVALUES,EIGENVECTORS
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Reduce real symmetric matrix stored in packed form to
!            symmetric tridiagonal matrix using orthogonal
!            transformations.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TRED3,
!     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     This subroutine reduces a REAL SYMMETRIC matrix, stored as
!     a one-dimensional array, to a symmetric tridiagonal matrix
!     using orthogonal similarity transformations.
!
!     On Input
!
!        n is the order of the matrix.
!
!        NV must be set to the dimension of the array parameter A
!          as declared in the calling program dimension statement.
!
!        A contains the lower triangle of the real symmetric
!          input matrix, stored row-wise as a one-dimensional
!          array, in its first N*(N+1)/2 positions.
!
!     On Output
!
!        A contains information about the orthogonal
!          transformations used in the reduction.
!
!        D contains the diagonal elements of the tridiagonal matrix.
!
!        E contains the subdiagonal elements of the tridiagonal
!          matrix in its last N-1 positions.  E(1) is set to zero.
!
!        E2 contains the squares of the corresponding elements of E.
!          E2 may coincide with E if the squares are not needed.
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  TRED3
!
IMPLICIT NONE

INTEGER I,J,K,L,N,II,IZ,JK,NV
REAL A(NV),D(N),E(N),E2(N)
REAL F,G,H,HH,SCALE

!.......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
!***FIRST EXECUTABLE STATEMENT  TRED3

DO II = 1, N

  I  = N + 1 - II
  L  = I - 1
  IZ = (I * L) / 2
  H  = 0.0E0
  SCALE = 0.0E0

  IF (L < 1) GO TO 130

!. SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........

  DO K = 1, L
    IZ = IZ + 1
    D(K)  = A(IZ)
    SCALE = SCALE + ABS(D(K))
  END DO

  IF (SCALE /= 0.0E0) GO TO 140

130 CONTINUE

  E(I)  = 0.0E0
  E2(I) = 0.0E0
  GO TO 290

140 CONTINUE

  DO K = 1, L
    D(K) = D(K) / SCALE
    H    = H + D(K) * D(K)
  END DO

  E2(I) = SCALE * SCALE * H
  F     = D(L)
  G     = -SIGN(SQRT(H),F)
  E(I)  = SCALE * G
  H     = H - F * G
  D(L)  = F - G
  A(IZ) = SCALE * D(L)

  IF( L /= 1 )THEN

    F = 0.0E0

    DO J = 1, L
      G  = 0.0E0
      JK = (J * (J-1)) / 2

!. FORM ELEMENT OF A*U ..........

      DO K = 1, L
        JK = JK + 1
        IF (K > J) JK = JK + K - 2
        G = G + A(JK) * D(K)
      END DO

!. FORM ELEMENT OF P ..........

      E(J) = G / H
      F    = F + E(J) * D(J)
    END DO

    HH = F / (H + H)
    JK = 0

!. FORM REDUCED A ..........

    DO J = 1, L
      F = D(J)
      G = E(J) - HH * F
      E(J) = G

      DO K = 1, J
        JK = JK + 1
        A(JK) = A(JK) - F * E(K) - G * D(K)
      END DO
    END DO

  END IF

290 CONTINUE

  D(I)    = A(IZ+1)
  A(IZ+1) = SCALE * SQRT(H)

END DO

RETURN
END
