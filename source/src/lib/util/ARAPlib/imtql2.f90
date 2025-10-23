SUBROUTINE IMTQL2(NM,N,D,E,Z,IERR)
!***BEGIN PROLOGUE  IMTQL2
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  D4A5,D4C2A
!***KEYWORDS  LIBRARY=SLATEC(EISPACK),TYPE=SINGLE PRECISION(IMTQL2-S),
!             EIGENVALUES,EIGENVECTORS
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Computes eigenvalues and eigenvectors of symmetric
!            tridiagonal matrix using implicit QL method.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure IMTQL2,
!     NUM. MATH. 12, 377-383(1968) by Martin and Wilkinson,
!     as modified in NUM. MATH. 15, 450(1970) by Dubrulle.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
!
!     This subroutine finds the eigenvalues and eigenvectors
!     of a SYMMETRIC TRIDIAGONAL matrix by the implicit QL method.
!     The eigenvectors of a FULL SYMMETRIC matrix can also
!     be found if  TRED2  has been used to reduce this
!     full matrix to tridiagonal form.
!
!     On INPUT
!
!        NM must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        N is the order of the matrix.
!
!        D contains the diagonal elements of the input matrix.
!
!        E contains the subdiagonal elements of the input matrix
!          in its last N-1 positions.  E(1) is arbitrary.
!
!        Z contains the transformation matrix produced in the
!          reduction by  TRED2, if performed.  If the eigenvectors
!          of the tridiagonal matrix are desired, Z must contain
!          the identity matrix.
!
!      On OUTPUT
!
!        D contains the eigenvalues in ASCENDING order.  If an
!          error exit is made, the eigenvalues are correct but
!          UNORDERED for indices 1,2,...,IERR-1.
!
!        E has been destroyed.
!
!        Z contains orthonormal eigenvectors of the symmetric
!          tridiagonal (or full) matrix.  If an error exit is made,
!          Z contains the eigenvectors associated with the stored
!          eigenvalues.
!
!        IERR is set to
!          ZERO       for normal return,
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
!***END PROLOGUE  IMTQL2
!
INTEGER I,J,K,L,M,N,II,NM,MML,IERR
REAL D(N),E(N),Z(NM,N)
REAL B,C,F,G,P,R,S,S1,S2
REAL PYTHAG

!***FIRST EXECUTABLE STATEMENT  IMTQL2

IERR = 0
IF (N == 1) GO TO 1001

DO I = 2, N
  E(I-1) = E(I)
END DO
!
E(N) = 0.0E0

Lloop : DO L = 1, N
  J = 0
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  DO
     DO M = L, N
        IF (M == N) EXIT
        S1 = ABS(D(M)) + ABS(D(M+1))
        S2 = S1 + ABS(E(M))
        IF (S2 == S1) EXIT
     END DO

     P = D(L)
     IF (M == L) CYCLE Lloop
     IF (J == 30) GO TO 1000
     J = J + 1
!     .......... FORM SHIFT ..........
     G = (D(L+1) - P) / (2.0E0 * E(L))
     R = PYTHAG(G,1.0E0)
     G = D(M) - P + E(L) / (G + SIGN(R,G))
     S = 1.0E0
     C = 1.0E0
     P = 0.0E0
     MML = M - L
!.......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
     DO II = 1, MML
        I = M - II
        F = S * E(I)
        B = C * E(I)
        IF (ABS(F) >= ABS(G)) THEN
          C = G / F
          R = SQRT(C*C+1.0E0)
          E(I+1) = F * R
          S = 1.0E0 / R
          C = C * S
        ELSE
          S = F / G
          R = SQRT(S*S+1.0E0)
          E(I+1) = G * R
          C = 1.0E0 / R
          S = S * C
        END IF
        G = D(I+1) - P
        R = (D(I) - G) * S + 2.0E0 * C * B
        P = S * R
        D(I+1) = G + P
        G = C * R - B
!.......... FORM VECTOR ..........
        DO K = 1, N
           F = Z(K,I+1)
           Z(K,I+1) = S * Z(K,I) + C * F
           Z(K,I) = C * Z(K,I) - S * F
        END DO

     END DO

     D(L) = D(L) - P
     E(L) = G
     E(M) = 0.0E0

  END DO

END DO Lloop
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
IIloop :DO II = 2, N
   I = II - 1
   K = I
   P = D(I)

   DO J = II, N
      IF (D(J) >= P) CYCLE
      K = J
      P = D(J)
   END DO

   IF (K == I) CYCLE IIloop
   D(K) = D(I)
   D(I) = P

   DO J = 1, N
      P = Z(J,I)
      Z(J,I) = Z(J,K)
      Z(J,K) = P
   END DO

END DO IIloop

GO TO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 IERR = L
1001 RETURN

END
