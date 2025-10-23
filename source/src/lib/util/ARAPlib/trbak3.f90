SUBROUTINE TRBAK3(NM,N,NV,A,M,Z)
!***BEGIN PROLOGUE  TRBAK3
!***DATE WRITTEN   760101   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  D4C4
!***KEYWORDS  LIBRARY=SLATEC(EISPACK),TYPE=SINGLE PRECISION(TRBAK3-S),
!             EIGENVALUES,EIGENVECTORS
!***AUTHOR  SMITH, B. T., ET AL.
!***PURPOSE  Forms eigenvectors of real symmetric matrix from the
!            eigenvectors of symmetric tridiagonal matrix formed
!            TRED3.
!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure TRBAK3,
!     NUM. MATH. 11, 181-195(1968) by Martin, Reinsch, and Wilkinson.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     This subroutine forms the eigenvectors of a REAL SYMMETRIC
!     matrix by back transforming those of the corresponding
!     symmetric tridiagonal matrix determined by  TRED3.
!
!     On Input
!
!        NM must be set to the row dimension of two-dimensional
!          array parameters as declared in the calling program
!          dimension statement.
!
!        N is the order of the matrix.
!
!        NV must be set to the dimension of the array parameter A
!          as declared in the calling program dimension statement.
!
!        A contains information about the orthogonal transformations
!          used in the reduction by  TRED3  in its first
!          N*(N+1)/2 positions.
!
!        M is the number of eigenvectors to be back transformed.
!
!        Z contains the eigenvectors to be back transformed
!          in its first M columns.
!
!     On Output
!
!        Z contains the transformed eigenvectors
!          in its first M columns.
!
!     Note that TRBAK3 preserves vector Euclidean norms.
!
!     Questions and comments should be directed to b. s. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!***REFERENCES  B. T. SMITH, J. M. BOYLE, J. J. DONGARRA, B. S. GARBOW,
!                 Y. IKEBE, V. C. KLEMA, C. B. MOLER, *MATRIX EIGEN-
!                 SYSTEM ROUTINES - EISPACK GUIDE*, SPRINGER-VERLAG,
!                 1976.
!***ROUTINES CALLED  (NONE)
!***END PROLOGUE  TRBAK3

IMPLICIT NONE

INTEGER I,J,K,L,M,N,IK,IZ,NM,NV
REAL A(NV),Z(NM,M)
REAL H,S
!
!***FIRST EXECUTABLE STATEMENT  TRBAK3
IF (M == 0) RETURN
IF (N == 1) RETURN
!
DO I = 2, N

  L = I - 1
  IZ = (I * L) / 2
  IK = IZ + I
  H = A(IK)
  IF (H == 0.0E0)CYCLE

  DO J = 1, M
    S = 0.0E0
    IK = IZ

    DO K = 1, L
      IK = IK + 1
      S = S + A(IK) * Z(K,J)
    END DO
! DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
    S = (S / H) ! / H
    IK = IZ

    DO K = 1, L
      IK = IK + 1
      Z(K,J) = Z(K,J) - S * (A(IK)/H)
    END DO

  END DO

END DO

RETURN
END
