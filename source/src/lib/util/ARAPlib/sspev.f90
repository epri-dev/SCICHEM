SUBROUTINE SSPEV(A,N,E,V,LDV,WORK,JOB,INFO)
!***BEGIN PROLOGUE  SSPEV
!***DATE WRITTEN   800808   (YYMMDD)
!***REVISION DATE  861211   (YYMMDD)
!***CATEGORY NO.  D4A1
!***KEYWORDS  LIBRARY=SLATEC(EISPACK),TYPE=SINGLE PRECISION(SSPEV-S),
!             EIGENVALUES,EIGENVECTORS,PACKED,SYMMETRIC
!***AUTHOR  KAHANER, K. K., (NBS)
!           MOLER, C. B., (U. OF NEW MEXICO)
!           STEWART, G. W., (U. OF MARYLAND)
!***PURPOSE  Compute the eigenvalues and, optionally, the eigen-
!            vectors of a REAL SYMMETRIC matrix stored in packed form.
!***DESCRIPTION
!
!     LICEPACK.  This version dated 08/08/80.
!     David Kahaner, Cleve Moler, Pete Stewart
!          N.B.S.       U.N.M.     N.B.S./U.MD.
!
!     Abstract
!      SSPEV computes the eigenvalues and, optionally, the eigenvectors
!      of a real symmetric matrix stored in packed form.
!
!     Call Sequence Parameters-
!       (The values of parameters marked with * (star) will be  changed
!         by SSPEV.)
!
!        A*      REAL(N*(N+1)/2)
!                real symmetric packed input matrix.  Contains upper
!                triangle and diagonal of A, by column (elements
!                11, 12, 22, 13, 23, 33, ...).
!
!        N       INTEGER
!                set by the user to
!                the order of the matrix A.
!
!        E*      REAL(N)
!                on return from SSPEV, E contains the eigenvalues of A.
!                See also INFO below.
!
!        V*      REAL(LDV,N)
!                on return from SSPEV, if the user has set JOB
!                = 0        V is not referenced.
!                = nonzero  the N eigenvectors of A are stored in the
!                first N columns of V.  See also INFO below.
!
!        LDV     INTEGER
!                set by the user to
!                the leading dimension of the array V if JOB is also
!                set nonzero.  In that case, N must be .LE. LDV.
!                If JOB is set to zero, LDV is not referenced.
!
!        WORK*   REAL(2N)
!                temporary storage vector.  Contents changed by SSPEV.
!
!        JOB     INTEGER
!                set by the user to
!                = 0        eigenvalues only to be calculated by SSPEV.
!                           Neither V nor LDV are referenced.
!                = nonzero  eigenvalues and vectors to be calculated.
!                           In this case, A & V must be distinct arrays.
!                           Also, if LDA .GT. LDV, SSPEV changes all the
!                           elements of A thru column N.  If LDA < LDV,
!                           SSPEV changes all the elements of V through
!                           column N.  If LDA=LDV, only A(I,J) and V(I,
!                           J) for I,J = 1,...,N are changed by SSPEV.
!
!       INFO*   INTEGER
!               on return from SSPEV, the value of INFO is
!               = 0 for normal return.
!               = K if the eigenvalue iteration fails to converge.
!                   Eigenvalues and vectors 1 through K-1 are correct.
!
!
!     Error Messages-
!          No. 1   recoverable  N is greater than LDV and JOB is nonzero
!          No. 2   recoverable  N is less than one
!
!     Subroutines Used
!
!      EISPACK- IMTQL2, TQLRAT, TRBAK3, TRED3
!      SLATEC- XERROR
!***REFERENCES  (NONE)
!***ROUTINES CALLED  IMTQL2,TQLRAT,TRBAK3,TRED3,XERROR
!***END PROLOGUE  SSPEV

IMPLICIT NONE

INTEGER I,INFO,J,LDV,M,N,JOB,NDIMA

REAL A(*),E(N),V(LDV,N),WORK(*)
!***FIRST EXECUTABLE STATEMENT  SSPEV
!       IF(N .GT. LDV) CALL XERROR( 'SSPEV-N .GT. LDV.',17,1,1)
IF(N > LDV) RETURN
!       IF(N .LT. 1) CALL XERROR( 'SSPEV-N .LT. 1',14,2,1)
IF(N < 1) RETURN

!       CHECK N=1 CASE

E(1) = A(1)
INFO = 0
IF(N == 1) RETURN

NDIMA = (N*(N+1))/2

IF( JOB == 0)THEN

!     EIGENVALUES ONLY

  CALL TRED3(N,NDIMA,A,E,WORK(1),WORK(N+1))
  CALL TQLRAT(N,E,WORK(N+1),INFO)

ELSE

!     EIGENVALUES AND EIGENVECTORS

  CALL TRED3(N,NDIMA,A,E,WORK(1),WORK(1))

  DO I = 1, N
    DO J = 1, N
      V(I,J) = 0.
    END DO
    V(I,I) = 1.
  END DO

  CALL IMTQL2(LDV,N,E,WORK,V,INFO)
  M = N
  IF(INFO /= 0) M = INFO - 1
  CALL TRBAK3(LDV,N,NDIMA,A,M,V)

END IF

RETURN
END
