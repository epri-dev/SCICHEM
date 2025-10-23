SUBROUTINE cosqb( n,x,xwrk,wsave )

!     ******************************************************************
!
!     SUBROUTINE COSQB(N,X,WSAVE)
!
!     ******************************************************************
!
!  changed 6/9/89 to remove actual work space from WSAVE for multiprocessing
!
!
!     SUBROUTINE COSQB COMPUTES THE FAST FOURIER TRANSFORM OF QUARTER
!     WAVE DATA. THAT IS , COSQB COMPUTES A SEQUENCE FROM ITS
!     REPRESENTATION IN TERMS OF A COSINE SERIES WITH ODD WAVE NUMBERS.
!     THE TRANSFORM IS DEFINED BELOW AT OUTPUT PARAMETER X.
!
!     COSQB IS THE UNNORMALIZED INVERSE OF COSQF SINCE A CALL OF COSQB
!     FOLLOWED BY A CALL OF COSQF WILL MULTIPLY THE INPUT SEQUENCE X
!     BY 4*N.
!
!     THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE COSQB MUST BE
!     INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE).
!
!
!     INPUT PARAMETERS
!
!     N       THE LENGTH OF THE ARRAY X TO BE TRANSFORMED.  THE METHOD
!             IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.
!
!     X       AN ARRAY WHICH CONTAINS THE SEQUENCE TO BE TRANSFORMED
!
!     xwrk    an array of length n used as work space
!
!     WSAVE   A WORK ARRAY THAT MUST BE DIMENSIONED AT LEAST 3*N+15 - old
!     WSAVE   A WORK ARRAY THAT MUST BE DIMENSIONED AT LEAST 2*N+15 - new
!             IN THE PROGRAM THAT CALLS COSQB. THE WSAVE ARRAY MUST BE
!             INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE) AND A
!             DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT
!             VALUE OF N. THIS INITIALIZATION DOES NOT HAVE TO BE
!             REPEATED SO LONG AS N REMAINS UNCHANGED THUS SUBSEQUENT
!             TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.
!
!     OUTPUT PARAMETERS
!
!     X       FOR I=1,...,N
!
!                  X(I)= THE SUM FROM K=1 TO K=N OF
!
!
!                    4*X(K)*COS((2*K-1)*(I-1)*PI/(2*N))
!
!                  A CALL OF COSQB FOLLOWED BY A CALL OF
!                  COSQF WILL MULTIPLY THE SEQUENCE X BY 4*N.
!                  THEREFORE COSQF IS THE UNNORMALIZED INVERSE
!                  OF COSQB.
!
!     WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT
!             BE DESTROYED BETWEEN CALLS OF COSQB OR COSQF.
!
!     ******************************************************************

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: n
REAL, DIMENSION(*), INTENT( IN    ) :: wsave
REAL, DIMENSION(*), INTENT( INOUT ) :: x
REAL, DIMENSION(*), INTENT( INOUT ) :: xwrk

REAL, PARAMETER :: TSQRT2 = 2.82842712474619

REAL x1

IF( n < 2 )THEN
  x(1) = 4.*x(1)
ELSE IF( n == 2 )THEN
  x1 = 4.*(x(1)+x(2))
  x(2) = TSQRT2*(x(1)-x(2))
  x(1) = x1
ELSE
  CALL cosqb1( n,x,xwrk,wsave,wsave(n+1) )
END IF

RETURN
END

!==============================================================================

SUBROUTINE cosqb1( n,x,xwrk,w,xh )

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: n
REAL, DIMENSION(*), INTENT( INOUT ) :: x
REAL, DIMENSION(*), INTENT( INOUT ) :: xwrk
REAL, DIMENSION(*), INTENT( IN    ) :: w
REAL, DIMENSION(*), INTENT( IN    ) :: xh

INTEGER ns2, np2, modn, i, k, kc
REAL    xim1

ns2 = (n+1)/2
np2 = n+2

DO i = 3,n,2
  xim1 = x(i-1)+x(i)
  x(i) = x(i)-x(i-1)
  x(i-1) = xim1
END DO

x(1) = x(1)+x(1)
modn = MOD(n,2)
IF( modn == 0 ) x(n) = x(n)+x(n)

CALL rfftb( n,x,xwrk,xh )

DO k = 2,ns2
  kc = np2-k
  xwrk(k) = w(k-1)*x(kc)+w(kc-1)*x(k)
  xwrk(kc) = w(k-1)*x(k)-w(kc-1)*x(kc)
END DO

IF( modn == 0 )x(ns2+1) = w(ns2)*(x(ns2+1)+x(ns2+1))

DO k = 2,ns2
  kc = np2-k
  x(k) = xwrk(k)+xwrk(kc)
  x(kc) = xwrk(k)-xwrk(kc)
END DO
x(1) = x(1)+x(1)

RETURN
END

!==============================================================================

subroutine cosqf( n,x,xwrk,wsave )

!     ******************************************************************
!
!     SUBROUTINE COSQF(N,X,WSAVE)
!
!     ******************************************************************
!
!     SUBROUTINE COSQF COMPUTES THE FAST FOURIER TRANSFORM OF QUARTER
!     WAVE DATA. THAT IS , COSQF COMPUTES THE COEFFICIENTS IN A COSINE
!     SERIES REPRESENTATION WITH ONLY ODD WAVE NUMBERS. THE TRANSFORM
!     IS DEFINED BELOW AT OUTPUT PARAMETER X
!
!     COSQF IS THE UNNORMALIZED INVERSE OF COSQB SINCE A CALL OF COSQF
!     FOLLOWED BY A CALL OF COSQB WILL MULTIPLY THE INPUT SEQUENCE X
!     BY 4*N.
!
!     THE ARRAY WSAVE WHICH IS USED BY SUBROUTINE COSQF MUST BE
!     INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE).
!
!
!     INPUT PARAMETERS
!
!     N       THE LENGTH OF THE ARRAY X TO BE TRANSFORMED.  THE METHOD
!             IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.
!
!     X       AN ARRAY WHICH CONTAINS THE SEQUENCE TO BE TRANSFORMED
!
!     xwrk    an array of length n used as work space
!
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15 - old
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 2*N+15 - new
!             IN THE PROGRAM THAT CALLS COSQF. THE WSAVE ARRAY MUST BE
!             INITIALIZED BY CALLING SUBROUTINE COSQI(N,WSAVE) AND A
!             DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT
!             VALUE OF N. THIS INITIALIZATION DOES NOT HAVE TO BE
!             REPEATED SO LONG AS N REMAINS UNCHANGED THUS SUBSEQUENT
!             TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.
!
!     OUTPUT PARAMETERS
!
!     X       FOR I=1,...,N
!
!                  X(I) = X(1) PLUS THE SUM FROM K=2 TO K=N OF
!
!                     2*X(K)*COS((2*I-1)*(K-1)*PI/(2*N))
!
!                  A CALL OF COSQF FOLLOWED BY A CALL OF
!                  COSQB WILL MULTIPLY THE SEQUENCE X BY 4*N.
!                  THEREFORE COSQB IS THE UNNORMALIZED INVERSE
!                  OF COSQF.
!
!     WSAVE   CONTAINS INITIALIZATION CALCULATIONS WHICH MUST NOT
!             BE DESTROYED BETWEEN CALLS OF COSQF OR COSQB.
!
!     ******************************************************************

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: n
REAL, DIMENSION(*), INTENT( INOUT ) :: x
REAL, DIMENSION(*), INTENT( INOUT ) :: xwrk
REAL, DIMENSION(*), INTENT( IN    ) :: wsave

REAL, PARAMETER :: SQRT2 = 1.4142135623731

REAL tsqx

IF( n == 2 )THEN
  tsqx = SQRT2*x(2)
  x(2) = x(1)-tsqx
  x(1) = x(1)+tsqx
ELSE
  CALL cosqf1( n,x,xwrk,wsave,wsave(n+1) )
END IF

RETURN
END

!==============================================================================

SUBROUTINE cosqf1( n,x,xwrk,w,xh )

IMPLICIT NONE

INTEGER,            INTENT( IN    ) :: n
REAL, DIMENSION(*), INTENT( INOUT ) :: x
REAL, DIMENSION(*), INTENT( INOUT ) :: xwrk
REAL, DIMENSION(*), INTENT( IN    ) :: w
REAL, DIMENSION(*), INTENT( IN    ) :: xh

INTEGER ns2, np2, modn, i, k, kc
REAL    xim1

ns2 = (n+1)/2
np2 = n+2

DO k = 2,ns2
  kc = np2-k
  xwrk(k) = x(k)+x(kc)
  xwrk(kc) = x(k)-x(kc)
END DO

modn = MOD(N,2)
IF( modn == 0 )xwrk(ns2+1) = x(ns2+1)+x(ns2+1)

DO k = 2,ns2
  kc = np2-k
  x(k) = w(k-1)*xwrk(kc)+w(kc-1)*xwrk(k)
  x(kc) = w(k-1)*xwrk(k)-w(kc-1)*xwrk(kc)
END DO

IF( modn == 0 )x(ns2+1) = w(ns2)*xwrk(ns2+1)

CALL rfftf( n,x,xwrk,xh )

DO i = 3,n,2
  xim1 = x(i-1)-x(i)
  x(i) = x(i-1)+x(i)
  x(i-1) = xim1
END DO

RETURN
END

!==============================================================================

SUBROUTINE cosqi( n,wsave )

!     ******************************************************************
!
!     SUBROUTINE COSQI(N,WSAVE)
!
!     ******************************************************************
!
!  changed 6/9/89 to remove actual work space from WSAVE for multiprocessing
!
!
!     SUBROUTINE COSQI INITIALIZES THE ARRAY WSAVE WHICH IS USED IN
!     BOTH COSQF AND COSQB. THE PRIME FACTORIZATION OF N TOGETHER WITH
!     A TABULATION OF THE TRIGONOMETRIC FUNCTIONS ARE COMPUTED AND
!     STORED IN WSAVE.
!
!     INPUT PARAMETER
!
!     N       THE LENGTH OF THE ARRAY TO BE TRANSFORMED.  THE METHOD
!             IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.
!
!     OUTPUT PARAMETER
!
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 3*N+15. - old
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 2*N+15. - new
!             THE SAME WORK ARRAY CAN BE USED FOR BOTH COSQF AND COSQB
!             AS LONG AS N REMAINS UNCHANGED. DIFFERENT WSAVE ARRAYS
!             ARE REQUIRED FOR DIFFERENT VALUES OF N. THE CONTENTS OF
!             WSAVE MUST NOT BE CHANGED BETWEEN CALLS OF COSQF OR COSQB.
!
!     ******************************************************************

IMPLICIT NONE

INTEGER,            INTENT( IN  ) :: n
REAL, DIMENSION(*), INTENT( OUT ) :: wsave

REAL, PARAMETER :: PIH = 1.57079632679491

INTEGER k
REAL    dt

dt = PIH/FLOAT(n)

DO k = 1,n
  wsave(k) = COS(FLOAT(k)*dt)
END DO

CALL rffti( n,wsave(n+1) )

RETURN
END
