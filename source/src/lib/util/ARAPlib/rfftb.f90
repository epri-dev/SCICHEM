SUBROUTINE rfftb (N,R,RWRK,WSAVE)
!      SUBROUTINE RFFTB (N,R,WSAVE)
!     ******************************************************************
!
!     SUBROUTINE RFFTB(N,R,WSAVE)
!
!     ******************************************************************
!
!  changed 6/9/89 to remove actual work space from WSAVE for multiprocessing
!
!
!     SUBROUTINE RFFTB COMPUTES THE REAL PERODIC SEQUENCE FROM ITS
!     FOURIER COEFFICIENTS (FOURIER SYNTHESIS). THE TRANSFORM IS DEFINED
!     BELOW AT OUTPUT PARAMETER R.
!
!     INPUT PARAMETERS
!
!     N       THE LENGTH OF THE ARRAY R TO BE TRANSFORMED.  THE METHOD
!             IS MOST EFFICIENT WHEN N IS A PRODUCT OF SMALL PRIMES.
!             N MAY CHANGE SO LONG AS DIFFERENT WORK ARRAYS ARE PROVIDED
!
!     R       A REAL ARRAY OF LENGTH N WHICH CONTAINS THE SEQUENCE
!             TO BE TRANSFORMED
!
!     rwrk    a real array of length n used as work space
!
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST 2*N+15. - old
!     WSAVE   A WORK ARRAY WHICH MUST BE DIMENSIONED AT LEAST N+15. - new
!             IN THE PROGRAM THAT CALLS RFFTB. THE WSAVE ARRAY MUST BE
!             INITIALIZED BY CALLING SUBROUTINE RFFTI(N,WSAVE) AND A
!             DIFFERENT WSAVE ARRAY MUST BE USED FOR EACH DIFFERENT
!             VALUE OF N. THIS INITIALIZATION DOES NOT HAVE TO BE
!             REPEATED SO LONG AS N REMAINS UNCHANGED THUS SUBSEQUENT
!             TRANSFORMS CAN BE OBTAINED FASTER THAN THE FIRST.
!             THE SAME WSAVE ARRAY CAN BE USED BY RFFTF AND RFFTB.
!
!
!     OUTPUT PARAMETERS
!
!     R       FOR N EVEN AND FOR I = 1,...,N
!
!                  R(I) = R(1)+(-1)**(I-1)*R(N)
!
!                       PLUS THE SUM FROM K=2 TO K=N/2 OF
!
!                        2.*R(2*K-2)*COS((K-1)*(I-1)*2*PI/N)
!
!                       -2.*R(2*K-1)*SIN((K-1)*(I-1)*2*PI/N)
!
!             FOR N ODD AND FOR I = 1,...,N
!
!                  R(I) = R(1) PLUS THE SUM FROM K=2 TO K=(N+1)/2 OF
!
!                       2.*R(2*K-2)*COS((K-1)*(I-1)*2*PI/N)
!
!                      -2.*R(2*K-1)*SIN((K-1)*(I-1)*2*PI/N)
!
!      *****  NOTE
!                  THIS TRANSFORM IS UNNORMALIZED SINCE A CALL OF RFFTF
!                  FOLLOWED BY A CALL OF RFFTB WILL MULTIPLY THE INPUT
!                  SEQUENCE BY N.
!
!     WSAVE   CONTAINS RESULTS WHICH MUST NOT BE DESTROYED BETWEEN
!             CALLS OF RFFTB OR RFFTF.
!
!
!     ******************************************************************
IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: N
REAL, DIMENSION(N),    INTENT( INOUT ) :: R
REAL, DIMENSION(N),    INTENT( OUT   ) :: RWRK
REAL, DIMENSION(N+15), INTENT( IN    ) :: WSAVE

INTEGER, DIMENSION(15) :: IFAC

IF( N == 1 )RETURN

IFAC = TRANSFER(WSAVE(N+1:N+15),IFAC)

CALL rfftb1( N,R,RWRK,WSAVE,IFAC )

RETURN
END
