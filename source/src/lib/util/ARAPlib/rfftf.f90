SUBROUTINE rfftf (N,R,RWRK,WSAVE)
!      SUBROUTINE RFFTF (N,R,WSAVE)
!     ******************************************************************
!
!     SUBROUTINE RFFTF(N,R,WSAVE)
!
!     ******************************************************************
!
!  changed 6/9/89 to remove actual work space from WSAVE for multiprocessing
!
!
!     SUBROUTINE RFFTF COMPUTES THE FOURIER COEFFICIENTS OF A REAL
!     PERODIC SEQUENCE (FOURIER ANALYSIS). THE TRANSFORM IS DEFINED
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
!             IN THE PROGRAM THAT CALLS RFFTF. THE WSAVE ARRAY MUST BE
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
!     R       R(1) = THE SUM FROM I=1 TO I=N OF R(I)
!
!             IF N IS EVEN SET L =N/2   , IF N IS ODD SET L = (N+1)/2
!
!               THEN FOR K = 2,...,L
!
!                  R(2*K-2) = THE SUM FROM I = 1 TO I = N OF
!
!                       R(I)*COS((K-1)*(I-1)*2*PI/N)
!
!                  R(2*K-1) = THE SUM FROM I = 1 TO I = N OF
!
!                      -R(I)*SIN((K-1)*(I-1)*2*PI/N)
!
!             IF N IS EVEN
!
!                  R(N) = THE SUM FROM I = 1 TO I = N OF
!
!                       (-1)**(I-1)*R(I)
!
!      *****  NOTE
!                  THIS TRANSFORM IS UNNORMALIZED SINCE A CALL OF RFFTF
!                  FOLLOWED BY A CALL OF RFFTB WILL MULTIPLY THE INPUT
!                  SEQUENCE BY N.
!
!     WSAVE   CONTAINS RESULTS WHICH MUST NOT BE DESTROYED BETWEEN
!             CALLS OF RFFTF OR RFFTB.
!
!
!     ******************************************************************
IMPLICIT NONE

INTEGER,               INTENT( IN    ) :: N
REAL, DIMENSION(N),    INTENT( INOUT ) :: R
REAL, DIMENSION(N),    INTENT( INOUT ) :: RWRK
REAL, DIMENSION(N+15), INTENT( IN    ) :: WSAVE

INTEGER, DIMENSION(15) :: IFAC

IF( N == 1 )RETURN

IFAC = TRANSFER(WSAVE(N+1:N+15),IFAC)

CALL rfftf1( N,R,RWRK,WSAVE,IFAC )

RETURN
END
