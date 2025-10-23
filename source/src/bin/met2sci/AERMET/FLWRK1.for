      SUBROUTINE FLWRK1
C=======================================================================
C        FLWRK1 Module of the AERMET Meteorologoical Preprocessor
C
C     Purpose:  Initializes real-valued work arrays that are used in
C               reading and processing data.
C
C     Called by:  Routines that needs to use work arrays
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INCLUDE 'WORK1.INC'
      
      INTEGER I

      DO I = 1,AD1
         WORK1(I) = -9999.0
      ENDDO

      RETURN
      END

