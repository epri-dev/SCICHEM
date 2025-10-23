      SUBROUTINE FLIWK1
C=======================================================================
C        FLIWK1 Module of the AERMET Meteorologoical Preprocessor
C
C     Purpose:  Initializes integer-value work arrays that are used in
C               reading and processing data.
C
C     Called by:  Routines that needs to use work arrays
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INTEGER I
      
      INCLUDE 'WORK1.INC'

      DO I = 1,AD1
         IWORK1(I) = -9999
      ENDDO

      RETURN
      END

