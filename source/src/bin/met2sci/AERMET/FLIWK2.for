      SUBROUTINE FLIWK2
C=======================================================================
C        FLIWK2 Module of the AERMET Meteorologoical Preprocessor
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
      
      INTEGER I, J
      
      INCLUDE 'WORK1.INC'

      DO I = 1,AD2
         DO J = 1,AD3
            IWORK2(I,J) = -9999
         ENDDO
      ENDDO

      RETURN
      END

