      SUBROUTINE FLWRK2
C=======================================================================
C        FLWRK2 Module of the AERMET Meteorologoical Preprocessor
C
C     Purpose:  Initializes real-value work arrays that are used in
C               reading and processing data.
C
C     Called by:  Routines that needs to use work arrays
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INCLUDE 'WORK1.INC'
      
      INTEGER I, J

      DO I = 1,AD2
         DO J = 1,AD3
            WORK2(I,J) = -9999.0
         ENDDO
      ENDDO

      RETURN
      END

