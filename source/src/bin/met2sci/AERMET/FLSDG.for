      SUBROUTINE FLSDG(MH)
C=======================================================================
C        FLSDG Module of the AERMET Meteorologoical Preprocessor
C
C    Purpose:  Initializes buffer that are used in reading and
C              processing upper air data.
C
C    Called by:  Routines that needs to initialize an array or scalar
C
C    Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      

      INTEGER MH, I,J,K

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'

C-----------------------------------------------------------------------
C MH = Maximum number of hours to flush, beginning with 1
C
      DO K = 1,MH
         UAYR(K)  = -9
         UAMO(K)  = -9
         UADAY(K) = -9
         UAHR(K)  = -9
         DO I = 1,UAML
            DO J =1,UAMV
               UAOBS(K,I,J) = UAQA(J,2)
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END

