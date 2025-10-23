      SUBROUTINE FLOS(MH)
C=====================================================================**
C        FLOS Module of the AERMET Meteorologoical Preprocessor
C
C     Purpose:  Initializes buffer that are used in reading and
C               processing onsite data.
C
C     Called by:  Routines that needs to initialize an array or scalar
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INTEGER I, J, K, MH
      
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
C
C MH = Maximum number of hours to flush, beginning with 1
C
      DO K = 1,MH
         OSYR(K)  = NINT( OSQA(52,2) )
         OSMO(K)  = NINT( OSQA(53,2) )
         OSDAY(K) = NINT( OSQA(54,2) )
         OSHR(K)  = NINT( OSQA(55,2) )
         OSMN(K)  = NINT( OSQA(56,2) )
         DO J = 1,OSNL
            DO I = 15,29
               OSVOBS(K,J,I-14) = OSQA(I,2)
            ENDDO
         ENDDO
C
         DO I = 1,14
            OSSOBS(K,I) = OSQA(I,2)
         ENDDO

         DO I =30,51
            OSSOBS(K,I-15) = OSQA(I,2)
         ENDDO

C        Treat cloud cover separately because the array OSQA appears
C        to have a concatenated variable (total and opaque):
C        element 19 in OSSOBS is the cloud cover
         OSSOBS(K,19) = OSTSKY(2)

      ENDDO
C
      RETURN
      END

