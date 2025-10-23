      SUBROUTINE UAMOVE
C=====================================================================**
C          UAMOVE Module of the AERMET Meteorological Preprocessor
C
C     Purpose:   Moves the upper air data from the temporary work
C                array and variables to the permanent variables
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INTEGER LEV1, IVBL

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

      UAYR(IWORK1(20))   = UAGYR
      UAMO(IWORK1(20))   = UAGMO
      UADAY(IWORK1(20))  = UAGDY
      UAHR(IWORK1(20))   = UAGHR
      UALEV(IWORK1(20))  = IWORK1(11)

      DO LEV1 = 1,UALEV(IWORK1(20))
         DO IVBL = 1,UAMV
            UAOBS(IWORK1(20),LEV1,IVBL) = IWORK2(LEV1,IVBL)
         ENDDO
      ENDDO

      RETURN

      END

