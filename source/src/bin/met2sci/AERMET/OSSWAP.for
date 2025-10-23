        SUBROUTINE OSSWAP( HOUR )
C=====================================================================**
C          OSSWAP Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  To transfer buffered onsite data (from call to OSFILL
C               or OSREAD) to scalar and vector master data arrays.
c               Data are swapped into first hour of these arrays.
C
C     CALLED BY:     OSFILL, OSREAD
C
C     CALLS TO:      -NONE-
C
C     Initial Release: Dec 15, 1992
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C=======================================================================

C    LOCAL VARIABLES


      IMPLICIT NONE
      
      INTEGER  I,J,HOUR

      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C      HOUR    POSITION IN THE ARRAY TO OCCUPY

C---------------------------------------------------------------------
C     SWAP BUFFER IN FOR PROCESSING

      OSDAY(HOUR) = IWORK1(1452)
      OSMO(HOUR)  = IWORK1(1453)
      OSYR(HOUR)  = IWORK1(1454)
      OSHR(HOUR)  = IWORK1(1455)
      OSMN(HOUR)  = IWORK1(1456)

      DO I=1,14
         OSSOBS(HOUR,I) = WORK1(1400+I)
      ENDDO

      DO I=30,51
         OSSOBS(HOUR,14+I-29) = WORK1(1400+I)
      ENDDO

      DO I=15,29
         DO J=1,OSNL
            OSVOBS(HOUR,J,I-14) = WORK2(100+J,I-14)
         ENDDO
      ENDDO

      RETURN
      END

