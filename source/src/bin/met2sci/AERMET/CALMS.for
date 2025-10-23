      SUBROUTINE CALMS(NUML3,NCALM)
C=======================================================================
C          CALMS Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  To check for zero wind speed and a non-zero direction.
C             Changes the wind direction to 0.0
C
C   Called by:  GET620
C
C   Arguments:
C      NUML3   Number of levels in the sounding
C      NCALM   Number of wind directions set to 0.0
C
C   Initial release:  December 15, 1992
C
C   Revision history:
C      11/30/94
C        -  Made into a separate subroutine from an ENTRY point.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER  LEV, NCALM
      INTEGER, intent(in) :: NUML3

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
      DATA PATH/'UPPERAIR  '/, LOC/'CALMS'/

      DO LEV = 1,NUML3
         IWORK1(500) = NINT(WORK2(LEV,5))
         IWORK1(501) = NINT(WORK2(LEV,6))
         IF(IWORK1(501) .EQ. 0) THEN
            IF(IWORK1(500) .NE. 0) THEN
               WORK2(LEV,5) = 0.0
               NCALM = NCALM + 1
               MESS =  BLNK80
               WRITE(MESS,65) UAGYR,UAGMO,UAGDY,UAGHR,LEV
               CALL ERRHDL (NCALM,PATH,'I32',LOC,MESS)
            ENDIF
         ENDIF
      ENDDO

   65 FORMAT(' ',3I2,'/',I2,'; LVL',I3,' -CALM, DIR''N SET TO 0')

      RETURN
      END

