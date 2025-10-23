      SUBROUTINE TDPEST(NUML4,NTMP,NDEW)
C=======================================================================
C          TDPEST Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  If temperature or dew-point is missing at a level above
C             the surface, an estimate of the value is determined by
C             linearly interpolating between the levels above and below,
C             assuming no missing data
C
C   Called by: GET620
C
C   Arguments:
C      NUML4      Number of levels in the sounding
C      NDEW       Number of levels of missing dewpt values
C      NTMP       Number of levels of missing temp values
C
C   Initial release:  December 15, 1992
C
C   Revision History:
C      11/30/94
C        - Made into a separate subroutine from an ENTRY point
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INTEGER  NTMP, NDEW, LEV, LEV1, NUML4
      REAL     XLOG
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
      DATA PATH/'UPPERAIR  '/, LOC/'TDPEST'/
C
      NTMP = 0
      NDEW = 0

C---- Temperature

      DO LEV = 2,NUML4-1

         IF(WORK2(LEV,3) .LT. -9000.0) THEN
            IF( (WORK2(LEV-1,1) .GT. -9000.0) .AND.
     &          (WORK2(LEV,1)   .GT. -9000.0) .AND.
     &          (WORK2(LEV-1,3) .GT. -9000.0) )THEN
               DO LEV1 = LEV+1,NUML4
                  IF( (WORK2(LEV1,1) .GT. -9000.0) .AND.
     &                (WORK2(LEV1,3) .GT. -9000.0) )THEN
                     XLOG = ALOG(WORK2(LEV,1)/WORK2(LEV-1,1))/
     &                      ALOG(WORK2(LEV1,1)/WORK2(LEV-1,1))
                     WORK2(LEV,3) = WORK2(LEV-1,3) +
     &                          (WORK2(LEV1,3) - WORK2(LEV-1,3))*XLOG
                     MESS =  BLNK80
                     WRITE(MESS,200) UAGYR,UAGMO,UAGDY,UAGHR,LEV
                     CALL ERRHDL(LEV,PATH,'I32',LOC,MESS)
                     NTMP = NTMP + 1
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO

C---- Dew point

      DO LEV = 2,NUML4-1

         IF(WORK2(LEV,4) .LT. -9000.0) THEN
            IF( (WORK2(LEV-1,1) .GT. -9000.0) .AND.
     &          (WORK2(LEV,1)   .GT. -9000.0) .AND.
     &          (WORK2(LEV-1,4) .GT. -9000.0) )THEN
               DO LEV1 = LEV+1,NUML4
                  IF( (WORK2(LEV1,1) .GT. -9000.0) .AND.
     &               (WORK2(LEV1,4)  .GT. -9000.0) )THEN
                     XLOG = ALOG(WORK2(LEV,1)/WORK2(LEV-1,1))/
     &                      ALOG(WORK2(LEV1,1)/WORK2(LEV-1,1))
                     WORK2(LEV,4) = WORK2(LEV-1,4) +
     &                           (WORK2(LEV1,4) - WORK2(LEV-1,4))*XLOG
                     MESS =  BLNK80
                     WRITE(MESS,400) UAGYR,UAGMO,UAGDY,UAGHR,LEV
                     CALL ERRHDL(LEV,PATH,'I32',LOC,MESS)
                     NDEW = NDEW + 1
                     EXIT
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
C

  200 FORMAT(' ',3I2,'/',I2,'; LVL',I3,' - TEMPERATURE ESTIMATED')
  400 FORMAT(' ',3I2,'/',I2,'; LVL',I3,' - DEW POINT ESTIMATED')

C---- END OF SUBROUTINE
      RETURN
      END

