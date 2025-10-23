      SUBROUTINE OSPATH
C=====================================================================**
C          Module OSPATH of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Controls the processing calls for the ONSITE data
C               pathway.
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      
      INTEGER ISTAT

C---- 1.  Any actions to complete?
      IRD1 = 4
      IF( OSSTAT.GE.2 .AND. OSSTAT.LE.7 )THEN
         CONTINUE
      ELSE
         RETURN
      ENDIF

      IF( STATUS(1,3).GT.0 )THEN
         RETURN
      ENDIF

C---- 2.  Should the data be QA'd?  Recall that there is no data
C         extraction for on-site observations.

      ISTAT = 0
      IF( OSSTAT.EQ.2 .OR. OSSTAT.EQ.3 )THEN
         WRITE( *,100)
         CALL OSQACK( ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            OSSTAT = -1
            RUNERR = .TRUE.
         ENDIF
      ENDIF

      RETURN

 100  FORMAT( ' ' )
      END

