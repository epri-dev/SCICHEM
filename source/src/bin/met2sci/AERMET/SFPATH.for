      SUBROUTINE SFPATH
C=====================================================================**
C          Module SFPATH of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Controls the processing calls for the SURFACE data
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
      IRD1 = 3
      IF( SFSTAT.GE.1 .AND. SFSTAT.LE.6 )THEN
         CONTINUE
      ELSE
         RETURN
      ENDIF

      IF( STATUS(1,3).GT.0 )THEN
         RETURN
      ENDIF

C---- 2.  Should data be extracted from the archive file?

      ISTAT = 0
      IF( SFSTAT.EQ.1 .OR. SFSTAT.EQ.3 )THEN
         WRITE( *,100)
         CALL SFEXT (ISTAT)
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            RUNERR = .TRUE.
         ENDIF
      ENDIF

C---- 3.  Should the data be QA'd?

      IF( SFSTAT.EQ.2 .OR. SFSTAT.EQ.3 )THEN
         WRITE( *,100)
         CALL SFQASM (ISTAT)
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            RUNERR = .TRUE.
         ENDIF
      ENDIF

      RETURN

 100  FORMAT( ' ' )
      END

