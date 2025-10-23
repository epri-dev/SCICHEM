        SUBROUTINE SFEXST( ITEST )
C=====================================================================**
C          SFEXST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Subr.TEST has determined that the 'DATA' keyword is
C               present for the SURFACE pathway; this subroutine
C               verifies that sufficient data are present (and correct)
C               to attempt to extract hourly surface obs.
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
C---- Local variable


      IMPLICIT NONE
      
      INTEGER ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize variables

      PATH = 'JOB       '
      LOC  = 'SFEXST'
      ITEST = 0


C---- Does the input archive data exist?

      IF( STATUS(3,4).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E26'
         WRITE( MESS,1000 )
1000     FORMAT(' SUMMARY: SURFACE ''DATA'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Does the output file for 'extracted' data exist?

      IF( STATUS(3,7).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E26'
         WRITE( MESS,2000 )
2000     FORMAT(' SUMMARY: SURFACE ''EXTRACT'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Are the extraction dates specified?

      IF( STATUS(3,9).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E26'
         WRITE( MESS,3000 )
3000     FORMAT(' SUMMARY: SURFACE ''XDATES'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Is the station LOCATION defined?

      IF( STATUS(3,10).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E26'
         WRITE( MESS,4000 )
4000     FORMAT(' SUMMARY: SURFACE ''LOCATION'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END

