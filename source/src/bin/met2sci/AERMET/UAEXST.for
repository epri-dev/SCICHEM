        SUBROUTINE UAEXST( ITEST )
C=====================================================================**
C          UAEXST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Subr.TEST has determined that the 'DATA' keyword is
C               present for the UPPERAIR pathway; this subroutine
C               verifies that sufficient data are present (and correct)
C               to attempt to extract sounding data
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

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization

      PATH = 'JOB       '
      LOC  = 'UAEXST'
      ITEST = 0


C---- Is the input archive data file defined?

      IF( STATUS(2,4).EQ.2 )THEN
         CONTINUE
      ELSE

         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,1000 )
1000     FORMAT(' SUMMARY: UPPERAIR ''DATA'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Is the output file defined?

      IF( STATUS(2,7).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,2000 )
2000     FORMAT(' SUMMARY: UPPERAIR ''EXTRACT'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Have the extraction dates been defined?

      IF( STATUS(2,9).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,3000 )
3000     FORMAT(' SUMMARY: UPPERAIR ''XDATES'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF


C---- Is the station location defined?

      IF( STATUS(2,10).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,4000 )
4000     FORMAT(' SUMMARY: UPPERAIR ''LOCATION'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END

