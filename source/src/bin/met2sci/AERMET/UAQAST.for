        SUBROUTINE UAQAST( ITEST )
C=====================================================================**
C          UAQAST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Subr.TEST has determined that the 'EXTRACT' and 'QAOUT'
C               keywords are present for the UPPERAIR pathway;
C               this subroutine verifies that sufficient data are
C               present (and correct) to attempt to QA sounding data
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
C
C---- Local variable
C

      IMPLICIT NONE
      
      INTEGER ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize values

      PATH = 'JOB       '
      LOC  = 'UAQAST'
      ITEST = 0

C---- Does the QA input data file exist?

      IF( STATUS(2,7).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,1000 )
1000     FORMAT(' SUMMARY: UPPERAIR ''EXTRACT'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

C---- Does the QA output file exist?

      IF( STATUS(2,8).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,2000 )
2000     FORMAT(' SUMMARY: UPPERAIR ''QAOUT'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

C---- Is there a valid LOCATION defined?

      IF( STATUS(2,10).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E25'
         WRITE( MESS,3000 )
3000     FORMAT(' SUMMARY: UPPERAIR ''LOCATION'' MISSING/IN ERROR')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END

