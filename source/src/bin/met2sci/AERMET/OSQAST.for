        SUBROUTINE OSQAST( ITEST )
C=====================================================================**
C          OSQAST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Subr.TEST has determined that the 'DATA' and 'QAOUT'
C               keywords are present for the ONSITE pathway;
C               this subroutine verifies that sufficient data are
C               present (and correct) to attempt to QA on-site data
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
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize variables

      PATH = 'JOB       '
      LOC  = 'OSQAST'
      ITEST = 0

C---- Is there a valid DATA record defining the input data file?

      IF( STATUS(4,4).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,1000 )
1000     FORMAT(' SUMMARY: ONSITE ''DATA'' keyword missing ',
     &           'or in error.')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

C---- Is there a QAOUT record defining the output file?

      IF( STATUS(4,8).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,2000 )
2000     FORMAT(' SUMMARY: ONSITE ''QAOUT'' keyword missing ',
     &           'or in error.')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

C---- Is there a valid LOCATION record defining the site location?

      IF( STATUS(4,10).EQ.2 )THEN
         CONTINUE

      ELSE
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,3000 )
3000     FORMAT(' SUMMARY: ONSITE ''LOCATION'' keyword missing ',
     &           'or in error.')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

C---- Are the input data formats defined correctly?

      IF( OSDCRD.EQ.0 )THEN
         MESS =  BLNK80
         WRITE( MESS,5000 )
5000     FORMAT(' SUMMARY: ONSITE ''READ/FORMAT'' keywords not ',
     &           'defined or in error.')
         ECODE = 'E27'
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END

