      SUBROUTINE FDPATH( KOUNT,CARD,IDN )
C=====================================================================**
C          FDPATH Module of the AERMET Meteorological Preprocessor
C
C     Purpose: Identifies the pathway through the varaible IDN
C              IDN = -1    error trying to determine the pathway;
C                          returned from S.GETWRD
C                  =  0    no match on pathway; could be an invalid
C                          pathway or a zero-parameter keyword
C                  =  1..6 valid pathway (JOB, SURFACE, UPPERAIR, ONSITE,
C                          MERGE, METPREP)
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER IDN,ISTAT,I
      CHARACTER CARD*(*)
      CHARACTER*12 BLK

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization

      PATH = 'JOB       '
      LOC  = 'FDPATH'
      MESS =  BLNK80
      BLK  = '            '

C---- Read the contents of the field; save in 'BLK'        ---- CALL GETWRD

      CALL GETWRD(1,KOUNT,CARD,1,12,2,'PATHWAY ',BLK,ISTAT)
      IF( ISTAT.EQ.1 )THEN
C------- Error reading the field
         IDN = -1
         RETURN
      ENDIF

C---- Determine the path
      DO I=1,6
         IF( BLK .EQ. PATHWD(I) )THEN
            IDN = I
            RETURN
         ENDIF
      ENDDO

C---- No match found: either BLK is a keyword or a nonexistent path
      IDN = 0

      RETURN
      END

