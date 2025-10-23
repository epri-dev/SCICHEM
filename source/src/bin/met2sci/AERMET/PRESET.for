      SUBROUTINE PRESET( ISTAGE )
C=====================================================================**
C          PRESET Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine pre-reads runstream input file ('aermet.inp')
C               to determine which stage of processing is being performed
C               based on pathways included.
C               This allows better handling of file opens to include
C               STATUS='REPLACE' where appropriate to avoid re-reading
C               headers from previously failed runs.
C
C     Called By:  AERMET
C
C     Initial Release:  
C
C     Revision History:
C
C-----------------------------------------------------------------------

C---- Data Declarations


      IMPLICIT NONE
      
      INTEGER NEWPTH, ISTAGE, I
      INTEGER IPATHS(6)
      CHARACTER (LEN=500) :: BUFFER500

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C     NEWPTH -  pathway number as returned from subr.FDPATH


C---- 1.  Initialize variables

      PATH = 'JOB       '
      LOC  = ' PRESET'
      IPATHS = 0
      NEWPTH = 0
      KOUNT  = 0
      PATHID = 1
      BUF80(1) = BLNK80
      BUFFER500 = BLN500

      WRITE( *,100 )
 100  FORMAT(/, '   Pre-processing the Setup Information')


C---- 2. Process the runstream images:
C        Note that data are read to BUF01*1 array, but are EQUIVALENCE'd 
C        to BUF80*80 in WORK1.INC.
C        Read data twice, once to BUF01*1(132) array, and once to larger
C        BUFFER500 variable, in order to check for information in the
C        runstream file that extends beyond column 132.
  10  BUF80(1) = BLNK80
      KOUNT = KOUNT + 1
      READ( DEVIN,1000,ERR=70,IOSTAT=IOFLAG,END=80 ) (BUF01(I),I=1,132),
     &                                                BUFFER500
1000  FORMAT(132A1,T1, A)

C---- If the first two columns '**', then this is a comment card; ignore it

      IF( BUF80(1)(1:2) .EQ. '**' ) GO TO 10


C---- 3. Define location of words in image                 ---- CALL DEFINE
C        (there is no error checking in subr.DEFINE)

      CALL DEFINE( 132,BUF80(1) )


C---- 4. Check NWORDS (returned through common from SUBR.DEFINE);
C        it contains the number of fields defined.  If it is zero,
C        then the image is blank.

      IF( NWORDS .EQ. 0 ) GO TO 10


C---- 5. Image is not blank; if the number of fields = 1,  ---- CALL FDPATH
C        try to match to a valid pathway ID (it could be
C        a keyword without parameters, e.g., CHK_SYNTAX)

C        IF NEWPTH = 0, then there was no match on the pathway;
C        If NEWPTH > 0 , then NEWPTH is a path ID number =
C            1 = JOB
C            2 = UPPERAIR
C            3 = SURFACE
C            4 = ONSITE
C            5 = MERGE
C            6 = METPREP

      IF( NWORDS .EQ. 1 )THEN

         CALL FDPATH( KOUNT,BUF80(1),NEWPTH )
         
         IF( NEWPTH .LT. 0 )THEN

C---------- Read not successful, go to next record
            GO TO 10

         ELSEIF( NEWPTH .GE. 1  .AND.  NEWPTH .LE. 6 )THEN
         
C---------- A valid pathway name was found on this record;
C           set flag for this pathway

            IPATHS(NEWPTH) = 1
            
C----       Go to next record
            GO TO 10

         ENDIF

      ENDIF                        ! NWORDS = 1

C---- Not a pathway record (NWORDS > 1); go to next record
      GO TO 10

C---- Read error encountered
  70  CONTINUE
  
C---- E-O-F encountered
  80  CONTINUE

C---- Rewind runstream input file for SETUP
      REWIND DEVIN

C---- Now assign processing stage (ISTAGE) based on pathways
C     included in runstream input file
      IF( IPATHS(6) .EQ. 1 )THEN
C----    METPREP pathway implies STAGE 3
         ISTAGE = 3
      ELSEIF( IPATHS(5) .EQ. 1 )THEN
C----    MERGE pathway implies STAGE 2
         ISTAGE = 2
      ELSEIF( IPATHS(2) .EQ. 1 .OR.
     &        IPATHS(3) .EQ. 1 .OR.
     &        IPATHS(4) .EQ. 1 )THEN
C----    UPPERAIR, SURFACE, or ONSITE pathways, without
C        METPREP or MERGE, implies STAGE 1
         ISTAGE = 1
      ENDIF

      RETURN
      END

