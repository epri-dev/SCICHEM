      SUBROUTINE JBCARD( KOUNT,CARD )
C=====================================================================**
C          JBCARD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the runstream records for the JOB pathway
C
C     The allowable keywords for the JOB pathway are:
C
C     Position in
C     KEYWRD array    Keyword      Action
C     ------------    -------      ------
C          1          REPORT       Assign disk file DEV50
C          2          MESSAGES     Assign disk file DEV60
C          3          CHK_SYNTAX   Check syntax of runstream;
C                                  don't process any data
C-----------------------------------------------------------------------

C---- Data declarations


      IMPLICIT NONE
      
      INTEGER ISTAT,ITYPE
      CHARACTER CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C       ISTAT reports the status opening a file/tape.
C       ITYPE is the STATUS type for opening files

C---- Data Initialization

      PATH = 'JOB       '
      LOC  = 'JBCARD'
      ITYPE = 0

C---- 1.  Check status to see if this keyword has been seen before.

      IF( STATUS(1,KEYID).NE.0 )THEN
C        ERROR: this keyword has been seen before
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KEYWRD(KEYID)
1000     FORMAT(' ',A10,' keyword duplicated on JOB pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         JBSTAT = -1
         SETERR = .TRUE.
         RETURN
      ENDIF

C---- 2.  Decipher keyword

      IF( KEYID .EQ. 1 )THEN
C------- REPORT keyword - assign summary report file

         ISTAT = 0
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV50,DISK50,ITYPE,
     &                                                           ISTAT )
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            JBSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 2 )THEN
C------- MESSAGES keyword - assign file to which to write messages

         ISTAT = 0
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV60,DISK60,ITYPE,
     &                                                           ISTAT )
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            JBSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 3 )THEN
C------- CHK_SYNTAX record - check the runstream and stop after
C                   processing setup cards; this keyword does not
C                   have any parameters or secondary keywords

         STATUS(PATHID,KEYID) = 2

      ELSE
C------- The keyword is not valid for the JOB pathway
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEYWRD(KEYID)
 900     FORMAT(1X,A10,' keyword invalid for JOB pathway')
         CALL ERRHDL ( KOUNT,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         SETERR = .TRUE.
      ENDIF

      RETURN
      END

