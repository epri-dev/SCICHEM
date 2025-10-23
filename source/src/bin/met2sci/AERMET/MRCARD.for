      SUBROUTINE MRCARD( KOUNT,CARD )
C=====================================================================**
C          MRCARD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Process MERGE pathway keywords
C
C     The allowable keywords for the MERGE pathway are:
C
C     Position in
C     KEYWRD array    Keyword    Action
C     ------------    -------    ------
C          9          XDATES     Define dates for merged data set
C         22          OUTPUT     Assign disk file name for merged data
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER   ISTAT, ITYPE
      CHARACTER CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT is the status of an operation
C            1 = failed
C            2 = succeeded
C      CARD  is the record from the runstream file

C---- Data Initializations

      PATH = 'MERGE     '
      LOC  = 'MRCARD'
      ITYPE = 0

C---- Check STATUS array to determine of this keyword has been
C     used before before in this run

      IF( STATUS(PATHID,KEYID).NE.0 )THEN
C------- ERROR: this keyword has been seen before
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KEYWRD(KEYID)
1000     FORMAT(' ',A10,' keyword duplicated on MERGE pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         MRSTAT = -1
         SETERR = .TRUE.
      ENDIF

C---- Decipher keyword

      IF( KEYID.EQ.9 )THEN
C------- XDATES record - define date information
         ISTAT = 0
         CALL XDTCRD( KOUNT,CARD,MRYR1,MRGMO1,MRGDY1,
     &                           MRYR2,MRGMO2,MRGDY2,ISTAT )

         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
           MRSTAT = -1
           SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID.EQ.22 )THEN
C------- OUTPUT record - assign disk file for merge (new)
         ISTAT = 0
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV40,DISK40,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            MRSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSE
C------- The keyword is not valid for the MERGE pathway
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEYWRD(KEYID)
 900     FORMAT(1X,A10,' keyword invalid for MERGE pathway')
         CALL ERRHDL ( KOUNT,PATH,ECODE,LOC,MESS )
         MRSTAT = -1
         SETERR = .TRUE.
      ENDIF

      RETURN
      END

