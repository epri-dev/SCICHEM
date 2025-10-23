      SUBROUTINE SFCARD( KOUNT,CARD,ISTAGE,HDR )
C=====================================================================**
C          SFCARD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the runstream records for the SURFACE pathway
C
C     The allowable keywords for the SURFACE pathway are:
C
C        KEYID        Keyword       Action
C     ------------    -------       ------
C          4          DATA          Assign disk/tape for sf data
C          5          NO_MISSING    Suppress detailed messages on QA
C          7          EXTRACT       Assign disk file DEV21
C          8          QAOUT         Assign disk file DEV22
C          9          XDATES        Define extract dates
C         10          LOCATION      Define location information
C         13          RANGE         Override a default QA limit check
C         14          AUDIT         Add audit variables to this pathway
C         33          ASOS1MIN      Assign to disk file DEV23
C
C     KEYID = Position in KEYWRD array
C
C-----------------------------------------------------------------------

C---- Data Declarations


      IMPLICIT NONE
      
      INTEGER ISTAT,ISTAGE,ITYPE
      CHARACTER CARD*(*)
      CHARACTER DUMMY*96
      
      LOGICAL HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

C---- Data initializations

      PATH = 'SURFACE'
      LOC  = 'SFCARD'
      ITYPE = 0
      DUMMY = BLNK96

C---- 1.  Check status to determine if this keyword has been seen before.
C         NOTE: duplicate 'RANGE', 'AUDIT', and 'NO_MISSING' records are 
C         allowable; Also DATA, LOCATION, and ASOS1MIN for reprocessing 
C         headers.

      IF( KEYID.EQ.13 .OR. KEYID.EQ.14 .OR. KEYID.EQ.5 .OR.
     &         (HDR .AND. (KEYID.EQ. 4 .OR. KEYID.EQ.10 .OR. 
     &                                      KEYID.EQ.33)) )THEN
         CONTINUE

      ELSE
         IF( STATUS(PATHID,KEYID).NE.0 )THEN
C---------- ERROR: this keyword has been seen before
            ECODE = 'E03'
            MESS =  BLNK80
            WRITE( MESS,1000 ) KEYWRD(KEYID)
1000        FORMAT(' ',A10,' keyword duplicated on SURFACE pathway')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            STATUS(PATHID,KEYID) = 1
            SFSTAT = -1
            SETERR = .TRUE.
            RETURN
         ENDIF
      ENDIF

C---- 2.  Decipher keyword

      IF( KEYID .EQ. 4 )THEN
C------- DATA keyword - attempt to open file for SURFACE data
         IF( STATUS(PATHID,7) .NE. 0 )THEN
C----       EXTRACT keyword already found on this pathway, 
C           implying DATA as input file and EXTRACT as output file;
C           close DEV22 used for EXTRACT file and reopen with 
C           STATUS='REPLACE' (ITYPE=5)
            ISTAT = 0
            CLOSE (DEV22)
            ITYPE = 5
            CALL FLOPEN(DEV22,DUMMY,KOUNT,'NAME '//DISK22,ITYPE,1,1,1,
     &                                                            ISTAT)
            STATUS(PATHID,7) = ISTAT

            IF( ISTAT .EQ. 1 )THEN
               SFSTAT = -1
               SETERR = .TRUE.
            ENDIF
         ENDIF

C----    Now process DATA keyword
         IF( STATUS(PATHID,KEYID) .EQ. 0 )THEN
            SFFMT  = BLNK08
            SFBLKF = 0
            ISTAT  = 0
            CALL DATCRD( KOUNT,CARD,SFFMT,SFBLKF,ISTAT,HDR )
            STATUS(PATHID,KEYID) = ISTAT
            IF( ISTAT .EQ. 1 )THEN
               SFSTAT = -1
               SETERR = .TRUE.
            ENDIF
         ENDIF

      ELSEIF( KEYID .EQ. 5 )THEN
C------- NO_MISSING keyword - add NO_MISSING variables for this pathway
         CALL SFTRA( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).NE.1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 7 )THEN
C------- EXTRACT keyword - attempt to assign input disk file

         ISTAT = 0
C ---    Check to see if DATA keyword has been specified,
C        i.e., STATUS(3,4) = 2: 
         IF( STATUS(3,4) .EQ. 2 )THEN
C ---       DATA keyword is present, therefore EXTRACT file is for 
C           output, so set ITYPE = 5 for STATUS='REPLACE';
            ITYPE = 5
         ELSE
C ---       No DATA keyword, so EXTRACT file may be intended as 
C           input, so set ITYPE = 2 for STATUS='OLD';
C           If DATA keyword occurs later, EXTRACT file will be
C           reopened as STATUS='REPLACE'
            ITYPE = 2
         ENDIF

         CALL GETFIL(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV21,DISK21,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 8 )THEN
C------- QAOUT keyword - attempt to assign output disk file for QA

         ISTAT = 0
C ---    Assign file "type" (or open STATUS) based on which stage
C        of processing is being performed; for Stage 1, QAOUT file
C        is intended for output, so open with STATUS='REPLACE';
C        for Stage 2, QAOUT file is intended for input, so open
C        with STATUS='OLD'
         IF( ISTAGE.EQ.1 )THEN
C ---       Set ITYPE = 5 for STATUS='REPLACE'
            ITYPE = 5
         ELSEIF( ISTAGE.EQ.2 )THEN
C ---       Set ITYPE = 2 for STATUS='OLD'
            ITYPE = 2
         ENDIF
         CALL GETFIL(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV22,DISK22,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 9 )THEN
C------- XDATES keyword - define extract information
         ISTAT = 0
         CALL XDTCRD( KOUNT,CARD,SFYR1,SFGMO1,SFGDY1,
     &                           SFYR2,SFGMO2,SFGDY2,ISTAT )

         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 10 )THEN
C------- LOCATION keyword - define location information
         IF( STATUS(PATHID,KEYID) .EQ. 0 )THEN
            SFLOC = BLNK08
            SFLON = BLNK08
            SFLAT = BLNK08
            ISTAT = 0
            CALL LOCCRD( KOUNT,CARD,SFLOC,SFLAT,SFLON,SFLST,ISTAT )
            STATUS(PATHID,KEYID) = ISTAT
            IF( ISTAT .EQ. 1 )THEN
               SFSTAT = -1
               SETERR = .TRUE.
            ELSE
               SFLOC1 = SFLOC
            ENDIF
         ENDIF

      ELSEIF( KEYID .EQ. 13 )THEN
C------- RANGE keyword - alter default range checks for SURFACE variable
         CALL SFCHK( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).NE.1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 14 )THEN
C------- AUDIT keyword - add variables to QA audit for this pathway
         CALL SAUDIT( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).NE.1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 33 )THEN
C------- ASOS1MIN keyword
         ISTAT = 0
         CALL GETASOS(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV23,DISK23,
     &                                                 ISTAT,HDR)
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            SFSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSE
C------- The keyword is not valid for the SURFACE pathway
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEYWRD(KEYID)
 900     FORMAT(1X,A10,' keyword invalid for SURFACE pathway')
         CALL ERRHDL ( KOUNT,PATH,ECODE,LOC,MESS )
         SFSTAT = -1
         SETERR = .TRUE.
      ENDIF

      RETURN
      END

