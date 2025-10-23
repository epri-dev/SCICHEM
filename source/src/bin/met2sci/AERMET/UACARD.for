      SUBROUTINE UACARD( KOUNT,CARD,ISTAGE,HDR )
C=====================================================================**
C          UACARD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the runstream records for the UPPERAIR pathway
C
C     The allowable keywords for the UPPERAIR pathway are:
C
C     Position in
C     KEYWRD array    Keyword       Action
C     ------------    -------       ------
C          4          DATA          Assign file with sounding data
C          5          NO_MISSING    Suppress detailed messages on QA
C          7          EXTRACT       Assign disk file DEV12
C          8          QAOUT         Assign disk file DEV13
C          9          XDATES        Define extract dates
C         10          LOCATION      Define location information
C         11          CEILING       <-- removed 10/31/96 -->
C         12          MODIFY        Turn on adjustments to soundings
C         13          RANGE         Override a default QA limit
C         14          AUDIT         Add variables to audit list
C
C
C     Initial Release:  December 15, 1992
C
C     Revision history:
C        04/21/95
C          - Changed comments for NOMODIF to reflect the change
C            in processing - sounding modifications are no longer
C            automatic; keyword is now MODIFY (in MASTER.INC)
C        10/31/96
C          - Removed the CEILING keyword; soundings are hardwired to
C            5000 m through UATOP in MASTER.INC
C
C-----------------------------------------------------------------------

C---- Variable declarations

      IMPLICIT NONE
      
      INTEGER ISTAT,ISTAGE,ITYPE
      CHARACTER CARD*(*)

      LOGICAL HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C         WIDTH is the computed length of a field on a record
C         ISTAT reports the status of an operation:
C               1 = failed
C               2 = succeeded

C---- Data Initializations

      PATH = 'UPPERAIR'
      LOC  = 'UACARD'
      ITYPE = 0

C---- 1.  Check status to see if this keyword has been seen before.
C         NOTE: duplicate RANGE (13), AUDIT (14), and NO_MISSING (5)
C         keywords are allowed; LOCATION (10) for reprocessing headers.

      IF( KEYID.EQ.13 .OR. KEYID.EQ.14 .OR. KEYID.EQ.5 .OR.
     &                           (HDR .AND. KEYID.EQ.10) )THEN
         CONTINUE

      ELSEIF( STATUS(PATHID,KEYID).NE.0 )THEN
C        ERROR - this keyword has been seen before
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KEYWRD(KEYID)
1000     FORMAT(' ',A10,' keyword duplicated on UPPERAIR pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         UASTAT = -1
         SETERR = .TRUE.
         RETURN
      ENDIF

C---- 2.  Decipher keyword with call to appropriate subroutine

      IF( KEYID .EQ. 4 )THEN
C------- DATA keyword - assign archive file/tape with upper air
C             sounding data; if there are no errors on this
C             record, then ISTAT will either be 2 (for ASCII data)
C             or 3 (for EBCDIC data)

         CALL DATCRD ( KOUNT,CARD,UAFMT,UABLKF,ISTAT,HDR )
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 5 )THEN
C------- NO_MISSING keyword - variables that are tracked but without
C                   detailed information (date, etc.) in QA; repeatable

         CALL UATRA( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            SETERR = .TRUE.
            UASTAT = -1
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID).NE.1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID. EQ. 7 )THEN
C------- EXTRACT keyword - assign input disk file for QA

         ISTAT = 0
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV12,DISK12,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 8 )THEN
C------- QAOUT keyword - assign output disk file for QA

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
         CALL GETFIL(KOUNT,CARD,PATH,KEYWRD(KEYID),DEV13,DISK13,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 9 )THEN
C------- XDATES keyword - define extract dates

         ISTAT = 0
         CALL XDTCRD( KOUNT,CARD,UAYR1,UAGMO1,UAGDY1,
     &                           UAYR2,UAGMO2,UAGDY2,ISTAT )

         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID. EQ. 10 )THEN
C------- LOCATION keyword - define station location information
C                 This keyword is processed only once, whether it
C                 is in a runstream or in a header record.

         IF( STATUS(PATHID,KEYID) .EQ. 0 )THEN
            UALOC = BLNK08
            UALON = BLNK08
            UALAT = BLNK08
            ISTAT = 0
            CALL LOCCRD( KOUNT,CARD,UALOC,UALAT,UALON,UALST,ISTAT )
            STATUS(PATHID,KEYID) = ISTAT
            IF( ISTAT .EQ. 1 )THEN
               UASTAT = -1
               SETERR = .TRUE.
            ELSE
               UALOC1 = UALOC
            ENDIF
         ENDIF

      ELSEIF( KEYID .EQ. 12 )THEN
C------- MODIFY keyword - turn on sounding adjustments option --
C               there are no additional parameters or secondary
C               keywords.
         STATUS(PATHID,KEYID) = 2

      ELSEIF( KEYID .EQ. 13 )THEN
C------- RANGE keyword - modifies the default values (missing
C              indicator, upper and lower bounds) for the QA
C              ** A repeatable and reprocessed keyword **

         CALL UACHK( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).NE.1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 14 )THEN
C------- AUDIT keyword - add variables to track in QA for UPPERAIR

         CALL UAUDIT( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            UASTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).NE.1 )THEN
             STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSE
C------- The keyword is not valid for the UPPERAIR pathway
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEYWRD(KEYID)
 900     FORMAT(1X,A10,' keyword invalid for UPPERAIR pathway')
         CALL ERRHDL ( KOUNT,PATH,ECODE,LOC,MESS )
         UASTAT = -1
         SETERR = .TRUE.
      ENDIF

      RETURN
      END

