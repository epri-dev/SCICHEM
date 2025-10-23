        SUBROUTINE OSCARD( KOUNT,CARD,ISTAGE )
C=====================================================================**
C          OSCARD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the runstream records for the ONSITE pathway.
C
C     These are the available keywords:
C
C     Position in
C     KEYWRD array   Keyword       Action
C
C         4          DATA          Assign input file (DEV32)
C         5          NO_MISSING    Suppress detailed messages on QA
C         6          FORMAT        Define data format
C         8          QAOUT         Assign output file from QA (DEV33)
C         9          XDATES        Define dates to process
C        10          LOCATION      Define site location
C        13          RANGE         Override default QA limits
C        14          AUDIT         Add variables to audit list
C        15          DELTA_TEMP    Define temperature difference heights
C        16          OSHEIGHTS     Define tower heights
C        17          THRESHOLD     Define threshold wind (calms)
C        18          READ          Define variables to be read
C        28          OBS/HOUR      Define the maximum number of obs/hour
C
C     Initial Release: December 1992
C
C     Revision History:
C        January 1997  moved the surface characteristics from ONSITE
C                      pathway to METPREP pathway
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations

      INTEGER ISTAT,ISTAGE,ITYPE
      INTEGER JULIAN
      CHARACTER CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C     WIDTH = the computed length of a field on the runstream record
C     NAME  = is the location for file/tape-names to be opened.
C     ISTAT = status of an operation:
C             1 = failed
C             2 = succeeded


C---- Data Initializations

      PATH = 'ONSITE'
      LOC  = 'OSCARD'
      ISTAT = 0
      ITYPE = 0

C---- Check the STATUS array to determine if this keyword has been used
C     in this run

C     NOTE: On this pathway, the following keywords can be repeated:
C           'RANGE', 'READ', 'FORMAT', 'DELTA_TEMP',
C           'NO_MISSING', 'AUDIT', 'OSHEIGHTS'

      IF( KEYID.EQ.13 .OR. KEYID.EQ.18 .OR. KEYID.EQ.6  .OR.
     &    KEYID.EQ.14 .OR. KEYID.EQ.15 .OR. KEYID.EQ.5  .OR.
     &    KEYID.EQ.16 )THEN
         CONTINUE

      ELSEIF( STATUS(PATHID,KEYID).NE.0 )THEN
C------- ERROR, this keyword has been used before in this run
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KEYWRD(KEYID)
 1000    FORMAT(' KEYWORD ',A10,' DUPLICATED ON ONSITE PATH')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         RETURN
      ENDIF

C---- Decipher keyword

      IF( KEYID .EQ. 4 )THEN
C------- DATA keyword - assign input disk file for QA

C ---    Set ITYPE = 2 for STATUS='OLD'
         ITYPE = 2
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV31,DISK31,ITYPE,
     &                                                           ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 5 )THEN
C------- NO_MISSING keyword - variables that are tracked but without
C                   detailed info in message file

         CALL OSTRA( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 6 )THEN
C------- FORMAT keyword - define data format

         CALL FMTCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 8 )THEN
C------- QAOUT keyword - assign output disk file for QA

         IF( ISTAGE.EQ.1 )THEN
C ---       Set ITYPE = 5 for STATUS='REPLACE'
            ITYPE = 5
         ELSEIF( ISTAGE.EQ.2 )THEN
C ---       Set ITYPE = 2 for STATUS='OLD'
            ITYPE = 2
         ENDIF
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV32,DISK32,ITYPE,
     &                                                           ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 9 )THEN
C------- XDATES keyword - define period to QA

         CALL XDTCRD( KOUNT,CARD,OSYR1,OSGMO1,OSGDY1,
     &                           OSYR2,OSGMO2,OSGDY2,ISTAT )

C        NOTE: We depart from the naming convention started in processing
C              surface and upperair data; for those two data types,
C              xxDAY1 ansd xxDAY2 are chronological days (sine 1/1/1900)
C              but for on-site data, OSDAY1 and OSDAY2 are Julian days

         OSDAY1 = JULIAN( OSYR1, OSGMO1, OSGDY1 )
         OSDAY2 = JULIAN( OSYR2, OSGMO2, OSGDY2 )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 10 )THEN
C------- LOCATION keyword - define station location information

         IF( STATUS(PATHID,KEYID) .EQ. 0 )THEN
            OSLOC = BLNK08
            OSLON = BLNK08
            OSLAT = BLNK08
            CALL LOCCRD( KOUNT,CARD,OSLOC,OSLAT,OSLON,OSLST,ISTAT )

            IF( ISTAT.EQ.1 )THEN
               OSSTAT = -1
               SETERR = .TRUE.
               STATUS(PATHID,KEYID) = 1
            ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
               STATUS(PATHID,KEYID) = ISTAT
            ENDIF
         ENDIF

      ELSEIF( KEYID .EQ. 13 )THEN
C------- RANGE keyword - modify the default QA values or missing data
C              indicator for an on-site variable

         CALL OSCHK( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 14 )THEN
C------- AUDIT keyword - add variables to track in QA for this pathway

         CALL OAUDIT( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 15 )THEN
C------- DELTA_TEMP keyword - define the heights for temperature difference

         CALL DTCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 16 )THEN
C------- OSHEIGHTS keyword - define tower heights

         CALL HGTCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 17 )THEN
C------- THRESHOLD keyword - define the threshold winds to define calms

         CALL CLMCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 18 )THEN
C------- READ keyword - define the variable names to read

         CALL VARCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 28 )THEN
C------- OBS/HOUR keyword - define the number of observation periods per hour

         CALL AVGCRD( KOUNT,CARD,ISTAT )

         IF( ISTAT.EQ.1 )THEN
            OSSTAT = -1
            SETERR = .TRUE.
            STATUS(PATHID,KEYID) = 1
         ELSEIF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSE
C------- Invalid keyword for ONSITE pathway
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,900 ) KEYWRD(KEYID)
 900     FORMAT(1X,A10,' keyword invalid for ONSITE pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         OSSTAT = -1
         SETERR = .TRUE.

      ENDIF

      RETURN
      END

