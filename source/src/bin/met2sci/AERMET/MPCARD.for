        SUBROUTINE MPCARD ( KOUNT, CARD )
C=======================================================================
C     Module MPCARD of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine processes the control cards for the
C               METPREP pathway.
C
C     Arguments:
C        CARD    Character      Runstream record
C        KOUNT   Integer        Runstream record #
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C          June 1993     added KEYID=29, instrument measurement height;
C                        restructured the routine to eliminate a computed
C                        GO TO for KEYID's 22...27
C
C          January 1997  moved the surface characteristics from ONSITE
C                        pathway to METPREP pathway
C
C          December 2008 added calls to routines to process the
C                        secondary surface characteristics
C
C          March 2012    added the THRESH_1MIN keyword
C-----------------------------------------------------------------------

C---- Local variables

      IMPLICIT NONE
      
      INTEGER    ISTAT, IFRM, JULIAN, ITYPE
      CHARACTER  CARD*(*), TNAME*96

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'WORK1.INC'

C------ IFRM   - defines the type of disk file to open.
C       WIDTH  - the computed length of a word within the card image
C       TNAME  - temporary storage location for files to be opened
C       ISTAT  - temporary location to report the status of an attempt
C                 to open a file.

C---- Initialize data

      PATH = 'METPREP'
      LOC  = 'MPCARD'
      ITYPE = 0

C.......................................................................
C     The allowable keywords for the METPREP (Stage 3) pathway are:
C
C     VALUE  KEYWRD       ACTION
C
C        4   DATA         Assign file DEV40, the merged input data
C        9   XDATES       Start and stop dates for processing
C       10   LOCATION     Location for site-dependent processing - OBSOLETE!!
C       19   FREQ_SECT    Setup surface characteristics
C       20   SITE_CHAR    Define surface characteristics
C       21   SECTOR       Define wind direction sectors
C       22   OUTPUT       Assign file DEV80 for SURFACE MET OUTPUT
C       23   MODEL        Set dispersion model
C       24   METHOD       Set processing parameters
C       26   TRACE        Debug - undocumented; not operational
C       27   PROFILE      Assign file DEV85 for PROFILE met output
C       29   NWS_HGT      Assigns the instrument height for NWS data
C       30   FREQ_SECT2   Setup for secondary surface characteristics
C       31   SITE_CHAR2   Define secondary surface characteristics
C       32   SECTOR2      Define wind direction sectors for secondary
C                          surface characteristics
C       34   AERSURF      To include a file of primary sfc characteristics
C       35   AERSURF2     To include a file of secondary sfc characteristics
C       36   THRESH_1MIN  Set minimum wind speed for 1-minute ASOS data
C                          (an optional keyword)
C
C.......................................................................
C
C     1.  Check STATUS to see if this keyword has been seen before.
C         Repeatable  keywords are: SITE_CHAR(keyid=20), SECTOR (21),
C         and METHOD (24), SITE_CHAR2(31), SECTOR2 (32)

      IF( KEYID.EQ.20 .OR. KEYID.EQ.21 .OR. KEYID.EQ.24 .OR.
     1    KEYID.EQ.31 .OR. KEYID.EQ.32 )THEN
         CONTINUE

      ELSEIF( STATUS(PATHID,KEYID).NE.0 )THEN
C------- Error, this keyword has been seen previously
         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KEYWRD(KEYID)
1000     FORMAT(' ',A12,' keyword duplicated on METPREP pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         MPSTAT = -1
         SETERR = .TRUE.
         STATUS(PATHID,KEYID) = 1
      ENDIF

C-----Decipher keyword

      IF( KEYID .EQ. 4 )THEN

C------- DATA keyword - assign input (merged) meteorology file
C             open the file and check the status

         TNAME = BLNK96
         ISTAT = 0
C ---    Set ITYPE = 2 for STATUS='OLD'
         ITYPE = 2
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV40,TNAME,ITYPE,
     &                                                          ISTAT )
         DISK40 = TNAME
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ELSE
            DISK40 = TNAME
         ENDIF


      ELSEIF( KEYID .EQ. 9 )THEN

C------- XDATES keyword - define extraction dates

         ISTAT = 0
         CALL XDTCRD ( KOUNT,CARD,JBYR1,JBGMO1,JBGDY1,
     1                 JBYR2,JBGMO2,JBGDY2,ISTAT )

         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 2 )THEN
C---------- Compute the Julian days for start and stop dates
            JBDAY1 = JULIAN(JBYR1,JBGMO1,JBGDY1)
            JBDAY2 = JULIAN(JBYR2,JBGMO2,JBGDY2)

C---------- Compute chronological day for start and stop dates
            CALL CHROND ( PATH,JBYR1,JBDAY1,JBCDY1 )
            CALL CHROND ( PATH,JBYR2,JBDAY2,JBCDY2 )
         ELSE
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


C ----LOCATION keyword in Stage 3 is now obsolete; location for
C     determining sunrise/sunset for CBL heights now based on primary 
C     surface station location in Stage 1 (ONSITE if available,
C     otherwise SURFACE data).  Location for selecting upper air 
C     sounding is based on UPPERAIR station location in Stage 1.
      ELSEIF( KEYID .EQ. 10 )THEN

C------- LOCATION keyword - define location information; METPREP uses
C                 onsite location as the reference point

         ISTAT = 0
         MPLOC = BLNK08
         MPLON = BLNK08
         MPLAT = BLNK08
         CALL LOCCRD( KOUNT,CARD,MPLOC,MPLAT,MPLON,ZONE,ISTAT )

         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ELSE
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF


      ELSEIF( KEYID .EQ. 19 )THEN
C-----   FREQ_SECT keyword - define the frequency and number of sectors
C                for the surface characteristics

         CALL SFCCRD( KOUNT,CARD,ISTAT )
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 20 )THEN
C-----   SITE_CHAR keyword - define the surface characteristics

         CALL CHRCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 21 )THEN
C-----   SECTOR keyword - define wind sectors

         CALL SECCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 22 )THEN

C------- OUTPUT keyword - assign output meteorology file;
C               file format depends on model chosen

         IFRM = 1
         TNAME = BLNK96
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV80,TNAME,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ELSE
            DISK80 = TNAME
         ENDIF


      ELSEIF( KEYID .EQ. 23 )THEN

C------- MODEL keyword - define dispersion model

         CALL MDCARD(KOUNT,CARD,ISTAT)
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 24 )THEN

C------- METHOD keyword - define processing information

         ISTAT = 0
         CALL VRCARD( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).EQ.1 )THEN
            CONTINUE
         ELSE
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF


      ELSEIF( KEYID .EQ. 25 )THEN

C------- UAWINDOW keyword - define sounding window

         ISTAT = 0
         CALL UAWNDW( KOUNT,CARD,
     &                UAWINDOWBEGIN, UAWINDOWEND, ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).EQ.1 )THEN
            CONTINUE
         ELSE
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 26 )THEN

C------- TRACE keyword - turn on complete audit trail of processing

         STATUS(PATHID,KEYID) = 2


      ELSEIF( KEYID .EQ. 27 )THEN

C------- PROFILE keyword - assign profile file;
C                file format depends on model chosen
         IFRM = 1
         TNAME = BLNK96
C ---    Set ITYPE = 5 for STATUS='REPLACE'
         ITYPE = 5
         CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV85,TNAME,ITYPE,
     &                                                          ISTAT)
         STATUS(PATHID,KEYID) = ISTAT

         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ELSE
            DISK85 = TNAME
         ENDIF


      ELSEIF( KEYID .EQ. 29 )THEN

C------- NWS_HGT keyword - define NWS instrument height

         ISTAT = 0
         CALL NWSHGT ( KOUNT,CARD,ISTAT )
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF
         IF( STATUS(PATHID,KEYID).EQ.1 )THEN
            CONTINUE
         ELSE
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 30 )THEN
C----- FREQ_SECT2 keyword - define the frequency and number of sectors
C                for the surface characteristics

         CALL SFCCRD2( KOUNT,CARD,ISTAT )
         STATUS(PATHID,KEYID) = ISTAT
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 31 )THEN
C----- SITE_CHAR2 keyword - define the surface characteristics

         CALL CHRCRD2( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF


      ELSEIF( KEYID .EQ. 32 )THEN
C----- SECTOR2 keyword - define wind sectors

         CALL SECCRD2( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 34 )THEN
C----- AERSURF keyword - include file of 'primary' site sfc char

         CALL AERSURF( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 35 )THEN
C----- AERSURF2 keyword - include file of 'secondary' site sfc char

         CALL AERSURF2( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSEIF( KEYID .EQ. 36 )THEN
C----- THRESH_1MIN keyword - 
         CALL THRESH1MIN( KOUNT,CARD,ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
C           Parameter processed without error
            STATUS(PATHID,KEYID) = ISTAT
            L_1MINASOS_THRESH = .TRUE.
         ENDIF
         IF( ISTAT .EQ. 1 )THEN
            MPSTAT = -1
            SETERR = .TRUE.
         ENDIF

      ELSE
C------- The KEYWORD is not recognized for the METPREP block

         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1011 ) KEYWRD(KEYID)
1011     FORMAT(1X,A10,' keyword invalid for METPREP pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         MPSTAT = -1
         SETERR = .TRUE.
      ENDIF

      RETURN
      END

