        SUBROUTINE AERSURF2 ( KOUNT,CARD,ISTAT )
C========================================================================
C       Module AERSURF2 of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the AERSURF2 keyword for the 'secondary' surface
C               characteristics (SURFACE station with the SUBNWS option)
C               for the METPREP pathway.
C
C               Since this option is intended to allow users to reference
C               an AERSURFACE output file directly, rather than copy
C               and paste the data into an AERMET input file, all
C               inputs in the AERSURF2 file are interpreted as being
C               for the 'secondary' surface location, so user's are
C               not required to modify the FREQ_SECT, SECTOR, and
C               SITE_CHAR keywords to include the additional '2'.
C               Note that FREQ_SECT2, SECTOR2, and SITE_CHAR2 keywords
C               are also accepted here.
C
C     Arguments:
C        KOUNT   Integer        Runstream record #
C        CARD    Character      Runstream record
C        ISTAT   Integer        Status of the processing
C
C     Initial Release: March 2009
C
C
C     Revision history:
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C-----Local variables
      INTEGER    I, ISTAT, INTKOUNT, ITYPE
      CHARACTER  CARD*(*), TNAME*96

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'


C-----Data Initializations

      PATH = 'METPREP'
      LOC  = 'AERSURF '
      ISTAT = 0
      ITYPE = 0
      INTKOUNT = 0

C---- Get the filename from the input control file and open the file of
C     surface characteristics


      TNAME  = BLNK96
      DISK42 = BLNK96
      ISTAT = 0
C --- Set ITYPE = 2 for STATUS='OLD'
      ITYPE = 2
      CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV41,TNAME,ITYPE,
     &                                                       ISTAT)
      STATUS(PATHID,KEYID) = ISTAT

      IF( ISTAT .EQ. 1 )THEN
         RETURN
      ELSE
C----    Assign filename for AERSURF file to DISK42; issue informational message
         DISK42 = TNAME
         ECODE = 'I88'
         MESS =  BLNK80
         IF( KEYID .EQ. 35 )THEN
            WRITE( MESS,101 ) DISK42(1:MIN(LEN_TRIM(DISK42),72))
101         FORMAT(' AERSURF2 file opened successfully: ',A )
         ENDIF
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      ENDIF


  10  BUF80(1) = BLN132
      KOUNT = KOUNT + 1
      INTKOUNT = INTKOUNT + 1
      READ( DEV41,1000,ERR=70,IOSTAT=IOFLAG,END=80 ) (BUF01(I),I=1,132)
1000  FORMAT(132A1)


C---- If the first two columns '**', then this is a comment card; ignore it

      IF( BUF80(1)(1:2) .EQ. '**' ) GO TO 10

C---- 3. Define location of words in image
C        (there is no error checking in subr.DEFINE)

      CALL DEFINE( 132,BUF80(1) )

C---- 4. Check NWORDS (returned through common from SUBR.DEFINE);
C        it contains the number of fields defined.  If it is zero,
C        then the image is blank.

      IF( NWORDS .EQ. 0 ) GO TO 10

C---- 5. Get the keyword
      CALL FDKEY( KOUNT,BUF80(1),KEYID )


C---- 6. Check KEYID.  It contains the KEYWRD value (array position) found.
C        If no valid match was found, KEYID = 0.

      IF( KEYID .EQ. 0 )THEN
         ECODE = 'E03'
         WRITE( MESS,3100 ) BUF80(1)(IC1(1):IC2(1))
3100     FORMAT(' Keyword unknown: ', A20 )
         CALL ERRHDL ( KOUNT,PATHWD(PATHID),ECODE,LOC,MESS )
         ISTAT = 1
         GO TO 10
      ENDIF


C---- The only keywords expected are for the surface characteristics.

      IF( KEYID .EQ. 30 .OR. KEYID .EQ. 19 )THEN
C----- FREQ_SECT2 keyword - define the frequency and number of sectors
C                for the surface characteristics

C-----   Assign KEYID = 30 since inputs from AERSURF2 file are 
C        interpreted as 'secondary' site characteristics
         KEYID = 30
         
         CALL SFCCRD2( KOUNT,BUF80(1),ISTAT )
         STATUS(PATHID,KEYID) = ISTAT

      ELSEIF( KEYID .EQ. 31 .OR. KEYID .EQ. 20 )THEN
C----- SITE_CHAR2 keyword - define the surface characteristics

C-----   Assign KEYID = 31 since inputs from AERSURF2 file are 
C        interpreted as 'secondary' site characteristics
         KEYID = 31

         CALL CHRCRD2( KOUNT,BUF80(1),ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 32 .OR. KEYID .EQ. 21 )THEN
C----- SECTOR2 keyword - define wind sectors

C-----   Assign KEYID = 32 since inputs from AERSURF2 file are 
C        interpreted as 'secondary' site characteristics
         KEYID = 32

         CALL SECCRD2( KOUNT,BUF80(1),ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSE
C----- The KEYWORD is not recognized for the METPREP block

         ECODE = 'E03'
         MESS =  BLNK80
         WRITE( MESS,1011 ) KEYWRD(KEYID)
1011     FORMAT(1X,A10,' keyword invalid for METPREP pathway')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(PATHID,KEYID) = 1
         ISTAT = 1
      ENDIF

      GO TO 10

C---- Error reading the input image
  70  CONTINUE
      ECODE = 'E01'
      WRITE( MESS,4000 ) INTKOUNT
4000  FORMAT(' Error reading AERSURF2 file at record # ',I3 )
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      SETERR = .TRUE.
      GO TO 10

C---- E-O-F encountered
  80  CONTINUE
      ECODE = 'I19'
      MESS =  BLNK80
      WRITE( MESS,5000 ) DEVIN, INTKOUNT-1
5000  FORMAT(' "END OF FILE" on unit ',I3,
     &       ' after AERSURF2 file record # ', I3)
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      CLOSE (UNIT = DEV41)


      RETURN
      END
