        SUBROUTINE AERSURF ( KOUNT,CARD,ISTAT )
C========================================================================
C       Module AERSURF of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the AERSURF keyword for the 'primary' surface
C               characteristics for the METPREP pathway.
C               Note that FREQ_SECT2, SECTOR2, and SITE_CHAR2 keywords
C               will cause fatal errors if encountered in the AERSURF
C               file.  Secondary site surface characteristics can be
C               "included" using the AERSURF2 keyword option.
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
      DISK41 = BLNK96
      ISTAT = 0
C --- Set ITYPE = 2 for STATUS='OLD'
      ITYPE = 2
      CALL GETFIL( KOUNT,CARD,PATH,KEYWRD(KEYID),DEV41,TNAME,ITYPE,
     &                                                       ISTAT)
      STATUS(PATHID,KEYID) = ISTAT

      IF( ISTAT .EQ. 1 )THEN
         RETURN
      ELSE
C----    Assign filename for AERSURF file to DISK41; issue informational message
         DISK41 = TNAME
         ECODE = 'I88'
         MESS =  BLNK80
         IF( KEYID .EQ. 34 )THEN
            WRITE( MESS,100 ) DISK41(1:MIN(LEN_TRIM(DISK41),72))
100         FORMAT(' AERSURF file opened successfully:  ',A )
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

      IF( KEYID .EQ. 19 )THEN
C----- FREQ_SECT keyword - define the frequency and number of sectors
C                for the surface characteristics

         CALL SFCCRD( KOUNT,BUF80(1),ISTAT )
         STATUS(PATHID,KEYID) = ISTAT

      ELSEIF( KEYID .EQ. 20 )THEN
C----- SITE_CHAR keyword - define the surface characteristics

         CALL CHRCRD( KOUNT,BUF80(1),ISTAT )
         IF( STATUS(PATHID,KEYID) .NE. 1 )THEN
            STATUS(PATHID,KEYID) = ISTAT
         ENDIF

      ELSEIF( KEYID .EQ. 21 )THEN
C----- SECTOR keyword - define wind sectors

         CALL SECCRD( KOUNT,BUF80(1),ISTAT )
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
4000  FORMAT(' Error reading AERSURF file at record # ',I3 )
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      SETERR = .TRUE.
      GO TO 10

C---- E-O-F encountered
  80  CONTINUE
      ECODE = 'I19'
      MESS =  BLNK80
      WRITE( MESS,5000 ) DEVIN, INTKOUNT-1
5000  FORMAT(' "END OF FILE" on unit ',I3,
     &       ' after AERSURF file record # ', I3)
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      CLOSE (UNIT = DEV41)

      RETURN
      END
