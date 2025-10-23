      SUBROUTINE SETUP( ISTAGE )
C=====================================================================**
C          SETUP Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine controls the processing of the user
C               supplied runstream images.
C
C     Called By:  AERMET
C
C     Initial Release:  December 1992
C
C     Revision History:
C        05/29/08: MACTEC Engineering & Consulting
C                - added data initializations
C
C-----------------------------------------------------------------------

C---- Data Declarations


      IMPLICIT NONE
      
      INTEGER NEWPTH, ISTAGE, I, ISTAT
      LOGICAL HDR
      CHARACTER (LEN=500) :: BUFFER500

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C     NEWPTH -  pathway number as returned from subr.FDPATH


C---- 1.  Initialize variables

      PATH = 'JOB       '
      LOC  = ' SETUP'
      JBSTAT = 0
      UASTAT = 0
      SFSTAT = 0
      OSSTAT = 0
      MRSTAT = 0
      MPSTAT = 0
      KOUNT  = 0
      NEWPTH = 0
      PATHID = 1
      BUF80(1) = BLNK80
      BUFFER500 = BLN500
C---- Initialize OSFRMT array to blank
      OSFRMT = BLNK80

      WRITE( *,100 )
 100  FORMAT(/, '   Processing the Setup Information')


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

C---- Check for runstream image > 132 characters based on length of BUFFER500
      IF( LEN_TRIM(BUFFER500) .GT. 132 .AND. 
     &             BUFFER500(1:2) .NE. '**' )THEN
C        Issue warning message about potentially truncated runstream image
         MESS  = BLNK80
         ECODE = 'W02'
         WRITE( MESS,1001 ) KOUNT, LEN_TRIM(BUFFER500)
1001     FORMAT(' Runstream image #',I5,' exceeds 132 ',
     &          'characters! Errors may occur or information ',
     &          'lost. RECL=',I4 )
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         WRITE( MESS,1002 ) BUFFER500(133:MIN(202,LEN_TRIM(BUFFER500)))
1002     FORMAT('  Information beyond col 132: ',A )
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ENDIF


C---- If the first two columns = '**', then this is a comment card; ignore it

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

C---------- There was an error processing this record; write message
            MESS  = BLNK80
            ECODE = 'E02'
            WRITE( MESS,1100 ) KOUNT
1100        FORMAT(' ERROR DEFINING PATHWAY ON RECORD # ', I3)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            SETERR = .TRUE.
            GO TO 10


C--------6. This is a valid pathway ID:
C           Is it a valid path for this stage?
C           Is it a duplicate pathway ID?
C           If so, write error message

         ELSEIF( NEWPTH .GE. 1  .AND.  NEWPTH .LE. 6 )THEN              ! rwb #502  06299
C---------- This record defines a valid pathway for all stages

               IF( STABLK(NEWPTH) .EQ. 0 )THEN
C---------------- This pathway has not been seen before in this run;
                  IRD1   = NEWPTH
                  PATHID = NEWPTH
                  STABLK(PATHID) = 2

C---------------- Write the record to the temporary file   ---- CALL WRTCRD
                  BUF03 = '   '
                  CALL WRTCRD( KOUNT,BUF03,BUF80(1),DEV75 )

               ELSE
C---------------- This pathway has been seen before in this run;
C                 this is an error condition
                  STABLK(NEWPTH) = 1
                  ECODE = 'E02'
                  WRITE( MESS,2001 ) PATHWD(NEWPTH)
2001              FORMAT(' DUPLICATE ',A10,' PATHWAY RECORD')
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  SETERR = .TRUE.
               ENDIF
               GO TO 10

         ENDIF

      ENDIF                        ! NWORDS = 1


C---- 7. Probably not a PATHWAY (unless pathway was misspelled),
C        so check for valid keyword                        ---- CALL FDKEY

      CALL FDKEY( KOUNT,BUF80(1),KEYID )

C---- 8. Check KEYID.  It contains the KEYWRD value (array position) found.
C        If no valid match was found, KEYID = 0.
C        If the number of fields = 1, the problem _could_ be a pathway

      IF( KEYID .EQ. 0 )THEN
         IF( NWORDS .EQ. 1 )THEN
            ECODE = 'E03'
            WRITE( MESS,3000 ) BUF80(1)(IC1(1):IC2(1))
3000        FORMAT(' PATHWAY/KEYWORD UNKNOWN: ', A20 )
            CALL ERRHDL ( KOUNT,PATHWD(PATHID),ECODE,LOC,MESS )
            SETERR = .TRUE.
            GO TO 10
         ELSE
            ECODE = 'E03'
            WRITE( MESS,3100 ) BUF80(1)(IC1(1):IC2(1))
3100        FORMAT(' KEYWORD UNKNOWN: ', A20 )
            CALL ERRHDL ( KOUNT,PATHWD(PATHID),ECODE,LOC,MESS )
            SETERR = .TRUE.
            GO TO 10
         ENDIF
      ENDIF


C---- 9. Keyword recognized; write record to the temporary ---- CALL WRTCRD
C        file (attached to DEV75) for possible later use;
C        reset the pathway index

      BUF03 = '   '
      CALL WRTCRD( KOUNT,BUF03,BUF80(1),DEV75 )


C---- 10. Call appropriate routine to decipher this image for the pathway.

      IF( PATHID .EQ. 1)THEN
C------- JOB pathway                                       ---- CALL JBCARD
         CALL JBCARD( KOUNT,BUF80(1) )

      ELSEIF( PATHID .EQ. 2 )THEN
C------- UPPERAIR pathway                                  ---- CALL UACARD
         HDR = .FALSE.
         CALL UACARD( KOUNT,BUF80(1),ISTAGE,HDR )

      ELSEIF( PATHID .EQ. 3 )THEN
C------- SURFACE pathway                                   ---- CALL SFCARD
         HDR = .FALSE.
         CALL SFCARD( KOUNT,BUF80(1),ISTAGE,HDR )

      ELSEIF( PATHID .EQ. 4 )THEN
C------- ONSITE pathway                                    ---- CALL OSCARD
         CALL OSCARD( KOUNT,BUF80(1),ISTAGE )

      ELSEIF( PATHID .EQ. 5 )THEN
C------- MERGE pathway                                     ---- CALL MRCARD
         CALL MRCARD( KOUNT,BUF80(1) )

      ELSEIF( PATHID .EQ. 6 )THEN
C------- METPROC pathway                                   ---- CALL MPCARD
         CALL MPCARD( KOUNT,BUF80(1) )
      ENDIF

C---- Read next record      
      GO TO 10

C---- Error reading the input image
  70  CONTINUE
      ECODE = 'E01'
      MESS =  BLNK80
      WRITE( MESS,4000 ) KOUNT
4000  FORMAT(' ERROR READING INPUT RUNSTREAM RECORD # ',I3 )
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      SETERR = .TRUE.
      GO TO 10

C---- E-O-F encountered
  80  CONTINUE
      ECODE = 'I19'
      MESS =  BLNK80
      WRITE( MESS,5000 ) DEVIN, KOUNT-1
5000  FORMAT(' "END OF FILE" ON UNIT ',I2, ' AFTER RECORD # ', I3)
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

C---- Modify to allow for STAGE3 processing with single AERMET executable
C----
      IF( PATHID .LE. 5 )THEN

C----    11.  Read and process the header records of input    ---- CALL HDPROC
C             files that were generated previously by AERMET

         CALL HDPROC( ISTAGE )

C----    12.  Verify that there is sufficient input to        ---- CALL TEST
C             process the input data

         CALL TEST( ISTAGE )

C----    13. Construct and write header records to appropriate
C            output files.  Selected header records in the input
C            files will be reprocessed.                       ---- CALL HEADER
C            (Note: only if SUBR.TEST shows no errors detected
C             on all (repeat: all) pathways, will header(s) be
C             constructed and written)

         IF( .NOT. SETERR )THEN
            CALL HEADER( ISTAGE )
         ENDIF

      ENDIF


      IF( PATHID .EQ. 6 )THEN

C------  Now process previously written header records in the
C        merged data file

         CALL MPPROC( ISTAT,ISTAGE )
         IF( ISTAT .NE. 2 )THEN
C---------- Returned with an error - no need to proceed
            MPSTAT = -1
            SETERR = .TRUE.
         ELSEIF( .NOT.UADATA .AND. .NOT.OSMIX )THEN
C-------    No upper air data or ONSITE mixing heights 
C           included in the merged file: set MPSTAT = -1,
C           but leave SETERR as is to allow call to 
C           MPTEST below.
            MPSTAT = -1
         ENDIF


C------- Test the status array for completeness
         IF( .NOT. SETERR )THEN
            CALL MPTEST
         ENDIF

C------- Now check for adequate set of met data;
C        accounting for presence of ONSITE mixing
C        heights.
         IF( (.NOT.UADATA .AND. .NOT.OSMIX) .OR. 
     &       (.NOT.SFDATA .AND. .NOT.OSDATA) )THEN
            ECODE = 'E80'
            MESS =  BLNK80
            WRITE( MESS,8000 ) UADATA, SFDATA, OSDATA, OSMIX
8000        FORMAT(' Input data are incomplete: UADATA, SFDATA,'
     &             ' OSDATA, OSMIX:  ',4(L1,2X))
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            SETERR = .TRUE.
         ENDIF

C------- Now construct and write header records to appropriate
C        output files.  (NOTE: only if call to 'test' shows no
C        errors detected on all (repeat: all) pathways, will
C        header(s) be constructed and written)

         IF( .NOT. SETERR )THEN
            CALL MPHEAD
         ENDIF

           
      ENDIF

      RETURN
      END

