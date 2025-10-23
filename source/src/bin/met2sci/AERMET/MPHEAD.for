        SUBROUTINE MPHEAD
C=====================================================================**
C     Module MPHEAD of the AERMET Meteorological Preprocessor
C
C     Purpose:  To write, if appropriate, header(s) to output files.
C
C     Arguments:  <none>
C
C     Revision history:
C       <none>
C-----------------------------------------------------------------------

C---- Local variables

      IMPLICIT NONE
      
      INTEGER   NUMBER,NEWPTH
      CHARACTER CARD*132,OPATH*2

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C       NUMBER  Keeps track of the number of header records,
C                allowing AERMET to create special headers when it
C                first starts to write a header.
C       ITEST   Temporary storage of pathway status.
C
C.......................................................................
C
C---- Initialize values
C
      PATH  = PATHWD(PATHID)
      LOC   = 'MPHEAD'
      OPATH = '  '
C
C---- Check pathways's status words
C     If any are LE. 0, then there are no header records to process
C
      IF( ( JBSTAT .LT. 0 ) .OR.
     1    ( UASTAT .LT. 0 ) .OR.
     1    ( SFSTAT .LT. 0 ) .OR.
     1    ( OSSTAT .LT. 0 ) )THEN
         RETURN
      ENDIF
C
C---- Initialize header counter
C
      NUMBER = 0
C
C---- Make sure DEV80 and DEV85 are available (assumes output is
C     for AERMOD)
C
      IF( ( STATUS(6,22) .LT. 2 ) .OR.
     1    ( STATUS(6,27) .LT. 2 ) ) RETURN

C     None of the given models can accept header records
C     so for now we skip this logic.  Note, it has been tested
C     and appears to work just fine.  We use the test on
C     STATUS(6,22) to fool the FORTRAN compiler into thinking
C     that statements below the 'GO TO 20' are reachable.

      IF( STATUS(6,22) .EQ. 2 ) GO TO 20

      REWIND DEV85
      REWIND DEV80
      REWIND DEV70
   10 READ( DEV70,1000,END=20 ) BUF08(1),CARD
      BUF02 = BUF08(1)(1:2)
      CALL DEFINE( 132,CARD )

C---- Skip input block cards

      IF( NWORDS .EQ. 1)THEN
         CALL FDPATH ( KOUNT,CARD,NEWPTH )
         IF( NEWPTH .GT. 0 ) GOTO 10
      ENDIF

      NUMBER = NUMBER + 1

C---- Skip 'sub-header' records

      IF( CARD(14:19) .EQ. 'HEADER' ) GO TO 10

C---- See if we need a sub-header record

      IF( ( BUF02 .NE. '  ' ) .AND. ( BUF02 .NE. OPATH ) )THEN
         BUF80(10) = BLN132
         BUF03     = '   '
         WRITE( BUF80(10),1500 ) BUF02

         WRITE( DEV80,1600 ) BUF80(10),BUF03

         NUMBER = NUMBER + 1
         OPATH = BUF02
      ENDIF

C---- Write header to file

      BUF03 = '   '
      WRITE( BUF03,2000,IOSTAT=IOFLAG ) BUF08(1)(3:3),BUF08(1)(8:8)

      WRITE( DEV80,2100 ) BUF03,CARD

C---- Check write status

      IF( IOFLAG .NE. 0 )THEN
         MESS =  BLNK80
         ECODE = 'E21'
         WRITE( MESS,3000 ) PATHWD(6)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

      GO TO 10

   20 RETURN

 1000 FORMAT( A8,A80 )
 1500 FORMAT( '*T',14X,'HEADER RECORDS FOR ',A2,'-PATHWAY' )
 1600 FORMAT(A80,A3)
 2000 FORMAT( '*',2A1 )
 2100 FORMAT(A3,A80)
 3000 FORMAT(' ERROR HEADER FOR ',A10)

      END

