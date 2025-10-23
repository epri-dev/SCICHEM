      SUBROUTINE MDCARD( KOUNT,CARD,ISTAT )
C=====================================================================**
C     Module MDCARD of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  To process the user's selection of the dispersion
C               model, and thereby the format of the meteorological
C               output data file.
C
C     Arguments
C        CARD        runstream record
C        ISTAT       status of processing the record
C                    1 = error occurred
C                    2 = all ok
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER    ISTAT, I
      CHARACTER  CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      PATH = PATHWD(6)
      LOC  = 'MDCARD'
      ISTAT = 0


C---- Check the number of fields on this record
      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''MODEL'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''MODEL'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C.... The following section of code is not needed but remains for
C     possible future usage
C
C     IF AN OUTPUT CARD WAS ALREADY READ, THEN THE MODEL CHOSEN
C     MAY NOT AGREE WITH THE DEFAULT USED
C
C      IF (STATUS(PATHID,22) .LT.2) THEN
C         MESS =  BLNK80
C         ECODE = 'E--'
C         WRITE( MESS,4044 )
C4044      FORMAT(1X,' MODEL CARD MUST PRECEDE OUTPUT CARD')
C         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
C         ISTAT = 1
C         MDSTAT = 0
C         RETURN
C      ENDIF
C.....


C---- Fetch of 'MODEL' name
      BUF08(2) = BLNK08
      BUF08(1) = 'MODELNM '
      CALL GETWRD(2, KOUNT, CARD, 1,8,1,BUF08(1),BUF08(2),ISTAT)
      IF (ISTAT.EQ.1 )THEN
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,2000 )
2000     FORMAT(' ERROR FROM S.GETWRD: MODEL NAME')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Compare to the array of dispersion model names
      DO I=1,NDISP
         IF( BUF08(2).EQ.DISPMD(I) )THEN
            MDSTAT = I
            GO TO 20
         ENDIF
      ENDDO

C---- This point is reached only if no match is found

      MESS =  BLNK80
      ECODE = 'E06'
      WRITE( MESS,4000 ) BUF08(1)
4000  FORMAT(1X,A8,':NO MATCH WITH AVAILABLE MODELS')
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      MDSTAT = 0
      RETURN

C---- All looks ok, return
  20  ISTAT = 2

      RETURN
      END

