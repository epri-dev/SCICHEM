      SUBROUTINE GETWRD( NUM,KOUNT,CARD,
     &                   LENMIN,LENMAX,JUST,WNAME,VALUE,ISTAT )
C=====================================================================**
C          GETWRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:     Retrieves the 'VALUE' (as a character string) of the
C                  'NUM'-th word within the control card image.
C
C     Arguments:
C
C        NUM    = Number (position) of word within image
C
C        CARD   = The 80 character 'image'
C
C        LENMIN = Minimum acceptable length of the word
C                 0 = optional entry
C
C        LENMAX = Maximimum acceptable length of the word
C
C        JUST   = How to store within value
C                 If 1 then right justify
C                 If 2 then left  justify
C
C        WNAME  = Name of word we are attempting to fetch
C
C        VALUE  = The NUM-th word, in ASCII characters
C                 (has length as defined in the calling program).
C                 Assumption: LENMAX is the actual length of VALUE.
C                             If this is not correct, errors may occur.
C
C        ISTAT  = Status of fetch, 1 = errors occurred
C                                  2 = fetch worked
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER       WIDTH,NUM,LENMAX,LENMIN,ISTAT,JUST, I
      CHARACTER     CARD*(*),WNAME*(*)
      CHARACTER*(*) VALUE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'GETWRD'
      ISTAT = 0

      DO I = 1,LENMAX
         VALUE(I:I) = ' '
      ENDDO

C---- Check to insure requested field exists on control card image

      IF( ( IC1(NUM) .EQ. 0 ) .AND.
     &    ( IC2(NUM) .EQ. 0 ) .AND.
     &      ( LENMIN .GT. 0 ) )THEN
         ISTAT = 1
         ECODE = 'E00'
         MESS =  BLNK80
         WRITE( MESS,1000 ) WNAME,NUM
 1000    FORMAT(1X,A12,'(FIELD ',I2,') IS BLANK')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C---- Check values of LENMIN, LENMAX and JUST

      IF( LENMIN .LT. 0 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE( MESS,2000 ) WNAME
 2000    FORMAT(' INTERNAL ERROR: MIN LEN INVALID FOR ', A12)
         ECODE = 'E00'
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN

      ELSEIF( LENMAX .LE. 0 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE( MESS,2010 ) WNAME
 2010    FORMAT(' INTERNAL ERROR: MAX LEN INVALID FOR ', A12)
         ECODE = 'E00'
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

      IF( (JUST .LT. 1) .OR. (JUST .GT. 2) )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE( MESS,3000 ) WNAME
 3000    FORMAT(' INTERNAL ERROR: JUSTIFICATION INVALID FOR ',A12)
         ECODE = 'E00'
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C---- Compute WIDTH of NUM-th word

      WIDTH = IC2(NUM) - IC1(NUM) + 1
      
      IF( (WIDTH .LT. LENMIN) .OR. (WIDTH .GT. LENMAX) )THEN
         ISTAT = 1
         MESS =  BLNK80
         IF( WIDTH .LE. 12 )THEN
            WRITE( MESS,4000 ) CARD(IC1(NUM):IC2(NUM)), NUM
         ELSE
            WRITE( MESS,4000 ) CARD(IC1(NUM):IC1(NUM)+11), NUM
         ENDIF
 4000    FORMAT(' Field length incorrect on input image: (',A12,')',
     &                                         '; Field No. = ',I2)
         ECODE = 'E07'
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
C----    Assign value based on actual width, up to LENMAX, for QA purposes
         VALUE(1:MIN(WIDTH,LENMAX)) = CARD( IC1(NUM):IC2(NUM) )
         RETURN
      ENDIF

C----Check JUST (justification of the value; right=1, left=2 )
C        and store accordingly

      IF( JUST .EQ. 1 )THEN
         POSINX = LENMAX - WIDTH + 1
         VALUE(POSINX:LENMAX) = CARD( IC1(NUM):IC2(NUM) )
      ELSE
         VALUE(1:WIDTH) = CARD( IC1(NUM):IC2(NUM) )
      ENDIF

C---- All seems well to this point
      ISTAT = 2
      RETURN
      END

