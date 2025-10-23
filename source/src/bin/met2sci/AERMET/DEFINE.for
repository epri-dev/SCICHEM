      SUBROUTINE DEFINE( ICOL,CARD )
C=====================================================================**
C          DEFINE Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Searches columns 1 through 80 of the card image to define
C               the beginning and end of each keyword and parameter field.
C
C               Array IC1 defines the column where fields begin;
C               Array IC2 defines the column where fields end.
C
C-----------------------------------------------------------------------

C---- Local variables


      IMPLICIT NONE

      INTEGER FLDMAX,ICOL,I
      CHARACTER*(*) CARD

      LOGICAL :: INFLD, INQUOTE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C     ICOL    Number of columns in the runstream record being processed
C     FLDMAX  Maximum number of fields that can be defined on a record

      DATA FLDMAX/30/
      PATH = 'JOB       '
      LOC  = 'DEFINE'

C     Initialize the Blank Line and In-field Status Indicators
      INFLD   = .FALSE.
      INQUOTE = .FALSE.

C     IC1, IC2  defines the beginning and ending column of a field
C     NWORDS    keeps a count on the number of fields found.
C     MODE      tracks search for a field beginning or ending
C                  0 = searching for field beginning
C                  1 = searching for field ending

C---- Convert any lower case letters to upper case

      BUF80(1) = CARD
      CALL LWRUPR(BUF80(1))
      CARD = BUF80(1)

      DO I=1,FLDMAX
         IC1(I) = 0
         IC2(I) = 0
      ENDDO

      NWORDS = 0

C---- 2.  Loop on first ICOL columns of card

      DO I=1,ICOL

         IF( .NOT.INFLD .AND. BUF01(I).EQ.'"' )THEN
C           Location is the Beginning of a Field using "'s
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Set Mark of in a Quote Field
            INQUOTE = .TRUE.
C           Increment the Field Counter
            NWORDS = NWORDS + 1
            IF( NWORDS .GT. FLDMAX )THEN
               NWORDS = NWORDS - 1
               RETURN
            ELSE
C              Record the Location of Beginning of the Field
               IC1(NWORDS) = I + 1
            ENDIF
         ELSEIF( .NOT.INFLD .AND. BUF01(I).NE.' ' .AND. 
     &                            BUF01(I).NE.',' )THEN
C           Location is the Beginning of a Field
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            NWORDS = NWORDS + 1
            IF( NWORDS .GT. FLDMAX )THEN
               NWORDS = NWORDS - 1
               RETURN
            ELSE
C              Record the Location of Beginning of the Field
               IC1(NWORDS) = I
            ENDIF
         ELSEIF( INQUOTE .AND. BUF01(I).EQ.'"' )THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Set Mark of Not in a Quote Field
            INQUOTE = .FALSE.
C           Record the Location of Ending of the Field
            IC2(NWORDS) = I - 1
         ELSEIF( .NOT.INQUOTE .AND. INFLD .AND. (BUF01(I).EQ.' ' .OR. 
     &                                           BUF01(I).EQ.',') )THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            IC2(NWORDS) = I - 1
         ENDIF

C        Check for End of Input String
         IF( INFLD .AND. I.EQ.ICOL )THEN
C           Last field ends at ICOL
            IC2(NWORDS) = I
         ENDIF

      ENDDO

      RETURN
      END

