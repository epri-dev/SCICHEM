      SUBROUTINE DEF256( ICOL, CARD, IFC )
C=====================================================================**
C        DEF256 Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Search columns (up to 256) of the SAMSON header record
C               defining the variables in the data file; array IC1
C               contains the column where fields begin and IC2 where
C               those fields end.
C
C               A field is a group of letters or numbers separated by
C               one or more spaces.
C
C     NOTE:     This routine is specific to the SAMSON format for the
C               hourly surface observations
C
C-----------------------------------------------------------------------
C     Data declarations
C

      IMPLICIT NONE
      
      INTEGER MODE,FLDMAX,ICOL, IFC, I, IRD3
      CHARACTER*(*) CARD

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     ICOL        Maximum length of card record
C     FLDMAX      Maximum number of fields to define
C
C     Data initialization
      DATA FLDMAX/30/
C
      PATH = 'SF'
      LOC  = 'DEF256'

C---- 1.  Initialize IC1, IC2, IRD3 and MODE.
C         IRD3 keeps a count on the number of fields found.
C         MODE keeps track of searching for a field beginning or ending
C              0 = searching for field beginning
C              1 = searching for field ending


      DO I=1,FLDMAX
         IC1(I) = 0
         IC2(I) = 0
      ENDDO

      IRD3 = 1
      MODE  = 0

C---- 2.  Loop on ICOL columns of record

      DO I=1,ICOL

         IF( MODE.EQ.0 )THEN
C---------- Searching for beginning of field
            IF( CARD(I:I).NE.' ' )THEN
C------------- Found beginning of a field
               IC1(IRD3) = I
               MODE = 1
            ENDIF

         ELSE
C---------- Searching for ending of field
            IF ( CARD(I:I).EQ.' ' )THEN
C------------- Found ending of a field
               IC2(IRD3) = I - 1
               IRD3 = IRD3 + 1
               IF( IRD3.GT.FLDMAX )THEN
                  IRD3 = IRD3 - 1
                  RETURN
               ENDIF
               MODE = 0
            ENDIF

         ENDIF

      ENDDO

      IFC = IRD3 - 1

C---- Test number of fields found, if less than 5, then at most only
C     the date and observation flag fields are in the file

      IF(IRD3.LE.5) THEN
         ECODE = 'E45'
         WRITE( MESS,1000 )
1000     FORMAT(' NO DATA FIELDS DEFINED FOR SAMSON FILE')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         SFSTAT = -1
      ENDIF

      RETURN
      END

