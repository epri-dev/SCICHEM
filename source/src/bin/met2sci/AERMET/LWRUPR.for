      SUBROUTINE LWRUPR (RUNST1)
C=====================================================================**
C        LWRUPR Module the AERMET Meteorological Preprocessor
C        [Adapted from the code for PCRAMMET (version 95xxx)]
C
C     Purpose: Transfer all characters from lower case to
C              upper case (using INDEX Intrinsic*Function)
C              note that the CHAR*80 RUNST1 variable includes
C              the original case for echoing and for later use
C              to retrieve filenames.
C
C     Programmer: Roger Brode, Kevin Stroupe for ISC2
C
C     Date:    March 2, 1992
C
C     Modified:   Jayant Hardikar, PES, Inc., February 13, 1995
C                 Adapted for PCRAMMET - Made Generic
C
C     Inputs:  Input runstream card image (80 character array)
C              number of characters in string, PARAMETER ISTRG
C
C     Outputs: Input runstream card image (array) in uppercase
C
C-----------------------------------------------------------------------
C*    Variable Declarations

      IMPLICIT NONE

      INTEGER I, INDCHK
      
      INTEGER, PARAMETER :: ISTRG = 132
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26
      CHARACTER*1 RUNST(ISTRG)
      CHARACTER   RUNST1*132

C*    Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO I = 1, ISTRG
         READ (RUNST1(I:I),'(A1)') RUNST(I)
      ENDDO

      DO I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            ENDIF
         ENDIF
      ENDDO

      DO I = 1, ISTRG
         WRITE (RUNST1(I:I),'(A1)') RUNST(I)
      ENDDO

      RETURN
      END

