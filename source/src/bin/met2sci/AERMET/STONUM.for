      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C=====================================================================**
C*       STONUM Module for the AERMET Meteorological Preprocessor
C*                    (borrowed from ISCST)
C*
C*    Purpose: Gets number from a string variable
C*
C*    Programmer: Jeff Wang, Roger Brode
C*
C*    Date:    March 2, 1992
C*
C*    Inputs:  Input String Variable (STRVAR)
C*             Length of Character String (LENGTH)
C*
C*    Outputs: Numeric value (FNUM)
C*             ?? (IMUTI)
C*
C*    Called from: (This is a utility program required by several
C*                    routines
C-----------------------------------------------------------------------
C
C     Variable Declarations

      IMPLICIT NONE
      
      CHARACTER STRVAR*(*), CHK, NUMS*10
      REAL FNUM, CNUM, FDEC, FDC1, HEAD
      INTEGER I, IMUTI, LENGTH
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      NUMS = '0123456789'
      I = 1
      MEND = .FALSE.
      IN = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0
      HEAD  = 0.0
      IMUTI = 1
      FDEC  = 1.0

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LENGTH)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.+FLOAT(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*FLOAT(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               ENDIF
            ELSE
C              Handle The E-Type Real Number
               IF (.NOT.EMARK .AND. CHK.EQ.'E') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  ENDIF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0
               ELSEIF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSEIF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSEIF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSEIF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &                  .NOT.NMARK) THEN
C                 Set Repeat Number
                  MMARK = .TRUE.
                  IMUTI = INT(CNUM)
                  CNUM = 0.0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               ENDIF
            ENDIF
         ELSEIF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         ENDIF
         I = I + 1
      ENDDO

      FNUM = CNUM

C     In Case Of Negative Field, Value Set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      ENDIF

C     In Case of E-Format, Check for Exponents Out of Range
      IF (EMARK .AND. ABS(FNUM) .LE. 30.0) THEN
         FNUM = HEAD*10**(FNUM)
      ELSEIF (EMARK .AND. ABS(FNUM) .GT. 30.0) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSEIF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 10**30.0
         ENDIF
         GO TO 9999
      ENDIF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END

