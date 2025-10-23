        SUBROUTINE READRL (FIELD,STRLEN,VAR,ISTAT)
C=====================================================================**
C
C     Purpose:  To read a real value into VAR from CHAR*LEN field
C
C     Revision History:
C          March 2012   Changed variable name LEN to STRLEN to avoid 
C                         conflict with the function LEN
C                       Changed variable name from DCPT to DECIMALPT
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      CHARACTER*(*) FIELD
      CHARACTER*8 RDFMT
      INTEGER STRLEN,ISTAT,DECIMALPT,DEC
      REAL VAR
C
      DECIMALPT = INDEX(FIELD,'.')
      IF( DECIMALPT .EQ. 0 ) DECIMALPT = STRLEN
      DEC = STRLEN - DECIMALPT
      WRITE( RDFMT,10 ) STRLEN,DEC
 10   FORMAT ('(F', I2, '.', I2, ')')
      READ( FIELD,FMT=RDFMT, IOSTAT=ISTAT) VAR
      RETURN
      END

