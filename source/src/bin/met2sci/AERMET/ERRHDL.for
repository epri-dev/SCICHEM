      SUBROUTINE ERRHDL( INUM,CPATH,CODE,CLOC,CMESS )
C=====================================================================**
C        ERROR Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To write messages to the log file.  All messages fit a
C               standard format, which then makes it possible to reread
C               them and generate general summary reports.
C
C     Called by:    MOST SUBROUTINES
C
C     Calls to:     -NONE-
C
C     Initial Release:  December 15,1996
C
C     Revision History:
C        10/31/96
C          -  changed length of error message (MESS) in the format
C             format statement from 40 to 48 characters; modified
C             'NUMBER' to print as a character so a 0 (zero) would
C             print as a blank
C
C        06/29/99
C          - changed subroutine name from ERROR to ERRHDL
C          - changed variable name from NUMBER to INUM
C
C        10/31/96
C          -  changed length of error message (MESS) in the format
C             format statement from 48 to 60 characters.
C
C-----------------------------------------------------------------------

C---- Local declarations

      IMPLICIT NONE
      

      INTEGER   IDEVIC,INUM
      CHARACTER CPATH*(*),CODE*3,CLOC*(*),CMESS*(*),CNUM*8               ! mec 1/2010

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C---- Determine which file unit to use for the message file:
C     DEV60 for a file; DEVIO if no file was defined (keyword REPORT)
      IF( STATUS(1,2).EQ.2 )THEN
         IDEVIC = DEV60
      ELSE
         IDEVIC = DEVIO
      ENDIF

C---- Convert NUMBER to a character string; convert a zero to a blank
      IF( INUM .GT. 0 .AND. INUM .LE. 99999999 )THEN
         WRITE( CNUM, 500 ) INUM
      ELSEIF( INUM .GT. 99999999 )THEN
         CNUM = '99999999'
      ELSEIF( INUM .LT. 0 .AND. INUM .GE. -9999999 )THEN
         WRITE( CNUM, 500 ) INUM
      ELSEIF( INUM .LT. -9999999 )THEN
         CNUM = '-9999999'
      ELSE
         CNUM = '        '
      ENDIF
  500 FORMAT(I8)

      WRITE(IDEVIC,1000) CNUM,CPATH,CODE,CLOC,CMESS(1:110)
 1000 FORMAT(A8,1X,A10,1X,A3,1X,A8,':',A110)                             ! mec 1/2010

      RETURN
      END

