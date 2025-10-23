      SUBROUTINE DATER ( DCALL, TCALL )
C=====================================================================**
C          DATER Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To return the system date and time.  This routine is for
C               IBM or compatible personal computers using the Lahey
C               Fortran 90 compiler
C
C     Called by: SUMRY1, SUMRY2, AUDIT, STAGE 3 PROCESSING
C
C     Input:
C        <none>
C
C     Output:
C        DCALL    = 9-byte string with the system date
C        TCALL    = 8-byte string with the system time
C
C     Initial Release: December 15, 1992
C
C     Revision History:
C        12/2000 - Use Fortran 90 DATE_AND_TIME routine
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      CHARACTER   DCALL*9, TCALL*8
      CHARACTER   CDATE*8, CTIME*10, CZONE*5
      CHARACTER*3 AMON(12),APTMON

      INTEGER  :: IDATETIME(8)
      INTEGER  :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

      DATA AMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',
     &          'OCT','NOV','DEC'/

      DCALL = ' '
      TCALL = ' '

C     Call date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

C     Convert year to two digits and store array variables
      IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
      IPTMON = IDATETIME(2)
      APTMON = AMON(IPTMON)
      IPTDAY = IDATETIME(3)
      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL, '(I2, "-", A3, "-", I2.2)' ) IPTDAY, APTMON, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC


      RETURN
      END

