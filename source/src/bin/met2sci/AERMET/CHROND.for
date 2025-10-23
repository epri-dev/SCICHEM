      SUBROUTINE CHROND( PATH,IYR,IJD,IYDY )
C=======================================================================
C          CHROND Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Converts date to a common sequential day.  The zero 
C               point is January 1, 1900, i.e., day 1 is January 1,1900
C
C     Assumption: Valid years are 1950 - 2049
C
C     Argument list:
C          Input:   IYR  - year (2 digits, e.g., 94 = 1994, 02 = 2002)
C                   IJD  - Julian day (3 digits)
C          Output:  IYDY - date converted to sequential day since 1/1/1900
C
C
C     Called by:     UAEXT,GETSDG,GETMIX,SFEXT,GETSFC,MERGE
C
C     Calls to:      ERROR
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

        IMPLICIT NONE

        INTEGER NDAY,ICNT, I2000
        INTEGER, intent(in) :: IYR,IJD
        INTEGER, intent(out) :: IYDY
        INCLUDE 'WORK1.INC'
        
        LOC = '  CHROND'

C-----------------------------------------------------------------------
C *** Check for invalid data

        IF( IYR.LT.0 .OR. IYR.GE.100 )THEN
           MESS =  BLNK80
           ECODE = 'E19'
           WRITE(MESS,1000) IYR
           CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
           IYDY = -9999
           RETURN
        ENDIF

C *** Test for leap year and determine the number of days in the year
C     (Not valid for years 2100, 2200, 2300)
        IF( MOD(IYR,4) .EQ. 0 .OR. IYR .EQ. 0 )THEN
           NDAY = 366
        ELSE
           NDAY = 365
        ENDIF

C *** Check for a valid julian day

        IF( IJD .LT. 1  .OR. IJD .GT. NDAY )THEN
           MESS =  BLNK80
           ECODE = 'E19'
           WRITE(MESS,1030) IJD
           CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
           IYDY = -9999
           RETURN
        ENDIF

C *** Compute the chronological day -- for Y2K compliance, 100 is added
C     to the year for the years 2000 - 2049

        IF( IYR .GE. 50  .AND.  IYR .LE. 99 )THEN
C--------- 20th century
           ICNT = (IYR+3)/4
           IYDY = 366*ICNT + 365*(IYR-ICNT) + IJD
        ELSEIF( IYR .GE. 00 .AND.  IYR .LE. 49 )THEN
C--------- 21st century
           I2000 = IYR + 100
           ICNT = (I2000+3)/4
           IYDY = 366*ICNT + 365*(I2000-ICNT) + IJD
        ENDIF

        RETURN
C-----------------------------------------------------------------------
C *** Format statements.

 1000   FORMAT(' INPUT YEAR (',I4,') OUT OF RANGE')
 1010   FORMAT(' JULIAN DAY (',I4,') OUTSIDE RANGE OF 1-366')
 1030   FORMAT(' INVALID INPUT: JULIAN DAY = ',I6)
        END

