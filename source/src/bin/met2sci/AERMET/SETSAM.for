      SUBROUTINE SETSAM( ISTAT )
C=====================================================================**
C        SETSAM Module of the AERMET Meteorological Pre-processor
C
C     PURPOSE:    To determine the data structure of the SAMSON data
C                 (up to 21 variables may be in the file)
C
C     INPUTS:     SAMSON met data file (only first 2 records are needed
C                 in this routine)
C
C     OUTPUTS:    SAMFMT - format of the data (in COMMON block)
C                 NVARS  - number of variables that will be read
C                 IDVAR  - variable number (e.g., wind speed is 13)
C                 JVALUE -
C
C     Assumptions:  The first record of the SAMSON file contains station
C                   information and the second contains format information.
C                   These are as written by the program that retrieves
C                   data from the CD (run by the user *prior* to AERMET)
C
C     CALLED FROM: SFEXT
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        12/19/00: PES
C                - changed the format specifier of the precipitation
C                  field to include the precipitation flag (from A6 to A7)
C        05/29/08: MACTEC Federal Programs
C                -  added string length to format when reading character
C                   variable ALINE and writing part of a format statement
C
C       1/2010 - MACTEC
C         - Added capability to determine ASOS commission date for
C           for treatment of truncated ASOS wind speeds
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      INTEGER       NCOL, IZMSL, IOST20, ISTN, ISTAT, IFC, IPOS 
      INTEGER       IVAR, IMIT
      REAL          FNUM, ELEV_DIFF
      CHARACTER*256 ALINE
      CHARACTER*40  FIELD(MAXVAR)
      INTEGER ISAMWBAN
C      INTEGER IDVAR(MAXVAR-5)
      CHARACTER*2 VFMT(MAXVAR-5)
      CHARACTER*4 AZMSL

      
      INCLUDE 'WORK1.INC'

C---- Data Initializations
      DATA VFMT /'A4','A4','A7','A7','A7','A2','A2','A5','A5','A3',
     &           'A4','A3','A5','A6','A6','A9','A4','A6','A4','A3',
     &           'A7'/
      
      PATH = 'SURFACE   '
      LOC  = 'SETSAM'
      NCOL = 256

C---- Initialize 4-char and 3-char CALL IDs
      csfcobsCALL4 = ' '
      csfcobsCALL3 = ' '
      ELEV_DIFF = 0.0

C     Read station header record for the station ID and elevation
      READ (DEV20,6000) ISAMWBAN, AZMSL
 6000 FORMAT (T2,I5,T56,A4)
C6000 FORMAT (T2,I5,T8,A22,T31,A2,T34,I3,T39,A1,T40,I2,T43,I2,
C    &       T47,A1,T48,I3,T52,I2,T56,I4)

C     set iSfcObsWBAN and cSfcObsWBAN
      iSfcObsWBAN = ISAMWBAN
      
      WRITE(cSfcObsWBAN,'(I5)') iSfcObsWBAN      

C     Search for ASOS commission date based on iSfcObsWBAN
      IF( .not. SrchCommDate )then
         CALL FNDCOMDT(PATH)
      ENDIF

      IF( AZMSL .EQ. '    ' )THEN
C ---    Elevation from header record is missing
         MESS =  BLNK80
         WRITE (MESS, 6200) ISAMWBAN
 6200    FORMAT(' SURFACE station elevation in SAMSON file is ',
     &                 'missing for WBAN # ',I6)
         CALL ERRHDL(1,PATH,'W46',LOC,MESS)
         IF( GOTPWELEV(3) )THEN
C ---       User specified station elevation on the LOCATION keyword,
C           include in warning message
            MESS =  BLNK80
            WRITE (MESS, 6250) PWELEV(3)
 6250       FORMAT('  However, user-specified elevation on LOCATION ',
     &               'keyword = ',F8.2,'m')
            CALL ERRHDL(2,PATH,'W46',LOC,MESS)
            
         ELSE
C ---       No user specified station elevation on the LOCATION keyword,
C           and no elevation from file, issue warning message
            MESS =  BLNK80
            WRITE (MESS, 6260)
 6260       FORMAT('  SURFACE elevation also missing from LOCATION ',
     &               'keyword; recommend specifying elevation.')
            CALL ERRHDL(2,PATH,'W46',LOC,MESS)
         
         ENDIF

      ELSE      
C ---    Elevation field is not blank, read value from character string
         READ (AZMSL,'(I4)') IZMSL
         SFELEV = FLOAT(IZMSL)
         GOTSFELEV = .TRUE.
C ---    If user specified station elevation on the LOCATION keyword,
C        check for consistency with value from the file (within +/- 2%):
         IF( GOTPWELEV(3) )THEN
            ELEV_DIFF = PWELEV(3) - SFELEV
            IF( ABS(ELEV_DIFF) .GT. 0.02*SFELEV )THEN
               MESS =  BLNK80
               WRITE (MESS, 6300) PWELEV(3), SFELEV
 6300          FORMAT(' SURFACE elevation on LOCATION ',
     &                 'keyword (',F8.2,') differs from ',
     &                 'elevation in SAMSON file (',F8.2,').')
               CALL ERRHDL(0,PATH,'W46',LOC,MESS)
            ENDIF
         ELSE
C ---       Elevation available from data file, but not used, and
C           no elevation specified on SURFACE LOCATION keyword
            MESS =  BLNK80
            WRITE (MESS, 6400) SFELEV
 6400       FORMAT(' NO SURFACE elevation on LOCATION ',
     &              'keyword, but elevation from SAMSON ',
     &              'file header = ',F8.1,' m;')
            CALL ERRHDL(1,PATH,'W46',LOC,MESS)
            MESS =  BLNK80
            WRITE (MESS, 6450) 
6450        FORMAT('  Recommend specifying elevation on ',
     &               'the LOCATION keyword')
            CALL ERRHDL(2,PATH,'W46',LOC,MESS)
         ENDIF

      ENDIF       

C     Compare the station ID to the station-to-extract ID
      READ( SFLOC, 2000, IOSTAT=IOST20 ) ISTN
 2000 FORMAT(I8)

      IF( IOST20 .NE. 0 )THEN
         MESS =  BLNK80
         WRITE (MESS, 6600) SFLOC
 6600    FORMAT(' ERROR converting SURFACE station ID (',A8,') on',
     &           'LOCATION keyword to integer.')
         CALL ERRHDL(0,PATH,'E47',LOC,MESS)

      ELSEIF( ISTN .NE. ISAMWBAN )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE (MESS, 6700) ISAMWBAN, ISTN
 6700    FORMAT(' SAMSON: Station ID (' ,I5, ') from data file does ',
     &           'NOT match station ID on LOCATION keyword (',I8,').' )
         CALL ERRHDL(0,PATH,'E47',LOC,MESS)
         RETURN
      ENDIF

C     Read extracted-variables header record
      READ (DEV20,'(A256)') ALINE

C     We need to know if station pressure is one of the variables for
C     later use
      IF( INDEX(ALINE,'11') .NE. 0 )THEN
         STNPINFILE = .TRUE.
      ELSE
         STNPINFILE = .FALSE.
      ENDIF
      
C     Parse the line to determine the individual variables
      CALL DEF256 (NCOL,ALINE,IFC)

C     Get contents of each field as a character variable (FIELD)
      CALL GETFLD( IFC,ALINE,FIELD )

C     Decrement the counter for number of variables to exclude
C     the <yr,mo,dy,hr,i> fields
      NVARS = IFC-5

C     Determine the format and the variables to read
      WRITE (SAMFMT(1:15),'(A15)') "(4A3,1X,A1,1X,"
      IPOS = 14

      DO IVAR = 1,NVARS

C        Get the ID of the variable, beginning with field 6
         CALL STONUM(FIELD(IVAR+5),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
            MESS =  BLNK80
            WRITE (MESS, 6500) FIELD(IVAR+5)(1:5)
 6500       FORMAT(' Error converting SAMSON variable ID: ', A5)
            CALL ERRHDL(0,PATH,'E46',LOC,MESS)
            ISTAT = 1
            GO TO 999
         ELSE
            IDVAR(IVAR) = INT(FNUM)
         ENDIF

         IPOS = IPOS+1
         WRITE (SAMFMT(IPOS:IPOS+1),'(A2)') VFMT(IDVAR(IVAR))
         IPOS = IPOS + 2
         IF (IVAR .NE. NVARS) THEN
            WRITE (SAMFMT(IPOS:IPOS+3),'(A4)') ",1X,"
            IPOS = IPOS +3
         ELSE
           WRITE (SAMFMT(IPOS:IPOS),'(A1)') ")"
         ENDIF

      ENDDO

999   RETURN
      END

