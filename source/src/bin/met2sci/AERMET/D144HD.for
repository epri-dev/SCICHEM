      SUBROUTINE D144HD(CARD,NEWLOC,NEWYR,NEWMO,NEWDY,NEWHR,
     7                  ISTAT)
C=====================================================================**
C        D144HD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decode the station and date group from the CD144 and
C               SCRAM format "header" data and return them to the main
C               program.
C
C     Called by: GETSDG
C
C     Arguments:
C       CARD     Input   Card image with surface data
C       NEWLOC   Output  Station identifier
C       NEWYR    Output  Year retrieved
C       NEWMO    Output  Month retrieved
C       NEWDY    Output  Day retrieved
C       NEWHR    Output  Hour retrieved
C       ISTAT    Output  Program status of the decode
C
C     Revision history:
C
C       11/30/94
C         - Made into a separate subroutine from an ENTRY point
C           reformatted.
C
C       1/2010 - MACTEC
C         - Added capability to determine ASOS commission date for
C           for treatment of truncated ASOS wind speeds
C         - Checks for internal WBAN consistency. WBAN in control file
C           should = WBAN in NWS surface file which should contain only one
C         - Check format valid date range to determine if data was likely 
C           reformatted
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

      INTEGER     ISTAT, IOST20, NEWYR, NEWMO, NEWDY, NEWHR
      CHARACTER   CARD*(*), NEWLOC*8             ! mec 1/2010: replaced variable length CARD with *80
      
      INTEGER     IMIT
      
      INTEGER     YR4, IYYYYMMDD                ! mec 1/2010: 4-digit year obs, 8-digit year-month-day obs
      INTEGER     JULIAN                        ! mec 1/2010: Julian day function, in GREG.FOR
      INTEGER     IJDAY                         ! mec 1/2010: julian day obs
      
C     Data initialization
      DATA LOC/'D144HD'/ , PATH/'SURFACE   '/

C---- Initialize 4-char and 3-char CALL IDs
      csfcobsCALL4 = ' '
      csfcobsCALL3 = ' '

C     Set valid start and end dates separately for CD144 and SCRAM formats
C     CD144                                     ! mec 1/2010
      IF (INDEX(SFFMT,'CD144') .NE. 0) THEN     ! mec 1/2010
         SfcVldStart = 0                        ! mec 1/2010
         SfcVldEnd   = 19951231                 ! mec 1/2010
C     SCRAM                                     ! mec 1/2010
      ELSE
         SfcVldStart = 19840101                 ! mec 1/2010
         SfcVldEnd   = 19921231                 ! mec 1/2010
      ENDIF
      
      
C-----------------------------------------------------------------------
C     Read the "header" data - note that there is no need to check for
C     an end-of-file condition because the read is from a character
C     string

      READ(CARD,20,ERR=200,IOSTAT=IOST20) NEWLOC, NEWYR, NEWMO,
     &                                            NEWDY, NEWHR

   20 FORMAT(A5,4I2)
   

C ----------------------------------------------------------------------
C     MACTEC, 1/2010
C     Store WBAN from first observation - this will be used to get
C     the commission date and check consistency between WBAN in 
C     control file and across all observations.
C
C     GETSFC.FOR assumes data is being read from tape and by-passes
C     observations for stations that do not match. It is doubtful
C     tape access is still in use.  It is assumed a single CD144 met
C     file will contain data for only one WBAN.  WBAN should match
C     control file and be internally consistent.  Processing stops
C     if a mismatch is encountered. 

C ----------------------------------------------------------------------
C     Store WBAN From First Observation
C     Compare WBAN from control file to first obs WBAN
C        No match - error, abort
C     This assignment and check should only be performed once, after
C        the first observation was read.
C     For all subsequent observations, compare WBAN from current obs
C        to the WBAN stored from the first obs.
C ----------------------------------------------------------------------   

      IF (.NOT. GotSfcObsWBAN) THEN                                  ! mec 1/2010 
      
         cSfcObsWBAN = NEWLOC                                        ! mec 1/2010
         
C        convert character date to integer date
         READ(cSfcObsWBAN,'(I5)',ERR=900) ISfcObsWBAN                ! mec 1/2010
         
         GotSfcObsWBAN = .TRUE.                                      ! mec 1/2010
         
C        check to see that WBAN in control file = WBAN in sfc file
         IF (SFLOC(4:8) .NE. cSfcObsWBAN) THEN                       ! mec 1/2010
            MESS =  BLNK80                                           ! mec 1/2010
            WRITE (MESS, 6510) SFLOC(4:8), cSfcObsWBAN               ! mec 1/2010
            CALL ERRHDL(0,PATH,'E46',LOC,MESS)                       ! mec 1/2010
C           Set ISTAT=2 indicating station mismatch and return
            ISTAT = 2
            RETURN
         ENDIF                                                       ! mec 1/2010
         
      ELSE

C        check to see that WBAN is consistent throughout sfc file
c        as each record is processed
         IF (NEWLOC .NE. cSfcObsWBAN) THEN                           ! mec 1/2010
            MESS =  BLNK80                                           ! mec 1/2010
            WRITE (MESS, 6520) cSfcObsWBAN, NEWLOC                   ! mec 1/2010
            CALL ERRHDL(0,PATH,'E46',LOC,MESS)                       ! mec 1/2010
C           Set ISTAT=2 indicating station mismatch and return
            ISTAT = 2
            RETURN
         ENDIF                                                       ! mec 1/2010
         
      ENDIF

 6510 FORMAT(' Site ID mismatch between data file and LOCATION ',
     &        'keyword. LOCATION = ',A5,'; SURFACE file = ',A5)
 6520 FORMAT(' Site ID mismatch within SURFACE data file: ',A5,' / ',A5)
   

C     Search for ASOS commission date based on iSfcObsWBAN
      IF( .not. SrchCommDate )then                                   ! mec 1/2010
         CALL FNDCOMDT(PATH)                                         ! mec 1/2010
      ENDIF                                                          ! mec 1/2010



C ----------------------------------------------------------------------
C     Check obs date against valid date range for SAMSON format
C     If date is outide of date range, set flag to issue a warning.
C ----------------------------------------------------------------------    

c     get 4-digit year
      CALL YR2TOYR4(newyr,YR4)                                       ! mec 1/2010

c     compute 8 and 6-digit date
      IYYYYMMDD = YR4*10000 + newmo*100 + newdy                      ! mec 1/2010

C     Check each record to see if date is valid for this format
c     No need to check once one record out of range is encountered
c     since warning will be issued only once per file
      IF ( .NOT. ReformattedSFC )THEN                                ! mec 1/2010
         IF( (IYYYYMMDD .LT. SfcVldStart) .OR.                       ! mec 1/2010
     &       (IYYYYMMDD .GT. SfcVldEnd) )THEN                        ! mec 1/2010
            ReformattedSFC = .TRUE.                                  ! mec 1/2010

            MESS =  BLNK80                                      ! mec 1/2010
            WRITE(MESS,6610) SFFMT, SfcVldStart, SfcVldEnd      ! mec 1/2010
 6610       FORMAT(' SURFACE observations are outside range of valid ',
     &              'dates for ',A8,' format: ',I8,'-',I8 )
            CALL ERRHDL(0,PATH,'W40',LOC,MESS)              ! mec 1/2010
         
            MESS =  BLNK80                                  ! mec 1/2010
            WRITE(MESS,'(A)')
     &        '  Cloud cover will be set to missing for ASOS records!'! mec 1/2010
            CALL ERRHDL(0,PATH,'W40',LOC,MESS)              ! mec 1/2010

         ENDIF                                                      ! mec 1/2010
      ENDIF                                                          ! mec 1/2010

C     get julian day for the current surface observation
      IJDAY = JULIAN(NEWYR,NEWMO,NEWDY)                              ! mec 1/2010

C     determine if the current record is ASOS based on the obs date
      CALL ASOSREC(NEWYR,NEWMO,NEWDY,NEWHR,IJDAY,SFLST)              ! mec 1/2010
   
      ISTAT = 0
      RETURN

  200 CONTINUE

C     error encountered; set ISTAT=1
      ISTAT = 1

      RETURN
      
 900  MESS =  BLNK80
      WRITE (MESS, 3500) NEWLOC
 3500 FORMAT(' ERROR converting SURFACE station WBAN to integer: ', A5)
      CALL ERRHDL(0,PATH,'E46',LOC,MESS)
      GO TO 200
      
      END

