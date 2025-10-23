      SUBROUTINE D3280H(CARD32,NEWLOC,NEWYR,NEWMO,NEWDY,ISTAT)
C-----------------------------------------------------------------------
C     D3280H Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decode the station and date group from the TD3280-format
C               observations and return them to the main program.
C
C     Called by: GETSFC
C
C     Arguments:
C       CARD32   Input   Card image with surface data
C       NEWLOC   Output  Station identifier
C       NEWYR    Output  Year retrieved
C       NEWMO    Output  Month retrieved
C       NEWDY    Output  Day retrieved
C       NEWHR    Output  Hour retrieved
C       ISTAT    Output  Program status of the decode
C
C     Revision history:
C       11/30/94 - Made into a separate subroutine from an ENTRY point
C                  (PES, Inc.)
C
C       1/2010 - MACTEC
C         - Added capability to determine ASOS commission date for
C           for treatment of truncated ASOS wind speeds
C         - Checks for internal WBAN consistency. WBAN in control file
C           should = WBAN in NWS surface file which should contain only one
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER     ISTAT, IOST20, NEWYR, NEWMO, NEWDY
      CHARACTER   CARD32*32, NEWLOC*8
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
      
C     Data initialization
      DATA LOC/'D3280H'/ , PATH/'SURFACE   '/

C---- Initialize 4-char and 3-char CALL IDs
      csfcobsCALL4 = ' '
      csfcobsCALL3 = ' '

C-----------------------------------------------------------------------
C     Read the "header" data - note that there is no need to check for
C     an end-of-file condition because the read is from a character
C     string.

      READ(CARD32,20,ERR=200,IOSTAT=IOST20) NEWLOC, NEWYR, NEWMO, NEWDY
   20 FORMAT (6X, A5, 8X, I2, I2, 2X, I2)                                ! dtb121 02092
      
C ----------------------------------------------------------------------
C     MACTEC, 1/2010
C     Store WBAN from first observation - this will be used to get
C     the commission date and check consistency between WBAN in 
C     control file and across all observations.
C
C     GETSFC.FOR assumes data is being read from tape and by-passes
C     observations for stations that do not match. It is doubtful
C     tape access is still in use.  It is assumed a single TD3280 met
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

      IF (.NOT. GotSfcObsWBAN) THEN  
      
         cSfcObsWBAN = NEWLOC
         
C        convert character date to integer date
         READ(cSfcObsWBAN,'(I5)',ERR=900) ISfcObsWBAN
         
         GotSfcObsWBAN = .TRUE. 
         
         IF (SFLOC(4:8) .NE. cSfcObsWBAN) THEN
            MESS =  BLNK80
            WRITE (MESS, 6510) SFLOC(4:8), cSfcObsWBAN
            CALL ERRHDL(0,PATH,'E46',LOC,MESS)
C           Set ISTAT=2 indicating station mismatch and return
            ISTAT = 2
            RETURN
         ENDIF
         
      ELSE

         IF (NEWLOC .NE. cSfcObsWBAN) THEN
            MESS =  BLNK80
            WRITE (MESS, 6520) cSfcObsWBAN, NEWLOC
            CALL ERRHDL(0,PATH,'E46',LOC,MESS)
C           Set ISTAT=2 indicating station mismatch and return
            ISTAT = 2
            RETURN
         ENDIF
         
      ENDIF

 6510 FORMAT(' Site ID mismatch between data file and LOCATION ',
     &        'keyword. LOCATION = ',A5,'; SURFACE file = ',A5)
 6520 FORMAT(' Site ID mismatch within SURFACE data file: ',A5,' / ',A5)
   
C     Search for ASOS commission date based on iSfcObsWBAN
      IF( .not. SrchCommDate )then
         CALL FNDCOMDT(PATH)
      ENDIF  
   
   
      ISTAT = 0
      RETURN

  200 CONTINUE
  
      ISTAT = 1
      WRITE (DEV60,2001) CARD32
 2001 FORMAT(A32)
      WRITE (DEV60,2002) NEWLOC, NEWYR, NEWMO, NEWDY
 2002 FORMAT (A8, 3I6)
 
      RETURN
      
 900  MESS =  BLNK80
      WRITE (MESS, 3500) NEWLOC
 3500 FORMAT(' ERROR converting SURFACE station WBAN to integer: ', A5)
      CALL ERRHDL(0,PATH,'E46',LOC,MESS)
      GO TO 200
      
      END

