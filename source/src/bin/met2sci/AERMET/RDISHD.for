      SUBROUTINE RDISHD(KOUNT, ISTAT)

C=====================================================================**
C     PURPOSE:    Extract TD-3505 (ISHD) surface data, process as necessary,
C                 and write remapped data to the IQA file
C
C     PROGRAMMER: Desmond Bailey
C
C     VERSION DATE: December 2002
C
C     INPUTS:     TD-3505 [Integrated Surface Hourly Data (ISHD)] file
C
C     OUTPUTS:    Hourly meteorological data (IQA file)
C
C     CALLED FROM: SFEXT
C
C     REVISION HISTORY:
C        05/29/08: MACTEC Federal Programs
C                - Use 0.0 for station elevation if the elevation was not
C                  specified on the LOCATION keyword record (and the
C                  elevation is not in the data file)
C                - Compute station pressure from sea level
C                  pressure and station elevation or just station
C                  elevation using the standard atmosphere
C                - added'QNN' as record delimeter; moved read statements
C                  for GA1-GA6 codes to prevent reading unless needed.
C                - intialized KDISCARD
C
C       1/2010 - MACTEC
C         - Added capability to determine ASOS commission date for
C           for treatment of truncated ASOS wind speeds
C         - Check for consistency between internal ASOS flag
C           and ASOS commissioning date
C         - Checks for internal WBAN consistency. Should contain only one
C-----------------------------------------------------------------------

C     Variable Declarations


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

      REAL     SFSP, DB, DP, RH, TEMP, SLVP
      REAL     STA_LAT, STA_LON, ELEV_DIFF

      INTEGER  LHR, THIS_HR, THIS_MN, LAST_HR, LAST_MN, KDISCARD         ! dtb #506  06299

      INTEGER  I, INC, IYYYYMMDD, IFIX, ISTAT, IVBL
      INTEGER  SF2YR, SF4YR, SFJDY, SFCDY
      INTEGER  SFGMN, ID_WBAN, ID_USAF
      INTEGER  ITEMP, IBOG, MW1, MW2, ASKY                               ! rwb #518  06341

      INTEGER  JULIAN, LDAYC, NRECS, SFBFST, NCALM, NVAR                 ! dtb #505  06299
      INTEGER  ISHOB(35), MISS(35)                                       ! rwb #518  06341
      INTEGER  LENGTH(18)                                                ! rwb #518  06341
      INTEGER  BEG_IT, END_IT, REM, EQD, QNN, ENDADD, LVAR
      INTEGER  IDATE     ! year-month-day from initial read of CONTROL for error reporting

      LOGICAL  FLAG12, CDROM, SPECIAL, GOT_NONSPEC                       ! dtb #509  06299
      LOGICAl  L_ElevWarn, L_MissElevWarn

c     Increase array sizes for FIELD and FLD to include GA1-GA6 cloud cover
c     codes.                                                             ! rwb #518  06341
      CHARACTER*15 FIELD(18)                                             ! rwb #518  06341
      CHARACTER*3  FLD(18)                                               ! rwb #518  06341

      CHARACTER CONTROL*60, MANDATORY*45, VARIABLE*1000,ADDITIONAL*1000  ! rwb #518  06341

      CHARACTER*17 VFMT

      CHARACTER*5   RPTTYPE
      CHARACTER*4   ID_CALL4
      CHARACTER*3   ID_CALL3
      character*6   cSfcObsUSAF
      character*1   cDataSource

      integer save_ws, save_wd, save_tmp
      
C     Data initialization
      DATA LOC/'RDISHD'/ , PATH/'SURFACE   '/

      DATA ISHOB /35*0/
      DATA FLD /'AA1', 'AG1', 'GD1', 'GD2', 'GD3', 'GD4', 'GD5',
     &          'GD6', 'GF1', 'MA1', 'MW1', 'MW2',
     &          'GA1', 'GA2', 'GA3', 'GA4', 'GA5', 'GA6'/                ! rwb #518  06341

      DATA LENGTH /11, 7, 6*4, 8, 15, 6, 6, 6*5/                         ! rwb #518  06341

c --- initialize GOT_NONSPEC variable
      GOT_NONSPEC = .FALSE.

c     Initialize initial READ Variables                                  ! rwb #518  06341
      CONTROL    = ' '                                                   ! rwb #518  06341
      MANDATORY  = ' '                                                   ! rwb #518  06341
      VARIABLE   = ' '                                                   ! rwb #518  06341
      ADDITIONAL = ' '                                                   ! rwb #518  06341

      ID_CALL4 = ' '
      ID_CALL3 = ' '
      csfcobsCALL4 = ' '
      csfcobsCALL3 = ' '
      cDataSource  = ' '
      
      ELEV_DIFF = 0.0
      L_ElevWarn = .FALSE.
      L_MissElevWarn = .FALSE.
      
c     Set missing value indicators for ISHD variables

      MISS( 6) =   9999      !  6  Global radiation
      MISS( 7) =    999      !  7  Net radiation
      MISS( 8) =     99      !  8  Total cloud
      MISS( 9) =     99      !  9  Opaque cloud
      MISS(10) =  +9999      ! 10  Dry-bulb
      MISS(11) =  +9999      ! 11  Dew point

      MISS(13) =  99999      ! 13  Station pressure
      MISS(14) =    999      ! 14  Wind direction
      MISS(15) =   9999      ! 15  Wind speed
      MISS(16) = 999999      ! 16  Visibility
      MISS(17) =  99999      ! 17  Ceiling
      MISS(18) =   9999      ! 18  Precipation type
      MISS(19) =   9999      ! 19  Precipitation amount

c     Assign missing codes to temporary variables (ws, wd, temp)
      save_ws  = SFQA(51,2)
      save_wd  = SFQA(50,2)
      save_tmp = SFQA(46,2)
      
      MISS(20) =     99      ! 20  ASKY - Total sky from summation codes
      MISS(21) =     99      ! 21  ACHT - Ceiling from summation codes

      MISS(22) =  99999      ! 22  Sea-level pressure
      MISS(23) =  99999      ! 23  Altimeter setting

      MISS(24) =      9      ! 24  Sky cover summation level 1 (GD1)
      MISS(25) =      9      ! 25  Sky cover summation level 2 (GD2)
      MISS(26) =      9      ! 26  Sky cover summation level 3 (GD3)
      MISS(27) =      9      ! 27  Sky cover summation level 4 (GD4)
      MISS(28) =      9      ! 28  Sky cover summation level 5 (GD5)
      MISS(29) =      9      ! 29  Sky cover summation level 6 (GD6)

      MISS(30) =     99      ! 30  Sky cover level 1 (GA1)               ! rwb #518  06341
      MISS(31) =     99      ! 31  Sky cover level 2 (GA2)               ! rwb #518  06341
      MISS(32) =     99      ! 32  Sky cover level 3 (GA3)               ! rwb #518  06341
      MISS(33) =     99      ! 33  Sky cover level 4 (GA4)               ! rwb #518  06341
      MISS(34) =     99      ! 34  Sky cover level 5 (GA5)               ! rwb #518  06341
      MISS(35) =     99      ! 35  Sky cover level 6 (GA6)               ! rwb #518  06341

c     Initialize ISHOB placeholders to missing
      DO I = 6, 35                                                       ! rwb #518  06341
         ISHOB(I) = MISS(I)
      ENDDO

c     Open file for writing discarded ISHD records
      OPEN(UNIT=DEV61,FILE='Discarded_ISHD_Records.dat',
     &     STATUS='REPLACE',FORM='FORMATTED',ERR=399)
      WRITE(DEV61,10) SFLOC
  10  FORMAT(1X,'Discarded ISHD Records for Surface Station: ',A8/)

      ISHOB(18) = 0          !  Precipitation type is initialized to none

      KDISCARD = 0           !  Counter for # of discarded records
      NRECS    = 0           !  Counter for # of records read
      KOUNT    = 0           !  Counter for # of records extracted and written
      NCALM    = 0           !  Counter for # of calms
      NVAR     = 0           !  Counter for # of variable winds          ! dtb #505 06299
      NADJASOS = 0           !  Counter for # of hours wind speed
                             !   should be adjusted for ASOS truncation
      LHR     = 24
      LAST_HR = 24
      LAST_MN =  0

c     Assign default window of 30 minutes before the hour (accept data
c     from 30 minutes past the hour up to the hour, inclusive).

      INC = 30                                                           ! dtb #517  06340

c     Read the first record to determine if latitude and longitude are included;
c     we check for latitude by looking for a '+' or '-' sign in column 29.  If
c     latitude/longitude is not included then we assume that the record was
c     extracted from the ISHD CD-ROM.

C --- Branch to statement 300 is used to read next record
  300 CONTINUE
  
      READ(DEV20,         '(A60)', END=380, ERR=390) CONTROL             ! dtb #514  06299
      READ(CONTROL(5:10),  '(I6)')   ID_USAF
      READ(CONTROL(11:15), '(I5)')   ID_WBAN
      READ(CONTROL(16:23), '(I8)')   IDATE
      READ(CONTROL(28:28), '(A1)')   cDataSource

      IF( CONTROL(29:29).NE.'+' .AND. CONTROL(29:29).NE.'-' )THEN
C        Data retrieved from CD:
C          Latitude, longitude, station elevation NOT PRESENT in the data
         CDROM = .TRUE.
         VFMT(1:12) =  '(A33, A45, A'
C        Use assigned station elevation from the LOCATION keyword OR
C        set it to 0.0 if the elevation was not on the LOCATION keyword
         IF( GOTPWELEV(3) )THEN
            ZMSL2USE = PWELEV(3)
         ELSE 
            ZMSL2USE = 0.0
         ENDIF

C ---    Read station Call IDs for comparison with ASOS commission date list
         READ(CONTROL(34:38), '(A4)')   ID_CALL4     
         READ(CONTROL(39:42), '(A3)')   ID_CALL3

C ---    Station elevation is NOT included in the data file;
C        issue warning for first instance of this condition
         IF( GOTPWELEV(3) .AND. .NOT.L_MissElevWarn )THEN
C ---       User specified station elevation on the LOCATION keyword,
C           include qualifier to warning message
            MESS =  BLNK80
            WRITE (MESS, 6200) ID_WBAN
 6200       FORMAT(' SURFACE station elevation in ISHD file is ',
     &              'missing for WBAN # ',I6)
            CALL ERRHDL(1,PATH,'W46',LOC,MESS)
            MESS =  BLNK80
            WRITE (MESS, 6250) PWELEV(3)
 6250       FORMAT('  However, user specified elevation on ',
     &               'LOCATION keyword = ',F8.2,'m')
            CALL ERRHDL(2,PATH,'W46',LOC,MESS)
         ELSEIF( .NOT.GOTPWELEV(3) .AND. .NOT.L_MissElevWarn )THEN
C ---       NO station elevation specified on the LOCATION keyword,
C           and no elevation from data file
            MESS =  BLNK80
            WRITE (MESS, 6200) ID_WBAN
            CALL ERRHDL(1,PATH,'W46',LOC,MESS)
            MESS =  BLNK80
            WRITE (MESS, 6260)
 6260       FORMAT('  SURFACE elevation also missing from LOCATION ',
     &               'keyword; recommend specifying elevation.')
            CALL ERRHDL(2,PATH,'W46',LOC,MESS)
         ENDIF
         L_MissElevWarn = .TRUE.

      ELSE
C        Data obtained from other source:
C          Latitude, longitude, station elevation PRESENT in the data file
         CDROM = .FALSE.
         VFMT(1:12) =  '(A60, A45, A'
         READ(CONTROL(29:34), '(F6.3)') STA_LAT
         READ(CONTROL(35:41), '(F7.3)') STA_LON
         READ(CONTROL(47:51), '(F5.0)') ZMSL2USE                         !  Read station elevation (m).
         
C ---    Read station Call IDs for comparison with ASOS commission date list
         READ(CONTROL(52:55), '(A4)')   ID_CALL4     
         READ(CONTROL(52:54), '(A3)')   ID_CALL3

C        Check for "missing" elevation of +9999m for files expanded      ! dtb #525  06341
C        using NCDC utility program; use assigned value from optional
C        ELEV field on the LOCATION keyword record if elevation is not
C        present.
         IF( ZMSL2USE .GT. 9000. .AND. .NOT.L_MissElevWarn )THEN
C ---       Station elevation is in the data file, but is missing;
C           issue warning for first instance of this condition
            IF( GOTPWELEV(3) )THEN
C ---          User specified station elevation on the LOCATION keyword,
C              include qualifier to warning message
               MESS =  BLNK80
               WRITE (MESS, 6200) ID_WBAN
               CALL ERRHDL(1,PATH,'W46',LOC,MESS)
               MESS =  BLNK80
               WRITE (MESS, 6250) PWELEV(3)
               CALL ERRHDL(2,PATH,'W46',LOC,MESS)
            ELSE
C ---          NO station elevation specified on the LOCATION keyword,
C              and no elevation from data file
               MESS =  BLNK80
               WRITE (MESS, 6200) ID_WBAN
               CALL ERRHDL(1,PATH,'W46',LOC,MESS)
               MESS =  BLNK80
               WRITE (MESS, 6260)
               CALL ERRHDL(2,PATH,'W46',LOC,MESS)
            ENDIF
            L_MissElevWarn = .TRUE.

         ELSE
C ---       Station elevation is available from data file; if
C           user also specified elevation on the LOCATION keyword
C           check for consistency with data file (within +/- 2%):
            GOTSFELEV = .TRUE.
            IF( GOTPWELEV(3) .AND. .NOT.L_ElevWarn )THEN
C              Compare elevation from file with user-specifed
C              elevation for first instance
               ELEV_DIFF = PWELEV(3) - ZMSL2USE
               IF( ABS(ELEV_DIFF) .GT. 0.02*ZMSL2USE .AND. 
     &                                           .NOT.L_ElevWarn )THEN
                  L_ElevWarn = .TRUE.
                  MESS =  BLNK80
                  WRITE (MESS, 6300) PWELEV(3), ZMSL2USE
 6300             FORMAT(' SURFACE elevation on LOCATION ',
     &                    'keyword (',F8.2,') differs from ',
     &                    'elevation in ISHD file (',F8.2,').')
                  CALL ERRHDL(IDATE,PATH,'W46',LOC,MESS)
               ENDIF

            ELSEIF( .NOT.GOTPWELEV(3) .AND. .NOT.L_ElevWarn )THEN
C              Issue separate warning message if elevation is available
C              in the file, but not specified on the LOCATION keyword
               MESS =  BLNK80
               WRITE (MESS, 6400) ZMSL2USE
6400           FORMAT(' NO SURFACE elevation on LOCATION ',
     &                 'keyword, but elevation from ISHD ',
     &                 'file = ',F8.1,' m;')
               CALL ERRHDL(IDATE,PATH,'W46',LOC,MESS)
               MESS =  BLNK80
               WRITE (MESS, 6450) 
6450           FORMAT('  Recommend specifying elevation on ',
     &                  'the LOCATION keyword')
               CALL ERRHDL(2,PATH,'W46',LOC,MESS)
               L_ElevWarn = .TRUE.
           
            ENDIF

         ENDIF                                                          ! dtb #525  06341
      ENDIF

      BACKSPACE DEV20

C ----------------------------------------------------------------------
C     Store WBAN and CALL ID From First Observation
C     This assignment should only be performed once, after
C        the first observation is read.
C     For all subsequent observations, compare WBAN from current obs
C        to the WBAN stored from the first obs, but allow for missing
C        codes which may occur for some records.
C ----------------------------------------------------------------------

      IF( .NOT. GotSfcObsWBAN .AND. ID_WBAN .NE. 99999 )THEN         ! mec 1/2010
C ---    Non-missing WBAN found, save for comparison with subsequent records

         iSfcObsWBAN = ID_WBAN                                       ! mec 1/2010
         WRITE(cSfcObsWBAN,'(I5)') iSfcObsWBAN                       ! mec 1/2010

         GotSfcObsWBAN = .TRUE.                                      ! mec 1/2010

C ---    Compare WBAN from file with user-specified location ID
         IF( INDEX(SFLOC, cSfcObsWBAN) .EQ. 0 )THEN
C           User-specifed WBAN doesn't match file WBAN; 
C           check for match with WMO/USAF number
            WRITE(cSfcObsUSAF,'(I6)') ID_USAF                        ! mec 1/2010
            IF( INDEX(SFLOC, cSfcObsUSAF) .GT. 0 )THEN
C              Match found with WMO/USAF number in file
C              Issue warning message since WMO number will not
C              work for check of anemometer height in Stage 3.
               MESS =  BLNK80
               ECODE = 'W47'
               WRITE(MESS,430) SFLOC, ID_WBAN
               CALL ERRHDL(IDATE,PATH,ECODE,LOC,MESS)
            ELSE
C              Issue warning message station ID on LOCATION keyword
C              does not match WBAN or WMO/USAF number in data file.
               MESS =  BLNK80
               ECODE = 'W47'
               WRITE(MESS,431) SFLOC, ID_WBAN
               CALL ERRHDL(IDATE,PATH,ECODE,LOC,MESS)
            ENDIF
         ENDIF

      ELSEIF( GotSfcObsWBAN )THEN                                   ! mec 1/2010
C ---    Non-missing WBAN already found, compare to subsequent records

         IF( ID_WBAN .NE. iSfcObsWBAN .AND.
     &       ID_WBAN .NE. 99999 )THEN                               ! mec 1/2010
C-----------------------------------------------------------------------
C           There is a mismatch between the WBAN of the first observation 
C           and some subsequent observation.  Issue a warning message.

            WRITE( MESS, 425 )iSfcObsWBAN, ID_WBAN                  ! mec 1/2010
            CALL ERRHDL( IDATE, PATH, 'W47',LOC,MESS )              ! mec 1/2010

         ENDIF                                                      ! mec 1/2010

      ENDIF                                                         ! mec 1/2010

C --- Check for CALL ID's since WBAN may be missing for some ASOS sites
      IF( .NOT. GotSfcObsCALL .AND. ID_CALL4 .NE. '9999' )THEN      ! mec 1/2010

         IF( ID_CALL4(4:4) .NE. ' ' )THEN
            csfcobsCALL4 = ID_CALL4                                 ! mec 1/2010

            GotSfcObsCALL = .TRUE.                                  ! mec 1/2010
         ENDIF

      ENDIF

      IF( .NOT. GotSfcObsCALL .AND. ID_CALL3 .NE. '   ' .AND.
     &                              ID_CALL3 .NE. '999' )THEN       ! mec 1/2010

         csfcobsCALL3 = ID_CALL3                                    ! mec 1/2010

         GotSfcObsCALL = .TRUE.                                     ! mec 1/2010

      ENDIF                                                         ! mec 1/2010
      

      VFMT(13:16) = '    '   !  Placeholder for length of variable data section
      VFMT(17:17) = ')'

      IF( CDROM )THEN                                               ! dtb #514  06299
         READ (DEV20, '(A33)', END=380, ERR=390) CONTROL
      ELSE
         READ (DEV20, '(A60)', END=380, ERR=390) CONTROL
      ENDIF

      NRECS = NRECS + 1

      READ(CONTROL(18:19), '(I2)') SF2YR
      READ(CONTROL(16:19), '(I4)') SF4YR
      READ(CONTROL(20:21), '(I2)') SFGMO
      READ(CONTROL(22:23), '(I2)') SFGDY

      SFGYR = SF4YR
      IYYYYMMDD = SF4YR*10000 + SFGMO*100 + SFGDY

      SFJDY = JULIAN(SF2YR, SFGMO, SFGDY)
      CALL CHROND (PATH, SF2YR, SFJDY, SFCDY)

c     Read and process date and time information

      READ(CONTROL(24:25), '(I2)') SFGHR
      READ(CONTROL(26:27), '(I2)') SFGMN
      THIS_MN = SFGMN
      THIS_HR = SFGHR

c     Check for observation window
      IF( SFGMN .NE. 0 )THEN          ! An observation on the hour is always accepted
         IF( SFGMN .GE. (60-INC) )THEN
            CONTINUE
         ELSE                         !  Read another record
            GOTO 300
         ENDIF
      ENDIF

      IF( SFGMN .GE. (60-INC) )THEN    ! Increment hour
         SFGHR = SFGHR + 1
         SFGMN = 01
      ENDIF

C     Search for ASOS commission date based on iSfcObsWBAN or 
C     csfcobsCALL4(or3), if available
      IF( .not. SrchCommDate .AND. 
     &         (GotSfcObsWBAN .OR. GotSfcObsCALL) )THEN
         CALL FNDCOMDT(PATH)
      ENDIF 

      IF( SFCDY .LT. SFDAY1 )THEN        !  Date is prior to extract window
         GO TO 300

      ELSEIF( SFCDY.GE.SFDAY1 .AND. SFCDY.LE.SFDAY2 )THEN  !  Date is in the extract window
C        Date is in the extract window
         LDAYC = SFCDY
         IF( SFGHR .EQ. 1 ) WRITE(*,610) SFGMO, SFGDY, SF4YR

C        determine if current record is ASOS based on the obs date vs. comm date
         CALL ASOSREC(SF2YR,SFGMO,SFGDY,SFGHR,SFJDY,SFLST)

C-----------------------------------------------------------------------            
C        Compare data source flag (cDataSource = 6 or 7 for ASOS/AWOS)
C        with results of ASOS commission date search.  Data will be 
C        treated as ASOS if the date is after the commission date,
C        OR if the DataSource flag in the data file indicates ASOS. 
C        However, warnings will be issued if the DataSource flag
C        indicates ASOS but the commission date does not.
C        No messages generated if the DataSource flag does NOT indicate
C        ASOS, but the commission date does, since DataSource flag in
C        ISHD data does not routinely indicate ASOS.
C-----------------------------------------------------------------------            
         IF( (cDataSource .EQ. '6' .OR. cDataSource .EQ. '7') .AND.
     &                                            ISASOS1 .NE. 'A' )THEN
C           Issue warning message for ASOS flag mismatch, wording depends
C           on whether commission date has been found
            IF( .NOT. GotCommDate )THEN
               MESS = BLNK80
               ECODE = 'W49'            
               WRITE(MESS,1030) THIS_HR
 1030          FORMAT(' Data flag = ASOS but CommissionDate not found;',
     &                ' ASOS data flag used for hour: ',I2)
               CALL ERRHDL(IYYYYMMDD,PATH,ECODE,LOC,MESS)
C              Set ISASOS1 to 'A' to indicate that this is an ASOS observation
               ISASOS1 = 'A'
            ELSE
               MESS = BLNK80
               ECODE = 'W49'            
               WRITE(MESS,1040) THIS_HR
 1040          FORMAT(' Data flag = ASOS prior to the Commission Date;',
     &                ' ASOS data flag used for hour: ',I2)
               CALL ERRHDL(IYYYYMMDD,PATH,ECODE,LOC,MESS)
C              Set ISASOS1 to 'A' to indicate that this is an ASOS observation
               ISASOS1 = 'A'
            ENDIF
         ENDIF

C ---    Set ISASOS1 = 'A' if ISHD_ASOS option was set; this option overrides
C        all other indicators
         IF( ISHD_ASOS )THEN
            ISASOS1 = 'A'
         ENDIF

c        The report type is indicated in fields 29-33 in the ISHD CD-ROM records.
c        This information is indicated in fields 42-46 in records obtained prior
c        to the creation of the ISHD CD-ROM.

c        The report types are as follows (more may be added later):

c             FM-12  SYNOP report from a fixed land station
c             FM-13  SHIP report from a sea station
c             FM-14  SYNOP MOBIL report from a mobil land station
c             FM-15  METAR aviation routine weather report
c             FM-16  SPECI aviation selected special weather report
c             FM-18  BUOY report of a buoy observation
c             SAO    Airways report (includes record specials)
c             SAOSP  Airways special report (excluding record specials)
c             AERO   Aerological report
c             AUTO   Report from an automatic station
c             SY-AE  Synoptic and aero merged report
c             SY-SA  Synoptic and airways merged report
c             SY-MT  Synoptic and METAR merged report
c             SY-AU  Synoptic and auto merged report
c             SA-AU  Airways and auto merged report
c             S-S-A  Synoptic, airways, and auto merged report
c             BOGUS  Bogus report
c             SMARS  Supplementary airways station report

c        We process selected report types as follows:

c        Initialize SPECIAL for each record
         SPECIAL = .FALSE.                                               ! dtb #509  06299

         IF( CDROM )THEN
            READ(CONTROL(29:33), '(A5)') RPTTYPE
         ELSE
            READ(CONTROL(42:46), '(A5)') RPTTYPE
         ENDIF

         IF( SFGHR .NE. LHR )THEN                                        ! rwb #518  06341
c           Initialize GOT_NONSPEC for new hour to track non-special obs ! rwb #518  06341
            GOT_NONSPEC = .FALSE.                                        ! rwb #518  06341
         ENDIF                                                           ! rwb #518  06341

         IF(RPTTYPE.NE.'FM-12' .AND. RPTTYPE.NE.'FM-13' .AND.
     &      RPTTYPE.NE.'FM-14' .AND. RPTTYPE.NE.'FM-15' .AND.
     &      RPTTYPE.NE.'FM-16' .AND. RPTTYPE.NE.'FM-18' .AND.
     &      RPTTYPE.NE.'SAO  ' .AND. RPTTYPE.NE.'SAOSP' .AND.
     &      RPTTYPE.NE.'AERO ' .AND. RPTTYPE.NE.'AUTO ' .AND.
     &      RPTTYPE.NE.'SY-AE' .AND. RPTTYPE.NE.'SY-SA' .AND.
     &      RPTTYPE.NE.'SY-MT' .AND. RPTTYPE.NE.'SY-AU' .AND.
     &      RPTTYPE.NE.'SA-AU' .AND. RPTTYPE.NE.'S-S-A' .AND.
     &      RPTTYPE.NE.'SMARS')THEN

c           Record type not recognized or not supported - Skip to next record
c           Several record types are not supported, including BOGUS, SOD, SOM,
c           WBO, COOPD, COOPS, PCP15, PCP60, CRN05, and CRN15

            GOTO 300                    !  Read another record

         ELSE
c           Identify if record is for "special" observation.  Also identify
c           if a non-special observation is being processed.  We will only
c           use special observations if they are the only obs available
c           for the hour.

c           Assign logical variables to track whether special or "regular"
c           observations are being processed.

            IF( (RPTTYPE .EQ. 'SAOSP') .OR. (RPTTYPE .EQ. 'FM-16') )THEN  ! dtb #509  06299
               SPECIAL = .TRUE.                                          ! dtb #509  06299
            ELSE
               GOT_NONSPEC = .TRUE.
            ENDIF                                                        ! dtb #509  06299


c           Get here only if this is a record that we want to process

            VFMT(13:16) = CONTROL(1:4)   !  Assign length of variable data section
            READ(VFMT(13:16),'(I4)')LVAR

            BACKSPACE DEV20
            READ(DEV20,VFMT, END=380, ERR=390)CONTROL, MANDATORY,
     &           VARIABLE

c           The variable data section may include additional data fields
c           'ADD' remarks 'REM', element quality data fields 'EQD', and/or
c           'QNN', which is not clearly documented as a delimeter.
c           We are only interested in the additional data fields.  First
c           we locate the beginning of the 'REM' and 'EQD' fields.

            REM = INDEX(VARIABLE, 'REM')
            EQD = INDEX(VARIABLE, 'EQD')
            QNN = INDEX(VARIABLE, 'QNN')

            IF( REM .GT. 0 )THEN
               ENDADD = REM-1

            ELSEIF( EQD .GT. 0 )THEN
               ENDADD = EQD-1

            ELSEIF( QNN .GT. 0 )THEN
               ENDADD = QNN-1

            ELSE
               ENDADD = LVAR

            ENDIF

c           Assign only the useful portion of VARIABLE to ADDITIONAL     ! rwb #518  06341
            ADDITIONAL = VARIABLE(1:ENDADD)                              ! rwb #518  06341

c           Search the additional character string for the data to keep.
c           Data are identified by 3-character strings as follows.

c             Code     Discription                              Length

c              AA101   1-hour precipitation                     [5+6]
c              AG1     Bogus precipitation/Wx indicator         [3+4]
c              GA1     Sky cover level 1                        [3+2]
c              GA2     Sky cover level 2                        [3+2]
c              GA3     Sky cover level 3                        [3+2]
c              GA4     Sky cover level 4                        [3+2]
c              GA5     Sky cover level 5                        [3+2]
c              GA6     Sky cover level 6                        [3+2]
c              GD1     Sky cover summation level 1              [3+1]
c              GD2     Sky cover summation level 2              [3+1]
c              GD3     Sky cover summation level 3              [3+1]
c              GD4     Sky cover summation level 4              [3+1]
c              GD5     Sky cover summation level 5              [3+1]
c              GD6     Sky cover summation level 6              [3+1]
c              GF1     Sky condition (total & opaque)           [3+5]
c              MA1     Atmospheric pressure                     [3+12]
c              MW1     Present weather                          [3+3]
c              MW2     Present weather                          [3+3]

c           Initialize the additional fields to missing

            FIELD(1)  = 'AA101000095    '        ! Initialize 1-hour precipitation to none
            FIELD(2)  = 'AG19999        '        ! Bogus precipitation/Wx indicator

            FIELD(3)  = 'GD19999+99999  '        ! Sky cover summation level 1
            FIELD(4)  = 'GD29           '        ! Sky cover summation level 2
            FIELD(5)  = 'GD39           '        ! Sky cover summation level 3
            FIELD(6)  = 'GD49           '        ! Sky cover summation level 4
            FIELD(7)  = 'GD59           '        ! Sky cover summation level 5
            FIELD(8)  = 'GD69           '        ! Sky cover summation level 6

            FIELD(9)  = 'GF199999       '        ! Sky condition (total & opaque)
            FIELD(10) = 'MA1999999999999'        ! Atmospheric pressure

c           Initialize present weather to none
            FIELD(11) = 'MW1001         '
            FIELD(12) = 'MW1002         '

c           Add GA1-GA6 cloud layer codes as third tier, after GF1 and
c           GD1-GD6.  GA1-GA6 codes may not be important for primary stations,
c           but could increase data recovery for secondary stations      ! rwb #518  06341
            FIELD(13) = 'GA199          '                                ! rwb #518  06341
            FIELD(14) = 'GA299          '                                ! rwb #518  06341
            FIELD(15) = 'GA399          '                                ! rwb #518  06341
            FIELD(16) = 'GA499          '                                ! rwb #518  06341
            FIELD(17) = 'GA599          '                                ! rwb #518  06341
            FIELD(18) = 'GA699          '                                ! rwb #518  06341
            IBOG      =  9

            BEG_IT = INDEX(ADDITIONAL, 'AA101')
            IF( BEG_IT .GT. 0 )THEN
               END_IT = BEG_IT + LENGTH(1) -1
               FIELD(1) = ADDITIONAL(BEG_IT:END_IT)
            ENDIF


            DO I = 3, 18                                                 ! rwb #518  06341
               BEG_IT = INDEX(ADDITIONAL, FLD(I))
               IF( BEG_IT .GT. 0 )THEN
                  END_IT = BEG_IT + LENGTH(I) -1
                  FIELD(I) = ADDITIONAL(BEG_IT:END_IT)
               ENDIF
            ENDDO

c           Map the extracted data to ISHOB

            READ(MANDATORY( 1: 3), '(I3)')ISHOB(14)    !  14  Wind direction
            READ(MANDATORY( 6: 9), '(I4)')ISHOB(15)    !  15  Wind speed
            READ(MANDATORY(11:15), '(I5)')ISHOB(17)    !  17  Ceiling
            READ(MANDATORY(19:24), '(I6)')ISHOB(16)    !  16  Visibility
            READ(MANDATORY(28:32), '(I5)')ISHOB(10)    !  10  Dry-bulb
            READ(MANDATORY(34:38), '(I5)')ISHOB(11)    !  11  Dew point
            READ(MANDATORY(40:44), '(I5)')ISHOB(22)    !  22  Sea level pressure

            READ(FIELD( 1)( 6: 9), '(I4)')ISHOB(19)    !  19  Precipitation

            READ(FIELD( 2)( 4: 4), '(I1)')IBOG

            READ(FIELD( 3)( 4: 4), '(I1)')ISHOB(24)     !  24  GD1 code
            READ(FIELD( 4)( 4: 4), '(I1)')ISHOB(25)     !  25  GD2 code
            READ(FIELD( 5)( 4: 4), '(I1)')ISHOB(26)     !  26  GD3 code
            READ(FIELD( 6)( 4: 4), '(I1)')ISHOB(27)     !  27  GD4 code
            READ(FIELD( 7)( 4: 4), '(I1)')ISHOB(28)     !  28  GD5 code
            READ(FIELD( 8)( 4: 4), '(I1)')ISHOB(29)     !  29  GD6 code

            READ(FIELD( 9)( 4: 5), '(I2)')ISHOB( 8)     !   8  Total cloud
            READ(FIELD( 9)( 6: 7), '(I2)')ISHOB( 9)     !   9  Opaque cloud

            READ(FIELD(10)( 4: 8), '(I5)')ISHOB(23)     !  23  Altimeter setting
            READ(FIELD(10)(10:14), '(I5)')ISHOB(13)     !  13  Station pressure
            READ(FIELD(11)( 4: 5), '(I2)')MW1           !      Present weather (MW1)
            READ(FIELD(12)( 4: 5), '(I2)')MW2           !      Present weather (MW2)

c           Only read GA codes if needed, below

c           Process the ISHOB variables as necessary

C-----------------------------------------------------------------------
C                          TOTAL SKY COVER
C-----------------------------------------------------------------------
c           Total Sky   [08] - Based on GF1 codes
            IF( ISHOB(8) .EQ. MISS(8) )THEN      ! Missing total sky
               ISHOB(8) = 99
            ELSE                                 ! Convert to tenths from oktas
               IF( ISHOB(8) .LE. 1 )THEN           ! 0 to 1 oktas, round to tenths
                  ITEMP = ISHOB(8)
               ELSEIF( ISHOB(8) .LE. 5 )THEN       ! 2 to 5 oktas, add 1
                  ITEMP = ISHOB(8) + 1
               ELSEIF( ISHOB(8) .LE. 8 )THEN       ! 6 to 8 oktas, add 2
                  ITEMP = ISHOB(8) + 2
               ELSEIF( ISHOB(8) .LE. 10 )THEN      ! Obscured, assign 10/10
                  ITEMP = 10
               ELSE                                                      ! rwb #518  06341
                  ITEMP = 99                                             ! rwb #518  06341
               ENDIF

               ISHOB(8) = ITEMP
            ENDIF

C-----------------------------------------------------------------------
C                          OPAQUE SKY COVER
C-----------------------------------------------------------------------
c           Opaque Sky   [09] - Based on GF1 codes
            IF( ISHOB(9) .EQ. MISS(9) )THEN      ! Missing opaque sky
               ISHOB(9) = 99
            ELSE                                 ! Convert to tenths from oktas
               IF( ISHOB(9) .LE. 1 )THEN           ! 0 to 1 oktas, round to tenths
                  ITEMP = ISHOB(9)
               ELSEIF( ISHOB(9) .LE. 5 )THEN       ! 2 to 5 oktas, add 1
                  ITEMP = ISHOB(9) + 1
               ELSEIF( ISHOB(9) .LE. 8 )THEN       ! 6 to 8 oktas, add 2
                  ITEMP = ISHOB(9) + 2
               ELSEIF( ISHOB(9) .LE. 10 )THEN      ! Obscured, assign 10/10
                  ITEMP = 10
               ELSE                                                      ! rwb #518  06341
                  ITEMP = 99                                             ! rwb #518  06341
               ENDIF

               ISHOB(9) = ITEMP
            ENDIF

            IF( ISHOB(8).EQ.MISS(8) .AND. ISHOB(9).EQ.MISS(9) )THEN      ! rwb #518  06341
c              Both total and opaque sky cover are missing.
c              Process ASOS cloud layers: First try GD1-GD6, then try GA1-GA6

c              Process the sky cover summation codes (GD1-GD6) beginning with
c              level 6 (i.e., the highest level).

c              Initialize ASKY to missing code (= 99)
               ASKY = MISS(20)

               IF( ISHOB(29) .NE. MISS(29) )THEN
                  ASKY = ISHOB(29)
               ELSEIF( ISHOB(28) .NE. MISS(28) )THEN
                  ASKY = ISHOB(28)
               ELSEIF( ISHOB(27) .NE. MISS(27) )THEN
                  ASKY = ISHOB(27)
               ELSEIF( ISHOB(26) .NE. MISS(26) )THEN
                  ASKY = ISHOB(26)
               ELSEIF( ISHOB(25) .NE. MISS(25) )THEN
                  ASKY = ISHOB(25)
               ELSEIF( ISHOB(24) .NE. MISS(24) )THEN
                  ASKY = ISHOB(24)
               ELSE
                  ASKY = MISS(20)
               ENDIF

               IF( ASKY .NE. MISS(20) )THEN
C                 Process cloud codes for GD1-GD6 cloud summation layer

                  IF( ASKY .GE. 4 )THEN        ! Overcast or Obscured
                     ISHOB(20) = 10
                  ELSEIF( ASKY .EQ. 3 )THEN    ! Broken (5/8 to 7/8)
                     ISHOB(20) =  7
                  ELSEIF( ASKY .EQ. 2 )THEN    ! Scattered (3/8 to 4/8)
                     ISHOB(20) =  4
                  ELSEIF( ASKY .EQ. 1 )THEN    ! Few (1/8 to 2/8)
                     ISHOB(20) =  2
                  ELSE                       ! Clear
                     ISHOB(20) =  0
                  ENDIF

               ELSE
C                 Total/Opaque (GF1) and Cloud Summation (GD1-GD6) are missing;
C                 Now look for Cloud Cover by Layer (GA1-GA6), use largest value

C                 First read GA1-GA6 codes from FIELD array
                  READ(FIELD(13)( 4: 5), '(I2)')ISHOB(30)     !  30  GA1 code
                  READ(FIELD(14)( 4: 5), '(I2)')ISHOB(31)     !  31  GA2 code
                  READ(FIELD(15)( 4: 5), '(I2)')ISHOB(32)     !  32  GA3 code
                  READ(FIELD(16)( 4: 5), '(I2)')ISHOB(33)     !  33  GA4 code
                  READ(FIELD(17)( 4: 5), '(I2)')ISHOB(34)     !  34  GA5 code
                  READ(FIELD(18)( 4: 5), '(I2)')ISHOB(35)     !  35  GA6 code

                  IF( ISHOB(35) .NE. MISS(35) )THEN
                     ASKY = ISHOB(35)
                  ENDIF
                  IF( ISHOB(34) .NE. MISS(34) )THEN
                     ASKY = MAX(ASKY,ISHOB(34))
                  ENDIF
                  IF( ISHOB(33) .NE. MISS(33) )THEN
                     ASKY = MAX(ASKY,ISHOB(33))
                  ENDIF
                  IF( ISHOB(32) .NE. MISS(32) )THEN
                     ASKY = MAX(ASKY,ISHOB(32))
                  ENDIF
                  IF( ISHOB(31) .NE. MISS(31) )THEN
                     ASKY = MAX(ASKY,ISHOB(31))
                  ENDIF
                  IF( ISHOB(30) .NE. MISS(30) )THEN
                     ASKY = MAX(ASKY,ISHOB(30))
                  ENDIF

C                 Assign maximum GA1-GA6 code in oktas to ASKY in tenths
                  IF( ASKY .LE. 1 )THEN
                     ISHOB(20) = ASKY            ! 0 to 1 oktas, round to tenths
                  ELSEIF( ASKY .LE. 5 )THEN
                     ISHOB(20) = ASKY + 1        ! 2 to 5 oktas, add 1
                  ELSEIF( ASKY .LE. 8 )THEN
                     ISHOB(20) = ASKY + 2        ! 6 to 8 oktas, add 2
                  ELSEIF( ASKY .LE. 10 )THEN
                     ISHOB(20) = 10              ! Obscured, assign 10/10
                  ELSE
                     ISHOB(20) = 99
                  ENDIF

               ENDIF

            ELSE
c              Assign missing code to ASOS clouds for this hour
               ISHOB(20) = MISS(20)

            ENDIF

c           FLAG12 is set to .TRUE. to indicate that we have sufficient data to
c           calculate relative humidity.  If either dry bulb or dew point is
c           missing, FLAG12 is reset to .FALSE.

            FLAG12 = .TRUE.

C-----------------------------------------------------------------------
C                       DRY BULB TEMPERATURE
C-----------------------------------------------------------------------
c           Dry Bulb    [10]
            IF( ISHOB(10) .EQ. MISS(10) )THEN      !  Missing dry bulb
               ISHOB(10) = SFQA(46,2)
               FLAG12 = .FALSE.
            ENDIF

C-----------------------------------------------------------------------
C                       DEW POINT TEMPERATURE
C-----------------------------------------------------------------------
c           Dew Point   [11]
            IF( ISHOB(11) .EQ. MISS(11) )THEN      !  Missing dew point
               ISHOB(11) = SFQA(48,2)
               FLAG12 = .FALSE.
            ENDIF

C-----------------------------------------------------------------------
C                         RELATIVE HUMIDITY
C-----------------------------------------------------------------------
c           RELATIVE HUMIDITY  [12]  ISHOB does not report RH
            IF( FLAG12 )THEN                       !  Compute RH
               DB = FLOAT(ISHOB(10))/10.
               DP = FLOAT(ISHOB(11))/10.
               CALL AERMET_HUMID(DB, DP, RH)
               ISHOB(12) = IFIX(RH)
            ELSE
               ISHOB(12) = SFQA(49,2)
            ENDIF

C-----------------------------------------------------------------------
C                           WIND DIRECTION
C-----------------------------------------------------------------------
c           Wind Direction     [14]
            IF( ISHOB(14) .EQ. MISS(14) )THEN      !  Missing  wind direction

               IF( (MANDATORY(4:5).EQ.'59' .AND. ISHOB(15).EQ.0) .OR.     ! dtb #505  06299
     &              MANDATORY(5:5).EQ. 'C' )THEN    ! Valid calm          ! dtb #505  06299

c                  mandatory(4:5) is a data quality code
c
c                  note: data acceptance is currently based on missing codes
c
c                  mandatory(5:5) is a type code as follows:
c
c                              C = calm     (WD and WS both coded as 0)
c                              V = variable (missing WD with non-missing WS)
c                              9 = missing
c                              N = normal

                  ISHOB(14) = 0                    ! Wind dir is reset to zero for calm
                  NCALM = NCALM + 1
               ELSEIF( MANDATORY(5:5).EQ.'V' .OR.                        ! rwb #518 06341
     &                 (ISHOB(14).EQ.MISS(14) .AND.                      ! rwb #518 06341
     &                  ISHOB(15).NE.MISS(15)) )THEN   ! Varible WD      ! dtb #505 06299
                  ISHOB(14) = 9990                                       ! rwb #518 06341
                  NVAR = NVAR + 1                                        ! rwb #518 06341
               ELSE
                  ISHOB(14) = 9990                                       ! dtb #304 04245
               ENDIF
            ENDIF

C-----------------------------------------------------------------------
C                             WIND SPEED
c           If an ASOS commissioning date is available, then wind speed
c           is to be adjusted (in Stage 3) for the ASOS truncation
c           problem if the date on the data record is after the
c           commissoning date.
C-----------------------------------------------------------------------
c           Wind Speed         [15]
            IF( ISHOB(15) .EQ. MISS(15) )THEN      ! Wind speed missing
               ISHOB(15) = SFQA(51,2)              ! Missing value for wind speed is 999
            ENDIF


C-----------------------------------------------------------------------
C                             VISIBILITY
C-----------------------------------------------------------------------
c           Visibility         [16]
            IF( ISHOB(16) .EQ. MISS(16) )THEN      !  Missing visibility
               ISHOB(16) = SFQA(45,2)
            ELSE
c              (convert from meters to km * 10)
               ISHOB(16) = (ISHOB(16)/1000)*10
            ENDIF

C-----------------------------------------------------------------------
C                           CEILING HEIGHT
C-----------------------------------------------------------------------
c           Ceiling            [17]
            IF( ISHOB(17) .EQ. MISS(17) )THEN       !  Missing ceiling
               ISHOB(17) = SFQA(33,2)
            ELSEIF( ISHOB(17) .EQ. 22000 )THEN        !  Unlimited
               ISHOB(17) = 300
            ELSE
               TEMP = FLOAT(ISHOB(17))/1000.        ! Convert to km
               ISHOB(17) = IFIX(TEMP*10.)
            ENDIF

C-----------------------------------------------------------------------
C                           PRECIPITATION
C-----------------------------------------------------------------------
c           Precipitation Type [18]
c           Determine the type of precipitation (liquid or frozen ), if any.

            CALL ISHWX(MW1, MW2, ISHOB(18))

c           ISHOB(18) returns an integer with values as follows:

c            0 = none
c            1 = liquid precipitation
c            2 = frozen precipitation
c            3 = mixed
C            8 = undetermined
c            9 = missing

c           Precipitation Amount [19]
            IF( ISHOB(19) .EQ. MISS(19) )THEN      !  Missing precipitation
               ISHOB(19) = -9
            ELSE
c              (convert from mm*10 to mm*100)
               ISHOB(19) = ISHOB(19)*10                                  ! dtb #305 04356
            ENDIF

c           Adjust precip type code if no measurable precipitation occurred
            IF( ISHOB(19) .LE. 0 )THEN
               ISHOB(18) = 0                                             ! rwb #526  06341
            ENDIF

C-----------------------------------------------------------------------
C                         ALTIMETER SETTING
C-----------------------------------------------------------------------
c           Altimeter Setting   [23]
            IF( ISHOB(23) .EQ. MISS(23) )THEN      !  Missing altimeter setting
               ISHOB(23) = 99999
            ELSEIF( ISHOB(13).EQ.MISS(13) .AND. 
     &              ISHOB(22).EQ.MISS(22) )THEN    !  Non-missing altimeter setting
                                                   !  with missing SLVP and SFSP
C ---          Compute station pressure using station elevation from
C              ISHD data file based on altimeter setting, using
C              same conversion used for sea-level pressure (which
C              is "close enough" for our purposes.
               SLVP = FLOAT(ISHOB(23))/10. 
               CALL GEO(2, ZMSL2USE, 290., SFSP, SLVP)
               ISHOB(13) = NINT(SFSP)*10 
            ENDIF

C-----------------------------------------------------------------------
C                              PRESSURE
C-----------------------------------------------------------------------
            IF( ISHOB(13).EQ.MISS(13) .AND. ISHOB(22).EQ.MISS(22) )THEN   ! Missing station and sea-level pressure
C              Station pressure and sea-level pressure are missing;
C              If station elevation is available from ISHD file, but
C              not specified on the LOCATION keyword, then apply 
C              subsitutions for missing station pressure here;
C              otherwise potential substitutions for missing station
C              pressure are handled in subroutine SUBST during 
C              Stage 3.  First preference is to substitute based on
C              adjusting sea-level pressure for station elevation, 
C              but substitution based on station pressure and standard
C              atmosphere will be used if sea-level pressure is also
C              missing.

               IF( GOTSFELEV .AND. .NOT.GOTPWELEV(3) )THEN
C ---             Compute station pressure using station elevation from
C                 ISHD data file based on standard atmosphere                                   
                  XRD1 = 1013.25 *
     &                  (1.0 - ((6.5e-3/288.15) * ZMSL2USE))**5.255
                  ISHOB(13) = NINT(XRD1*10)
               ELSE
                  ISHOB(13) = SFQA(32,2)
               ENDIF
               
               ISHOB(22) = SFQA(31,2)

            ELSEIF( ISHOB(13) .EQ. MISS(13) )THEN
C              Station pressure is missing but sea-level pressure
C              is available; if station elevation is available 
C              from ISHD file, but not specified on the LOCATION 
C              keyword, then apply substitutions for missing 
C              station pressure based on sea-level pressure and
C              elevation.

               IF( GOTSFELEV .AND. .NOT.GOTPWELEV(3) )THEN
C ---             Compute station pressure using station elevation from
C                 ISHD data file based on sea-level pressure
                  SLVP = FLOAT(ISHOB(22))/10. 
                  CALL GEO(2, ZMSL2USE, 290., SFSP, SLVP)
                  ISHOB(13) = NINT(SFSP)*10 
               ELSE
                  ISHOB(13) = SFQA(32,2)
               ENDIF

            ELSEIF( ISHOB(22) .EQ. MISS(22) )THEN

               IF( GOTSFELEV .AND. .NOT.GOTPWELEV(3) )THEN
C ---             Compute sea-level pressure using station elevation from
C                 ISHD data file based on station pressure
                  SFSP = FLOAT(ISHOB(13))/10.
                  CALL GEO(1, ZMSL2USE, 290., SFSP, SLVP)
                  ISHOB(22) = NINT(SLVP)*10
               ELSE
                  ISHOB(22) = SFQA(31,2)
               ENDIF

            ENDIF

C           Conversion to local time was performed in subr.ASOSREC

            IF( SFGHR .EQ. 0 )THEN
               SFCDY = SFCDY - 1
               CALL HR0024( SF2YR, SFJDY, SFGHR )
               CALL GREG( SF2YR, SFJDY, SFGMO, SFGDY )
               CALL CHROND (PATH, SF2YR, SFJDY, SFCDY)
            ENDIF
C-----------------------------------------------------------------------
C                       SPECIAL OBS PROCESSING
C-----------------------------------------------------------------------
            ISHOB(1) = SF4YR
            ISHOB(2) = SFGMO
            ISHOB(3) = SFGDY
            ISHOB(4) = SFGHR
            ISHOB(5) = SFGMN

            IF( SFGHR.EQ.LHR .AND. KOUNT.NE.0 )THEN
c-----
c             Multiple records for the same observation hour. Decide 
c              whether to overwrite data from previous records for this
c              hour.  We want to keep the last non-special observation 
c              up to (and including) the hour (min=00).  We will only 
c              use special observations if there are no "regular" 
c              observations available for that hour.
c-----

               IF( SPECIAL .AND. GOT_NONSPEC )THEN
c                 Do not use a special obs to replace regular obs        ! dtb #509  06299
                  GO TO 300                                              ! dtb #509  06299

c              Check data to avoid overwriting good data with missing data
               ELSEIF( GOT_NONSPEC .AND. 
     &            ((ISHOB(15) .EQ. SFQA(51,2) .AND. 
     &                             save_ws .NE. SFQA(51,2)) .OR.
     &             (ISHOB(14) .EQ. SFQA(50,2) .AND. 
     &                             save_wd .NE. SFQA(50,2)) .OR.
     &             (ISHOB(10) .EQ. SFQA(46,2) .AND. 
     &                             save_tmp .NE. SFQA(46,2))) )THEN
                  WRITE(DEV61,*) ' Skipping NONSPEC record for ', 
     &                 ishob(1), ishob(2), ishob(3), ishob(4), ishob(5)
                  GO TO 300

               ELSE                                                      ! dtb #509  06299
c                 It's ok to overwrite previous records for this hour.  
c                 However the overwritten data will be output to the 
c                 message file before proceeding.  
c                 Read the last hourly record and write it to the error/
c                 message file.  Then position the pointer at the 
c                 beginning of the last record for overwrite.

                  BACKSPACE DEV21                                        ! dtb #506  06299
                  BACKSPACE DEV21                                        ! dtb #506  06299

                  READ(DEV21, '(A80 / A80)') BUF80(1), BUF80(2)          ! dtb #506  06299
                  WRITE(DEV61, 1022)LAST_HR, LAST_MN, BUF80(1), BUF80(2) ! dtb #506  06299
 1022             FORMAT (1X, 'Discarded records for ', I2.2, ':', I2.2,
     &                        ' follow:  ' / A80 / A80)

                  BACKSPACE DEV21                                        ! dtb #506  06299
                  BACKSPACE DEV21                                        ! dtb #506  06299
                  KDISCARD = KDISCARD + 1                                ! dtb #506  06299
                  KOUNT = KOUNT - 1                                      ! dtb #506  06299
               ENDIF                                                     ! dtb #509  06299
            ENDIF

C           Remap to the SFOBS array and write a record to the extract file.

            SFOBS(1,30) = ISHOB(19)  !  PRCP  Precipitation
            SFOBS(1,31) = ISHOB(22)  !  SLVP  Sea level pressure
            SFOBS(1,32) = ISHOB(13)  !  PRES  Station pressure
            SFOBS(1,33) = ISHOB(17)  !  CLHT  Ceiling height

                                     !  TSKC  Total and opaque sky

            SFOBS(1,34) = ISHOB(8)*100 + ISHOB(9)

            SFOBS(1,42) = ISHOB(18)     !  PWTH  Present Weather

            SFOBS(1,43) = ISHOB(20)     !  ASKY  Total sky from
                                        !        summation codes

            SFOBS(1,45) = ISHOB(16)     !  HZVS  Visibility
            SFOBS(1,46) = ISHOB(10)     !  TMPD  Dry bulb
            SFOBS(1,48) = ISHOB(11)     !  DPTP  Dew point
            SFOBS(1,49) = ISHOB(12)     !  RHUM  Relative humidity
            SFOBS(1,50) = ISHOB(14)/10  !  WDIR  Wind direction (tens of degrees)
            SFOBS(1,51) = ISHOB(15)     !  WSPD  Wind speed

C           Save wind speed, direction and temperature for comparison to subsequent records
            save_ws = sfobs(1,51)
            save_wd = sfobs(1,50)
            save_tmp = sfobs(1,46)
            
C     NOTE: The time and date are converted from Greenwich to local time
C           in subr.SFQATM
            WRITE(DEV21,2110) SF2YR,SFGMO,SFGDY,SFGHR,
     &                       (SFOBS(1,IVBL),IVBL=30,51), ISASOS1

 2110       FORMAT(1X,4I2.2,4(1X,I5),6(1X,I5.5),/
     &             8X,5(1X,I5.5),7(1X,I5),2X,A1)

            CALL FLSFC(1)                              ! Reset SFOBS to missing

            KOUNT    = KOUNT + 1
            LHR      = SFGHR
            LAST_HR  = THIS_HR
            LAST_MN  = THIS_MN
            GOTO 300
         ENDIF


      ELSEIF( SFCDY .GT. SFDAY2 )THEN                 ! End of extract window

         MESS = BLNK40
         WRITE( MESS, 470 ) NRECS                     ! 470: End of extract window
         CALL ERRHDL( IDATE, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 472 ) KOUNT                     ! 472: Report number of extracted records
         CALL ERRHDL( 0, PATH, 'I49',LOC,MESS )
         SFBFST = -1

         MESS = BLNK40
         WRITE(MESS,474) KDISCARD                     ! 474: Report number of discarded records
         CALL ERRHDL(0,PATH,'W48',LOC,MESS)

         MESS = BLNK40
         WRITE(MESS,482)NCALM                         ! 482: Report # of records flagged as calm
         CALL ERRHDL(0,PATH,'W48',LOC,MESS)

         MESS = BLNK40
         WRITE(MESS,484)NVAR                          ! 484: Report # of records flagged as variable
         CALL ERRHDL(0,PATH,'W48',LOC,MESS)

      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     Processing continues here on an end of file.

  380 SFBFST = -1
      MESS = BLNK40
      WRITE(MESS,480) NRECS                           ! 480: End of file after record #
      CALL ERRHDL(IDATE,PATH,'I48',LOC,MESS)

      WRITE( MESS, 472 ) KOUNT                        ! 472: Report number of extracted records
      CALL ERRHDL( 0, PATH, 'I48',LOC,MESS )
      SFBFST = -1

      MESS = BLNK40
      WRITE(MESS,474) KDISCARD                        ! 474: Report number of discarded records
      CALL ERRHDL(0,PATH,'W48',LOC,MESS)

      MESS = BLNK40
      WRITE(MESS,482)NCALM                            ! 482: Report # of records flagged as calm
      CALL ERRHDL(0,PATH,'W48',LOC,MESS)

      MESS = BLNK40
      WRITE(MESS,484)NVAR                             ! 484: Report # ofrecords flagged as variable
      CALL ERRHDL(0,PATH,'W48',LOC,MESS)


      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurs reading a record.

  390 MESS =  BLNK80
      WRITE(MESS,490) NRECS + 1
      CALL ERRHDL(IDATE,PATH,'E42',LOC,MESS)
      ISTAT = 1                                   !  Abnormal end
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurrs decoding
C     a header record.

  392 CONTINUE
      ISTAT = 1                                   !  Abnormal end
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurrs decoding
C     a data record.

  394 CONTINUE
      ISTAT = 1                                   !  Abnormal end
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when there is an error opening the
C     file that contains discarded ISHD records.

  399 CONTINUE
      WRITE( MESS, '(A)' ) 'Error opening discarded ISHD record file!'
      CALL ERRHDL( 0, PATH, 'W48',LOC,MESS )          ! mec 1/2010
      RETURN                                          ! mec 1/2010

C-----------------------------------------------------------------------

  100 FORMAT(A6,1X,I4,4I2,1X,I3,2(1X,I4),1X,I3,3X,I2,17X,I6,2X,
     &       4(1X,I3),10X,2(1X,I4),1X,I5,7X,I4)

c     Note the length of the placeholder for error messages (MESS)
c     needs to be increased from 48 to 60 (suggested) for format
c     statements 460.

  425 FORMAT(' Site ID mismatch within SURFACE data file: ',I5,' / ',I5)
  430 FORMAT(' UserID (',A8,') matches WMO, not WBAN (',I6,').',
     &       ' Use WBAN to allow Stage 3 anem hgt check.')
  431 FORMAT(' Site ID mismatch between data file and LOCATION ',
     &        'keyword. LOCATION = ',A8,'; SURFACE file = ',I5)
  452 FORMAT(' Precipitation/Wx mismatch,  Bogus code is:', I2)
  460 FORMAT(' First record on Julian day: ', I3, ' is for hour ', I2)

  470 FORMAT(' End of extract window after record:       ', I6)
  472 FORMAT(' The # of extracted records is:            ', I6)
  474 FORMAT(' The # of discarded records is:            ', I6,
     &       '.  See ''Discarded_ISHD_Records.dat'' file.')

  480 FORMAT(' End of file after record:                 ', I6)
  482 FORMAT(' The # of records flagged as calm is:      ', I6)
  484 FORMAT(' The # of records flagged as variable is:  ', I6)
  490 FORMAT(' Error reading record number:              ', I6)

  496 FORMAT(' Error attempting to backspace met file at: ', I6)

  610 FORMAT('+  Stage 1: Extracting surface data for ',
     &             'month-day-year ', 2(I2.2,:'-'),I4)

      END

