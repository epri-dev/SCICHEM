      SUBROUTINE RDHUSW( KOUNT, ISTAT )
C=====================================================================**
C     PURPOSE:    Fills in the Appropriate Met Variable Arrays with raw
c                 data from HUSWO,  This code assumes that the data file
c                 which was created with the HUSWO CD-ROM software is in
C                 the metric format.


C     PROGRAMMER: Desmond Bailey

C     VERSION DATES    Created:    December 1999
C                      Revised:    March 2002
C                      Revised:    July 2006, R.W. Brode
C                      Revised:    May 2008, MACTEC
C                      Revised:    Sept 2009, MACTEC

C     Revision History:
C        05/29/08: MACTEC E&C
C                - Added code to compute station pressure using the
C                  standard atmosphere and the elevation on the LOCATION
C                  keyword, if present, or 0.0 (returns sea level pressure)
C                  Note that sea level pressure is not available in HUSWO
C                - Changed the initialization of the HUSWO variables
C                  from 1..20 to 3..19
C                - Set sky cover and ceiling height for non-ASOS cases
C
C        09/31/09: MACTEC E&C
C                - Write flag at end of each hour's 2nd record to
C                  indicate if the data are ASOS (1) or not (0).
C
C       1/2010 - MACTEC
C         - Added capability to determine ASOS commission date for
C           for treatment of truncated ASOS wind speeds
C         - Check for consistency between internal ASOS flag
C           and ASOS commissioning date
C         - Checks for internal WBAN consistency. WBAN in control file
C           should = WBAN in NWS surface file which should contain only one
C         - Check format valid date range to determine if data was likely 
C           reformatted


C     INPUTS:     Surface Data from HUSWO in character format

C     OUTPUTS:    Hourly surface observations in Stage 1 format

C     CALLED FROM: SFEXT
C-----------------------------------------------------------------------

C     Variable Declarations


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

      INTEGER  I, J, JDYHR, IWX, JJ, ISKY, ICHT, IVBL, ID_WBAN, ISTAT
      INTEGER  SF2YR, SF4YR, SFJDY, SFCDY, IYYYYMMDD,IYYMMDD      ! mec 1/2010
      INTEGER  SAVYR, SAVMO, SAVDY, SAVHR
      INTEGER  JULIAN, LDAYC, NRECS
      INTEGER  STAID
      INTEGER  HUSOB(20)
      INTEGER  HIMISS(20), AMISS(20)
      INTEGER  IASOSCNT

      REAL     HRMISS(20), VTEMP
      LOGICAL  ASOS

      CHARACTER*8   AHUS(20), PWTH
      CHARACTER*1   AFLAG, PFLAG

C     Data initialization
      DATA LOC/'RDHUSW'/ , PATH/'SURFACE   '/
      DATA IASOSCNT/0/

C---- Initialize 4-char and 3-char CALL IDs
      csfcobsCALL4 = ' '
      csfcobsCALL3 = ' '

C     set valid date range for format
      SfcVldStart = 19900101                                      ! mec 1/2010
      SfcVldEnd = 19951231                                        ! mec 1/2010

      NRECS = 0
      
      SAVYR = 0
      SAVMO = 0
      SAVDY = 0
      SAVHR = 0

C     Counters used for reporting summary statistics

c     IWORK1(71)   Number of ASOS records ---- not used

c     The following counters track processing of present weather codes:

c     IWORK1(72)   Number of records with 'none' reported for present weather
c     IWORK1(73)   Number of records reporting liquid precipitation
c     IWORK1(74)   Number of records reporting frozen precipitation
c     IWORK1(75)   Number of records with both liquid and frozen precipitation
c     IWORK1(76)   Number of records with an undetermined present Wx code

c     The following counters track processing of precipitation flags:

c     IWORK1(77)   Number of records with valid non-zero precipitation
c     IWORK1(78)   Number of records with an accumulation (A) flag
c     IWORK1(79)   Number of records with a deleted (D), or missing (M) flag

      DO I = 71, 79
         IWORK1(I) = 0
      ENDDO


C     Data formats:
C     The first two numbered fields in a HUSWO data record are
c     reserved for the station id and date.  The data begin with
c     the third numbered field as follows:

c          03   Global Radiation
c          04   Direct Radiation
C          05   Total sky cover
C          06   Opaque sky cover
C          07   Dry bulb temperature
C          08   Dew point temperature
C          09   Relative humidity
C          10   Station pressure
C          11   Wind direction
C          12   Wind speed
C          13   Visibility
C          14   Ceiling height
C          15   Present weather
C          16   ASOS cloud cover layer 1
C          17   ASOS cloud cover layer 2
C          18   ASOS cloud cover layer 3
C          19   Precipitation
C          20   Snow depth

C          Note that HUSWO has no sea level pressure; it is calculated
C          below once station pressure is obtained

c     Set missing value indicators for HUSWO data
c     Integer variables

      HIMISS( 3) =   9999      !     3  Global Rad          INSO
      HIMISS( 4) =   9999      !     4  Direct Rad          US01
      HIMISS( 5) =     99      !     5  Total Cloud         TS
      HIMISS( 6) =     99      !     6  Opaque Cloud          KC


      HIMISS( 9) =    999      !     9  Rel Humidity        RHUM
      HIMISS(10) =   9999      !    10  Sta. Pressure       PRES
      HIMISS(11) =    999      !    11  Wind Direction      WDIR


      HIMISS(14) =  99999      !    14  Ceiling             CLHT
C     HIMISS(15) =             !    15  Present Wx          PWTH
      HIMISS(16) =  99999      !    16  ASOS-1              ASO1
      HIMISS(17) =  99999      !    17  ASOS-2              ASO2
      HIMISS(18) =  99999      !    18  ASOS-3              ASO3
C     HIMISS(19) =             !    19  Hourly precip       PRCP


c     Set missing value indicators for HUSWO data
C     Real variables

      HRMISS( 7) =  999.9      !     7  Dry Bulb            TMPD
      HRMISS( 8) =  999.9      !     8  Dew Point           DPTP



      HRMISS(12) =   99.9      !    12  Wind Speed          WSPD
      HRMISS(13) = 9999.9      !    13  Visibility          HZVS


c     Initialize AMISS to missing value flags used internally in AERMET

      AMISS( 3) =   9999      !     3  Global Rad          GRAD
      AMISS( 4) =   9999      !     4  Direct Rad          DRAD
      AMISS( 5) =     99      !     5  Total Cloud         TS
      AMISS( 6) =     99      !     6  Opaque Cloud          KC
      AMISS( 7) =    999      !     7  Dry Bulb            DRYB
      AMISS( 8) =    999      !     8  Dew Point           DEWP
      AMISS( 9) =    999      !     9  Rel Humidity        RHUM
      AMISS(10) =  99999      !    10  Sta. Pressure       PRES
      AMISS(11) =    999      !    11  Wind Direction      WDIR
      AMISS(12) =    999      !    12  Wind Speed          WSPD
      AMISS(13) =  99999      !    13  Visibility          HZVS
      AMISS(14) =    999      !    14  Ceiling             CLHT
      AMISS(15) =      9      !    15  Present Wx          PWTH
      AMISS(16) =  99999      !    16  ASOS-1              ASO1
      AMISS(17) =  99999      !    17  ASOS-2              ASO2
      AMISS(18) =  99999      !    18  ASOS-3              ASO3
      AMISS(19) =     -9      !    19  Hourly precip       PRCP

  300 CONTINUE

      ASOS = .FALSE.
      BUF140 = BLN140


c     Initialize HUSOB array to missing value flags used in AERMET

      DO J = 3, 19
         HUSOB(J) = AMISS(J)
      ENDDO


C     Read one record from the HUSWO data file.  The array AHUS(i) may
c     contain up to 18 values (HUSWO id numbers 3 - 20).

      READ(DEV20, '(140A)', END=380,ERR=390) BUF140

      NRECS = NRECS + 1

c     Assign station ID, date, and time
      READ(BUF140( 1: 5), '(I5)') STAID
      READ(BUF140( 6: 6), '(A1)') AFLAG
      READ(BUF140( 9:10), '(I2)') SF2YR
      READ(BUF140( 7:10), '(I4)') SF4YR
      READ(BUF140(11:12), '(I2)') SFGMO
      READ(BUF140(13:14), '(I2)') SFGDY
      READ(BUF140(15:16), '(I2)') SFGHR    
      
      IYYYYMMDD = SF4YR*10000 + SFGMO*100 + SFGDY
      IYYMMDD   = SF2YR*10000 + SFGMO*100 + SFGDY

C --- Check for problems with date sequence
      IF( SAVYR .GT. SF4YR )THEN
         MESS =  BLNK80
         ECODE = 'E44'
         WRITE(MESS,2000) SF4YR,SFGMO,SFGDY,SFGHR
2000     FORMAT(' HUSWO data is out-of-sequence at ',
     &                             '(Yr,Mn,Dy,Hr): ',4I4 )
         CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
         ISTAT = 1
         RETURN
         
      ELSEIF( SAVYR .EQ. SF4YR .AND. SAVMO .GT. SFGMO )THEN
         MESS =  BLNK80
         ECODE = 'E44'
         WRITE(MESS,2000) SF4YR,SFGMO,SFGDY,SFGHR
         CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
         ISTAT = 1
         RETURN
      
      ELSEIF( SAVYR .EQ. SF4YR .AND. SAVMO .EQ. SFGMO .AND.
     &                               SAVDY .GT. SFGDY )THEN
         MESS =  BLNK80
         ECODE = 'E44'
         WRITE(MESS,2000) SF4YR,SFGMO,SFGDY,SFGHR
         CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
         ISTAT = 1
         RETURN

      ELSEIF( SAVYR .EQ. SF4YR .AND. SAVMO .EQ. SFGMO .AND.
     &        SAVDY .EQ. SFGDY .AND. SAVHR .GE. SFGHR )THEN
         MESS =  BLNK80
         ECODE = 'E44'
         WRITE(MESS,2000) SF4YR,SFGMO,SFGDY,SFGHR
         CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
         ISTAT = 1
         RETURN

      ENDIF
      
      SAVYR = SF4YR
      SAVMO = SFGMO
      SAVDY = SFGDY
      SAVHR = SFGHR

C ----------------------------------------------------------------------
C     Check obs date against valid date range for HUSWO format
C     If date is outide of date range, set flag issue a warning.
C     Only needs to check until a true is returned. 
C ----------------------------------------------------------------------

      IF( .NOT. ReformattedSFC )THEN
         IF( (IYYYYMMDD .LT. SfcVldStart) .OR. 
     &       (IYYYYMMDD .GT. SfcVldEnd) )THEN
     
            ReformattedSFC = .TRUE. 

            MESS =  BLNK80 
            WRITE(MESS,6610) SFFMT, SfcVldStart, SfcVldEnd 
 6610       FORMAT(' SURFACE observations are outside range of valid ',
     &              'dates for ',A8,' format: ',I8,'-',I8 )
            CALL ERRHDL(0,PATH,'W40',LOC,MESS)
         
            MESS =  BLNK80 
            WRITE(MESS,'(A)')
     &        '  Cloud cover will be set to missing for ASOS records!'
            CALL ERRHDL(0,PATH,'W40',LOC,MESS)

         ENDIF
   
      ENDIF


C ----------------------------------------------------------------------
C     Store WBAN From First Observation
C     Compare WBAN from control file to first obs WBAN
C        No match - error, abort
C     This assignment and check should only be performed once, after
C        the first observation was read.
C     For all subsequent observations, compare WBAN from current obs
C        to the WBAN stored from the first obs.
C ----------------------------------------------------------------------

      IF( .NOT. GotSfcObsWBAN )THEN
   
         iSfcObsWBAN = STAID 
         WRITE(cSfcObsWBAN,'(I5)') iSfcObsWBAN
      
         GotSfcObsWBAN = .TRUE.

         IF( SFLOC(4:8) .NE. cSfcObsWBAN )THEN
            GO TO 396
         ENDIF
         
      ELSE

         IF( STAID .NE. iSfcObsWBAN )THEN
            GO TO 397
         ENDIF
         
      ENDIF

C     Search for ASOS commission date based on iSfcObsWBAN
      IF( .not. SrchCommDate )THEN
         CALL FNDCOMDT(PATH)
      ENDIF


      IF( NRECS .EQ. 1 )THEN
         CONTINUE
      ENDIF

      SFJDY = JULIAN(SF2YR, SFGMO, SFGDY)
      CALL CHROND (PATH, SF2YR, SFJDY, SFCDY)
      JDYHR = 100*SFJDY + SFGHR

      IF( SFCDY .LT. SFDAY1 )THEN
C        The date is prior to the extract window
         GO TO 300

      ELSEIF( SFCDY .GE. SFDAY1  .AND.  SFCDY .LE. SFDAY2 )THEN
C        Date is in the extract window
         LDAYC  = SFCDY

         IF( SFGHR .EQ. 1 ) WRITE(*,610) SFGMO, SFGDY, SF4YR

c        Process as necessary and map to the HUSOB array


         READ(BUF140, HUSFMT, ERR=390)(AHUS(I), I = 1, NVARS)

         IWX = 0               !  Initialize present weather code


C        Loop Over the HUSWO Variables
         DO J = 3, NVARS

c        IDVAR contains the variable id numbers from the HUSWO header record

c           Process variables in the order they occur in the HUSWO record.
c           We begin with global radiation (IDVAR(J)=3).

            IF( IDVAR(J) .EQ. 3 )THEN                     !  3  Global Radiation
               READ(AHUS(J), '(I4)') HUSOB( 3)
               IF( HUSOB( 3) .EQ. HIMISS( 3))THEN
                  HUSOB( 3) = AMISS(3)
               ELSE
                  CONTINUE
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  4 )THEN                !  4  Direct Radiation
               READ(AHUS(J), '(I4)') HUSOB( 4)
               IF( HUSOB( 4) .EQ. HIMISS( 4))THEN
                  HUSOB( 4) = AMISS(4)
               ELSE
                  CONTINUE
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  5 )THEN                !  5  Total Cloud Cover
               READ(AHUS(J), '(I2)') HUSOB( 5)
               IF( HUSOB( 5) .EQ. HIMISS( 5))THEN
                  HUSOB( 5) = AMISS(5)
               ELSE
                  CONTINUE
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  6 )THEN                !  6  Opaque Cloud Cover
               READ(AHUS(J), '(I2)') HUSOB( 6)
               IF( HUSOB( 6) .EQ. HIMISS( 6))THEN
                  HUSOB(6) = AMISS(6)
               ELSE
                  CONTINUE
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  7 )THEN                !  7  Dry Bulb Temperature
               READ(AHUS(J), '(F5.0)') VTEMP
               IF( VTEMP .EQ. HRMISS( 7) )THEN
                  HUSOB( 7) = AMISS(7)
               ELSE
                  HUSOB( 7) = NINT(VTEMP * 10)
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  8 )THEN                !  8  Dew Point
               READ(AHUS(J), '(F5.0)') VTEMP
               IF( VTEMP .EQ. HRMISS( 8))THEN
                  HUSOB( 8) = AMISS(8)
               ELSE
                  HUSOB( 8) = NINT(VTEMP * 10)
               ENDIF

            ELSEIF( IDVAR(J) .EQ.  9 )THEN                !  9  Relative Humidity
               READ(AHUS(J), '(I3)') HUSOB( 9)
               IF( HUSOB( 9) .EQ. HIMISS( 9))THEN
                  HUSOB( 9) = AMISS(9)
               ELSE
                  CONTINUE
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 10 )THEN                ! 10  Station Pressure
               READ(AHUS(J), '(I4)') HUSOB(10)
               IF( HUSOB(10) .EQ. HIMISS(10))THEN
C                 Station pressure is missing; assign missing code to SFOBS array element
                  SFOBS(1,32) = SFQA(32,2)
               ELSE
                  HUSOB(10) = HUSOB(10)*10
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 11 )THEN                ! 11  Wind Direction
               READ(AHUS(J), '(I3)') HUSOB(11)
               IF( HUSOB(11) .EQ. HIMISS(11))THEN
                  HUSOB(11) = AMISS(11)
               ELSE
                  HUSOB(11) = HUSOB(11)/10
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 12 )THEN                ! 12  Wind Speed
               READ(AHUS(J), '(F4.0)') VTEMP
               IF( VTEMP .EQ. HRMISS(12))THEN
                  HUSOB(12) = AMISS(12)
               ELSE
                  HUSOB(12) = NINT(VTEMP * 10)
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 13 )THEN                ! 13  Visibility
               READ(AHUS(J), '(F6.0)') VTEMP
               IF( VTEMP .EQ. HRMISS(13))THEN
                  HUSOB(13) = AMISS(13)
               ELSE
                  HUSOB(13) = NINT(VTEMP * 10)
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 14 )THEN                ! 14  Ceiling
               READ(AHUS(J), '(I5)') HUSOB(14)
               IF( HUSOB(14) .EQ. HIMISS(14))THEN
                  HUSOB(14) = AMISS(14)
               ELSEIF( HUSOB(14) .EQ. 77777 )THEN  !  Unlimited
                  HUSOB(14) = 300
               ELSE          !  Convert from meters to km * 10
                  HUSOB(14) = NINT(HUSOB(14)/100.)
               ENDIF


            ELSEIF( IDVAR(J) .EQ. 15 )THEN                 ! 15  Present Weather
               READ(AHUS(J), '(A8)') PWTH
               CALL HUSWX(PWTH, IWX)
               HUSOB(15) = IWX
               IF( IWX .LE. 3)THEN
                  JJ = IWX + 72
                  IWORK1(JJ) = IWORK1(JJ) + 1
               ELSEIF( IWX.GT.3 .AND. IWX .LT.9)THEN
                  IWORK1(76) = IWORK1(76) + 1
               ENDIF


            ELSEIF( IDVAR(J).EQ.16 .AND. AFLAG.EQ.'A')THEN  !  16  ASOS Layer 1
               ASOS = .TRUE.
               READ(AHUS(J), '(I5)') HUSOB(16)
               IF( HUSOB(16) .EQ. HIMISS(16) )THEN
                  HUSOB(16) = AMISS(16)
               ENDIF

            ELSEIF( IDVAR(J).EQ.17 .AND. AFLAG.EQ.'A')THEN  !  17  ASOS Layer 2
               ASOS = .TRUE.
               READ(AHUS(J), '(I5)') HUSOB(17)
               IF( HUSOB(17) .EQ. HIMISS(17) )THEN
                  HUSOB(17) = AMISS(17)
               ENDIF

            ELSEIF( IDVAR(J).EQ.18 .AND. AFLAG.EQ.'A')THEN  !  18  ASOS Layer 3
               ASOS = .TRUE.
               READ(AHUS(J), '(I5)') HUSOB(18)
               IF( HUSOB(18) .EQ. HIMISS(18) )THEN
                  HUSOB(18) = AMISS(18)
               ENDIF

            ELSEIF( IDVAR(J) .EQ. 19 )THEN                  ! 19  Precipitation
               READ(AHUS(J)(4:4), '(A1)')PFLAG
               IF( PFLAG .EQ. ' ')THEN
                  READ(AHUS(J)(1:3), '(F4.0)')VTEMP
                  HUSOB(19) = NINT(VTEMP*100)
                  IF( VTEMP .GT. 0)THEN
                     IWORK1(77) = IWORK1(77) + 1
                  ENDIF
               ELSEIF( PFLAG .EQ. 'A')THEN
                  IWORK1(78) = IWORK1(78) + 1
               ELSEIF( PFLAG.EQ.'D' .OR. PFLAG.EQ.'M')THEN  !  Missing
                  HUSOB(19) = AMISS(19)
                  IWORK1(79) = IWORK1(79) + 1
               ENDIF
            ENDIF


         ENDDO                                             !  End Loop Over Variables

C        determine if the current record is ASOS based on the obs date
         CALL ASOSREC(SF2YR,SFGMO,SFGDY,SFGHR,SFJDY,SFLST)             ! mec 1/2010

C-----------------------------------------------------------------------            
C        Compare AFLAG from data for consistency with commission date.
C        Data will be treated as ASOS if the date is after the commission
C        date, OR if the AFLAG in the data file indicates ASOS. However,
C        warnings will be issued if there is a mismatch between the AFLAG
C        and the commission date.
C-----------------------------------------------------------------------
         IF( ISASOS1 .NE. 'A' .AND. AFLAG .EQ. 'A' )THEN 
C ---       Data flag indicates ASOS, but commission date does not; 
C           ASOS flag based on data file will be used.  However, 
C           issue warning message for ASOS flag mismatch, wording depends
C           on whether commission date has been found
            IASOSCNT = IASOSCNT + 1
            IF( IASOSCNT .LE. 24 )THEN
               MESS = BLNK80
               ECODE = 'W49'           
            ELSEIF( IASOSCNT .GT. 24 )THEN      
               MESS = BLNK80
               ECODE = 'I49'            
            ENDIF
C           Set ISASOS1 to 'A' to indicate that 
C           this is an ASOS observation
            ISASOS1 = 'A'
            ASOS = .TRUE.
            IF( .NOT. GotCommDate )THEN
               WRITE(MESS,1030) SFGHR
 1030          FORMAT(' Data flag = ASOS but CommissionDate not',
     &                ' found; ASOS data flag used for hour: ',I2)
               CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
            ELSE
               WRITE(MESS,1040) SFGHR
 1040          FORMAT(' Data flag = ASOS prior to the Commission',
     &                ' Date;  ASOS data flag used for hour: ',I2)
               CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
            ENDIF
            IF( IASOSCNT .EQ. 24 )THEN
               MESS = BLNK80
               ECODE = 'W49'            
               WRITE( MESS, 1050 )
 1050          FORMAT('  NOTE: Additional messages regarding ASOS ',
     &         'flag/CommDate mismatches included in message file.')
               CALL ERRHDL( IYYMMDD, PATH, ECODE,LOC,MESS )
            ENDIF
         ELSEIF( ISASOS1 .EQ. 'A' .AND. AFLAG .NE. 'A' )THEN
C ---       Data flag does NOT indicate ASOS, but commission date does;
C           Issue warning message for ASOS flag mismatch, but assume
C           ASOS observation
            IASOSCNT = IASOSCNT + 1
            IF( IASOSCNT .LE. 24 )THEN
               MESS = BLNK80
               ECODE = 'W49'           
            ELSEIF( IASOSCNT .GT. 24 )THEN      
               MESS = BLNK80
               ECODE = 'I49'            
            ENDIF
            WRITE(MESS,1060) SFGHR
 1060       FORMAT(' Data flag does NOT = ASOS but Commission',
     &             ' Date DOES; ASOS is assumed for hour: ',I2)
            CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
C           Set ISASOS1 to 'A' to indicate that this is an ASOS observation
            ISASOS1 = 'A'
            ASOS = .TRUE.
            IF( IASOSCNT .EQ. 24 )THEN
               MESS = BLNK80
               ECODE = 'W49'            
               WRITE( MESS, 1050 )
               CALL ERRHDL( IYYMMDD, PATH, ECODE,LOC,MESS )
            ENDIF
         ENDIF                                                     

C------------------------- End of ASOS Obs Tag--------------------------

         IF( ASOS .and. .NOT.ReformattedSFC )THEN
C ---       Call DOCLDS to process cloud cover and ceiling heights;
C           passing character array elements 16, 17, and 18,
C           but only if data period is not outside valide date range
C           for HUSWO data (1/1/1990 to 12/31/1995).
C           Pass ASOS cloud cover fields as character variables
            CALL DOCLDS(SF2YR,SFGMO,SFGDY,SFGHR,
     &                 AHUS(16),AHUS(17),AHUS(18),ISKY,ICHT)

         ELSE
C           Initialize values for non-ASOS cases (and for cases 'ASOS'
C           outside valid date range for HUSWO).
            ISKY = 99
            ICHT = 999
         ENDIF

C        Remap to the SFOBS array and write a record to the extract file.

         SFOBS(1,30) = HUSOB(19)  !  PRCP  Precipitation
         SFOBS(1,31) = SFQA(31,2) !  SLVP  Sea level pressure
         SFOBS(1,32) = HUSOB(10)  !  PRES  Station pressure
         SFOBS(1,33) = HUSOB(14)  !  CLHT  Ceiling height

         IF( .NOT.ReformattedSFC )THEN 
C                                 !  TSKC  Total and opaque sky cover
C                                          concatenated field
            SFOBS(1,34) = HUSOB(6) + HUSOB(5)*100
         ELSE
C                                 !  Set to missing for reformatted
C                                    data outside valid date range
            SFOBS(1,34) = 9999
         ENDIF

         SFOBS(1,42) = HUSOB(15)     !  PWTH  Present Weather
         SFOBS(1,43) = ISKY          !  ASKY  ASOS cloud cover
         SFOBS(1,44) = ICHT          !  ACHT  ASOS Ceiling
         SFOBS(1,45) = HUSOB(13)     !  HZVS  Visibility
         SFOBS(1,46) = HUSOB( 7)     !  TMPD  Dry bulb
         SFOBS(1,48) = HUSOB( 8)     !  DPTP  Dew point
         SFOBS(1,49) = HUSOB( 9)     !  RHUM  Relative humidity
         SFOBS(1,50) = HUSOB(11)     !  WDIR  Wind direction (tens of degrees)
         SFOBS(1,51) = HUSOB(12)     !  WSPD  Wind speed



         WRITE(DEV21,2110) SF2YR,SFGMO,SFGDY,SFGHR,
     &                     (SFOBS(1,IVBL),IVBL=30,51), ISASOS1
 2110    FORMAT(1X,4I2.2,4(1X,I5),6(1X,I5.5),/,
     &          8X,5(1X,I5.5),7(1X,I5),2X,A1)


         KOUNT = KOUNT + 1

c        Reset SFOBS to missing
         CALL FLSFC(1)

         GO TO 300

      ELSEIF( SFCDY .GT. SFDAY2 )THEN                     !  End of extract window

         MESS = BLNK40
         WRITE( MESS, 470 ) NRECS-1
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 472 ) IWORK1(72)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 473 ) IWORK1(73)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 474 ) IWORK1(74)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 475 ) IWORK1(75)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 476 ) IWORK1(76)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 477 ) IWORK1(77)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 478 ) IWORK1(78)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 479 ) IWORK1(79)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

      ENDIF

      RETURN


C-----------------------------------------------------------------------
C     Processing continues here on an end of file.

  380 MESS = BLNK40
      WRITE(MESS,480) NRECS
      CALL ERRHDL(NRECS,PATH,'I49',LOC,MESS)
      IF( KOUNT .LT. 1 )THEN
         MESS = BLNK40
         WRITE(MESS,485)
C----    No records were extracted; issue fatal error message
         CALL ERRHDL(NRECS,PATH,'E44',LOC,MESS)
         SFSTAT = 1
      ELSE

         MESS = BLNK40
         WRITE( MESS, 472 ) IWORK1(72)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 473 ) IWORK1(73)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 474 ) IWORK1(74)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 475 ) IWORK1(75)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 476 ) IWORK1(76)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 477 ) IWORK1(77)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 478 ) IWORK1(78)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

         MESS = BLNK40
         WRITE( MESS, 479 ) IWORK1(79)
         CALL ERRHDL( JDYHR, PATH, 'I49',LOC,MESS )

      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurrs reading a record.

  390 MESS = BLNK40
      WRITE(MESS,490) NRECS + 1
      CALL ERRHDL(NRECS,PATH,'E42',LOC,MESS)
      SFSTAT = -1
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurrs decoding
C     a header record.

  392 CONTINUE
      SFSTAT = -1
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here when an error occurrs decoding
C     a data record.

  394 CONTINUE
      SFSTAT = -1
      RETURN
      
C-----------------------------------------------------------------------
C     Processing continues here when there is a mismatch between the 
C     WBAN in the control file and the WBAN in the first observation
C     of the surface file. 

  396 WRITE( MESS, 420 ) SFLOC(4:8), cSfcObsWBAN
      CALL ERRHDL( 0, PATH, 'E47',LOC,MESS )
      ISTAT = 1
      RETURN
      
C-----------------------------------------------------------------------
C     Processing continues here when there is a mismatch between the 
C     WBAN of the first observation and some subsequent observation.

  397 WRITE( MESS, 425 )iSfcObsWBAN, ID_WBAN
      CALL ERRHDL( 0, PATH, 'E47',LOC,MESS )
      ISTAT = 1
      RETURN
            
C-----------------------------------------------------------------------

  420 FORMAT(' Site ID mismatch between data file and LOCATION ',
     &        'keyword. LOCATION = ',A5,'; SURFACE file = ',A5)
  425 FORMAT(' Site ID mismatch within SURFACE data file: ',I5,' / ',I5)
  460 FORMAT(' Invalid hourly label: ', I2, ' on Julian day: ', I3)

C             ....+....1....+....2....+....3....+....4....+....5....+....6
  470 FORMAT(' End of extract window after record:       ', I5)

  472 FORMAT(' The # of records with none reported for PWTH is:   ', I5)
  473 FORMAT(' The number of records with liquid precipitation is: ',I5)
  474 FORMAT(' The number of records with frozen precipitation is: ',I5)
  475 FORMAT(' The # of hours with both liquid and frozen precip is',I5)
  476 FORMAT(' The # of hours with an undetermined PWTH code is:  ', I5)

  477 FORMAT(' The number of hours with valid non-zero precip is: ', I5)
  478 FORMAT(' The number of hours with an accumulation flag is:  ', I5)
  479 FORMAT(' The # of hours with a deleted or missing flag is:  ', I5)

  480 FORMAT(' End of file after record:                 ', I5)
  485 FORMAT(' No records extracted!  Check extract dates.')
  490 FORMAT(' Error reading record number:              ', I5)

  610 FORMAT('+  Stage 1: Extracting surface data for ',
     &             'month-day-year ', 2(I2.2,:'-'),I4)


c     The following is the format for the proposed mapping.

 2112 FORMAT(1X, I4, 4I3, I8, 5I6, 2I4.2, 14I6)

      END

