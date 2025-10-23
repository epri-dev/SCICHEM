      SUBROUTINE D3280L(ISTAT,JJJ)
C=====================================================================**
C     D3280L Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decode the TD3280 surface data and put it into the
C               required format.  Note that the TD3280 data are element-
C               oriented (up to 24 values for an element (surface
C               variable)) rather than hour-oriented (all elements for
C               a single hour).
C
C     Called by: GETSFC
C
C     Arguments:
C       ISTAT    Output  Program status of the decode
C       IOST20   Output  Operating system status of the decode
C
C     Revision history:
C        11/30/94 (PES)
C          - Made into a separate subroutine from an ENTRY point
C
C        12/14/00 (PES)
C          - Restructured code to eliminate a computed GOTO; removed
C            the processing of the 'PRCP' element since there is no
C            such element in TD-3280
C
C        03/05/02 (EPA revision dtb #120 02064)
C          - Restructured code to process ASOS variable names for wind direction
C            and wind speed 'WND2', and for sky condition and layer height
C            'ALC1' ... 'ALC6'.
C
C        04/30/08 (MACTEC)
C          - Added code to compute station pressure using the
C            standard atmosphere and the elevation on the LOCATION
C            keyword, if present, or 0.0 (returns sea level pressure)
C            Note that sea level pressure is not processed at
C            this time, so it is not an option to compute
C            station pressure from sea level pressure
C
C        12/11/09 (MACTEC)
C          - Added code to identify whether the data are ASOS observations
C            or not so the adjustment for the ASOS truncation can be 
C            applied in Stage 3; need to pass the ASOS flag since this
C            is an element-based format and the data are not written
C            until all elements for a day are processed.  Whatever flag 
C            is on the last element for a day will be the flag written
C            to the output file.
C
C       1/2010 - MACTEC
C         - Determine if record is ASOS obs based on ASOS commission date
C         - Check format valid date range to determine if data was likely 
C           reformatted
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER   ICEIL, ISTAT
      INTEGER   IOST20, VINDEX, IHR, IFT, IWD, IWS 
      INTEGER   HR(30), VALUE(30), NVALUE
      INTEGER   SFINDEX 
      INTEGER   JJJSAVE, JJJ, J, ISKY, ICL, ICHT
      INTEGER   YR2, YR4, MO, DY, IYYMMDD, IYYYYMMDD 
      INTEGER   JULIAN 
      INTEGER   IJDAY 
      INTEGER, SAVE :: IASOSCNT

      REAL      VSBY
      CHARACTER VARNM*4, VUNITS*2
      CHARACTER CODE_1*1, CODE_2*1 
      CHARACTER VNAME(24)*4

      CHARACTER*1 FLAG(30)

      LOGICAL   SEENPWTH
      LOGICAL   GotExtASOSFlg 

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

      DATA JJJSAVE /0/
      DATA IASOSCNT/0/

      DATA VNAME   /'ALTP', 'CLHT', 'DPTC', 'DPTP', 'DRAD', 'GRAD',      ! dtb #120 02064
     &              'HZVS', 'PRES', 'PWTH', 'PWVC', 'RHUM', 'SLVP',      ! dtb #120 02064
     &              'TMCD', 'TMPD', 'TSKC', 'WIND', 'WND2', 'ALC1',      ! dtb #120 02064
     &              'ALC2', 'ALC3', 'ALC4', 'ALC5', 'ALC6', 'XXXX'/      ! dtb #121 02092

      PATH = 'SURFACE   '
      LOC  = 'D3280L'
      ISTAT = 0
      
      SEENPWTH = .FALSE.
      GotExtASOSFlg = .FALSE.
      ISASOS24 = 'N'

      
C---- There may be more than one occurrence of the present weather element
C     (PWTH) in the data; set the logical variable used to indicate if the
C     PWTH has been encountered for the current processing day/
      IF( JJJ .NE. JJJSAVE )THEN
         SEENPWTH = .FALSE.
         GotExtASOSFlg = .FALSE.
         ISASOS24 = 'N'
         JJJSAVE = JJJ
      ENDIF
C-----------------------------------------------------------------------

C---- Read TD-3280 record
      READ(BUFNWS,3280,ERR=900,IOSTAT=IOST20)
     &     VARNM, VUNITS, YR4, MO, CODE_1, CODE_2, DY,           ! dtb #120 02064
     &     NVALUE, (HR(J), VALUE(J), FLAG(J), J = 1, NVALUE)     ! dtb #120 02064

 3280 FORMAT (11X, A4, A2, I4, I2, 2A1, I2, I3, 30(I2,2X,I6,1A, 1X))         ! dtb #120 02064
 
C     compute 8-digit date 
      IYYYYMMDD = YR4*10000 + MO*100 + DY

C     convert 4-digit year to 2-digits
      CALL YR4TOYR2(YR4,YR2) 

C     compute 6-digit date      
      IYYMMDD = YR2*10000 + MO*100 + DY 

C---- Attempt to match the variable name in this record (VARNM) with
c     the list of variables we wish to extract (VNAME()).

      VINDEX = 1
      DO WHILE( VARNM .NE. VNAME(VINDEX) .AND. VINDEX .LE. 23 )
         VINDEX = VINDEX + 1
      ENDDO

      IF( VINDEX .GT. 23 )THEN        
C----    No match on variable name; variable currently not used by AERMET
C        Return to calling routine
         RETURN
      ENDIF

C---- The SFOBS array is dimensioned with a lower bound of 1 and an
C     upper bound of 24 for the 'hours' (first) dimension;  however,
C     TD-3280 uses the 0-23 clock.  To prevent the array limits from
C     being exceeded, the hour (IHR) is incremented by +1 for storage
C     purposes.  When the data are written to the output extract file, 
C     in subroutine GETSFC, this process is reversed.

C---- Match the element name in the data file to a name
C     in the array VNAMES and process the data

      GOTO (30,33,48,62,60,  61,45,32,42,64,  49,31,46,63,34,
     &      50,50,71,72,73,  74,75,76), VINDEX


   30 SFINDEX = 30      ! This is a placeholder for altimeter   ! 30 ALTP
      RETURN            ! setting.


   31 SFINDEX = 31      ! Process sea-level pressure (mb *10)   ! 31 SLVP
      DO J = 1, NVALUE
C        See note above regarding IHR being incremented by +1
         IHR = HR(J) + 1

         IF( VALUE(J) .EQ. SFQA(31,2) )THEN      !  Missing sea-level pressure
C           Sea-level pressure is missing; assign missing code to SFOBS array element
C           Potential subsitutions for missing station pressure handled in SUBST.
            SFOBS(1,31) = SFQA(31,2)
         
         ELSE
C           Sea-level pressure is available, covert to proper units
            SFOBS(IHR, 31) = VALUE(J)
         ENDIF
      ENDDO
      RETURN


   32 SFINDEX = 32      ! Process station pressure (mb *10)     ! 32 PRES
      DO J = 1, NVALUE
         IHR = HR(J) + 1

         IF( VALUE(J) .EQ. SFQA(32,2) )THEN      !  Missing station pressure
C            Station pressure is missing; assign missing code to SFOBS array element
C            Potential subsitutions for missing station pressure handled in SUBST.
             SFOBS(1,32) = SFQA(32,2)
         
         ELSE
C           Station pressure is available, covert to proper units
            XRD2 = FLOAT(VALUE(J))/1000.
            CALL P2MMBR(XRD2,XRD1)
            SFOBS(IHR, 32) = NINT(XRD1 * 10.0)
         ENDIF
      ENDDO
      RETURN


   33 SFINDEX = 33      ! Process ceiling height (km *10)       ! 33 CLHT
      DO J = 1, NVALUE
         IHR = HR(J) + 1

         IF( FLAG(J).EQ.'U' .OR. VALUE(J).EQ.99999 )THEN        !  Unlimited Ceiling
            SFOBS(IHR, 33) = 300

         ELSEIF( VALUE(J) .EQ. 00999 )THEN                      !  Missing ceiling
            SFOBS(IHR, 33) = SFQA(33,2)

         ELSE
            IFT = VALUE(J) * 100                                !  ceiling in feet
            CALL P2MMTR(IFT,ICEIL)                              !  ceiling in meters

            SFOBS(IHR, 33) = NINT((FLOAT(ICEIL)/1000.0)*10.0)
         ENDIF
      ENDDO
      RETURN


   34 SFINDEX = 34      ! Process total & opaque sky            ! 34 TSKC
      GOTO 800


   42 SFINDEX = 42      ! Process present weather               ! 42 PWTH
      IF( .NOT. SEENPWTH )THEN
         DO J = 1, NVALUE
            IHR = HR(J) + 1
            SFOBS(IHR, 42) = VALUE(J)
         ENDDO

         SEENPWTH = .TRUE.
      ENDIF
      RETURN


   45 SFINDEX = 45      ! Process visibility (km *10)           ! 45 HZVS

C     HZVS - Convert hundreths of a mile to km*10

      DO J = 1, NVALUE
         IHR = HR(J) + 1

         IF( VALUE(J) .EQ. SFQA(45,2) )THEN                     !  Missing visibility
            SFOBS(IHR, 45) = VALUE(J)
         ELSE
            VSBY = FLOAT(VALUE(J))/100.
            VSBY = VSBY * 1609.3 / 1000.
            SFOBS(IHR, 45) = NINT(VSBY*10.0)
         ENDIF
      ENDDO
      RETURN


   46 SFINDEX = 46      ! Process dry-bulb temperature (C *10)  ! 46 TMCD
                        ! TMCD is extracted in (C *10)
      GOTO 800


   48 SFINDEX = 48      ! Process dew point (C *10)             ! 48 DPTC
                        ! DPTC is extracted in (C* 10)

      GOTO 800


   49 SFINDEX = 49      ! Process relative humidity             ! 49 RHUM

      GOTO 800

   50 SFINDEX = 50      ! Process wind direction                ! 50 WDIR
                        ! Process wind speed                    ! 51 WSPD

C        WIND - First two of five digits are wind direction (in 10s of
C               degrees) and the last 3 digits are wind speed in knots;
C               convert to m/s * 10.  Missing wind may only be reflected
C               in the wind direction (documentation is unlcear on this).

      DO J = 1, NVALUE

         IHR = HR(J) + 1

C        Convert coded data to temporary direction (IWD) and speed (IWS)! rwb #521 06341
         IWD = INT(FLOAT(VALUE(J))/1000.0) 
         XRD1 = FLOAT(VALUE(J)) - FLOAT(IWD)*1000.0
         CALL P2MMSC(XRD1,XRD2)
         IWS = NINT(XRD2*10.0)
         
C        Check for missing values and assign to SFOBS array
         IF( IWD .EQ. 99 )THEN
            SFOBS(IHR,50) = SFQA(50,2)        ! Missing wind direction
         ELSE    
            SFOBS(IHR,50) = IWD
         ENDIF
         
         IF( IWS .GT. 900 )THEN
            SFOBS(IHR,51) = SFQA(51,2)        ! Missing wind speed
         ELSE 
            SFOBS(IHR,51) = IWS


C           get julian day for the current surface observation
            IJDAY = JULIAN(YR2,MO,DY)

C           determine if the current record is ASOS based on the obs date
            CALL ASOSREC(YR2,MO,DY,HR(J),IJDAY,SFLST) 
            
C           set array for all hours since 24 hours/record
            ISASOS24(IHR) = ISASOS1  
            
C-----------------------------------------------------------------------            
C           Compare ASOS code from data for consistency with commission date.
C           Data will be treated as ASOS if the date is after the commission
C           date, OR if the ASOS code in the data file indicates ASOS. However,
C           warnings will be issued if there is a mismatch between the ASOS 
C           code and the commission date.
C-----------------------------------------------------------------------
            IF( ISASOS24(IHR) .NE. 'A' .AND. CODE_1 .EQ. 'A' )THEN 
C ---          Data flag indicates ASOS, but commission date does not; 
C              ASOS flag based on data file will be used.  However, 
C              issue warning message for ASOS flag mismatch, wording depends
C              on whether commission date has been found
               IASOSCNT = IASOSCNT + 1
               IF( IASOSCNT .LE. 24 )THEN
                  MESS = BLNK80
                  ECODE = 'W49'           
               ELSEIF( IASOSCNT .GT. 24 )THEN      
                  MESS = BLNK80
                  ECODE = 'I49'            
               ENDIF
C              Set ISASOS24 and ISASOS1 to 'A' to indicate that 
C              this is an ASOS observation
               ISASOS24(IHR) = 'A'
               ISASOS1 = 'A'
               IF( .NOT. GotCommDate )THEN
C                 Adjust IHR by -1 since +1 was added above 
                  WRITE(MESS,1030) IHR-1
 1030             FORMAT(' Data flag = ASOS but CommissionDate not',
     &                   ' found; ASOS data flag used for hour: ',I2)
                  CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
               ELSE
C                 Adjust IHR by -1 since +1 was added above 
                  WRITE(MESS,1040) IHR-1
 1040             FORMAT(' Data flag = ASOS prior to the Commission',
     &                   ' Date;  ASOS data flag used for hour: ',I2)
                  CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
               ENDIF
               IF( IASOSCNT .EQ. 24 )THEN
                  MESS = BLNK80
                  ECODE = 'W49'            
                  WRITE( MESS, 1050 )
 1050             FORMAT('  NOTE: Additional messages regarding ASOS ',
     &            'flag/CommDate mismatches included in message file.')
                  CALL ERRHDL( IYYMMDD, PATH, ECODE,LOC,MESS )
               ENDIF
            ELSEIF( ISASOS24(IHR) .EQ. 'A' .AND. CODE_1 .NE. 'A' )THEN
C ---          Data flag does NOT indicate ASOS, but commission date does.
C              Issue warning message for ASOS flag mismatch, wording depends
C              on whether commission date has been found
               IASOSCNT = IASOSCNT + 1
               IF( IASOSCNT .LE. 24 )THEN
                  MESS = BLNK80
                  ECODE = 'W49'           
               ELSEIF( IASOSCNT .GT. 24 )THEN      
                  MESS = BLNK80
                  ECODE = 'I49'            
               ENDIF
C              Adjust IHR by -1 since +1 was added above 
               WRITE(MESS,1060) IHR-1
 1060          FORMAT(' Data flag does NOT = ASOS but Commission',
     &                ' Date DOES; ASOS is assumed for hour: ',I2)
               CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
C              Set ISASOS1 to 'A' to indicate that this is an ASOS observation
               ISASOS1 = 'A'
               IF( IASOSCNT .EQ. 24 )THEN
                  MESS = BLNK80
                  ECODE = 'W49'            
                  WRITE( MESS, 1050 )
                  CALL ERRHDL( IYYMMDD, PATH, ECODE,LOC,MESS )
               ENDIF
            ENDIF                                                     
            
         ENDIF                                                        
 
      ENDDO


      RETURN


   60 SFINDEX = 60      ! Process direct radiation              ! 60 DRAD
      RETURN            ! This is a placeholder


   61 SFINDEX = 61      ! Process global radiation              ! 61 GRAD
      RETURN            ! This is a placeholder


   62 SFINDEX = 48      ! Process dew point temperature         ! 62/48 DPTP

c     This code is redundent if processing data acquired after
c     July 1, 1996 [see variable 48 (DPTC): dew point in deg C x 10]


      DO J = 1, NVALUE
         IHR = HR(J) + 1

         IF( VALUE(J) .EQ. SFQA(48,2) )THEN                     !  Missing dew point
            SFOBS(IHR, 48) = VALUE(J)
         ELSE
C           Convert dew point (whole deg F to deg C * 10)
            XRD1 = FLOAT(VALUE(J))
            CALL P2MCEN(XRD1,XRD2)
            SFOBS(IHR, 48) = NINT(XRD2*10.0)
         ENDIF
      ENDDO
      RETURN

   63 SFINDEX = 46      ! Process dry-bulb temperature          ! 63/46 TMPD

c     This code is redundent if processing data acquired after
c     July 1, 1996 [see variable 46 (TMCD): dry-bulb in deg C x 10]


      DO J = 1, NVALUE
         IHR = HR(J) + 1

         IF( VALUE(J) .EQ. SFQA(46,2) )THEN                     !  Missing dry-bulb
            SFOBS(IHR,46) = VALUE(J)
         ELSE
c           Convert dry-bulb (whole deg F to deg C * 10)
            XRD1 = FLOAT(VALUE(J))
            CALL P2MCEN(XRD1,XRD2)
            SFOBS(IHR, 46) = NINT(XRD2*10.0)
         ENDIF
      ENDDO
      RETURN


   64 SFINDEX = 41      ! Process present wx in vicinity        ! 64/41 PWVC
      RETURN            ! This is a placeholder


   71 SFINDEX = 35      ! Process ASOS cloud layer 1            ! 71/35 ALC1
      DO J = 1, NVALUE

         IF( VALUE(J) .NE. 09999 )THEN

            IHR  = HR(J) + 1
            ISKY = VALUE(J)/1000
            ICL  = VALUE(J) - ISKY*1000

            IF(ISKY .EQ. 09)THEN                                ! Missing
               ISKY = 99
            ELSEIF(ISKY .GE. 6)THEN                             ! Overcast
               ISKY = 10
            ELSEIF(ISKY .EQ. 4)THEN                             ! Broken
               ISKY = 7
            ELSEIF(ISKY .EQ. 2)THEN                             ! Scattered
               ISKY = 3
            ELSE
               ISKY = 0                                         ! Clear
            ENDIF


            IF( ISKY .GT. 5 )THEN                               ! We have a ceiling
               ICL = ICL*100                                    ! Ceiling in feet
               CALL P2MMTR(ICL, ICHT)                           ! Ceiling in meters
               ICHT = NINT((FLOAT(ICHT)/1000.0)*10)             ! Ceiling in tens of km
            ELSE
               ICHT = 300                                       ! Unlimited ceiling
            ENDIF

            SFOBS(IHR,35) = VALUE(J)
            SFOBS(IHR,43) = ISKY
            SFOBS(IHR,44) = ICHT
         ENDIF

      ENDDO
      RETURN

   72 SFINDEX = 36      ! Process ASOS cloud layer 2            ! 72/36 ALC2
      DO J = 1, NVALUE

         IF( VALUE(J) .NE. 09999 )THEN

            IHR  = HR(J) + 1
            ISKY = VALUE(J)/1000
            ICL  = VALUE(J) - ISKY*1000

            IF( ISKY .EQ. 09 )THEN                                ! Missing
               ISKY = 99
            ELSEIF( ISKY .GE. 6 )THEN                             ! Overcast
               ISKY = 10
            ELSEIF( ISKY .EQ. 4 )THEN                             ! Broken
               ISKY = 7
            ELSEIF( ISKY .EQ. 2 )THEN                             ! Scattered
               ISKY = 3
            ELSE
               ISKY = 0                                           ! Clear
            ENDIF


            IF( ISKY .GT. 5 )THEN                               ! We have a ceiling
               ICL = ICL*100                                    ! Ceiling in feet
               CALL P2MMTR(ICL, ICHT)                           ! Ceiling in meters
               ICHT = NINT((FLOAT(ICHT)/1000.0)*10)             ! Ceiling in tens of km
            ELSE
               ICHT = 300                                       ! Unlimited ceiling
            ENDIF

            SFOBS(IHR,36) = VALUE(J)

            IF( SFOBS(IHR,43).NE.99 .AND. ISKY.GT.SFOBS(IHR,43) )THEN
               SFOBS(IHR,43) = ISKY
            ENDIF

            IF( ICHT .LT. SFOBS(IHR,44) )THEN
               SFOBS(IHR,44) = ICHT
            ENDIF

         ENDIF

      ENDDO
      RETURN

   73 SFINDEX = 37      ! Process ASOS cloud layer 3            ! 73/37 ALC3

      DO J = 1, NVALUE

         IF( VALUE(J) .NE. 09999 )THEN

            IHR  = HR(J) + 1
            ISKY = VALUE(J)/1000
            ICL  = VALUE(J) - ISKY*1000

            IF( ISKY .EQ. 09 )THEN                                ! Missing
               ISKY = 99
            ELSEIF( ISKY .GE. 6 )THEN                             ! Overcast
               ISKY = 10
            ELSEIF( ISKY .EQ. 4 )THEN                             ! Broken
               ISKY = 7
            ELSEIF( ISKY .EQ. 2 )THEN                             ! Scattered
               ISKY = 3
            ELSE
               ISKY = 0                                           ! Clear
            ENDIF

            IF( ISKY .GT. 5 )THEN                               ! We have a ceiling
               ICL = ICL*100                                    ! Ceiling in feet
               CALL P2MMTR(ICL, ICHT)                           ! Ceiling in meters
               ICHT = NINT((FLOAT(ICHT)/1000.0)*10)             ! Ceiling in tens of km
            ELSE
               ICHT = 300                                       ! Unlimited ceiling
            ENDIF

            SFOBS(IHR,37) = VALUE(J)

            IF( SFOBS(IHR,43).NE.99 .AND. ISKY.GT.SFOBS(IHR,43) )THEN
               SFOBS(IHR,43) = ISKY
            ENDIF

            IF( ICHT .LT. SFOBS(IHR,44) )THEN
               SFOBS(IHR,44) = ICHT
            ENDIF

         ENDIF

      ENDDO

      RETURN


   74 SFINDEX = 38      ! Process ASOS cloud layer 4            ! 73/38 ALC4
      GOTO 800

   75 SFINDEX = 39      ! Process ASOS cloud layer 5            ! 73/39 ALC5
      GOTO 800

   76 SFINDEX = 40      ! Process ASOS cloud layer 6            ! 73/40 ALC6
      GOTO 800


  800 CONTINUE

C     Add 1 to the hour so the subscript is not out of range in SFOBS(1-24)
      DO J = 1, NVALUE
         IHR = HR(J) + 1
         SFOBS(IHR, SFINDEX) = VALUE(J)
      ENDDO

      RETURN
C-----------------------------------------------------------------------
C- Processing continues here if there is an error decoding the string

  900 CONTINUE
      ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,395) JJJ
 395  FORMAT(' Internal read error for TD-3280 data for YR/MO/DY: ',I8)
      CALL ERRHDL(JJJ,PATH,'E43',LOC,MESS)

      RETURN
      END

