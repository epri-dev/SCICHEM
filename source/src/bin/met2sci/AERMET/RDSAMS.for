      SUBROUTINE RDSAMS(buf140,newyr,newmo,newdy,newhr,istat)
C=====================================================================**
C          RDSAMS Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Fills in the Appropriate Met Variable Arrays
C               with Raw Data from SAMSON
C
C     Called from: SFEXT
C
C     Inputs:     Surface Data from SAMSON in character format
C
C     Outputs:    24 Hours of Met Parameters; date and time group are
C                 returned through calling arguments; data are returned
C                 vai COMMON
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        12/19/00: PES
C                - added code to process the missing data and accumulation
C                  flags for precipitation
C        05/29/08: MACTEC Federal Programs
C                - added varaible declarations, intializations, process
C                  variables 1-5 and 16 as CHARACTER strings, changed
C                  function from INT to NINT for ITOT and IOPQ, and
C                  added comments for clarity
C
C       1/2010 (MACTEC)
C         - Check format valid date range to determine if data was likely 
C           reformatted
C         - Determine if the current record is ASOS based on the obs date
C         - For records on or after ASOS commissioning date,  
C           set total and opaque sky cover to missing
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      INTEGER     IWX, IV, ISTAT, NEWYR, NEWMO, NEWDY, NEWHR
      INTEGER     ITOT, IOPQ, IYYMMDD, IVNDX, KSPEED, JJJ
      INTEGER     IMULT(8), NRECS
      INTEGER     YR4, IYYYYMMDD 
      INTEGER     JULIAN, IJDAY  

      INTEGER, SAVE :: IASOSCNT, IASOSCNT2

      CHARACTER*9 ASAM(MAXVAR)
      CHARACTER*6 WFMT(MAXVAR)
      CHARACTER*9 ATEMP
      CHARACTER*1 PFLAG, MISSFLAG, ACCFLAG
      REAL        VTEMP, STNP

      INCLUDE 'WORK1.INC'

C---- Data initialization
      DATA LOC/'RDSAMS'/ , PATH/'SURFACE'/
      DATA IASOSCNT/0/, IASOSCNT2/0/

      JJJ  = NEWYR*10000 + NEWMO*100 + NEWDY

C     Set start and end dates to mark valid format date range
      SfcVldStart = 19610101              ! mec 1/2010
      SfcVldEnd   = 19901231              ! mec 1/2010

C---- Data formats:
C     The first five elements are for the date, time and obs. flag
C     The second five are for radiation measurements
C     The remaining are weather variables (preceded by the field number
C     displayed when data are retrieved):
C           6 = Total Sky Cover
C           7 = Opaque Sky Cover
C           8 = Dry Bulb Temperature
C           9 = Dew Point Temperature
C          10 = Relative Humidity
C          11 = Station Pressure
C          12 = Wind Direction
C          13 = Wind Speed
C          14 = Visibility
C          15 = Ceiling Height
C          16 = Present Weather
C          17 = [Precipitable Water - not processed]
C          18 = [Broadband Aerosol Optical Depth - not processed]
C          19 = [Snow Depth - not processed]
C          20 = [Days Since Last Snowfall - not processed]
C          21 = Hourly Precipitation and Flag
C
C          Note that there is no sea level pressure in SAMSON data

C---- Data Declarations
      DATA WFMT /'(I3)','(I3)','(I3)','(I3)','(I1)',
     &           '(A4)','(A4)','(A7)','(A7)','(A7)',
     &           '(F2.0)','(F2.0)','(F5.0)','(F5.0)','(F3.0)',
     &           '(F4.0)','(F3.0)','(F5.0)','(F6.0)','(F6.0)',
c    &           '(I9)','(F4.0)','(F6.0)','(F4.0)','(F3.0)',            ! dtb #524  06341
     &           '(A9)','(F4.0)','(F6.0)','(F4.0)','(F3.0)',            ! dtb #524  06341
     &           '(A7)'/

      DATA IMULT /10,10, 1,10, 1,10,10,10/, MISSFLAG/' '/, ACCFLAG/' '/

      NRECS = 0

C     Note: The station ID appears only once - in a header, so it cannot
C           be checked here as is done in the CD144 retrieval process.
C           The station is checked when the header is read.

      NRECS = NRECS + 1

C---- Decode the SAMSON record
      READ( buf140, SAMFMT, ERR=320 ) (ASAM(IV),IV=1,NVARS+5)

      IF( ASAM(1)(1:1) .EQ. '~' )THEN
C        There is more than one year of data in this file; stop
C        processing and let the user know.
         MESS =  BLNK80
         WRITE(MESS, 3900)
 3900    FORMAT(' Second set of SAMSON headers in file')
         CALL ERRHDL( NRECS,PATH,'W45',LOC,MESS )
         ISTAT  =  1
         RETURN
      ENDIF

C---- Decode the date and time information from the record
      READ (ASAM(1),WFMT(1)) newyr
      READ (ASAM(2),WFMT(2)) newmo
      READ (ASAM(3),WFMT(3)) newdy
      READ (ASAM(4),WFMT(4)) newhr

C     Initialize the cloud cover variables
      ITOT = 99
      IOPQ = 99

C     Some SAMSON data may be modeled rather than observed;
C     if so, write a message to the user
      IF( ASAM(5) .EQ. '9' )THEN
C        Data are modeled, not observed; warn the user
         MESS =  BLNK80
         WRITE(MESS, 4000) NEWYR,NEWMO,NEWDY,NEWHR
 4000    FORMAT(' SAMSON data are ''MODELED'' on ',4(1X,I2) )
         CALL ERRHDL( 0,PATH,'I47',LOC,MESS )
      ENDIF

C ----------------------------------------------------------------------
C     Check obs date against valid date range for SAMSON format
C     If date is outide of date range, set flag to issue a warning.
C ----------------------------------------------------------------------    
      CALL YR2TOYR4(newyr,YR4)                              

      IYYYYMMDD = YR4*10000 + newmo*100 + newdy             
      IYYMMDD =   newyr*10000 + newmo*100 + newdy           

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
     &    '  Cloud cover will be set to missing for ASOS records!' 
            CALL ERRHDL(0,PATH,'W40',LOC,MESS)             

         ENDIF                                             
      ENDIF                                                


C     get julian day for the current surface observation
      IJDAY = JULIAN(NEWYR,NEWMO,NEWDY)                    

C     determine if the current record is ASOS based on the obs date
      CALL ASOSREC(NEWYR,NEWMO,NEWDY,NEWHR,IJDAY,SFLST)   

C---- Loop Over the SAMSON Variables
      DO IV = 1,NVARS

C        Process the variables, skipping variables 1-5 and
C        17-20; SAMSON and AERMET use the metric system for all
C        variables except precipitation, so no SAMSON variables
C        have to have units converted other than precipitation

         IF (IDVAR(IV) .LE. 5 .OR. IDVAR(IV) .EQ. 16) THEN       
C           Read as character string                             
            READ( ASAM(IV+5), WFMT(IDVAR(IV)+5), ERR= 330) ATEMP 
         ELSEIF (IDVAR(IV) .NE. 21 )THEN                         
C           Read as real variable                                
            READ( ASAM(IV+5), WFMT(IDVAR(IV)+5), ERR= 330) VTEMP
         ELSEIF( IDVAR(IV) .EQ. 21 )THEN
C---------- Precipitation data are in two parts - a data value and
C           possibly a flag
            READ( ASAM(IV+5), WFMT(IDVAR(IV)+5), ERR= 340) ATEMP
            READ( ATEMP, '(F6.0,A1)' ) VTEMP, PFLAG
         ENDIF

         IF (IDVAR(IV) .GE. 6  .AND. IDVAR(IV) .LE. 15 )THEN            ! Total Sky Cover
            IF( IDVAR(IV) .EQ. 6 )THEN
C              Total sky cover: 99 = missing, same as AERMET default
C              Set to missing if ASOS observation          
C                (determined by commission date)           
               IF (ISASOS1 .EQ. 'A') THEN                  
                  ITOT = 99                                
                  IASOSCNT = IASOSCNT + 1
                  IF( IASOSCNT .LE. 24 )THEN
                     MESS =  BLNK80                           
                     WRITE(MESS,602) NEWHR                    
                     CALL ERRHDL(IYYMMDD,PATH,'W41',LOC,MESS) 
                  ELSEIF( IASOSCNT .GT. 24 )THEN
                     MESS =  BLNK80                           
                     WRITE(MESS,602) NEWHR                    
                     CALL ERRHDL(IYYMMDD,PATH,'I41',LOC,MESS) 
                  ENDIF
                  IF( IASOSCNT .EQ. 24 )THEN
                     MESS =  BLNK80 
                     WRITE(MESS,604)
                     CALL ERRHDL(JJJ,PATH,'W41',LOC,MESS)
                  ENDIF
               ELSE                                        
                  ITOT = NINT(VTEMP)              
               ENDIF                                       

 602  FORMAT(' Total  sky cover set to missing for reformatted ',
     &        'ASOS HR ',I2)
 604  FORMAT('  NOTE: Additional messages regarding ASOS clouds ',
     &         'for reformatted data included in message file.')
  
            ELSEIF( IDVAR(IV) .EQ. 7 )THEN                              ! Opaque Sky Cover
C              Opaque sky cover: 99 = missing, same as AERMET default
C              Set to missing if ASOS observation           
C                (determined by commission date)            
               IF (ISASOS1 .EQ. 'A') THEN                  
                  IOPQ = 99                                
                  IASOSCNT2 = IASOSCNT2 + 1
                  IF( IASOSCNT2 .LE. 24 )THEN
                     MESS =  BLNK80                           
                     WRITE(MESS,603) NEWHR                    
                     CALL ERRHDL(IYYMMDD,PATH,'W41',LOC,MESS) 
                  ELSEIF( IASOSCNT2 .GT. 24 )THEN
                     MESS =  BLNK80                           
                     WRITE(MESS,603) NEWHR                    
                     CALL ERRHDL(IYYMMDD,PATH,'I41',LOC,MESS) 
                  ENDIF
                  IF( IASOSCNT2 .EQ. 24 )THEN
                     MESS =  BLNK80 
                     WRITE(MESS,604)
                     CALL ERRHDL(JJJ,PATH,'W41',LOC,MESS)
                  ENDIF
               ELSE                                        
                  IOPQ = NINT(VTEMP)              
               ENDIF                                       

 603  FORMAT(' Opaque sky cover set to missing for reformatted ',
     &        'ASOS HR ',I2)
     
            ELSEIF( IDVAR(IV) .EQ. 8 )THEN                              ! 46 Dry Bulb Temperature
               IVNDX = IDVAR(IV) - 7
C              Dry bulb temperature: 9999=missing in SAMSON;
C              Documentation says 9999, but field width suggests 999
C                                      999=missing in AERMET default
               IF( VTEMP .LT. 900.0 )THEN
                  SFOBS(1,46) = NINT((VTEMP)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,46) = SFQA(46,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 9 )THEN                              ! 48 Dew Point Temperature
               IVNDX = IDVAR(IV) - 7
C              Dew point temperature: 9999=missing in SAMSON;
C                                      999=missing in AERMET default
               IF( VTEMP .LT. 9000.0 )THEN
                  SFOBS(1,48) = NINT((VTEMP)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,48) = SFQA(48,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 10 )THEN                             ! 49 Relative Humidity
               IVNDX = IDVAR(IV) - 7
C              Relative humidity: 999=missing in SAMSON;
C                                 999=missing in AERMET default
               IF( VTEMP .LT. 900.0 )THEN
                  SFOBS(1,49) = NINT((VTEMP)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,49) = SFQA(49,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 11 )THEN                             ! 32 Station Pressure
               IVNDX = IDVAR(IV) - 7
C              Station pressure: 9999=missing in SAMSON;
C                               99999=missing is AERMET default
               IF( VTEMP .LT. 9000.0 )THEN
                  SFOBS(1,32) = NINT((VTEMP)*IMULT(IVNDX))

               ELSE
C                 Station pressure is missing; if user-specified
C                 elevation on LOCATION keyword, then substitutions 
C                 for missing station pressure based on elevation 
C                 will be handled in sub SUBST during Stage 3. 
C                 However, if user did not specify station elevation,
C                 but elevation is available from SAMSON file header,
C                 then use that elevation to substitute for missing
C                 pressure here.

                  IF( GOTSFELEV .AND. .NOT.GOTPWELEV(3) )THEN
C ---                Set elevation to use for substitution based
C                    on value from SAMSON file header
                     ZMSL2USE = SFELEV

C                    Compute station pressure from station elevation using 
C                    standard atmosphere                                   
                     XRD1 = 1013.25 *
     &                     (1.0 - ((6.5e-3/288.15) * ZMSL2USE))**5.255
                     SFOBS(1,32) = NINT(XRD1*10)

                  ELSE
C ---                Assign missing code for station pressure

                     SFOBS(1,32) = SFQA(32,2)

                  ENDIF
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 12 )THEN                             ! 50 Wind Direction
               IVNDX = IDVAR(IV) - 7
C              Wind direction: 999=missing in SAMSON;
C                               99=missing in AERMET default
               IF( VTEMP .LE. 360.0 )THEN
                  SFOBS(1,50) = NINT((VTEMP/10.0)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,50) = SFQA(50,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 13 )THEN                             ! 51 Wind Speed
               IVNDX = IDVAR(IV) - 7
C              Wind speed: 9999 or 99=missing in SAMSON;
C                               -9999=missing in AERMET default
C              The conversion from m/s to knots then back to
C              m/s is to maintain some consistency with PCRAMMET
               IF( VTEMP .LT. 100.0 )THEN
                  KSPEED = NINT( VTEMP/0.51444 )
                  VTEMP  = FLOAT(KSPEED) * 0.51444
                  SFOBS(1,51) = NINT((VTEMP)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,51) = SFQA(51,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 14 )THEN                             ! 45 Horizontal Visibility
               IVNDX = IDVAR(IV) - 7
C              Visibility: 99999=missing in SAMSON;
C                          99999=missing in AERMET default
               IF( VTEMP .LT. 99990.0 )THEN
                  SFOBS(1,45) = NINT((VTEMP)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,45) = SFQA(45,2)
               ENDIF

            ELSEIF( IDVAR(IV) .EQ. 15 )THEN                             ! 33 Ceiling Height
               IVNDX = IDVAR(IV) - 7
C              Ceiling height: 999999=missing in SAMSON;
C                                 999=missing in AERMET
               IF( VTEMP .GT. 77776.0  .AND.
     &                        VTEMP .LT. 77778.0 )THEN
                  SFOBS(1,33) = 300
               ELSEIF( VTEMP .LT. 99990.0 )THEN
                  SFOBS(1,33) = NINT((VTEMP/1000.0)*IMULT(IVNDX))
               ELSE
                  SFOBS(1,33) = SFQA(33,2)
               ENDIF
            ENDIF

         ELSEIF( IDVAR(IV) .EQ. 16 )THEN
            CALL SAMWX (ASAM(IV+5), IWX)                                ! dtb #303 04208
            SFOBS(1,42) = IWX                                           ! dtb #303 04208

         ELSEIF( IDVAR(IV) .EQ. 21 )THEN                                ! 30 Precipitation
C           Precipitation: inches and hundredths converted to
C           millimeters; integerized by multiplying by 1000
C           factor of 10 = 1000 (integerize) / 100 (hundredths)
C              Precipitation: 99999=missing in SAMSON;
C                                -9=missing in AERMET
C           Note: the precipitation flag for missing periods (M)
C                 is the only flag processed completely at this time;
C                 a message is written if the accumualtion flag (A)
C                 is encountered and the total accumulated precip is
C                 assigned to the last hour of the acculmulation period.
C                 No check is made for the 'deleted' flag (D) since it
C                 it not known if there will be any value (99999?)
C                 associated with it.

            IF( PFLAG .EQ. ' ' )THEN
               IF(MISSFLAG .EQ. ' ' )THEN
                  SFOBS(1,30) = NINT(VTEMP * 25.4)                    ! dtb #305 04356
               ELSEIF (MISSFLAG .EQ. 'M') THEN
                  SFOBS(1,30) = SFQA(30,2)
               ENDIF
            ELSEIF( PFLAG .EQ. 'M' )THEN
               SFOBS(1,30) = SFQA(30,2)
               IF( MISSFLAG .EQ. ' ' )THEN
                  MISSFLAG = 'M'
               ELSE
                  MISSFLAG = ' '
               ENDIF
            ELSEIF( PFLAG .EQ. 'A' )THEN
               IF(VTEMP .LT. 99990.0 )THEN
                  SFOBS(1,30) = NINT(VTEMP * 25.4)                    ! dtb #305 04356
               ELSE
                  SFOBS(1,30) = SFQA(30,2)
               ENDIF
               MESS =  BLNK80
               IF( ACCFLAG .EQ. ' ' )THEN
                  WRITE(MESS,610) newyr,newmo,newdy,newhr
  610             FORMAT(' PRECIP accumulation started:', 4i3)
                  ACCFLAG = 'A'
               ELSEIF( ACCFLAG .EQ. 'A' )THEN
                  WRITE(MESS,611) newyr,newmo,newdy,newhr
  611             FORMAT(' PRECIP accumulation ended  :', 4i3)
                  ACCFLAG = ' '
               ENDIF
               CALL ERRHDL(JJJ,PATH,'I45',LOC,MESS)
            ELSEIF( PFLAG .EQ. 'D' )THEN
               MESS =  BLNK80
               WRITE(MESS,615) newyr,newmo,newdy,newhr
  615          FORMAT(' PRECIP deleted flag not processed:', 4i3)
               CALL ERRHDL(KOUNT,PATH,'I45',LOC,MESS)
            ENDIF
         ENDIF

C     End Loops Over Variables
      ENDDO

C     Concatenate the sky covers                                       ! 34 Total//Opaque Sky Cover
      SFOBS(1,34) = ITOT*100 + IOPQ

C     The station pressure was not in the input file
      IF( .NOT. STNPINFILE .AND. GOTSFELEV )THEN
C        Compute the station pressure from the standard
C        atmosphere using the elevation in the data file
         ZMSL2USE = SFELEV

         IF( ZMSL2USE .GT. -900. .AND. ZMSL2USE .LT. 9000. )THEN
            STNP = 1013.25 *                                   
     &             (1.0 - ((6.5e-3/288.15) * ZMSL2USE))**5.255 
            SFOBS(1,32) = NINT(STNP * 10)                      
         ENDIF                                                 

      ENDIF

C---- Return to the calling program
      RETURN

C     Processing continues here if an error was encountered decoding
C     the variables in the record.

  320 MESS =  BLNK80
      WRITE(MESS,605) NRECS
  605 FORMAT(' Error decoding variables from record buffer #',I5)
      CALL ERRHDL(KOUNT,PATH,'E43',LOC,MESS)
      ISTAT = 1
      RETURN

C     Processing continues here if an error was encountered while
C     resolving a variable other than precipitation.

  330 MESS =  BLNK80
      WRITE(MESS,607) IV, NRECS
  607 FORMAT(' Error decoding  variable # ', I3, '  REC #',I5)
      CALL ERRHDL(KOUNT,PATH,'E43',LOC,MESS)
      ISTAT = 1
      RETURN

C     Processing continues here if an error was encountered while
C     resolving the precipitation value or flag

  340 MESS =  BLNK80
      WRITE(MESS,608) NRECS, ASAM(26), WFMT(26)
  608 FORMAT(' Error decoding precipitation , REC #',I5, A9, A6)
      CALL ERRHDL(KOUNT,PATH,'E43',LOC,MESS)
      ISTAT = 1
      RETURN


      END

