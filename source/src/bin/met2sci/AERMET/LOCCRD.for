      SUBROUTINE LOCCRD( KOUNT,CARD,DDLOC,DDLAT,DDLON,DDLST,ISTAT )
C=====================================================================**
C          LOCCRD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the LOCATION keyword
C
C     Form:  LOCATION  station_ID  lat(lon)  lon(lat)  GMT-to-LST  Elev  ! dtb #508  06299
C
C            station_ID = identifier, usually the WBAN #
C            lat(lon)   = latitude (or longitude), decimal degrees,
C                         with N or S (E or W) qualifier
C            lon(lat)   = longitude (or latitude), decimal degrees,
C                         with E or W (N or S) qualifier
C            GMT-to-LST = factor to convert hour to local standard time;
C                         optional, default = 0
C                         (most common usage: Greenwich to local)

C            Elev       = Optional station elevation (m) for SFCARD and OSCARD

C     Called by:  UACARD, SFCARD, OSCARD
C
C     Initial Release:  December 15, 1992
C
C     Revision History:
C        26 Oct 2006 Added code to read a station elevation
C        14 Apr 2008 Revised code that reads station elevation to
C                    make it an error if the elevation is specified for
C                    upper air and stage 3
C-----------------------------------------------------------------------

C---- Data Declarations


      IMPLICIT NONE
      
      CHARACTER CARD*(*), DDLOC*8, DDLAT*8, DDLON*8
      INTEGER   ISTAT, DDLST, ITEST, ITEST3, ITEST4, IRD4

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C-----------------------------------------------------------------------
C        DDLOC        Station ID (e.g. station number)
C        DDLAT        Latitude (E.G. 30.00N)
C        DDLON        Longitude (E.G. 10.00E)
C        DDLST        Conversion factor equal to (GMT-LST)
C        ISTAT        Process status 1 = Error in processing
CC                                    2 = Processing OK
C        CARD         Record with LOCATION data
C-----------------------------------------------------------------------

C---- Variable Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'LOCCRD'
      ISTAT = 0
      IWORK1(5) = 0
      IWORK1(6) = 0
      DDLST = 0

C---- Check number of fields

      IF( NWORDS.LT.4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''LOCATION'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ELSEIF( NWORDS .GT. 6 )THEN                                      ! dtb #508  06299
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1010 )
1010     FORMAT(' Too many fields on ''LOCATION'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Check for METPREP pathway (IRD1 = 6); issue warning
C     message since LOCATION keyword is no longer used 
C     on Stage 3 METPREP pathway.
      IF( IRD1 .EQ. 6 )THEN
         ECODE = 'W03'
         MESS = BLNK80
         WRITE( MESS,1020 )
1020     FORMAT(' LOCATION keyword on the METPREP ',
     &          'pathway is OBSOLETE - INPUTS IGNORED!')
         CALL ERRHDL( 1,PATH,ECODE,LOC,MESS )
         ECODE = 'W03'
         MESS = BLNK80
         WRITE( MESS,1021 )
1021     FORMAT('  The LOCATION used for sunrise in CBL calcs is',
     &          ' from ONSITE (if availabile) or SURFACE station.' )
         CALL ERRHDL( 2,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

C-----Fetch Station ID                                   ---- CALL GETWRD
      BUF08(1) = '  SITEID'
      BUF08(2) = BLNK08
      CALL GETWRD( 2,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      ISTAT = ITEST

      IF( ISTAT .NE. 1 )THEN
C------- Station ID retrieved successfully, save it
         DDLOC = BUF08(2)

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' ERROR FROM S.GETWRD: STATION/SITE IDENTIFIER')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ENDIF


C-----Fetch latitude and longitude

C-----First lat/long data group                          ---- CALL GETWRD
      BUF08(1) = 'LAT/LONG'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,3,8,1,BUF08(1),BUF08(2),ITEST3 )

C-----Second lat/long data group                         ---- CALL GETWRD
      BUF08(1) = 'LAT/LONG'
      BUF08(3) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,3,8,1,BUF08(1),BUF08(3),ITEST4 )


      IF( ITEST3 .EQ. 1 .OR. ITEST4 .EQ. 1 )THEN
         ISTAT = 1
         IF( ITEST3 .EQ. 1 )THEN
C---------- Something went wrong getting the first group
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,1200 )
1200        FORMAT(' ERROR FROM S.GETWRD: 1st LAT/LON GROUP')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ENDIF

         IF( ITEST4 .EQ. 1 )THEN
C---------- Something went wrong getting the second group
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,1300 )
1300        FORMAT(' ERROR FROM S.GETWRD: 2nd LAT/LON GROUP')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ENDIF

      ELSE

C------- LAT/LONG retrieved; check the values in BUF08(2) and BUF08(3)
C              (1) One should have either an 'E' or an 'W',
C                  This is a valid longitude if the
C                  numeric value is between 0 and 180.
C              (2) The other should have an 'N' or an 'S',
C                  This is a valid latitiude if the
C                  numeric value is between 0 and 90.

         IF( BUF08(2)(8:8).EQ.'E' .OR. BUF08(2)(8:8).EQ.'W' )THEN
C---------- Longitude has been found in BUF08(2)
            CALL LATLON( KOUNT,2,BUF08(2),XRD2,IWORK1(5) )
            DDLON = BUF08(2)

C---------- BUF08(3) should contain the latitude
            CALL LATLON( KOUNT,1,BUF08(3),XRD3,IWORK1(6) )
            DDLAT = BUF08(3)

         ELSEIF( BUF08(2)(8:8).EQ.'N' .OR. BUF08(2)(8:8).EQ.'S' )THEN
C---------- Latitude has been found in BUF08(2)
            CALL LATLON( KOUNT,1,BUF08(2),XRD3,IWORK1(6) )
            DDLAT = BUF08(2)

C---------- BUF08(3) should contain a longitude
            CALL LATLON( KOUNT,2,BUF08(3),XRD2,IWORK1(5) )
            DDLON = BUF08(3)

         ELSE
C---------- Error condition: no E/W or N/S suffix(es)
            ISTAT = 1
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2000 ) BUF08(2),BUF08(3)
2000        FORMAT(' NO N/S or E/W WITH LAT/LONG: ',A8,1X,A8 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ENDIF                   ! N/S and E/W specifier

C------- Check stored status checks (IWORK1(5) and IWORK1(6) from
C        above); both should equal 2, if all checks were passed.

         IF( ISTAT .NE. 1  .AND.  IWORK1(5).EQ.2 .AND.
     &                            IWORK1(6).EQ.2 )THEN
            ISTAT = 2
         ELSE
            ISTAT = 1
         ENDIF

      ENDIF                      ! ITEST3, ITEST4

C-----Now, fetch factor to convert time to LST, if one was specified
      IF( NWORDS.GE.5 )THEN											! rwb Changed GT to GE
         BUF08(1) = ' LST/GMT'
         BUF08(4) = BLNK08
         CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )
         ISTAT = ITEST
         IF( ITEST.EQ.2 )THEN
C---------- Convert GMT to LST factor to integer value

            READ( BUF08(4),3000,IOSTAT=IOFLAG ) DDLST
3000        FORMAT( I8 )
            IF( IOFLAG.EQ.0 )THEN
C------------- Check DDLST value
               IF( DDLST.LT.-12 .OR. DDLST.GT.12 )THEN
                  MESS =  BLNK80
                  ECODE = 'E06'
                  WRITE( MESS,4000 ) DDLST
4000              FORMAT(' CONVERSION TO LST < -12 OR > +12: ',I8)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1

               ELSE
C---------------- This check is on the sign of the time adjustment
C                   (DDLST) that is opposite of what is expected for
C                   the longitude (XRD2)
C                   AERMET expects the adjustment to be positive for
C                   longitudes west of Greenwich
C                   The value of XRD2 is positive for west longitudes
C                   and negative for east longitudes
C                 Note that at this point in the processing of the
C                   control records, the data format (e.g., SAMSON,
C                   ISHD) is not known
C                 If the sign is opposite of what is expected, write
C                   a message.

                  IF( XRD2.LT.0.0 .AND. DDLST.GT.0 )THEN
                     MESS =  BLNK80
                     ECODE = 'W06' 
                     WRITE (MESS,6000 ) XRD2
6000                 FORMAT(' Sign of time adjustment opposite',
     &                      ' for lon: ',F7.2)
                     CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

                     IF( DDLST .GE. 2 )THEN 
C                       Fatal error - 
                        MESS =  BLNK80 
                        ECODE = 'E06'
                        WRITE (MESS,6050 ) DDLST 
6050                    FORMAT('  and time adjustment',
     &                         ' exceeds one hour: ',I4)
                        CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                        ISTAT = 1 
                     ENDIF

                  ELSEIF( XRD2.GT.0.0 .AND. DDLST.LT.0 )THEN
                     MESS =  BLNK80
                     ECODE = 'W06' 
                     WRITE (MESS,6000 ) XRD2
                     CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

                     IF( DDLST .LE. -2 )THEN 
C                       Fatal error -   
                        MESS =  BLNK80 
                        ECODE = 'E06' 
                        WRITE (MESS,6050 ) DDLST 
                        CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                        ISTAT = 1 
                     ENDIF 

                  ENDIF             ! Longitude/DDLST check

               ENDIF                ! DDLST -12 <= DDLST <= 12

            ELSE
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE( MESS,7000 ) BUF08(4)
7000           FORMAT(' Error decoding conversion to LST: ', A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF                   ! IOFLAG = 0

         ELSE
            MESS =  BLNK80
            ECODE = 'E07'
            WRITE( MESS,8000 )
8000        FORMAT(' Error from S.GETWRD: LST time conversion factor')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         DDLST = 0
      ENDIF

      IF( NWORDS.EQ.6 )THEN   ! Fetch station elevation               ! dtb #508  06299
                                                                      ! mfp #---  08105
C        If there is no station elevation on the LOCATION card, then
C        the default elevation is 0 meters (initialized in BLOCK1.INC)
C        IRD1 = path ID
         STNELEV: SELECT CASE (IRD1)
            CASE (1,5)
C              Should not be possible since the LOCATION keyword is not
C              valid for the JOB and MERGE paths.

            CASE (2)
C              Upper air LOCATION elevation is now obsolete;
C              issue warning message.
C             (METPREP LOCATION keyword is also obsolete)
               MESS = BLNK40
               ECODE = 'W04'
               WRITE(MESS, 1065) PATH
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

            CASE (3,4)
C              Surface and On-site: Get the elevation from the LOCATION
C              keyword and decode it.

               BUF08(6) = BLNK08
               CALL GETWRD(6, KOUNT,CARD,1,8,1,'ELEV',BUF08(6),ITEST)
               IF( ISTAT.EQ.1 )THEN
                  CONTINUE
               ELSE
                  ISTAT = ITEST
               ENDIF

               READ(BUF08(6), '(F8.0)', IOSTAT=IRD4) PWELEV(IRD1)

               IF( IRD4.EQ.0 )THEN
C                 Set a flag to know that an elevation was specified on
C                 the LOCATION keyword
                  IF( IRD1 .EQ. 3 )THEN
C                     Flag for SURFACE station elevation
                      GOTPWELEV(3) = .TRUE.
                  ELSEIF( IRD1 .EQ. 4 )THEN
C                     Flag for ONSITE station elevation
                      GOTPWELEV(4) = .TRUE.
                  ENDIF
               ELSE
                  MESS = BLNK40
                  ECODE = 'E06'
                  WRITE(MESS, 1070) BUF08(6)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

            CASE DEFAULT
               MESS = BLNK40
               ECODE = 'E00'
               WRITE(MESS, 1075) PATH
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1

         END SELECT STNELEV

      ELSEIF( NWORDS.LT.6 .AND. IRD1.EQ.4 )THEN
C ---    Issue warning message regarding default station elevation for 
C        ONSITE data, with recommendation to include elevation
         MESS =  BLNK80
         WRITE (MESS, 6200) 
 6200    FORMAT(' NO ONSITE elevation specified on LOCATION ',
     &           'keyword, default of 0m assumed;')
         CALL ERRHDL(1,PATH,'W51',LOC,MESS)
         MESS =  BLNK80
         WRITE (MESS, 6300) 
 6300    FORMAT('  Recommend specifying station elevation.')
         CALL ERRHDL(2,PATH,'W51',LOC,MESS)
      ENDIF

1065  FORMAT(1X, 'Station elevation not allowed for ',A10,
     &           ' pathway: input ignored!')
1070  FORMAT(1X, 'Error reading elevation on LOCATION keyword ',A8)
1075  FORMAT(1X, 'Internal programming error for station elevation')

      RETURN
      END

