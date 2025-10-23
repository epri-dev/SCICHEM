      SUBROUTINE D144LV(CARD,ISTAT,JJJ)
C=====================================================================**
C        D144LV Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decode the CD144 surface data and puts it into the
C               required format.
C
C     Called by: GETSFC
C
C     Arguments:
C       CARD     Input   Card image with surface data
C       JJJ      Input   Date in the form YYMMDD
C       ISTAT    Output  Program status of the decode
C
C     Revision history:
C       11/30/94 
C         - Made into a separate subroutine from an ENTRY point
C         - Corrected error in decoding sky condition if all or
C           part of sky obscured
C
C       04/30/08 (MACTEC)
C         - Added code to compute station pressure from sea level
C           pressure and station elevation or just station elevation
C           using the standard atmosphere
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER, PARAMETER :: NUMOVR=35
      INTEGER     IWX, I, J, JJJ
      INTEGER     IABSNT, IVIS(20), ICEIL
      INTEGER     IWETH, IVSBY
      INTEGER     ISTAT, IV
      INTEGER     ISUMTT, ISUMOP, IOST20, ITEST
      REAL        ABSENT, VISDIS(20), PRESSO, DEWPT, WDIR, WSP, STAPR
      REAL        RH, WETTMP, DRYTMP, VSBY
      CHARACTER   CARD*(*)
      CHARACTER*1 OVRPCH(NUMOVR),OVR11(10),OVR12(10),OVRNOR(10)
      LOGICAL     GOTVIS

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Declarations
      DATA OVR11/'[','A','B','C','D','E','F','G','H','I'/,
     &     OVR12/']','J','K','L','M','N','O','P','Q','R'/,
     &    OVRNOR/'0','1','2','3','4','5','6','7','8','9'/
     
      DATA IVIS/0,1,2,3,4,5,6,7,8,9,10,12,14,16,17,18,19,20,24,27/

      DATA VISDIS/0.0, 0.067, 0.125, 0.188, 0.25, 0.312, 0.375, 0.50,
     &            0.625, 0.75, 1.0, 1.125, 1.25, 1.375, 1.5, 1.625,
     &            1.75, 2.0, 2.25, 2.5/

      DATA IABSNT/-9999/, ABSENT/-9999.0/

      PATH = 'SURFACE   '
      LOC  = 'D144LV'

C-----------------------------------------------------------------------
C     Read the surface weather observations from the 80-character
C     string passed into the subroutine

      READ(CARD,141,ERR=20001,IOSTAT=IOST20) SFGHR,
     *    (OVRPCH(I),I=1,7), IVSBY, IWETH, PRESSO, OVRPCH(8),
     *    DEWPT, WDIR, OVRPCH(9), WSP, STAPR, OVRPCH(10), DRYTMP,
     *    OVRPCH(11), WETTMP, RH, (OVRPCH(J),J=12,35)

141   FORMAT (11X,I2,7A1,I3,I8,F4.0,A1,2F2.0,A1,F1.0,F4.0,A1,
     *  F2.0,A1,F2.0,F3.0,24A1)

C---- Go through each overpunch to decode its meaning and make the
C     appropriate transformation.  If they are actually overpunched
C     (resulting in an alphabetic character), 10 is added to more
C     easily identify them.
C     The flag for an 'X' (an overpunch) or a '-' is -8888 (which is
C     interpreted later)

      DO I=1,NUMOVR
         SFOVR(I)=-99999
         IF(OVRPCH(I).EQ.'X'.OR.OVRPCH(I).EQ.'-') THEN
            SFOVR(I)=-8888

         ELSEIF(OVRPCH(I).EQ.' ') THEN
            SFOVR(I)=IABSNT

         ELSE
            DO J=0,9
              IF(OVRPCH(I).EQ.OVRNOR(J+1)) THEN
                 SFOVR(I)=J
                 EXIT
              ELSEIF(OVRPCH(I).EQ.OVR11(J+1)) THEN
                 SFOVR(I)=J+10
                 EXIT
              ELSEIF(OVRPCH(I).EQ.OVR12(J+1)) THEN
                 SFOVR(I)=J+10
                 EXIT
              ENDIF
            ENDDO
         ENDIF


C------- Check to make sure a proper overpunch character was identified

         IF(SFOVR(I).EQ.-99999) THEN
            MESS =  BLNK80
            WRITE(MESS,390) I, SFGHR
  390       FORMAT(' OVERPUNCH # ',I2,' NOT PROPERLY DECODED FOR HR ',
     &              I2.2)
            CALL ERRHDL(JJJ,PATH,'W43',LOC,MESS)
         ENDIF

      ENDDO

C---- Convert selected "blank" overpunches to "zero" -- we assume that
C     such blanks are intentional, since missing values will be caught
C     by checking for blanks in the data fields.

C---- Ceiling height:
       ITEST = SFOVR(1)+SFOVR(2)+SFOVR(3)
       IF(ITEST .NE. (3*IABSNT)) THEN
          DO I=1,3
             IF(SFOVR(I) .EQ. IABSNT) SFOVR(I)=0
          ENDDO
       ENDIF

C---- Temperatures and wind speed:
       DO I=8,11
          IF(SFOVR(I) .EQ. IABSNT) SFOVR(I)=0
       ENDDO

C-----------------------------------------------------------------------
C     Decode the data in the order it will appear on output
C     Note: Because only one hour at a time is processed, the data
C           are stored in the first hour element of SFOBS(hr,vbl);
C           SFOBS will be used later to store 24 hours of data.

C---- Precipitation (not available on TD-1440 format)
      SFOBS(1,30) = SFQA(30,2)                             ! 30 Precipitation (PRCP)

C---- Sea level pressure (MB)                              ! 31 Sea Level Pressure (SLVP)
      IF(CARD(32:35).EQ.'    ') THEN
      
         SFOBS(1,31) = SFQA(31,2)                                       ! rwb 07-02-2010

      ELSE
C------- This does not match the documentation for CD-144, but seems
C        to be correct, at least for some earlier versions.  Since
C        sea level pressure should normally not be less than 800,
C        it is compatible with later versions (we hope)

         IF (PRESSO.LT.800.0) THEN
            PRESSO = PRESSO/10.0 + 1000.0
         ELSE
            PRESSO = PRESSO/10.0
         ENDIF
         SFOBS(1,31) = INT(PRESSO * 10.0)
      ENDIF

C---- Station pressure (mb)                                ! 32 Station Pressure (PRES)
      IF(CARD(43:46).EQ.'    ') THEN
C        Station pressure is missing; assign missing code to SFOBS array element
C        Potential subsitutions for missing station pressure handled in SUBST.
         SFOBS(1,32) = SFQA(32,2)
         
      ELSE
C        Station pressure is available, covert to proper units
         STAPR = STAPR/100.0
         CALL P2MMBR(STAPR,XRD1)
         SFOBS(1,32) = NINT(XRD1 * 10.0)
      ENDIF

C---- Ceiling height (km & tenths) - ( 300 (30.0 km) ==> unlimited)
      CALL CLHT(1,ICEIL,IABSNT,SFQA(33,2))                 ! 33 Ceiling height (CLHT)
      SFOBS(1,33) = ICEIL

C                 ***** START CONCATENATED VARIABLES *****

C---- Total and opaque sky cover (tenths)                  ! 34 Total // Opaque Sky Cover (TSKC)
      CALL CVG(12,ISUMTT,IABSNT,SFQA1(34,1))
      CALL CVG(35,ISUMOP,IABSNT,SFQA1(34,2))
      SFOBS(1,34) = ISUMTT*100 + ISUMOP


c     Variables 30 - 35 are place holders for ASOS (sky condition       ! dtb #120 02064
c     and layer height).  These variables are not available in the      ! dtb #120 02064
c     TD-1440 archive.                                                  ! dtb #120 02064

c     Variable 41 is a place holder for weather in the vicinity


C---- Determine the pricipitation type (liquid or frozen) from the 
c     present weather code.
      CALL SFCWXX(IWETH, IWX)                              !            ! dtb #303 04208
      SFOBS(1,42) = IWX                                    ! 42 PWTH    ! dtb #303 04208

c     Variables 43 and 44 are place holders for ASOS derived            ! dtb #120 02064
c     variables, total cloud cover and ceiling, respectively.           ! dtb #120 02064

C---- Horizontal visiblity (km)  (1609.3 meters/mile conversion used)
C     to the (nearest whole kilometer * 10)

      IF(CARD(21:23) .EQ. '   ') THEN                      ! 45 Horizontal Visibility (HZVS)
         VSBY = -9999.0

      ELSEIF (IVSBY .LT. 30) THEN
         IV = 1
         GOTVIS = .FALSE.
         DO WHILE (GOTVIS  .OR.  IV .GT. 20)
            IF (IVSBY .EQ. IVIS(IV)) THEN
               VSBY = VISDIS(IV)
               GOTVIS = .TRUE.
               IF(VSBY .LT. 0.0) THEN
                  SFOBS(1,45) = SFQA(45,2)
               ELSE
                  VSBY = VSBY*1609.3/1000.0
                  SFOBS(1,45) = NINT( VSBY*10.0 )
               ENDIF

            ELSE
               IV = IV + 1

            ENDIF
         ENDDO

      ELSEIF (IVSBY .LT. 990) THEN
         VSBY = IVSBY/10.0

      ELSEIF(IVSBY .GE. 990) THEN
         VSBY = 100.0

      ELSE
         VSBY = ABSENT

      ENDIF

C---- Dry bulb temperature (deg C and tenths)              ! 46 Dry Bulb Temperature
      IF(CARD(48:49) .EQ. '  '   .OR.  SFOVR(10) .EQ. -99999) THEN
C        Temperature is missing
         SFOBS(1,46) = SFQA(46,2)

      ELSEIF(SFOVR(10).EQ.-8888) THEN
C        Temperature is negative
         DRYTMP = -DRYTMP
         CALL P2MCEN(DRYTMP,XRD2)
         SFOBS(1,46) =  NINT(XRD2 * 10.0)

      ELSE
         IF(SFOVR(10).GE.10) THEN
C           Temperature is less than -99.9
            DRYTMP = -100.0 - DRYTMP
            CALL P2MCEN(DRYTMP,XRD2)
            SFOBS(1,46) = NINT(XRD2 * 10.0)
         ELSE
C           Temperature is positive
            DRYTMP = SFOVR(10)*100.0 + DRYTMP
            CALL P2MCEN(DRYTMP,XRD2)
            SFOBS(1,46) = NINT(XRD2 * 10.0)
         ENDIF
      ENDIF

C---- Wet bulb temperature (deg C and tenths)              ! 47 Wet Bulb Temperature
      IF(CARD(51:52) .EQ. '  '   .OR. SFOVR(11) .EQ. -99999) THEN
C        Wet bulb temperature is missing
         SFOBS(1,47) = SFQA(47,2)

      ELSEIF(SFOVR(11).EQ.-8888) THEN
C        Wet bulb temperature is negative
         WETTMP = -WETTMP
         CALL P2MCEN(WETTMP,XRD2)
         SFOBS(1,47) = NINT(XRD2 * 10.0)

      ELSE
C        Wet bulb temperature is positive
         WETTMP = SFOVR(11)*100.0 + WETTMP
         CALL P2MCEN(WETTMP,XRD2)
         SFOBS(1,47) = NINT(XRD2 * 10.0)
      ENDIF

C---- Dew point (deg C and tenths)                         ! 48 Dew Point Temperature
      IF(CARD(37:38) .EQ. '  '  .OR.  SFOVR(8) .EQ. -99999) THEN
C        Dew point is missing
         SFOBS(1,48) = SFQA(48,2)

      ELSEIF(SFOVR(8).EQ.-8888) THEN
C        Dew point is negative
         DEWPT = -DEWPT
         CALL P2MCEN(DEWPT,XRD2)
         SFOBS(1,48) = NINT(XRD2 * 10.0)

      ELSE
C        Dew point is positive
         DEWPT = SFOVR(8)*100.0 + DEWPT
         CALL P2MCEN(DEWPT,XRD2)
         SFOBS(1,48) = NINT(XRD2 * 10.0)
      ENDIF

C---- Relative humidity (whole percent)                    ! 49 Relative Humidity
      IF(CARD(53:55) .EQ. '   ') THEN
         SFOBS(1,49) = SFQA(49,2)

      ELSE
         SFOBS(1,49) = NINT( RH )

      ENDIF

C---- Wind direction (tens of degrees from north)          ! 50 Wind Direction
      IF(CARD(39:40) .EQ. '  ') THEN
         SFOBS(1,50) = SFQA(50,2)

      ELSE
         SFOBS(1,50) = NINT( WDIR )

      ENDIF

C---- Wind speed (meters/sec and tenths)                   ! 51 Wind Speed
      IF(CARD(42:42) .EQ. ' '   .OR.
     1     SFOVR(9) .EQ. -8888  .OR.
     2     SFOVR(9) .EQ. -99999) THEN
        SFOBS(1,51) = SFQA(51,2)

      ELSE
        WSP = WSP + SFOVR(9)*10.0
        CALL P2MMSC(WSP,XRD2)
        SFOBS(1,51) = NINT(XRD2 * 10.0)

      ENDIF

      RETURN

C-----------------------------------------------------------------------
C- Processing continues here if there is an error decoding the string

20001 CONTINUE
      ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,395) SFGHR
 395  FORMAT(' Internal read error for CD144 data for HR: ',I2)
      CALL ERRHDL(JJJ,PATH,'E43',LOC,MESS)

      RETURN
      END

