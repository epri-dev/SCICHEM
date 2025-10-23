      SUBROUTINE MPPBL( ISTAT )
C=======================================================================
C          MPPBL Module of the AERMET Meteorological Preprocessor
C
C     Purpose:
C
C     This subroutine computes the surface friction velocity (USTAR),
C     heat flux (HFLUX), and planetary boundary layer height (PBLHT) for
C     the convective boundary layer and stable boundary layer for
C     each hour of each day using solar elevation angle, surface
C     temperature, temperature profiles, bowen ratio, cloud cover
C     fraction, wind speed and wind direction.
C
C     For the convective boundary layer (CBL), the incoming solar
C     radiation is computed from the solar elevation angle and corrected
C     for cloud cover.  From this and the Bowen ratio representative of
C     surrounding environment, the heat flux is determined (Holtslag and
C     van Ulden).  Next, an iterative procedure is used to obtain USTAR
C     and the Monin-Obukhov length (MOL).
C
C     For the stable boundary layer (SBL), a THETA-STAR is calculated
C     from cloud cover (van Ulden and Holtslag), where THETA-STAR
C     is the temperature scale used for profiling.  The routine
C     determines whether the solution for USTAR will be real- or
C     complex-valued.  If it is a real solution, the Venkatram solution
C     is used.  A linear interpolation developed by van Ulden and
C     Holtslag is used for complex-valued solutions (a critical USTAR
C     is defined by the variable CHEK).  The friction velocity is then
C     found and is substituted into the heat flux formula to find the
C     surface heat flux (HFLUX).
C
C     The CBL height is found from the Carson-Weil-Brower method.  Carson's
C     method is based on a one-dimensional (with ht.) energy balance approach
C     in which the heat flux into the cbl at the surface and entrained from
C     the stable air aloft leads to vertical mixing, a rise in the base of the
C     elevated inversion, and an increase of the energy of the boundary
C     layer air.  The original Carson model is based on an initial (early
C     morning) potential temperature profile that is assumed to be linear
C     with height.  Weil and Brower extended Carson's model to an arbitrary
C     initial temperature distribution with height and allowed for
C     stress-induced mixing at the top of the boundary layer.
C
C
C     Called by: MODEL
C
C     Calls to:  NR_ANG, RHOCAL, INCRAD, NETRAD, BULKRI, UCALST,
C                HEAT, UCALCO
C
C                SUBST, SBLHT, SMTHZI, XTNDUA, PTAREA, SUMHF,      ! AERMET only
C                MIDNITE, INTHF, CBLHT, PTGRAD, SFCCH              ! AERMET only
C
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C     Modified (Draft) 11/07/2006
C        Added checks for missing cloud cover and/or temperature before
C        calling subroutine NR_ANG, and assign hardcoded missing indicators.
C        Modified check for HFLUX < 0 for CBL to check for HFLUX </= 0.
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      REAL    RMISS(51)

      INTEGER JMPDATE, MHR,JLEV,ITIME,ILVLS,IHR, IFST, ILST, INITT
      INTEGER KK,IGMT,KK60,NUM,ILSTM,JJ
      INTEGER KLEV,IW,JULIAN, UAJDY, HFTEST, ITEST
      INTEGER SDG2USE(-12:12), CHRNDSDG, CHRNDSDGHR,
     &        CHRNDTODAY, CHRNDPREV, CHRONDIFF
      INTEGER START_WINDOW, END_WINDOW, MyZone
      
      INTEGER J, ISEC, JULDAY, NUALEV, NLEVEL
      
      INTEGER ISTAT

      DOUBLE PRECISION  AA, BB, CC
      
      REAL    GRAV, VONK, RAD2DEG, BETAM, AVGANG, ALDO, DELT, Z1, Z2
      REAL    T1, CDN, HLIM, XLIMIT, USTROUT, SKYFRACT
      REAL    ZISMTH, BASEHT, SDGTOP, THETAZ, DENSITY, ZI2USE, RHOMIN
      
      REAL    CHRND_UASRISE
      REAL    SHEAT, OLDH, RHOCP, PTMAX, HAREA, B1
      REAL    ANGLE, CP, DTHDZ, PBLHT, ACRIT
      REAL    SURF(3)   !  Used for average surface characteristics
                        !  Averaging is across sectors

      INTEGER, PARAMETER :: INC=15
      REAL, PARAMETER    :: CAPA=0.2

      CHARACTER*3 DAYNIGHT(24)
      LOGICAL XTENDED

c     LOGICAL GOTSOL, GOTNET, GOTSKY, GOTTMP, GOTSTP, DAYTIME, GOTWND
      LOGICAL GOTSOL, GOTNET,                 GOTSTP, DAYTIME

      DATA    SDG2USE/-12,12,12,12,12,12,12,12,12,0,0,0,0,0,0,0,0,0,0,0,
     &                -12,-12,-12,-12,-12/
     
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP2.INC'
C.......................................................................

      PATH = 'METPREP '
      LOC  = ' MPPBL'

C---- Initialize UA windows variables
      START_WINDOW = -99
      END_WINDOW   = -99
      MyZone       = -99

C---- Initialize ISTAT variable to 2, i.e., no errors
      ISTAT = 2

C---- Initialize constants
C     CP   = specific heat of air at constant pressure
C     GRAV = acceleration due to gravity (m/(sec*sec))
C     VONK = von Karman constant

      CP      = 1004.0
      GRAV    = 9.80655
      VONK    = 0.4

      IF( ADJ_USTAR )THEN
         BETAM = 4.7
      ELSE
         BETAM = 5.0            
      ENDIF

      RAD2DEG = 57.2958

      NO_SKY  = 99       ! Initialize missing value flag for cloud cover
      N_CALM  = 0
      N_VARWD = 0

C---- Assign values to RMISS used to check for missing data
      RMISS( 7) = OSQA( 7,2)     ! INSO
      RMISS( 8) = OSQA( 8,2)     ! NRAD
      RMISS(32) = SFQA(32,2)     ! PRES

      RMISS( 9) = OSQA( 9,2)     ! DT01

      RMISS( 1) = OSQA( 1,2)     ! HFLX
      RMISS( 2) = OSQA( 2,2)     ! USTR
      RMISS( 3) = OSQA( 3,2)     ! MHGT

C---- Initalize the arrays for this 24-hour period.
      DO IW = 1, 24

         PBL(IW)    = '   '

C----    Initialize variables based on default missing codes
         HFLUX(IW)  =  -999.0   ! HFLX
         USTAR(IW)  =  -9.0     ! USTR
         QR(IW)     =  9999.0   ! INSO
         RN(IW)     =  999.0    ! NRAD
         P(IW)      =  99999.0  ! PRES
         T(IW)      =  999.0    ! DRYB
         RH(IW)     =  999.0    ! RHUM
         WDIR(IW)   =  999.0    ! WDIR
         WSPD(IW)   =  999.0    ! WSPD

         CCVR(IW)   =  99

         RHO(IW)    = -9.0
         ZIMECH(IW) = -999.0
         ZICONV(IW) = -999.0
         MOL(IW)    = -99999.0
         WSTAR(IW)  = -9.0
         VPTG(IW)   = -9.0
         BOWEN(IW)  = -9.0
         ALBEDO(IW) = -9.0
         Z0(IW)     = -9.0
         THSTAR(IW) = -9.0

C----    Initialize wind and temp reference heights to -9
         ZREF(IW)   = -9.0
         ZTREF(IW)  = -9.0

      ENDDO


C---- Compute the 6-digit date, YYMMDD
      JMPDATE = MPYR*10000 + MPCMO*100 + MPCDY


c     If there are no onsite data [IWORK1(22) = 0] then check for availability
c     of NWS data [assuming NWS substitution is active].  If there are no NWS
c     data [IWORK1(21) = 0] or if NWS substitution is inactive then write a
c     warning indicating that surface data are not available for the day being
c     processed.

C     IWORK1(21) = # of hours during the day for which there are
C                  NWS surface data

C     IWORK1(22) = # of hours during the day for which there are
C                  onsite data

      IF( IWORK1(22) .GT. 0 )THEN                   !  Have onsite data
         CONTINUE

      ELSEIF( SUBSTNWS .AND. IWORK1(21) .GT. 0 )THEN  !  Have NWS data
         CONTINUE

      ELSE                                          !  No data available
C        Initialize logical variables and arrays to .FALSE.
         GOTSOL = .FALSE.     ! Logical for insolation      7 INSO
         GOTNET = .FALSE.     ! Logical for net radiation   8 NRAD
         GOTSKY = .FALSE.     ! Logical for sky cover      34 TSKY
         GOTTMP = .FALSE.     ! Logical for dry-bulb       46 DRYB   21 TTnn
         GOTSTP = .FALSE.     ! Logical for sta. pressure  32 PRES
         GOTWND = .FALSE.     ! Logical for wind
                              !    wind direction          50 WDIR   22 WDnn
                              !    wind speed              51 WSPD   23 WSnn
         HRWINDOS   = .FALSE.
         HRWINDNWS  = .FALSE.
         HRWINDASOS = .FALSE.

         MESS =  BLNK80
         ECODE='I71'
         WRITE(MESS, 1010) MPJDY
         CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
         RETURN

      ENDIF


      HOUR_LOOP: DO IHR = 1, 24

         GOTSOL = .FALSE.     ! Logical for insolation      7 INSO
         GOTNET = .FALSE.     ! Logical for net radiation   8 NRAD
         GOTSKY = .FALSE.     ! Logical for sky cover      34 TSKY
         GOTTMP = .FALSE.     ! Logical for dry-bulb       46 DRYB   21 TTnn
         GOTSTP = .FALSE.     ! Logical for sta. pressure  32 PRES
         GOTWND = .FALSE.     ! Logical for wind
                              !    wind direction          50 WDIR   22 WDnn
                              !    wind speed              51 WSPD   23 WSnn

C        Call routine that searches for data in the surface and onsite
C        obsevation arrays.

         CALL SUBST(ISTAT, IHR)
         
         IF( ISTAT .EQ. 1 )THEN
            MESS =  BLNK80
            ECODE = 'E79'
            WRITE (MESS, 1009) IHR
            CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
 1009       FORMAT(' Error assigning met variables in SUBST for hour: ',
     &                     I3.2)
            RETURN
         ENDIF


         IF( IHR.GT.TSR .AND. IHR.LT.TSS )THEN
            DAYTIME = .TRUE.
            DAYNIGHT(IHR) = ' D '
         ELSE
            DAYTIME = .FALSE.
            DAYNIGHT(IHR) = ' N '
         ENDIF

         QR(IHR) = OSSOBS(IHR,7)
         IF( ABS(QR(IHR) - RMISS(7)) .GT. 0.01 )THEN        ! Have insolation
            GOTSOL = .TRUE.
         ENDIF

         RN(IHR) = OSSOBS(IHR,8)
         IF( ABS(RN(IHR) - RMISS(8)) .GT. 0.01 )THEN        ! Have net radiation
            GOTNET = .TRUE.
         ENDIF

C        GOTSKY and GOTTMP are set in subroutine SUBST

         IF( ABS(P(IHR) - RMISS(32)) .GT. 0.01 )THEN        ! Have station pressure
            GOTSTP = .TRUE.
         ENDIF

c        Determine surface characteristics (i.e., roughness, albedo, and
c        bowen ratio).  The surface characteristics are wind direction
c        dependent so first we need to check if we have a valid wind
c        direction (ie., a valid non-calm condition).

         IF( .NOT.GOTWND .OR. CALM(IHR) .OR. VARWD(IHR) )THEN
C           No wind direction or a calm or a variable wind -
C           Use average (over all sectors) surface characteristics 
C           for the month; 
C           if there is site-specific met data, use the primary
C           surface characteristics associated with those data;
C           however, if off-site (SURFACE or 1-min ASOS) wind
C           data are substituted for missing or calm on-site winds,
C           then use surface roughness from secondary location

            DO J = 1,3
               SURF(J) = 0.0
               DO ISEC = 1, OSNWDS
                  SURF(J) = SURF(J) + OSSFC(MPCMO,ISEC,J)
               ENDDO
               SURF(J) = SURF(J)/OSNWDS
            ENDDO

            IF( .NOT.OSDATA .OR. .NOT.GOTWND )THEN 
C              No on-site data available or no wind data for this hour;
C              use primary surface roughness, already calculated above
               CONTINUE

            ELSEIF( OSDATA .AND. CALM(IHR) .AND. HRWINDOS(IHR) )THEN 
C              On-site calm hour, use primary (on-site) surface roughness,
C              already calculated above
               CONTINUE

            ELSEIF( OSDATA .AND. (VARWD(IHR) .OR.
     &                       (CALM(IHR) .AND. .NOT.HRWINDOS(IHR))) )THEN
C              Off-site variable wind has been substituted for
C              missing or calm on-site wind data, or off-site calm
C              hour has been substituted for missing on-site wind;
C              use secondary surface roughness 

               IF( HRWINDNWS(IHR) .OR. HRWINDASOS(IHR) )THEN
C                 Use secondary surface characteristics for the average
C                 Only needed for surface roughness length
                  SURF(3) = 0.0
                  DO ISEC = 1, OSNWDS2
                     SURF(3) = SURF(3) + OSSFC2(MPCMO,ISEC,3)
                  ENDDO
                  SURF(3) = SURF(3)/OSNWDS2

               ELSE
C                 This "error" condition should not happen
                  MESS =  BLNK80
                  ECODE='E90'
                  WRITE(MESS, 1015)IHR
                  CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
 1015             FORMAT(' Error getting surface roughness for hour ',
     &                     I3.2,'; possible programming flaw.')
                  ISTAT = 1
                  RETURN
               ENDIF

            ENDIF


            IF( CALM(IHR) )THEN
               N_CALM = N_CALM + 1
            ENDIF
            
            IF( VARWD(IHR) )THEN
               N_VARWD = N_VARWD + 1
            ENDIF

            ALBEDO(IHR) = SURF(1)
            BOWEN(IHR)  = SURF(2)
            Z0(IHR)     = SURF(3)

         ELSE
C           There is a valid wind direction
C           If there is site-specific met data, use the
C           surface characteristics associated with those data

C           The primary chracateristics are always used for the albedo
C             and Bowen ratio; the primary or secondary characteristics
C             are used for the roughness length, depending on the
C             presence or absence of site-specific and 1-min ASOS or NWS
C             hourly observations

            CALL SFCCH(IHR, WDIR(IHR), ITEST)

            IF( HRWINDNWS(IHR) .OR. HRWINDASOS(IHR) )THEN
C              Need to check if site-specific met data were included
               IF( OSDATA )THEN
                  CALL SFCCH2(IHR, WDIR(IHR), ITEST)
               ENDIF
            ENDIF

            IF( ITEST .EQ. -1 )THEN                                      ! rwb #519  06341
                                                                         ! rwb #519  06341
C              Error in determining sector for surface                   ! rwb #519  06341
C              characteristics.  Normally this should                    ! rwb #519  06341
C              not happen.  Cycle loop to next hour.                     ! rwb #519  06341
                                                                         ! rwb #519  06341
               CYCLE HOUR_LOOP                                           ! rwb #519  06341
                                                                         ! rwb #519  06341
            ENDIF                                                        ! rwb #519  06341

         ENDIF

c        Calculate the solar elevation at which the net radiation is
c        theoritically zero (ACRIT) and compare this to the actual
c        solar elevation; we assign the hour to the CBL bin if the
c        solar elevation is .GE. ACRIT, otherwise the hour is assigned to
c        the SBL bin.  As necessary, missing values for cloud cover and
c        temperature are replaced locally with fixed values (5 and 288,
c        respectively); the missing value flags are retained in the global
c        variables.

C        Compute the average of the solar elevation angles at the
C        beginning and end of the hour.  Check for the 1st hour.

         IF( IHR.GT.1 )THEN
            AVGANG = (ANG(IHR-1)+ANG(IHR))/2.0
         ELSE
            AVGANG = (ANG(24)+ANG(1))/2.0
         ENDIF

C        Convert angle from radians to degrees
         ANGLE = AVGANG * RAD2DEG
         ANGD(IHR) = ANGLE

         IF( GOTSOL )THEN
c           If we have measurments of solar radiation, compute an
c           equivalent sky cover, based on incoming radiation.
c           The equivalent sky cover replaces the observed sky cover
C           and is used in subsequent calculations such as net radiation.

            IF( ANGLE.GT.0. .AND. QR(IHR).GT.0. )THEN                    ! ! rwb400 04205
C              Calculate equivalent sky cover from radiation measurement ! ! rwb400 04205
               CALL EQ_CCVR(qr(ihr), angle, ccvr(ihr))                   ! ! rwb400 04205
               GOTSKY = .TRUE.                                           ! ! rwb400 04205
            ENDIF                                                        ! ! rwb400 04205

         ENDIF

c        Calculate the the critical solar elevation angle (ACRIT);       ! ! rwb400 04205
c          This is performed iteratively since ACRIT is a function of    ! ! rwb400 04205
c          ALBEDO, which is a function of solar angle.                   ! ! rwb400 04205
c          Pass unadjusted value of ALBEDO as ALDO                       ! ! rwb400 04205

         ALDO = ALBEDO(IHR)                                              ! ! rwb400 04205

c        Check for missing sky cover or temperature and pass hardwired
c        missing codes.
         IF( GOTSKY .AND. GOTTMP )THEN                                   ! ! rwb??? 06341
            CALL NR_ANG(ALDO, angle, ccvr(ihr), t(ihr), acrit)           ! ! rwb400 04205
         ELSEIF( GOTSKY )THEN
            CALL NR_ANG(ALDO, angle, ccvr(ihr), -999., acrit)            ! ! rwb400 04205
         ELSEIF( GOTTMP )THEN
            CALL NR_ANG(ALDO, angle, 99, t(ihr), acrit)                  ! ! rwb400 04205
         ELSE
            CALL NR_ANG(ALDO, angle, 99, -999., acrit)                   ! ! rwb400 04205
         ENDIF

         ACRT(IHR) = ACRIT

C        Adjust the albedo for later use using the average solar angle:

         B1 = 1.0 - ALBEDO(IHR)

         IF( ANGLE .LE. 0.0 )THEN      !  Set the nighttime albedo to 1.0

            ALBEDO(IHR) = 1.0

         ELSE                          !  Adjust albedo for solar elevation

            ALBEDO(IHR) = ALBEDO(IHR) + B1*EXP(-0.1*ANGLE+(-0.5*B1*B1))      ! Eq. (3)
         ENDIF

c        At this point we have sufficient information to assign
c        this hour to the CBL or SBL bin

         CBL(IHR) = .FALSE.
         PBL(IHR) =  'SBL'

         IF( ANGLE .GE. ACRIT )THEN
            CBL(IHR) = .TRUE.
            PBL(IHR) =  'CBL'
         ENDIF

c        Check for valid temperature data.  We require a valid
c        temperature to proceed beyond this point.

         IF( .NOT. GOTTMP )THEN          ! Missing temperature
            MESS =  BLNK80
            ECODE='I71'
            WRITE(MESS, 1020)IHR
            CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)

            CYCLE HOUR_LOOP
         ENDIF

c        Check for valid pressure data.  We require a valid station
c        pressure to proceed beyond this point.

         IF( .NOT. GOTSTP )THEN          ! Missing station pressure
            MESS =  BLNK80
            ECODE='I71'
            WRITE(MESS, 1022)IHR
            CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)

            CYCLE HOUR_LOOP
         ENDIF


         CALL RHOCAL( P(IHR),T(IHR),RHO(IHR) )   ! Calculate air density

C ---    Assign minimum density based on station pressure,
C        to avoid extraneous messages for high elevation sites.
         IF( P(IHR) .GE. 800.0 )THEN
            RHOMIN = 0.90
         ELSE
            RHOMIN = 0.75
         ENDIF

c        If the density is out of bounds, write a warning.
         IF( (RHO(IHR) .LT. RHOMIN)  .OR.  (RHO(IHR) .GT. 1.5) )THEN
            MESS =  BLNK80
            ECODE = 'W78'
            WRITE(MESS, 1070) RHO(IHR),IHR
            CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
         ENDIF


c        Proceed with processing using stable boundary layer (SBL)
c        algorithms if the solar elevation angle is < ACRIT (CBL =
c        .FALSE.);  otherwise, use convective boundary layer (CBL)
c        algorithms.

         IF( .NOT. CBL(IHR) )THEN

C-------    Stable boundary layer (SBL) processing

c           We require valid non-calm conditions to proceed with SBL
c           estimates

            IF( .NOT. GOTWND .OR. CALM(IHR) )THEN          ! No wind direction
               MESS =  BLNK80
               ECODE='I71'
               WRITE(MESS, 1030)IHR
               CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
               CYCLE HOUR_LOOP
            ENDIF

C           Compute the friction velocity (USTAR), Monin-Obukhov length (L),
c           and heat flux (HFLUX).

            DELT = OSSOBS(IHR, 9)        !  Delta-T measurement
            IF( BULKRN .AND. (ABS(DELT - RMISS(9)) .GT. 0.01) 
     &                                        .AND. HRWINDOS(IHR) )THEN   ! Use Bulk Ri algorithm, 
                                                                          ! only if onsite winds
                                                                          ! are available

c              Compute the VERTICAL potential temperature gradient (PTG)
c              in deg C per m.

               Z1 = OSLL(1)              !  Lower level for Delta-T
               Z2 = OSUL(1)              !  Upper level for Delta-T

               T1 = T(IHR)

c              DTDZ = DELT/(Z2-Z1)       !  temperature gradient
c              PTG = DTDZ + 0.0098       !  potential temperature gradient

               CALL BULKRI(IHR, ANGLE, ACRIT, DELT, T1, Z1, Z2)          ! ! rwb400 04205
c              ustar and thstar are passed in common block /mpsfc1/
               IF( USTAR(IHR).NE.-9. .AND. THSTAR(IHR).NE.-9. )THEN

C                 For the case of strong winds, the product USTAR*THSTAR,
C                 and thus HFLUX, may become unrealistically large (negative).
C                 To avoid this, the heat flux (HLIM) is limited to -64 W/m**2.

C                 XLIMIT is the kinematic heat flux (m/s * K)

                  CDN = VONK / (ALOG(ZREF(IHR) /Z0(IHR) ) )
                  HLIM = -64.
                  XLIMIT = -HLIM / (RHO(IHR)*CP)
                  IF( (USTAR(IHR) * THSTAR(IHR)) .GT. XLIMIT )THEN
                     AA = DBLE( -CDN * WSPD(IHR) )
                     BB = 0.0D0
                     CC = DBLE( BETAM*ZREF(IHR) * GRAV * XLIMIT * CDN /
     &                                                          T(IHR) )
                     CALL CUBIC ( AA, BB, CC, USTROUT )                  ! ! rwb400 04205

                     IF( USTROUT .NE. -9. )THEN      ! Recalculate THSTAR  ! ! rwb400 04205
                        USTAR(IHR)  = USTROUT                            ! ! rwb400 04205
                        THSTAR(IHR) = XLIMIT / USTAR(IHR)
                     ELSE    ! Keep USTAR, and Recalculate THSTAR to give H = HLIM
C
                        THSTAR(IHR) = -HLIM / (RHO(IHR)*CP*USTAR(IHR))   ! ! rwb400 04205
                     ENDIF                                               ! ! rwb400 04205
                  ENDIF
               ENDIF

C              Calculate equivalent cloud cover from THSTAR if missing, used for deposition
               IF( .NOT. GOTSKY )THEN                                    ! ! rwb400 04205
                  IF( THSTAR(IHR) .GE. 0.09 )THEN                        ! ! rwb400 04205
                     CCVR(IHR) = 0                                     ! ! rwb400 04205
                  ELSE                                                 ! ! rwb400 04205
                     SKYFRACT = SQRT((1.-THSTAR(IHR)/0.09)/0.5)        ! ! rwb400 04205
                     IF( SKYFRACT .LE. 0. )THEN                          ! ! rwb400 04205
                        CCVR(IHR) = 0                                  ! ! rwb400 04205
                     ELSEIF( SKYFRACT .GE. 1. )THEN                      ! ! rwb400 04205
                        CCVR(IHR) = 10                                 ! ! rwb400 04205
                     ELSE                                              ! ! rwb400 04205
                        CCVR(IHR) = NINT(SKYFRACT*10.)                 ! ! rwb400 04205
                     ENDIF                                             ! ! rwb400 04205
                  ENDIF                                                ! ! rwb400 04205
                  GOTSKY = .TRUE.                                      ! ! rwb400 04205
               ENDIF                                                   ! ! rwb400 04205

            ELSE
C ---          Issue message if BULKRN not used due to missing onsite winds
               IF( BULKRN .AND. (ABS(DELT - RMISS(9)) .GT. 0.01)
     &                                   .AND. .NOT.HRWINDOS(IHR) )THEN   ! Bulk Ri algorithm not used due to 
                                                                         ! lack of onsite winds, issue message
                  MESS =  BLNK80
                  ECODE = 'I86'
                  WRITE(MESS, 1074)IHR
                  CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
               ENDIF

c              The alternative procedures for the SBL both require cloud
c              cover so we check for the availability of cloud cover first.

               IF( .NOT. GOTSKY )THEN         !  Missing cloud cover
                  MESS =  BLNK80
                  ECODE = 'I78'
                  WRITE(MESS, 1075)IHR
                  CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
                  CYCLE HOUR_LOOP

               ELSE          !  Have cloud cover, proceed with alternatives

C ---             Check for observed USTAR (OSSOBS(IHR,2))
                  IF( ABS(OSSOBS(IHR,2)-RMISS(2)) 
     &                                          .LT. 0.01 )THEN     
C ---                No observed USTAR available, use Holtslag-van Ulden method

                     CALL UCALST(IHR, ANGLE, ACRIT)        ! Return Ustar and THstar

                  ELSE                                     
C ---                Use onsite u*

                     USTAR(IHR)  = OSSOBS(IHR, 2)

                     THSTAR(IHR) = 0.09 * (1.0 - 0.5*( (CCVR(IHR)/10.0)        ! Eq. (24)
     &                             **2))

                  ENDIF

                  IF( ABS( USTAR(IHR) - (-9.))  .GT. 0.01 .AND. 
     &                ABS( THSTAR(IHR) - (-9.)) .GT. 0.01 )THEN

C                    For the case of strong winds, the product USTAR*THSTAR,
C                    and thus HFLUX, may become unrealistically large (negative).
C                    To avoid this, the heat flux (HLIM) is limited to -64 W/m**2.

C                    XLIMIT is the kinematic heat flux (m/s * K)

                     IF( ADJ_USTAR )THEN
                        CDN = VONK/
     &                         ( ALOG((ZREF(IHR)-5.*Z0(IHR))/Z0(IHR)) )
                        HLIM = -64.
                        XLIMIT = -HLIM / (RHO(IHR)*CP)
                        IF( (USTAR(IHR) * THSTAR(IHR)) .GT. XLIMIT )THEN
                           AA = DBLE( -CDN * WSPD(IHR) )
                           BB = 0.0D0
                           CC = DBLE( BETAM*ZREF(IHR)*GRAV*XLIMIT*CDN /
     &                                                         T(IHR) )
                           CALL CUBIC ( AA, BB, CC, USTROUT )                 ! rwb #400 04205

                           IF( ABS( USTROUT - (-9.)) .GT. 0.01 )THEN      ! Recalculate THSTAR ! rwb #400 04205
                              USTAR(IHR)  = USTROUT                           ! rwb #400 04205
                              THSTAR(IHR) = XLIMIT / USTAR(IHR)
                           ELSE    ! Keep USTAR, and Recalculate THSTAR to give H = HLIM
C
                              THSTAR(IHR) = -HLIM /
     &                                       (RHO(IHR)*CP*USTAR(IHR))   ! rwb #400 04205
                           ENDIF                                              ! rwb #400 04205
                        ENDIF

                     ELSE
C ---                   Use default method
                        CDN = VONK / (ALOG(ZREF(IHR) /Z0(IHR) ) )
                        HLIM = -64.
                        XLIMIT = -HLIM / (RHO(IHR)*CP)
                        IF( (USTAR(IHR) * THSTAR(IHR)) .GT. XLIMIT )THEN
                           AA = DBLE( -CDN * WSPD(IHR) )
                           BB = 0.0D0
                           CC = DBLE( BETAM*ZREF(IHR)*GRAV*XLIMIT*CDN /
     &                                                         T(IHR) )
                           CALL CUBIC ( AA, BB, CC, USTROUT )           ! rwb #400 04205

                           IF( ABS( USTROUT - (-9.)) .GT. 0.01 )THEN      ! Recalculate THSTAR ! rwb #400 04205
                              USTAR(IHR)  = USTROUT                           ! rwb #400 04205
                              THSTAR(IHR) = XLIMIT / USTAR(IHR)
                           ELSE    ! Keep USTAR, and Recalculate THSTAR to give H = HLIM
C
                              THSTAR(IHR) = -HLIM /
     &                                       (RHO(IHR)*CP*USTAR(IHR))   ! rwb #400 04205
                           ENDIF                                              ! rwb #400 04205
                        ENDIF
                     ENDIF
                  ENDIF

               ENDIF

            ENDIF

c           Recheck to make sure we still have valid/non-missing values for
c           USTAR and THSTAR.

            IF( USTAR(IHR).NE.-9. .AND. THSTAR(IHR).NE.-9. )THEN

               MOL(IHR)   = T(IHR) * USTAR(IHR) * USTAR(IHR) /
     &                   ( VONK * GRAV * THSTAR(IHR) )                  ! Eqs. (8 & 25)

               HFLUX(IHR)  = -THSTAR(IHR)*RHO(IHR)*CP*USTAR(IHR)        ! Eq. (25)

            ELSE
               MOL(IHR)    = -99999.

               HFLUX(IHR)  = -999.

            ENDIF

c           Back-calculate the net radiation (from the heat flux
c           and bowen ratio) if it is missing.

            if( .not. gotnet  .and. hflux(ihr).ne.-999. )THEN
               rn(ihr) = hflux(ihr)*(1.0+1.0/bowen(ihr))/0.9         ! Eq. (1)
            ENDIF

c           Check the net radiation, we expect a negative value;
c           if not, write an informational message.

            IF( GOTNET .AND. RN(IHR) .GT. 0.0 )THEN
               MESS =  BLNK80
               ECODE = 'I77'
               WRITE(MESS, 1080) RN(IHR), IHR
               CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
            ENDIF

         ELSEIF( CBL(IHR) )THEN    
C
C-------    Convective boundary layer (CBL) processing
C
C           If the net radiation is missing, then attempt to calculate it
C           from temperature, fractional cloud cover, and solar insolation.

            IF( .NOT. GOTNET )THEN

               IF( GOTSOL .AND. GOTSKY )THEN   ! We have insolation and sky cover
c                 Calculate net radiation

                  CALL NETRAD(JMPDATE,IHR)
                  GOTNET = .TRUE.

               ELSEIF( GOTSKY )THEN            ! We have sky cover only
c                 Calculate solar insolation from solar elevation, fractional
c                 cloud cover, and albedo; then calculate net radiation

                  CALL INCRAD(JMPDATE,IHR, ANGLE, RMISS(7) )
                  GOTSOL = .TRUE.

                  CALL NETRAD(JMPDATE,IHR)
                  GOTNET = .TRUE.

               ELSE                            ! Missing cloud cover
                  MESS =  BLNK80
                  ECODE='I71'
                  WRITE(MESS, 1022)IHR
                  CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
                  CYCLE HOUR_LOOP

               ENDIF

            ENDIF

            IF( GOTNET )THEN
               CALL HEAT(IHR)
            ENDIF

C           Check the heat flux; for CBL processing, we expect
c           a positive value; issue message if HF </= 0.

            IF( HFLUX(IHR).LE.0.0 .AND. 
     &         ABS( HFLUX(IHR)- (-999.0) ) .GT. 0.01 )THEN
               MESS =  BLNK80
               ECODE = 'I77'
               WRITE(MESS, 1090) HFLUX(IHR), IHR
               CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)

C              Set heat flux to 0.1 W/m^2 and continue
               HFLUX(IHR) = 0.1

            ENDIF

c           At this point we should have sufficient data for use in
c           estimating convective mixing heights.


            IF( .NOT. GOTWND .OR. CALM(IHR) )THEN       ! No wind direction
               MESS =  BLNK80
               ECODE='I71'
               WRITE(MESS, 1030)IHR
               CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
               CYCLE HOUR_LOOP
            ENDIF

            IF( ABS(OSSOBS(IHR,2) - RMISS(2)) .LT. 0.01 )THEN    !  Calculate u*

               CALL UCALCO(IHR)

            ELSE                                      !  Use onsite u*

               USTAR(IHR)  = OSSOBS(IHR,2)

               MOL(IHR)    = -T(IHR) * (USTAR(IHR)**3)*RHO(IHR)*CP /
     &                       (VONK * GRAV * HFLUX(IHR))               ! Eq. (8)
            ENDIF

         ENDIF

C
C------- SBL vs CBL processing completed; mixing height calcs below
C


C        Large absolute values of the Monin-Obkuhov length indicate
c        near-neutral conditions; therefore, we limit the absolute
c        value to 8888.

         IF( MOL(IHR) .NE. -99999. )THEN                        ! Valid MOL

            IF( MOL(IHR) .GT. 8888. )THEN                     ! Neutral/SBL
               MOL(IHR) = 8888.
            ENDIF

            IF( MOL(IHR) .LT. -8888. )THEN                      ! Neutral/CBL
               MOL(IHR) = -8888.
            ENDIF

            IF( ABS(MOL(IHR)) .LT. 1.0 )THEN
               IF( MOL(IHR) .LT. 0.0) MOL(IHR) = -1.0         ! Unstable
               IF( MOL(IHR) .GT. 0.0) MOL(IHR) =  1.0         ! Stable
            ENDIF

         ENDIF


C        Calculate the mechanical mixing height


         IF( USTAR(IHR) .NE. -9. )THEN
            CALL SBLHT(IHR, USTAR(IHR), PBLHT)
         ELSE
            PBLHT = -999.
         ENDIF

         IF( OSMIX )THEN                ! On-site mixing heights are available

            IF( ABS(OSSOBS(IHR,3) - RMISS(3)) .GT. 0.01 )THEN

               IF( MOL(IHR) .GT. 0.0 )THEN     ! Assign onsite value to PBLHT
                                               ! for stable conditions
                  PBLHT = OSSOBS(IHR,3)

               ELSE                            ! Assign onsite value to ZICONV
                                               ! for convective conditions
                  ZICONV(IHR) = OSSOBS(IHR,3)
               ENDIF

            ENDIF

         ENDIF


         IF( PBLHT .GT. SBLMAX )THEN
C ---       Apply upper limit of 4,000m to PBLHT
            PBLHT = SBLMAX
         ENDIF

C        Smooth the mechanical mixing height;
C        the subroutine checks for missing values
C        of PBLHT and USTAR

         CALL SMTHZI( IHR, PBLHT, USTAR(IHR), ZISMTH )
         IF( ZISMTH .GT. 0.0 )THEN
            ZIMECH(IHR) = ZISMTH
         ENDIF

      ENDDO HOUR_LOOP


C               ----------------------------------------
C               Begin convective mixing height estimates
C               ----------------------------------------


C     NOTE: if an on-site mixing height is available, then it was
C           assigned to ZICONV in the DO 100 loop above because it
C           is possible that this section of code is skipped if
C           specific variables are missing for the day;

c     Check for availability of the necessary upper-air data first.
C     If the day matches and there is a sounding within one hour of
C     1200 GMT, then use the sounding.

C     IWORK1(20) is the number of soundings read from the merge file for
C       the day being processed (there will be soundings from the day
C       before and the day of)

      IF( IWORK1(20) .GT. 0 )THEN
         NUM = -1

C        CHRNDTODAY = chronological day of day being processed
C        CHRNDPREV = previous chronological day
         JULDAY = JULIAN( MPYR, MPCMO, MPCDY )
         CALL CHROND(PATH,MPYR,JULDAY,CHRNDTODAY)
         CHRNDPREV = CHRNDTODAY - 1

         IF( .NOT. SUNRISE4UA )THEN
C ---       Use the 00Z/12Z sounding search criteria
C           First assign MyZone based on UA LOCATION longitude
C           to determine appropriate reference sounding.

            IF( INDEX(MPLON,'W') .NE. 0 )THEN
               MyZone = NINT((UAST3LON/15.0) + 0.5)
            ELSEIF( INDEX(MPLON,'E') .NE. 0 )THEN
               MyZone = NINT((UAST3LON/15.0) - 0.5)
            ENDIF
            MyZone = -MyZone              !  AERMET East/West designation backwards
            MySounding = Sdg2Use(MyZone)

cprt            print *,'UAST3LON,MyZone,MySounding'
cprt            print *, UAST3LON,MyZone,MySounding

C ---       Define the start and end windows for the sounding search
C           Dependent on location on the globe

            IF( MySounding .EQ. 12 )THEN
               START_WINDOW = CHRNDTODAY*100 + MySounding +
     &                                                    UAWINDOWBEGIN
               END_WINDOW   = CHRNDTODAY*100 + MySounding +
     &                                                    UAWINDOWEND

            ELSEIF( MySounding .EQ. 0 )THEN
               START_WINDOW = CHRNDPREV*100 + 24 + MySounding +
     &                                                    UAWINDOWBEGIN
               END_WINDOW   = CHRNDTODAY*100     + MySounding +
     &                                                    UAWINDOWEND

            ELSEIF( MySounding .EQ. -12 )THEN
               START_WINDOW = CHRNDPREV*100 + 24 + MySounding +
     &                                                    UAWINDOWBEGIN
               END_WINDOW   = CHRNDPREV*100 + 24 + MySounding +
     &                                                    UAWINDOWEND

            ENDIF

cprt            print *,' 00/12: Start, End window',Start_window, End_window

            DO JJ = 1,IWORK1(20)
C              Convert back to GMT to search for sounding to use
               IGMT = UAHR(JJ) + ZONE
               UAJDY = JULIAN( UAYR(JJ), UAMO(JJ), UADAY(JJ) )
               CALL CHROND(PATH,UAYR(JJ),UAJDY, CHRNDSDG)

C ---          Calculate chronological day for current sounding,
C              for comparison to sounding window
               IF( IGMT .GE. 0 )THEN
                  CHRNDSDGHR = CHRNDSDG*100 + IGMT
               ELSEIF( IGMT .LT. 0 )THEN
C ---             Adjust calculation if IGMT < 0 to keep hour 
C                 digits in CHRNDSDGHR between 0 and 24 
                  CHRNDSDGHR = (CHRNDSDG-1)*100 + 24 + IGMT
               ENDIF

               IF( (CHRNDSDGHR .GE. START_WINDOW)  .AND.
     &             (CHRNDSDGHR .LE. END_WINDOW) )THEN

C                 A sounding before the reference time is preferred, 
C                 but use the first available sounding
C                 within the window after the reference time, if none
C                 are available up to and including the reference time.

C                 Compare current sounding to START_WINDOW
                  CHRONDIFF = CHRNDSDGHR - START_WINDOW + UAWINDOWBEGIN
                  
cprt                  print *,' chrondiff=',chrondiff

                  IF( CHRONDIFF .LE. 0 )THEN
                     NUM = JJ

                  ELSE
                     IF( NUM .EQ. -1 )THEN
                        NUM = JJ
                     ENDIF
                     EXIT
                  ENDIF      ! Time difference
               ENDIF      ! In Window
             ENDDO  ! JJ=1,IWORK1(20)

cprt            IF( num .eq. -1 )THEN
cprt               print *,'NO SOUNDING SELECTED FOR!: ',mpyr,mpcmo,mpcdy
cprt            else
cprt               print *,'SOUNDING SELECTED FOR: ',mpyr,mpcmo,mpcdy
cprt               print *,UAYR(NUM),UAMO(NUM),UADAY(NUM),UAHR(NUM)
cprt            endif
cprt            pause


         ELSE
C           Use sunrise as the sounding search criteria

C           Compute chronological day + hour for sunrise for the day being processed
            CHRND_UASRISE = FLOAT(CHRNDTODAY)*100.0 + UASRISE

cprt            print *, ' UA-ST3 Longitude          = ',uast3lon
cprt            print *, ' UA-ST3 sunrise time       = ',uasrise
cprt            print *, ' '
cprt            print *, ' SFCST3 Longitude          = ',st3lon
cprt            print *, ' SFCST3 sunrise time       = ',tsr
cprt            print *, ' input time zone, chrnd_uasrise= ',zone,
cprt     &                                                   chrnd_uasrise
cprt            pause
            
C ---       With the time of sunrise, compute the search window;
C           truncate CHRND_UASRISE so that START_WINDOW and END_WINDOW 
C           are referenced to the beginning of the hour for sunrise.
C           First check for START_WINDOW extending to previous day.
            IF( (INT(UASRISE) + UAWINDOWBEGIN) .GE. 0 )THEN
               START_WINDOW = INT(CHRND_UASRISE) + UAWINDOWBEGIN
            ELSE 
               START_WINDOW = CHRNDPREV*100 + 24 + 
     &                        INT(UASRISE) + UAWINDOWBEGIN
            ENDIF

C ---       Set END_WINDOW; no need to adjust for end window 
C           extending into next day due to limits appied on window
            END_WINDOW   = INT(CHRND_UASRISE) + UAWINDOWEND

cprt            print *,' SR: Start, End window',Start_window,End_window
cprt            pause

            DO JJ = 1,IWORK1(20)
               IGMT = UAHR(JJ) + ZONE
               UAJDY = JULIAN( UAYR(JJ), UAMO(JJ), UADAY(JJ) )
               CALL CHROND(PATH,UAYR(JJ),UAJDY, CHRNDSDG)

C              This differs from the 00Z/12Z search since we are now
C              looking for LOCAL sunrise
               CHRNDSDGHR = CHRNDSDG*100 + UAHR(JJ)

               IF( (CHRNDSDGHR .GE. START_WINDOW)  .AND.
     &             (CHRNDSDGHR .LE. END_WINDOW) )THEN

C                 A sounding before sunrise is preferred, but the default
C                 window allows one within two hours after sunrise if 
C                 one before sunrise is not available; the UAWINDOW
C                 keyword overrides the default values (-6 and +2)

C----             Use truncated value for CHRND_UASRISE to reference
C                 comparisons to START_WINDOW based on the beginning 
C                 of the hour for sunrise
                  CHRONDIFF = CHRNDSDGHR - INT(CHRND_UASRISE)

cprt                  print *,' chrondiff=',chrondiff
cprt                  pause

                  IF( CHRONDIFF .LE. 0 )THEN
                     NUM = JJ

                  ELSE
                     IF( NUM .EQ. -1 )THEN
                        NUM = JJ
                     ENDIF
                     EXIT
                  ENDIF      ! Time difference
               ENDIF      ! In Window
            ENDDO  ! JJ=1,IWORK1(20)

cprt            IF( num .eq. -1 )THEN
cprt               print *,'NO SOUNDING SELECTED FOR!: ',mpyr,mpcmo,mpcdy
cprt            else
cprt               print *,'SOUNDING SELECTED FOR: ',mpyr,mpcmo,mpcdy
cprt               print *,UAYR(NUM),UAMO(NUM),UADAY(NUM),UAHR(NUM)
cprt            endif
cprt            pause

         ENDIF

cprt         print *, ' Sounding number = ',NUM
cprt         print *, ' '

         IF( NUM .EQ. -1 )THEN
C---------- No suitable soundings for this day; check for ONSITE mixing hts
            IF (.NOT.OSMIX )THEN
               MESS =  BLNK80
               ECODE = 'W73'
               WRITE(MESS, 1130) MPJDY
               CALL ERRHDL(JMPDATE, PATH, ECODE, LOC, MESS)
               GO TO 650
            ELSE
               MESS =  BLNK80
               ECODE = 'IW73'
               WRITE(MESS, 1130) MPJDY
               CALL ERRHDL(JMPDATE, PATH, ECODE, LOC, MESS)
            ENDIF
         ELSE
C---------- Report sounding selected for this day
            MESS = BLNK80
            ECODE = 'I84'
            IF( UAHR(NUM)+ZONE .GE. 0 )THEN
               WRITE(MESS, 1131) UAHR(NUM)+ZONE
            ELSE
               WRITE(MESS, 1132) UAHR(NUM)+ZONE
            ENDIF
            CALL ERRHDL(JMPDATE, PATH, ECODE, LOC, MESS)
         ENDIF

C------- Save the hour and number of levels available.
         MHR  = UAHR(NUM)
         KLEV = UALEV(NUM)

C------- Calculate the potential temperature profile from the
C        temperature/pressure profile; as a precaution, subtract
C        the height of the first level from the height of each
C        level, i.e., convert to height above local ground level
C        (this should have been performed in the extract process
C        (stage 1) but is repeated here in case stage 1 was
C        skipped)

         BASEHT = UAOBS(NUM,1,2)
         NUALEV = 0
         DO JLEV=1,KLEV

C---------- If height (2), temperature(3) or pressure (1) is missing,
C           skip the level; NUM is the NUMth sounding of the day

            IF( UAOBS(NUM,JLEV,1) .NE. UAQA(1,2)  .AND.
     &          UAOBS(NUM,JLEV,2) .NE. UAQA(2,2)  .AND.
     &          UAOBS(NUM,JLEV,3) .NE. UAQA(3,2)  )THEN

               NUALEV = NUALEV + 1
               TMP(NUM,NUALEV)  = FLOAT(UAOBS(NUM,JLEV,3))/10.0 + 273.15
               PR(NUM,NUALEV)   = UAOBS(NUM,JLEV,1)/10.0
               HT(NUM,NUALEV)   = UAOBS(NUM,JLEV,2)
               PTMP(NUM,NUALEV) = TMP(NUM,NUALEV) *
     &                            (1000.0/PR(NUM,NUALEV))**0.2857

c              HT(NUM,NUALEV)   = HT(NUM,JLEV) - BASEHT
               HT(NUM,NUALEV)   = HT(NUM,NUALEV) - BASEHT

cprt               write(*,*) nualev,ht(num,nualev),tmp(num,nualev),
cprt     &                    pr(num,nualev),ptmp(num,nualev)
            ENDIF

         ENDDO

C------- Retain the top of the original sounding and extend
C        the sounding if it is below 5000 meters (defined
C        in MASTER.INC as UATOP)                           ---- CALL XTNDUA

         XTENDED = .FALSE.
         SDGTOP = HT(NUM,NUALEV)
         IF( SDGTOP .LT. UATOP )THEN
            IF( NUALEV .LT. UAML )THEN
               CALL XTNDUA( SDGTOP, NUM, NUALEV, THETAZ, XTENDED )

            ELSE
               MESS =  BLNK80
               ECODE = 'W72'
               WRITE(MESS, 1150) UAML, MPJDY
               CALL ERRHDL( JMPDATE,PATH,ECODE,LOC,MESS)

            ENDIF
         ENDIF

C------- Determine the number of levels in the sounding
         IF( XTENDED )THEN
            NLEVEL = NUALEV + 1
         ELSE
            NLEVEL = NUALEV
         ENDIF

C------- Convert sounding time, HH, to HHMM.  Set up initial
C        conditions for potential temperature, area under the
C        potential temperature profile, and sum of the area under
C        the potential temperature profile.

         ITIME  = MHR * 100
         PTMAX  = PTMP(NUM,1)
         PTA(1) = 0.0
         PTSUM(1) = 0.0


C------- Compute the potential temperature integrals and sum the
C        integrals to get the sum of the area under the potential
C        temperature profile.                              ---- CALL PTAREA
         DO ILVLS = 2,NLEVEL
            CALL PTAREA( NUM,ILVLS,PTMAX )
            PTSUM(ILVLS) = PTSUM(ILVLS-1) + PTA(ILVLS)
Cprt            write(*,*) ILVLS, ': PT, PTArea, PTSUM = ',
Cprt     &                 PTMP(num,ilvls), pta(ilvls),PTSUM(ILVLS)
         ENDDO

C------- Compute the integrated heat flux.                 ---- CALL SUMHF
C        IFST  = first hour of the day when the heat flux
C                is greater than 0.0001 Watts/sq. meter
C        ILST  = hour of the day after sunset when heat flux < 0.0 for
C                2 consecutive hrs
C        INITT = time, in minutes = hour*60 - 30, past midnight when
C                the heat flux first becomes upward ( > 0 ) for the
C                day (e.g., hour 6 becomes 6*60 - 30 = 330 minutes)

         CALL SUMHF( IFST,ILST,INITT, JMPDATE, HFTEST )
         IF( HFTEST .EQ. 1 )THEN
            MESS =  BLNK80
            ECODE = 'W79'
            WRITE(MESS, 1155) MPJDY
            CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
            GOTO 650
         ENDIF

C------- Compute the number of minutes past midnight       ---- CALL MIDNITE
C        ILSTM = time, in minutes, of the last hour of the day when
C                QST > 0.001 ly/hr for 2 consecutive hours

         CALL MIDNITE( ILST*100,ILSTM )

C------- Initialize the cumulative heat flux to 0.0.
         OLDH = 0.0

C------- Integrate in INC-minute increments (see PARAMETER statement in
C        this subprogram for the value of INC).

         DO KK = INITT+INC, ILSTM, INC

C---------- Interpolate the integrated surface heat flux   ---- CALL INTHF
C           to the time KK
            CALL INTHF( KK, SHEAT, OLDH )
            KK60 = KK / 60
            DENSITY = RHO(KK60)

            IF( DENSITY .EQ. -9.0 )THEN    ! Missing density/temperature
               DENSITY = 1.2
            ENDIF

            RHOCP = DENSITY*CP
            HAREA = (SHEAT / RHOCP) * (1.0 + 2.0 * CAPA)
C            print  *, '   MPPBL: KK, KK60, HAREA = ', KK,KK60,HAREA
C---------- Calculate the convective boundary layer height

            PBLHT = -999.0
            CALL CBLHT( KK, NUM, NLEVEL, HAREA, PBLHT )
c           PBLHT returns with the CBL height for this 15-min period

C---------- Determine if the time is "on-the-hour"; if it is and the hour
C           has been assigned to the CBL bin, then set the CBL height for
c           this hour - assign the onsite value, if available, otherwise
c           assign the PBLHT value.

            KK60 = KK / 60

            IF( MOD( KK,60 ) .EQ. 0 )THEN
               KK60 = KK / 60
               IF( CBL(KK60) )THEN
                  IF( ABS(OSSOBS(KK60,3) - RMISS(3)) 
     &                                             .GT. 0.001 )THEN
C------------------- An observed mixing height is available to use
                     ZICONV(KK60) = OSSOBS(KK60,3)

                  ELSE
C------------------- No on-site mixing height; upward heat flux -
C                    convective atmosphere - use calculated value
                     ZICONV(KK60) = PBLHT
                     IF( OSMIX )THEN
C---------------------- Print message informing user that a calculated
C                       value was used because the on-site mixing height,
C                       which is in the data base (OSMIX = true), is missing
                        MESS =  BLNK80
                        ECODE = 'W79'
                        WRITE(MESS, 1160) KK60
                        CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
                     ENDIF

C------------------- If the sounding was extended and the PBLHT is computed
C                    to be above the top of the observed sounding height,
C                    then write a message
                     IF( XTENDED  .AND.  PBLHT .GT. SDGTOP )THEN
                        MESS =  BLNK80
                        ECODE='W75'
                        WRITE(MESS, 1170) PBLHT, KK60
                        CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)

                        MESS =  BLNK80
                        WRITE(MESS, 1180) THETAZ
                        CALL ERRHDL(0,'          ','   ',BLNK08,MESS)
                     ENDIF
                  ENDIF               ! Nonmissing on-site mixing ht
               ENDIF                  ! End for CBL .EQ. .TRUE.

C------------- Limit the convective mixing height to 4000 m
C              (CBLMAX defined in MP2.INC)
               IF( ZICONV(KK60) .GT. CBLMAX )THEN
                  ZICONV(KK60) = CBLMAX
               ENDIF

            ENDIF                     ! IF time is on the hour

         ENDDO

      ELSEIF( .NOT.OSMIX )THEN        ! No upper-air data and no OS mixing height

C------- No upper-air soundings available for this day, and no OSMIX;
C        Cannot calculate convective boundary layer height, write warning.
         MESS =  BLNK80
         ECODE='W73'
         WRITE(MESS, 1130) MPJDY
         CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)

      ENDIF                           ! IF there are upper air data

C---- Compute convective parameters
C           - convective velocity scale, w*
C           - potential temperature gradient above zi, vptg ---- CALL PTGRAD

C     Notes - calm winds are handled properly because the mixing
C             heights are initialized to -999.0 and no computations
C             are made if the winds are calm.
C           - SUBR.PTGRAD is called with the number of levels in the
C             UN-extended sounding (NUALEV).

 650  CONTINUE
 
      DO IHR = 1,24
         IF( MOL(IHR) .LT. 0.0      .AND.
     &      MOL(IHR) .NE. -99999.0  .AND.
     &      RHO(IHR) .GT. 0.0       .AND.
     &      T(IHR) .GT. 0.0         .AND.
     &      T(IHR) .LT. 900.0       .AND.
     &      HFLUX(IHR) .GT. -900.0  .AND.
     &      HFLUX(IHR) .NE. -777.0  .AND.
     &      ZICONV(IHR) .GT. 0.0 )THEN
            WSTAR(IHR) = (GRAV * HFLUX(IHR) * ZICONV(IHR) /
     &                   (RHO(IHR) * CP * T(IHR)))**(0.333)
            IF( WSTAR(IHR) .GT. 0.0  .AND.  WSTAR(IHR) .LT. 0.001 )THEN
               WSTAR(IHR) = 0.001
               MESS =  BLNK80
               ECODE='W76'
               WRITE(MESS, 1200) IHR
               CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
            ENDIF
            ZI2USE = AMAX1( ZICONV(IHR), ZIMECH(IHR) )

C---------- Compute the potential temperature gradient
C           above the convective mixing height             ---- CALL PTGRAD
            CALL PTGRAD ( NUM, NUALEV, SDGTOP, ZI2USE, DTHDZ )
            VPTG(IHR) = DTHDZ
         ENDIF

      ENDDO

      RETURN

 1010 FORMAT(' No surface data for BL calculations on ',
     &        'Julian day: ', I4.3)

 1020 FORMAT(' Missing temperature for hour: ', I3.2)

 1022 FORMAT(' No estimate for insolation due to missing sky cover for',
     &       ' hour: ', I3.2)

 1030 FORMAT(' Calm conditions -  No BL calculations for hour: ', I3.2)

 1040 FORMAT(' Albedo (',F6.2, ') is out or range for hour: ',I3.2)

 1050 FORMAT(' Insolation (',F7.0, ') is out or range for hour: ',I3.2)

 1060 FORMAT(' Net radiation (',F7.0, ') is out or range for hour:',I4)

 1070 FORMAT(' Density (',F7.2, ') is out or range for hour: ',I3.2)

 1074 FORMAT(' BULKRN not used due to missing OS wind for hr: ',I3.2)

 1075 FORMAT(' Sky cover is missing - no SBL estimates for hour: 'I3.2)

 1080 FORMAT(' Net radiation (',F6.1,') invalid for SBL for hour:',I4)

 1090 FORMAT(' Suspect (neg) heat flux (',F6.1,') for CBL - reset to ',
     &       '+0.1 for hour: ', I4)

 1100 FORMAT(' USTAR (',F7.3, ') is out or range for hour: ',I3.2)

 1110 FORMAT(' HFLUX (',F7.0, ') is out or range for hour: ',I3.2)

 1120 FORMAT(' Onsite ZI missing, mechanical ZI computed for hour:',I4)

 1130 FORMAT(' No sounding to calculate convective parameters on ',
     &        'Julian day: ',I4)

 1131 FORMAT(' Upper air sounding selected for this day: ',I2,' Z')

 1132 FORMAT(' Upper air sounding selected from "previous" day: ',
     &                                                     I2,' Z')

 1140 FORMAT(' Insufficient surface data to calculate convective ',
     &       'parameters on Julian day: ', I4)

 1150 FORMAT(' Cannot extend sounding; # levels is ',I3,' on ',
     &        'Julian day:', I5)

 1155 FORMAT(' No convective ZI due to missing heatflux on ',
     &        'Julian day:', I5)

 1160 FORMAT(' Onsite ZI missing; convective ZI computed for hour: ',I4)

 1170 FORMAT(' Estimated convective ZI (',F5.0,') exceeds top of ',
     &       'sounding for hour: ', I4)

 1180 FORMAT(' Sounding was extended with DthetaDZ = ', F8.5)

 1200 FORMAT(' W* < 0.001; w* reset to 0.001 m/s for hour: ', I3.2)


      END

