      SUBROUTINE SUBST (ISTAT, IH)
C=====================================================================
C     SUBST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Position onsite and/or National Weather Service
C               (NWS) data into the appropriate arrays for use in
C               the computations that follow.  The reference height
C               for winds, ZREF, and temperature, ZTREF, are
C               determined from the data.  Sets flags for missing
c               winds, missing temperatures, and calms.  SUBST is
c               called once every hour from MPPBL,
c
C
C     Called By: MPPBL
C
C
C     Input:
C       IH       Integer    Current hour
C       OSVOBS              Array of on-site profile data
C       OSSOBS              On-site scalar data
C       SFOBS               NWS hourly observations
C
C
C     Output:
C
C       WSPD                Wind speed at ZREF
C       WDIR                Wind direction at ZREF
C       ZREF                Reference height for winds
C       T                   Ambient temperature at ZTREF
C       ZTREF               Reference height for temperature
C       RH                  Relative humidity
C       P                   Station pressure
C       CCVR                Cloud cover
C
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C       01/29/93
C         -restructured the code to eliminate a jump into
C          the middle of an IF..THEN..ENDIF block
C
C       02/16/94 (by Russ Lee, EPA)
C         - 1) Changed minimum wind measurement height selected
C              from 5*ZO to 20*ZO,
C         - 2) added a missing statement to increment to the next
C              height to find the appropriate wind measurement height,
C         - 3) moved the call to obtain ZO to a point before
C              it is needed.
C
C       06/06/96 (PES)
C         - Minimum wind measurement height changed from 20*ZO to 7*ZO
C
C       10/18/96 (PES)
C         - 1) Added code to compare on-site wind speed to the
C              threshold value; goes to next level if speed is less
C              than threshold.
C         - 2) Added code to change profile value to 1/2 the
C              threshold for ALL profile levels where the speed
C              is less than the threshold
C         - 3) Added code to let user know if the wind speed reference
C              height (with on-site data only) is below 20*ZO
C         - 4) Changed the call to SFCCH to include an argument
C              for wind direction
C         - 5) Restructured logic for locating the reference
C              level wind and temperature based on the 7/31/96
C              memo from Steve Perry and the meeting with AERMIC
C              on 9/25/96 and miscellaneous phone calls.
C
C       12/29/97  version dated 98022
C         - 1) added the check for minimum wind speed for the reference
C              level;
C
C       01/22/98  finalize AERMET for next 'release'
C         -    minimum sigma_v finalized to 0.2 for version 98022
c              the minimum wind speed = sqrt(2) * sigvmin,
C              (sigvmin is defined in a parameter statement below)
C
C       12/19/00 (version 00357)
C         - if OSHEIGHTS keyword was used, data provided in OSHT used to
C           fill in heights in OSVOBS
c
C       04/13/02 (! dtb127 02113)
C         - Added code to set flags for missing temperatures, missing
C           winds, and calms.  Relocated calls to subroutine sfcch (the
C           routine for processing surface characteristics) to subroutine
C           MPPBL.
C
C       04/15/08
C         - Hierachy of calculations to get station pressure using sea
C           level pressure and station elevation (PWELEV) added
C
C       A comment on surface characteristics
C          A call to subr.SFCCH to define the surface characterisitcs is
C          made only for site-specific winds (if they are in the data).
C          There is NO call to subr.SFCCH for NWS winds.  The reason is
C          that for multiple levels of site-specific data, the surface
C          characteristics are dependent on selecting the 'correct' or
C          'appropriate' reference height.
C
C          The surface characteristics are selected for the reference
C          height (there is only one height for NWS data) in MPPBL.
C
C       12/31/09
C
C---- Local variables --------------------------------------------------
C
C     ZMAX4U  - maximum height in on-site profile for which to search
C               for a reference level for wind
C     ZMAX4T  - maximum height in on-site profile for which to search
C               for a reference level for temperature
C     PCNT2   - the number of times 1013.25 is substituted (nearest 1000
C               occurrences, rounded down)
C     IASOSCNT- number of warnings for 1-minute ASOS wind data period to
C               January 1, 2000; after issuing 24 warnings, message type
C               is changed to informational
C     LTZMAX4U- logical that indicates if the on-site profile level is
C               at or below ZMAX4U
C     LTZMAX4T- logical that indicates if the on-site profile level is
C               at or below ZMAX4T
C     CLMHGT  - height that is used in the AERMET surface output file
C               if the reference wind speed is calm for the hour
C     REFCOEFF- coefficient multiplied by the roughness length to
C               define the lower bound for a wind speed/direction
C               reference level
C     SIGVMIN - minimum sigma-v for use in minimum wind speed for the
C               reference level


      IMPLICIT NONE

      REAL, PARAMETER :: ZMAX4U=100.0, ZMAX4T=100.0, REFCOEFF=7.0
      REAL, PARAMETER :: SIGVMIN = 0.2
C --- Define parameter for truncation adjustment for ASOS wind speeds,
C     ASOSADJ, based on 0.5 knot = 0.257 m/s
      REAL, PARAMETER :: ASOSADJ = 0.257           

      INTEGER   IZREF, IH, TEST, PCNT2, IASOSCNT
      INTEGER   JMPDATE, JMPDAT8, ISTAT, ILEVEL, ILVL, IZTMP
      INTEGER   ITOTAL, IOPAQ, MISTOT, MISOPQ
      INTEGER   L
      REAL      SFSP, CLMHGT, ZBOT
      LOGICAL   LTZMAX4U, LTZMAX4T, GOT_OSTMP

      ! mec 1/2010: Temp ASOS flag used to reset flag based on
      ! substitution that may occur during processing
      CHARACTER*1 ISASOSTMP

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize variables ---------------------------------------------

      DATA  PCNT2/0/, IASOSCNT/0/

      PATH    = 'METPREP'
      LOC     = ' SUBST'

C---- Initialize ISTAT variable to 2, i.e., no errors
      ISTAT = 2

      GOTTMP         = .FALSE.
      GOTWND         = .FALSE.
      GOT_OSTMP      = .FALSE.    ! used of OS temp for RH calc from DewPT and pressure
      HRWINDOS(IH)   = .FALSE.
      HRWINDNWS(IH)  = .FALSE.
      HRWINDASOS(IH) = .FALSE.
      GOTSKY         = .FALSE.
      CALM(IH)       = .FALSE.
      VARWD(IH)      = .FALSE.

      ISASOSTMP = 'N'

      LTZMAX4U  = .TRUE.
      LTZMAX4T  = .TRUE.
      CLMHGT    = -9.0

c     NO_SKY is the missing value flag for cloud cover.  It is passed
c     in common (MP2.INC) to netrad and incrad.

      NO_SKY   = NINT( OSTSKY(2) )   ! Set missing value for cloud cover ! dtb #122 02096
      CCVR(IH) = NO_SKY                                                  ! dtb #122 02096
      QR(IH)   = OSQA(7,2)
      RN(IH)   = OSQA(8,2)

      JMPDATE = MPYR*10000 + MPCMO*100 + MPCDY

C --- Calculate 8-digit year from 6-digit JMPDATE
      IF( JMPDATE .GE. 500000 )THEN
         JMPDAT8 = 19000000 + JMPDATE
      ELSE
         JMPDAT8 = 20000000 + JMPDATE
      ENDIF

C---- Begin processing -------------------------------------------------

      IF ( OSDATA )THEN
C        There are onsite data in the data base, search for usable data

         IF( STATUS(4,16) .GE. 2 )THEN
C           The OSHEIGHTS keyword is in use; the data associated with the
C           keyword will be used to fill the on-site heights array,
C           OSVOBS(hour,level,1), and heights specified within the data
C           using the HTnn code will be ignored.
            DO ILEVEL = 1,OSNL
               OSVOBS(IH,ILEVEL,1) = OSHT(ILEVEL)
            ENDDO
         ENDIF
C                                WINDS
C     ------------------------------------------------------------------
C        Search for the lowest level of nonmissing wind data from the
C        on-site profile such that the wind speed is above a user-
C        defined threshold and is above 7*surface_roughness_length but
C        below 100 meters
C     ------------------------------------------------------------------

C        OSNL = number of levels of on-site data
C        OSVOBS(hour,level,9) = wind speed
C        OSVOBS(hour,level,8) = wind direction
C        OSVOBS(hour,level,1) = profile height
C        IZREF = counter for the profile levels
C        OSCALM = user-specified wind speed threshold (meters/second)

         IZREF = 1
         DO WHILE( (IZREF.LE.OSNL) .AND. (.NOT.GOTWND) .AND. LTZMAX4U )

C---      Modified to check for on-site calm hours first, based on
C         WS only (since WD has already been assigned missing code for
C         calm hours).

          IF( ABS(OSVOBS(IH,IZREF,9)-OSQA(23,2)) .GT. 0.01 .AND.
     &           (OSVOBS(IH,IZREF,9) .LT. OSCALM) )THEN
C            The wind speed is less than the threshold value;
C            save the height for possible calm wind condition
C            (if a height hasn't already been saved for this
C            purpose) and increment the height/level counter.
             IF( CLMHGT .LT. 0.0 )THEN
                CLMHGT = OSVOBS(IH,IZREF,1)
             ENDIF
             IZREF = IZREF + 1

          ELSE

            IF ( ABS(OSVOBS(IH,IZREF,9)-OSQA(23,2)).GT. .01 .AND.
     &           ABS(OSVOBS(IH,IZREF,8)-OSQA(22,2)).GT. .01 .AND.
     &           ABS(OSVOBS(IH,IZREF,1)-OSQA(15,2)).GT. .01 )THEN

C              The winds at this level are not missing or calm;
C              is the profile level below ZMAX4U?

               IF( OSVOBS(IH,IZREF,1) .LE. ZMAX4U )THEN

C                 The height is at or below ZMAX4U;
C                 is the wind speed above threshold?

                  IF( OSVOBS(IH,IZREF,9) .GE. OSCALM )THEN

C------------------- Get the surface characteristics (roughness length,
C                    albedo and Bowen ratio) corresponding to the
C                    wind direction - also checks to be sure this level is
C                    is above the minimum height of 7 times the roughness
C                    length - if not check the next level.

                     CALL SFCCH (IH,OSVOBS(IH,IZREF,8),TEST)

                     IF (TEST .EQ. -1) THEN

C                       Error in determining sector for surface
C                       characteristics.  Normally this should
C                       not happen.  Cycle loop to next level.

                        CYCLE

                     ENDIF

C                    Define the lower bound for the profile height as a
C                    function of the roughness length

                     ZBOT =  REFCOEFF * Z0(IH)        ! refcoeff = 7

C                    The profile height must be AT OR ABOVE this lower
C                    bound

                     IF( OSVOBS(IH,IZREF,1) .GE. ZBOT )THEN
                        GOTWND = .TRUE.
                        HRWINDOS(IH) = .TRUE.
                        WSPD(IH)  = OSVOBS(IH,IZREF,9)
                        WDIR(IH)  = OSVOBS(IH,IZREF,8)
                        ZREF(IH)  = OSVOBS(IH,IZREF,1)
C                       set temp asos flag to indicate these are not asos winds
                        ISASOSTMP = 'N'

C                       Check for a minumum wind speed
                        IF( WSPD(IH) .LT. SQRT(2.0)*SIGVMIN )THEN
                           MESS =  BLNK80
                           ECODE = 'I80'
                           WRITE(MESS,1010) WSPD(IH), IH
                           CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )

                           WSPD(IH) = SQRT(2.0)*SIGVMIN
                        ENDIF

C                       Is this profile height below 20*roughness length

                        IF( ZREF(IH) .LT. (20.0 * Z0(IH) ) )THEN
                           MESS =  BLNK80
                           ECODE = 'I80'
                           WRITE(MESS, 1020) IH
                           CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                        ENDIF

                     ELSE
C                       Measurement height is too low, go to next level
                        IZREF = IZREF + 1

                     ENDIF

                  ENDIF

               ELSE
C                 The profile height is above ZMAX4U
                  LTZMAX4U = .FALSE.

               ENDIF

            ELSE
C              One or more of the pieces of data (WS, WD, Z) are missing
               IZREF = IZREF + 1

            ENDIF

          ENDIF

C        End 'do while' for onsite winds
         ENDDO

         IF( CLMHGT .GT. 0.0  .AND.  .NOT. GOTWND )THEN    !  Calm
C ------    Only valid onsite wind observation is a calm; assign logical
C           flags and values for valid/calm onsite wind for now, which
C           may be overwritten if valid NWS data are substituted below.
            CALM(IH) = .TRUE.
            GOTWND   = .TRUE.
            HRWINDOS(IH) = .TRUE.
            WSPD(IH) = 0.0
            WDIR(IH) = 0.0
            ZREF(IH) = CLMHGT
C           set temp asos flag to indicate these are not asos winds
            ISASOSTMP = 'N'                                 ! mec 1/2010
         ENDIF

C        Now check _all_ the levels of on-site wind speed data:
C        if the wind speed is less than the threshold, set the
C        speed and direction to missing

         DO ILVL = 1,OSNL
            IF( ABS(OSVOBS(IH,ILVL,9) - OSQA(23,2)) .GT. 0.01
     &        .AND. OSVOBS(IH,ILVL,9) .LT. OSCALM )THEN
               OSVOBS(IH,ILVL,9) = OSQA(23,2)
               OSVOBS(IH,ILVL,8) = OSQA(22,2)
            ENDIF
         ENDDO


C                              TEMPERATURE
C     ------------------------------------------------------------------
C        Search for the lowest level of nonmissing temperature data
C        from the onsite profile
C     ------------------------------------------------------------------

C        OSVOBS(hour,level,7) = ambient temperature
C        OSVOBS(hour,level,1) = profile height
C        IZTMP = counter for the profile levels

         IZTMP = 1

         DO WHILE( (IZTMP.LE.OSNL) .AND. (.NOT.GOTTMP) .AND. LTZMAX4T )

            IF( ABS(OSVOBS(IH,IZTMP,7)-OSQA(21,2)).GT. 0.01
     &    .AND. ABS(OSVOBS(IH,IZTMP,1)-OSQA(15,2)).GT. 0.01 )THEN

C              The temperature at this level is not missing;
C              is the profile level above the roughness length but
C              below ZMAX4T?

               IF( OSVOBS(IH,IZTMP,1) .GT. Z0(IH)  .AND.
     &             OSVOBS(IH,IZTMP,1) .LE. ZMAX4T )THEN

C                 The height is below ZMAX4T; convert to kelvins
C                 and save the information
                  GOTTMP    = .TRUE.
                  GOT_OSTMP = .TRUE.
                  T(IH)     = OSVOBS(IH,IZTMP,7) + 273.15
                  ZTREF(IH) = OSVOBS(IH,IZTMP,1)

               ELSE
C                 Measurement height exceeds the maximum allowable
C                 for temperature
                  LTZMAX4T = .FALSE.

               ENDIF

             ELSE            !  height and/or temperature is missing
C               At least one piece of data was missing at this
C               level, so increment the height/level counter.

                IZTMP = IZTMP + 1

             ENDIF

C        End 'do while' for onsite temperature
         ENDDO

      ENDIF                       ! for OSDATA


C     ================================================================++
C     Now check to see if reference winds and temperature were defined;
C     if not, and the user has specified the option to substitute NWS
C     data (with the METHOD REFLEVEL SUBNWS keyword), then make the
C     substitution if there are data to substitute for this day/hour
C     ================================================================++

C     ------------------------------------------------------------------
C                                WINDS
C     ------------------------------------------------------------------

      IF( .NOT. GOTWND .OR. CALM(IH) )THEN
C        A reference level wind was not located with the site-specific
C        winds (either there are no data input or the hour is missing),
C        or the reference site-specific wind is calm.

         IF( SUBSTNWS )THEN
C           The user specified NWS substitution -
C           WARNING: if there are no site-specific data in the file and
C           the user did not specify to substitute NWS data, then there
C           will be no reference winds for the entire period of record

            IF( ASOSOBS(IH,1).GE. 0.0 .AND. ASOSOBS(IH,2).GT.0.0 .AND.
     &          ASOSOBS(IH,1).LT.900. .AND. ASOSOBS(IH,2).LT.900. )THEN

C              If the user has specified a threshold wind speed for the
C              1-minute ASOS data, apply the threshold:
C                1) if the ASOS wind speed is less than the threshold
C                   plus the truncation adjustment if it was specified
C                2) if the ASOS wind speed is less than the threshold
C                   and the truncation adjustment option is not specified
               IF( L_1MINASOS_THRESH )THEN
                  IF (ASOS_ADJ .AND.
     &                ((ASOSOBS(IH,1) + ASOSADJ) .LT. THRESH1SPD) )THEN
                     ASOSOBS(IH,1) = 0.0
                     ASOSOBS(IH,2) = 0.0
                     iCALM1MIN = iCALM1MIN + 1
                  ELSEIF( .NOT. ASOS_ADJ .AND.
     &                    ASOSOBS(IH,1) .LT. THRESH1SPD )THEN
                     ASOSOBS(IH,1) = 0.0
                     ASOSOBS(IH,2) = 0.0
                     iCALM1MIN = iCALM1MIN + 1
                  ENDIF
               ENDIF
            ENDIF

            IF( ASOSOBS(IH,1).GE. 0.0 .AND. ASOSOBS(IH,2).GT.0.0 .AND.
     &          ASOSOBS(IH,1).LT.900. .AND. ASOSOBS(IH,2).LT.900. )THEN
C ---          The hourly averaged 1-min ASOS winds are present and not missing or calm.
C              Note that WSPD may be 0.0 and WDIR non-zero for post-sonic ASOS sites,
C              which represents a valid non-calm observation.  However, if both
C              WSPD and WDIR are zero for 1-min ASOS data this implies a calm hour
C              based on pre-sonic ASOS data; in this case we still may use the standard
C              NWS archived observation if it is non-calm and non-missing.

C ---          Check for 1-min ASOS winds prior to Jan. 2000 (since 1-min data
C              are not availabe prior to that), or prior to ASOS commission date;
C              if on-site winds are missing, issue warning and set
C              1-min ASOS winds to missing if prior to Jan. 2000 or commission date;
C              if on-site winds are calm, issue warning if prior to Jan. 2000 or
C              commission date, but don't set winds to missing.
               IF( .NOT. GOTWND )THEN
C ---             No on-site wind available:
                  IF( JMPDAT8 .LT. 20000000 )THEN
C                    Issue message for 1-min ASOS winds prior to Jan. 2000,
C                    the earliest date for archived 1-min ASOS wind data.
C                    Set reference wind speed and direction to missing and exit
C                    wind data hierarchy.
C                    Increment counter for number of cases to manage the
C                    warning and informational messages.
                     IASOSCNT = IASOSCNT + 1
                     IF( .NOT. L_1minAsos_pre2000 )THEN
C ---                   If flag has not been set for 1-min ASOS data before Jan. 2000
C                       issue warning message; also check for number of warnings
C                       .ge. 24 and set flag so additional messages will be
C                       informational
                        ECODE = 'W98'
                        IF( IASOSCNT .GE. 24 )THEN
                           L_1minAsos_pre2000 = .TRUE.
                        ENDIF
                     ELSE
                        ECODE = 'I98'
                     ENDIF
                     MESS =  BLNK80
                     WRITE (MESS, 1032) IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                     IF( IASOSCNT .EQ. 24 )THEN
C ---                   Write message indicating that additional hours with
C                       ASOS winds prior to Jan. 2000 will be in message file
C                       as informational messages
                        MESS  = BLNK80
                        ECODE = 'W98'
                        WRITE (MESS, 1034)
                        CALL ERRHDL ( 2,PATH,ECODE,LOC,MESS )
                     ENDIF
                     WSPD(IH) = 999.0
                     WDIR(IH) = 999.0
                     ZREF(IH) = -9.0
C ---                Skip to 100 since no 1-min ASOS winds will be used
                     GOTO 100
                  ELSEIF( ISASOS24(IH).EQ.'N' .OR.
     &                    ISASOS24(IH).EQ.'n' .OR.
     &                    (iCommDate .GT. 0 .AND.
     &                     iCommDate .GT. JMPDAT8) )THEN
C                    Issue message for 1-min ASOS winds prior to commission date;
C                    however, use reference wind speed and direction from 1-min
C                    data since it is by definition ASOS.
                     MESS =  BLNK80
                     ECODE = 'W99'
                     WRITE (MESS, 1033) IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                  ENDIF
               ELSEIF( GOTWND .AND. CALM(IH) )THEN
C ---             On-site wind available, but calm:
                  IF( JMPDAT8 .LT. 20000000 )THEN
C                    Issue message for 1-min ASOS winds prior to commission date;
C                    since on-site wind is calm, leave reference wind speed and
C                    direction unchanged, but bypass surface data
C                    Increment counter for number of cases to manage the
C                    warning and informational messages.
                     IASOSCNT = IASOSCNT + 1
                     IF( .NOT. L_1minAsos_pre2000 )THEN
C ---                   If flag has not been set for 1-min ASOS data before Jan. 2000
C                       issue warning message; also check for number of warnings
C                       .ge. 24 and set flag so additional messages will be
C                       informational
                        ECODE = 'W98'
                        IF( IASOSCNT .GE. 24 )THEN
                           L_1minAsos_pre2000 = .TRUE.
                        ENDIF
                     ELSE
                        ECODE = 'I98'
                     ENDIF
                     MESS =  BLNK80
                     WRITE (MESS, 1032) IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                     IF( IASOSCNT .EQ. 24 )THEN
C ---                   Write message indicating that additional hours with
C                       ASOS winds prior to Jan. 2000 will be in message file
C                       as informational messages
                        MESS  = BLNK80
                        ECODE = 'W98'
                        WRITE (MESS, 1034)
                        CALL ERRHDL ( 2,PATH,ECODE,LOC,MESS )
                     ENDIF
C ---                Skip to 100 since no 1-min ASOS winds will be used
                     GOTO 100
                  ELSEIF( ISASOS24(IH).EQ.'N' .OR.
     &                    ISASOS24(IH).EQ.'n' .OR.
     &                    (iCommDate .GT. 0 .AND.
     &                     iCommDate .GT. JMPDAT8) )THEN
C                    Issue message for 1-min ASOS winds prior to commission date;
C                    since on-site wind is calm, leave reference wind speed and
C                    direction unchanged, but bypass surface data
                     MESS =  BLNK80
                     ECODE = 'W98'
                     WRITE (MESS, 1033) IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                  ENDIF
               ENDIF               ! (.NOT. GOTWIND)

C              set temp asos flag to indicate these ARE asos winds
               ISASOSTMP = 'A'

C ---          We have a valid non-calm 1-minute ASOS wind observation;
C              Reset GOTWND to .T.
               GOTWND = .TRUE.
C              Assign 1-min ASOS winds to speed, direction, and zref variables
               WSPD(IH) = ASOSOBS(IH,1)
               WDIR(IH) = ASOSOBS(IH,2)
               ZREF(IH) = INSTHT(1)
C ---          Reset CALM(IH) and HRWINDOS(IH) to .F. in case flags
C              were set for calm onsite data
               CALM(IH) = .FALSE.
               HRWINDOS(IH)  = .FALSE.
C              The flag HRWINDASOS is used to determine which set
C              of surface characteristics is to be used
               HRWINDASOS(IH) = .TRUE.

               IF( OSDATA )THEN   ! 1-min ASOS winds used for this hour
                  MESS =  BLNK80
                  ECODE = 'I81'
                  WRITE (MESS, 1031) IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
               ENDIF

C              If wind is not a calm, apply ASOS adjustment if
c              default has not been over-ridden and check for
c              minimum wind speed

C              0.257 m/s (0.5 knot) is added to adjust wind speed
c              for the truncation if not over-ridden
               IF( ASOS_ADJ )THEN
                  WSPD(IH) = WSPD(IH) + ASOSADJ
               ENDIF

C              If not a calm, check for a minumum wind speed
c              and reset as necessary
               IF( WSPD(IH) .LT. SQRT(2.0)*SIGVMIN .AND.
     &             WSPD(IH) .GT. 0.0 )THEN
                  MESS =  BLNK80
                  ECODE = 'I81'
                  WRITE(MESS, 1041) WSPD(IH), IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                  WSPD(IH) = SQRT(2.0)*SIGVMIN
               ENDIF

            ELSEIF( ( SFOBS(IH,51) .NE. SFQA(51,2) ) .AND.
     &              ( SFOBS(IH,50) .NE. SFQA(50,2) ) )THEN
C              NWS winds are not missing - go ahead and substitute;
C              (INSTHT(1) is the user-specified anemometer height
C              specified on the METPREP pathway with the NWS_HGT
C              keyword)
C              The flag HRWINDNWS will be used to determine which set
C              of surface characteristics is to be used

C              set temp asos flag
               ISASOSTMP = ISASOS24(IH)

C              Calm winds: If either speed or direction are set to zero
C              set CALM flag and set both to zero.
               IF( SFOBS(IH,51).EQ.0 .OR. SFOBS(IH,50).EQ.0 )THEN
                  CALM(IH) = .TRUE.
                  WSPD(IH) = 0.0
                  WDIR(IH) = 0.0
                  IF( .NOT. HRWINDOS(IH) )THEN
C ----               No onsite data for this hour, so set GOTWND = .T.
C                    and set anemometer height to NWS site
                     GOTWND = .TRUE.
                     ZREF(IH) = INSTHT(1)
C                    The flag HRWINDNWS is used to determine which set
C                    of surface characteristics is to be used
                     HRWINDNWS(IH) = .TRUE.
                  ENDIF
               ELSE
C                 Got a valid non-calm NWS wind observation
                  GOTWND = .TRUE.
C                 set speed, direction, and zref
                  WSPD(IH) = FLOAT(SFOBS(IH,51))/10.0
                  WDIR(IH) = FLOAT(SFOBS(IH,50))*10.0
                  ZREF(IH) = INSTHT(1)
C ---             Reset CALM(IH) and HRWINDOS(IH) to .F. in case flags
C                 were set for calm onsite data
                  CALM(IH) = .FALSE.
                  HRWINDOS(IH) = .FALSE.
C                 The flag HRWINDNWS is used to determine which set
C                 of surface characteristics is to be used
                  HRWINDNWS(IH) = .TRUE.

                  IF( OSDATA )THEN   ! NWS winds used for this hour
                     MESS =  BLNK80
                     ECODE = 'I81'
                     WRITE (MESS, 1030) IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                  ENDIF

               ENDIF


C              If not a calm: 1) if ASOS and adjustment not over-ridden,
c              apply adjustment; 2) check for minimum wind speed; and
c              3) randomize wind direction if specified
               IF( .NOT. CALM(IH) )THEN

C                 0.257 m/s (0.5 knots) is added to adjust wind speed for
c                 the truncation if not over-ridden; also set the flag
C                 so a proper indication can be written in the stage 3
C                 report file
                  IF( (ISASOS24(IH) .EQ. 'A' .OR. ISASOS24(IH) .EQ. 'a')
     &               .AND. ASOS_ADJ )THEN
                     WSPD(IH) = WSPD(IH) + ASOSADJ
                  ENDIF

C                 Check for a minumum wind speed and reset as necessary
                  IF( WSPD(IH) .LT. SQRT(2.0)*SIGVMIN  .AND.
     &                WSPD(IH) .GT. 0.0 )THEN
                     MESS =  BLNK80
                     ECODE = 'I81'
                     WRITE(MESS, 1040) WSPD(IH), IH
                     CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
                     WSPD(IH) = SQRT(2.0)*SIGVMIN
                  ENDIF

C                 Randomize the NWS wind direction if instructed
C                 (through the METHOD WIND keyword); CONTRL(1) is
C                 assigned the value of 2 if user specifies to randomize;
C                 also, insure direction is < 360 after randomizing
C
                  IF( CONTRL(1) .EQ. 2 )THEN
                     WDIR(IH) = WDIR(IH) + IRND(IH,MPJDY) - 4.0
                     IF( WDIR(IH) .GT. 360.0) THEN
                        WDIR(IH) = WDIR(IH) - 360.0
                     ELSEIF (WDIR(IH) .LT. 0.0) THEN
                        WDIR(IH) = WDIR(IH) + 360.0
                     ENDIF
                  ENDIF

               ENDIF


            ELSEIF( ( SFOBS(IH,51) .NE. SFQA(51,2) ) .AND.
     &              ( SFOBS(IH,51) .NE. 0 .AND.
     &                SFOBS(IH,51) .LE. 31 ) .AND.
     &              ( SFOBS(IH,50) .EQ. SFQA(50,2) ) .AND.
     &              ( JMPDAT8 .GE. 19960701 ) )THEN
C              NWS wind speed is non-missing but direction is missing;   ! rwb #518 06341
C              if WS .LE. 6 kts (3.1 m/s) treat as "VARIABLE" wind (may occur for
C              TD-3280 and ISHD data after introduction of METAR on
C              July 1, 1996.)
C              Do not need to treat for calm since condition
C              eliminates calms (.NE. 0)

C              set temp asos flag
               ISASOSTMP = ISASOS24(IH)

               WSPD(IH) = FLOAT(SFOBS(IH,51))/10.0
               WDIR(IH) = SFQA(50,2)
               ZREF(IH) = INSTHT(1)

               VARWD(IH) = .TRUE.                                        ! rwb #518 06341

C              Got a valid non-calm wind observation
               GOTWND = .TRUE.
C ---          Reset CALM(IH) and HRWINDOS(IH) to .F. in case flags
C              were set for calm onsite data
               CALM(IH) = .FALSE.
               HRWINDOS(IH) = .FALSE.
C              The flag HRWINDNWS is used to determine which set
C              of surface characteristics is to be used
               HRWINDNWS(IH) = .TRUE.

               IF( OSDATA )THEN   ! NWS winds used for this hour
                  MESS =  BLNK80
                  ECODE = 'I81'
                  WRITE (MESS, 1030) IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
               ENDIF

C              ASOS winds were truncated; 0.257 m/s (0.5 knots) is added
C              to adjust wind speed for the truncation
               IF( ISASOS24(IH) .EQ. 'A' .OR. ISASOS24(IH) .EQ. 'a'
     &             .AND. ASOS_ADJ )THEN
                  WSPD(IH) = WSPD(IH) + ASOSADJ
               ENDIF

C              Check for a minumum wind speed and reset as necessary
               IF( WSPD(IH) .LT. SQRT(2.0)*SIGVMIN  .AND.
     &             WSPD(IH) .GT. 0.0 )THEN
                  MESS =  BLNK80
                  ECODE = 'W81'
                  WRITE(MESS, 1040) WSPD(IH), IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )

                  WSPD(IH) = SQRT(2.0)*SIGVMIN
               ENDIF

            ELSE
C------------- Surface and 1-min ASOS winds are missing

C              set temp asos flag
               ISASOSTMP = ISASOS24(IH)

               IF( OSDATA )THEN
                  MESS =  BLNK80
                  ECODE = 'I71'
                  WRITE(MESS, 1050)IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
               ENDIF

            ENDIF  ! 1-min ASOS winds present and not missing or calm

         ELSE
C           Not substituing NWS data

            IF( OSDATA )THEN
C              No reference level data
               MESS =  BLNK80
               ECODE = 'I71'
               WRITE(MESS, 1060) IH
               CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
            ENDIF

         ENDIF                                   ! SUBSTNWS

      ENDIF                                      ! NOT GOTWND

100   CONTINUE

c     Reset ASOS flag based on any substitution that occurred above
      ISASOS24(IH) = ISASOSTMP

c     print*, JMPDATE,IH,'OS:',HRWINDOS(IH),'1-min:',HRWINDASOS(IH),
c    &        'NWS:',HRWINDNWS(IH)
C     ------------------------------------------------------------------
C                              TEMPERATURE
C     ------------------------------------------------------------------

      IF( .NOT. GOTTMP )THEN
C        A reference level temperature is not available from on-site data

         IF( SUBSTNWS )THEN
C           And the user specified NWS substitution

            IF( SFOBS(IH,46) .NE. SFQA(46,2) )THEN
C              NWS temperature is not missing - substitute

               GOTTMP    = .TRUE.
               T(IH)     = SFOBS(IH,46) / 10.0 + 273.15
               ZTREF(IH) = ZNWST
               IF( OSDATA )THEN
                  MESS =  BLNK80
                  ECODE = 'I82'
                  WRITE(MESS, 1070) IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
               ENDIF

            ELSE
C              NWS temperature is not available
               IF( OSDATA )THEN
C                 User specified to substitute
                  MESS =  BLNK80
                  ECODE = 'I71'
                  WRITE(MESS, 1080) IH
                  CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
               ENDIF
            ENDIF                                ! NWS TEMP. MISSING?

         ELSE
            IF( OSDATA )THEN
C              Not substituting NWS temperature
               MESS =  BLNK80
               ECODE = 'I71'
               WRITE(MESS, 1090) IH
               CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )
            ENDIF

         ENDIF                                   ! SUBSTNWS

      ELSEIF( GOTTMP .AND. GOTWND .AND. .NOT.HRWINDOS(IH) .AND.
     &                                                OSNL .GT. 1 )THEN
C ---    On-site temperature data available, but off-site winds
C        are being substituted.  Use only reference on-site
C        temperature to avoid inconsistency between observed
C        on-site temp profile and BL characterization based
C        on off-site winds.  Also, set sigma-w to missing,
C        if available.  Sigma-theta should be missing anyway,
C        but set it to missing just in case.
         DO ILVL = 1,OSNL
            IF( ABS(OSVOBS(IH,ILVL,1)-ZTREF(IH)) .GT. 0.01 )THEN
                    OSVOBS(IH,ILVL,7) = OSQA(21,2)      ! on-site temperature
            ENDIF
            OSVOBS(IH,ILVL,2) = OSQA(16,2)              ! on-site sigma-theta
            OSVOBS(IH,ILVL,5) = OSQA(19,2)              ! on-site sigma-w
         ENDDO

         MESS =  BLNK80
         ECODE = 'I85'
         WRITE(MESS, 1095) IH
         CALL ERRHDL ( JMPDATE,PATH,ECODE,LOC,MESS )

      ENDIF                                      ! NOT GOTTMP


C---- Retrieve other pertinent information -
C       P    = surface pressure - use onsite if it exists, otherwise NWS,
C              but also base selection on whether onsite or NWS temp is used
C       CCVR = total cloud cover - use onsite if it exists, otherwise NWS
C       QR   = insolation - use onsite data
C       RN   = net radiation - use onsite data
C
C-------------------------------------------------------------------------------
C
C     Atmospheric pressure: hierarchy is based on source of temperature data that
C     will be used with pressure to calculate density, either ONSITE or SURFACE.
C     Look for station pressure from temperature site first; then adjust sea-level
C     pressure from temperature site if station elevation is available; then
C     use estimates based on standard atmosphere, preferrably using elevation
C     of the temperature site.
C     Note that if station elevation is included in the SURFACE data file (for
C     SAMSON and ISHD formats), and the user has not specified the elevation
C     on the SURFACE LOCATION keyword, then appropriate substitutions are made
C     in Stage 1 for hours with missing station pressure (since the elevation
C     from the data file is not available in Stage 3).  Otherwise, all estimates
C     for missing station pressure are made here in Stage 3. Also note that the
C     use of NWS station pressure is NOT controlled by the SUBNWS variable.
C
C     Use subroutine GEO to adjust pressures for elevation.  While these
C     adjustments are approximations, they are probably good enough for
C     our purposes given limited sensitivity to station pressure within
C     AERMET.  However, we attempt to get best estimate of station pressure
C     associated with the ambient temperature being used (ONSITE vs. NWS),
C     since they are used together to compute air density.
C
      IF( GOT_OSTMP )THEN
C ---    ON-SITE temperature is available;
C        obtain best estimate of pressure at ON-SITE tower location
         IF( ABS(OSSOBS(IH,17)-OSQA(32,2)) .GT. 0.01 )THEN
C           Use the ON-SITE station pressure
            P(IH) = OSSOBS(IH,17) / 10.0

         ELSEIF( ABS(OSSOBS(IH,16)-OSQA(31,2)) .GT. 0.01 .AND.
     &                                              GOTPWELEV(4) )THEN
C           Use ON-SITE sea level pressure and station elev to convert
C           to station pressure
            CALL GEO(2, PWELEV(4), 290.0, SFSP, OSSOBS(IH,16)/10.0)
            P(IH) = SFSP

         ELSEIF( SFOBS(IH,32) .NE. SFQA(32,2) .AND.
     &                           GOTPWELEV(3) .AND. GOTPWELEV(4) )THEN
C           Adjust SURFACE (hourly wx obs) station pressure to ON-SITE
C           station elevation
            CALL GEO(2, (PWELEV(4)-PWELEV(3)), 290.0, SFSP,
     &                                         FLOAT(SFOBS(IH,32))/10.0)
            P(IH) = SFSP

         ELSEIF( SFOBS(IH,31) .NE. SFQA(31,2) .AND. GOTPWELEV(4) )THEN
C           Convert SURFACE sea-level pressure & OS elevation to
C           station pressure
            CALL GEO(2, PWELEV(4), 290.0, SFSP,
     &                                         FLOAT(SFOBS(IH,31))/10.0)
            P(IH) = SFSP

         ELSEIF( SFOBS(IH,32) .NE. SFQA(32,2) )THEN
C           Use the SURFACE (hourly wx obs) station pressure
            P(IH) = FLOAT (SFOBS(IH,32) ) / 10.0

         ELSEIF( SFOBS(IH,31) .NE. SFQA(31,2) .AND. GOTPWELEV(3) )THEN
C           Convert SURFACE sea-level pressure & SF elevation to
C           station pressure
            CALL GEO(2, PWELEV(3), 290.0, SFSP,
     &                                         FLOAT(SFOBS(IH,31))/10.0)
            P(IH) = SFSP

         ELSEIF( GOTPWELEV(4) )THEN
C           Use the ON-SITE elevation and determine station pressure from
C           standard atmosphere
            P(IH) = 1013.25 * (1.0 - ((6.5e-3/288.15)*PWELEV(4)))**5.255

         ELSEIF( GOTPWELEV(3) )THEN
C           Use the SURFACE elevation and determine station pressure from
C           standard atmosphere
            P(IH) = 1013.25 * (1.0 - ((6.5e-3/288.15)*PWELEV(3)))**5.255

         ELSEIF( ABS((OSSOBS(IH,16)-OSQA(31,2))) .GT. 0.01 .AND.
     &                                          .NOT. GOTPWELEV(4) )THEN
C           Assign ONSITE sea-level pressure to station pressure,
C           assuming elevation=0
            P(IH) = OSSOBS(IH,16) / 10.0

         ELSEIF( SFOBS(IH,31) .NE. SFQA(31,2) .AND.
     &                                          .NOT. GOTPWELEV(3) )THEN
C           Assign SURFACE sea-level pressure to station pressure,
C           assuming elevation=0
            P(IH) = FLOAT(SFOBS(IH,31))/10.0

         ELSE
C           Station pressure is used only to calculate density; rather than
C           skip the computations if pressure is missing, assume a pressure
C           of 1013.25 (sea level pressure) if all variables are missing.
C           This condition should seldom be hit since there should always
C           be a calculation for the hourly surface obs.; the exception is
C           if the on-site data contains all the necessary met. data and
C           the hourly obs are not needed.

            P(IH) = 1013.25
            PCNT2 = PCNT2 + 1

C           Limit the number of times the message is written
            IF( MOD(PCNT2,1000) .EQ. 0 )THEN
               MESS =  BLNK80
               ECODE = 'W82'
               WRITE (MESS, 1110) PCNT2
               CALL ERRHDL ( JMPDATE, PATH, ECODE, LOC, MESS )
            ENDIF

         ENDIF

      ELSEIF( GOTTMP )THEN
C ---    SURFACE temperature is available, but no ON-SITE temperature;
C        obtain best estimate of pressure at SURFACE station location
         IF( SFOBS(IH,32) .NE. SFQA(32,2) )THEN
C           Use the SURFACE (hourly wx obs) station pressure
            P(IH) = FLOAT (SFOBS(IH,32) ) / 10.0

         ELSEIF( SFOBS(IH,31) .NE. SFQA(31,2) .AND. GOTPWELEV(3) )THEN
C           Convert SURFACE sea-level pressure & SF elevation to
C           station pressure
            CALL GEO(2, PWELEV(3), 290.0, SFSP,
     &                                         FLOAT(SFOBS(IH,31))/10.0)
            P(IH) = SFSP

         ELSEIF( ABS(OSSOBS(IH,17)-OSQA(32,2)) .GT. 0.01 .AND.
     &                           GOTPWELEV(3) .AND. GOTPWELEV(4) )THEN
C           Adjust ON-SITE station pressure to surface station elevation
            CALL GEO(2, (PWELEV(3)-PWELEV(4)), 290.0, SFSP,
     &                                          OSSOBS(IH,17)/10.0)
            P(IH) = SFSP

         ELSEIF( ABS(OSSOBS(IH,17)-OSQA(32,2)) .GT. 0.01 .AND.
     &                                              GOTPWELEV(3) )THEN
C           Adjust ON-SITE sea-level pressure to surface station elevation
            CALL GEO(2, PWELEV(3), 290.0, SFSP, OSSOBS(IH,16)/10.0)
            P(IH) = SFSP

         ELSEIF( ABS(OSSOBS(IH,17)-OSQA(32,2)) .GT. 0.01 )THEN
C           Use the ON-SITE station pressure
            P(IH) = OSSOBS(IH,17) / 10.0

         ELSEIF( ABS(OSSOBS(IH,16)-OSQA(31,2)) .GT. 0.01 .AND.
     &                                              GOTPWELEV(4) )THEN
C           Use ON-SITE sea level pressure and station elev to convert
C           to station pressure
            CALL GEO(2, PWELEV(4), 290.0, SFSP, OSSOBS(IH,16)/10.0)
            P(IH) = SFSP

         ELSEIF( GOTPWELEV(3) .AND. PWELEV(3) .NE. 0.0 )THEN
C           Estimate station pressure based on SURFACE elevation using
C           standard atmosphere; using elevation provided by user
            P(IH) = 1013.25 * (1.0 - ((6.5e-3/288.15)*PWELEV(3)))**5.255

         ELSEIF( SFOBS(IH,31) .NE. SFQA(31,2) .AND.
     &                                          .NOT. GOTPWELEV(3) )THEN
C           Assign SURFACE sea-level pressure to station pressure,
C           assuming elevation=0
            P(IH) = FLOAT(SFOBS(IH,31))/10.0

         ELSEIF( ABS((OSSOBS(IH,16)-OSQA(31,2))) .GT. 0.01 .AND.
     &                                          .NOT. GOTPWELEV(4) )THEN
C           Assign ONSITE sea-level pressure to station pressure,
C           assuming elevation=0
            P(IH) = OSSOBS(IH,16) / 10.0

         ELSE
C           Station pressure is used only to calculate density; rather than
C           skip the computations if pressure is missing, assume a pressure
C           of 1013.25 (sea level pressure) if all variables are missing.
C           This condition should seldom be hit since there should always
C           be a calculation for the hourly surface obs.; the exception is
C           if the on-site data contains all the necessary met. data and
C           the hourly obs are not needed.

            P(IH) = 1013.25
            PCNT2 = PCNT2 + 1

C           Limit the number of times the message is written
            IF( MOD(PCNT2,1000) .EQ. 0 )THEN
               MESS =  BLNK80
               ECODE = 'W82'
               WRITE (MESS, 1110) PCNT2
               CALL ERRHDL ( JMPDATE, PATH, ECODE, LOC, MESS )
            ENDIF

         ENDIF

      ELSE
C ---    No temperature available; use simplified hierarchy based on
C        ON-SITE pressure if available, surface pressure, or
C        elevation-based standard pressure
         IF( ABS(OSSOBS(IH,17)-OSQA(32,2)) .GT. 0.01 )THEN
C           Use the ON-SITE station pressure
            P(IH) = OSSOBS(IH,17) / 10.0

         ELSEIF( SFOBS(IH,32) .NE. SFQA(32,2) )THEN
C           Use the SURFACE (hourly wx obs) station pressure
            P(IH) = FLOAT (SFOBS(IH,32) ) / 10.0

         ELSEIF( GOTPWELEV(4) )THEN
C           Use the ON-SITE elevation and determine station pressure from
C           standard atmosphere
            P(IH) = 1013.25 * (1.0 - ((6.5e-3/288.15)*PWELEV(4)))**5.255

         ELSEIF( GOTPWELEV(3) )THEN
C           Estimate station pressure based on SURFACE elevation using
C           standard atmosphere
            P(IH) = 1013.25 * (1.0 - ((6.5e-3/288.15)*PWELEV(3)))**5.255

         ELSE
C           Station pressure is used only to calculate density; rather
C           than skip the computations if pressure is missing, assume a
C           pressure of 1013.25 (sea level) if all variables are missing.
C           This condition should seldom be hit since there should always
C           be a calculation for the hourly surface obs.; the exception is
C           if the on-site data contains all the necessary met. data and
C           the hourly obs are not needed.

            P(IH) = 1013.25
            PCNT2 = PCNT2 + 1

C           Limit the number of times the message is written
            IF( MOD(PCNT2,1000) .EQ. 0 )THEN
               MESS =  BLNK80
               ECODE = 'W82'
               WRITE (MESS, 1110) PCNT2
               CALL ERRHDL ( JMPDATE, PATH, ECODE, LOC, MESS )
            ENDIF

         ENDIF

      ENDIF                            ! for station pressure



C     ------------------------------------------------------------------
C                              CLOUD COVER
C     ------------------------------------------------------------------

      IF( .NOT. GOTSKY )THEN   !  Check availability of cloud cover


         IF( ABS(OSSOBS(IH,19)-FLOAT(NO_SKY)) .GT. 0.01 )THEN    ! Use onsite cloud cover
            CCVR(IH) = NINT(OSSOBS(IH,19))
            GOTSKY = .TRUE.

         ELSE                                                    ! Use NWS cloud cover

c           First we split the concatenated variable TOTAL//OPAQUE
c           and the missing value indicator.

            ITOTAL = SFOBS(IH,34) / 100
            IOPAQ  = SFOBS(IH,34) - ITOTAL * 100
            MISTOT = SFQA(34,2) / 100
            MISOPQ = SFQA(34,2) - MISTOT * 100

c           Check opaque cloud cover first and use if it's not missing

            IF( IOPAQ .NE. MISOPQ )THEN                 ! Use opaque
               CCVR(IH) = IOPAQ
               GOTSKY = .TRUE.

            ELSEIF( ITOTAL .NE. MISTOT )THEN            ! Use total
               CCVR(IH) = ITOTAL
               GOTSKY = .TRUE.

            ELSEIF(SFOBS(IH,43) .NE. SFQA(43,2))THEN    ! Use ASOS
c              ! Check for ASOS data
               CCVR(IH) = SFOBS(IH,43)
               GOTSKY = .TRUE.

            ENDIF

         ENDIF
      ENDIF                                             !  NOT GOTSKY



C     ------------------------------------------------------------------
C                       DEW POINT AND RELATIVE HUMIDITY
C     ------------------------------------------------------------------

C ----   Check for on-site dew point and relative humidity
C        First is dew point, stored in OSVOBS(IH,ILVL,11);
C        If available, assign to OSSOBS(IH,48)
         OSSOBS(IH,48) = FLOAT(SFQA(48,2))
         DO L = 1, OSNL
            IF( ABS(OSVOBS(IH,L,11)-OSQA(25,2)) .GT. 0.01 )THEN
               OSSOBS(IH,48) = OSVOBS(IH,L,11)
               EXIT
            ENDIF
         ENDDO

C ----   Next look for on-site RH, stored in OSVOBS(IH,ILVL,12);
C        If available, assign to OSSOBS(IH,49)
         OSSOBS(IH,49) = FLOAT(SFQA(49,2))
         DO L = 1, OSNL
            IF( ABS(OSVOBS(IH,L,12)-OSQA(26,2)) .GT. 0.01 )THEN
               OSSOBS(IH,49) = OSVOBS(IH,L,12)
               EXIT
            ENDIF
         ENDDO


C ----   Now apply hierarchy of ONSITE or SURFACE data for DEWP and RH.
C        First for dew point:
         IF( ABS(OSSOBS(IH,48)-FLOAT(SFQA(48,2))) .GT. 0.01 )THEN
C ----      Use ONSITE dew point, converting to Kelvin
            DEWP(IH) = OSSOBS(IH,48) + 273.15
         ELSEIF( SFOBS(IH,48) .NE. SFQA(48,2) )THEN
C ----      Use SURFACE dew point, converting to Kelvin
            DEWP(IH) = FLOAT(SFOBS(IH,48))/10.0 + 273.15
         ELSE                                     ! Missing dew point
            DEWP(IH) = FLOAT(SFQA(48,2))
         ENDIF

C ----   Next for relative humidity:
C        Since RH is output to surface file for use with deposition
C        algorithms, calculate RH from T and Td if needed.
         IF( ABS(OSSOBS(IH,49) - FLOAT(SFQA(49,2))) .GT. 0.01 )THEN
C ---       Use ONSITE RH, if available
            RH(IH) = OSSOBS(IH,49)
         ELSEIF( GOT_OSTMP .AND.
     &           ABS( OSSOBS(IH,48)-FLOAT(SFQA(48,2)) ) .GT. 0.01 )THEN
C ---       Calculate RH from OS temp and dew point, if available;
C           adjust T(IH) and TD since already in degrees K;
C           subroutine HUMID assumes inputs in C
            CALL AERMET_HUMID( T(IH)-273.15, DEWP(IH)-273.15, RH(IH) )
         ELSEIF( SFOBS(IH,49) .NE. SFQA(49,2) )THEN
C ---       Use SURFACE station RH, if available
            RH(IH) = FLOAT(SFOBS(IH,49))
         ELSEIF( GOTTMP .AND. .NOT.GOT_OSTMP .AND.
     &           SFOBS(IH,48) .NE. SFQA(48,2) )THEN
C ---       Calculate RH from SURFACE temp and dew point, if available;
C           adjust T(IH) and TD since already in degrees K;
C           subroutine HUMID assumes inputs in C
            CALL AERMET_HUMID( T(IH)-273.15, (FLOAT(SFOBS(IH,48))/10.0),
     &                                                    RH(IH) )
         ELSEIF( SFOBS(IH,46) .NE. SFQA(46,2) .AND.
     &           SFOBS(IH,48) .NE. SFQA(48,2) )THEN
C ---       Calculate RH from SURFACE temp and dew point, if available;
C           adjust T(IH) and TD since already in degrees K;
C           subroutine HUMID assumes inputs in C
            CALL AERMET_HUMID( FLOAT(SFOBS(IH,46))/10.0,
     &                  FLOAT(SFOBS(IH,48))/10.0, RH(IH) )
         ELSE
C ---       Missing relative humidity, assign SF data missing code
            RH(IH) = FLOAT(SFQA(49,2))
         ENDIF



C     ------------------------------------------------------------------
C                      PRECIPITATION AMOUNT AND TYPE
C     ------------------------------------------------------------------

C     ================================================================++
C       If there are no data in the file (neither onsite nor NWS) for
C       this day, then it will have been caught by the calling
C       routine, MPPBL, and SUBST will not have been called.
C     ================================================================++

c     Retrieve precipitation code and amount
      IPCODE(IH) = SFOBS(IH,42)

c     Precipitation codes are as follows:

c           0 = none
c           1 = liquid precipitation
c           2 = frozen precipitation
c           3 = both liquid and frozen
c           9 = missing

c     Process precipitation code for use in AERMET

      IF(IPCODE(IH).EQ.1 .OR. IPCODE(IH).EQ.3)THEN    ! Liquid
         IPCODE(IH) = 11
      ELSEIF(IPCODE(IH) .EQ. 2)THEN                   ! Frozen
         IPCODE(IH) = 22
      ELSEIF(IPCODE(IH) .EQ. 9)THEN                   ! Missing
         IPCODE(IH) = 99
      ENDIF

      IF( (SFOBS(IH,30) .EQ. SFQA(30,2)) .AND.
     &   ABS(OSSOBS(IH,6)-OSQA(6,2)) .LT. 0.01 .AND.
     &   ABS(OSSOBS(IH,15)-OSQA(30,2)) .LT. 0.01 )THEN
C        All precip variables missing, assign missing code to PAMT
         PAMT(IH) = -9.0
      ELSEIF( ABS(OSSOBS(IH,6)-OSQA(6,2)) .GT. 0.01 )THEN
C        Convert OS PAMT in cm to mm/hr
         PAMT(IH) = OSSOBS(IH,6)*10.
C        Assign precipitation type code for on-site PAMT
C        based on ambient temperature
         IF (PAMT(IH) .GT. 0.0 .AND. GOTTMP) THEN
            IF (T(IH) .GE. 273.15) THEN
               IPCODE(IH) = 11
            ELSEIF (T(IH) .LT. 273.15) THEN
               IPCODE(IH) = 22
            ENDIF
         ELSEIF (PAMT(IH) .GT. 0.0 .AND. .NOT.GOTTMP) THEN
            IPCODE(IH) = 99
         ENDIF
      ELSEIF( ABS(OSSOBS(IH,15)-OSQA(30,2)) .GT. 0.01 )THEN
C        Convert OS PRCP in mm*100 to mm
         PAMT(IH) = OSSOBS(IH,15)/100.
C        Assign precipitation type code for on-site PAMT
C        based on ambient temperature
         IF (PAMT(IH) .GT. 0.0 .AND. GOTTMP) THEN
            IF (T(IH) .GE. 273.15) THEN
               IPCODE(IH) = 11
            ELSEIF (T(IH) .LT. 273.15) THEN
               IPCODE(IH) = 22
            ENDIF
         ELSEIF (PAMT(IH) .GT. 0.0 .AND. .NOT.GOTTMP) THEN
            IPCODE(IH) = 99
         ENDIF
      ELSE
C        Convert SF PRCP in cm*10 to mm
         PAMT(IH) = FLOAT(SFOBS(IH,30))/100.
      ENDIF

      RETURN

c             ....+....1....+....2....+....3....+....4....+....5....+...

 1010 FORMAT(' OS ref WSPD (',F6.2,') < MIN - reset to 0.28m/s for',
     &       ' hour:', I3.2)

 1020 FORMAT(' Ref level for wind below 20*Z0 for hour: ', I3.2)

 1030 FORMAT(' NWS winds used as reference winds for hour: ', I3.2)

 1031 FORMAT(' 1-min ASOS winds used as reference winds for hour: ',
     &         I3.2)

 1032 FORMAT(' 1-min ASOS winds prior to January 1, 2000 for hour: ',
     &         I3.2,' - INVALID data period - winds NOT used!')

 1033 FORMAT(' 1-min ASOS winds prior to commission date for hour: ',
     &         I3.2)

 1034 FORMAT('  NOTE: Additional messages regarding 1-min ASOS ',
     &         'winds prior to Jan 2000 included in message file.')

 1040 FORMAT(' NWS ref WSPD (',F6.2,') < MIN - reset to 0.28m/s for',
     &       ' hour: ', I3.2)

 1041 FORMAT(' 1-min ASOS ref WSPD (',F6.2,') < MIN - reset to',
     &       ' 0.28m/s for hour: ', I3.2)

 1050 FORMAT(' No reference wind for hour: ', I3.2)

 1060 FORMAT(' No ref wind (NWS option was not requested) for hour: ',
     &         I3.2)

 1070 FORMAT(' NWS temperature used as reference for hour: ', I3.2)

 1080 FORMAT(' No reference temperature for hour: ',I3.2)

 1090 FORMAT(' No ref temp (NWS option was not requested) for hour: ',
     &         I3.2)

 1095 FORMAT(' On-site reference temp only (no profile) used with',
     &       ' off-site WS for hour: ',I3.2)

 1110 FORMAT(' Standard sea-level pressure (1013.25 mb) used ',
     &        'by default; No. of times >/= ', I6)

      END
