C     METPROCESSOR (Stage 3) COMMON BLOCKS

      CHARACTER*6 ACTION
      CHARACTER*8 ITEM, INSTR, DISPMD, MPLAT, MPLON, MPLOC

      INTEGER     MPYR,MPCMO,MPCDY,MPCRDY,MPJDY
      INTEGER     CONTRL,MDSTAT
      INTEGER     KEYLVL,ZONE,TMPLVL,STKLVL
      INTEGER     PCNTRL
      INTEGER*2   IRND
      INTEGER     OSNWDS, SFCFRQ, NUMSEC, ICHCNT, NKFREQ,
     &            GOTCHR
      INTEGER     OSNWDS2, SFCFRQ2, NUMSEC2, ICHCNT2, NKFREQ2,
     &            GOTCHR2
      INTEGER     MFLAG(24), N_CALM, K_CALM, iCALM1MIN                ! dtb #131 02122, jop --- 12069
      INTEGER     N_VARWD, K_VARWD
      INTEGER     MySounding
      
      REAL        ST3LAT, ST3LON, UAST3LAT, UAST3LON
      REAL        OSSFC, OSWDS, OSSFC2, OSWDS2
      REAL        INSTHT,STKHGT,ANEHGT,PLMTOP,HILLHT,
     &            MPHGTS,FRHGTS,THRESH1SPD,TSR,TSS
      REAL        MPZO,SPEED,TEMP,FLWVEC,RANFLW,ZIHGTS,SE,SA
      REAL        ZNWST

      LOGICAL     SUBSTNWS, OSDATA, BULKRN, CBL(24), CALM(24)
      LOGICAL     SFDATA, UADATA                                      ! dtb #111 02245
      LOGICAL     GOTWND, GOTTMP, GOTSKY                              ! dtb #127 02113
      LOGICAL     HRWINDNWS(24), HRWINDOS(24),HRWINDASOS(24),
     &            ASOS_ADJ, SUNRISE4UA
      LOGICAL     L_1minAsos_pre2000, L_1MINASOS_THRESH
	  LOGICAL     ADJ_USTAR

C     Add logical array for VARIABLE winds                            ! rwb #518 06341
      LOGICAL     VARWD(24)                                           ! rwb #518 06341
      CHARACTER*3 PBL(24)

      INTEGER, PARAMETER :: OSMSEC=16, OSMFRQ=12, OSMSITEC = 3
      INTEGER, PARAMETER :: MFRQSEC = OSMFRQ*OSMSEC
      INTEGER, PARAMETER :: MPMLVL=50,MPMHRS=24,NITEM=16,NACT=8,
     &                      NINSTR=1     
      INTEGER, PARAMETER :: NDISP=5                                           ! dtb #100 01249

C     NITEM       Number of methodologies
C     NACT        Number of actions
C     NINSTR      Number of NWS instruments for which the user
C                 will supply a measurement height
C     NDISP       Number of dispersion models

      COMMON/MPBK01/ ITEM(NITEM), ACTION(NACT), INSTR(NINSTR),
     &               DISPMD(NDISP)
      COMMON/MP01A/  CONTRL(NITEM)

      COMMON/MPBK02/ MPYR, MPCMO, MPCDY, MPCRDY, MPJDY, MDSTAT,
     1               KEYLVL, MPLOC, MPLAT, MPLON,ST3LAT, ST3LON,
     1               UAST3LAT, UAST3LON, ZONE, TMPLVL, STKLVL

      COMMON/MPBK03/ INSTHT(NINSTR),STKHGT,ANEHGT,PLMTOP,HILLHT,
     1               ZNWST,THRESH1SPD, TSR,TSS,MPZO,
     1               MPHGTS(MPMLVL),FRHGTS(MPMLVL),
     1               SPEED(MPMLVL,MPMHRS),TEMP(MPMLVL,MPMHRS),
     1               FLWVEC(MPMLVL,MPMHRS),
     1               RANFLW(MPMHRS),ZIHGTS(2,MPMHRS),
     1               SE(MPMLVL,MPMHRS),SA(MPMLVL,MPMHRS)

      COMMON/MPBK04/ IRND(24,366)

      COMMON/MPBK05/ PCNTRL, N_CALM, K_CALM, N_VARWD, K_VARWD, iCALM1MIN  ! dtb #131 02122

      COMMON/MPBK06/ GOTWND, GOTTMP, GOTSKY                           ! dtb #127 02113

      COMMON/MPBK07/ SUBSTNWS, OSDATA, BULKRN, CBL, PBL, MFLAG, CALM, ! dtb #111 02045
     &               SFDATA, UADATA, VARWD,                           ! rwb #??? 06341
     &               HRWINDNWS, HRWINDOS, L_1MINASOS_THRESH,
     &               HRWINDASOS, ASOS_ADJ, SUNRISE4UA, MySounding,
     &               L_1minAsos_pre2000, ADJ_USTAR

      COMMON/MPBK08/ OSSFC(OSMFRQ,OSMSEC,OSMSITEC),OSWDS(OSMSEC,2),
     &               SFCFRQ,NUMSEC,OSNWDS,ICHCNT,NKFREQ,
     &               GOTCHR(OSMFRQ,OSMSEC)

      COMMON/MPBK09/ OSSFC2(OSMFRQ,OSMSEC,OSMSITEC),OSWDS2(OSMSEC,2),
     &               SFCFRQ2,NUMSEC2,OSNWDS2,ICHCNT2,NKFREQ2,
     &               GOTCHR2(OSMFRQ,OSMSEC)

C     GOTWND  - logical that determines if a reference wind speed and    ! dtb #127 02113
C               direction were found for the hour                        ! dtb #127 02113

C     GOTTMP  - logical that determines if a reference temperature was   ! dtb #127 02113
C               found for the hour                                       ! dtb #127 02113

C     GOTSKY    logical that determines if a valid cloud cover was found ! dtb #127 02113
C               for the hour                                             ! dtb #127 02113

C     MPMLVL    Maximum number of levels provided for in output
C               meteorology (presently not activated)
C
C     MPMHRS    Maximum number of hours processed within a day
C
C     ITEM      8-character keyword used to indicate which
C               metprocessor METHOD is to be used or redefined.
C
C     ACTION    A 6-character keyword that instructs the metprocessor
C               How a method (ITEM) is to be processed.
C
C     EXAMPLE INPUT LINE FOR 'METHOD': METHOD 'ITEM'  'ACTION'  xxxx
C
C      For ITEM 1 WIND_DIR - defines how the wind direction is to be
C                            determined.
C
C          Valid Actions
C               1   NORAND - (DEFAULT) Use nearest 10 deg as input (NWS)
C               2   RANDOM - Randomize direction with 10 deg range
C                            using random numbers in file specified
C
C      For ITEM 2 TEMP -     defines how the ambient air temperature
C                            is to be defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 3 TURB_INT - defines how the vertical and lateral
C                            turbulence intensities (used for dispersion
C                            estimation) are to be defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 4 MIX_HGT  - defines how the mixing height is to be
C                            defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 5 TEMPGRAD - defines how the two temperature gradients
c                            used in RTDM) are to be defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 6 STABILTY - defines how the Pasquill stability category
C                            is to be defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 7 HEATFLUX - defines how the surface heat flux is to be
C                            defined.
C                            (NOT ACTIVATED)
C
C
C      For ITEM 8 FRICVELO - defines how the surface friction velocity is to
C                            be defined.
C                            (NOT ACTIVATED)
C
C      For ITEM 9 HEIGHTS  - defines how the levels at which meteorology are
C                            to be output are to be determined.
C                            (NOT ACTIVATED)
C
C      For ITEM 10 REFLEVEL - determines if NWS data are to be substituted
C                            for on-site data in obtaining the reference
C                            level data
C
C         Valid Actions
C              3   SUBNWS  - substitute NWS data if this item/action is
C                            present (DEFAULT is not to substitute)


C      For ITEM 11 STABLEBL - defines algorithm(s) for the SBL            ! dtb #020 01204
C
C          Valid Actions
C              4   BULKRN - Use Bulk Richardson Number                   ! dtb #020 01204
C
C          Valid Actions
C              7   ADJ_U* - Use u* adjustment for SBL                    ! rwb v12345
C
C 
C      For ITEM 11 ASOS_ADJ - to correct for ASOS truncation error        ! mec #--- 10xxx
C
C          Valid Actions
C              5   NO_ADJ - by default, ASOS data are adjusted; this
C                           option turns off this adjustment             ! mec #--- 10xxx

C      For ITEM 12 UASELECT - by default, AERMET looks for 00Z/12Z        ! mec #--- 10xxx
C                            sounding to get mixing hieghts; this
C                            option tells AERMET to search for the
C                            sounding closest to sunrise
C          Valid Actions
C              6   SUNRIS - Use sunrise to select the sounding           ! mec #--- 10xxx

C     SUBSTNWS  Logical that controls the substitution in the code
C
C     INSTR     Meteorological parameter for which a measuement height
C                 is required
C
C               WIND - anemometer height (the only option currently
C                      available)
C
C     DISPMD    Array of possible regulatory dispersion model for which
C               meteorology data can be preprocessed.
C
C     MDSTAT    Index within DISPMD, dispersion model defined by user
C
C     IRND      Random numbers for hour and julian day
C     SPEED     Output wind speed for hour(s), m/s, and MPGHTS.
C               currently only one level is output.
C     TEMP      OUTPUT AMBIENT TEMPERATURE FOR HOUR(S), DEG-C UNLESS
C               DISPERSION MODEL REQUIRES KELVIN.
C               CURRENTLY ONLY ONE LEVEL IS OUTPUT.
C     FLWVEC    WIND DIRECTION/OR FLOW VECTOR (DIRECTION TOWARDS WHICH
C               WIND IS BLOWING), DEG, FOR HOUR(S) AND MPHGTS.
C               CURRENTLY ONLY ONE LEVEL IS OUTPUT.
C     SE        SIGMA-E VALUES, RADIANS, FOR HOUR(S) AND MPHGTS,
C               (NOT ACTIVATED)
C     SA        SIGMA-A VALUES, RADIANS, FOR HOUR(S) AND MPHGTS,
C               (NOT ACTIVATED)
C     RANFLW    WIND DIRECTION/ OR WIND FLOW VECTOR (RANDOMIZED +5 OR
C               -4 DEGREES), DEG, FOR HOUR(S).
C     ZIHGTS    MIXING HEIGHTS (M) FOR HOUR(S). SUBSCRIPT 1) YIELDS
C               RURAL VALUES, AND SUBSCRIPT 2) YIELDS URBAN VALUES.
C     INSTHT()  USER-SPECIFIED HEIGHT OF NWS INSTRUMENTATION (M)
C     STKHGT    USER SPECIFIED STACK HEIGHT (M)
C     ANEHGT    USER SPECIFIED ANEMOMETER HEIGHT (M), DEFINES LEVEL
C               OF ON SITE DATA TO BE USED FOR DETERMINING THE
C               STABILITY CATEGORY, DEFAULT IS 10 M.
C     KEYLVL    INDEX POINTING WITHIN OSHT-ARRAY, TO LEVEL CLOSEST
C               TO ANEHGT
C     MPZO      SURFACE ROUGHNESS LENGTH FOR GIVEN HOUR, THIS IS
C               DETERMINED HOURLY, AS NEEDED
C     PLMTOP    USER SPECIFIED HEIGHT FOR 'PLUME TOP' (M), USED AS
C               UPPER LEVEL IN DEFINING TEMPERATURE GRADIENT FOR
C               RTDM (NOT ACTIVATED)
C     HILLHT    USER SPECIFIED HEIGHT FOR 'HILL TOP' (M), USED AS
C               UPPER LEVEL IN DEFINING TEMPERATURE GRADIENT FOR
C               RTDM (NOT ACTIVATED)
C     MPHGTS    METPROCESSOR HEIGHTS, M, USED IN OUTPUT METEOROLOGY
C               (NOT ACTIVATED)
C     FRHGTS    FRACTIONS OF CURRENT HOUR'S MIXING HEIGHT, ONE
C               POSSIBLE METHOLDOLOGY FOR SPECIFYING MPHGTS
C
C     WDNUM     1 = NUMBER OF VALID WIND DIRECTIONS STORED IN
C                   ARRAY RANFLW
C               2 = NUMBER OF MISSING VALUES OF WIND DIRECTION
C                   FOUND WITHIN ARRAY RANFLW
C
C     WSNUM     1 = NUMBER OF VALID WIND SPEEDS STORED IN
C                   ARRAY SPEED(1,J)
C               2 = NUMBER OF MISSING VALUES OF WIND SPEED FOUND
C                   WITHIN ARRAY SPEED(1,J)
C               3 = NUMBER OF CALMS
C
C     ZINUM     1 = NUMBER OF VALID MIXING HEIGHTS STORED IN
C                   ARRAY ZIHGTS(J,K), I= 1(RURAL); 2(URBAN);
C                   K(HOUR)
C                     ZINUM(J,K,L) WHERE J = 1(VALID), 2(MISSING);
C                                        K = 1(RURAL), 2(URBAN);
C                                        L = PASQUILL CATEGORY; 7 USED
C                                            FOR CASES WHEN PASQUILL
C                                            CATEGORY IS MISSING.
C               2 = Number of missing values of mixing heights found
C                   within array ZIHGTS
C
C     TTNUM     1 = Number of valid temperatures
C                   stored in array TEMP(1,J)
C               2 = Number of missing values of temperature
C                   found within array TEMP(1,J)
C
C     WSCLSS         Upper bound values for wind speed classes
C
C     PCNTRL         Control flag for printed output.
C                    LE. 0 = no output
C                    GT. 0 = list generated meteorology
C                    (default is PCNTRL = 0)
C
C     OSDATA         Logical that defines whether or not there are
C                    on-site data in the data base.
C
C     OSNWDS         Number of wind direction sectors for this site
C
C     OSSFC          Albedo, bowen ratio, roughness length for up
C                      to 12 wind sectors
C                      Defaults are albedo 0.25
C                                   bowen  0.75
C                                   roughness(m)  0.15
C
C     OSTIME         Number of values used to define the date and
C                      time information
C
C     OSWDS          Start and stop wind directions for each
C                     wind sector
C
C     OSMSEC         Maximum number of wind direction sectors
C
C     OSMFRQ         Maximum number of unique time periods
C
C     ICHCNT         The number of valid SITE_CHAR keywords in runstream
C
C     NUMSEC         The number of valid SECTOR keywords in a runstream
C
C     NKFREQ         The number of periods implied by the frequency on
C                     the SITE_CHAR keyword
C
C     GOTCHR         2-D array identifying the SITE_CHAR keywords that
C                     were processed; elements are either 0 (indicating
C                     a problem) or 1 (processed without error) for the
C                     period and sector on the SITE_CHAR keyword
C
C     OSMIX          Logical that identifies if the data base
C                      contains on-site mixing heights.
C
C     PBL            Character array indicating the state of the
C                    boundary layer - CBL for convective, SBL for stable
C
C     ISASOS         Flag to indicate whether the current hour is from
C                    an ASOS station ('A') or not ('N')
C
C     SUNRISE4UA     Logical to look for sounding nearest sunrise rather 
C                    than 00Z/12Z soundings
C     MySounding     Sounding time being used for the 00Z/12Z selection
