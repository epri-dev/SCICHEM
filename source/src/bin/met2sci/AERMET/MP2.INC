C     MP2.INC

C     METPROCESSOR COMMON BLOCKS (CONTINUED)

C  MPLVL: maximum number of levels in a sounding; 
C         should match UAML parameter in UA1.INC,
C         and AD2 parameter in WORK1.INC
C  MPHR:  maximum number of soundings available;
C         should match UAMH parameter in UA1.INC
C
      INTEGER, PARAMETER :: MPLVL=300, MPHR=48

      INTEGER CCVR, NO_SKY
      INTEGER N500, N250, N000

      INTEGER IPCODE                                                     ! dtb #011 01180
      REAL    PAMT                                                       ! dtb #011 01180

      REAL, PARAMETER :: SBLMAX=4000.0, CBLMAX=4000.0

      REAL    T,WSPD,WDIR,P,RN,QR,PR,TMP,HT, DEWP, RH                    ! dtb #300 03071
      REAL    BOWEN,ALBEDO,RHO,Z0
      REAL    ZREF, ANG, ANGD, ACRT, PTSUM, SMH, PTA, PTMP
      REAL    HFLUX,USTAR,MOL,THSTAR,ZTREF,WSTAR,VPTG
      REAL    ZICONV, ZIMECH
      REAL    DT500, DT250, DT000

      COMMON/MPSUB1/T(24),WSPD(24),WDIR(24),P(24),
     &       ZREF(24),ZTREF(24),IPCODE(24),PAMT(24),DEWP(24),RH(24)      ! dtb #300 03071

      COMMON/MPSUB2/RN(24),QR(24),CCVR(24), NO_SKY                       ! dtb #122 02096
      
      COMMON/MPSUB3/PR(MPHR,MPLVL),TMP(MPHR,MPLVL),HT(MPHR,MPLVL)
      
      COMMON/MPSFC1/HFLUX(24),USTAR(24),MOL(24),THSTAR(24)
      COMMON/MPSFC2/Z0(24),BOWEN(24),ALBEDO(24),RHO(24)
      COMMON/MPSFC3/ZICONV(24),ZIMECH(24),WSTAR(24),VPTG(24)
      
      COMMON/MPSFC4/PTMP(MPHR,MPLVL), PTA(MPLVL)
      
      COMMON/MPSFC5/PTSUM(MPLVL),ANG(24),ANGD(24),ACRT(24),SMH(24)
      COMMON/MPSFC6/N500,N250,N000,DT500,DT250,DT000

C  COMMON MPLOC1 :  Variables used for calculation of the
C                   solar elevation angle, sunrise and sunset.
C                   LAT,LON: Latitude and longitude of ??
C
C  COMMON MPCONS :  CONST  : Constant for conversion of degrees to radians
C
C  COMMON MPSUB1 :  T      : Reference level surface temperature
C                   WSPD   : Reference level surface wind speed
C                   WDIR   : Reference level surface wind direction
C                   ZREF   : Reference height for wind speed and
C                            wind direction
C                   ZTREF  : Reference height for surface temperature
C                   P      : Surface pressure
C                   IPCODE : Precipitation code                          ! dtb #011 01180
c                   PAMT   : Precipitation amount (mm)                   ! dtb #011 01180
C
C  COMMON MPSUB2 :  RN     : Net radiation
C                   QR     : Incoming radiation
C                   CCVR   : Cloud cover
C                   NO_SKY : Missing value flag for cloud cover for use  ! dtb #122 02096
c                            in Stage 3.  It is intialized in MPPBL to   ! dtb #122 02096
c                            OSTSKY(2)                                   ! dtb #122 02096
C
C  COMMON MPSUB3 :  PR     : Upper air pressure
C                   TMP    : Upper air temperature
C                   HT     : Upper air height
C
C  COMMON MPSFC1 :  HFLUX  : Sensible heat flux
C                   USTAR  : Friction velocity
C                   MOL    : Monin-Obukhov length
C
C  COMMON MPSFC2 :  Z0     : Surface roughness length                    ! dtb #111 02045
c                            Changed all occurrences of 'ZO' to 'Z0'     ! dtb #111 02045
C                   BOWEN  : Bowen ratio
C                   ALBEDO : Surface albedo
C                   RHO    : Density of dry air
C
C  COMMON MPSFC3 :  ZICONV : Convective planetary boundary layer height
C                   ZIMECH : Stable planetary boundary layer height
C                   WSTAR  : Convective velocity scale
C                   VPTG   : Vertical potential temperature gradient
C
C  COMMON MPSFC4 :  PTMP   : Potential temperture
C                   PTA    : Integrated area under the potential temperature
C                            profile
C
C  COMMON MPSFC5 :  PTSUM  : Sum of the integrated area under the
C                            potential temp. profile
C                   ANG    : Solar elevation angle
C                   SMH    : Time-integrated heat flux
C
C  COMMON MPSFC6 :  N500   : Number of times the vptg computed over 500 m
C                   N250   : Number of times the vptg computed over 250 m
C                   N000   : Number of times the default vptg used
C                   DT500  : Sum of the vptg's in the 500 m layer
C                   DT250  : Sum of the vptg's in the 250 m layer
C                   DT000  : Sum of the default vptg's

