C  UA1.INC
C***    UA PATHWAY
C
C       WE ANTICIPATE THESE WILL BE USEFUL BOTH
C       IN THE PREPROCESSING AND IN THE MET PROCESSOR
C
      REAL        UASRISE, UASSET
      INTEGER, PARAMETER :: UAMH=48,UAMV=6,UAML=300,UAVR=12
      INTEGER     UAOBS,UABLKF
      INTEGER     UAWINDOWBEGIN, UAWINDOWEND
      Integer     UASRWINDOWBEGIN, UASRWINDOWEND
      INTEGER     UA0012WINDOWBEGIN, UA0012WINDOWEND
      CHARACTER   UALOC*8,UALON*8,UALAT*8,UAFMT*8
C
      COMMON /UABK01/ UAOBS(UAMH,UAML,UAMV)
      COMMON /UABK02/ UALOC,UALON,UALAT,UAFMT,UABLKF, UASRISE, UASSET
      COMMON /UABK2A/ UAWINDOWBEGIN, UAWINDOWEND
      COMMON /UABK2B/ UASRWINDOWBEGIN, UASRWINDOWEND
      COMMON /UABK2c/ UA0012WINDOWBEGIN, UA0012WINDOWEND
C
C       UAMH            MAXIMUM NUMBER OF HOURS MERGED
C                         Should match MPHR parameter in MP2.INC
C       UAMV            MAXIMUM NUMBER OF VARIABLES IN SOUNDINGS
C       UAML            MAXIMUM NUMBER OF LEVELS IN A SOUNDING
C                         Should match MPLVL parameter in MP2.INC,
C                         and AD2 parameter in WORK1.INC
C       UAVR            MAXIMUM NUMBER OF QA VARIABLES (VECTOR+SCALAR)
C       UALOC           UA STATION ID
C       UAOBS           ARRAY OF SOUNDING DATA (HOUR,LEVEL,VARIABLE)
C       UALON           LONGITUDE*100 (DEGREES) (E.G. 8015E)
C       UALAT           LATTITUDE*100 (DEGREES) (E.G. 3012N)
C       UAFMT           UA SOUNDING FORMAT (E.G. 6201FB)
C       UABLKF          UA BLOCKING FACTOR FOR NWS DATA FILE

C       UAWINDOWBEGIN     The number of hours before sounding time to 
C                         search for the morning sounding
C       UAWINDOWEND       The number of hours after sounding time to 
C                         search for the morning sounding
C       UASRWINDOWBEGIN   The number of hours before sunrise to 
C                         search for the morning sounding
C       UASRWINDOWEND     The number of hours after sunrise to 
C                         search for the morning sounding
C       UA0012WINDOWBEGIN The number of hours before 00Z/12Z to 
C                         search for the morning sounding
C       UA0012WINDOWEND   The number of hours after 00Z/12Z to 
C                         search for the morning sounding
 

