C----  OS2.INC
C
      INTEGER      DEV31,DEV32,OSDAY1,OSDAY2,OSYR1,OSYR2
      INTEGER      OSGDY1,OSGDY2,OSGMO1,OSGMO2,OSGDY,OSGMO,OSGYR,OSGHR
      INTEGER      OSGMN,OSLDY,OSLMO,OSLYR,OSLHR,OSLMI,OSDAY,OSMO,OSHR
      INTEGER      OSMN,OSYR,OSDAYC,OSAVG,OSMIN,OSLST,OSTIME
      INTEGER      OSSAUD,OSVAUD,OSAUD1,OSAUD2,OSSTRA
      REAL         OSQA, OSTSKY
      CHARACTER*96 DISK31,DISK32
      LOGICAL      OSMIX

      COMMON /OSBK05/ DEV31,DEV32
      COMMON /OSBK06/ DISK31,DISK32
      COMMON /OSBK07/ OSDAY1,OSDAY2,OSDAYC,OSYR1,OSYR2,OSGDY1,
     1                OSGDY2, OSGMO1,OSGMO2,OSGDY,OSGMO,OSGYR,OSGHR,
     1                OSGMN,OSLDY,OSLMO,OSLYR,OSLHR,OSLMI,OSLST,
     1                OSTIME,OSSAUD(OSMV),OSVAUD(OSML,OSMVEC),
     1                OSAUD1(OSMV,0:3), OSAUD2(OSML,OSMVEC,0:3),
     1                OSSTRA(OSMV), OSQA(OSMV,4), OSTSKY(4)
      COMMON /OSBK08/ OSDAY(OSMH),OSMO(OSMH),OSHR(OSMH),OSAVG,OSMIN,
     1                OSYR(OSMH),OSMN(OSMH)
      COMMON /OSBK09/ OSMIX
C
C       DEV31           Device number of OS QA input data
C       DEV32           Device number of OS QA output data
C       DISK31          Filename for DEV31
C       DISK32          Filename for DEV32
C       OSDAY1,OSDAY2   Start and stop julian days for processing
C       OSDAYC          Current julian day being processed
C       OSYR1,OSYR2     Start and stop years for processing
C       OSGDY1,OSGDY2   Start and stop days (Gregorian)
C       OSGMO1,OSGMO2   STart and stop months (Gregorian)
C       OSGDY...OSGMN   Current day, month, year, hour and minute
C                       being processed
C       OSLDY...OSLMI   Last day, month, year, hour and
C                       minute processed
C       OSDAY...OSHR    Day, month, year, and hour for
C                       Each hour merged
C       OSLST           Conversion factor for user time to solar time,
C                        such that user time - factor = solar time
C       OSAVG           Number of observations reported each hour
C       OSMIN           Minimum acceptable number of obs. to compute
C                       one-hour averages
C                       If OSAVG(=EVEN) then OSMIN = OSAVG/2
C                                       else OSMIN = OSAVG/2 + 1
C       OSQA            QA default parameters
C       OSTSKY          QA defaults for TSKY variable
C       OSSAUD(I)       Audit indicator for onsite variables (i=1,56 to
C                          correspond to the SFQA array size)
C                          (corresponds to variables on AUDIT keyword)
C                          0 = No output of audit results
C                          1 = Output audit results for this (i) variable.
C       OSSTRA(I)       Trace indicator for onsite variableS (I=1,56 to
C                          correspond to the SFQA array size)
C                          (corresponds to variables on NO_MISSING keyword)
C                          0 = No trace output
C                          1 = Output trace data for this (i) variable.
C       OSAUD1(I,J)     Audit results for scalar variables
C                          J = 0 QA good counter
C                              1 missing counter
C                              2 lower bound violation counter
C                              3 upper bound violation counter
C       OSVAUD(L,I)     Same as OSSAUD except for vector variables,
C                          The special difference here is that we
C                          define this array after the os data map is
C                          defined.  thus we know whether requested
C                          vector variable to be audited occurs at this
C                          level (l), or not.
C       OSAUD2(L,I,J)   Same as OSAUD1 except for vector variables
C                           L = level
C                           I = variable
C                           J = audit results (see OSAUD1)

