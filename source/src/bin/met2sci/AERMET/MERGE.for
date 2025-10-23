      SUBROUTINE AERMET_MERGE (ISTAT)
C=====================================================================**
C          MERGE Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Combine on-site, rawinsonde and surface observations
C               in 24-hour blocks into a single formatted data file.
C
C     Called by:     MRPATH
C
C     Calls to:      BANNER, CHROND, ERROR, GMTLST, GREG, HR0024, ICHRND,
C                    JULIAN, MIXCNT, MRHDR, OSDUMP, OSFILL2, OSSWAP, UAMOVE
C                    FLIWK1, FLIWK2, FLSDG, FLSFC, FLZI, FLOS
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        03/07/97: Pacific Envirnomental Service (PES)
C                - improved messages and handling of error conditions
C        05/29/08: MACTEC Federal Programs
C                - corrected array index from 454 to 1454 for IWORK1
C                  array in JOS
C        03/15/09: added capability to process hrly-avg 1-min ASOS
C                - rename variables, removing most 'IWORK' variables
C        10/2009 - Added flag ISASOS() to the surface data read
C                  statement; processed flag - if present continue; if 
C                  missing, write a message and stop
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE

      INTEGER      ISTAT, NUA_PREV, NUA_CURR, IUAHR, ISFHR
      INTEGER      JASOSDATE, IASOSGYR, IASOSGMO, IASOSGDAY, IASOSGHR
      INTEGER      JASOSHR, IASOSJDAY, IASOSDAYC, IOSHR, JMRG1, JMRG2
      INTEGER      IYEAR, IJULDA, IMRMO, IMRDA
      INTEGER      JMR, JMRYR, JMRJDAY, JMRMO, JMRDAY, M, ISDG
      REAL         ASOSWS, ASOSWD
      INTEGER      JULIAN, IOST13,IOST22,IOST32, JV,NV, LEV,LEV2,
     &             JLEV, IHR, IVBL, NVBL, KDAY, LVBL, IFLAG, NUM
      INTEGER      UAJDAY, SFJDAY
      INTEGER      NUA_PROCESSED, NSF_PROCESSED, NOS_PROCESSED, 
     &             NASOS_PROCESSED
      INTEGER      OSCNTR, RCNTR, PCNTR
      INTEGER      DASTAT(10,8), DEVICE
      INTEGER      JUA, JSF, JOS, JASOS
      CHARACTER*5  ADAY
      LOGICAL      OSEOF,SFEOF,UAEOF, ASOSEOF
      LOGICAL      PRVDAY, CURDAY, WR2RPT

      CHARACTER    PROGNAME*9, VERSN*6, CHR1*7, CHR2*5, CHR3*4, CHR4*5
      CHARACTER    CALL4_A1*4, CALL3_A1*3, cWBAN_A1*5

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      DATA OSEOF/.FALSE./, SFEOF/.FALSE./, UAEOF/.FALSE./
      DATA ASOSEOF/.FALSE./
      DATA PRVDAY/.FALSE./, CURDAY/.FALSE./
      DATA WR2RPT/.FALSE./
      DATA NUA_PROCESSED /0/, NSF_PROCESSED/0/, NOS_PROCESSED/0/
      DATA NASOS_PROCESSED /0/
      DATA RCNTR/0/, PCNTR/0/, OSCNTR/0/
      DATA IFLAG/0/

      DATA PATH/'MERGE     '/, LOC/' MERGE'/

      IWBAN_A1 = 0
C=======================================================================
C
C    The following is a map of the work arrays which contain the
C    meteorological data
C
C    UADAYC = Upper air chronological day
C    IWORK1(11) = Number of levels in a sounding
C    SFDAYC = Surface obs. chronological day (computational)
C    IWORK1(101:122) = Surface obs. data, one observation, temporary
C    OSDAYC = On-site data chronological day (computational)
C    JMRYR, JMRJDAY = Current merge year : Julian day
C    IWORK1(910:911) = Beginning : ending chronological days to merge
C    JMRMO = Current merge month, returned from routine 'GREG'
C    JMRDAY = Current merge day, returned from routine 'GREG'
C
C    IWORK2(1:UAML,1:6) = Upper air data, one sounding, temporary
C
C    IWORK1(20)   = Number of upper air obs. to write for the merge day
C    IWORK1(21)   = Number of sfc. obs. to write for the merge day
C    IWORK1(22)   = Number of on-site obs. to write for the merge day
C    IWORK1(23)   = Number of hrly-avg 1-min ASOS obs to write for the merge day
C
C=======================================================================
C
C     IOST32 = RECORD STATUS PASSED BACK FROM OSFILL2:
C
C     IOST32  RECORD STATUS   0 = fill to buffer worked ok
C                             1 = filled buffer but had read errors
C                             2 = the allowable number of read errors
C                                 has been exceeded (MAXERR)
C                             3 = end of file encountered sooner
C                                 than expected.
C                             4 = end of file encountered
C
C    DASTAT(1:10,1:7) = Array of statistics for summary table
C
C    IFLAG          = Flag returned from OSDUMP
C    KDAY           = Loop index beginning with merge chronological day
C                      IWORK1(910) and ending with day IWORK1(911);
C                      also used in determining chronological days if
C                      there was no merge XDATES keyword
C
C    NUA_PROCESSED,   = Counters tracking the number of reads of the files
C    NSF_PROCESSED,
C    NOS_PROCESSED
C    OSCNTR         = Counter for subroutine osfill2
C
C    PRVDAY,CURDAY, = Logical flag indicating whether or not there is a
C     NXTDAY           sounding for the previous, current and next days
C    UAEOF, SFOEF,  = END-OF-FILE FLAGS
C     OSEOF, ASOSEOF
C    WR2RPT         = Flag indicating if the daily statistics have been
C                      written to the summary file
C    ISASOS         - (Character variable) - Flag indicating if the data
C                     are ASOS observations ('A') or non-ASOS ('N')
C=======================================================================

C---- Select the unit to write the summary report
      IF( STATUS(1,1) .EQ. 2 )THEN
         DEVICE = DEV50
      ELSE
         DEVICE = DEVIO
      ENDIF

C---- Echo user input parameters to report file; if the user did not
C     specify begin and end dates, wait until the dates are determined
C     based on the input data
c     CALL BANNER ( DEVICE )
c     WRITE(DEVICE,130)
c     IF(  MRYR1 .GT. 0 )THEN
c        WRITE(DEVICE,200) MRYR1,MRGMO1,MRGDY1,MRYR2,MRGMO2,MRGDY2
c     ENDIF

C---- WRITE HEADER FOR USER OUTPUT SUMMARY TABLE
c     WRITE(DEVICE,210)

C---- Write each file's headers to the formatted output file and check
C     the status of the write by interrogating 'NUM'
      NUM = 5
      CALL MRHDR(PATH,' MRHDR  ',NUM,DEV13,DEV22,DEV32,DEV40,DEV60,
     &           ISTAT)
      LOC = 'MERGE'
      IF( ISTAT .EQ. 1 )THEN
         MESS =  BLNK80
         WRITE(MESS,135)
         CALL ERRHDL(0,PATH,'E66',LOC,MESS)
         RETURN
      ENDIF

      BUF80(2) = BLN132
      BUF80(2) = ' EOH: END OF MERGE HEADERS'
      BUF03 = '***'
      WRITE (DEV40,83) BUF03, BUF80(2)

C-----------------------------------------------------------------------
C---- Flush all arrays; initialize variables
      CALL FLIWK1
      CALL FLIWK2
      CALL FLSDG(UAMH)
      CALL FLSFC(SFMH)
      CALL FLOS(OSMH)

      IWORK1(20) = 0
      NUA_PREV   = 0
      NUA_CURR   = 0
      IWORK1(21) = 0
      IWORK1(22) = 0
      IWORK1(23) = 0

C=======================================================================
C     Read each file and retrieve the first observation in each file
C     if there is data to merge
C     NOTE: the sounding data are already on a 1-24 clock
C
C     JUA = Date of upper air data in the form YYMMDD;
C     JSF = Date of surface data in the form YYMMDD;
C     JOS = Date of on-site data in the form YYMMDD;
C     IUAHR = Save current hour for upper air data
C     ISFHR = Save current hour for surface data
C     IOSHR = Save current hour for on-site data
C=======================================================================

C     ---- PROCESS THE 1ST OBSERVATION OF UPPER AIR DATA ----

      IF( STATUS(2,8) .LT. 2 )THEN
         UAEOF = .TRUE.
         UADAYC = 0
         GO TO 22
      ENDIF

      JUA = 0
      READ(DEV13,1350,IOSTAT=IOST13,ERR=13010,END=13040) UAGYR,
     &                           UAGMO, UAGDY, UAGHR, IWORK1(11)
      JUA = 10000*UAGYR + 100*UAGMO + UAGDY
      IUAHR = UAGHR
      DO LEV = 1,IWORK1(11)
         READ(DEV13,1355,IOSTAT=IOST13,ERR=13020,END=13030)
     &                           (IWORK2(LEV,IVBL),IVBL=1,UAMV)
      ENDDO
      NUA_PROCESSED = NUA_PROCESSED + 1

      UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
      CALL CHROND( PATH,UAGYR,UAJDAY,UADAYC )
      IF( UADAYC .LT. 0 )THEN
         MESS =  BLNK80
         WRITE(MESS,140) PATHWD(2), UAGHR
         CALL ERRHDL(JUA,PATH,'E60',LOC,MESS)
         ISTAT = 1
         RETURN
      ENDIF

      GO TO 22                        ! To process SURFACE data

13010 MESS =  BLNK80
      WRITE(MESS,255) JUA, IUAHR
      CALL ERRHDL(NUA_PROCESSED,PATH,'E62',LOC,MESS)
      ISTAT = 1
      RETURN

13020 MESS =  BLNK80
      WRITE(MESS,256) LEV, UAGHR
      CALL ERRHDL(JUA,PATH,'E62',LOC,MESS)
      ISTAT = 1
      RETURN

13030 MESS =  BLNK80
      WRITE(MESS,257) LEV,UAGHR
      CALL ERRHDL(JUA,PATH,'E62',LOC,MESS)
      ISTAT = 1
      RETURN

13040 UAEOF = .TRUE.

C-----------------------------------------------------------------------

C     ---- PROCESS THE 1ST OBSERVATION OF SURFACE DATA ----
   
   22 CONTINUE
   
      IF( STATUS(3,8) .LT. 2 )THEN
        SFEOF = .TRUE.
        SFDAYC = 0
        GO TO 27
      ENDIF

      JSF = 0
      READ(DEV22,2265,IOSTAT=IOST22,ERR=22010,END=22040) SFGYR,SFGMO,
     &      SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,22),ISASOS1

      NSF_PROCESSED = NSF_PROCESSED + 1

      JSF = 10000*SFGYR + 100*SFGMO + SFGDY
      ISFHR = SFGHR
      SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)
      
      IF( SFGHR .EQ. 0 )THEN
         CALL HR0024(SFGYR,SFJDAY,SFGHR)
         CALL GREG(SFGYR,SFJDAY,SFGMO,SFGDY)

      ENDIF

      CALL CHROND( PATH,SFGYR,SFJDAY,SFDAYC )
      IF( SFDAYC .LT. 0 )THEN
         MESS =  BLNK80
         WRITE(MESS,140) PATHWD(3), SFGHR
         CALL ERRHDL(JSF,PATH,'E60',LOC,MESS)
         ISTAT = 1
         RETURN
      ENDIF

      GO TO 27                        ! To process 1-MIN ASOS data

22010 MESS =  BLNK80
      WRITE(MESS,267) JSF, ISFHR
      CALL ERRHDL(NSF_PROCESSED,PATH,'E63',LOC,MESS)
      ISTAT = 1
      RETURN

22040 SFEOF = .TRUE.

C-----------------------------------------------------------------------

C     ---- PROCESS THE 1ST OBSERVATION OF HRLY AVG 1-MIN ASOS DATA ----

   27 CONTINUE

      IF( STATUS(3,33) .LT. 2 )THEN
         ASOSEOF = .TRUE.
         IASOSDAYC = 0
         GO TO 32
      ENDIF
      
C---- Initialize JASOSDATE
      JASOSDATE = 0

C---- Read header record for version number, WBAN number and call IDs
      READ(DEV23,*,IOSTAT=IOST22,ERR=22510,END=22540)
     &             PROGNAME,CHR1,VERSN,CHR2,IWBAN_A1,
     &             CHR3,CHR4,CALL4_A1
      
      CALL3_A1 = CALL4_A1(2:4)
      
C---  Write warning/informational message regarding 1-min ASOS wind data,
C     including version number of the AERMINUTE program used to generate
C     the 1-min ASOS wind data
      MESS =  BLNK80
      WRITE(MESS,146) PROGNAME, VERSN, IWBAN_A1, CALL4_A1
      CALL ERRHDL(JASOSDATE,PATH,'W66',LOC,MESS)
         
C---  Assign WBAN and Call IDs from 1-minute file header to variables 
C     used in sub_FNDCOMDT to search for ASOS commission date
      iSfcObsWBAN  = IWBAN_A1
      cSfcObsCALL4 = CALL4_A1
      cSfcObsCALL3 = CALL3_A1

C---  Write integer WBAN for 1-min ASOS data to character variable
      WRITE(cWBAN_A1,'(I5)') IWBAN_A1
      IF( .not. SrchCommDate )THEN
         CALL FNDCOMDT(PATH)
      ENDIF

C---  Check for consistency between 1-min ASOS WBAN and surface 
C     station WBAN
      IF( SFDAYC .GT. 0 .AND. INDEX(SFLOC, cWBAN_A1) .EQ. 0 )THEN
C---     1-min ASOS WBAN .ne. Surface data WBAN; issue warning message
         MESS =  BLNK80
         ECODE = 'W66'
         WRITE(MESS,147) IWBAN_A1, SFLOC
         CALL ERRHDL(JASOSDATE,PATH,ECODE,LOC,MESS)
      ELSEIF( SFDAYC .GT. 0 .AND. INDEX(SFLOC, cWBAN_A1) .GT. 0 )THEN
C---     1-min ASOS WBAN and Surface data WBAN match; 
C        issue informational message
         MESS =  BLNK80
         ECODE = 'I66'
         WRITE(MESS,148) IWBAN_A1, SFLOC
         CALL ERRHDL(JASOSDATE,PATH,ECODE,LOC,MESS)
      ENDIF

C     read first obs
      JASOS = 0
      READ(DEV23,*,IOSTAT=IOST22,ERR=22510,END=22540)
     &            IASOSGYR, IASOSGMO, IASOSGDAY, IASOSGHR,
     &            ASOSWS, ASOSWD

      NASOS_PROCESSED = NASOS_PROCESSED + 1

      JASOSDATE = 10000*IASOSGYR + 100*IASOSGMO + IASOSGDAY
      JASOSHR = IASOSGHR
      IASOSJDAY = JULIAN(IASOSGYR,IASOSGMO,IASOSGDAY)

      IF( IASOSGHR .EQ. 0 )THEN
         CALL HR0024(IASOSGYR,IASOSJDAY,IASOSGHR)
         CALL GREG(IASOSGYR,IASOSJDAY,IASOSGMO,IASOSGDAY)
      ENDIF

      CALL CHROND( PATH,IASOSGYR,IASOSJDAY,IASOSDAYC )

      IF( IASOSDAYC .LT. 0 )THEN
         MESS =  BLNK80
         WRITE(MESS,145) IASOSGHR
         CALL ERRHDL(JASOSDATE,PATH,'E60',LOC,MESS)
         ISTAT = 1
         RETURN
      ENDIF

      GO TO 32                        ! To process ONSITE data

22510 MESS =  BLNK80
      WRITE(MESS,268) JASOSDATE, JASOSHR
      CALL ERRHDL(NASOS_PROCESSED,PATH,'E63',LOC,MESS)
      ISTAT = 1
      RETURN

22540 ASOSEOF = .TRUE.
C-----------------------------------------------------------------------

C     ---- PROCESS THE 1ST OBSERVATION OF ON-SITE DATA ----

   32 CONTINUE
C---  Initialize JOS and IOSHR in case no data found in OS QAOUT file
      JOS   = 0
      IOSHR = 0
      
      IF( STATUS(4,8) .LT. 2 )THEN
         OSEOF = .TRUE.
         IWORK1(250) = 0
         GO TO 9001
      ENDIF

      CALL OSFILL2(5,OSCNTR,DEV32,IOST32)
      NOS_PROCESSED = NOS_PROCESSED + 1

      IF( IOST32 .EQ. 0 )THEN
         CONTINUE
      ELSEIF( IOST32 .EQ. 4 )THEN
         NOS_PROCESSED = NOS_PROCESSED - 1
         GO TO 32040
      ELSE
C---     Error or premature EOF reading OS QAOUT file 
         GO TO 32010
      ENDIF

      IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1454),IWORK1(1453),
     &                       IWORK1(1452),IWORK1(1455),OSLST)
      OSDAYC = JULIAN(IWORK1(1454),IWORK1(1453),IWORK1(1452))

      JOS = 10000*IWORK1(1454) + 100*IWORK1(1453) + IWORK1(1452)
      IOSHR = IWORK1(1455)
      IF( IWORK1(1455) .EQ. 0 )THEN
         CALL HR0024(IWORK1(1454),OSDAYC,IWORK1(1455))
         CALL GREG(IWORK1(1454),OSDAYC,IWORK1(1453),IWORK1(1452))
      ENDIF

      CALL CHROND( PATH,IWORK1(1454),OSDAYC,IWORK1(250) )
      IF( IWORK1(250).LT.0 )THEN
         MESS =  BLNK80
         WRITE(MESS,140) PATHWD(4),IWORK1(1455)
         CALL ERRHDL(JOS,PATH,'E60',LOC,MESS)
         ISTAT =1
         RETURN
      ENDIF

      GO TO 9001                       ! To begin iteration on days

32010 MESS =  BLNK80
      WRITE(MESS,271) JOS, IOSHR
      CALL ERRHDL(NOS_PROCESSED,PATH,'E64',LOC,MESS)
      ISTAT = 1
      RETURN

32040 OSEOF = .TRUE.

C=======================================================================
C                       BEGIN ITERATION ON DAYS:
C
C     First determine if the user gave any merge dates.  If not use the
C     earliest date available from the 3 pathways as the beginning date
C     and use that day plus 367 as the last day to merge
C
C     Compute the merge beginning and ending julian days and chronological
C     days (IWORK1(910:911))

 9001 CONTINUE

C---- If there was no XDATES keyword for the MERGE pathway, then
C     the earliest day in the files is the day to start merging data

C     UADAYC  = first chronological day in the upper air data
C                   (=0 if there are no data)
C     SFDAYC  = first chronological day in the surface data
C                   (=0 if there are no data)
C     IWORK1(250) = first chronological day in the on-site data
C                   (=0 if there are no data)
C     KDAY = 1,2, or 3 depending on data availability
C     KDAY = 0 is an error condition - no data!!

      IF( STATUS(5,9) .EQ. 0 )THEN
C        No XDATES keyword for the MERGE pathway
C        Save the chronological day of the first obs. for each data
C        type, if processed, into a unique working array element
         KDAY = 0

         IF( UADAYC .GT. 0 )THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = UADAYC
         ENDIF

         IF( SFDAYC .GT. 0 )THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = SFDAYC
         ENDIF

         IF( IWORK1(250) .GT. 0 )THEN
            KDAY = KDAY + 1
            IWORK1(30 + KDAY) = IWORK1(250)
         ENDIF

         IF( KDAY .EQ. 0 )THEN
C---------- There is no upper air, surface wx. obs, 
C           or on-site data to merge
            MESS =  BLNK80
            WRITE(MESS,143)
            CALL ERRHDL(0,PATH,'E67',LOC,MESS)
            ISTAT = 1
            RETURN

         ELSEIF( KDAY .EQ. 1 )THEN
C---------- Only one of the data types was present - 
C           use that chronological day
            IWORK1(910) = IWORK1(31)

         ELSEIF( KDAY .EQ. 2 )THEN
C---------- Two of the data types were present - 
C           use the earliest chronological day
            IWORK1(910) = MIN (IWORK1(31),IWORK1(32))

         ELSE
C---------- Three of the data types were present - 
C           use the earliest chronological day
            IWORK1(910) = MIN (IWORK1(31),IWORK1(32),IWORK1(33))
         ENDIF

C------- Set default end date as start day plus 367
         IWORK1(911) = IWORK1(910) + 367
         MESS =  BLNK80
         WRITE(MESS,144) IWORK1(910),IWORK1(911)
         CALL ERRHDL(0,PATH,'I67',LOC,MESS)

C------- Compute year/month/day from chronological dates 
C        for the summary file
         CALL ICHRND(PATH,IWORK1(910),MRYR1,MRDAY1)
         CALL GREG(MRYR1,MRDAY1,MRGMO1,MRGDY1)
         CALL ICHRND(PATH,IWORK1(911),MRYR2,MRDAY2)
         CALL GREG(MRYR2,MRDAY2,MRGMO2,MRGDY2)

      ELSE
         MRDAY1 = JULIAN(MRYR1,MRGMO1,MRGDY1)
         MRDAY2 = JULIAN(MRYR2,MRGMO2,MRGDY2)
         JMRG1 = 10000*MRYR1 + 100*MRGMO1 + MRGDY1
         JMRG2 = 10000*MRYR2 + 100*MRGMO2 + MRGDY2

         CALL CHROND( PATH,MRYR1,MRDAY1,IWORK1(910) )
         IF( IWORK1(910).LT.0 )THEN
            MESS =  BLNK80
            ADAY = 'START'
            WRITE(MESS,142) ADAY
            CALL ERRHDL(JMRG1,PATH,'E60',LOC,MESS)
            ISTAT = 1
            RETURN
         ENDIF

         CALL CHROND( PATH,MRYR2,MRDAY2,IWORK1(911) )
         IF( IWORK1(911).LT.0 )THEN
            MESS =  BLNK80
            ADAY = ' END '
            WRITE(MESS,142) ADAY
            CALL ERRHDL(JMRG2,PATH,'E60',LOC,MESS)
            ISTAT = 1
            RETURN
         ENDIF
      ENDIF

C---- Print the banner and the beginning and end dates to merge
      PGNUM = PGNUM + 1
      CALL BANNER ( PGNUM, 2, VERSNO, DEVICE )
      WRITE(DEVICE,130)
      WRITE(DEVICE,200) MRYR1,MRGMO1,MRGDY1,MRYR2,MRGMO2,MRGDY2

C---- WRITE HEADER FOR USER OUTPUT SUMMARY TABLE
      WRITE(DEVICE,210)

C========================== BEGIN PROCESSING ===========================

      LOOP_DAYS: DO KDAY = IWORK1(910),IWORK1(911)
         CALL ICHRND(path,kday,iyear,ijulda)
         CALL GREG(iyear,ijulda,imrmo,imrda)
         JMR = 10000*IYEAR + 100*IMRMO + IMRDA

         WRITE( *,610 ) imrmo,imrda,iyear
  610    FORMAT('+  Stage 2: Merging month/day/year ',I2.2, 2('/',I2.2))


C                  ---- PROCESS THE UPPER AIR DATA ----

13000    CONTINUE

         IF( .NOT. UAEOF )THEN

13001       CONTINUE

            IF( UADAYC .LT. KDAY )THEN
C           UA day is before the merge day

               IF( UADAYC .EQ. (KDAY-1) )THEN                  
C                 The sounding in the temporary arrays is for the day 
C                 before the merge day                                  
                  IF( IWORK1(11) .GT. 0 )THEN                          
                     PRVDAY = .TRUE.                                 
                     NUA_PREV = NUA_PREV + 1
                     IWORK1(20) = IWORK1(20) + 1                      
                     CALL UAMOVE                                     
                  ENDIF                                             
               ENDIF                                                

               IWORK1(11) = 0

C              Read the next sounding
               READ(DEV13,1350,IOSTAT=IOST13,ERR=13011,END=13041) UAGYR,
     &              UAGMO, UAGDY, UAGHR, IWORK1(11)
               JUA = 10000*UAGYR + 100*UAGMO + UAGDY
               IUAHR = UAGHR
               DO LEV = 1,IWORK1(11)
                  READ(DEV13,1355,IOSTAT=IOST13,ERR=13021,END=13031)
     &                (IWORK2(LEV,IVBL),IVBL=1,UAMV)
               ENDDO
               NUA_PROCESSED = NUA_PROCESSED + 1

               UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
               CALL CHROND( PATH,UAGYR,UAJDAY,UADAYC )
               IF( UADAYC .LT. 0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(2), UAGHR
                  CALL ERRHDL(JUA,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

C              Process the sounding
               GO TO 13001

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

            ELSEIF( UADAYC .EQ. KDAY )THEN
C------------- The upper air day is equal to the merge day; move the
C              data from the work arrays to the permanent arrays

               IF( IWORK1(11) .GT. 0 )THEN
                  CURDAY = .TRUE.
                  NUA_CURR = NUA_CURR + 1
                  IWORK1(20) = IWORK1(20) + 1
                  CALL UAMOVE
               ENDIF

               IWORK1(11) = 0

C------------- Read the next sounding in the file
               READ(DEV13,1350,IOSTAT=IOST13,ERR=13011,END=13041) UAGYR,
     &              UAGMO, UAGDY, UAGHR, IWORK1(11)
               JUA = 10000*UAGYR + 100*UAGMO + UAGDY
               IUAHR = UAGHR

               DO LEV = 1,IWORK1(11)
                  READ(DEV13,1355,IOSTAT=IOST13,ERR=13021,END=13031)
     &                (IWORK2(LEV,IVBL),IVBL=1,UAMV)
               ENDDO
               NUA_PROCESSED = NUA_PROCESSED + 1

               UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
               CALL CHROND( PATH,UAGYR,UAJDAY,UADAYC )
               IF( UADAYC .LT. 0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(2),UAGHR
                  CALL ERRHDL(JUA,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 13001

            ENDIF
         ENDIF
         GO TO 22000


C-----------------------------------------------------------------------
C------- Continue here if there was an error reading the upper air data
C        (13011 = HDR ERROR, 13021 = LVL ERROR, 13031 = EOF @ LVL)

13011    MESS =  BLNK80
         WRITE(MESS,255) JUA,IUAHR
         CALL ERRHDL(NUA_PROCESSED,PATH,'E62',LOC,MESS)
         ISTAT = 1
         RETURN

13021    MESS =  BLNK80
         WRITE(MESS,256) LEV,UAGHR
         CALL ERRHDL(JUA,PATH,'E62',LOC,MESS)
         ISTAT = 1
         RETURN

13031    MESS =  BLNK80
         WRITE(MESS,257) LEV,UAGHR
         CALL ERRHDL(JUA,PATH,'E62',LOC,MESS)
         ISTAT = 1
         RETURN

C------- Continue here if an end of file was encountered

13041    UAEOF  = .TRUE.


C=======================================================================

C                  ---- PROCESS THE SURFACE DATA ----

22000    CONTINUE

         IF( .NOT. SFEOF )THEN
         
22001       CONTINUE

            IF( SFDAYC .LT. KDAY )THEN
C------------- The surface obs. day is before the merge day
               READ(DEV22,2265,IOSTAT=IOST22,ERR=22011,END=22041) SFGYR,
     &           SFGMO,SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,22),
     &           ISASOS1

               NSF_PROCESSED = NSF_PROCESSED + 1
               JSF = 10000*SFGYR + 100*SFGMO + SFGDY
               ISFHR = SFGHR

               SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)
               IF( SFGHR .EQ. 0 )THEN
                  CALL HR0024(SFGYR,SFJDAY,SFGHR)
                  CALL GREG(SFGYR,SFJDAY,SFGMO,SFGDY)
               ENDIF

               CALL CHROND( PATH,SFGYR,SFJDAY,SFDAYC )

               IF( SFDAYC.LT.0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(3),SFGHR
                  CALL ERRHDL(JSF,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 22001

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( SFDAYC .EQ. KDAY )THEN
C------------- The surface obs. day is equal to the merge day


C ---          If read was successful, make sure ASOS flag is either 'A' or 'N'.
C              If ASOS flag is not present or is not a valid code (old data 
C              from a previous version), then issue error message and abort 
C              further processing.
               IF( ISASOS1 .NE. 'A' .AND. ISASOS1 .NE. 'a' .AND.
     &             ISASOS1 .NE. 'N' .AND. ISASOS1 .NE. 'n' )THEN
                  MESS =  BLNK80
                  WRITE(MESS,138) ISASOS1
                  CALL ERRHDL(JSF,PATH,'E68',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               IWORK1(21) = IWORK1(21) + 1
C ---          Check for IWORK1(21) > 24; this would cause an
C              array subscript out-of-bounds runtime error and
C              could indicate a problem with the date sequence 
C              in the SURFACE data file.
               IF( IWORK1(21) .GT. 24 )THEN
C ---             Issue error message and abort
                  MESS =  BLNK80
                  WRITE(MESS,139) SFGYR,SFGMO,SFGDY
                  CALL ERRHDL(JSF,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF
               SFYR(IWORK1(21))   = SFGYR
               SFMO(IWORK1(21))   = SFGMO
               SFDAY(IWORK1(21))  = SFGDY
               SFHR(IWORK1(21))   = SFGHR
               SFASOS(IWORK1(21)) = ISASOS1
               
               DO NVBL = 1,22
                  SFOBS(IWORK1(21),NVBL+29) = IWORK1(NVBL+100)
               ENDDO

               READ(DEV22,2265,IOSTAT=IOST22,ERR=22011,END=22041) SFGYR,
     &           SFGMO,SFGDY, SFGHR, (IWORK1(100+IVBL),IVBL=1,22),
     &           ISASOS1

               NSF_PROCESSED = NSF_PROCESSED + 1
               JSF = 10000*SFGYR + 100*SFGMO + SFGDY
               ISFHR = SFGHR

               SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)
               IF( SFGHR .EQ. 0 )THEN
                  CALL HR0024(SFGYR,SFJDAY,SFGHR)
                  CALL GREG(SFGYR,SFJDAY,SFGMO,SFGDY)
               ENDIF

               CALL CHROND( PATH,SFGYR,SFJDAY,SFDAYC )
               IF( SFDAYC.LT.0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(3),SFGHR
                  CALL ERRHDL(JSF,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 22001

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( SFDAYC .GT. KDAY )THEN
C------------- The surface obs. day is after the merge day

               GO TO 22500         ! to process hrly-avg 1-min ASOS data
            ENDIF
         ENDIF

         GO TO 22500               ! to process hrly-avg 1-min ASOS data

C-----------------------------------------------------------------------
C-------- Continue here if there was an error reading the hourly surface data
22011     MESS =  BLNK80
          WRITE(MESS,267) JSF,ISFHR
          CALL ERRHDL(NSF_PROCESSED,PATH,'E63',LOC,MESS)
          ISTAT = 1
          RETURN

C-------- Continue here if an end of file was encountered on the surface file

22041     SFEOF  = .TRUE.

C=======================================================================

C           ---- PROCESS THE HRLY-AVERAGED 1-MIN ASOS DATA ----

22500    CONTINUE

         IF( .NOT. ASOSEOF )THEN

22501       CONTINUE

            IF( IASOSDAYC .LT. KDAY )THEN
C------------- The ASOS day is before the merge day
               READ(DEV23,*,IOSTAT=IOST22,ERR=22511,END=22541)
     &             IASOSGYR, IASOSGMO, IASOSGDAY, IASOSGHR,
     &             ASOSWS, ASOSWD

               NASOS_PROCESSED = NASOS_PROCESSED + 1
               JASOSDATE = 10000*IASOSGYR + 100*IASOSGMO + IASOSGDAY
               JASOSHR = IASOSGHR

               IASOSJDAY = JULIAN(IASOSGYR,IASOSGMO,IASOSGDAY)
               IF( IASOSGHR .EQ. 0 )THEN
                  CALL HR0024(IASOSGYR,IASOSJDAY,IASOSGHR)
                  CALL GREG(IASOSGYR,IASOSDAYC,IASOSGMO,IASOSGDAY)
               ENDIF
               CALL CHROND( PATH,IASOSGYR,IASOSJDAY,IASOSDAYC )

               IF( IASOSDAYC .LT. 0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,145) JASOSHR
                  CALL ERRHDL(JASOSDATE,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 22501

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( IASOSDAYC .EQ. KDAY )THEN
C------------- The ASOS day is equal to the merge day

               IWORK1(23) = IWORK1(23) + 1
               IASOSYR(IWORK1(23))   = IASOSGYR
               IASOSMO(IWORK1(23))   = IASOSGMO
               IASOSDAY(IWORK1(23))  = IASOSGDAY
               IASOSHR(IWORK1(23))   = IASOSGHR
               ASOSOBS(IWORK1(23),1) = ASOSWS
               ASOSOBS(IWORK1(23),2) = ASOSWD
c               ASOSOBS(IWORK1(23),3) = ASOSANEM

               READ(DEV23,*,IOSTAT=IOST22,ERR=22511,END=22541)
     &             IASOSGYR, IASOSGMO, IASOSGDAY, IASOSGHR,
     &             ASOSWS, ASOSWD

               NASOS_PROCESSED = NASOS_PROCESSED + 1
               JASOSDATE = 10000*IASOSGYR + 100*IASOSGMO + IASOSGDAY
               JASOSHR = IASOSGHR

               IASOSJDAY = JULIAN(IASOSGYR,IASOSGMO,IASOSGDAY)
               IF( IASOSGHR .EQ. 0 )THEN
                  CALL HR0024(IASOSGYR,IASOSJDAY,IASOSGHR)
                  CALL GREG(IASOSGYR,IASOSDAYC,IASOSGMO,IASOSGDAY)
               ENDIF
               CALL CHROND( PATH,IASOSGYR,IASOSJDAY,IASOSDAYC )

               IF( IASOSDAYC .LT. 0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,145) JASOSHR
                  CALL ERRHDL(JASOSDATE,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 22501

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( IASOSDAYC .GT. KDAY )THEN
C------------- The ASOS day is after the merge day

               GO TO 32000            ! to process site-specific data
            ENDIF
         ENDIF

         GO TO 32000                  ! to process site-specific data

C-----------------------------------------------------------------------
C------- Continue here if there was an error reading the ASOS data
22511    MESS =  BLNK80
         WRITE(MESS,268) JASOSDATE,JASOSHR
         CALL ERRHDL(NASOS_PROCESSED,PATH,'E63',LOC,MESS)
         ISTAT = 1
         RETURN

C------- Continue here if an end of file was encountered on the surface file

22541    ASOSEOF  = .TRUE.

C=======================================================================

C                  ---- PROCESS THE ON-SITE DATA ----

32000    CONTINUE

         IF( .NOT. OSEOF )THEN
         
32001       CONTINUE

            IF( IWORK1(250) .LT. KDAY )THEN
C------------- The on-site day is before the merge day
               CALL OSFILL2(5,OSCNTR,DEV32,IOST32)
               NOS_PROCESSED = NOS_PROCESSED + 1

               IF( IOST32 .EQ. 0 )THEN
                  CONTINUE
               ELSEIF( IOST32 .EQ. 4 )THEN
                  NOS_PROCESSED = NOS_PROCESSED - 1
                  GO TO 32041
               ELSE
                  GO TO 32011
               ENDIF

               IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1454),IWORK1(1453),
     &                           IWORK1(1452),IWORK1(1455),OSLST)
               OSDAYC = JULIAN(IWORK1(1454),IWORK1(1453),IWORK1(1452))
               IF( IWORK1(1455) .EQ. 0 )THEN
                  CALL HR0024(IWORK1(1454),OSDAYC,IWORK1(1455))
                  CALL GREG(IWORK1(1454),OSDAYC,IWORK1(1453),
     &                      IWORK1(1452))
               ENDIF
               JOS = 10000*IWORK1(1454)+100*IWORK1(1453)+IWORK1(1452)
               IOSHR = IWORK1(1455)

               CALL CHROND( PATH,IWORK1(1454),OSDAYC,IWORK1(250) )
               IF( IWORK1(250).LT.0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(4),IWORK1(1455)
                  CALL ERRHDL(JOS,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 32001

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( IWORK1(250) .EQ. KDAY )THEN
C------------- The onsite day are equal to the merge day; store the data
C              and read again

               IWORK1(22) = IWORK1(22) + 1

               CALL OSSWAP(IWORK1(22))
               CALL OSFILL2(5,OSCNTR,DEV32,IOST32)
               NOS_PROCESSED = NOS_PROCESSED + 1

               IF( IOST32 .EQ. 0 )THEN
                  CONTINUE
               ELSEIF( IOST32 .EQ. 4 )THEN
                  NOS_PROCESSED = NOS_PROCESSED - 1
                  GO TO 32041
               ELSE
                  GO TO 32011
               ENDIF

               IF( OSLST .NE. 0) CALL GMTLST(IWORK1(1454),IWORK1(1453),
     &                           IWORK1(1452),IWORK1(1455),OSLST)
               OSDAYC = JULIAN(IWORK1(1454),IWORK1(1453),IWORK1(1452))
               IF( IWORK1(1455) .EQ. 0 )THEN
                  CALL HR0024(IWORK1(1454),OSDAYC,IWORK1(1455))
                  CALL GREG(IWORK1(1454),OSDAYC,IWORK1(1453),
     &                      IWORK1(1452))
               ENDIF

               JOS = 10000*IWORK1(1454)+100*IWORK1(1453)+IWORK1(1452)
               IOSHR = IWORK1(1455)
               CALL CHROND( PATH,IWORK1(1454),OSDAYC,IWORK1(250) )
               IF( IWORK1(250).LT.0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,140) PATHWD(4),IWORK1(1455)
                  CALL ERRHDL(NOS_PROCESSED,PATH,'E60',LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF

               GO TO 32001

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
            ELSEIF( IWORK1(250) .GT. KDAY )THEN
C------------- The onsite day is after the merge day

               GO TO 42000
            ENDIF
         ENDIF

         GO TO 42000

C-----------------------------------------------------------------------
C------- Continues here if an error processing on-site data was encountered
32011    MESS =  BLNK80
         WRITE(MESS,271) JOS,IOSHR
         CALL ERRHDL(NOS_PROCESSED,PATH,'E64',LOC,MESS)
         ISTAT = 1
         RETURN

C------- Continues here if an eof was encountered in the on-site data
32041    OSEOF = .TRUE.


C=======================================================================

C             ---- WRITE THE DATA TO THE OUTPUT FILE ----

42000    CONTINUE

         CALL ICHRND(PATH,KDAY,JMRYR,JMRJDAY)
         IF( (JMRYR .LT. 0) .OR. (JMRJDAY .LT. 0) )THEN
            MESS =  BLNK80
            WRITE(MESS,141)
            CALL ERRHDL(JMR,PATH,'E61',LOC,MESS)
            ISTAT = 1
            RETURN
         ENDIF
         CALL GREG (JMRYR, JMRJDAY, JMRMO, JMRDAY )
         WRITE(DEV40,1110) JMRYR, JMRMO, JMRDAY, JMRJDAY,
     &                     IWORK1(20), NUA_PREV, NUA_CURR,
     &                     IWORK1(21),IWORK1(22),IWORK1(23)

         IF( IWORK1(20).GT.0 )THEN
            DO M = 1,IWORK1(20)
               WRITE(DEV40,1110) UAYR(M),UAMO(M),UADAY(M),UAHR(M),
     &                           UALEV(M)
C           This code (IF..THEN) avoids writing a blank record if there
C           is only one sounding with no levels
               IF( UALEV(M) .GT. 0 )THEN
                  WRITE(DEV40,1110) ((UAOBS(M,JLEV,JV),JV=1,UAMV),
     &                                JLEV=1,UALEV(M))
               ENDIF
            ENDDO
         ENDIF

         IF( IWORK1(21) .GT. 0 )THEN
            DO M = 1,IWORK1(21)
               WRITE(DEV40,1111) SFYR(M),SFMO(M),SFDAY(M),SFHR(M),
     &                           SFASOS(M)
               WRITE(DEV40,1110) (SFOBS(M,IVBL+29),IVBL=1,22)
            ENDDO
         ENDIF

         IF( IWORK1(23) .GT. 0 )THEN
            DO M = 1,IWORK1(23)
               WRITE(DEV40,1110) IASOSYR(M),IASOSMO(M),IASOSDAY(M),
     &                           IASOSHR(M)
               WRITE(DEV40,1120) ASOSOBS(M,1),ASOSOBS(M,2)
            ENDDO
         ENDIF

         IF( IWORK1(22) .GT. 0 )THEN
            DO LVBL = 1,IWORK1(22)
               CALL OSDUMP(0,LVBL,IFLAG)
               IF( IFLAG .NE. 0 )THEN
                  RETURN
               ENDIF
            ENDDO
         ENDIF

C-----------------------------------------------------------------------
C------- Prepare the statistics for the day for the summary file
         RCNTR = RCNTR + 1
         DASTAT(RCNTR,1) = JMRYR
         DASTAT(RCNTR,2) = JMRMO
         DASTAT(RCNTR,3) = JMRDAY
         DASTAT(RCNTR,4) = IWORK1(20)
         DASTAT(RCNTR,5) = IWORK1(31)  ! this DASTAT element is never used
         DASTAT(RCNTR,6) = IWORK1(21)
         DASTAT(RCNTR,7) = IWORK1(22)
         DASTAT(RCNTR,8) = IWORK1(23)
         WR2RPT = .FALSE.

C------- Write the statistics when 10 days have been accumulated

         IF( RCNTR .EQ. 10 )THEN
            WRITE(DEVICE,510) (DASTAT(JV,2),DASTAT(JV,3),JV=1,RCNTR)
            WRITE(DEVICE,515) (DASTAT(JV,4),JV=1,RCNTR)
            WRITE(DEVICE,525) (DASTAT(JV,6),JV=1,RCNTR)
            WRITE(DEVICE,530) (DASTAT(JV,7),JV=1,RCNTR)
            WRITE(DEVICE,535) (DASTAT(JV,8),JV=1,RCNTR)

C---------- Reinitialize everything; WR2RPT indicates the stats have
C           been written; add 1 to the page counter (pcntr)

            DO JV = 1,10
               DO NV = 1,8
                  DASTAT(JV,NV) = 0
               ENDDO
            ENDDO

            RCNTR = 0
            WR2RPT = .TRUE.
            PCNTR = PCNTR + 1
         ENDIF

C-------- Start a new page when 8 sets of 10-day statistics (= 1 page)
C         have been written

         IF( PCNTR .EQ. 8 )THEN
            PGNUM = PGNUM + 1
            CALL BANNER ( PGNUM,2,VERSNO,DEVICE )
            WRITE(DEVICE,211)
            PCNTR = 0
         ENDIF

C-----------------------------------------------------------------------

         DO ISDG = 1,NUA_CURR
            UAYR(ISDG)  = UAYR(NUA_PREV+ISDG)
            UAMO(ISDG)  = UAMO(NUA_PREV+ISDG)
            UADAY(ISDG) = UADAY(NUA_PREV+ISDG)
            UAHR(ISDG)  = UAHR(NUA_PREV+ISDG)
            UALEV(ISDG) = UALEV(NUA_PREV+ISDG)
            DO LEV2 = 1,UALEV(NUA_PREV+ISDG)
               DO IVBL = 1,UAMV
                 UAOBS(ISDG,LEV2,IVBL)=UAOBS(NUA_PREV+ISDG,LEV2,IVBL)
               ENDDO
            ENDDO          
         ENDDO

C------- Reinitialize the remainder of the arrays
         DO IHR = NUA_CURR+1,UAMH
            UAYR(IHR)  = -9
            UAMO(IHR)  = -9
            UADAY(IHR) = -9
            UAHR(IHR)  = -9
            DO LEV2 = 1,UAML
               DO IVBL = 1,UAMV
                  UAOBS(IHR,LEV2,IVBL) = UAQA(IVBL,2)
               ENDDO
            ENDDO
         ENDDO

         NUA_PREV = NUA_CURR
         NUA_CURR = 0
         IWORK1(20) = NUA_PREV


         CALL FLSFC(SFMH)
         CALL FLOS(OSMH)

C        IWORK1(21) = Number of sfc. obs. to write for the merge day
C        IWORK1(22) = Number of on-site obs. to write for the merge day
C        IWORK1(23) = Number of hrly-avg 1-min ASOS obs to write for the merge day
C
         IWORK1(21) = 0
         IWORK1(22) = 0
         IWORK1(23) = 0

      ENDDO LOOP_DAYS


C=======================================================================
C---- Write the final statistics and message and return

      IF( .NOT. WR2RPT )THEN
         WRITE(DEVICE,510) (DASTAT(JV,2),DASTAT(JV,3),JV=1,RCNTR)
         WRITE(DEVICE,515) (DASTAT(JV,4),JV=1,RCNTR)
         WRITE(DEVICE,525) (DASTAT(JV,6),JV=1,RCNTR)
         WRITE(DEVICE,530) (DASTAT(JV,7),JV=1,RCNTR)
         WRITE(DEVICE,535) (DASTAT(JV,8),JV=1,RCNTR)
      ENDIF
      WRITE(DEVICE,501) NUA_PROCESSED,NSF_PROCESSED,NOS_PROCESSED,
     &                  NASOS_PROCESSED

      RETURN

C=======================================================================

   83 FORMAT(A3,A80)
 1110 FORMAT(10(I8,1X))
 1111 FORMAT(4(I8,1X),A1)
 1120 FORMAT(F8.3,f8.1)
  130 FORMAT(/25X, 'Merging the Meteorological Data'/)
  135 FORMAT(' ERROR PROCESSING HEADERS, NO MERGE!')
  138 FORMAT(' Invalid/missing ASOS flag (',A1,') in SURFACE QAOUT ',
     &        'file; If blank, QAOUT file likely from old version.')
  139 FORMAT(' # of SURFACE obs > 24 at YR/MN/DY:',3I3,'; could be ',
     &       'date sequencing problem in SURFACE file.')
  140 FORMAT(' CHRONOLOGICAL DAY < 0 FOR ',A10,' PATH; HR ',I3.2)
  141 FORMAT(' FAILURE COMPUTING JULIAN DAY FOR MERGE FILE')
  142 FORMAT(' CHRONOLOGICAL DAY < 0 FOR ',A5,' WINDOW DATE')
  143 FORMAT(' NO UPPERAIR, SURFACE OR ONSITE DATA TO MERGE')
  144 FORMAT(' CHRONOLOGICAL DAYS TO MERGE: ',I5,' - ',I5)
  145 FORMAT(' CHRONOLOGICAL DAY < 0 FOR HR-AVG 1-MIN ASOS; HR ',I3.2)

  146 FORMAT(' 1-MIN ASOS winds from ',A9,' Version ',A6,
     &       ' used for WBAN: ',I5,'; Call_ID: ',A4)
  147 FORMAT(' 1-MIN ASOS WBAN: ',I5,' does not match SURFACE WBAN: ',
     &                                                         A8)
  148 FORMAT(' 1-MIN ASOS WBAN: ',I5,' matches SURFACE WBAN: ',A8)
  200 FORMAT(22X,'Merged Data Begin (Yr/Mo/Da) ',I2.2,'/',I2.2,'/',I2.2,
     &      /22X,'              End            ',I2.2,'/',I2.2,'/',I2.2)
  210 FORMAT(/19X,'***** Daily Output Statistics *****')
  211 FORMAT(/,19X,'***** Daily Output Statistics, continued *****')

  255 FORMAT(' ERROR READING SDG. HDR. AFTER ',I6.6, '; HR ',I3.2 )
  256 FORMAT(' ERROR READING SDG. LEVEL ',I3,'; HR ',I3.2)
  257 FORMAT(' E-O-F READING LEVEL ', I3,'; HR ', I3.2)

  267 FORMAT(' ERROR reading SURFACE observation after ', I6.6, 
     &       '; HR ',I3.2)
  268 FORMAT(' ERROR reading 1-MIN ASOS wind data after ', I6.6,
     &       '; HR ',I3.2)

  271 FORMAT(' ERROR reading ONSITE observation after ', I6.6,
     &       '; HR ',I3.2)

  400 FORMAT('MR *** MERGE, VERSION: ',A8)
  501 FORMAT(//1X,'TOTAL OBSERVATIONS READ:',
     &        /4X,'Upper Air Soundings : ',I6,
     &        /4X,'NWS Sfc Observations: ',I6,
     &        /4X,'On-site Observations: ',I6,
     &        /4X,'1-min ASOS Wind Data: ',I6,
     &       //19X,'***** MERGE PROCESS COMPLETED *****')
  510 FORMAT(14X,'MO/DY:',10(1X,I2,'/',I2))
  515 FORMAT(1X,'NWS  Upper Air  Sdgs',2X,9(I2,4X),I2)
  525 FORMAT(1X,'NWS Sfc Observations',2X,9(I2,4X),I2)
  530 FORMAT(1X,'On-site Observations',2X,9(I2,4X),I2)
  535 FORMAT(1x,'Ave 1-min ASOS Winds',2x,9(I2,4x),I2,/)
 1350 FORMAT(1X,4I2,I5)
 1355 FORMAT(6(1X,I6))
 2265 FORMAT(1X,4I2,4(1X,I5),6(1X,I5.5),/,8X,5(1X,I5.5),7(1X,I5),2x,a1)

      END

