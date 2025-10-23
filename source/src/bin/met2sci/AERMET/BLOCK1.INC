C       FILE:  BLOCK1.INC
C
C-----------------------------------------------------------------------
C     Revision History
C         01/2009 - removed the default surface characteristics since
C                   AERMET will not process data without specifying the
C                   surface characteristics in the control file
C
C     Revision History
C       08/27/2012 - added KEYWRD THRESH_1MIN and initialized counter iCALM1MIN
C
C-----------------------------------------------------------------------
C --- Assign AERMET version date, VERSNO, included in the header record of the
C     surface file.  Note that the length of VERSNO is 6 characters.  Unless
C     the version 'date' is preceded by another character (such as 'B' for Beta
C     or 'D' for Draft), the first character must be blank in order for AERMOD
C     to read the version date properly.
C ---
      DATA VERSNO/' 12345'/   ! December 10, 2012

      DATA DISPMD /'  AERMOD', '  DEBUG1', '  DEBUG2', '  DEBUG3',       ! dtb #100 01249
     &             '  DEBUG4'/

      DATA DEVIN/5/, DEVIO/6/
      DATA DEV10/10/, DEV12/12/, DEV13/13/
      DATA DEV20/20/, DEV21/21/, DEV22/22/, DEV23/23/
      DATA DEV40/40/, DEV41/41/, DEV50/50/, DEV60/60/, DEV61/61/
      DATA DEV70/70/, DEV75/75/, DEV80/80/, DEV85/85/

      DATA PCNTRL /0/

      DATA CONTRL /1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/                      ! dtb #020 01204
C     Modify default model type to be AERMOD                             ! rwb #518 06341
      DATA MDSTAT /1/                                                    ! rwb #518 06341


C     NWS temperature sensor height
      DATA ZNWST  /2.0/

      DATA (PWELEV(I),I=2,4) /3*0.0/   ! default station elevation       ! dtb #508 06299
      DATA (GOTPWELEV(I),I=2,4) /3*.FALSE./

      DATA SFELEV /-999.0/
      DATA GOTSFELEV /.FALSE./
      DATA STNPINFILE /.FALSE./
      DATA ReformattedSFC /.FALSE./

C     ASOS Commission Date Related Data
      DATA SrchCommDate /.FALSE./
      DATA GotCommDate /.FALSE./
      DATA ISASOS1      /'N'/
      DATA (ISASOS24(I),I=1,24) /24*'N'/
      DATA cCommDate /""/
      DATA iSfcObsWBAN /0/
      DATA cSfcObsWBAN /""/
      DATA GotSfcObsWBAN /.FALSE./
      DATA GotSfcObsCALL /.FALSE./
      DATA NADJASOS /0/
      DATA iCALM1MIN /0/
      
C     Sounding window default values
      DATA UASRWINDOWBEGIN/-6/, UASRWINDOWEND/+2/
      DATA UA0012WINDOWBEGIN/-1/, UA0012WINDOWEND/+1/

      DATA PATHWD /'JOB       ','UPPERAIR  ','SURFACE   ',
     1             'ONSITE    ','MERGE     ','METPREP   '/

      DATA  KEYWRD /'REPORT      ','MESSAGES    ','CHK_SYNTAX  ',
     1              'DATA        ','NO_MISSING  ','FORMAT      ',
     1              'EXTRACT     ','QAOUT       ','XDATES      ',
     1              'LOCATION    ','            ','MODIFY      ',
     1              'RANGE       ','AUDIT       ','DELTA_TEMP  ',
     1              'OSHEIGHTS   ','THRESHOLD   ','READ        ',
     1              'FREQ_SECT   ','SITE_CHAR   ','SECTOR      ',
     1              'OUTPUT      ','MODEL       ','METHOD      ',
     1              'UAWINDOW    ','DEBUG       ','PROFILE     ',
     1              'OBS/HOUR    ','NWS_HGT     ','FREQ_SECT2  ',
     1              'SITE_CHAR2  ','SECTOR2     ','ASOS1MIN    ',
     1              'AERSURF     ','AERSURF2    ','THRESH_1MIN ',
     1              '            ','            ','            ',
     1              '            '/

C     Most of the ITEMs are not used because methods do not exist
C     in AERMET that would use them (e.g., HEATFLUX, FRICVELO)
C     Currently in use: WIND_DIR, REFLEVEL, STABLEBL, ASOS_ADJ, UASELECT

      DATA ITEM   /'WIND_DIR','AMB_TEMP','MIX_HGTS','STABILTY',
     1             'TEMPGRAD','SIGMAS  ','HEATFLUX','FRICVELO',
     1             'HEIGHTS ','REFLEVEL','STABLEBL','ASOS_ADJ',
     1             'UASELECT','        ','        ','        '/          ! dtb #020 01204

      DATA ACTION /'NORAND','RANDOM','SUBNWS','BULKRN',
     1             'NO_ADJ','SUNRIS','ADJ_U*','      '/                  ! dtb #020 01204

      DATA INSTR  /'WIND    '/

C.......................................................................
C  Initialize the path/keyword STATUS and path status (STABLK) arrays;
C  error statuses
C.......................................................................

      DATA STATUS/240*0/,STABLK/6*0/

      DATA SETERR/.FALSE./, RUNERR/.FALSE./

C.......................................................................
C  Define upper air variable names
C.......................................................................

      DATA (UAVAR(M),M=1,4)  /'UAPR','UAHT','UATT','UATD'/
      DATA (UAVAR(M),M=5,8)  /'UAWD','UAWS','UASS','UADS'/
      DATA (UAVAR(M),M=9,12) /'UALR','UADD','UAM1','UAM2'/

      DATA UATOP/5000/, UAINC/500/
      DATA UALOC1/'--------'/
      DATA UALOC /'        '/

C  Initialize upper air QA variables (remember that many of these values
C     are multiplied by 10)

      DATA (UAQA(1,M),M=1,4) /1,99999,5000,10999/
      DATA (UAQA(2,M),M=1,4) /2,-99999,0,5000/
      DATA (UAQA(3,M),M=1,4) /1,-9990,-350,+350/
      DATA (UAQA(4,M),M=1,4) /1,-9990,-350,+350/
      DATA (UAQA(5,M),M=1,4) /2,999,0,360/
      DATA (UAQA(6,M),M=1,4) /1,9990,0,500/
      DATA (UAQA(7,M),M=1,4) /2,-9999,0,5/
      DATA (UAQA(8,M),M=1,4) /2,-9999,0,90/
      DATA (UAQA(9,M),M=1,4) /2,-9999,-2,5/
      DATA (UAQA(10,M),M=1,4) /2,-9999,0,2/
      DATA (UAQA(11,M),M=1,4) /2,-9999,50,500/
      DATA (UAQA(12,M),M=1,4) /2,-9999,500,3500/

C  Zero UA-pathway audit arrays

      DATA (UAVAUD(M),M=1,10) /10*0/
      DATA (((UAAUDT(II,J,M),II=1,10),J=1,10),M=0,3) /400*0/

C  Zero UA trace arrays
C
      DATA (UAVTRA(M),M=1,10) /10*0/


C.......................................................................
C  Define onsite and NWS surface variable names
C.......................................................................

      DATA (VNAMES(K),K=1,4)  /'HFLX','USTR','MHGT','ZOHT'/
      DATA (VNAMES(K),K=5,8)  /'SAMT','PAMT','INSO','NRAD'/
      DATA (VNAMES(K),K=9,12) /'DT01','DT02','DT03','US01'/
      DATA (VNAMES(K),K=13,16)/'US02','US03','HT  ','SA  '/
      DATA (VNAMES(K),K=17,20)/'SE  ','SV  ','SW  ','SU  '/
      DATA (VNAMES(K),K=21,24)/'TT  ','WD  ','WS  ','VV  '/
      DATA (VNAMES(K),K=25,28)/'DP  ','RH  ','V1  ','V2  '/
      DATA (VNAMES(K),K=29,32)/'V3  ','PRCP','SLVP','PRES'/
      DATA (VNAMES(K),K=33,36)/'CLHT','TSKC','ALC1','ALC2'/              ! dtb #120 02064
      DATA (VNAMES(K),K=37,40)/'ALC3','ALC4','ALC5','ALC6'/              ! dtb #120 02064
      DATA (VNAMES(K),K=41,44)/'PWVC','PWTH','ASKY','ACHT'/              ! dtb #120 02064
      DATA (VNAMES(K),K=45,48)/'HZVS','TMPD','TMPW','DPTP'/
      DATA (VNAMES(K),K=49,52)/'RHUM','WDIR','WSPD','OSDY'/
      DATA (VNAMES(K),K=53,56)/'OSMO','OSYR','OSHR','OSMN'/

      DATA SFLOC1/'--------'/,SFLOC/'        '/
      DATA SFFMT/'        '/

      DATA (SFQA(30,L),L=1,4) /2,-9,0,25400/          ! 30 PRCP
      DATA (SFQA(31,L),L=1,4) /1,99999,9000,10999/    ! 31 SLVP
      DATA (SFQA(32,L),L=1,4) /1,99999,9000,10999/    ! 32 PRES
      DATA (SFQA(33,L),L=1,4) /2,999,0,300/           ! 33 CLHT
      DATA (SFQA(34,L),L=1,4) /2,9999,0,1010/         ! 34 TSKC

      DATA (SFQA(35,L),L=1,4) /2,09999,0,07300/       ! 35 ALC1          ! dtb #120 02064
      DATA (SFQA(36,L),L=1,4) /2,09999,0,07300/       ! 36 ALC2          ! dtb #120 02064
      DATA (SFQA(37,L),L=1,4) /2,09999,0,07300/       ! 37 ALC3          ! dtb #120 02064
      DATA (SFQA(38,L),L=1,4) /2,09999,0,07850/       ! 38 ALC4          ! dtb #120 02064
      DATA (SFQA(39,L),L=1,4) /2,09999,0,07850/       ! 39 ALC5          ! dtb #120 02064
      DATA (SFQA(40,L),L=1,4) /2,09999,0,07850/       ! 40 ALC6          ! dtb #120 02064
      DATA (SFQA(41,L),L=1,4) /2,9999,0,9800/         ! 41 PWVC          ! dtb #120 02064
      DATA (SFQA(42,L),L=1,4) /2,9999,0,9800/         ! 42 PWTH          ! dtb #120 02064
      DATA (SFQA(43,L),L=1,4) /2,99,0,10/             ! 43 ASKY          ! dtb #120 02064
      DATA (SFQA(44,L),L=1,4) /2,999,0,888/           ! 44 ACHT          ! dtb #120 02064

      DATA (SFQA(45,L),L=1,4) /2,99999,0,1640/        ! 45 HZVS
      DATA (SFQA(46,L),L=1,4) /1,999,-300,400/        ! 46 TMPD
      DATA (SFQA(47,L),L=1,4) /1,999,-650,350/        ! 47 TMPW
      DATA (SFQA(48,L),L=1,4) /1,999,-650,350/        ! 48 DPTP
      DATA (SFQA(49,L),L=1,4) /2,999,0,100/           ! 49 RHUM
      DATA (SFQA(50,L),L=1,4) /2,999,0,36/            ! 50 WDIR
      DATA (SFQA(51,L),L=1,4) /2,999,0,500/           ! 51 WSPD

      DATA (SFQA(52,L),L=1,4) /0,0,0,0/
      DATA (SFQA(53,L),L=1,4) /0,0,0,0/
      DATA (SFQA(54,L),L=1,4) /0,0,0,0/
      DATA (SFQA(55,L),L=1,4) /0,0,0,0/
      DATA (SFQA(56,L),L=1,4) /0,0,0,0/

C     Set default SF-pathway audit selections.  We set the audit toggle to
c     '1' (on) for dry-bulb, wind direction, and wind speed.

      DATA (SFSAUD(L),L=1,11)  /0,0,0,0,0,0,0,0,0,0,0/
      DATA (SFSAUD(L),L=12,22) /0,0,0,0,0,1,0,0,0,1,1/

      DATA (SFSTRA(L),L=1,11)  /0,0,0,0,0,0,0,0,0,0,0/
      DATA (SFSTRA(L),L=12,22) /0,0,0,0,0,0,0,0,0,0,0/

      DATA ((SFAUD(II,L),II=1,22),L=0,3) /88*0/
      DATA ((SFAUD1(II,L),II=1,11),L=0,3) /44*0/
      DATA ((SFAUD2(II,L),II=1,11),L=0,3) /44*0/

      DATA SFCFRQ/0/, SFCFRQ2/0/

      DATA OSLOC/'        '/
      DATA DEV31/31/,DEV32/32/
      DATA OSAVG/1/,OSMIN/1/
      DATA OSDAY1/-9/,OSDAY2/-9/
      DATA OSNWDS/0/
      DATA OSNL/0/
      DATA ((OSSFC(II,N,1),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSSFC(II,N,2),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSSFC(II,N,3),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSWDS(II,N),II=1,12),N=1,2) /24 * 0.0/
C                                                              position in array
      DATA OSNWDS2/0/
      DATA ((OSSFC2(II,N,1),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSSFC2(II,N,2),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSSFC2(II,N,3),II=1,12),N=1,12) /144 * 0.0/
      DATA ((OSWDS2(II,N),II=1,12),N=1,2) /24 * 0.0/
C                                                                OSSOBS OSVOBS
      DATA (OSQA(1,L),L=1,4)  /1.,-999.,-100.,800./       !  1  HFLX     1    -   ! dtb   06305
      DATA (OSQA(2,L),L=1,4)  /1., -9.,0.,2./             !  2  USTR     2    -   ! dtb #036 01228
      DATA (OSQA(3,L),L=1,4)  /1.,9999.,0.,4000./         !  3  MHGT     3    -
      DATA (OSQA(4,L),L=1,4)  /1.,999.,0.,2./             !  4  Z0HT     4    -
      DATA (OSQA(5,L),L=1,4)  /2.,999.,0.,250./           !  5  SAMT     5    -
      DATA (OSQA(6,L),L=1,4)  /2.,999.,0.,25./            !  6  PAMT     6    -
      DATA (OSQA(7,L),L=1,4)  /2.,9999.,0.,1250./         !  7  INSO     7    -
      DATA (OSQA(8,L),L=1,4)  /1.,999.,-100.,800./        !  8  NRAD     8    -
      DATA (OSQA(9,L),L=1,4)  /1.,9.,-2.,5./              !  9  DT01     9    -
      DATA (OSQA(10,L),L=1,4) /1.,9.,-2.,5./              ! 10  DT02    10    -
      DATA (OSQA(11,L),L=1,4) /1.,9.,-2.,5./              ! 11  DT03    11    -
      DATA (OSQA(12,L),L=1,4) /1.,999.,0.,100./           ! 12  US01    12    -
      DATA (OSQA(13,L),L=1,4) /1.,999.,0.,100./           ! 13  US02    13    -
      DATA (OSQA(14,L),L=1,4) /1.,999.,0.,100./           ! 14  US03    14    -

      DATA (OSQA(15,L),L=1,4) /1.,9999.,0.,4000./         ! 15  HTnn     -    1
      DATA (OSQA(16,L),L=1,4) /1.,99.,0.,35./             ! 16  SAnn     -    2
      DATA (OSQA(17,L),L=1,4) /1.,99.,0.,25./             ! 17  SEnn     -    3
      DATA (OSQA(18,L),L=1,4) /1.,99.,0.,3./              ! 18  SVnn     -    4
      DATA (OSQA(19,L),L=1,4) /1.,99.,0.,3./              ! 19  SWnn     -    5
      DATA (OSQA(20,L),L=1,4) /1.,99.,0.,3./              ! 20  SUnn     -    6
      DATA (OSQA(21,L),L=1,4) /1.,99.,-30.,40./           ! 21  TTnn     -    7
      DATA (OSQA(22,L),L=1,4) /2.,999.,0.,360./           ! 22  WDnn     -    8
      DATA (OSQA(23,L),L=1,4) /2.,99.,0.,50./             ! 23  WSnn     -    9
      DATA (OSQA(24,L),L=1,4) /1.,999.,0.,5./             ! 24  VVnn     -   10
      DATA (OSQA(25,L),L=1,4) /1.,99.,-65.,35./           ! 25  DPnn     -   11
      DATA (OSQA(26,L),L=1,4) /2.,999.,0.,100./           ! 26  RHnn     -   12
      DATA (OSQA(27,L),L=1,4) /1.,999.,0.,100./           ! 27  V1nn     -   13
      DATA (OSQA(28,L),L=1,4) /1.,999.,0.,100./           ! 28  V2nn     -   14
      DATA (OSQA(29,L),L=1,4) /1.,999.,0.,100./           ! 29  V3nn     -   15

      DATA (OSQA(30,L),L=1,4) /2.,-9.,0.,25400./          ! 30  PRCP    15    -
      DATA (OSQA(31,L),L=1,4) /1.,99999.,9000.,10999./    ! 31  SLVP    16    -
      DATA (OSQA(32,L),L=1,4) /1.,99999.,9000.,10999./    ! 32  PRES    17    -
      DATA (OSQA(33,L),L=1,4) /2.,999.,0.,300./           ! 33  CLHT    18    -
      DATA (OSQA(34,L),L=1,4) /2.,99.,0.,10./             ! 34  TSKC    19    -


      DATA (OSTSKY(L),L=1,4)  /2.,99.,0.,10./

C --- Note that dew point and relative humidity are
C     available as multi-level variables with ONSITE
C     data (DPnn and RHnn).  However single level
C     array elements are used to correspond with
C     SURFACE dew point and relative humidity.
C     Assignment to these single level arrays is done
C     in SUBROUTINE SUBST.
C
C --- OSSOBS array element for single level dew point ! 48  DPTP    48
C --- OSSOBS array element for single level rel humid ! 49  RHUM    49

      DATA (OSQA(52,L),L=1,4) /2.,-9.,1.,31./             ! 52  OSDY
      DATA (OSQA(53,L),L=1,4) /2.,-9.,1.,12./             ! 53  OSMO
      DATA (OSQA(54,L),L=1,4) /2.,-9.,0.,99./             ! 54  OSYR
      DATA (OSQA(55,L),L=1,4) /2.,-9.,0.,24./             ! 55  OSHR
      DATA (OSQA(56,L),L=1,4) /2.,-9.,0.,60./             ! 56  OSMN

C     Zero OS trace array
      DATA (OSSTRA(N),N=1,OSMV) /OSMV*0/

C  Initialize OS-pathway audit arrays

C  Set OS-pathway default audit selections
C   Scalars: none
C   Vectors: TT  ,WD  ,WS
C   OSMV = 56 in OS1.INC  (7/2/95)

      DATA OSSAUD(21) /1/, OSSAUD(22) /1/
      DATA OSSAUD(23) /1/

      DATA (OSSAUD(N),N=1,20) /20*0/
      DATA (OSSAUD(N),N=24,OSMV) /33*0/

      DATA ((OSVAUD(L,N),L=1,OSML),N=1,OSMVEC) /MLMVEC*0/
      DATA (OSAUD1(II,0),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,1),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,2),II=1,OSMV) /OSMV*0/
      DATA (OSAUD1(II,3),II=1,OSMV) /OSMV*0/
      DATA ((OSAUD2(L,II,0),II=1,OSMVEC),L=1,OSML) /MLMVEC*0/
      DATA ((OSAUD2(L,II,1),II=1,OSMVEC),L=1,OSML) /MLMVEC*0/
      DATA ((OSAUD2(L,II,2),II=1,OSMVEC),L=1,OSML) /MLMVEC*0/
      DATA ((OSAUD2(L,II,3),II=1,OSMVEC),L=1,OSML) /MLMVEC*0/

C.......................................................................
C  Initialize the MERGE date variables
C.......................................................................

      DATA MRYR1/0/, MRYR2/0/, MRGMO1/0/, MRGMO2/0/, MRGDY1/0/,
     &     MRGDY2/0/

