C     WORK1.INC
C***    WORK BLOCK
C
C       WE ANTICIPATE THESE WILL BE USEFUL BOTH
C       IN THE PREPROCESSING AND IN THE MET PROCESSOR
C
C       Revisions History:
C          05/29/08: MACTEC Federal Programs
C                  - Deleted redundant DATA statements for
C                    EQUIVALENCE'd variables
C-----------------------------------------------------------------------
C
      CHARACTER BLNK01(132)*1,BLNK04*4,BLNK08*8,BLNK40*40,BLNK28*28
      CHARACTER BLNK32*32, BLNK96*96,BLNK80*80,BLN132*132,BLN140*140
      CHARACTER BLN500*500

      CHARACTER PATH*10,ECODE*3,LOC*8,MESS*120

      CHARACTER BUF01(132)*1, BUF02*2,    BUF03*3,    BUF04*4, BUF08*8
      CHARACTER BUF28*28,     BUF32*32,   BUF40*40,   BUF96*96
      CHARACTER BUF80(10)*132,BUF132*132, BUF140*140, BUF500*500
      CHARACTER BUFNWS*12000

      INTEGER, PARAMETER :: AD1=1500, AD2=300, AD3=50
      INTEGER   PATHID,KEYID,NWORDS,IOFLAG,POSINX
      INTEGER   IWORK1,IWORK2
      INTEGER   KOUNT, NCHB1, NCHB2
      INTEGER   PGNUM
      REAL      XRD1,XRD2,XRD3,WORK1,WORK2
C
C     NOTE:  The AD2 parameter is used to define the size of 
C     the IWORK2 and WORK2 arrays, and therefore should
C     match the UAML parameter in UA1.INC and the MPLVL
C     parameter in MP2.INC.
C
      COMMON /WRK01/ BUFNWS,BUF132
      COMMON /WRK02/ BUF02,BUF03,BUF04(10),BUF08(10),BUF32,BUF40,BUF96
      COMMON /WRK03/ PATHID,KEYID,NWORDS,IOFLAG,POSINX,
     1               IWORK1(AD1),IWORK2(AD2,AD3),PGNUM
      COMMON /WRK04/ XRD1,XRD2,XRD3,WORK1(AD1),WORK2(AD2,AD3)
C
      EQUIVALENCE (BLNK01,BLNK04,BLNK08,BLNK28,BLNK32,
     1             BLNK40,BLNK96,BLNK80,BLN132,BLN140,
     2             BLN500)
      EQUIVALENCE (BUF01,BUF80)
C          Redefinitions used in SETUP
C
C       BUF01...BUF32   CHARACTER ARRAYS USED IN PROCESSING AND
C                       WORKING WITH THE DATA
C       IWORK1,IWORK2   INTEGER ARRAYS USED IN PROCESSING AND
C                       WORKING WITH THE DATA
C       XRD1...WORK2    READ ARRAYS USED IN PROCESSING AND WORKING
C                       WITH THE DATA
C
C       BLNK1...BLNK80  CHARACTER ARRAYS FILLED WITH BLANKS
C       BLN132..BLN500  CHARACTER FIELDS FILLED WITH BLANKS
C
C       KOUNT,PATH,     ARE USED FOR CONSTRUCTING ERROR MESSAGES
C       ECODE,LOC,      KOUNT = CARD IMAGE COUNT, PATH = PATHWAY ID
C       MESS            ECODE = ERROR CODE, LOC = SUBROUTINE NAME
C                       MESS = MESSAGE
C
        DATA BLN500(1:40)   /'                                        '/
        DATA BLN500(41:80)  /'                                        '/
        DATA BLN500(81:120) /'                                        '/
        DATA BLN500(121:160)/'                                        '/
        DATA BLN500(161:200)/'                                        '/
        DATA BLN500(201:240)/'                                        '/
        DATA BLN500(241:280)/'                                        '/
        DATA BLN500(281:320)/'                                        '/
        DATA BLN500(321:360)/'                                        '/
        DATA BLN500(361:400)/'                                        '/
        DATA BLN500(401:440)/'                                        '/
        DATA BLN500(441:480)/'                                        '/
        DATA BLN500(481:500)/'                    '/

