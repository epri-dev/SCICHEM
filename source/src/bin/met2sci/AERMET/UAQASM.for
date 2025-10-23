      SUBROUTINE UAQASM (ISTAT)
C=======================================================================
C          UAQASM Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To assess the quality of the upper air data by checking
C               the upper and lower bounds of pressure, height, temperature,
C               dew point temperature, wind direction, wind speed, 'calm'
C               winds, dew-point exceeding temperature and selected
C               vertical gradients
C
C     Called by: UAPATH
C
C     Initial release:  December 15, 1992
C
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        11/30/94
C          - Added check for missing on additional variables
C            in computing the dew point deviation
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER COUNTS, COUNTL, COUNTH, QARSLT, IDOM, NUM
      INTEGER IVBL, LEV, UANTD, UAWSWD, UACALM, JULIAN, ISTAT
      INTEGER IWD1,IWD2
      INTEGER NSHORT, IYMD, I, NLEVS, KSTAT, J
      REAL DH, DIFWD, TDEST1, DS, DWDDZ, DTDZ, DPEST, DVDZ
      REAL RMISS0, RMISS7, RMISS8, RMISS9
      REAL WSHRL, WSHRU, TINVR,TSUPR, DPDL, DPDU, WDHRL, WDHRU
C
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

      DATA PATH/'UPPERAIR  '/,  LOC/'UAQASM'/

C-----------------------------------------------------------------------
C  Variable definitions
C    COUNTH          Counter for upper air reports
C    COUNTL          Counter for sounding levels
C    COUNTS          Counter for upper air soundings
C    UANTD           Counter for number of times dew-point exceeds temp.
C    UAWSWD          Counter for the number of nonzero wind directions
C                      with corresponding zero wind speeds
C    UACALM          Counter for the number of calm winds
C    WSHRL,WSHRU     Lower and upper bounds for wind speed shear
C    WDHRL,WDHRU     Lower and upper bounds for wind directional shear
C    TSUPR,TINVR     Lower and upper bounds for temperature lapse rates
C    DPDL,DPDU       Lower and upper bounds for dew point deviations
C    RMISS7,RMISS8   Missing value indicators for wind speed shear,
C     RMISS9,RMISS0   directional shear, lapse rate, and dew pt. dev'n.
C    DH              Height difference
C    DTDZ            Observed lapse rate
C    DIFWD           Observed directional change
C    DWDDZ           Observed directional shear (= DIFWD/DH)
C    DS              Speed change
C    DVDZ            Speed shear (= DS/DH)
C    TDEST1          Interpolated dew point difference estimate
C    DPEST           Interpolated dew point difference estimate gradient
C                     (= TDEST1/DH)
C    IDOM            Sounding layer in increments (meters) of 'UAINC'
C    IVBL,LEV,K      Loop counters
C
C    JULIAN          Integer function computing julian day
C
C *** Subroutines called
C
C    OTHHDR        Reads/writes input and output file headers
C    HTCALC        Recalculates the sounding heights
C    ERROR         Write error/violation messages
C    INTEQA        QA'S integer values
C    REALQA        QA'S real values
C
C-----------------------------------------------------------------------
C---- Initialize counters
      COUNTS = 0
      COUNTH = 0
      UANTD  = 0
      UACALM = 0
      UAWSWD = 0
      UADAYC = 0
      NUM = 2
      NSHORT = 0
      IDOM = 0

C---- Convert vertical gradient limits to real values
      WSHRL = FLOAT(UAQA(7,3))
      WSHRU = FLOAT(UAQA(7,4))
      WDHRL = FLOAT(UAQA(8,3))
      WDHRU = FLOAT(UAQA(8,4))
      TSUPR = FLOAT(UAQA(9,3))
      TINVR = FLOAT(UAQA(9,4))
      DPDU  = FLOAT(UAQA(10,4))
      DPDL  = FLOAT(UAQA(10,3))


C---- Convert integer missing value indicators to real numbers
      RMISS7 = FLOAT(UAQA(7,2))
      RMISS8 = FLOAT(UAQA(8,2))
      RMISS9 = FLOAT(UAQA(9,2))
      RMISS0 = FLOAT(UAQA(10,2))


C---- Read the file headers; if there is a problem, NUM is returned as -1:
C     write a message and do not QA the data
C     (Note: NUM defines the pathway)

      CALL OTHHDR(PATH,LOC,NUM,DEV12,DEV13,ISTAT)
      IF( ISTAT .EQ. 1 )THEN
         MESS =  BLNK80
         WRITE(MESS,135)
         CALL ERRHDL(0,PATH,'E20',LOC,MESS)
         RETURN
      ENDIF
      WRITE(DEV13,1204)


C---- Read the sounding data header after initializing data
   20 CONTINUE

      COUNTH = COUNTH + 1
      UAGYR = 0
      UAGMO = 0
      UAGDY = 0
      UAGHR = 0
      UALEV(1) = 0
Cjop      UALDY = UADAYC     ! never used

      READ(DEV12,301,ERR=330,END=390) UAGYR,UAGMO,UAGDY,UAGHR,UALEV(1)
  301 FORMAT(1X,4I2.2,I5)

      WRITE( *,610 ) UAGMO,UAGDY,UAGYR
  610 FORMAT('+  Stage 1: QA''ing upper air data for month/day/year ',
     &             3(I2.2,:'/'))


C---- Write sounding data header to the output file

      WRITE(DEV13,301)UAGYR,UAGMO,UAGDY,UAGHR,UALEV(1)
      UADAYC = JULIAN(UAGYR,UAGMO,UAGDY)
      IYMD = UAGYR*10000 + UAGMO*100 + UAGDY
      IWORK1(1210) = UADAYC*100 + UAGHR


C---- Read variables at data levels if there are data (UALEV > 0)

      IF( UALEV(1) .GT. 0 )THEN
         COUNTL = 0
         COUNTS = COUNTS + 1
         CALL FLSDG(1)
         DO I=1,UALEV(1)
            READ(DEV12,302,ERR=340,END=350)(UAOBS(1,I,IVBL),IVBL=1,UAMV)
  302       FORMAT(6(1X,I6))

            COUNTL=COUNTL+1
         ENDDO


C------- Check the top of the sounding - does it make it to UATOP
C        (5000 meters)

         NLEVS = UALEV(1)
         IF(UAOBS(1,NLEVS,2) .LT. UATOP )THEN
            NSHORT = NSHORT + 1
C           MESS =  BLNK80
C           XHGT = UAOBS(1,NLEVS,2)
C           WRITE(MESS,5005) UAGHR,UATOP,XHGT
C           CALL ERRHDL(IYMD,PATH,'W36',LOC,MESS)
         ENDIF
C5005    FORMAT(' TOP OF ',I2.2,'00 LST SNDG < ',I5,'m; TOP IS AT ',
C    &          F5.0, 'm')


C------- Check the sounding heights
         CALL HTCALC(KSTAT)
         IF(KSTAT .EQ. 1) THEN
            MESS =  BLNK80
            WRITE(MESS,174) UAGHR
            CALL ERRHDL( IYMD,PATH,'Q36',LOC,MESS)
         ENDIF


C------- Write the levels to the output file
         DO J=1,UALEV(1)
            WRITE(DEV13,302) (UAOBS(1,J,IVBL),IVBL=1,UAMV)
         ENDDO

Cjop     IF( ISTAT .EQ. -1) GO TO 20

C=======================================================================

C------- Check variables for limit violations
C        IWORK1(1210) is the Julian day + hour of the sounding;
C        it is recomputed because HTCALC flushed the work array

         IWORK1(1210) = UADAYC*100 + UAGHR
         LOOP_LVLS: DO LEV = 1,UALEV(1)

C---------- Determine the layer based on height above ground level
C           1=surface, 2-9=layer in increments of uainc, 10= above
C           8*UAINC meters above ground level

            IF(LEV .EQ. 1) THEN
               IDOM = 1
            ELSEIF((UAOBS(1,LEV,2) - UAOBS(1,1,2)) .GT. (8*UAINC))THEN
               IDOM = 10
            ELSEIF((UAOBS(1,LEV,2) - UAOBS(1,1,2)) .GT. 0) THEN
               IDOM = (UAOBS(1,LEV,2) - UAOBS(1,1,2))/UAINC + 2
            ELSE
               MESS =  BLNK80
               WRITE(MESS,184) UAGHR
               CALL ERRHDL(IYMD,PATH,'I39',LOC,MESS)
            ENDIF

            LOOP_VARS: DO IVBL = 1,UAMV
               IF( UAVAUD(IVBL) .EQ. 1 )THEN
                  QARSLT = 5
                  CALL INTEQA( 2,0,UAQA(IVBL,1),UAQA(IVBL,2),
     &                         UAQA(IVBL,3),UAQA(IVBL,4),
     &                         UAOBS(1,LEV,IVBL),UAVAR(IVBL),
     &                         UAGYR,UAGMO,UAGDY,UAGHR, QARSLT )

                  UAAUDT(IVBL,IDOM,QARSLT) = UAAUDT(IVBL,IDOM,QARSLT)+1
C*                UAVTRA is the 'flag' array corresponding to the
C*                NO_MISSING keyword

                  IF( QARSLT .EQ. 1 .AND.  UAVTRA(IVBL) .EQ. 0)THEN
                     MESS =  BLNK80
                     WRITE(MESS,192) UAVAR(IVBL),UAGHR,LEV
                     CALL ERRHDL(IYMD,PATH,'Q39',LOC,MESS)
                  ENDIF

               ENDIF
            ENDDO LOOP_VARS


C---------- Check for calm winds and wind speeds of zero with non-zero
C           wind directions

            IF( UAOBS(1,LEV,6) .EQ. 0 )THEN
               IF( UAOBS(1,LEV,5) .EQ. 0 )THEN
                  UACALM = UACALM + 1
                  MESS =  BLNK80
                  WRITE(MESS,155) UAGHR,LEV
                  CALL ERRHDL (IYMD,PATH,'CLM',LOC,MESS)

               ELSEIF( UAOBS(1,LEV,5) .GT. 0 )THEN
                  UAWSWD = UAWSWD + 1
                  MESS =  BLNK80
                  WRITE(MESS,155) UAGHR,LEV
                  CALL ERRHDL (IYMD,PATH,'WDS',LOC,MESS)
               ENDIF

            ENDIF


C---------- Compare the dew point to the temperature

            IF( (UAOBS(1,LEV,4) .NE. UAQA(4,2)) .AND.
     &          (UAOBS(1,LEV,3) .NE. UAQA(3,2)) )THEN
               IF( UAOBS(1,LEV,4) .GT. UAOBS(1,LEV,3) )THEN
                  UANTD = UANTD + 1
                  MESS =  BLNK80
                  WRITE(MESS,154) UAGHR,LEV
                  CALL ERRHDL(IYMD,PATH,'TDT',LOC,MESS)
               ENDIF

            ENDIF


C---------- Vertical gradients: these tests cannot be performed at
C                                the first level

            DVDZ  = 0.0
            DWDDZ = 0.0
            DTDZ  = 0.0
            DPEST = 0.0

            IF( LEV .GT. 1 )THEN
               IF( (UAOBS(1,LEV,2) .NE. UAQA(2,2)) .AND.
     &             (UAOBS(1,LEV-1,2) .NE. UAQA(2,2)) )THEN
                  DH = UAOBS(1,LEV,2) - UAOBS(1,LEV-1,2)


C---------------- Wind speed shear: audit variable 7
                  IF( UAVAUD(7) .EQ. 1 )THEN

                     IF( (UAOBS(1,LEV,6)   .NE. UAQA(6,2)) .AND.
     &                   (UAOBS(1,LEV-1,6) .NE. UAQA(6,2)) )THEN
                        DS = (UAOBS(1,LEV,6)-UAOBS(1,LEV-1,6))/10.
                        DVDZ = (ABS(DS/DH)) * 100.0
                     ELSE
                        DVDZ = RMISS7
                     ENDIF

                     QARSLT = 5
                     CALL REALQA(2,0,UAQA(7,1),RMISS7,
     &                     WSHRL, WSHRU, DVDZ,UAVAR(7),
     &                     UAGYR, UAGMO, UAGDY, UAGHR,99,LEV,QARSLT)

                     UAAUDT(7,IDOM,QARSLT) = UAAUDT(7,IDOM,QARSLT) + 1
                  ENDIF


C---------------- Wind directional shear: audit variable 8

                  IF( UAVAUD(8) .EQ. 1 )THEN
                     IF( (UAOBS(1,LEV,5)   .NE. UAQA(5,2)) .AND.
     &                   (UAOBS(1,LEV-1,5) .NE. UAQA(5,2)) )THEN

C*                      Condition: directions between 0 and 360
                        IWD1=MOD(UAOBS(1,LEV,5),360)
                        IF(IWD1 .LT. 0) IWD1=IWD1+360
                        IWD2=MOD(UAOBS(1,LEV-1,5),360)
                        IF(IWD2 .LT. 0) IWD2=IWD2+360

C*                      Compute the absolute difference
                        DIFWD = ABS(IWD1-IWD2)
C*                      If > 180, take complementary difference
                        IF(DIFWD .GT. 180.0) DIFWD=360.0-DIFWD
                        DWDDZ = (DIFWD/DH) * 100.0

                     ELSE
                        DWDDZ = RMISS8
                     ENDIF

                     QARSLT = 5
                     CALL REALQA(2,0,UAQA(8,1),RMISS8,
     &                           WDHRL,WDHRU, DWDDZ,UAVAR(8),
     &                           UAGYR,UAGMO,UAGDY,UAGHR,99,LEV,QARSLT)

                     UAAUDT(8,IDOM,QARSLT) = UAAUDT(8,IDOM,QARSLT) + 1
                  ENDIF


C---------------- Temperature lapse rate: audit variable 9

                  IF( UAVAUD(9) .EQ. 1 )THEN
                     IF( (UAOBS(1,LEV,3)   .NE. UAQA(3,2)) .AND.
     &                   (UAOBS(1,LEV-1,3) .NE. UAQA(3,2)) )THEN
                        DTDZ = ( ((UAOBS(1,LEV,3) - UAOBS(1,LEV-1,3)) /
     &                          10.0) / DH) * 100.0
                     ELSE
                        DTDZ = RMISS9
                     ENDIF

                     QARSLT = 5
                     CALL REALQA(2,0,UAQA(9,1),RMISS9,
     &                           TSUPR,TINVR, DTDZ,UAVAR(9),
     &                           UAGYR,UAGMO,UAGDY,UAGHR,99,LEV,QARSLT)

                     UAAUDT(9,IDOM,QARSLT) = UAAUDT(9,IDOM,QARSLT) + 1
                  ENDIF


C---------------- Dew point deviation (omit the upper level):
C                 audit vbl 10

                  IF( UAVAUD(10) .EQ. 1 )THEN
                     IF( LEV .LT. UALEV(1) )THEN
                        IF( (UAOBS(1,LEV,4)   .NE. UAQA(4,2)) .AND.
     &                      (UAOBS(1,LEV-1,4) .NE. UAQA(4,2)) .AND.
     &                      (UAOBS(1,LEV+1,4) .NE. UAQA(4,2)) .AND.
     &                      (UAOBS(1,LEV+1,2) .NE. UAQA(2,2)) )THEN
                           TDEST1 = UAOBS(1,LEV-1,4) +
     &                     FLOAT(UAOBS(1,LEV,2) - UAOBS(1,LEV-1,2)) /
     &                     FLOAT(UAOBS(1,LEV+1,2) - UAOBS(1,LEV-1,2)) *
     &                          (UAOBS(1,LEV+1,4) - UAOBS(1,LEV-1,4))
                           DPEST = ABS(TDEST1-FLOAT(UAOBS(1,LEV,4)))/DH

                        ELSE
                           DPEST = RMISS0
                        ENDIF

                        QARSLT = 5
                        CALL REALQA(2,0,UAQA(10,1),RMISS0, DPDL, DPDU,
     &                              DPEST,UAVAR(10),UAGYR,UAGMO,
     &                              UAGDY,UAGHR,99,LEV,QARSLT)

                     ENDIF

                     UAAUDT(10,IDOM,QARSLT) =
     &                                   UAAUDT(10,IDOM,QARSLT) + 1
                  ENDIF

               ELSE
C---------------- The height at either the upper or lower level is
C                 missing
                  MESS =  BLNK80
                  WRITE(MESS,164) UAGHR,LEV-1,LEV
                  CALL ERRHDL(IYMD,PATH,'Q34',LOC,MESS)

               ENDIF

            ENDIF                      ! LEV > 1

C------- End of loop for sounding levels

         ENDDO LOOP_LVLS

      ELSE
         MESS =  BLNK80
         WRITE(MESS,5010) UAGHR
 5010    FORMAT(' NO DATA LEVELS FOR THE ',I3.2,'00 LST SOUNDING')
         CALL ERRHDL(IYMD,PATH,'Q33',LOC,MESS)

      ENDIF                           ! UALEV() > 0

      GO TO 20

C=======================================================================
C---- Processing continues here if there was an error or end-of-file
C     reading the data (i.e., premature EOF)

330   MESS =  BLNK80
      WRITE(MESS,430) COUNTH
      CALL ERRHDL(COUNTH,PATH,'E38',LOC,MESS)
430   FORMAT(' ERROR READING UPPERAIR DATA HEADER # ',I3)
      ISTAT = 1
      RETURN

340   MESS =  BLNK80
      WRITE(MESS,440) COUNTL,UAGHR
      CALL ERRHDL(IYMD,PATH,'E38',LOC,MESS)
440   FORMAT(' ERROR READING LEVEL ',I3,' FOR HR ',I3.2)
      ISTAT = 1
      RETURN

350   MESS =  BLNK80
      WRITE(MESS,450) COUNTL,UAGHR
      CALL ERRHDL(IYMD,PATH,'E38',LOC,MESS)
450   FORMAT(' PREMATURE EOF READ AT LEVEL ',I3,' FOR HR ',I3.2)
      ISTAT = 1
      RETURN

Cjop 380   MESS =  BLNK80
Cjop       WRITE(MESS,480) COUNTH
Cjop       CALL ERRHDL(COUNTH,PATH,'E32',LOC,MESS)
Cjop 480   FORMAT(' ERROR READING UA HEADER #',I3)
Cjop       RETURN

390   MESS =  BLNK80
      WRITE(MESS,490) COUNTH-1
      CALL ERRHDL(0,PATH,'I39',LOC,MESS)
490   FORMAT(' EOF AFTER UPPERAIR SOUNDING # ',I5)

      IF( COUNTS .GT. 0 )THEN
         WRITE(DEV70,601)
         WRITE(DEV70,600) COUNTH-1,UACALM,UAWSWD,UANTD,NSHORT,UATOP,
     &                    DISK60
         WRITE(DEV70,601)
      ENDIF

      RETURN

C-----------------------------------------------------------------------

  100 FORMAT('*  UA     UPPER AIR DATA QUALITY ASSESSMENT,',
     &       ' VERSION: ',A8)
  135 FORMAT(' ERROR PROCESSING HEADERS, NO QA')
  154 FORMAT(' TD > T FOR HR ', I3.2, ' AT LEVEL ', I2)
  155 FORMAT(' CALM WINDS FOR HR ',I3.2, ' AT LEVEL ', I2)
  164 FORMAT(' NO GRADIENTS COMPUTED FOR HR ',I3.2,', LEVELS ',
     &        I2, ' - ',I2)
  174 FORMAT(' COULD NOT RECOMPUTE HTS. FOR HR ',I3.2)
  184 FORMAT(' PROBLEM COMPUTING QA HGT RANGE FOR HR ',I3.2)

  192 FORMAT(1X,A4,' MISSING FOR HR ',I3.2,' AT LEVEL ',I2)
  600 FORMAT(8X,'In addition, for the ',I4,' soundings, AERMET ',
     &          'reports that there are:',//
     &      10X,I5,' CALM WIND CONDITIONS (WS=0, WD=0)'/
     &      10X,I5,' ZERO WIND SPEEDS WITH NONZERO WIND DIRECTIONS'/
     &      10X,I5,' DEW POINT GREATER THAN DRY BULB TEMPERATURE'/
     &      10X,I5,' SOUNDINGS THAT DO NOT EXTEND TO ',I5,' METERS'//
     &       8X,'The date, time and level of the occurrences for ',
     &          'the first three'/
     &       8X,'can be found in the message file',2X,A96,/
     &       8X,'with qualifiers CLM, WDS, TDT (resp.)',/)
  601 FORMAT('$UAUA$  ')
 1204 FORMAT('*** EOH: END OF UPPERAIR QA HEADERS')

      END

