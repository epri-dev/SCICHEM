        SUBROUTINE OSNEXT( NUMBER,NHRS,ISTAT )
C=======================================================================
C          OSNEXT Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:   Fetches the next on-site data observation,
C                using SUBR.OSFILL. The data are QA'd, if requested.
C                The date and time for the observation just read are
C                stored in the work arrays, and are compared to the
C                date and time in the OSxOBS arrays.  If the data in
C                the work arrays are for the same hour, then processing
C                continues.  Otherwise, the date and time are checked to
C                be sure it is for a later period.  If the date and time
C                are for an *earlier* period, then it is an error
C                condition.  For multiple periods per hour, the sums
C                are accumulated (SUBR.OSSUMS) and the hourly average
C                is computed (SUBR.OSHRAV) when a new hour is read,
C                or when the end-of-file is reached or end of the
C                user-specified extraction period is reached.
C
C-----------------------------------------------------------------------

C---- Variable declarations


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      INTEGER   I, J, K, L, ITMP, ITMP2
      INTEGER   NUMBER,ISTAT,RCSTAT,NHRS,JULIAN
      INTEGER   KEY,MISS,UPPER,LOWER,VALUE,QARSLT
      INTEGER   IDYMAX(12), JMPDATE
      INTEGER, SAVE :: IPREVDAY, IPREVHOUR, IPREVMIN
      REAL      RMISS,REALDN,REALUP,RVALUE, EPS
      LOGICAL, SAVE :: DataGap
      CHARACTER NAME*4

      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/

C --- Initialize variables to store the previous day,
C     hour and minute for date sequence checks      
      DATA IPREVDAY/0/, IPREVHOUR/-9/, IPREVMIN/-9/

C --- Initialize logical variable used to flag gaps in 
C     ONSITE data record
      DATA DataGap/.FALSE./

C-----------------------------------------------------------------------
C
C        NUMBER  Record number of file
C        NHRS    Counter for the hour being processed
C        ISTAT   Status of process  0 = Initial value (passed
C                                       from OSPATH through OSQACK)
C                                   1 = Problems, force OSSTAT = -1
C                                       to stop processing
C                                   2 = Completed all actions, with
C                                       no fatal errors
C                                   3 = No further data to be
C                                       processed (either EOF or
C                                       rest of data is presumed to
C                                       be beyond "extract" date
C                                       window).
C
C        RCSTAT  Status returned by SUBR.OSFILL
C
C-----------------------------------------------------------------------


C---- Data Initialization

      PATH = 'ONSITE    '
      LOC  = 'OSNEXT'
      EPS  = 0.01

C----------------------------------------------------------
C     Initialize the 'processing' arrays to missing values:
C     First the scalar variables
C----------------------------------------------------------
     
      DO I=1,14
         OSSOBS(1,I) = OSQA(I,2)
         IWORK1(1400+I) = 0
      ENDDO

      DO I=30,34
         IF( I.EQ.34 )THEN
            OSSOBS(1,14+I-29) = OSTSKY(2)
         ELSE
            OSSOBS(1,14+I-29) = OSQA(I,2)
         ENDIF
         IWORK1(1400+I) = 0
      ENDDO

C---- Then the multi-level variables

      DO I=15,29
         DO J=1,OSNL
            OSVOBS(1,J,I-14) = OSQA(I,2)
            IWORK2(100+J,I-14) = 0
            NUMCALM(J) = 0
         ENDDO
      ENDDO

C---- Finally the date group

      OSGDY = NINT( OSQA(52,2) )
      OSGMO = NINT( OSQA(53,2) )
      OSGYR = NINT( OSQA(54,2) )
      OSGHR = NINT( OSQA(55,2) )
      OSGMN = NINT( OSQA(56,2) )

C--------------------------------------------------
C     Test to see if this is first time in S.OSNEXT
C--------------------------------------------------

      IF( NHRS.LT.1 ) GO TO 85

C---- If not, then swap the contents of the buffer into the on-site
C     data arrays for processing - this section of code is
C     processed only once per call to S.OSNEXT

   45 OSGDY = IWORK1(1452)
      OSGMO = IWORK1(1453)
      OSGYR = IWORK1(1454)
      OSGHR = IWORK1(1455)
      OSGMN = IWORK1(1456)

C---- Compute date variable as YYYYMMDD for error reporting
      JMPDATE = OSGYR*10000 + OSGMO*100 + OSGDY
      
C     Swap from work array to the OSSOBS or OSVOBS arrays if the
C     observation is not missing.  If the observation is missing,
C     initialize to zero

C     Scalars: heat flux, u*, ... user-defined #3
      DO I=1,14
         OSSOBS(1,I) = WORK1(1400+I)
         RMISS = OSQA(I,2)
         IF( ABS( WORK1(1400+I)-RMISS ) .LT. EPS )THEN
            CYCLE
         ELSE
            IWORK1(1400+I) = 1
         ENDIF
      ENDDO

      DO I=30,34
         IF( I.EQ.34 )THEN
            RMISS = OSTSKY(2)
         ELSE
            RMISS = OSQA(I,2)
         ENDIF

         OSSOBS(1,14+I-29) = WORK1(1400+I)
         IF( ABS( WORK1(1400+I)-RMISS ) .LT. EPS )THEN
            CYCLE
         ELSE
            IWORK1(1400+I) = 1
         ENDIF
      ENDDO

C---- Multi-level data: height, sigma_A, ..., user-defined #3
      HGHT_LOOP: DO J=1,OSNL

C------- First do wind speed to deal with calms, since calm
C        winds affect other variables
         RMISS = OSQA(23,2)
         IF( (ABS(WORK2(100+J,9)-RMISS)) .LT. EPS )THEN
            OSVOBS(1,J,9) = RMISS
         ENDIF
         
         IF( ((ABS(WORK2(100+J,9)-RMISS)) .GE. EPS ) .AND. 
     &                    WORK2(100+J,9) .LT. OSCALM )THEN
C---------- Wind less than threshold (set obs. WS=OSCALM/2; 
C           also set WD and sigma variables to missing for
C           this level)
            OSVOBS(1,J,9) = OSCALM/2.0
            IWORK2(100+J,9) = 1
            NUMCALM(J) = 1
C ----      First do WORK2 array
            WORK2(100+J,8) = OSQA(22,2)
            WORK2(100+J,2) = OSQA(16,2)
            WORK2(100+J,3) = OSQA(17,2)
            WORK2(100+J,4) = OSQA(18,2)
            WORK2(100+J,5) = OSQA(19,2)
            WORK2(100+J,6) = OSQA(20,2)
C ----      Next do OSVOBS array
            OSVOBS(1,J,8)  = OSQA(22,2)
            OSVOBS(1,J,2)  = OSQA(16,2)
            OSVOBS(1,J,3)  = OSQA(17,2)
            OSVOBS(1,J,4)  = OSQA(18,2)
            OSVOBS(1,J,5)  = OSQA(19,2)
            OSVOBS(1,J,6)  = OSQA(20,2)
         ELSE
C---------- Assign WORK2 value to OSVOBS array for WS,
C           but only set IWORK2 = 1 if non-missing
            OSVOBS(1,J,9) = WORK2(100+J,9)
            IF( ( ABS(WORK2(100+J,9)-RMISS) ) .GE. EPS )THEN
               IWORK2(100+J,9) = 1
            ENDIF
         ENDIF

C------- Now do the rest
         DO I=15,29

            IF( I .EQ. 23 )THEN             
C ---          Wind speed addressed above, cycle to next variable
               CYCLE
            ENDIF

C ---       Assign missing code to RMISS
            RMISS = OSQA(I,2)

            IF( ABS( WORK2(100+J,I-14)-RMISS ) .LT. EPS )THEN
C ---          Variable is missing; set OSVOBS value to missing
               OSVOBS(1,J,I-14) = WORK2(100+J,I-14)
               IF( I .EQ. 22 )THEN
C ---             Also set WORK1 = WORK2 for WD
                  WORK1(100+J) = WORK2(100+J,I-14)
               ENDIF
            ELSE
C------------- Observation is not missing for this variable/level
               IF( (I-14).GE.2 .AND. (I-14).LE.6 )THEN
C ---             Assign square of observation for sigma variables
                  OSVOBS(1,J,I-14) = WORK2(100+J,I-14) *
     &                               WORK2(100+J,I-14)
               ELSEIF( I .EQ. 22 )THEN
C ---             Store WD to OSVOBS and WORK1 (used to handle 0/360 crossover)
                  OSVOBS(1,J,I-14) = WORK2(100+J,I-14)
                  WORK1(100+J) = WORK2(100+J,I-14)
               ELSE
C --------------- For all other variables, assign WORK2 value to OSVOBS array
                  OSVOBS(1,J,I-14) = WORK2(100+J,I-14)
                  
                  IF( STATUS(4,16) .NE. 2 .AND. I .EQ. 15 )THEN
C ----               OSHEIGHTS keyword not specified; get OSHT's from data;
C                    but save previous value to check for height changes.
                     OSHTPREV(J) = OSHT(J)
                     OSHT(J) = OSVOBS(1,J,I-14)
                     IF( ABS( OSHT(J) - OSHTPREV(J) ) .GT. 0.01 . AND.
     &                                  OSHTPREV(J) .GT. 0.0 )THEN
C ----                  OSHT changed from previous hour; issue warning
                        L_HgtsVary = .TRUE.
                        MESS =  BLNK80
                        ECODE = 'W50'
                        WRITE(MESS,900) J, OSHTPREV(J), OSGHR, OSHT(J)
900                     FORMAT(' ONSITE height level ',I2,' of ',F7.2,
     &                         'm for hour ',I3.2,' differs from ',
     &                         'previous hour (',F7.2,'m)')
                        CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
                     ENDIF
                  ENDIF

C ----            Now check for OSHT's decreasing with "height";
C                 must be in order of increasing height
                  IF( J.GT.1 )THEN
                     IF( OSHT(J).LT.OSHT(J-1) )THEN
C ----                  OSHT decreases with height; issue warnings,
C                       and set all data for all levels to missing
                        MESS =  BLNK80
                        ECODE = 'W50'
                        WRITE( MESS,4200 ) J, J-1, OSHT(J), OSHT(J-1)
4200                    FORMAT(' ONSITE height level ',I2,' is lower ',
     &                         'than level ',I2,'; ',F7.2,' vs. ',F7.2)
                        CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )

                        MESS =  BLNK80
                        ECODE = 'W50'
                        WRITE( MESS,4300 ) OSGHR
4300                    FORMAT('  All heights and data will be set to ',
     &                           'missing for hour ',I3.2,'!')
                        CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )
C ----                  Loop through all levels and all data to set
C                       values to missing for this hour
                        DO K = 15,29
                           DO L = 1,OSNL
                              OSVOBS(1,L,K-14)  = OSQA(K,2)
                              WORK2(100+L,K-14) = OSQA(K,2)
                              IF( K .EQ. 22 )THEN
C ----                           Also set WORK1 = missing for WD
                                 WORK1(100+K) = OSQA(K,2)
                              ENDIF
C ----                        Set IWORK2 (counter) = 0 for all variables
                              IWORK2(100+L,K-14) = 0
                           ENDDO
                        ENDDO
C ----                  EXIT the loop through heights for multi-level data
                        EXIT HGHT_LOOP
                     ENDIF
                  ENDIF

               ENDIF

C ----         Now set the counter for non-missing data
               IWORK2(100+J,I-14) = 1
            ENDIF

         ENDDO        ! loop through multi-level variables
         
      ENDDO HGHT_LOOP

C-----Fill buffer with next observation

   85 CALL OSFILL( 4,NUMBER,DEV31,RCSTAT )
           
C     RCSTAT = record status   0 = fill to buffer worked ok
C                              1 = filled buffer but had read errors
C                              2 = the allowable number of read errors
C                                  has been exceeded (MAXERR)
C                              3 = end of file encountered sooner
C                                  than expected.
C                              4 = end of file encountered

      IF( RCSTAT.EQ.1 .OR. RCSTAT.EQ.2 )THEN
C ---    Read error(s); set ISTAT=1 and return
         ISTAT = 1
         RETURN

      ELSEIF( RCSTAT.EQ.3 )THEN
C ---    Premature end-of-file; set ISTAT=1 and return
         ISTAT = 1
         RETURN

      ELSEIF( RCSTAT.EQ.4 )THEN
C ---    Normal end-of-file encountered;
C        if subhourly data being processed, increment
C        to next hour before calling OSHRAV to average
C        data for the last hour; this preserves the 
C        "hour-ending" convention for hourly averaged data
C        (during normal processing, the the hour has been
C        incremented by OSFILL before calling OSHRAV)
         IF( OSAVG.GT.1 )THEN
            OSGHR = OSGHR + 1
            IF( OSGHR.GT.24 )THEN
               OSGHR = 1
               OSGDY = OSGDY + 1
               IF( (OSGMO.NE.2 .AND. 
     &              OSGDY.GT.IDYMAX(OSGMO)) )THEN
                  OSGDY = 1
                  OSGMO = OSGMO + 1
               ELSEIF( OSGMO.EQ.2 .AND.
     &               ((MOD(OSGYR,4) .NE. 0) .OR.
     &                (MOD(OSGYR,100) .EQ. 0 .AND.
     &                 MOD(OSGYR,400) .NE. 0) .AND.
     &                 OSGDY.GT.IDYMAX(OSGMO)-1) )THEN
                  OSGDY = 1
                  OSGMO = OSGMO + 1
               ELSEIF( OSGDY.GT.IDYMAX(OSGMO) )THEN
                  OSGDY = 1
                  OSGMO = OSGMO + 1
               ENDIF
               IF( OSGMO.GT.12 )THEN
                  OSGMO = 1
                  OSGYR = OSGYR + 1
               ENDIF
            ENDIF
C ---       Ready to call OSHRAV to process last hour read
            CALL OSHRAV( NHRS,ISTAT )
            ISTAT = 3
            RETURN
         ELSE
C ---       Still need to call OSHRAV for last hour, even
C           if not subhourly data (error handling and SA calcs)
            CALL OSHRAV( NHRS,ISTAT )
            ISTAT = 3
            RETURN
         ENDIF

      ELSEIF( RCSTAT .EQ. 0 )THEN
C------- Normal return from OSFILL; 
C        Check buffer date/time, if any elements are missing
C        refill buffer with new data.

         DO I=52,55
            IF( IWORK1(1400+I).EQ.OSQA(I,2) ) GO TO 85
         ENDDO

C------- Convert current buffer date to number of days since 1900;
C        first get Julian day (day number of the year)
         IWORK1(1202) = JULIAN(IWORK1(1454),IWORK1(1453),
     &                                      IWORK1(1452) )
         CALL CHROND( PATH,IWORK1(1454),IWORK1(1202),IWORK1(1203))

         IF( IWORK1(1203) .LT. 0 )THEN
C ----      Chronological day is invalid
            MESS =  BLNK80
            ECODE = 'E50'
            WRITE(MESS,1030) IWORK1(1454),IWORK1(1453),IWORK1(1452)
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
            ISTAT = 1
            RETURN
         ENDIF

C------- Assign previous day/hour/minute if needed; used for
C        checks of chronological day/hour/minute sequence
         IF( IPREVDAY .EQ. 0 )THEN
C ----      Initialize previous day/hour/minute to current 
C           day/hour/minute since this is the first read
C ----      NOTE: IPREVDAY and IWORK1(1203) are the
C           previous and current CHRONOLOGICAL days
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IF( OSAVG .GT. 1 )THEN
C ----         Save previous minute for subhourly data
               IPREVMIN = IWORK1(1456)
            ENDIF
C
C------- Begin checks for date/time sequence;
C           First look for dates/times out-of-sequence = ERRORS;
C           Next look for duplicated dates/times, also = ERRORS;
C           Then look for gaps in date/time sequence   = Warnings; unless
C             data gap is > 366 days (allows gap of
C             up to 1 year in multi-year data file)
C
C             Messages for non-subhourly data and multi-day
C             gaps report number of days and hours in gap;
C             messages for subhourly data gaps report number
C             of hours and minutes in the gap.
C
C-------
         ELSEIF( IPREVDAY .GT. IWORK1(1203) )THEN
C ----      Previous day is later then current day, issue error
            MESS =  BLNK80
            ECODE = 'E54'
            IF( OSAVG .EQ. 1 )THEN
               WRITE(MESS,2000) IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                                    IWORK1(1455)
2000           FORMAT(' ONSITE data is out-of-sequence at ',
     &                                    '(Yr,Mn,Dy,Hr):',4I4 )
            ELSEIF( OSAVG .GT. 1 )THEN
               WRITE(MESS,2100) IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                       IWORK1(1455),IWORK1(1456)
2100           FORMAT(' ONSITE data is out-of-sequence at ',
     &                                    '(Yr,Mn,Dy,Hr,Mn): ',5I3 )
            ENDIF
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Set ISTAT = 1 to indicate error condition and RETURN
            ISTAT = 1
            RETURN

         ELSEIF( IPREVDAY  .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR .GT. IWORK1(1455) )THEN
C ----      Previous hour is later then current hour on same day, issue error
            MESS =  BLNK80
            ECODE = 'E54'
            IF( OSAVG .EQ. 1 )THEN
               WRITE(MESS,2000) IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                                    IWORK1(1455)
            ELSEIF( OSAVG .GT. 1 )THEN
               WRITE(MESS,2100) IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                       IWORK1(1455),IWORK1(1456)
            ENDIF
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Set ISTAT = 1 to indicate error condition and RETURN
            ISTAT = 1
            RETURN

         ELSEIF( OSAVG .GT. 1 .AND.
     &           IPREVDAY  .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR .EQ. IWORK1(1455) .AND.
     &           IPREVMIN  .GT. IWORK1(1456) )THEN
C ----      Issue error for minute out-of-sequence for subhourly data;
C           current minute is  earlier than previous minute for same hour
            MESS =  BLNK80
            ECODE = 'E54'
            WRITE(MESS,2100) IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                    IWORK1(1455),IWORK1(1456)
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Set ISTAT = 1 to indicate error condition and RETURN
            ISTAT = 1
            RETURN

         ELSEIF( OSAVG .EQ. 1 .AND.
     &           IPREVDAY  .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR .EQ. IWORK1(1455) )THEN
C ----      Issue error for duplicated observation period
            MESS =  BLNK80
            ECODE = 'E57'
            WRITE(MESS,3000) IWORK1(1454),IWORK1(1453),
     &                       IWORK1(1452),IWORK1(1455)
3000        FORMAT(' Duplicated date, with 1 OBS/HOUR, in ONSITE data ',
     &             'at (Yr,Mn,Dy,Hr): ', 4I4 )
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Set ISTAT = 1 to indicate error condition and RETURN
            ISTAT = 1
            RETURN

         ELSEIF( OSAVG .GT. 1 .AND.
     &           IPREVDAY  .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR .EQ. IWORK1(1455) .AND.
     &           IPREVMIN  .EQ. IWORK1(1456) )THEN
C ----      Issue error for duplicated observation period for subhourly data
            MESS =  BLNK80
            ECODE = 'E57'
            WRITE(MESS,3100) OSAVG,
     &                       IWORK1(1454),IWORK1(1453),IWORK1(1452),
     &                                    IWORK1(1455),IWORK1(1456)
3100        FORMAT(' Duplicated date, with ',I2,' OBS/HOUR, in ',
     &             'ONSITE data at (Yr,Mn,Dy,Hr,Mn): ', 5I3 )
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Set ISTAT = 1 to indicate error condition and RETURN
            ISTAT = 1
            RETURN

         ELSEIF( IPREVDAY .EQ. IWORK1(1203) .AND.
     &           IWORK1(1455)-IPREVHOUR .GT. 1 )THEN
C ----      Appears to be a gap of at least 1 hour within the day
            MESS =  BLNK80
            ECODE = 'W59'
            IF( OSAVG .EQ. 1 )THEN
C ----         Specify gap in terms of # days and # hours
C              for OBS/HOUR (OSAVG) = 1. 
C ----         Assign 0 days to ITMP
               ITMP = 0
C ----         Assign number of hours to ITMP2
               ITMP2 = IWORK1(1455)-IPREVHOUR-1
               WRITE(MESS,4000) ITMP,ITMP2,
     &                          IWORK1(1454),IWORK1(1453),
     &                          IWORK1(1452),IWORK1(1455)
4000           FORMAT(' Data gap of ',I4,' days and ',I3,' hours ',
     &                   'in ONSITE data at (Yr,Mn,Dy,Hr):', 4I4 )
            ELSEIF( OSAVG .GT. 1 )THEN
C ----         Specify gap in terms of # hours and # minutes
C              for OBS/HOUR (OSAVG) > 1;
C ----         First assign total number of minutes in gap to ITMP
               ITMP = (IWORK1(1455)-IPREVHOUR)*60 + 
     &                (IWORK1(1456)-IPREVMIN) - 
     &                 NINT(60./FLOAT(OSAVG))
C ----         Next assign number of minutes in partial hour
C              to ITMP2 and number of full hours to ITMP
               ITMP2 = MOD(ITMP,60)
               ITMP  = INT(ITMP/60)
               WRITE(MESS,4100) ITMP,ITMP2,OSAVG,IWORK1(1454),
     &                              IWORK1(1453),IWORK1(1452),
     &                              IWORK1(1455),IWORK1(1456)
4100           FORMAT(' Data gap of ',I2,' hrs & ',I2,' mins ',
     &                'with ',I2,' OBS/HOUR in ONSITE data at ',
     &                '(Yr,Mn,Dy,Hr,Mn): ',5I3 )
            ENDIF            
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Update value of IPREVDAY and IPREVHOUR
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IF( OSAVG .GT. 1 )THEN
               IPREVMIN = IWORK1(1456)
            ENDIF
C ----      Assign logical flag for data gap; this is used to
C           skip call to OSSUMS when inappropriate due to gap, and
C           to trigger incrementing hour before call to OSHRAV
            DataGap = .TRUE.

         ELSEIF( IPREVDAY+1 .EQ. IWORK1(1203) .AND.
     &           (IWORK1(1455)+24)-IPREVHOUR .GT. 1 )THEN
C ----      Appears to be a gap of at least 1 hour between 
C           successive days
            MESS =  BLNK80
            ECODE = 'W59'
            IF( OSAVG .EQ. 1 )THEN
C ----         Specify gap in terms of # days and # hours
C              for OBS/HOUR (OSAVG) = 1. 
               IF( IWORK1(1455) .GT. IPREVHOUR )THEN
C ----            Set ITMP to 1 day, assign hour-diff to ITMP2
                  ITMP  = 1
                  ITMP2 = IWORK1(1455)-IPREVHOUR-1
               ELSE
C ----            Set ITMP to 0 days, assign hour-diff to ITMP2
                  ITMP  = 0
                  ITMP2 = (IWORK1(1455)+24)-IPREVHOUR-1
               ENDIF
               WRITE(MESS,4000) ITMP,ITMP2,
     &                          IWORK1(1454),IWORK1(1453),
     &                          IWORK1(1452),IWORK1(1455)
            ELSEIF( OSAVG .GT. 1 )THEN
C ----         Specify gap in terms of # hours and # minutes
C              for OBS/HOUR (OSAVG) > 1;
C ----         First assign total number of minutes in gap to ITMP
               ITMP = (IWORK1(1455)+24-IPREVHOUR)*60 + 
     &                (IWORK1(1456)-IPREVMIN) - 
     &                 NINT(60./FLOAT(OSAVG))
C ----         Next assign number of minutes in partial hour
C              to ITMP2 and number of full hours to ITMP
               ITMP2 = MOD(ITMP,60)
               ITMP  = INT(ITMP/60)
               WRITE(MESS,4100) ITMP,ITMP2,OSAVG,IWORK1(1454),
     &                              IWORK1(1453),IWORK1(1452),
     &                              IWORK1(1455),IWORK1(1456)
            ENDIF            
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Update value of IPREVDAY and IPREVHOUR
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IF( OSAVG .GT. 1 )THEN
               IPREVMIN = IWORK1(1456)
            ENDIF
C ----      Assign logical flag for data gap; this is used to
C           skip call to OSSUMS when inappropriate due to gap, and
C           to trigger incrementing hour before call to OSHRAV
            DataGap = .TRUE.

         ELSEIF( OSAVG .GT. 1 .AND. 
     &           IPREVDAY  .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR .EQ. IWORK1(1455) .AND.
     &           IWORK1(1456)-IPREVMIN .GT. 
     &                                    NINT(60./FLOAT(OSAVG)) )THEN
C ----      Appears to be a gap within the hour for subhourly data
            MESS =  BLNK80
            ECODE = 'W59'
C ----      Assign 0 hours to ITMP
            ITMP = 0
C ----      Assign number of minutes in gap to ITMP2
            ITMP2 = IWORK1(1456)-IPREVMIN-NINT(60./FLOAT(OSAVG))
            WRITE(MESS,4100) ITMP,ITMP2,OSAVG,
     &                     IWORK1(1454),IWORK1(1453),
     &                     IWORK1(1452),IWORK1(1455),IWORK1(1456)
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Update values of IPREVDAY, IPREVHOUR and IPREVMIN
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IPREVMIN  = IWORK1(1456)
C ----      Don't assign logical flag for data gap in this case, since
C           data gap is within the hour; only need to track data gaps
C           for gaps that cross hours

         ELSEIF( OSAVG .GT. 1 .AND. 
     &           IPREVDAY    .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR+1 .EQ. IWORK1(1455) .AND.
     &           IWORK1(1456)+60-IPREVMIN .GT. 
     &                                    NINT(60./FLOAT(OSAVG)) )THEN
C ----      Appears to be a gap between successive hours on the same day
C           for subhourly data
            MESS =  BLNK80
            ECODE = 'W59'
C ----      Specify gap in terms of # hours and # minutes
C           for OBS/HOUR (OSAVG) > 1;
C ----      First assign total number of minutes in gap to ITMP
            ITMP = (IWORK1(1455)-IPREVHOUR)*60 + 
     &             (IWORK1(1456)-IPREVMIN) - 
     &              NINT(60./FLOAT(OSAVG))
C ----      Next assign number of minutes in partial hour
C           to ITMP2 and number of full hours to ITMP
            ITMP2 = MOD(ITMP,60)
            ITMP  = INT(ITMP/60)
            WRITE(MESS,4100) ITMP,ITMP2,OSAVG,IWORK1(1454),
     &                           IWORK1(1453),IWORK1(1452),
     &                           IWORK1(1455),IWORK1(1456)
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Update value of IPREVDAY, IPREVHOUR and IPREVMIN
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IPREVMIN  = IWORK1(1456)
C ----      Assign logical flag for data gap; this is used to
C           skip call to OSSUMS when inappropriate due to gap, and
C           to trigger incrementing hour before call to OSHRAV
            DataGap = .TRUE.

         ELSEIF( OSAVG .GT. 1 .AND. 
     &           IPREVDAY+1   .EQ. IWORK1(1203) .AND.
     &           IPREVHOUR-23 .EQ. IWORK1(1455) .AND.
     &           IWORK1(1456)+60-IPREVMIN .GT. 
     &                                    NINT(60./FLOAT(OSAVG)) )THEN
C ----      Appears to be a gap between successive hours across midnight
C           for subhourly data
            MESS =  BLNK80
            ECODE = 'W59'
C ----      Specify gap in terms of # hours and # minutes
C           for OBS/HOUR (OSAVG) > 1;
C ----      First assign total number of minutes in gap to ITMP
            ITMP = (IWORK1(1455)+24-IPREVHOUR)*60 + 
     &             (IWORK1(1456)-IPREVMIN) - 
     &              NINT(60./FLOAT(OSAVG))
C ----      Next assign number of minutes in partial hour
C           to ITMP2 and number of full hours to ITMP
            ITMP2 = MOD(ITMP,60)
            ITMP  = INT(ITMP/60)
            WRITE(MESS,4100) ITMP,ITMP2,OSAVG,IWORK1(1454),
     &                           IWORK1(1453),IWORK1(1452),
     &                           IWORK1(1455),IWORK1(1456)
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
C ----      Update values of IPREVDAY, IPREVHOUR and IPREVMIN
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IPREVMIN  = IWORK1(1456)
C ----      Assign logical flag for data gap; this is used to
C           skip call to OSSUMS when inappropriate due to gap, and
C           to trigger incrementing hour before call to OSHRAV
            DataGap = .TRUE.

         ELSEIF( IWORK1(1203)-IPREVDAY .GT. 1 )THEN
C ----      There appears to be a multi-day gap in the ONSITE data; 
C           issue warning if gap is less than 367 days, but
C           issue error message if gap is larger than 366
            MESS =  BLNK80
            IF( IWORK1(1203)-IPREVDAY .LE. 366 )THEN
               ECODE = 'W59'
            ELSEIF( IWORK1(1203)-IPREVDAY .GT. 366 )THEN
               ECODE = 'E59'
            ENDIF
C ----      Write message identifying size of data gap in
C           in number of days and hours
            IF( IWORK1(1455) .GT. IPREVHOUR )THEN
               WRITE(MESS,4000) IWORK1(1203)-IPREVDAY, 
     &                          IWORK1(1455)-IPREVHOUR-1,
     &                          IWORK1(1454),IWORK1(1453),
     &                          IWORK1(1452),IWORK1(1455)
            ELSEIF( IWORK1(1455) .LE. IPREVHOUR )THEN
               WRITE(MESS,4000) IWORK1(1203)-IPREVDAY-1, 
     &                      ( 24-(IPREVHOUR-IWORK1(1455)) )-1,
     &                          IWORK1(1454),IWORK1(1453),
     &                          IWORK1(1452),IWORK1(1455)
            ENDIF
            CALL ERRHDL(NUMBER,PATH,ECODE,LOC,MESS)
            
C ----      Check for Error and Warning and act accordingly
            IF( ECODE .EQ. 'E59' )THEN
C ----         Set ISTAT = 1 to indicate error condition and RETURN
               ISTAT = 1
               RETURN
            ELSE
C ----         Message is only a warning;
C              Update values of IPREVDAY, IPREVHOUR and IPREVMIN
               IPREVDAY  = IWORK1(1203)
               IPREVHOUR = IWORK1(1455)
               IF( OSAVG .GT. 1 )THEN
C ---             Update value of IPREVMIN for subhourly data
                  IPREVMIN = IWORK1(1456)
               ENDIF
C ----         Assign logical flag for data gap; this is used to
C              skip call to OSSUMS when inappropriate due to gap, and
C              to trigger incrementing hour before call to OSHRAV
               DataGap = .TRUE.
            ENDIF
            
         ELSE
C ----      Update value of IPREVDAY and IPREVHOUR
            IPREVDAY  = IWORK1(1203)
            IPREVHOUR = IWORK1(1455)
            IF( OSAVG .GT. 1 )THEN
C ---          Update value of IPREVMIN for subhourly data
               IPREVMIN = IWORK1(1456)
            ENDIF

         ENDIF

C------- Check current buffer date/time against "QA" dates:
C        OSDAY1 = beginning Julian date
C        OSDAY2 = ending Julian date

         IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 )THEN
            IF( IWORK1(1203) .LT. IWORK1(1200) )THEN
C              Buffer data is earlier than extract dates,
C              go get next os observation
               GO TO 85
            ENDIF

            IF( IWORK1(1203) .GT. IWORK1(1201) )THEN
C ---          Buffer date is beyond extract window, compute
C              averages if necessary, set ISTAT = 3 and return.
               IF( OSAVG.GT.1 )THEN
C ---             Increment to next hour before calling OSHRAV to 
C                 average data for the last hour in the extract window; 
C                 this preserves "hour-ending" convention for hourly 
C                 averaged data
C                 (during normal processing, the the hour has been
C                 incremented by OSFILL before calling OSHRAV)
                  OSGHR = OSGHR + 1
                  IF( OSGHR.GT.24 )THEN
                     OSGHR = 1
                     OSGDY = OSGDY + 1
                     IF( (OSGMO.NE.2 .AND. 
     &                    OSGDY.GT.IDYMAX(OSGMO)) )THEN
                        OSGDY = 1
                        OSGMO = OSGMO + 1
                     ELSEIF( OSGMO.EQ.2 .AND.
     &                     ((MOD(OSGYR,4) .NE. 0) .OR.
     &                      (MOD(OSGYR,100) .EQ. 0 .AND.
     &                       MOD(OSGYR,400) .NE. 0) .AND.
     &                       OSGDY.GT.IDYMAX(OSGMO)-1) )THEN
                        OSGDY = 1
                        OSGMO = OSGMO + 1
                     ELSEIF( OSGDY.GT.IDYMAX(OSGMO) )THEN
                        OSGDY = 1
                        OSGMO = OSGMO + 1
                     ENDIF
                     IF( OSGMO.GT.12 )THEN
                        OSGMO = 1
                        OSGYR = OSGYR + 1
                     ENDIF
                  ENDIF
C ---             Now ready to call OSHRAV for last extracted hour
                  CALL OSHRAV ( NHRS,ISTAT )
                  ISTAT = 3
                  RETURN
               ELSEIF( OSAVG.EQ.1 )THEN
C ---             Still need to call OSHRAV for last hour, even
C                 if not subhourly data (error handling and SA calcs)
                  CALL OSHRAV ( NHRS,ISTAT )
                  ISTAT = 3
                  RETURN
               ENDIF
            ENDIF

         ENDIF

C------- QA buffer values if 'OSAVG', the number of observations
C        that constitute a one hour average, is greater than 1;
C        also do QA check for hourly WDIR, if it is being audited
C        since this will check for WDIR values out-of-range before
C        they are adjusted to fall within 0 to 360 degrees;
C        compute DAY*100 + HOUR first

         IF( OSAVG.GT.1 .OR. OSSAUD(22).EQ.1 )THEN
            IWORK1(1210) = IWORK1(1202)*100 + IWORK1(1455)

C---------- First the scalar variables

            DO I = 1,14
               IF( OSAVG.EQ.1 ) EXIT             ! exit loop for OSAVG=1
               IF( OSSAUD(I) .EQ. 1 )THEN
                  RVALUE = WORK1(1400+I)
                  KEY    = NINT( OSQA(I,1) )
                  RMISS  = OSQA(I,2)
                  REALDN = OSQA(I,3)
                  REALUP = OSQA(I,4)
                  IF( I.GE.9 .AND. I.LE.11 .AND. 
     &                             ABS( RVALUE-RMISS ) .GT. EPS )THEN
C                    Compute the gradient of temperature difference
                     RVALUE = RVALUE*100/( OSUL(I-8)-OSLL(I-8) )
                  ENDIF
                  NAME   = VNAMES(I)
                  QARSLT = 6
                  CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,
     &                         REALUP,RVALUE,NAME,IWORK1(1454),
     &                         IWORK1(1453),IWORK1(1452),IWORK1(1455),
     &                         IWORK1(1456),0,QARSLT)
               ENDIF
            ENDDO

C---------- Then the multi-level variables

            DO I=15,29
               IF( OSAVG.EQ.1 .AND. I.NE.22 ) CYCLE    ! cycle loop for OSAVG=1 unless WDIR
               IF( OSSAUD(I) .EQ. 1 )THEN
                  KEY    = NINT( OSQA(I,1) )
                  RMISS  = OSQA(I,2)
                  REALDN = OSQA(I,3)
                  REALUP = OSQA(I,4)
                  NAME   = VNAMES(I)
                  DO J=1,OSNL
                     RVALUE = WORK2(100+J,I-14)
                     QARSLT = 6
                     CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,
     &                           REALUP,RVALUE,NAME,IWORK1(1454),
     &                           IWORK1(1453),IWORK1(1452),IWORK1(1455),
     &                           IWORK1(1456),J,QARSLT)
                  ENDDO
               ENDIF
            ENDDO

C---------- Then the variables common with NWS surface data

            DO I=30,34
               IF( OSAVG.EQ.1 ) EXIT             ! exit loop for OSAVG=1
               IF( OSSAUD(I) .EQ. 1 )THEN
                  IF( I.EQ.34 )THEN
                     KEY    = NINT( OSTSKY(1) )
                     RMISS  = OSTSKY(2)
                     REALDN = OSTSKY(3)
                     REALUP = OSTSKY(4)
                  ELSE
                     KEY    = NINT( OSQA(I,1) )
                     RMISS  = OSQA(I,2)
                     REALDN = OSQA(I,3)
                     REALUP = OSQA(I,4)
                  ENDIF
                  NAME   = VNAMES(I)
                  RVALUE = WORK1(1400+I)
                  QARSLT = 6
                  CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,
     &                         REALUP,RVALUE,NAME,IWORK1(1454),
     &                         IWORK1(1453),IWORK1(1452),IWORK1(1455),
     &                         IWORK1(1456),0,QARSLT)
               ENDIF
            ENDDO

C---------- Finally, the on-site date group (always QA)

            DO I=52,56
               IF( OSAVG.EQ.1 ) EXIT             ! exit loop for OSAVG=1
               KEY   = NINT( OSQA(I,1) )
               MISS  = NINT( OSQA(I,2) )
               LOWER = NINT( OSQA(I,3) )
               UPPER = NINT( OSQA(I,4) )
               VALUE = IWORK1(1400+I)
               NAME  = VNAMES(I)
               QARSLT = 6
               CALL INTEQA( 4,IWORK1(1210),KEY,MISS,LOWER,
     &                      UPPER,VALUE,NAME,IWORK1(1454),IWORK1(1453),
     &                      IWORK1(1452),IWORK1(1455),QARSLT)
               IF( QARSLT.NE.0 .AND. OSAVG.GT.1 )THEN
C-------          Error or out-of-range found for date group;
C                 issue error message and abort processing
                  MESS =  BLNK80
                  ECODE = 'E50'
                  WRITE(MESS,5000) IWORK1(1454),IWORK1(1453),
     &                             IWORK1(1452),IWORK1(1455),
     &                             IWORK1(1456)
5000              FORMAT(' Invalid date value for OSGYR, OSGMO,', 
     &                   ' OSGDY, OSGHR, or OSGMN: ',5I4)
                  CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
                  ISTAT = 1
                  RETURN
               ENDIF
            ENDDO

         ENDIF               ! OSAVG > 1, or OSSAUD(22)=1

C------- Compare times
C        Note: the first time in SUBR.OSNEXT, OSGDY,... are
C              all missing; the second part of this IF
C              structure (GO TO 45) takes care of this

         IF( (IWORK1(1452).EQ.OSGDY  .AND.
     &        IWORK1(1453).EQ.OSGMO  .AND.
     &        IWORK1(1454).EQ.OSGYR  .AND.
     &        IWORK1(1455).EQ.OSGHR) .OR.
     &       (OSAVG.GT.1 .AND. 
     &        IWORK1(1456).EQ.0) )THEN

C-------    Data in buffer arrays (IWORK1, WORK2) are for 
C           current processing hour, increment sums, 
C           then go to 85 to read next record; also 
C           increment sums if minute 0 of "next" hour
C           is in buffer, since that is part of the 
C           current hour for subhourly data, but 
C           hour (IWORK1(1455)) has already been
C           incremented.
            CALL OSSUMS
            GO TO 85

         ELSEIF( OSGDY .EQ. OSQA(52,2) )THEN
C---------- Current date group is not defined - most likely
C           the first time in SUBR.OSNEXT; back up to
C           swap buffer data into current processing arrays
            GO TO 45

         ELSE
C---------- New hour's data in buffer arrays, assign date field for
C           final subhourly record within the hour, compute hourly 
C           averages and return;
C           but only do this if subhourly data are being processed,
C           and only if DataGap flag has not be set

            IF( OSAVG .GT. 1 .AND. .NOT.DataGap )THEN
               OSGDY = IWORK1(1452)
               OSGMO = IWORK1(1453)
               OSGYR = IWORK1(1454)
               OSGHR = IWORK1(1455)
               OSGMN = IWORK1(1456)
C----------    Compute hourly averages
               CALL OSHRAV( NHRS,ISTAT)
               IF( ISTAT.EQ.1 )THEN
C                 Errors encountered
                  CONTINUE
               ELSE
C                 No errors, reset status
                  ISTAT = 2
               ENDIF

            ELSEIF( OSAVG .GT. 1 .AND. DataGap )THEN
C ---          DataGap flag has been set, so we need to increment 
C              to the next hour before calling OSHRAV to average data 
C              for the last hour (or partial hour) of data in the 
C              buffer arrays, since the hour hasn't been incremented
C              to represent the hourly average by reading the data;
C              this preserves "hour-ending" convention for hourly 
C              averaged data;
C              (during normal processing, the the hour has been
C              incremented by OSFILL before calling OSHRAV)
               OSGHR = OSGHR + 1
               IF( OSGHR.GT.24 )THEN
                  OSGHR = 1
                  OSGDY = OSGDY + 1
                  IF( (OSGMO.NE.2 .AND. 
     &                 OSGDY.GT.IDYMAX(OSGMO)) )THEN
                     OSGDY = 1
                     OSGMO = OSGMO + 1
                  ELSEIF( OSGMO.EQ.2 .AND.
     &                  ((MOD(OSGYR,4) .NE. 0) .OR.
     &                   (MOD(OSGYR,100) .EQ. 0 .AND.
     &                    MOD(OSGYR,400) .NE. 0) .AND.
     &                    OSGDY.GT.IDYMAX(OSGMO)-1) )THEN
                     OSGDY = 1
                     OSGMO = OSGMO + 1
                  ELSEIF( OSGDY.GT.IDYMAX(OSGMO) )THEN
                     OSGDY = 1
                     OSGMO = OSGMO + 1
                  ENDIF
                  IF( OSGMO.GT.12 )THEN
                     OSGMO = 1
                     OSGYR = OSGYR + 1
                  ENDIF
               ENDIF
C----------    Now ready to call OSHRAV for last hour, or partial
C              hour extracted before the data gap;
C              Compute hourly averages
               CALL OSHRAV( NHRS,ISTAT)
C----------    Reinitialize DataGap flag since hourly average has
C              been processed
               DataGap = .FALSE.
               IF( ISTAT.EQ.1 )THEN
C                 Errors encountered
                  CONTINUE
               ELSE
C                 No errors, reset status
                  ISTAT = 2
               ENDIF

            ELSEIF( OSAVG .EQ. 1 )THEN
C----------    Still need to call OSHRAV, even if not 
C              subhourly data (error handling and SA calcs)
               CALL OSHRAV( NHRS,ISTAT)
C----------    Reinitialize DataGap flag since hourly average has
C              been processed (even though it's not really needed 
C              for non-subhourly data)
               DataGap = .FALSE.
               IF( ISTAT.EQ.1 )THEN
C                 Errors encountered
                  CONTINUE
               ELSE
C                 No errors, reset status
                  ISTAT = 2
               ENDIF

            ENDIF

C---------- Check buffer date with current hour's date.  Insure
C           that buffer data is for later time.  If so, then
C           initialize the summation array for the square of the
C           wind direction; if not, print an error message and set
C           ISTAT = 1 to kill further processing.

            OSDAYC = JULIAN( OSGYR,OSGMO,OSGDY )
            CALL CHROND( PATH,OSGYR,OSDAYC,IWORK1(1204) )
            
C---------- Check for error with chronological day (< 0)
            IF( IWORK1(1204) .LT. 0 )THEN
               MESS =  BLNK80
               ECODE = 'E50'
               WRITE(MESS,1030) OSGYR, OSGMO, OSGDY
1030           FORMAT(' Invalid chronological day for OSGYR, OSGMO,', 
     &                ' OSGDY: ',3I4)
               CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
               ISTAT = 1
               RETURN
            ENDIF

            IF( (IWORK1(1204)-1)*24+OSGHR  .LE.
     &          (IWORK1(1203)-1)*24+IWORK1(1455) )THEN
C------------- Date sequence is ok
               CONTINUE

            ELSE
C------------- Dates are out-of-sequence; this has probably 
C              already been caught above, but will leave code
C              here just in case
               ISTAT = 1
               MESS =  BLNK80
               ECODE = 'E54'
               WRITE( MESS,1000 ) OSGYR,OSGMO,OSGDY,OSGHR,OSGMN
1000           FORMAT(1X,'Observations are NOT sequenced in time at ',
     &                '(YR/MN/DY/HR/MN): ',5I4 )
               CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
               RETURN
            ENDIF

         ENDIF

      ENDIF

      RETURN
      END

