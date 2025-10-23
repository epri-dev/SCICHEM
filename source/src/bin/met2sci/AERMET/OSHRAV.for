        SUBROUTINE OSHRAV( NHRS,ISTAT )
C=====================================================================**
C          OSHRAV Module of the AERMET Meteorological Preprocessor
C
C     Purpose:   Computes hour averages of on-site data, checking that
C                there are a sufficient number of values for a valid
C                average.
C
C     Release date: December 1992
C
C     Called by: OSNEXT
C
C-----------------------------------------------------------------------
C
C------ Variable Declarations


      IMPLICIT NONE
      
      INTEGER NHRS,ISTAT,L,N
      REAL RMISS, EPS

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      LOGICAL  SAFLAG(OSML), CALM_WD(OSML)

C      NHRS      Hour passed in from calling program
C      ISTAT     Processing status:
C                      0 - no errors
C                      1 - error
C      SAFLAG    Flag to determine if an average sigma_A can be
C                computed at a level

C---- Data Initializations
      DATA   SAFLAG /OSML*.FALSE./, CALM_WD /OSML*.FALSE./
      PATH = PATHWD(4)
      LOC  = 'OSHRAV'
      EPS  = 0.01

C---- Process the data: First the scalar variables

C---- Onsite HFLX, USTR, MHGT, Z0HT, SAMT, PAMT, INSO,
C            NRAD, DT01, DT02, DT03, US01, US02, US03
      DO L=1,14
         IF( (ABS( OSSOBS(1,L)-OSQA(L,2) ).LT.EPS .AND. 
     &                               IWORK1(1400+L).LE.1) .OR.
     &                               IWORK1(1400+L).EQ.0 )THEN
C---------- Missing data; 
C           includes check for sum of valid data = missing indicator
            CYCLE

         ELSEIF( IWORK1(1400+L) .LT. OSMIN .AND.
     &                   L.NE.5 .AND. L.NE.6 )THEN
C---------- The number of valid subhourly values is less than
C           the minimum number required to compute the average,
C           so set the hourly value to missing, except for SAMT
C           or PAMT.
            OSSOBS(1,L) = OSQA(L,2)

C---------- Issue informational message for missing due to 
C           number of subhourly values < OSMIN
            MESS =  BLNK80
            ECODE = 'I55'
            WRITE(MESS,800) IWORK1(1400+L), VNAMES(L),
     &                      IWORK1(1454), IWORK1(1453),
     &                      IWORK1(1452), IWORK1(1455)
  800       FORMAT(' Only ',I3,' values (less than OSMIN) for ',A4,
     &             ' at (Yr,Mn,Dy,Hr) ', 4I4,
     &             '; hourly average missing.')
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )

         ELSEIF( IWORK1(1400+L) .LT. OSMIN .AND.
     &                  (L.EQ.5 .OR. L.EQ.6) )THEN
C---------- The number of valid subhourly values is less than
C           the minimum number required to compute the average,
C           leave total for SAMT or PAMT, but issue warning.

C---------- Issue warning message for total precipitation with
C           number of subhourly values < OSMIN
            MESS =  BLNK80
            ECODE = 'W55'
            WRITE(MESS,900) IWORK1(1400+L), OSAVG, VNAMES(L),
     &                      IWORK1(1454), IWORK1(1453),
     &                      IWORK1(1452), IWORK1(1455)
  900       FORMAT(' Only ',I3,' values out of ',I3,' for total ',A4,
     &             ' at (Yr,Mn,Dy,Hr): ', 4I4)
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )

         ELSEIF( IWORK1(1400+L) .GT. OSAVG )THEN
C---------- The number of valid subhourly values exceeds the
C           number specified by the user - error
            MESS =  BLNK80
            ECODE = 'E55'
            WRITE(MESS,1000) IWORK1(1400+L), VNAMES(L),
     &                       IWORK1(1454), IWORK1(1453),
     &                       IWORK1(1452), IWORK1(1455)
 1000       FORMAT(' Found ',I4,' values for variable ',A4,
     &             ' at (Yr,Mn,Dy,Hr): ', 4I4 )
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )
            OSSOBS(1,L) = OSQA(L,2)
            ISTAT = 1

         ELSEIF( (OSSOBS(1,L).EQ.0.0).AND.(IWORK1(1400+L).EQ.0) )THEN
C---------- All the subhourly values were missing - hourly
C           value set to missing
            OSSOBS(1,L) = OSQA(L,2)

         ELSEIF( L.NE.5 .AND. L.NE.6 )THEN
C---------- Calculate hourly averages, except for SAMT and PAMT
            OSSOBS(1,L) = OSSOBS(1,L)/FLOAT( IWORK1(1400+L) )

         ENDIF

      ENDDO


C---- Onsite PRCP, SLVP, PRES, CLHT, and TSKC
      DO L=30,34
         IF( L.EQ.34 )THEN
            RMISS = OSTSKY(2)
         ELSE
            RMISS = OSQA(L,2)
         ENDIF

         IF( (ABS( OSSOBS(1,L-15)-RMISS ).LT.EPS .AND. 
     &                               IWORK1(1400+L).LE.1) .OR.
     &                               IWORK1(1400+L).EQ.0 )THEN
C---------- Missing data; 
C           includes check for sum of valid data = missing indicator
            CYCLE

         ELSEIF( IWORK1(1400+L) .LT. OSMIN .AND.
     &                                    L.NE.30 )THEN
C---------- The number of valid subhourly values is less than
C           the minimum number required to compute the average,
C           so set the hourly value to missing, except for PRCP.
            OSSOBS(1,L-15) = RMISS

C---------- Issue informational message for missing due to 
C           number of subhourly values < OSMIN
            MESS =  BLNK80
            ECODE = 'I55'
            WRITE(MESS,800) IWORK1(1400+L), VNAMES(L),
     &                      IWORK1(1454), IWORK1(1453),
     &                      IWORK1(1452), IWORK1(1455)
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )

         ELSEIF( IWORK1(1400+L) .LT. OSMIN .AND.
     &                                    L.EQ.30 )THEN
C---------- The number of valid subhourly values is less than
C           the minimum number required to compute the average,
C           leave total for PRCP, but issue warning.

C---------- Issue warning message for total precipitation with
C           number of subhourly values < OSMIN
            MESS =  BLNK80
            ECODE = 'W55'
            WRITE(MESS,900) IWORK1(1400+L), OSAVG, VNAMES(L),
     &                      IWORK1(1454), IWORK1(1453),
     &                      IWORK1(1452), IWORK1(1455)
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )

         ELSEIF( IWORK1(1400+L).GT.OSAVG )THEN
C---------- The number of valid subhourly values exceeds the
C           number specified by the user - error
            MESS =  BLNK80
            ECODE = 'E55'
            WRITE(MESS,1000) IWORK1(1400+L), VNAMES(L),
     &                       IWORK1(1454), IWORK1(1453),
     &                       IWORK1(1452), IWORK1(1455)
            CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )
            OSSOBS(1,L-15) = RMISS
            ISTAT = 1

         ELSEIF( (OSSOBS(1,L-15).EQ.0.0)  .AND.
     &           (IWORK1(1400+L).EQ.0) )THEN
C---------- All the subhourly values were missing - hourly
C           value set to missing
            OSSOBS(1,L-15) = RMISS

         ELSEIF( L.NE.30 )THEN
C---------- Calculate hourly averages, except for PRCP
            OSSOBS(1,L-15) = OSSOBS(1,L-15)/FLOAT( IWORK1(1400+L) )

         ENDIF

      ENDDO

C---- Then the multi-level variables

C---- Onsite HT, SA, SE, SV, SW, SU, TT, WD, WS, VV, 
C            DP, RH, V1, V2, V3
C---- Initialize logical array to track CALM based on WD
      CALM_WD(:) = .FALSE.

      DO L=15,29

C------- Loop through tower levels
         DO N=1,OSNL
            IF( (ABS( OSVOBS(1,N,L-14)-OSQA(L,2) ).LT.EPS .AND. 
     &                            IWORK2(100+N,L-14).LE.1) .OR.
     &                            IWORK2(100+N,L-14).EQ.0 )THEN
C------------- Missing data; 
C              includes check for sum of valid data = missing indicator
               CYCLE

            ELSEIF( IWORK2(100+N,L-14).LT.OSMIN )THEN
C------------- The number of valid subhourly values is less than
C              the minimum number required to compute the average,
C              so generally the hourly value will be set to missing;
C------------- however, first check for calm hour based on at least
C              half of the subhourly samples being calm for WD to 
C              avoid labeling a calm hour as missing
               IF( L.EQ.22 .AND. NUMCALM(N) .GE. OSMIN )THEN
C --------------- At least half the samples for the hour are calm, but
C                 less than the minimum number of valid, non-calm values
C                 are available; since WD is set missing for calm samples 
C                 in OSSUMS, reset it to 0.0 and set CALM_WD flag for 
C                 this level for use below when processing average WS
                  OSVOBS(1,N,L-14) = 0.0
                  CALM_WD(N) = .TRUE.
               ELSE

                  OSVOBS(1,N,L-14) = OSQA(L,2)

C---------------- Issue informational message for missing due to 
C                 number of subhourly values < OSMIN
                  MESS =  BLNK80
                  ECODE = 'I55'
             
C---------------- Adjust informational message regarding too few 
C                 subhourly values to account for calm vs. missing
C                 for wind speed/wind direction.
                  IF( L-14 .NE. 8 .AND. L-14 .NE. 9 )THEN
C ---                Use format 800 for variables other than WS/WD
                     WRITE(MESS,800) IWORK2(100+N,L-14), VNAMES(L),
     &                               IWORK1(1454), IWORK1(1453),
     &                               IWORK1(1452), IWORK1(1455)
                  ELSE
C ---                Use format 801 for WS/WD since hour may be 
C                    calm rather than missing
                     WRITE(MESS,801) IWORK2(100+N,L-14), VNAMES(L),
     &                               IWORK1(1454), IWORK1(1453),
     &                               IWORK1(1452), IWORK1(1455)
  801               FORMAT(' Only ',I3,' values (less than OSMIN) for ',
     &                     A4,' at (Yr,Mn,Dy,Hr) ', 4I4,
     &                     '; hrly ave miss or calm.')
                  ENDIF
                  CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )

               ENDIF ! NUMCALM IF-THEN Block
               
            ELSEIF( IWORK2(100+N,L-14).GT.OSAVG )THEN
C------------- The number of valid subhourly values exceeds the
C              number specified by the user - error
               MESS =  BLNK80
               ECODE = 'E55'
               WRITE(MESS,1000) IWORK2(100+N,L-14), VNAMES(L),
     &                          IWORK1(1454), IWORK1(1453),
     &                          IWORK1(1452), IWORK1(1455)
               CALL ERRHDL( NHRS,PATH,ECODE,LOC,MESS )
               OSVOBS(1,N,L-14) = OSQA(L,2)
               ISTAT = 1

            ELSEIF( (OSVOBS(1,N,L-14) .EQ. 0.0) .AND.
     &                (IWORK2(100+N,L-14) .EQ. 0) )THEN
C------------- All the subhourly values were missing - hourly
C              value set to missing
               OSVOBS(1,N,L-14) = OSQA(L,2)

            ELSE
C------------- Calculate hourly averages
               IF( L .GE. 16  .AND. L .LE. 20 )THEN
C---------------- The standard deviations
                  OSVOBS(1,N,L-14) =
     &             SQRT( OSVOBS(1,N,L-14)/FLOAT( IWORK2(100+N,L-14)))

               ELSEIF( L .EQ. 22 )THEN
C---------------- Wind direction; adjust to be between 0 and 360
                  OSVOBS(1,N,8) = OSVOBS(1,N,8) /
     &                             FLOAT( IWORK2(100+N,L-14) )

                  DO WHILE( OSVOBS(1,N,8) .GT. 360.0  .OR.
     &                      OSVOBS(1,N,8) .LT.   0.0 )
                     IF( OSVOBS(1,N,8) .GT. 360.0 )THEN
                         OSVOBS(1,N,8) = OSVOBS(1,N,8) - 360.0
                     ELSEIF( OSVOBS(1,N,8) .LT. 0.0 )THEN
                         OSVOBS(1,N,8) = OSVOBS(1,N,8) + 360.0
                     ENDIF
                  ENDDO

                  IF( OSVOBS(1,N,8) .LE. 0.05 )THEN
C---------------     Adjust WD to output 360 rather than 0 degrees
                     OSVOBS(1,N,8) = 360.0
                  ENDIF

               ELSEIF( L.EQ.21  .OR.  L.GE.23 )THEN
C---------------- The remaining 'simple average' multi-level variables
C                 L = 21, 23-29
C---------------- First check for calm hour based on at least half of 
C                 the subhourly samples being calm; CALM_WD flag should
C                 have been set .T. above since WD is set missing for
C                 calm samples.  
                  IF( L.EQ.23 .AND. NUMCALM(N) .GE. OSMIN .AND.
     &                                              CALM_WD(N) )THEN
C------------------- If at least half the hour is calm and WD is missing for 
C                    this hour/level; set WS (L=23) to 0.0 rather than missing
                     OSVOBS(1,N,L-14) = 0.0
                  ELSE
                     OSVOBS(1,N,L-14) = OSVOBS(1,N,L-14) /
     &                                   FLOAT( IWORK2(100+N,L-14) )
                  ENDIF

               ENDIF
            ENDIF

         ENDDO

      ENDDO

      RETURN
      END

