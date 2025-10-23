        SUBROUTINE OSSUMS
C=====================================================================**
C          OSSUMS Module of the AERMET Meteorological Preprocessor
C
C   PURPOSE:  Sums buffer data that are in processing arrays
C             (if data are not missing) and increments counter.
C
C   NOTE:     The purpose of setting the array elements to
C             zero is to properly sum the observed values.
C             If there are multiple observations per hour
C             and the first observation for the hour, which is
C             already swapped into the OS arrays, is missing, the
C             sum will be in error if os_obs is not set to 0.
C
C   VERSION DATE:  15 DEC 1992
C
C   CALLED BY: OSNEXT
C
C   Revision history:
C       Aug 02, 1995 - Sum the square of the subhourly wind direction
C                      for the hourly average for sigma_A
C       April 2009   - Sum of the square of the subhourly wind direction
C                      for sigma_a removed - see EPA Meteorological
C                      Monitoring Guidance, Section 6.2.1, Eq. 6.2.10
C
C-----------------------------------------------------------------------

C---- Varibale Declarations


      IMPLICIT NONE
      
      REAL     RMISS, EPS
      INTEGER  L, N
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      EPS = 0.01

C---- First the scalar variables

      DO L=1,14
         IF( ABS(WORK1(1400+L)-OSQA(L,2)) .LT. EPS )THEN
            CYCLE
         ELSEIF( L.EQ.6 .AND. WORK1(1400+L) .LT. 0.0 )THEN
C----       Don't sum negative precip values (PAMT)
            CYCLE
         ELSE
            IF( ABS(OSSOBS(1,L)-OSQA(L,2)) .LT. EPS .AND.
     &              IWORK1(1400+L) .LE. 1 )THEN
               OSSOBS(1,L) = 0.0
            ENDIF
            
            OSSOBS(1,L) = OSSOBS(1,L) + WORK1(1400+L)
            IWORK1(1400+L) = IWORK1(1400+L) + 1
         ENDIF
      ENDDO

C---- The altimeter pressure, sea level pressure, station pressure,
C     ceiling height, and sky coverage can also appear as on-site
C     varaibles
      DO L=30,34
         IF( L.EQ.34 )THEN
            RMISS = OSTSKY(2)
         ELSE
            RMISS = OSQA(L,2)
         ENDIF
C
         IF( ABS(WORK1(1400+L)-RMISS) .LT. EPS )THEN
            CYCLE
         ELSEIF( L.EQ.30 .AND. WORK1(1400+L) .LT. 0.0 )THEN
C----       Don't sum negative precip values (PRCP)
            CYCLE
         ELSE
            IF( ABS(OSSOBS(1,L-15)-RMISS) .LT. EPS .AND.
     &              IWORK1(1400+L) .LE. 1 )THEN
               OSSOBS(1,L-15) = 0.0
            ENDIF
            
            OSSOBS(1,L-15) = OSSOBS(1,L-15) + WORK1(1400+L)
            IWORK1(1400+L) = IWORK1(1400+L) + 1
         ENDIF
      ENDDO

C---- Then the multi-level variables, looping on the number of levels
C     in the on-site data (OSNL)

      DO N=1,OSNL

C------- First process wind speed

         IF( ABS(WORK2(100+N,9)-OSQA(23,2)) .LT. EPS )THEN
C---------- Wind speed is missing
            CONTINUE

         ELSE
            IF( ABS(OSVOBS(1,N,9)-OSQA(23,2)) .LT. EPS .AND.
     &            IWORK2(100+N,9) .LE. 1 )THEN
C------------- OSVOBS value is missing; reset to 0.0 if .LE. 1
C              value for this hour
               OSVOBS(1,N,9) = 0.0
            ENDIF

            IF( WORK2(100+N,9) .LT. OSCALM )THEN
C------------- Wind less than threshold (set obs. WS=OSCALM/2; ignore WD)
               OSVOBS(1,N,9) = OSVOBS(1,N,9) + OSCALM/2.0
               IWORK2(100+N,9) = IWORK2(100+N,9) + 1
C------------- Increment number of calm samples during current hour, NUMCALM
               NUMCALM(N) = NUMCALM(N) + 1
C------------- Set WD and turbulence values to missing for this level
               WORK2(100+N,8) = OSQA(22,2)
               WORK2(100+N,2) = OSQA(16,2)
               WORK2(100+N,3) = OSQA(17,2)
               WORK2(100+N,4) = OSQA(18,2)
               WORK2(100+N,5) = OSQA(19,2)
               WORK2(100+N,6) = OSQA(20,2)

            ELSE
C------------- Wind speed is equal to or above threshold
               OSVOBS(1,N,9) = OSVOBS(1,N,9) + WORK2(100+N,9)
               IWORK2(100+N,9) = IWORK2(100+N,9) + 1
            ENDIF
         ENDIF

C------- Next process wind direction

         IF( ABS(WORK2(100+N,8)-OSQA(22,2)) .LT. EPS )THEN
C---------- Wind direction is missing
            CONTINUE

         ELSE
            IF( ABS(OSVOBS(1,N,8)-OSQA(22,2)) .LT. EPS .AND.
     &            IWORK2(100+N,8) .LE. 1 )THEN
C------------- OSVOBS value is missing; reset to 0.0 if .LE. 1
C              value for this hour
               OSVOBS(1,N,8) = 0.0
            ENDIF

            IF( IWORK2(100+N,8) .EQ. 0 )THEN
C------------- First value for hour treatment
               OSVOBS(1,N,8) = WORK2(100+N,8)
               IWORK2(100+N,8) = 1
               WORK1(100+N) = WORK2(100+N,8)

            ELSE
C------------- Another value for the hour encountered;
C              the following procedure is necessary to account for
C              any cross-over at 360 degrees

               XRD3 = WORK2(100+N,8) - WORK1(100+N)
               IF(XRD3 .LT. -180.0) THEN
                  XRD3 = XRD3 + 360.0
               ELSEIF(XRD3 .GT. 180.0 )THEN
                  XRD3 = XRD3 - 360.0
               ENDIF

C------------- Sum the wind direction (OSVOBS) and
C              increment counter

               WORK1(100+N) = WORK1(100+N) + XRD3
               OSVOBS(1,N,8) = OSVOBS(1,N,8) + WORK1(100+N)
               IWORK2(100+N,8) = IWORK2(100+N,8) + 1
            ENDIF

         ENDIF

C------- Finally process all the rest, skipping WD (#22) and WS (#23)

         DO L = 15,29
            IF( L.EQ.22 .OR. L.EQ.23 ) CYCLE

            IF( ABS(WORK2(100+N,L-14)-OSQA(L,2)) .LT. EPS )THEN
               CYCLE

            ELSE
               IF( ABS(OSVOBS(1,N,L-14)-OSQA(L,2)) .LT. EPS .AND.
     &               IWORK2(100+N,L-14) .LE. 1 )THEN
C ---             If I=15 (for HTnn), skip setting value to 0.0
                  IF( L.GT.15 ) OSVOBS(1,N,L-14) = 0.0
               ENDIF

               IF( L .GE. 16 .AND.  L .LE. 20 )THEN
C---------------- Accumulate sums of the squared value for
C                 standard deviations
                  OSVOBS(1,N,L-14) = OSVOBS(1,N,L-14) +
     &                           WORK2(100+N,L-14)*WORK2(100+N,L-14)
                  IWORK2(100+N,L-14) = IWORK2(100+N,L-14) + 1

               ELSE
                  OSVOBS(1,N,L-14) = OSVOBS(1,N,L-14) +
     &                              WORK2(100+N,L-14)
                  IWORK2(100+N,L-14) = IWORK2(100+N,L-14) + 1
               ENDIF

            ENDIF

         ENDDO
      ENDDO

      RETURN
      END

