      SUBROUTINE D6201L( NUMLEV,ISTAT )
C=====================================================================**
C          D6201L Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To interpret the levels of data in an upper air sounding
C
C     Called by: GET620
C
C     Arguments:
C        NUMLEV   Output  Number of levels in the sounding
C                         (initialized as 0 in calling program)
C        ISTAT    Output  Status of the read: 0 = good; 1 = error
C
C     Revision history:
C        11/30/94
C          - Corrected an error in saving the temperature in the
C            work array (divide by 10 was *not* necessary)
C          - Missing RH remains missing rather than change to
C            10% as in previous version => dew point not computed
C          - If either height, pressure, temperature or relative
C            humidity is missing, then that level is deleted from
C            the sounding
C
C        5/6/98
C          - Corrected an error that resulted in a subscript out of
C            range if the top of the sounding was below UATOP (the
C            subscript was continaully decremented without first being
C            incremented after reaching the top of the sounding)
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      REAL     PRES(200),HGT(200),TEMP(200),RH(200),WSPD(200),WDIR(200)
      REAL     A, B, ES, E, TETEN
      INTEGER  IOST10,ISTAT,NUMLEV, NVALUE,LSTHGT,J
      LOGICAL  ATTOP

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'


C *** Variable descriptions
C
C     HGT        Height (m)
C     PRES       Pressure (mb)
C     TEMP       Temperature (deg. C
C     RH         Relative humidity (percent)
C     DEWPT      Dew-point (deg. C)
C     WSPD       Wind speed (m/s)
C     WDIR       Wind direction (degrees from north)
C ***

C---- Data Initialization
C     ATTOP must be reset to false for each sounding
      ATTOP = .FALSE.

C---- Read and store all the sounding data from the buffer

      READ(BUFNWS,6201,ERR=10001,IOSTAT=IOST10) NVALUE,
     &     (PRES(J), HGT(J), TEMP(J), RH(J), WDIR(J), WSPD(J),
     &      J = 1, NVALUE)
6201  FORMAT (29X,I3,200(5X,F5.2,F6.0,F4.1,3(F3.0),7X))

      ISTAT = 0
      LSTHGT = NINT(HGT(1))
      NUMLEV = 0

C---- Decode each value at each level
      DO J=1,NVALUE

         IF( ATTOP )THEN
C---------- Return to the calling program if the height exceeds the
C           limit imposed by AERMET (5000 meters)

            RETURN

         ELSE

            NUMLEV = NUMLEV + 1
            IF( NUMLEV .GT. UAML )THEN
C------------- The maximum number of levels was reached on the previous
C              level - decrement the counter for the number of levels
C              and return to the calling program

               NUMLEV = NUMLEV - 1
               RETURN
            ENDIF

c           Check for null record                                        ! dtb003 01128

            IF( PRES(J) .EQ. 0 )THEN                                       ! dtb003 01128
               NUMLEV = NUMLEV - 1                                       ! dtb003 01128
               RETURN                                                    ! dtb003 01128
            ENDIF                                                        ! dtb003 01128

C---------- In the TD-6201 format, there can be consecutive occurrences
C           of the *same* height ("corrections"), with the last
C           occurrence being the correct one.  If there are consecutive
C           records with the same height, do not increment the counter
C           (that was done for the first occurrence of the height),
C           but do work with the upper air variables.

            IF( HGT(J) .NE. LSTHGT )THEN
C------------- Save the height to check for duplicate heights in the
C              sounding

               LSTHGT = NINT(HGT(J))
            ENDIF


C NOTE:  The -9999. values should be retained; they are converted
C        to the UAQA values in the calling routine;
C        work array values should be in whole units, not *10, etc.

C------- Pressure
            IF( PRES(J) .EQ. 999.99 )THEN          !  Missing pressure     ! dtb015 01200
               WORK2(NUMLEV,1) = -9999.0
            ELSE
C              Convert hundreths of kilopascals to millibars
C                 (1 kp = 10 mb, so 1 mb = .1 kp)
               WORK2(NUMLEV,1) = PRES(J) * 10.0
            ENDIF

C------- Height
            IF( HGT(J) .EQ. -99999. )THEN          !  Missing height       ! dtb015 01200
               WORK2(NUMLEV,2) = -9999.0
            ELSE
               WORK2(NUMLEV,2) = HGT(J)
            ENDIF

C------- Temperature
C        (Note: TEMP(J) was read in with F4.1 format; so -999 ==> -99.9;

            IF( TEMP(J) .EQ. -99.9 )THEN           !  Missing temperature  ! dtb015 01200
               WORK2(NUMLEV,3) = -9999.0
            ELSE
C              Temperature was read in tenths of deg C
               WORK2(NUMLEV,3) = TEMP(J)
            ENDIF

C------- Dew-point from relative humidity:
C        If the RH is greater than or equal to 100%, but not missing,
C        then assume it is 99.9;

C        (Note: As noted in the documentation for TD-6201 data, relative humidity
c        is computed w.r. to a water surface for all temperatures, even for T < 0.0)

            IF( RH(J).EQ.999 .OR. TEMP(J).EQ.-99.9 )THEN                   ! dtb015 01200
               WORK2(NUMLEV,4) = -9999.0         !  Missing dew point    ! dtb015 01200
            ELSE

               IF( RH(J) .GE. 100.0 )RH(J) = 99.9                         ! dtb015 01200

               A = 7.5
               B = 237.3
               ES = 6.1078 * (10**(A*TEMP(J) / (TEMP(J) + B)))
               E  = ES * (RH(J)/100.0)
               TETEN  = ALOG10(E/6.1078)
               WORK2(NUMLEV,4) = B*TETEN/(A-TETEN)

            ENDIF

C------- Wind direction
            IF( WDIR(J) .EQ. 999. )THEN            !  Missing WDIR         ! dtb015 01200
               WORK2(NUMLEV,5) = -9999.0
            ELSE
               WORK2(NUMLEV,5) = WDIR(J)
            ENDIF

C------- Wind speed
            IF( WSPD(J)*10. .EQ. 999. )THEN        !  Missing wind speed   ! dtb015 01200
               WORK2(NUMLEV,6) = -9999.0
            ELSE
               WORK2(NUMLEV,6) = WSPD(J)
            ENDIF

C------- If critical data are missing, then skip the level by
C        decrementing the counter so the data are overwritten
C         1 = pressure, 2 = height, 3 = temperature, 4 = dew point
            IF( WORK2(NUMLEV,1) .LT. -9000.0  .OR.
     &          WORK2(NUMLEV,2) .LT. -9000.0  .OR.
     &          WORK2(NUMLEV,3) .LT. -9000.0  .OR.
     &          WORK2(NUMLEV,4) .LT. -9000.0 )THEN
               NUMLEV = NUMLEV - 1

            ELSEIF( HGT(J).GT.UATOP+HGT(1) )THEN
C------------- The sounding height and set the flag to stop processing
C              if the height is above the sounding limit

               ATTOP = .TRUE.
            ENDIF

         ENDIF

      ENDDO
      
      RETURN
C-----------------------------------------------------------------------
C---- Processing continues here if there is an error decoding a level

10001  CONTINUE
       ISTAT = 1
       RETURN
       END

