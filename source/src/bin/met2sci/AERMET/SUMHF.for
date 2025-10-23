      SUBROUTINE SUMHF(IFST,ILST,INITT,JDAY,ITEST)
C=====================================================================**
C     SUMHF Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Determine the begining and ending hours for the summation
C               of heat flux and carry out summation.  We allow the heat
C               the heat flux to flip/flop about zero during the convective
c               day.  Hours with negative (downward) heat flux are considered
c               as zero for the summation process.
c
C
C     Calling Arguments: 
C        IFST      Input     Integer   Beginning hour for summation
C        ILST      Input     Integer   Ending hour for summation
C        INITT     Output    Integer   Number of minutes past midnight
C                                      that the cbl computations begin
C                                      (= hour*60 - 30)
C
C        JDAY                Integer   The 6-digit date for use in messages
C
C        ITEST     Output    Integer   Instructs the calling routine, MPPBL,
c                                      to proceed with or skip convective
c                                      mixing height estimates.
c                                         1 = Skip
c                                         2 = Proceed
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        02/22/02 (EPA/PES)
C          - change ILST to correspond with last CBL hour
C          - linearly interpolate for single hours of missing heat flux during
C            the CBL, except if first CBL hour is missing
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      REAL, PARAMETER :: H0MIN = 0.0001
      INTEGER IFST,ILST,INITT,IIHR,IHR,JTIME,ISTART
      INTEGER MISSED, ITEST, JDAY

      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

      MISSED = -999                                                      ! dtb111 02045
      IFST =   99
      ILST =  -99

      ITEST = 1

C---- Initialize SMH array (sum of heat flux) to 0.0
      SMH(:) = 0.0

C---- IFST is the begining hour for summation of the heat flux; corresponds
c          with the last SBL hour.

C     ILST is the ending hour for the summation of the heat flux; corresponds
c          with the last CBL hour preceeding two or more SBL hours.

c     Check to see if the first convective hour has a valid              ! dtb128 02114
c     (non-missing) heat flux; we require a valid heat flux              ! dtb128 02114
c     for the first convective hour in order to proceed with             ! dtb128 02114
c     the integration of the heat flux.                                  ! dtb128 02114

      DO IHR = 2, 23                                                     ! dtb128 02114
         IF (PBL(IHR) .EQ. 'CBL') THEN   ! This is the first CBL hour    ! dtb128 02114
            IF (HFLUX(IHR)  .NE. MISSED)THEN ! Have a valid heat flux    ! dtb128 02114
               ITEST = 2                                                 ! dtb128 02114
               EXIT                                                      ! dtb128 02114
            ELSE                             ! Missing heat flux         ! dtb128 02114
               ITEST = 1                                                 ! dtb128 02114
               RETURN                                                    ! dtb128 02114
            ENDIF                                                        ! dtb128 02114
         ENDIF                                                          ! dtb128 02114
      ENDDO                                                             ! dtb128 02114
      
           
C---- Check HFLUX for isolated one-hour gaps (missing value = -999);     ! dtb111 02045
C     We flag these gaps for interpolation. The missing value flag,      ! dtb111 02045
C     MFLAG, is assigned as follows:                                     ! dtb111 02045

C               0   Flag has not been set                                ! dtb111 02045
C              -1   Missing do not fill                                  ! dtb111 02045
C               1   Valid, non-missing data                              ! dtb111 02045
C               2   Missing, OK to fill                                  ! dtb111 02045

C---- Set flag for interpolating single hours of missing heat flux.
C     We will only fill missing hours if hour before or hour after has
C     a positive heat flux (i.e., not interpolation for the SBL).

      DO IHR = 2, 23                                                     ! dtb111 02045
         MFLAG(IHR) = 0
         IF( HFLUX(IHR) .EQ. MISSED )THEN               !  Missing
            IF( HFLUX(IHR+1).NE.MISSED .AND. HFLUX(IHR-1).NE.MISSED 
     &   .AND. (HFLUX(IHR+1).GT.0.0 .OR. HFLUX(IHR-1).GT.0.0) )THEN
               MFLAG(IHR) = 2
            ELSE
               MFLAG(IHR) = -1
            ENDIF
         ELSE                                         !  Not missing
            MFLAG(IHR) = 1
         ENDIF
      ENDDO                                                              ! dtb111 02045

c     Fill one-hour gaps in the heat flux array using interpolation.     ! dtb111 02045

      DO IHR = 2, 23                                                     ! dtb111 02045
         IF(MFLAG(IHR) .EQ. 2)THEN
            HFLUX(IHR) = (HFLUX(IHR-1) + HFLUX(IHR+1))/2
         ENDIF
      ENDDO

C---- Set first hour for summation of the heat flux.  IFST corresponds with the
C     last SBL hour (HFLUX .LE. 0.0) since hour is defined as the hour ending
C     at the specified time.

      IHR = 2
      DO WHILE (IFST .GT. 90  .AND.  IHR .LE. 24)
         IF( HFLUX(IHR)   .GT. H0MIN  .AND.
     &       HFLUX(IHR-1) .LT. H0MIN) THEN
            IFST = IHR - 1
            EXIT
         ELSE
            IHR = IHR + 1
         ENDIF
      ENDDO                                                             ! dtb111 02045

C     Set the last hour for the summation of the heat flux.  ILST corresponds
C     to the last CBL hour before two consecutive SBL hours (note, hour is
C     defined as the hour ending at the specified time).  This allows the heat
c     flux to flip/flop without shutting off convection for the day.

      DO WHILE (ILST .LT. -90  .AND. IHR .LE. 24 )
         IF( (HFLUX(IHR)   .LT. H0MIN)  .AND.
     &       (HFLUX(IHR-1) .LT. H0MIN) )THEN
            ILST = IHR - 2
            EXIT
         ELSE
            IHR = IHR + 1
         ENDIF
      ENDDO

      JTIME = (IFST + 1) * 100

C---- Convert to minutes from midnight.                    ---- CALL MIDNITE
      CALL MIDNITE(JTIME,INITT)

c     MIDNIGHT returns with INITT (the time in minutes from midnight
c     when heat flux first becomes positive).  We subtract 30 minutes
c     and return the result to MPPBL where it is used as the start
c     time for the heat flux integration.

      INITT  = INITT-30
      ISTART = IFST + 1
C
C---- Compute the integrated heat flux; convert units from
C     Watts/meter**2 to Joule/meter**2 by multiplying by 3600
C
C
      DO IIHR=1,ILST

         IF( IIHR.EQ.ISTART )THEN
            SMH(IIHR) = HFLUX(IIHR) * 3600.0

         ELSEIF( IIHR.GT.ISTART .AND. HFLUX(IIHR).GE.0. )THEN
            SMH(IIHR) = HFLUX(IIHR) * 3600.0 + SMH(IIHR-1)

         ELSEIF( IIHR.GT.ISTART .AND. HFLUX(IIHR).LE.0 )THEN
C           The heat flux is downward, persist the previous hour's
C           cumulative flux.
            SMH(IIHR) = SMH(IIHR-1)

         ENDIF

      ENDDO

      RETURN
      END

