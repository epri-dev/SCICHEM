      SUBROUTINE SMTHZI( IHR,ZIM,USTAR,ZISMTH )
C=====================================================================**
C     SMTHZI Module of the AERMET Meteorological Preprocessor
C
C     Purpose: Computes a smoothed boundary layer height based on the
C              mechanical mixing height (from subr.SBLHT) for all 24
C              hours of the day
C
C     Called by:  MPPBL
C
C
C     Input:   Current hour's value of mechanical boundary layer height;
C              previous hour's smoothed boundary layer height; and
C              current hour's friction velocity
C
C     Output:  The smoothed boundary layer height if it can be
C              computed; otherwise, a missing value
C
C     Assumptions: A missing mixing height forces the smoothing to
C                  restart with the next nonmissing mixing height;
C                  all hourly mechanical mixing heights are smoothed
C
C     Initial release: September 1993
C
C     Developed by: Pacific Environmental Services, Inc. (PES)
C                   Research Triangle Park, NC
C
C     Revision History:
C        10/22/96 (PES)
C          - The subprogram was moved from AERMOD (subr.METEXT) to
C            AERMET
C          - The smooting algorithm was changed to an exponential
C            function in accordance with 10/3/96 discussion with &
C            memo from AERMIC
C
C        02/28/97 (PES)
C          - changed the smoothing constant from 0.3 to 1.0; made the
C            constant a parameter
C
C-----------------------------------------------------------------------
C
C---- Local Variables

      IMPLICIT NONE
      
      REAL, PARAMETER :: BETATAU = 2.0
      REAL    HNPREV, USTAR, ZIM, TAU, XPONEN, ZISMTH
      REAL    EXPLIM, DELTAT                                         ! rwb #523  06341
      INTEGER IHRSAV, IHR

      SAVE HNPREV, IHRSAV

C---- Variable Initializations
      DATA DELTAT/3600.0/, HNPREV/-999.0/, EXPLIM/50.0/              ! rwb #523  06341


      IF( (HNPREV .GE. 0.0) .AND. (ZIM .GT. 0.0) .AND.
     &    (USTAR .NE. -9) .AND. (IHR .EQ. IHRSAV+1) )THEN            ! rwb #523  06341

C------- The previous hour's smoothed PBL height, the current hour's
C        PBL height and USTAR are not missing and an hour has not been
C        skipped due to missing data (i.e., routine not called):
C        compute the current hour's smoothed PBL height

         TAU   =  HNPREV / (BETATAU * USTAR)
         XPONEN = DELTAT / TAU
         IF (XPONEN .GT. EXPLIM) THEN                                ! rwb #523  06341
C-------    Avoid underflow; assign ZISMTH = ZIM                     ! rwb #523  06341
            ZISMTH = ZIM                                             ! rwb #523  06341
         ELSE                                                        ! rwb #523  06341
            ZISMTH = HNPREV*EXP(-XPONEN) + ZIM*(1.0 - EXP(-XPONEN))  ! rwb #523  06341
         ENDIF                                                      ! rwb #523  06341

C------- Save the current values for the next hour's computations
         HNPREV = ZISMTH

      ELSE
C------- One of the required parameters to smooth the mixing height
C        is missing, so the computation cannot be made
         ZISMTH = ZIM
         HNPREV = ZIM

      ENDIF


C---- Save the hour so the routine can check to see if an hour has been
C     skipped the next time this routine is executed
      IF( IHR .NE. 24 )THEN
         IHRSAV = IHR
      ELSE
         IHRSAV = 0
      ENDIF

      RETURN
      END

