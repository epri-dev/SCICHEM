      SUBROUTINE CBLHT( KK, KSDG, NUMLEV, HCUMUL, PBLHT )
C=====================================================================**
C          CBLHT Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the convective boundary layer height by
C               comparing the cumulative dayteim heat flux to the area
C               under the morning sounding potential temperature profile.
C
C     Called by:  MPPBL
C
C     Arguments:
C        KK        Input     Integer   Time in minutes since midnight
C                                      (AERMET processes cbl mixing
C                                       heights in 15-min increments)
C
C        KSDG      Input     Integer   Sounding for the day used in calcs
C
C        NUMLEV    Input     Integer   Number of levels in the profile,
C                                      including the extra level of the
C                                      sounding was extended to 5000 m
C
C        HCUMUL    Input     Real      Cumulative heat flux up to hour
C
C        PBLHT     Output    Real      Convective boundary layer height
C
C     Other I/O:
C        PTSUM     Input     Real      Summed area under the potential
C                  (in MP2)            temperature profile
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        10/31/96 (PES)
C          - changed the maximum CBL height from 4999.0 to 4000.0 meters
C
C        08/13/10 (MACTEC)
C          - removed SDGTOP as a calling argument since it is not used
C
C        08/27/2012 (EPA/AMEC)
C          - corrected calculation of convective mixing height
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

      INTEGER  KLVL,KSDG,NUMLEV, JJJ, JJHR, KK, JJMIN
      REAL     HCUMUL, PBLHT
      LOGICAL  GOTZIC

C---- Data Initializations

      PATH   = 'METPREP '
      LOC    = ' CBLHT'
      GOTZIC = .FALSE.
      JJJ    = MPYR*10000 + MPCMO*100 + MPCDY
      KLVL   = 2

C     Compute the time in hours and minutes (recall that the CBL height 
C     is computed every 15 minutes, and the value 'on the hour' is 
C     defined as the CBL height for the hour)
      JJHR = KK/60
      JJMIN = MOD(KK,60)

C---- Compare the cumulative heat flux up to the hour being processed
C     to the area under the potential temperature profile at each level
C     until the area is greater than the cumulative heat flux.

      DO WHILE( .NOT. GOTZIC  .AND.  (KLVL .LE. NUMLEV) )

         IF( HCUMUL .LT. PTSUM(KLVL) )THEN

C---------- Compute the cbl height:
C           if there is no change in the area under the potential
C           temperature curve between the level determined above and
C           the level below, then use the lower level as the cbl
C           height; otherwise, interpolate between the two levels.

            IF( PTSUM(KLVL) .EQ. PTSUM(KLVL-1) )THEN
               PBLHT = HT(KSDG,KLVL-1)

            ELSE

C---------     PBLHT computation modified with version 12345 to properly 
C              interpolate between sounding levels that straddle the 
C              convective mixing height, based on work presented by 
C              Dr. Akula Venkatram and Dr. Ken Raynor

               PBLHT = SQRT( HT(KSDG,KLVL-1)**2 + 
     &                 (HCUMUL-PTSUM(KLVL-1)) *
     &                        (HT(KSDG,KLVL)**2-HT(KSDG,KLVL-1)**2) /
     &                               (PTSUM(KLVL)-PTSUM(KLVL-1)) )

            ENDIF
            GOTZIC = .TRUE.

         ELSE
            KLVL = KLVL + 1
         ENDIF
      ENDDO

C---- The cumulative heat flux is such that it exceeds the area under
C     potential temperature profile

      IF( .NOT. GOTZIC )THEN
      
         IF( HCUMUL .GE. PTSUM(NUMLEV) )THEN
C---------- The cumulative heat flux is never less than the area under
C           the profile, set the convective mixing height to the maximum 
 
            PBLHT = HT(KSDG,NUMLEV)
            
C           MESS =  BLNK80
C           ECODE = 'W77'
C           WRITE( MESS,50 ) JJHR, JJMIN
C 50        FORMAT( ' CUMUL H0 > AREA UNDER SDG THETA-PRFL FOR ', 
C    &              I2.2,':',I2.2)
C           CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS)

         ENDIF
 
      ENDIF

      RETURN
      END

