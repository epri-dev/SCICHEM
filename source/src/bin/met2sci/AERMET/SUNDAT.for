      SUBROUTINE SUNDAT(fLAT,fLON,TZONE,JULDAY,SUNRISE,SUNSET, ELEVANG)
C=====================================================================**
C           SUNDAT Module of the AERMET Meteorological Preprocessor
C
C      PURPOSE:    Compute sunrise and sunset, and optionally the solar 
C                  elevation angle for all hours of the day
C
C      Revision HIstory:
C         August 2010: Removed use of INCLUDE statements; added logical
C                      to control whether or not to compute the solar 
C                      elevation angle for all hours of the day - added
C                      to compute sunrise and sunset when it is needed 
C                      for locations other than the application site.
C
C-----------------------------------------------------------------------
C     VARIABLE DECLARATIONS

      IMPLICIT NONE

      REAL, INTENT(IN) :: fLAT,fLON
      REAL, INTENT(OUT) :: SUNRISE, SUNSET
      REAL, INTENT(OUT), OPTIONAL :: ELEVANG(1:24)

      REAL :: DAYNO,TDAYNO,SIND,COSD,SINTD,COSTD,SIGMA,
     &        H2,HCOS,HI,ALFSN,DEG_PER_RAD,
     &        AMM,TEMPZ,DUM,SINLAT,COSLAT,DSIN,DCOS

      INTEGER, INTENT (IN) :: JULDAY, TZONE
      INTEGER :: IHOUR
      LOGICAL COMPUTE_ANG

C      INCLUDE 'MP1.INC'
c      INCLUDE 'MP2.INC'

      DEG_PER_RAD = 57.29578
      DUM    = fLON/15.0 - TZONE
      TEMPZ  = 15.0 * TZONE - fLON
      SINLAT = SIN( fLAT/DEG_PER_RAD )
      COSLAT = COS( fLAT/DEG_PER_RAD )

      COMPUTE_ANG = PRESENT(ELEVANG)

C---- Determine the fraction of a year for this date.
C        (0.0172028 = 360.0/365.242*57.29578)

      DAYNO  = (JULDAY - 1.0) * 0.0172028
      TDAYNO = 2.0 * DAYNO
      SIND   = SIN(DAYNO)
      COSD   = COS(DAYNO)
      SINTD  = SIN(TDAYNO)
      COSTD  = COS(TDAYNO)


C---- Account for ellipticity of earth's orbit.

      SIGMA = 279.9348 + (DAYNO*DEG_PER_RAD) + 1.914827*SIND -
     &        0.079525*COSD + 0.019938*SINTD - 0.00162*COSTD


C---- Find the sine of the solar declination.
C        0.39785 = sin(0.4091720193) = sin(23.44383/57.29578)

      DSIN = 0.39785*SIN(SIGMA/DEG_PER_RAD)
      DCOS = SQRT(1.0-DSIN*DSIN)


C---- Determine time(hrs) of meridian passage

      AMM = 12.0 + 0.12357*SIND - 0.004289*COSD + 0.153809*SINTD +
     &      0.060783*COSTD
      HCOS = (-SINLAT*DSIN)/(COSLAT*DCOS)

      IF(ABS(HCOS) .LT. 1.0)THEN 


C----    Determine solar hour angle of sunrise-sunset.
         H2 = (ATAN2(SQRT(1.0-HCOS*HCOS),HCOS)/15.0)*DEG_PER_RAD


C----    Time of sunrise(SUNRISE) and time of sunset(SUNSET) are expressed
C        in local standard time since the zone correction has already 
C        been made.  Otherwise they would be in Greenwich Mean Time.

         SUNRISE = AMM - H2 + DUM
         SUNSET  = AMM + H2 + DUM

      ELSEIF(HCOS .GE. 1.0)THEN                   !  The sun never rises
         SUNRISE = 12
         SUNSET  = 12

      ELSEIF(HCOS .LE. -1.0)THEN                  !  The sun never sets
         SUNRISE = 24
         SUNSET  = 24

      ENDIF


      IF( COMPUTE_ANG )THEN
C----    Determine solar hour angle(in radians) for each hour of the day
         DO IHOUR = 1,24
            HI = (15.0 * (IHOUR-AMM) + TEMPZ) / DEG_PER_RAD
            ALFSN = SINLAT*DSIN + DCOS*COSLAT*COS(HI)

            ELEVANG(IHOUR) = ATAN2(ALFSN,SQRT(1.0-ALFSN*ALFSN))   ! Solar angle (radians)
         ENDDO
      ENDIF
         

      RETURN
      END

