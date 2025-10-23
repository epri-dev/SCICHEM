      SUBROUTINE UCALCO(IHR)
C=====================================================================**
C          UCALCO Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This subroutine will set up the iteration needed to
C               calculate the friction velocity during unstable
C               atmospheric cases in which HFLUX > 0.0.
C               NOTE: to complete this calculation, HFLUX has already
C               been calculated in SUBR.HEAT, which uses the Holtslag
C               and van Ulden method.
C
C     Input:
C      IHR       Integer   Hour of day
C      WSPD      Real      Wind speed at the reference height ZREF
C      ZREF      Real      Reference wind speed height
C      HFLUX     Real      Sensible heat flux
C      T         Real      Ambient temperature at ZTREF
C      ZO        Real      Roughness length
C      RHO       Real      Density of air
C
C     Output:
C      USTAR     Real      Surface friction velocity
C      MOL       Real      Monin-Obukhov length
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C     Revision history:
C        10/18/96 (PES)
C          - subprogram restructured to eliminate GO TO statements
C
C-----------------------------------------------------------------------

C---- Variable declarations

      IMPLICIT NONE
      
      INTEGER IHR
      REAL PSIZL, PSIZOL, LASTL
      REAL MU, MU0

      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'

C---- Data initialization
      REAL, PARAMETER :: CP=1004.0, PI=3.14159, GRAV=9.80655,
     &                   EPS=0.01, VONK=0.4

C       CP   = specific volume of dry air at constant pressure
C       PI   = 3.14159
C       GRAV = gravitational acceleration (m/sec*sec)
C       VONK = von Karman constant
C       EPS  = convergence criterion (1% here) for Monin-Obukov length


C==== Begin Processing =================================================

C---- Make first guess for iteration initialize PSIZL and PSIZOL to zero.
C     Initialize other parameters:
C        LASTL =  previous value of the Monin-Obukhov length

      PSIZL = 0.0
      PSIZOL = 0.0
      LASTL = 0.0

      IF( WSPD(IHR) .EQ. 0.0 )THEN
C------- Calm conditions for convective boundary layer.
         USTAR(IHR) = 0.0
         MOL(IHR) = 0.0

      ELSE
C------- Set up iteration loop over Monin-Obukhov length (MOL) and
C        friction velocity (USTAR).  The first guess for the
C        iteration is with PSIZL and PSIZOL set equal to zero.
C        The next guess is made by reevaluating PSIZL and PSIZOL.

         USTAR(IHR) = VONK * WSPD(IHR) /
     &                ( ALOG( (ZREF(IHR)) / Z0(IHR) ) -
     &                PSIZL + PSIZOL)                                ! Eq. (6)

         MOL(IHR) = -RHO(IHR) * CP * T(IHR) * (USTAR(IHR)**3) /
     &               ( VONK * GRAV * HFLUX(IHR) )                    ! Eq. (8)

         DO WHILE( ABS(MOL(IHR)-LASTL) .GT. ABS(EPS*MOL(IHR) ) )
            LASTL = MOL(IHR)

C---------- Calculate new MU, MU0, PSIZL and PSIZOL values.

c            MU  = (1.0 - 16.0 *
c     &            ( ZREF(IHR) / MOL(IHR) ) ) **0.25

            MU  = (1.0 - 16.0*(ZREF(IHR)/MOL(IHR)) )**0.25

            MU0 = (1.0 - 16.0  * ( Z0(IHR) / MOL(IHR) ) ) **0.25

            PSIZL = 2.0 * ALOG( (1.0 + MU) / 2.0) +
     &              ALOG( (1.0 + MU * MU) / 2.0) -
     &              2.0 * ATAN(MU) + PI/2.0                          ! Eq. (7)

            PSIZOL = 2.0 * ALOG( (1.0 + MU0) / 2.0) +
     &               ALOG( (1.0 + MU0 * MU0) / 2.0 ) -
     &               2.0 * ATAN(MU0) + PI/2.0                        ! Eq. (7)

C---------- Recompute USTAR and MOL

            USTAR(IHR) = VONK * WSPD(IHR) /
     &                   ( ALOG( ZREF(IHR) / Z0(IHR) ) -
     &                   PSIZL + PSIZOL)                             ! Eq. (6)

            MOL(IHR) = -RHO(IHR) * CP * T(IHR) * (USTAR(IHR)**3) /
     &                  ( VONK * GRAV * HFLUX(IHR) )                 ! Eq. (8)

         ENDDO

      ENDIF

      RETURN
      END

