      SUBROUTINE SBLHT(IHR,USTAR,PBLHT)
C=====================================================================**
C          SBLHT Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To calculate a mechanically-generated mixed layer
C               height, or use an observed value if one is available
C
C     Called by:  MPPBL
C
C     Calling arguments:
C        IHR       Integer  In     Hour of the day
C        USTAR     Real     In     Surface friction velocity
C        PBLHT     Real     Out    Computed stable pbl height
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        11/01/93 (PES)
C          - Added the check to use obseverd SBL height if one is in
C            the onsite data
C
C        03/26/96 (PES)
C          - Modified the algorithm to be a function of only friction
C            velocity: 2300*(friction velocity)^(3/2)
C
C        10/31/96
C          - made USTAR, PBLHT, OSZIHT, OSMISS calling arguments
C
C        01/15/97
C          - moved check for observed mixing height out of subroutine
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      REAL     PBLHT, USTAR
      INTEGER  IHR

      REAL, PARAMETER :: XCONST = 2300.0

      PBLHT = XCONST * (USTAR**1.5)

      RETURN
      END

