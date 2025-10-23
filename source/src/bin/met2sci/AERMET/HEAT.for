      SUBROUTINE HEAT(IHR)
C=====================================================================**
C          HEAT Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To calculate the heat flux, HFLUX, from the net radiation
C              and Bowen ratio.  The equation for the heat flux is:
C              (CONSTANT*NET RADIATION) / (1+(1/BOWEN RATIO))
C
C     Calling Arguments:
C             IHR      In    INTEGER   HOUR OF DAY
C
C     Called by:  MPPBL
C
C     Calls to:   ---
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      INTEGER IHR
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'

      REAL, PARAMETER :: CSUBG = 0.1


C---- Calculate the HEAT FLUX

      HFLUX(IHR) = ( (1.0-CSUBG) * RN(IHR) )/(1.0+(1.0/BOWEN(IHR)))  ! Eq. (1)

      RETURN
      END

