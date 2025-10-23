      SUBROUTINE RHOCAL(P1,T1,DEN)
C=====================================================================**
C          RHOCAL Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To calculate the density using the ideal gas law equation
C              The equation for density is:
C                DENSITY = PRESSURE/(GAS CONSTANT*TEMPERATURE)
C
C     Calling Arguments
C             P1       In    REAL     PRESSURE
C             T1       In    REAL     TEMPERATURE
C             DEN      In    REAL     DENSITY (kg/cu. meter)
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
C-----------------------------------------------------------------------

      IMPLICIT NONE
      
      REAL P1,T1,RD,DEN

C---- Compute the density, DEN, from pressure (P1) and temperature (T1)

      RD = 287.04
      DEN = P1*100.0/(RD*T1)

      RETURN
      END

