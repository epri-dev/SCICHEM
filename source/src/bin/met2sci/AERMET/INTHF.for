      SUBROUTINE INTHF(IT,SHEAT,OLDH)
C=====================================================================**
C          INTHF Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This subroutine will interpolate integrated sensible
C               heat flux.  An important assumption to remember is
C               that the integrated heat flux is not allowed to decrease.
C
C     Calling Arguments:
C        IT        Input     Integer   Time in minutes
C        SHEAT     Output    Real      Integrated heat flux
C        OLDH      Output    Real      Previous hour's integrated flux
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        10/26/95 (PES)
C          - Added the check on IT2
C
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INTEGER IT,IT1,IT2
      REAL    SHEAT, OLDH
      INCLUDE 'MP2.INC'

C---- Interpolate time-integrated heat flux, SHEAT.
C     The check on IT2 is in the event the hourly heat flux is upward
C     at the end of the day (as might happen in an urban environment)

      IT1=IT/60
      IT2=IT1+1
      IF(IT1.EQ.0)  IT1= 1
      IF(IT2.EQ.25) IT2=24
      SHEAT=SMH(IT1)+((SMH(IT2)-SMH(IT1))/60.0)*(FLOAT(IT)-IT1*60.0)

C---- Check to make sure the integrated heat flux did not decrease
      IF(SHEAT.LT.OLDH) SHEAT = OLDH
      OLDH = SHEAT
      RETURN
      END

