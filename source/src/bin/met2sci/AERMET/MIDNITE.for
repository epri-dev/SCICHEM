      SUBROUTINE MIDNITE(KTIM,KMIN)
C=====================================================================**
C          MIDNITE Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the number of minutes past midnight.
C
C     Calling Arguments:
C        KTIM      Input     Integer   Time in hours (HHMM)
C        KMIN      Output    Integer   Time in minutes past midnight
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        <none>
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER KTIM, KMIN
C
C---- Compute minutes from midnight from HHMM (I.E. 100, 200 ETC.)

      KMIN= KTIM/100*60+(KTIM-KTIM/100*100)
      RETURN
      END

