      SUBROUTINE AERMET_HUMID(DB, DP, RH)
C --- Calculate relative humidity (percent) given the dry-bulb and dew
C     point temperatures. DB is dry bulb temperature in C, and 
C     DP is dew point temperature in C.  Percent relative humidity (RH)
C     is returned.


c     WRITE(6,*) 'Enter the dry bulb and dew point temperatures (C)'
c     WRITE(6,*)
c     READ (5,*)  DB, DP

      IMPLICIT NONE
      
      REAL DB, DP, TTDB, TTDP, VPSAT, VPACT, RH
      REAL DBK, DPK

      DBK = DB + 273.15
      DPK = DP + 273.15

      TTDB = (1.0/273.15) - (1.0/DBK)
      TTDP = (1.0/273.15) - (1.0/DPK)

      VPSAT = 6.11 * EXP(5418.0 * TTDB)
      VPACT = 6.11 * EXP(5418.0 * TTDP)

      RH = 100. * (VPACT/VPSAT)

c     WRITE(6,100)DB, DP,  RH
c 100 FORMAT('   DB = ', F6.2,/'   DP = ', F6.2/'   RH = ', F6.2/)

      RETURN
      END

