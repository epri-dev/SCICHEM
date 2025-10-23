      SUBROUTINE NETRAD(JMPDATE,IHR)
C=====================================================================**
C     NETRAD Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To compute the net radiation (RN) from solar insolation,
C              albedo, cloud cover, and temperature.
C
C     Calling Arguments:
C             IHR      In    INTEGER    HOUR OF DAY
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
C
C-----------------------------------------------------------------------


C---- Data declarations


      IMPLICIT NONE
      
      INTEGER IHR, JMPDATE
      REAL C1, C2, C3, STEFB, SKY                                               ! ! dtb105 02123

      INCLUDE 'WORK1.INC'
      INCLUDE 'MP2.INC'

C---- Constants used in the computation
      C1 = 5.31E-13
      C2 = 60.0
      C3 = 1.12
      STEFB = 5.67E-08

      IF(CCVR(IHR) .EQ. NO_SKY)THEN   ! Missing cloud cover             ! ! dtb122 02096
C----     SKY = 5.                                                      ! ! dtb105 02123
          MESS =  BLNK80
          ECODE='E91'
          WRITE(MESS, 1000)IHR
          CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)
 1000     FORMAT(' Error: Missing sky cover for hour ',i2,
     &           ' - logic error should be caught prior to this point')

          RN(IHR) = -999.0
          RETURN

      ELSE                                                              ! ! dtb105 02123
         SKY = FLOAT(CCVR(IHR))                                         ! ! rwb400 04205
      ENDIF                                                             ! ! dtb016 01205


C---- Compute the net radiation from solar insolation, albedo, surface
C     temperature and cloud fraction.

      RN(IHR) = ((1.0-ALBEDO(IHR))*QR(IHR)+C1*(T(IHR)**6)-STEFB*
     &          (T(IHR)**4) + C2*(SKY/10.0))/C3                ! Eq. (2) ! dtb105 02123


      RETURN
      END

