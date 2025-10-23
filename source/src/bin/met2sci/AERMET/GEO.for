      SUBROUTINE GEO(IOPT, ZMSL, TBAR, SFSP, SLVP)

c     Calculate sea-level pressure given station pressure or
c     calculate station pressure given sea-level pressure.

c     IOPT   1 = calculate sea-level pressure given station pressure
c            2 = calculate station pressure given sea-level pressure

c     ZMSL   Station elevation (m)
c     SFSP   Station pressure (mb)
c     SLVP   Sea-level pressure (mb)
c     TBAR   The mean virtual temperature of the fictitious column of air
c            between sea level and station elevation.


      IMPLICIT NONE
      
      REAL  ZMSL, TBAR, SFSP, SLVP
      REAL  R, G, DENOM
      INTEGER IOPT

      R = 287.04   !  Gas constant for dry air
      G = 9.8      !  Acceleration of gravity

C     WRITE(6,*) 'Enter IOPT [1,2], pressure (mb), T(K), and elevation (m)'
C     WRITE(6,*)
C     READ (5,*)  IOPT, PPPP, TBAR, ZMSL

      DENOM = R*TBAR/G

      IF(IOPT .EQ. 1)THEN   !  calculate sea-level pressure
C        SFSP = PPPP
         SLVP = SFSP * EXP(ZMSL/DENOM)
      ELSEIF(IOPT .EQ. 2)THEN  !  calculate station pressure
C        SLVP = PPPP
         SFSP = SLVP * EXP(-ZMSL/DENOM)
      ENDIF

C     WRITE(6,100)IOPT, ZMSL, TBAR, SLVP, SFSP
C 100 FORMAT('   IOPT = ',I4/'   ZMSL = ',F6.0/'   TBAR = ',F6.1/
C    &       '   SLVP = ',F6.1/'   SFSP = ',F6.1)

      RETURN
      END

