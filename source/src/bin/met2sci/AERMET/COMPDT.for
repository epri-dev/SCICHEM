      SUBROUTINE COMPDT( NUM, NLEV, ZI, DELZ, DTHDZ )
C=====================================================================**
C
C     Purpose:  To compute the daytime vertical potential temperature
C               gradient (vptg) above the mixing height
C
C     Called by:  PTGRAD
C
C     Assumptions:
C
C     Calling Arguments:
C        NUM       Integer   Input     Sounding number for the day
C        NLEV      Integer   Input     3 levels in the unextended sounding
C        ZI        Real      Input     Mixing height
C        DELZ      Real      Input     Depth over which to calculate vptg
C        DTHDZ     Real      Output    vptg
C
C     Other I/O:
C        HT()      Real                Sounding height array
C        PTMP()    Real                Sounding pot. temperature array
C
C     Initial Release:  October 1996
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, intent(in)  :: NUM, NLEV
      REAL, intent(in)     :: ZI, DELZ
      REAL, intent(out)    :: DTHDZ

      INTEGER :: LEVEL
      REAL    :: Z1, Z2, Z3, Z4, THETA1, THETA2, THETA3, THETA4,
     &           PTZI, PTDELZ, ZDELZ

C---- Variable Declarations
      INCLUDE 'MP2.INC'

C---- Data Initializations
      PTZI   = -999.0
      PTDELZ = -999.0
      THETA1 = -999.0
      THETA3 = -999.0
      Z1     = -999.0
      Z3     = -999.0

C     ----------------------------------------------------------
C     Locate the sounding levels immediately below and above ZI;
C     there is always a sounding level at the surface, so there
C     at least one sounding level below ZI
C     ----------------------------------------------------------

      DO LEVEL = 1,NLEV

         IF( ZI .GT. HT(NUM,LEVEL) )THEN
            Z1     = HT(NUM,LEVEL)
            THETA1 = PTMP(NUM,LEVEL)

         ELSE
C---------- The level above and below ZI have been found;
C           interpolate to ZI

            Z2     = HT(NUM,LEVEL)
            THETA2 = PTMP(NUM,LEVEL)
            PTZI   = ( ( ZI - Z1 ) / ( Z2 - Z1 ) ) *
     &               ( THETA2 - THETA1 ) + THETA1
            GO TO 100
         ENDIF

      ENDDO

C---- This point is reached if there wasn't a level below and above ZI
      IF( PTZI .LT. -900.0 )THEN
         PTZI = 0.0
      ENDIF

C     ------------------------------------------------
C     Locate the sounding levels immediately above and
C     below ZI+DELZ meters and interpolate
C     ------------------------------------------------

 100  ZDELZ = ZI + DELZ

      DO LEVEL = 1,NLEV

         IF( ZDELZ .GT. HT(NUM,LEVEL) )THEN
             Z3     = HT(NUM,LEVEL)
             THETA3 = PTMP(NUM,LEVEL)

         ELSE
C---------- The level above and below ZI+ZDELZ have been
C           found; interpolate to ZI+ZDELZ

            Z4     = HT(NUM,LEVEL)
            THETA4 = PTMP(NUM,LEVEL)
            PTDELZ = ( ( ZDELZ - Z3 ) / ( Z4 - Z3 ) ) *
     &               ( THETA4 - THETA3 ) + THETA3
            GO TO 200
         ENDIF

      ENDDO

C---- This point is reached if there wasn't a level below and above ZI+DELZ
      IF( PTDELZ .LT. -900.0 )THEN
         PTDELZ = 0.0
      ENDIF

C---- Compute the vertical potential temperature gradient

  200 DTHDZ  = ( PTDELZ - PTZI ) / ( ZDELZ - ZI )

      RETURN
      END

