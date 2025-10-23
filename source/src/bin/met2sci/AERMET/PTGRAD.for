      SUBROUTINE PTGRAD ( NUM, NLEV, SDGTOP, ZI, DTHDZ )
C=====================================================================**
C          PTGRAD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  To compute the vertical potential temperature gradient
C               above the convective mixing height (ZI) in a layer from
C               ZI to ZI+DELZ (meters); the larger of the computed value
C               and a minimum default value is returned.
C
C     Called by:  MPPBL
C
C     Arguments:
C        NUM       Integer   Input     Sounding number for the day
C        NLEV      Integer   Input     Number of levels in the sounding
C        SDGTOP    Real      Input     The height of the top of the
C                                      UN-extended sounding
C        ZI        Real      Input     Convective mixing height,
C                                      computed in MPPBL
C        DTHDZ     Real      Output    Vertical potential temperature
C                                      gradient over the layer ZI to
C                                      ZI+DELZ (DELZ defined below)
C
C     Other I/O:
C
C
C     Assumptions:
C        - The height and potential temperature profiles contain no
C          missing data
C
C     Initial release:  June 1993
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        10/31/96 (PES)
C          - check is made to determine if a 500 m layer or 250 m layer
C            is used in the computation or if a default value should be
C            used; counters and accumulators added for statistics on
C            the vptg; computation moved to (new) subr.COMPDT
C
C-----------------------------------------------------------------------

C---- Variable declarations

      IMPLICIT NONE
      

      INTEGER      NLEV, NUM
      REAL         DTHDZ, ZI, DELZ, DTHETA, SDGTOP

      REAL, PARAMETER :: VPTGDY=0.005, DTHMIN = 0.005

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MP2.INC'

C---- Data Initialization

C        DELZ      Real      The thickness of the layer over which the
C                            gradient is to be computed
C        VPTGDY    Real      Default daytime vertical potential temper-
C                            ature gradient
C        DTHMIN    Real      Minimum computed vptg

      DTHDZ = 0.0


C---- Check the location of the mixing height relative to the top
C     of the sounding

      IF( (SDGTOP - ZI) .GT. 250.0 )THEN
C------- The sounding is high enough to compute the vptg   ---- CALL COMPDT
         DELZ = MIN( 500.0, (SDGTOP-ZI-0.1) )
         CALL COMPDT( NUM, NLEV, ZI, DELZ, DTHETA )

C------- The computed value, DTHETA, must be at least DTHMIN
         DTHDZ = MAX( DTHETA, DTHMIN )

C------- Keep stats
         N500 = N500 + 1
         DT500 = DT500 + DTHDZ

      ELSE
C------- The sounding is not sufficiently high to confidently compute a
C        vptg; use the default value.

         DTHDZ = VPTGDY
         N000 = N000 + 1

      ENDIF

      RETURN
      END

