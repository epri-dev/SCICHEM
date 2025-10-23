      SUBROUTINE CUBIC( A,B,C,Z )
C=====================================================================**
C     CUBIC Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To solve a cubic equation to get the proper solution for 
C               the friction velocity (UST), where the cubic equation in
C               this case looks similar to: Z**3 + A*Z**2 + B*Z + C = 0.
C
C     Calling Arguments:
C        A         Input     Real      Coefficient of UST**2
C        B         Input     Real      Coefficient of UST**1
C        C         Input     Real      Coefficient of UST**0
C        Z         Output    Real      New friction velocity
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:

C        04/22/03  (DTB)
C          - Replaced 'GOTO' program structure with 'IF/THEN/ELSE' code.
C

C-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION, intent(in)  :: A,B,C
      REAL, intent(out) :: Z
      
      DOUBLE PRECISION CM,SGN,A3,AP,BP,AP3,TROOT,BP2,APP,BSV
      DOUBLE PRECISION ALPHA,TR,BPP,ONE, TEST

      DATA ONE/1.00D0/

      A3  = A/3.0D0
      AP  = B-A*A3
      BP  = 2.0D0*A3**3-A3*B+C
      AP3 = AP/3.0D0
      BP2 = BP/2.0D0
      TROOT = BP2*BP2+AP3*AP3*AP3

      IF(TROOT .GT. 0.0D0)THEN                                            ! dtb301 03112

         TR   = DSQRT(TROOT)
         TEST = -BP2 + TR
         BSV  = -BP2 - TR

         IF(TEST .LT. 00D0)THEN                                         ! dtb301 03112
            Z = -9.                                                     ! rwb400 04205
            RETURN
         ELSE                                                           ! dtb301 03112
            APP = TEST**0.3333330D0
         ENDIF                                                          ! dtb301 03112

         IF(BSV .NE. 0.00D0)THEN                                           ! dtb301 03112
            SGN = DSIGN(ONE,BSV)
            BPP = SGN*(DABS(BSV))**0.3333330D0
            Z   = SNGL( APP+BPP-A3 )
            RETURN
         ELSE                                                           ! dtb301 03112
            Z = SNGL( APP-A3 )
            RETURN
         ENDIF                                                          ! dtb301 03112

      ELSE                                                              ! dtb301 03112
         CM    = 2.0D0*DSQRT(-AP3)
         ALPHA = DACOS(BP/(AP3*CM))/3.0D0
         Z     = SNGL( CM*DCOS(ALPHA)-A3 )
         RETURN
      ENDIF                                                             ! dtb301 03112

      END
      
