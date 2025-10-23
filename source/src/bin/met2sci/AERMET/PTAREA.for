      SUBROUTINE PTAREA(IH,ILV,PTMAX)
C=====================================================================**
C          PTAREA Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the area under the potential temperature
C               profile for the interval ILVLS-1 to ILVLS using the
C               trapeziod method.
C
C     Calling Arguments:
C        ILV       Input     Integer   Rawinsonde level
C        PTMAX     Input     Real      Potential temperature
C        IH        Input     Integer   Sounding for the day used in the 
C                                      calculations
C
C     Initial release: December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C        <none>
C
C-----------------------------------------------------------------------

C---- Variable declarations

      IMPLICIT NONE
      
      INTEGER IH,ILV
      REAL PTMAX
      INCLUDE 'MP2.INC'

C---- Does the potential temperature increase with height?
      IF( PTMP(IH,ILV) .LT. PTMAX )THEN

C------- The potential temperature decreased with height
         PTA(ILV) = 0.0
         PTMP(IH,ILV) = PTMAX

      ELSE
C------- Compute the area under the potential temperature profile for the
C        integral from ILVLS-1 to ILVLS using the formula for the area of
C        a trapezoid (0.5 x base x height).
         PTA(ILV) = 0.5 * ( PTMP(IH,ILV)-PTMP(IH,ILV-1) )  *
     &                    ( HT(IH,ILV) + HT(IH,ILV-1) )

C------- Save the maximum potential temperature encountered thus far to
C        check that the values are increasing with height.
         PTMAX = AMAX1(PTMP(IH,ILV),PTMAX)

      ENDIF

      RETURN
      END

