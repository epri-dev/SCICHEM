      SUBROUTINE GETFLD( IFC, ALINE, FIELD )
C=====================================================================**
C        GETFLD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE: Gets Contents of Fields on the second SAMSON header record
C
C     DATE:    April 28, 1995
C
C     INPUTS:  Fields on a line
C
C     OUTPUTS: Contents of Fields on Card
C
C     CALLED FROM: SETSAM
C
C-----------------------------------------------------------------------

C     Variable Declarations

      IMPLICIT NONE
      
      INTEGER       I, J, IFC, LLEN
      CHARACTER*(*) ALINE
      character*1   aline1(256)
      CHARACTER*40  FIELD(IFC)
      INCLUDE 'MAIN2.INC'

      llen = ic2(ifc)
      do i=1,llen
         aline1(i) = aline(i:i)
      enddo

      DO I = 1, IFC
         IF (IC2(I)-IC1(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters

            WRITE(FIELD(I),9004) (ALINE1(J),J=IC1(I),IC2(I))
         ELSE
            WRITE(FIELD(I),9004) (ALINE1(J),J=IC1(I),IC2(I))
         ENDIF
      ENDDO

 9004 FORMAT(40(A1:))

      RETURN
      END

