      SUBROUTINE SAMWX( ATEMP, IWX )
C=====================================================================**
C     SAMWX  Module of the AERMET Meteorological Pre-processor
C
C     PURPOSE:    Interprets the present weather character varaible and
C                 returns with codes for liquid and frozen precipitation
C
C     INPUTS:     9-character present weather code for the hour
C
C     OUTPUTS:    Liquid and frozen precipitation codes
C
C     CALLED FROM: RDSAMS
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C       <none>
C-----------------------------------------------------------------------
C
C---- Variable Declarations

      IMPLICIT NONE
      
      CHARACTER*9  ATEMP
      CHARACTER*1  IP
      INTEGER      ILIQ, IFRZN, IWX, ICODE, NUMIP, IPCODE

      IWX   = 0
      ILIQ  = 0
      IFRZN = 0
      
      DO ICODE = 6,2,-1
         IP = ATEMP(ICODE:ICODE)
         IF( IP .EQ. '9' )THEN
            CYCLE
         ELSE
            READ( IP,100 ) NUMIP
            IPCODE = ICODE*10 + NUMIP
         ENDIF

         IF( IPCODE .Le. 39 )THEN
            ILIQ  = 1
         ELSE
            IFRZN = 2
         ENDIF
      ENDDO

      IWX = ILIQ + IFRZN
      
      RETURN
  100 FORMAT(I1)

      END

