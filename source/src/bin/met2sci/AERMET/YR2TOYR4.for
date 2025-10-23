      SUBROUTINE YR2TOYR4(iYR2,iYR4)
      
C-----------------------------------------------------------------------
C     YR2TO4YR
C     
C     Convert 2-digit year to 4-digit year
C 
C     Input: 2-digit year
C     Output: 4-digit year
C
C     1/2010, MACTEC
C-----------------------------------------------------------------------
       
      IMPLICIT NONE

c     Input       
      INTEGER, intent(in)  :: iYR2   ! 2-digit year
       
c     Output
      INTEGER, intent(out) :: iYR4   ! 4-digit year
       
      IF(iYR2 .GE. 50)THEN
         iYR4 = 1900 + iYR2
      ELSE
         iYR4 = 2000 + iYR2
      ENDIF
       
      RETURN
       
      END SUBROUTINE YR2TOYR4
