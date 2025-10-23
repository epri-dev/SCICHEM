      SUBROUTINE YR4TOYR2(iYR4,iYR2)
      
C-----------------------------------------------------------------------
C     YR4TO2YR
C     
C     Convert 4-digit year to 2-digit year
C 
C     Input: 4-digit year
C     Output: 2-digit year
C
C     1/2010, MACTEC
C-----------------------------------------------------------------------
       
      IMPLICIT NONE
       
c     Output
      INTEGER, intent(in)  :: iYR4   ! 4-digit year

c     Input       
      INTEGER, intent(out) :: iYR2   ! 2-digit year
       
      IF(iYR4 .LT. 2000)THEN
         iYR2 = iYR4-1900
      ELSE
         iYR2 = iYR4-2000
      ENDIF
       
      RETURN
       
      END SUBROUTINE YR4TOYR2
