      SUBROUTINE ASOSREC(iRECYR2,iRECMO,iRECDAY,iRECHR,iRECJDAY,iRECZN)
      
C ----------------------------------------------------------------------
C     ASOSREC - ASOS Record
C     
C     Determines if the current surface data record was most likely
C     observed with ASOS instrumentation based on a comparison of the
C     date of the observation and the ASOS commission date, if known.
C
C     If record is determined to be ASOS, increment counter that keeps
C     a running sum of the number of ASOS observations in the data file.
C
C     Input: iRECYR2  - 2-digit year based on LST of observation
C            iRECMO   - 2-digit month of observation (LST)
C            iRECDAY  - 2-digit day of observation (LST)
C            iRECHR   - 2-digit hour of observation (LST)
C            iRECJDAY - Julian day of observation
C            iRECZN   - local time zone of observation
C
C     Assumes date and hour of observation has been converted to LST.
C     Hour 1 of the commission date is the first hour assigned as ASOS.
C     For those formats in which the convention is 0-23 hours, hour 0 
C     is converted to hour 24 of the previous day during stage 2.  For
C     consistency, hour 0 of the commission date is not considered ASOS.
C
C     1/18/2010, MACTEC Engineering and Consulting, Inc.
C ----------------------------------------------------------------------

      IMPLICIT NONE      
      
      INCLUDE 'SF1.INC'

c     Input variables      
      INTEGER, intent(in) :: iRECYR2   ! 2-digit year of observation
      INTEGER, intent(in) :: iRECMO    ! 2-digit month of observation
      INTEGER, intent(in) :: iRECDAY   ! 2-digit hour of observation
      INTEGER, intent(in) :: iRECHR    ! 2-digit hour of observation, 0-24
      INTEGER, intent(in) :: iRECJDAY  ! julian day of observations
      INTEGER, intent(in) :: iRECZN    ! time zone of observation (# hours from GMT, + West longitude 

c     Local variables
      INTEGER  :: iLOCYR2   ! 2-digit year of obs, LST
      INTEGER  :: iLOCYR4   ! 4-digit year of obs, LST
      INTEGER  :: iLOCMO    ! 2-digit month of obs, LST
      INTEGER  :: iLOCDAY   ! 2-digit day of obs, LST
      INTEGER  :: iLOCHR    ! 2-digit hour of obs, LST
      INTEGER  :: iLOCJDAY  ! julian day of obs, LST
      INTEGER  :: iLOCZN    ! time zone of obs, LST  (this should not change, but just in case)
      INTEGER  :: iRECDATE  ! date of observation from surface file, YYYYMMDD
      
      INTEGER  :: iObsTime   ! observation date-time YYYYMMDDHH
      INTEGER  :: iCommTime  ! asos commission date-time YYYYMMDDHH 
      
c     Global variables
c     ISASOS1:     character, asos flag for the current hour (Y or N)
c     NADJASOS:    integer, count of asos observations
c     GotCommDate: logical, got asos commission date for the sfc file
c     iCommDate:   integer, YYYYMMDD, asos commission date for the sfc file
      
c     Initialize variables
      ISASOS1 = 'N' 

c     determine if ASOS obs
      IF( GotCommDate )THEN    

c        set local variables
         iLOCYR2  = iRECYR2
         iLOCDAY  = iRECDAY
         iLOCMO   = iRECMO
         iLOCDAY  = iRECDAY
         iLOCHR   = iRECHR
         iLOCJDAY = iRECJDAY
         iLOCZN   = iRECZN

c        Convert hour 0 to hour 24 of the previous 
         IF( iLOCHR .EQ. 0 )THEN
            CALL HR0024(iLOCYR2,iLOCJDAY,iLOCHR)  
            CALL GREG(iLOCYR2,iLOCJDAY,iLOCMO,iLOCDAY)
         ENDIF

c        Convert to local time if time zone not equal to zero
         IF( iRECZN .NE. 0 )THEN
            CALL GMTLST(iLOCYR2,iLOCMO,iLOCDAY,iLOCHR,iLOCZN)
         ENDIF

c        Convert 2-digit year to 4-digits
         CALL YR2TOYR4(iLOCYR2,iLOCYR4)

c        set observation and commission date-time stamp (YYYYMMDDHH),
c        using 8-byte integer to preserve precision
         iObsTime  = iLOCYR4*1000000 + iLOCMO*10000 + iLOCDAY*100 + 
     &               iLOCHR
         iCommTime = (iCommDate*100) + 1      ! date/time starts at hour 1

         IF( iObsTime .GE. iCommTime )THEN 
            ISASOS1 = 'A'
            NADJASOS = NADJASOS + 1  
         ENDIF

      ENDIF      
      
      
      END SUBROUTINE
