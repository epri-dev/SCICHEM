      SUBROUTINE INCRAD(JMPDATE,IHR, ANGLE, RMISS)
C======================================================================
C     INCRAD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the total incoming solar radiation from
C               fractional cloud cover and solar elevation angle.
C
C     Calling Arguments:
C             IHR      In    Integer   In  Hour of day
C
C     Called by:  MPPBL
C
C     Calls to:   none
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
c         05/18/01  (JSI)
c          - added code to check for solar elevation angle less than
c            ArcSin(30/990) = 1.74 degrees
c
c         08/28/01 (PES, RWB)
c          - coverted angle from degrees to radians (ANGLE/RAD2DEG) for
c            SIN argument.
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C-----------------------------------------------------------------------

C     ANGLE  is the elevation angle of the sun in degrees.  The average
c            hourly value is calculated in MPPBL as the mean of the
C            elevation angles for the current and previous hours, since
C            hour is interpreted as the end of the observation period.  ! ! rwb038 01240


C     Data declarations


      IMPLICIT NONE
      
      INTEGER IHR, JMPDATE                                                  ! ! dtb126 02107
      REAL RMISS
      REAL ANGLE, SKY, RAD2DEG                                          ! ! dtb105 02123

      INCLUDE 'WORK1.INC'                                              ! ! dtb126 02107
      INCLUDE 'MP2.INC'

      PATH = 'METPREP '
      LOC  = 'INCRAD'
      RAD2DEG = 57.2958

c     The solar insolation is set to zero for solar elevations          ! ! jsi030 01110
c     less than ArcSin (30/990) = 1.74.                                 ! ! jsi030 01110

      IF(ANGLE .LE. 1.74)THEN                                           ! ! jsi030 01110
         QR(IHR) = 0.0                                                  ! ! jsi030 01110

      ELSEIF(CCVR(IHR) .NE. NO_SKY)THEN          ! Have cloud cover     ! ! dtb126 02107
         QR(IHR)=  990.0*SIN(ANGLE/RAD2DEG) - 30.0           ! Eq. (5)  ! ! rwb038 01240

C        Adjust for cloud cover (CCVR).
         SKY = CCVR(IHR)
         QR(IHR)=QR(IHR)*(1.0 - 0.75*((SKY/10.0)**3.4))    ! Eq. (4)    ! ! dtb105 02123

      ELSE                                       ! Missing cloud cover  ! ! dtb126 02107
         QR(IHR) = RMISS                                                ! ! dtb126 02107

         MESS =  BLNK80                                                 ! ! dtb126 02107
         ECODE='I71'                                                    ! ! dtb126 02107
         WRITE(MESS, 1000) IHR                                          ! ! dtb126 02107
         CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS)                       ! ! dtb126 02107

      ENDIF

c     IF(CCVR(IHR) .EQ. NO_SKY)THEN  ! Missing cloud cover              ! ! dtb122 02096
c        SKY = 0                                                        ! ! dtb105 02123
c     ELSE                                                              ! ! dtb105 02123
c        SKY = CCVR(IHR)                                                ! ! dtb105 02123
c     ENDIF                                                            ! ! dtb105 02123


      RETURN

 1000 FORMAT(' No estimate of insolation, cloud cover missing'
     &       ' for hour:  ',I3)                                         ! ! dtb126 02107
      END

