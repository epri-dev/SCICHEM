      SUBROUTINE MODEL( ITEST )
C====================================================================
C          MODEL Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the day's meteorology to formulate the
C               met output for the specified dispersion model.
C
C     ITEST: Status of processing
C          1   errors have occurred
C          2   all OK
C
C     Initial Release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C----------------------------------------------------------------------

C---- Variable declarations


      IMPLICIT NONE
      
      INTEGER ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

      INTERFACE
         SUBROUTINE SUNDAT(fLAT,fLON,TZONE,JULDAY,
     &                      SUNRISE,SUNSET,ELEVANG)
            REAL, INTENT(IN) :: fLAT,fLON
            REAL, INTENT(OUT) :: SUNRISE, SUNSET
            REAL, INTENT(OUT), OPTIONAL :: ELEVANG(1:24)
            INTEGER, INTENT (IN) :: JULDAY, TZONE
         END SUBROUTINE SUNDAT
      END INTERFACE

c---- Data Initialization
      PATH = 'METPREP   '
      LOC  = ' MODEL'
      ITEST = 2

cprt      print *,zone, mpjdy

C---- Compute sunrise and sunset
cprt      print *,st3lat,st3lon,ualst, mpjdy

      CALL SUNDAT(ST3LAT,ST3LON,ZONE,MPJDY,TSR,TSS,ANG)
      
C---- Compute the sunrise and sunset times for the latitude and longitude 
C     asscoiated with the upper air site; the solar elevation the hours 
C     of the day is not computed

cprt      print *,uast3lat,uast3lon,ualst, mpjdy

      CALL SUNDAT(UAST3LAT,UAST3LON,UALST,MPJDY,UASRISE,UASSET)

C---- Estimate the boundary layer parameters
      CALL MPPBL(ITEST)

      RETURN
      END

