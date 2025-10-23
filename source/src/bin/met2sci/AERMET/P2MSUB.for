       SUBROUTINE P2MSUB
C=====================================================================**
C        P2MSUB module for the AERMET Meteorological Preprocessor
C
C     Purpose:  To convert data to standard meteorological units;
C               There are several entry points to make these
C               computations
C
C     Called by: SFLEVS
C
C-----------------------------------------------------------------------
C     Local Declarations


      IMPLICIT NONE
      
      REAL      INCHES,MBARS,KNOTS,MTRSEC
      REAL      FAHRN, CENTG, AVG, AN, RH, VRH
      REAL      T, TD, ES, E, B
      INTEGER   IFEET, IMETER, IMILE
      REAL, PARAMETER :: MILBAR=33.8639, FAHR=32., FAHR1=5./9.,
     &                   FEET=.3048
      REAL, PARAMETER :: MPM=1609.0, IABSNT=-9999, ABSENT=-9999.0
      REAL, PARAMETER :: KNTS=.514791
C
C  ABSENT   = MISSING VALUE INDICATOR
C  AN       = VALUE INDICATING IF NONMISSING RH VALUE
C  AVG      = VALUE USED TO DETERMINE IF RH IS IN FRACTION OR %
C  B        = COMPUTATIONAL VARIABLE USED IN CALCULATION OF DEW POINT
C  CENTG    = TEMPERATURE IN DEGREES CENTIGRADE
C  E        = VAPOR PRESSURE
C  ES       = SATURATED VAPOR PRESSURE
C  FAHR     = FREEZING POINT FOR FAHRENHEIT SCALE
C  FAHR1    = CONVERSION FACTOR FOR DEGREES F TO C
C  FAHRN    = TEMPERATURE IN DEGREES FAHRENHEIT
C  FEET     = CONVERSION FOR FEET TO METERS
C  IFEET    = DISTANCE IN FEET
C  IMETER   = DISTANCE IN METERS
C  IMILE    = DISTANCE IN MILES
C  INCHES   = PRESSURE IN INCHES
C  IPCT     = VALUE IN PERCENT
C  KNOTS    = SPEED IN KNOTS
C  KNTS     = CONVERSION FACTOR FOR KNOTS TO METERS/SEC
C  LA1      = INTEGER VALUE TO BE CONVERTED TO PERCENT
C  MBARS    = PRESSURE IN MILLIBARS
C  MILBAR   = CONVERSION FACTOR FROM INCHES TO MILLIBARS
C  MPM      = CONVERSION FACTOR FOR MILES TO METERS (1609 METERS/MI)
C  MTRSEC   = SPEED IN METERS PER SECOND
C  RH       = RELATIVE HUMIDITY
C  T        = TEMPERATURE IN DEGREES C
C  TD       = DEW POINT(DEG C)
C  VRH      = CONVERSION FACTOR TO PUT RH IN PERCENT
C
C=======================================================================
       ENTRY P2MMBR(INCHES,MBARS)
C
C     THIS SUBROUTINE CONVERTS FROM INCHES TO MILLIBARS.
C
           IF(INCHES.LT.0.0) THEN
            MBARS=ABSENT
            GO TO 100
           ENDIF
           MBARS=INCHES*MILBAR
100    RETURN
C
C=======================================================================
       ENTRY P2MCEN(FAHRN,CENTG)
C
C     THIS ENTRY CONVERTS FAHRENHEIT TO CELSIUS
C
           IF(FAHRN.LT.-200.0) THEN
            CENTG=ABSENT
            GO TO 200
           ENDIF
           CENTG=FAHR1*(FAHRN-FAHR)
C
200    RETURN
C
C=======================================================================
       ENTRY P2MMTR(IFEET,IMETER)
C
C     THIS ENTRY CONVERTS FEET TO METERS
C
           IF(IFEET.LT.0) THEN
            IMETER=-9999
            GO TO 300
           ENDIF
           IMETER = INT(FLOAT(IFEET)*FEET)
           IF(IMETER .LT. 0) THEN
           ENDIF
C
300    RETURN
C
C=======================================================================
       ENTRY P2MMSC(KNOTS,MTRSEC)
C
C     THIS ENTRY CONVERTS KNOTS TO METERS PER SEC
C
           IF(KNOTS.LT.0.0) THEN
            MTRSEC=ABSENT
            GO TO 400
           ENDIF
           MTRSEC=KNOTS*KNTS
C
400        RETURN
C
C=======================================================================
       ENTRY P2MDP (T,RH,TD)
C*
C     THIS ENTRY CALCULATES DEWPOINT FROM TEMPERATURE AND
C      RELATIVE HUMIDITY
C*
C     TEMPERATURE IN DEGREES C AND RH MAY
C      BE EXPRESSED AS EITHER A FRACTION OR A PERCENT
C*
       AVG=0.
       AN=0.
       IF(RH.LT.0.0) GO TO 41
       AVG=AVG+RH
       AN=AN+1.
41     IF(AN.GT.0.) AVG=AVG/AN
C
C*  AVG IS USED TO DETERMINE WHETHER RH IS PERCENT OR FRACTION
       IF(AVG.GT.1.) THEN
        VRH=0.01
       ELSE
        VRH=1.
       ENDIF
       IF(T.LT.-200.0 .OR. RH.LT.0.0) THEN
        TD=ABSENT
        GO TO 42
       ELSE
        ES=6.1078*EXP(17.2964*T/(T+237.3))
        E=VRH*RH*ES
        B=ALOG(E/6.1078)
        TD=237.3*B/(17.2964-B)
       ENDIF
42    CONTINUE
      RETURN
C
C=======================================================================
      ENTRY P2MMM(IMILE,IMETER)
C
C    THIS ROUTINE CONVERTS MILES TO METERS
C
       IF(IMILE.LT.0) THEN
         IMETER=ABSENT
       ELSE
         IMETER = INT(FLOAT(IMILE)*MPM + 0.5)
       ENDIF
C
      RETURN
      END

