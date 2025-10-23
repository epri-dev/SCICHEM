      SUBROUTINE GMTLST( JYR,JMO,JDA,JHR,JZONE )
C=====================================================================**
C     GMTLST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Computes Local Standard Time from Greenwich Mean Time
C
C     Called by:    GET620, MERGE
C
C     Calls to:     GREG, JULIAN
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER JULIAN, JYR, JMO, JDA, JZONE, JULHR, JULDA, NDAY
      INTEGER JHR, JULHRC, JDATE
C
C  JYR        = INPUT/OUTPUT YEAR (2 DIGITS)
C  JMO        = INPUT/OUTPUT MONTH
C  JDA        = INPUT/OUTPUT DAY
C  JHR        = INPUT/OUTPUT HOUR
C  JZONE      = INPUT TIME ZONES FROM GREENWICH
C               + ==> WEST LONGITUDE
C               - ==> EAST LONGITUDE
C  JDATE      = TEMPORARY JULIAN DATE IN COMPUTATION
C  JULDA      = JULIAN DATE
C  JULHR      = JULIAN HOUR FROM JULIAN DATE
C  JULHRC     = JULIAN HOUR CORRECTED FROM GMT TO LST
C  NDAY       = NUMBER OF DAYS IN THE YEAR
C
C-----------------------------------------------------------------------
C
C---  Compute Julian day
      JULDA = JULIAN(JYR,JMO,JDA)
C
C---  Compute 'Julian hour'
      JULHR = (JULDA-1)*24 + JHR
C
C---  Subtract the time zone factor to get corrected 'Julian hour'
      JULHRC = JULHR - JZONE
C
C---  Determine the Julian day from the corrected 'Julian hour'
      JDATE = JULHRC/24 + 1
C
C---  Determine the number of days in the year
      IF(MOD(JYR,4) .EQ. 0)THEN
         NDAY = 366
      ELSE
         NDAY = 365
      ENDIF

C---  Compute the year, month, day and hour for the corrected
C---  'Julian hour'
      IF(JULHRC .LT. 0)THEN

         IF(JYR .EQ. 0)THEN                                              ! dtb014 01199
          JYR = 99                                                       ! dtb014 01199
         ELSE                                                            ! dtb014 01199
            JYR   = JYR - 1                                              ! dtb014 01199
         ENDIF                                                           ! dtb014 01199

         JMO   = 12
         JDA   = 31
         JHR   = 24 + JHR - JZONE

      ELSEIF(JDATE .GT. NDAY)THEN
         JDATE = JDATE - NDAY
         JYR   = JYR + 1
         CALL GREG(JYR,JDATE,JMO,JDA)
         JHR   = MOD(JULHRC,24)

      ELSE
         JYR   = JYR
         CALL GREG(JYR,JDATE,JMO,JDA)
         JHR   = MOD(JULHRC,24)

      ENDIF
C
      RETURN
      END

