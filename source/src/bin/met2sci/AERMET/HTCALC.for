      SUBROUTINE HTCALC(ISTAT)
C=======================================================================
C          HTCALC Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  To recompute the heights in an upper air sounding, but
C             only report how the sounding would be affected without
C             making the changes
C
C   Called by:  UAQASM
C
C   Initial release:  December 15, 1992
C
C   Revision History:
C      04/21/95
C        - Sounding is *not* changed but a message is written if the
C          height change would be greater than 50 m
C      10/25/96
C        - curtailed the number of messages written to the message file
C
C-----------------------------------------------------------------------

C----  Variable declarations


      IMPLICIT NONE
      
      INTEGER ISTAT, I, IYMD
      REAL    E, Q, TV1, TV2

      REAL, PARAMETER :: CLR=2500./0.461, T0=1./302.16, 
     &                   RDG=287.0406/9.80616

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations
      DATA LOC/'HTCALC'/, PATH/'UPPERAIR  '/

C ***  Variables

C  CLR       Constant used in calculation of virtual temp
C  E         Vapor pressure used in calculation of virt temp
C  H         Array containing heights for the sounding
C  I         Loop counter
C  N         Number of levels in sounding
C  P         Array containing pressure values for sounding
C  Q         Specific humidity used in calculating virtual temp
C  RDG       Constant used in height calculation (R Divided by Gravity)
C  T         Array of temperatures for the sounding
C  TD        Array of dew points for the sounding
C  TO        Constant used in calculation of virtual temp
C  TV1       Virtual temperature for level below TV2
C  TV2       Virtual temperature for level above TV1
C  WD        Array containing wind directions for the sounding
C  WS        Array containing wind speeds for the sounding

C.......................................................................

      ISTAT = 0
      CALL FLIWK1

C---- Check for missing surface height

      IF(UAOBS(1,1,2) .EQ. UAQA(2,2)) THEN
         ISTAT = 1
         RETURN
      ELSE
         IWORK1(1) = UAOBS(1,1,2)
      ENDIF


C---- Check for missing surface temperature and dew point at surface
C      REMEMBER: these are integers and some have been multiplied by 10

      IF(UAOBS(1,1,3) .EQ. UAQA(3,2)) THEN
         ISTAT = 1
         RETURN

      ELSEIF( UAOBS(1,1,4) .EQ. UAQA(4,2) )THEN
         ISTAT = 1
         RETURN
      ENDIF


C---- Calculate the virtual temperature for the surface

      E   = EXP(CLR*(T0-1.0/((UAOBS(1,1,4)/10.0) + 273.16))) * 40.0
      Q   = (0.62197*E)/((UAOBS(1,1,1)/10.0) - 0.37803*E)
      TV1 = ((UAOBS(1,1,3)/10.0) + 273.16) * (1.+0.6078*Q)


C---- Loop through the sounding levels aloft

      DO I=2,UALEV(1)

C------- Check for missing temperature, dew point and pressure at
C        the current level and the pressure at the level below

         IF(UAOBS(1,I,3) .EQ. UAQA(3,2)) THEN
            ISTAT = 1
            RETURN

         ELSEIF( UAOBS(1,I,4) .EQ. UAQA(4,2) )THEN
            ISTAT = 1
            RETURN

         ELSEIF( (UAOBS(1,I,1) .EQ. UAQA(1,2)) .OR.
     &            (UAOBS(1,I-1,1) .EQ. UAQA(1,2)) )THEN
            ISTAT = 1
            RETURN
         ENDIF


C------- Calculate the virtual temperature
C        (equations for "Q" and "TV2" were derived from information
C          in the Smithsonian Met. Tables, sixth ed., pp. 295 and 347.)
C        (Equation for "E" obtained from Hess', "Introduction to
C          Theoretical Meteorology" (1959), p. 49.)

         E   = EXP(CLR*(T0-1./((UAOBS(1,I,4)/10.0) + 273.16))) * 40.0
         Q   = (0.62197*E)/((UAOBS(1,I,1)/10.0) - 0.37803*E)
         TV2 = ((UAOBS(1,I,3)/10.0) + 273.16) * (1.+0.6078*Q)


C------- Calculate the height for the current level; retain in work
C        array in case there are problems at a level (see above)

         IWORK1(I) = IWORK1(I-1) + NINT( 0.5*RDG*(TV2+TV1) * ALOG
     &    ((FLOAT(UAOBS(1,I-1,1))/10.0)/(FLOAT(UAOBS(1,I,1))/10.0)) )


C------- Save the virtual temperature for the next calculation

         TV1 = TV2

      ENDDO


C---- Successful at all levels; swap heights from work array back to
C     UAOBS array and return
C     AERMET now only checks the differences and reports if there are
C     large discrepancies between the sounding height and the calculated
C     height (4/21/95)

      IYMD = UAGYR*10000 + UAGMO*100 + UAGDY
      DO I = 2,UALEV(1)
         IF( ABS( UAOBS(1,I,2) - IWORK1(I) ) .GT. 50.0 )THEN
            MESS =  BLNK80
            WRITE(MESS,5060) I, UAGHR
            CALL ERRHDL(IYMD,PATH,'Q35',LOC,MESS)
         ENDIF
      ENDDO

 5060 FORMAT(' |CALC-OBS| SDG HTS > 50m: LVL ',i3,
     &       ', HR: ',I2.2)

      RETURN
      END

