      SUBROUTINE D028LV(CARD,ISTAT,JJJ)
C=====================================================================**
C        D028LV Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decode the SCRAM-format NWS surface data and put it
C               into the required format.
C
C     Called by: GETSFC
C
C     Arguments:
C       CARD     Input   Card image with surface data (just 28 characters
C                                                      for SCRAM data)
C       ISTAT    Output  Program status of the decode
C       IOST20   Output  Operating system status of the decode
C
C     Revision history:
C       11/30/94 (PES)
C         - Made into a separate subroutine from an ENTRY point
C
C       04/30/08 (MACTEC)
C         - Added code to compute station pressure from sea level
C           pressure and station elevation or just station elevation
C           using the standard atmosphere
C
C       1/2010 (MACTEC)
C         - For records on or after ASOS commissioning date,  
C           set total and opaque sky cover to missing
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER, PARAMETER :: NUMOVR=5
      INTEGER IABSNT, ICEIL, ISUMTT, ISUMOP, IOST20
      INTEGER IDRYT, ITEST, I, J, JJJ, ISTAT
      INTEGER, SAVE :: IASOSCNT
      REAL    WDIR, WSP, DRYTMP
      CHARACTER CARD*28
      CHARACTER*1 OVRPCH(NUMOVR),OVR11(10),OVR12(10),OVRNOR(10)
      CHARACTER*2 AWDIR,ATOT,AOPQ
      CHARACTER*3 AWSP,ADRYTMP,ACLG

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations
      DATA  OVR11/'[','A','B','C','D','E','F','G','H','I'/,
     &      OVR12/']','J','K','L','M','N','O','P','Q','R'/,
     &     OVRNOR/'0','1','2','3','4','5','6','7','8','9'/
      DATA  IABSNT/-9999/
      DATA  IASOSCNT/0/

      PATH = 'SURFACE   '
      LOC  = 'D028LV'

C----------------------------------------------------------------------
C     Read SCRAM record
      READ(CARD,1001,ERR=20001,IOSTAT=IOST20) SFGHR, ACLG,
     &                              AWDIR, AWSP, ADRYTMP, ATOT, AOPQ
 1001 FORMAT (11X,I2,A3,A2,2A3,2A2)

C---- Process CLG, TOT, and OPQ as overpunch fields
C            CLG=OVRPCH(I),I=1,3
C            TOT=OVRPCH(4)
C            OPQ=OVRPCH(5)
C     Translate SCRAM representation of CLG,TOT,OPQ to CD144 representation;
C     fill missing OPQ following procedures used in MET144 program.

C---- Ceiling:
      OVRPCH(1) = ACLG(1:1)
      OVRPCH(2) = ACLG(2:2)
      OVRPCH(3) = ACLG(3:3)

C---- Opaque cover:
      IF( AOPQ .EQ. '  ' )THEN
         IF( ATOT .NE. '  ' )THEN
            IF( ATOT .EQ. '10' )THEN
               AOPQ = '--'
            ELSE
               AOPQ = ATOT
            ENDIF
         ELSEIF( ACLG .EQ. '   ' )THEN
            AOPQ = '  '
         ELSEIF( ACLG .EQ. '---' )THEN
            AOPQ = '00'
         ELSEIF( ACLG .LT. '070' )THEN
            AOPQ = '07'
         ELSE
            AOPQ = '00'
         ENDIF
      ELSEIF( AOPQ .EQ. '10' )THEN
         AOPQ='--'
      ENDIF
      OVRPCH(5) = AOPQ(2:2)

C---- Total cover:
      IF( ATOT .EQ. '10' ) ATOT='--'
      OVRPCH(4) = ATOT(2:2)

C---- Process each overpunch and decode its meaning and make
C     appropriate transformation; if they are actually overpunched
C     (making them alphabetic characters), add 10 to more easily
C     identify them.
C     The flag for 'X' or '-' is -8888 (which is interpreted later)

      DO I = 1,NUMOVR
         SFOVR(I)=-99999
         IF( OVRPCH(I).EQ.'X'.OR.OVRPCH(I).EQ.'-' )THEN
            SFOVR(I)=-8888
         ELSEIF( OVRPCH(I).EQ.' ' )THEN
            SFOVR(I)=IABSNT
         ELSE
            DO J=0,9
               IF( OVRPCH(I).EQ.OVRNOR(J+1) )THEN
                  SFOVR(I)=J
                  EXIT
               ELSEIF( OVRPCH(I).EQ.OVR11(J+1) )THEN
                  SFOVR(I)=J+10
                  EXIT
               ELSEIF( OVRPCH(I).EQ.OVR12(J+1) )THEN
                  SFOVR(I)=J+10
                  EXIT
               ENDIF
            ENDDO
         ENDIF

C------- Check to make sure a proper overpunch character was found

         IF( SFOVR(I).EQ.-99999 )THEN
            MESS =  BLNK80
            WRITE(MESS,1390) I,SFGHR
 1390       FORMAT(' OVERPUNCH # ',I2,' not properly decoded for HR ',
     &              I2.2 )
            CALL ERRHDL(JJJ,PATH,'W43',LOC,MESS)
         ENDIF
      ENDDO

C---- Convert selected "blank" overpunches to "zero" -- we assume that
C     such blanks are intentional, since missing values will be caught
C     by checking for blanks in the data fields.

C---- Ceiling height:
       ITEST=SFOVR(1)+SFOVR(2)+SFOVR(3)
       IF( ITEST .NE. (3*IABSNT) )THEN
          DO I=1,3
             IF( SFOVR(I) .EQ. IABSNT ) SFOVR(I)=0
          ENDDO
       ENDIF

C-----------------------------------------------------------------------
C     Process the variables in the order of the SFOBS array

C---- Precipitation, sea level pressure, and station pressure are not
C     available from SCRAM data
      SFOBS(1,30) = SFQA(30,2)
      SFOBS(1,31) = SFQA(31,2)
      SFOBS(1,32) = SFQA(32,2)

C---- Ceiling height (km & tenths) - ( 300 (30.0 km) ==> unlimited)
      CALL CLHT(1,ICEIL,IABSNT,SFQA(33,2))
      SFOBS(1,33) = ICEIL

C---- Concatenate total and opaque sky cover (tenths):

c     IF SCRAM and ASOS record, data was reformatted, 
c     set cloud cover to missing
      IF( INDEX(SFFMT,'SCRAM') .NE. 0 .AND. ISASOS1 .EQ. 'A' )THEN
         ISUMTT = SFQA1(34,1)                                     
         ISUMOP = SFQA1(34,2)                                     
         IASOSCNT = IASOSCNT + 1
         IF( IASOSCNT .LE. 24 )THEN
            MESS =  BLNK80                                           
            WRITE(MESS,1392) SFGHR                                   
            CALL ERRHDL(JJJ,PATH,'W41',LOC,MESS)
         ELSEIF( IASOSCNT .GT. 24 )THEN
            MESS =  BLNK80                                           
            WRITE(MESS,1392) SFGHR                                   
            CALL ERRHDL(JJJ,PATH,'I41',LOC,MESS)
         ENDIF
         IF( IASOSCNT .EQ. 24 )THEN
            MESS =  BLNK80                                           
            WRITE(MESS,1393)                                    
            CALL ERRHDL(JJJ,PATH,'W41',LOC,MESS)
         ENDIF
      ELSE   
         CALL CVG(4,ISUMTT,IABSNT,SFQA1(34,1))
         CALL CVG(5,ISUMOP,IABSNT,SFQA1(34,2))
      ENDIF
                      
 1392 FORMAT(' Cloud cover set to missing for reformatted ASOS HR ',I2)
 1393 FORMAT('  NOTE: Additional messages regarding ASOS clouds ',
     &         'for reformatted data included in message file.')

      
      SFOBS(1,34) = ISUMTT*100 + ISUMOP


c     Variables 35-44 are not avaiable in the SCRAM archive


C---- Present weather is not available from SCRAM data

      SFOBS(1,42) = 0                                                   ! dtb #120 02064

C---- Horizontal visibility is not available from SCRAM data
      SFOBS(1,45) = SFQA(45,2)

C---- Dry bulb temperature (deg C and tenths)
      IF( ADRYTMP .EQ. '   ' )THEN
         SFOBS(1,46) = SFQA(46,2)

      ELSE
         READ(ADRYTMP,1002) IDRYT
 1002    FORMAT(I3)
         DRYTMP = FLOAT(IDRYT)
         CALL P2MCEN(DRYTMP,XRD2)
         SFOBS(1,46) = NINT(XRD2 *10.0)

      ENDIF

C---- Wet bulb, dew point, and rel humidity missing are not available
C     from SCRAM data
      SFOBS(1,47) = SFQA(47,2)
      SFOBS(1,48) = SFQA(48,2)
      SFOBS(1,49) = SFQA(49,2)

C---- Wind direction (tens of degrees from north)
      IF( AWDIR .EQ. '  ' )THEN
         SFOBS(1,50) = SFQA(50,2)

      ELSE
         READ(AWDIR,1003) WDIR
 1003    FORMAT(F2.0)
         SFOBS(1,50) = NINT( WDIR )

      ENDIF

C---- Wind speed (meters/sec and tenths)
       IF( AWSP .EQ. '   ' )THEN
          SFOBS(1,51) = SFQA(51,2)

       ELSE
          READ(AWSP,1004) WSP
 1004     FORMAT (F3.0)
          CALL P2MMSC(WSP,XRD2)
          SFOBS(1,51) = NINT(XRD2 * 10.0)

       ENDIF

       RETURN

C-----------------------------------------------------------------------
C- Processing continues here if there is an error decoding the string

20001 CONTINUE
      ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,395) SFGHR
 395  FORMAT(' Internal read error for SCRAM data for HR: ',I2)
      CALL ERRHDL(JJJ,PATH,'E43',LOC,MESS)

      RETURN
      END

