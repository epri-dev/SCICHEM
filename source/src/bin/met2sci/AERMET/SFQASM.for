      SUBROUTINE SFQASM (ISTAT)
C=====================================================================**
C        SFQASM Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Assess the quality of the NWS surface data by performing
C               upper and lower bound checks on all the variables
C
C     Called by: SFPATH
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision history:
C       11/30/94 - Removed version date
C                - (by PES)
C
C       07/26/95 - Added logic to QA variable only if SFSAUD = 1
C                  (by PES)
C
C       11/07/96 - modified the messages written for the QA
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
C     INTEGER JULIAN,READHO,IPART1,IPART2,CONCAT(22),IVBL,MISS1,NUM,     ! ! dtb120 02064
      INTEGER JULIAN,READHO,IPART1,IPART2,CONCAT(15),IVBL,MISS1,NUM,     ! ! dtb120 02064
     1        IQA1L,IQA1U,MISS2,IQA2U,IQA2L,QARSLT,SFCALM,SFWSWD,TDGTT,
     2        JJJ, ISAVHR, ISTAT, I, PPTERR

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations

      DATA CONCAT/4*1, 100, 8*1000, 2*100 /                              ! ! dtb120 02064

      PATH = 'SURFACE   '
      LOC  = 'SFQASM'

C-----------------------------------------------------------------------
C *** Variable descriptions
C
C      READHO        = observation # counter
C      CONCAT        = array of multipliers used to separate
C                      concatenated variables
C      NUM           = pathway number, header status
C
C      The following are for the concatenated variables
C        --1 = leading variable; --2 = trailing variable
C      MISS1,MISS2   = Missing value indicators
C      IQA1L,IQA2L   = Lower QA bounds
C      IQA1U,IQA2U   = Upper QA bounds
C      IPART1,IPART2 = Observations 'un'concatenated
C
C-----------------------------------------------------------------------

C---- Initialize counters; set NUM to 3 to tell subr.OTHHDR that this
C     is pathway #3 (surface)

      ISAVHR = 0
      READHO = 0
      SFCALM = 0
      SFWSWD = 0
      TDGTT  = 0
      JJJ = 0
      NUM = 3

C---- Read the file headers; if there is a problem, write a message
C     and do not QA the data

      CALL OTHHDR(PATH,LOC,NUM,DEV21,DEV22,ISTAT)
      IF( ISTAT .EQ. 1 )THEN
         MESS =  BLNK80
         WRITE(MESS,135)
         CALL ERRHDL(0,PATH,'E20',LOC,MESS)
         RETURN
      ENDIF
      WRITE( DEV22, 2200 )
      WRITE( DEV22, 2102 )

   20 CONTINUE
   
      READHO = READHO + 1

C---- Initialize the arrays

      SFGYR = SFQA(54,2)
      SFGMO = SFQA(53,2)
      SFGDY = SFQA(52,2)
      SFGHR = SFQA(55,2)
      SFASOS = 'N'
      CALL FLSFC(1)


C---- Read one record, compute the day in the form 'yymmdd'


      READ (DEV21,2110,END=410,ERR=400) SFGYR,SFGMO,SFGDY,
     &      SFGHR,(SFOBS(1,IVBL),IVBL=30,51),SFGASOS
 2110 FORMAT(1X,4I2.2,4(1X,I5),6(1X,I5.5),/,
     &      8X,5(1X,I5.5),7(1X,I5),2x,A1)

      JJJ = SFGYR*10000 + SFGMO*100 + SFGDY
      ISAVHR = SFGHR

C---- Before writing extracted data to QAOUT file, check for 
C     valid ASOS flag.  This should only be a problem if extracted
C     surface data from old version of AERMET are processed in 
C     Stage 1, rather than raw surface data.
C --- If ASOS flag is not present or is not a valid code (old data 
C     from a previous version), then issue error message and abort 
C     further processing.
      IF( SFGASOS .NE. 'A' .AND. SFGASOS .NE. 'a' .AND.
     &    SFGASOS .NE. 'N' .AND. SFGASOS .NE. 'n' )THEN
         MESS =  BLNK80
         WRITE(MESS,138) SFGASOS
         CALL ERRHDL(JJJ,PATH,'E48',LOC,MESS)
         ISTAT = 1
         RETURN
      ENDIF

C---- Display on the screen the day being processed
      IF( SFGHR .EQ. 1 ) WRITE(*,610 )SFGMO,SFGDY,SFGYR
  610 FORMAT('+  Stage 1: QA''ing surface data for month/day/year ',
     &               3(I2.2,:'/'))


C---- Write the data to the output file without modification

      WRITE (DEV22,2110) SFGYR,SFGMO,SFGDY,
     &                   SFGHR,(SFOBS(1, IVBL),IVBL=30,51),SFGASOS 


C---- Begin the QA; if the value returned in QARSLT is a 2 or 3, then
C     there was a lower/upper bound violation; a value of 1 indicates
C     that the meteorological value was missing

      IWORK1(1210) = JULIAN(SFGYR,SFGMO,SFGDY)*100 + SFGHR


      DO 40 I=30,51
         IF( SFSAUD(I-29) .EQ. 1 )THEN
C---------- Variable numbers 34 through 40 are a concatenation of two    ! ! dtb120 02064
C           variables; the data, missing value indicators and bounds
C           must be separated before QA'ing

            IF( I.GE.34 .AND. I.LE.40 )THEN                               ! ! dtb120 02064

C------------ PART 1

                MISS1  = SFQA(I,2)/CONCAT(I-29)
                IQA1L  = SFQA(I,3)/CONCAT(I-29)
                IQA1U  = SFQA(I,4)/CONCAT(I-29)
                IPART1 = SFOBS(1, I)/CONCAT(I-29)                        ! ! dtb002 01085

                QARSLT = 5
                CALL INTEQA(3,IWORK1(1210),SFQA(I,1),MISS1,IQA1L,
     &                      IQA1U,IPART1,VNAMES(I),
     &                      SFGYR,SFGMO,SFGDY,SFGHR,QARSLT)

                SFAUD1(I-33,QARSLT) = SFAUD1(I-33,QARSLT) + 1

                IF(QARSLT .EQ. 1 .AND. SFSTRA(I-29) .EQ. 0) THEN
                   MESS =  BLNK80
                   WRITE(MESS,360) VNAMES(I),SFGHR
                   CALL ERRHDL(JJJ,PATH,'Q49',LOC,MESS)
                ENDIF

C------------ PART 2

                MISS2  = SFQA(I,2) - MISS1*CONCAT(I-29)
                IQA2L  = SFQA(I,3) - IQA1L*CONCAT(I-29)
                IQA2U  = SFQA(I,4) - IQA1U*CONCAT(I-29)
                IPART2 = SFOBS(1, I) - IPART1*CONCAT(I-29)               ! ! dtb002 01085

                QARSLT = 5
                CALL INTEQA(3,IWORK1(1210),SFQA(I,1),MISS2,IQA2L,
     &                      IQA2U,IPART2,VNAMES(I),
     &                      SFGYR,SFGMO,SFGDY,SFGHR,QARSLT)

                SFAUD2(I-33,QARSLT) = SFAUD2(I-33,QARSLT) + 1

                IF(QARSLT .EQ. 1 .AND. SFSAUD(I-29).EQ.1  .AND.
     &             SFSTRA(I-29) .EQ. 0) THEN
                   MESS =  BLNK80
                   WRITE(MESS,360) VNAMES(I),SFGHR
                   CALL ERRHDL(JJJ,PATH,'Q49',LOC,MESS)
                ENDIF

            ELSE
C------------- The remaining variables are not concatenated;
C              notice that sfaud(5,-) thru sfaud(15,-) are not used here

               QARSLT = 5
               CALL INTEQA(3,IWORK1(1210),SFQA(I,1),SFQA(I,2),
     &                     SFQA(I,3),SFQA(I,4),SFOBS(1, I),              ! ! dtb002 01085
     &                     VNAMES(I),SFGYR,SFGMO,SFGDY,SFGHR,QARSLT)

               SFAUD(I-29,QARSLT) = SFAUD(I-29,QARSLT) + 1

               IF(QARSLT .EQ. 1 .AND. SFSAUD(I-29).EQ.1  .AND.
     &            SFSTRA(I-29) .EQ. 0) THEN
                  MESS =  BLNK80
                  WRITE(MESS,360) VNAMES(I),SFGHR
                  CALL ERRHDL(JJJ,PATH,'Q49',LOC,MESS)
               ENDIF

            ENDIF
         ENDIF                         ! sfsaud = 1
   40 CONTINUE

C---- Check for calm winds (element 51=wind direction, 50=speed)

      IF( SFOBS(1, 51) .EQ. 0 )THEN                                      ! ! dtb002 01085
         IF( SFOBS(1, 50) .EQ. 0 )THEN                                    ! ! dtb002 01085

C---------- Winds are calm
            SFCALM = SFCALM + 1
            MESS =  BLNK80
            WRITE(MESS,320) SFGHR
            CALL ERRHDL(JJJ,PATH,'CLM',LOC,MESS)

         ELSEIF( SFOBS(1, 50) .GT. 0 )THEN                               ! ! dtb002 01085
C---------- Zero wind speed, nonzero wind direction
            SFWSWD = SFWSWD + 1
            MESS =  BLNK80
            WRITE(MESS,330) SFGHR
            CALL ERRHDL(JJJ,PATH,'WDS',LOC,MESS)
         ENDIF
      ENDIF

C---- Check for T > DEWPT (element 48=dew point, 46=temperature)

      IF( (SFOBS(1, 48) .NE. SFQA(48,2)) .AND.                           ! ! dtb002 01085
     &    (SFOBS(1, 46) .NE. SFQA(46,2)) )THEN                           ! ! dtb002 01085
         IF( SFOBS(1, 48) .GT. SFOBS(1, 46) )THEN                        ! ! dtb002 01085

C---------- Dew-point exceeds the dry bulb temperature
            TDGTT = TDGTT + 1
            MESS =  BLNK80
            WRITE(MESS,340) SFGHR
            CALL ERRHDL(JJJ,PATH,'TDT',LOC,MESS)
         ENDIF
      ENDIF

      IF( SFSAUD(30-29).EQ.1 .AND. SFSAUD(42-29).EQ.1 )THEN

C        Compare precipitation amount (if it is not missing) and present
C        weather for consistency (if precip <==> if proper weather).
C        The present weather is stored in element 42, the precip in 30.  ! ! dtb120 02064

         MISS1  = SFQA(42,2)/CONCAT(42-29)                               ! ! dtb120 02064
         IPART1 = SFOBS(1, 42)/CONCAT(42-29)                             ! ! dtb120 02064
         MISS2  = SFQA(42,2) - MISS1*CONCAT(42-29)                       ! ! dtb120 02064
         IPART2 = SFOBS(1, 42) - IPART1*CONCAT(42-29)                    ! ! dtb120 02064

         IF( SFOBS(1, 30) .NE. SFQA(30,2) )THEN                          ! ! dtb002 01085

            IF( SFOBS(1, 30) .GT. 0 )THEN                                ! ! dtb002 01085
                IF( IPART1 .EQ. MISS1  .AND.  IPART2 .EQ. MISS2 )THEN
C                  Precipitation without weather!
                   PPTERR = PPTERR + 1
                   MESS =  BLNK80
                   WRITE(MESS,370) SFGHR
                   CALL ERRHDL(JJJ,PATH,'PPT',LOC,MESS)
                ENDIF

            ELSE
               IF( (IPART1 .GT. 0  .AND. IPART1 .LT. 70)  .OR.
     &              IPART1 .GE. 90 )THEN
C                 Weather without precipitation !
                  PPTERR = PPTERR + 1
                  MESS =  BLNK80
                  WRITE(MESS,380) SFGHR
                  CALL ERRHDL(JJJ,PATH,'PPT',LOC,MESS)
               ENDIF
            ENDIF

         ENDIF

      ENDIF

C---- Process next hourly observation

      GO TO 20


C---- Processing continues here if an EOF or error is encountered

  400 MESS =  BLNK80
      WRITE(MESS,800) ISAVHR
      CALL ERRHDL(JJJ,PATH,'E48',LOC,MESS)
  800 FORMAT(' Error reading hourly obs from SURFACE extract file ',
     &        'after hour ',I3.2)
      ISTAT = 1
      RETURN

  410 MESS =  BLNK80
      WRITE(MESS,810) READHO - 1
      CALL ERRHDL(0,PATH,'I49',LOC,MESS)
  810 FORMAT(' END OF FILE AFTER HOURLY OBS # ',I5)

C---- Write stuff to temporary file to include in summary report
      WRITE(DEV70,601)
      WRITE(DEV70,600) READHO-1,SFCALM,SFWSWD,TDGTT,DISK60
      WRITE(DEV70,601)
      RETURN

C-----------------------------------------------------------------------
  135 FORMAT(' ERROR PROCESSING HEADERS, NO QA')
  138 FORMAT(' Invalid/missing ASOS flag (',A1,') in SURFACE EXTRACT ',
     &        'file; If blank, file is likely from old version.')
  300 FORMAT(80A1)
  320 FORMAT(' CALM WINDS FOR HR ',I2.2)
  330 FORMAT(' WS .EQ. 0, WD .NE. 0 FOR HR ',I2.2)
  340 FORMAT(' DRY BULB .GT. DEW-POINT FOR HR ',I2.2)
  360 FORMAT(1X,A4,' MISSING FOR HR ',I2.2)
  370 FORMAT( ' PRECIP WITHOUT WEATHER FOR HR ', I2.2)
  380 FORMAT( ' WEATHER WITHOUT PRECIP FOR HR ', I2.2)
  600 FORMAT(8X,'In addition, for the ',I5,' hourly obs, ',
     &          'AERMET reports that there are:',//
     &      10X,I5,' CALM WIND CONDITIONS (WS=0, WD=0)'/
     &      10X,I5,' ZERO WIND SPEEDS WITH NONZERO WIND DIRECTIONS'/
     &      10X,I5,' DEW-POINT GREATER THAN DRY BULB TEMPERATURES'//
     &       8X,'The date & time of these occurrences can be found in',/
     &       8X,'the message file',2X,A96,/
     &       8X,'with the qualifiers CLM, WDS, TDT (resp.)'/)
  601 FORMAT('$SFSF$  ')
 2102 FORMAT('*** EOH: END OF SURFACE QA HEADERS')
 2200 FORMAT('*  SF     SURFACE DATA QUALITY ASSESSMENT' )

      END

