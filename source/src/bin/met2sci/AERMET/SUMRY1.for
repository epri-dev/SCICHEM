      SUBROUTINE SUMRY1(ISTAGE)
C=====================================================================**
C          SUMRY1 Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  To write information regarding the anticipated data
C               processing based on the setup information provided by
C               the user.  Status by pathway, file names, etc. are
C               summarized in the report file.
C
C     CALLED BY: MET1N2 (main program)
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C=======================================================================

C---- Data declarations

      IMPLICIT NONE
      
      CHARACTER*3  MONTHS(12)
      CHARACTER*9  DMY1,DMY2
      CHARACTER*16 PATHNM(3)
      CHARACTER*44 MESSAG(-1:8)
      INTEGER      PSTAT(3),IP, DEVICE
      INTEGER      ISTAGE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C---- Data initializations

      DATA PATHNM/'Upper Air Data  ','NWS Surface Data',
     &            'On-site Data    '/
      DATA MESSAG/' NONE, ERROR(S) ON INPUT FOR THIS PATH      ',
     1            ' NONE, NO DATA TO BE PROCESSED ON THIS PATH ',
     2            ' EXTRACT ONLY                               ',
     3            ' QUALITY ASSESSMENT ONLY                    ',
     4            ' EXTRACT AND QUALITY ASSESSMENT             ',
     5            ' MERGE ONLY                                 ',
     6            ' EXTRACT AND MERGE                          ',
     7            ' QA AND MERGE                               ',
     8            ' EXTRACT, QA AND MERGE                      ',
     9            ' THIS RUN ONLY CHECKS THE RUNSTREAM SETUP   '/

      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
C
C-----------------------------------------------------------------------

C---- Put the path statuses in an array; determine what device to write
C     to and write the first page header

      PSTAT(1) = UASTAT
      PSTAT(2) = SFSTAT
      PSTAT(3) = OSSTAT

C---- Check to make sure that the report file is available.
C     if not, use DEVIO, the default output device (screen)

      IF( STATUS(1,1) .EQ. 2 )THEN
         DEVICE = DEV50
      ELSE
         DEVICE = DEVIO
      ENDIF

      PGNUM = PGNUM + 1
      CALL BANNER( PGNUM,ISTAGE,VERSNO,DEVICE )

C---- JOB SETUP: Write message file name or an error message if none
C                was defined; write report file name

C---- Was this just a check on the syntax?  If so, write a message
C     to the summary file and screen (if the summary file is defined)
      IF( STATUS(1,3) .GT. 0 )THEN
         WRITE(DEVICE,5020) MESSAG(8)
         IF( DEVICE .NE. DEVIO) WRITE( DEVIO, 5020 ) MESSAG(8)

      ENDIF

      IF( .NOT.SETERR .AND. .NOT.RUNERR )THEN
C----    No Setup or Runtime errors, but check for 
C        invalid combination of processing options
      
         DO IP = 2,4
            IF( PSTAT(IP-1) .EQ. 5  .OR.  PSTAT(IP-1) .EQ. 6  .OR.
     &          PSTAT(IP-1) .EQ. 7 )THEN
C----          Option not allowed, assign SETERR = .T. and issue error message
               MESS =  BLNK80
               ECODE = 'E05'
               WRITE( MESS, 5000 ) MESSAG(PSTAT(IP-1))
               CALL ERRHDL( 0,BLNK08,ECODE,'SUMRY1  ',MESS )
               SETERR = .TRUE.
               EXIT
            ENDIF
         ENDDO

      ENDIF

C---- Did AERMET finish successfully or unsuccessfully? - write message
C     to the summary file and screen (if summary file is defined)
      IF( JBSTAT .LT. 0  .OR.  UASTAT .LT. 0  .OR.  SFSTAT .LT. 0  .OR.
     &    OSSTAT .LT. 0  .OR.  MRSTAT .LT. 0  .OR.
     &    SETERR .OR. RUNERR )THEN

         WRITE(DEVICE,5210)
         IF( DEVICE .NE. DEVIO ) WRITE(DEVIO,5210)

      ELSE
         WRITE(DEVICE,5220)
         IF( DEVICE .NE. DEVIO ) WRITE(DEVIO,5220)

      ENDIF

C---- Write JOB parameters
      WRITE(DEVICE,5005)
      IF( STATUS(1,2) .EQ. 2)THEN
         WRITE(DEVICE,5010) DISK60
      ELSE
         WRITE(DEVICE,5015)
      ENDIF

      IF( STATUS(1,1) .EQ. 2 )THEN
         WRITE(DEVICE,5011) DISK50
      ELSE
         WRITE(DEVICE,5016) DEVIO
      ENDIF

C---- Results from UA, SF, OS SETUP: determine status; write messages,
C     file names, etc.

      DO IP = 2,4
         WRITE(DEVICE,5025) IP,PATHNM(IP-1)

         IF( PSTAT(IP-1) .EQ. 5  .OR.  PSTAT(IP-1) .EQ. 6  .OR.
     &       PSTAT(IP-1) .EQ. 7 )THEN
C---------- Option not allowed
            WRITE( DEVICE, 5100 ) MESSAG(PSTAT(IP-1))
            CYCLE

         ELSEIF( IP .EQ. 2 )THEN
C---------- Upper Air Observations

            IF( UALOC .NE. BLNK08 )THEN
               WRITE(DEVICE,5049)
               WRITE(DEVICE,5055) UALOC,UALAT,UALON,UALST
               IF( ISTAGE .EQ. 1 ) WRITE(DEVICE,5057) UAFMT
            ENDIF

            WRITE(DEVICE,5028) MESSAG(PSTAT(IP-1))

            IF( STATUS(IP,4) .NE. 0 )THEN
               IF( STATUS(IP,4) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,4) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5030) BUF08(3),UNIT10
            ENDIF

            IF( STATUS(IP,7) .NE. 0 )THEN
               IF( STATUS(IP,7) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,7) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5035) BUF08(3),DISK12
            ENDIF

            IF( STATUS(IP,8) .NE. 0 )THEN
               IF( STATUS(IP,8) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,8) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5040) BUF08(3),DISK13
            ENDIF

            IF( STATUS(IP,4) .GE. 2 )THEN
               IF( STATUS(IP,9) .EQ. 2 )THEN
                  WRITE(DMY1,5080) UAGDY1,MONTHS(UAGMO1),UAYR1
                  WRITE(DMY2,5080) UAGDY2,MONTHS(UAGMO2),UAYR2
                  WRITE(DEVICE,5060) DMY1,DMY2
               ELSEIF( STATUS(IP,9) .EQ. 1 )THEN
                  WRITE(DEVICE,5061)
               ENDIF

               IF( STATUS(IP,11) .EQ. 0 .OR. STATUS(IP,11) .EQ. 2 )THEN
                  WRITE(DEVICE,5065) UATOP
               ELSEIF( STATUS(IP,11) .EQ. 1 )THEN
                  WRITE(DEVICE,5066)
               ENDIF

C              Report on status of automatic sounding checks

               IF( STATUS(IP,12) .EQ. 2 )THEN
                  BUF03 = ' ON'
                  WRITE(DEVICE,5070) BUF03
               ELSEIF( STATUS(IP,12) .EQ. 0 )THEN
                  BUF03 = 'OFF'
                  WRITE(DEVICE,5070) BUF03
               ENDIF
            ENDIF

         ELSEIF( IP .EQ. 3 )THEN
C---------  Hourly Surface Observations
          
            IF( SFLOC .NE. BLNK08 )THEN
               IF( ISTAGE .EQ. 1 .OR. ISTAGE .EQ. 3 )THEN
C                 Include station elevation for Stages 1 and 3 summary report
                  WRITE(DEVICE,5051)
                  WRITE(DEVICE,5056) SFLOC,SFLAT,SFLON,SFLST,PWELEV(3)
               ELSEIF( ISTAGE .EQ. 2 )THEN
C                 Do not include elevation for Stage 2 summary report
                  WRITE(DEVICE,5050)
                  WRITE(DEVICE,5055) SFLOC,SFLAT,SFLON,SFLST
               ENDIF
               IF( ISTAGE .EQ. 1 )THEN
                  IF( STATUS(3,4) .EQ. 0 )THEN
C ----               DATA keyword not processed, indicating that
C                    "pre-extracted" SURFACE data were used, rather 
C                    than raw SURFACE data. Therefore SURFACE data 
C                    format has not beed defined, assign value of
C                    SFFMT = 'EXTRACT'
                     SFFMT = 'EXTRACT'
                     WRITE(DEVICE,5057) SFFMT
                  ELSEIF( .NOT. ISHD_ASOS )THEN
                     WRITE(DEVICE,5057) SFFMT
                  ELSEIF( ISHD_ASOS )THEN
                     WRITE(DEVICE,5058) SFFMT
                  ENDIF
               ENDIF
            ENDIF
          
            WRITE(DEVICE,5028) MESSAG(PSTAT(IP-1))
          
C---------  Check for blank filename, which should mean that call to
C           FLOPEN was bypassed while re-processing the DATA keyword
C           from a header record (only for the SURFACE pathway).
            IF( STATUS(IP,4) .NE. 0 .AND. UNIT20 .NE. BLNK08 )THEN
               IF( STATUS(IP,4) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,4) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5030) BUF08(3),UNIT20
            ENDIF
          
            IF( STATUS(IP,7) .NE. 0 )THEN
               IF( STATUS(IP,7) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,7) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5035) BUF08(3),DISK21
            ENDIF
          
            IF( STATUS(IP,8) .NE. 0 )THEN
               IF( STATUS(IP,8) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,8) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5040) BUF08(3),DISK22
            ENDIF
          
            IF( STATUS(IP,4) .GE. 2 )THEN
               IF( STATUS(IP,9) .EQ. 2 )THEN
                  WRITE(DMY1,5080) SFGDY1,MONTHS(SFGMO1),SFYR1
                  WRITE(DMY2,5080) SFGDY2,MONTHS(SFGMO2),SFYR2
                  WRITE(DEVICE,5060) DMY1,DMY2
               ELSEIF( STATUS(IP,9) .EQ. 1 )THEN
                  WRITE(DEVICE,5061)
               ENDIF
            ENDIF
          
            IF( STATUS(IP,33) .NE. 0 )THEN
C------------- 1-min ASOS wind data
               IF( ISTAGE .EQ. 3 )THEN
C ---             Found 1-min ASOS data in merged file
C                 in Stage 3; set status string to 'USED'
                  BUF08(3) = '    USED'
               ELSEIF( STATUS(IP,33) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,33) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5041) BUF08(3),DISK23
            ENDIF
          
         ELSEIF( IP .EQ. 4 )THEN
C---------- ONSITE data

            IF( OSLOC .NE. BLNK08 )THEN
              IF( ISTAGE .EQ. 1 .OR. ISTAGE .EQ. 3 )THEN
C                Include station elevation for Stages 1 and 3 summary report
                 WRITE(DEVICE,5051)
                 WRITE(DEVICE,5056) OSLOC,OSLAT,OSLON,OSLST,PWELEV(4)
              ELSEIF( ISTAGE .EQ. 2 )THEN
C                Do not include elevation for Stage 2 summary report
                 WRITE(DEVICE,5050)
                 WRITE(DEVICE,5055) OSLOC,OSLAT,OSLON,OSLST
              ENDIF
            ENDIF

            WRITE(DEVICE,5028) MESSAG(PSTAT(IP-1))

            IF( STATUS(IP,4) .NE. 0 )THEN
               IF( STATUS(IP,4) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,4) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5035) BUF08(3),DISK31
            ENDIF

            IF( STATUS(IP,8) .NE. 0 )THEN
               IF( STATUS(IP,8) .GE. 2 )THEN
                  BUF08(3) = '    OPEN'
               ELSEIF( STATUS(IP,8) .EQ. 1 )THEN
                  BUF08(3) = 'NOT OPEN'
               ENDIF
               WRITE(DEVICE,5040) BUF08(3),DISK32
            ENDIF

            IF( STATUS(IP,9) .EQ. 2 )THEN
               WRITE(DMY1,5080) OSGDY1,MONTHS(OSGMO1),OSYR1
               WRITE(DMY2,5080) OSGDY2,MONTHS(OSGMO2),OSYR2
               WRITE(DEVICE,5060) DMY1,DMY2
            ELSEIF( STATUS(IP,9) .EQ. 1 )THEN
               WRITE(DEVICE,5061)
            ENDIF

         ENDIF

      ENDDO

C---- MERGE SETUP

      IF( STATUS(5,22) .NE. 0 )THEN
         IF( STATUS(5,22) .GE. 2 )THEN
            BUF08(3) = '    OPEN'
         ELSEIF( STATUS(5,22) .EQ. 1 )THEN
            BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(DEVICE,5042)
         WRITE(DEVICE,5045) BUF08(3),DISK40
      ENDIF

      RETURN

C-----------------------------------------------------------------------
 5000 FORMAT(  1X,'INVALID COMBINATION OF PROCESSING OPTIONS: ', A44)
 5005 FORMAT( /1X,'1.  Job File Names')
 5010 FORMAT( /4X,' Listing of Messages: ',A96)
 5011 FORMAT(  4X,' Summary (this file): ',A96)
 5015 FORMAT( /4X,' THERE IS NO FILE OPEN TO WRITE MESSAGES TO')
 5016 FORMAT(  4X,' Summary of Run: Standard Output Device, Unit ',I2)
 5020 FORMAT( /14X,56('*'),/14X,'***   ',A44,'   ***',/14X,56('*')/)
 5025 FORMAT(//1X,I1,'.  ',A16)
 5028 FORMAT( /4X,' AERMET Has Determined That Processing For This',
     &           ' Pathway Includes:', /9X,A44/)
 5030 FORMAT(  10X,'Extract Input -',A8,': ',A96)
 5035 FORMAT(  10X,'Extract Output-',A8,': ',A96)
 5040 FORMAT(  10X,'QA Output     -',A8,': ',A96)
 5041 FORMAT(  10X,'1-MIN ASOS    -',A8,': ',A96)
 5042 FORMAT(//1X,'5.  Merged Data')
 5045 FORMAT(  10X,'Merge Output  -',A8,': ',A96)
 5049 FORMAT( /5X,' Site ID ',3X,'Latitude(deg.)',3X,'Longitude(deg.)',
     &         3X,'Time Adjustment')                                    ! dtb #508  06299
 5050 FORMAT( /5X,' Site ID ',3X,'Latitude(deg.)',3X,'Longitude(deg.)',
     &         3X,'Time Adjustment')
 5051 FORMAT( /5X,' Site ID ',3X,'Latitude(deg.)',3X,'Longitude(deg.)',
     &         3X,'Time Adjustment', 3X, 'Elev. (m)')                   ! dtb #508  06299
 5055 FORMAT(  5X,A8,5X,A8,10X,A8,12X,I4)                               ! dtb #508  06299
 5056 FORMAT(  5X,A8,5X,A8,10X,A8,12X,I4, 11X, F6.1)                    ! dtb #508  06299
 5057 FORMAT( /5X,' Data Format: ',A8) 
 5058 FORMAT( /5X,' Data Format: ',A8,5X,'User specified data as ASOS.')
 5060 FORMAT( /10X,'The Extract Dates Are:    Starting: ',A9/
     &         10X,'                            Ending: ',A9)
 5061 FORMAT( /10X,'The Extract Dates Are: *** IN ERROR ***')
 5065 FORMAT( /10X,'Upper Air Data Above the First Level Above',I5,
     &             ' Meters Not Extracted')
 5066 FORMAT( /10X,'The Clipping Height (UATOP) Is In Error')
 5070 FORMAT(  10X,'Upper Air Automatic Data Checks Are: ',A3)
 5080 FORMAT(  I2,'-',A3,'-',I2.2)
 5100 FORMAT(  6X,'INVALID COMBINATION OF PROCESSING OPTIONS: ', A44)

 5210 FORMAT( /14X,56('*'),
     & /14X,'***       AERMET Setup Finished UN-successfully      ***',
     & /14X,'********************************************************')

 5220 FORMAT( /14X,56('*'),
     & /14X,'***        AERMET Setup Finished Successfully        ***',
     & /14X,'********************************************************')

C-----------------------------------------------------------------------
      END

