      SUBROUTINE GETSFC(ISTAT,NCHREC,KOUNT,RECDSZ,RECPOS,MAXBLK,
     &                  NHR)
C=====================================================================**
C        GETSFC Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Retrieves the surface obs. in the date/station window
C               from the archive file.  This routine is used for all
C               SURFACE data formats except ISHD and HUSWO, which are
C               handled separately through the RDISHD and RDHUSW 
C               routines, respectively.
C
C     Called by: SFEXT
C
C     Arguments:
C       ISTAT    Output   Data retrieval status
C       NCHREC   Input    Number of characters per record
C       KOUNT    Output   Number of records written
C       RECDSZ   Input    Size of block to read; equal to record size for
C                         disk reads
C       RECPOS   Input    Position in character buffer to begin reading
C       MAXBLK   Input    Maximum number of characters in the buffer
C       NHR      Input    Number of hours to output with each write
C       NADJASOS Output   Number of ASOS hours that could be corrected
C                         for the truncation problem
C
C     Revision history:
C       11/30/94  - Removed statement that incremented the hour just before
C                   the data were written
C                 - "Scan" results written only when reading tapes
C                   (code removed 12/10/2009)
C       12/11/09  - added code to write ASOD flags for CD144, SCRAM,
C                   SAMSON, and 3280 data
C     
C-----------------------------------------------------------------------

C---- Variable Declaration

      IMPLICIT NONE
      
      INTEGER IHRS, JJJ, ICHECK
      CHARACTER*79 SCSTR3(100)
      INTEGER      SCANSF, SFCERR, JULIAN, RECDSZ, RECPOS
      INTEGER      ISTAT, IOST20, ISTR, NCHREC, MAXBLK, NHR
      INTEGER      NEWYR, NEWMO, NEWDY, NEWHR, NEWDAYC
      INTEGER      IHR, IVBL, JULDAY
      INTEGER      SAVYR, SAVMO, SAVDY, SAVHR
      CHARACTER*8  NEWLOC
      LOGICAL      OK3280

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data initializations
      DATA  OK3280/.FALSE./, SCANSF/1/, JJJ/0/

      PATH = 'SURFACE   '
      LOC  = 'GETSFC'

      SAVYR = -9
      SAVMO = -9
      SAVDY = -9
      SAVHR = -9


C     OK3280 = data for one day extracted from TD-3280 format; write the
C              data to the output file
C     JJJ    = date in the form YYMMDD; computed after reading a date
C              group

C-----------------------------------------------------------------------
C     Processing begins here for surface obs. data.
C     CD144 & SCRAM files are either 80 or 28 characters per record,
C     1 record per obs.  A SCRAM record is read into BUF28, then copied
C     into the first 28 characters of BUF80(1) in order to make use of
C     some of the CD144 structure.

C---- Initialize the arrays and scalars                    ---- CALL FLSFC
      CALL FLSFC(NHR)

C---- Issue warning message if user did not specify SURFACE station
C     elevation on the LOCATION keyword for formats that do not 
C     include elevations.  Station elevation can improve
C     representativeness of estimated station pressure when 
C     pressure is missing (performed in subroutine SUBST during
C     Stage 3).
      IF( .NOT.GOTPWELEV(3) .AND. (INDEX(SFFMT,'CD144') .NE. 0 .OR.
     &                             INDEX(SFFMT,'SCRAM') .NE. 0 .OR.
     &                             INDEX(SFFMT,'3280')  .NE. 0) )THEN
C ---    No user specified station elevation on the LOCATION keyword,
C        and no elevation from file, issue warning message
         MESS =  BLNK80
         WRITE (MESS, 6260) SFFMT
 6260    FORMAT(' NO SURFACE elevation specified on LOCATION keyword ',
     &           'or from ',A8,' file, default of 0m assumed;')
         CALL ERRHDL(1,PATH,'W46',LOC,MESS)
         MESS =  BLNK80
         WRITE (MESS, 6300)
 6300    FORMAT('  Recommend specifying station elevation.')
         CALL ERRHDL(2,PATH,'W46',LOC,MESS)
      ENDIF


C---- Branch to statement 300 is used to read next record
  300 CONTINUE

C---- Read a 'block' of data and, for CD144 and SCRAM formats,
C     store the first record in BUF80(1)                   ---- CALL RDLREC

C---- CD144 format
      IF( NCHREC.EQ.80 )THEN
         BUF80(1) = BLN132
         CALL RDLREC(DEV20,IOST20,RECDSZ,RECPOS,BUFNWS,MAXBLK,SFBLK)
         IF( IOST20.LT.0 )THEN                   ! end-of-file
            GOTO 310

         ELSEIF( IOST20.GT.0 )THEN               ! error
            ISTAT = 1
            GO TO 320
         ENDIF

         READ(BUFNWS,2010) BUF80(1)
 2010    FORMAT(A80)

C---- SCRAM format
      ELSEIF( NCHREC.EQ.28 )THEN
         BUF80(1) = BLN132
         CALL RDLREC(DEV20,IOST20,RECDSZ,RECPOS,BUFNWS,MAXBLK,SFBLK)
         IF( IOST20.LT.0 )THEN                   ! end-of-file
            GOTO 310

         ELSEIF( IOST20.GT.0 )THEN               ! error
            ISTAT = 1
            GO TO 320
         ENDIF

         READ(BUFNWS,2011) BUF80(1)
 2011    FORMAT(A28)
 
         BUF80(1) = ADJUSTL(BUF80(1))

C---- SAMSON format
      ELSEIF( NCHREC.EQ.140 )THEN
         BUF140 = BLN140
         CALL RDLREC(DEV20,IOST20,RECDSZ,RECPOS,BUFNWS,MAXBLK,SFBLK)
         IF( IOST20.LT.0 )THEN                   ! end-of-file
            GOTO 310

         ELSEIF( IOST20.GT.0 )THEN               ! error
            ISTAT = 1
            GO TO 320
         ENDIF

         READ(BUFNWS,2012) BUF140
 2012    FORMAT(A140)

C---- TD-3280 fixed-length block format
      ELSEIF( NCHREC.EQ.318 )THEN
         CALL RDLREC(DEV20,IOST20,RECDSZ,RECPOS,BUFNWS,MAXBLK,SFBLK)
         IF( IOST20.LT.0 )THEN                   ! end-of-file
            GOTO 310

         ELSEIF( IOST20.GT.0 )THEN               ! error
            ISTAT = 1
            GO TO 320
         ENDIF

C---- TD-3280 variable-length format
      ELSEIF( NCHREC.EQ.1230 )THEN
         CALL RDLREC(DEV20,IOST20,RECDSZ,RECPOS,BUFNWS,MAXBLK,SFBLK)
         IF( IOST20.LT.0 )THEN                   ! end-of-file
            GOTO 310

         ELSEIF( IOST20.GT.0 )THEN               ! error
            ISTAT = 1
            GO TO 320
         ENDIF

      ENDIF


C---- Decode station/date group; save data for a 'report' if no data
C     are retrieved (tape only); process all data, including variables,
C     for SAMSON data

      IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
C------- CD-144 format                                     ---- CALL D144HD
         CALL D144HD(BUF80(1),NEWLOC,NEWYR,NEWMO,NEWDY,NEWHR,
     &               ISTAT)

      ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
C------- SCRAM format                                      ---- CALL D144HD
C        Replaced first paramater with BUF80 for Intel compiler
         CALL D144HD(BUF80(1),NEWLOC,NEWYR,NEWMO,NEWDY,NEWHR,ISTAT)

      ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
C------- All the data on the SAMSON record, both header and weather data,
C         will be processed with this call to subr. RDSAMS ---- CALL RDSAMS
         CALL RDSAMS(BUF140,NEWYR,NEWMO,NEWDY,NEWHR,ISTAT)


      ELSEIF( INDEX(SFFMT,'3280') .NE. 0 )THEN
C------- TD-3280 format                                    ---- CALL D3280H
         BUF32 = ADJUSTL(BUFNWS(1:32))
         NEWHR = 0
         CALL D3280H(BUF32,NEWLOC,NEWYR,NEWMO,NEWDY,ISTAT)
         SFGHR = 0

      ENDIF

C --- Check for problems with date sequence for 
C     non-3280 surface formats (since TD-3280 
C     data include 24 hours of data per record)
      IF( INDEX(SFFMT,'3280') .EQ. 0 )THEN

         IF( SAVYR .GT. NEWYR )THEN
            MESS =  BLNK80
            ECODE = 'E44'
            WRITE(MESS,2000) SFFMT,NEWYR,NEWMO,NEWDY,NEWHR
2000        FORMAT(1X,A8,' data is out-of-sequence at ',
     &                                '(Yr,Mn,Dy,Hr): ',4I4 )
            CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
            ISTAT = 1
            RETURN
            
         ELSEIF( SAVYR .EQ. NEWYR .AND. SAVMO .GT. NEWMO )THEN
            MESS =  BLNK80
            ECODE = 'E44'
            WRITE(MESS,2000) SFFMT,NEWYR,NEWMO,NEWDY,NEWHR
            CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
            ISTAT = 1
            RETURN
         
         ELSEIF( SAVYR .EQ. NEWYR .AND. SAVMO .EQ. NEWMO .AND.
     &                                  SAVDY .GT. NEWDY )THEN
            MESS =  BLNK80
            ECODE = 'E44'
            WRITE(MESS,2000) SFFMT,NEWYR,NEWMO,NEWDY,NEWHR
            CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
            ISTAT = 1
            RETURN

         ELSEIF( SAVYR .EQ. NEWYR .AND. SAVMO .EQ. NEWMO .AND.
     &           SAVDY .EQ. NEWDY .AND. SAVHR .GE. NEWHR )THEN
            MESS =  BLNK80
            ECODE = 'E44'
            WRITE(MESS,2000) SFFMT,NEWYR,NEWMO,NEWDY,NEWHR
            CALL ERRHDL(KOUNT,PATH,ECODE,LOC,MESS)
            ISTAT = 1
            RETURN

         ENDIF
         
         SAVYR = NEWYR
         SAVMO = NEWMO
         SAVDY = NEWDY
         SAVHR = NEWHR
         
      ENDIF
      
C---- Error decoding the information in the header (ISTAT=1);
C     or Station ID mismatch (ISTAT=2)
      IF( ISTAT .EQ. 1 )THEN
C        Decoding error, go to 520
         GO TO 520
      ELSEIF( ISTAT .EQ. 2 )THEN
C        Station ID mismatch, reset ISTAT=1, 
C        but return, bypassing general message 
C        606 for decoding errors; message 
C        regarding station ID mismatch already
C        generated in subroutine reading the header
         ISTAT = 1
         RETURN
      ENDIF

C---- Compute the Julian and chronological days; check for
C     inclusion in the station/date window

      JJJ = NEWYR*10000 + NEWMO*100 + NEWDY
      JULDAY = JULIAN(NEWYR,NEWMO,NEWDY)

      CALL CHROND(PATH,NEWYR,JULDAY,NEWDAYC)
      IF( NEWDAYC .LT. 0 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,630)
         CALL ERRHDL(JJJ,PATH,'E40',LOC,MESS)
 630     FORMAT(' ERROR COMPUTING CHRONOLOGICAL DAY')
         RETURN
      ENDIF

C---- For TD-3280 files, a decision must be made as to whether to write
C     a record of the previous day's data or not.  This is not a decision
C     for the other types of data.

      IF( INDEX(SFFMT,'3280') .NE. 0 )THEN

C-------- If this is a new station or a new day, then write the previous
C         observation.  This first requires checking the location

         IF( OK3280 )THEN
            ICHECK = INDEX(SFLOC1,NEWLOC(1:5))                           ! dtb #121 02092
            IF( NEWYR.NE.SFGYR  .OR.  NEWMO.NE.SFGMO  .OR.
     &          NEWDY.NE.SFGDY  .OR.  ICHECK .EQ. 0 )THEN
               DO IHRS=1,NHR
C----             IHRS is adjusted by minus 1 since data index
C                 used for data arrays included +1 to handle
C                 TD-3280 data being stored for hours 00-23
                  WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHRS-1,
     &                              (SFOBS(IHRS,IVBL),IVBL=30,51),
     &                               ISASOS24(IHRS)
                  KOUNT = KOUNT + 1
               ENDDO
               CALL FLSFC(NHR)

C------------- We want to print the remaining record when a new location
C              is found, but we only want to do it once!
               IF( IWORK1(21).EQ.0 )THEN
                  OK3280 = .FALSE.
                  GO TO 310
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      SFGYR  = NEWYR
      SFGMO  = NEWMO
      SFGDY  = NEWDY
      SFGHR  = NEWHR

      SFDAYC = NEWDAYC

C-----------------------------------------------------------------------
C     The INDEX function is used to insure a correct match on station
C     identifiers - BUF08 is left justified, SFLOC1 is right justified.
C     (If the identifier field requires 8 characters there is no blank)
C     This logic is not needed for SAMSON data because the station id
C     appears only once in the file - in a header that was read by
C     SUBR.SETSAM
C
C---- NOTE: This code may be redundant, since Station ID's are now
C     checked in subroutine D144HD for CD144 and SCRAM data formats
C     and subroutine D3280H for TD-3280 format; will leave it in 
C     for now.

      IF( INDEX(SFFMT,'SAMSON') .EQ. 0 )THEN
C------- Not a SAMSON data set
         BUF08(1) = NEWLOC

C------- How long is the station ID?  Look for a blank in the string -
C        remember that the ID is left justified in BUF08
         IWORK1(20) = INDEX(BUF08(1),' ')

         IF( IWORK1(20) .EQ. 0 )THEN
C---------- The ID requires the entire 8 characters
            IWORK1(21) = INDEX(SFLOC1,BUF08(1)(1:8))

         ELSEIF( IWORK1(20) .EQ. 1 )THEN
C---------- The first character in BUF08 is blank --> field is empty
            ISTAT = 1
            MESS =  BLNK80
            WRITE(MESS,673)
            CALL ERRHDL(0,PATH,'E44',LOC,MESS)
            GO TO 320

         ELSE
C---------- Now compare the station ID from postion 1 to the position just
C           before the blank character located above.
            IWORK1(21) = INDEX(SFLOC1,BUF08(1)(1:IWORK1(20)-1))
         ENDIF

C------- The stations do not match -- read next record
         IF( IWORK1(21) .EQ. 0) GO TO 300

      ENDIF

      IF( SFDAYC .LT. SFDAY1 )THEN
C------- The stations match but the data are before the extract window
         GO TO 300

      ELSEIF( SFDAYC .GT. SFDAY2 )THEN
C------- The stations match but the data are after the extract window
C        *** EXTRACTION PROCESS COMPLETED ***
         MESS =  BLNK80
         WRITE(MESS,702)
         CALL ERRHDL(0,PATH,'I49',LOC,MESS)
         RETURN
      ENDIF

C-----------------------------------------------------------------------
C---- The correct time span and station have been located.  Process the
C     station data as necessary

      IF( sfghr .eq. 1 ) WRITE(*, 610 ) sfgmo,sfgdy,sfgyr
 610  FORMAT('+  Stage 1: Extracting surface data for ',
     &       'month/day/year ', 3(I2.2,:'/'))
      OK3280 = .TRUE.

      IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
         CALL D144LV(BUF80(1),ISTAT,JJJ)

      ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
         CALL D028LV(BUF80(1)(1:28),ISTAT,JJJ)

      ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
C        SAMSON data: one complete record was read and processed
         CONTINUE

      ELSEIF( INDEX(SFFMT,'3280') .NE. 0 )THEN
         CALL D3280L(ISTAT,JJJ)

      ENDIF

C---- If there was an error decoding the weather data, stop processing
      IF( ISTAT .EQ. 1 ) GO TO 521

C --- Increment record KOUNT for non-3280 data formats
      IF( INDEX(SFFMT,'3280') .EQ. 0 )THEN
         KOUNT = KOUNT + 1
      ENDIF
      
C---- We do not have to wait to write non-3280 file records
      IF( INDEX(SFFMT,'3280') .EQ. 0 )THEN
         WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,SFGHR,
     &                     (SFOBS(1,IVBL),IVBL=30,51), ISASOS1

         CALL FLSFC(NHR)
      ENDIF
      GO TO 300

C=======================================================================
C---- Processing continues here if an end of file was encountered while
C     reading a record with RDLREC for any of the formats, but first ...

C---- Write the last record if its OK before writing to the message file
C      Need to subtract 1 from IHR since 3280 data are on 0:23 clock
  310 CONTINUE
  
      IF( OK3280  .AND.  NHR .GT. 1 )THEN
         DO IHR = 1, NHR
            WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHR-1,
     &                       (SFOBS(IHR,IVBL),IVBL=30,51),
     &                        ISASOS24(IHR)
            KOUNT = KOUNT + 1
         ENDDO
      ENDIF

      MESS =  BLNK80
      WRITE(MESS,600)
      CALL ERRHDL(0,PATH,'I49',LOC,MESS)
      IF( KOUNT .LT. 1 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,701)
         CALL ERRHDL(0,PATH,'E41',LOC,MESS)

         IF( INDEX(SFFMT,'3280') .NE. 0 )THEN
C           Write scan output for tape input
            WRITE(DEV70,5004)
            WRITE(DEV70,5005) UNIT20,DEV20
            DO ISTR = 1,SCANSF
               WRITE(DEV70,5010) BLNK08,SCSTR3(ISTR)
            ENDDO
            WRITE(DEV70,5004)
         ENDIF

      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here if an error was encountered;

  320 CONTINUE
  
      SFCERR = SFCERR + 1
      IF( SFCERR .LE. MAXERR )THEN
C------- Error reading a logical record - there are MAXERR chances to
C        read data from the archive file correctly
         MESS =  BLNK80
         WRITE(MESS,602) SFCERR, MAXERR
         CALL ERRHDL(KOUNT,PATH,'W42',LOC,MESS)
         GO TO 300

      ELSE
C------- The maximum number of errors was exceeded reading data from
C        the archive file
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,603) JJJ, NEWHR
         CALL ERRHDL(0,PATH,'E42',LOC,MESS)
         RETURN
      ENDIF

C-----------------------------------------------------------------------
C     Continue here if any of the decodes errored out

C---- Header record

  520 CONTINUE
      ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,606) JJJ, NEWHR
      CALL ERRHDL(0,PATH,'E43',LOC,MESS)
      RETURN

C---- Data record

  521 CONTINUE
      ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,607)
      CALL ERRHDL(JJJ,PATH,'E43',LOC,MESS)
      RETURN

C-----------------------------------------------------------------------

  600 FORMAT(' END-OF-FILE encountered')
  601 FORMAT(' NO observations retrieved; See unit ',I3,' for scan')
  602 FORMAT(' ERROR # ',I2,' reading SURFACE file (MAX. # = ',I2,')' )
  603 FORMAT(' MAX # ERRORS reading SURFACE file after (YYMMDD) ',
     &         I6.6,'; hour ',I2)
  606 FORMAT(' Error decoding file header information after (YYMMDD) ',
     &         I6.6,'; hour ',I2)
  607 FORMAT(' ERROR decoding weather data')
  673 FORMAT(' SURFACE Station ID read from file is blank!')
  701 FORMAT(' NO hourly observations retrieved')
  702 FORMAT(' END-OF-DATA window encountered' )

 2110 FORMAT(1X,4I2.2,4(1X,I5),6(1X,I5.5),/
     &       8X,5(1X,I5.5),7(1X,I5),2x,A1)
 5004 FORMAT('$SFSCAN$')
 5005 FORMAT(//,5X,'File contents for ',A96,', Unit=',I3,
     &        /,36X,'from',15X,'to',
     &        /,18X,' Station',2X,'YR',2(1X,'MO',1X,'DA',1X,'HR',1X,
     &          'JDAY',4X))
 5010 FORMAT(A8,A80)

      END

