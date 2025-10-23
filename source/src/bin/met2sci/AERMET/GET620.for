      SUBROUTINE GET620( ISTAT,NSDGS,RECDSZ,RECPOS,MAXBLK )
C=======================================================================
C           GET620 Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  To retrieve soundings within the station/date window and
C             write the results to a disk file
C
C   Called by: UAEXT
C
C   Arguments:

C      ISTAT  Input/  Status of data retrieval:
C                       = 0 on input from calling program
C             Output    = 0 => data retrieved, no errors
C                       = 1 =  error(s) retrieving data
C      NSDGS  Output  Counter for number of soundings retrieved
C                     (initialized in calling program)
C     These next three arguments only apply to TD-6201 data
C      RECDSZ Input   Size of a physcial record
C      RECPOS Input   Starting position of the next sounding
C      MAXBLK Input   Maximum block size
C
C   Initial release:  December 15, 1992
C
C   Revision history:
C      11/30/94
C        - All soundings extracted before returning to UAEXT
C        - Moved all error and EOF handling to end of routine
C        - Removed calls to SUBR.SGNCHK
C        - Get surface height only once and use it on all sdgs
C        - Replaced calls to SUBR.SGNFCN with intrinsic NINT
C
C      06/28/95
C        - Added logic to check for multiple records defining
C          one sounding (primarily for data on diskette)
C
C      11/06/96
C        - restructured the code; eliminated most code that
C          attempted to recover from errors - now most errors
C          are immediately fatal.
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      INTEGER JJJ, IUACUR, IUAHR

      CHARACTER*79 SCSTR1(100), STRNG1
      INTEGER      SCANUA, NSDGS, RECDSZ, RECPOS, MAXBLK
      INTEGER      DELCNT, SDGERR
      INTEGER      IOST10, ISTAT, ISTR, NUMLEV, LVL, ILEV, IVBL
      INTEGER      NCALM, NTMP, NDEW, JULIAN, MULT(6), UAARG
      REAL         ZSFC
      LOGICAL      SCFLAG

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      PATH = 'UPPERAIR  '
      LOC  = 'GET620'

      DATA SCANUA/1/
      DATA ZSFC /-0.1/
      DATA SCFLAG /.TRUE./
      DATA SDGERR/0/,  DELCNT/0/
      DATA NCALM/0/, NTMP/0/, NDEW/0/
      DATA MULT/10, 1, 10, 10, 1, 10/
      DATA IUACUR/-1/, IUAHR/-1/

C *** Variable descriptions
C
C      The following variables are calling arguments for the routine
C
C      SCSTR1(I)   String with a information regarding file contents
C      STRNG1      Same as above, except used as passing argument
C      SCANUA      Counter for the number of scan strings written
C      IOST10      I/O status word of read or decode
C      NUMLEV      Number of levels extracted and retained in a sounding
C      MULT(I)     Multiplier to make data integers with sufficient
C                   number of decimal places retained for accuracy
C      SDGERR      Counter for the number of read or decode errors
C                   (cannot equal or exceed maxerr)
C      UAARG       an integer variable to use for whatever purpose
C
C      The following variables are counters used in simple QA on a sdg
C
C      SGNCNT     Number of levels at which sign of temperature was
C                  changed
C      NCALM      Number of occurrences of nonzero wind direction with
C                  a corresponding zero wind speed (direction set to 0)
C      NDEW       Number of levels of missing dew-points
C                  (interpolated data replace missing data)
C      NTMP       Number of levels of missing temperatures
C                  (interpolated data replace missing data)
C      DELCNT     Number of mandatory levels deleted
C
C      ISTR       Loop indices
C      LVL,ILEV,
C      IVBL
C
C      JULIAN      Integer function to calculate the JULIAN day
C
C      IUACUR,     Variables in which the sounding chronological day
C      IUAHR       and hour are stored to compare to the next data
C                  in the next read from the file.
C
C *** Subroutines called
C
C      ERROR       Writes error/warning messages
C      SCNGEN      Writes a scan record
C      D6201H      Decodes TD-6201 format sounding headers
C      D6201L      Decodes TD-6201 format sounding levels
C      MANDEL      Deletes mandatory levels
C      CALMS       Checks for nonzero wind direction and zero speed
C                   changes direction to zero
C      TDPEST      Interpolates temperature and dew-point if data are
C                   missing
C
C-----------------------------------------------------------------------

  100 CONTINUE
  
      CALL RDLREC( DEV10, IOST10, RECDSZ, RECPOS, BUFNWS,
     &             MAXBLK, UABLK )
      IF( IOST10.LT.0 )THEN                      ! end-of-file
         MESS =  BLNK80
         WRITE(MESS,600)
         CALL ERRHDL(0,PATH,'I39',LOC,MESS)
         GO TO 1000

      ELSEIF( IOST10.GT.0 )THEN                  ! error
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,604) NSDGS + 1
         CALL ERRHDL(0,PATH,'E32',LOC,MESS)
         GO TO 1000
      ENDIF


C---- Decode the 'header' containing the station ID, date and
C     number of levels in the sounding                     ---- CALL D6201H

      ISTAT = 0
      CALL D6201H(ISTAT)
      IF(ISTAT .EQ. 1 )THEN
C------- Error decoding the sounding header
         MESS =  BLNK80
         WRITE(MESS,606) NSDGS
         CALL ERRHDL(0,PATH,'E33',LOC,MESS)
         GO TO 1000
      ENDIF

C---- Retain information on file contents                  ---- CALL SCNGEN
      IF( SCANUA .LT. 100 )THEN
         CALL SCNGEN(SCANUA,SCFLAG,BUF08(1),UAGYR,UAGMO,UAGDY,
     &            UAGHR,1200,STRNG1)
            SCSTR1(SCANUA) = STRNG1
      ENDIF


C---- How long is the station ID?  Look for a blank in the string -
C     remember that the ID is left justified in BUF08
C     The index function is used to insure a correct match on station
C     identifiers - BUF08 is left justified, UALOC1 is right justified.
C     If the identifier field requires 8 characters, there is no blank

      IWORK1(10) = INDEX(BUF08(1),' ')

      IF( IWORK1(10) .EQ. 0 )THEN
C------- The ID requires the entire 8 characters
         IWORK1(11) = INDEX(UALOC1,BUF08(1)(1:8))

      ELSEIF( IWORK1(10) .EQ. 1 )THEN
C------- The first character in BUF08 is blank --> field is empty
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,615) NSDGS
         CALL ERRHDL(0,PATH,'E34',LOC,MESS)
         GO TO 1000

      ELSE
C------- Field is not completely filled nor is it empty; compare the
C        station ID (in BUF08) from postion 1 to the position just
C        before the blank character determined above.
         IWORK1(11) = INDEX(UALOC1,BUF08(1)(1:IWORK1(10)-1))

      ENDIF


      IF(IWORK1(11) .EQ. 0) THEN
C------- No match on the station - read the next record
         GO TO 100
      ENDIF


C---- Station match; now check the date and time - first convert
C     the dates to Julian and chronological days

C---- Convert from Greenwich Mean Time to Local Standard
C     Time                                                 ---- CALL GMTLST
      CALL GMTLST(UAGYR,UAGMO,UAGDY,UAGHR,UALST)

C---- Compute the Julian day                               ---- FUNC JULIAN
      UAARG = JULIAN(UAGYR,UAGMO,UAGDY)

C---- Convert the hour to 24 if the hour is 00             ---- CALL HR0024
C                                                          ---- CALL GREG
      IF(UAGHR .EQ. 0) THEN
         CALL HR0024(UAGYR,UAARG,UAGHR)
         CALL GREG(UAGYR,UAARG,UAGMO,UAGDY)
      ENDIF

C---- Compute the date as YYMMDD
      JJJ = UAGYR*10000 + UAGMO*100 + UAGDY

C---- Compute the chronological day, UADAYC                ---- CALL CHROND
      CALL CHROND('UPPERAIR  ',UAGYR,UAARG,UADAYC)

C---- Check the chronological day
      IF( UADAYC .LT. 0 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,630) NSDGS
         CALL ERRHDL(JJJ,PATH,'E30',LOC,MESS)
 630     FORMAT(' ERROR COMPUTING CHRONOLOGICAL DAY FOR SDG ',I4)
         RETURN
      ENDIF


      IF ( UADAYC .LT. UADAY1 )THEN
C------- The stations match but the date is before the beginning
C        extract date; read next record
         GO TO 100

      ELSEIF ( UADAYC .GT. UADAY2 )THEN
C------- The stations match but the data are beyond the extract window,
C        the extraction process is completed
         GO TO 1000

      ENDIF


C---- The station id's match and the data are in the data window
      WRITE( *,699 ) UAGMO,UAGDY,UAGYR
  699 FORMAT('+  Stage 1: Extracting upper air data for ',
     &       'month/day/year ',3(I2.2,:,'/'))


      IF( IUACUR  .EQ. UADAYC  .AND.  IUAHR .EQ. UAGHR )THEN
C------- The date on the record just read matches the previous record;
C        skip the record (AERMET only works with one record (79 levels)
C        of data)

         GO TO 100

      ELSE
C------- Save the chronological day and hour for future comparisons
         IUACUR = UADAYC
         IUAHR  = UAGHR

      ENDIF

C---- Increment the number of soundings retrieved
      NSDGS = NSDGS + 1

C---- Initialize variables                                 ---- CALL FLWRK2
      CALL FLWRK2
      NUMLEV = 0

C---- Now decode each level, checking to be sure not to exceed the
C     maximum number of levels allowed, clipping the sounding to the
C     specified height, and converting the data to the proper units
C     and integerizing.                                    ---- CALL D6201L

      ISTAT = 0
      CALL D6201L(NUMLEV,ISTAT)
      IF( ISTAT .EQ. 1 )THEN

C------- There was an error decoding a level of data.  Check for maximum
C        number of sounding error decodes/reads.  If the maximum is not
C        attained, then AERMET will attempt to continue processing data;
C        otherwise, the status is set to an error condition and control
C        returns to the calling program

         SDGERR = SDGERR + 1

         IF(SDGERR .LT. MAXERR) THEN
            MESS =  BLNK80
            WRITE(MESS,605) SDGERR,NUMLEV,UAGHR
            CALL ERRHDL(JJJ,PATH,'W33',LOC,MESS)

         ELSE
            ISTAT = 1
            MESS =  BLNK80
            WRITE(MESS,608) MAXERR
            CALL ERRHDL(JJJ,PATH,'E33',LOC,MESS)
            GO TO 1000

         ENDIF
      ENDIF

C---- As long as at least one level of data was decoded, then process
C     the sounding

      IF( NUMLEV .GT. 0 )THEN

C------- Save the height of the first level (ZSFC); since the height of
C        the first level does/should not change between soundings
C        (unless the station moves, whcih CAN happen), this is done only
C        once.  (Because missing data are set to -9999.0 in the work
C        arrays check against -9000.0 rather than UAQA(vbl,2))

         IF( ZSFC .LT. 0.0 )THEN
            IF( WORK2(1,2) .GT. -9000.0 )THEN
               ZSFC = WORK2(1,2)
            ELSE
               MESS =  BLNK80
               WRITE(MESS,607) NSDGS
               CALL ERRHDL(JJJ,PATH,'W34',LOC,MESS)
            ENDIF
         ENDIF

C------- Adjust the sounding to be above local ground level if the
C        height at the first level is not missing
         IF( ZSFC .GT. 0.0 )THEN
            DO LVL = 1,NUMLEV
               IF( WORK2(LVL,2) .GT. -9000.0 )THEN
                   WORK2(LVL,2) = WORK2(LVL,2) - ZSFC
               ENDIF
            ENDDO
         ENDIF


C------- Modify the sounding if the user specified the keyword MODIFY
C        in the runstream
C        - delete mandatory levels that meet criteria,     ---- CALL MANDEL
C        - adjust winds to properly reflect calms,         ---- CALL CALMS
C        - fill in missing temperature and dew point       ---- CALL TDPEST
         IF( STATUS(2,12) .EQ. 2 )THEN
            CALL MANDEL(NUMLEV,DELCNT)
            CALL CALMS(NUMLEV,NCALM)
            CALL TDPEST(NUMLEV,NTMP,NDEW)
         ENDIF


C------- Transfer the data from the work array to the upper air array,

         CALL FLSDG(1)
         UALEV(1) = NUMLEV
         DO ILEV = 1,UALEV(1)
            DO IVBL = 1,UAMV
               IF( WORK2(ILEV,IVBL) .LT. -900.0 )THEN
                   UAOBS(1,ILEV,IVBL) = UAQA(IVBL,2)
               ELSE
                  UAOBS(1,ILEV,IVBL) = NINT(WORK2(ILEV,IVBL)*MULT(IVBL))
               ENDIF
            ENDDO
         ENDDO


C------- Write the data to the output file

         WRITE(DEV12,1210) UAGYR,UAGMO,UAGDY,UAGHR,UALEV(1)
         DO ILEV = 1,UALEV(1)
            WRITE(DEV12,1220) (UAOBS(1,ILEV,IVBL),IVBL=1,UAMV)
         ENDDO
 1210    FORMAT(1X,4I2.2,I5)
 1220    FORMAT(6(1X,I6))


      ELSE
C------- The data decoding did not get past the first level;

         UALEV(1) = 0
         WRITE(DEV12,1210) UAGYR,UAGMO,UAGDY,UAGHR,UALEV(1)
         MESS =  BLNK80
         WRITE(MESS,610) NSDGS
         CALL ERRHDL(JJJ,PATH,'W35',LOC,MESS)
      ENDIF

C---- Process another sounding
      GO TO 100

C-----------------------------------------------------------------------
 1000 CONTINUE

      IF( NSDGS .GT. 0 .AND. ISTAT .EQ. 0) THEN
C------- Data retrieval was successful, nothing more to do
c        ISTAT = 0
         MESS =  BLNK80
         WRITE(MESS,601)
         CALL ERRHDL(0,PATH,'I39',LOC,MESS)

      ELSEIF( NSDGS .EQ. 0  .AND.  ISTAT .EQ. 0 )THEN
C------- No soundings extracted and there were no errors; most likely a
C        problem with the station or dates in the runstream (or wrong
C        file?); a list of stations & dates will be written to messages

         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,602)
         CALL ERRHDL(0,PATH,'E31',LOC,MESS)
         WRITE(DEV70,5000)
         WRITE(DEV70,5003)                                             ! dtb #516  06306
         WRITE(DEV70,5005) DEV10,UNIT10
         DO ISTR = 1,SCANUA
            WRITE(DEV70,5010) BLNK08,SCSTR1(ISTR)
         ENDDO
         WRITE(DEV70,5000)

      ELSEIF( NSDGS .GT. 0  .AND.  ISTAT .EQ. 1 )THEN
C------- A problem occurred after one or more soundings were retrieved
c        ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,640) NSDGS
         CALL ERRHDL(JJJ,PATH,'E35',LOC,MESS)

      ELSEIF( NSDGS .EQ. 0  .AND.  ISTAT .EQ. 1 )THEN
C------- A problem occurred before any soundings were retrieved
c        ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,603)
         CALL ERRHDL(0,PATH,'E36',LOC,MESS)

      ENDIF

      RETURN


C=======================================================================
C---- Format statements

  600 FORMAT(' END-OF-FILE ENCOUNTERED')
  601 FORMAT(' END-OF DATA WINDOW ENCOUNTERED' )
  602 FORMAT(' NO SOUNDINGS RETRIEVED; NO OTHER ERRORS')
  603 FORMAT(' NO SOUNDINGS RETRIEVED; EXITING WITH AN ERROR')
  604 FORMAT(' ERROR READING ARCHIVE FILE AT SOUNDING # ',I4)
  605 FORMAT(' ERROR # ',I2,'-DECODING LEVEL',I3,' FOR HR ',I2.2)
  606 FORMAT(' ERROR DECODING DATE GROUP ON SOUNDING # ', I4)
  607 FORMAT(' SURFACE HEIGHT MISSING FOR SOUNDING NUMBER',I4)
  608 FORMAT(' MAX ERRORS (',I2,') DECODING DATA; PROCESSING STOPS')
  610 FORMAT(' NO LEVELS OF DATA FOR SOUNDING # ',I4 )
  615 FORMAT(' STATION IDENTIFIER BLANK AT SDG # ',I4)
  640 FORMAT(' EXTRACT NOT COMPLETED; STOPPED AFTER SDG # ',I4)
 5000 FORMAT('$UASCAN$')

 5003 FORMAT(/'           Check the WBAN# on the LOCATION input image'/ ! dtb #516  06306
     &        '           You may need to insert 3 leading zeros ' /)   ! dtb #516  06306
 5005 FORMAT(//,8X,'CONTENTS FOR UNIT ',I3,', FILE: ',A96,
     &        /,36X,'FROM',15X,'TO',
     &        /,18X,' STATION',2X,'YR',2(1X,'MO',1X,'DA',1X,'HR',1X,
     &          'JDAY',4X))
 5010 FORMAT(A8,A80)

      END

