      SUBROUTINE GETFSL( ISTAT, NSDGS )
C=====================================================================**
C          GETFSL Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  To retrieve soundings within the station/date window and
C             write the results to a disk file
C
C   Called by: UAEXT
C
C   Arguments:
C      ISTAT   Input/  Status of data retrieval:
C                        = 0 on input from calling program
C              Output    = 0 => data retrieved, no errors
C                        = 1 =  error(s) retrieving data
C      NSDGS   Output  Counter for number of soundings retrieved
C                      (initialized in calling program)
C
C     Input:    Raw data extracted from the CD-ROM
C
C     Output:   Upper air soundings in AERMET format for QA
C
C     Initial release:  August 1, 1995 (as a standalone product)
C
C     Revision history:
C                       July 26, 2006;  Adjusted range of acceptable
C                                       surface pressures to avoid
C                                       invalidating good data.
C
C     This program was written by Pacific Environmental Services, Inc.
C                                 5001 South Miami Blvd, Suite 300
C                                 P.O. Box 12077
C                                 Research Triangle Park, NC 27709-2077
C-----------------------------------------------------------------------


      IMPLICIT NONE
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'

C---- Data declarations

      REAL            XKT2MS
      REAL            XLAT, XLON
      INTEGER         JULIAN, IBASEZ, NSDGS, NREAD
      INTEGER         ITYPE, IHOUR, IDAY, IYEAR, ISTAT, NMONTH
      INTEGER         NRECS, ISAVMB, NCOUNT, N4FILE, NLEVEL, IPRESS
      INTEGER         IHGHT, ITEMP, IDEWPT, IWDIR, IWSPD, ILEV, IVBL
      INTEGER         JULDAY, JJJ, I
      CHARACTER*3     CMONTH, FSLMON(12)
      CHARACTER*2     WSUNIT
      LOGICAL         ATTOP
      INTEGER         MISSED  ! Missing value indicator for FSL data     ! dtb004 01134
      INTEGER         PFACT   ! Multiplier for pressure                  ! dtb004 01134


C---- Data initializations

      DATA NREAD /0/
      DATA XKT2MS /0.51444/

      DATA FSLMON/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     &            'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/

      DATA MISSED /32767/  ! Missing value indicator for FSL version 1.  ! dtb004 01134
      DATA PFACT  /10/     ! Multiplier for pressure for FSL version 1.  ! dtb004 01134

      PATH = 'UPPERAIR  '
      LOC  = 'GETFSL'
      ISTAT = 0

C---- Begin processing a sounding --------------------------------------

  100 NREAD = NREAD + 1
      ATTOP = .FALSE.

C---- Read the first record of the sounding -- check the record type
      READ( DEV10, 1000, ERR=1001, END=10000 ) ITYPE, IHOUR, IDAY,
     &                                         CMONTH, IYEAR
 1000 FORMAT( I7, I7, I7, 6X, A3, I8 )

C---- Make sure that the record is the first header of the sounding -
C     required to be of type 254
      IF( ITYPE .NE. 254 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,610) NREAD
         CALL ERRHDL(0,PATH,'E33',LOC,MESS)
      ENDIF
 610  FORMAT(' 1st RECORD NOT OF FSL TYPE 254 AT SDG # ',i4)


C---- Convert the year to a two-digit value
      IF( IYEAR .GE. 2000 )THEN
          UAGYR = IYEAR - 2000
      ELSE
          UAGYR = IYEAR - 1900
      ENDIF

C---- Convert the 3-character month to a numeric value
      UAGMO = 0
      NMONTH = 1
      DO WHILE ( UAGMO .EQ. 0  .AND.  NMONTH .LE. 12 )
         IF( FSLMON(NMONTH) .NE. CMONTH )THEN
            NMONTH = NMONTH + 1
         ELSE
            UAGMO = NMONTH
         ENDIF
      ENDDO

      UAGDY = IDAY
      UAGHR = IHOUR

C---- Convert date to julian day

      IF( UAGMO .NE. 0 )THEN
         JULDAY = JULIAN ( UAGYR, UAGMO, UAGDY )
         JJJ = UAGYR*10000 + UAGMO*100 + UAGDY
      ELSE
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,620) NREAD
         CALL ERRHDL(0,PATH,'E33',LOC,MESS)
      ENDIF
 620  FORMAT(' UNABLE TO DETERMINE MONTH FOR SDG # ',i4)

C---- Read the second record -- station wban id, latitude, longitude
      READ( DEV10, 1050, ERR=1051, END=1002 ) BUF08(1), XLAT, XLON
 1050 FORMAT( T10, A5, T23, F6.2, 1X, F6.2 )

C---- AERMET requires that soundings be written to disk in local
C     standard time (LST); convert the time from Greenwich (GMT) to LST
C     to LST using the time zone adjustment

      UAGHR = UAGHR - UALST
      IF( UAGHR .LT. 0 )THEN
C       Conversion to LST converted the date to the previous day -
C       recompute hour, Julian day, and year
        UAGHR = UAGHR + 24
        JULDAY = JULDAY - 1
        IF( JULDAY .LT. 1 )THEN
           IF( IYEAR .NE. 2000 )THEN
             UAGYR = UAGYR - 1
           ELSE
             UAGYR = 99
           ENDIF

           IF( MOD(UAGYR,4) .NE. 0 )THEN
              JULDAY = 365
           ELSE
              JULDAY = 366
           ENDIF
        ENDIF

      ELSEIF( UAGHR .GT. 23 )THEN
C       Conversion to LST converted the date to the next day -
C       recompute hour, Julian day, and year
        UAGHR = UAGHR - 24
        JULDAY = JULDAY + 1
        IF( JULDAY .EQ. 367 )THEN
           UAGYR = UAGYR + 1
           JULDAY = 1
           
        ELSEIF( JULDAY .EQ. 366 )THEN
           IF( MOD(IYEAR,4) .NE. 0 )THEN
              UAGYR = UAGYR + 1
              JULDAY = 1
           ENDIF
        ENDIF

      ENDIF

      CALL GREG( UAGYR, JULDAY, UAGMO, UAGDY )


C---- Check the station ID.  Remember that the ID is left justified in
C     BUF08 and UALOC1 is right justified.

      IF( INDEX(UALOC1,TRIM(BUF08(1))) .EQ. 0 )THEN
C        The station ID does not match the station specified on the
C        LOCATION record in the control file


         MESS =  BLNK80
         WRITE(MESS,690) BUF08(1), UALOC1
CRWB     Modify the message on mismatched UA station IDs between
CRWB     Stage 1 LOCATION keyword and UA data file from fatal 
CRWB     to non-fatal warning.  This facilitates use of UA data
CRWB     files that include substitutions from representative 
CRWB     alternative UA stations for periods with missing UA
CRWB     soundings. These warning messages will also serve to
CRWB     document when substitutions were made.
         CALL ERRHDL(JJJ,PATH,'W31',LOC,MESS)
         ISTAT = 0
CRWB         RETURN


C        read the data until the next sounding is reached

c        READ( DEV10, 1100, ERR=1101, END=1002 ) NRECS
c*       print *, " # levels= ", nrecs
c        READ( DEV10, 999, ERR=1151, END=1002 )
c        DO 500 I=1,NRECS-4
c           READ( DEV10, 999, ERR=1201, END=1002 )
c 500    CONTINUE
c        GO TO 100
      ENDIF

  999 FORMAT (1X)

C---- Compute the chronological day, UADAYC                ---- CALL CHROND
      CALL CHROND('UPPERAIR  ',UAGYR,JULDAY,UADAYC)

C---- Check the chronological day
      IF( UADAYC .LT. 0 )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,625) NREAD
         CALL ERRHDL(JJJ,PATH,'E30',LOC,MESS)
 625     FORMAT(' ERROR COMPUTING CHRONOLOGICAL DAY FOR SDG ',I4)
         RETURN
      ENDIF

c*    print *, ' Chron day: ', uagyr,julday,uadayc,uaday1,uaday2

      IF ( UADAYC .LT. UADAY1 )THEN
C------- The stations match but the date is before the beginning
C        extract date; finish reading the sounding then read next
C        soundings headers
         READ( DEV10, 1100, ERR=1101, END=1002 ) NRECS
         READ( DEV10, 999, ERR=1151, END=1002 )

         DO I=1,NRECS-4
            READ( DEV10, 999, ERR=1201, END=1002 )
         ENDDO

         GO TO 100

      ELSEIF ( UADAYC .GT. UADAY2 )THEN
C------- The stations match but the data are beyond the extract window,
C        the extraction process is completed
         GO TO 10000

      ENDIF

C---- The station id's match and the data are in the data window
      WRITE( *,699 ) UAGMO,UAGDY,UAGYR
  699 FORMAT('+  Stage 1: Extracting upper air data for ',
     &       'month/day/year ',3(I2.2,:,'/'))


C---- Increment the counter for the number of soundings retrieved
C     (Note that the variable NREAD used below in many of the messages
C     is the counter for the number of soundings read but not
C     necessarily written to the output file)
      NSDGS = NSDGS + 1

C---- Read the third record for the total number of records,
C     which includes the four header records, in this sounding

      READ( DEV10, 1100, ERR=1101, END=1002 ) NRECS
 1100 FORMAT( 28X,I7 )

C---- Read the fourth record for the units for wind speed:
C     kt = knots, ms = meters/second

      READ( DEV10, 1150, ERR=1151, END=1002 ) WSUNIT
 1150 FORMAT( 47X,A2 )

      IF( WSUNIT .NE. 'kt' .AND. WSUNIT .NE. 'ms' )THEN
         ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,635) NREAD
         CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
         RETURN
      ENDIF
  635 FORMAT( ' INCORRECT UNITS FOR FSL WIND SPEED, SDG # ',I4 )


C---- Begin reading the sounding data
C     ncount = the number of sounding levels read
C     n4file = the number of levels to write to the output file

      IBASEZ  = 0
      ISAVMB  = 0
      NCOUNT  = 0
      N4FILE  = 0
      NLEVEL = NRECS - 4

      DO WHILE( NCOUNT .LT. NLEVEL )

         NCOUNT = NCOUNT + 1
         READ( DEV10, 1200, ERR=1201, END=1002 ) ITYPE, IPRESS,
     &         IHGHT, ITEMP, IDEWPT, IWDIR, IWSPD
 1200    FORMAT( 7I7 )

c                                                                        ! dtb004 01134
         IF( NSDGS .EQ. 1 )THEN                                          ! dtb004 01134
            IF( NCOUNT.EQ.1 .AND. ITYPE.EQ.9 )THEN                       ! dtb004 01134
               IF( IPRESS.EQ.32767 .OR. IPRESS.EQ.99999 )THEN            ! dtb004 01134
c                 Missing surface pressure                               ! dtb004 01134
                  ISTAT = 1                                              ! dtb004 01134
                  MESS =  BLNK80                                         ! dtb004 01134
                  WRITE(MESS,672) IPRESS                                 ! dtb004 01134
                  CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)                  ! dtb004 01134
                  RETURN                                                 ! dtb004 01134
               ELSEIF( IPRESS.GE.700 .AND. IPRESS.LE.1100 )THEN          ! rwb #500  06299
                  continue                                               ! dtb004 01134
               ELSEIF( IPRESS.GE.7000 .AND. IPRESS.LE.11000 )THEN        ! rwb #500  06299
                  MISSED = 99999                                         ! dtb004 01134
                  PFACT  = 1                                             ! dtb004 01134
               ELSE          !  Unable to determine FSL version          ! dtb004 01134
                  ISTAT = 1                                              ! dtb004 01134
                  MESS =  BLNK80                                         ! dtb004 01134
                  WRITE(MESS,674) IPRESS                                 ! dtb004 01134
                  CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)                  ! dtb004 01134
                  RETURN                                                 ! dtb004 01134
               ENDIF                                                     ! dtb004 01134
                                                                         ! dtb004 01134
            ELSEIF( NCOUNT.EQ.1 .AND. ITYPE.NE.9 )THEN                   ! dtb004 01134
c              First level is not the surface                            ! dtb004 01134
               ISTAT = 1                                                 ! dtb004 01134
               MESS =  BLNK80                                            ! dtb004 01134
               WRITE(MESS,676)                                           ! dtb004 01134
               CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)                     ! dtb004 01134
               RETURN                                                    ! dtb004 01134
            ENDIF                                                        ! dtb004 01134
         ENDIF                                                           ! dtb004 01134
c                                                                        ! dtb004 01134

C------- Do not process data if the arrays are full or the height is
C        above UATOP above the ground.  Note:  'ibasez' is defined
C        below, but is not required the first time through this loop
C        unless the station is above UATOP above sea level.

         IF( NCOUNT .LE. UAML .AND.  (.NOT. ATTOP) )THEN

C---------- Check the first record: if it is not the surface (record
C           type 9), warn the user, skip the sounding, and process the
C           next sounding; if it is the surface, then save the height
C           to adjust the sounding heights to above local ground.
C           In the rare event that the surface record is exactly the
C           height of a mandatory level, save the pressure from this
C           level to compare against each level processed

            IF( NCOUNT .EQ. 1  .AND.  ITYPE .NE. 9 )THEN
               MESS =  BLNK80
               WRITE(MESS,645) NREAD
               CALL ERRHDL (JJJ,PATH,'W36',LOC,MESS)
               DO I=1,NRECS-5
                  READ( DEV10, 999, ERR=1201, END=1002 )
               ENDDO

C------------- Process the next sounding
               GO TO 100

            ELSEIF( NCOUNT .EQ. 1 )THEN
               IF( IHGHT .NE. MISSED )THEN                               ! dtb004 01134
                  IBASEZ = IHGHT
               ELSE
                  IBASEZ = 0
                  MESS =  BLNK80
                  WRITE(MESS,646) NREAD
                  CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
               ENDIF
            ENDIF
  645 FORMAT( ' SDG SKIPPED: 1st LEVEL NOT TYPE 9, SDG # ', I4)
  646 FORMAT( ' SFC HEIGHT MISSING - SET TO 0, SDG # ',I4)


C---------- Process the data except if this record is type 6 for which
C           only winds are reported

            IF( ITYPE .GE. 4  .AND.  ITYPE .NE. 6 )THEN


C------------- Retain only those levels with nonmissing temperature
C              (missing data code = MISSED ); assume that pressure and   ! dtb004 01134
C              height are never missing
C              Also, do not keep the record if the pressure matches
C              the surface pressure (this implies that the surface
C              record appears twice and is likely equal to a mandatory
C              level)
C              Save the pressure to check for a duplicate level

               IF( ITEMP .NE. MISSED .AND. IPRESS .NE. MISSED .AND.      ! dtb004 01134
     &             IHGHT .NE. MISSED) THEN                               ! dtb004 01134

                  IF( IPRESS .NE. ISAVMB )THEN
                     ISAVMB = IPRESS

                     N4FILE = N4FILE + 1
                     IF( IHGHT .GT. (UATOP+IBASEZ) )THEN
                        ATTOP = .TRUE.
                     ENDIF

C------------------- Temperature
                     UAOBS(1,N4FILE,3) = ITEMP

C------------------- Pressure and height
                     UAOBS(1,N4FILE,1) = IPRESS * PFACT                  ! dtb004 01134
                     UAOBS(1,N4FILE,2) = IHGHT - IBASEZ

C------------------- Dew point
                     IF( IDEWPT .NE. MISSED )THEN                        ! dtb004 01134
                        UAOBS(1,N4FILE,4) = IDEWPT
                     ELSE
                        UAOBS(1,N4FILE,4) = -9990
                     ENDIF

C------------------- Wind direction
                     IF( IWDIR .NE. MISSED )THEN                         ! dtb004 01134
                        UAOBS(1,N4FILE,5) = IWDIR
                     ELSE
                        UAOBS(1,N4FILE,5) = 999
                     ENDIF

C------------------- Wind speed
                     IF( IWSPD .NE. MISSED )THEN                         ! dtb004 01134
                        IF( WSUNIT .EQ. 'kt' )THEN
                           UAOBS(1,N4FILE,6) = 
     &                                NINT( (FLOAT(IWSPD)*XKT2MS)*10.0 )
                        ELSE
                           UAOBS(1,N4FILE,6) = IWSPD
                        ENDIF
                     ELSE
                        UAOBS(1,N4FILE,6) = 9990
                     ENDIF
                  ENDIF            ! duplicate pressure check
               ENDIF               ! nonmissing temperature, pressure, height
            ENDIF                  ! record type
         ENDIF                     ! ncount < maxlvl, ht < 5000 m
      ENDDO


C---- Write the sounding to the output file
      WRITE(DEV12,2050) UAGYR,UAGMO,UAGDY,UAGHR,N4FILE

      DO ILEV = 1,N4FILE
         WRITE(DEV12,2100) (UAOBS(1,ILEV,IVBL),IVBL=1,UAMV)
      ENDDO

 2050 FORMAT( 1X,4I2.2,I5 )
 2100 FORMAT( 6(1X,I6) )


C---- Process the next sounding
      GO TO 100

C---- Error conditions

 1001 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,630) NREAD
      CALL ERRHDL(JJJ,PATH,'E33',LOC,MESS)
      RUNERR =.TRUE.
      RETURN

 1051 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,640) NREAD
      CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
      RETURN

 1101 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,650) NREAD
      CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
      RETURN

 1151 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,660) NREAD
      CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
      RETURN

 1201 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,670) NCOUNT, NREAD
      CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
      RETURN

  630 FORMAT(' ERROR READING 1st RECORD OF FSL SDG # ', I4)
  640 FORMAT(' ERROR READING 2nd RECORD OF FSL SDG # ', I4)
  650 FORMAT(' ERROR READING 3rd RECORD OF FSL SDG # ', I4)
  660 FORMAT(' ERROR READING 4th RECORD OF FSL SDG # ', I4)
  670 FORMAT(' ERROR READING LEVEL ',I3, ' OF FSL SDG # ', I4)

C---- End of file conditions

 1002 ISTAT = 1
      MESS =  BLNK80
      WRITE(MESS,680) NREAD
      CALL ERRHDL (JJJ,PATH,'E33',LOC,MESS)
      RETURN
  680 FORMAT(' UNEXPECTED E-O-F ON FSL SDG #', I4)

10000 IF( NSDGS .GT. 0 .AND. ISTAT .EQ. 0) THEN
C------- Data retrieval was successful, nothing more to do
C        ISTAT = 0
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

      ELSEIF( NSDGS .GT. 0  .AND.  ISTAT .EQ. 1 )THEN
C------- A problem occurred after one or more soundings were retrieved
c        ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,603) NSDGS
         CALL ERRHDL(JJJ,PATH,'E35',LOC,MESS)

      ELSEIF( NSDGS .EQ. 0  .AND.  ISTAT .EQ. 1 )THEN
C------- A problem occurred before any soundings were retrieved
c        ISTAT = 1
         MESS =  BLNK80
         WRITE(MESS,604)
         CALL ERRHDL(0,PATH,'E36',LOC,MESS)

      ENDIF

  601 FORMAT(' END-OF DATA WINDOW ENCOUNTERED' )
  602 FORMAT(' NO SOUNDINGS RETRIEVED; NO OTHER ERRORS')
  603 FORMAT(' EXTRACT NOT COMPLETED; STOPPED AFTER SDG # ',I4)
  604 FORMAT(' NO SOUNDINGS RETRIEVED; EXITING WITH AN ERROR')

  672 FORMAT(' Could not determine FSL version (missing pressure)  ',I7) ! dtb004 01134
  674 FORMAT(' Could not determine FSL version (suspect pressure)  ',I7) ! dtb004 01134
  676 FORMAT(' Could not determine FSL version (missing surface level)') ! dtb004 01134

  690 FORMAT(' UA ID in file (', A8, ') does not match ID on LOCATION',
     &       ' keyword (', A8, ') on the date specified')


C---- THE END

      END

