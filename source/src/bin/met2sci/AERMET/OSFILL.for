      SUBROUTINE OSFILL( IPATH,NUMBER,DEVNUM,RCSTAT )
C=====================================================================**
C          OSFILL Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Called during Stage 1 to Fetch one on-site observation 
C               from the specified device (DEVNUM).  The on-site data 
C               are stored ('buffered') into three files within the 
C               WORK1 common block.
C               The scalar data values are stored within IWORK1(1400+)
C               and WORK1(1400+), and the vector values are stored within
C               WORK2(100++,+).
C               Note: the date and time data, which are integer variables
C               are stored in IWORK1(1400+).
C               The + in the above description is the variable's index
C               within the VNAME array, and ++ is the tower level index.
C
C     Called by:
C
C     Calls to:      ERROR
C
C     Initial Release:  December 1992
C
C-----------------------------------------------------------------------

C---- Data Declarations
C

      IMPLICIT NONE
      
      INTEGER  IPATH,NUMBER,DEVNUM,RCSTAT,NERR
      INTEGER  ISUM, I, J 
      INTEGER  NDEC
      INTEGER, SAVE :: NSAV, NSAV2
      REAL     RMISS
      LOGICAL, SAVE :: L_DecMismatch, L_DecVaries, L_DecVaries2

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C-----------------------------------------------------------------------
C     IPATH   PATHWAY (LIKELY 4=OS OR 5=MR)
C     NUMBER  RECORD NUMBER OF FILE
C     DEVNUM  DEVICE NUMBER FROM WHICH WE ARE TO READ OS DATA
C
C     RCSTAT  RECORD STATUS   0 = fill to buffer worked ok
C                             1 = filled buffer but had read errors
C                             2 = the allowable number of read errors
C                                 has been exceeded (MAXERR)
C                             3 = end of file encountered sooner
C                                 than expected.
C                             4 = end of file encountered
C
C     NERR    NUMBER OF READ ERRORS ENCOUNTERED THUS FAR
C
C-----------------------------------------------------------------------

C---- Varaible Initialization

      DATA NSAV/0/, NSAV2/0/
      DATA L_DecMismatch/.FALSE./, L_DecVaries/.FALSE./,
     &                             L_DecVaries2/.FALSE./
      PATH = PATHWD(IPATH)
      LOC  = 'OSFILL'
      RCSTAT = 0
      NERR   = 0
      IOFLAG = 0
      NDEC   = 0
      
C---- Set all values in 'buffer' to missing flag values

C     First the scalar variables

      DO I=1,51
         IF( I.EQ.34 )THEN
            RMISS = OSTSKY(2)
         ELSE
            RMISS = OSQA(I,2)
         ENDIF
         WORK1(1400+I) = RMISS
      ENDDO


C---- Then the vector variables

      DO I=15,29

         DO J=1,OSNL
            WORK2(100+J,I-14) = OSQA(I,2)
         ENDDO

      ENDDO


C---- Finally, the integer variables

      DO I=52,56
         IWORK1(1400+I) = NINT( OSQA(I,2) )
      ENDDO

50    CONTINUE

C---- 1.  Read the data

C     1A. First process record
C         OSDNUM(1) contains the number of fields on the first record
C         OSTIME contains the number of date/time fileds read

C---- Read data into character buffer first
      BUF500 = BLN500
      READ( DEVNUM, '(A)', IOSTAT=IOFLAG,END=99 ) BUF500
      NDEC = 0
      DO I=1,LEN_TRIM(BUF500)
         IF( BUF500(I:I) .EQ. '.' )THEN
            NDEC = NDEC + 1
         ENDIF
      ENDDO

99    CONTINUE

C --- Increment number of records read
      NUMBER = NUMBER + 1
      
C --- Reset IOFLAG to 0; shouldn't be errors reading data to BUF500
      IOFLAG = 0
      
C --- Backspace to re-read data with full format, rather than
C     from data buffer (BUF500)
      BACKSPACE DEVNUM
      
      IF( OSFRMT(1) .EQ. '*' )THEN
         READ( DEVNUM, *,
     &         IOSTAT=IOFLAG,END=300 )
     &       ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     &       ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
      
      ELSE
         READ( DEVNUM,OSFRMT(1),
     &         IOSTAT=IOFLAG,END=300 )
     &       ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     &       ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
      ENDIF
       
      IF( IOFLAG.NE.0 )THEN
C----    ONSITE Read Error, set flags and issue error messages;
C        included statement regarding data types assumed in READ.
         NERR = NERR + 1

         IF( NERR .LE. MAXERR .AND. NUMBER-NERR .LE. MAXERR )THEN
            ECODE = 'W52'
         ELSE
            ECODE = 'E52'
         ENDIF

         I = 1            ! assign I=1 for first read for error handling
         MESS =  BLNK80
         WRITE( MESS,1000 ) NUMBER
1000     FORMAT(' Error reading ONSITE record #',I6,
     &          '; Dates are read as INT, ',
     &          'data variables read as REAL.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )

         MESS =  BLNK80
         WRITE( MESS,1100 ) I,OSDNUM(I),OSFRMT(I)(1:90)
1100     FORMAT('  Read#',I3,'; #Flds=',I2,'; Format: ',A90)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         IF( LEN_TRIM(OSFRMT(I)) .GT. 90 )THEN
            WRITE( MESS, 1200 ) 
     &             OSFRMT(I)(91:MIN(132,LEN_TRIM(OSFRMT(I))))
1200        FORMAT('  Format (cont.): ',A)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         ENDIF

         MESS =  BLNK80
         WRITE( MESS,1300 ) BUF500(1:110)
1300     FORMAT('  Record: ',A)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )

         IF( NERR.GT.MAXERR .OR. (NUMBER-NERR).GT.MAXERR )THEN
C----       Maximum allowed number of READ errors exceeded;
C           branch to statement 100 to set RCSTAT and return
            GO TO 100
         ELSE 
            MESS =  BLNK80
            ECODE = 'W52'
            WRITE( MESS,1310 ) MAXERR
1310        FORMAT('  Max # of READ errors (',I2,') allowed',
     &             ' for ONSITE data file ''headers'' NOT exceeded.',
     &             ' Record skipped.')
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         ENDIF

C----    Go to 50 to read the next record, before moving on to
C        reading records 2, 3, etc. due to read error
         GO TO 50
         
      ENDIF

C ----Assign data to proper WORK arrays
      DO J=OSTIME+1,OSDNUM(1)

          IF( OSDVAR(1,J,2).LE.0 )THEN
C----------- Scalar variable
             WORK1(1400+OSDVAR(1,J,1)) = WORK1(J)
          ELSE
C----------- Vector variable
             WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14) = WORK1(J)
          ENDIF

      ENDDO

C---- Check status of the read

C---- Check for all date variables (YR,MN,DY,HR) = 0, indicating 
C     possible blank records at end of file (ignoring OSMN).
      ISUM = 0
      DO J=1,MIN(4,OSTIME)
         ISUM = ISUM + IWORK1(1400+OSDVAR(1,J,1))
      ENDDO
      IF( ISUM .EQ. 0 .AND. NUMBER .EQ. 1 .AND. 
     &                      LEN_TRIM(BUF500) .GE. 10 )THEN
C        Treat this as possible "header" record at beginning
C        of ONSITE file
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,800 ) 
 800     FORMAT(' All date variables = 0 for first record.',
     &          ' Treated as ''header'' record and skipped.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
C        Increment NERR since treating as read "error"
         NERR = NERR + 1
         GO TO 50
      ELSEIF( ISUM .EQ. 0 )THEN
C        Date variables missing or zero; 
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,900 ) NUMBER
 900     FORMAT(' All date variables = 0 for record # ',I6,
     &          '; possible blank record. Treated as end-of-file.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         DO J=1,MIN(4,OSTIME)
            IWORK1(1400+OSDVAR(1,J,1)) = NINT( OSQA(OSDVAR(1,J,1),2) )
         ENDDO
         GO TO 300
      ENDIF
      
C-----Check for inconsistencies in number of decimals included in
C     data buffer for REAL variables
      IF( IPATH.EQ.4 .AND. NDEC.LT.OSDNUM(1)-OSTIME .AND. 
     &                                      .NOT.L_DecMismatch )THEN
C ---    Not all real variables include decimals, issue
C        warning message for first occurrence.            
         I = 1            ! assign I=1 for first read for error handling
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,811 ) OSDNUM(1)-OSTIME, NDEC
 811     FORMAT(' Some REALs do not have decimals;  ',
     &          'Extracted data should be verified. ',
     &          'NumVar =', I2, '; NumDec =', I2)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
C ---    Print message with data buffer
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,812 ) I, BUF500(1:100)
 812     FORMAT('  Read#',I3,';  Data: ',A)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         L_DecMismatch = .TRUE. 
      ENDIF

      IF( IPATH.EQ.4 .AND. NSAV.NE.0 .AND. NSAV.NE.NDEC .AND.
     &                                     .NOT.L_DecVaries )THEN
C ---    Number of real variables with decimals has changed,
C        issue warning message for first occurrence.
         I = 1            ! assign I=1 for first read for error handling
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,813 ) NSAV, NDEC
 813     FORMAT(' Number of REALs with decimals varies; ',
     &          'Extracted data may be suspect! ',
     &          'NumOld =', I2, '; NumNew =', I2)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
C ---    Print message with data buffer
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,812 ) I, BUF500(1:100)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         L_DecVaries = .TRUE.
      ENDIF
      NSAV = NDEC

C---- Loop on rest of records

C---- Reinitialize IOFLAG = 0
      IOFLAG = 0
      
      DO I=2,OSDCRD

C ---    Branch to statement 60 if READ error found, to 
C        allow re-reading the data without advancing to the
C        next READ (allows for up to 3 embedded "headers" 
C        in the ONSITE data file)
60       CONTINUE

C ---    First record data record into buffer to check for decimals
         BUF500 = BLN500
         READ( DEVNUM, '(A)', IOSTAT=IOFLAG,END=98 ) BUF500
         NDEC = 0
         DO J=1,LEN_TRIM(BUF500)
            IF( BUF500(J:J) .EQ. '.' )THEN
               NDEC = NDEC + 1
            ENDIF
         ENDDO

98       CONTINUE

C ---    Increment number of records read
         NUMBER = NUMBER + 1
         
C ---    Reset IOFLAG to 0; shouldn't be errors reading data to BUF500
         IOFLAG = 0
         
C ---    Backspace to re-read data with full format, rather than
C        from data buffer (BUF500)
         BACKSPACE DEVNUM
         
C ---    Read next record with user-specified format; branch to
C        statement 200 (abnormal EOF) if end-of-file reached 
C        within this loop since all data records for this 
C        observation should be available.
         IF( OSFRMT(I) .EQ. '*' )THEN
C ---       Read data using FREE format
            READ( DEVNUM, *, IOSTAT=IOFLAG,END=200 )
     &                             ( WORK1(J),J=1,OSDNUM(I) )
         ELSE
C ---       Read data with user-specified format
            READ( DEVNUM,OSFRMT(I),IOSTAT=IOFLAG,END=200 )
     &                             ( WORK1(J),J=1,OSDNUM(I) )
         ENDIF

C------- Check read status
         IF( IOFLAG.NE.0 )THEN
C----       ONSITE Read Error, set flags and issue error messages;
C           including statement regarding data types assumed in READ.
            NERR = NERR + 1
            
            IF( NERR .LE. MAXERR .AND. NUMBER-NERR .LE. MAXERR )THEN
               ECODE = 'W52'
            ELSE
               ECODE = 'E52'
            ENDIF
            
            MESS =  BLNK80
            WRITE( MESS,1000 ) NUMBER
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         
            MESS =  BLNK80
            WRITE( MESS,1100 ) I,OSDNUM(I),OSFRMT(I)(1:90)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )

            IF( LEN_TRIM(OSFRMT(I)) .GT. 90 )THEN
               WRITE( MESS, 1200 ) 
     &                OSFRMT(I)(91:MIN(132,LEN_TRIM(OSFRMT(I))))
               CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            ENDIF
         
            MESS =  BLNK80
            WRITE( MESS,1300 ) BUF500(1:110)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         
            IF( NERR.GT.MAXERR .OR. (NUMBER-NERR).GT.MAXERR )THEN
               GO TO 100
            ELSE 
               MESS =  BLNK80
               ECODE = 'W52'
               WRITE( MESS,1310 ) MAXERR
              CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            ENDIF
         
C----       Go to 60 to read the next record
            GO TO 60
            
         ENDIF


C------- Assign data to proper WORK arrays
         DO J=1,OSDNUM(I)

            IF( OSDVAR(I,J,2) .LE. 0 )THEN
C------------  Scalar variable
               WORK1(1400+OSDVAR(I,J,1)) = WORK1(J)

            ELSE
C------------- Vector variable
               WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1)-14) = WORK1(J)
            ENDIF

         ENDDO

         IF( IPATH.EQ.4 .AND. NDEC.LT.OSDNUM(I) .AND. 
     &                                         .NOT.L_DecMismatch )THEN
C ---       Not all real variables include decimal places, issue
C           warning message for first occurrence.
            MESS =  BLNK80
            ECODE = 'W52'
            WRITE( MESS,811 ) OSDNUM(I), NDEC
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
C ---       Print message with data buffer
            MESS =  BLNK80
            ECODE = 'W52'
            WRITE( MESS,812 ) I, BUF500(1:100)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            L_DecMismatch = .TRUE. 
         ENDIF

C-----   Check for inconsistencies in number of decimals included in
C        data buffer for REAL variables, but only for first 2 READs
         IF( I.EQ.2 .AND. IPATH.EQ.4 .AND. NSAV2.NE.0 .AND. 
     &                                     NSAV2.NE.NDEC .AND.
     &                                        .NOT.L_DecVaries2 )THEN
C ---       Number of decimal places for real variables has changed,
C           issue warning message for first occurrence.
            MESS =  BLNK80
            ECODE = 'W52'
            WRITE( MESS,813 ) NSAV2, NDEC
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
C ---       Print message with data buffer
            MESS =  BLNK80
            ECODE = 'W52'
            WRITE( MESS,812 ) I, BUF500(1:100)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            L_DecVaries2 = .TRUE.
         ENDIF

C ---    Save number of decimals for READ number 2
         IF( I.EQ.2 ) NSAV2 = NDEC

      ENDDO

      RETURN

C-----Exceeded allowable limit of read errors
 100  RCSTAT = 2
      MESS =  BLNK80
      ECODE = 'E52'
      WRITE( MESS,2000 ) MAXERR
2000  FORMAT(' Exceeded limit for ONSITE READ errors (',I2,
     &        ') or error READing non-''header'' record within ',
     &        'data file.')
      CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
      RETURN

C-----Found EOF too soon
 200  RCSTAT = 3
      MESS =  BLNK80
      ECODE = 'E56'
      WRITE( MESS,3000 )
3000  FORMAT(' EOF encountered too soon with ONSITE data.')
      CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
      RETURN

C-----Found EOF on data file
 300  RCSTAT = 4
      MESS =  BLNK80
      ECODE = 'I59'
      WRITE( MESS,4000 ) NUMBER
4000  FORMAT(' EOF for ONSITE data after observation #:', I8)
      CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
      RETURN

      END

