        SUBROUTINE OSQACK( ISTAT )
C=====================================================================**
C          OSQACK Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  QA's the on-site data such that
C                 1) only data within the extract date
C                    window are output to the output file, and
C                 2) hourly averages are computed as needed (if
C                    the OBS/HOUR keyword is used and is greter than 1).
C
C     Called by: OSPATH
C
C     Initial Release:  December 1992
C
C     Developed by: Pacific Environmental Services, Inc. (PES)
C                   Research Triangle Park, NC
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER    I, J, JJJ
      INTEGER    NUMBER,NHRS,ISTAT,KEY,QARSLT, NHDRS
      REAL       RMISS,REALUP,REALDN,RVALUE
      CHARACTER  NAME*4

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C-----------------------------------------------------------------------
C        NUMBER      Last record processed from OS-IQA file
C        NHRS        Counter for number of hours processed
C        ISTAT       Status returned from OSNEXT (initialized to 0 when
C                                                 OSQACK is called from
C                                                 SUBROUTINE OSPATH) 
C                      1 = fatal errors have occurred, set OSSTAT = -1
C                      2 = so far so good
C                      3 = Either eof found or data beyond extract
C                          window.
C
C        KEY,RMISS...NAME  used in performing QA
C
C-----------------------------------------------------------------------

C---- Data Initialization
C     NHRS must be initialized to 1 to read the site-specific data
C      correctly; there is a statement in subr.OSNEXT that would cause
C      the second hour in the site-specific data to be skipped if NHRS
C      is initialized to 0.

      PATH = PATHWD(4)
      LOC  = 'OSQACK'
      NUMBER = 0
      NHDRS  = 0
      NHRS   = 0

C---- Check for extract dates; if present, convert start and
C        stop extract dates to equivalents in number of hours
C        since beginning of 1900.  OSDAY1 and OSDDAY2 are Julian
C        days for the respective years (OSYR1 and OSYR2)

      IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 )THEN
         CALL CHROND( PATH,OSYR1,OSDAY1,IWORK1(1200) )
         CALL CHROND( PATH,OSYR2,OSDAY2,IWORK1(1201) )
      ENDIF

C---- Read to end of headers (if any are present): data file

      REWIND DEV31

   5  CONTINUE
      BUF03 = '   '
      READ( DEV31,1000,IOSTAT=IOFLAG,END=10 ) BUF03
1000  FORMAT( A3 )

C---- Check status of the read

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = 'E51'
         WRITE( MESS,2000 )
 2000    FORMAT(' Error reading HEADER records: ONSITE ''DATA'' file.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         GO TO 100
      ENDIF

      IF( BUF03.EQ.'***') GOTO 15
      IF( BUF03(1:1).EQ.'*' )THEN
         NHDRS = NHDRS + 1
         GO TO 5
      ENDIF


C---- This short section is a safeguard against not having the record
C     with the three asterisks (***) to indicate end-of-headers

  10  REWIND DEV31
      DO I=1,NHDRS
         READ( DEV31,1000,IOSTAT=IOFLAG,END=10 ) BUF03
      ENDDO


C---- Read to end of headers (if any are present): QA file

  15  REWIND DEV32
      NHDRS = 0

  20  CONTINUE

      BUF03 = '   '
      READ( DEV32,1000,IOSTAT=IOFLAG,END=33 ) BUF03

C---- Ckeck the status of the read

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = 'E51'
         WRITE( MESS,2010 )
 2010    FORMAT(' Error reading HEADER records: ONSITE ''QAOUT'' file.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         GO TO 100
      ENDIF

      IF( BUF03.EQ.'***') GOTO 33
      IF( BUF03(1:1).EQ.'*' )THEN
          NHDRS = NHDRS + 1
          GO TO 20
      ENDIF

C---- This short section is a safeguard against not having the record
C     with the three asterisks (***) to indicate end-of-headers

  33  REWIND DEV32
      DO I=1,NHDRS
         READ( DEV32,1000) BUF03
      ENDDO

C---- Insert master headers in output QA file
      WRITE(DEV32,1001)
 1001 FORMAT('*  OS     ON-SITE QUALITY ASSESSMENT ')
      WRITE (DEV32,1111)
 1111 FORMAT('*** EOH: END OF ON-SITE QA HEADERS')
C

C-----> Processing the data starts and returns here

   35 CONTINUE

      CALL OSNEXT( NUMBER,NHRS,ISTAT )

C---- Check the status of the data retrieval

      IF( ISTAT.EQ.1 )THEN
C----    Problem encountered in OSNEXT, return to OSPATH
         RETURN

      ELSE
C------- QA hourly values; set QARSLT = 5 for the hourly QA; compute
C        the working hour based on Julian day and hour
C        (Julian day*100 + hour)

         IWORK1(1210) = OSDAYC*100 + OSGHR
         IF( OSGHR .EQ. 1 ) WRITE( *,1210 ) OSGMO,OSGDY,OSGYR
 1210    FORMAT( '+  Stage 1: QA''ing on-site data for ',
     &           'month/day/year ', 3(I2.2,:,'/') )

         JJJ = OSGYR*10000 + OSGMO*100 + OSGDY

C------- QA the data: first the scalar variables
C
         DO I=1,14
            IF( OSSAUD(I) .NE. 1 ) CYCLE              !  Check if audit is on
            KEY    = NINT( OSQA(I,1) )
            RMISS  = OSQA(I,2)
            REALDN = OSQA(I,3)
            REALUP = OSQA(I,4)
            RVALUE = OSSOBS(1,I)
            NAME   = VNAMES(I)

            IF( ABS( RVALUE-RMISS ) .LT. 0.01 )THEN                  !  Missing

               OSAUD1(I,1) = OSAUD1(I,1) + 1

               IF( OSSTRA(I) .EQ. 0 .AND. OSSAUD(I).EQ.1 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,450) NAME,OSGHR
                  CALL ERRHDL(JJJ,PATH,'Q59',LOC,MESS)
  450             FORMAT(1X,A4,' missing for HR: ',I3.2)
               ENDIF

            ELSE                                        !  Not missing

               QARSLT = 5

               CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                      RVALUE,NAME,OSGYR,OSGMO,OSGDY,OSGHR,99,0,
     &                      QARSLT )

c              If QARSLT comes back as 1 (missing) then something is amiss.

               IF( QARSLT .EQ. 1 )THEN     !  Something is amiss        ! dtb103 02022
                  CYCLE                    !  Insert warning            ! dtb103 02022
               ELSE

                  OSAUD1(I,QARSLT) = OSAUD1(I,QARSLT) + 1

               ENDIF

            ENDIF

         ENDDO

         DO I=30,34

            IF( OSSAUD(I) .NE. 1 ) CYCLE
            IF( I.EQ.34 )THEN
               KEY    = NINT( OSTSKY(1) )
               RMISS  = OSTSKY(2)
               REALDN = OSTSKY(3)
               REALUP = OSTSKY(4)

            ELSE
               KEY    = NINT( OSQA(I,1) )
               RMISS  = OSQA(I,2)
               REALDN = OSQA(I,3)
               REALUP = OSQA(I,4)
            ENDIF

            RVALUE = OSSOBS(1,14+I-29)
            NAME   = VNAMES(I)
            QARSLT = 5
            CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                   RVALUE,NAME,OSGYR,OSGMO,OSGDY,OSGHR,99,0,
     &                   QARSLT)

            IF( QARSLT .EQ. 1 .AND. OSSAUD(I).EQ.1 .AND.
     &          OSSTRA(I) .EQ. 0 )THEN
               MESS =  BLNK80
               WRITE(MESS,450) NAME,OSGHR
               CALL ERRHDL(JJJ,PATH,'Q59',LOC,MESS)
            ENDIF

            OSAUD1(I,QARSLT) = OSAUD1(I,QARSLT) + 1

         ENDDO

C------- Continue with the multi-level variables

         DO I=15,29
            IF( OSSAUD(I) .NE. 1 ) CYCLE
            KEY    = NINT( OSQA(I,1) )
            RMISS  = OSQA(I,2)
            REALDN = OSQA(I,3)
            REALUP = OSQA(I,4)
            NAME   = VNAMES(I)

            DO J=1,OSNL
               RVALUE = OSVOBS(1,J,I-14)
               QARSLT = 5
               CALL REALQA( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                      RVALUE,NAME,OSGYR,OSGMO,OSGDY,OSGHR,99,J,
     &                      QARSLT)
               IF( QARSLT .EQ. 1 .AND. OSSAUD(I).EQ.1 .AND.
     &             OSSTRA(I) .EQ. 0 )THEN
                  MESS =  BLNK80
                  WRITE(MESS,452) NAME,OSGHR,J
 452              FORMAT(1X,A4,' missing for HR: ',I3.2,
     &                         '; LEVEL : ',I2)
                  CALL ERRHDL(JJJ,PATH,'Q59',LOC,MESS)
               ENDIF

               OSAUD2(J,I-14,QARSLT) = OSAUD2(J,I-14,QARSLT) + 1

            ENDDO

         ENDDO

C------- Write hourly data to the on-site QA output file

         CALL OSWRTE( NUMBER,ISTAT )

         IF( ISTAT.EQ.1 )THEN
C----       Problem encountered in OSWRTE, return to OSPATH
            RETURN

         ELSE

C-------    Increment hourly counter
            NHRS = NHRS + 1
         
         ENDIF

         IF( ISTAT.EQ.2 )THEN
C-------    No problems with processing data yet;
C           go to 35 to continue processing until
C           error (ISTAT=1) or EOF (ISTAT=3)
            GO TO 35
         ENDIF

      ENDIF

100   RETURN
      END

