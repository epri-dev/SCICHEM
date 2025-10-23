      SUBROUTINE OSFILL2( IPATH,NUMBER,DEVNUM,RCSTAT )
C=====================================================================**
C          OSFILL2 Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Called during Stage 2 (MERGE) to fetch one on-site 
C               observation from the specified device (DEVNUM).  
C               The on-site data are stored ('buffered') into
C               three files within the WORK1 common block.
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
      REAL     RMISS

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

      PATH = PATHWD(IPATH)
      LOC  = 'OSFILL2'
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

C---- 1.  Read the data

C     1A. First process record
C         OSDNUM(1) contains the number of fields on the first record
C         OSTIME contains the number of date/time fileds read

      IF( OSDNUM(1).EQ.OSTIME )THEN
C        The first record for the hour only has the date/time variables
         IF( OSFRMT(1) .EQ. '*' )THEN
            READ( DEVNUM, *,
     &            IOSTAT=IOFLAG,END=300 )
     &          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
         ELSE
            READ( DEVNUM,OSFRMT(1),
     &            IOSTAT=IOFLAG,END=300 )
     &          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
         ENDIF
         NUMBER = NUMBER + 1

      ELSE
C        The first record includes date and observed variables
         IF( OSFRMT(1) .EQ. '*' )THEN
            READ( DEVNUM, *,
     &            IOSTAT=IOFLAG,END=300 )
     &          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     &          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
        
         ELSE
            READ( DEVNUM,OSFRMT(1),
     &            IOSTAT=IOFLAG,END=300 )
     &          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     &          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
         ENDIF
         NUMBER = NUMBER + 1
          
         DO J=OSTIME+1,OSDNUM(1)

             IF( OSDVAR(1,J,2).LE.0 )THEN
C-------------- Scalar variable
                WORK1(1400+OSDVAR(1,J,1)) = WORK1(J)
             ELSE
C-------------- Vector variable
                WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14) = WORK1(J)
             ENDIF

        ENDDO

      ENDIF

C---- Check for all date variables (YR,MN,DY,HR) = 0, indicating 
C     possible blank records at end of file (ignoring OSMN).
      ISUM = 0
      DO J=1,MIN(4,OSTIME)
         ISUM = ISUM + IWORK1(1400+OSDVAR(1,J,1))
      ENDDO
      IF( ISUM .EQ. 0 )THEN
         MESS =  BLNK80
         ECODE = 'W52'
         WRITE( MESS,900 ) NUMBER
 900     FORMAT(' All date variables = 0 for record # ',I6,
     &          '; possible blank record. Treated as end-of-file.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         GO TO 300
      ENDIF
      
C---- Check status of the read

      IF( IOFLAG.NE.0 )THEN
C----    ONSITE Read Error, set flags and issue error messages;
C        included statement regarding data types assumed in READ.
         RCSTAT = 1
         NERR = NERR + 1
         MESS =  BLNK80
         ECODE = 'E52'
         I = 1
         WRITE( MESS,1000 ) NUMBER
1000     FORMAT(' Error reading ONSITE record #',I6,
     &          '; Dates are read as INT, ',
     &          'data variables read as REAL.')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )

         MESS =  BLNK80
         ECODE = 'E52'
         WRITE( MESS,1100 ) I,OSDNUM(I),OSFRMT(I)(1:90)
1100     FORMAT('  Read#',I3,'; #Flds=',I2,'; Format: ',A90)
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         IF( LEN_TRIM(OSFRMT(I)) .GT. 90 )THEN
            WRITE( MESS, 1200 ) 
     &             OSFRMT(I)(91:MIN(132,LEN_TRIM(OSFRMT(I))))
1200        FORMAT('  Format (cont.): ',A)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         ENDIF

         IF( NERR.GT.MAXERR )THEN
            GO TO 100
         ENDIF

      ENDIF

C---- Loop on rest of records

      DO I=2,OSDCRD

         IF( OSFRMT(I) .EQ. '*' )THEN
            READ( DEVNUM, *, IOSTAT=IOFLAG,END=200 )
     &                             ( WORK1(J),J=1,OSDNUM(I) )
         ELSE
            READ( DEVNUM,OSFRMT(I),IOSTAT=IOFLAG,END=200 )
     &                             ( WORK1(J),J=1,OSDNUM(I) )
         ENDIF

C------- Check read status
         IF( IOFLAG.NE.0 )THEN
C----       ONSITE Read Error, set flags and issue error messages;
C           included statement regarding data types assumed in READ.

            RCSTAT = 1
            NERR = NERR + 1
            MESS =  BLNK80
            ECODE = 'E52'
            WRITE( MESS,1000 ) NUMBER
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )

            MESS =  BLNK80
            ECODE = 'E52'
            WRITE( MESS,1100 ) I,OSDNUM(I),OSFRMT(I)(1:90)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IF( LEN_TRIM(OSFRMT(I)) .GT. 90 )THEN
               WRITE( MESS, 1200 ) 
     &                OSFRMT(I)(91:MIN(132,LEN_TRIM(OSFRMT(I))))
               CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            ENDIF

            IF( NERR.GT.MAXERR )THEN
               GO TO 100
            ENDIF

         ENDIF

         DO J=1,OSDNUM(I)

            IF( OSDVAR(I,J,2) .LE. 0 )THEN
C------------  Scalar variable
               WORK1(1400+OSDVAR(I,J,1)) = WORK1(J)

            ELSE
C------------- Vector variable
               WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1)-14) = WORK1(J)
            ENDIF

         ENDDO

      ENDDO

      RETURN

C-----Exceeded allowable limit of read errors
 100  RCSTAT = 2
      MESS =  BLNK80
      ECODE = 'E52'
      WRITE( MESS,2000 ) MAXERR
2000  FORMAT(' Exceeded limit for ONSITE data READ errors (',I2,
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
 300  CONTINUE
C --- Check for number of ONSITE records read, if 0, then
C     no ONSITE data available to MERGE, go to statement 
C     200 and set error for premature EOF
      IF( NUMBER .EQ. 0 )THEN
         MESS =  BLNK80
         ECODE = 'I59'
         WRITE( MESS,4000 ) NUMBER
4000     FORMAT(' EOF for ONSITE data after observation #:', I8)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         GO TO 200
      ELSEIF( NUMBER .GT. 0 )THEN
C ---    At least some ONSITE data available, go with 
C        "normal" EOF message 
         RCSTAT = 4
         MESS =  BLNK80
         ECODE = 'I59'
         WRITE( MESS,4000 ) NUMBER
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF

      END

