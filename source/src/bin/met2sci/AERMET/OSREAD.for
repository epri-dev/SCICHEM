        SUBROUTINE OSREAD( PATHN,NUMBER,HOUR,TEST )
C=======================================================================
C          OSREAD Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  To read onsite data stored in the file of merged data.
C
C     CALLED BY:
C
C     CALLS TO:      ERROR
C
C     Initial Release: December 1992
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

C     LOCAL VARIABLES


      IMPLICIT NONE

      INTEGER  I, J
      INTEGER  PATHN,NUMBER,HOUR,TEST
      INTEGER, SAVE :: IFLAG
      REAL     RMISS
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      DATA IFLAG/0/

C    PATHN    PATHWAY NUMBER IN METPROCESSOR SYSTEM
C             5 = MR
C             6 = MP
C    NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C             OUTPUT FILE
C    IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN READING THE MERGE
C             FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS ARE
C             MADE TO READ MERGE FILE.
C    HOUR    POSITION WITHIN OS-SCALAR AND OS-VECTOR MASTER DATA
C             ARRAYS THAT DATA BELONG.
C    TEST    STATUS OF PROCESS
C             1 = ERROR HAS OCCURRED
C             2 = ALL OK
C             3 = EOF FOUND

C-----------------------------------------------------------------------
C   INITIALIZE VALUES

      PATH = PATHWD(PATHN)
      LOC  = 'OSREAD'
      TEST = 0

C     CHECK IFLAG

      IF( IFLAG.EQ.-1 )THEN
         RETURN
      ENDIF

C     SET ALL VALUES IN 'BUFFER' TO MISSING FLAG VALUES

C     FIRST THE SCALAR VARIABLES

      DO I=1,51
         IF( I.EQ.34 )THEN
            RMISS = OSTSKY(2)
         ELSE
            RMISS = OSQA(I,2)
         ENDIF
         WORK1(1400+I) = RMISS
      ENDDO

C     THEN THE VECTOR VARIABLES

      DO I=15,29
         DO J=1,OSNL
            WORK2(100+J,I-14) = OSQA(I,2)
         ENDDO
      ENDDO

C     FINALLY, THE INTEGER VARIABLES

      DO I=52,56
         IWORK1(1400+I) = NINT( OSQA(I,2) )
      ENDDO

C-----1.  READ FIRST RECORD

      IF( OSDNUM(1).EQ.OSTIME )THEN
         READ( DEV40,111,IOSTAT=IOFLAG,END=100 )
     1            ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
      ELSE
         READ( DEV40,111,IOSTAT=IOFLAG,END=100 )
     1        ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
 111     FORMAT(8(I8,1X))
         READ( DEV40,222,IOSTAT=IOFLAG,END=100 )
     2        ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
 222     FORMAT(6(F14.6,1X))

      ENDIF

C-----CHECK READ STATUS

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = 'E73'
         WRITE( MESS,1000 )
1000     FORMAT(' ERROR reading first MERGE''d ONSITE data record!')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         TEST = 1
         IFLAG = -1
         RETURN

      ELSEIF( OSDNUM(1) .GT. OSTIME )THEN
         DO J=OSTIME+1,OSDNUM(1)

            IF( OSDVAR(1,J,2) .LE. 0 )THEN
C              SCALAR VARIABLE
               WORK1(1400+OSDVAR(1,J,1)) = WORK1(J)
            ELSE
C              VECTOR VARIABLE
               WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14) = WORK1(J)
            ENDIF
         ENDDO
      ENDIF

C-----DEFINE HOUR

      HOUR = IWORK1(1400+55)

C-----TEST HOUR

      IF( HOUR.EQ.OSQA(55,2) )THEN
         MESS =  BLNK80
         ECODE = 'E76'
         WRITE( MESS,2000 )
2000     FORMAT(' HOUR IS DEFINED AS MISSING')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         TEST = 1
         IFLAG = -1
         RETURN
      ENDIF

      IF( HOUR.LT.1 .OR. HOUR.GT.24 )THEN
         MESS =  BLNK80
         ECODE = 'E74'
         WRITE( MESS,2500 )
2500     FORMAT(' OS DATA NOT ON 1 - 24 HOUR CLOCK')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         TEST = 1
         IFLAG = -1
         RETURN
      ENDIF

      TEST = 2

C---- Check for OSMN with OSAVG=1 (no subhourly data);
C     set OSMN to missing if this occurs
      IF( OSAVG .EQ. 1 .AND.
     &          ABS( IWORK1(1456) - NINT( OSQA(56,2)) ) .GT. 0.01 )THEN
         IWORK1(1456) = NINT( OSQA(56,2) )
      ENDIF

C-----LOOP ON REST RECORDS

      DO I=2,OSDCRD
         READ( DEV40,222,IOSTAT=IOFLAG,END=100 )
     1            ( WORK1(J),J=1,OSDNUM(I) )

C        CHECK READ STATUS

         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E73'
            WRITE( MESS,1500 ) I
1500        FORMAT(' ERROR READING MERGED ONSITE DATA RECORD #', I2.2)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            TEST = 1
            IFLAG = -1
            RETURN
         ELSE
            DO J=1,OSDNUM(I)
               IF( OSDVAR(I,J,2) .LE. 0 )THEN
C                 SCALAR VARIABLE
                  WORK1(1400+OSDVAR(I,J,1)) = WORK1(J)
               ELSE
C                 VECTOR VARIABLE
                  WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1) - 14)=WORK1(J)
               ENDIF

            ENDDO

         ENDIF

      ENDDO

      RETURN

100   TEST = 3
      RETURN
      END

