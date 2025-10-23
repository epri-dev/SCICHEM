      SUBROUTINE OSPRNT( NUMBER )
C=====================================================================**
C          OSPRNT Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To print data stored in 1st hour position of arrays
C              OSSOBS and OSVOBS to report file.  A debugging utility
C
C     Called by:
C
C     Calls to:      ERROR
C
C     Version date:  Dec 15, 1992
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C=======================================================================

C     LOCAL VARIABLES


      IMPLICIT NONE
      
      INTEGER NUMBER, I, J
      INTEGER, SAVE :: IFLAG

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      DATA IFLAG/0/

C   NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C            OUTPUT PRINT FILE
C   IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN WRITING TO OUTPUT
C            FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS
C            ARE MADE TO WRITE TO OUTPUT FILE.

C-----------------------------------------------------------------------
C     INITIALIZE VALUES

      PATH = PATHWD(4)
      LOC  = 'OSPRNT'

C     CHECK IFLAG

      IF( IFLAG.EQ.-1 )THEN
         RETURN
      ENDIF

C     2.  WRITE FIRST RECORD

      IF( OSDNUM(1).EQ.OSTIME )THEN
         WRITE( IOFLAG,OSFRMT(1),IOSTAT=IOFLAG )
     &            ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )

      ELSE
         DO J=OSTIME+1,OSDNUM(1)
           IF( OSDVAR(1,J,2) .LE. 0 )THEN
C             SCALAR VARIABLE
              WORK1(J) = WORK1(1400+OSDVAR(1,J,1))
           ELSE
C             VECTOR VARIABLE
              WORK1(J) = WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
           ENDIF

         ENDDO

         WRITE( IOFLAG,OSFRMT(1),IOSTAT=IOFLAG )
     &        ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     &        ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
      ENDIF

C     CHECK WRITE STATUS

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = ' 3P'
         WRITE( MESS,1000 )
1000     FORMAT(' ERROR WRITING FIRST ONSITE DATA RECORD')
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         IFLAG = -1
         RETURN
      ENDIF

C     LOOP ON REST OF RECORDS

      DO I=2,OSDCRD

         DO J=1,OSDNUM(I)
            IF( OSDVAR(I,J,2) .LE. 0 )THEN
C              SCALAR VARIABLE
               WORK1(J) = WORK1(1400+OSDVAR(I,J,1))
            ELSE
C              VECTOR VARIABLE
               WORK1(J) = WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
            ENDIF
         ENDDO

         WRITE( IOFLAG,OSFRMT(I),IOSTAT=IOFLAG )
     &            ( WORK1(J),J=1,OSDNUM(I) )
C        CHECK WRITE STATUS

         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = ' 3P'
            WRITE( MESS,1500 ) I
1500        FORMAT(' ERROR WRITING ONSITE DATA RECORD #',I3.2)
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IFLAG = -1
            RETURN
         ENDIF

      ENDDO

      RETURN
      END

