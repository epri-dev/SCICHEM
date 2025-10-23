      SUBROUTINE OSWRTE( NUMBER,ISTAT )
C=====================================================================**
C          OSWRTE Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To write the data stored in 1st hour
c               position of OSSOBS and OSVOBS to OS-OQA file.
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER NUMBER, I, J, ISTAT
      INTEGER, SAVE :: IFLAG

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C      NUMBER  Counter for observation being processed to output file
C      IFLAG   Initially = 0;  if errors occur in writing to output
C              file, reset to -1.  If IFLAG = -1, no more attempts are
C              made to write to output file.


C---- Data Initializations
      DATA IFLAG/0/
      PATH = PATHWD(4)
      LOC  = 'OSWRTE'
      
C---- Initialize IOFLAG for I/O errors 
      IOFLAG = 0

C---- Check IFLAG; if it is -1, then an error occurred and no more
C     attempts are made to write data to the output file

      IF( IFLAG.EQ.-1 )THEN
C----   Set ISTAT = 1 to indicate error condition
        ISTAT = 1
        RETURN
      ENDIF

C---- Swap data to buffer for output processing
C      Swap buffer in for processing

      IWORK1(1300+52) = OSGDY
      IWORK1(1300+53) = OSGMO
      IWORK1(1300+54) = OSGYR
      IWORK1(1300+55) = OSGHR
      IWORK1(1300+56) = NINT( OSQA(56,2) )

      DO I=1,14
         WORK1(1300+I) = OSSOBS(1,I)
      ENDDO

      DO I=30,34
         WORK1(1300+I) = OSSOBS(1,14+I-29)
      ENDDO

      DO I=15,29
         DO J=1,OSNL
            WORK2(150+J,I-14) = OSVOBS(1,J,I-14)
         ENDDO
      ENDDO

C---- Write first record

      IF( OSDNUM(1) .EQ. OSTIME )THEN
         IF( OSFRMT(1) .EQ. '*' )THEN
            WRITE( DEV32,*, IOSTAT=IOFLAG )
     &             ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
         ELSE
            WRITE( DEV32,OSFRMT(1),IOSTAT=IOFLAG )
     &             ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
         ENDIF
      ELSE
         DO J=OSTIME+1,OSDNUM(1)
            IF( OSDVAR(1,J,2) .LE. 0 )THEN
C------------- Scalar variable
               WORK1(J) = WORK1(1300+OSDVAR(1,J,1))

            ELSE
C------------- Multi-level variable
               WORK1(J) = WORK2(150+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
            ENDIF

         ENDDO
         IF( OSFRMT(1) .EQ. '*' )THEN
            WRITE( DEV32,*, IOSTAT=IOFLAG )
     &           ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME ),
     &           ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
         ELSE         
            WRITE( DEV32,OSFRMT(1),IOSTAT=IOFLAG )
     &           ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME ),
     &           ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
         ENDIF
      ENDIF

C---- Check write status

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         ECODE = 'E53'
         WRITE( MESS,1000 ) NUMBER
1000     FORMAT(' ERROR WRITING ONSITE DATA RECORD #',I5 )
         CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
         IFLAG = -1
C----    Set ISTAT = 1 to indicate error condition
         ISTAT = 1
         RETURN
      ENDIF

C---- Loop on rest of records

      DO I=2,OSDCRD
         DO J=1,OSDNUM(I)
            IF( OSDVAR(I,J,2) .LE. 0 )THEN
C------------- Scalar variable
               WORK1(J) = WORK1(1300+OSDVAR(I,J,1))
            ELSE
C------------- Multi-level variable
               WORK1(J) = WORK2(150+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
            ENDIF

         ENDDO

         IF( OSFRMT(I) .EQ. '*' )THEN
            WRITE( DEV32,*, IOSTAT=IOFLAG )
     &                          ( WORK1(J),J=1,OSDNUM(I) )
         ELSE
            WRITE( DEV32,OSFRMT(I),IOSTAT=IOFLAG )
     &                          ( WORK1(J),J=1,OSDNUM(I) )
         ENDIF

C------- Check write status
         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E53'
            WRITE( MESS,1000 ) NUMBER
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IFLAG = -1
C----       Set ISTAT = 1 to indicate error condition
            ISTAT = 1
            RETURN
         ENDIF

      ENDDO
      
C---- Set IFLAG = 2 to indicate successful write to the
C     calling subroutine, OSQACK
      IFLAG = 2

      RETURN
      END

