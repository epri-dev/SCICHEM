      SUBROUTINE OSDUMP( NUMBER,HOUR,IFLAG )
C=====================================================================**
C          OSDUMP Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Writes data stored in 'hour' position of OSSOBS and
C               OSVOBS to merge output file.
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER NUMBER,HOUR, IFLAG, I, J

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C        NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C                OUTPUT FILE
C        IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN WRITING TO OUTPUT
C                FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS ARE
C                MADE TO WRITE TO OUTPUT FILE.
C        HOUR    POSITION WITHIN OS-SCALAR AND OS-VECTOR MASTER DATA
C                ARRAYS TO BE WRITTEN TO MERGE OUTPUT DEVICE.


C---- Data Initialization
      PATH = PATHWD(5)
      LOC  = 'OSDUMP'

C      CHECK IFLAG

      IF( IFLAG.EQ.-1 )THEN
         RETURN
      ENDIF

C      1.  SWAP DATA TO BUFFER FOR OUTPUT PROCESSING
C      SWAP BUFFER IN FOR PROCESSING

C      IWORK1(1300+52) = OSGDY
C      IWORK1(1300+53) = OSGMO
C      IWORK1(1300+54) = OSGYR
C      IWORK1(1300+55) = OSGHR
C      IWORK1(1300+56) = OSGMI

      IWORK1(1300+52) = OSDAY(HOUR)
      IWORK1(1300+53) = OSMO(HOUR)
      IWORK1(1300+54) = OSYR(HOUR)
      IWORK1(1300+55) = OSHR(HOUR)
      IWORK1(1300+56) = OSMN(HOUR)

      DO I=1,14
         WORK1(1300+I) = OSSOBS(HOUR,I)
      ENDDO

      DO I=30,51
         WORK1(1300+I) = OSSOBS(HOUR,14+I-29)
      ENDDO

      DO I=15,29
         DO J=1,OSML
            WORK2(150+J,I-14) = OSVOBS(HOUR,J,I-14)
         ENDDO
      ENDDO


C---- Write first record
      IF( OSDNUM(1).EQ.OSTIME )THEN
         WRITE( DEV40,111,IOSTAT=IOFLAG )
     &            ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
      ELSE
         DO J=OSTIME+1,OSDNUM(1)

            IF( OSDVAR(1,J,2) .LE. 0 )THEN
C              scalar variable
               WORK1(J) = WORK1(1300+OSDVAR(1,J,1))
            ELSE
C              vector variable
               WORK1(J) = WORK2(150+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
            ENDIF
         ENDDO

         WRITE( DEV40,111,IOSTAT=IOFLAG )
     &        ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
111      FORMAT(8(I8,1X))

C        CHECK WRITE STATUS

         IF( IOFLAG .NE.0 )THEN

            MESS =  BLNK80
            ECODE = 'E65'
            WRITE( MESS,1000 )
1000        FORMAT(' ERROR WRITING ONSITE DATA RECORD')
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IFLAG = -1
            RETURN
         ENDIF

         WRITE( DEV40,222,IOSTAT=IOFLAG )
     &        ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
 222     FORMAT(6(F14.6,1X))

C        Check write status

         IF( IOFLAG .NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E65'
            WRITE( MESS,1000 )
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IFLAG = -1
            RETURN
         ENDIF
      ENDIF

C---- Loop on rest of records

      DO I=2,OSDCRD
         DO J=1,OSDNUM(I)
            IF( OSDVAR(I,J,2) .LE. 0 )THEN
C              scalar variable
               WORK1(J) = WORK1(1300+OSDVAR(I,J,1))
            ELSE
C              vector variable
               WORK1(J) = WORK2(150+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
            ENDIF

         ENDDO

         WRITE( DEV40,222,IOSTAT=IOFLAG )( WORK1(J),J=1,OSDNUM(I))

C        Check write status

         IF( IOFLAG.NE.0 )THEN
            MESS =  BLNK80
            ECODE = 'E65'
            WRITE( MESS,1000 ) IOFLAG
            CALL ERRHDL( NUMBER,PATH,ECODE,LOC,MESS )
            IFLAG = -1
            RETURN
         ENDIF

      ENDDO

      RETURN
      END

