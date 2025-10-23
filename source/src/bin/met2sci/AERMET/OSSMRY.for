      SUBROUTINE OSSMRY (DEVICE)
C=====================================================================**
C          OSSMRY Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Print a report of all on-site data that is either
C               user-specified or defaulted.
C
C-----------------------------------------------------------------------

C---- Data Declarations

      IMPLICIT NONE
      
      INTEGER :: DEVICE, I

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'


      IF( OSSTAT.EQ.0 ) RETURN

      PGNUM = PGNUM + 1
      CALL BANNER( PGNUM,1,VERSNO,DEVICE )
      
      WRITE (DEVICE,90)
  90  FORMAT(/,20X,'THE FOLLOWING ON-SITE VALUES ARE IN EFFECT'//)

C---- Number of observations per hour for sub-hourly data

      WRITE (DEVICE,100) OSAVG
 100  FORMAT(' Number of OBS/HOUR: ',//,I6//)

C---- Threshold wind speed

      WRITE (DEVICE,101) OSCALM
 101  FORMAT(' Threshold wind speed (m/s): ',//,F8.2)

C---- On-site profile heights

      IF( OSNL .LE. 0 )THEN
         WRITE(DEVICE,133)
      ELSE
         IF( STATUS(4,16) .EQ. 2 )THEN
            WRITE(DEVICE,102)
         ELSE
            WRITE(DEVICE,103)
         ENDIF
         IF( L_HgtsVary )THEN
            WRITE(DEVICE,104)
         ENDIF
         WRITE(DEVICE,105) (OSHT(I),I=1,OSNL)
      ENDIF

 102  FORMAT(//,' Heights for tower data (m), based on ',
     &           'OSHEIGHTS keyword: ',/)
 103  FORMAT(//,' Heights for tower data (m), based on ',
     &           'HTnn fields in data file: ',/)
 104  FORMAT('   ONSITE tower heights vary within the data file!',/
     &      '   Heights listed below are based on the last hour read.'/)
 105  FORMAT(10F8.2)
 133  FORMAT(//'  No tower heights specified. ')


C-----Delta_temperature

      IF( OSNDT .GT. 0 )THEN
         WRITE (DEVICE,106)
         DO I=1,OSNDT
            WRITE (DEVICE,107) I,OSLL(I),OSUL(I)
         ENDDO
      ENDIF

 106  FORMAT(//,' Temperature difference heights (m):',/,
     &       5X,'Index',7X,'Lower',5X,'Upper')
 107  FORMAT(7X,I2,5X,F8.2,2X,F8.2)


      RETURN
      END  

