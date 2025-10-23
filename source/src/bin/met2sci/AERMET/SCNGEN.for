      SUBROUTINE SCNGEN( KOUNT,FLAG,LOC1,IYR,IMO,IDY,IHR,N1,ST1 )
C=====================================================================**
C        SCNGEN Module of the AERMET Meteorological Preprocessor
C
C     Purpose: Generates a report of magnetic tape contents
C
C     Called by:     GETSDG, GETSFC
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------
C

      IMPLICIT NONE
      
      INTEGER    IYR,IMO,IDY,IHR,N1,NN,JULIAN,IOST
      CHARACTER  LOC1*8,LOC2*8,ST1*79
      LOGICAL    FLAG
C
      INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C
C     KOUNT      A COUNTER
C     FLAG       FLAG INDICATING IF THIS IS THE FIRST ACCESS TO THIS
C                 SUBROUTINE ON THIS PATHWAY
C     LOC1       CURRENT RECORD STATION ID
C     LOC2       PREVIOUS RECORD STATION ID
C     IYR,IMO    PREVIOUS RECORD YEAR, MONTH, DAY AND HOUR
C      IDY,IHR
C     N1         STARTING LOCATION IN IWORK ARRAY TO STORE DATA -
C                 VARIES BY PATH
C     ST1        STRING RETURNED TO CALLING PROGRAM WITH SCAN DATA
C
C-----------------------------------------------------------------------
C---- The first part of the IF- block is executed only for the first call
C     to this routine by each data type
C
      IF( FLAG )THEN
         FLAG = .FALSE.
         LOC2 = LOC1
         IWORK1(N1)   = IYR
         IWORK1(N1+1) = IMO
         IWORK1(N1+2) = IDY
         IWORK1(N1+3) = IHR
         IWORK1(N1+4) = JULIAN(IYR,IMO,IDY)
         IWORK1(N1+11) = 0
         IWORK1(N1+12) = 0
         IWORK1(N1+13) = 0
         IWORK1(N1+14) = 0
         WRITE(ST1,98,ERR=900,IOSTAT=IOST)LOC2,
     &        (IWORK1(NN),NN=N1,N1+4), (IWORK1(NN),NN=N1+11,N1+14)
      ELSE

C------- This part of the IF- block is executed for every subsequent
C        call.  If the station or year changes, then a report is
C        written; otherwise the most 'recent' station/date information
C        is retained
C
         IF( LOC2.NE.LOC1 .OR. IWORK1(N1).NE.IYR )THEN
            KOUNT = KOUNT + 1
            WRITE(ST1,98,ERR=900,IOSTAT=IOST)LOC2,
     &           (IWORK1(NN),NN=N1,N1+4), (IWORK1(NN),NN=N1+11,N1+14)
            LOC2 = LOC1
            IWORK1(N1)   = IYR
            IWORK1(N1+1) = IMO
            IWORK1(N1+2) = IDY
            IWORK1(N1+3) = IHR
            IWORK1(N1+4) = JULIAN(IYR,IMO,IDY)
            IWORK1(N1+11) = 0
            IWORK1(N1+12) = 0
            IWORK1(N1+13) = 0
            IWORK1(N1+14) = 0

         ELSE
            IWORK1(N1+11) = IMO
            IWORK1(N1+12) = IDY
            IWORK1(N1+13) = IHR
            IWORK1(N1+14) = JULIAN(IYR,IMO,IDY)
            WRITE(ST1,98,ERR=900,IOSTAT=IOST)LOC2,
     &           (IWORK1(NN),NN=N1,N1+4), (IWORK1(NN),NN=N1+11,N1+14)
         ENDIF
      ENDIF
      RETURN

C---- Jump to here if an error condition is encountered writing data
  900 CONTINUE
      WRITE(ST1,99) IOST
      RETURN

C---- Format statements
   98 FORMAT(10X,A8,4(1X,I2),1X,I4,4X,3(1X,I2),1X,I4)
   99 FORMAT(' ERROR WRITING THE SCAN REPORT - I/O STATUS = ',I6)

      END

