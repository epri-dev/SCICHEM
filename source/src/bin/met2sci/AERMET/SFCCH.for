      SUBROUTINE SFCCH(HR, WD4CHR, KTEST)
C=====================================================================**
C          SFCCH Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To determine the wind sector from the wind direction
C               (WD4CHR) for the hour (HR) and set the Bowen ratio,
C               albedo, and surface roughness length corresponding for
C               the wind sector and month currently being processed.
C
C     Arguments passed: HR       INTEGER     Hour
C                       KTEST    INTEGER     Status check on data
C                       WD4CHR   REAL        Wind direction
C
C     Initial release:  December 1992
C                       Pacific Environmental Services, Inc. (PES)
C                       Research Triangle Park, NC
C
C     Revision history:
C        06/96 - Changed the call to SFCCH to include an argument
C                for wind direction
C
C-----------------------------------------------------------------------

C---- Variable declarations

      IMPLICIT NONE
      
      INTEGER  KTEST, HR, NN, NSEC, JJJ
      REAL     WD4CHR
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

      PATH = 'METPREP '
      LOC = ' SFCCH'
      JJJ = MPYR*10000 + MPCMO*100 + MPCDY


C---- Find wind sector for the given hour: the lower bound is
C     INCLUDED in the sector and the upper bound is EXCLUDED
C     from the sector.  Therefore, we must trap on a sector
C     with an upper bound of 360.0.

      KTEST = 0
      NN = 0
   30 NN = NN + 1
      IF( OSWDS(NN,1).LT.OSWDS(NN,2) )THEN
         IF( WD4CHR .GE. OSWDS(NN,1) .AND.
     &       WD4CHR .LT. OSWDS(NN,2) )THEN
            NSEC=NN
            GO TO 50
         ENDIF

         IF( OSWDS(NN,2).EQ.360. )THEN
            IF( WD4CHR .GE. OSWDS(NN,1) .AND.
     &          WD4CHR .LE. OSWDS(NN,2) )THEN
               NSEC=NN
               GO TO 50
            ENDIF
         ENDIF

      ELSE
         IF( WD4CHR.LT.OSWDS(NN,1).AND.WD4CHR.LT.OSWDS(NN,2) .OR.
     &       WD4CHR.GE.OSWDS(NN,1).AND.WD4CHR.GE.OSWDS(NN,2) )THEN
            NSEC=NN
            GO TO 50
         ENDIF

      ENDIF

      IF( NN .LT. OSNWDS) GOTO 30

C---- If the program reaches this point, there is an error in the
C     range of wind sectors.

      MESS =  BLNK80
      ECODE='E77'
      WRITE( MESS,2000 ) HR
 2000 FORMAT(' Cannot determine wind direction sector for HR: ',I3.2)
      CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)
      KTEST = -1                                                       ! dtb #109 02039
      RETURN

C---- Assign albedo, bowen ratio, and surface roughness length
C     based on the month and wind sector.

   50 KTEST = 2
      ALBEDO(HR) = OSSFC(MPCMO,NSEC,1)
      BOWEN(HR)  = OSSFC(MPCMO,NSEC,2)
      Z0(HR)     = OSSFC(MPCMO,NSEC,3)

      RETURN
      END

