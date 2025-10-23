      SUBROUTINE SFCCH2(HR, WD4CHR, KTEST)
C=====================================================================**
C          SFCCH2 Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To determine the wind sector from the wind direction
C               (WD4CHR) for the hour (HR) and set the surface roughness
C               length corresponding for the wind sector and month
C               currently being processed.
C
C               This subroutine is specificall for the secondary
C               surface characteristics
C
C     Arguments passed: HR       INTEGER     Hour
C                       KTEST    INTEGER     Status check on data
C                       WD4CHR   REAL        Wind direction
C
C     Initial release:  March 2009
C                       MACTEC Engineering and Consulting
C                       Research Triangle Park, NC
C
C     Revision history:
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
      LOC = ' SFCCH2'
      JJJ = MPYR*10000 + MPCMO*100 + MPCDY


C---- Find wind sector for the given hour: the lower bound is
C     INCLUDED in the sector and the upper bound is EXCLUDED
C     from the sector.  Therefore, we must trap on a sector
C     with an upper bound of 360.0.

      KTEST = 0
      NN = 0
   30 NN = NN + 1
      IF( OSWDS2(NN,1).LT.OSWDS2(NN,2) )THEN
         IF( WD4CHR .GE. OSWDS2(NN,1) .AND.
     &       WD4CHR .LT. OSWDS2(NN,2) )THEN
            NSEC=NN
            GO TO 50
         ENDIF

         IF( OSWDS2(NN,2).EQ.360. )THEN
            IF( WD4CHR .GE. OSWDS2(NN,1) .AND.
     &          WD4CHR .LE. OSWDS2(NN,2) )THEN
               NSEC=NN
               GO TO 50
            ENDIF
         ENDIF

      ELSE
         IF( WD4CHR.LT.OSWDS2(NN,1).AND.WD4CHR.LT.OSWDS2(NN,2) .OR.
     &       WD4CHR.GE.OSWDS2(NN,1).AND.WD4CHR.GT.OSWDS2(NN,2) )THEN
            NSEC=NN
            GO TO 50
         ENDIF

      ENDIF

      IF( NN .LT. OSNWDS2) GOTO 30

C---- If the program reaches this point, there is an error in the
C     range of wind sectors.

      MESS =  BLNK80
      ECODE='E77'
      WRITE( MESS,2000 ) HR
 2000 FORMAT(' Cannot determine SECONDARY wind direction sector for ',
     &          'HR: ',I3.2)
      CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)
      KTEST = -1
      RETURN

C---- Assign surface roughness length based on the month and wind
C     sector.
C
C     Note that the secondary characteristics are only needed for the
C     roughness length, hence the albedo and Bowen ratio are commented
C     out here.

   50 KTEST = 2
C      ALBEDO(HR) = OSSFC2(MPCMO,NSEC,1)
C      BOWEN(HR)  = OSSFC2(MPCMO,NSEC,2)
      Z0(HR)     = OSSFC2(MPCMO,NSEC,3)

      RETURN
      END

