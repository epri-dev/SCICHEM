      SUBROUTINE MANDEL(NUML1,IPRES)
C=====================================================================**
C          MANDEL Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  Deletes mandatory levels if within 1% of a significant
C             level (prevents strong shears during QA.  The NWS
C             interpolates to these levels, so no information is lost.
C
C     NOTE:  The work arrays still use -9999.0 as the missing value
C            indicators.  The missing value indicators defined by
C            UAQA(-,2) will be substituted after the sounding is checked
C
C   Called by: GET620
C
C   Arguments:
C      NUML1    Number of levels remaining after the deletions
C      IPRES    Number of levels deleted
C
C   Initial release:  December 15, 1992
C
C   Revision History:
C      11/30/94
C        -  Made into a separate subroutine from an ENTRY point.
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      
      REAL    PLEV(19), XPCHK1, XPCHK2
      INTEGER I, J
      INTEGER IPWRK,IPRES,NACT,NUML1,L

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      DATA PLEV/1000.,950.,900.,850.,800.,750.,700.,650.,600.,
     &           550.,500.,450.,400.,350.,300.,250.,200.,150.,100./
      DATA PATH/'UPPERAIR  '/, LOC/'MANDEL'/
C
C *** Varaible descriptions
C
C     NACT     Number of levels before any deletions
C
C ***

      IPRES = 0
      NACT  = NUML1

      LOOP_LVLS: DO J=1,NACT-1
      
  10     IF( NUML1.LE.J ) CYCLE LOOP_LVLS
         XPCHK2 = 100.0

         IF( (WORK2(J,1).LT.-9000.0) .OR.
     &       (WORK2(J+1,1).LT.-9000.0) )THEN
            CYCLE LOOP_LVLS
         ELSE
            XPCHK1 = WORK2(J,1)-WORK2(J+1,1)
         ENDIF

         IF( J.LT.2 )THEN
            CYCLE LOOP_LVLS
         ELSEIF( J.GE.2 .AND. WORK2(J-1,1).LT.-9000.0 )THEN
            CYCLE LOOP_LVLS
         ELSE
            XPCHK2 = WORK2(J-1,1)-WORK2(J,1)
         ENDIF

         IF( (XPCHK1.LE.(0.01*WORK2(J,1))) .OR.
     &       (XPCHK2.LE.(0.01*WORK2(J,1))) )THEN

C---------- Delete the level from the sounding;
C           (Note: 19 = # of mandatory levels in array PLEV)
            DO L=1,19
               IWORK1(1) = NINT(WORK2(J,1))*10
               IPWRK     = NINT(PLEV(L))*10
               IF( IWORK1(1) .EQ. IPWRK )THEN
                  NUML1 = NUML1 - 1
                  IPRES = IPRES + 1
                  DO I = J,NUML1
                     WORK2(I,1) = WORK2(I+1,1)
                     WORK2(I,2) = WORK2(I+1,2)
                     WORK2(I,3) = WORK2(I+1,3)
                     WORK2(I,4) = WORK2(I+1,4)
                     WORK2(I,5) = WORK2(I+1,5)
                     WORK2(I,6) = WORK2(I+1,6)
                  ENDDO
                  MESS =BLNK96
                  WRITE(MESS,40) UAGYR,UAGMO,UAGDY,UAGHR,PLEV(L)
                  CALL ERRHDL(IPRES,PATH,'I32',LOC,MESS)
                  GO TO 10
               ENDIF
            ENDDO
         ENDIF

      ENDDO LOOP_LVLS

 40   FORMAT(' ',3I2,'/',I2,'; ',F6.0,'mb - MANDATORY LEVEL DELETED')

      RETURN
      END

