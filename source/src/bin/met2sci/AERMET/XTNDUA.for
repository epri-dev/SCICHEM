      SUBROUTINE XTNDUA ( SDGTOP, NUM, NLEV, DTDZ, XTENDED )
C=====================================================================**
C          XTNDUA Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To extend an upper air sounding to 5000 meters; only
C               one additional level is added (at 5000 m).
C
C     Assumptions:
C
C     Called by:  MPPBL
C
C     Calling Arguments:
C        SDGTOP    Real      In     Height of the top of the sdg
C        NUM       Integer   In     Sounding number for the day
C        NLEV      Integer   In     # levels in the sounding
C        XTENDED   Logical   Out    Was sounding extended?
C
C     Other I/O:
C        UATOP     Integer   In     Maximum height for soundings
C        HT()      Real      I/O    Sounding heights
C        PTMP()    Real      I/O    Sounding potential temperature
C
C     Called by:  MPPBL
C
C     Calls to:   ERROR
C
C     Initial Release:  October 1996
C
C     Developed by: Pacific Environmental Services, Inc. (PES)
C                   Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------
C
C---- Data Declarations


      IMPLICIT NONE
      
      LOGICAL  XTENDED
      
      INTEGER  JJJ, K, NLEV, NUM
      REAL     SDGTOP, PTZDZ, DTDZ, DT, POTEMP, ZDZ

      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initializations

      PATH = 'METPREP'
      LOC  = 'XTNDUA'
      JJJ  = MPYR*10000 + MPCMO*100 + MPCDY

C     XTENDED is initialized in the calling program, MPPBL

C---- Does the sounding exceed 600 meters?  If so, go ahead and
C     compute the potential temperature gradient and extend
C     the sounding

      ZDZ = SDGTOP - 500.0
      IF( ZDZ .GE. 100.0 )THEN

         DO K = NLEV-1, 1, -1

            IF( SDGTOP - HT(NUM,K) .GE. 500.0 )THEN
C------------- Compute the potential temperature at ZDZ, PTZDZ
C              Level 'K' will be the level just below ZDZ

               PTZDZ = PTMP(NUM,K) + (ZDZ - HT(NUM,K)) /
     &                               (HT(NUM,K+1) - HT(NUM,K)) *
     &                               (PTMP(NUM,K+1) - PTMP(NUM,K))
               GO TO 1
            ENDIF

         ENDDO

C------- This section of code is executed if two levels could not be
C        located to compute potential temperature at ZDZ

         MESS =  BLNK80
         ECODE = 'W72'
         WRITE( MESS,50 )
  50     FORMAT( ' CANNOT EXTEND A.M. SOUNDING  ')
         CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
         GO TO 100

C------- The program continues here if two levels of data are located
C        to compute the gradient

   1     DTDZ = (PTMP(NUM,NLEV) - PTZDZ ) / 500.0
         DT   = ( UATOP - HT(NUM,NLEV) ) * DTDZ
         POTEMP = PTMP(NUM,NLEV) + DT

         PTMP(NUM,NLEV+1) = POTEMP
         HT(NUM,NLEV+1)   = UATOP

         XTENDED = .TRUE.
Cjop     MESS =  BLNK80
Cjop     ECODE = 'W73'
Cjop     WRITE( MESS,55 ) SDGTOP, DTDZ
Cjop55   FORMAT( ' SOUNDING EXTENDED FROM ',F6.0, ' USING ',F7.3,' K/m')
Cjop     CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )

      ELSE
C------- Insufficient height to extend sounding
         MESS =  BLNK80
         ECODE = 'W72'
         WRITE( MESS,60 )
  60     FORMAT( ' TOP OF SOUNDING TOO LOW TO EXTEND SOUNDING ')
         CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )

      ENDIF

  100 CONTINUE

      RETURN
      END

