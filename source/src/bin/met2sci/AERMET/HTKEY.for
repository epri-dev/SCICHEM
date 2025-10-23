      SUBROUTINE HTKEY( ITEST )
C=====================================================================**
C          HTKEY Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To check heights for multi-level data
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C---- Variable Declarations
      INTEGER ITEST,I,J,K,L, JMPDATE
      REAL    RMISS

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      PATH = 'METPREP   '
      LOC  = 'HTKEY '
      ITEST = 2

C---- Compute date variable as YYYYMMDD for error reporting
      JMPDATE = MPYR*10000 + MPCMO*100 + MPCDY
      
      IF( OSNL .GT. 0 )THEN
         RMISS = OSQA(15,2)
         
         HOUR_LOOP: DO I = 1,24
           LVL_LOOP: DO J = 1,OSNL
           
            IF( J.GT.1 )THEN
C ----         Check for OSHT's decreasing with "height";
C              must be in order of increasing height
               IF( (ABS( OSVOBS(I,J,1)-RMISS ) .GT. 0.01 ) .AND.
     &                   OSVOBS(I,J,1).LT.OSVOBS(I,J-1,1) )THEN  
C ----            OSHT decreases with height; issue warnings,
C                 and set all data for all levels to missing
C                 for this hour
C
C ---             Check for use of OSHEIGHTS or HTnn; 
C                 if OSHEIGHTS are used then issue informational
C                 message, but if HTnn are used then issue a
C                 warning message and proceed to set heights
C                 to and data to missing;
C                 NOTE: this latter condition should not occur
C                 since it should have been addressed in Stage 1
C
                  IF( STATUS(4,16) .EQ. 2 )THEN
C ---                OSHEIGHTS keyword used
                     MESS =  BLNK80
                     ECODE = 'I27'
                     WRITE( MESS,4100 ) J, J-1, I
4100                 FORMAT(' ONSITE hgt lvl HT',I2.2,' lower ',
     &                      'than lvl HT',I2.2,' for HR ',I2.2,
     &                      '; however, OSHEIGHTS will be used ',
     &                      'to set ONSITE heights.')
                     CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )
                  ELSE
C ---                HTnn values used; this still should not 
C                    happen since heights and data should have
C                    been set to missing in Stage 1 without
C                    OSHEIGHTS keyword
                     MESS =  BLNK80
                     ECODE = 'W50'
                     WRITE( MESS,4200 ) J, J-1, OSHT(J), OSHT(J-1)
4200                 FORMAT(' ONSITE height level ',I2,' is lower ',
     &                      'than level ',I2,'; ',F7.2,' vs. ',F7.2,
     &                      ' meters:')
                     CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )
C ---                HTnn values used, issue warning and set
C                    data to missing
                     MESS =  BLNK80
                     ECODE = 'W50'
                     WRITE( MESS,4300 )
4300                 FORMAT('  All heights and data will be set to ',
     &                     'missing for this hour!')
                     CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )
C ---                Issue additional warning since old MERGE file
C                    may have been used
                     MESS =  BLNK80
                     ECODE = 'W50'
                     WRITE( MESS,4300 )
4400                 FORMAT('  This should have been addressed in',
     &                       ' Stage 1; possibly an old MERGE file',
     &                       ' is being used!')
                     CALL ERRHDL(JMPDATE,PATH,ECODE,LOC,MESS )
C ---                Set all data to missing
                     DO K = 15,29
                        DO L = 1,OSNL
                           OSVOBS(I,L,K-14) = OSQA(K,2)
                        ENDDO
                     ENDDO
                  ENDIF
                  EXIT
               ENDIF
            ENDIF

           ENDDO LVL_LOOP
         ENDDO HOUR_LOOP
         
      ENDIF

      RETURN
      END

