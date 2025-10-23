      SUBROUTINE TEST( ISTAGE )
C=====================================================================**
C          TEST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the 'STATUS' array to determine whether
C               the input is available on each pathway (without errors)
C               and sets the pathway status word accordingly.
C
C     Called by: SETUP
C
C     Initial Release: December 1992
C
C     Revisions History
C        02/20/09: MACTEC Engineering & Consulting
C                - added test for 1-min ASOS 
C
C-----------------------------------------------------------------------
C       JBSTAT,      STATUS WORDS THAT ARE SET BASED ON FINAL
C       UASTAT,      ASSESSMENT OF USER DEFINED SETUP DATA.
C       SFSTAT,        -1  ERROR CONDITION SET FOR THIS PATH
C       OSSTAT          0  NULL PATHWAY
C                       1  EXTRACT DATA ONLY
C                       2  QA DATA ONLY
C                       3  EXTRACT AND QA, BUT DO NOT MERGE
C                       4  MERGE ONLY
C                       5  EXTRACT AND MERGE, BUT DO NOT QA
C                       6  QA AND MERGE ONLY
C                       7  EXTRACT, QA AND MERGE
C                          (5, 6, and 7 should not be allowed)
C
C       MRSTAT       MERGE STATUS
C                       0  NO MERGE (INITIAL STATE)
C                       2  MERGE SUBROUTINE CALLED
C-----------------------------------------------------------------------
C
C---- Local variables


      IMPLICIT NONE
      
      INTEGER ISTAT, ISTAGE, I, J

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C---- Initialize values
      LOC  = '  TEST'


C---- Process 'JOB' pathway

      JBSTAT = 0
      ISTAT  = 0
      PATH = 'JOB       '

C---- The next loop will only catch an error if the keyword was
C     spelled correctly, but the record had errors

      DO I=1,NKEYWD
         IF( STATUS(1,I).EQ.1 ) ISTAT = ISTAT + 1
      ENDDO

      IF( ISTAT.GT.0 )THEN
         MESS =  BLNK80
         ECODE = 'E24'
         WRITE( MESS,1000 )
1000     FORMAT(' ERROR(S) DETECTED ON JOB PATHWAY')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         SETERR = .TRUE.
         ISTAT  = 1
      ENDIF

C---- Is a message file open?  If it is not, the messages are written
C     to the screen and the report summary does not contain the summary
C     table.  No data are processed but files are opened.

      IF( STATUS(1,2).NE.2 )THEN
         MESS =  BLNK80
         ECODE = 'E24'
         WRITE( MESS,1500 )
1500     FORMAT(' SUMMARY: THE MESSAGE FILE IS NOT DEFINED')
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         SETERR = .TRUE.
         ISTAT = 1
      ENDIF

      IF(ISTAT.EQ.0) JBSTAT = 1

C---- Process pathways UA(2), SF(3) and OS(4)

      DO J=2,4

C------- Initialize pathway's status
         IF( J.EQ.2 )THEN
            UASTAT = 0
         ELSEIF( J.EQ.3 )THEN
            SFSTAT = 0
         ELSEIF( J.EQ.4 )THEN
            OSSTAT = 0
         ENDIF

C------- Initialize error status indicator and look for any errors
C        anywhere on this pathway

         ISTAT  = 0
         DO I=1,NKEYWD
            IF( STATUS(J,I).EQ.1 ) ISTAT = ISTAT + 1
         ENDDO

         IF( ISTAT .GT. 0 )THEN
            MESS =  BLNK80
            IF( J.EQ.2 )THEN
               ECODE = 'E25'
               UASTAT = -1
               SETERR = .TRUE.
            ELSEIF( J.EQ.3 )THEN
               ECODE = 'E26'
               SFSTAT = -1
               SETERR = .TRUE.
            ELSEIF( J.EQ.4 )THEN
               ECODE = 'E27'
               OSSTAT = -1
               SETERR = .TRUE.
            ENDIF
            WRITE( MESS,1010 ) PATHWD(J)
1010        FORMAT( ' ERROR(S) DETECTED ON ',A10,' PATHWAY')
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

C------- For OS-pathway, call routine to perform           ---- CALL OSTEST
C        various checks to insure user has
C        formats for all reads, etc.

         IF( J.EQ.4 .AND. STABLK(4) .EQ. 2 )THEN
            ISTAT = 0
            CALL OSTEST( ISTAGE,ISTAT )
            IF( ISTAT.EQ.1 )THEN
               OSSTAT = -1
               SETERR = .TRUE.
            ENDIF
         ENDIF

         IF( ISTAT.EQ.1 )THEN
C---------- An error has been encountered in the keywords; no need
C           to check processing requirements
            CONTINUE

         ELSE
C---------- See if extract is required in this block
            IF( STATUS(J,4) .EQ. 0 )THEN
C               No 'DATA' card - assume no data extraction
                IF( (J .EQ. 2)  .OR.  (J .EQ. 3) )THEN
C                  Recall: there is no data extraction for on-site data
                   MESS =  BLNK80
                   IF( J.eq.2 )THEN
                      ECODE = 'I25'
                   ELSEIF( J.eq.3 )THEN
                      ECODE = 'I26'
                   ENDIF
                   WRITE( MESS,2000 ) PATHWD(J)
2000               FORMAT(' SUMMARY: NO DATA EXTRACTION FOR ',A10)
                   CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                ENDIF

            ELSEIF( ISTAGE .EQ. 1 .AND. STATUS(J,4) .EQ. 2 )THEN
C------------- If STAGE1 processing, then we must want to extract;
C              test extract setup.  Checking STAGE number since we
C              could get here in STAGE2 since DATA keyword is now
C              reprocessed from header records of QAOUT file.
               IF( J.EQ.2 )THEN
                  ISTAT = 0
                  CALL UAEXST( ISTAT )
                  IF( ISTAT.EQ.1 )THEN
                     UASTAT = -1
                     SETERR = .TRUE.
                  ELSE
                     UASTAT = 1
                  ENDIF

               ELSEIF( J.EQ.3 )THEN
                  ISTAT = 0
                  CALL SFEXST( ISTAT )
                  IF( ISTAT.EQ.1 )THEN
                     SFSTAT = -1
                     SETERR = .TRUE.
                  ELSE
                     SFSTAT = 1
                  ENDIF
               ENDIF

            ENDIF            ! test for EXTRACT
         ENDIF               ! IF ISTAT = 1

C------- If no problem with test for data extract, then test for data QA
         IF( ISTAT.EQ.1 )THEN
            CONTINUE

         ELSE
C---------- See if QA called for on this pathway
            IF( ( (J.EQ.2 .OR. J.EQ.3 ) .AND.
     &            ( STATUS(J,7).EQ.0 .OR. STATUS(J,8).EQ.0) ) .OR.
     &          (J.EQ.4 .AND.
     &            ( STATUS(J,4).EQ.0 .OR. STATUS(J,8).EQ.0) ) )THEN

C------------- Input and/or output files not available for QA
               MESS =  BLNK80
               IF( J.EQ.2 )THEN
                  ECODE = 'I25'
               ELSEIF( J.EQ.3 )THEN
                  ECODE = 'I26'
               ELSEIF( J.EQ.4 )THEN
                  ECODE = 'I27'
               ENDIF
               WRITE( MESS,2500 ) PATHWD(J)
2500           FORMAT(' SUMMARY: NO DATA QA FOR ',A10)
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

            ELSE
C------------- Must want to QA (input & output present) - test QA setup
               ISTAT = 0
               IF( J.EQ.2 )THEN
                  CALL UAQAST( ISTAT )
                  IF( ISTAT.EQ.1 )THEN
                     UASTAT = -1
                     SETERR = .TRUE.
                  ELSE
                     UASTAT = UASTAT + 2
                  ENDIF
               ELSEIF( J.EQ.3 )THEN
                  CALL SFQAST( ISTAT )
                  IF( ISTAT.EQ.1 )THEN
                     SFSTAT = -1
                     SETERR = .TRUE.
                  ELSE
                     SFSTAT = SFSTAT + 2
                  ENDIF
               ELSEIF( J.EQ.4 )THEN
                  ISTAT = 0
                  CALL OSQAST( ISTAT )
                  IF( ISTAT.EQ.1 )THEN
                     OSSTAT = -1
                     SETERR = .TRUE.
                  ELSE
                     OSSTAT = 2
                  ENDIF
               ENDIF

            ENDIF           ! test for QA
         ENDIF              ! IF ISTAT = 1

      ENDDO                  ! UA, SF, OS pathways

C---- Check MERGE pathway

      IF( STABLK(5) .EQ. 2 )THEN
         ISTAT  = 0
         DO I=1,NKEYWD
            IF( STATUS(5,I).EQ.1 ) ISTAT = ISTAT + 1
         ENDDO

         IF( ISTAT.GT.0 )THEN
            MESS =  BLNK80
            ECODE = 'E28'
            WRITE( MESS,2600 )
2600        FORMAT(' ERROR(S) DETECTED ON MERGE PATHWAY')
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            MRSTAT = -1
            SETERR = .TRUE.
         ENDIF

         IF( STATUS(5,22) .EQ. 2 )THEN
C---------- There is a valid output file for merged data;
C           are there data to merge?

            DO J =2,4
               IF( STATUS(J,8).EQ.0 )THEN

C---------------- No input file available for MERGE
                  MESS =  BLNK80
                  IF( J.EQ.2 )THEN
                    ECODE = 'I25'
                  ELSEIF( J.EQ.3 )THEN
                    ECODE = 'I26'
                  ELSEIF( J.EQ.4 )THEN
                    ECODE = 'I27'
                  ENDIF
                  WRITE( MESS,3000 ) TRIM(PATHWD(J))
3000              FORMAT(' SUMMARY: NO ',A,' data to merge')
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )

               ELSEIF( STATUS(J,8).EQ.2 )THEN
C---------------  QAOUT keyword processed without error - merge possible
C                 A pathway status of 4 is the indicator in MRPATH that
C                 there is data to merge for that pathway

                  IF( J.EQ.2 )THEN
                     UASTAT = UASTAT + 4

                  ELSEIF( J.EQ.3 )THEN
                     SFSTAT = SFSTAT + 4
C                    NOTE: in addition to the QAOUT keyword (NKEYWD=8)
C                          1-min ASOS (NKEYWD=33) needs to be checked,
C                          which is performed below

                  ELSEIF( J.EQ.4 )THEN
C------------------- Are the on-site data structures defined?
                     IF( OSDCRD.EQ.0 )THEN
                        MESS =  BLNK80
                        WRITE( MESS,3250 )
3250                    FORMAT(' SUMMARY: ONSITE data structures not',
     &                         ' defined.')
                        ECODE = 'E27'
                        CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                        OSSTAT = -1
                        SETERR = .TRUE.
                        ISTAT = 1
                     ELSE
                        OSSTAT = OSSTAT + 4
                     ENDIF
                  ENDIF

               ELSEIF( STATUS(J,8).EQ.1 )THEN
C---------------- QAOUT keyword processed with error - merge NOT possible
                  MESS =  BLNK80
                  IF( J.eq. 2 )THEN
                     ECODE = 'E25'
                  ELSEIF( J.eq. 3 )THEN
                     ECODE = 'E26'
                  ELSEIF( J.eq. 4 )THEN
                     ECODE = 'E27'
                  ENDIF
                  WRITE( MESS,3500 ) PATHWD(J)
3500              FORMAT(' SUMMARY: ''QAOUT'' keyword error for ',A10)
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                  MRSTAT = -1
                  SETERR = .TRUE.
                  ISTAT = 1
               ENDIF
            ENDDO

C---------- Check the status of the 1-min ASOS keyword (ASOS1MIN): 
C           if the keyword was processed, go ahead and check it,      
C           otherwise testing can be ignored                                       

            IF( STATUS(3,33) .EQ. 0 )THEN                             
C------------- ASOS1MIN keyword not seen
               CONTINUE                                               

            ELSEIF( STATUS(3,33) .EQ. 1 )THEN                         
C------------- Keyword seen with errors - will not merge data
               ECODE = 'E26'                                          
               WRITE( MESS,3500 ) PATHWD(3)                           
3550           FORMAT(' SUMMARY: ''ASOS1MIN'' keyword error for ',A10)
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )                   
               MRSTAT = -1                                            
               SETERR = .TRUE.                                        
               ISTAT = 1                                              

            ELSEIF( STATUS(3,33) .EQ. 2 )THEN                         
C------------ Keyword seen without error - no action necessary        
C             The assumption is that if 1-min ASOS data are used, then
C             the hourly surface weather obs (as defined on the QAOUT
C             record is also present
              CONTINUE                                                
            ENDIF

         ELSE
C---------- The MERGE OUTPUT file is not defined
            MESS =  BLNK80
            ECODE = 'E28'
            WRITE( MESS,3600 )
3600        FORMAT(' ''OUTPUT'' keyword not defined for MERGE')
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            MRSTAT = -1
            SETERR = .TRUE.
            ISTAT = 1
         ENDIF

      ENDIF

      RETURN
      END

