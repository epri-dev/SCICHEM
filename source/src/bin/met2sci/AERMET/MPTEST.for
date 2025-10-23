      SUBROUTINE MPTEST
C=======================================================================
C     MPTEST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To process the status array to determine if there were
C               any errors on the runstream records and if there is
C               sufficient information to proceed with the data
C               processing.
C
C     Arguments:  <none>
C
C     Revision history:
C          10/4/96    Added a check for the existence of on-site data
C                     based on the presence of the on-site data
C                     definition; modified the content of the error
C                     messages
C
C          1/27/97    moved the check on surface characteristics from
C                     OSTEST to MPTEST
C
C-----------------------------------------------------------------------
C-----Local variables

      IMPLICIT NONE

      INTEGER  NJOB, NPREP, I, ISEC, JSEC, IFRQ, ITEST, ITEST2, ISTAT

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C     NPREP   A counter that is never used
C.....................................................................

C-----The STATUS(n,m) array utilizes the following conventions:
C        n = pathway
C        m = keyword
C        value of pathway 'n', keyword 'm'
C           0 = keyword not seen
C           1 = keyword seen, errors detected
C           2 = keyword seen, no errors detected

C-----Initialize values
      PATH = 'JOB       '
      LOC  = 'MPTEST'
      JBSTAT = 0
      NJOB   = 0
      NPREP  = 0
      ITEST  = 0
      ITEST2 = 0
      ISTAT  = 0

C-----Process 'JOB' pathway

      DO I = 1,NKEYWD
         IF( STATUS(1,I) .EQ. 1 ) NJOB = NJOB + 1
      ENDDO

C-----Make sure we have opened a MESSAGE file

      IF( STATUS(1,2) .NE. 2 )THEN
         NJOB = NJOB + 1
         MESS =  BLNK80
         ECODE = 'E24'
         WRITE( MESS,1500 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         SETERR = .TRUE.
      ENDIF

C-----If there are errors in the JOB setup, tell the user
C     how many there are
      IF( NJOB .GT. 0 )THEN
         MESS =  BLNK80
         ECODE = '   '
         WRITE( MESS,1000 )  PATHWD(1)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         SETERR = .TRUE.
      ENDIF

C-----Now check the MP-pathway status for errors in setup

      PATH = 'METPREP   '
      DO I = 1,NKEYWD
         IF( STATUS(6,I) .EQ. 1 ) NPREP = NPREP + 1
      ENDDO

C-----Now test for several required control cards for STAGE 3 processing.
C     If the STATUS is not equal to 2, then there were errors in
C     decoding the control card or one was not present in the
C     runstream.  If the STATUS=0, then the keyword did not appear.

C-----Make sure an input file of merged meteorology is open

      IF( STATUS(6,4) .eq. 0 )THEN
         NPREP = NPREP + 1
         MESS =  BLNK80
         ECODE = 'E12'
         WRITE( MESS,2000 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         SETERR = .TRUE.
         MPSTAT = -1
      ENDIF

C-----Make sure an output file for surface (scalar) data for
C       processed diffusion meteorology is open

      IF( STATUS(6,22) .EQ. 0 )THEN
         NPREP = NPREP + 1
         MESS =  BLNK80
         ECODE = 'E12'
         WRITE( MESS,2500 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         SETERR = .TRUE.
         MPSTAT = -1
      ENDIF

C-----Make sure a file for profile data for is open
C       (an output file for the diffusion model)

      IF( STATUS(6,27) .eq. 0 )THEN
         NPREP = NPREP + 1
         MESS =  BLNK80
         ECODE = 'E12'
         WRITE( MESS,2550 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         SETERR = .TRUE.
         MPSTAT = -1
      ENDIF


C-----Make sure there is a valid NWS instrument height card
      IF( STATUS(6,29) .EQ. 0 )THEN
C------- A missing instrument height is an error only if
C        NWS substitution was specified
         IF( CONTRL(10) .GT. 1) THEN
            NPREP = NPREP + 1
            MESS =  BLNK80
            ECODE = 'E12'
            WRITE( MESS,2570 )
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            SETERR = .TRUE.
            MPSTAT = -1
         ENDIF
      ENDIF

C-----Process OS pathway (if appropriate) to insure data
C     map and formats are sufficient for processing

C     Also make sure that there are data to process:
C     OSDATA is .TRUE. if the VARS and FORMAT records
C     are defined, indicating the presence of on-site
C     data; SUBSTNWS indicates if the user specified
C     substituting NWS data if on-site data are missing.
C
      PATH = 'ONSITE    '
      OSDATA = .FALSE.
C --- Pass value of 3 for ISTAGE
      CALL OSTEST( 3,ISTAT )

      IF( ISTAT .EQ. 1 )THEN
C------- Errors were detected - set flags to abort processing
         OSSTAT = -1
         SETERR = .TRUE.

      ELSE
C------- Check for the READ and FORMAT records
         IF( STATUS(4,18) .EQ. 2  .AND.  STATUS(4,6) .EQ. 2 )THEN

C---------- The VARS and FORMAT records are present and without error
            OSDATA = .TRUE.

         ELSEIF( (STATUS(4,18).EQ.0) .AND. (STATUS(4,6).EQ.0) .AND.
     &           (.NOT. SUBSTNWS) )THEN

C---------- There are no on-site data defined and the user has not
C           specified to substitute NWS data - there are no data to
C           make the computations!!  Abort processing

            OSSTAT = -1
            SETERR = .TRUE.
            MESS =  BLNK80
            ECODE = 'E12'
            WRITE( MESS,2600 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

         ENDIF

      ENDIF

      PATH = 'METPREP   ' 
C     Note that the surface characteristics were made part of the
C     met processing (MP path) in 1997
C-----Check the definition of the primary surface characteristics

C     OSMSEC = maximum number of sectors allowed
C     OSNWDS = number of sectors specified by FREQ_SECT keyword
C     NUMSEC = number of unique SECTOR keyword records encountered
C     OSWDS(i,j) = wind sector definition:
C                     i=sector #,
C                     j=beginning (j=1) or end (j=2) of sector

      ITEST = 0
      IF( NUMSEC .EQ. OSNWDS )THEN
C------- The number of SECTOR keywords matches the number specified
C        on FREQ_SECT keyword

         IF( OSNWDS.GE.1  .AND.  OSNWDS.LE.OSMSEC )THEN
C---------- Valid number of wind sectors

            IF( OSWDS(1,1) .NE. 0.0  .AND.
     &         (OSWDS(1,1) .NE. OSWDS(OSNWDS,2)) )THEN
C------------- The first and last sector boundaries do not match
               MESS =  BLNK80
               ECODE = 'E15'
               WRITE( MESS, 8000 )
8000           FORMAT(' Start of 1st SECTOR .NE. end of',
     &                ' last SECTOR')
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               NPREP = NPREP + 1
               ITEST = 1

            ELSEIF( OSWDS(1,1) .EQ. 0.0  .AND.
     &         OSWDS(OSNWDS,2) .NE. 360.0 )THEN
               MESS =  BLNK80
               ECODE = 'E15'
               WRITE( MESS, 8025 )
8025           FORMAT(' Start of 1st SECTOR = 0;',
     &                ' end of last SECTOR .NE. 360')
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               NPREP = NPREP + 1
               ITEST = 1

            ELSEIF( OSWDS(1,1) .NE. 0.0  .AND.
     &         OSWDS(OSNWDS,2) .EQ. 360.0 )THEN
               MESS =  BLNK80
               ECODE = 'E15'
               WRITE( MESS, 8050 )
8050           FORMAT(' End of last SECTOR = 360;',
     &                ' start of 1st SECTOR .NE. 0')
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               NPREP = NPREP + 1
               ITEST = 1

            ENDIF

            DO ISEC = 1, OSNWDS-1
               IF( ABS(OSWDS(ISEC,2) - OSWDS(ISEC+1,1)) .GT. 0.01 )THEN
                  MESS =  BLNK80
                  ECODE = 'E15'
                  WRITE( MESS, 8100 ) ISEC, ISEC+1
8100              FORMAT(' Last WD on SECTOR ',I2,
     &                   ' .NE. first WD on SECTOR ',I2)
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                  NPREP = NPREP + 1
                  ITEST = 1
               ENDIF
            ENDDO

         ELSEIF( OSNWDS .EQ. 0 )THEN
C---------- Somehow the minimum value of 1 was changed to 0!
            MESS =  BLNK80
            ECODE = 'E15'
            WRITE( MESS, 8200 )
8200        FORMAT(' Number of sectors on FREC_SECT keyword = 0; ',
     &              'check surface characteristics keywords')
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            NPREP = NPREP + 1
            ITEST = 1

         ELSEIF( OSNWDS .GT. OSMSEC )THEN
C---------- The number of sectors exceeds maximum (should have been
C           caught earlier)
            MESS =  BLNK80
            ECODE = 'E15'
            WRITE( MESS,8250 ) OSNWDS, OSMSEC
8250        FORMAT(' Number of sectors on FREC_SECT keyword (',I3,
     &              ') exceeds maximum of ',I2)
            CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            NPREP = NPREP + 1
            ITEST = 1

         ENDIF

      ELSE
C------- The number of sectors specified on 'FREQ_SECT' does not
C        equal the number of SECTOR keywords in the runstream
         MESS =  BLNK80
         ECODE = 'E15'
         WRITE( MESS, 8300 ) OSNWDS,NUMSEC
8300     FORMAT(' Number of SECTORs expected = ',I2,
     &          '; Number of SECTOR keywords = ',I2 )
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         NPREP = NPREP + 1
         ITEST = 1
      ENDIF


C-----Compare the number of occurrences of the SITE_CHAR keyword
C     to the number implied by the parameter on FREQ_SECT

C     SFCFRQ = integer value of frequency implied on FREQ_SECT
C     OSNWDS = number of wind direction sectors on FREQ_SECT
C     NKFREQ = number of periods as implied by frequency on FREQ_SECT
C                = 12 for MONTHLY  (SFCFRQ = 2)
C                =  4 for SEASONAL (SFCFRQ = 3)
C                =  1 for ANNUAL   (SFCFRQ = 4)
C     ICHCNT = number of SITE_CHAR keyword records processed
C              without error
C     GOTCHR = 2-D array identifying the SITE_CHAR keywords that
C              were processed; elements are either 0 (indicating
C              a problem) or 1 (processed without error) for the
C              period and sector on the SITE_CHAR keyword

      IF( NKFREQ*OSNWDS .NE. ICHCNT )THEN
C------- The number of SITE_CHAR keywords does not match the number
C        expected from the frequency and # of sectors on FREQ_SECT;

         MESS =  BLNK80
         ECODE = 'E15'
         WRITE( MESS, 8600 ) NKFREQ*OSNWDS,ICHCNT
8600     FORMAT(' Expected ',I2,' SITE_CHAR keywords; ',
     &          I2,' processed.')
         CALL ERRHDL( 1,PATH,ECODE,LOC,MESS )

         DO IFRQ = 1,NKFREQ
            DO JSEC = 1,OSNWDS
               IF( GOTCHR(IFRQ,JSEC) .NE. 1 )THEN
                  MESS =  BLNK80
                  ECODE = 'E15'
                  WRITE( MESS, 8700 ) IFRQ,JSEC
8700              FORMAT('  Inputs missing for period ', I2,
     &                   '; primary sector ',I2)
                  CALL ERRHDL( 2,PATH,ECODE,LOC,MESS )
               ENDIF
            ENDDO
          ENDDO

         ITEST = 1
      ENDIF       ! # surface char. keywords/# expected mismatch

C-----A secondary set of surface characteristics is required if there
C     is site-specific data (OSDATA), hourly weather data (SFDATA),
C     and NWS substitution is 'ON' (SUBSTNWS)

      IF( OSDATA )THEN
C------- Site-specific data are present

         IF( SFDATA )THEN
C---------- Hourly surface data are present

            IF( SUBSTNWS )THEN
C------------- And the user has specified to substiture NWS data as needed

C------------- Now check to see if the secondary surface charqacteristics
C              were defined without error - were FREQ_SECT2 (STATUS(6,30))
C              and SITE_CHAR2 (STATUS(6,31)) defined and without error
C              on the control file? (NOTE: # sectors is checked later)

               IF( STATUS(6,30) .EQ. 2 .and. STATUS(6,31) .EQ. 2 )THEN
C---------------- Keywords ok; check the definition of the secondary
C                 surface characteristics

C---------------- OSMSEC2 = maximum number of sectors allowed (12)
C                 OSNWDS2 = # of sectors specified by FREQ_SECT2 keyword
C                 NUMSEC2 = # of SECTOR2 keywords
C                 OSWDS2(i,j) = wind sector definition:
C                            i=sector #,
C                            j=beginning (j=1) or end (j=2) of sector

                  ITEST2 = 0
                  IF( NUMSEC2 .EQ. OSNWDS2 )THEN
C------------------- The number of SECTOR2 keywords matches the number
C                    specified on FREQ_SECT2 keyword

                     IF( OSNWDS2.GE.1  .AND.  OSNWDS2.LE.OSMSEC )THEN
C---------------------- Valid number of wind sectors

                        IF( OSWDS2(1,1) .NE. 0.0  .AND.
     &                     (OSWDS2(1,1) .NE. OSWDS2(OSNWDS2,2)) )THEN
C------------------------- The first & last sector boundaries do not match
                           MESS =  BLNK80
                           ECODE = 'E15'
                           WRITE( MESS, 8001 )
8001                       FORMAT(' Start of 1st SECTOR2 .NE. end of',
     &                            ' last SECTOR2')
                           CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                           NPREP = NPREP + 1
                           ITEST2 = 1

                        ELSEIF( OSWDS2(1,1) .EQ. 0.0  .AND.
     &                          OSWDS2(OSNWDS2,2) .NE. 360.0 )THEN
                           MESS =  BLNK80
                           ECODE = 'E15'
                           WRITE( MESS, 8026 )
8026                       FORMAT(' Start of 1st SECTOR2 = 0;',
     &                            ' end of last SECTOR2 .NE. 360')
                           CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                           NPREP = NPREP + 1
                           ITEST2 = 1

                        ELSEIF( OSWDS2(1,1) .NE. 0.0  .AND.
     &                          OSWDS2(OSNWDS2,2) .EQ. 360.0 )THEN
                           MESS =  BLNK80
                           ECODE = 'E15'
                           WRITE( MESS, 8051 )
8051                       FORMAT(' End of last SECTOR2 = 360;',
     &                           ' start of 1st SECTOR2 .NE. 0')
                           CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                           NPREP = NPREP + 1
                           ITEST2 = 1

                        ENDIF

                        DO ISEC = 1, OSNWDS2-1
                           IF( ABS(OSWDS2(ISEC,2) - OSWDS2(ISEC+1,1))
     &                                                .GT. 0.01 )THEN
                              MESS =  BLNK80
                              ECODE = 'E15'
                              WRITE( MESS, 8101 ) ISEC, ISEC+1
8101                          FORMAT(' Last WD on SECTOR2 ',I2,
     &                               ' .NE. first WD on SECTOR2 ',I2)
                              CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
                              NPREP = NPREP + 1
                              ITEST2 = 1
                           ENDIF
                        ENDDO

                     ELSEIF( OSNWDS2 .EQ. 0 )THEN
C---------------------- An error was identified earlier and the # of WD
C                       sectors was set to 0
                        MESS =  BLNK80
                        ECODE = 'E15'
                        WRITE( MESS, 8201 )
8201                    FORMAT(' Number of sectors on FREC_SECT2 ',
     &                         'keyword = 0; check secondary surface ',
     &                         'characteristics keywords')
                        CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                        NPREP = NPREP + 1
                        ITEST2 = 1

                     ELSEIF( OSNWDS2 .GT. OSMSEC )THEN
C---------------------- The number of sectors exceeds maximum (should have been
C                       caught earlier)
                        MESS =  BLNK80
                        ECODE = 'E15'
                        WRITE( MESS,8251 ) OSMSEC
8251                    FORMAT(' Number of sectors on FREC_SECT2 ',
     &                        'keyword (',I3,') exceeds maximum of ',I2)
                        CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                        NPREP = NPREP + 1
                        ITEST2 = 1
                     ENDIF

                  ELSE
C------------------- The number of sectors specified on 'FREQ_SECT2' does not
C                    equal the number of SECTOR keywords in the runstream
                     MESS =  BLNK80
                     ECODE = 'E15'
                     WRITE( MESS, 8301 ) OSNWDS2,NUMSEC2
8301                 FORMAT(' Number of SECTOR2s expected = ',I2,
     &                      '; Number of SECTOR2 keywords = ',I2 )
                     CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                     NPREP = NPREP + 1
                     ITEST2 = 1
                  ENDIF

C---------------- Compare the number of occurrences of the SITE_CHAR2 keyword
C                 to the number implied by the parameter on FREQ_SECT2

C                 SFCFRQ2 = integer value of frequency implied on FREQ_SECT2
C                 OSNWDS2 = number of wind direction sectors on FREQ_SECT2
C                 NKFREQ2 = number of periods as implied by frequency on FREQ_SECT2
C                         = 12 for MONTHLY  (SFCFRQ2 = 2)
C                         =  4 for SEASONAL (SFCFRQ2 = 3)
C                         =  1 for ANNUAL   (SFCFRQ2 = 4)
C                 ICHCNT2 = number of SITE_CHAR2 keyword records processed
C                           without error
C                 GOTCHR2 = 2-D array identifying the SITE_CHAR2 keywords that
C                     were processed; elements are either 0 (indicating
C                     a problem) or 1 (processed without error) for the
C                     period and sector on the SITE_CHAR keyword

                  IF( NKFREQ2*OSNWDS2 .NE. ICHCNT2 )THEN
C------------------- The number of SITE_CHAR2 keywords does not match the number
C                    expected from the frequency and # of sectors on FREQ_SECT2;

                     MESS =  BLNK80
                     ECODE = 'E15'
                     WRITE( MESS, 8601 ) NKFREQ2*OSNWDS2,ICHCNT2
8601                 FORMAT(' Expected ',I2,' SITE_CHAR2 keywords; ',
     &                     I2,' processed.')
                     CALL ERRHDL( 1,PATH,ECODE,LOC,MESS )

                     DO  IFRQ = 1,NKFREQ2
                        DO  JSEC = 1,OSNWDS2
                           IF( GOTCHR2(IFRQ,JSEC) .NE. 1 )THEN
                              MESS =  BLNK80
                              ECODE = 'E15'
                              WRITE( MESS, 8701 ) IFRQ,JSEC
8701                          FORMAT('  Inputs missing for period ',I2,
     &                               '; secondary sector ',I2)
                              CALL ERRHDL( 2,PATH,ECODE,LOC,MESS )
                           ENDIF
                        ENDDO
                     ENDDO

                     ITEST2 = 1

                  ENDIF       ! # surface char. keywords - # expected mismatch

               ELSE
                  MESS = BLNK80
                  ECODE = 'E15'
                  WRITE( MESS, 8711 )
8711              FORMAT(' Expected Secondary Surface Characteristics',
     &                   ' with SUBNWS option: Errors on input or',
     &                   ' none found.')
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
                  MPSTAT = -1
                  SETERR = .TRUE.
               ENDIF    ! secondary characteristics exist

            ELSE
C------------- Not substituting NWS data; only need 1 set of sfc charac
               IF( STATUS(6,30) .EQ. 2 .and. STATUS(6,31) .EQ. 2 )THEN
C                 Two sets of characteristics are defined
                  MESS = BLNK80
                  ECODE = 'W15'
                  WRITE( MESS, 8706 )
8706              FORMAT(' Two sets of surface characteristics but ',
     &                   'NO NWS subst; Only one set required - ',
     &                   'using first set.')
                  CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
               ENDIF
            ENDIF    ! SUBST NWS data
         
         ELSE        ! no SF data
            IF( STATUS(6,30) .EQ. 2 .and. STATUS(6,31) .EQ. 2 )THEN
C              Two sets of characteristics are defined
               MESS = BLNK80
               ECODE = 'W15'
               WRITE( MESS, 8707 )
8707           FORMAT(' Two sets of surface characteristics but ',
     &                'NO SURFACE data; Only one set required - ',
     &                'using first set.')
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            ENDIF
         ENDIF    ! OS data but no SF data
            
      ELSE
         IF( SFDATA )THEN
C        No OS data, but there is SF data - is one set of
C        surface characteristics defined?
            IF( STATUS(6,30) .EQ. 2 .and. STATUS(6,31) .EQ. 2 )THEN
C              Two sets of characteristics are defined - only need one
               MESS = BLNK80
               ECODE = 'W15'
               WRITE( MESS, 8708 )
8708           FORMAT(' Two sets of surface characteristics but ',
     &                'NO ONSITE data; Only one set required - ',
     &                'using first set.')
               CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
            ENDIF
         ENDIF   ! SFDATA but no OS data
      ENDIF      ! OSDATA

C-----If there are errors in the METPREP setup, tell the user
      IF( ITEST .EQ. 1 .OR. ITEST2 .EQ. 1 )THEN
         MESS =  BLNK80
         ECODE = 'E29'
         WRITE( MESS,1000 )  PATHWD(6)
         CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
         MPSTAT = -1
         SETERR = .TRUE.
      ENDIF


      RETURN

C.....................................................................
 1000 FORMAT( ' SETUP ERROR(S) DETECTED FOR ',A10,' PATHWAY ')
 1500 FORMAT( ' MESSAGE FILE NOT DEFINED OR OPENED' )
 2000 FORMAT( ' MERGED MET INPUT FILE NOT DEFINED OR OPENED' )
 2500 FORMAT( ' SURFACE MET OUTPUT FILE NOT DEFINED OR OPENED' )
 2550 FORMAT( ' PROFILE MET OUTPUT FILE NOT DEFINED OR OPENED' )
 2560 FORMAT( ' LOCATION keyword missing or invalid')
 2570 FORMAT( ' NWS instrument height (NWS_HGT keyword) ',
     &         'missing or invalid.' )
 2600 FORMAT( ' NO ONSITE data & NOT substituting w/ NWS data.' )

      END
