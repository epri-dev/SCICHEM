      SUBROUTINE OSTEST( ISTAGE,ITEST )
C=====================================================================**
C          OSTEST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:   Checks the 'STATUS' array, looking for problems in
C                the user specified on-site input runstream data.
C
C     Initial Release: December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        1/27/97 (PES)
C         - moved the check on surface characteristics from OSTEST 
C           to MPTEST
C
C        8/13/10 (MACTEC)
C         - added additional test for OSHT (heights defined using the 
C           OSHEIGHTS keyword): too many heights compared to data, 
C           duplicate heights, and order.
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'WORK1.INC'

C---- Local Variable Declarations
      INTEGER :: N, I, J, ILEV, ITEST, ERRNUM, HMIN, HMAX
      INTEGER :: NRDS
      INTEGER :: ISTAGE
      LOGICAL :: L_HTvars

C      ITEST         Status of completeness checks,
C                      0 means all ok,
C                      1 means we found a problem
C      ERRNUM        A dummy value (=0) for calls to
C                      s.ERRHDL
C      HMIN          Minimum level index found
C      HMAX          Maximum level index found

C---- Data Initialization

      PATH = 'JOB       '
      LOC  = 'OSTEST'
      ERRNUM = 0
      HMIN   = 900
      HMAX   = 0
      OSTIME = 0
      ITEST  = 0
      L_HTvars = .FALSE.

C---- Initialize IWORK1 array
      IWORK1 = 0

C---- See if user specified the data variables and formats.

      IF( STATUS(4,18) .EQ. 0 .AND. STATUS(4,6) .NE. 0 )THEN
C------- 'FORMAT' was given but no 'READ'.
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,1000 )
 1000    FORMAT(' ERROR: ONSITE FORMATs defined, but READs undefined!')
         CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
         ITEST = 1
         RETURN

      ELSEIF( STATUS(4,18) .NE. 0 .AND. STATUS(4,6) .EQ. 0 )THEN
C------- 'READ' was given, but no 'FORMAT'.
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,2000 )
 2000    FORMAT(' ERROR: ONSITE READs defined, but FORMATs undefined!')
         CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
         ITEST = 1

      ELSEIF( STATUS(4,6) .GE. 2 .AND. STATUS(4,18) .GE. 2 )THEN
C------- 'READ' and 'FORMAT' keywords present:
C        check completeness of formats for reads,
C        assuming formats were given and no errors were
C        detected in format given.  As we go, store off
C        occurrences of delta-T values for later use in
C        completeness checks.

         NRDS = 0
         DO I = 1, OSMRDS
            IF( OSDNUM(I) .GT. 0 )THEN
               NRDS = I
            ENDIF
         ENDDO
         
C------- Assign local NRDS variable to global 
C        OSDCRD variable defining number of 
C        ONSITE data records
         OSDCRD = NRDS

C------- Now check for more FORMAT keywords than READ keywords
         NRDS = 0
         DO I = 1, OSMRDS
            IF( LEN_TRIM(OSFRMT(I)) .NE. 0 )THEN
C              FORMAT has been specified for this read index
               NRDS = I
            ENDIF
         ENDDO

C------- Compare number of reads based on READ keywords vs.
C        number of reads based on FORMAT keywords
         IF( NRDS .GT. OSDCRD )THEN
C---------- More FORMAT keywords than READ keywords;
C           issue error message
            MESS =  BLNK80
            ECODE = 'E27'
            WRITE( MESS,2500 ) NRDS, OSDCRD
 2500       FORMAT(' Number of FORMAT keywords (', I2, ') for ',
     &              'ONSITE data exceeds number of READ indices ',
     &              '(',I2,').' )
            CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
            ITEST = 1
         ENDIF
         
C------- Loop through ONSITE READ/FORMAT keywords to
C        check for completeness
         DO N = 1, OSDCRD
         
            IF( OSDNUM(N) .GT. OSMDAT )THEN
C------------- Too many variables in this data record
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,2600 ) OSDNUM(N),N,OSMDAT
 2600          FORMAT(' Number of variables (', I2, ') for ONSITE ',
     &                 'record # ',I2,' exceeds maximum (',I2,')' )
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1

            ELSEIF( OSDNUM(N).EQ.0 .AND.
     &              LEN_TRIM(OSFRMT(N)) .GT. 0 )THEN
C------------- No variables defined but FORMAT specified for this READ index, 
C              issue error message
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,2620 ) N
 2620          FORMAT(' FORMAT keyword specified but no READ ',
     &                 'variables for record index: ',I3)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
         
            ELSEIF( OSDNUM(N).GT.0 .AND.
     &              LEN_TRIM(OSFRMT(N)) .EQ. 0 )THEN
C------------- No FORMAT defined but variables specified for this READ index, 
C              issue error message
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,2630 ) N
 2630          FORMAT(' FORMAT keyword has NOT been specified ',
     &                 'for ONSITE record (READ) index: ',I3)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
         
            ELSEIF( OSDNUM(N).EQ.0 .AND.
     &              LEN_TRIM(OSFRMT(N)) .EQ. 0 )THEN
C------------- No variables defined AND no format specified for this READ index, 
C              issue error message
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,2640 ) N
 2640          FORMAT(' No READ variables or FORMAT keyword specified ',
     &                 'for record index: ',I3)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
         
            ENDIF
         
C---------- Check FORMAT character string for problems
            IF( LEN_TRIM(OSFRMT(N)) .GT. 0 )THEN
               CALL DEFINE( 132,OSFRMT(N) )
               IF( OSFRMT(N)(IC1(1):IC1(1)).EQ.'(' .AND.
     &             OSFRMT(N)(IC2(NWORDS):IC2(NWORDS)).EQ.')' )THEN
                  CONTINUE
               ELSEIF( NWORDS .EQ. 1 .AND.
     &                     OSFRMT(N)(IC1(1):IC1(1)) .EQ. '*' )THEN
                  CONTINUE
               ELSE
                  MESS =  BLNK80
                  ECODE = 'E27'
                  WRITE( MESS,2700 ) N
2700              FORMAT(' ONSITE data FORMAT error:',
     &                   ' Check parentheses for READ #: ',I2)
                  CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                  WRITE( DEV60,1100 ) n,OSDNUM(n),OSFRMT(n)
1100              FORMAT(I3,I3,1X,A)
                  ITEST = 1
               ENDIF
            ENDIF
            
            IF( ITEST .EQ. 1 )THEN
C----------    Error(s) found in READ/FORMAT keywords;
C              return to calling routine
               RETURN
            ENDIF

C---------- No problems to this point; continue checks
C           for delta-T information, date variables
C           and height variables
            IF( OSDNUM(N).LE.OSMDAT )THEN
               OSNDT = 0
               DO J=1,OSDNUM(N)
         
C---------------- Store occurrences of delta-t info.
                  IF( OSDVAR(N,J,1).GE.9 .AND. 
     &                OSDVAR(N,J,1).LE.11 )THEN
                     POSINX = 100 + OSDVAR(N,J,1) - 9
                     IWORK1(POSINX) = 1
                     OSNDT = OSNDT + 1
                  ENDIF
         
C---------------- Store occurrences of date and time info.
                  IF( OSDVAR(N,J,1).GE.52 .AND. 
     &                OSDVAR(N,J,1).LE.56 )THEN
                     POSINX = 200 + OSDVAR(N,J,1) - 52
                     OSTIME = OSTIME + 1
                     IWORK1(POSINX) = 1
         
                     IF( N.NE.1 )THEN
                        MESS =  BLNK80
                        ECODE = 'E27'
                        WRITE( MESS,3000 )
3000                    FORMAT(' ONSITE Date & Time must be part of ',
     &                          '1st READ.')
                        CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                        ITEST = 1
                     ENDIF
                  ENDIF
         
C---------------- Check for use of OBS/HOUR = 1 with OSMN on READs
                  IF( ISTAGE .EQ. 1 .AND.
     &                OSDVAR(N,J,1) .EQ. 56 .AND. 
     &                OSDVAR(N,J,2) .EQ.  0 .AND. OSAVG .EQ. 1 )THEN
C----                OSMN specified in READ but OSAVG = 1
                     MESS =  BLNK80
                     ECODE = 'W22'
                     WRITE( MESS,3100 )
3100                 FORMAT(' OSMN is specified on READ keyword,',
     &                      ' but OBS/HOUR = 1; OSMN variable will',
     &                      ' be ignored.')
                     CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                  ENDIF
         
C---------------- As the statements are checked, store information
C                 regarding tower profile heights for later QA checks.
         
C---------------- Looking for minimum and maximum level index
C                 referenced in data map.
         
                  IF( OSDVAR(N,J,2).GT.0 .AND.
     &                OSDVAR(N,J,2).LT.HMIN ) HMIN = OSDVAR(N,J,2)
         
                  IF( OSDVAR(N,J,2).GT.0 .AND.
     &                OSDVAR(N,J,2).GT.HMAX ) HMAX = OSDVAR(N,J,2)
         
C---------------- Looking for heights for the various levels referenced
C                 in the variable list.
C                 Use IWORK1 array to 'flag' values found.
C                 OSDVAR(N,J,1) = variable type
C                 OSDVAR(N,J,2) = tower level index
         
                  IF( OSDVAR(N,J,1).EQ.15 )THEN
                     IWORK1( OSDVAR(N,J,2) ) = 1
C----                Set logical flag to indicate that ONSITE data
C                    profile heights (HTnn) are included in the data.
                     L_HTvars = .TRUE.
                  ENDIF
         
               ENDDO
            ENDIF

         ENDDO

C------- Check to insure we have all tower levels defined with a 
C        height.  Tower heights can be defined one of two ways:
C          1) Using 'OSHEIGHTS' keyword to define heights explicitly,
C             (OSNL > 0 if OSHEIGHTS is used) 
C          2) Read in on tower data records
C             (OSNL = 0)
C
C        NOTE: Must use either OSHEIGHTS or HTnn variables, not both!
C
C        The following tests are primarily for when the user defines 
C        the tower heights using the OSHEIGHTS keyword

  65     CONTINUE
  
         IF( OSNL.GT.0 )THEN
C----       The heights were defined using the OSHEIGHTS keyword
C           Check for heights included in data files also (HTnn variables)
            IF( L_HTvars )THEN
               MESS =  BLNK80
               ECODE = 'W21'
               WRITE( MESS, 3900 ) 
3900           FORMAT(' OSHEIGHTS and HTnn both specified.',
     &                ' OSHEIGHTS keyword will be used',
     &                ' and HTnn variables ignored.')
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
            ENDIF

            IF( HMIN.LT.1 )THEN
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS, 4000 ) HMIN
4000           FORMAT(' Lowest ONSITE profile level referenced is: ',
     &                I3)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF
            
C----       Check for appropriate number of OSHEIGHTS (too few?)
            IF( HMAX .GT. OSNL )THEN
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,4100 ) OSNL, HMAX
4100           FORMAT(' Fewer heights defined with OSHEIGHTS (',I3,
     &                ') than data records READ (',I3,')')
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1

C----       Check for appropriate number of OSHEIGHTS (too many?)
            ELSEIF( OSNL .GT. HMAX )THEN
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,4150 ) OSNL, HMAX
4150           FORMAT(' More heights defined with OSHEIGHTS (',I3,
     &                ') than data records READ (',I3,')')
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF

C----       Check the OSHT array for heights in ascending order
            DO ILEV = 1,OSNL-1
               IF( OSHT(ILEV+1)+0.05 .LE. OSHT(ILEV) )THEN
                  MESS =  BLNK80
                  ECODE = 'E27'
                  WRITE( MESS,4160 ) ILEV, ILEV+1
4160              FORMAT(' ONSITE height at level ',I3,
     &                   ' is ABOVE height at level ',I3)
                  CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                  ITEST = 1
               ENDIF
            ENDDO

C----      Check the OSHT array for duplicate heights
            DO J = 2, OSNL
               DO ILEV = J-1, 1, -1
                  IF( ABS(OSHT(J)-OSHT(ILEV)) .LT. 0.05 )THEN
                     MESS =  BLNK80
                     ECODE = 'E27'
                     WRITE( MESS,4170 ) OSHT(ILEV)
4170                 FORMAT(' Duplicate ONSITE heights specified at ',
     &                        F7.2,' meters!')
                     CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                     ITEST = 1
                  ENDIF
               ENDDO
            ENDDO
           

         ELSE
C----       The heights appeared on each data record - check the work 
C           array (IWORK1) to insure that height data is on the
C           tower data records.

            IF( HMIN.NE.1 )THEN
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS, 4000 ) HMIN
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF

            IF( HMAX.GE.HMIN )THEN
C ---          Assign value for OSNL, number of ONSITE levels, 
C              based on number of levels identified in the READs,
C              for later use in processing the data.
               OSNL = HMAX
               
C ---          Issue informational message regarding OS heights
C              derived from data
               MESS =  BLNK80
               ECODE = 'I27'
               WRITE( MESS,4190 ) HMIN, HMAX
4190           FORMAT(' Minimum and maximum ONSITE height level ',
     &                 'indices derived from data file are: ', 2I4)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )

C ---          Check for any gaps between ONSITE level indices defined 
C              in the READs
               DO N=HMIN,HMAX
                  IF( IWORK1(N).EQ.1 )THEN
                     CONTINUE
                  ELSE
                     MESS =  BLNK80
                     ECODE = 'E27'
                     WRITE( MESS,4200 ) N
4200                 FORMAT(' Height for ONSITE level ',I3,
     &                       ' in undefined.')
                     CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                     ITEST = 1
                  ENDIF
               ENDDO

            ELSE
               MESS =  BLNK80
               ECODE = 'E00'
               WRITE( MESS,4300 ) HMAX, HMIN
4300           FORMAT(' PROGRAM LOGIC ERROR: ONSITE height indices ',
     &                 'HMAX < HMIN: ',2I6)
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF
         ENDIF

C------- Check DELTA-T data definitions

         IF( STATUS(4,15).EQ.0 )THEN
C---------- Status says no deltaT heights are defined;
C           make sure no deltaT values are being read

            IF( IWORK1(100).EQ.0 .AND. IWORK1(101).EQ.0 .AND.
     &          IWORK1(102).EQ.0  )THEN
               CONTINUE
            ELSE
               MESS =  BLNK80
               ECODE = 'E27'
               WRITE( MESS,5000 ) OSNDT
5000           FORMAT(' ONSITE deltaT # ',I2.2,' has no heights ',
     &                'defined.')
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
            ENDIF
         ELSE

            DO J=1,OSNDT
C------------- Verify that there is are deltatT-values for each
C              set of deltaT-levels

               IF( IWORK1(100+J-1).EQ.0 )THEN
                  MESS =  BLNK80
                  ECODE = 'E27'
                  WRITE( MESS,5100 ) J
5100              FORMAT(' Heights for ONSITE deltaT # ',I2.2,
     &                   ' are defined, but not the data. ')
                  CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
                  ITEST = 1
                ENDIF
            ENDDO
         ENDIF

C------- Check for date (year, month and day) and time (at least
C        hour) variables within the scalar data variable list.
C        Each on-site data observation must be labeled with a year,
C        month, day, and hour.

         DO J=200,203
            IF( IWORK1(J).EQ.1 )THEN
               CONTINUE
            ELSE
               MESS =  BLNK80
               WRITE( MESS,6000 ) VNAMES(52+J-200)
6000           FORMAT(' ONSITE date/time is incomplete; no ',A4)
               ITEST = 1
               ECODE = 'E27'
               CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
            ENDIF
         ENDDO

C------- Did the user define a wind speed threshold value with
C        the MIN_WIND keyword?

         IF( STATUS(4,17).EQ.0 )THEN
            MESS =  BLNK80
            WRITE( MESS,7000 )
7000        FORMAT(' ''THRESHOLD'' for ONSITE wind speed has not been ',
     &              'defined.')
            ITEST = 1
            ECODE = 'E27'
            CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
         ENDIF

C------- Review audits requested versus defined 'data map'
         CALL AUTCHK

      ENDIF                         ! Check on read/format status

C---- Check for OSAVG > 1, but without OSMN variable defined on READ,
C     but only if READ and FORMAT keyword status is ok (=2) and 
C     ITEST .ne. 1.  
      IF( ISTAGE .EQ. 1 .AND. ITEST .NE. 1 .AND.
     &    STATUS(4,6) .EQ. 2 .AND. STATUS(4,18) .EQ. 2 .AND.
     &    IWORK1( 204 ) .EQ. 0 .AND. OSAVG .GT. 1 )THEN
C----    OSMN NOT specified in READ but OSAVG > 1
         MESS =  BLNK80
         ECODE = 'E27'
         WRITE( MESS,3200 ) OSAVG
3200     FORMAT(' OSMN is NOT included on READ,',
     &           ' but OBS/HOUR = ',I2,'; OSMN required',
     &           ' for sub-hourly ONSITE data.')
         CALL ERRHDL( ERRNUM,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END

