      SUBROUTINE FETCH( ITEST )
C==================================================================
C          FETCH Module of the AERMET Meteorological Preprocessor
C
C
C     PURPOSE:   To retrieve the next day's data.  This includes
C                upper air soundings and, NWS surface hourly
C                observations, and onsite hourly observations,
C                to the extent these data are present.
C
C     Initial Release:  December 1992
C
C     Revision History:
C
C        10/2009 - Added flag ISASOS() to the surface data read
C                  statement; processed flag - if present continue; if 
C                  missing, write a message and stop
C
C        01/2009 - Removed initialization of local variable JMPDATE so it
C                  would not be 00000 when EOF reading input data reached
C                - Changed local variable COUNT to ICOUNT to avoid
C                  possible conflict with FORTRAN reserved word
C        07/2010 - Re-added initialization of local variable JMPDate.  
C                  When compiled with Intel with "check" option
C                  enabled, code fails with a runtime error when 
C                  EOF or read error is encountered since code attempts
C                   to write uninitialized JMPDATE to the MESS variable.
C------------------------------------------------------------------
C---- Variable declarations

      IMPLICIT NONE
      
      INTEGER JMPDATE, JV,JLEV,IVBL,ITEST,HOUR, I
      INTEGER, SAVE :: ICOUNT
      INTEGER M, NUA_PREV, NUA_CURR
      INTEGER ITEMPYR, ITEMPMO, ITMEPDAY, ITEMPHR, IHOUR
      REAL    TEMPWS, TEMPWD
      
C     JV,JLEV,IVBL  Dummy indices
C     ITEST         Status of process
C                      1 = error
C                      2 = all OK
C                      3 = EOF found
C     ICOUNT        Internal counter,
C                      1 = first day processed
C                      2 = second, etc.

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      DATA ICOUNT/0/

      PATH = 'METPREP   '
      LOC  = ' FETCH'
      
C---- Initialize date variable
      JMPDATE = 0

C---- Initialize data arrays
      CALL FLIWK1
      CALL FLSDG(UAMH)
      CALL FLSFC(SFMH)
      CALL FLOS(OSMH)

C---- Reinitialize the date group to 0
      DO M=1,24
         SFYR(M)  = 0
         SFMO(M)  = 0
         SFDAY(M) = 0
         SFHR(M)  = 0
      ENDDO

C---- 1.  Read current day's master header

      READ(DEV40,111,END=700,ERR=701,IOSTAT=IOFLAG)
     &     MPYR,MPCMO,MPCDY,MPJDY,IWORK1(20), NUA_PREV, NUA_CURR,
     &     IWORK1(21),IWORK1(22),IWORK1(23)

C---- Read was apparently successful, so increment ICOUNT
      ICOUNT = ICOUNT + 1

C---- Compute date variable, JMPDATE
      JMPDATE = MPYR*10000 + MPCMO*100 + MPCDY
   
C     JMPDATE      = date in the form yymmdd
C     MPYR         = current year
C     MPCMO        = current month
C     MPCDY        = current day
C     MPJDY        = current Julian day
C     IWORK1(20)   = number of upper air obs. written for the merge day
C     IWORK1(21)   = number of sfc. obs. written for the merge day
C     IWORK1(22)   = number of onsite obs. written for the merge day


C*** Rework this debug and tie to STATUS(6,26)
C>>>>> debug for 'watching' header records
C
C      WRITE(DEV50,5599) MPYR,MPCMO,MPCDY,MPJDY,IWORK1(20),IWORK1(21),
C     &                  IWORK1(22)


C---- 2.  Read current day's UA data, if present.
C
      IF( IWORK1(20).GT.0 )THEN

      DO M=1,IWORK1(20)
         READ(DEV40,111,END=710,ERR=711,IOSTAT=IOFLAG)
     &        UAYR(M),UAMO(M),UADAY(M),UAHR(M),UALEV(M)


C>>>>>>> debug
C        WRITE(DEV50,5500) UAYR(M),UAMO(M),UADAY(M),UAHR(M),UALEV(M)


         IF( UALEV(M).GT.0 )THEN
            READ(DEV40,111,END=712,ERR=713,IOSTAT=IOFLAG)
     &                   ((UAOBS(M,JLEV,JV),JV=1,UAMV),JLEV=1,UALEV(M))

C>>>>>>>>>> debug
C           DO 2 JV = 1,UALEV(M)
C              WRITE(DEV50,5501) (UAOBS(M,JV,IVBL),IVBL=1,6)
C    2      CONTINUE


         ENDIF

      ENDDO

      ENDIF

C---- 3.  Read current day's SF data, if present.

      IF( IWORK1(21) .GT. 0 )THEN
         DO M = 1,IWORK1(21)
            READ(DEV40,110,END=720,ERR=721,IOSTAT=IOFLAG)
     &          (IWORK1(I),I=1,4),ISASOS24(IWORK1(4))
            HOUR = IWORK1(4)

C---------- Check hour: must be from 1 to 24, or there is an error

            IF( HOUR.LT.1 .OR. HOUR.GT.24 )THEN
               MESS =  BLNK80
               ECODE = 'E74'
               WRITE( MESS,1000)
1000           FORMAT(' SURFACE OBS NOT ON 1-to-24 HOUR CLOCK')
               CALL ERRHDL( MPJDY,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF

            IF( ISASOS24(HOUR).NE.'N' .AND. ISASOS24(HOUR).NE.'n' .AND. 
     &          ISASOS24(HOUR).NE.'A' .AND. ISASOS24(HOUR).NE.'a' )THEN
               MESS =  BLNK80
               ECODE = 'E81'
               WRITE( MESS,1010)
1010           FORMAT(' ASOS flag is invalid - check the MERGED data')
               CALL ERRHDL( MPJDY,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ENDIF
            
C           If either of the two checks above returns an error return to 
C            the calling program (MPMET)
            IF( ITEST .EQ. 1 )THEN 
               RETURN
            ENDIF

            SFYR(HOUR)  = IWORK1(1)
            SFMO(HOUR)  = IWORK1(2)
            SFDAY(HOUR) = IWORK1(3)
            SFHR(HOUR)  = IWORK1(4)

            READ(DEV40,111,END=722,ERR=723,IOSTAT=IOFLAG)
     &                                  (SFOBS(HOUR,IVBL+29),IVBL=1,22)

         ENDDO

C------- Special logic to handle missing data for hour 24
C        when other 23 hours are present - primarily for the end of file
C        on CD-144 and SCRAM formats

         IF( IWORK1(21).EQ.23 .AND. SFYR(24).EQ.0 )THEN

            SFYR(24)  = SFYR(23)
            SFMO(24)  = SFMO(23)
            SFDAY(24) = SFDAY(23)
            SFHR(24)  = 24

            ISASOS24(24) = ISASOS24(23)

            DO IVBL=1,22
               SFOBS(24,IVBL+29) = SFOBS(23,IVBL+29)
            ENDDO

C---------- Write warning to message file

            MESS =  BLNK80
            ECODE = 'I70'
            WRITE( MESS,1500 )
1500        FORMAT(' SWAPPED HR 23 INTO HR 24 FOR SURFACE OBS')
            CALL ERRHDL( MPJDY,PATH,ECODE,LOC,MESS )

         ENDIF
      ENDIF

C---- 4.  Read current day's hrly-avg 1-min ASOS data, if present.

      IF( IWORK1(23) .GT. 0 )THEN
         DO M = 1,IWORK1(23)
            READ(DEV40,*,END=740,ERR=741,IOSTAT=IOFLAG)
     &         ITEMPYR, ITEMPMO, ITMEPDAY, ITEMPHR,
     &         TEMPWS, TEMPWD
            IHOUR = ITEMPHR

C---------- Check hour: must be from 1 to 24, or there is an error

            IF( ITEMPHR .LT. 1 .OR. ITEMPHR .GT. 24 )THEN
               MESS =  BLNK80
               ECODE = 'E74'
               WRITE( MESS,1600)
1600           FORMAT(' 1-MIN ASOS NOT ON 1-to-24 HOUR CLOCK')
               CALL ERRHDL( MPJDY,PATH,ECODE,LOC,MESS )
               ITEST = 1
               RETURN
            ENDIF

            IASOSYR(IHOUR)   = ITEMPYR
            IASOSMO(IHOUR)   = ITEMPMO
            IASOSDAY(IHOUR)  = ITMEPDAY
            IASOSHR(IHOUR)   = ITEMPHR
            ASOSOBS(IHOUR,1) = TEMPWS
            ASOSOBS(IHOUR,2) = TEMPWD

         ENDDO
      ENDIF

C---- 5.  Read current day's OS data, if present.

      IF( IWORK1(22) .GT. 0 )THEN
         DO M =1,IWORK1(22)
            CALL OSREAD(6,MPJDY,HOUR,ITEST)

            IF( ITEST .EQ. 2 )THEN
C------------- Move onsite data from work arrays into correct arrays
               CALL OSSWAP(HOUR)

            ELSEIF( ITEST .EQ. 3 )THEN
               GO TO 730

            ELSE
               GO TO 731
            ENDIF

         ENDDO

      ENDIF

      ITEST = 2

      RETURN

C=======================================================================

  110 FORMAT(4(I8,1X),A1)
  111 FORMAT(10(I8,1X))
 5500 FORMAT(1X,4I2,1X,I3,6(1X,I5))
 5501 FORMAT(6(1X,I6))
 5502 FORMAT(1X,4I2,10(1X,I5),/,8X,12(1X,I5))
 5598 FORMAT(1X,A3,A80)
 5599 FORMAT(1X,7(1X,I10))

C=======================================================================

C---- EOF and error conditions

C---- EOF on master header:
  700 CONTINUE
      IF( ICOUNT .EQ. 0 )THEN
C----    End-of-file reached before any data read, issue error message
         MESS =  BLNK80
         ECODE = 'E71'
         WRITE( MESS,1900 ) 
1900     FORMAT(' EOF reached on MERGE file before any data were read!')
         CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ELSE
C----    Normal end-of-file encountered, issue informational message
         MESS =  BLNK80
         ECODE = 'I79'
         WRITE( MESS,2000 ) JMPDATE
2000     FORMAT(' EOF reached on input data file after ' ,I6.6)
         CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 3
      ENDIF
      RETURN

C---- ERR on master header:
  701 CONTINUE
      MESS =  BLNK80
      ECODE = 'E70'
      WRITE( MESS,2500 ) MPJDY+1
2500  FORMAT(' ERROR Reading master header on Julian day ', I3.3)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on UA header:
  710 CONTINUE
      MESS =  BLNK80
      ECODE = 'E71'
      WRITE( MESS,2600 ) JMPDATE
2600  FORMAT(' EOF reading UPPERAIR header on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on UA header:
  711 CONTINUE
      MESS =  BLNK80
      ECODE = 'E71'
      WRITE( MESS,2700 ) JMPDATE
2700  FORMAT(' ERROR reading UPPERAIR header on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on UA data:
  712 CONTINUE
      MESS =  BLNK80
      ECODE = 'E71'
      WRITE( MESS,2800 ) JMPDATE
2800  FORMAT(' EOF reading UPPERAIR data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on UA data:
  713 CONTINUE
      MESS =  BLNK80
      ECODE = 'E71'
      WRITE( MESS,2900 ) JMPDATE
2900  FORMAT(' ERROR reading UPPERAIR data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on SF header:
  720 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,3000 ) JMPDATE
3000  FORMAT(' EOF reading SURFACE header on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on SF header:
  721 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,3500 ) JMPDATE, IOFLAG
3500  FORMAT(' ERROR reading SURFACE header on ', I6.6,
     &       '; I/O STAT =',I8)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on SF data:
  722 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,3600 ) JMPDATE
3600  FORMAT(' EOF reading SURFACE data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on SF data:
  723 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,3700 ) JMPDATE
3700  FORMAT(' ERROR reading SURFACE data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on OS data:
  730 CONTINUE
      MESS =  BLNK80
      ECODE = 'E73'
      WRITE( MESS,4000 ) JMPDATE
4000  FORMAT(' EOF reading ONSITE data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on OS data:
  731 CONTINUE
      MESS =  BLNK80
      ECODE = 'E73'
      WRITE( MESS,4500 ) JMPDATE
4500  FORMAT(' ERROR reading ONSITE data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- EOF on 1-min ASOS data:
  740 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,5000 ) JMPDATE
5000  FORMAT(' EOF reading 1-MIN ASOS data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN

C---- ERR on 1-min ASOS data:
  741 CONTINUE
      MESS =  BLNK80
      ECODE = 'E72'
      WRITE( MESS,5100 ) JMPDATE
5100  FORMAT(' ERROR reading 1-MIN ASOS data on ', I6.6)
      CALL ERRHDL( ICOUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1

      RETURN
      END

