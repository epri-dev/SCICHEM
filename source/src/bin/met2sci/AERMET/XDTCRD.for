      SUBROUTINE XDTCRD( KOUNT,CARD,YR1,MO1,DY1,YR2,MO2,DY2,ISTAT )
C=====================================================================**
C          XDTCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the XDATES runstream image
C
C     Called By:
C
C     Form:      XDATES   yb/mb/db   [to]   ye/me/de
C     Arguments:         YR1/MO1/DY1       YR2/MO2/DY2
C
C            yb/mb/db = beginning year, month, and day
C            [to]     = optional
C            ye/me/de = ending year, month, and day
C
C     Initial Release: December 15, 1992
C
C     Revision History:
C        7/30/99 - modified to handle 2-digit or 4-digit years and
C                  to work with year 2000.
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      

      CHARACTER CARD*(*),CDATES*10
      CHARACTER AYR1*4,AYR2*4,AMO1*2,AMO2*2,ADY1*2,ADY2*2
      INTEGER   YR1,MO1,DY1,YR2,MO2,DY2,ISTAT,l3,ITEST
      INTEGER   IDELIM1, IDELIM2, LENYR1, LENYR2, I4DYR1, I4DYR2
      INTEGER   IDATE1, IDATE2

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C      JULIAN         FUNCTION (CONVERTS GREGORIAN TO JULIAN)
C      YR1,YR2        START AND STOP YEARS FOR EXTRACTION
C      MO1,MO2        START AND STOP MONTHS FOR EXTRACTION
C      DY1,DY2        START AND STOP DAYS (GREGORIAN) FOR EXTRACTION
C      ISTAT          PROCESS STATUS 1 = ERROR IN PROCESSING
C                                    2 = PROCESSING OK
C      WIDTH          COMPUTED WIDTH OF A WORD (VALUE)
C      CARD           RECORD WITH EXTRACT DATA


C---- Data Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'XDTCRD'
      ISTAT = 2

C---- Check the number of fields on the record:
C     a minimum of 3 and a maximum of 4 (with the option 'TO')

      L3 = IC2(3) - IC1(3) + 1

      IF( NWORDS.LT.3 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''XDATES'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ELSEIF( NWORDS .GT. 4 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1010 )
1010     FORMAT(' Too many fields on ''XDATES'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


      IF( NWORDS.EQ.3 .AND. L3.LT.6 )THEN
C------- The third, and last field, on this record is not long enough
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1020 )
1020     FORMAT(' May be too few fields on ''XDATES'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

C---- Read start date (note: dates are LEFT justified in the field
C     when returned from s.getwrd)

      IDELIM1 = 0
      IDELIM2 = 0
      BUF08(1) = 'DATES   '
      CDATES = '          '
      CALL GETWRD(2,KOUNT,CARD,6,10,2,BUF08(1),CDATES,ITEST)

      IF( ITEST.EQ.2 )THEN
         IDELIM1 = INDEX(CDATES,'/')
         IF( IDELIM1 .NE. 0 )THEN
C---------- Located first delimeter in start date string

            IDELIM2 = INDEX(CDATES,'/',BACK=.TRUE.)
            IF( IDELIM2 .NE. 0 )THEN
C------------- Located second delimeter in start date string

               AYR1 = CDATES(1:IDELIM1-1)
               AMO1 = CDATES(IDELIM1+1:IDELIM2-1)
               ADY1 = CDATES(IDELIM2+1:10)
               LENYR1 = IDELIM1 - 1

               READ (AYR1,'(I4)',IOSTAT=IOFLAG) YR1
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3000 ) CDATES
 3000             FORMAT(' XDATES - ERROR READING START YR: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               READ (AMO1,'(I2)',IOSTAT=IOFLAG) MO1
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3001 ) CDATES
 3001             FORMAT(' XDATES - ERROR READING START MONTH: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               IF( MO1 .LT. 0  .OR.  MO1 .GT. 12 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3002 ) CDATES
 3002             FORMAT(' XDATES - INVALID START MONTH: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               READ (ADY1,'(I2)',IOSTAT=IOFLAG) DY1
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3003 ) CDATES
 3003             FORMAT(' XDATES - ERROR READING START DAY: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               IF( DY1 .GT. 31 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3004 ) CDATES
 3004             FORMAT(' XDATES - INVALID START DAY: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

            ELSE
C------------- Failed to locate second delimeter in start date string
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,2000 ) BUF08(2)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF                   ! IDELIM2 = 0

         ELSE
C---------- Failed to locate first delimeter in start date string
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,2000 ) CDATES
2000        FORMAT(' START DATE NOT PROPERLY SPECIFIED: ',A10)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF                      ! IDELIM1 = 0

      ELSE
C------- Problem getting the start date from GETWRD
         ISTAT = 1
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3200 )
3200     FORMAT(' ERROR FROM S.GETWRD: START DATE ')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

      ENDIF

C-----> DEBUG
C       WRITE(MESS,3300) CDATES,YR1,MO1,DY1
C3300   FORMAT(' XDT1 ',A10,1X,3I3)
C       CALL ERRHDL( KOUNT,PATH,'I**',LOC,MESS)


C---- Read end date - the last field on the record

      IDELIM1 = 0
      IDELIM2 = 0
      BUF08(1) = 'DATES   '
      CDATES = '          '
      CALL GETWRD(NWORDS,KOUNT,CARD,6,10,2,BUF08(1),CDATES,ITEST)

      IF( ITEST.EQ.2 )THEN
         IDELIM1 = INDEX(CDATES,'/')
         IF( IDELIM1 .NE. 0 )THEN
C---------- Located first delimeter in end date string

            IDELIM2 = INDEX(CDATES,'/',BACK=.TRUE.)
            IF( IDELIM2 .NE. 0 )THEN
C------------- Located second delimeter in end date string

               AYR2 = CDATES(1:IDELIM1-1)
               AMO2 = CDATES(IDELIM1+1:IDELIM2-1)
               ADY2 = CDATES(IDELIM2+1:10)
               LENYR2 = IDELIM1 - 1

               READ (AYR2,'(I4)',IOSTAT=IOFLAG) YR2
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3010 ) CDATES
 3010             FORMAT(' XDATES - ERROR READING END YEAR: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               READ (AMO2,'(I2)',IOSTAT=IOFLAG) MO2
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3011 ) CDATES
 3011             FORMAT(' XDATES - ERROR READING END MONTH: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               IF( MO2 .LT. 0  .OR.  MO2 .GT. 12 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3012 ) CDATES
 3012             FORMAT(' XDATES - INVALID END MONTH: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               READ (ADY2,'(I2)',IOSTAT=IOFLAG) DY2
               IF(IOFLAG .NE. 0 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3013 ) CDATES
 3013             FORMAT(' XDATES - ERROR READING END DAY: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

               IF( MO1 .LT. 0  .OR.  MO1 .GT. 12 )THEN
                  ECODE = 'E06'
                  MESS =  BLNK80
                  WRITE( MESS,3014 ) CDATES
 3014             FORMAT(' XDATES - INVALID END DAY: ',A10)
                  CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
               ENDIF

            ELSE
C------------- Failed to locate second delimeter in end date string
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,2000 ) BUF08(2)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF                   ! IDELIM2 = 0

         ELSE
C---------- Failed to locate first delimeter in end date string
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,2000 ) CDATES
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF                      ! IDELIM1 = 0

      ELSE
C------- Problem getting the end date from GETWRD
         ISTAT = 1
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3201 )
3201     FORMAT(' ERROR FROM S.GETWRD: END DATE ')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

      ENDIF


C---- Decoded dates; check to be sure they are properly ordered

C      A.  Check YR1 AND YR2, and MO1 AND MO2
C          (1) If YR1 > YR2             - ERROR CONDITION
C          (2) If MO1 or MO2 > 12       - ERROR CONDITION

      IF( ISTAT.NE.1 )THEN
C------- Start and end dates parsed properly - check to be sure the
C        start date is not after the end date
C        By using the 4-digit year, this subprogram is Y2K compatible
         IF( YR1 .GE. 50 .AND. YR1 .LE. 99 )THEN
            I4DYR1 = YR1 + 1900
         ELSEIF( YR1 .GE. 0 .AND. YR1 .LE. 49 )THEN
            I4DYR1 = YR1 + 2000
         ELSE
            I4DYR1 = YR1
         ENDIF

         IF( YR2 .GE. 50 .AND. YR2 .LE. 99 )THEN
            I4DYR2 = YR2 + 1900
         ELSEIF( YR2 .GE. 0 .AND. YR2 .LE. 49 )THEN
            I4DYR2 = YR2 + 2000
         ELSE
            I4DYR2 = YR2
         ENDIF

         IDATE1 = I4DYR1*10000 + MO1*100 + DY1
         IDATE2 = I4DYR2*10000 + MO2*100 + DY2


         IF( IDATE1 .GT. IDATE2 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) IDATE1, IDATE2
5000        FORMAT(' START DATE (',I8,') AFTER END DATE (',I8,')')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

         IF( I4DYR1 .LT. 1900 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5100 ) I4DYR1
5100        FORMAT(' INVALID START YEAR: PRIOR TO 1900 (',I5,')' )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

         IF( I4DYR2 .LT. 1900 )THEN
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5200 ) I4DYR2
5200        FORMAT(' INVALID END YEAR: PRIOR TO 1900 (',I5,')' )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

         IF( ISTAT .NE. 1 )THEN
C---------- Convert the years to 2-digits for the remainder of the
C           processing; note that other routines in AERMET assume that
C           there are no dates prior to 1900.

            IF( YR1 .GT. 1900 )THEN
               YR1 = MOD(YR1,100)
            ENDIF

            IF( YR2 .GT. 1900 )THEN
               YR2 = MOD(YR2,100)
            ENDIF
         ENDIF
      ENDIF


      RETURN
      END

