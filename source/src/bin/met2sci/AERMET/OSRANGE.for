      SUBROUTINE OSRANGE(KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ISTAT)
C=====================================================================**
C          OSRANGE Module of the AERMET Meterological Preprocessor
C
C     Purpose: Reads a RANGE keyword record for OS pathway that the user 
C              has specified to change the QA value for one or more of 
C              the parameters for the variable
C
C     Called By: OSCARD
C
C     Form:      RANGE  variable  lower   <[=]   upper   missing
C     Arguments:        NAME      LBOUND   KEY   UBOUND  MISS
C
C            variable = variable name as defined in tables in App. C
C            lower    = lower bound for range of values in QA
C            <[=]     = exclude ( < ) or include ( <= ) endpoints
C                       in the range of values
C            upper    = upper bound for range of values in QA
C            missing  = missing value indicator
C
C     NOTE:  user may specify lower, upper, and missing as type REAL,
C            but KEY is interpreted as an INTEGER
C
C-----------------------------------------------------------------------

C------ Variable Declarations


      IMPLICIT NONE
      
      INTEGER   KEY,ISTAT,ITEST
      REAL      MISS,LBOUND,UBOUND
      CHARACTER CARD*(*), NAME*4, KEY1*1, KEY2*1

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C     NAME       CHAR*4 variable name
C     KEY        Type of range check (1 or 2)
C     MISS       New missing value
C     LBOUND     New lower bound for range checks
C     UBOUND     New upper bound for range checks

C---- Data Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'OSRANGE'
      ISTAT = 0

C---- Get new parameters for QA check data

C---- Variable name                                      ---- CALL GETWRD
      BUF08(1) = '   VNAME'
      BUF08(2) = BLNK08
      CALL GETWRD( 2,KOUNT,CARD,2,8,2,BUF08(1),BUF08(2),ITEST )
      IF( ITEST .EQ. 2 )THEN
         NAME = BUF08(2)(1:4)
      ELSE
C------- GETWRD failed
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS, 500 )
 500     FORMAT(' ERROR FROM S.GETWRD: VARIABLE NAME')
      ENDIF
      ISTAT = ITEST

C---- Redfine the include/exclude endpoints symbol       ---- CALL GETWRD
      IF( ISTAT.EQ.2 )THEN
         BUF08(1) = '    <[=]'
         BUF08(3) = BLNK08
         CALL GETWRD( 4,KOUNT,CARD,1,8,2,BUF08(1),BUF08(3),ITEST )

         IF( ITEST.EQ.2 )THEN
            READ( BUF08(3),1000,IOSTAT=IOFLAG ) KEY1,KEY2
1000        FORMAT( 2a1 )
            IF( IOFLAG .EQ. 0 )THEN
C------------- Internal read succeeded
               ITEST = 0
               KEY = IC2(4) - IC1(4) + 1
               IF( KEY.LE.0 .OR. KEY.GT.2  .OR. KEY1.NE.'<' )THEN
                  ITEST = 1
               ENDIF

               IF( KEY2.EQ.'=' .OR. KEY2.EQ.' ' )THEN
                  CONTINUE
               ELSE
                  ITEST = 1
               ENDIF

            ELSE
               ITEST = 1
            ENDIF

            IF( ITEST .EQ. 1 )THEN
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE( MESS,2500 ) BUF08(3),NAME
2500           FORMAT(' ',A8,' NOT RECOGNIZED FOR: ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF

         ELSE
C---------- GETWRD failed
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS, 510 ) NAME
 510        FORMAT(' ERROR FROM S.GETWRD: "<=" FOR ', A8)
            ISTAT = 1

         ENDIF

      ENDIF


C---- Redefine the missing data indicator                ---- CALL GETWRD
      IF( ISTAT .EQ. 2 )THEN
         BUF08(1) = ' MISSING'
         BUF08(4) = BLNK08
         CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )

         IF( ITEST.EQ.2 )THEN
            READ( BUF08(4),1100,IOSTAT=IOFLAG ) MISS
1100        FORMAT(F8.0)
            IF( IOFLAG.NE.0 )THEN
C------------- Internal read failed
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE( MESS,3000 ) BUF08(4)
3000           FORMAT(' Error decoding the OS MISSING indicator: ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF

         ELSE
C---------- GETWRD failed
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS, 520 ) NAME
 520        FORMAT(' ERROR FROM S.GETWRD: MISSING VALUE FOR ', A8)
            ISTAT = 1
         ENDIF
      ENDIF

C---- Redefine the lower bound                           ---- CALL GETWRD
      IF( ISTAT .EQ. 2 )THEN
         BUF08(1) = '  LBOUND'
         BUF08(5) = BLNK08
         CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(5),ITEST )

         IF( ITEST.EQ.2 )THEN
            READ( BUF08(5),1100,IOSTAT=IOFLAG ) LBOUND
            IF( IOFLAG.NE.0 )THEN
C------------- Internal read succeeded
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE( MESS,4000 ) BUF08(5)
4000           FORMAT(' Error decoding the OS LOWER BOUND: ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF
         ELSE
C---------- GETWRD failed
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS, 530 ) NAME
 530        FORMAT(' ERROR FROM S.GETWRD: LOWER BOUND FOR ', A8)
            ISTAT = 1
         ENDIF
      ENDIF

C---- Redefine the upper bound                           ---- CALL GETWRD
      IF( ISTAT .EQ. 2 )THEN
         BUF08(1) = '  UBOUND'
         BUF08(6) = BLNK08
         CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(6),ITEST )

         IF( ITEST.EQ.2 )THEN
            READ( BUF08(6),1100,IOSTAT=IOFLAG ) UBOUND
            IF( IOFLAG.NE.0 )THEN
C------------- Internal read succeeded
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE( MESS,5000 ) IOFLAG,BUF08(6)
5000           FORMAT(' Error decoding the OS UPPER BOUND: ',A8)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF
         ELSE
C---------- GETWRD failed
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS, 540 ) NAME
 540        FORMAT(' ERROR FROM S.GETWRD: UPPER BOUND FOR: ', A8)
            ISTAT = ITEST
         ENDIF
      ENDIF

      RETURN
      END

