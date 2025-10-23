      SUBROUTINE INTEQA( NPATH,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,
     &                   IYR,IMO,IDY,IHR,QAFLG )
C=====================================================================**
C            INTEQA Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To perform range check on integer valued variables.
C
C     Called by:     UAQASS
C                    SFQASS
C                    OSQA
C
C     Calls to:      ERROR
C
C     Revision History:
C        10/31/96
C          - modified the calling arguments and QA messages to include
C            the date and time
C          - renamed the subroutine from INTECK
C
C-----------------------------------------------------------------------
C
C     Local variables
C

      IMPLICIT NONE
      
      INTEGER       JJJ, IYR, IMO, IDY, IHR

      INTEGER       NPATH,NUM,KEY,MISS,LOWER,UPPER,VALUE,QAFLG
      CHARACTER     NAME*4
      CHARACTER     E1*1

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C        NPATH      Pathway number
C        NUM        Passed number to routine, may or may not be of use;
C                   originally was to help identify record having probs.
C        KEY        Type of range check 1 exclude bounds
C                                       2 include bounds
C        LOWER      Integer valued lower bound
C        UPPER      Integer valued upper bound
C        MISS       Integer valued missing value key
C        VALUE      Integer value being tested
C        NAME       Name of value being range checked
C        QAFLG      As an input argument, defines the type of message
C                       5 = 'Q' - QA message
C                       6 = 'I' - Infomation message
C                   As an output argument, defines the status of QA
C                   returned to calling program
C                       0 = QA passed
C                       1 = Missing data
C                       2 = Lower bound violation
C                       3 = Upper bound violation
C
C-----------------------------------------------------------------------

C---- Initialize values
      PATH  = PATHWD(NPATH)
      LOC   = 'INTEQA'
      JJJ   = IYR*10000 + IMO*100 +IDY

      IF(QAFLG .EQ. 5) THEN
         E1 = 'Q'
      ELSEIF(QAFLG .EQ.6) THEN
         E1 = 'I'
      ENDIF
      QAFLG = 0

      IF( VALUE.EQ.MISS )THEN
C ***    Value to QA is missing
         QAFLG = 1
         RETURN
      ENDIF

      IF( KEY.LE.1 )THEN
C ***    Values at the boundaries are considered violations

         IF( VALUE.GE.UPPER )THEN
            QAFLG = 3
            MESS =  BLNK80
            WRITE( MESS,1000 ) NAME,VALUE,UPPER,IHR
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
 1000    FORMAT(' UB: ',A4,'=',I8,' >= ',I8,' FOR HR ',I3.2)

         IF( VALUE.LE.LOWER )THEN
            QAFLG = 2
            MESS =  BLNK80
            WRITE( MESS,2000 ) NAME,VALUE,LOWER,IHR
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
 2000    FORMAT(' LB: ',A4,'=',I8,' <= ',I8,' FOR HR ',I3.2)

      ELSE
C ***    Values at the boundaries are not considered violations

         IF( VALUE.GT.UPPER )THEN
            QAFLG = 3
            MESS =  BLNK80
            WRITE( MESS,3000 ) NAME,VALUE,UPPER,IHR
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
 3000    FORMAT(' UB: ',A4,'=',I8,' > ',I8,' FOR HR ',I3.2)


         IF( VALUE.LT.LOWER )THEN
            QAFLG = 2
            MESS =  BLNK80
            WRITE( MESS,4000 ) NAME,VALUE,LOWER,IHR
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            RETURN
         ENDIF
 4000    FORMAT(' LB: ',A4,'=',I8,' < ',I8,' FOR HR ',I3.2)

      ENDIF

  500 FORMAT(I2)
      END

