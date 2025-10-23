      SUBROUTINE REALQA(NPATH,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,
     &                  IYR,IMO,IDY,IHR,IMN,LVL,QAFLG)
C=====================================================================**
C            REALQA Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Performs range check on real valued variables.
C
C     Called by:     UAQASS
C                    OSQA
C
C     Calls to:      ERROR
C
C     Revision History:
C        10/31/96
C          - modified the calling arguments and QA messages to include
C            the date and time
C          - renamed the subroutine from REALCK
C
C-----------------------------------------------------------------------
C
C   Local variables
C

      IMPLICIT NONE
      
      INTEGER       NPATH,NUM,KEY,QAFLG
      INTEGER       JJJ, IYR, IMO, IDY, LVL, IHR, IMN
      REAL          MISS,LOWER,UPPER,VALUE
      CHARACTER     NAME*4
      CHARACTER     E1*1
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C        NPATH      Pathway number
C        NUM        Passed number to routine, may or may not be of use;
C                   Originally was to help identify record having probs.
C        KEY        Type of range check 1 exclude bounds
C                                       2 include bounds
C        LOWER      Real valued lower bound
C        UPPER      Real valued upper bound
C        MISS       Real valued missing value key
C        VALUE      Value being tested
C        NAME       Name of value being range checked
C        QAFLG      As an input argument, defines the type of message
C                       5 = 'Q' - QA message
C                       6 = 'I' - Information message (used for 
C                                 subhourly ONSITE data)
C                   As an output argument, defines the status of QA
C                   returned to calling program
C                       0 = QA passed
C                       1 = missing data
C                       2 = lower bound violation
C                       3 = upper bound violation
C
C-----------------------------------------------------------------------
C---- Initialize values

      PATH  = PATHWD(NPATH)
      LOC   = 'REALQA'
      JJJ   = IYR*10000 + IMO*100 +IDY

      IF( QAFLG .EQ. 5 )THEN
C ***   Set message code to 'Q' for QA messages, used
C       for UA data and hourly average ONSITE data
        E1 = 'Q'
      ELSEIF( QAFLG .EQ.6 )THEN
C ***   Set message code to 'I' for Informational
C       messages, used for subhourly ONSITE data
        E1 = 'I'
      ENDIF

C *** Reinitialize QAFLG to indicate status on 
C     return to calling subroutine
      QAFLG = 0

      IF( ABS( VALUE-MISS ) .LT. 0.01 )THEN
C ***    Value to QA is missing
         QAFLG = 1
         RETURN
      ENDIF

      IF( KEY.LE.1 )THEN
C ***    Values at the boundaries are considered violations

         IF( VALUE.GE.UPPER )THEN
            QAFLG = 3
            MESS =  BLNK80
            IF( E1.EQ.'I' )THEN
C ***          This is for subhourly ONSITE data, include hour in message
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,1000 ) NAME,VALUE,UPPER,IHR,IMN
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,1000 ) NAME,VALUE,UPPER,IHR,IMN,LVL
               ENDIF
            ELSE
C ***          This is for UA or hourly ONSITE data, don't include hour in msg
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,1100 ) NAME,VALUE,UPPER,IHR
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,1100 ) NAME,VALUE,UPPER,IHR,LVL
               ENDIF
            ENDIF
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            GO TO 999
         ENDIF
 1000    FORMAT(' UB: ',A4,'=',F10.2,' >= ',F10.2,' FOR HR/MN ',
     ^           I3.2,'/',I3.2,:,', LVL ',I3.2)
 1100    FORMAT(' UB: ',A4,'=',F10.2,' >= ',F10.2,' FOR HR ',I3.2,:,
     &          ', LVL ',I3.2)


         IF( VALUE.LE.LOWER )THEN
            QAFLG = 2
            MESS =  BLNK80
            IF( E1.EQ.'I' )THEN
C ***          This is for subhourly ONSITE data, include hour in message
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,2000 ) NAME,VALUE,LOWER,IHR,IMN
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,2000 ) NAME,VALUE,LOWER,IHR,IMN,LVL
               ENDIF
            ELSE
C ***          This is for UA or hourly ONSITE data, don't include hour in msg
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,2100 ) NAME,VALUE,LOWER,IHR
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,2100 ) NAME,VALUE,LOWER,IHR,LVL
               ENDIF
            ENDIF
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            GO TO 999
         ENDIF
 2000    FORMAT(' LB: ',A4,'=',F10.2,' =< ',F10.2,' FOR HR/MN ',
     ^           I3.2,'/',I3.2,:,', LVL ',I3.2)
 2100    FORMAT(' LB: ',A4,'=',F10.2,' =< ',F10.2,' FOR HR ',I3.2,:,
     &          ', LVL ',I3.2)

      ELSE
C ***    KEY=2; Values at the boundaries are not considered violations

         IF( VALUE.GT.UPPER )THEN
            QAFLG = 3
            MESS =  BLNK80
            IF( E1.EQ.'I' )THEN
C ***          This is for subhourly ONSITE data, include hour in message
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,3000 ) NAME,VALUE,UPPER,IHR,IMN
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,3000 ) NAME,VALUE,UPPER,IHR,IMN,LVL
               ENDIF
            ELSE
C ***          This is for UA or hourly ONSITE data, don't include hour in msg
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,3100 ) NAME,VALUE,UPPER,IHR
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,3100 ) NAME,VALUE,UPPER,IHR,LVL
               ENDIF
            ENDIF
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            GO TO 999
         ENDIF
 3000    FORMAT(' UB: ',A4,'=',F10.2,' >  ',F10.2,' FOR HR/MN ',
     ^           I3.2,'/',I3.2,:,', LVL ',I3.2)
 3100    FORMAT(' UB: ',A4,'=',F10.2,' >  ',F10.2,' FOR HR ',I3.2,:,
     &          ', LVL ',I3.2)

         IF( VALUE.LT.LOWER )THEN
            QAFLG = 2
            MESS =  BLNK80
            IF( E1.EQ.'I' )THEN
C ***          This is for subhourly ONSITE data, include hour in message
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,4000 ) NAME,VALUE,LOWER,IHR,IMN
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,4000 ) NAME,VALUE,LOWER,IHR,IMN,LVL
               ENDIF
            ELSE
C ***          This is for UA or hourly ONSITE data, don't include hour in msg
               IF( LVL .EQ. 0 )THEN
                  WRITE( MESS,4100 ) NAME,VALUE,LOWER,IHR
               ELSEIF( LVL .GT. 0 )THEN
                  WRITE( MESS,4100 ) NAME,VALUE,LOWER,IHR,LVL
               ENDIF
            ENDIF
            BUF02 = '  '
            WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
            ECODE = E1//BUF02
            CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
            GO TO 999
         ENDIF
 4000    FORMAT(' LB: ',A4,'=',F10.2,' < ',F10.2,' FOR HR/MN ',
     ^           I3.2,'/',I3.2,:,', LVL ',I3.2)
 4100    FORMAT(' LB: ',A4,'=',F10.2,' < ',F10.2,' FOR HR ',I3.2,:,
     &          ', LVL ',I3.2)

      ENDIF

  999 CONTINUE

C *** Now check for values that are not missing, but significantly beyond the
C     upper or lower bounds, to flag cases where the missing code used in the 
C     data doesn't match the missing code used by AERMET; a Warning message
C     will be used for ONSITE data to include these messages in the report file.

      IF( VALUE .GT. UPPER+0.5*ABS(UPPER-LOWER) )THEN
         MESS  = BLNK80
         BUF02 = '  '
         WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
         IF( E1.EQ.'I' )THEN
C ***       This is for subhourly ONSITE data, include hour in message
C           and set error code to 'W' for warning
            ECODE = 'W'//BUF02
            IF( LVL .EQ. 0 )THEN
               WRITE( MESS,5000 ) VALUE,NAME,IHR,IMN
            ELSEIF( LVL .GT. 0 )THEN
               WRITE( MESS,5000 ) VALUE,NAME,IHR,IMN,LVL
            ENDIF
         ELSE
C ***       This is for UA or hourly ONSITE data, don't include hour in msg
C           but set error code to 'W' for hourly ONSITE data or 'Q' for UA 
            IF( NPATH.EQ.4 )THEN
               ECODE = 'W'//BUF02
            ELSE
               ECODE = 'Q'//BUF02
            ENDIF
            IF( LVL .EQ. 0 )THEN
               WRITE( MESS,5100 ) VALUE,NAME,IHR
            ELSEIF( LVL .GT. 0 )THEN
               WRITE( MESS,5100 ) VALUE,NAME,IHR,LVL
            ENDIF
         ENDIF
         CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF
 5000 FORMAT(' Non-missing value (',F10.2,') much > UB;',
     &       ' check missing data code for ',A4,', HR/MN ',
     &         I3.2,'/',I3.2:,', LVL ',I3.2)
 5100 FORMAT(' Non-missing value (',F10.2,') much > UB;',
     &       ' check missing data code for ',A4,', HR ',I3.2,:,
     &       ', LVL ',I3.2)

      IF( VALUE .LT. LOWER-0.5*ABS(UPPER-LOWER) )THEN
         MESS  = BLNK80
         BUF02 = '  '
         WRITE(BUF02,500) NPATH*10 + 15 + QAFLG
         IF( E1.EQ.'I' )THEN
C ***       This is for subhourly ONSITE data, include hour in message
C           and set error code to 'W' for warning
            ECODE = 'W'//BUF02
            IF( LVL .EQ. 0 )THEN
               WRITE( MESS,6000 ) VALUE,NAME,IHR,IMN
            ELSEIF( LVL .GT. 0 )THEN
               WRITE( MESS,6000 ) VALUE,NAME,IHR,IMN,LVL
            ENDIF
         ELSE
C ***       This is for UA or hourly ONSITE data, don't include hour in msg
C           but set error code to 'W' for hourly ONSITE data or 'Q' for UA 
            IF( NPATH.EQ.4 )THEN
               ECODE = 'W'//BUF02
            ELSE
               ECODE = 'Q'//BUF02
            ENDIF
            IF( LVL .EQ. 0 )THEN
               WRITE( MESS,6100 ) VALUE,NAME,IHR
            ELSEIF( LVL .GT. 0 )THEN
               WRITE( MESS,6100 ) VALUE,NAME,IHR,LVL
            ENDIF
         ENDIF
         CALL ERRHDL( JJJ,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF
 6000 FORMAT(' Non-missing value (',F10.2,') much < LB;',
     &       ' check missing data code for ',A4,', HR/MN ',
     &         I3.2,'/',I3.2:,', LVL ',I3.2)
 6100 FORMAT(' Non-missing value (',F10.2,') much < LB;',
     &       ' check missing data code for ',A4,', HR ',I3.2,:,
     &       ', LVL ',I3.2)


  500 FORMAT(I2)
      END

