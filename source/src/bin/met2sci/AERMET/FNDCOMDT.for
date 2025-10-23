      SUBROUTINE FNDCOMDT(PATH)
      
C ----------------------------------------------------------------------
C     FNDCOMDT - Find Commission Date
C     
C     Finds the ASOS commission date based on the WBAN from the sfc met 
C     file, stored in the global variable iSfcObsWBAN, read from either   
C     the sfc data file header (SAMSON and SCRAM) or the first 
C     observation record in the sfc met file (ISHD, HUSWO, CD144, TD3280).
C     
C     Assumes all global variables.
C     Variable PATH is declared in WORK1.INC, set in a DATA statement
C     in each of the surface read data templates.
C
C     Commission date is searched only once per sfc met file, at the end
C     of this template, SrchCommDate is set to FALSE so it will not 
C     be called again.  If date is found in proper format, GotCommDate
C     is set to TRUE.  Commission date must be converted an integer
C     for comparison later.
C
C     1/18/2010, MACTEC Engineering and Consulting, Inc.
C ----------------------------------------------------------------------

    
      USE     mod_AsosCommDates
      
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
      
      INTEGER iasos
      
      DATA LOC/'FNDCOMDT'/

      call InitAsosCommInfo (ASOSCommDates)

C     Find the commissioning date - 
C     cCommDate is character, iCommDate is integer
C     Loop over commission dates array to find WBAN and set comm date
      DO iasos=1,NCommDates
         IF( ASOSCommDates(iasos) % IWBAN   .eq. iSfcObsWBAN .OR.
     &       ASOSCommDates(iasos) % INTCALL .eq. cSfcObsCALL4 .OR.
     &       ASOSCommDates(iasos) % USACALL .eq. cSfcObsCALL3 )THEN
         
            cCommDate = ASOSCommDates(iasos) % CommisDate

C           convert character date to integer date
            READ(cCommDate,'(I8)',ERR=910) iCommDate

C           set GotCommDate flag to true to indicate date was found in proper format 
            GotCommDate = .true.
            
C           Also get ASOS anemometer height for comparison with user-input
            ASOS_ANEM_HGT = ASOSCommDates(iasos) % ANEM_METERS

C           Assign Call ID values to global variables based on WBAN match
            IF(  ASOSCommDates(iasos) % IWBAN   .eq. iSfcObsWBAN )THEN
               cSfcObsCALL4 = ASOSCommDates(iasos) % INTCALL
               cSfcObsCALL3 = ASOSCommDates(iasos) % USACALL
            ENDIF
            
C           if conversion successful, write date, set GotCommDate flag
            MESS =  BLNK80
            ECODE = 'I41'
            WRITE(MESS,1020) iSfcObsWBAN, cSfcObsCALL4, cSfcObsCALL3, 
     &                       iCommDate
            CALL ERRHDL(0,PATH,ECODE,LOC,MESS)

            EXIT
         ENDIF
      ENDDO
      
  50  CONTINUE
  
C     if commission date not found, set to missing indicator and write message
      IF( .not. GotCommDate )THEN 
         iCommDate = -9
         MESS =  BLNK80
         ECODE = 'I81'
         WRITE(MESS,1030)iSfcObsWBAN, cSfcObsCALL4
         CALL ERRHDL(0,PATH,ECODE,LOC,MESS)
      ENDIF

C     Set SrchCommDate to true to indicate date has been searched for regardless
C     whether a date was found.
      SrchCommDate = .true.
      
      RETURN


C --- ERRORS ----------------------------------------------------------

C     Commission date found will not convert to an integer 
 910  CONTINUE
      MESS =  BLNK80
      ECODE = 'W49'
      WRITE(MESS,1010)cCommDate
      CALL ERRHDL(0, PATH, ECODE,LOC,MESS)
      GO TO 50
      
C --- FORMAT STATEMENTS -----------------------------------------------

1010  FORMAT(' ASOS commission date cannot convert to integer: ',A8)
1020  FORMAT(' ASOS commission date FOUND for WBAN = ',I6,
     &                                    '; CALL4 = ',A4,
     &                                    '; CALL3 = ',A3,
     &                                    '; CommDate = ',I8)
1030  FORMAT(' ASOS commission date NOT found for WBAN = ',I6,
     &                                   '; of Call_ID = ',A4) 

      END SUBROUTINE FNDCOMDT
