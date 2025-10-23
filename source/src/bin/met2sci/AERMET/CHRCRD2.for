      SUBROUTINE CHRCRD2( KOUNT,CARD,ISTAT )
C=====================================================================**
C          CHRCRD2 Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes the secondary surface characteristics
C
C     Initial Release:  December 1992
C
C     Revision History:
C          1/27/97  moved from ONSITE pathway
C
C          12/2008  copied CHRCRD routine to CHRCRD2 to process the 
C                   secondary surface characteristics
C
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE

      CHARACTER CARD*(*)
      INTEGER   ISTAT,FREQ,SECT,ITEST,MN1,MN2, I, J
      REAL      ALBED,BOWENR,ROUGH

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'                      
      INCLUDE 'MP1.INC'
      INCLUDE 'MP2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT    Process status 1 = error in processing
C                              2 = processing ok
C      CARD     Record with the surface characteristics
C      FREQ     Frequency index,
C                      If MONTHLY, then FREQ is 1 through 12
C                      If SEASONAL, then FREQ is 1 through 4 
C                         1 = winter months: 12,1,2
C                         2 = spring months:  3,4,5
C                         3 = summer months:  6,7,8
C                         4 = autumn months:  9,10,11
C                      If ANNUAL, FREQ is 1
C      SECT     Wind direction sector, must be le. OSNWDS
C      ICHCNT   Number of valid SITE_CHAR keywords encountered
C      GOTCHR   Tracks presence of valid surface characteristics
C               based on frequency and sector; array with values
C               of 0 and 1

C---- Data Initializations

      PATH = 'METPREP'
      LOC  = 'CHRCRD2' 
      ISTAT = 0


C---- Check the number of fields on the record: must be 6

      IF( NWORDS.LT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''SITE_CHAR2'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.6 )THEN
         ECODE = 'E04'
         MESS =  BLNK80      
         WRITE( MESS,1100 )
1100     FORMAT(' Too many fields on ''SITE_CHAR2'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF 

C---- Check that a valid FREQ_SECT keyword is defined

      IF( SFCFRQ2 .EQ. 0 )THEN
C------- FREQ_SECT2 not defined
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,2000 ) 
2000     FORMAT(' Missing or misplaced ''FREQ_SECT2'' keyword; ',
     &          '''FREQ_SECT2'' must appear before ''SITE_CHAR2''.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         OSNWDS2 = 0
         RETURN

      ELSEIF( SFCFRQ2 .EQ. 1 )THEN
C------- FREQ_SECT2 in error
         ECODE = 'E15'
         MESS =  BLNK80
         WRITE( MESS,2100 ) 
2100     FORMAT(' Error on ''FREQ_SECT2''; ',
     &          '''SITE_CHAR2'' NOT processed!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         OSNWDS2 = 0
         RETURN
      ENDIF

C---- Decipher surface characteristics

      FREQ = 0
      SECT = 0
      ALBED  = 0.0
      BOWENR = 0.0
      ROUGH  = 0.0
      CALL VALCRD( KOUNT,CARD,FREQ,SECT,ALBED,BOWENR,ROUGH,ITEST )
      IF( ITEST .EQ. 1 )THEN
         ISTAT = ITEST
         RETURN
      ENDIF

      IF( SECT .LE. 0 .OR. SECT .GT. OSNWDS2 )THEN
C------- Invalid sector index
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4000 ) SECT, OSNWDS2
4000     FORMAT(' Number of SECTORs (',I2,') > # allowed (',I2,
     &          ') for ''SITE_CHAR2''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Check the value of the surface characteristics in a general sense

      IF( ALBED.LE.0.0 .OR. ALBED.GT.1.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4500 ) ICHCNT2,albed
4500     FORMAT(' ALBEDO invalid on secondary SITE_CHAR2 # ',
     &          I3,': ',F8.3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      IF( BOWENR .LE. 0.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4600 ) ICHCNT2,bowenr
4600     FORMAT(' BOWEN RATIO invalid on secondary SITE_CHAR2 # ',
     &          I3,': ',F8.3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

      IF( ROUGH .LE. 0.0 )THEN
         ECODE = 'E06'
         MESS =  BLNK80
         WRITE( MESS,4700 ) ICHCNT2,rough
4700     FORMAT(' ROUGHNESS invalid on secondary SITE_CHAR2 # ',
     &          I3,': ',F8.5)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Store values in OSSFC2 arrays

      IF( SFCFRQ2 .EQ. 2 )THEN
C------- The FREQ-SECT2 keyword was successfully processed and is
C        defined as MONTHLY

C------- Check frequency on the SITE_CHAR keyword
         IF( FREQ.GE.1 .AND. FREQ.LE.12 )THEN
            OSSFC2(FREQ,SECT,1) = ALBED
            OSSFC2(FREQ,SECT,2) = BOWENR
            OSSFC2(FREQ,SECT,3) = ROUGH
            IF( ISTAT .NE. 1 ) ISTAT = 2
         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5000 ) FREQ
5000        FORMAT(' Secondary frequency index =', I3,
     &             '; out of range for MONTHLY option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSEIF( SFCFRQ2 .EQ. 3 )THEN
C------- The FREQ-SECT2 keyword was successfully processed and is
C        defined as SEASONAL

C------- Check frequency on the SITE_CHAR2 keyword

         IF( FREQ.GE.1 .AND. FREQ.LE.4 )THEN
C---------- Loop on proper months for given 'SEASON'
            MN1 = 12 + (FREQ-1)*3                 
            MN2 = MN1 + 2
            DO I=MN1,MN2
               IF( I.GT.12 )THEN
                  J = I - 12
               ELSE
                  J = I
               ENDIF
               OSSFC2(J,SECT,1) = ALBED
               OSSFC2(J,SECT,2) = BOWENR
               OSSFC2(J,SECT,3) = ROUGH

            ENDDO
            IF( ISTAT .NE. 1 ) ISTAT = 2

         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5100 ) FREQ
5100        FORMAT(' Secondary frequency index =', I3,
     &             '; out of range for SEASONAL option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1           
         ENDIF
           
      ELSEIF( SFCFRQ2.EQ.4 )THEN
C------- The FREQ-SECT2 keyword was successfully processed and is
C        defined as ANNUAL

C------- Check frequency on the SITE_CHAR2 keyword
         IF( FREQ.EQ.1 )THEN
            DO I=1,12
               OSSFC2(I,SECT,1) = ALBED
               OSSFC2(I,SECT,2) = BOWENR
               OSSFC2(I,SECT,3) = ROUGH
            ENDDO
            IF( ISTAT.NE. 1 ) ISTAT = 2

         ELSE
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,5200 ) FREQ
5200        FORMAT(' Secondary frequency index =', I3,
     &             '; out of range for ANNUAL option')
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1           

         ENDIF
      ENDIF

      IF( ISTAT .EQ. 2 )THEN
C------- Increment the counters:
C        ICHCNT2 = total number of valid SITE_CHAR keywords
C        GOTCHR2 = array of SITE_CHAR keywords based on frequency
C                  and sector: 1 ==> valid characteristics for the
C                                   period and sector 
         ICHCNT2 = ICHCNT2 + 1
         GOTCHR2(FREQ,SECT) = 1
      ENDIF

      RETURN
      END

