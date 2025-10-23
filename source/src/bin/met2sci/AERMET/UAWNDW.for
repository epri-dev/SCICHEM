      SUBROUTINE UAWNDW( KOUNT,CARD,WNDWBEGIN,WNDWEND,ISTAT )
C=====================================================================**
C          WINDOW Module of the AERMET Meteorological Preprocessor
C
C     PURPOSE:  Processes the UAWINDOW keyword - defines the data 
C               search window for the the upper air soundings;
C               only for use in Stage3 at this time.
C
C     Form:  UAWINDOW  WindowBegin  WindowEnd
C
C            WindowBegin = Beginnning of sounding window, in hours
C                          relative to preferred sounding (12Z, 00Z,
C                          of -12Z for default selection, or local
C                          sunrise for the UASELECT SUNRIS option)
C            WindowEnd   = Beginnning of sounding window, in hours
C                          relative to preferred sounding 
C
C     Called by:  MPCARD
C
C     Initial Release:  May 2008
C
C     Revision History:
C
C-----------------------------------------------------------------------

C---- Data Declarations

      IMPLICIT NONE
      
      CHARACTER CARD*(*)
      INTEGER   ISTAT, WNDWBEGIN, WNDWEND, ITEST

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C-----------------------------------------------------------------------
C        CARD         Record with UAWINDOW data
C-----------------------------------------------------------------------

C---- Variable Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'WINDOW'
      ISTAT = 0
      IWORK1(5) = 0
      IWORK1(6) = 0

C---- Check number of fields

      IF( NWORDS .LT. 3 )THEN
C ---    Not enough fields specified
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
1000     FORMAT(' Too few fields on ''UAWINDOW'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ELSEIF( NWORDS .GT. 3 )THEN 
C ---    Too many fields specified
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1010 )
1010     FORMAT(' Too many fields on ''UAWINDOW'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Fetch beginning hour of search window                         ---- CALL GETWRD
      BUF08(1) = ' WNDWBEG'
      BUF08(2) = BLNK08
      CALL GETWRD( 2,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      ISTAT = ITEST

      IF( ISTAT .NE. 1 )THEN
C------- Beginning hour successfully retrieved, save it
         READ( BUF08(2),'(I8)' )WNDWBEGIN
cprt         print *, ':',BUF08(2),':', WNDWBEGIN

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,1100 )
1100     FORMAT(' ERROR FROM S.GETWRD: SEARCH WINDOW - BEGINNING HR')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ENDIF


C---- Fetch last hour of search window                              ---- CALL GETWRD
      BUF08(1) = ' WNDWEND'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      ISTAT = ITEST
      
      IF( ISTAT .NE. 1 )THEN
C------- Last hour successfully retrieved, save it
         READ( BUF08(2),'(I8)' )WNDWEND
cprt         print *, ':',BUF08(2),':', WNDWEND

      ELSE
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,1110 )
1110     FORMAT(' ERROR FROM S.GETWRD: SEARCH WINDOW - LAST HR')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ENDIF

C---- Check for END window being less than BEGIN window
      IF( WNDWEND .LT. WNDWBEGIN )THEN
C----    END window is less than BEGIN window, which would reject
C        all soundings.  Issue error message and set ISTAT=1 to abort.
         ECODE = 'E06'
         MESS  = BLNK80
         WRITE( MESS,1120 )
 1120    FORMAT(' ERROR: UA WNDWEND < WNDWBEGIN on UAWINDOW!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF
      IF( WNDWBEGIN .LT. -12 )THEN
C----    BEGIN window is less than -12, which would allow daytime 
C        soundings from previous day.
C        Issue error message and set ISTAT=1 to abort.
         ECODE = 'E06'
         MESS  = BLNK80
         WRITE( MESS,1121 )
 1121    FORMAT(' ERROR: WNDWBEGIN < -12 on UAWINDOW!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF
      IF( WNDWEND .GT. 12 )THEN
C----    END window is greater than +12, which would allow daytime 
C        soundings from next day.
C        Issue error message and set ISTAT=1 to abort.
         ECODE = 'E06'
         MESS  = BLNK80
         WRITE( MESS,1122 )
 1122    FORMAT(' ERROR: WNDWEND > +12 on UAWINDOW!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      RETURN
      END

