      SUBROUTINE VRCARD( KOUNT,CARD,ISTAT )
C=====================================================================**
C         Module VRCARD of the AERMET Meteorological Preprocessor
C
C     Purpose:  This routine processes the user's choices as to how the
C               input meteorological data are to be processed in
C               developing the output meteorological data file for
C               the chosen dispersion model.
C
C               These options are determined through the METHOD keyword.
C
C     Arguments
C        KOUNT   Passed in    Card image number
C        CARD    Passed in    Image of processor control information
C        ISTAT   Returned     Status of processing image
C                                1 = error occurred
C                                2 = all ok
C
C     Revision history:
C        12/15/92     Removed processing that is not pertinent to AERMET;
C                     Added actions to read filename, open and read
C                     random # data
C
C        01/29/93     Removed a jump into the middle of an
C                     IF..THEN..ENDIF structure
C
C        05/22/95     Removed the code to read the random numbers from
C                     a file; the numbers are now in BLOCK2.INC
C-----------------------------------------------------------------------

C---- Variable Declarations

      IMPLICIT NONE
      

      INTEGER      ISTAT, ITM, JACT
      CHARACTER CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Intialization

      PATH  = PATHWD(6)
      LOC   = 'VRCARD'
      ISTAT = 2

C---- Syntax:    METHOD  item  action

C     Valid 'items' and corresponding 'actions':
C                WIND_DIR   NORAND  or RANDOM
C                REFLEVEL   SUBNWS
C                STABLEBL   BULKRN  or ADJ_U*           ! RWB Ustar_adjustment option
C                ASOS_ADJ   NO_ADJ

C---- Check NWORDS: for the METHOD keyword, there must be
C     at least 3 fields including the keyword

      IF( NWORDS.LT.3 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 ) KOUNT
1000     FORMAT(' Too few fields on ''METHOD'' keyword; RECORD # ',I3)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Attempt fetch 'ITEM' - the length of each item is exactly 8
C     characters; items shorter than 8 characters are padded on
C     the right with blanks

      BUF08(10) = '   ITEM '
      BUF08(8) = BLNK04
      CALL GETWRD( 2,KOUNT,CARD,8,8,2,BUF08(10),BUF08(8),ISTAT)
      IF( ISTAT.EQ.1 )THEN
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,2000 )
2000     FORMAT(' ERROR FROM S.GETWRD: METHOD ''ITEM''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Look for match on list of 'ITEMS' (primary keyword on the METHOD card)
      DO ITM =1,NITEM
         IF( BUF08(8).EQ.ITEM(ITM) ) GOTO 21
      ENDDO

C---- Error condition to reach this point - no match on 'ITEM' list
      MESS =  BLNK80
      ECODE = 'E06'
      WRITE( MESS,3000 ) BUF08(8)
3000  FORMAT(1X,A8,': NO MATCH WITH ''METHOD'' ')
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      RETURN


  21  CONTINUE
  
C---- Fetch 3rd word to complete definition of CONTRL
      BUF08(9) = '  VALUE '
      BUF08(10) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,6,8,2,BUF08(9),BUF08(10),ISTAT)
      IF( ISTAT.EQ.1 )THEN
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,4000 )
4000     FORMAT(' ERROR FROM S.GETWRD: METHOD ITEM ''ACTION''')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Find match on 'ACTION' (secondary keyword on METHOD card)
C     To allow for slight misspelling, especially SUNRIS', the 
C     INDEX fucntion is used.
      DO JACT=1,NACT
         IF( INDEX(BUF08(10)(1:6),ACTION(JACT) ) .NE. 0 )THEN
            CONTRL(ITM) = JACT
            GOTO 22
         ENDIF
      ENDDO

C---- Error condition to reach this point - no match on 'ACTION' list.
      MESS =  BLNK80
      ECODE = 'E06'
      WRITE( MESS,5000 ) BUF08(10),ITEM(ITM)
5000  FORMAT(' NO MATCH ON ACTION: ',A8, ', FOR ',A8)
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      RETURN

C---- Does it make sense that for atmos. item (ITM)
C     that it is to be processed using ACTION (JACT)?
C     (Only ITEMs 1, 10, and 11 are in use)                            ! dtb #020 01204
C     ITEM  1: WIND_DIR   ACTION: 1 or 2 (NORAND or RANDOM)
C     ITEM 10: REFLEVEL   ACTION: 3 (SUBNWS)
c     ITEM 11: STABLEBL   ACTION: 4 (BULKRN)                           ! dtb #020 01204
c     ITEM 12: ASOSADJ    ACTION: 5 (NO_ADJ)
c     ITEM 11: STABLEBL   ACTION: 7 (ADJ_U*)                           ! rwb v12345

   22 CONTINUE

      IF( ITM .EQ. 1 )THEN
C------- Item: WIND_DIR

         IF( JACT .EQ. 1 .OR. JACT .EQ. 2 )THEN
            CONTINUE

         ELSE
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2010 ) ITEM(ITM)
2010        FORMAT(' INVALID ACTION FOR ', A8 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

      ELSEIF( ITM .EQ. 10 )THEN
C------- Item: REFLEVEL

         IF( JACT .EQ. 3 )THEN
C---------- Action: SUBNWS
C           Set the SUBSTNWS flag to true
C           (default is FALSE in BLOCK2)
            SUBSTNWS = .TRUE.

         ELSE
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2011 ) ITEM(ITM)
2011        FORMAT(' INVALID ACTION FOR ', A8 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

      ELSEIF( ITM .EQ. 11 )THEN                                       ! dtb #020 01204
C------- Item: STABLEBL                                               !
                                                                      !
         IF( JACT .EQ. 4 )THEN                                        !
C---------- Action: BULKRN                                            !
C           Set the BULKRN flag to true                               !
C           (default is FALSE in BLOCK2)
            BULKRN = .TRUE.                                           !
                                                                      !
         ELSEIF( JACT .EQ. 7 )THEN                                    !
C---------- Action: ADJ_U*                                            !
C           Set the ADJ_U* flag to true                               !
C           (default is FALSE in BLOCK2)
            ADJ_USTAR = .TRUE.                                        !
                                                                      !
         ELSE                                                         !
            MESS =  BLNK80                                            !
            ECODE = 'E06'                                             !
            WRITE( MESS,2012 ) ITEM(ITM)                              !
2012        FORMAT(' INVALID ACTION FOR ', A8 )                       !
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )                  !
            ISTAT = 1                                                 !
            RETURN                                                    !
         ENDIF                                                        ! dtb #020 01204

      ELSEIF( ITM .EQ. 12 )THEN
C------- Item: ASOS_ADJ

         IF( JACT .EQ. 5 )THEN
C---------- Action: NO_ADJ
C           Set the flag to bypass the adjustment for ASOS truncation
C           (default is TRUE in BLOCK2)
            ASOS_ADJ = .FALSE.

         ELSE
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2013 ) ITEM(ITM)
2013        FORMAT(' INVALID ACTION FOR ', A8 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

      ELSEIF( ITM .EQ. 13 )THEN
C------- Item: UASELECT

         IF( JACT .EQ. 6 )THEN
C---------- Action: SUNRIS
C           Set the flag to use sunrise as the search criterion for
C           the 'morning' sounding
C           (default is to search for the 12Z/00Z sounding)
            SUNRISE4UA = .TRUE.

         ELSE
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2014 ) ITEM(ITM)
2014        FORMAT(' INVALID ACTION FOR ', A8 )
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF
      ENDIF

      RETURN
      END

