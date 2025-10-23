      SUBROUTINE NWSHGT( KOUNT,CARD,ISTAT )
C=======================================================================
C          NWSHGT Module of the AERMET Meteorological Preprocessor
C
C   Purpose:  This routine processes the user's input for the
C             height of the NWS instrumentation.  The 6/9/93 version
C             only processes anemometer height, but could be expanded
C             to handle other instruments.
C
C   Arguments:
C        KOUNT      Card image number
C        CARD       Image of processor control information
C        ISTAT      Status of processing image
C                      1 = error occurred
C                      2 = all ok
C
C   Programmed by:  J. Paumier, PES              June 9, 1993
C     - Original programming for AERMET 1.00
C
C-----------------------------------------------------------------------

C---- Variables Declarations


      IMPLICIT NONE
      
      INTEGER       I, ISTAT, IOS
      CHARACTER CARD*(*)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C---- Data Intialization

      PATH = PATHWD(6)
      LOC  = 'NWSHGT'
      ISTAT = 0


C---- Check NWORDS, the number of fields on this record

      IF( NWORDS .LT. 3 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 )
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Fetch the second field - the met. parameter;
C      the length of each item is exactly 8 characters;
C      items shorter than 8 characters are padded on
C      the right with blanks (by the program).
C      Check the status of the 'fetch'.

      BUF08(10) = 'HT_PARAM'
      BUF08(8) = BLNK04
      CALL GETWRD ( 2,KOUNT,CARD,4,8,2,BUF08(10),BUF08(8),ISTAT )
      IF( ISTAT .EQ. 1 )THEN
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,2000 )
2000     FORMAT(' ERROR FROM S.GETWRD: VARIABLE FIELD')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF


C---- Look for match on list of 'INSTR' met. parameters
      DO I = 1,NINSTR
         IF( BUF08(8) .EQ. INSTR(I) )THEN

C---------- Fetch instrument height
            BUF08(9) = 'INSTR_HT'
            BUF08(10) = BLNK08
            CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(9),BUF08(10),ISTAT)
            IF( ISTAT .EQ. 1 )THEN
               ECODE = 'E07'
               MESS =  BLNK80
               WRITE( MESS,3000 )
3000           FORMAT(' ERROR FROM S.GETWRD: INST. HEIGHT FIELD')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
               RETURN
            ENDIF

C---------- Decode the instrument height and check it for a valid height
            READ( BUF08(10), '(F8.0)', ERR=500, IOSTAT=IOS) INSTHT(I)
            IF( INSTHT(I) .LE. 0.0 )THEN
               MESS =  BLNK80
               ECODE = 'E06'
               WRITE ( MESS, 3100) BUF08(8), BUF08(10)
               CALL ERRHDL ( KOUNT, PATH, ECODE, LOC, MESS )
               ISTAT = 1
            ELSEIF( INSTHT(I) .GT. 30.0 )THEN
               MESS =  BLNK80
               ECODE = 'W06'
               WRITE ( MESS, 3200) BUF08(8), BUF08(10)
               CALL ERRHDL ( KOUNT, PATH, ECODE, LOC, MESS )
            ENDIF

C---------- See if station shows up in ASOS commission date list,
C           which includes anemometer heights.  First assign
C           surface station WBAN used for search.

            RETURN

         ENDIF

      ENDDO

C---- No match on met params - write message, set status and return
      MESS =  BLNK80
      ECODE = 'E06'
      WRITE( MESS,3500 ) BUF08(8)
      CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      RETURN

C---- Error conditions and FORMAT statements
  500 MESS =  BLNK80
      ECODE = 'E06'
      WRITE ( MESS, 3300) BUF08(10)
      CALL ERRHDL ( KOUNT, PATH, ECODE, LOC, MESS )
      ISTAT = 1
      RETURN

 1000 FORMAT ( ' Too few fields on ''NWS_HGT'' keyword!')
 3100 FORMAT ( 1X, A8,' INSTRUMENT HT <= 0: ', A8 )
 3200 FORMAT ( 1X, A8,' INSTRUMENT HT SUSPECT:', A8 )
 3300 FORMAT ( ' ERROR DECODING ', A8, ' INSTRUMENT HT' )
 3500 FORMAT ( 1X, A8,': NO MATCH WITH MET PARAMS')

      END

