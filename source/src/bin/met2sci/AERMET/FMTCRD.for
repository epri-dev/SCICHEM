      SUBROUTINE FMTCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C          FMTCRD Module of the AERMET Meteorological Preprocessor
C
C      PURPOSE:  Processes the onsite pathway data formats associated
C                with the input data.
C
C
C      SYNTAX:   FORMAT  recd#  Fortran-format-stmt
C
C-----------------------------------------------------------------------

C------ Variable Declarations

      IMPLICIT NONE
      

      CHARACTER CARD*(*)
      INTEGER   ISTAT,ITEST,FMTNDX

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C      ISTAT          PROCESS STATUS 1 = ERROR IN PROCESSING
C                                    2 = PROCESSING OK
C      CARD           Runstream record

C---- Data Initialization

      PATH = PATHWD(PATHID)
      LOC  = 'FMTCRD'
      ISTAT = 0
      FMTNDX = 0

C---- Check the number of fields on this record: must be >= 3
C     (commas are interpreted as field delimiters, so commas
C     in the format are interpreted as separating fields; this
C     'problem' is resolved later)
      IF( NWORDS.LT.3 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS, 1000 )
1000     FORMAT(' Too few fields on ''FORMAT'' keyword.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C---- Fetch the second field on the card: format index

      BUF08(1) = 'FMTINDEX'
      BUF02 = '  '
      CALL GETWRD( 2,KOUNT,CARD,1,2,1,BUF08(1),BUF02,ITEST )
      ISTAT = ITEST
      IF( ITEST .EQ. 2 )THEN
         READ( BUF02,2000,IOSTAT=IOFLAG ) FMTNDX
2000     FORMAT( I2 )
         IF( IOFLAG.NE.0  )THEN
C---------- Error decoding the format index
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,2500 ) BUF02
2500        FORMAT(' Error decoding FORMAT index: ',A2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSE
         MESS =  BLNK80
         ECODE = 'E07'
         WRITE( MESS,2600 )
2600     FORMAT(' ERROR FROM S.GETWRD: ''FORMAT'' INDEX')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF


C---- Check FMTNDX to see that index is reasonable: cannot exceed
C     OSMRDS, the maximum number of read/format statements

      IF( FMTNDX .GT. OSMRDS )THEN
          MESS =  BLNK80
          ECODE = 'E06'
          WRITE( MESS,3000 ) FMTNDX,OSMRDS
3000      FORMAT(' Too many ''FORMAT'' keywords specified: ',I3,
     &           '; MAX = ',I3)
          CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
          ISTAT = 1
       ENDIF

C---- Store the format
      IF( ISTAT.NE.1 )THEN
C        First check for non-blank value, indicating that this
C        format index has already been defined
         IF( LEN_TRIM(OSFRMT(FMTNDX)) .NE. 0 )THEN
            MESS =  BLNK80
            ECODE = 'E06'
            WRITE( MESS,4000 ) FMTNDX
4000        FORMAT(' FORMAT keyword has already been specified ',
     &              'for ONSITE record index: ',I3)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSE
            OSFRMT(FMTNDX) = ADJUSTL( CARD( IC1(3):IC2(NWORDS) ) )
            IF( OSFRMT(FMTNDX) .EQ. 'FREE' )THEN
               OSFRMT(FMTNDX) = ADJUSTL( '*' )
            ENDIF
            ISTAT = 2
         ENDIF
      ENDIF

      RETURN
      END


