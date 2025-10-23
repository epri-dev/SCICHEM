      SUBROUTINE DOCLDS(SF2YR,SFGMO,SFGDY,SFGHR,
     &                  ASOS1,ASOS2,ASOS3,ICCVR,ICEIL)
C***********************************************************************
C*    DOCLDS Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Decodes the ASOS Cloud Data
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    MODIFIED:   Changed ASOS cloud cover/ceiling height codes
C*                to character string, instead of integers, to
C*                correct for errors that occurred when blank 
C*                spaces occur between the cloud cover and ceiling
C*                height fields.  Also eliminated source code that 
C*                accepted invalid ASOS cloud cover codes.
C*                R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C*
C*    INPUTS:     3 levels of ASOS cloud data of the form XXYYY
C*                where XX = cloud coverage, YYY = ceiling in hundreds
C*                of feet
C*
C*    OUTPUTS:    1 Hour Numeric Value for Ceiling and Cloud Cover
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations

      IMPLICIT NONE
      
      INCLUDE 'WORK1.INC'

      CHARACTER*5 ASOS1,ASOS2,ASOS3
      INTEGER  ACC1,ACC2,ACC3,ACHT1,ACHT2,ACHT3
      INTEGER  MAXCLD, ICCVR, ICEIL
      INTEGER  SF2YR, SFGMO, SFGDY, SFGHR, IYYMMDD

      ACC1 = 99
      ACC2 = 99
      ACC3 = 99
      MAXCLD  = 99
      ICCVR   = 99
      ICEIL   = 77777


C --- Process cloud cover/ceiling height codes;
C     first two digits are cloud cover and
C     last three digits are ceiling height;
C     99999 indicates missing data
C
      IF( ASOS1 .NE. '99999' )THEN
         READ(ASOS1,'(I2)') ACC1
         READ(ASOS1,'(2X,I3)') ACHT1
         MAXCLD = ACC1
         IF( MAXCLD .GT. 02 )THEN
            ICEIL = ACHT1
         ENDIF
      ENDIF

      IF( ASOS2 .NE. '99999' )THEN
         READ(ASOS2,'(I2)') ACC2
         READ(ASOS2,'(2X,I3)') ACHT2
         IF( ACC2 .NE. 99  .AND.  ACC2 .GT. MAXCLD )THEN
            MAXCLD = ACC2
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT2
            ENDIF
         ENDIF
      ENDIF

      IF( ASOS3 .NE. '99999' )THEN
         READ(ASOS3,'(I2)') ACC3
         READ(ASOS3,'(2X,I3)') ACHT3
         IF( ACC3 .NE. 99  .AND.  ACC3 .GT. MAXCLD )THEN
            MAXCLD = ACC3
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT3
            ENDIF
         ENDIF
      ENDIF

C---- Convert the ASOS sky condition codes to  fractional cloud cover
C     (tenths).  Note that the VALID codes are 0, 2, 4, 6, 7, 9, and 99.
C     The remaining codes (1, 3, 5, 8, and 10) are not valid according to
C     the HUSWO documentation; previous versions of AERMOD allowed to these
C     invalid codes to pass through as 1/10, 3/10, etc. (per conversation 
C     with Dennis Atkinson, USEPA, 10/30/98).  
C---  Beginning with version 12345 these invalid codes are treated 
C     as missing data.

      IF( MAXCLD .EQ. 0 )THEN                         ! Clear
C*       A valid ASOS Sky Condition Value
         ICCVR = 0
      ELSEIF( MAXCLD .EQ. 02 )THEN
C*       A valid ASOS Sky Condition Value             ! Scattered
         ICCVR = 3
      ELSEIF( MAXCLD .EQ. 04 )THEN                    ! Broken
C*       A valid ASOS Sky Condition Value
         ICCVR = 7
      ELSEIF( MAXCLD .EQ. 06 )THEN                    ! Overcast
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 07 )THEN
C*       A valid ASOS Sky Condition Value             ! Obstruction
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 09 )THEN                    ! Unknown
C*       A valid ASOS Sky Condition Value
         ICCVR = 99
      ELSEIF( MAXCLD .EQ. 99 )THEN
         ICCVR = 99                                   ! Missing
C        WRITE( IDIAG, 510 ) IY, IM, ID, IH
      ELSE
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 99
         MESS = BLNK80
         ECODE = 'W49'           
         IYYMMDD = SF2YR*10000 + SFGMO*100 + SFGDY
         WRITE( MESS, 500 ) SF2YR,SFGMO,SFGDY,SFGHR
         CALL ERRHDL( IYYMMDD,PATH,ECODE,LOC,MESS)
      ENDIF

  500 FORMAT( ' DOCLDS: ASOS cloud cover invalid for (yy/mm/dd/hh) ',
     &         4(I3.2:,'/') )


      RETURN
      END

