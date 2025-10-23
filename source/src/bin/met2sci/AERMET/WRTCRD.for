      SUBROUTINE WRTCRD( IHDNUM,PREP,CARD,DEVICE )
C=====================================================================**
C          WRTCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Writes runstream record to a temporary file for
C               possible use later either in constructing headers
C               for output file(s), or for possible use in reporting
C               processing errors.
C
C     Initial Release: December 15, 1992
C
C     Revision History:
C        11/15/96
C          - added a call to subr.DEFINE (just before the call to
C            subr.FDPATH) to correct a recurrent error in stage 2
C            whereby the SURFACE and ONSITE pathways were not properly
C            identified in the merge file header records for stage 3
C
C-----------------------------------------------------------------------

C---- Variable Declarations


      IMPLICIT NONE
      
      INTEGER   ITEST,IHDNUM,ISTAT,NKEY, DEVICE, IPATH
      CHARACTER CARD*(*),FNAME*96,PREP*3,PREFNM*2
      DIMENSION PREFNM(6)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

      DATA ITEST/-1/
      DATA PREFNM/'JB','UA','SF','OS','MR','MP'/

C      ITEST   = Using this to key whetheror not the temporary files
C                have been opened
C      IHDNUM  = If record is from current runstream, this will be the
C                record position; if record is from a file's header,
C                this will be position in header.
C      CARD    = Record to be written to temporary file
C      PREP    = 3-character prefix of header record
C      ISTAT   = Status from returned from SUBR.FLOPEN


C---- Data Initializations

      IPATH = PATHID
      PATH = PATHWD(IPATH)
      LOC  = 'WRTCRD'
      NKEY = 0

C---- Check itest; if less than zero, then open temporary files.
C     (ITEST indicates whether or not the temporary files are open)

      IF( ITEST.LT.0 )THEN
         ITEST = 1
         BUF80(5) = BLN132

C----    First open then close to delete temporary file in case
C        it exists from a previously aborted run; then reopen.
C        Open with ITYPE = 4 for 'SCRATCH' file
         CALL FLOPEN( DEV70,FNAME,KOUNT,BUF80(5),4,1,1,1,ISTAT )
         CLOSE( DEV70 )
         CALL FLOPEN( DEV70,FNAME,KOUNT,BUF80(5),4,1,1,1,ISTAT )
         IF( ISTAT.EQ.1 )THEN
            ITEST = 0
            RETURN
         ENDIF

C----    First open then close to delete temporary file in case
C        it exists from a previously aborted run; then reopen.
C        Open with ITYPE = 4 for 'SCRATCH' file
         CALL FLOPEN( DEV75,FNAME,KOUNT,BUF80(5),4,1,1,1,ISTAT )
         CLOSE( DEV75 )
         CALL FLOPEN( DEV75,FNAME,KOUNT,BUF80(5),4,1,1,1,ISTAT )
         IF( ISTAT.EQ.1 )THEN
            ITEST = 0
            RETURN
         ENDIF

      ELSEIF( ITEST .EQ. 0 )THEN
C------ There was an error opening the temporary files the first
C       time in this routine
        SETERR = .TRUE.
        MESS =  BLNK80
        ECODE = 'E08'
        WRITE( MESS, 1500 )
1500    FORMAT(' A TEMPORARY FILE WAS NOT OPENED PROPERLY')
        CALL ERRHDL( 0,PATH,ECODE,LOC,MESS )
        RETURN

      ENDIF


C---- Scan the record to determine if this is one of those
C     input control cards that are always processed;
C     add the special character as needed.
C
C     - Always process PATHWAY card
C
C     - For UPPERAIR and SURFACE PATHWAYS, look for 'RANGE' (keyword 13)

C     - For ONSITE Pathway, look for 'FORMAT' (6), 'RANGE' (13),
C       'DELTA_TEMP' (15), 'OSHEIGHTS' (16), 'THRESHOLD' (17),
C       'READ' (18), 'FREQ_SECT' (19),' SITE_CHAR' (20), 'SECTOR' (21)

C---- Initialize 'prefix'


      BUF08(1) = BLNK08
      WRITE( BUF08(1),2000 ) PREFNM(IPATH),IHDNUM,PREP(3:3)
2000  FORMAT( A2,1X,I3,1X,A1 )

      CALL DEFINE( 132, CARD )
      CALL FDPATH(IHDNUM,CARD,NKEY)
      IF( NKEY.EQ.0 )THEN
         CALL FDKEY(IHDNUM,CARD,NKEY)
         IF( NKEY.GT.0 )THEN
            IF( IPATH.EQ.2 .OR. IPATH.EQ.3 )THEN
               IF( IPATH.EQ.3 .AND. NKEY.EQ.4 ) BUF08(1)(3:3) = '@'
               IF( NKEY.EQ.13 ) BUF08(1)(3:3) = '$'
               IF( NKEY.EQ.10 ) BUF08(1)(3:3) = '@'
               IF( NKEY.EQ.33 ) BUF08(1)(3:3) = '@'
            ELSEIF( IPATH.EQ.4 )THEN
               IF( (NKEY.GE.15 .AND. NKEY.LE.21) .OR.
     &              NKEY.EQ.6 .OR. NKEY.EQ.13) BUF08(1)(3:3) = '$'
               IF( NKEY.EQ.10) BUF08(1)(3:3) = '@'
            ENDIF
         ENDIF
      ELSE
         BUF08(1)(3:3) = '%'
      ENDIF

C---- Write to temporary file

      WRITE( DEVICE,3000 ) BUF08(1),CARD
3000  FORMAT( A8,A132 )

      RETURN
      END

