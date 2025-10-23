      SUBROUTINE GETASOS( KOUNT,CARD,CPATH,CKEY,IDEV,CNAME,ISTAT,HDR )
C=====================================================================**
C          GETASOS Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Opens the 1-min ASOS wind data file identified on 
C               on the QAOUT keyword under the SURFACE pathway
C               in Stage 2. This routine is also used in Stage 3
C               to obtain the 1-min ASOS data filename, without 
C               opening.  The HDR variable is used to determine
C               whether or not to open the file.
C
C     Called By: UACARD, SFCARD, OSCARD, AERSURF
C
C     Form:  keyword  filename
C
C            keyword   = keyword: MESSAGES, REPORT, EXTRACT, QAOUT, OUTPUT
C            filename  = name of data file
C
C-----------------------------------------------------------------------

C---- Variable declarations


      IMPLICIT NONE
      
      INTEGER   IDEV,ISTAT,ITEST, ITYPE
      CHARACTER CPATH*(*), CNAME*96, CARD*(*), CKEY*12

      LOGICAL   HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C---- Data Initialization
      DATA LOC/'GETFIL'/
      ISTAT = 0

C---- Check the number of fields on the record
      IF( NWORDS.LT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 ) CKEY
1000     FORMAT(' Too few fields on ', A12,' keyword.')
         CALL ERRHDL( KOUNT,CPATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ELSEIF( NWORDS.GT.2 )THEN
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 ) CKEY
1100     FORMAT(' Too many fields on ', A12,' keyword.')
         CALL ERRHDL( KOUNT,CPATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      ENDIF

C     ITYPE  = TYPE OF FILE 1 = 'NEW'
C                           2 = 'OLD'
C                           3 = 'UNKNOWN'
C                           4 = 'SCRATCH' (A TEMPORARY FILE)
C                           5 = 'REPLACE'

C     There are several files that must be defined as 'OLD' depending on
C     the stage of processing:
C         input to Stage 3 (merged data file and the INCLUDED sfc. char. file)
C         site-specific data for Stage 1
C         hourly average 1-min ASOS data for Stage 2.
C     Files that are being generated should be opened with status = 'REPLACE'
C     to overwrite any existing file, which may contain headers from an
C     aborted run.

      IF( INDEX(CPATH,'SURFACE') .NE. 0 .AND.
     &    INDEX(CKEY,'ASOS1MIN') .NE. 0 )THEN
C----    This is the only legitimate case for this routine
         ITYPE = 2

         BUF96 = BLNK96
         BUF08(1) = 'FILENAME'
         CALL GETWRD( 2,KOUNT,CARD,2,96,2,BUF08(1),BUF96,ITEST )
         ISTAT = ITEST
         IF( ISTAT .NE. 1 )THEN
            CNAME = BUF96
         ELSE
            MESS =  BLNK80
            WRITE( MESS,1600 )
1600        FORMAT(' ERROR FROM S.GETWRD: FILE NAME')
            ECODE = 'E07'
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF

      ELSE
C        Make all other files 'UNKNOWN'
         ITYPE = 3
      ENDIF

C---- Open the file                                        ---- CALL FLOPEN
C     subr.FLOPEN will return, through ISTAT, a value of
C          1 if file was not opened
C          2 if file was     opened
      IF( .NOT. HDR )THEN
         CALL FLOPEN(IDEV,CNAME,KOUNT,CARD,ITYPE,1,1,1,ISTAT) 
         IF( ISTAT .EQ. 1 )THEN
            ECODE = 'E08'
            MESS =  BLNK80
            WRITE( MESS,1200 ) TRIM(CKEY), TRIM(CPATH)
1200        FORMAT(' Failed to open ',A,' file on the ', A,' pathway')
            CALL ERRHDL( KOUNT,CPATH,ECODE,LOC,MESS )
         ENDIF
      ENDIF

      RETURN
      END

