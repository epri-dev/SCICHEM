      SUBROUTINE GETFIL( KOUNT,CARD,CPATH,CKEY,IDEV,CNAME,ITYPE,ISTAT )
C=====================================================================**
C          GETFIL Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Opens files that are specified on records that only
C               contain a keyword and file name (e.g., EXTRACT and QAOUT)
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
      
      INTEGER   IDEV,ISTAT, ITYPE
      CHARACTER CPATH*(*), CNAME*96, CARD*(*), CKEY*12

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
C                           5 = 'REPLACE' (replace if file alread exists)

C     There are several files that must be defined as 'OLD' depending on
C     the stage of processing:
C         input to Stage 3 (merged data file and the INCLUDED sfc. char. file)
C         site-specific data for Stage 1
C         hourly average 1-min ASOS data for Stage 2.
C     Files that are being generated should be opened with status = 'REPLACE'
C     to overwrite any existing file, which may contain headers from an
C     aborted run.


C---- Open the file                                        ---- CALL FLOPEN
C     subr.FLOPEN will return, through ISTAT, a value of
C          1 if file was not opened
C          2 if file was     opened
      CALL FLOPEN(IDEV,CNAME,KOUNT,CARD,ITYPE,1,1,1,ISTAT) 
      IF( ISTAT .EQ. 1 )THEN
         ECODE = 'E08'
         MESS =  BLNK80
         WRITE( MESS,1200 ) TRIM(CKEY), TRIM(CPATH)
1200     FORMAT(' FAILED TO OPEN ',A,' FILE ON THE ', A,' PATHWAY')
         CALL ERRHDL( KOUNT,CPATH,ECODE,LOC,MESS )
      ENDIF

      RETURN
      END

