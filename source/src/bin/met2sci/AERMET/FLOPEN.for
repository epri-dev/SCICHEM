       SUBROUTINE FLOPEN( DEVNUM,DEVNAM,KOUNT,CARD,
     1                       ITYPE,IFRMAT,NWS,BLKFAC,ISTAT )
C=====================================================================**
C        FLOPEN Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To decipher user supplied input data and open a disk
C               file.
C
C               DEVNUM = unit number
C               DEVNAM = filename
C               KOUNT  = control file record number
C               CARD   = control file record image
C               ITYPE  = file type = new, old, unknown, ...
C               IFRMAT =
C               NWS    = archive type
C               BLKFAC = unused - see note below
C               ISTAT  = status returned to calling program
C
C     Revision History:
C        05/29/08: MACTEC Engineering & Consulting
C                - Deleted assignment of BLKFAC and BLKSIZE
C                  NOTE: although BLKFAC is no longer used,
C                        it is passed as an argument; until the calling
C                        routines to FLOPEN are changed, BLKFAC
C                        needs to be retained, but unused.
C                - Removed record lengths and block sizes from
C                  OPEN statements
C-----------------------------------------------------------------------

C---- Variable declarations


      IMPLICIT NONE
      
      LOGICAL   LVAR
      INTEGER   DEVNUM,ISTAT,ITEST,ITYPE,IFRMAT,NWS,BLKFAC
      CHARACTER DEVNAM*96,CARD*(*),CTYPE(5)*8,FTYPE(2)*12

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'

C        DEVNUM = DEVICE NUMBER
C        DEVNAM = DEVICE NAME
C        CARD   = CARD 'IMAGE' FILE OPEN DATA
C        CTYPE  = TYPE OF FILE: ITYPE = 1 => 'NEW'
C                                       2 => 'OLD'
C                                       3 => 'UNKNOWN'
C                                       4 => 'SCRATCH' (A TEMPORARY FILE)
C                                       5 => 'REPLACE'
C
C        IFRMAT = FORMAT OF DISK FILE 1 = 'FORMATTED'
C                                     2 = 'UNFORMATTED'
C        NWS    = 1 GENERIC (NON-ARCHIVE DATA) FILE such as SAMSON and HUSWO data
C                 2 CD144
C                 3 SCRAM
C                 4 TD-3280FB
C                 5 TD-3280VB
C                 6 TD-3505VB  (aka ISHD )
C
C                10 FSL
C                11 TD-6201FB
C                12 TD-6201VB
C        ISTAT  = 1 FAILURE IN ATTEMPT
C                 2 OPEN WAS SUCCESSFUL
C        FTYPE  = ALL FILES HAVE A FORM OF FORMATTED.
C.......................................................................

C---- Data Initialization

      DATA CTYPE/'NEW     ','OLD     ','UNKNOWN ','SCRATCH ',
     &           'REPLACE '/
      DATA FTYPE/'FORMATTED   ','UNFORMATTED '/

      PATH = PATHWD(PATHID)
      LOC  = 'FLOPEN'
      ISTAT = 0
      LVAR  = .FALSE.

C---- Read filename

      IF( ITYPE .EQ. 4 )THEN
C------- No filename used for SCRATCH (temporary) files
         CONTINUE
      ELSE
C------- Get filename from 2nd word on CARD
         BUF96 = BLNK96
         BUF08(1) = 'FILENAME'
         CALL GETWRD( 2,KOUNT,CARD,2,96,2,BUF08(1),BUF96,ITEST )
         ISTAT = ITEST
         IF( ISTAT .NE. 1 )THEN
            DEVNAM = BUF96
C---------- Insure this filename is not already in use
            INQUIRE( FILE=DEVNAM,OPENED=LVAR )

            IF( LVAR )THEN
C------------- This file name is in use
               MESS =  BLNK80
               WRITE( MESS,1500 ) DEVNAM
1500           FORMAT(' FILE ALREADY IN USE: ',A96 )
               ECODE = 'E08'
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
               RETURN
            ENDIF

         ELSE
            MESS =  BLNK80
C           Output partial filename since MESS is 120 chars
            WRITE( MESS,1600 ) DEVNAM(1:86)
1600        FORMAT(' ERROR FROM S.GETWRD: FileName= ',A86)
            ECODE = 'E07'
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         ENDIF
      ENDIF

C---- Open the file;
C             UNIT            = file device number
C             FILE            = filename
C             STATUS          = status/type of file being opened
C             IOSTAT          = error/status code for operation

      IF( ITYPE .EQ. 4 )THEN
C------- Open a SCRATCH file (i.e., no file name)
         OPEN(UNIT=DEVNUM,  STATUS=CTYPE(ITYPE),
     &               FORM=FTYPE(IFRMAT), IOSTAT=IOFLAG)
      ELSE
C------- Open other files using filename extracted from 
C        the CARD image

         OPEN( UNIT=DEVNUM, FILE=DEVNAM, STATUS=CTYPE(ITYPE),
     &               FORM=FTYPE(IFRMAT), IOSTAT=IOFLAG)
     
      ENDIF

C-----Check open status

      IF( IOFLAG.NE.0 )THEN
         MESS =  BLNK80
         WRITE( MESS,2000 ) DEVNAM
2000     FORMAT(' ERROR OPENING FILE: ',A96)
         ECODE = 'E08'
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ENDIF

      IF( ISTAT.EQ.1 )THEN
C ---    Error occurred opening file, continue
C        with ISTAT = 1 to indicate problem to
C        calling routine
         CONTINUE

      ELSE
C ---    Open was successful
         ISTAT = 2
      ENDIF

      RETURN
      END

