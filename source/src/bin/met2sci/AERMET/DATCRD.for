      SUBROUTINE DATCRD( KOUNT,CARD,DATFMT,BLKFAC,ISTAT,HDR )
C=====================================================================**
C          DATCRD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Processes setup data to open archive data files for the
C               UPPERAIR and SURFACE pathways
C
C     Called By: UACARD, SFCARD, OSCARD
C
C     Form: DATA  filename  file_format (ASOS)
C
C            filename     = name of data file
C            file_format  = format of the file (e.g., TD6201FB, SAMSON)
C            (ASOS)       = optional flag for ISHD surface data to
C                           indicate that data are ASOS; should be
C                           used only if station is not included
C                           in the ASOS commission list in 
C                           mod_AsosCommDates.for
C            blocking factor and collating sequence no longer used
C                AERMET assumes blocking factor = 1
C                            collating sequence = ASCII
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------

C---- Variable declarations


      IMPLICIT NONE
      
      INTEGER   ISTAT,TDFLAG,FTYPE,VBLK,FBLK,BLKFAC
      CHARACTER CARD*(*),DATFMT*8
      LOGICAL   HDR

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'


C---- Data Initialization
      PATH = PATHWD(PATHID)
      LOC  = 'DATCRD'
      ISTAT = 0

C---- Initialize filenames for UPPERAIR and SURFACE data, 
C     in case call to FLOPEN is bypassed due to errors.
      IF( PATHID .EQ. 2 )THEN
         UNIT10 = BLNK96             ! UPPERAIR file
      ELSEIF( PATHID .EQ. 3 )THEN
         UNIT20 = BLNK96             ! SURFACE file
      ENDIF

C---- Check for allowable number of fields (words) for the DATA keyword
      IF( NWORDS .LT. 3 )THEN
C----    Too few fields; issue error message and abort.
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1000 ) NWORDS
1000     FORMAT(' Too few fields (',I2,') on ''DATA'' keyword!')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ELSEIF( PATHID .EQ. 2 .AND. NWORDS .GT. 3 )THEN
         ECODE = 'W04'
         MESS =  BLNK80
         WRITE( MESS,1100 ) NWORDS
1100     FORMAT(' Too many fields (',I2,') on ''DATA'' keyword!',
     &          ' Block factor and type not ',
     &          'used: BLKFAC=1, FTYPE=ASCII.')
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

      ELSEIF( PATHID .EQ. 3 .AND. NWORDS .EQ. 5 )THEN
C----    Too many fields, but could be obsolete BLKFAC and FTYPE;
C        up to 4 fields may be OK with ASOS flag for ISHD surface data.
         ECODE = 'W04'
         MESS =  BLNK80
         WRITE( MESS,1100 ) NWORDS
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )

      ELSEIF( NWORDS .GT. 5 )THEN
C----    Too many fields, even with obsolete BLKFAC and FTYPE;
C        issue error message and abort.
         ECODE = 'E04'
         MESS =  BLNK80
         WRITE( MESS,1100 ) NWORDS
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN

      ENDIF

C---- Optional file type (ASCII or EBCDIC) no longer supported;
C     assumed default is ASCII (FTYPE = 2).  Optional blocking 
C     factor also not supported; assumed default is BLKFAC = 1.
      FTYPE  = 2
      BLKFAC = 1
      
C---- Read format of file (3rd field): the allowable formats are:
C     NWS formats: CD144, 3280FB, 3280VB, SCRAM, 6201FB, 6201VB
C     non-NWS formats: SAMSON, FSL                            ---- CALL GETWRD

      DATFMT = BLNK08
      BUF08(1) = 'FILEFORM'
      CALL GETWRD( 3,KOUNT,CARD,3,8,2,BUF08(1),DATFMT,ISTAT )

C---- SURFACE pathway formats
      IF( ISTAT.EQ.2 )THEN
C        Call to S.GETWRD successful

         FBLK = INDEX(DATFMT,'FB')
         VBLK = INDEX(DATFMT,'VB')

         TDFLAG = 0                  ! tracks file format
         IF( PATHID .EQ. 3 )THEN
            IF (INDEX(DATFMT,'HUSWO') .NE. 0) THEN
               TDFLAG = 1

            ELSEIF (INDEX(DATFMT,'SAMSON') .NE. 0) THEN
               TDFLAG = 1

            ELSEIF (INDEX(DATFMT,'CD144') .NE. 0) THEN
               TDFLAG = 2

            ELSEIF (INDEX(DATFMT,'SCRAM') .NE. 0) THEN
               TDFLAG = 3

            ELSEIF (INDEX(DATFMT,'3280') .NE. 0) THEN
               IF (FBLK.NE.0) TDFLAG = 4
               IF (VBLK.NE.0) TDFLAG = 5

            ELSEIF (INDEX(DATFMT,'ISHD') .NE. 0) THEN
               TDFLAG = 6 
               
C ---          Initialize ISHD_ASOS flag to .F.
               ISHD_ASOS = .FALSE.

            ENDIF

C ---    UPPERAIR pathway formats
         ELSEIF( PATHID.EQ.2 )THEN
            IF (INDEX(DATFMT,'FSL') .NE. 0) THEN
               TDFLAG = 10

            ELSEIF (INDEX(DATFMT,'6201') .NE. 0) THEN
               IF (FBLK.NE.0) TDFLAG = 11
               IF (VBLK.NE.0) TDFLAG = 12
            ENDIF

         ENDIF

         IF( TDFLAG .EQ. 0 )THEN
C ---       No valid data format was found, issue error message
            ECODE = 'E06'
            MESS =  BLNK80
            WRITE( MESS,3000 ) DATFMT, PATHWD(PATHID)
3000        FORMAT(' File format error (',A8,') for ',A10)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            
         ENDIF

      ELSE
C        Depending on the error, 'DATFMT' may be blank or contain garbage
         ECODE = 'E07'
         MESS =  BLNK80
         WRITE( MESS,3100 ) DATFMT
3100     FORMAT(' ERROR FROM S.GETWRD: FILE FORMAT ',A8)
         CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1

      ENDIF

      IF( PATHID .EQ. 3 .AND. NWORDS .EQ. 4 .AND. TDFLAG .EQ. 6 )THEN
C------- Four fields on the record and SURFACE format is ISHD;
C        assume the last field is the optional 'ASOS' flag to 
C        specify that surface data in ISHD format are from an 
C        ASOS site.
C        This optional field is not valid for other surface
C        file formats.

         BUF08(2) = BLNK08
         BUF08(1) = 'ASOS    '
         CALL GETWRD(4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ISTAT)
         
         IF( ISTAT.EQ.2 )THEN
C           Call to GETWRD was successful
            IF( INDEX(BUF08(2),'ASOS') .NE. 0 )THEN
C              Set flag indicating that ISHD surface data are ASOS,
C              so that adjustment for truncated wind speeds will be 
C              applied.
               ISHD_ASOS = .TRUE.
C              Issue warning message
               ECODE = 'W06'
               MESS =  BLNK80
               WRITE( MESS,3200 ) 
3200           FORMAT(' ASOS flag specified for ISHD on DATA keyword -',
     &               ' use only if not in AERMET ASOS commission list.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ELSE
C              Issue warning message for invalid field
               ECODE = 'W06'
               MESS =  BLNK80
               WRITE( MESS,3250 ) BUF08(2)
3250           FORMAT(' Invalid/obsolete entry on ''DATA'' keyword: ',
     &             A8,'; only valid option is ''ASOS'' for ISHD data.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ENDIF

         ELSE
C           Depending on the error, 'BUF08(2)' may be blank or contain garbage
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,3300 ) BUF08(2)
3300        FORMAT(' ERROR FROM S.GETWRD: ''ASOS'' flag for ISHD data: '
     &              ,A8)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSEIF( PATHID.EQ.3 .AND. NWORDS .EQ. 4 .AND. TDFLAG .NE. 6 )THEN
C------- Four fields on the record and SURFACE format is NOT ISHD;
C        issue warning message if 4th field is not 'ASOS', but
C        issue error message if 4th field IS 'ASOS'.

         BUF08(2) = BLNK08
         BUF08(1) = 'ASOS    '
         CALL GETWRD(4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ISTAT)
         
         IF( ISTAT.EQ.2 )THEN
C           Call to GETWRD was successful
            IF( INDEX(BUF08(2),'ASOS') .NE. 0 )THEN
C              Issue error message for 'ASOS' flag with non-ISHD data
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,3400 ) 
3400           FORMAT(' ''ASOS'' flag specified on DATA keyword ',
     &                'for non-ISHD data! This flag is only ',
     &                'allowed for ISHD data.')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ELSE
C              Issue warning message for invalid field .ne. 'ASOS',
C              such as obsolete blocking factor.
               ECODE = 'W06'
               MESS =  BLNK80
               WRITE( MESS,3250 ) BUF08(2)
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ENDIF

         ELSE
C           Depending on the error, 'BUF08(2)' may be blank or contain garbage
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,3300 ) BUF08(2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ELSEIF( PATHID.EQ.3 .AND. NWORDS .EQ. 5 )THEN
C----    Finally, check for ASOS flag in field 

         BUF08(2) = BLNK08
         BUF08(1) = 'ASOS    '
         CALL GETWRD(5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ISTAT)
         
         IF( ISTAT.EQ.2 )THEN
C           Call to GETWRD was successful
            IF( INDEX(BUF08(2),'ASOS') .NE. 0 .AND. TDFLAG .EQ. 6 )THEN
C              Issue error message for 'ASOS' flag in wrong position for ISHD data
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,3500 ) 
3500           FORMAT(' ''ASOS'' flag specified in wrong position ',
     &                'on DATA keyword for ISHD data!')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ELSEIF( INDEX(BUF08(2),'ASOS') .NE. 0 .AND. 
     &                                              TDFLAG .NE. 6 )THEN
C              Issue error message for 'ASOS' flag with non-ISHD data
               ECODE = 'E06'
               MESS =  BLNK80
               WRITE( MESS,3600 ) 
3600           FORMAT(' ''ASOS'' flag specified for non-ISHD data ',
     &                'and in wrong position on DATA keyword!')
               CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            ENDIF
         ELSE
C           Depending on the error, 'BUF08(2)' may be blank or contain garbage
            ECODE = 'E07'
            MESS =  BLNK80
            WRITE( MESS,3300 ) BUF08(2)
            CALL ERRHDL( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ENDIF

      ENDIF

C---- Check for "fatal" errors (ISTAT=1)
      IF( ISTAT .EQ. 1 )THEN
         RETURN
      ENDIF


C---- Open the file - the file name will be retrieved from the record
C     in SUBR.FLOPEN; but skip the call to FLOPEN if this call is based on 
C     processing a header record (HDR = .T.).
C     Open with ITYPE=2 for STATUS = 'OLD' files since data files should
C     exist, otherwise an error occurs. 

      IF( PATHID .EQ. 2 .AND. .NOT.HDR )THEN
         CALL FLOPEN(DEV10,UNIT10,KOUNT,CARD,2,1,TDFLAG,BLKFAC,ISTAT)

      ELSEIF( PATHID .EQ. 3 .AND. .NOT.HDR )THEN
         CALL FLOPEN(DEV20,UNIT20,KOUNT,CARD,2,1,TDFLAG,BLKFAC,ISTAT)

      ENDIF

      IF( ISTAT .EQ. 1 )THEN
         ECODE = 'E08'
         MESS =  BLNK80
         WRITE( MESS, 4000 ) PATHWD(PATHID)
4000     FORMAT(' Failed to open ''DATA'' file on ', A10,' pathway.')
         CALL ERRHDL( KOUNT, PATH, ECODE, LOC, MESS)
      ENDIF

C---- The STATUS(PATHID,KEYID) is used to indicate ASCII (2) or EBCDIC (3)
      IF( ISTAT.NE.1 )THEN
         ISTAT = FTYPE
      ENDIF

      RETURN
      END

