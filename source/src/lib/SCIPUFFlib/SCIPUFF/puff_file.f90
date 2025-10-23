MODULE oldPuff_fi
  IMPLICIT NONE
  SAVE
  INTEGER naux_old !Needed for reading old style puff files
END MODULE oldPuff_fi
!===============================================================================
! OpenPuffFile
!===============================================================================
SUBROUTINE OpenPuffFile( lun,file,lnew,lreadonly )

USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file
LOGICAL,      INTENT( IN ) :: lnew
LOGICAL,      INTENT( IN ) :: lreadonly

INTEGER ios, fileversion

CHARACTER(12) action,status,position
CHARACTER(24) errmsg
CHARACTER(24) versionHeader

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Check status, Close if already open

IF( IsPuffFileOpen( lun,file,.NOT.lreadonly ) )THEN
  CALL ClosePuffFile( lun,file )
  IF( nError /= NO_ERROR )GOTO 9999
END IF

!==== set status and action values

IF( lnew )THEN
  status   = 'NEW'
  action   = 'READWRITE'
  position = 'ASIS'
  errmsg   = 'does not already exist'
ELSE
  status = 'OLD'
  IF( lreadonly )THEN
    action   = 'READ'
    position = 'REWIND'
  ELSE
    action   = 'READWRITE'
    position = 'ASIS'
  END IF
  errmsg = 'exists'
END IF

!==== Open file

OPEN( UNIT=lun,FILE=file,STATUS=status,FORM='UNFORMATTED',ACTION=action,POSITION=position,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = OP_ERROR
  eRoutine = 'OpenPuffFile'
  eMessage = 'Error opening SCIPUFF puff file for '//TRIM(action)
  CALL ReportFileName( eInform,'File=',file )
  eAction  = 'Make sure file '//TRIM(errmsg)
  GOTO 9999
END IF

!==== Set pufffile version

IF( lnew )THEN

  pufffile_version = PUFFFILE_VERSION_VALUE

  WRITE(lun,IOSTAT=ios)TRIM(PUFFFILE_VERSION_STRING),pufffile_version
  IF( ios /= 0 )THEN
    nError   = WR_ERROR
    eRoutine = 'OpenPuffFile'
    WRITE(eMessage,'(A,I0)') 'Error writing version header to SCIPUFF puff file: IOS=',ios
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

ELSE

  READ(lun,IOSTAT=ios)versionHeader,fileVersion
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'OpenPuffFile'
    WRITE(eMessage,'(A,I0)') 'Error reading version header on SCIPUFF puff file: IOS=',ios
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF

  IF( TRIM(versionHeader)==TRIM(PUFFFILE_VERSION_STRING) )THEN
    pufffile_version = fileVersion
  ELSE
    pufffile_version = 0
    CALL BackspacePuffFile( lun,file )
  END IF

  SELECT CASE( pufffile_version )
    CASE( 0,1 )
    CASE( PUFFFILE_VERSION_VALUE )
    CASE DEFAULT
      CALL SetUnrecognizedVersionError( 'OpenPuffFile' )
      GOTO 9999
  END SELECT

 !==== Close/Reopen file with APPEND if not readonly

  IF( .NOT.lreadonly )THEN
    fileversion = pufffile_version
    CALL ClosePuffFile( lun,file )   !ClosePuffFile resets pufffile_version so save the value
    pufffile_version = fileVersion
    position = 'APPEND'
    OPEN(UNIT=lun,FILE=file,STATUS=status,FORM='UNFORMATTED',ACTION=action,POSITION=position,IOSTAT=ios)
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'OpenPuffFile'
      eMessage = 'Error opening SCIPUFF puff file for '//TRIM(position)
      CALL ReportFileName( eInform,'File=',file )
      eAction  = 'Make sure file '//TRIM(errmsg)
      GOTO 9999
    END IF
  END IF

END IF

pufffile_record = PUFFFILE_TIME_HEADER

9999 CONTINUE

RETURN
END
!===============================================================================
! IsPuffFileOpen
!===============================================================================
LOGICAL FUNCTION IsPuffFileOpen( lun,file,lwrite ) RESULT( isOpen )

USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file
LOGICAL,      INTENT( IN ) :: lwrite

INTEGER ios
CHARACTER(8) openWrite

!==== Check status

isOpen = .FALSE.  !Assume the puff file is closed

INQUIRE( UNIT=lun,OPENED=isOpen,IOSTAT=ios )
IF( ios /= 0 )THEN
  nError   = RD_ERROR
  eRoutine = 'IsPuffFileOpen'
  WRITE(eMessage,'(A,I0)') 'Error checking on open status of puff file: IOS=',ios
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== Check status for writing

IF( isOpen .AND. lwrite )THEN
  INQUIRE( UNIT=lun,WRITE=openWrite,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = RD_ERROR
    eRoutine = 'IsPuffFileOpen'
    WRITE(eMessage,'(A,I0)') 'Error checking on write status of puff file: IOS=',ios
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
  IF( openWrite(1:3)/='YES' )THEN
    nError   = WR_ERROR
    eRoutine = 'IsPuffFileOpen'
    eMessage = 'Write status of SCIPUFF puff file is '//TRIM(openWrite)
    CALL ReportFileName( eInform,'Unable to write to file=',file )
    GOTO 9999
  END IF
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
! ClosePuffFile
!===============================================================================
SUBROUTINE ClosePuffFile( lun,file )

USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

INTEGER ios
LOGICAL isOpen

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Normal Close

IF( nError == 0 )THEN

!==== Check status

  isOpen = IsPuffFileOpen( lun,file,.FALSE. )
  IF( nError /= NO_ERROR )GOTO 9999

!==== If Opened close file

  IF( isOpen )THEN
    CLOSE( UNIT=lun,IOSTAT=ios )
    IF( ios /= 0 )THEN
      nError   = OP_ERROR
      eRoutine = 'ClosePuffFile'
      WRITE(eMessage,'(A,I0)') 'Error closing SCIPUFF puff file: IOS=',ios
      CALL ReportFileName( eInform,'File=',file )
      GOTO 9999
    END IF
  END IF

!==== Close on an Error

ELSE

  CLOSE( UNIT=lun,IOSTAT=ios )

END IF

pufffile_version = -1

9999 CONTINUE

RETURN
END
!===============================================================================
! RewindPuffFile
!===============================================================================
SUBROUTINE RewindPuffFile( lun,file )

USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

INTEGER ios
LOGICAL isOpen

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

!==== If Opened close file

IF( isOpen )THEN
  REWIND( UNIT=lun,IOSTAT=ios )
  IF( ios /= 0 )THEN
    nError   = OP_ERROR
    eRoutine = 'RewindPuffFile'
    WRITE(eMessage,'(A,I0)') 'Error rewinding SCIPUFF puff file: IOS=',ios
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
  SELECT CASE( pufffile_version )
    CASE( 0 )
    CASE( PUFFFILE_VERSION_VALUE )
      READ(lun,IOSTAT=ios)       !Skip version record
      IF( ios /= 0 )THEN
        nError   = RD_ERROR
        eRoutine = 'RewindPuffFile'
        WRITE(eMessage,'(A,I0)') 'Error reading version header from puff file: IOS=',ios
        CALL ReportFileName( eInform,'File=',file )
        GOTO 9999
      END IF
    CASE DEFAULT
      CALL SetUnrecognizedVersionError( 'RewindPuffFile' )
      GOTO 9999
  END SELECT
ELSE
  nError   = OP_ERROR
  eRoutine = 'RewindPuffFile'
  eMessage = 'Error rewinding SCIPUFF puff file'
  CALL ReportFileName( eInform,'File not open: ',file )
  GOTO 9999
END IF

pufffile_record = PUFFFILE_TIME_HEADER

9999 CONTINUE

RETURN
END
!===============================================================================
! BackspacePuffFile
!===============================================================================
SUBROUTINE BackspacePuffFile( lun,file )

USE files_fi
USE error_fi

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

INTEGER ios, nrec, i
LOGICAL isOpen

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

!==== If Opened close file

IF( isOpen )THEN
  SELECT CASE( pufffile_version )
    CASE( 0 )
      BACKSPACE( UNIT=lun,IOSTAT=ios )
      IF( ios /= 0 )THEN
        nError   = OP_ERROR
        eRoutine = 'BackspacePuffFile'
        WRITE(eMessage,'(A,I0)') 'Error backspacing puff file: IOS=',ios
        CALL ReportFileName( eInform,'File=',file )
        GOTO 9999
      END IF
      pufffile_record = PUFFFILE_TIME_HEADER
    CASE( 1,PUFFFILE_VERSION_VALUE )
      nrec = pufffile_record - PUFFFILE_TIME_HEADER
      IF( nrec == 0 )THEN  !Already pointing to a time header. Back up a full break
        nrec = PUFFFILE_NUM_RECORD
        pufffile_record = pufffile_record + PUFFFILE_NUM_RECORD
      END IF
      DO i = 1,nrec
        BACKSPACE( UNIT=lun,IOSTAT=ios )
        IF( ios /= 0 )THEN
          nError   = OP_ERROR
          eRoutine = 'BackspacePuffFile'
          WRITE(eMessage,'(A,I0)') 'Error backspacing puff file: IOS=',ios
          CALL ReportFileName( eInform,'File=',file )
          GOTO 9999
        END IF
        CALL DecrementPuffFileRecord()
     END DO
    CASE DEFAULT
      CALL SetUnrecognizedVersionError( 'BackspacePuffFile' )
      GOTO 9999
  END SELECT
ELSE
  nError   = OP_ERROR
  eRoutine = 'BackspacePuffFile'
  eMessage = 'Error rewinding SCIPUFF puff file'
  CALL ReportFileName( eInform,'File not open: ',file )
  GOTO 9999
END IF

IF( pufffile_record /= PUFFFILE_TIME_HEADER )THEN
  CALL SetOutOfSyncError( 'BackspacePuffFile',PUFFFILE_TIME_HEADER )
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
! ReadPuffFileTimeHeader
!===============================================================================
SUBROUTINE ReadPuffFileTimeHeader( lun,file,t,npuf,numMet,ncrel )

!Reads next time header

USE files_fi
USE error_fi
USE project_fi
USE puffstruct_fd
USE oldPuff_fi      !For old version pufffiles

IMPLICIT NONE

INTEGER,      INTENT(  IN ) :: lun
CHARACTER(*), INTENT(  IN ) :: file
REAL,         INTENT( OUT ) :: t
INTEGER,      INTENT( OUT ) :: npuf
INTEGER,      INTENT( OUT ) :: numMet
INTEGER,      INTENT( OUT ) :: ncrel

LOGICAL isOpen
INTEGER ios
INTEGER n0, n1, n2, i, j, nrec
REAL    dum
TYPE( puff_str_ri_NOaux_old )pdum

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999
IF( .NOT.isOpen )THEN
  CALL SetNotOpenedError( 'ReadPuffFileTimeHeader' )
  GOTO 9999
END IF

!==== If Opened then read next time header

SELECT CASE( pufffile_version )
  CASE( 0 )
#if defined(LINUX)
    READ(UNIT=lun,IOSTAT=ios) t,npuf
    IF( ios /= 0 )GOTO 9998
    IF( iversion/100 == iversion_code/100 )THEN
      BACKSPACE(UNIT=lun,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9998
      READ(UNIT=lun,IOSTAT=ios) t,npuf,(pdum,i=1,npuf),naux_old
      IF( ios /= 0 )GOTO 9998
      BACKSPACE(UNIT=lun,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9998
      READ(UNIT=lun,IOSTAT=ios) t,npuf,(pdum,i=1,npuf),  &
                                naux_old,(dum,i=1,naux_old-1),n0,numMet
      IF( ios /= 0 )GOTO 9998
      BACKSPACE(UNIT=lun,IOSTAT=ios)
      IF( ios /= 0 )GOTO 9998
      READ(UNIT=lun,IOSTAT=ios) t,npuf,(pdum,i=1,npuf),  &
                                naux_old,(dum,i=1,naux_old-1), &
                                n0,                      &
                                numMet,(n1,n2,dum,dum,dum,dum,(dum,j=1,n1*n2),i=1,numMet), &
                                ncrel
      IF( ios /= 0 )GOTO 9998
    ELSE
      numMet = 1
      ncrel  = 0
    END IF
#else
    IF( iversion/100 < iversion_code/100 )THEN
      READ(UNIT=lun,IOSTAT=ios) t,npuf,(pdum,i=1,npuf),naux_old
      numMet = 1
      ncrel  = 0
    ELSE
      READ(UNIT=lun,IOSTAT=ios) t,npuf,(pdum,i=1,npuf),  &
                                naux_old,(dum,i=1,naux_old-1), &
                                n0,                      &
                                numMet,(n1,n2,dum,dum,dum,dum,(dum,j=1,n1*n2),i=1,numMet), &
                                ncrel
    END IF
#endif
    IF( ios /= 0 )GOTO 9998
    pufffile_record = PUFFFILE_MET_HEADER
  CASE( 1,PUFFFILE_VERSION_VALUE )
!------ Advance to the next time header if not already pointing to a time header
    IF( pufffile_record /= PUFFFILE_TIME_HEADER )THEN
      nrec = PUFFFILE_NUM_RECORD - pufffile_record + PUFFFILE_TIME_HEADER
      DO i = 1,nrec
        READ(lun,IOSTAT=ios)
        IF( ios /= 0 )GOTO 9997
        CALL IncrementPuffFileRecord()
      END DO
    END IF
!------ Read the time header
    READ(lun,IOSTAT=ios) t,npuf,numMet,ncrel
    IF( ios /= 0 )GOTO 9998
    CALL IncrementPuffFileRecord()
  CASE DEFAULT
    CALL SetUnrecognizedVersionError( 'ReadPuffFileTimeHeader' )
    GOTO 9999
END SELECT

IF( pufffile_record /= PUFFFILE_MET_HEADER )THEN
  CALL SetOutOfSyncError( 'ReadPuffFileTimeHeader',PUFFFILE_MET_HEADER )
  GOTO 9999
END IF

9999 CONTINUE

RETURN

9998 CONTINUE

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  CALL SetReadError( 'ReadPuffFileTimeHeader','time header',file,ios )
  GOTO 9999
ELSE IF( ios < 0 )THEN
  BACKSPACE( UNIT=lun,IOSTAT=ios )  !returns to EOF position position
  CALL SetEOFError( 'ReadPuffFileTimeHeader','time header' )
  GOTO 9999
END IF

9997 CONTINUE

IF( ios > 0 )GOTO 9998
CALL SetUnexpectedEOFError( 'ReadPuffFileTimeHeader','time header' )
GOTO 9999

END
!===============================================================================
! ReadPuffFileDataHeader
!===============================================================================
SUBROUTINE ReadPuffFileDataHeader( lun,file,numMet,MetGrid,ncrel,cDefinition )

!Reads next data header

USE files_fi
USE error_fi
USE puffstruct_fd
USE SWIMgridStr_fd
USE cont_rel_fd
USE cont_rel_functions

IMPLICIT NONE

INTEGER,                                 INTENT(  IN ) :: lun
CHARACTER(*),                            INTENT(  IN ) :: file
INTEGER,                                 INTENT(  IN ) :: numMet
TYPE( SWIMgridStr ), DIMENSION(numMet),  INTENT( OUT ) :: MetGrid
INTEGER,                                 INTENT(  IN ) :: ncrel
TYPE( cont_release_def ),  DIMENSION(*), INTENT( OUT ) :: cDefinition

LOGICAL isOpen
INTEGER ios
INTEGER i, j, nrec

LOGICAL, EXTERNAL :: IsPuffFileOpen

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999
IF( .NOT.isOpen )THEN
  CALL SetNotOpenedError( 'ReadPuffFileDataHeader' )
  GOTO 9999
END IF

!==== If Opened then read next header

SELECT CASE( pufffile_version )
  CASE( 0 )
!------ This routine is only called on resume and restart
!------ We should never get here because version checks should throw an error
!------ We will set an error here just in case
!------ With the elimination of src_aux array we can't properly read the old versions
    nError   = IV_ERROR
    eRoutine = 'ReadPuffFileDataHeader'
    eMessage = 'Old SCIPUFF puff file format version encountered'
    eInform  = 'Unable to resume or restart from the old version'
    GOTO 9999
  CASE( 1,PUFFFILE_VERSION_VALUE )
!------ Advance to the next data header if not already pointing to a data header
    IF( pufffile_record /= PUFFFILE_MET_HEADER )THEN
!------ If past the current data header
!------ First advance to the next time header then advance upto the next data header
      IF( pufffile_record > PUFFFILE_MET_HEADER )THEN
        nrec = PUFFFILE_NUM_RECORD - pufffile_record + PUFFFILE_MET_HEADER
!------ Else just advance up to the next data header
      ELSE
        nrec = PUFFFILE_MET_HEADER - pufffile_record
      END IF
      DO i = 1,nrec
        READ(lun,IOSTAT=ios)
        IF( ios /= 0 )GOTO 9997
        CALL IncrementPuffFileRecord()
      END DO
    END IF
!------ Read the met header
    READ(lun,IOSTAT=ios) (MetGrid(i)%nx,MetGrid(i)%ny,i=1,numMet), &
                         (cDefinition(i)%rSet%nrel,i=1,ncrel)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Allocate cDefinition%rels so next record can be read
    CALL allocate_Definitions_rel( ncrel,cDefinition )
    IF( nError /= NO_ERROR )GOTO 9999
!------ Read the src header
    READ(lun,IOSTAT=ios) ((cDefinition(i)%rSet%rels(j)%naux,j=1,cDefinition(i)%rSet%nrel),i=1,ncrel)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
  CASE DEFAULT
    CALL SetUnrecognizedVersionError( 'ReadPuffFileDataHeader' )
    GOTO 9999
END SELECT

IF( pufffile_record /= PUFFFILE_PUFF_RECORD )THEN
  CALL SetOutOfSyncError( 'ReadPuffFileDataHeader',PUFFFILE_PUFF_RECORD )
  GOTO 9999
END IF

9999 CONTINUE

RETURN

9998 CONTINUE

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  CALL SetReadError( 'ReadPuffFileDataHeader','data header',file,ios )
  GOTO 9999
ELSE IF( ios < 0 )THEN
  BACKSPACE( UNIT=lun,IOSTAT=ios )  !returns to EOF position position
  CALL SetEOFError( 'ReadPuffFileDataHeader','data header' )
  GOTO 9999
END IF

9997 CONTINUE
IF( ios > 0 )GOTO 9998
CALL SetUnexpectedEOFError( 'ReadPuffFileDataHeader','data header' )
GOTO 9999

END
!===============================================================================
! ReadPuffFileDataRecord
!===============================================================================
SUBROUTINE ReadPuffFileDataRecord( lun,file,npuf,puff,numMet,MetGrid,ncrel,cDefinition )

!Reads next data record

USE files_fi
USE error_fi
USE puffstruct_fd
USE SWIMgridStr_fd
USE cont_rel_fd

IMPLICIT NONE

INTEGER,                                INTENT(  IN ) :: lun
CHARACTER(*),                           INTENT(  IN ) :: file
INTEGER,                                INTENT(  IN ) :: npuf
TYPE( puff_str ),         DIMENSION(*), INTENT( OUT ) :: puff
INTEGER,                                INTENT(  IN ) :: numMet
TYPE( SWIMgridStr ),      DIMENSION(*), INTENT( OUT ) :: MetGrid
INTEGER,                                INTENT(  IN ) :: ncrel
TYPE( cont_release_def ), DIMENSION(*), INTENT( OUT ) :: cDefinition

LOGICAL isOpen
INTEGER ios
INTEGER i, j, k, nrec

LOGICAL, EXTERNAL :: IsPuffFileOpen
INTEGER, EXTERNAL :: ReadPuffArray
INTEGER, EXTERNAl :: allocatePuffAuxs

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999
IF( .NOT.isOpen )THEN
  CALL SetNotOpenedError( 'ReadPuffFileDataRecord' )
  GOTO 9999
END IF

!==== If Opened then read next time header

SELECT CASE( pufffile_version )
  CASE( 0,1 )
!------ This routine is only called on resume and restart
!------ We should never get here because version checks should throw an error
!------ We will set an error here just in case
!------ With the elimination of src_aux array we can't properly read the old versions
    nError   = IV_ERROR
    eRoutine = 'ReadPuffFileDataRecord'
    eMessage = 'Old SCIPUFF puff file format version encountered'
    eInform  = 'Unable to resume or restart from the old version'
    GOTO 9999
  CASE( PUFFFILE_VERSION_VALUE )
!------ Advance to the next puff record if not already pointing to a puff record
    IF( pufffile_record /= PUFFFILE_PUFF_RECORD )THEN
!------ If past the current puff record
!------ First advance to the next time header then advance upto the next puff record
      IF( pufffile_record > PUFFFILE_PUFF_RECORD )THEN
        nrec = PUFFFILE_NUM_RECORD - pufffile_record + PUFFFILE_PUFF_RECORD
!------ Else just advance upto the next puff record
      ELSE
        nrec = PUFFFILE_PUFF_RECORD - pufffile_record
      END IF
      DO i = 1,nrec
        READ(lun,IOSTAT=ios)
        IF( ios /= 0 )GOTO 9997
        CALL IncrementPuffFileRecord()
      END DO
    END IF
!------ Read puff record. Use Function to "cast" puff to a structure that makes reading easier
    ios = ReadPuffArray( lun,puff,npuf )
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Allcoate puff aux
    ios = allocatePuffAuxs( npuf,puff )
    IF( ios /= 0 )GOTO 9996
!------ Read the aux record
    READ(lun,IOSTAT=ios)((puff(i)%aux(j),j=1,puff(i)%naux),i=1,npuf)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Read the met record
    READ(lun,IOSTAT=ios)(MetGrid(i)%dx,MetGrid(i)%dy,             &
                         MetGrid(i)%xmin,MetGrid(i)%ymin,         &
                         (MetGrid(i)%zi(j),j=1,MetGrid(i)%nx*MetGrid(i)%ny),i=1,numMet)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Read the src record
    READ(lun,IOSTAT=ios)((cDefinition(i)%rSet%rels(j)%plen,                                 &
                         (cDefinition(i)%rSet%rels(j)%saux(k),k=1,cDefinition(i)%rSet%rels(j)%naux), &
                                                        j=1,cDefinition(i)%rSet%nrel),      &
                                                        i=1,ncrel)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()

CASE DEFAULT
    CALL SetUnrecognizedVersionError( 'ReadPuffFileDataRecord' )
    GOTO 9999
END SELECT

IF( pufffile_record /= PUFFFILE_TIME_HEADER )THEN
  CALL SetOutOfSyncError( 'ReadPuffFileDataRecord',PUFFFILE_TIME_HEADER )
  GOTO 9999
END IF

9999 CONTINUE

RETURN

9998 CONTINUE

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  CALL SetReadError( 'ReadPuffFileDataRecord','data record',file,ios )
  GOTO 9999
ELSE IF( ios < 0 )THEN
  BACKSPACE( UNIT=lun,IOSTAT=ios )  !returns to EOF position position
  CALL SetEOFError( 'ReadPuffFileDataRecord','data record' )
  GOTO 9999
END IF

9997 CONTINUE
IF( ios > 0 )GOTO 9998
CALL SetUnexpectedEOFError( 'ReadPuffFileDataRecord','data record' )
GOTO 9999

9996 CONTINUE
nError   = UK_ERROR
eRoutine = 'ReadPuffFileDataRecord'
eMessage = 'Error allocating puff auxiliary arraya'
WRITE(eInform,'(A,I10)') 'IOS =',ios
GOTO 9999

END
!===============================================================================
! ReadPuffFilePuffRecord
!===============================================================================
SUBROUTINE ReadPuffFilePuffRecord( lun,file,npuf,puff )

!Reads next data record - puff data only

USE files_fi
USE error_fi
USE puffstruct_fd
USE oldPuff_fi    !For old version pufffiles

IMPLICIT NONE

INTEGER,                        INTENT( IN    ) :: lun
CHARACTER(*),                   INTENT( IN    ) :: file
INTEGER,                        INTENT( IN    ) :: npuf
TYPE( puff_str ), DIMENSION(*), INTENT( INOUT ) :: puff

LOGICAL isOpen
INTEGER ios
INTEGER i, j, nrec

LOGICAL, EXTERNAL :: IsPuffFileOpen
INTEGER, EXTERNAL :: ReadPuffArray
INTEGER, EXTERNAL :: ReadPuffArrayOld
INTEGER, EXTERNAL :: allocatePuffAuxs

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999
IF( .NOT.isOpen )THEN
  CALL SetNotOpenedError( 'ReadPuffFilePuffRecord' )
  GOTO 9999
END IF

!==== If Opened then read next time header

SELECT CASE( pufffile_version )
  CASE( 0 )
    IF( pufffile_record /= PUFFFILE_TIME_HEADER )CALL BackspacePuffFile( lun,file )
    IF( nError /= 0 )GOTO 9999
!------ Read puff, puff aux from old record. Use Function to "cast" puff to a structure that makes reading easier
    ios = ReadPuffArrayOld( lun,puff,npuf )
    IF( ios /= 0 )GOTO 9998
    pufffile_record = PUFFFILE_TIME_HEADER
  CASE( 1,PUFFFILE_VERSION_VALUE )
!------ Advance to the next puff record if not already pointing to a puff record
    IF( pufffile_record /= PUFFFILE_PUFF_RECORD )THEN
!------ If past the current puff record
!------ First advance to the next time header then advance upto the next puff record
      IF( pufffile_record > PUFFFILE_PUFF_RECORD )THEN
        nrec = PUFFFILE_NUM_RECORD - pufffile_record + PUFFFILE_PUFF_RECORD
!------ Else just advance upto the next puff record
      ELSE
        nrec = PUFFFILE_PUFF_RECORD - pufffile_record
      END IF
      DO i = 1,nrec
        READ(lun,IOSTAT=ios)
        IF( ios /= 0 )GOTO 9997
        CALL IncrementPuffFileRecord()
      END DO
    END IF
!------ Read puff record. Use Function to "cast" puff to a structure that makes reading easier
    ios = ReadPuffArray( lun,puff,npuf )
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Allcoate puff aux
    ios = allocatePuffAuxs( npuf,puff )
    IF( ios /= 0 )GOTO 9996
!------ Read the aux record
    READ(lun,IOSTAT=ios)((puff(i)%aux(j),j=1,puff(i)%naux),i=1,npuf)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Advance over the met record
    READ(lun,IOSTAT=ios)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
!------ Advance over the src record
    READ(lun,IOSTAT=ios)
    IF( ios /= 0 )GOTO 9997
    CALL IncrementPuffFileRecord()
  CASE DEFAULT
    CALL SetUnrecognizedVersionError( 'ReadPuffFilePuffRecord' )
    GOTO 9999
END SELECT

IF( pufffile_record /= PUFFFILE_TIME_HEADER )THEN
  CALL SetOutOfSyncError( 'ReadPuffFileDataRecord',PUFFFILE_TIME_HEADER )
  GOTO 9999
END IF

9999 CONTINUE

RETURN

9998 CONTINUE

! Check for end of file in all compilers
IF( IS_IOSTAT_END(ios) )ios = -1

IF( ios > 0 )THEN
  CALL SetReadError('ReadPuffFilePuffRecord','puff record',file,ios)
  GOTO 9999
ELSE IF( ios < 0 )THEN
  BACKSPACE( UNIT=lun,IOSTAT=ios )  !returns to EOF position
  CALL SetEOFError( 'ReadPuffFilePuffRecord','puff record' )
  GOTO 9999
END IF

9997 CONTINUE
IF( ios > 0 )GOTO 9998
CALL SetUnexpectedEOFError( 'ReadPuffFilePuffRecord','puff record' )
GOTO 9999

9996 CONTINUE
nError   = UK_ERROR
eRoutine = 'ReadPuffFilePuffRecord'
eMessage = 'Error allocating puff auxiliary arraya'
WRITE(eInform,'(A,I10)') 'IOS =',ios
GOTO 9999

END
!===============================================================================
! NearestPuffTime
!===============================================================================
SUBROUTINE NearestPuffTime( lun,file,time,timeID,nearestTime )

!------ Build list of puff times

USE error_fi

IMPLICIT NONE

INTEGER,      INTENT(  IN ) :: lun
CHARACTER(*), INTENT(  IN ) :: file
REAL,         INTENT(  IN ) :: time
INTEGER,      INTENT( OUT ) :: timeID
REAL,         INTENT( OUT ) :: nearestTime

INTEGER npuf, nm, nr
REAL    tx
REAL    currentTime, previousTime

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun,file,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR) GOTO 9999

!------ Count timebreaks

timeID       = 0
currentTime  = -1.0E36
previousTime = -1.0E36

DO WHILE( currentTime <= time )
  CALL ReadPuffFileTimeHeader( lun,file,tx,npuf,nm,nr )
  SELECT CASE( nError )
    CASE( NO_ERROR )
      IF( npuf > 0 )THEN
        previousTime = currentTime
        currentTime  = tx/3600.
        timeID       = timeID + 1
      END IF
    CASE( EOF_ERROR )
      CALL init_error()
      EXIT
    CASE DEFAULT
      GOTO 9999
  END SELECT
END DO

!------ Determine nearest

IF( timeID <= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'NearestPuffTime'
  eMessage = 'No times on file'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
ELSE IF( timeID == 1 )THEN
  nearestTime = currentTime
ELSE IF( ABS(time-currentTime) < ABS(time-previousTime) )THEN
  nearestTime = currentTime
ELSE
  nearestTime = previousTime
  timeID      = timeID-1
END IF

9999 CONTINUE

CALL ClosePuffFile( lun,file )

RETURN
END
!===============================================================================
! IncrementPuffFileRecord()
!===============================================================================
SUBROUTINE IncrementPuffFileRecord()

USE files_fi

IMPLICIT NONE

pufffile_record = pufffile_record + 1
IF( pufffile_record > PUFFFILE_NUM_RECORD )pufffile_record = PUFFFILE_TIME_HEADER

RETURN
END
!===============================================================================
! IncrementPuffFileRecord()
!===============================================================================
SUBROUTINE DecrementPuffFileRecord()

USE files_fi

IMPLICIT NONE

pufffile_record = pufffile_record - 1
IF( pufffile_record < PUFFFILE_TIME_HEADER )pufffile_record = PUFFFILE_NUM_RECORD

RETURN
END
!===============================================================================
! WritePuffArray
!===============================================================================
INTEGER FUNCTION WritePuffArray( lun,max,p,num ) RESULT( ios )

USE puffstruct_fd

IMPLICIT NONE

INTEGER,                             INTENT( IN ) :: lun !Unit number
INTEGER,                             INTENT( IN ) :: max !Dimension of puff array
TYPE( puff_str_ri ), DIMENSION(max), INTENT( IN ) :: p   !puff array
INTEGER,                             INTENT( IN ) :: num !Number of puffs to write

INTEGER i

WRITE(lun,IOSTAT=ios)(p(i)%p_dble,p(i)%p_real,p(i)%p_int,i=1,num)

RETURN
END
!===============================================================================
! ReadPuffArray
!===============================================================================
INTEGER FUNCTION ReadPuffArray( lun,p,num ) RESULT( ios )

USE puffstruct_fd
USE files_fi, ONLY: pufffile_version

IMPLICIT NONE

INTEGER,                             INTENT( IN    ) :: lun !Unit number
INTEGER,                             INTENT( IN    ) :: num !Number of puffs to write
TYPE( puff_str_ri ), DIMENSION(num), INTENT( INOUT ) :: p   !puff array

INTEGER i

REAL, DIMENSION(:), ALLOCATABLE :: xbar, ybar

SELECT CASE( pufffile_version )
  CASE( 1 )
    ALLOCATE( xbar(num),ybar(num),STAT=ios )
    IF( ios /= 0 )GOTO 9999

    READ(lun,IOSTAT=ios)(xbar(i),ybar(i),p(i)%p_real,p(i)%p_int,i=1,num)
    IF( ios /= 0 )GOTO 9999

    DO i = 1,num
      p(i)%p_dble(1) = DBLE(xbar(i))
      p(i)%p_dble(2) = DBLE(ybar(i))
    END DO

  CASE( 2 )
    READ(lun,IOSTAT=ios)(p(i)%p_dble,p(i)%p_real,p(i)%p_int,i=1,num)

END SELECT

9999 CONTINUE

RETURN
END
!===============================================================================
! allocatePuffAuxs
!===============================================================================
RECURSIVE INTEGER FUNCTION allocatePuffAuxs( npuf,puff ) RESULT( ios )

USE puffstruct_fd

IMPLICIT NONE

INTEGER,                        INTENT( IN    ) :: npuf
TYPE( puff_str ), DIMENSION(*), INTENT( INOUT ) :: puff

INTEGER i

INTEGER, EXTERNAL :: allocatePuffAux

ios = 0

DO i = 1,npuf
  ios = allocatePuffAux( puff(i) )
  IF( ios /= 0 )EXIT
END DO

RETURN
END
!===============================================================================
! allocatePuffAux
!===============================================================================
RECURSIVE INTEGER FUNCTION allocatePuffAux( puff ) RESULT( ios )

USE puffstruct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: puff

INTEGER, EXTERNAL :: reallocatePuffAux

ios = 0

IF( ASSOCIATED(puff%aux) )THEN
  ios = -999
ELSE
  IF( puff%naux > 0 )THEN
    ALLOCATE( puff%aux(puff%naux),STAT=ios )
    IF( ios == 0 )puff%aux = 0.0
  ELSE
    NULLIFY( puff%aux )
  END IF
END IF

RETURN
END
!===============================================================================
! deallocatePuffAux
!===============================================================================
INTEGER FUNCTION deallocatePuffAux( puff ) RESULT( ios )

USE puffstruct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: puff

IF( ASSOCIATED(puff%aux) )THEN
  DEALLOCATE( puff%aux,STAT=ios )
  NULLIFY( puff%aux )
ELSE
  ios = 0
END IF

RETURN
END
!===============================================================================
! reallocatePuffAux
!===============================================================================
INTEGER FUNCTION reallocatePuffAux( puff,naux ) RESULT( ios )

USE puffstruct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( INOUT ) :: puff
INTEGER,          INTENT( IN    ) :: naux

INTEGER i, maux
REAL, DIMENSION(:), POINTER :: aux

INTEGER, EXTERNAL :: allocatePuffAux
INTEGER, EXTERNAL :: deallocatePuffAux

NULLIFY(aux)
ios = 0
IF( ASSOCIATED(puff%aux) )THEN
  IF( naux == 0 )THEN
    ios = deallocatePuffaux( puff )
    puff%naux = naux
  ELSE IF( puff%naux /= naux )THEN
    aux => puff%aux
    maux = puff%naux
    NULLIFY( puff%aux )
    puff%naux = naux
    ios = allocatePuffAux( puff )
    IF( ios /= 0 )GOTO 9999
    maux = MIN(maux,naux)
    IF( maux > 0 )puff%aux(1:maux) = aux(1:maux)
  END IF
ELSE
  puff%naux = naux
  ios = allocatePuffAux( puff )
END IF

9999 CONTINUE

IF( ASSOCIATED(aux) )DEALLOCATE( aux,STAT=i )

RETURN
END
!===============================================================================
! countPuffAuxs
!===============================================================================
INTEGER FUNCTION countPuffAuxs( npuf,puff ) RESULT( npaux )

USE puffstruct_fd

IMPLICIT NONE

INTEGER,                        INTENT( IN    ) :: npuf
TYPE( puff_str ), DIMENSION(*), INTENT( INOUT ) :: puff

INTEGER i

npaux = 0

DO i = 1,npuf
  npaux = npaux + puff(i)%naux
END DO

RETURN
END
!===============================================================================
! ReadPuffArrayOld
!===============================================================================
INTEGER FUNCTION ReadPuffArrayOld( lun,p,np ) RESULT( ios )

USE puffstruct_fd
USE oldPuff_fi

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: np  !Number of puffs to read
INTEGER,                         INTENT( IN    ) :: lun !Unit number
TYPE( puff_str ), DIMENSION(np), INTENT( INOUT ) :: p   !puff array

INTEGER i, j, n1,n2
REAL    dum

TYPE( puff_str_ri_NOaux_old )pdum

INTEGER, EXTERNAl :: allocatePuffAuxs
INTEGER, EXTERNAl :: ReadPuffsOld

ios = ReadPuffsOld( lun,p,np )
IF( ios /= 0 )GOTO 9999

BACKSPACE( UNIT=lun,IOSTAT=ios )
IF( ios /= 0 )GOTO 9999

CALL resetPuffAuxOld( np,p,naux_old )

ios = allocatePuffAuxs( np,p )
IF( ios /= 0 )GOTO 9999

READ(lun,IOSTAT=ios)dum,n1,(pdum,i=1,np),n2,((p(i)%aux(j),j=1,p(i)%naux),i=1,np)
IF( ios /= 0 )GOTO 9999

CALL resetLiquidAuxs( np,p )


9999 CONTINUE

RETURN
END
!===============================================================================
! ReadPuffsOld
!===============================================================================
INTEGER FUNCTION ReadPuffsOld( lun,p,np ) RESULT( ios )

USE puffstruct_fd
USE SWIMgridStr_fd
USE cont_rel_fd
USE oldPuff_fi

IMPLICIT NONE

INTEGER,                            INTENT( IN  ) :: lun !Unit number
INTEGER,                            INTENT( IN  ) :: np  !Number of puffs to read
TYPE( puff_str_ri ), DIMENSION(np), INTENT( OUT ) :: p   !puff array

INTEGER i, n1
REAL    dum

REAL, DIMENSION(:), ALLOCATABLE :: xbar, ybar

ALLOCATE( xbar(np),ybar(np),STAT=ios )
IF( ios /= 0 )GOTO 9999

READ(lun,IOSTAT=ios)dum,n1,(xbar(i),ybar(i),p(i)%p_real,p(i)%p_int,i=1,np)
IF( ios /= 0 )GOTO 9999

DO i = 1,np
  p(i)%p_dble(1) = DBLE(xbar(i))
  p(i)%p_dble(2) = DBLE(ybar(i))
END DO

9999 CONTINUE

RETURN
END
!===============================================================================
! resetPuffAuxOld
!===============================================================================
SUBROUTINE resetPuffAuxOld( npuf,puff,np )

USE puffstruct_fd

IMPLICIT NONE

INTEGER,                           INTENT( IN    ) :: npuf
INTEGER,                           INTENT( IN    ) :: np
TYPE( puff_str ), DIMENSION(npuf), INTENT( INOUT ) :: puff

INTEGER i, inext

IF( npuf > 0 )THEN
  i = 1
  DO WHILE( i <= npuf )
    IF( puff(i)%naux > 0 )THEN
      inext = i + 1
      DO WHILE( inext <= npuf )
        IF( puff(inext)%naux > 0 )EXIT
        inext = inext + 1
      END DO
      IF( inext <= npuf )THEN
        puff(i)%naux = puff(inext)%naux - puff(i)%naux
      ELSE
        puff(i)%naux = np - puff(i)%naux
      END IF
      i = inext
    ELSE
      i = i + 1
    END IF
  END DO
END IF

RETURN
END
!===============================================================================
! resetLiquidAuxs
!===============================================================================
SUBROUTINE resetLiquidAuxs( np,p )

USE puffstruct_fd
USE scipuff_fi

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: np
TYPE( puff_str ), DIMENSION(np), INTENT( INOUT ) :: p

INTEGER i, j, irv, nskp, maux

LOGICAL, EXTERNAL :: IsLiquid
INTEGER, EXTERNAL :: reallocatePuffAux

IF( np > 0 )THEN
  DO i = 1,np
    IF( IsLiquid(typeID(p(i)%ityp)%icls) )THEN
      maux = typeID(p(i)%ityp)%npaux
      irv = reallocatePuffAux( p(i),p(i)%naux+1 )
      IF( iversion < 2800 )THEN
        nskp = nskp_dyn - NAUX_DYNAMICS_PART + NAUX_DYNAMICS_PART_OLD
      ELSE
        nskp = nskp_dyn
      END IF
      nskp = nskp + NAUX_LIQUID
      IF( typeID(p(i)%ityp)%ltot )nskp = nskp + NAUX_TOTALCC
      DO j = p(i)%naux,nskp+1,-1
        p(i)%aux(j) = p(i)%aux(j-1)
      END DO
      p(i)%aux(nskp) = 0.0
    END IF
  END DO
END IF

RETURN
END
!===============================================================================
! SetUnrecognizedVersionError
!===============================================================================
SUBROUTINE SetUnrecognizedVersionError( routine )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine

nError   = IV_ERROR
eRoutine = TRIM(routine)
eMessage = 'SCIPUFF puff file format version unrecognized'
WRITE(eInform,'(A,I0)')'Puff file format version=',pufffile_version

RETURN
END
!===============================================================================
! SetOutOfSyncError
!===============================================================================
SUBROUTINE SetOutOfSyncError( routine,irec )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
INTEGER,      INTENT( IN ) :: irec

nError   = IV_ERROR
eRoutine = TRIM(routine)
eMessage = 'SCIPUFF puff file record out of sync'
WRITE(eInform,'(A,I0,A,I0)')'Puff file record=',pufffile_record,' : Expected record=',irec

RETURN
END
!===============================================================================
! SetNotOpenedError
!===============================================================================
SUBROUTINE SetNotOpenedError( routine )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine

nError   = RD_ERROR
eRoutine = TRIM(routine)
eMessage = 'SCIPUFF puff file not opened'
eInform  = 'Puff file record not read'

RETURN
END
!===============================================================================
! SetReadError
!===============================================================================
SUBROUTINE SetReadError( routine,readRecord,file,ios )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: readRecord
CHARACTER(*), INTENT( IN ) :: file
INTEGER,      INTENT( IN ) :: ios

nError   = RD_ERROR
eRoutine = TRIM(routine)
eMessage = 'Error reading '//TRIM(readRecord)//' from the SCIPUFF puff file'
WRITE(eMessage,'(A,I0)') 'Error reading '//TRIM(readRecord)//' from puff file: IOS=',ios
CALL ReportFileName( eInform,'File=',file )

RETURN
END
!===============================================================================
! SetEOFError
!===============================================================================
SUBROUTINE SetEOFError( routine,readRecord )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: readRecord

nError   = EOF_ERROR
eRoutine = TRIM(routine)
eMessage = 'EOF encountered reading '//TRIM(readRecord)//' from the SCIPUFF puff file'

RETURN
END
!===============================================================================
! SetEOFError
!===============================================================================
SUBROUTINE SetUnexpectedEOFError( routine,readRecord )

USE files_fi
USE error_fi

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: routine
CHARACTER(*), INTENT( IN ) :: readRecord

nError   = RD_ERROR
eRoutine = TRIM(routine)
eMessage = 'EOF unexpectedly encountered reading '//TRIM(readRecord)//' from the SCIPUFF puff file'

RETURN
END
!*******************************************************************************
!*******************************************************************************
!===============================================================================
! allocatePuffs
!===============================================================================
INTEGER FUNCTION allocatePuffs( mxpuf ) RESULT( ios )

USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: mxpuf

INTEGER i

ios = 0

IF( ALLOCATED(puff) )CALL DeallocatePuffs()

IF( ios == 0 .AND. mxpuf > 0 )THEN
  ALLOCATE( puff(mxpuf),STAT=ios )
  IF( ios == 0 )THEN
    DO i = 1,mxpuf
      NULLIFY( puff(i)%aux )
    END DO
  END IF
END IF

RETURN
END
!===============================================================================
! deallocatePuffs
!===============================================================================
SUBROUTINE deallocatePuffs()

USE scipuff_fi

IMPLICIT NONE

INTEGER alloc_stat, i

INTEGER, EXTERNAL :: deallocatePuffAux

IF( ALLOCATED(puff) )THEN
  DO i = 1,SIZE(puff)
    alloc_stat = deallocatePuffAux( puff(i) )
  END DO
  DEALLOCATE( puff,STAT=alloc_stat )
END IF

npuf = 0

RETURN
END
!===============================================================================
! CreatePuffFile
!===============================================================================
SUBROUTINE CreatePuffFile()

USE error_fi
USE files_fi

IMPLICIT NONE

!------ Open the puff file - new, read/write

CALL OpenPuffFile( lun_puf,file_puf,.TRUE.,.FALSE. )

RETURN
END
!===============================================================================
! HasPuffs
!===============================================================================
LOGICAL FUNCTION HasPuffs()

!------ Checks to see if any puff time has npuf>0

USE files_fi
USE error_fi
USE plotlist_fi

IMPLICIT NONE

INTEGER npuf, nm, nr
REAL    tx

IF( MemoryField )THEN
  HasPuffs = .TRUE.
ELSE

  HasPuffs = .FALSE.

!------ Open the puff file - exisiting, readonly, rewind

  CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.TRUE. )
  IF( nError /= NO_ERROR )GOTO 9999

!------ Find first timebreak with npuf=0

  DO WHILE( .NOT.HasPuffs )
    CALL ReadPuffFileTimeHeader( lun_puf,file_puf,tx,npuf,nm,nr )
    SELECT CASE( nError )
      CASE( NO_ERROR )
        HasPuffs = npuf > 0
      CASE( EOF_ERROR )
        CALL init_error()
        EXIT
      CASE DEFAULT
        GOTO 9999
    END SELECT
  END DO

9999 CONTINUE

  CALL ClosePuffFile( lun_puf,file_puf )

END IF

RETURN
END
!===============================================================================
! CurrentPuffHeader
!===============================================================================
SUBROUTINE CurrentPuffHeader( t,npuf )

!------ Gets the last puff fiel header

USE files_fi
USE error_fi
USE plotlist_fi

IMPLICIT NONE

REAL,    INTENT( OUT ) :: t
INTEGER, INTENT( OUT ) :: npuf

INTEGER np, nm, nr
REAl tx

!------ Initialize

t    = 0.0
npuf = 0

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read to the end

DO
  CALL ReadPuffFileTimeHeader( lun_puf,file_puf,tx,np,nm,nr )
  SELECT CASE( nError )
    CASE( NO_ERROR )
      t = tx
      npuf = np
    CASE( EOF_ERROR )
      CALL init_error()
      EXIT
    CASE DEFAULT
      GOTO 9999
  END SELECT
END DO

9999 CONTINUE

CALL ClosePuffFile( lun_puf,file_puf )

RETURN
END
!===============================================================================
! NumberPuffTimes
!===============================================================================
INTEGER FUNCTION NumberPuffTimes( includeZero )

!------ Counts number of puff times with npuf>0

USE files_fi
USE error_fi
USE plotlist_fi

IMPLICIT NONE

LOGICAL, INTENT( IN ) :: includeZero

INTEGER npuf, nm, nr
REAL    tx

NumberPuffTimes = 0

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Count number of puff times with npuf > 0

DO
  CALL ReadPuffFileTimeHeader( lun_puf,file_puf,tx,npuf,nm,nr )
  SELECT CASE( nError )
    CASE( NO_ERROR )
      IF( npuf > 0 .OR. includeZero )NumberPuffTimes = NumberPuffTimes + 1
    CASE( EOF_ERROR )
      CALL init_error()
      EXIT
    CASE DEFAULT
      GOTO 9999
  END SELECT
END DO

9999 CONTINUE

CALL ClosePuffFile( lun_puf,file_puf )

RETURN
END
!===============================================================================
! GetPuffTimes
!===============================================================================
SUBROUTINE GetPuffTimes( start,maxt,times )

!------ Get puff times with npuf>0

USE files_fi
USE error_fi
USE plotlist_fi

IMPLICIT NONE

TYPE( startT ),                     INTENT( IN  ) :: start
INTEGER,                            INTENT( IN  ) :: maxt
TYPE( SCIPTimeT ), DIMENSION(maxt), INTENT( OUT ) :: times

INTEGER npuf, nm, nr, n
REAL    tx

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

n = 0

!------ Get puff times with npuf > 0

DO
  CALL ReadPuffFileTimeHeader( lun_puf,file_puf,tx,npuf,nm,nr )
  SELECT CASE( nError )
    CASE( NO_ERROR )
      IF( npuf > 0 )CALL FillPuffTime( n,tx,npuf,start,maxt,times )
      IF( nError /= NO_ERROR) GOTO 9999
    CASE( EOF_ERROR )
      CALL init_error()
      EXIT
    CASE DEFAULT
      GOTO 9999
  END SELECT
END DO

9999 CONTINUE

CALL ClosePuffFile( lun_puf,file_puf )

RETURN
END
!===============================================================================
! FillPuffTime
!===============================================================================
SUBROUTINE FillPuffTime( n,tx,npuf,start,maxt,times )

USE time_fd
USE plotlist_fd
USE error_fi
USE SCIPresults_fd

IMPLICIT NONE

INTEGER,                            INTENT( INOUT ) :: n
REAL,                               INTENT( IN    ) :: tx
INTEGER,                            INTENT( IN    ) :: npuf
TYPE( startT ),                     INTENT( IN    ) :: start
INTEGER,                            INTENT( IN    ) :: maxt
TYPE( SCIPTimeT ), DIMENSION(maxt), INTENT( INOUT ) :: times

INTEGER ios
INTEGER iout
REAL tout

INTEGER, EXTERNAL :: ComputeTime
INTEGER, EXTERNAL :: TimeToString

n = n + 1

IF( n > maxt )THEN
  nError = SZ_ERROR
  eRoutine = 'FillPuffTime'
  eMessage = 'Puff time array too small for additional time'
  WRITE(eInform,'(A,I0)')'Current puff time array size=',maxt
  GOTO 9999
END IF

times(n)%nItems = npuf
ios = ComputeTime( start,tx/3600.,times(n)%time )
IF( ios /= SCIPsuccess )THEN
  nError   = UK_ERROR
  eRoutine = 'FillPuffTime'
  eMessage = 'Error converting relative puff time to absolute time'
  GOTO 9999
END IF

IF( tx/3600. > 1000. .AND. n > 1000 )THEN
  !--- Adjust time to nearest multiple of output time step
  tout = times(2)%time%hour - times(1)%time%hour
  iout = NINT(times(n)%time%hour/tout)
  times(n)%time%hour = FLOAT(iout)*tout
END IF

ios = TimeToString( times(n)%time,times(n)%string )
IF( ios /= SCIPsuccess )THEN
  nError   = UK_ERROR
  eRoutine = 'FillPuffTimes'
  eMessage = 'Error converting puff time to string format'
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
! ReadPuffsID
!===============================================================================
SUBROUTINE ReadPuffsID( timeID )

!------ Read puff data at time identified by iTimeID
!       If timeID <=0 count all times else only count npuf>0 times

USE files_fi
USE scipuff_fi

IMPLICIT NONE

INTEGER, INTENT( IN ) :: timeID

LOGICAL includeZero
INTEGER npuf_local
INTEGER ios, count, tID, i, ifld, nm, nr

INTEGER, EXTERNAL :: NumberPuffTimes
INTEGER, EXTERNAL :: getPuffifld_2000
INTEGER, EXTERNAL :: allocatePuffs

!------ Initialize

count = 0
npuf_local = 0

includeZero = (timeID <= 0)   !timeID based on surface files times so include times with npuf=0

tID = MAX(1,ABS(timeID))
tID = MIN(tID,NumberPuffTimes( includeZero ))

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Skip to correct timeID


!------ Skip times if tID > 1

IF( tID > 1 )THEN
  count = 1
  DO WHILE( count < tID )
    CALL ReadPuffFileTimeHeader( lun_puf,file_puf,t,npuf_local,nm,nr )
    IF( nError /= NO_ERROR) GOTO 9999
    IF( npuf_local > 0 .OR. includeZero )count = count + 1
  END DO
END IF

!------ Read next break with npuf_local > 0

DO
  CALL ReadPuffFileTimeHeader( lun_puf,file_puf,t,npuf_local,nm,nr )
  IF( nError /= NO_ERROR )GOTO 9999
  IF( npuf_local > 0 .OR. includeZero )EXIT
END DO

!------ Allocate puff and puff aux arrays

ios = allocatePuffs( npuf_local )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'ReadPuffsID'
  eMessage = 'Error allocating puff array'
  WRITE(eInform,'(A,I10)') 'Requested size =',npuf_local
  GOTO 9999
END IF

!------ Read puff data
CALL ReadPuffFilePuffRecord( lun_puf,file_puf,npuf_local,puff )
IF( nError /= NO_ERROR) GOTO 9999

!---- Adjust ifld in puff%ipgd for older versions
!---- This assumes that the Project (.prj)  file has been read to get iversion

IF( iversion < 2800 )THEN        ! No horizonal momentum prior to 2800
  IF( dynamic )THEN
    CALL resetPuffAuxs_2700(npuf_local,puff)
  END IF
  IF( iversion < 2000 )THEN   ! No ifld prior to SWIM nested met fields
    DO i = 1,npuf_local
      CALL setPuffifld( puff(i),1 )
      CALL setPuffirel( puff(i),0 )
    END DO
  ELSE IF( iversion < 2100 )THEN
    DO i = 1,npuf_local
      ifld = getPuffifld_2000( puff(i) )
      CALL setPuffifld( puff(i),ifld )
      CALL setPuffirel( puff(i),0 )
    END DO
  END IF
END IF

9999 CONTINUE

CALL ClosePuffFile( lun_puf,file_puf )

! set npuf value from local npuf
npuf = npuf_local

RETURN
END
!*******************************************************************************
!            Move puff data
!*******************************************************************************
SUBROUTINE LoadPuff( pIn,pOut )

USE spcstruct_fd
USE puffstruct_fd

IMPLICIT NONE

TYPE( puff_str ), INTENT( IN  ) :: pIn !Almost a puff_str except for iaux/naux
TYPE( puffT    ), INTENT( OUT ) :: pOut

TYPE( puffCopy ) pCopy

pCopy = TRANSFER(pIn,pCopy)

pOut = TRANSFER(pCopy%puff,pOut)

RETURN
END
!*******************************************************************************
!            Move puff data
!*******************************************************************************
SUBROUTINE UnloadPuff( pIn,pOut )

USE spcstruct_fd
USE puffstruct_fd

IMPLICIT NONE

TYPE( puffT    ), INTENT( IN  ) :: PIn
TYPE( puff_str ), INTENT( OUT ) :: pOut !Really a puff_str but for now they are identical

TYPE( puffCopy ) pCopy

pCopy%puff = TRANSFER(pIn,pCopy%puff)
NULLIFY(pCopy%aux)

pOut = TRANSFER(pCopy,pOut)

RETURN
END
!===============================================================================
! resetPuffAuxs_2700
!===============================================================================
SUBROUTINE resetPuffAuxs_2700( np,p )

USE puffstruct_fd
USE scipuff_fi

IMPLICIT NONE

INTEGER,                         INTENT( IN    ) :: np
TYPE( puff_str ), DIMENSION(np), INTENT( INOUT ) :: p

INTEGER i, j, irv, newAux

TYPE( puff_dynamics ) pd

LOGICAL, EXTERNAL :: IsGas
INTEGER, EXTERNAL :: reallocatePuffAux

IF( np > 0 )THEN
  DO i = 1,np
    CALL get_dynamics_2700(p(i),pd)
    IF( IsGas(typeID(p(i)%ityp)%icls) )THEN
      newAux = NAUX_DYNAMICS_GAS - NAUX_DYNAMICS_GAS_OLD
    ELSE
      newAux = NAUX_DYNAMICS_PART - NAUX_DYNAMICS_PART_OLD
    END IF
    irv = reallocatePuffAux( p(i),p(i)%naux+newAux )
    DO j = p(i)%naux,NAUX_DYNAMICS_PART_OLD+newAux,-1
      p(i)%aux(j) = p(i)%aux(j-newAux)
    END DO
    CALL put_dynamics( p(i),pd )
  END DO
END IF

RETURN
END
!===============================================================================
! get_dynamics_2700
!===============================================================================
SUBROUTINE get_dynamics_2700( p,pd )

USE scipuff_fi

IMPLICIT NONE

TYPE( puff_str ),      INTENT( IN  ) :: p
TYPE( puff_dynamics ), INTENT( OUT ) :: pd

INTEGER ipaux

LOGICAL, EXTERNAL :: IsGas

ipaux  = 1
pd%wcb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%ctb = p%aux(ipaux)
ipaux  = ipaux + 1
pd%wcp = p%aux(ipaux)
ipaux  = ipaux + 1
pd%ctp = p%aux(ipaux)

pd%ucb = 0.0
pd%vcb = 0.0
pd%ucp = 0.0
pd%vcp = 0.0

IF( IsGas(typeID(p%ityp)%icls) )THEN
  pd%un = 0.0
  pd%vn = 0.0
  ipaux = ipaux + 1
  pd%w  = p%aux(ipaux)
  ipaux = ipaux + 1
  pd%t  = p%aux(ipaux)
ELSE
  pd%w  = 0.0
  pd%t  = 0.0
  pd%un = 0.0
  pd%vn = 0.0
END IF

IF( buoy_gas )THEN
  ipaux  = ipaux + 1
  pd%bcb = p%aux(ipaux)
  ipaux  = ipaux + 1
  pd%bcp = p%aux(ipaux)
ELSE
  pd%bcb = 0.0
  pd%bcp = 0.0
END IF

IF( dense_gas .AND. p%c > SMALL )THEN
  ipaux = ipaux + 1
  pd%u  = p%aux(ipaux) / p%c
  ipaux = ipaux + 1
  pd%v  = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dudx = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dudy = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dvdx = p%aux(ipaux) / p%c
  ipaux   = ipaux + 1
  pd%dvdy = p%aux(ipaux) / p%c

  IF( IsGas(typeID(p%ityp)%icls) )THEN
    ipaux = ipaux + 1
    pd%u0 = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%X  = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%Y  = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%sn = p%aux(ipaux) / p%c
    ipaux = ipaux + 1
    pd%cs = p%aux(ipaux) / p%c
  ELSE
    pd%u0 = 0.0
    pd%X  = 0.0
    pd%Y  = 0.0
    pd%sn = 0.0
    pd%cs = 0.0
  END IF

ELSE

  pd%u    = 0.0
  pd%v    = 0.0
  pd%dudx = 0.0
  pd%dudy = 0.0
  pd%dvdx = 0.0
  pd%dvdy = 0.0
  pd%u0   = 0.0
  pd%X    = 0.0
  pd%Y    = 0.0
  pd%sn   = 0.0
  pd%cs   = 0.0

END IF

RETURN
END

