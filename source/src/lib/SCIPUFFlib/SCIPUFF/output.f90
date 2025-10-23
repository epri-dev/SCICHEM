!===============================================================================
! write_puff
!===============================================================================
SUBROUTINE write_puff()

USE scipuff_fi
USE met_fi
USE files_fi
USE cont_rel_fi
USE SWIMparam_fd
USE cont_rel_functions

!------  this subroutine writes the current data to the puff file

IMPLICIT NONE

INTEGER hour, min, sec, year, month, day

CHARACTER(32) ctem

IF( lymd )THEN
  CALL time_cnv( t,local,lymd,hour,min,sec,year,month,day,ctem )
  WRITE(lun_log,'(A,1PG11.4,3A)',ERR=9998)'Writing puff output at t =',t/3600., &
                                       'hrs. (',TRIM(ctem),')'
ELSE
  WRITE(lun_log,'(A,1PG11.4,3A)',ERR=9998)'Writing puff output at t =',t/3600.,'hrs.'
END IF

!---- Write puff file

CALL WritePuffData( lun_puf,file_puf )
IF( nError /= NO_ERROR )GOTO 9999

lzi_prj = .TRUE.

9999 CONTINUE

RETURN

!------ set log write error and goto return

9998 CONTINUE
nError   = WR_ERROR
eRoutine = 'output'
eMessage = 'Error writing SCIPUFF log file'
CALL ReportFileName( eInform,'File=',file_log )
GOTO 9999

END
!===============================================================================
! WritePuffData
!===============================================================================
SUBROUTINE WritePuffData( lun,file )

USE files_fi
USE error_fi
USE scipuff_fi
USE met_fi
USE cont_rel_fi
USE SWIMparam_fd

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

LOGICAL isOpen
INTEGER i, j, k, ios, irv

INTEGER, DIMENSION(:), ALLOCATABLE :: IDrel

LOGICAL, EXTERNAL :: IsPuffFileOpen
INTEGER, EXTERNAL :: SWIMgetMixingHt
INTEGER, EXTERNAL :: WritePuffArray

!==== Check status

isOpen = IsPuffFileOpen( lun,file,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999
IF( .NOT.isOpen )THEN
  nError   = WR_ERROR
  eRoutine = 'WritePuffData'
  eMessage = 'SCIPUFF puff file not opened for writing'
  CALL ReportFileName( eInform,'File=',file )
  GOTO 9999
END IF

!==== If Opened then write

!----- set up output

IF( numMet > 0 )THEN

  DO i = 1,numMet
    irv = SWIMgetMixingHt( i,MetGrid(i) )
    IF( irv /= SWIMsuccess )THEN
      CALL setSWIMerror( 'SWIMgetMixingHt' )
      GOTO 9999
    END IF
  END DO

END IF

!-----  write time header

WRITE(lun,IOSTAT=ios) t,npuf,numMet,numDefinition
IF( ios /= 0 )GOTO 9998

!-----  write met/src header

WRITE(lun,IOSTAT=ios)(MetGrid(i)%nx,MetGrid(i)%ny,i=1,numMet), &
                     (cDefinition(i)%rSet%nrel,i=1,numDefinition)
IF( ios /= 0 )GOTO 9998

!-----  write src aux header

WRITE(lun,IOSTAT=ios)((cDefinition(i)%rSet%rels(j)%naux,j=1,cDefinition(i)%rSet%nrel),i=1,numDefinition)
IF( ios /= 0 )GOTO 9998

!-----  write puff data. Use Function to "cast" puff to a structure that makes writing easier

ios = WritePuffArray( lun,SIZE(puff),puff,npuf )
IF( ios /= 0 )GOTO 9998

!-----  write aux data

WRITE(lun,IOSTAT=ios)((puff(i)%aux(j),j=1,puff(i)%naux),i=1,npuf)
IF( ios /= 0 )GOTO 9998

!-----  write met data

WRITE(lun,IOSTAT=ios)(MetGrid(i)%dx,MetGrid(i)%dy,             &
                      MetGrid(i)%xmin,MetGrid(i)%ymin,         &
                      (MetGrid(i)%zi(j),j=1,MetGrid(i)%nx*MetGrid(i)%ny),i=1,numMet)
IF( ios /= 0 )GOTO 9998

!-----  write src data

WRITE(lun,IOSTAT=ios)((cDefinition(i)%rSet%rels(j)%plen,                                     &
                      (cDefinition(i)%rSet%rels(j)%saux(k),k=1,cDefinition(i)%rSet%rels(j)%naux), &
                                                        j=1,cDefinition(i)%rSet%nrel),         &
                                                        i=1,numDefinition)
IF( ios /= 0 )GOTO 9998
CALL FLUSH(lun)

9999 CONTINUE

DEALLOCATE( IDrel,STAT=ios )

RETURN

9998 CONTINUE
nError   = WR_ERROR
eRoutine = 'WritePuffData'
WRITE(eMessage,'("Error writing puff file with ios=",I4)')ios
WRITE(eInform,'(A,F10.3)')'Time=',t/3600.
CALL ReportFileName( eAction,'File=',file )
GOTO 9999

END
!===============================================================================
! read_puff
!===============================================================================
SUBROUTINE read_puff()

USE scipuff_fi
USE met_fi
USE files_fi
USE cont_rel_fi
USE SWIMparam_fd
USE cont_rel_functions

!------  this subroutine opens the puff file and reads last set of puff data

IMPLICIT NONE

INTEGER ios

!------ Open the puff file - existing, read/write, append

CALL OpenPuffFile( lun_puf,file_puf,.FALSE.,.FALSE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Since the above positions the file at the end backup one record

CALL BackspacePuffFile( lun_puf,file_puf )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read the time header from the file

CALL ReadPuffFileTimeHeader( lun_puf,file_puf,t,npuf,numMet,numDefinition )
IF( nError /= NO_ERROR )GOTO 9999

!------ Allocate cDefinition

CALL allocate_Definitions( MAX(1,numDefinition) )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read the data header from the file
!------ MetGrid has already been allocated.
!------ Assumption is that numMet on .prj and .puf files are the same

CALL ReadPuffFileDataHeader( lun_puf,file_puf,numMet,MetGrid,numDefinition,cDefinition )
IF( nError /= NO_ERROR )GOTO 9999

!------- Allocate data arrays

CALL AllocatePuffData()
IF( nError /= NO_ERROR )GOTO 9999

CALL allocate_contRels_aux()
IF( nError /= NO_ERROR )GOTO 9999

!------ Read puff,met,src data

CALL ReadPuffFileDataRecord( lun_puf,file_puf,npuf,puff,numMet,MetGrid,numDefinition,cDefinition )
IF( nError /= NO_ERROR )GOTO 9999

!------ This should leave the file positioned at the end for future writing

9999 CONTINUE

IF( numDefinition == 0 .AND. ALLOCATED( cDefinition ) )THEN
  ios = deallocate_Definitions()
END IF

RETURN
END
!===============================================================================
! AllocatePuffData
!===============================================================================
SUBROUTINE AllocatePuffData()

USE scipuff_fi
USE files_fi
USE cont_rel_fi

!------  this subroutine allocated the puff arrays

IMPLICIT NONE

INTEGER ios
INTEGER, EXTERNAl :: allocatePuffs

IF( npuf <= MAXPUF )THEN
  ios = allocatePuffs( MAXPUF )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'AllocatePuffData'
    eMessage = 'Error allocating puff array'
    WRITE(eInform,'(A,I10)') 'Requested size =',MAXPUF
    GOTO 9999
  END IF
ELSE
  nError   = SZ_ERROR
  eRoutine = 'AllocatePuffData'
  eMessage = 'Too many puffs for resuming'
  WRITE(eInform,'(A,I10)') 'Specified maximum =',MAXPUF
  WRITE(eAction,'(A,I10)') 'No. for restart   =',npuf
  GOTO 9999
END IF

9999 CONTINUE

RETURN
END
!===============================================================================
! read_puff_rst
!===============================================================================
SUBROUTINE read_puff_rst( lun,file )

USE scipuff_fi
USE met_fi
USE files_fi
USE cont_rel_fi
USE special_rst_fi
USE cont_rel_functions

!------  this subroutine opens a puff file and reads a set of puff data to initialize
!        this projects puff data

IMPLICIT NONE

INTEGER,      INTENT( IN ) :: lun
CHARACTER(*), INTENT( IN ) :: file

INTEGER ios, i, nn
REAL    tSearch, tPrevious, tHead

!------ Open the puff file - exisiting, readonly, rewind

CALL OpenPuffFile( lun,file,.FALSE.,.TRUE. )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read the time header down the file until the file

tSearch = time_rst
IF( tSearch == DEF_VAL_R )tSearch = HUGE(tSearch)

n_rst=0

tPrevious = 0.0
CALL ReadPuffFileTimeHeader( lun,file,tCurrent,npuf,numOld,numDefinition )
IF( nError /= NO_ERROR )GOTO 9999

IF( npuf /= 0 )n_rst = n_rst+1   !skip npuf=0 on first record

DO WHILE( tCurrent < tSearch )
  CALL ReadPuffFileTimeHeader( lun,file,tHead,npuf,numOld,numDefinition )
  SELECT CASE( nError )
    CASE( NO_ERROR )
      tPrevious = tCurrent
      tCurrent  = tHead
      n_rst     = n_rst + 1
    CASE( EOF_ERROR )
      CALL init_error()
      CALL BackspacePuffFile( lun,file )  !last good record
      IF( nError /= NO_ERROR )GOTO 9999
      CALL ReadPuffFileTimeHeader( lun,file,tCurrent,npuf,numOld,numDefinition )
      IF( nError /= NO_ERROR )GOTO 9999
      EXIT
    CASE DEFAULT
      GOTO 9999
  END SELECT
END DO

!------ Check to see which time is closest

IF( tSearch /= DEF_VAL_R )THEN
  IF( ABS(tPrevious-time_rst) < ABS(tCurrent-time_rst) )THEN
    CALL BackspacePuffFile( lun,file )  !Backup to the start tCurrent record
    IF( nError /= NO_ERROR )GOTO 9999
    n_rst = n_rst - 1
    CALL BackspacePuffFile( lun,file )  !Backup to the start of tPrevious record
    IF( nError /= NO_ERROR )GOTO 9999
    n_rst = n_rst - 1
    CALL ReadPuffFileTimeHeader( lun,file,tCurrent,npuf,numOld,numDefinition )
    IF( nError /= NO_ERROR )GOTO 9999
    n_rst = n_rst + 1
  END IF
END IF

!------ Allocate old met grid

ALLOCATE( OldGrid(MAX(numOld,1)),STAT=ios )
IF( ios /= 0 )THEN
  nError   = UK_ERROR
  eRoutine = 'read_puff_rst'
  eMessage = 'Error allocating old met grid array'
  WRITE(eInform,'(A,I10)') 'Requested size =',numOld
  GOTO 9999
END IF

!------ Allocate cDefinition

CALL allocate_Definitions( numDefinition )
IF( nError /= NO_ERROR )GOTO 9999

!------ Read the data header from the file

CALL ReadPuffFileDataHeader( lun,file,numOld,OldGrid,numDefinition,cDefinition )
IF( nError /= NO_ERROR )GOTO 9999

DO i = 1,numOld
  nn = OldGrid(i)%nx*OldGrid(i)%ny
  ALLOCATE( OldGrid(i)%zi(nn),STAT=ios )
  IF( ios /= 0 )THEN
    nError   = UK_ERROR
    eRoutine = 'read_puff_rst'
    eMessage = 'Error allocating old met grid zi array'
    WRITE(eInform,'(A,I10)') 'Requested size =',nn
    GOTO 9999
  END IF
END DO

CALL allocate_contRels_aux()
IF( nError /= NO_ERROR )GOTO 9999

!------- Check size of data arrays

IF( npuf > MAXPUF )THEN
  nError = SZ_ERROR
  eRoutine = 'read_puff_rst'
  eMessage = 'Too many puffs for restarting'
  WRITE(eInform,'(A,I10)') 'Specified maximum =',MAXPUF
  WRITE(eAction,'(A,I10)') 'No. for restart   =',npuf
  GOTO 9999
END IF

!------ Read puff,met,src data

CALL ReadPuffFileDataRecord( lun,file,npuf,puff,numOld,OldGrid,numDefinition,cDefinition )
IF( nError /= NO_ERROR )GOTO 9999

!------ check old grid dimensions

DO i = 1,numOld
  IF( OldGrid(i)%nx > MAX1D_MET .OR. OldGrid(i)%ny > MAX1D_MET )THEN
    nError   = SZ_ERROR
    eRoutine = 'read_puff_rst'
    eMessage = 'Met grid dimensions too large for restarting'
    CALL ReportFileName( eInform,'File=',file )
    GOTO 9999
  END IF
END DO

!------ check time

IF( ABS(tCurrent-time_rst) > 60. )THEN ! 1 minute tolerance
  nError = UK_ERROR
  eRoutine = 'read_puff_rst'
  eMessage = 'Restart time mismatch'
  WRITE(eInform, &
        "('Specified time =',F8.1,' Nearest time =',F8.1)") time_rst,tCurrent
  eAction = 'Restart Puff File='//TRIM(file)
  GOTO 9999
END IF


9999 CONTINUE

CALL ClosePuffFile( lun,file )

RETURN
END
