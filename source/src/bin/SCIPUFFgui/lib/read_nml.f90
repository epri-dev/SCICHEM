!***********************************************************************
!               ReadNamelist
!***********************************************************************
SUBROUTINE read_namelist( iwnd_db,id_dialog,filename,lok )

USE resource_fd
USE create_fi
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE errorParam_fd
USE param_fd
USE GUItool_fi

!     This routine reads a namelist

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db
INTEGER              id_dialog
CHARACTER(*)         filename
LOGICAL              lok

TYPE( ProjectStructure) prjdlg

INTEGER ios,nchf,irv

INTEGER nError
CHARACTER(128) eMessage,eInform,eRoutine

LOGICAL, EXTERNAL :: CheckFile

lok = .FALSE.
eRoutine = 'ReadNamelist'

nchf = LEN(TRIM(filename))
IF( nchf > 1 )THEN
  IF( .NOT.CheckFile(filename) )THEN
    nError = NF_ERROR
    eMessage = 'File not found'
    CALL ReportFileName( eInform,'File=',filename )
    CALL SetError( nError,eMessage,eInform,' ',eRoutine )
    GOTO 9997
  END IF
ELSE
  nError = IV_ERROR
  eMessage = 'No file name specified'
  eInform  = ' '
  CALL SetError( nError,eMessage,eInform,' ',eRoutine )
  GOTO 9997
END IF

!==== SCIP Tool

SELECT CASE( id_dialog )

  CASE( IDB_RESTART )
    prjdlg = project(DEFAULT_LEVEL)
    CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
    CALL RemoveExtension( prjdlg%ID%name )
    CALL GUI_SCIP_ctrl( prjdlg,ctrl )
    irv = SCIPLoadCtrlF( ToolCallerID,ctrl )
    IF( irv == SCIPfailure )THEN
      CALL GetToolError( eRoutine )
      GOTO 9997
    END IF

  CASE( IDB_PRJDEF )
    prjdlg = project(DEFAULT_LEVEL)
    CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
    CALL RemoveExtension( prjdlg%ID%name )
    CALL GUI_SCIP_flags( prjdlg,flags )
    irv = SCIPLoadFlagsF( ToolCallerID,flags )
    IF( irv == SCIPfailure )THEN
      CALL GetToolError( eRoutine )
      GOTO 9997
    END IF

  CASE( IDB_OPTIONS )
    CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
    CALL RemoveExtension( prjdlg%ID%name )
    CALL GUI_SCIP_options( prjdlg,dlgOptions(DEFAULT_LEVEL),prjOptions )
    irv = SCIPLoadOptionsF( ToolCallerID,prjOptions )
    IF( irv == SCIPfailure )THEN
      CALL GetToolError( eRoutine )
      GOTO 9997
    END IF

  CASE( IDB_TIME )
    CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
    CALL RemoveExtension( prjdlg%ID%name )
    CALL GUI_SCIP_time( prjdlg,dlgTime(DEFAULT_LEVEL),time )
    irv = SCIPLoadTimeF( ToolCallerID,time )
    IF( irv == SCIPfailure )THEN
      CALL GetToolError( eRoutine )
      GOTO 9997
    END IF

  CASE( IDB_DOMAIN )
    CALL SplitName( filename,prjdlg%ID%name,prjdlg%ID%path )
    CALL RemoveExtension( prjdlg%ID%name )
    CALL GUI_SCIP_domain( prjdlg,dlgDomain(DEFAULT_LEVEL),domain )
    irv = SCIPLoadDomainF( ToolCallerID,domain )
    IF( irv == SCIPfailure )THEN
      CALL GetToolError( eRoutine )
      GOTO 9997
    END IF

  CASE DEFAULT

END SELECT

!==== SCIP Tool

lok = .TRUE.

9999 CONTINUE

CLOSE( UNIT=lun_tmp,IOSTAT=ios )
RETURN

9997 CONTINUE
CALL ShowErrorMessage( iwnd_db )
lok = .FALSE.
GOTO 9999

END
