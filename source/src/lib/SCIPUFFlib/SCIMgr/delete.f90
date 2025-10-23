!*******************************************************************************
!            DeleteProject
!*******************************************************************************
INTEGER FUNCTION DeleteProject( UserID,project,request )

USE SCIMgr_fd
USE SCIMgr_fi
USE SCIMgrState
USE error_fi

!     Deletes SCIP project files

IMPLICIT NONE

INTEGER,             INTENT( IN ) :: UserID  !USER ID tag
TYPE ( projectIDT ), INTENT( IN ) :: project !Project ID
INTEGER,             INTENT( IN ) :: request !Delete instructions

CHARACTER(PATH_MAXLENGTH)   basename, filename
CHARACTER(PATH_MAXLENGTH*2) test_string

CHARACTER(16) ext

INTEGER irv, i
INTEGER ReturnValue, iBit, currentState

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddExtension

!==== Initialize

DeleteProject = SCIPnull

IF( SCIMgrCheckState(HS_IDLEBUSY) )THEN !Not available during callbacks
  currentState = SCIMgrSetState( HS_BUSY )
ELSE
  CALL SCIMgrSetBusyMsg()
  RETURN
END IF

CALL set_messaging( userID )

!==== Check total project file name length


test_string = TRIM(project%name); CALL AddPath( test_string,TRIM(project%path) )
irv = LEN_TRIM(test_string)
IF( irv+5 > PATH_MAXLENGTH )THEN
  eRoutine = 'DeleteProject'
  nError   = IV_ERROR
  eMessage = 'Project name w/ path and extension is too long'
  WRITE(eInform,'(A,I4,A,I4)') 'Current length: ',irv,' Max is ',PATH_MAXLENGTH-5
  GOTO 9999
END IF

!==== Loop over request bits

basename = project%name
CALL AddPath( basename,project%path )

ReturnValue = 0

DO iBit = 0,HD_MAXBITS

  IF( BTEST(request,iBit) )THEN

    filename = ''

    SELECT CASE( iBit )
      CASE( HDB_METFILE ) !Delete Met files
      CASE( HDB_TERFILE ) !Delete Terrain files
      CASE( HDB_RELFILE ) !Delete CLOUDTRANS files
      CASE( HDB_SAMFILE ) !Delete Sampler input files

      CASE( HDB_INPFILE ) !Delete INP file
        filename = AddExtension(basename,'inp')

      CASE( HDB_MSCFILE ) !Delete MSC file
        filename = AddExtension(basename,'msc')

      CASE( HDB_SCNFILE ) !Delete SCN file
        filename = AddExtension(basename,'scn')

      CASE( HDB_RADFILE ) !Delete RAD file
        filename = AddExtension(basename,'rad')

      CASE( HDB_PRJFILE ) !Delete PRJ file
        filename = AddExtension(basename,'prj')

      CASE( HDB_PUFFILE ) !Delete PUF file
        filename = AddExtension(basename,'puf')

      CASE( HDB_DOSFILE ) !Delete DOS file
        DO i = 1,999
          WRITE(ext,'(I3.3)') i
          ext = 'dos'//TRIM(ADJUSTL(ext))
          filename = AddExtension(basename,ext)
          CALL DeleteProjectFile( filename,ibit,ReturnValue )
        END DO

        filename = AddExtension(basename,'ados')
        CALL DeleteProjectFile( filename,ibit,ReturnValue )
        filename = AddExtension(basename,'ave')
        CALL DeleteProjectFile( filename,ibit,ReturnValue )
        filename = AddExtension(basename,'dos')

      CASE( HDB_DEPFILE ) !Delete DEP file
        filename = AddExtension(basename,'dep')

      CASE( HDB_LOGFILE ) !Delete LOG file
        filename = AddExtension(basename,'log')

      CASE( HDB_MCWFILE ) !Delete MCW file and nested/smooth fields
        DO i = 2,99
          WRITE(ext,*) i
          ext = 'mc'//TRIM(ADJUSTL(ext))
          filename = AddExtension(basename,ext)
          CALL DeleteProjectFile( filename,ibit,ReturnValue )
        END DO

        filename = AddExtension(basename,'mcw')

      CASE( HDB_SMPFILE ) !Delete SMP file
        filename = AddExtension(basename,'asmp')
        CALL DeleteProjectFile( filename,ibit,ReturnValue )
        filename = AddExtension(basename,'smp')

      CASE( HDB_SPSFILE ) !Delete binary sampler file
        filename = AddExtension(basename,'sps')

      CASE( HDB_DGNFILE ) !Delete chemistry diagnostics and log files
        DO i = 1,999
          WRITE(ext,'(I3.3)')i
          filename = TRIM(basename)//"_p"//TRIM(ext)//".log"
          CALL DeleteProjectFile( filename,ibit,ReturnValue )
          filename = TRIM(basename)//"_p"//TRIM(ext)//".qlog"
          CALL DeleteProjectFile( filename,ibit,ReturnValue )
        END DO
        filename = AddExtension(basename,'dgn')

      CASE( HDB_AMRFILE ) !Delete ambient restart file
        filename = AddExtension(basename,'amr')

      CASE( HDB_ICDFILE ) !Delete ICD file
        filename = AddExtension(basename,'icd')

      CASE( HDB_CAUTION ) !Delete Caution log file
        filename = AddExtension(basename,'clog')

      CASE DEFAULT
    END SELECT

    IF( LEN_TRIM(filename) > 0 )CALL DeleteProjectFile( filename,ibit,ReturnValue )

  END IF

END DO

!==== Return

DeleteProject = ReturnValue

9999 CONTINUE

irv = SCIMgrSetState( currentState )

CALL reset_messaging()

RETURN
END

!==============================================================================

SUBROUTINE DeleteProjectFile( filename,ibit,ReturnValue )

USE SCIPresults_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN    ) :: filename
INTEGER,      INTENT( IN    ) :: ibit
INTEGER,      INTENT( INOUT ) :: ReturnValue

INTEGER irv
LOGICAL lexist

INTEGER, EXTERNAL :: sysDeleteFile

INQUIRE(FILE=filename,EXIST=lexist)
IF( lexist )THEN
  irv = sysDeleteFile( filename )
  IF( irv == SCIPfailure )THEN
    ReturnValue = IBCLR(ReturnValue,iBit)
  ELSE
    ReturnValue = IBSET(ReturnValue,iBit)
  END IF
ELSE
  ReturnValue = IBSET(ReturnValue,iBit)
END IF

RETURN
END
