
!===============================================================================

SUBROUTINE read_material_fileX( mat,filenam,matl,id )

USE tooluser_fd
USE errorParam_fd
USE pcscipuf_fi
USE GUImatl_fi
USE create_fi
USE dialog_fi
USE GUItool_fi

IMPLICIT NONE

CHARACTER(*) filenam
CHARACTER(*) matl
INTEGER     id

INTEGER irv,i,nmatl_start

TYPE( matdef_str ) mat

LOGICAL lok,lexist,CheckFile_NoError
CHARACTER(PATH_MAXLENGTH) AddExtension

TYPE( ProjectStructure ) prjdlg

INTEGER nError,nMtl
CHARACTER(128) eMessage,eInform,eAction,eRoutine
TYPE( fileNameT ) fileT

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'Read_Material_FileX'

!---- Check to see if already have material definition

CALL find_material_list(mat%material,mat%nmatl,matl,id)

!------ Yes - Do Nothing for now

IF( id > 0 )THEN
  GOTO 9999
ELSE

!------ No - See if file exists

  IF( LEN(TRIM(filenam)) > 0 )THEN
    lexist = CheckFile_NoError(filenam)
  ELSE
    lexist = .FALSE.
  END IF

!------ Not Exist - report error

  IF( .NOT.lexist )THEN
    nError = OP_ERROR
    eMessage = 'Unable to find Material file'
    CALL ReportFileName( eInform,'File=',filenam )
    GOTO 9998
  ELSE

!------ Exists - read file for specific material

!==== Count materials and allocate space

    fileT%string = filenam
    irv = SCIPCountMaterial(ToolCallerID,fileT,nmtl)
    CALL AllocateMtlList(nmtl)

    nmatl_start = mat%nmatl + 1
    CALL SplitName(filenam,string1,prjdlg%ID%path)
    CALL SplitExtension(string1,prjdlg%ID%name,matdef%control%fileExtension)
    matdef%control%mode = SCIPnull
    matdef%control%mode = IBSET(matdef%control%mode,HCB_FILE)
    matdef%control%mode = IBSET(matdef%control%mode,HCB_SEARCH)
    matdef%control%searchName = TRIM(matl)
    CALL GUI_SCIP_material( prjdlg,mat,matdef,mtlList,SIZE(mtlList) )
    irv = SCIPLoadmaterialF( ToolCallerID,matdef,mtlList )
    lok = irv == SCIPsuccess
    IF( lok )THEN
      CALL SCIP_GUI_material( mat,matdef,mtlList )
      id_matl = mat%nmatl
      DO i = nmatl_start+1,mat%nmatl
        IF( mat%material(i)%file /= ' ' )THEN
          CALL RemoveExtension(mat%material(i)%file)
          mat%material(i)%file = AddExtension( mat%material(i)%file,'mtl' )
          IF( mat%material(i)%path == ' ' )THEN
            mat%material(i)%path = TRIM(prjdlg%ID%path)
          END IF
        END IF
        CALL set_conc_min( mat%material(i) )
      END DO
    END IF

!-------- Check Read Status - error

    IF( .NOT.lok )THEN
      nError = RD_ERROR
      eMessage = 'Unable to read Material from file'
      CALL ReportFileName( eInform,'File=',filenam )
      GOTO 9998

!-------- Check Read Status - Success

    ELSE

!---------- Check to make sure material is now in the list

      CALL find_material_list(mat%material, &
                        mat%nmatl, &
                        matl,id)

!---------- Still not in list - Report error

      IF( id <= 0 )THEN
        nError = IV_ERROR
        eMessage = 'Unable to find material on Material file'
        eInform  = 'Material='//TRIM(matl)
        GOTO 9998
      END IF
    END IF
  END IF
END IF

9999  CONTINUE

CALL DeallocateMtlList()

RETURN

9998 CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9999

END

