!*******************************************************************************
!                create_project
!*******************************************************************************
SUBROUTINE create_project( iwnd_db )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE create_fi
USE GUImatl_fi
USE defineok_fd
USE GUItool_fi
USE SwinAPI

!     Create Input files for a NEW SCIPUFF run and run SCIPUFF

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !window handle

INTEGER irv

LOGICAL, DIMENSION(3) :: ldelete

CHARACTER(PATH_MAXLENGTH) filename, dir

TYPE( ProjectStructure ) prjdlg

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull,StripNull
LOGICAL, EXTERNAL :: CheckFile_NoError

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CreateProject'

!==== Check input data

CALL check_scipuff_input( iwnd_db )
IF( hasError() )GOTO 9999

!---- Material file

ldelete(1) = .TRUE.
lcreate    = .TRUE.

!---- Release file

ldelete(2) = .TRUE.

!---- Meteorology file

ldelete(3) = .TRUE.

!==== Set Current Directory

dir = AddNull( project(BASE_LEVEL)%ID%path )

IF( SetCurrentDirectory(dir) == FALSE )THEN
  nError   = API_ERROR
  eMessage = 'Unable to set directory'
  eInform  = TRIM(dir)
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9000
END IF

!==== SCIP Tool

prjdlg = project(BASE_LEVEL)
CALL GUI_SCIP_time( prjdlg,dlgTime(BASE_LEVEL),time )
CALL GUI_SCIP_flags( prjdlg,flags )
CALL GUI_SCIP_domain( prjdlg,dlgDomain(BASE_LEVEL),domain )
CALL GUI_SCIP_options( prjdlg,dlgOptions(BASE_LEVEL),prjOptions )

CALL AllocateMtlList(materials(BASE_LEVEL)%nmatl)
IF( hasError() )GOTO 9000

IF( project(BASE_LEVEL)%Restart )THEN
  CALL GUI_SCIP_ctrl( prjdlg,ctrl )
  createRst%project       = domain%project
  createRst%input%ctrl    = ctrl%ctrl
  createRst%input%end     = time%time%end
  createRst%input%audit   = flags%flags%audit
  createRst%input%domain  = domain%spatial
  createRst%input%option  = prjOptions%option
  CALL GUI_SCIP_material( prjdlg,materials(BASE_LEVEL),matdef,mtlList,SIZE(mtlList) ) !done just to make RAD file stuff easier in run_scipuff
ELSE
  CALL GUI_SCIP_material( prjdlg,materials(BASE_LEVEL),matdef,mtlList,SIZE(mtlList) )
  createNew%project       = domain%project
  createNew%input%time    = time%time
  createNew%input%flags   = flags%flags
  createNew%input%domain  = domain%spatial
  createNew%input%option  = prjOptions%option
  createNew%input%mtlHead = matdef%mtlHead
END IF
IF( hasError() )GOTO 9000

!==== Create .SCN file

IF( ldelete(2) )THEN
  IF( .NOT.project(BASE_LEVEL)%Restart )THEN
    CALL SizeRelList( scenario(BASE_LEVEL) )
    IF( hasError() )GOTO 9000
    CALL GUI_SCIP_scenario( project(BASE_LEVEL),scenario(BASE_LEVEL), &
                            materials(BASE_LEVEL),reldef,relList,SIZE(relList),relMCList,SIZE(relMCList) )
    IF( hasError() )GOTO 9000
    createNew%scnHead = reldef%scnHead
  END IF
ELSE
  createNew%scnHead%max    = -1
  createNew%scnHead%number = 0
END IF

!==== Create .MSC file

IF( ldelete(3) )THEN

  CALL GUI_SCIP_met( project(BASE_LEVEL),metdef(BASE_LEVEL),weather )
  IF( project(BASE_LEVEL)%Restart )THEN
    createRst%weather = weather%weather
  ELSE
    createNew%weather = weather%weather
  END IF

END IF

!=====Create Project

project(BASE_LEVEL)%OK   = .FALSE.
project(BASE_LEVEL)%Edit = .TRUE.
CALL run_scipuff( iwnd_db,.TRUE.,file_inp )

9999 CONTINUE

IF( hasError() )THEN
  CALL ShowErrorMessage( iwnd_db )
  lok_create = .FALSE.
  CALL PushButton( hwnd_tb,IDB_EDTPRJ,IDB_EDTPRJ,irv )

  lok_create = (DefinedOK == DF_ALL .OR. DefinedOK == DF_OK)
  lok_create = lok_create .AND. project(1)%Edit

  CALL ShowControl( hwnd_db,IDB_BUTTON12,SW_SHOWNORMAL )
  CALL EnableControlL( hwnd_db,IDB_BUTTON12,lok_create )
  IF( project(BASE_LEVEL)%OK )THEN
    CALL SetControlText( hwnd_db,ID_CANCEL,'&Cancel Changes' )
  END IF
END IF

RETURN

9000  CONTINUE
CALL AddErrorAction('Create SCIPUFF Project Terminated')
GOTO 9999

9100  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to delete existing SCIPUFF file'
eInform  = TRIM(StripNull(TRIM(filename)))
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

9200  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to open SCIPUFF input file'
eInform  = TRIM(filename)
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

9300  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to close SCIPUFF input file'
eInform  = TRIM(filename)
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

END
!*******************************************************************************
!                create_restart
!*******************************************************************************
SUBROUTINE create_restart( iwnd_db )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE SwinAPI

!     Create Input files to restart a SCIPUFF run and run SCIPUFF

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Window handle

CHARACTER(PATH_MAXLENGTH) filename, dir, file_sav
INTEGER nch, irv

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'RestartProject'

!==== Change name of .INP file to .RST

file_sav = TRIM(file_inp)
nch      = LEN(TRIM(file_inp))
file_inp(nch-2:) = 'rst'

!==== Set Current Directory

dir = AddNull( TRIM(project(BASE_LEVEL)%ID%path) )

IF( SetCurrentDirectory(dir) == FALSE )THEN
  nError   = API_ERROR
  eMessage = 'Unable to set directory'
  eInform  = TRIM(dir)
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9000
END IF

!==== Create .RST file

!=====Run SCIPUFF

CALL run_scipuff( iwnd_db,.FALSE.,file_sav )

9999  CONTINUE

!==== Check Errors

IF( hasError() )THEN
  CALL ShowErrorMessage( iwnd_db )
  CALL PushButton( hwnd_tb,IDB_EDTPRJ,IDB_EDTPRJ,irv )
END IF

RETURN

9000  CONTINUE
CALL AddErrorAction('Create SCIPUFF Project Terminated')
GOTO 9999

9100  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to delete existing SCIPUFF file'
eInform  = TRIM(filename)
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

9200  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to open SCIPUFF input file'
eInform  = TRIM(filename)
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

9300  CONTINUE
nError   = OP_ERROR
eMessage = 'Unable to close SCIPUFF input file'
eInform  = TRIM(filename)
CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
GOTO 9000

END
!*******************************************************************************
!                check_scipuff_input
!*******************************************************************************
SUBROUTINE check_scipuff_input( iwnd_db )

USE resource_fd
USE mettype_fd
USE reldef_fd
USE tooluser_fd
USE errorParam_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE create_fi
USE GUImatl_fi
USE mettype_fd
USE script_fi
USE relparam_fd
USE GUItool_fi
USE UtilMtlAux

!     check input data for a NEW SCIPUFF run

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !window handle

REAL  tz,tzc,xmid

LOGICAL need_tzone,local
LOGICAL lsv_warn,def
LOGICAL, EXTERNAL :: IsGas,IsParticle
CHARACTER(PATH_MAXLENGTH) filenam
CHARACTER(16)  missmatl,matl1,matl2

INTEGER itz,imz,nz_check,i,j,k,nfmatl,jhit,id,ios
INTEGER indx_max,nsg,max_vres
INTEGER,DIMENSION(:),ALLOCATABLE :: hit
CHARACTER(16),DIMENSION(:),ALLOCATABLE ::  filematl

TYPE( liquid_material ) pmatliq

LOGICAL, EXTERNAL :: IsLiquid, IsWetParticle
LOGICAL, EXTERNAL :: IsAerosol

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError,CheckFile_NoError
INTEGER, EXTERNAL :: lastError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'CheckScipuffInput'

!==== Check delmin

IF( .NOT.BTEST(project(BASE_LEVEL)%Mode,FAST_MODE) )THEN
  IF( dlgOptions(BASE_LEVEL)%delMin == 0.0 )dlgOptions(BASE_LEVEL)%delMin = DEF_VAL_R
END IF

!==== Check time zone

local = (dlgTime(BASE_LEVEL)%time%start%time%reference == HT_LOCAL)
need_tzone = local /= metdef(BASE_LEVEL)%local
need_tzone = need_tzone .OR. &
            (metdef(BASE_LEVEL)%bl == BL_SIMPLE .AND. .NOT.local)
need_tzone = need_tzone .OR. &
            (metdef(BASE_LEVEL)%bl == BL_CALC .AND. local)
need_tzone = need_tzone .OR. &
            (metdef(BASE_LEVEL)%bl == BL_OPERATIONAL .AND. local)

IF( need_tzone )THEN

  IF( ((dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_CARTESIAN).OR. &
       (dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_METERS) .OR. &
       (dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_UTM)) .AND. &
        dlgDomain(BASE_LEVEL)%spatial%reference%lon /= NOT_SET_R )THEN

    tzc = FLOAT(INT( dlgDomain(BASE_LEVEL)%spatial%reference%lon+7.5)/15)
    IF( dlgDomain(BASE_LEVEL)%spatial%reference%lon < -7.5 )tzc = tzc - 1.

  ELSE IF( dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_LATLON .AND. &
           dlgDomain(BASE_LEVEL)%spatial%domain%xMin /= DEF_VAL_R .AND. &
           dlgDomain(BASE_LEVEL)%spatial%domain%xMax /= DEF_VAL_R )THEN

    xmid  = 0.5*( dlgDomain(BASE_LEVEL)%spatial%domain%xMin+ dlgDomain(BASE_LEVEL)%spatial%domain%xMax)
    tzc = FLOAT(INT(xmid+7.5)/15)
    IF( xmid < -7.5 )tzc = tzc - 1.

  ELSE

    tzc = NOT_SET_R

  END IF

  tz = dlgTime(BASE_LEVEL)%time%start%zone
  IF( tz /= DEF_VAL_R .AND. tz /= NOT_SET_R )CALL fix_tzone( tz )

  IF( tz == NOT_SET_R .OR. tz == DEF_VAL_R )THEN

    IF( tzc == NOT_SET_R )THEN
      nError   = IV_ERROR
      eMessage = 'The time zone is needed but not set'
      eInform  = 'and cannot be inferred from the domain data'
      eAction  = 'Please set the time zone in the TIME dialog'
      CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
      GOTO 9999
    ELSE
      dlgTime(BASE_LEVEL)%time%start%zone = tzc
    END IF

  ELSE

    IF( tzc /= NOT_SET_R )THEN
      IF( ABS(tzc-tz) > 1./60. )THEN
        nError   = WN_ERROR
        eMessage = 'The time zone is inconsistent with the spatial domain'
        IF( tzc < 0. )tzc = tzc + 24
        itz = INT(tzc)
        imz = NINT(60.*(tzc - FLOAT(INT(tzc))))
        WRITE(eInform,'(A,I2,'':'',I2.2)')'Expected time zone = ',itz,imz
        eAction  = 'Do you want to continue anyway?'
        CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
        CALL ShowWarningMessage( iwnd_db,.TRUE. )
        eRoutine = 'CheckScipuffInput'
        IF( hasError() )THEN
          CALL AddErrorAction('Please correct the time zone in the TIME dialog')
          GOTO 9999
        END IF
      END IF
    END IF

  END IF

END IF

!==== Check domain

IF( dlgDomain(BASE_LEVEL)%spatial%domain%xMin /= DEF_VAL_R .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%xMax /= DEF_VAL_R )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%xMax <= dlgDomain(BASE_LEVEL)%spatial%domain%xMin )THEN
    nError   = IV_ERROR
    eMessage = 'Invalid horizontal domain'
    eInform  = 'The maximum value must be > the minimum value'
    eAction  = 'Please correct the domain in the DOMAIN dialog'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF
END IF

IF( dlgDomain(BASE_LEVEL)%spatial%domain%yMin /= DEF_VAL_R .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%yMax /= DEF_VAL_R )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%yMax <= dlgDomain(BASE_LEVEL)%spatial%domain%yMin )THEN
    nError   = IV_ERROR
    eMessage = 'Invalid horizontal domain'
    eInform  = 'The maximum value must be > the minimum value'
    eAction  = 'Please correct the domain in the DOMAIN dialog'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF
END IF

IF( dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_UTM )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%zoneUTM == NOT_SET_I )THEN
    nError = IV_ERROR
    eMessage = 'Invalid UTM refernce zone'
    eInform  = 'Must set a reference zone for UTM runs'
    eAction  = 'Please set the zone in the DOMAIN dialog'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF
END IF

CALL set_utm_reference( BASE_LEVEL )
IF( hasError() )GOTO 9999

IF( dlgDomain(BASE_LEVEL)%spatial%domain%vRes /= DEF_VAL_R )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%zMax < dlgDomain(BASE_LEVEL)%spatial%domain%vRes )THEN
    nError = IV_ERROR
    eMessage = 'Invalid vertical domain or resolution'
    eInform  = 'The vertical domain must be > vertical resolution'
    eAction  = 'Please correct the domain/resolution in the DOMAIN dialog'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF

  nz_check = NINT( dlgDomain(BASE_LEVEL)%spatial%domain%zMax/dlgDomain(BASE_LEVEL)%spatial%domain%vRes )
  IF( nz_check > max_vres() )THEN
    nError   = IV_ERROR
    eMessage = 'Invalid vertical domain or resolution'
    WRITE(string3,*)max_vres()
    string3 = ADJUSTL(string3)
    eInform = 'The vertical resolution must be > (vertical domain)/' &
                                             //TRIM(string3)
    eAction = 'Please correct the domain/resolution in the DOMAIN dialog'
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    GOTO 9999
  END IF
END IF

!==== Check Domain/Duration for LSV=Operational Warning

IF( dlgDomain(BASE_LEVEL)%spatial%domain%xMin /= DEF_VAL_R .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%xMax /= DEF_VAL_R .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%yMin /= DEF_VAL_R .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%yMax /= DEF_VAL_R )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%coord == I_LATLON )THEN
    lsv_warn = (dlgDomain(BASE_LEVEL)%spatial%domain%xMax - dlgDomain(BASE_LEVEL)%spatial%domain%xMin > 1.0) .OR. &
               (dlgDomain(BASE_LEVEL)%spatial%domain%yMax - dlgDomain(BASE_LEVEL)%spatial%domain%yMin > 1.0)
  ELSE
    lsv_warn = (dlgDomain(BASE_LEVEL)%spatial%domain%xMax - dlgDomain(BASE_LEVEL)%spatial%domain%xMin > 100.0) .OR. &
               (dlgDomain(BASE_LEVEL)%spatial%domain%yMax - dlgDomain(BASE_LEVEL)%spatial%domain%yMin > 100.0)
  END IF
  lsv_warn = lsv_warn .AND. metdef(BASE_LEVEL)%lsv == LSV_NONE .AND. &
             dlgTime(BASE_LEVEL)%time%end%time%runTime > 6.0
  IF( lsv_warn )THEN
    nError   = WN_ERROR
    eMessage = 'This project''s domain and/or duration suggests that'
    eInform  = 'large scale variability should be included'
    eAction  = 'Do you want to switch to LSV=Operational?'
    def = .FALSE.
    CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
    CALL ShowWarningMessage( iwnd_db,def ) !else it is FALSE
    eRoutine = 'CheckScipuffInput'
    IF( .NOT.hasError() )THEN
      metdef(BASE_LEVEL)%lsv = LSV_OPERATIONAL
    ELSE
      IF( BTEST(pcscipuf_mode,IPMB_SCRIPT) )THEN
        SELECT CASE( metdef(BASE_LEVEL)%lsv )
          CASE( LSV_NONE )
            eAction  = 'No large-scale variability will be included'
          CASE DEFAULT
            eAction  = 'LSV not changed'
        END SELECT
        CALL AddErrorAction(eAction)
        CALL ShowInfoMessage( iwnd_db )
      ELSE
        CALL InitError()
      END IF
      eRoutine = 'CheckScipuffInput'
    END IF
  END IF
END IF

!==== Check Material units
CALL check_units( materials(BASE_LEVEL)%nmatl,materials(BASE_LEVEL)%material, &
                  materials(BASE_LEVEL)%mat_aux )
IF( hasError() )THEN
  IF( lastError() /= UK_ERROR )THEN
    CALL AddErrorAction('Please modify the material release units')
  END IF
  GOTO 9999
END IF

!==== Check viscosity
DO i = 1,materials(BASE_LEVEL)%nmatl
  IF( IsLiquid(materials(BASE_LEVEL)%material(i)%icls) )THEN
    CALL GetLiquidParam( pmatliq,materials(BASE_LEVEL)%material(i)%iaux, &
                         2,materials(BASE_LEVEL)%mat_aux )
    IF( pmatliq%viscosity == NOT_SET_R )THEN
      IF( dlgOptions(BASE_LEVEL)%substrate > 0 )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid material parameters'
        eInform  = TRIM(materials(BASE_LEVEL)%material(i)%cmat)//' viscosity must be set for substrates other than impermeable'
        eAction  = 'Please correct the material description or reset the substrate'
        CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
        GOTO 9999
      END IF
    ELSE IF( pmatliq%viscosity == DEF_VAL_R )THEN
      nError   = IV_ERROR
      eMessage = 'Invalid material parameters'
      eInform  = 'No default value for '//TRIM(materials(BASE_LEVEL)%material(i)%cmat)//' viscosity'
      eAction  = 'Please correct the material description'
      CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
      GOTO 9999
    ELSE IF( pmatliq%viscosity <= 0 )THEN
      nError   = IV_ERROR
      eMessage = 'Invalid material parameters'
      eInform  = TRIM(materials(BASE_LEVEL)%material(i)%cmat)//' viscosity must be positive'
      eAction  = 'Please correct the material description'
      CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
      GOTO 9999
    END IF
  END IF
END DO

!==== Check materials if not copying Release scenario file

ALLOCATE( filematl(MAXMTYP),STAT=ios )
IF( ios /= 0 )THEN
  nError   = SZ_ERROR
  eMessage = 'Allocation error : filematl '
  WRITE(eInform,*)'Request=',MAXMTYP,' : ErrorCode=',ios
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9999
END IF
ALLOCATE( hit(MAXMTYP+1),STAT=ios )
IF( ios /= 0 )THEN
  nError   = SZ_ERROR
  eMessage = 'Allocation error : hit '
  WRITE(eInform,*)'Request=',MAXMTYP+1,' : ErrorCode=',ios
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9999
END IF
DO i = 1,materials(BASE_LEVEL)%nmatl+1
  hit(i) = 0
END DO
DO i = 1,scenario(BASE_LEVEL)%nrel
    matl1 =  TRIM(scenario(BASE_LEVEL)%release(i)%matl)
    CALL cupper( matl1 )
    IF( scenario(BASE_LEVEL)%release(i)%spec == REL_DATA )THEN
      jhit = materials(BASE_LEVEL)%nmatl + 1
      DO j = 1,materials(BASE_LEVEL)%nmatl
        matl2 = TRIM(materials(BASE_LEVEL)%material(j)%cmat)
        CALL cupper( matl2 )
        IF( TRIM(matl2) == TRIM(matl1) )jhit = j
      END DO
      hit(jhit) = hit(jhit) + 1
      IF( jhit == materials(BASE_LEVEL)%nmatl + 1 )THEN
        missmatl = scenario(BASE_LEVEL)%release(i)%matl
      END IF
    ELSE
      filenam = TRIM(scenario(BASE_LEVEL)%release(i)%file)
      CALL AddPath( filenam,TRIM(scenario(BASE_LEVEL)%release(i)%path) )
      IF( CheckFile_NoError(filenam) )THEN
        CALL get_cldtrans_matl( filenam,MAXMTYP,nfmatl,filematl )
        IF( hasError() .OR. nfmatl == 0 )THEN
          nfmatl = materials(BASE_LEVEL)%nmatl
          DO j = 1,materials(BASE_LEVEL)%nmatl
            filematl(j) = materials(BASE_LEVEL)%material(j)%cmat
          END DO
        END IF
      ELSE
        IF( scenario(BASE_LEVEL)%release(i)%matl /= ' ' )THEN
          nfmatl = 1
          filematl(1) = scenario(BASE_LEVEL)%release(i)%matl
        ELSE
          nfmatl = materials(BASE_LEVEL)%nmatl
          DO j = 1,materials(BASE_LEVEL)%nmatl
            filematl(j) = materials(BASE_LEVEL)%material(j)%cmat
          END DO
        END IF
      END IF
      DO k = 1,nfmatl
        jhit = materials(BASE_LEVEL)%nmatl + 1
        matl1 = TRIM(filematl(k))
        CALL cupper( matl1 )
        DO j = 1,materials(BASE_LEVEL)%nmatl
          matl2 = TRIM(materials(BASE_LEVEL)%material(j)%cmat)
          CALL cupper( matl2 )
          IF( TRIM(matl2) == TRIM(matl1) )jhit = j
        END DO
        hit(jhit) = hit(jhit) + 1
        IF( jhit == materials(BASE_LEVEL)%nmatl + 1 )THEN
          missmatl = matl1     !scenario(BASE_LEVEL)%release(i)%matl
        END IF
      END DO
    END IF
END DO
IF( hit(materials(BASE_LEVEL)%nmatl + 1) > 0 )THEN
  nError   = UK_ERROR
  eMessage = 'Release material not found in list'
  eInform  = 'Material='//TRIM(missmatl)
  eAction  = 'Please define the material'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  GOTO 9999
ELSE
  nfmatl = materials(BASE_LEVEL)%nmatl
  DO i = nfmatl,1,-1
    IF( hit(i) == 0 )THEN
      id = i
      CALL delete_current_material( 0,id )
    END IF
  END DO
END IF

!==== Check release indx/distrib

DO i = 1,scenario(BASE_LEVEL)%nrel
    IF( scenario(BASE_LEVEL)%release(i)%spec == REL_DATA )THEN
      CALL find_material_list( materials(BASE_LEVEL)%material, &
                               materials(BASE_LEVEL)%nmatl, &
                               scenario(BASE_LEVEL)%release(i)%matl,id )
      IF( id <= 0 )THEN
        nError   = IV_ERROR
        eMessage = 'Invalid release material'
        eInform  = 'Release material ('//TRIM(scenario(BASE_LEVEL)%release(i)%matl)// &
                   ' not found in material list'
        eAction  = 'Please correct the release description'
        CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
        GOTO 9999
      ELSE
        IF( .NOT.IsGas(materials(BASE_LEVEL)%material(id)%icls) .AND. &
            .NOT.IsAerosol(materials(BASE_LEVEL)%material(id)%icls) )THEN
          IF( IsParticle(materials(BASE_LEVEL)%material(id)%icls) )THEN
            nsg = GetSubgroups( materials(BASE_LEVEL)%material(id),materials(BASE_LEVEL)%mat_aux)
            indx_max = nsg + MAXDISTRIBUTION
          ELSE IF( IsWetParticle(materials(BASE_LEVEL)%material(id)%icls) )THEN
            nsg = GetSubgroups(materials(BASE_LEVEL)%material(id),materials(BASE_LEVEL)%mat_aux)
            indx_max = nsg + MAXDISTRIBUTION
            IF( scenario(BASE_LEVEL)%release(i)%type(1:2) == 'C ' .OR. &
                scenario(BASE_LEVEL)%release(i)%type(1:2) == 'CM' .OR. &
                scenario(BASE_LEVEL)%release(i)%type(1:2) == 'CS' .OR. &
                scenario(BASE_LEVEL)%release(i)%type(1:2) == 'I ' )THEN
              indx_max = indx_max + MAXSPECIALWETP
            END IF
          ELSE
            nsg = GetSubgroups(materials(BASE_LEVEL)%material(id),materials(BASE_LEVEL)%mat_aux)
            IF( scenario(BASE_LEVEL)%release(i)%type(1:2) == 'CP' )THEN
              indx_max = nsg + MAXDISTRIBUTION + REL_LIQUIDPOOL
            ELSE
              indx_max = nsg + MAXDISTRIBUTION + REL_GASPHASE
            END IF
          END IF
          IF( scenario(BASE_LEVEL)%release(i)%indx < 1 .OR. &
              scenario(BASE_LEVEL)%release(i)%indx > indx_max )THEN
            nError   = IV_ERROR
            eMessage = 'Invalid release material subgroup/phase'
            eInform  = 'Release material subgroup pointer out of range'
            eAction  = 'Please correct the release description'
            CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
            GOTO 9999
          END IF
          IF( scenario(BASE_LEVEL)%release(i)%indx == nsg + MAXDISTRIBUTION )THEN
            IF( scenario(BASE_LEVEL)%release(i)%param(REL_MMD_INDX) == NOT_SET_R .OR. &
                scenario(BASE_LEVEL)%release(i)%param(REL_SIGMA_INDX) == NOT_SET_R .OR. &
                scenario(BASE_LEVEL)%release(i)%param(REL_MMD_INDX) == DEF_VAL_R .OR. &
                scenario(BASE_LEVEL)%release(i)%param(REL_SIGMA_INDX) == DEF_VAL_R )THEN
              nError   = IV_ERROR
              eMessage = 'Invalid release parameters'
              eInform  = 'LogNormal distribution parameters out of range'
              eAction  = 'Please correct the release description'
              CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
              GOTO 9999
            END IF
          END IF
        END IF
      END IF
    END IF
END DO

9999  CONTINUE
IF( ALLOCATED(filematl) )DEALLOCATE( filematl,STAT=ios )
IF( ALLOCATED(hit)      )DEALLOCATE( hit,STAT=ios )

RETURN
END
