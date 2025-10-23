!***********************************************************************
!               GUI_SCIP_scenario
!***********************************************************************
SUBROUTINE GUI_SCIP_scenario( prjdlg,scndlg,matdlg,tool,relList,mxRel,relMCList,nMC )

USE status_fd
USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE GUImatl_fd
USE default_fd

IMPLICIT NONE

TYPE( preleaseT ) tool
TYPE( releaseT  ) relList(*)
TYPE( reldef_str ) scndlg
TYPE( matdef_str ) matdlg
TYPE( ProjectStructure ) prjdlg
INTEGER mxRel
INTEGER nMC
TYPE( releaseMCT  ) relMCList(*)

INTEGER i
INTEGER GUIgroup
INTEGER id
INTEGER imc, j
REAL    GUItdur
REAL    GUIzrel
INTEGER irv

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsLiquid,IsWetParticle
LOGICAL, EXTERNAL :: IsMulti
INTEGER, EXTERNAL :: GUI_SCIP_Stack

!==== Project id member

tool%project = prjdlg%ID

!==== Material List header

tool%scnHead%max    = mxRel
tool%scnHead%number = scndlg%nrel

!==== Material List

imc = 0
DO i = 1,scndlg%nrel

!====== Basic parameters

  relList(i)%status = STATUS_VALID

  relList(i)%xRel = scndlg%release(i)%xRel
  relList(i)%yRel = scndlg%release(i)%yRel
  relList(i)%zRel = scndlg%release(i)%zRel

  relList(i)%tRel = scndlg%release(i)%time
  relList(i)%notUsed = 1.0

  relList(i)%material = TRIM(scndlg%release(i)%matl)

  IF( scndlg%release(i)%defName )THEN
    relList(i)%relDisplay = ' '
  ELSE
    relList(i)%relDisplay = scndlg%release(i)%string(7:)
  END IF
  relList(i)%relName = relList(i)%relDisplay

!====== Modify release parameters for liquid material vapor-phase and pool release

  IF( scndlg%release(i)%matl /= ' ' .AND. TRIM(scndlg%release(i)%type) /= 'X' )THEN
    CALL find_material_list( matdlg%material,matdlg%nmatl,scndlg%release(i)%matl,id )
    IF( id <= 0 .OR. id > matdlg%nmatl )THEN
      CALL SetError( IV_ERROR, &
                    'Invalid material specification', &
                    'Material not found M='//TRIM(scndlg%release(i)%matl),' ', &
                    'GUI->SCIP scenario' )
      GOTO 9999
    END IF
    IF( IsWetParticle(matdlg%material(id)%icls) )THEN
      IF( scndlg%release(i)%distrib == MAXDISTRIBUTION + REL_SLURRY )THEN
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = -1
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      ELSE IF( scndlg%release(i)%distrib == MAXDISTRIBUTION )THEN
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = -1
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      ELSE
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = MAX(1,scndlg%release(i)%indx)
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      END IF
    ELSE IF( IsParticle(matdlg%material(id)%icls) )THEN
      IF( scndlg%release(i)%distrib == MAXDISTRIBUTION )THEN
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = -1
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      ELSE
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = MAX(1,scndlg%release(i)%indx)
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      END IF
    ELSE IF( IsLiquid(matdlg%material(id)%icls) )THEN
      IF( scndlg%release(i)%distrib == MAXDISTRIBUTION + REL_GASPHASE )THEN
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = 0
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      ELSE IF( scndlg%release(i)%distrib == MAXDISTRIBUTION + REL_LIQUIDPOOL )THEN
        GUIgroup = scndlg%release(i)%indx
        GUItdur  = scndlg%release(i)%dur  !tdur and zrel set more
        GUIzrel  = scndlg%release(i)%zRel !as a precaution than a
        scndlg%release(i)%indx = 0        !necessity (I think)
        scndlg%release(i)%dur  = DEF_VAL_R
        scndlg%release(i)%zRel = NOT_SET_R
      ELSE IF( scndlg%release(i)%distrib == MAXDISTRIBUTION )THEN
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = -1
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      ELSE
        GUIgroup = scndlg%release(i)%indx
        scndlg%release(i)%indx = MAX(1,scndlg%release(i)%indx)
        GUItdur  = NOT_SET_R
        GUIzrel  = NOT_SET_R
      END IF
    ELSE
      GUIgroup = scndlg%release(i)%indx
      scndlg%release(i)%indx = MAX(1,scndlg%release(i)%indx)
      GUItdur  = NOT_SET_R
      GUIzrel  = NOT_SET_R
    END IF
  ELSE
    GUIgroup = NOT_SET_I
  END IF

!====== Type specific parameters

  SELECT CASE( TRIM(scndlg%release(i)%type) )
    CASE( 'I' )
      IF( scndlg%release(i)%file == ' ' )THEN
        relList(i)%type = HR_INST
        CALL GUI_SCIP_Inst( scndlg%release(i),relList(i)%relData )
      ELSE
        relList(i)%type = HR_FILE
        CALL GUI_SCIP_File( scndlg%release(i),relList(i)%relData )
      END IF
    CASE( 'IF' )
      relList(i)%type = HR_FILE
      CALL GUI_SCIP_File( scndlg%release(i),relList(i)%relData )
    CASE( 'IA' )
      relList(i)%type = HR_FINST
      CALL GUI_SCIP_Flash( scndlg%release(i),relList(i)%relData )
    CASE( 'C' )
      relList(i)%type = HR_CONT
      CALL GUI_SCIP_Cont( scndlg%release(i),relList(i)%relData )
    CASE( 'CM' )
      relList(i)%type = HR_MOVE
      CALL GUI_SCIP_Move( scndlg%release(i),relList(i)%relData )
    CASE( 'CF' )
      relList(i)%type = HR_CONTF
      CALL GUI_SCIP_ContFile( scndlg%release(i),relList(i)%relData )
    CASE( 'CS' )   !May be HR_STACK or HR_STACK3
      relList(i)%type = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CSF' )   !May be HR_STACK or HR_STACK3
      relList(i)%type = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CA' )
      relList(i)%type = HR_AREA
      irv = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CAF' )
      relList(i)%type = HR_AREAF
      irv = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CSP' )
      relList(i)%type = HR_PRIME
      irv = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CSFP' )
      relList(i)%type = HR_STACKFP
      irv = GUI_SCIP_Stack( scndlg%release(i),relList(i)%relData )
    CASE( 'CP ')
      relList(i)%type = HR_POOL
      CALL GUI_SCIP_Pool( scndlg%release(i),relList(i)%relData )
    CASE DEFAULT
      WRITE(string1,'(A,I3)')'type =',relList(i)%type
      CALL SetError( IV_ERROR, &
                    'Invalid release type', &
                     string1,' ', &
                    'GUI_SCIP_Release' )
      GOTO 9999
  END SELECT

!====== Return release parameters to original values if necessary

  IF( GUIgroup /= NOT_SET_I )THEN
    scndlg%release(i)%indx = GUIgroup
    IF( GUItdur /= NOT_SET_R )THEN
      scndlg%release(i)%dur  = GUItdur
      scndlg%release(i)%zRel = GUIzrel
    END IF
  END IF

  IF( GUIgroup /= NOT_SET_I )THEN
    IF( IsMulti(matdlg%material(id)%icls) )THEN
      IF( ASSOCIATED(scndlg%release(i)%mc) )THEN
        DO j = 1,SIZE(scndlg%release(i)%mc)
          IF( scndlg%release(i)%mc(j) /= 0.0 )THEN
            imc = imc + 1
            IF( imc <= nMC )THEN
              relMCList(imc).relID = i
              relMCList(imc).MCname = matdlg%materialMC(id)%name(j)
              relMCList(imc).MCmass = scndlg%release(i)%mc(j)
            ELSE
              CALL SetError( IV_ERROR, &
                            'Invalid release set', &
                             'Releases contain too many multicomponent values',' ', &
                            'GUI_SCIP_Release' )
              GOTO 9999
            END IF
          END IF
        END DO
      END IF
    ELSE
      IF( ASSOCIATED(scndlg%release(i)%mc) )THEN
        CALL SetError( IV_ERROR, &
                      'Invalid release', &
                       'Release contains multicomponent values but not a multicomponent material',' ', &
                      'GUI_SCIP_Release' )
        GOTO 9999
      END IF
    END IF
  END IF
END DO

9999 CONTINUE

RETURN
END
!***********************************************************************
!               SCIP_GUI_scenario
!***********************************************************************
SUBROUTINE SCIP_GUI_scenario( scndlg,matdlg,tool,relList,nMC,relMCList )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE create_fi
USE errorParam_fd
USE class_fd
USE GUImatl_fd
USE default_fd

IMPLICIT NONE

TYPE( preleaseT ) tool
TYPE( releaseT  ) relList(1)
TYPE( reldef_str ) scndlg
TYPE( matdef_str ) matdlg
INTEGER ios, id, jd
INTEGER nMC
TYPE( releaseMCT  ) relMCList(*)
LOGICAL, EXTERNAL :: IsMulti

INTEGER i, j
LOGICAL do_setIndex

CHARACTER(128), EXTERNAL :: StripNull

LOGICAL, EXTERNAL :: hasError

!==== Material structure header

scndlg%nrel = 0

!==== Loop over releases

IF( tool%scnHead%number > MAXREL )THEN
  CALL ReallocateScenario( tool%scnHead%number )
  IF( hasError() )GOTO 9999
END IF

DO i = 1,tool%scnHead%number

  scndlg%nrel = scndlg%nrel + 1

!====== Initialize

  scndlg%release(i) = scenario(DEFAULT_LEVEL)%release(1)

!====== Basic Parameters

  scndlg%release(i)%xRel = relList(i)%xRel
  scndlg%release(i)%yRel = relList(i)%yRel
  scndlg%release(i)%zRel = relList(i)%zRel

  scndlg%release(i)%time = relList(i)%tRel

  DO j = 1,MAXRELPARAM
    scndlg%release(i)%param(j) = NOT_SET_R
  END DO

  DO j = 1,NUM_DYNAMIC
    scndlg%release(i)%dynam(j) = 0.0
  END DO

  scndlg%release(i)%matl = StripNull( relList(i)%material )

  IF( LEN_TRIM(relList(i)%relDisplay) > 0 )THEN
    scndlg%release(i)%string(7:) = TRIM(relList(i)%relDisplay)
    scndlg%release(i)%defName = .FALSE.
  ELSE
    scndlg%release(i)%string = ' '
  END IF

!====== Type specific data

  scndlg%release(i)%spec = REL_DATA
  scndlg%release(i)%file = ' '
  scndlg%release(i)%path = ' '
  do_setIndex = .TRUE.
  SELECT CASE( relList(i)%type )
    CASE( HR_INST )
      scndlg%release(i)%type = 'I'
      CALL SCIP_GUI_Inst( relList(i)%relData,scndlg%release(i) )

    CASE( HR_XINST )
      scndlg%release(i)%type = 'I'
      CALL SCIP_GUI_XInst( relList(i)%relData,scndlg%release(i) )

    CASE( HR_XINST3 )
      scndlg%release(i)%type = 'I'
      CALL SCIP_GUI_XInst3( relList(i)%relData,scndlg%release(i) )

    CASE( HR_FILE )
      scndlg%release(i)%type = 'IF'
      scndlg%release(i)%spec = REL_FILE
      do_setIndex = .FALSE.
      CALL SCIP_GUI_File( relList(i)%relData,scndlg%release(i) )

    CASE( HR_FINST )
      scndlg%release(i)%type = 'IA'
      CALL SCIP_GUI_Flash( relList(i)%relData,scndlg%release(i) )

    CASE( HR_CONT )
      scndlg%release(i)%type = 'C'
      CALL SCIP_GUI_Cont( relList(i)%relData,scndlg%release(i) )

    CASE( HR_CONTF )
      scndlg%release(i)%type = 'CF'
      CALL SCIP_GUI_ContFile( relList(i)%relData,scndlg%release(i) )

    CASE( HR_STACKF, HR_STACK3F )
      scndlg%release(i)%type = 'CSF'
      CALL SCIP_GUI_StackFile( relList(i)%type,relList(i)%relData,scndlg%release(i) )

    CASE( HR_STACKFP )
      scndlg%release(i)%type = 'CSFP'
      CALL SCIP_GUI_StackFile( relList(i)%type,relList(i)%relData,scndlg%release(i) )

    CASE( HR_AREA )
      scndlg%release(i)%type = 'CA'
      CALL SCIP_GUI_Area( relList(i)%relData,scndlg%release(i) )

    CASE( HR_AREAF )
      scndlg%release(i)%type = 'CAF'
      CALL SCIP_GUI_AreaFile( relList(i)%relData,scndlg%release(i) )

    CASE( HR_MOVE )
      scndlg%release(i)%type = 'CM'
      CALL SCIP_GUI_Move( relList(i)%relData,scndlg%release(i) )

    CASE( HR_STACK, HR_STACK3 )
      scndlg%release(i)%type = 'CS'
      CALL SCIP_GUI_Stack( relList(i)%type,relList(i)%relData,scndlg%release(i) )

    CASE( HR_PRIME )
      scndlg%release(i)%type = 'CSP'
      CALL SCIP_GUI_Stack( HR_STACK,relList(i)%relData,scndlg%release(i) )

    CASE( HR_POOL )
      scndlg%release(i)%type = 'CP'
      CALL SCIP_GUI_Pool( relList(i)%relData,scndlg%release(i) )

    CASE DEFAULT
      WRITE(string1,'(A,A4)')'type =',relList(i)%type
      CALL SetError( IV_ERROR, &
                    'Invalid release type', &
                     string1,' ', &
                    'SCIP_GUI_Release' )
      GOTO 9999

  END SELECT

!====== Additional GUI parameters

  IF( do_setIndex )THEN
    CALL set_release_indx( scndlg%release(i),matdlg,.TRUE. )
    IF( scndlg%release(i)%distrib <= 0 )THEN
      scndlg%release(i)%param(REL_MMD_INDX)   = NOT_SET_R
      scndlg%release(i)%param(REL_SIGMA_INDX) = NOT_SET_R
!      scndlg%release(i)%param(REL_WMFRAC_INDX) = NOT_SET_R
    END IF
  ELSE
    scndlg%release(i)%distrib = 0
    scndlg%release(i)%indx    = 1
  END IF

  DO j = 1,3
    IF( scndlg%release(i)%sig(j) <= 0.0)scndlg%release(i)%sig(j) = NOT_SET_R
  END DO

  CALL release_string( scndlg%release(i),matdlg )

  IF( scndlg%release(i)%matl /= ' ' .AND. TRIM(scndlg%release(i)%type) /= 'X' )THEN
    CALL find_material_list( matdlg%material,matdlg%nmatl,scndlg%release(i)%matl,id )
    IF( id <= 0 .OR. id > matdlg%nmatl )THEN
      CALL SetError( IV_ERROR, &
                    'Invalid material specification', &
                    'Material not found M='//TRIM(scndlg%release(i)%matl),' ', &
                    'GUI->SCIP scenario' )
      GOTO 9999
    END IF
    IF( IsMulti(matdlg%material(id)%icls) )THEN
      IF( ASSOCIATED(scndlg%release(i)%mc) )THEN
        DEALLOCATE( scndlg%release(i)%mc,STAT=ios )
        NULLIFY( scndlg%release(i)%mc )
      END IF
      ALLOCATE( scndlg%release(i)%mc(matdlg%material(id)%nmc) )
      DO j = 1,matdlg%material(id)%nmc
        scndlg%release(i)%mc(j) = 0.0
      END DO
      IF( nMC > 0 )THEN
        DO j = 1,nMC
          IF( relMCList(j)%relID == i )THEN
            CALL find_materialMC( matdlg%materialMC(id),relMCList(j)%MCname,jd )
            IF( jd > 0 )THEN
              scndlg%release(i)%mc(jd) = relMCList(j)%MCmass
            END IF
          END IF
        END DO
      END IF
    END IF
  END IF

END DO

DO i = 1,nMC
  IF( scndlg%release(relMCList(i)%relID)%matl /= ' ' .AND. TRIM(scndlg%release(relMCList(i)%relID)%type) /= 'X' )THEN
    CALL find_material_list( matdlg%material,matdlg%nmatl,scndlg%release(relMCList(i)%relID)%matl,id )
    IF( id <= 0 .OR. id > matdlg%nmatl )THEN
      CALL SetError( IV_ERROR, &
                    'Invalid material specification', &
                    'Material not found M='//TRIM(scndlg%release(i)%matl),' ', &
                    'GUI->SCIP scenario' )
      GOTO 9999
    END IF
    IF( .NOT.IsMulti(matdlg%material%icls) )EXIT   !Skip if not recognized as multi-component
    CALL find_materialMC( matdlg%materialMC(id),relMCList(i)%MCname,jd )
    IF( jd <= 0 .OR. jd > matdlg%material(id)%nmc )THEN
      CALL SetError( IV_ERROR, &
                    'Invalid multicomponent specification', &
                    'multicomponent not found M='//TRIM(relMCList(i)%MCname),' ', &
                    'GUI->SCIP scenario' )
      GOTO 9999
    END IF
    IF( ASSOCIATED(scndlg%release(relMCList(i)%relID)%mc) )THEN
      scndlg%release(relMCList(i)%relID)%mc(jd) = relMCList(i)%MCmass
    END IF
  END IF
END DO

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Cont
!*******************************************************************************
SUBROUTINE GUI_SCIP_Cont( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relContT )    relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = release%indx

relData%rate     = release%rate
relData%duration = release%dur

relData%sigY = release%sig(2)
relData%sigZ = release%sig(3)

relData%momentum = release%dynam(2)
relData%buoyancy = release%dynam(1)

relData%activeFrac = release%param(REL_AFRAC_INDX)
relData%nextUpdtTime = release%param(REL_NXTUPDT_INDX)

IF( release%param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  relData%sigma = release%param(REL_SIGMA_INDX)
!  relData%dryFrac = release%param(REL_WMFRAC_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
!  relData%dryFrac = NOT_SET_R
END IF
relData%dryFrac = release%param(REL_WMFRAC_INDX)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_ContFile
!*******************************************************************************
SUBROUTINE GUI_SCIP_ContFile( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str  ) release
TYPE( relContFileT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Load

relData%distribution = release%indx

relData%rate     = release%rate
relData%duration = release%dur

relData%sigY = release%sig(2)
relData%sigZ = release%sig(3)

relData%momentum = release%dynam(2)
relData%buoyancy = release%dynam(1)

relData%activeFrac = release%param(REL_AFRAC_INDX)

IF( release%param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  relData%sigma = release%param(REL_SIGMA_INDX)
!  relData%dryFrac = release%param(REL_WMFRAC_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
!  relData%dryFrac = NOT_SET_R
END IF
relData%dryFrac = release%param(REL_WMFRAC_INDX)

relData%relFile = TRIM(release%file)
CALL AddPath( relData%relFile,TRIM(release%path) )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Inst
!*******************************************************************************
SUBROUTINE GUI_SCIP_Inst( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relInstT )    relData

INTEGER, EXTERNAL :: Real2Int
REAL,    EXTERNAL :: ScaleReal

!==== Set pointer

relData%distribution = release%indx

relData%mass  = release%rate

relData%sigX = release%sig(1)
relData%sigY = release%sig(2)
relData%sigZ = release%sig(3)

relData%momentum = release%dynam(2)
relData%buoyancy = release%dynam(1)

relData%activeFrac = release%param(REL_AFRAC_INDX)

IF( release%param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  relData%sigma = release%param(REL_SIGMA_INDX)
!  relData%dryFrac = release%param(REL_WMFRAC_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
!  relData%dryFrac = NOT_SET_R
END IF
relData%dryFrac = release%param(REL_WMFRAC_INDX)

IF( release%param(REL_RAND_INDX) > 0.0 )THEN
  relData%nRandom   = Real2Int( release%param(REL_RAND_INDX) )
  relData%ranSeed   = Real2Int( release%param(REL_SEED_INDX) )
  relData%ranSpread = release%param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Flash
!*******************************************************************************
SUBROUTINE GUI_SCIP_Flash( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relInstT )    relData

INTEGER, EXTERNAL :: Real2Int
REAL,    EXTERNAL :: ScaleReal

!==== Set pointer

relData%distribution = release%indx

relData%mass  = release%rate

relData%sigX = release%sig(1)
relData%sigY = release%sig(2)
relData%sigZ = release%sig(3)

relData%momentum = release%dynam(2)
relData%buoyancy = release%dynam(1)

IF( release%param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  relData%sigma = release%param(REL_SIGMA_INDX)
!  relData%dryFrac = release%param(REL_WMFRAC_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
!  relData%dryFrac = NOT_SET_R
END IF
relData%dryFrac = release%param(REL_WMFRAC_INDX)

IF( release%param(REL_RAND_INDX) > 0.0 )THEN
  relData%nRandom   = Real2Int( release%param(REL_RAND_INDX) )
  relData%ranSeed   = Real2Int( release%param(REL_SEED_INDX) )
  relData%ranSpread = release%param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Move
!*******************************************************************************
SUBROUTINE GUI_SCIP_Move( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relMoveT )    relData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Load

relData%distribution = release%indx

relData%rate     = release%rate
relData%duration = release%dur

relData%sigY = release%sig(2)
relData%sigZ = release%sig(3)

relData%velX = release%vel(1)
relData%velY = release%vel(2)
relData%velZ = release%vel(3)

relData%momentum = release%dynam(2)
relData%buoyancy = release%dynam(1)

relData%activeFrac = release%param(REL_AFRAC_INDX)
relData%nextUpdtTime = release%param(REL_NXTUPDT_INDX)

IF( release%param(REL_MMD_INDX)/= NOT_SET_R )THEN
  relData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  relData%sigma = release%param(REL_SIGMA_INDX)
!  relData%dryFrac = release%param(REL_WMFRAC_INDX)
ELSE
  relData%MMD   = NOT_SET_R
  relData%sigma = NOT_SET_R
!  relData%dryFrac = NOT_SET_R
END IF
relData%dryFrac = release%param(REL_WMFRAC_INDX)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Pool
!*******************************************************************************
SUBROUTINE GUI_SCIP_Pool( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relPoolT )    relData

!==== Set pointer

!==== Load

relData%mass  = release%rate

relData%sizeX = release%sig(1)
relData%sizeY = release%sig(2)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Stack
!*******************************************************************************
INTEGER FUNCTION GUI_SCIP_Stack( release,relData ) RESULT( stackType )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE


TYPE( release_str ) release
TYPE( relGenT )   relData
TYPE( relStackT )   stackData
TYPE( relStack3T )   stack3Data

REAL, EXTERNAL :: ScaleReal

!==== Determine type

IF( release%dynam(2) == DEF_VAL_R .AND. release%dynam(3) == DEF_VAL_R )THEN
  stackType = HR_STACK
ELSE
  stackType = HR_STACK3
END IF

!==== Load

IF( stackType == HR_STACK )THEN
  stackData = TRANSFER(relData,stackData)

  stackData%distribution = release%indx

  stackData%rate     = release%rate
  stackData%duration = release%dur

  stackData%diameter = release%sig(1)

  stackData%exitVel  = release%dynam(4)

  stackData%exitTemp = release%dynam(1)

  IF( release%distrib > 0 )THEN
    stackData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
    stackData%sigma = release%param(REL_SIGMA_INDX)
  ELSE
    stackData%MMD   = NOT_SET_R
    stackData%sigma = NOT_SET_R
  END IF

  stackData%dryFrac = release%param(REL_WMFRAC_INDX)

  stackData%activeFrac = release%param(REL_AFRAC_INDX)
  stackData%nextUpdtTime = release%param(REL_NXTUPDT_INDX)

  relData = TRANSFER(stackData,relData)
ELSE
  stack3Data = TRANSFER(relData,stack3Data)
  stack3Data%distribution = release%indx

  stack3Data%rate     = release%rate
  stack3Data%duration = release%dur

  stack3Data%diameter = release%sig(1)

  stack3Data%exitVel(1)  = release%dynam(2)
  stack3Data%exitVel(2)  = release%dynam(3)
  stack3Data%exitVel(3)  = release%dynam(4)

  stack3Data%exitTemp = release%dynam(1)

  IF( release%distrib > 0 )THEN
    stack3Data%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
    stack3Data%sigma = release%param(REL_SIGMA_INDX)
  ELSE
    stack3Data%MMD   = NOT_SET_R
    stack3Data%sigma = NOT_SET_R
  END IF

  stack3Data%dryFrac = release%param(REL_WMFRAC_INDX)

  stack3Data%activeFrac = release%param(REL_AFRAC_INDX)
  stack3Data%nextUpdtTime = release%param(REL_NXTUPDT_INDX)

  relData = TRANSFER(stack3Data,relData)
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_Area
!*******************************************************************************
SUBROUTINE GUI_SCIP_Area( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relGenT )   relData
TYPE( relAreaT )   areaData

REAL, EXTERNAL :: ScaleReal

!==== Load

areaData = TRANSFER(relData,areaData)

areaData%distribution = release%indx

areaData%rate     = release%rate
areaData%duration = release%dur

areaData%xlen = 2.*release%sig(1) !*******
areaData%ylen = 2.*release%sig(2)
areaData%ang  = 0. !********

areaData%exitVel  = release%dynam(4)

areaData%exitTemp = release%dynam(1)

IF( areaData%distribution > 0 )THEN
  areaData%MMD   = ScaleReal( release%param(REL_MMD_INDX),1./HCF_M2MICRON )
  areaData%sigma = release%param(REL_SIGMA_INDX)
ELSE
  areaData%MMD   = NOT_SET_R
  areaData%sigma = NOT_SET_R
END IF

areaData%dryFrac = release%param(REL_WMFRAC_INDX)

areaData%activeFrac = release%param(REL_AFRAC_INDX)
areaData%nextUpdtTime = release%param(REL_NXTUPDT_INDX)

!CALL SplitName( areaData%relFile,release%file,release%path )

relData = TRANSFER(areaData,relData)

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        GUI_SCIP_File
!*******************************************************************************
SUBROUTINE GUI_SCIP_File( release,relData )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load an SCIP Material structure from SCIPUFF commons

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relFileT )    relData

INTEGER, EXTERNAL :: Real2Int

!==== Set pointer

relData%relFile = TRIM(release%file)
CALL AddPath( relData%relFile,TRIM(release%path) )

!==== Load

IF( release%param(REL_RAND_INDX) > 0.0 )THEN
  relData%nRandom   = Real2Int( release%param(REL_RAND_INDX) )
  relData%ranSeed   = Real2Int( release%param(REL_SEED_INDX) )
  relData%ranSpread = release%param(REL_SPREAD_INDX)
ELSE
  relData%nRandom   = NOT_SET_I
  relData%ranSeed   = NOT_SET_I
  relData%ranSpread = NOT_SET_R
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Cont
!*******************************************************************************
SUBROUTINE SCIP_GUI_Cont( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relContT )    relData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer
!==== Unload

release%indx = relData%distribution

release%rate = relData%rate
release%dur  = relData%duration

release%sig(1) = NOT_SET_R
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
ELSE
  release%dynam(2) = 0.0
END IF

release%param(REL_AFRAC_INDX) = relData%activeFrac
release%param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
ELSE
  release%dynam(1) = 0.0
END IF

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma
release%param(REL_WMFRAC_INDX)= relData%dryFrac

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_ContFile
!*******************************************************************************
SUBROUTINE SCIP_GUI_ContFile( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str  ) release
TYPE( relContFileT ) relData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer
!==== Unload

release%indx = relData%distribution

release%rate = relData%rate
release%dur  = relData%duration

release%sig(1) = NOT_SET_R
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
ELSE
  release%dynam(2) = 0.0
END IF

release%param(REL_AFRAC_INDX) = relData%activeFrac

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
ELSE
  release%dynam(1) = 0.0
END IF

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma
release%param(REL_WMFRAC_INDX)= relData%dryFrac

CALL SplitName( relData%relFile,release%file,release%path )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Inst
!*******************************************************************************
SUBROUTINE SCIP_GUI_Inst( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relInstT    ) relData

REAL, EXTERNAL :: Int2Real
REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

release%indx = relData%distribution

release%rate = relData%mass

release%sig(1) = relData%sigX
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
END IF

release%param(REL_AFRAC_INDX) = relData%activeFrac

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma

IF( relData%nRandom > 0 )THEN
  release%param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  release%param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  release%param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  release%param(REL_RAND_INDX)   = NOT_SET_R
  release%param(REL_SEED_INDX)   = NOT_SET_R
  release%param(REL_SPREAD_INDX) = NOT_SET_R
END IF

release%param(REL_WMFRAC_INDX) = relData%dryFrac

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_XInst
!*******************************************************************************
SUBROUTINE SCIP_GUI_XInst( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relXInstT    ) relData

REAL, EXTERNAL :: Int2Real
REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

release%indx = relData%distribution

release%rate = relData%mass

release%sig(1) = relData%sigX
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
END IF

release%param(REL_AFRAC_INDX) = relData%activeFrac

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma

IF( relData%nRandom > 0 )THEN
  release%param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  release%param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  release%param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  release%param(REL_RAND_INDX)   = NOT_SET_R
  release%param(REL_SEED_INDX)   = NOT_SET_R
  release%param(REL_SPREAD_INDX) = NOT_SET_R
END IF

release%param(REL_WMFRAC_INDX) = relData%dryFrac

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_XInst3
!*******************************************************************************
SUBROUTINE SCIP_GUI_XInst3( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relXInst3T  ) relData

REAL, EXTERNAL :: Int2Real
REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

release%indx = relData%distribution

release%rate = relData%mass

release%sig(1) = relData%sigX
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
END IF

release%param(REL_AFRAC_INDX) = relData%activeFrac

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma

IF( relData%nRandom > 0 )THEN
  release%param(REL_RAND_INDX)    = Int2Real( relData%nRandom )
  release%param(REL_SEED_INDX)    = Int2Real( relData%ranSeed )
  release%param(REL_SPREAD_INDX)  = relData%ranSpreadA
  release%param(REL_SPREADT_INDX) = relData%ranSpreadT
  release%param(REL_SPREADV_INDX) = relData%ranSpreadV
  release%param(REL_RANDIR_INDX)  = relData%ranDir
ELSE
  release%param(REL_RAND_INDX)    = NOT_SET_R
  release%param(REL_SEED_INDX)    = NOT_SET_R
  release%param(REL_SPREAD_INDX)  = NOT_SET_R
  release%param(REL_SPREADT_INDX) = NOT_SET_R
  release%param(REL_SPREADV_INDX) = NOT_SET_R
  release%param(REL_RANDIR_INDX)  = NOT_SET_R
END IF

release%param(REL_WMFRAC_INDX) = relData%dryFrac

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Inst
!*******************************************************************************
SUBROUTINE SCIP_GUI_Flash( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relInstT )    relData

REAL, EXTERNAL :: Int2Real
REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

release%indx = relData%distribution

release%rate = relData%mass

release%sig(1) = relData%sigX
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
END IF

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma

IF( relData%nRandom > 0 )THEN
  release%param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  release%param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  release%param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  release%param(REL_RAND_INDX)   = NOT_SET_R
  release%param(REL_SEED_INDX)   = NOT_SET_R
  release%param(REL_SPREAD_INDX) = NOT_SET_R
END IF

release%param(REL_WMFRAC_INDX) = relData%dryFrac

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Move
!*******************************************************************************
SUBROUTINE SCIP_GUI_Move( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relMoveT )    relData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

release%indx = relData%distribution

release%rate = relData%rate
release%dur  = relData%duration

release%sig(1) = NOT_SET_R
release%sig(2) = relData%sigY
release%sig(3) = relData%sigZ

release%vel(1) = relData%velX
release%vel(2) = relData%velY
release%vel(3) = relData%velZ

IF( relData%momentum /= NOT_SET_R )THEN
  release%dynam(2) = relData%momentum
END IF

IF( relData%buoyancy /= NOT_SET_R )THEN
  release%dynam(1) = relData%buoyancy
END IF

release%param(REL_MMD_INDX)   = ScaleReal( relData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = relData%sigma
release%param(REL_WMFRAC_INDX)= relData%dryFrac

release%param(REL_AFRAC_INDX) = relData%activeFrac
release%param(REL_NXTUPDT_INDX) = relData%nextUpdtTime

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Pool
!*******************************************************************************
SUBROUTINE SCIP_GUI_Pool( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relPoolT )    relData

!==== Set pointer

!==== Unload

release%rate = relData%mass
release%dur  = DEF_VAL_R !SCIPUFF expects this

release%sig(1) = relData%sizeX
release%sig(2) = relData%sizeY
release%sig(3) = 0.0 !SCIPUFF expects this

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Stack
!*******************************************************************************
SUBROUTINE SCIP_GUI_Stack( stackType,relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER             stackType
TYPE( release_str ) release
TYPE( relGenT )     relData
TYPE( relStackT )   stackData
TYPE( relStack3T )   stack3Data

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

IF( stackType == HR_STACK )THEN
  stackData = TRANSFER(relData,stackData)

  release%indx = stackData%distribution

  release%rate = stackData%rate
  release%dur  = stackData%duration

  release%sig(1) = stackData%diameter
  release%sig(2) = NOT_SET_R
  release%sig(3) = NOT_SET_R

  IF( stackData%exitVel /= NOT_SET_R )THEN
    release%dynam(4) = stackData%exitVel
  END IF
  release%dynam(2) = DEF_VAL_R
  release%dynam(3) = DEF_VAL_R

  IF( stackData%exitTemp /= NOT_SET_R )THEN
    release%dynam(1) = stackData%exitTemp
  END IF

  release%param(REL_MMD_INDX)   = ScaleReal( stackData%MMD,HCF_M2MICRON )
  release%param(REL_SIGMA_INDX) = stackData%sigma
  release%param(REL_WMFRAC_INDX)= stackData%dryFrac

  release%param(REL_AFRAC_INDX) = stackData%activeFrac
  release%param(REL_NXTUPDT_INDX) = stackData%nextUpdtTime

ELSE
  stack3Data = TRANSFER(relData,stack3Data)

  release%indx = stack3Data%distribution

  release%rate = stack3Data%rate
  release%dur  = stack3Data%duration

  release%sig(1) = stack3Data%diameter
  release%sig(2) = NOT_SET_R
  release%sig(3) = NOT_SET_R

  IF( stack3Data%exitVel(3) /= NOT_SET_R )THEN
    release%dynam(2) = stack3Data%exitVel(1)
    release%dynam(3) = stack3Data%exitVel(2)
    release%dynam(4) = stack3Data%exitVel(3)
  END IF

  IF( stack3Data%exitTemp /= NOT_SET_R )THEN
    release%dynam(1) = stack3Data%exitTemp
  END IF

  release%param(REL_MMD_INDX)   = ScaleReal( stack3Data%MMD,HCF_M2MICRON )
  release%param(REL_SIGMA_INDX) = stack3Data%sigma
  release%param(REL_WMFRAC_INDX)= stack3Data%dryFrac

  release%param(REL_AFRAC_INDX) = stack3Data%activeFrac
  release%param(REL_NXTUPDT_INDX) = stack3Data%nextUpdtTime

END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_Area
!*******************************************************************************
SUBROUTINE SCIP_GUI_Area( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relGenT )     relData
TYPE( relAreaT )    areaData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

areaData = TRANSFER(relData,areaData)

release%indx = areaData%distribution

release%rate = areaData%rate
release%dur  = areaData%duration

release%sig(1) = areaData%xlen
release%sig(2) = areaData%ylen
release%sig(3) = NOT_SET_R

IF( areaData%exitVel /= NOT_SET_R )THEN
  release%dynam(4) = areaData%exitVel
END IF
release%dynam(2) = DEF_VAL_R
release%dynam(3) = DEF_VAL_R

IF( areaData%exitTemp /= NOT_SET_R )THEN
  release%dynam(1) = areaData%exitTemp
END IF

release%param(REL_MMD_INDX)   = ScaleReal( areaData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = areaData%sigma
release%param(REL_WMFRAC_INDX)= areaData%dryFrac

release%param(REL_AFRAC_INDX) = areaData%activeFrac
release%param(REL_NXTUPDT_INDX) = areaData%nextUpdtTime

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_AreaFile
!*******************************************************************************
SUBROUTINE SCIP_GUI_AreaFile( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relGenT )     relData
TYPE( relAreaFileT ) areaData

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

areaData = TRANSFER(relData,areaData)

release%indx = areaData%distribution

release%rate = areaData%rate
release%dur  = areaData%duration

release%sig(1) = areaData%xlen
release%sig(2) = areaData%ylen
release%sig(3) = NOT_SET_R

IF( areaData%exitVel /= NOT_SET_R )THEN
  release%dynam(4) = areaData%exitVel
END IF
release%dynam(2) = DEF_VAL_R
release%dynam(3) = DEF_VAL_R

IF( areaData%exitTemp /= NOT_SET_R )THEN
  release%dynam(1) = areaData%exitTemp
END IF

release%param(REL_MMD_INDX)   = ScaleReal( areaData%MMD,HCF_M2MICRON )
release%param(REL_SIGMA_INDX) = areaData%sigma
release%param(REL_WMFRAC_INDX)= areaData%dryFrac

release%param(REL_AFRAC_INDX) = areaData%activeFrac
!!DEC$ IF DEFINED (OPVIEW)
!release%param(REL_NXTUPDT_INDX) = areaData%nextUpdtTime
!!DEC$ ENDIF

CALL SplitName( areaData%relFile,release%file,release%path )

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SCIP_GUI_StackFile
!*******************************************************************************
SUBROUTINE SCIP_GUI_StackFile( stackType,relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

INTEGER                stackType
TYPE( release_str )    release
TYPE( relGenT )        relData
TYPE( relStackFileT )  stackData
TYPE( relStack3FileT ) stack3Data

REAL, EXTERNAL :: ScaleReal

!==== Set pointer

!==== Unload

IF( stackType == HR_STACKF .OR. stackType == HR_STACKFP )THEN
  stackData = TRANSFER(relData,stackData)

  release%indx = stackData%distribution

  release%rate = stackData%rate
  release%dur  = stackData%duration

  release%sig(1) = stackData%diameter
  release%sig(2) = NOT_SET_R
  release%sig(3) = NOT_SET_R

  IF( stackData%exitVel /= NOT_SET_R )THEN
    release%dynam(4) = stackData%exitVel
  END IF
  release%dynam(2) = DEF_VAL_R
  release%dynam(3) = DEF_VAL_R

  IF( stackData%exitTemp /= NOT_SET_R )THEN
    release%dynam(1) = stackData%exitTemp
  END IF

  release%param(REL_MMD_INDX)   = ScaleReal( stackData%MMD,HCF_M2MICRON )
  release%param(REL_SIGMA_INDX) = stackData%sigma
  release%param(REL_WMFRAC_INDX)= stackData%dryFrac

  release%param(REL_AFRAC_INDX) = stackData%activeFrac

  CALL SplitName( stackData%relFile,release%file,release%path )

ELSE

  stack3Data = TRANSFER(relData,stack3Data)

  release%indx = stack3Data%distribution

  release%rate = stack3Data%rate
  release%dur  = stack3Data%duration

  release%sig(1) = stack3Data%diameter
  release%sig(2) = NOT_SET_R
  release%sig(3) = NOT_SET_R

  IF( stack3Data%exitVel(3) /= NOT_SET_R )THEN
    release%dynam(2) = stack3Data%exitVel(1)
    release%dynam(3) = stack3Data%exitVel(2)
    release%dynam(4) = stack3Data%exitVel(3)
  END IF

  IF( stack3Data%exitTemp /= NOT_SET_R )THEN
    release%dynam(1) = stack3Data%exitTemp
  END IF

  release%param(REL_MMD_INDX)   = ScaleReal( stack3Data%MMD,HCF_M2MICRON )
  release%param(REL_SIGMA_INDX) = stack3Data%sigma
  release%param(REL_WMFRAC_INDX)= stack3Data%dryFrac

  release%param(REL_AFRAC_INDX) = stack3Data%activeFrac

  CALL SplitName( stack3Data%relFile,release%file,release%path )

END IF

9999 CONTINUE

RETURN
END

!*******************************************************************************
!        SCIP_GUI_File
!*******************************************************************************
SUBROUTINE SCIP_GUI_File( relData,release )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE default_fd

!     Load SCIPUFF commons from an SCIP Material structure

IMPLICIT NONE

TYPE( release_str ) release
TYPE( relFileT )    relData

REAL,                      EXTERNAL :: Int2Real
CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!==== Set pointer

CALL SplitName( relData%relFile,release%file,release%path )
release%file = StripNull( release%file )
release%path = StripNull( release%path )

!==== Unload

IF( relData%nRandom > 0 )THEN
  release%param(REL_RAND_INDX)   = Int2Real( relData%nRandom )
  release%param(REL_SEED_INDX)   = Int2Real( relData%ranSeed )
  release%param(REL_SPREAD_INDX) = relData%ranSpread
ELSE
  release%param(REL_RAND_INDX)   = NOT_SET_R
  release%param(REL_SEED_INDX)   = NOT_SET_R
  release%param(REL_SPREAD_INDX) = NOT_SET_R
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!        SizeRelList
!*******************************************************************************
SUBROUTINE SizeRelList( scndlg )

USE relparam_fd
USE release_gui_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE errorParam_fd
USE GUImatl_fd
USE default_fd
USE guitool_fi

IMPLICIT NONE

TYPE( reldef_str ) scndlg

INTEGER nrel
INTEGER nmc, i, j

nrel = scndlg%nrel

!==== Material List

nmc = 0
DO i = 1,scndlg%nrel

  IF( ASSOCIATED(scndlg%release(i)%mc) )THEN
    DO j = 1,SIZE(scndlg%release(i)%mc)
      IF( scndlg%release(i)%mc(j) /= 0.0 )nmc = nmc + 1
    END DO
  END IF

END DO

CALL AllocateRelList( nrel,nmc )

RETURN
END
!*******************************************************************************
!        AllocateRelList
!*******************************************************************************
SUBROUTINE AllocateRelList( nRel,nMC )

USE errorParam_fd
USE guitool_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER nRel
INTEGER nMC

INTEGER ios

CHARACTER(48) aString

IF( ALLOCATED(relList) )THEN
  IF( SIZE(relList) < nrel )THEN
    DEALLOCATE( relList,STAT=ios )
    aString = 'Reallocation of relList'
    ALLOCATE( relList(nrel),STAT=ios )
    IF( ios /= 0 )GOTO 9999
  END IF
ELSE
  aString = 'Allocation of relList'
  ALLOCATE( relList(MAX(1,nrel)),STAT=ios )
  IF( ios /= 0 )GOTO 9999
END IF

IF( ALLOCATED(relMCList) )THEN
  IF( SIZE(relMCList) < nMC )THEN
    DEALLOCATE( relMCList,STAT=ios )
    aString = 'Reallocation of relMCList'
    ALLOCATE( relMCList(nMC),STAT=ios )
    IF( ios /= 0 )GOTO 9999
  END IF
ELSE
  aString = 'Allocation of relMCList'
  ALLOCATE( relMCList(MAX(1,nMC)),STAT=ios )
  IF( nMC == 0 )relMCList(1)%relID = -1
  IF( ios /= 0 )GOTO 9999
END IF

1000 CONTINUE

RETURN

9999 CONTINUE
WRITE(string1,*)'new size =',nRel,nMC
WRITE(string2,*)'Allocation call sequence ='//TRIM(aString)
CALL SetError( SZ_ERROR,'Allocation Error',string2,string1,'ReallocateScenario' )
GOTO 1000

END
!*******************************************************************************
!        DeallocateRelList
!*******************************************************************************
SUBROUTINE DeallocateRelList()

USE guitool_fi

IMPLICIT NONE

INTEGER ios

IF( ALLOCATED(relList) )DEALLOCATE( relList,STAT=ios )

IF( ALLOCATED(relMCList) )DEALLOCATE( relMCList,STAT=ios )

RETURN
END
