!*******************************************************************************
!             GetRestartInfo
!*******************************************************************************
SUBROUTINE GetRestartInfo( inputFile )

USE dialog_fi
USE tooluser_fd
USE errorParam_fd
USE SCIPtool

IMPLICIT NONE

CHARACTER(*) inputFile

TYPE( projectIDT ) :: Project

INTEGER irv,ios,nTimeX1,nTimeX2,userID

INTEGER nTimeX3

TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: timeX1
TYPE(SCIPTimeT), DIMENSION(:), ALLOCATABLE :: timeX2

CHARACTER(32) who,what

!==============================================================================
!Deallocate restart time array
!==============================================================================

IF( ALLOCATED(timeRestart) )DEALLOCATE( timeRestart,STAT=ios )

!==============================================================================
!Set Project name/path
!==============================================================================

CALL SplitName( inputFile,Project%name,Project%path )
CALL RemoveExtension( Project%name )

!==============================================================================
!Get number of plot times
!==============================================================================
userID = 3333

irv = SCIPNumPlotTimes( userID,Project,nTimeRestart,nTimeX1,nTimeX2,nTimeX3 )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'GetRestartInfo' )
  GOTO 9999
END IF

!==============================================================================
!Allocate plot time arrays
!==============================================================================

who = 'Restart times'
ALLOCATE( timeRestart(nTimeRestart),STAT=ios )

IF( ios == 0 )THEN
  who = 'surface times'
  ALLOCATE( timeX1(ntimeX1),STAT=ios )
END IF

IF( ios == 0 )THEN
  who = 'Meteorology times'
  ALLOCATE( timeX2(nTimeX2),STAT=ios )
END IF

IF( ios /= 0 )THEN
  WRITE(what,*)ios
  CALL SetError( UK_ERROR,'Error allocating memory for '//TRIM(who), &
                'ALLOCATE status = '//TRIM(ADJUSTL(what)),' ', &
                'AllocateRestartTime' )
  GOTO 9999
END IF

!==============================================================================
!Get plot times
!==============================================================================

irv = SCIPGetPlotTimes( userID,Project,timeRestart,timeX1,timeX2 )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( 'GetRestartInfo' )
  GOTO 9999
END IF

9999 CONTINUE

IF( ALLOCATED(timeX1) )DEALLOCATE( timeX1,STAT=ios )
IF( ALLOCATED(timeX2) )DEALLOCATE( timeX2,STAT=ios )

RETURN
END
!*******************************************************************************
!             GetMatInfo
!*******************************************************************************
SUBROUTINE GetMatInfo( iwnd_db,ldos,lsrf )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE SCIAPIversion_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE plotdlg_fi
USE errorParam_fd
USE metparam_fd
USE srfparam_fd
USE GUImatl_fi
USE dialog_fi
USE GUItool_fi
USE pltchoice_fi
USE SwinAPI
USE UtilMtlAux

!     This routine sets the Lists for Materials

IMPLICIT NONE

REAL    dsize,rtmp(4)

INTEGER(POINTER_LEN) iwnd_db !Window handle
INTEGER i,nsg,irv,j
INTEGER prj_version

INTEGER nRel,nMtl

LOGICAL LatLon
LOGICAL ldos,lsrf,lunit,ltot,reldyn
LOGICAL Cartesian,Def_axes,Def_map,Need_Def
INTEGER nMC

CHARACTER(16)  ctmp1

LOGICAL, EXTERNAL :: IsParticle
LOGICAL, EXTERNAL :: IsAerosol
LOGICAL, EXTERNAL :: IsWetParticle,IsLiquid

TYPE( ProjectStructure ) prjdlg

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine

LOGICAL, EXTERNAL :: hasError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'GetMatInfo'

!==== SCIP Tool

prjdlg = project(DEFAULT_LEVEL)

CALL SplitName( file_inp,prjdlg%ID%name,prjdlg%ID%path )
CALL RemoveExtension( prjdlg%ID%name )

CALL GUI_SCIP_ctrl    ( prjdlg,ctrl )
CALL GUI_SCIP_time    ( prjdlg,dlgTime(DEFAULT_LEVEL),time )
CALL GUI_SCIP_flags   ( prjdlg,flags )
CALL GUI_SCIP_domain  ( prjdlg,dlgDomain(DEFAULT_LEVEL),domain )
CALL GUI_SCIP_options ( prjdlg,dlgOptions(DEFAULT_LEVEL),prjOptions )
CALL GUI_SCIP_met     ( prjdlg,metdef(DEFAULT_LEVEL),weather )

irv = SCIPSizeProjectMC(ToolCallerID,ctrl%project, &
                        nMtl, &
                        nRel, &
                        nMC )
IF( irv == SCIPfailure )THEN
  CALL GetToolError( eRoutine )
  GOTO 9999
END IF

!==== Reallocate to big enough if necessary
IF( nRel > MAXREL )THEN
  CALL ReallocateScenario(nRel)
  IF( hasError() )GOTO 9998
END IF

IF( nMtl > MAXMTYP .OR. 30*nMtl > MAXMAUX )THEN
  CALL ReallocateMaterials( MAX(nMtl,MAXMTYP),MAX(30*nMtl,MAXMAUX) )
  IF( hasError() )GOTO 9998
END IF

matdef%mtlHead%max    = MAXMTYP

CALL AllocateRelList(nrel,nMC)
IF( hasError() )GOTO 9998

CALL AllocateMtlList(nMtl)
IF( hasError() )GOTO 9998

CALL GUI_SCIP_material( prjdlg,materials(DEFAULT_LEVEL),matdef,mtlList,SIZE(mtlList) )
IF( hasError() )GOTO 9998

loadPrj%project       = ctrl%project
loadPrj%input%ctrl    = ctrl%ctrl
loadPrj%input%time    = time%time
loadPrj%input%flags   = flags%flags
loadPrj%input%domain  = domain%spatial
loadPrj%input%option  = prjOptions%option
loadPrj%input%mtlHead = matdef%mtlHead
loadPrj%weather       = weather%weather

loadPrj%scnHead%max    = nRel
loadPrj%scnHead%number = 0

IF( nMC > 0 )THEN
  irv = SCIPLoadProjectMC( ToolCallerID,loadPrj, &
                           mtlList, &
                           relList, &
                           relMCList )
ELSE
  irv = SCIPLoadProject( ToolCallerID,loadPrj, &
                         mtlList, &
                         relList )
END IF
IF( irv == SCIPfailure )THEN
  CALL GetToolError( eRoutine )
  GOTO 9999
END IF

ctrl%ctrl       = loadPrj%input%ctrl
time%time       = loadPrj%input%time
flags%flags     = loadPrj%input%flags
domain%spatial  = loadPrj%input%domain
prjOptions%option = loadPrj%input%option
matdef%mtlHead  = loadPrj%input%mtlHead

CALL SCIP_GUI_flags   ( project(BASE_LEVEL),flags )
CALL SCIP_GUI_ctrl    ( project(BASE_LEVEL),ctrl )
IF( hasError() )THEN
  CALL AddErrorAction('Project restart turned off')
  CALL ShowErrorMessage(iwnd_db)
  project(BASE_LEVEL)%Restart = .FALSE.
  project(BASE_LEVEL)%RestartFile = ' '
  project(BASE_LEVEL)%RestartTimeIndx = NOT_SET_I
END IF
CALL SCIP_GUI_time    ( dlgTime(BASE_LEVEL),time )
CALL SCIP_GUI_domain  ( dlgDomain(BASE_LEVEL),domain )
CALL SCIP_GUI_options ( dlgOptions(BASE_LEVEL),prjOptions )
CALL SCIP_GUI_material( materials(BASE_LEVEL),matdef,mtlList )
IF( hasError() )THEN
  DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)
  GOTO 9998
END IF

!---- Check for old project -> convert

prj_version = loadPrj%project%version

IF( prj_version < SCIAPI_00_VERSION )THEN
  WRITE(eMessage,'(A,I3)')'Project created by Version 0.',prj_version
  eInform  = 'Unable to read Projects created prior to Version 0.200'
  eAction  = 'Use Version 0.2xx to convert the project'
  GOTO 9999
ELSE IF( prj_version/100 < scipuff_version/100 )THEN
  IF( prj_version < 1000 )THEN
    WRITE(eMessage,'(A,I3)')'Project created by Version 0.',prj_version
  ELSE IF( prj_version < 2000 )THEN
    WRITE(eMessage,'(A,I3.3)')'Project created by Version 1.',prj_version - 1000
  ELSE IF( prj_version < 3000 )THEN
    WRITE(eMessage,'(A,I3.3)')'Project created by Version 2.',prj_version - 2000
  ELSE
    WRITE(eMessage,'(A,I3.3)')'Project created by Version 3.',prj_version - 3000
  END IF
  eInform  = 'Project may be plotted/reviewed but not continued'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
  CALL ShowInfoMessage(iwnd_db)
  eRoutine = 'GetMatInfo'
END IF

ldos = BTEST(loadPrj%current%status,HSB_HASDOS)
lsrf = BTEST(loadPrj%current%status,HSB_HASDEP)

project(BASE_LEVEL)%Run = (prj_version/100 == scipuff_version/100)
project(BASE_LEVEL)%MapCoord = dlgDomain(BASE_LEVEL)%spatial%domain%coord
project(BASE_LEVEL)%Ref = ( MIN(dlgDomain(BASE_LEVEL)%spatial%reference%x,dlgDomain(BASE_LEVEL)%spatial%reference%y,dlgDomain(BASE_LEVEL)%spatial%reference%lat,dlgDomain(BASE_LEVEL)%spatial%reference%lon) /= NOT_SET_R ) .AND. &
                   project(BASE_LEVEL)%MapCoord /= I_UTM
project(BASE_LEVEL)%Terrain = BTEST(loadPrj%current%status,HSB_HASTERRAIN)

DefinedOK = IBSET(DefinedOK,DF_TIME)
DefinedOK = IBSET(DefinedOK,DF_DOMAIN)
DefinedOK = IBSET(DefinedOK,DF_OPTION)
DefinedOK = IBSET(DefinedOK,DF_MATERIAL)

!---- Check materials; intialize surface blocks

lunit = .TRUE.


!---- Set material lists

DO i = 1,materials(BASE_LEVEL)%nmatl
  IF( IsParticle(materials(BASE_LEVEL)%material(i)%icls) &
     .OR. IsWetParticle(materials(BASE_LEVEL)%material(i)%icls) )THEN
    nsg = GetSubgroups( materials(BASE_LEVEL)%material(i),materials(BASE_LEVEL)%mat_aux )
    ltot = nsg > 1
  ELSE IF( IsLiquid(materials(BASE_LEVEL)%material(i)%icls) )THEN
    ltot = .TRUE.
  ELSE IF( IsAerosol(materials(BASE_LEVEL)%material(i)%icls) )THEN
    ltot = .FALSE.
  ELSE
    ltot = .FALSE.
  END IF
  ctmp1 = materials(BASE_LEVEL)%material(i)%unit
  CALL clower( ctmp1 )
  lunit = lunit .AND. (materials(BASE_LEVEL)%material(i)%unit == materials(BASE_LEVEL)%material(1)%unit)
END DO !


!---- Set ATP plot flag


!---- Set default coordinate type and Map drawing

!---- See if current value is current default value

Def_axes = (axesdef(DEFAULT_LEVEL)%MapCoord == axesdef(BASE_LEVEL)%MapCoord)
IF( Def_axes )THEN
  DO i = 1,4
    Def_axes = Def_axes .AND. &
             axesdef(DEFAULT_LEVEL)%dbint(i) == axesdef(BASE_LEVEL)%dbint(i)
  END DO
  DO i = 5,16
    Def_axes = Def_axes .AND. &
             axesdef(DEFAULT_LEVEL)%dbreal(i) == axesdef(BASE_LEVEL)%dbreal(i)
  END DO
END IF
Def_map = mapdef(DEFAULT_LEVEL)%MapsOn == mapdef(BASE_LEVEL)%MapsOn

!---- Set CARTESIAN as default if Cartesian

Cartesian = project(BASE_LEVEL)%MapCoord == I_CARTESIAN .OR. &
            project(BASE_LEVEL)%MapCoord == I_METERS
IF( Cartesian )THEN
  axesdef(DEFAULT_LEVEL)%MapCoord = I_CARTESIAN
ELSE IF( project(BASE_LEVEL)%MapCoord == I_UTM )THEN
  axesdef(DEFAULT_LEVEL)%MapCoord = I_UTM
ELSE
  axesdef(DEFAULT_LEVEL)%MapCoord = I_LATLON
END IF

!---- LatLon run - Set default refernence point as middle of domain
!                  and turn on map drawing as default

IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN

  axesdef(DEFAULT_LEVEL)%Lon0 = 0.5*(dlgDomain(BASE_LEVEL)%spatial%domain%xMin + dlgDomain(BASE_LEVEL)%spatial%domain%xMax)
  axesdef(DEFAULT_LEVEL)%Lat0 = 0.5*(dlgDomain(BASE_LEVEL)%spatial%domain%yMin + dlgDomain(BASE_LEVEL)%spatial%domain%yMax)
  axesdef(DEFAULT_LEVEL)%X0   = 0.
  axesdef(DEFAULT_LEVEL)%Y0   = 0.
  IF( axesdef(BASE_LEVEL)%Lon0 == NOT_SET_R )axesdef(BASE_LEVEL)%Lon0 = axesdef(DEFAULT_LEVEL)%Lon0
  IF( axesdef(BASE_LEVEL)%Lat0 == NOT_SET_R )axesdef(BASE_LEVEL)%Lat0 = axesdef(DEFAULT_LEVEL)%Lat0
  mapdef(DEFAULT_LEVEL)%MapsOn = .TRUE.

ELSE

!---- UTM run or Cartesian run with reference point - set it
!                  and turn on map drawing as default

  IF( project(BASE_LEVEL)%Ref .OR. project(BASE_LEVEL)%MapCoord == I_UTM )THEN

    axesdef(DEFAULT_LEVEL)%Lon0 = dlgDomain(BASE_LEVEL)%spatial%reference%lon
    axesdef(DEFAULT_LEVEL)%Lat0 = dlgDomain(BASE_LEVEL)%spatial%reference%lat
    axesdef(DEFAULT_LEVEL)%X0   = dlgDomain(BASE_LEVEL)%spatial%reference%x
    axesdef(DEFAULT_LEVEL)%Y0   = dlgDomain(BASE_LEVEL)%spatial%reference%y
    axesdef(BASE_LEVEL)%Lon0  = dlgDomain(BASE_LEVEL)%spatial%reference%lon
    axesdef(BASE_LEVEL)%Lat0  = dlgDomain(BASE_LEVEL)%spatial%reference%lat
    axesdef(BASE_LEVEL)%X0    = dlgDomain(BASE_LEVEL)%spatial%reference%x
    axesdef(BASE_LEVEL)%Y0    = dlgDomain(BASE_LEVEL)%spatial%reference%y
    IF( project(BASE_LEVEL)%MapCoord == I_METERS )THEN
      mapdef(DEFAULT_LEVEL)%MapsOn = .FALSE.
    ELSE IF( project(BASE_LEVEL)%MapCoord == I_UTM .AND. dlgDomain(BASE_LEVEL)%spatial%domain%zoneUTM == DEF_VAL_I )THEN
      mapdef(DEFAULT_LEVEL)%MapsOn = .FALSE.
    ELSE
      mapdef(DEFAULT_LEVEL)%MapsOn = .TRUE.
    END IF

  ELSE

!---- Cartesian run without reference point - clear default reference point
!                  and turn off map drawing

    mapdef(DEFAULT_LEVEL)%MapsOn = .FALSE.
    axesdef(DEFAULT_LEVEL)%Lon0 = NOT_SET_R
    axesdef(DEFAULT_LEVEL)%Lat0 = NOT_SET_R
    axesdef(DEFAULT_LEVEL)%X0   = 0.
    axesdef(DEFAULT_LEVEL)%Y0   = 0.
    IF( axesdef(BASE_LEVEL)%Lon0 == DEF_VAL_R )THEN
      axesdef(BASE_LEVEL)%Lon0 = NOT_SET_R
      mapdef(BASE_LEVEL)%MapsOn = .FALSE.
    END IF
    IF( axesdef(BASE_LEVEL)%Lat0 == DEF_VAL_R )THEN
      axesdef(BASE_LEVEL)%Lat0 = NOT_SET_R
      mapdef(BASE_LEVEL)%MapsOn = .FALSE.
    END IF
  END IF
END IF

!---- If axes were default or Switching from/to UTM to/from Cartesian - set to new default

Need_Def = project(BASE_LEVEL)%MapCoord==I_UTM .AND. axesdef(BASE_LEVEL)%MapCoord==I_CARTESIAN
Need_Def = Need_Def .OR. &
     project(BASE_LEVEL)%MapCoord==I_CARTESIAN .AND. axesdef(BASE_LEVEL)%MapCoord==I_UTM

IF( Def_axes .OR. Need_Def )THEN
  axesdef(BASE_LEVEL) = axesdef(DEFAULT_LEVEL)
ELSE
  IF( PlotDef(BASE_LEVEL)%Field%Category /= HP_VSLICE .AND. &
      PlotDef(BASE_LEVEL)%Field%Category /= HP_HINT )THEN
    CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                            0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
    LatLon    = (project(BASE_LEVEL)%MapCoord == I_LATLON)
    Cartesian = (axesdef(BASE_LEVEL)%MapCoord /= I_LATLON)
    CALL axes_transform( axesdef(BASE_LEVEL)%dbreal(5),rtmp, &
                         axesdef(BASE_LEVEL)%dbreal(17),LatLon, &
                         Cartesian,.FALSE.,.TRUE. )
  END IF
END IF

!---- If maps were default - set to new default

IF( Def_map )mapdef(BASE_LEVEL)%MapsOn = mapdef(DEFAULT_LEVEL)%MapsOn

!---- Set default Hi/Low res maps - based on domain size

Def_map = (mapdef(BASE_LEVEL)%HiRes == mapdef(DEFAULT_LEVEL)%HiRes)
dsize = MIN( (dlgDomain(BASE_LEVEL)%spatial%domain%xMax - &
              dlgDomain(BASE_LEVEL)%spatial%domain%xMin), &
             (dlgDomain(BASE_LEVEL)%spatial%domain%yMax - &
              dlgDomain(BASE_LEVEL)%spatial%domain%yMin) )
IF( project(BASE_LEVEL)%MapCoord == I_LATLON )THEN
  mapdef(DEFAULT_LEVEL)%HiRes = dsize <= 2.0
ELSE IF( project(BASE_LEVEL)%MapCoord == I_METERS )THEN
  mapdef(DEFAULT_LEVEL)%HiRes = dsize <= 100000.0
ELSE
  mapdef(DEFAULT_LEVEL)%HiRes = dsize <= 200.0
END IF

IF( Def_map )mapdef(BASE_LEVEL)%HiRes = mapdef(DEFAULT_LEVEL)%HiRes

!---- Read Scenario file to create release list (PCSCIPUF only)

reldef%scnHead  = loadPrj%scnHead

CALL SCIP_GUI_scenario( scenario(BASE_LEVEL),materials(BASE_LEVEL),reldef,relList,nMC,relMCList )
IF( hasError() )GOTO 9998

IF( scenario(BASE_LEVEL)%nrel > 0 )THEN
  DefinedOK = IBSET(DefinedOK,DF_RELEASE)
ELSE
  DefinedOK = IBCLR(DefinedOK,DF_RELEASE)
END IF

IF( project(BASE_LEVEL)%audit%Version(1:3) == '0.2' )THEN !For projects built
  DO i = 1,scenario(BASE_LEVEL)%nrel !flag
   reldyn = .FALSE.
   DO j = 1,NUM_DYNAMIC
     reldyn = reldyn .OR. scenario(BASE_LEVEL)%release(i)%dynam(j) > 0.
   END DO
   project(BASE_LEVEL)%Dynamic = project(BASE_LEVEL)%Dynamic .OR. reldyn
  END DO
END IF

!---- Read Met file

weather%weather = loadPrj%weather
CALL SCIP_GUI_met( metdef(BASE_LEVEL),weather )

DefinedOK = IBSET(DefinedOK,DF_MET)
CALL convert_SCIP20_met( iwnd_db,prj_version,metdef(BASE_LEVEL),project(BASE_LEVEL) )

CALL ResetPlotField( BASE_LEVEL,file_inp )
IF( hasError() )GOTO 9998

CALL ResetTerrainField()

9999  CONTINUE
IF( nError /= NO_ERROR )CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
9998 CONTINUE
CALL DeallocateRelList()
CALL DeallocateMtlList()

RETURN
END
!***********************************************************************
!***********************************************************************
!
!***********************************************************************
!***********************************************************************
SUBROUTINE ResetPlotField( indx,filename )

USE pltchoice_fi
USE pcscipuf_fi
USE param_fd
USE default_fd

IMPLICIT NONE

INTEGER      indx
CHARACTER(*) filename

LOGICAL      HaveCurrent,InRange
INTEGER      nPltType

INTEGER      i,j

TYPE( PlotChoice ) :: DefPlt, CurPlt

LOGICAL, EXTERNAL :: hasError

!==============================================================================
! Set Default values
!==============================================================================

DefPlt = DefaultPlot

IF( DefPlt%Type == NOT_SET_I )DefPlt%Type = HP_MEAN

IF( DefaultPlot%Category == NOT_SET_I )DefPlt%Category = HP_SURF

IF( TRIM(DefPlt%ClassStr) == 'Default' )THEN
  DefPlt%ClassStr = 'Surface Dosage'
END IF

IF( TRIM(DefPlt%ChoiceStr) == 'Default' )THEN
  DefPlt%ChoiceStr  = ' '
END IF

IF( TRIM(DefPlt%KindStr) == 'Default' )THEN
  DefPlt%KindStr = 'Total'
END IF

!==============================================================================
! Check to see if Current values are available
!==============================================================================

HaveCurrent = ALLOCATED(ClassStr) .AND. ALLOCATED(ChoiceStr) .AND. ALLOCATED(KindStr)
HaveCurrent = HaveCurrent .AND. InRange( 1,HP_NUMCAT,PlotDef(indx)%Field%Category )
HaveCurrent = HaveCurrent .AND. InRange( 1,nPltClass,PlotDef(indx)%Field%Class )

!==============================================================================
! Save Current values (as default if not available)
!==============================================================================

CurPlt = DefPlt
IF( HaveCurrent )THEN
  nPltType = HP_NUMTYP
  CurPlt%Category = PlotDef(indx)%Field%Category
  CurPlt%ClassStr = ClassStr(PlotDef(indx)%Field%Class)%string
  IF( InRange(1,nPltChoice,PlotDef(indx)%Field%Choice) )THEN
    CurPlt%ChoiceStr = ChoiceStr(PlotDef(indx)%Field%Choice)%string
  END IF
  IF( InRange(1,nPltKind,PlotDef(indx)%Field%Kind) )THEN
    CurPlt%KindStr = KindStr(PlotDef(indx)%Field%Kind)%string
  END IF
  IF( InRange(1,nPltType,PlotDef(indx)%Type) )THEN
    CurPlt%Type = PlotDef(indx)%Type
  END IF
END IF

!==============================================================================
! Reset available lists
!==============================================================================

IF( LEN_TRIM(filename) > 0 )THEN
  CALL init_plot_choice( filename,SyncMode )
  IF( hasError() )GOTO 9999
  IF( .NOT.SyncMode )THEN
    IF( BTEST(project(BASE_LEVEL)%Mode,REVERSE_MODE) )THEN
      project(BASE_LEVEL)%Plot = nPltClass > 0 .AND. MAX( nTimePuff,nTimeSrf ) > 0
    ELSE
      project(BASE_LEVEL)%Plot = nPltClass > 0 .AND. MAX( nTimePuff,nTimeSrf,nTimeMet ) > 0
    END IF
    IF( .NOT.project(BASE_LEVEL)%Plot )GOTO 9999
  END IF
END IF

!==============================================================================
! Set Project defaults
!==============================================================================

CALL SetPlotChoice( PlotDef(DEFAULT_LEVEL),DefPlt,DefPlt )
IF( LEN_TRIM(filename) > 0 )THEN
  CALL SplitName( filename,PlotDef(DEFAULT_LEVEL)%Field%Project,PlotDef(DEFAULT_LEVEL)%Field%Path )
  CALL RemoveExtension( PlotDef(DEFAULT_LEVEL)%Field%Project )
END IF

!==============================================================================
! Set Type if appropriate
!==============================================================================

nPltType = HP_NUMTYP
IF( CatClassArray(PlotDef(DEFAULT_LEVEL)%Field%Category,PlotDef(DEFAULT_LEVEL)%Field%Class)%type == SCIPtrue )THEN
  IF( InRange(1,nPltType,DefPlt%Type) )THEN
    PlotDef(DEFAULT_LEVEL)%Type = DefPlt%Type
  ELSE
    PlotDef(DEFAULT_LEVEL)%Type = HP_MEAN
  END IF
END IF

!==============================================================================
! Set Current if available requested
!==============================================================================

IF( indx /= DEFAULT_LEVEL )THEN
  DefPlt%Category  = PlotDef(DEFAULT_LEVEL)%Field%Category
  DefPlt%Type      = PlotDef(DEFAULT_LEVEL)%Type
  DefPlt%ClassStr  = ClassStr(PlotDef(DEFAULT_LEVEL)%Field%Class)%string
  DefPlt%ChoiceStr = ChoiceStr(PlotDef(DEFAULT_LEVEL)%Field%Choice)%string
  IF( InRange(1,nPltKind,PlotDef(DEFAULT_LEVEL)%Field%Kind) )THEN
    DefPlt%KindStr = KindStr(PlotDef(DEFAULT_LEVEL)%Field%Kind)%string
  ELSE
    DefPlt%KindStr = 'empty'
  END IF
  CALL SetPlotChoice( PlotDef(indx),CurPlt,DefPlt )
  PlotDef(indx)%Field%Project = PlotDef(DEFAULT_LEVEL)%Field%Project
  PlotDef(indx)%Field%Path    = PlotDef(DEFAULT_LEVEL)%Field%Path

!==============================================================================
! Set Type if appropriate
!==============================================================================

  nPltType = HP_NUMTYP
  IF( CatClassArray(PlotDef(indx)%Field%Category,PlotDef(indx)%Field%Class)%type == SCIPtrue )THEN
    IF( InRange(1,nPltType,CurPlt%Type) )THEN
      PlotDef(indx)%Type = CurPlt%Type
    ELSE IF( InRange(1,nPltType,DefPlt%Type) )THEN
      PlotDef(indx)%Type = DefPlt%Type
    ELSE
      PlotDef(indx)%Type = HP_MEAN
    END IF
  END IF

END IF

!==============================================================================
! Free plot fields
!==============================================================================

CALL FreePlotField( 0 )

!==============================================================================
! Set default contour type (SCIP vs USER)
!==============================================================================
PlotDef(indx)%ContourIndex = USER_CONTOUR

9999 CONTINUE

RETURN
END
!***********************************************************************
!***********************************************************************
!
!***********************************************************************
!***********************************************************************
SUBROUTINE ResetTerrainField()

USE pltchoice_fi
USE pcscipuf_fi
USE param_fd

IMPLICIT NONE

INTEGER DrawMode
INTEGER i

TYPE( PlotChoice ) TerPlt

DrawMode = ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode
CALL ResetContours( TER_CONTOUR,BASE_LEVEL )
CALL FreePlotField( TER_INDEX )

TerPlt%ClassStr  = 'Met/Terrain'
TerPlt%ChoiceStr = 'Terrain'
DO i = 1,nPltChoice
  IF( INDEX(TRIM(ChoiceStr(i)%string),TRIM(TerPlt%ChoiceStr)) > 0 )THEN
    TerPlt%ChoiceStr = TRIM(ChoiceStr(i)%string)
  END IF
END DO
TerPlt%KindStr   = 'empty'

TerPlt%Category  = HP_SURF
TerPlt%Type      = HP_MEAN

IF( project(BASE_LEVEL)%Plot )THEN
  CALL SetPlotChoice( PlotTer,TerPlt,TerPlt )
  PlotTer%Field%Project = PlotDef(DEFAULT_LEVEL)%Field%Project
  PlotTer%Field%Path    = PlotDef(DEFAULT_LEVEL)%Field%Path
ELSE
  PlotTer%Field%Category = NOT_SET_I
END IF

IF( PlotTer%Field%Category /= TerPlt%Category )THEN
  ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = PLOT_NULL
ELSE IF( TRIM(ClassStr(PlotTer%Field%Class)%string) /= TRIM(TerPlt%ClassStr) )THEN
  ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = PLOT_NULL
ELSE
  IF( TRIM(ChoiceStr(PlotTer%Field%Choice)%string) /= TRIM(TerPlt%ChoiceStr) )THEN
    ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = PLOT_NULL
  ELSE
    IF( DrawMode == PLOT_NULL )THEN
      ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%DrawMode
    ELSE
      ContourList(TER_CONTOUR,BASE_LEVEL)%ListHdr%DrawMode = DrawMode
	  END IF
  END IF
END IF

RETURN
END
!***********************************************************************
!***********************************************************************
!
!***********************************************************************
!***********************************************************************
SUBROUTINE SetPlotChoice( PltField,CurPlt,DefPlt )

USE pltchoice_fi
USE pcscipuf_fi
USE param_fd
USE default_fd
USE SCIPtool

IMPLICIT NONE

TYPE( PlotChoice ) CurPlt, DefPlt
TYPE( PlotField  ) PltField

!INTEGER nPltType
LOGICAL InRange
INTEGER i
INTEGER j
INTEGER irv
INTEGER userID

userID = 9999

PltField%Field%Category = NOT_SET_I
PltField%Field%Class    = NOT_SET_I

!==============================================================================
! Check Current Class string
!==============================================================================
DO i = 1,nPltClass
  IF( TRIM(CurPlt%ClassStr) == TRIM(ClassStr(i)%string) )THEN
    IF( CatClassArray(CurPlt%Category,i)%available == SCIPtrue )THEN
      PltField%Field%Category = CurPlt%Category
      PltField%Field%Class    = i
      EXIT
    END IF
  END IF
END DO

!==============================================================================
! Current Class string not avaialable : Use Default
!==============================================================================
IF( PltField%Field%Category == NOT_SET_I )THEN
  DO i = 1,nPltClass
    IF( TRIM(DefPlt%ClassStr) == TRIM(ClassStr(i)%string) )THEN
      IF( CatClassArray(DefPlt%Category,i)%available == SCIPtrue )THEN
        PltField%Field%Category = DefPlt%Category
        PltField%Field%Class    = i
        EXIT
      END IF
    END IF
  END DO
END IF

!==============================================================================
! Default Class string not avaialable : Check for Default Category
!==============================================================================
IF( PltField%Field%Category == NOT_SET_I )THEN
  DO i = 1,nPltClass
    IF( CatClassArray(DefPlt%Category,i)%available == SCIPtrue )THEN
  	  PltField%Field%Category = DefPlt%Category
  	  PltField%Field%Class    = i
  	  EXIT
  	END IF
  END DO
END IF

!==============================================================================
! Default Category not available : Select first available
!==============================================================================
IF(PltField%Field%Category == NOT_SET_I)THEN
CAT:DO j = 1,HP_NUMCAT
CLS:  DO i = 1,nPltClass
      IF( CatClassArray(j,i)%available == SCIPtrue )THEN
  	    PltField%Field%Category = j
  	    PltField%Field%Class    = i
  	    EXIT CAT
  	  END IF
    END DO CLS
  END DO CAT
END IF

!==============================================================================
! Set Current Choice and Kind if valid Current Class (Should always be)
!==============================================================================
IF( InRange(1,nPltClass,PltField%Field%Class) )THEN
  PltField%Field%Choice  = NOT_SET_I

!==============================================================================
! Check for Current Choice string
!==============================================================================
  DO i = 1,nPltChoice
    IF( TRIM(CurPlt%ChoiceStr) == TRIM(ChoiceStr(i)%string) )THEN
      IF( BTEST(ClassChoiceArray(PltField%Field%Class,i)%available,HPB_AVAILABLE) )THEN
        PltField%Field%Choice = i
        EXIT
      END IF
    END IF
  END DO

!==============================================================================
! Current Choice string not available : Use Default
!==============================================================================
  IF( PltField%Field%Choice == NOT_SET_I )THEN
    DO i = 1,nPltChoice
      IF( TRIM(DefPlt%ChoiceStr) == TRIM(ChoiceStr(i)%string) )THEN
        IF( BTEST(ClassChoiceArray(PltField%Field%Class,i)%available,HPB_AVAILABLE) )THEN
          PltField%Field%Choice = i
          EXIT
        END IF
      END IF
    END DO
  END IF

!==============================================================================
! Default Choice string also not available : Use first available
!==============================================================================
  IF( PltField%Field%Choice == NOT_SET_I )THEN
    DO i = 1,nPltChoice
      IF( BTEST(ClassChoiceArray(PltField%Field%Class,i)%available,HPB_AVAILABLE) )THEN
        PltField%Field%Choice = i
        EXIT
      END IF
    END DO
  END IF

!==============================================================================
! Set Current Kind if Class/Choice has kinds
!==============================================================================
  PltField%Field%Kind = NOT_SET_I
  IF( ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%kind == SCIPtrue )THEN

!==============================================================================
! Check for Current Kind string
!==============================================================================
    DO i = 1,ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%nkind
      j = ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%ikind + i - 1
      IF( TRIM(CurPlt%KindStr) == TRIM(KindStr(j)%string) )THEN
        PltField%Field%Kind = j
        EXIT
      END IF
    END DO

!==============================================================================
! Current Kind not available : Use Default
!==============================================================================
    IF( PltField%Field%Kind == NOT_SET_I )THEN
      DO i = 1,ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%nkind
        j = ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%ikind + i - 1
        IF( TRIM(DefPlt%KindStr) == TRIM(KindStr(j)%string) )THEN
          PltField%Field%Kind = j
          EXIT
        END IF
      END DO
    END IF

!==============================================================================
! Default Kind string not available : Take first available
!==============================================================================
    IF( PltField%Field%Kind == NOT_SET_I )THEN
      PltField%Field%Kind = ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%ikind
    END IF
  END IF

!==============================================================================
! Invalid Current Class selection (Should never be) : Use first available
!==============================================================================
ELSE
  PltField%Field%Class = 1
  DO i = 1,nPltChoice
    IF( BTEST(ClassChoiceArray(PltField%Field%Class,i)%available,HPB_AVAILABLE) )THEN
     PltField%Field%Choice = i
     EXIT
   END IF
  END DO
  PltField%Field%Kind    = NOT_SET_I
  IF( ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%kind == SCIPtrue )THEN
  	PltField%Field%Kind = ClassChoiceArray(PltField%Field%Class,PltField%Field%Choice)%ikind
  END IF
END IF

!==============================================================================
! Set plot create flag to false to guarentee field is created
!==============================================================================

irv = SCIPDeleteField( userID, FieldID(FLD_INDEX) )
PltField%Created = .FALSE.

RETURN
END
!***********************************************************************
!               InRange
!***********************************************************************
LOGICAL FUNCTION InRange( lower,upper,var )

IMPLICIT NONE

INTEGER var,lower,upper

InRange = var >= lower .AND. var <= upper

RETURN
END
!***********************************************************************
!               GetInfoScn
!***********************************************************************
SUBROUTINE getinfo_scn( iwnd_db,file_read,lok,rel,mat )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE SCIAPIversion_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE errorParam_fd
USE GUImatl_fi
USE GUItool_fi
USE dialog_fi
USE SwinAPI

!     Read .scn file and build release list

IMPLICIT NONE

INTEGER(POINTER_LEN)     iwnd_db !Window handle
CHARACTER(*)             file_read !File to read
LOGICAL                  lok !Return flag
TYPE( reldef_str )       rel
TYPE( matdef_str )       mat

INTEGER id,idlm,nrel_start,nignore,ios
LOGICAL lzero,lfirst,lediting,load_project,load_rel
CHARACTER(PATH_MAXLENGTH) file_save
INTEGER ntotal,i,irv

TYPE( reldef_str ) GUIscn

TYPE( ProjectStructure ) prjdlg

TYPE( fileNameT ) fileT

INTEGER nError
CHARACTER(128) eMessage,eInform,eAction,eRoutine
INTEGER nMC
INTEGER nrel


LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

nError   = NO_ERROR
eMessage = ' '
eInform  = ' '
eAction  = ' '
eRoutine = 'GetScnInfo'

!---- Set file_scn to file being read for error messages from get_scn

file_save = file_scn
file_scn  = file_read

!---- Initialize return flag

lok    = .FALSE.
lzero  =  rel%nrel == 0
lfirst = .TRUE.
CALL FindHwndList( iwnd_db,id,idlm )
load_project = id == IDB_EDTPRJ
load_rel     = id == IDB_RELDEF
lediting     = load_project .OR. load_rel
nignore = 0

!---- Open the file (unit lun_scn)

1000  CONTINUE

!---- Get file size
fileT%string = file_scn
irv = SCIPCountReleaseMC(ToolCallerID,fileT,nrel,nMC)
CALL AllocateRelList(nrel,nMC)
IF( hasError() )GOTO 9999

nrel_start = rel%nrel
ntotal     = nrel_start

reldef%control%mode     = SCIPnull
reldef%control%searchID = ' '

CALL SplitName( file_scn,string1,prjdlg%ID%path )
CALL SplitExtension( string1,prjdlg%ID%name,reldef%control%fileExtension )

reldef%control%mode = IBSET(reldef%control%mode,HCB_FILE)
ALLOCATE( GUIscn%release(nrel),STAT=ios )
IF( ios /= 0 )THEN
  nError = SZ_ERROR
  eMessage = 'Allocation error : GUIscn '
  WRITE(eInform,*)'Request=',MAXREL,' : ErrorCode=',irv
  CALL SetError(nError,TRIM(eMessage),TRIM(eInform),' ',TRIM(eRoutine))
  GOTO 9999
END IF
DO i = 1,nrel
  NULLIFY( GUIscn%release(i)%mc )
END DO
CALL CopyScenario( scenario(DEFAULT_LEVEL),GUIscn )
CALL GUI_SCIP_scenario( prjdlg,GUIscn,mat,reldef,relList,SIZE(relList),relMCList,nMC )
IF( hasError() )GOTO 9999

i = SetCursor( hcur_wait ) !  Set Arrow
IF( nMC > 0 )THEN
  irv = SCIPLoadReleaseMCF( ToolCallerID,reldef,relList,relMCList )
ELSE
  irv = SCIPLoadReleaseF( ToolCallerID,reldef,relList )
END IF
IF( irv == SCIPfailure )THEN
  CALL GetToolError( eRoutine )
  IF( lastError() == SZ_ERROR )THEN
    GOTO 9100
  ELSE
!    CALL SetError(nError,TRIM(eMessage),TRIM(eInform),TRIM(eAction),TRIM(eRoutine))
    GOTO 9999
  END IF
END IF

i = SetCursor( hcur_arrow ) !  Set Arrow

lok = irv == SCIPsuccess

CALL SCIP_GUI_scenario( GUIscn,mat,reldef,relList,nMC,relMCList )
IF( ALLOCATED(relMCList) )DEALLOCATE(relMCList,STAT=ios)
IF( hasError() )THEN
  lfirst = .FALSE.
  GOTO 9999
END IF
IF( (rel%nrel+GUIscn%nrel) > MAXREL )THEN
  CALL ReallocateScenario(rel%nrel+GUIscn%nrel)
  IF( hasError() )THEN
    lfirst = .FALSE.
    GOTO 9999
  END IF
END IF

DO i = 1,GUIscn%nrel
  IF( rel%nrel >= MAXREL )GOTO 9100
  ntotal = ntotal + 1
  IF( rel%nrel < MAXREL )THEN
    CALL add_current_release( GUIscn%release(i),rel%release,rel%nrel,id )
  END IF
END DO

!---- Done

9999  CONTINUE

IF( ASSOCIATED(GUIscn%release) )THEN
  DO i = 1,GUIscn%nrel
    IF( ASSOCIATED( GUIscn%release(i)%mc ) )DEALLOCATE( GUIscn%release(i)%mc,STAT=irv )
  END DO
  DEALLOCATE( GUIscn%release,STAT=ios )
  NULLIFY( GUIscn%release)
END IF

IF( ntotal > MAXREL )THEN
  GOTO 9100
END IF

IF( rel%nrel == nrel_start .AND. lfirst )THEN
  nError = RD_ERROR
  eMessage = 'File contains no valid release data'
  CALL ReportFileName( eInform,'File=',file_scn )
  eAction  = 'Check file for valid namelist structures'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF
IF( hasError() )CALL ShowInfoMessage(iwnd_db)

lok = rel%nrel > nrel_start !Return flag

!---- put file_scn back to original value

file_scn  = file_save

RETURN

9100 CONTINUE
IF( lfirst .AND. lediting )THEN
  lfirst = .FALSE.
  nError = WN_ERROR
  eMessage = 'File contains more releases than allowed in this version'
  WRITE(eInform,*)'Maximum allowed is',MAXREL
  eAction  = 'Try using a larger version of SCIP'
  CALL SetError( nError,eMessage,eInform,eAction,eRoutine )
END IF
GOTO 9999

END
