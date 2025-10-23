!***********************************************************************
!                set_default
!***********************************************************************
SUBROUTINE set_default( hInstance,hWindow )

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE errorParam_fd
USE SwinAPI
USE datums

!     This routine initializes common areas

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: hInstance !Instance handle
INTEGER(POINTER_LEN), INTENT( IN ) :: hWindow   !Window handle

INTEGER i,kversion,jversion,iscx,iscy

LOGICAL, EXTERNAL :: hasError

!------ Initialize SAG linked list of grid structures

MyAppInst = hInstance
hwnd_mw   = hWindow
hwnd_tb   = 0
hwnd_pw   = 0
hwnd_db   = 0
hwnd_sw   = 0

!---- Initialize Error flags

CALL InitError()

!---- Initialize Dialog Box lists

DO i = 1,MAX_DLEV
  hwnd_list(1,i) = 0
  hwnd_list(2,i) = 0
END DO

!---- Set System type

kversion = GetVersion()
jversion = IAND( kversion,Z'80000000' )
lsysNT   = jversion == 0

!---- Set SCREEN Size

iscx = GetSystemMetrics( SM_CXSCREEN )
iscy = GetSystemMetrics( SM_CYSCREEN )
IF( iscx < 1024 .OR. iscy < 768 )THEN
  lVGA = .TRUE.
ELSE
  lVGA = .FALSE.
END IF

!---- Initialize Common areas

CALL InitNCARGraphics( MyAppInst )
CALL GetNCARPalette( i )
IF( i == NULL )THEN
  WRITE(string1,*)'Error code = ',GetLastError()
  CALL SetError( IV_ERROR,'Unable to create palette',string1,' ','InitNCARGraphics' )
  GOTO 9999
END IF
CALL init_erfci()
CALL InitMapFileIndex()

UTM_init = InitUTM( ini_file )
IF( .NOT.UTM_init )THEN
  CALL SetError( IV_ERROR, &
                'Failed to initialize UTM transforms - Check installation', &
                'Creation of UTM projects will be disabled', &
                'Plotting of UTM projects will be approximate', &
                'initUTM' )
  CALL ShowErrorMessage( hwnd_mw,.TRUE. )
END IF

CALL init_param()
CALL set_default_interface()
CALL set_default_new()
IF( hasError() )GOTO 9999
CALL set_default_contri()

CALL init_seed( MyAppInst,ran_seed )

9999 CONTINUE

RETURN
END
!***********************************************************************
!                set_default_interface
!***********************************************************************
SUBROUTINE set_default_interface()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE create_fi
USE param_fd
USE errorParam_fd
USE GUItool_fi
USE printdev_fi

IMPLICIT NONE

!     This routine sets the default parameters for the SCIPUFF interface

INTEGER irv

CHARACTER(80) saveClass,saveAnalyst

!---- Interface Flags

lpplot   = .TRUE.  !Print PLOT flag
lpbare   = .FALSE. !Print NO LABELS flag
lpstat   = .TRUE.  !Print STAT flag
lpclass  = .TRUE.  !Print CLASS flag
lpdate   = .TRUE.  !Print DATE flag
Play     = .FALSE. !Play an animation sequence
AnimDel  = .FALSE. !Delete animation sequence
lpmtime  = .FALSE. !Print Multiple times flag
nPrintTimes = 0    !Number of times to print

!      linitpd  = .false.     !Print Init Device flag
pd%hDevMode  = 0
pd%hDevNames = 0

lplotOK  = .FALSE. !OK to PLOT Flag
lprintOK = .FALSE. !Print sucessful flag

!---- NULL Character

cnull = CHAR(0)

!---- Dialog Box Resource names

DialogName(IDB_PLOT   -DIALOG_BASE) = 'PlotDialog'
DialogName(IDB_EDTPRJ -DIALOG_BASE) = 'NewDialog'
DialogName(IDB_SCIPUF -DIALOG_BASE) = 'RunDialog'
DialogName(IDB_CONTOUR-DIALOG_BASE) = 'ContourDialog'
DialogName(IDB_SLICE  -DIALOG_BASE) = 'SliceDialog'
DialogName(IDB_ZOOM   -DIALOG_BASE) = 'ZoomDialog'
DialogName(IDB_OPTIONS-DIALOG_BASE) = 'SetupDialog'
DialogName(IDB_MATDEF -DIALOG_BASE) = 'MaterialDialog'
DialogName(IDB_RELDEF -DIALOG_BASE) = 'ReleaseDialog'
DialogName(IDB_METDEF -DIALOG_BASE) = 'MeteorologyDialog_SCM'
DialogName(IDB_PLTOPT -DIALOG_BASE) = 'OptionsDialog'
DialogName(IDB_MAPS   -DIALOG_BASE) = 'MapDialog'
DialogName(IDB_AXES   -DIALOG_BASE) = 'AxesDialog'
DialogName(IDB_LABELS -DIALOG_BASE) = 'LabelsDialog'
DialogName(IDB_MATNEW -DIALOG_BASE) = 'NewMatlDialog'
DialogName(IDB_EDTLST -DIALOG_BASE) = 'EditListDialog'
DialogName(IDB_NEWLST -DIALOG_BASE) = 'EditListDialog'
DialogName(IDB_COMLST -DIALOG_BASE) = 'ComputeListDialog'
DialogName(IDB_SUMMARY-DIALOG_BASE) = 'SummaryDialog'
DialogName(IDB_RELNEW -DIALOG_BASE) = 'NewRelDialog'
DialogName(IDB_TIME   -DIALOG_BASE) = 'TimeDialog'
DialogName(IDB_DOMAIN -DIALOG_BASE) = 'DomainDialog'
DialogName(IDB_PRJDEF -DIALOG_BASE) = 'ProjectDialog'
DialogName(IDB_AUDIT  -DIALOG_BASE) = 'AuditDialog'
DialogName(IDB_DELPRJ -DIALOG_BASE) = 'DeleteDialog'
DialogName(IDB_ANIMAT -DIALOG_BASE) = 'AnimateDialog'
DialogName(IDB_PLAY   -DIALOG_BASE) = 'PlayDialog'
DialogName(IDB_LIQPARM-DIALOG_BASE) = 'LiquidParamDialog'
DialogName(IDB_RNDPARM-DIALOG_BASE) = 'RandomParamDialog'
DialogName(IDB_TERPARM-DIALOG_BASE) = 'TerrainParamDialog'
DialogName(IDB_SETUP  -DIALOG_BASE) = 'NewSetupDialog'
DialogName(IDB_ERRORBX-DIALOG_BASE) = 'SpecialErrorDialog'
DialogName(IDB_PICK   -DIALOG_BASE) = 'PickDialog'
DialogName(IDB_PLTMORE-DIALOG_BASE) = 'MoreOptionsDialog'
DialogName(IDB_MULTICOMP-DIALOG_BASE) = 'MulticompDialog'
DialogName(IDB_XYTABLE-DIALOG_BASE) = 'XyTableDialog'
DialogName(IDB_PRTPARM-DIALOG_BASE) = 'PartParamDialog'
DialogName(IDB_GASPARM-DIALOG_BASE) = 'GasParamDialog'
DialogName(IDB_RESTART-DIALOG_BASE) = 'RestartDialog'
DialogName(IDB_AERPARM-DIALOG_BASE) = 'AerosolParamDialog'
DialogName(IDB_UNCPARM-DIALOG_BASE) = 'UncertaintyParamDialog'
DialogName(IDB_OBSERVE-DIALOG_BASE) = 'ObserverDialog'

!---- Initialize Project/File names

irv = SCIPDefaultFlagsF( ToolCallerID,flags )

irv = SCIPDefaultCtrlF( ToolCallerID,ctrl )

saveClass   = TRIM(project(DEFAULT_LEVEL)%audit%Classification)
saveAnalyst = TRIM(project(DEFAULT_LEVEL)%audit%Analyst)
CALL SCIP_GUI_flags( project(DEFAULT_LEVEL),flags )
CALL SCIP_GUI_ctrl( project(DEFAULT_LEVEL),ctrl )

IF( LEN_TRIM(saveClass)   > 0 )project(DEFAULT_LEVEL)%audit%Classification = saveClass
IF( LEN_TRIM(saveAnalyst) > 0 )project(DEFAULT_LEVEL)%audit%Analyst        = saveAnalyst

project(DEFAULT_LEVEL)%OK         = .FALSE.
project(DEFAULT_LEVEL)%Run        = .FALSE.
project(DEFAULT_LEVEL)%Plot       = .FALSE.
project(DEFAULT_LEVEL)%Edit       = .FALSE.
project(DEFAULT_LEVEL)%Puff       = .FALSE.
project(DEFAULT_LEVEL)%Grid       = .FALSE.
project(DEFAULT_LEVEL)%Terrain    = .FALSE.
project(DEFAULT_LEVEL)%MapCoord   = I_LATLON
project(DEFAULT_LEVEL)%Ref        = .FALSE.
project(DEFAULT_LEVEL)%Weather    = 0
project(DEFAULT_LEVEL)%ID%name    = ' '
project(DEFAULT_LEVEL)%ID%ID      = NOT_SET_I
project(DEFAULT_LEVEL)%ID%version = NOT_SET_I

!     Set current project to Default project

project(BASE_LEVEL) = project(DEFAULT_LEVEL)
LastMap             = project(BASE_LEVEL)%MapCoord == I_LATLON

!---- SAVE File Types

nsavext = 11
isavasc = 1
isavarc = 2
isavavs = 3
isavbmp = 4
isavcts = 5
isaveis = 6
isavoil = 7
isavusa = 8
isavsci = 9
isavwmf = 10
isavtab = 11
isavbas = isavarc
savext(isavasc,1) = 'Text (ASCII) file'
savext(isavarc,1) = 'ARCVIEW Overlay file'
savext(isavavs,1) = 'AVS (ASCII) file'
savext(isavbmp,1) = 'Bitmap file'
savext(isavwmf,1) = 'PCX file'
savext(isavcts,1) = 'CTS Overlay file'
savext(isavsci,1) = 'SCIPUFF Overlay file'
savext(isavtab,1) = 'Table (ASCII) file'
savext(isavoil,1) = 'OILSTOCK Overlay file'
savext(isaveis,1) = 'EIS Plume file'
savext(isavusa,1) = 'Street Atlas USA®'
savext(isavasc,2) = '*.asc'
savext(isavarc,2) = '*.arc'
savext(isavavs,2) = '*.avs'
savext(isavbmp,2) = '*.bmp'
savext(isavwmf,2) = '*.pcx'
savext(isavcts,2) = '*.asc'
savext(isavsci,2) = '*.ovl'
savext(isavtab,2) = '*.tab'
savext(isavoil,2) = '*.oil'
savext(isaveis,2) = '*.plm'
savext(isavusa,2) = '*.usa'

nxytab = 0

ALLOCATE( xytab(2,maxList),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(string1,*)'Requested size =',maxList
  CALL SetError( UK_ERROR, &
                'Failed to allocate space for Tabular output', &
                 string1, &
                'Try reducing requested maximum tabel size', &
                'SetDefaultInterface' )
  GOTO 9999
END IF

ALLOCATE( dblst(MAX_DLEV,maxList),STAT=irv )
IF( irv /= 0 )THEN
  WRITE(string1,*)'Requested size =',maxList
  CALL SetError( UK_ERROR, &
                'Failed to allocate space for list box values', &
                 string1, &
                'Try reducing requested maximum tabel size', &
                'SetDefaultInterface' )
  GOTO 9999
END IF

isave_format = isavsci - isavbas + 1 !Save as SCIPUFF overlay

project_setup = .FALSE.

9999 CONTINUE

RETURN
END
!***********************************************************************
!                set_default_plot
!***********************************************************************
SUBROUTINE set_default_plot( filename )

USE files_fi
USE plotdlg_fi
USE pltchoice_fi
USE PlotOpt_fd
USE param_fd
USE PlotTrans_fi
USE GUIparam_fd

IMPLICIT NONE

!==============================================================================
! Function arguments
!==============================================================================
CHARACTER(*), INTENT( IN ) :: filename

!==============================================================================
! Local variables
!==============================================================================

INTEGER i
INTEGER j

!==============================================================================
! Plot Definition
!==============================================================================
CALL FreePlotField( 0 )

!==============================================================================
! Plot Choice strings
!==============================================================================
DefaultPlot%ClassStr    = 'Default'
DefaultPlot%ChoiceStr   = 'Default'
DefaultPlot%KindStr     = 'Default'
DefaultPlot%Type        = NOT_SET_I
DefaultPlot%Category    = NOT_SET_I

!==============================================================================
! Plot Definiton Field
!==============================================================================
PlotDef(DEFAULT_LEVEL)%Field%Category   = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Class      = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Choice     = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Kind       = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%TimeID     = DEF_VAL_I
PlotDef(DEFAULT_LEVEL)%Field%MaxCells   = 0
PlotDef(DEFAULT_LEVEL)%Field%MaxLev     = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Resolution = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%UserTime   = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Units      = ' '
PlotDef(DEFAULT_LEVEL)%Field%Project    = ' '
PlotDef(DEFAULT_LEVEL)%Field%Path       = ' '

!==============================================================================
! Plot Definiton Field Coordinates
!==============================================================================
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%Mode          = HD_LATLON
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%UTMZone       = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%Reference%x   = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%Reference%y   = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%Reference%lat = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%Reference%lon = NOT_SET_R

PlotDef(DEFAULT_LEVEL)%Field%Coordinate%VertSlice%Resolution = NOT_SET_I
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%VertSlice%StartPt%x  = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%VertSlice%StartPt%y  = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%Coordinate%VertSlice%EndPt      = PlotDef(DEFAULT_LEVEL)%Field%Coordinate%VertSlice%StartPt
PlotDef(DEFAULT_LEVEL)%Field%coordinate%horzSlice%height     = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%Field%coordinate%horzSlice%mode       = NOT_SET_I

!==============================================================================
! Plot Definiton Class Data
!==============================================================================
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%xmin = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%xmax = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%ymin = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%ymax = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%zmin = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%zmax = DEF_VAL_R
PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)%zres = 20.0		 !Integer - Number of grid points

PlotDef(DEFAULT_LEVEL)%ClassData(HSLICE_INDEX) = PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)
PlotDef(DEFAULT_LEVEL)%ClassData(HSLICE_INDEX)%zres = NOT_SET_R
PlotDef(DEFAULT_LEVEL)%ClassData(HSLICE_INDEX)%zmin = 0.0

!==============================================================================
! Plot Definiton Type and Type data
!==============================================================================
PlotDef(DEFAULT_LEVEL)%Type = HP_MEAN
PlotDef(DEFAULT_LEVEL)%TypeData(HP_MEAN   ) = 10.0 !Table - Risk Level
PlotDef(DEFAULT_LEVEL)%TypeData(HP_PROB   ) = 0.0  !Exceed level
PlotDef(DEFAULT_LEVEL)%TypeData(HP_EXCEED ) = 50.0 !Probability

!==============================================================================
! Other Plot Definiton parameters
!==============================================================================
PlotDef(DEFAULT_LEVEL)%Mode          = PLOT_FILL
PlotDef(DEFAULT_LEVEL)%ContourIndex  = USER_CONTOUR
PlotDef(DEFAULT_LEVEL)%Created       = .FALSE.

!==============================================================================
! Terrain Plot Definiton Field
!==============================================================================
PlotTer%Field%Category   = HP_SURF
PlotTer%Field%Class      = NOT_SET_I
PlotTer%Field%Choice     = NOT_SET_I
PlotTer%Field%Kind       = NOT_SET_I
PlotTer%Field%TimeID     = NOT_SET_I
PlotTer%Field%MaxCells   = 0
PlotTer%Field%MaxLev     = NOT_SET_I
PlotTer%Field%Resolution = NOT_SET_R
PlotTer%Field%UserTime   = NOT_SET_R
PlotTer%Field%Units      = ' '
PlotTer%Field%Project    = ' '
PlotTer%Field%Path       = ' '

!==============================================================================
! Terrain Plot Definiton Class Data
!==============================================================================
PlotTer%ClassData(VSLICE_INDEX)%xmin = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%xmax = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%ymin = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%ymax = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%zmin = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%zmax = NOT_SET_R
PlotTer%ClassData(VSLICE_INDEX)%zres = NOT_SET_R

PlotTer%ClassData(HSLICE_INDEX) = PlotDef(DEFAULT_LEVEL)%ClassData(VSLICE_INDEX)

!==============================================================================
! Terrain Plot Definiton Type nad Type Data
!==============================================================================
PlotTer%Type = HP_MEAN
PlotTer%TypeDATA(HP_MEAN   ) = NOT_SET_R
PlotTer%TypeDATA(HP_PROB   ) = NOT_SET_R
PlotTer%TypeDATA(HP_EXCEED ) = NOT_SET_R

!==============================================================================
! Other Terrain Plot Definiton Parameters
!==============================================================================
PlotTer%Mode          = PLOT_DRAW
PlotTer%ContourIndex  = TER_CONTOUR
PlotTer%Created       = .FALSE.

!==============================================================================
! Contour definitions
!==============================================================================
!==============================================================================
! Clear List Pointers
!==============================================================================
DO j = DEFAULT_LEVEL,EDIT_LEVEL
  DO i = 1,NUM_CONTOUR
    NULLIFY(ContourList(i,j)%ListPtr)
  END DO
END DO

!==============================================================================
! Terrain contours
!==============================================================================
ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Number    = 0
ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%DrawMode  = PLOT_OFF
ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%LabelMode = PLOT_OFF
ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Scale     = 1.0
ContourList(TER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Unit      = 'default'

ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)%Mode   = PLOT_LIN
ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)%Number = 5
ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)%Cmin   = DEF_VAL_R
ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)%Cmax   = DEF_VAL_R
ContourBuild(TER_CONTOUR,DEFAULT_LEVEL)%Del    = DEF_VAL_R

!==============================================================================
! USER contours
!==============================================================================
ContourList(USER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Number    = 0
ContourList(USER_CONTOUR,DEFAULT_LEVEL)%ListHdr%DrawMode  = PLOT_ON
ContourList(USER_CONTOUR,DEFAULT_LEVEL)%ListHdr%LabelMode = PLOT_OFF
ContourList(USER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Scale     = 1.0
ContourList(USER_CONTOUR,DEFAULT_LEVEL)%ListHdr%Unit      = 'default'

ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)%Mode   = PLOT_DEF
ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)%Number = 6
ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)%Cmin   = DEF_VAL_R
ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)%Cmax   = DEF_VAL_R
ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)%Del    = DEF_VAL_R

!==============================================================================
! Plot Options
!==============================================================================

poptdef(DEFAULT_LEVEL)%Maxlev = 99

popt2def(DEFAULT_LEVEL)%AR   = DEF_VAL_R
popt2def(DEFAULT_LEVEL)%Size = DEF_VAL_R
popt2def(DEFAULT_LEVEL)%X    = DEF_VAL_R
popt2def(DEFAULT_LEVEL)%Y    = DEF_VAL_R
popt2def(DEFAULT_LEVEL)%Font = 1.0

poptdef(DEFAULT_LEVEL)%Max      = .TRUE.
poptdef(DEFAULT_LEVEL)%Cell     = .FALSE.
poptdef(DEFAULT_LEVEL)%Contour  = .TRUE.
poptdef(DEFAULT_LEVEL)%Source   = .FALSE.
poptdef(DEFAULT_LEVEL)%SrcText  = .FALSE.
poptdef(DEFAULT_LEVEL)%ColorBox = .TRUE.
poptdef(DEFAULT_LEVEL)%FillLo   = .FALSE.
poptdef(DEFAULT_LEVEL)%FillHi   = .TRUE.
poptdef(DEFAULT_LEVEL)%Terrain  = .FALSE.
poptdef(DEFAULT_LEVEL)%Weather  = .FALSE.
poptdef(DEFAULT_LEVEL)%KmScale  = .FALSE.
poptdef(DEFAULT_LEVEL)%Overlay  = .FALSE.
poptdef(DEFAULT_LEVEL)%OverlayFile = ' '

!==============================================================================
! Plot Axes
!==============================================================================
axesdef(DEFAULT_LEVEL)%TicX     = 5
axesdef(DEFAULT_LEVEL)%TicY     = 5
axesdef(DEFAULT_LEVEL)%TicH     = 5
axesdef(DEFAULT_LEVEL)%TicV     = 5
axesdef(DEFAULT_LEVEL)%MapCoord = I_LATLON

axesdef(DEFAULT_LEVEL)%Xscale  = 1.
axesdef(DEFAULT_LEVEL)%Yscale  = 1.
axesdef(DEFAULT_LEVEL)%Hscale  = 1.
axesdef(DEFAULT_LEVEL)%Vscale  = 1.
axesdef(DEFAULT_LEVEL)%XscaleP = 1.
axesdef(DEFAULT_LEVEL)%YscaleP = 1.

axesdef(DEFAULT_LEVEL)%ShowX = .TRUE.
axesdef(DEFAULT_LEVEL)%ShowY = .TRUE.
axesdef(DEFAULT_LEVEL)%Cartesian = .FALSE.

axesdef(DEFAULT_LEVEL)%Xformat = '(F9.1)'
axesdef(DEFAULT_LEVEL)%Yformat = '(F9.1)'

axesdef(DEFAULT_LEVEL)%Lat0 = NOT_SET_R
axesdef(DEFAULT_LEVEL)%Lon0 = NOT_SET_R
axesdef(DEFAULT_LEVEL)%X0   = 0.
axesdef(DEFAULT_LEVEL)%Y0   = 0.

DO i = 5,22 !3 sets of 6 xmn,xmx,xsc,ymn,ymx,ysc
  IF( MOD(i-4,3) /= 0 )THEN !To skip Scale factors
    axesdef(DEFAULT_LEVEL)%dbreal(i) = DEF_VAL_R
  END IF
END DO
axesdef(DEFAULT_LEVEL)%Xlab = 'default'
axesdef(DEFAULT_LEVEL)%Ylab = 'default'

!==============================================================================
! Plot Maps
!==============================================================================
mapdef(DEFAULT_LEVEL)%popden = 0.0
mapdef(DEFAULT_LEVEL)%MapsOn = .TRUE.

mapdef(DEFAULT_LEVEL)%MapsOn   = .TRUE.
mapdef(DEFAULT_LEVEL)%HiRes    = .FALSE.
mapdef(DEFAULT_LEVEL)%PPArea   = .TRUE.
mapdef(DEFAULT_LEVEL)%PPText   = .TRUE.
mapdef(DEFAULT_LEVEL)%PPTextLo = .TRUE.

mapdef(DEFAULT_LEVEL)%PPSymbol    = .FALSE.
mapdef(DEFAULT_LEVEL)%Roads       = .FALSE.
mapdef(DEFAULT_LEVEL)%RRs         = .FALSE.
mapdef(DEFAULT_LEVEL)%Airport     = .FALSE.
mapdef(DEFAULT_LEVEL)%NucFac      = .FALSE.
mapdef(DEFAULT_LEVEL)%AirportText = .FALSE.
mapdef(DEFAULT_LEVEL)%NucFacText  = .FALSE.

!==============================================================================
! Map Field Coordinates
!==============================================================================
MapsCoordinate%Mode          = HD_LATLON
MapsCoordinate%UTMZone       = NOT_SET_I
MapsCoordinate%Reference%x   = NOT_SET_R
MapsCoordinate%Reference%y   = NOT_SET_R
MapsCoordinate%Reference%lat = NOT_SET_R
MapsCoordinate%Reference%lon = NOT_SET_R

MapsCoordinate%VertSlice%Resolution = NOT_SET_I
MapsCoordinate%VertSlice%StartPt%x  = NOT_SET_R
MapsCoordinate%VertSlice%StartPt%y  = NOT_SET_R
MapsCoordinate%VertSlice%EndPt      = MapsCoordinate%VertSlice%StartPt
MapsCoordinate%horzSlice%height     = NOT_SET_R
MapsCoordinate%horzSlice%mode       = NOT_SET_I

!==============================================================================
! Plot Titles
!==============================================================================
DO i = 1,3
  ttldef(DEFAULT_LEVEL)%Show(i) = .TRUE.
END DO

ttldef(DEFAULT_LEVEL)%ShowDate = .TRUE.
ttldef(DEFAULT_LEVEL)%ShowTime = .TRUE.

DO i = 1,6
  ttldef(DEFAULT_LEVEL)%dbreal(i) = DEF_VAL_R
END DO
DO i = 1,3
  ttldef(DEFAULT_LEVEL)%string(i) = 'default'
END DO

!==============================================================================
! Override defaults with PlotOptions file
!==============================================================================
IF( filename /= ' ' )CALL read_plotopt( 0,DEFAULT_OPTION,filename )

!==============================================================================
! Initialize current and select structures to default
!==============================================================================
PlotDef(BASE_LEVEL) = PlotDef(DEFAULT_LEVEL)
PlotDef(EDIT_LEVEL) = PlotDef(DEFAULT_LEVEL)
DO i = 1,NUM_CONTOUR
  ContourList (i,BASE_LEVEL) = ContourList (i,DEFAULT_LEVEL)
  ContourBuild(i,BASE_LEVEL) = ContourBuild(i,DEFAULT_LEVEL)
  ContourList (i,EDIT_LEVEL) = ContourList (i,DEFAULT_LEVEL)
  ContourBuild(i,EDIT_LEVEL) = ContourBuild(i,DEFAULT_LEVEL)
END DO

poptdef(BASE_LEVEL)  = poptdef(DEFAULT_LEVEL)
popt2def(BASE_LEVEL) = popt2def(DEFAULT_LEVEL)
axesdef(BASE_LEVEL)  = axesdef(DEFAULT_LEVEL)
mapdef(BASE_LEVEL)   = mapdef(DEFAULT_LEVEL)
ttldef(BASE_LEVEL)   = ttldef(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_zoom
!***********************************************************************
SUBROUTINE set_default_zoom()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd

USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi

!     This routine sets the default parameters for the ZOOM Dialog

IMPLICIT NONE

INTEGER i
LOGICAL LatLon,Cartesian

REAL, DIMENSION(4) :: rtmp

axesdef(BASE_LEVEL)%dbreal(17) = axesdef(DEFAULT_LEVEL)%dbreal(17)
axesdef(BASE_LEVEL)%dbreal(18) = axesdef(DEFAULT_LEVEL)%dbreal(18)
axesdef(BASE_LEVEL)%dbreal(20) = axesdef(DEFAULT_LEVEL)%dbreal(20)
axesdef(BASE_LEVEL)%dbreal(21) = axesdef(DEFAULT_LEVEL)%dbreal(21)

IF( PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_HINT )THEN
  DO i = 1,6
    axesdef(BASE_LEVEL)%dbreal(10+i) = axesdef(BASE_LEVEL)%dbreal(16+i)
  END DO
ELSE
  CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                          0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
  LatLon    = project(BASE_LEVEL)%MapCoord == I_LATLON
  Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
  CALL axes_transform( axesdef(BASE_LEVEL)%dbreal(5),rtmp, &
                       axesdef(BASE_LEVEL)%dbreal(17),LatLon, &
                       Cartesian,.FALSE.,.TRUE. )
END IF

RETURN
END
!***********************************************************************
!                set_default_contri
!***********************************************************************
SUBROUTINE set_default_contri()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd

USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE contri_fi
USE plotdlg_fi

!     This routine sets the default parameters for the contri program

IMPLICIT NONE

INTEGER i,irv

LOGICAL, EXTERNAL :: hasError

CALL set_default_map_scales()     !Map Scales
CALL set_default_plot( file_def ) !PLOT Dialog
CALL set_default_animate()        !Animate Button

!---- Parameters not set by Dialog Boxes

!----	real paramters

default = DEF_VAL_R

scv  = 1.0	    !scale factor : data
sca  = 1.0	    !scale factor : auxiliary data
shx  = 0.0	    !shift factor : x coordinate
shy  = 0.0	    !shift factor : y coordinate
shv  = 0.0	    !shift factor : data
sha  = 0.0	    !shift factor : auxiliary data
small = 1.E-30	!small numbers
big   = 1.E+30	!big   numbers
spv   = big   	!special value numbers

!----	integer paramters

ndx  = 1	      !horizontal divisions between ticks
ndy  = 1	      !vertical   divisions between ticks
nfnt = 7	      !label font number
nfnc = 0	      !data function number
mxfnc = 7	      !maximum number of functions

!----	logical paramters

ltri = .FALSE.   !triangles OFF
lfrm = .TRUE.		 !frame ON
lbck = .TRUE.		 !background ON
lact = .FALSE.	 !active plot OFF
lint = .TRUE.	   !interactive plotting ON
lcol = .FALSE.	 !color cells/contours OFF
lspv = .FALSE.	 !exclude triangles with special value points OFF
logtrp = .TRUE.  !Log interpolation within triangles ON
lprint = .FALSE. !Printing OFF
lctxlab = .FALSE. !Contour text labels

!----	character paramters

cvar = ' '
caux = ' '
cgmf = 'gmeta'
cpal = TRIM(file_pal)
clns = ' '

DO i=1,MAXCNT
  ctxlab(i) = ' '
END DO

!---- Contri Functions

cfnc(1) = 'Add : Var + Aux'
cfnc(2) = 'Multiply : Var*Aux'
cfnc(3) = 'Divide : Var/Aux'
cfnc(4) = 'Invert : 1./Var'
cfnc(5) = 'Square root : SQRT(Var)'
cfnc(6) = 'Variance/Mean Ratio : SQRT(Var)/Aux'
cfnc(7) = 'Probability : Clipped Normal'
cfnc(8) = 'Probability : Lognormal Normal'
cfnc(9) = 'Conditional Probability : Clipped Normal'

!---- Load the Palette file

lpal = .FALSE.
CALL read_pal()
CLOSE(UNIT=lun_pal,IOSTAT=irv)
IF( hasError() )THEN
  CALL ShowErrorMessage( hwnd_mw )
END IF

RETURN
END
!***********************************************************************
!                set_default_map_scales
!***********************************************************************
SUBROUTINE set_default_map_scales()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd

!     This routine sets the default map print scales
!     Current max dimension : 4 - no. coordinate types
!                            10 - no. map scales for each coordinate type

IMPLICIT NONE

REAL pi180,sphfac

!==== Number of map scales for each coordinate type

Nmap_scales(I_LATLON)    = 6
Nmap_scales(I_CARTESIAN) = 6
Nmap_scales(I_UTM)       = 6
Nmap_scales(I_METERS)    = 5

!==== Map scale factors for each coordinate type (inches -> coordinate unit)

pi180 = 4.*ATAN(1.)/180.
sphfac = pi180*6371.2
Fmap_scales(I_LATLON)    = 2.54E-5/sphfac !inches -> deg Latitude
Fmap_scales(I_CARTESIAN) = 2.54E-5 !inches -> km
Fmap_scales(I_UTM)       = 2.54E-5 !inches -> km
Fmap_scales(I_METERS)    = 2.54E-2 !inches -> m

!==== Lat/Lon Map Scales

Smap_scales(1,I_LATLON) = 1000000
Smap_scales(2,I_LATLON) = 500000
Smap_scales(3,I_LATLON) = 250000
Smap_scales(4,I_LATLON) = 100000
Smap_scales(5,I_LATLON) = 50000
Smap_scales(6,I_LATLON) = 25000

!==== Cartesian Map Scales

Smap_scales(1,I_CARTESIAN) = 1000000
Smap_scales(2,I_CARTESIAN) = 500000
Smap_scales(3,I_CARTESIAN) = 250000
Smap_scales(4,I_CARTESIAN) = 100000
Smap_scales(5,I_CARTESIAN) = 50000
Smap_scales(6,I_CARTESIAN) = 25000

!==== UTM Map Scales

Smap_scales(1,I_UTM) = 1000000
Smap_scales(2,I_UTM) = 500000
Smap_scales(3,I_UTM) = 250000
Smap_scales(4,I_UTM) = 100000
Smap_scales(5,I_UTM) = 50000
Smap_scales(6,I_UTM) = 25000

!==== Meter Map Scales

Smap_scales(1,I_METERS) = 1000
Smap_scales(2,I_METERS) = 500
Smap_scales(3,I_METERS) = 250
Smap_scales(4,I_METERS) = 100
Smap_scales(5,I_METERS) = 50

RETURN
END
 !***********************************************************************
!                set_default_animate
!***********************************************************************
SUBROUTINE set_default_animate()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE animate_fi

!     This routine sets the default animate parameters for the PLOT ANIMATE Dialog Box

IMPLICIT NONE

start_frame   = 1
end_frame     = 1
inc_frame     = 1
current_frame = 1
lanim_cont    = .FALSE.
file_animate  = 'animate'
path_animate  = 'C:/'

RETURN
END
!***********************************************************************
!                set_default_new
!***********************************************************************
SUBROUTINE set_default_new()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE create_fi

!     This routine sets the default parameters for the EDTPRJ Dialog Box

IMPLICIT NONE

CALL set_default_domain()
CALL set_default_time()
CALL set_default_rundef()
CALL set_default_options()
CALL set_default_matdef()
CALL set_default_reldef()
CALL set_default_metdef()

!---- Flags

lcreate = .FALSE. !Create - Inactive

RETURN
END
!***********************************************************************
!                set_default_time
!***********************************************************************
SUBROUTINE set_default_time()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE GUItool_fi

!     This routine sets the default parameters for the TIME Dialog Box

IMPLICIT NONE

INTEGER irv

!---- Start Time

irv = SCIPDefaultTimeF( ToolCallerID,time )

CALL SCIP_GUI_time( dlgTime(DEFAULT_LEVEL),time )

CALL time_string( dlgTime(DEFAULT_LEVEL)%time%start%time,dlgTime(DEFAULT_LEVEL)%startString )
CALL time_string( dlgTime(DEFAULT_LEVEL)%time%end%time  ,dlgTime(DEFAULT_LEVEL)%endString   )

dlgTime(DEFAULT_LEVEL)%default_save = .TRUE.

DefinedOK = IBCLR(DefinedOK,DF_TIME)

dlgTime(BASE_LEVEL) = dlgTime(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_rundef
!***********************************************************************
SUBROUTINE set_default_rundef()

USE dialog_fi

!     This routine sets the default parameters for the RUNDEF Dialog Box

IMPLICIT NONE

rundef(DEFAULT_LEVEL)%on          = .FALSE.
rundef(DEFAULT_LEVEL)%optionsFile = ' '
rundef(DEFAULT_LEVEL)%update      = 4

rundef(BASE_LEVEL) = rundef(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_domain
!***********************************************************************
SUBROUTINE set_default_domain()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE param_fd
USE GUItool_fi

!     This routine sets the default parameters for the DOMAIN Dialog Box

IMPLICIT NONE

INTEGER irv

!---- Domain

irv = SCIPDefaultDomainF( ToolCallerID,domain )

CALL SCIP_GUI_domain( dlgDomain(DEFAULT_LEVEL),domain )

dlgDomain(BASE_LEVEL) = dlgDomain(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_options
!***********************************************************************
SUBROUTINE set_default_options()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE GUItool_fi

!     This routine sets the default parameters for the EDTPRJ MORE Dialog Box

IMPLICIT NONE

INTEGER irv

irv = SCIPDefaultOptionsF( ToolCallerID,prjOptions )

CALL SCIP_GUI_options( dlgOptions(DEFAULT_LEVEL),prjOptions )

DefinedOK = IBCLR(DefinedOK,DF_OPTION)

dlgOptions(BASE_LEVEL) = dlgOptions(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_metdef
!***********************************************************************
SUBROUTINE set_default_metdef()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE errorParam_fd
USE files_fi
USE dialog_fi
USE units_fd
USE param_fd

!     This routine sets the default parameters for the EDTPRJ METDEF Dialog Box

IMPLICIT NONE

LOGICAL read_def,ldum,luse,lcat
INTEGER i,ios,idum

CHARACTER(40)  cdum
CHARACTER(128) eString

LOGICAL, EXTERNAL :: hasError

metdef(DEFAULT_LEVEL)%local     = .FALSE.

metdef(DEFAULT_LEVEL)%lmc       = .FALSE.
metdef(DEFAULT_LEVEL)%lmcout    = .FALSE.
metdef(DEFAULT_LEVEL)%lmcformat = .FALSE.
metdef(DEFAULT_LEVEL)%l2Dsave   = .TRUE.
metdef(DEFAULT_LEVEL)%l3Dsave   = .TRUE.

metdef(DEFAULT_LEVEL)%lcanopy     = .TRUE. !Cultivated lands
metdef(DEFAULT_LEVEL)%canopy      = 1.0    !Cultivated lands
metdef(DEFAULT_LEVEL)%rough       = 0.03   !Cultivated lands
metdef(DEFAULT_LEVEL)%canopyParam = 2.0    !Cultivated lands

metdef(DEFAULT_LEVEL)%lsv = LSV_OPERATIONAL

metdef(DEFAULT_LEVEL)%slb = 100.
metdef(DEFAULT_LEVEL)%uub = 0.

metdef(DEFAULT_LEVEL)%bl     = BL_OPERATIONAL
metdef(DEFAULT_LEVEL)%precip = PC_CLEAR       !No Precipitation

metdef(DEFAULT_LEVEL)%zimax  = 1000.
metdef(DEFAULT_LEVEL)%zimin  = 50.
metdef(DEFAULT_LEVEL)%hconst = 0.
metdef(DEFAULT_LEVEL)%hdiur  = 50.
metdef(DEFAULT_LEVEL)%bowen  = 0.6  !Cultivated lands/Normal wetness
metdef(DEFAULT_LEVEL)%albedo = 0.16 !Cultivated lands
metdef(DEFAULT_LEVEL)%cloud  = 0.0  !Clear skies
metdef(DEFAULT_LEVEL)%wetness = MST_NORMAL !Normal wetness

metdef(DEFAULT_LEVEL)%speed      =  4.0
metdef(DEFAULT_LEVEL)%direction  = 90.0
metdef(DEFAULT_LEVEL)%unit_spd   = UNIT_METERS_SEC
metdef(DEFAULT_LEVEL)%unit_dir   = UNIT_DEGREES
metdef(DEFAULT_LEVEL)%tbin       = 3600.
metdef(DEFAULT_LEVEL)%slhazard   = 100.

metdef(DEFAULT_LEVEL)%alpmin  = 0.001
metdef(DEFAULT_LEVEL)%alpmax  = 1.0
metdef(DEFAULT_LEVEL)%epsfft  = 1.E-5
metdef(DEFAULT_LEVEL)%epsprm  = 1.E-2
metdef(DEFAULT_LEVEL)%nfft    = 100
metdef(DEFAULT_LEVEL)%nprm    = 200

metdef(DEFAULT_LEVEL)%tout = NOT_SET_R

read_def = file_met /= ' '
IF( read_def )THEN
  OPEN(UNIT=lun_tmp,FILE=file_met,STATUS='OLD',IOSTAT=ios)
  IF( ios /= 0 )THEN
    WRITE(string1,*)'Fortran Error =',ios
    CALL ReportFileName( eString,'File=',file_met )
    CALL SetError( OP_ERROR, &
                  'Error opening Default VerticalGrid file', &
                   eString, &
                   string1, &
                  'SetDefault' )
    CALL ShowErrorMessage( NULL_POINTER )
  END IF

  IF( ios == 0 )THEN
    READ(lun_tmp,*,IOSTAT=ios)metdef(DEFAULT_LEVEL)%nz
    IF( ios /= 0 )THEN
      metdef(DEFAULT_LEVEL)%nz = 0
      WRITE(string1,*)'Fortran Error =',ios
      CALL ReportFileName( eString,'File=',file_met )
      CALL SetError( OP_ERROR, &
                    'Error reading Default VerticalGrid file (nz)', &
                     eString, &
                     string1, &
                    'SetDefault' )
      CALL ShowErrorMessage( NULL_POINTER )
    END IF
  END IF

  IF( ios == 0 )THEN
    metdef(DEFAULT_LEVEL)%nz = MIN(25,metdef(DEFAULT_LEVEL)%nz)

    READ(lun_tmp,*,IOSTAT=ios)(metdef(DEFAULT_LEVEL)%z(i),i=1,metdef(DEFAULT_LEVEL)%nz)
    IF( ios /= 0 )THEN
      metdef(DEFAULT_LEVEL)%nz = 0
      WRITE(string1,*)'Fortran Error =',ios
      CALL ReportFileName( eString,'File=',file_met )
      CALL SetError( OP_ERROR, &
                    'Error reading Default VerticalGrid file (z)', &
                     eString, &
                     string1, &
                    'SetDefault' )
      CALL ShowErrorMessage( NULL_POINTER )
    END IF
  END IF

  CLOSE(UNIT=lun_tmp,IOSTAT=ios)

ELSE

  metdef(DEFAULT_LEVEL)%nz = 0

END IF

file_met = ' '
DO i = metdef(DEFAULT_LEVEL)%nz+1,25
  metdef(DEFAULT_LEVEL)%z(i) = 0.0
END DO

metdef(DEFAULT_LEVEL)%profile = ' '
metdef(DEFAULT_LEVEL)%surfile = ' '
metdef(DEFAULT_LEVEL)%medfile = ' '
metdef(DEFAULT_LEVEL)%asmfile = ' '
metdef(DEFAULT_LEVEL)%terfile = TRIM(file_ter)

CALL check_ter_header( I_LATLON,file_ter,ldum,cdum,idum,luse,lcat )
IF( hasError() )THEN
  CALL InitError()
  metdef(DEFAULT_LEVEL)%lavailter   = .TRUE.
  metdef(DEFAULT_LEVEL)%lavaillc    = .FALSE.
  metdef(DEFAULT_LEVEL)%llccategory = .FALSE.
  metdef(DEFAULT_LEVEL)%luseter     = .TRUE.
  metdef(DEFAULT_LEVEL)%luselc      = .TRUE.
ELSE
  metdef(DEFAULT_LEVEL)%lavailter    = .TRUE.
  metdef(DEFAULT_LEVEL)%lavaillc     = luse
  metdef(DEFAULT_LEVEL)%llccategory  = lcat
  metdef(DEFAULT_LEVEL)%luseter      = .TRUE.
  metdef(DEFAULT_LEVEL)%luselc       = .TRUE.
END IF

file_ter = ' '

metdef(DEFAULT_LEVEL)%met = MET_NULL

metdef(DEFAULT_LEVEL)%nobs     = DEF_VAL_I
metdef(DEFAULT_LEVEL)%nsfc     = DEF_VAL_I

!---- Flags

DefinedOK = IBCLR(DefinedOK,DF_MET)

metdef(BASE_LEVEL) = metdef(DEFAULT_LEVEL)

RETURN
END
!***********************************************************************
!                set_default_matdef
!***********************************************************************
SUBROUTINE set_default_matdef()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE GUImatl_fi
USE files_fi
USE GUItool_fi

!     This routine sets the default parameters for the EDTPRJ MATDEF Dialog Box

IMPLICIT NONE

!include 'SCIPtool.inc'

INTEGER irv
LOGICAL, EXTERNAL :: hasError

!---- New material definitions

matdef%mtlHead%max    = MAXMTYP
matdef%mtlHead%number = 3
CALL AllocateMtlList(matdef%mtlHead%number)
IF( hasError() )GOTO 9999
mtlList(1)%type = HM_GAS
mtlList(2)%type = HM_LIQUID + HM_2NDEVAP
mtlList(matdef%mtlHead%number)%type = HM_PARTICLE
irv = SCIPDefaultMaterialF( ToolCallerID,matdef,mtlList )
IF( irv /= SCIPsuccess )THEN
  CALL GetToolError( 'set_default_matdef' )
  GOTO 9999
END IF

CALL SCIP_GUI_material( materials(DEFAULT_LEVEL),matdef,mtlList )
IF( hasError() )GOTO 9999

CALL DeallocateMtlList()

!---- Number of current materials

nmatl_def = materials(DEFAULT_LEVEL)%nmatl
nmaux_def = materials(DEFAULT_LEVEL)%nmaux

materials(DEFAULT_LEVEL)%material(1)%icls  = 0
materials(DEFAULT_LEVEL)%material(1)%iaux  = 0
materials(DEFAULT_LEVEL)%material(1)%ldosg = .FALSE.
materials(DEFAULT_LEVEL)%material(1)%cmat  = ' '
materials(DEFAULT_LEVEL)%nmatl = 0
materials(DEFAULT_LEVEL)%nmaux = 0
materials(DEFAULT_LEVEL)%material(1)%nmc  = 0

CALL CopyMaterial( materials(DEFAULT_LEVEL),materials(BASE_LEVEL) )

9999 CONTINUE

!---- Flags

DefinedOK = IBCLR(DefinedOK,DF_MATERIAL)

RETURN
END
!***********************************************************************
!                set_default_reldef
!***********************************************************************
SUBROUTINE set_default_reldef()

USE resource_fd
USE mettype_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUItool_fi

!     This routine sets the default parameters for the EDTPRJ RELDEF Dialog Box

IMPLICIT NONE

INTEGER i

!---- New release definition

scenario(DEFAULT_LEVEL)%release(1)%defName = .TRUE.
scenario(DEFAULT_LEVEL)%release(1)%spec    = REL_DATA
scenario(DEFAULT_LEVEL)%release(1)%indx    = NOT_SET_I
scenario(DEFAULT_LEVEL)%release(1)%distrib = NOT_SET_I
scenario(DEFAULT_LEVEL)%release(1)%time    = 0.0
scenario(DEFAULT_LEVEL)%release(1)%dur     = NOT_SET_R
scenario(DEFAULT_LEVEL)%release(1)%rate    = NOT_SET_R
scenario(DEFAULT_LEVEL)%release(1)%xRel    = NOT_SET_D
scenario(DEFAULT_LEVEL)%release(1)%yRel    = NOT_SET_D
scenario(DEFAULT_LEVEL)%release(1)%zRel    = NOT_SET_R
DO i = 1,3
  scenario(DEFAULT_LEVEL)%release(1)%sig(i) = NOT_SET_R
  scenario(DEFAULT_LEVEL)%release(1)%vel(i) = NOT_SET_R
END DO
DO i = 1,MAXRELPARAM
  scenario(DEFAULT_LEVEL)%release(1)%param(i) = NOT_SET_R
END DO
DO i = 1,NUM_DYNAMIC
  scenario(DEFAULT_LEVEL)%release(1)%dynam(i) = 0.0
END DO
scenario(DEFAULT_LEVEL)%release(1)%dynam(2) = DEF_VAL_R
scenario(DEFAULT_LEVEL)%release(1)%dynam(3) = DEF_VAL_R
NULLIFY(scenario(DEFAULT_LEVEL)%release(1)%mc)
scenario(DEFAULT_LEVEL)%release(1)%type   = ' '
scenario(DEFAULT_LEVEL)%release(1)%matl   = ' '
scenario(DEFAULT_LEVEL)%release(1)%file   = ' '
scenario(DEFAULT_LEVEL)%release(1)%path   = ' '
scenario(DEFAULT_LEVEL)%release(1)%string = ' '

!---- Number of current releases

scenario(DEFAULT_LEVEL)%nrel = 0

CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(BASE_LEVEL) )

!---- Flags

DefinedOK = IBCLR(DefinedOK,DF_RELEASE)

RETURN
END
!***********************************************************************
!                FileSize
!***********************************************************************
INTEGER FUNCTION FileSize( filename )

USE SwinAPI
USE DefSize_fd

IMPLICIT NONE

CHARACTER(*), INTENT( IN ) :: filename

CHARACTER(PATH_MAXLENGTH) string

INTEGER(SINT) hfile
INTEGER high

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

string = AddNull( TRIM(filename) )

hfile = lopen( string,OF_READ )
high  = 0

IF( hfile /= HFILE_ERROR )THEN

  FileSize = GetFileSize( ADDRESSOF(hfile),high )
  IF( FileSize == Z'FFFFFFFF' )THEN
    FileSize = -2
  END IF
  high = lclose( hfile )

ELSE
  FileSize = -1
END IF

RETURN
END

