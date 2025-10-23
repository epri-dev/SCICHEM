!***********************************************************************
!                init_dialog
!***********************************************************************
SUBROUTINE init_dialog( iwnd_db,id,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE create_fi
USE GUImatl_fi
USE plotdlg_fi
USE GUItool_fi
USE pltchoice_fi
!
!     This routine initializes Dialog Boxes
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog Box handle
INTEGER,              INTENT( IN ) :: id       !Dialog Box id
INTEGER,              INTENT( IN ) :: id_level !Dialog data level

INTEGER i

!---- Initialize flags

ldestroy   = .FALSE. !Clear Mouse Flag OFF
ltool      = .FALSE. !TOOLBAR Flag OFF
lokbutton  = .FALSE. !RELDEF,MATDEF flag

SELECT CASE( id )
  CASE( IDB_PLOT )
    ltool = .TRUE.
    CALL DeallocateContours( 0,EDIT_LEVEL )
	  PlotDef(EDIT_LEVEL) = PlotDef(BASE_LEVEL)
	  DO i = 1,NUM_CONTOUR
	    ContourList(i,EDIT_LEVEL)  = ContourList(i,BASE_LEVEL)
	    ContourBuild(i,EDIT_LEVEL) = ContourBuild(i,BASE_LEVEL)
	  END DO
    CALL init_dialog_plot( iwnd_db,id_level )

  CASE( IDB_CONTOUR )
    CALL init_dialog_contour( iwnd_db,id_level )

  CASE( IDB_PLTOPT )
    poptdef(EDIT_LEVEL) = poptdef(BASE_LEVEL)
    CALL init_dialog_pltopt( iwnd_db,id_level )

  CASE( IDB_PLTMORE )
    popt2def(EDIT_LEVEL) = popt2def(BASE_LEVEL)
    CALL init_dialog_pltmore( iwnd_db,id_level )

  CASE( IDB_MAPS )
    mapdef(EDIT_LEVEL) = mapdef(BASE_LEVEL)
    CALL init_dialog_map( iwnd_db,id_level )

  CASE( IDB_AXES )
    axesdef(EDIT_LEVEL) = axesdef(BASE_LEVEL)
    CALL init_dialog_axes( iwnd_db,id_level )

  CASE( IDB_ANIMAT )
    CALL init_dialog_animate( iwnd_db,id_level )

  CASE( IDB_PLAY )
    CALL init_dialog_play( iwnd_db,id_level )

  CASE( IDB_LABELS )
    ttldef(EDIT_LEVEL) = ttldef(BASE_LEVEL)
    CALL init_dialog_titles( iwnd_db,id_level )

  CASE( IDB_SLICE )
    ldestroy = .TRUE. !TESTING
    CALL init_dialog_slice( iwnd_db,id_level )
    CALL ZoomInit( iwnd_db,IDB_SLICE ) !TESTING

  CASE( IDB_PICK )
    ldestroy = .TRUE. !TESTING
    CALL init_dialog_pick( iwnd_db,id_level )
    CALL ZoomInit( iwnd_db,IDB_PICK ) !TESTING

  CASE( IDB_ZOOM )
    ldestroy = .TRUE.
    axesdef(EDIT_LEVEL) = axesdef(BASE_LEVEL)
    CALL init_dialog_zoom( iwnd_db,id_level )
    CALL ZoomInit( iwnd_db,IDB_ZOOM )

  CASE( IDB_AUDIT )
    CALL init_dialog_audit( iwnd_db,id_level )

  CASE( IDB_DELPRJ )
    CALL init_dialog_delete( iwnd_db )

  CASE( IDB_EDTPRJ )
    ltool = .TRUE.
    DefinedSaved = DefinedOK
    dlgDomain(EDIT_LEVEL_1)  = dlgDomain(BASE_LEVEL)
    metdef(EDIT_LEVEL_1)     = metdef(BASE_LEVEL)
    dlgOptions(EDIT_LEVEL_1) = dlgOptions(BASE_LEVEL)
    dlgTime(EDIT_LEVEL_1)    = dlgTime(BASE_LEVEL)
    project(EDIT_LEVEL_1)    = project(BASE_LEVEL)
    CALL CopyMaterial( materials(BASE_LEVEL),materials(EDIT_LEVEL_1) )
    CALL CopyScenario( scenario(BASE_LEVEL),scenario(EDIT_LEVEL_1) )
    CALL init_dialog_new( iwnd_db,id_level )

  CASE( IDB_OPTIONS )
    dlgOptions(EDIT_LEVEL_2) = dlgOptions(EDIT_LEVEL_1)
    CALL init_dialog_options( iwnd_db,id_level )

  CASE( IDB_PRJDEF )
    project(EDIT_LEVEL_2) = project(EDIT_LEVEL_1)
    CALL CopyMaterial( materials(EDIT_LEVEL_1),materials(EDIT_LEVEL_2) )
    CALL init_dialog_prjdef( iwnd_db,id_level )

  CASE( IDB_TIME )
    project(EDIT_LEVEL_2)   = project(EDIT_LEVEL_1)
    dlgDomain(EDIT_LEVEL_2) = dlgDomain(EDIT_LEVEL_1)
    dlgTime(EDIT_LEVEL_2)   = dlgTime(EDIT_LEVEL_1)
    CALL init_dialog_time( iwnd_db,id_level )

  CASE( IDB_DOMAIN )
    metdef(EDIT_LEVEL_2)    = metdef(EDIT_LEVEL_1)
    dlgTime(EDIT_LEVEL_2)   = dlgTime(EDIT_LEVEL_1)
    dlgDomain(EDIT_LEVEL_2) = dlgDomain(EDIT_LEVEL_1)
    CALL init_dialog_domain( iwnd_db,id_level )

  CASE( IDB_METDEF )
    project(EDIT_LEVEL_2) = project(EDIT_LEVEL_1)
    metdef(EDIT_LEVEL_2)  = metdef(EDIT_LEVEL_1)
    CALL init_dialog_metdef( iwnd_db,id_level )

  CASE( IDB_RELDEF )
    dlgTime(EDIT_LEVEL_2)   = dlgTime(EDIT_LEVEL_1)
    dlgDomain(EDIT_LEVEL_2) = dlgDomain(EDIT_LEVEL_1)
    project(EDIT_LEVEL_2)   = project(EDIT_LEVEL_1)
    CALL CopyScenario( scenario(EDIT_LEVEL_1),scenario(EDIT_LEVEL_2) )
    CALL init_dialog_reldef( iwnd_db,id_level )

  CASE( IDB_RELNEW )
    CALL init_dialog_relnew( iwnd_db,id_level )

  CASE( IDB_MATDEF )
    CALL CopyMaterial( materials(EDIT_LEVEL_1),materials(EDIT_LEVEL_2) )
    CALL CopyScenario( scenario(EDIT_LEVEL_1),scenario(EDIT_LEVEL_2) )
    project(EDIT_LEVEL_2) = project(EDIT_LEVEL_1)
    CALL init_dialog_matdef( iwnd_db,id_level )

  CASE( IDB_MATNEW )
    CALL init_dialog_matnew( iwnd_db,id_level )

  CASE( IDB_AERPARM,IDB_LIQPARM,IDB_PRTPARM,IDB_GASPARM )
    CALL init_dialog_matparam( iwnd_db,id_level )

  CASE( IDB_RNDPARM )
    CALL init_dialog_random( iwnd_db,id_level )

  CASE( IDB_UNCPARM )
    CALL init_dialog_uncertainty( iwnd_db,id_level )

  CASE( IDB_MULTICOMP )
    CALL init_dialog_multicomp( iwnd_db,id_level )

  CASE( IDB_TERPARM )
    CALL init_dialog_terrain( iwnd_db,id_level )

  CASE( IDB_EDTLST )
    CALL init_dialog_edtlst( iwnd_db,id_level )

  CASE( IDB_NEWLST )
    CALL init_dialog_newlst( iwnd_db,id_level )

  CASE( IDB_COMLST )
    CALL init_dialog_matcmp( iwnd_db,id_level )

  CASE( IDB_XYTABLE )
    CALL init_dialog_xytab( iwnd_db,id_level )

  CASE( IDB_SCIPUF )
    ltool = .TRUE.
    rundef(EDIT_LEVEL_1)  = rundef(BASE_LEVEL)
    dlgTime(EDIT_LEVEL_1) = dlgTime(BASE_LEVEL)
    CALL init_dialog_run( iwnd_db,id_level )

  CASE( IDB_SETUP )
    CALL CopyMaterial( materials(EDIT_LEVEL_1),materials(EDIT_LEVEL_2) )
    dlgDomain(EDIT_LEVEL_2) = dlgDomain(EDIT_LEVEL_1)
    dlgTime(EDIT_LEVEL_2)   = dlgTime(EDIT_LEVEL_1)
    project(EDIT_LEVEL_2)   = project(EDIT_LEVEL_1)
    CALL init_dialog_newsetup( iwnd_db,id_level )

  CASE( IDB_ERRORBX )
    CALL init_dialog_errorbox( iwnd_db )

  CASE( IDB_RESTART )
    project(EDIT_LEVEL_2) = project(EDIT_LEVEL_1)
    CALL init_dialog_restart( iwnd_db,id_level )

  CASE( IDB_OBSERVE )
    CALL init_dialog_observe( iwnd_db,id_level )

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!            Initialize PLOT Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_plot( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE script_fi
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER             , INTENT( IN ) :: id_level !Data level

INTEGER  enable(20)

INTEGER irv,MapIt

!---- Show the Plot Window

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  IF( project(BASE_LEVEL)%Plot )THEN
    irv = EnableWindow( hwnd_pw,TRUE )
  ELSE
    irv = EnableWindow( hwnd_pw,FALSE )
  END IF
  irv = ShowWindow( hwnd_pw,SW_SHOW )
END IF

IF( project(BASE_LEVEL)%MapCoord == I_UTM .AND. &
    dlgDomain(BASE_LEVEL)%spatial%domain%zoneUTM == DEF_VAL_I )THEN
  MapIt = FALSE
ELSE
  MapIt = TRUE
END IF
!
!     Buttons
!
enable( 1) = TRUE !DEFAULT

IF( lprintOK )THEN
  enable( 2) = TRUE !COPY
  enable( 3) = TRUE !SAVE
  enable( 4) = TRUE !PRINT
ELSE
  enable( 2) = FALSE !COPY
  enable( 3) = FALSE !SAVE
  enable( 4) = FALSE !PRINT
END IF

enable( 5) = TRUE  !OPTIONS
enable( 6) = FALSE !Vert. Slice Select
enable( 7) = TRUE  !TITLES
enable( 8) = MapIt !MAPS
enable( 9) = TRUE  !CONTOUR

IF( project(BASE_LEVEL)%Plot )THEN
  enable(10) = TRUE !DRAW
ELSE
  enable(10) = FALSE !DRAW
END IF

enable(11) = TRUE  !AXES
enable(12) = TRUE  !Animate
enable(13) = TRUE  !Save Settings
enable(14) = TRUE  !Load Settings
enable(15) = FALSE !Summary
enable(16) = FALSE !Zoom             > These 3 are pseudo buttons
enable(17) = FALSE !Auto-ZoomOut

CALL EnableButtons( iwnd_db,enable,1,15 )
!
!     Check Boxes
!
lcheck(1,id_level) = BTEST(PlotDef(EDIT_LEVEL)%Mode,BPLT_DRAW) !Draw Contours
lcheck(2,id_level) = BTEST(PlotDef(EDIT_LEVEL)%Mode,BPLT_FILL) !Color Fill

CALL SetChecks( iwnd_db,lcheck(1,id_level),1,2 )
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
dbreal(EXCD_EDIT_ID,id_level) = PlotDef(EDIT_LEVEL)%TypeData(HP_PROB) * &
                                ContourList(PlotDef(EDIT_LEVEL)%ContourIndex,EDIT_LEVEL)%ListHdr%Scale
CALL LoadPlotCombo( iwnd_db,id_level,0 )
!
!     Edit Boxes - Reals
!
dbreal(2,id_level) = PlotDef(EDIT_LEVEL)%ClassData(HSLICE_INDEX)%zmin

IF( PlotDef(EDIT_LEVEL)%Field%UserTime == NOT_SET_R )THEN
  dbreal(3,id_level)  = PlotDef(DEFAULT_LEVEL)%Field%UserTime
ELSE
  IF( PlotDef(EDIT_LEVEL)%Field%TimeID == nTimePlot )THEN
    IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%usertime == SCIPtrue )THEN
      IF( PlotDef(EDIT_LEVEL)%Field%UserTime > timePlot(nTimePlot-1)%time%runTime )THEN
        dbreal(3,id_level) = PlotDef(EDIT_LEVEL)%Field%UserTime/24.
  	  ELSE
        dbreal(3,id_level) = PlotDef(DEFAULT_LEVEL)%Field%UserTime/24.
	    END IF
	  ELSE
      dbreal(3,id_level) = PlotDef(DEFAULT_LEVEL)%Field%UserTime/24.
	  END IF
  ELSE
    dbreal(3,id_level) = PlotDef(DEFAULT_LEVEL)%Field%UserTime/24.
  END IF
END IF
IF( InputTimeMax /= DEF_VAL_R )dbreal(3,id_level) = MIN(InputTimeMax,dbreal(3,id_level))

dbreal(4,id_level) = PlotDef(EDIT_LEVEL)%TypeData(HP_EXCEED)
dbreal(5,id_level) = PlotDef(EDIT_LEVEL)%TypeData(HP_MEAN)

CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,5 )
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize CONTOUR Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_contour( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER             , INTENT( IN ) :: id_level !Data level

CHARACTER(128) AddNull

INTEGER enable(6),idef

LOGICAL lcnt,lprob
REAL    scaling

REAL, EXTERNAL :: ScaleReal
!
!     Determine plot type
!
IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%type == SCIPtrue )THEN
  lprob = PlotDef(EDIT_LEVEL)%Type == HP_PROB .OR. PlotDef(EDIT_LEVEL)%Type == HP_VARIANCE
ELSE
  lprob = .FALSE.
END IF
!
!     Buttons
!
lcnt  = ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Number > 0

enable(1) = TRUE !DEFAULT

CALL EnableButtons( iwnd_db,enable,1,1 )
!
!     Check Boxes
!
!
!     Radio Buttons
!
nradio(1,id_level) = 2 !Contour Type
IF( lcnt )THEN
  ichoice(1,id_level) = 2 !  Nonuniform
ELSE
  ichoice(1,id_level) = 1 !  Uniform
END IF

IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%type == SCIPtrue )THEN
  IF( PlotDef(EDIT_LEVEL)%Type == HP_PROB )THEN
    idef = 2 !  Linear
  ELSE
    IF( PlotDef(EDIT_LEVEL)%Field%interpType == SCIPon )THEN
      idef = 2 !  Linear
    ELSE
      idef = 1 !  Log
    END IF
  END IF
ELSE
  IF( PlotDef(EDIT_LEVEL)%Field%interpType == SCIPon )THEN
    idef = 2 !  Linear
  ELSE
    idef = 1 !  Log
  END IF
END IF

nradio(2,id_level)  = 2 !Contour Type
IF( lcnt )THEN
  ichoice(2,id_level) = idef
ELSE
  SELECT CASE( ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Mode )
    CASE( PLOT_LOG )
      ichoice(2,id_level) = 1 !  Log
    CASE( PLOT_LIN )
      ichoice(2,id_level) = 2 !  Linear
    CASE( PLOT_DEF )
      ichoice(2,id_level) = idef
    CASE DEFAULT
      ichoice(2,id_level) = idef
  END SELECT
END IF

nradio(3,id_level)  = 2 !Label Type
ichoice(3,id_level) = 1 !  Numeric

CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,3 )
CALL EnableControl( iwnd_db,IDB_RADIO03,FALSE )
CALL ShowControl( iwnd_db,IDB_RADIO03,SW_HIDE )
!
!     Edit Boxes - Text
!
dbtext(1,id_level) = AddNull( ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Unit )
CALL SetEditTs( iwnd_db,dbtext(1,id_level),1,1 )
!
!     Edit Boxes - Reals
!
scaling = ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Scale
dbreal(1,id_level) = ScaleReal( ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Cmin,scaling ) !Min. Contour
dbreal(2,id_level) = ScaleReal( ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Cmax,scaling ) !Max. Contour
IF( ichoice(2,id_level) == 1 )THEN !  Log
  dbreal(3,id_level) = ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Del !Contour Inc.
ELSE
  dbreal(3,id_level) = ScaleReal( ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Del,scaling ) !Contour Inc.
END IF
dbreal(4,id_level) = ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Scale !Contour Scale
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )
!
!     Edit Boxes - Integers
!
dbint(1,id_level) = ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Number !  Uniform - Approx. Number
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )
!
!     Combo Boxes
!
!
!     List Boxes
!
IF( lcnt )THEN !  Nonuniform Contours - Load
  nlst(id_level) = 0 !
  CALL build_list_contour( iwnd_db,IDB_LIST1,id_level, &
                           ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Number, &
							             ContourList(USER_CONTOUR,EDIT_LEVEL)%ListPtr,scaling )
ELSE !  Uniform - Clear
  CALL clear_list_numeric( iwnd_db,IDB_LIST1,id_level )
END IF !
!
!     Static Boxes
!
CALL show_contours( iwnd_db,ichoice(1,id_level) )

RETURN
END
!*******************************************************************************
!            Initialize MORE Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_pltopt( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull

INTEGER i,idlm
INTEGER enable(2)
INTEGER(POINTER_LEN) jwnd
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
enable(2) = TRUE !MORE
CALL EnableButtons( iwnd_db,enable,1,2 )
CALL ShowControl( iwnd_db,IDB_BUTTON3,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC13,SW_HIDE )
!
!     Check Boxes
!
lcheck( 1,id_level) = poptdef(EDIT_LEVEL)%Max      !Show Max Value
lcheck( 2,id_level) = poptdef(EDIT_LEVEL)%Cell     !Show cells
lcheck( 3,id_level) = poptdef(EDIT_LEVEL)%Source   !Show Sources
lcheck( 4,id_level) = poptdef(EDIT_LEVEL)%Contour  !Show Contour infor
lcheck( 5,id_level) = poptdef(EDIT_LEVEL)%ColorBox !Show Color Box
lcheck( 6,id_level) = poptdef(EDIT_LEVEL)%FillLo   !Low Fill
lcheck( 7,id_level) = poptdef(EDIT_LEVEL)%FillHi   !Hi Fill
lcheck( 9,id_level) = poptdef(EDIT_LEVEL)%Terrain  !Show terrain contours
lcheck(10,id_level) = poptdef(EDIT_LEVEL)%SrcText  !Show Sources Text
lcheck(11,id_level) = poptdef(EDIT_LEVEL)%Weather  !Show Weather stations
lcheck(12,id_level) = poptdef(EDIT_LEVEL)%KmScale  !Show Scale bar
lcheck(13,id_level) = poptdef(EDIT_LEVEL)%Overlay  !Show Overlay
IF( poptdef(EDIT_LEVEL)%Overlay )THEN
  dbtext(1,id_level) = AddNull( poptdef(EDIT_LEVEL)%OverlayFile )
ELSE
  dbtext(1,id_level) = ' '
END IF
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,4 )

!---- Check for Color Fill ON

CALL FindHwndListId( IDB_PLOT,jwnd,idlm )
IF( .NOT.lcheck(2,idlm) )THEN
 DO i = 5,7
    CALL EnableControl( iwnd_db,CHECK_BASE+i,FALSE )
 END DO
ELSE
  CALL SetChecks( iwnd_db,lcheck(5,id_level),5,3 )
END IF

CALL EnableControl( iwnd_db,IDB_CHECK8,FALSE )

!---- Check for Terrain enabled

IF( project(BASE_LEVEL)%Terrain )THEN
  CALL SetChecks( iwnd_db,lcheck(9,id_level),9,1 )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK9,FALSE )
END IF

!---- Check for Weather stations

IF( project(BASE_LEVEL)%Weather > 0 )THEN
  CALL SetChecks( iwnd_db,lcheck(11,id_level),11,1 )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK11,FALSE )
END IF

!---- Check for Scale Bar

IF( axesdef(BASE_LEVEL)%MapCoord /= I_LATLON )THEN
  CALL SetChecks( iwnd_db,lcheck(12,id_level),12,1 )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK12,FALSE )
END IF

!---- Check for Overlay

CALL SetChecks( iwnd_db,lcheck(13,id_level),13,1 )

string1 = 'reserved'
CALL EnableControl( iwnd_db,IDB_CHECK8,FALSE )
CALL SetControlText( iwnd_db,IDB_CHECK8,string1 )

string2 = 'Show Source Text'
CALL SetControlText( iwnd_db,IDB_CHECK10,string2 )
CALL SetChecks( iwnd_db,lcheck(10,id_level),10,1 )
IF( lcheck(3,id_level) )THEN
  CALL EnableControl( iwnd_db,IDB_CHECK10,TRUE )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK10,FALSE )
END IF
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!
!     Edit Boxes - Integers
!
dbint(1,id_level) = poptdef(EDIT_LEVEL)%Maxlev !Max plot grid level
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,1 )
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE )THEN
  CALL EnableControl( iwnd_db,IDB_INT1,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_INT1,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_STATIC17,SW_HIDE )
  CALL EnableControl( iwnd_db,IDB_BUTTON3,FALSE )
  CALL ShowControl  ( iwnd_db,IDB_BUTTON3,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_STATIC13,SW_HIDE )
  DO i = 1,13
    CALL EnableControl( iwnd_db,CHECK_BASE+i,FALSE )
    CALL ShowControl  ( iwnd_db,CHECK_BASE+i,SW_HIDE )
  END DO
  CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )
END IF

RETURN
END
!*******************************************************************************
!            Initialize MORE Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_pltmore( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER  enable(2)
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
CALL EnableButtons( iwnd_db,enable,1,1 )
!
!     Check Boxes
!
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
dbreal(1,id_level) = popt2def(EDIT_LEVEL)%AR !Plot Aspect Ratio
dbreal(2,id_level) = popt2def(EDIT_LEVEL)%Size !Plot size
dbreal(3,id_level) = popt2def(EDIT_LEVEL)%X !Plot center - X
dbreal(4,id_level) = popt2def(EDIT_LEVEL)%Y !Plot center - Y
dbreal(5,id_level) = popt2def(EDIT_LEVEL)%Font !Plot Font size
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,5 )
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize MAP Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_map( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER(4),           INTENT( IN ) :: id_level !Data level

INTEGER enable(2)
LOGICAL lref
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
enable(2) = TRUE !RESTORE
CALL EnableButtons( iwnd_db,enable,1,2 )
!
!     Check Boxes
!
lcheck(1 ,id_level) = mapdef(EDIT_LEVEL)%PPSymbol            !Place Text Symbols
lcheck(2 ,id_level) = mapdef(EDIT_LEVEL)%PPText              !Place Text
lcheck(3 ,id_level) = mapdef(EDIT_LEVEL)%PPArea              !Populated Places Area
lcheck(4 ,id_level) = mapdef(EDIT_LEVEL)%Roads               !Roads
lcheck(5 ,id_level) = mapdef(EDIT_LEVEL)%RRs                 !Railroads
lcheck(6 ,id_level) = mapdef(EDIT_LEVEL)%NucFac              !Nuclear Facilities
lcheck(7 ,id_level) = mapdef(EDIT_LEVEL)%Airport             !Airports
lcheck(8 ,id_level) = mapdef(EDIT_LEVEL)%NucFacText          !Nuc Fac Text
lcheck(9 ,id_level) = mapdef(EDIT_LEVEL)%AirportText         !Airport Text
lcheck(10,id_level) = mapdef(EDIT_LEVEL)%popden > 0.0 .OR. &
                      mapdef(EDIT_LEVEL)%popden == DEF_VAL_R !Population density
lcheck(11,id_level) = mapdef(EDIT_LEVEL)%popden == -1.0      !Area estimate

CALL SetChecks( iwnd_db,lcheck(1,id_level),1,11 )
!
!     Radio Buttons
!
lref = MIN(axesdef(BASE_LEVEL)%Lat0,axesdef(BASE_LEVEL)%Lon0, &
           axesdef(BASE_LEVEL)%X0,axesdef(BASE_LEVEL)%Y0) /= NOT_SET_R .OR. &
  ((project(BASE_LEVEL)%MapCoord == I_LATLON) .AND. .NOT.(axesdef(BASE_LEVEL)%MapCoord == I_CARTESIAN))
nradio(1,id_level) = 3 !Map Type
IF( lref )THEN
  IF( mapdef(EDIT_LEVEL)%MapsOn )THEN
    IF( mapdef(EDIT_LEVEL)%HiRes )THEN
      ichoice(1,id_level) = 1 !  Hi Res
    ELSE
      ichoice(1,id_level) = 2 !  Low Res
    END IF
  ELSE
    ichoice(1,id_level) = 3 !  No Map
  END IF
ELSE
  ichoice(1,id_level) = 3 !  No Map
  CALL EnableControl( iwnd_db,IDB_RADIO01,FALSE )
  CALL EnableControl (iwnd_db,IDB_RADIO02,FALSE )
END IF

nradio(2,id_level)  = 2 !Text Type
IF( .NOT.mapdef(EDIT_LEVEL)%PPTextLo )THEN
  ichoice(2,id_level) = 1 !  Full list
ELSE
  ichoice(2,id_level) = 2 !  Partial list
END IF

nradio(3,id_level)  = 2 !Text Type
IF( mapdef(EDIT_LEVEL)%popden == DEF_VAL_R )THEN
  dbreal(1,id_level)  = 1.0
  IF( PopData )THEN
    ichoice(3,id_level) = 1 !  Full list
  ELSE
    ichoice(3,id_level) = 2 !  Full list
  END IF
ELSE IF( mapdef(EDIT_LEVEL)%popden == -1.0 )THEN
  dbreal(1,id_level)  = 1.0
  IF( PopData )THEN
    ichoice(3,id_level) = 1 !  Full list
  ELSE
    ichoice(3,id_level) = 2 !  Full list
  END IF
ELSE
  ichoice(3,id_level) = 2 !  Partial list
  IF( mapdef(EDIT_LEVEL)%popden == 0.0 )THEN
    dbreal(1,id_level)  = 1.0
  ELSE
    dbreal(1,id_level)  = mapdef(EDIT_LEVEL)%popden
  END IF
END IF

CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,3 )
CALL EnableControl( iwnd_db,IDB_RADIO21,FALSE )
string1 = 'reserved'
CALL SetControlText( iwnd_db,IDB_RADIO21,string1 )
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,1 )
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
CALL map_check( iwnd_db,10,id_level )

RETURN
END
!*******************************************************************************
!            Initialize SLICE Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_slice( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE param_fd
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the SLICE Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER             , INTENT( IN ) :: id_level !Data level

INTEGER  enable(2),irv,i
REAL     dtmp(6),ptmp(6),rtmp(4)
LOGICAL  LatLon,Cartesian
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
enable(2) = TRUE !RESTORE
CALL EnableButtons( iwnd_db,enable,1,12 )
!
!     Check Boxes
!
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
dtmp(1) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmin !X Location Point 1
dtmp(4) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymin !Y Location Point 1
dtmp(2) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmax !X Location Point 2
dtmp(5) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymax !Y Location Point 2
dtmp(3) = 1.0 !axesdef(BASE_LEVEL).Xscale
dtmp(6) = 1.0 !axesdef(BASE_LEVEL).Yscale
ptmp(3) = axesdef(BASE_LEVEL)%Xscale
ptmp(6) = axesdef(BASE_LEVEL)%Yscale
CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                        0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
CALL axes_transform( dtmp,rtmp,ptmp,LatLon,Cartesian,.TRUE.,.FALSE. )
dbreal(1,id_level) = ptmp(1) !X Location Point 1
dbreal(2,id_level) = ptmp(4) !Y Location Point 1
dbreal(3,id_level) = ptmp(2) !X Location Point 2
dbreal(4,id_level) = ptmp(5) !Y Location Point 2
dbreal(5,id_level) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmin !Z Min.
dbreal(6,id_level) = PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmax !Z Max.
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,6 )
!
!     Edit Boxes - Integers
!
dbint(2,id_level) = NINT(PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zres) !Vertical resolution
CALL SetEditIs( iwnd_db,dbint(2,id_level),2,1 )

CALL SetScrollControlRange( iwnd_db,IDB_SCROLL1,0,100,irv )
CALL SetScrollControlPos( iwnd_db,IDB_SCROLL1,dbint(2,id_level),irv )
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize PICK Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_pick( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the PICK Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

!
!     Buttons
!
IF( nxytab >= maxList )CALL EnableControl( iwnd_db,IDB_BUTTON2,FALSE )
!
!     Check Boxes
!
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
dbreal(1,id_level) = NOT_SET_R !X Location Point 1
dbreal(2,id_level) = NOT_SET_R !Y Location Point 1
dbreal(3,id_level) = NOT_SET_R !data value
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,3 )
CALL EnableControl( iwnd_db,IDB_REAL3,FALSE )
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize ZOOM Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_zoom( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE SwinAPI
!
!     This routine initializes the ZOOM Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER  enable(2),i

!
!     Buttons
!
enable(1) = TRUE !DRAW
enable(2) = TRUE !ZOOM OUT
CALL EnableButtons( iwnd_db,enable,1,2 )
!
!     Edit - reals
!
DO i = 1,4
  dbreal(i,id_level) = DEF_VAL_R
END DO
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )

RETURN
END
!*******************************************************************************
!            Initialize SaveAs Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_SaveAs( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i,irv,maxtimes
!
!     Buttons
!
!
!     Check Boxes
!
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
IF( lanim_hook )THEN

  string1 = 'Animation Control' !  Set Group Label
  CALL SetControlText( iwnd_db,IDB_STATIC04,string1 )

  CALL ClearList( iwnd_db,IDB_COMBO1 )
  CALL ClearList( iwnd_db,IDB_COMBO2 )

  IF( timePlot(nTimePlot)%time%runTime == DEF_VAL_R )THEN
    maxtimes = nTimePlot - 1
  ELSE
    maxtimes = nTimePlot
  END IF

  DO i = 1,maxtimes
    CALL format_time( timePlot(i)%time%runTime,string1,0 )
    CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv ) !Put String in COMBO BOX
    CALL AddList( iwnd_db,IDB_COMBO2,-1,string1,irv ) !Put String in COMBO BOX
  END DO !

  idbcmbo(1,id_level) = 1
  idbcmbo(2,id_level) = maxtimes

  CALL format_time( timePlot(1)%time%runTime,string1,0 )
  dbcmbo(1,id_level) = TRIM(string1)
  CALL SetListSelString( iwnd_db,IDB_COMBO1,dbcmbo(1,id_level),irv ) !  Select this time in COMBO BOX
  CALL format_time( timePlot(maxtimes)%time%runTime,string1,0 )
  dbcmbo(2,id_level) = TRIM(string1)
  CALL SetListSelString( iwnd_db,IDB_COMBO2,dbcmbo(2,id_level),irv ) !  Select this time in COMBO BOX

ELSE

  CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE ) !  Disable first button
  CALL EnableControl( iwnd_db,IDB_COMBO2,FALSE ) !  Disable second button
  CALL ShowControl  ( iwnd_db,IDB_COMBO1,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_COMBO2,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_STATIC01,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_STATIC03,SW_HIDE )
  CALL ShowControl  ( iwnd_db,IDB_STATIC04,SW_HIDE )
  string1 = 'Export File as &Type'
  CALL SetControlText( iwnd_db,1089,string1 )

END IF
!
!     List Boxes
!
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize OpenFile Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_OpenFile( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE restart_fi
USE SwinAPI
!
!     This routine initializes the OpenFile Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
!
!     Buttons
!
!
!     Check Boxes
!
IF( enableRestart )THEN
  lcheck(1,id_level) = .FALSE.
  CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL ShowControl( iwnd_db,IDB_CHECK1,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC04,SW_HIDE )
END IF
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!

RETURN
END
!*******************************************************************************
!            Initialize Print Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_Print( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE param_fd
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv,i,indx,nn
REAL    scale_min
LOGICAL Cart

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull
!
!     Buttons
!
!
!     Check Boxes
!
lcheck(1,id_level) = lpplot  .AND. lprintOK
lcheck(2,id_level) = lpstat
lcheck(3,id_level) = .FALSE.
lcheck(4,id_level) = lpdate
lcheck(5,id_level) = lpmtime
lcheck(6,id_level) = lpbare
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,6 )
IF( .NOT.lplotOK )THEN
  CALL EnableControl( iwnd_db,IDB_CHECK1,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
END IF
CALL EnableControl( iwnd_db,IDB_CHECK3,FALSE )
CALL ShowControl( iwnd_db,IDB_CHECK3,SW_HIDE )
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
IF( Nmap_scales(axesdef(BASE_LEVEL)%MapCoord) <= 0  .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_HINT   .OR. &
    PlotDef(BASE_LEVEL)%Field%Category == HP_TABLE )THEN
  CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
  CALL ShowControl( iwnd_db,IDB_COMBO1,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC05,SW_HIDE )
  idbcmbo(1,id_level) = 1
  string1 = ' Fit to page'
  dbcmbo(1,id_level) = AddNull( TRIM(string1) )
ELSE
  Cart = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
  CALL check_plot_axes( project(BASE_LEVEL)%MapCoord,Cart,.FALSE., &
                        scale_min,Fmap_scales(axesdef(BASE_LEVEL)%MapCoord) )
  CALL ClearList( iwnd_db,IDB_COMBO1 )
  string1 = ' Fit to page'
  CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv ) !Put String in COMBO BOX

  DO i = 1,Nmap_scales(axesdef(BASE_LEVEL)%MapCoord)
    CALL i_format( Smap_scales(i,axesdef(BASE_LEVEL)%MapCoord),nn,string2 )
    IF( FLOAT(Smap_scales(i,axesdef(BASE_LEVEL)%MapCoord)) < scale_min )THEN
      string1 = '*1:'//string2
    ELSE
      string1 = ' 1:'//string2
    END IF
    CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv ) !Put String in COMBO BOX
  END DO

  indx = 0
  CALL SetListSel( iwnd_db,IDB_COMBO1,indx,irv )
  CALL GetListItem( iwnd_db,IDB_COMBO1,indx,string1,irv )
  idbcmbo(1,id_level) = indx + 1
  dbcmbo(1,id_level)  = AddNull( TRIM(string1(1:irv)) )
END IF
!
!     List Boxes
!
CALL ClearList( iwnd_db,IDB_LIST1 )

DO i = 1,nTimePlot
  CALL format_time( timePlot(i)%time%runTime,string1,0 )
  CALL AddList( iwnd_db,IDB_LIST1,-1,string1,irv ) !Put String in COMBO BOX
END DO !

IF( .NOT.lpmtime )THEN
  CALL EnableControl( iwnd_db,IDB_LIST1,FALSE )
  CALL ShowControl( iwnd_db,IDB_LIST1,SW_HIDE )
END IF
!
!     Static Boxes
!
RETURN
END
!*******************************************************************************
!            Initialize TITLES Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_titles( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER  enable(2),i,idlm
INTEGER(POINTER_LEN) jwnd
LOGICAL  lok

CHARACTER(128), EXTERNAL :: AddNull, StripNull
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
enable(2) = TRUE !RESTORE
CALL EnableButtons( iwnd_db,enable,1,2 )
!
!     Check Boxes
!
!
!
!     Check Boxes - Title Line ON/OFF Flags
!
DO i = 1,3
  lcheck(i,id_level) = ttldef(EDIT_LEVEL)%Show(i)
  dbtext(i,id_level) = AddNull( ttldef(EDIT_LEVEL)%string(i) )
  dbreal(2*i-1,id_level) = ttldef(EDIT_LEVEL)%X(i)
  dbreal(2*i  ,id_level) = ttldef(EDIT_LEVEL)%Y(i)
END DO
lcheck(4,id_level) = ttldef(EDIT_LEVEL)%ShowDate
lcheck(5,id_level) = ttldef(EDIT_LEVEL)%ShowTime
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,5 )
!
!     Radio Buttons
!
!
!     Edit Boxes - Text - Title Line Strings
!
CALL SetEditTs( iwnd_db,dbtext(1,id_level),1,3 )
!
!     Edit Boxes - Reals - Title Line Locations
!
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,6 )
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
CALL FindHwndListId( IDB_PLOT,jwnd,idlm )
lok = lcheck(3,id_level) .AND. &
StripNull(dbtext(3,id_level)) == 'default'
lok = lok .OR. (PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE)
IF( .NOT.lok )THEN
  CALL EnableControl( iwnd_db,IDB_CHECK4,FALSE )
  CALL EnableControl( iwnd_db,IDB_CHECK5,FALSE )
ELSE
  CALL EnableControl( iwnd_db,IDB_CHECK4,TRUE )
  CALL EnableControl( iwnd_db,IDB_CHECK5,TRUE )
END IF

RETURN
END
!*******************************************************************************
!            Initialize AXES Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_axes( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i,iset,idlm
INTEGER enable(2)
INTEGER zone
INTEGER(POINTER_LEN) jwnd
REAL    lat0,lon0,x0,y0

CHARACTER(128), EXTERNAL :: AddNull
!
!     Buttons
!
enable(1) = TRUE !DEFAULT
enable(2) = TRUE !RESTORE
CALL EnableButtons( iwnd_db,enable,1,2 )
!
!     Check Boxes - Label ON/OFF Flags
!
lcheck(1,id_level) = axesdef(EDIT_LEVEL)%ShowX
lcheck(2,id_level) = axesdef(EDIT_LEVEL)%ShowY
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,2 )
!
!     Radio Buttons
!
!---- Check Plot Type

CALL FindHwndListId( IDB_PLOT,jwnd,idlm )
IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
  iset = 11 !Vertical Slice
ELSE
  iset = 5 !The Rest
END IF

nradio(1,id_level) = 3 !Coordinate Type
ichoice(1,id_level) = axesdef(EDIT_LEVEL)%MapCoord !  Lat/Lon
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

!---- Set labels

IF( iset == 5 )THEN
  IF( ichoice(1,id_level) == I_CARTESIAN )THEN
    string1 = 'X (km)'
    string2 = 'Y (km)'
  ELSE IF( ichoice(1,id_level) == I_UTM )THEN
    string1 = 'E''ing'
    string2 = 'N''ing'
  ELSE
    string1 = 'Lon.'
    string2 = 'Lat.'
  END IF
ELSE
  string1 = 'X,Y (pts)'
  string2 = 'Z (km)'
END IF
CALL SetControlText( iwnd_db,IDB_STATIC22,string1 )
CALL SetControlText( iwnd_db,IDB_STATIC23,string2 )
!
!     Edit Boxes - Text
!
dbtext(1,id_level) = AddNull( axesdef(EDIT_LEVEL)%Xlab )    !Horizontal Axis Label
dbtext(2,id_level) = AddNull( axesdef(EDIT_LEVEL)%Ylab )    !Vertical Axis Label
dbtext(3,id_level) = AddNull( axesdef(EDIT_LEVEL)%Xformat ) !Horizontal Axis Format
dbtext(4,id_level) = AddNull( axesdef(EDIT_LEVEL)%Yformat ) !Vertical Axis Format
CALL SetEditTs( iwnd_db,dbtext(1,id_level),1,4 )
!
!     Edit Boxes - Reals
!
CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,ichoice(1,id_level),1,lon0,lat0,x0,y0,zone )

dbreal(1,id_level)  = lon0 !Local Coord. X origin
dbreal(2,id_level)  = lat0 !Local Coord. Y Origin
dbreal(9,id_level)  = x0   !Local Coord. X origin
dbreal(10,id_level) = y0   !Local Coord. Y Origin
DO i = 1,6
  dbreal(i+2,id_level) = axesdef(EDIT_LEVEL)%dbreal(16+i) !Axes Parameters
END DO
IF( dbreal(3,id_level) /= DEF_VAL_R )THEN
  dbreal(3,id_level) = dbreal(3,id_level)/dbreal(5,id_level)
END IF
IF( dbreal(4,id_level) /= DEF_VAL_R )THEN
  dbreal(4,id_level) = dbreal(4,id_level)/dbreal(5,id_level)
END IF
IF( dbreal(6,id_level) /= DEF_VAL_R )THEN
  dbreal(6,id_level) = dbreal(6,id_level)/dbreal(8,id_level)
END IF
IF( dbreal(7,id_level) /= DEF_VAL_R )THEN
  dbreal(7,id_level) = dbreal(7,id_level)/dbreal(8,id_level)
END IF
CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,10 )
!
!     Edit Boxes - Integers
!
IF( iset == 5 )THEN
  dbint(1,id_level) = axesdef(EDIT_LEVEL)%TicX !X Ticks
  dbint(2,id_level) = axesdef(EDIT_LEVEL)%TicY !Y Ticks
ELSE
  dbint(1,id_level) = axesdef(EDIT_LEVEL)%TicH !X Ticks
  dbint(2,id_level) = axesdef(EDIT_LEVEL)%TicV !Y Ticks
END IF
CALL SetEditIs( iwnd_db,dbint(1,id_level),1,2 )
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
CALL show_local( iwnd_db,project(BASE_LEVEL)%MapCoord,project(BASE_LEVEL)%Ref,ichoice(1,id_level) )

RETURN
END
!*******************************************************************************
!            Set_llc_reference
!*******************************************************************************
SUBROUTINE set_llc_reference( imap,iplt,ilev,lon0,lat0,x0,y0,zone )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE datums
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN  ) :: imap !Project type
INTEGER, INTENT( IN  ) :: iplt !Plot type
INTEGER, INTENT( IN  ) :: ilev !data level
REAL,    INTENT( OUT ) :: lon0,lat0
REAL,    INTENT( OUT ) :: x0,y0
INTEGER, INTENT( OUT ) :: zone

INTEGER irv

IF( iplt == I_UTM .AND. imap == I_LATLON )THEN
  IF( dlgDomain(BASE_LEVEL)%spatial%domain%xMin == DEF_VAL_R .OR. &
      dlgDomain(BASE_LEVEL)%spatial%domain%xMax == DEF_VAL_R .OR. &
      dlgDomain(BASE_LEVEL)%spatial%domain%yMin == DEF_VAL_R .OR. &
      dlgDomain(BASE_LEVEL)%spatial%domain%yMax == DEF_VAL_R )THEN
    lon0 = 0.
    lat0 = 0.
  ELSE
    lon0 = 0.5*(dlgDomain(BASE_LEVEL)%spatial%domain%xMax + dlgDomain(BASE_LEVEL)%spatial%domain%xMin)
    lat0 = 0.5*(dlgDomain(BASE_LEVEL)%spatial%domain%yMax + dlgDomain(BASE_LEVEL)%spatial%domain%yMin)
  END IF
  zone = 0
  irv = LL2UTM( lat0,lon0,zone,x0,y0 )
  IF( irv /= 0 )THEN
    CALL setUTMerrorGUI( 'LL2UTM',irv )
    GOTO 9999
  END IF
ELSE
  IF( imap == I_UTM )THEN
    zone = dlgDomain(BASE_LEVEL)%spatial%domain%zoneUTM
  ELSE
    zone = 0.
  END IF
  IF( axesdef(ilev)%Lon0 == NOT_SET_R .OR. axesdef(ilev)%Lat0 == NOT_SET_R )THEN
    lon0 = 0. !Local Coord. X origin
    lat0 = 0. !Local Coord. Y Origin
  ELSE
    lon0 = axesdef(ilev)%Lon0 !Local Coord. X origin
    lat0 = axesdef(ilev)%Lat0 !Local Coord. Y Origin
  END IF
  IF( axesdef(ilev)%X0 == NOT_SET_R .OR. axesdef(ilev)%Y0 == NOT_SET_R )THEN
    x0 = dlgDomain(BASE_LEVEL)%spatial%domain%xMin !Local Coord. X origin
    y0 = dlgDomain(BASE_LEVEL)%spatial%domain%yMin !Local Coord. Y Origin
  ELSE
    x0 = axesdef(ilev)%X0 !Local Coord. X origin
    y0 = axesdef(ilev)%Y0 !Local Coord. Y Origin
  END IF
END IF

9999 CONTINUE

RETURN
END
!*******************************************************************************
!            Initialize NEWPRJ Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_new( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE create_fi
USE GUImatl_fi
USE script_fi
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN  ) :: id_level !Data level

INTEGER istat,i
INTEGER   enable(11),nbtn
!
!     Buttons
!
nbtn = 7
enable    = FALSE
enable(1) = TRUE !Time
enable(2) = TRUE !Domain
enable(3) = TRUE !Options
enable(4) = TRUE !Material
IF( materials(EDIT_LEVEL_1)%nmatl > 0 )THEN
  enable(5) = TRUE !Release
END IF
enable(6) = TRUE !Meterorology
enable(7) = TRUE !Audit
enable(9)  = TRUE
enable(10) = TRUE
IF( lok_create .AND. project(EDIT_LEVEL_1)%Edit .AND. .NOT. project(EDIT_LEVEL_1)%OK )THEN
  enable(11) = TRUE !Create
END IF
CALL EnableButtons( iwnd_db,enable,1,nbtn )
CALL EnableButtons( iwnd_db,enable(9),10,3 )
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON10,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_BUTTON11,SW_HIDE )
END IF
IF( .NOT.project(EDIT_LEVEL_1)%Edit )THEN
  CALL ShowControl( iwnd_db,IDB_BUTTON12,SW_HIDE )
  CALL SetControlText( iwnd_db,IDB_BUTTON11,'Delete &Project' )
  CALL ShowControl( iwnd_db,IDB_BUTTON11,SW_SHOWNORMAL )
  CALL SetControlText( iwnd_db,IDB_BUTTON10,'Edit Pro&ject' )
  CALL ShowControl( iwnd_db,IDB_BUTTON10,SW_SHOWNORMAL )
  enable(1) = TRUE
  CALL EnableButtons( iwnd_db,enable,10,1 )
  string1 = 'PROJECT Viewer'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
ELSE IF( project(EDIT_LEVEL_1)%OK )THEN
  CALL SetControlText( iwnd_db,IDB_BUTTON10,'&Load...' )
  CALL SetControlText( iwnd_db,IDB_BUTTON11,'Clear &All' )
  CALL SetControlText( iwnd_db,ID_CANCEL,'&Continue' )
  CALL SetControlText( iwnd_db,IDB_BUTTON12,'Acce&pt Changes' )
  CALL ShowControl (iwnd_db,IDB_BUTTON12,SW_HIDE )
  string1 = 'PROJECT Editor'
  CALL SetControlText( iwnd_db,IDB_STATIC01,string1 )
END IF
!
!     Check Boxes
!
!
!     Radio Buttons
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Static Boxes
!
CALL hide_checkmark( iwnd_db,3 )

DO i = 1,6
  istat = STATIC_BASE + i*10 + 1
  IF( i /= 3 )CALL SetControlIcon( iwnd_db,istat,check_hndl )
  IF( BTEST(DefinedOK,i) )THEN
    CALL build_list( iwnd_db,i )
    IF( i /= 3 )CALL set_checkmark( iwnd_db,i )
  ELSE
    istat = LIST_BASE + i
    CALL ClearList( iwnd_db,istat )
    IF( i /= 3 )CALL hide_checkmark( iwnd_db,i )
  END IF
  istat = LIST_BASE + i
  CALL SetControlFont( iwnd_db,istat,fixfont )
END DO

CALL build_list( iwnd_db,7 )
CALL SetControlFont( iwnd_db,IDB_LIST7,fixfont )
CALL set_project_icon( iwnd_db,1,IDB_STATIC07 )

IF( BTEST(pcscipuf_mode,IPMB_INTERACTIVE) )THEN
  CALL ShowCheckMarks( iwnd_db )
  IF( project_setup )CALL PushButton( iwnd_db,0,IDB_BUTTON19,i ) !Run create
END IF

RETURN
END
!*******************************************************************************
!            Set the icon control based on project type
!*******************************************************************************
SUBROUTINE set_project_icon( iwnd_db,id_level,id_ctrl )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN  ) :: id_level !Data level
INTEGER,              INTENT( IN  ) :: id_ctrl  !icon control

INTEGER(POINTER_LEN) i

IF( BTEST(project(id_level)%Mode,FAST_MODE) )THEN
  i = fast_hndl
ELSE
  i = scipuff_hndl
  IF( project(id_level)%Dynamic )THEN
    IF( project(id_level)%DenseGas )THEN
      i = dense_hndl
    ELSE
      i = dynamic_hndl
    END IF
  ELSE
    i = scipuff_hndl
  END IF
END IF
CALL SetControlIcon( iwnd_db,id_ctrl,i )

RETURN
END
!*******************************************************************************
!            Initialize RUN Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_run( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE pltchoice_fi
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN  ) :: id_level !Data level

INTEGER  enable(2),n,indxd
INTEGER(POINTER_LEN) i
REAL     dfac
!
!     Buttons
!
enable(1) = TRUE !RUN
CALL EnableButtons( iwnd_db,enable,1,1 )
!
!     Icon
!
IF( BTEST(project(BASE_LEVEL)%Mode,FAST_MODE) )THEN
  i = fast_hndl
ELSE
  i = scipuff_hndl
  IF( project(BASE_LEVEL)%Dynamic )THEN
    IF( project(BASE_LEVEL)%DenseGas )THEN
      i = dense_hndl
    ELSE
      i = dynamic_hndl
    END IF
  ELSE
    i = scipuff_hndl
  END IF
END IF
CALL SetControlIcon( iwnd_db,IDB_STATIC07,i )
!
!     Check Boxes
!
lcheck(1,id_level) = rundef(EDIT_LEVEL_1)%on
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )
!
!     Radio Buttons
!
nradio(1,id_level)  = 2
IF( LEN_TRIM(rundef(EDIT_LEVEL_1)%optionsFile) > 0 )THEN
  string1 = TRIM(rundef(EDIT_LEVEL_1)%optionsFile)
  CALL SetControlText( iwnd_db,IDB_STATIC42,string1 )
  ichoice(1,id_level) = 2
ELSE
  ichoice(1,id_level) = 1
END IF
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )
IF( ichoice(1,id_level) == 1 )THEN
  CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE )
ELSE
  CALL EnableControl( iwnd_db,IDB_BUTTON4,TRUE )
END IF
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
dbreal(3,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%time%hour

IF( dlgTime(EDIT_LEVEL_1)%time%end%time%runTime == NOT_SET_R )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%time%runTime == 0. .OR. dlgTime(EDIT_LEVEL_1)%time%end%time%runTime >= 1. )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%time%runTime >= 1./60. )THEN
  dfac  = 60.
  indxd = 2
ELSE
  dfac  = 3600.
  indxd = 1
END IF
idbcmbo(1,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO1,id_level )
dbreal(4,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%time%runTime*dfac

IF( dlgTime(EDIT_LEVEL_1)%time%end%step%max == NOT_SET_R )THEN
  dfac  = 1.
  indxd = 1
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%step%max == 0. .OR. dlgTime(EDIT_LEVEL_1)%time%end%step%max <= 300. )THEN
  dfac  = 1.
  indxd = 1
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%step%max <= 1200. )THEN
  dfac  = 60.
  indxd = 2
ELSE
  dfac  = 3600.
  indxd = 3
END IF
idbcmbo(2,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO2,id_level )
dbreal(1,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%step%max/dfac

IF( dlgTime(EDIT_LEVEL_1)%time%end%step%output == NOT_SET_R .OR. dlgTime(EDIT_LEVEL_1)%time%end%step%output == DEF_VAL_R )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%step%output == 0. .OR. dlgTime(EDIT_LEVEL_1)%time%end%step%output >= 1. )THEN
  dfac  = 1.
  indxd = 3
ELSE IF( dlgTime(EDIT_LEVEL_1)%time%end%step%output >= 1./60. )THEN
  dfac  = 60.
  indxd = 2
ELSE
  dfac  = 3600.
  indxd = 1
END IF
idbcmbo(3,id_level) = indxd
CALL build_release_Tunit( iwnd_db,IDB_COMBO3,id_level )
dbreal(2,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%step%output*dfac

CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )
!
!     Edit Boxes - Integers
!
dbint(4,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%time%year
dbint(5,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%time%month
dbint(6,id_level) = dlgTime(EDIT_LEVEL_1)%time%end%time%day
dbint(7,id_level) = rundef(EDIT_LEVEL_1)%update
CALL SetEditIs( iwnd_db,dbint(4,id_level),4,4 )
!
!     Static Boxes
!
string1 = TRIM(project(BASE_LEVEL)%ID%path)
CALL SetControlText( iwnd_db,IDB_STATIC31,string1 )
string1 = TRIM(project(BASE_LEVEL)%ID%name)
CALL SetControlText( iwnd_db,IDB_STATIC30,string1 )

IF( project(BASE_LEVEL)%Puff )THEN
   CALL c_format( timePuff(nTimePuff)%time%runTime,n,string2 )
   string1 = TRIM(timePuff(nTimePuff)%string)//' ( '//TRIM(string2)//'hrs )'
ELSE
   CALL time_string( dlgTime(EDIT_LEVEL_1)%time%start%time,string2 )
   string1 = TRIM(string2)//' ( 0.0hrs )'
END IF
string1 = ADJUSTL(string1)
CALL SetControlText( iwnd_db,IDB_STATIC32,string1 )

IF( project(BASE_LEVEL)%Puff )THEN
  WRITE(string1,'(I6)')timePuff(nTimePuff)%nItems
  string1 = ADJUSTL(string1)
ELSE
  string1 = '0'
END IF
CALL SetControlText( iwnd_db,IDB_STATIC33,string1 )

IF( rundef(EDIT_LEVEL_1)%on )THEN
  CALL EnableControl( iwnd_db,IDB_INT7,TRUE )
  CALL EnableControl( iwnd_db,IDB_RADIO01,TRUE )
  CALL EnableControl( iwnd_db,IDB_RADIO02,TRUE )
  IF( ichoice(1,id_level) == 2 )THEN
    CALL EnableControl( iwnd_db,IDB_BUTTON4,TRUE )
  ELSE
    CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE )
  END IF
ELSE
  CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE )
  CALL EnableControl( iwnd_db,IDB_INT7,FALSE )
  CALL EnableControl( iwnd_db,IDB_RADIO01,FALSE )
  CALL EnableControl( iwnd_db,IDB_RADIO02,FALSE )
END IF

RETURN
END
!*******************************************************************************
!            Initialize DELETE Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_delete( iwnd_db )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE animate_fi
USE SwinAPI
!
!     This routine initializes the DELETE Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN  ) :: iwnd_db !Dialog handle

INTEGER irv,i,n
INTEGER  enable(2)
CHARACTER(64) list(50)
CHARACTER(PATH_MAXLENGTH) filename
!
!     Buttons
!
enable(1) = TRUE !DELETE
CALL EnableButtons( iwnd_db,enable,1,1 )
!
!     Static Text
!
IF( AnimDel )THEN
  CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC21,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC12,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC22,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC13,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC23,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC15,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC25,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC26,SW_HIDE )
  CALL SetControlText( iwnd_db,IDB_STATIC24,TRIM(path_animate) )
  CALL SetControlText( iwnd_db,IDB_STATIC01,'Animation Delete Control' )
  CALL SetControlText( iwnd_db,IDB_STATIC10,'File Description' )
ELSE
  CALL SetControlText( iwnd_db,IDB_STATIC21,TRIM(project(BASE_LEVEL)%audit%Analyst) )
  CALL SetControlText( iwnd_db,IDB_STATIC22,TRIM(project(BASE_LEVEL)%audit%CreateDate) )
  CALL SetControlText( iwnd_db,IDB_STATIC23,TRIM(project(BASE_LEVEL)%Title) )
  CALL SetControlText( iwnd_db,IDB_STATIC24,TRIM(project(BASE_LEVEL)%ID%path) )
  CALL SetControlText( iwnd_db,IDB_STATIC15,' ' )
  CALL SetControlText( iwnd_db,IDB_STATIC25,' ' )
  CALL SetControlText( iwnd_db,IDB_STATIC26,TRIM(project(BASE_LEVEL)%audit%Version) )
END IF
!
!     List Boxes
!
n = 0
IF( AnimDel )THEN
100  WRITE(filename,1000)TRIM(file_animate),n+1
1000 FORMAT(A,'.',I3.3)
   CALL AddPath( filename,path_animate )
   i = n
   CALL SetFileList( filename,list,n )
   IF( n > i .AND. n < 50 )GOTO 100
ELSE
  CALL SetFileList( file_dep,list,n )
  CALL SetFileList( file_dos,list,n )
  CALL SetFileList( file_err,list,n )
  CALL SetFileList( file_inp,list,n )
  CALL SetFileList( file_log,list,n )
  CALL SetFileList( file_msc,list,n )
  CALL SetFileList( file_prj,list,n )
  CALL SetFileList( file_puf,list,n )
  filename = TRIM(project(BASE_LEVEL)%ID%path)//TRIM(project(BASE_LEVEL)%ID%name)//'.rst'
  CALL SetFileList( filename,list,n )
  CALL SetFileList( file_scn,list,n )
  filename = TRIM(project(BASE_LEVEL)%ID%path)//TRIM(project(BASE_LEVEL)%ID%name)//'.sfo'
  CALL SetFileList( filename,list,n )
  CALL SetFileList( file_mcw,list,n )
END IF

CALL ClearList( iwnd_db,IDB_LIST1 )
IF( n > 0 )THEN
  DO i = 1,n
    CALL AddList( iwnd_db,IDB_LIST1,-999,list(i),irv )
  END DO
END IF

RETURN
END
!*******************************************************************************
!            Setup File List for project deletion
!*******************************************************************************
SUBROUTINE SetFileList( filename,list,n )

USE resource_fd
USE reldef_fd
USE tooluser_fd

IMPLICIT NONE

CHARACTER(*),               INTENT( IN    ) :: filename
CHARACTER(*), DIMENSION(*), INTENT( INOUT ) :: list
INTEGER ,                   INTENT( INOUT ) :: n

CHARACTER(PATH_MAXLENGTH) file,path

LOGICAL, EXTERNAL :: CheckFile

IF( CheckFile(filename) )THEN
  n = n + 1
  CALL SplitName( filename,file,path )
  list(n) = TRIM(file)
ELSE
  CALL InitError()
END IF

RETURN
END
!*******************************************************************************
!            Initialize NewSetup Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_newsetup( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the NewSetup Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv,indx
TYPE( CMD ) MyCmd !Command Structure

CHARACTER(128), EXTERNAL :: AddNull
!
!     Radio Buttons
!
nradio(1,id_level)  = 3 !Coordinate Type
ichoice(1,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord
nradio(2,id_level)  = 2 !Time Type
IF( dlgTime(EDIT_LEVEL_2)%time%start%time%reference == HT_LOCAL )THEN
  ichoice(2,id_level) = 2
ELSE
  ichoice(2,id_level) = 1
END IF
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
  dbint(3,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM !UTM Zone
ELSE
  dbint(3,id_level) = dlgDomain(DEFAULT_LEVEL)%spatial%domain%zoneUTM
END IF
!
!     Check Boxes
!
lcheck(1,id_level) = dlgDomain(EDIT_LEVEL_2)%hasReference !Reference point
!
!     Edit Boxes - Reals
!
dbreal(1,id_level)  = dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon !Lon origin
dbreal(2,id_level)  = dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat !Lat origin
dbreal(10,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%x !X origin
dbreal(11,id_level) = dlgDomain(EDIT_LEVEL_2)%spatial%reference%y !Y origin
CALL compute_DMS( dbreal(1,id_level),dbint(7,id_level),dbint(13,id_level),dbreal(13,id_level) )
CALL compute_DMS( dbreal(2,id_level),dbint(8,id_level),dbint(14,id_level),dbreal(14,id_level) )
!
!     Edit Boxes - Integers
!
IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == DEF_VAL_R )THEN
  dbint(1,id_level) = DEF_VAL_I
  dbint(2,id_level) = DEF_VAL_I
ELSE IF( dlgTime(EDIT_LEVEL_2)%time%start%zone == NOT_SET_R )THEN
  dbint(1,id_level) = NOT_SET_I
  dbint(2,id_level) = NOT_SET_I
ELSE
  dbint(1,id_level) = INT(dlgTime(EDIT_LEVEL_2)%time%start%zone)
  dbint(2,id_level) = NINT(60.*(dlgTime(EDIT_LEVEL_2)%time%start%zone - FLOAT(dbint(1,id_level))))
  IF( dbint(2,id_level) > 59 )THEN
    dbint(2,id_level) = dbint(2,id_level) - 60
    dbint(1,id_level) = dbint(1,id_level) + 1
  END IF
END IF
!
!     Combot/List Box
!
string1 = 'Passive (No dynamics) '
string2 = 'Dynamics (Buoyant)'
string3 = 'Dense gas dynamics'
CALL AddList( iwnd_db,IDB_COMBO2,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO2,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO2,-1,string3,irv )
IF( project(EDIT_LEVEL_2)%Dynamic )THEN
  IF( project(EDIT_LEVEL_2)%DenseGas )THEN
    string4 = string3
  ELSE
    string4 = string2
  END IF
ELSE
  string4 = string1
END IF
CALL SetControlIcon( iwnd_db,IDB_STATIC71,scipuff_hndl )
CALL SetControlIcon( iwnd_db,IDB_STATIC72,dynamic_hndl )
CALL SetControlIcon( iwnd_db,IDB_STATIC73,dense_hndl )

CALL SetListSelString( iwnd_db,IDB_COMBO2,string4,irv )

MyCmd%id     = IDB_SETUP
MyCmd%cntrl  = IDB_COMBO2
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Disable static puffs'
string2 = 'Enable static puffs'
CALL AddList( iwnd_db,IDB_COMBO6,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO6,-1,string2,irv )
IF( project(EDIT_LEVEL_2)%StaticPuffs )THEN
  string4 = string2
ELSE
  string4 = string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO6,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO6
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Standard'
string2 = 'Fast'
CALL AddList( iwnd_db,IDB_COMBO8,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO8,-1,string2,irv )
IF( BTEST(project(EDIT_LEVEL_2)%Mode,FAST_MODE) )THEN
  string4 = string2
ELSE
  string4 = string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO8,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO8
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Hazard Area Off'
string2 = 'Hazard Area On'
string3 = 'Dual Run'
CALL AddList( iwnd_db,IDB_COMBO7,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO7,-1,string2,irv )
CALL AddList( iwnd_db,IDB_COMBO7,-1,string3,irv )
string4 = string1

CALL SetListSelString( iwnd_db,IDB_COMBO7,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO7
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

string1 = 'Disable source nests'
string2 = 'Enable source nests'
CALL AddList( iwnd_db,IDB_COMBO9,-1,string1,irv )
CALL AddList( iwnd_db,IDB_COMBO9,-1,string2,irv )
IF( project(EDIT_LEVEL_2)%SourceNests )THEN
  string4 = string2
ELSE
  string4 = string1
END IF

CALL SetListSelString( iwnd_db,IDB_COMBO9,string4,irv )

MyCmd%id     = IDB_PRJDEF
MyCmd%cntrl  = IDB_COMBO9
MyCmd%type   = MyCmd%cntrl/CONTROL_INDEX
MyCmd%button = MyCmd%cntrl - COMBO_BASE
MyCmd%level  = id_level
CALL process_combo( iwnd_db,MyCmd )

CALL EnableControl( iwnd_db,IDB_COMBO10,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO10,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC40,SW_HIDE )

!
!     Load dialog controls
!
nradio(4,id_level)  = 3 !Dynamics Type
ichoice(4,id_level) = idbcmbo(2,id_level)
CALL SetEditIs( iwnd_db,dbint(3 ,id_level),3 ,1 )
CALL SetEditRs( iwnd_db,dbreal(1 ,id_level),1 ,2 )
CALL SetEditRs( iwnd_db,dbreal(10,id_level),10,2 )
CALL FormatEditIs( iwnd_db,'(i2.2)',dbint(1,id_level),1,2 )
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,4 )
CALL SetChecks( iwnd_db,lcheck(1,id_level),1,1 )

IF( .NOT.UTM_init )CALL EnableControl( iwnd_db,IDB_RADIO03,FALSE )

CALL ClearList( iwnd_db,IDB_COMBO4 )
string1 = 'Degrees'
CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv )
string1 = 'Deg/Min/Sec'
CALL AddList( iwnd_db,IDB_COMBO4,-1,string1,irv )
idbcmbo(4,id_level) = 1
indx = idbcmbo(4,id_level) - 1
CALL SetListSel( iwnd_db,IDB_COMBO4,indx,irv )
CALL GetListSel( iwnd_db,IDB_COMBO4,1,indx,irv )
string2 = 'Not Set'
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,IDB_COMBO4,indx,string2,irv )
  IF( irv > 0 )THEN
    idbcmbo(4,id_level) = indx + 1
    dbcmbo(4,id_level)  = AddNull( TRIM(string2(1:irv)) )
  END IF
END IF
idbcmbo(3,id_level) = 1
CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level), &
                           idbcmbo(3,id_level),idbcmbo(4,id_level),lcheck(1,id_level) )

!==== Show/Hide hazard controls

CALL EnableControl( iwnd_db,IDB_COMBO7,FALSE )
CALL ShowControl( iwnd_db,IDB_COMBO7,SW_HIDE )
CALL ShowControl( iwnd_db,IDB_STATIC16,SW_HIDE )

RETURN
END
!*******************************************************************************
!            Initialize Restart Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_restart( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE SwinAPI
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER irv,i

LOGICAL, EXTERNAL :: hasError

CALL GetRestartInfo( file_puf )
IF( hasError() )THEN
  CALL ShowErrorMessage( iwnd_db )
  nTimeRestart = 0
END IF

CALL ClearList( iwnd_db,IDB_COMBO1 )

IF( nTimeRestart <= 0 )THEN
  CALL EnableControl( iwnd_db,IDB_BUTTON1,FALSE )
  CALL EnableControl( iwnd_db,IDB_COMBO1,FALSE )
  CALL ShowControl( iwnd_db,IDB_COMBO1,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC11,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC12,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC14,SW_HIDE )
  string1 = 'No Restart times available in requested project'
  CALL SetControlText( iwnd_db,IDB_STATIC13,TRIM(string1) )
ELSE
  CALL SetControlText( iwnd_db,IDB_STATIC13,TRIM(file_puf) )
  DO i = 1,nTimeRestart !
    CALL format_time( timeRestart(i)%time%runTime,string1,0 )
    CALL AddList( iwnd_db,IDB_COMBO1,-1,string1,irv ) !Put String in COMBO BOX
  END DO !
  idbcmbo(1,id_level) = nTimeRestart
  CALL format_time( timeRestart(nTimeRestart)%time%runTime,string1,0 )
  dbcmbo(1,id_level) = TRIM(string1)
  CALL SetListSelString( iwnd_db,IDB_COMBO1,dbcmbo(1,id_level),irv ) !  Select this time in COMBO BOX
END IF

RETURN
END
!*******************************************************************************
!            Initialize Observer Dialog Box
!*******************************************************************************
SUBROUTINE init_dialog_observe( iwnd_db,id_level )
!
!     This routine initializes the Observer Dialog Box
!

USE pltchoice_fi
USE pcscipuf_fi
USE tooluser_fd
USE SwinAPI

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Dialog array pointer

REAL, DIMENSION(:), ALLOCATABLE :: ClassDataArray
INTEGER nClassData

INTEGER, EXTERNAL :: NumClassDataArray
INTEGER ios

REAL, EXTERNAL :: ScaleReal

nClassData = NumClassDataArray( PlotDef(EDIT_LEVEL) )

ALLOCATE( ClassDataArray(MAX( nClassData,1)),STAT=ios )
ClassDataArray = NOT_SET_R
ClassDataArray(nClassdata) = FLOAT(SCIPoff)

IF( ios == 0 )THEN

  CALL SetClassDataArray( PlotDef(EDIT_LEVEL),nClassData,ClassDataArray )

  dbreal(1,id_level) = ClassdataArray(2)
  dbreal(2,id_level) = ClassdataArray(3)
  dbreal(3,id_level) = ClassdataArray(4)
  dbreal(4,id_level) = ClassdataArray(6)

  IF( nClassData > 6 )dbreal(5,id_level) = ScaleReal(ClassdataArray(7),100.)

  nradio(1,id_level) = 2
  IF( ClassdataArray(5) /= SCIPoff )THEN
    ichoice(1,id_level) = 2
  ELSE
    ichoice(1,id_level) = 1
  END IF

ELSE

  dbreal(1,id_level) = DEF_VAL_R
  dbreal(2,id_level) = DEF_VAL_R
  dbreal(3,id_level) = DEF_VAL_R

  nradio(1,id_level)  = 2
  ichoice(1,id_level) = 1

  dbreal(4,id_level) = NOT_SET_R
  dbreal(5,id_level) = NOT_SET_R

END IF

IF( nClassData > 6 )THEN
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,5 )
ELSE
  CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,4 )
  CALL EnableControl( iwnd_db,IDB_REAL5,FALSE )
  CALL ShowControl( iwnd_db,IDB_REAL5,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC29,SW_HIDE )
  CALL ShowControl( iwnd_db,IDB_STATIC30,SW_HIDE )
END IF
CALL SetRadios( iwnd_db,ichoice(1,id_level),nradio(1,id_level),1,1 )

IF( ALLOCATED(ClassDataArray) )DEALLOCATE( ClassDataArray,STAT=ios )

RETURN
END
