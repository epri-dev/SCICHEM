!***********************************************************************
!                save_dialog
!***********************************************************************
SUBROUTINE save_dialog( iwnd_db,id,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE create_fi
USE GUImatl_fi
USE plotdlg_fi
USE GUItool_fi
USE files_fi
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
lokbutton  = .TRUE.  !RELDEF,MATDEF flag

SELECT CASE( id )
  CASE( IDB_PLOT )
    ltool = .TRUE.
    CALL save_dialog_plot( id_level )
    CALL DeallocateContours( 0,BASE_LEVEL )
	  PlotDef(BASE_LEVEL) = PlotDef(EDIT_LEVEL)
	  DO i = 1,NUM_CONTOUR
	    ContourList( i,BASE_LEVEL) = ContourList( i,EDIT_LEVEL)
	    ContourBuild(i,BASE_LEVEL) = ContourBuild(i,EDIT_LEVEL)
	  END DO
    lplotOK  = .TRUE.
    lprintOK = .FALSE.

  CASE( IDB_CONTOUR )
    CALL save_dialog_contour( id_level )

  CASE( IDB_PLTOPT )
    CALL save_dialog_pltopt( id_level )
    poptdef(BASE_LEVEL) = poptdef(EDIT_LEVEL)

  CASE( IDB_PLTMORE )
    CALL save_dialog_pltmore( id_level )
    popt2def(BASE_LEVEL) = popt2def(EDIT_LEVEL)

  CASE( IDB_MAPS )
    CALL save_dialog_map( id_level )
    mapdef(BASE_LEVEL) = mapdef(EDIT_LEVEL)

  CASE( IDB_AXES )
    CALL save_dialog_axes( id_level )
    axesdef(BASE_LEVEL) = axesdef(EDIT_LEVEL)
    lplotOK  = .FALSE.
    lprintOK = .FALSE.

  CASE( IDB_ANIMAT )
    CALL save_dialog_animate( id_level )

  CASE( IDB_PLAY )
    CALL save_dialog_play()

  CASE( IDB_LABELS )
    CALL save_dialog_titles( id_level )
    ttldef(BASE_LEVEL) = ttldef(EDIT_LEVEL)

  CASE( IDB_SLICE )
    ldestroy = .TRUE.
    CALL save_dialog_slice( id_level )

  CASE( IDB_AUDIT )
    CALL save_dialog_audit( id_level )

  CASE( IDB_DELPRJ )
    CALL save_dialog_delete( iwnd_db )

  CASE( IDB_ZOOM )
    ldestroy = .TRUE.
    CALL save_dialog_zoom( id_level )
    axesdef(BASE_LEVEL) = axesdef(EDIT_LEVEL)
    lplotOK  = .FALSE.
    lprintOK = .FALSE.

  CASE( IDB_EDTPRJ )
    ltool = .TRUE.
    dlgTime(   BASE_LEVEL) = dlgTime(   EDIT_LEVEL_1)
    dlgDomain( BASE_LEVEL) = dlgDomain( EDIT_LEVEL_1)
    metdef(    BASE_LEVEL) = metdef(    EDIT_LEVEL_1)
    dlgOptions(BASE_LEVEL) = dlgOptions(EDIT_LEVEL_1)
    project(   BASE_LEVEL) = project(   EDIT_LEVEL_1)
    CALL CopyMaterial( materials(EDIT_LEVEL_1),materials(BASE_LEVEL) )
    CALL CopyScenario(  scenario(EDIT_LEVEL_1), scenario(BASE_LEVEL) )
    lplotOK  = .FALSE.
    lprintOK = .FALSE.

  CASE( IDB_PRJDEF )
    CALL save_dialog_prjdef( iwnd_db,id_level )
    project(EDIT_LEVEL_1) = project(EDIT_LEVEL_2)
    CALL CopyMaterial( materials(EDIT_LEVEL_2),materials(EDIT_LEVEL_1) )

  CASE( IDB_OPTIONS )
    CALL save_dialog_options( id_level )
    dlgOptions(EDIT_LEVEL_1) = dlgOptions(EDIT_LEVEL_2)

  CASE( IDB_TIME )
    CALL save_dialog_time( id_level )
    dlgTime(  EDIT_LEVEL_1) = dlgTime(  EDIT_LEVEL_2)
    dlgDomain(EDIT_LEVEL_1) = dlgDomain(EDIT_LEVEL_2)
    project(  EDIT_LEVEL_1) = project(  EDIT_LEVEL_2)

  CASE( IDB_DOMAIN )
    CALL save_dialog_domain( id_level )
    dlgDomain(EDIT_LEVEL_1) = dlgDomain(EDIT_LEVEL_2)
    dlgTime(  EDIT_LEVEL_1) = dlgTime(  EDIT_LEVEL_2)
    metdef(   EDIT_LEVEL_1) = metdef(   EDIT_LEVEL_2)

  CASE( IDB_METDEF )
    CALL save_dialog_metdef( iwnd_db,id_level )
    metdef( EDIT_LEVEL_1) = metdef( EDIT_LEVEL_2)
    project(EDIT_LEVEL_1) = project(EDIT_LEVEL_2)

  CASE( IDB_RELDEF )
    CALL save_dialog_reldef()
    CALL CopyScenario(scenario( EDIT_LEVEL_2),scenario(EDIT_LEVEL_1) )
    dlgDomain(EDIT_LEVEL_1) = dlgDomain(EDIT_LEVEL_2)
    dlgTime(  EDIT_LEVEL_1) = dlgTime(  EDIT_LEVEL_2)
    project(  EDIT_LEVEL_1) = project(  EDIT_LEVEL_2)

  CASE( IDB_RELNEW )
    CALL save_dialog_relnew( id_level )

  CASE( IDB_MATDEF )
    CALL save_dialog_matdef()
    CALL CopyMaterial( materials(EDIT_LEVEL_2),materials(EDIT_LEVEL_1) )
    CALL CopyScenario( scenario(EDIT_LEVEL_2),scenario(EDIT_LEVEL_1) )
    project(EDIT_LEVEL_1) = project(EDIT_LEVEL_2)

  CASE( IDB_MATNEW )
    CALL save_dialog_matnew( id_level )

  CASE ( IDB_AERPARM,IDB_LIQPARM,IDB_PRTPARM,IDB_GASPARM )
    CALL save_dialog_matparam( iwnd_db,id_level )

  CASE( IDB_RNDPARM )
    CALL save_dialog_random( id_level )

  CASE( IDB_UNCPARM )
    CALL save_dialog_uncertainty( id_level )

  CASE( IDB_MULTICOMP )
    CALL save_dialog_multicomp( iwnd_db,id_level )

  CASE( IDB_TERPARM )
    CALL save_dialog_terrain( id_level )

  CASE( IDB_EDTLST )
    CALL save_dialog_edtlst( id_level )

  CASE( IDB_NEWLST )
    CALL save_dialog_newlst( iwnd_db,id_level )

  CASE( IDB_COMLST )
    CALL save_dialog_matcmp( iwnd_db,id_level )

  CASE( IDB_XYTABLE )
    CALL save_dialog_xytab( iwnd_db,id_level )

  CASE( IDB_SCIPUF )
    ltool = .TRUE.
    CALL save_dialog_run( iwnd_db,id_level )
    dlgTime(BASE_LEVEL) = dlgTime(EDIT_LEVEL_1)
    rundef( BASE_LEVEL) = rundef( EDIT_LEVEL_1)

  CASE( IDB_SETUP )
    project_setup = .FALSE.
    CALL save_dialog_newsetup( iwnd_db,id_level )
    dlgDomain(EDIT_LEVEL_1) = dlgDomain(EDIT_LEVEL_2)
    dlgTime(  EDIT_LEVEL_1) = dlgTime(  EDIT_LEVEL_2)
    project(  EDIT_LEVEL_1) = project(  EDIT_LEVEL_2)
    CALL CopyMaterial( materials(EDIT_LEVEL_2),materials(EDIT_LEVEL_1) )

  CASE( IDB_RESTART )
    CALL save_dialog_restart( id_level )
    project(EDIT_LEVEL_1) = project(EDIT_LEVEL_2)

  CASE( IDB_ERRORBX )
    CALL save_dialog_errorbox()

  CASE( IDB_OBSERVE )
    CALL save_dialog_observe( id_level )

  CASE DEFAULT

END SELECT

RETURN
END
!*******************************************************************************
!            save PLOT Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_plot( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE plotdlg_fi
USE dialog_fi
USE pltchoice_fi
USE basic_fd
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER ic

CHARACTER(128), EXTERNAL :: StripNull
!
!     Check Boxes
!
PlotDef(EDIT_LEVEL)%Mode = NULL

IF( lcheck(1,id_level) )PlotDef(EDIT_LEVEL)%Mode = PlotDef(EDIT_LEVEL)%Mode + PLOT_DRAW

IF( lcheck(2,id_level) )PlotDef(EDIT_LEVEL)%Mode = PlotDef(EDIT_LEVEL)%Mode + PLOT_FILL
!
!     Radio Buttons - Mean/Prob - changed to combobox
!
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
ic = PlotDef(EDIT_LEVEL)%ContourIndex

PlotDef(EDIT_LEVEL)%ClassData(HSLICE_INDEX)%zmin = dbreal(2,id_level)

PlotDef(EDIT_LEVEL)%TypeData(HP_PROB) = dbreal(EXCD_EDIT_ID,id_level) &
                                     / ContourList(ic,EDIT_LEVEL)%ListHdr%Scale

PlotDef(EDIT_LEVEL)%TypeData(HP_EXCEED) = dbreal(4,id_level)
PlotDef(EDIT_LEVEL)%TypeData(HP_MEAN)   = dbreal(5,id_level)
!
!     Edit Boxes - Integers
!
!
!     Combo Boxes
!
!     Variable type
!

!     PLOT Type

!     TIME

IF( nTimePlot > 0 )THEN
IF( TRIM(StripNull(timePlot(PlotDef(EDIT_LEVEL)%Field%TimeID)%string)) == 'User Input' )THEN
  PlotDef(EDIT_LEVEL)%Field%UserTime = dbreal(3,id_level)*24.

ELSE
  PlotDef(EDIT_LEVEL)%Field%UserTime = timePlot(PlotDef(EDIT_LEVEL)%Field%TimeID)%time%runTime

END IF
ELSE
  PlotDef(EDIT_LEVEL)%Field%UserTime = PlotDef(BASE_LEVEL)%Field%UserTime
END IF

!---- Set Flags

PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
                              PlotDef(EDIT_LEVEL)%Field%Category == PlotDef(BASE_LEVEL)%Field%Category .AND. &
                              PlotDef(EDIT_LEVEL)%Field%Class    == PlotDef(BASE_LEVEL)%Field%Class    .AND. &
                              PlotDef(EDIT_LEVEL)%Field%Choice   == PlotDef(BASE_LEVEL)%Field%Choice   .AND. &
                              PlotDef(EDIT_LEVEL)%Field%Kind     == PlotDef(BASE_LEVEL)%Field%Kind     .AND. &
                              PlotDef(EDIT_LEVEL)%Field%UserTime == PlotDef(BASE_LEVEL)%Field%UserTime

SELECT CASE( PlotDef(EDIT_LEVEL)%Field%Category )
  CASE( HP_VSLICE,HP_HINT )
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmin == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Xmin
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmax == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Xmax
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymin == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Ymin
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymax == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Ymax
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmin == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Zmin
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmax == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Zmax
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zres == PlotDef(BASE_LEVEL)%ClassData(VSLICE_INDEX)%Zres

  CASE( HP_HSLICE )
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%ClassData(HSLICE_INDEX)%Zmin == PlotDef(BASE_LEVEL)%ClassData(HSLICE_INDEX)%Zmin

  CASE( HP_TABLE )
    PlotDef(EDIT_LEVEL)%Created = PlotDef(EDIT_LEVEL)%Created .AND. &
	        PlotDef(EDIT_LEVEL)%TypeData(HP_MEAN) == PlotDef(BASE_LEVEL)%TypeData(HP_MEAN)

  CASE DEFAULT

END SELECT

!
!     List Boxes
!
RETURN
END
!*******************************************************************************
!            save CONTOUR Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_contour( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE pltchoice_fi

!
!     This routine saves the CONTOUR Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

REAL    scaling
INTEGER i,ios,idef
LOGICAL lSCIP,lcnt

CHARACTER(128), EXTERNAL :: StripNull
REAL,           EXTERNAL :: ScaleReal

!
!     Check Boxes
!
!
!     Radio Buttons
!
IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%type &
              == SCIPtrue )THEN
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

lSCIP = ichoice(1,id_level) == 3 !SCIP Flag
lcnt  = .FALSE.

IF( .NOT.lSCIP )THEN
  PlotDef(EDIT_LEVEL)%ContourIndex = USER_CONTOUR
  IF( ichoice(1,id_level) == 2) THEN
    lcnt = .TRUE.
  ELSE
    CALL DeallocateContours( USER_CONTOUR,EDIT_LEVEL )
    IF( ichoice(2,id_level) == idef )THEN
      ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Mode = PLOT_DEF
	  ELSE
      IF( ichoice(2,id_level) == 1 )THEN
        ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Mode = PLOT_LOG
	    ELSE
        ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Mode = PLOT_LIN
	    END IF
	  END IF
  END IF
END IF
!
!     Edit Boxes - Text
!
IF( .NOT.lSCIP )THEN
  ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Unit = StripNull( dbtext(1,id_level) ) !Units Axis Label
END IF
!
!     Edit Boxes - Reals
!
IF( .NOT.lSCIP )THEN
  ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Scale = dbreal(4,id_level)
  scaling = 1./dbreal(4,id_level)
END IF

IF( .NOT.lcnt .AND. .NOT.lSCIP )THEN
  ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Cmin = ScaleReal(dbreal(1,id_level),scaling)
  ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Cmax = ScaleReal(dbreal(2,id_level),scaling)
  IF( ichoice(2,id_level) == 1 )THEN
    ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Del  = dbreal(3,id_level)
  ELSE
    ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Del  = ScaleReal(dbreal(3,id_level),scaling)
  END IF
END IF
!
!     Edit Boxes - Integers
!
IF( .NOT.lcnt .AND. .NOT.lSCIP )THEN
  ContourBuild(USER_CONTOUR,EDIT_LEVEL)%Number = dbint(1,id_level)
END IF
!
!     Combo Boxes
!
!
!     List Boxes
!
!     Read list of CONTOURS if nonuniform

IF( lcnt .AND. .NOT.lSCIP )THEN
  IF( nlst(id_level) <= 0 )THEN
    ContourBuild(USER_CONTOUR,EDIT_LEVEL) = ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)
    ContourList(USER_CONTOUR,EDIT_LEVEL)  = ContourList(USER_CONTOUR,DEFAULT_LEVEL)
  ELSE
    CALL DeallocateContours( USER_CONTOUR,EDIT_LEVEL )
	  ALLOCATE( ContourList(USER_CONTOUR,EDIT_LEVEL)%ListPtr(nlst(id_level)),STAT=ios )
    IF( ios == 0 )THEN
      DO i = 1,nlst(id_level)
        ContourList(USER_CONTOUR,EDIT_LEVEL)%ListPtr(i)%Contour = dblst(id_level,i)/ &
                                    ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Scale
      END DO
      ContourList(USER_CONTOUR,EDIT_LEVEL)%ListHdr%Number = nlst(id_level)
	  ELSE
      ContourBuild(USER_CONTOUR,EDIT_LEVEL) = ContourBuild(USER_CONTOUR,DEFAULT_LEVEL)
      ContourList(USER_CONTOUR,EDIT_LEVEL)  = ContourList(USER_CONTOUR,DEFAULT_LEVEL)
	  END IF
  END IF
END IF

RETURN
END
!*******************************************************************************
!            save MAP Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_map( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
!
!     This routine saves the MAP Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level
!
!     Radio Buttons
!
mapdef(EDIT_LEVEL)%MapsOn = ichoice(1,id_level) <= 2 !No map
IF( mapdef(EDIT_LEVEL)%MapsOn )THEN
  mapdef(EDIT_LEVEL)%HiRes = ichoice(1,id_level) == 1 !Hi Res
END IF
mapdef(EDIT_LEVEL)%PPTextLo = ichoice(2,id_level) == 2 !Partial list
!
!     Check Boxes
!
IF( mapdef(EDIT_LEVEL)%MapsOn )THEN
  mapdef(EDIT_LEVEL)%PPText   = lcheck(2,id_level) !Populated places Text
  mapdef(EDIT_LEVEL)%PPSymbol = lcheck(1,id_level) !Populated places symbols
  IF( mapdef(EDIT_LEVEL)%HiRes )THEN
    mapdef(EDIT_LEVEL)%PPArea      = lcheck(3,id_level) !Populated places area
    mapdef(EDIT_LEVEL)%Roads       = lcheck(4,id_level) !Roads
    mapdef(EDIT_LEVEL)%RRs         = lcheck(5,id_level) !Railroads
    mapdef(EDIT_LEVEL)%NucFac      = lcheck(6,id_level) !Nuclear Facilities
    mapdef(EDIT_LEVEL)%Airport     = lcheck(7,id_level) !Airports
    mapdef(EDIT_LEVEL)%NucFacText  = lcheck(8,id_level) !Nuc Fac Text
    mapdef(EDIT_LEVEL)%AirportText = lcheck(9,id_level) !Airport Text
  END IF
END IF
!
!     Edit Boxes - Text
!
!
!     Edit Boxes - Reals
!
IF( lcheck(10,id_level) )THEN
  IF( ichoice(3,id_level) == 2 .OR. .NOT.PopData )THEN
    mapdef(EDIT_LEVEL)%popden = dbreal(1,id_level)
    IF( mapdef(EDIT_LEVEL)%popden == DEF_VAL_R )mapdef(EDIT_LEVEL)%popden = 1.0
  ELSE
    mapdef(EDIT_LEVEL)%popden = DEF_VAL_R
  END IF
ELSE IF( lcheck(11,id_level) )THEN
  mapdef(EDIT_LEVEL)%popden = -1.0
ELSE
  mapdef(EDIT_LEVEL)%popden = 0.0
END IF
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Edit Boxes - Integers
!
RETURN
END
!*******************************************************************************
!            save MORE Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_pltopt( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
!
!     This routine saves the Additional Plot Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

CHARACTER(128), EXTERNAL :: StripNull

!
!     Check Boxes
!
poptdef(EDIT_LEVEL)%Max      = lcheck(1,id_level) !Show Max Value
poptdef(EDIT_LEVEL)%Cell     = lcheck(2,id_level) !Draw Cells
poptdef(EDIT_LEVEL)%Contour  = lcheck(4,id_level) !Show contour info
poptdef(EDIT_LEVEL)%Source   = lcheck(3,id_level) !Show Sources
poptdef(EDIT_LEVEL)%ColorBox = lcheck(5,id_level) !Show Color Bar
poptdef(EDIT_LEVEL)%FillLo   = lcheck(6,id_level) !Lo Contour Fill
poptdef(EDIT_LEVEL)%FillHi   = lcheck(7,id_level) !Hi Contour fill
poptdef(EDIT_LEVEL)%Srctext  = lcheck(10,id_level)!Show Sources Text

IF( project(BASE_LEVEL)%Terrain )THEN
  poptdef(EDIT_LEVEL)%Terrain  = lcheck(9,id_level) !Show Terrain contours
END IF

IF( project(BASE_LEVEL)%Weather > 0 )THEN
  poptdef(EDIT_LEVEL)%Weather  = lcheck(11,id_level) !Show Weather stations
END IF

IF( axesdef(BASE_LEVEL)%MapCoord /= I_LATLON )THEN
  poptdef(EDIT_LEVEL)%KmScale  = lcheck(12,id_level) !Show Km Scale Bar
END IF

poptdef(EDIT_LEVEL)%Overlay  = lcheck(13,id_level) !Show Overlay
IF( poptdef(EDIT_LEVEL)%Overlay )THEN
  poptdef(EDIT_LEVEL)%OverlayFile = StripNull( dbtext(1,id_level) )
ELSE
  poptdef(EDIT_LEVEL)%OverlayFile = ' '
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
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Edit Boxes - Integers
!
poptdef(EDIT_LEVEL)%Maxlev = dbint(1,id_level)

RETURN
END
!*******************************************************************************
!            save MORE Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_pltmore( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
!
!     This routine saves the Additional Plot Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level
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
popt2def(EDIT_LEVEL)%AR    = dbreal(1,id_level)
popt2def(EDIT_LEVEL)%Size  = dbreal(2,id_level)
popt2def(EDIT_LEVEL)%X     = dbreal(3,id_level)
popt2def(EDIT_LEVEL)%Y     = dbreal(4,id_level)
popt2def(EDIT_LEVEL)%Font  = dbreal(5,id_level)
!
!     Combo Boxes
!
!
!     List Boxes
!
!
!     Edit Boxes - Integers
!
RETURN
END
!*******************************************************************************
!            save SLICE Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_slice( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE files_fi
USE param_fd
USE pltchoice_fi
!
!     This routine saves the SLICE Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER(POINTER_LEN) jwnd
INTEGER jd_level,i

REAL  dtmp(6),ptmp(6),rtmp(4)

LOGICAL LatLon,Cartesian

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
ptmp(1) = dbreal(1,id_level)
ptmp(4) = dbreal(2,id_level)
ptmp(2) = dbreal(3,id_level)
ptmp(5) = dbreal(4,id_level)
ptmp(3) = axesdef(BASE_LEVEL)%Xscale
ptmp(6) = axesdef(BASE_LEVEL)%Yscale
dtmp(3) = 1.0
dtmp(6) = 1.0

CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                        0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )

LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON

CALL axes_transform( dtmp,rtmp,ptmp,LatLon,Cartesian,.FALSE.,.FALSE.)

PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmin = dtmp(1)
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymin = dtmp(4)
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Xmax = dtmp(2)
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Ymax = dtmp(5)
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmin = dbreal(5,id_level)
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zmax = dbreal(6,id_level)
!
!     Edit Boxes - Integers
!
PlotDef(EDIT_LEVEL)%ClassData(VSLICE_INDEX)%Zres = FLOAT(dbint(2,id_level)) !Vertical resolution
!
!     Combo Boxes
!
!
!     List Boxes
!
CALL FindHwndListId( IDB_PLOT,jwnd,jd_level )

lcheck( 9,jd_level) = .TRUE. !Generate slice flag

RETURN
END
!*******************************************************************************
!            save ZOOM Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_zoom( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
!
!     This routine saves the ZOOM Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

LOGICAL lok,LatLon,Cartesian

REAL rtmp(4)

INTEGER ii(4),i,j
DATA ii / 1,4,2,5 /

!---- Set Default values if necessary

lok = dbreal(1,id_level) /= dbreal(3,id_level) .OR. dbreal(1,id_level) == DEF_VAL_R
lok = lok .AND. ( dbreal(2,id_level) /= dbreal(4,id_level) .OR. dbreal(2,id_level) == DEF_VAL_R )
IF( lok )THEN
  DO i = 1,4
    j = ii(i)
    axesdef(EDIT_LEVEL)%dbreal(16+j) = dbreal(i,id_level)
  END DO
  IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
      PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
    DO i = 1,6
      axesdef(EDIT_LEVEL)%dbreal(10+i) = axesdef(EDIT_LEVEL)%dbreal(16+i)
    END DO
  ELSE
    CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(EDIT_LEVEL)%MapCoord, &
                            1,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
    LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
    Cartesian = axesdef(EDIT_LEVEL)%MapCoord /= I_LATLON
    CALL axes_transform( axesdef(EDIT_LEVEL)%dbreal(5),rtmp, &
                         axesdef(EDIT_LEVEL)%dbreal(17),LatLon, &
                         Cartesian,.FALSE.,.TRUE. )
  END IF
END IF

RETURN
END
!*******************************************************************************
!            save OpenFile Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_OpenFile( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
USE restart_fi
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level
!
!     Check Boxes
!
IF( enableRestart )THEN
  IF( lcheck(1,id_level) )THEN
    project(EDIT_LEVEL_1)%RestartTimeIndx = DEF_VAL_I
  ELSE
    project(EDIT_LEVEL_1)%RestartTimeIndx = NOT_SET_I
  END IF
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
RETURN
END
!*******************************************************************************
!            save SaveAs Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_SaveAs( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE animate_fi
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level
!
!     Check Boxes
!
!
!     Radio Buttons
!
IF( lanim_hook )THEN
  start_frame = idbcmbo(1,id_level)
  end_frame   = idbcmbo(2,id_level)
END IF !
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
RETURN
END
!*******************************************************************************
!            save Print Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_Print( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE files_fi
USE dialog_fi
USE errorParam_fd
USE plotdlg_fi
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER nn,irv,i
INTEGER nError
INTEGER,ALLOCATABLE,DIMENSION(:) :: ii

CHARACTER(128) eMessage,eInform,eRoutine

LOGICAL, EXTERNAL :: hasError

!
!     Check Boxes
!
lpplot  = lcheck(1,id_level)
lpstat  = lcheck(2,id_level)
lpclass = lcheck(3,id_level)
lpdate  = lcheck(4,id_level)
lpmtime  = lcheck(5,id_level)
lpbare  = lcheck(6,id_level)
IF( lpmtime )THEN
  CALL ListSelCount( iwnd_db,IDB_LIST1,nn )
  IF( nn > 0 )THEN
    nPrintTimes = nn
  	ALLOCATE( ii(nPrintTimes),STAT=irv )
    IF( irv > 0 )THEN
      lpmtime = .FALSE.
      nError   = WN_ERROR
      eRoutine = 'MultiplePrint'
      eMessage = 'Error allocating space for retrieving print times'
      eInform  = 'Only current plot will be printed'
      CALL SetError( nError,eMessage,eInform,' ',eRoutine )
      CALL ShowWarningMessage( iwnd_db,.TRUE. )
	  ELSE
      CALL GetListSel( iwnd_db,IDB_LIST1,nPrintTimes,ii,irv )
      IF( irv <= 0 )THEN
        lpmtime = .FALSE.
        nError   = WN_ERROR
        eRoutine = 'MultiplePrint'
        eMessage = 'Error getting times for printing'
        eInform  = 'Only current plot will be printed'
        CALL SetError( nError,eMessage,eInform,' ',eRoutine )
        CALL ShowWarningMessage( iwnd_db,.TRUE. )
      ELSE
	      IF( ALLOCATED(PrintTindx) )DEALLOCATE(PrintTindx,STAT=irv)
	      ALLOCATE( PrintTindx(nPrintTimes),STAT=irv )
        IF( irv > 0 )THEN
          lpmtime = .FALSE.
          nError   = WN_ERROR
          eRoutine = 'MultiplePrint'
          eMessage = 'Error allocating space for print times'
          eInform  = 'Only current plot will be printed'
          CALL SetError( nError,eMessage,eInform,' ',eRoutine )
          CALL ShowWarningMessage( iwnd_db,.TRUE. )
		    ELSE
          DO i = 1,nn
            PrintTindx(i) = ii(i) + 1
          END DO
  		  END IF
      END IF
    END IF
	IF( ALLOCATED(ii) )DEALLOCATE( ii,STAT=irv )
  ELSE
    lpmtime = .FALSE.
    nError   = WN_ERROR
    eRoutine = 'MultiplePrint'
    eMessage = 'No times were selected for printing'
    eInform  = 'Current plot will be printed'
    CALL SetError( nError,eMessage,eInform,' ',eRoutine )
    CALL ShowWarningMessage( iwnd_db,.TRUE. )
  END IF
END IF

lokbutton = .NOT.hasError()
CALL InitError()
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
IF( idbcmbo(1,id_level) >= 2 )THEN
  map_scale = FLOAT(Smap_scales(idbcmbo(1,id_level)-1,axesdef(BASE_LEVEL)%MapCoord))
ELSE
  map_scale = DEF_VAL_R
END IF

RETURN
END
!*******************************************************************************
!            Save TITLES Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_titles( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

INTEGER i,idlm
INTEGER(POINTER_LEN) jwnd
LOGICAL lok

CHARACTER(128), EXTERNAL :: StripNull
!
!     Buttons
!
!
!     Check Boxes
!
DO i = 1,3
  ttldef(EDIT_LEVEL)%Show(i) = lcheck(i,id_level)
  ttldef(EDIT_LEVEL)%string(i) = StripNull(dbtext(i,id_level))
  ttldef(EDIT_LEVEL)%X(i) = dbreal(2*i-1,id_level)
  ttldef(EDIT_LEVEL)%Y(i) = dbreal(2*i,id_level)
END DO

CALL FindHwndListId( IDB_PLOT,jwnd,idlm )

lok = ttldef(EDIT_LEVEL)%Show(3) .AND. ttldef(EDIT_LEVEL)%string(3) == 'default'
lok = lok .OR. (PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE)
IF( lok )THEN
  ttldef(EDIT_LEVEL)%ShowDate = lcheck(4,id_level)
  ttldef(EDIT_LEVEL)%ShowTime = lcheck(5,id_level)
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
RETURN
END
!*******************************************************************************
!            Saves AXES Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_axes( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi
!
!     This routine initializes the CONTOUR Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

REAL    atmp(6),rtmp(4)
INTEGER i
LOGICAL LatLon,Cartesian

CHARACTER(128), EXTERNAL :: StripNull
!
!     Buttons
!
!
!     Check Boxes
!
axesdef(EDIT_LEVEL)%ShowX = lcheck(1,id_level)
axesdef(EDIT_LEVEL)%ShowY = lcheck(2,id_level)
!
!     Radio Buttons
!
axesdef(EDIT_LEVEL)%MapCoord = ichoice(1,id_level)
!
!     Edit Boxes - Text
!
axesdef(EDIT_LEVEL)%Xlab  = StripNull(dbtext(1,id_level)) !Horizontal Axis Label
axesdef(EDIT_LEVEL)%Ylab  = StripNull(dbtext(2,id_level)) !Vertical Axis Label
axesdef(EDIT_LEVEL)%Xformat   = StripNull(dbtext(3,id_level)) !Horizontal Axis Format
axesdef(EDIT_LEVEL)%Yformat   = StripNull(dbtext(4,id_level)) !Vertical Axis Format
!
!     Edit Boxes - Reals
!
IF( dbreal(3,id_level) /= DEF_VAL_R )THEN
  dbreal(3,id_level) = dbreal(3,id_level)*dbreal(5,id_level)
END IF

IF( dbreal(4,id_level) /= DEF_VAL_R )THEN
  dbreal(4,id_level) = dbreal(4,id_level)*dbreal(5,id_level)
END IF

IF( dbreal(6,id_level) /= DEF_VAL_R )THEN
  dbreal(6,id_level) = dbreal(6,id_level)*dbreal(8,id_level)
END IF

IF( dbreal(7,id_level) /= DEF_VAL_R )THEN
  dbreal(7,id_level) = dbreal(7,id_level)*dbreal(8,id_level)
END IF

atmp(1) = dbreal(3,id_level)
atmp(2) = dbreal(4,id_level)
atmp(3) = dbreal(5,id_level)
atmp(4) = dbreal(6,id_level)
atmp(5) = dbreal(7,id_level)
atmp(6) = dbreal(8,id_level)

IF( .NOT.(project(BASE_LEVEL)%MapCoord == I_LATLON .AND. axesdef(EDIT_LEVEL)%MapCoord == I_UTM) )THEN
  axesdef(EDIT_LEVEL)%Lon0 = dbreal(1,id_level)
  axesdef(EDIT_LEVEL)%Lat0 = dbreal(2,id_level)
  axesdef(EDIT_LEVEL)%X0 = dbreal(9,id_level)
  axesdef(EDIT_LEVEL)%Y0 = dbreal(10,id_level)
END IF

DO i = 1,6
  axesdef(EDIT_LEVEL)%dbreal(16+i) = atmp(i)
END DO
IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
  DO i = 1,6
    axesdef(EDIT_LEVEL)%dbreal(10+i) = axesdef(EDIT_LEVEL)%dbreal(16+i)
  END DO
ELSE
  CALL set_llc_reference( project(BASE_LEVEL)%MapCoord,axesdef(EDIT_LEVEL)%MapCoord, &
                          1,rtmp(2),rtmp(1),rtmp(3),rtmp(4),i )
  LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
  Cartesian = axesdef(EDIT_LEVEL)%MapCoord /= I_LATLON
  CALL axes_transform( axesdef(EDIT_LEVEL)%dbreal(5),rtmp, &
                       axesdef(EDIT_LEVEL)%dbreal(17),LatLon, &
                       Cartesian,.FALSE.,.TRUE. )
END IF
!
!     Edit Boxes - Integers
!
IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
  axesdef(EDIT_LEVEL)%TicH = dbint(1,id_level)
  axesdef(EDIT_LEVEL)%TicV = dbint(2,id_level)
ELSE
  axesdef(EDIT_LEVEL)%TicX = dbint(1,id_level)
  axesdef(EDIT_LEVEL)%TicY = dbint(2,id_level)
END IF
!
!     Combo Boxes
!
!
!     List Boxes
!
RETURN
END
!*******************************************************************************
!            save RUN Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_run( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
!
!     This routine saves the PLOT Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

LOGICAL check_YMD

CHARACTER(128), EXTERNAL :: StripNull
!
!     Check Boxes
!
rundef(EDIT_LEVEL_1)%on = lcheck(1,id_level)
IF( rundef(EDIT_LEVEL_1)%on )THEN
  rundef(EDIT_LEVEL_1)%update = dbint(7,id_level)
  IF( ichoice(1,id_level) == 1 )THEN
    rundef(EDIT_LEVEL_1)%optionsFile = ' '
  ELSE
    CALL GetControlText( iwnd_db,IDB_STATIC42,string1 )
    rundef(EDIT_LEVEL_1)%optionsFile = TRIM(StripNull(string1))
  END IF
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
dlgTime(EDIT_LEVEL_1)%time%end%time%runTime  = dbreal(4,id_level)
SELECT CASE( idbcmbo(1,id_level) )
  CASE( 1 )
    dlgTime(EDIT_LEVEL_1)%time%end%time%runTime = dlgTime(EDIT_LEVEL_1)%time%end%time%runTime/3600.
  CASE( 2 )
    dlgTime(EDIT_LEVEL_1)%time%end%time%runTime = dlgTime(EDIT_LEVEL_1)%time%end%time%runTime/60.
  CASE DEFAULT
END SELECT

dlgTime(EDIT_LEVEL_1)%time%end%step%max = dbreal(1,id_level)

SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    dlgTime(EDIT_LEVEL_1)%time%end%step%max = dlgTime(EDIT_LEVEL_1)%time%end%step%max*60.
  CASE( 3 )
    dlgTime(EDIT_LEVEL_1)%time%end%step%max = dlgTime(EDIT_LEVEL_1)%time%end%step%max*3600.
  CASE DEFAULT
END SELECT

dlgTime(EDIT_LEVEL_1)%time%end%step%output = dbreal(2,id_level)

IF( dlgTime(EDIT_LEVEL_1)%time%end%step%output /= DEF_VAL_R )THEN
  SELECT CASE( idbcmbo(3,id_level) )
    CASE( 1 )
      dlgTime(EDIT_LEVEL_1)%time%end%step%output = dlgTime(EDIT_LEVEL_1)%time%end%step%output/3600.
    CASE( 2 )
      dlgTime(EDIT_LEVEL_1)%time%end%step%output = dlgTime(EDIT_LEVEL_1)%time%end%step%output/60.
    CASE DEFAULT
  END SELECT
END IF

dlgTime(EDIT_LEVEL_1)%time%end%time%hour = dbreal(3,id_level)
!
!     Edit Boxes - Integers
!
dlgTime(EDIT_LEVEL_1)%time%end%time%year  = dbint(4,id_level)
dlgTime(EDIT_LEVEL_1)%time%end%time%month = dbint(5,id_level)
dlgTime(EDIT_LEVEL_1)%time%end%time%day   = dbint(6,id_level)

IF( .NOT.check_YMD(dlgTime(EDIT_LEVEL_1)%time%end%time) )THEN
  dlgTime(EDIT_LEVEL_1)%time%end%time%year  = NOT_SET_I
  dlgTime(EDIT_LEVEL_1)%time%end%time%month = NOT_SET_I
END IF

IF( dlgTime(EDIT_LEVEL_1)%time%end%time%day <= 0 )THEN
  dlgTime(EDIT_LEVEL_1)%time%end%time%day  = NOT_SET_I
END IF
!
!     Combo Boxes
!
!
!     List Boxes
!
project(BASE_LEVEL)%Edit = .FALSE.

RETURN
END
!*******************************************************************************
!            save Delete Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_delete( iwnd_db )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE errorParam_fd
USE pcscipuf_fi
USE create_fi
USE GUImatl_fi
USE dialog_fi
USE GUItool_fi
USE animate_fi
USE pltchoice_fi
USE SwinAPI
!
!     This routine saves the Delete Dialog Box Parameters
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog handle

INTEGER n,i,indx,irv,ios
LOGICAL lok

CHARACTER(128) eString

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull
LOGICAL,                   EXTERNAL :: hasError

CALL ListCount( iwnd_db,IDB_LIST1,n )

IF( n > 0 )THEN
  DO i = 1,n
    indx = i-1
    CALL GetListItem( iwnd_db,IDB_LIST1,indx,string3,irv )
    IF( irv > 0 )THEN
      string1 = string3(1:irv)
      IF( AnimDel )THEN
        CALL AddPath( string1,path_animate )
      ELSE
        CALL AddPath( string1,project(BASE_LEVEL)%ID%path )
      END IF
      string2 = AddNull( string1 )
      lok = .NOT.(DeleteFile(string2)==FALSE)
      IF( .NOT.lok )THEN
        WRITE(string3,*)GetLastError()
        string2  = 'Error='//TRIM(ADJUSTL(string3))
        CALL ReportFileName( eString,'File=',string1 )
        CALL SetError( API_ERROR,'Unable to delete file',eString,string2,'DeleteFile' )
        CALL ShowErrorMessage( iwnd_db )
      END IF
    END IF
  END DO
END IF

IF( .NOT.AnimDel )THEN
  LastMap = project(BASE_LEVEL)%MapCoord == I_LATLON
  project(   BASE_LEVEL) = project(   DEFAULT_LEVEL)
  dlgDomain( BASE_LEVEL) = dlgDomain( DEFAULT_LEVEL)
  metdef(    BASE_LEVEL) = metdef(    DEFAULT_LEVEL)
  dlgOptions(BASE_LEVEL) = dlgOptions(DEFAULT_LEVEL)
  dlgTime(   BASE_LEVEL) = dlgTime(   DEFAULT_LEVEL)
  CALL CopyMaterial( materials(DEFAULT_LEVEL),materials(BASE_LEVEL) )
  CALL CopyScenario( scenario(DEFAULT_LEVEL),scenario(BASE_LEVEL) )
  nTimePuff = 0
  nTimeSrf  = 0
  IF( ALLOCATED(timePuff) )THEN
    DEALLOCATE( timePuff,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    string2 = 'DEALLOCATE error = '//TRIM(ADJUSTL(string1))
      CALL SetError( UK_ERROR,'Failed to DEALLOCATE puff time array',string2,' ','SCIPtoGUI_ctrl' )
	  END IF
  END IF
  IF( ALLOCATED(timeSrf) )THEN
    DEALLOCATE( timeSrf,STAT=ios )
	  IF( ios /= 0 )THEN
	    WRITE(string1,*)ios
	    string2 = 'DEALLOCATE error = '//TRIM(ADJUSTL(string1))
      CALL SetError( UK_ERROR,'Failed to DEALLOCATE surface time array',string2,' ','SCIPtoGUI_ctrl' )
	  END IF
  END IF
  lplotOK  = .FALSE.
  lprintOK = .FALSE.
  CALL SetControlText( hwnd_tb,IDB_VIEWPRJ,'Project' )
END IF

CALL EnableControlL( hwnd_tb,IDB_PLOT  ,project(BASE_LEVEL)%Plot )
CALL EnableControlL( hwnd_tb,IDB_SCIPUF,project(BASE_LEVEL)%Run )
CALL EnableControlL( hwnd_tb,IDB_VIEWPRJ,project(BASE_LEVEL)%OK )

IF( hasError() )CALL ShowErrorMessage( NULL_POINTER )

RETURN
END
!*******************************************************************************
!            Save NewSetup Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_newsetup( iwnd_db,id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE dialog_fi
USE files_fi
USE GUImatl_fi
USE errorParam_fd
!
!     This routine saves the NewSetup Dialog Box
!
IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db  !Dialog handle
INTEGER,              INTENT( IN ) :: id_level !Data level

INTEGER i,j

LOGICAL, EXTERNAL :: hasError
INTEGER, EXTERNAL :: lastError

!
!     Radio Buttons
!
dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord = ichoice(1,id_level) !Coordinate Type
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_UTM )THEN
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM = dbint(3,id_level)
ELSE
  dlgDomain(EDIT_LEVEL_2)%spatial%domain%zoneUTM = dlgDomain(DEFAULT_LEVEL)%spatial%domain%zoneUTM
END IF

project(EDIT_LEVEL_2)%MapCoord = dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord !Enable/Disable Map drawing

IF( ichoice(2,id_level) == 2 )THEN
  dlgTime(EDIT_LEVEL_2)%time%start%time%reference = HT_LOCAL
ELSE
  dlgTime(EDIT_LEVEL_2)%time%start%time%reference = HT_UTC
END IF

dlgTime(EDIT_LEVEL_2)%time%end%time%reference   = dlgTime(EDIT_LEVEL_2)%time%start%time%reference
!
!     Check Boxes
!
IF( dlgDomain(EDIT_LEVEL_2)%spatial%domain%coord == I_CARTESIAN )THEN
  dlgDomain(EDIT_LEVEL_2)%hasReference = lcheck(1,id_level) !Reference point
ELSE
  dlgDomain(EDIT_LEVEL_2)%hasReference = .FALSE.
END IF
!
!     Edit Boxes - Integers
!
i = dbint(1,id_level)
j = dbint(2,id_level)
IF( i == NOT_SET_I .OR. j == NOT_SET_I )THEN
  dlgTime(EDIT_LEVEL_2)%time%start%zone = NOT_SET_R
ELSE IF( i == DEF_VAL_I .OR. j == DEF_VAL_I )THEN
  dlgTime(EDIT_LEVEL_2)%time%start%zone = DEF_VAL_R
ELSE
  dlgTime(EDIT_LEVEL_2)%time%start%zone = FLOAT(i) + FLOAT(j)/60.
END IF
!
!     Edit Boxes - Reals
!
IF( dlgDomain(EDIT_LEVEL_2)%hasReference )THEN
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon = dbreal( 1,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat = dbreal( 2,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%x   = dbreal(10,id_level)
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%y   = dbreal(11,id_level)
ELSE
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lon = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lon
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%lat = dlgDomain(DEFAULT_LEVEL)%spatial%reference%lat
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%x   = dlgDomain(DEFAULT_LEVEL)%spatial%reference%x
  dlgDomain(EDIT_LEVEL_2)%spatial%reference%y   = dlgDomain(DEFAULT_LEVEL)%spatial%reference%y
END IF
!
!     Combo/List Boxes
!
SELECT CASE( idbcmbo(2,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%Dynamic  = .TRUE.
    project(EDIT_LEVEL_2)%DenseGas = .FALSE.
  CASE( 3 )
    project(EDIT_LEVEL_2)%Dynamic  = .TRUE.
    project(EDIT_LEVEL_2)%DenseGas = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%Dynamic  = .FALSE.
    project(EDIT_LEVEL_2)%DenseGas = .FALSE.
END SELECT

SELECT CASE( idbcmbo(6,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%StaticPuffs = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%StaticPuffs = .FALSE.
END SELECT

project(EDIT_LEVEL_2)%Mode = 0

SELECT CASE( idbcmbo(8,id_level) )
  CASE( 1 )
    project(EDIT_LEVEL_2)%Mode = 0
  CASE( 2 )
    project(EDIT_LEVEL_2)%Mode = IBSET(project(EDIT_LEVEL_2)%Mode,FAST_MODE)
  CASE( 3 )
    project(2)%Mode = IBSET(project(2)%Mode,REVERSE_MODE)
  CASE( 4 )
    project(2)%Mode = IBSET(project(2)%Mode,REVERSE_MODE)
    project(2)%Mode = IBSET(project(2)%Mode,FAST_MODE)
END SELECT

SELECT CASE( idbcmbo(9,id_level) )
  CASE( 2 )
    project(EDIT_LEVEL_2)%SourceNests = .TRUE.
  CASE DEFAULT
    project(EDIT_LEVEL_2)%SourceNests = .FALSE.
END SELECT

RETURN
END
!*******************************************************************************
!            Save Restart Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_restart( id_level )

USE resource_fd
USE reldef_fd
USE tooluser_fd
USE files_fi
USE pcscipuf_fi
USE dialog_fi
USE plotdlg_fi
USE param_fd
!
!     This routine initializes the PLOT Dialog Box
!
IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Data level

IF( idbcmbo(1,id_level) >= 1 .AND. idbcmbo(1,id_level) <= nTimeRestart )THEN
  project(EDIT_LEVEL_2)%RestartTimeIndx = idbcmbo(1,id_level)
ELSE
  project(EDIT_LEVEL_2)%RestartTimeIndx = NOT_SET_I
END IF

RETURN
END
!*******************************************************************************
!            Save Observer Dialog Box
!*******************************************************************************
SUBROUTINE save_dialog_observe( id_level )
!
!     This routine saves the Observer Dialog Box
!
USE pltchoice_fi
USE pcscipuf_fi
USE tooluser_fd

IMPLICIT NONE

INTEGER, INTENT( IN ) :: id_level !Dialog array pointer

REAL, DIMENSION(:), ALLOCATABLE :: ClassDataArray
INTEGER nClassData

INTEGER ios

INTEGER, EXTERNAL :: NumClassDataArray
REAL,    EXTERNAL :: ScaleReal

nClassData = NumClassDataArray( PlotDef(EDIT_LEVEL) )

ALLOCATE( ClassDataArray(MAX(nClassData,1)),STAT=ios )

IF( ios == 0 )THEN

  ClassdataArray(2) = dbreal(1,id_level)
  ClassdataArray(3) = dbreal(2,id_level)
  ClassdataArray(4) = dbreal(3,id_level)

  IF( ichoice(1,id_level) == 2 )THEN
    ClassdataArray(5) = SCIPon
  ELSE
    ClassdataArray(5) = SCIPoff
  END IF

  ClassdataArray(6) = dbreal(4,id_level)
  IF( nCLassData > 6 )ClassdataArray(7) = ScaleReal(dbreal(5,id_level),0.01)

  CALL PutClassDataArray( PlotDef(EDIT_LEVEL), nClassData, ClassDataArray )

  PlotDef(EDIT_LEVEL)%Created = .FALSE.

END IF

IF( ALLOCATED(ClassDataArray) )DEALLOCATE( ClassDataArray,STAT=ios )

RETURN
END
