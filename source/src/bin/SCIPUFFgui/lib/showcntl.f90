!***********************************************************************
!                ShowLocal
!***********************************************************************
SUBROUTINE show_local( iwnd_db,lmap,lref,jopt )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE param_fd
USE SwinAPI

!     This routine shows/hides the local coordinate items

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              lmap    !project coordinate
LOGICAL              lref    !Fixed Reference
INTEGER              jopt    !Plot type

INTEGER enable,showflg

showflg = SW_SHOWNORMAL
enable  = TRUE
IF( (lmap == I_CARTESIAN .AND. lref) .OR. lmap == I_UTM )THEN
  enable  = FALSE
END IF
IF( lmap == I_LATLON .AND. jopt == I_UTM )THEN
  enable  = FALSE
END IF

!---- Hide/Show UTM Coordinate choice

IF( lmap == I_UTM )THEN
 CALL EnableControl(iwnd_db,IDB_RADIO01,TRUE) !LATLON
 CALL EnableControl(iwnd_db,IDB_RADIO02,FALSE) !CARTESIAN
 CALL EnableControl(iwnd_db,IDB_RADIO03,TRUE) !UTM
ELSE IF( lmap == I_CARTESIAN )THEN
 CALL EnableControl(iwnd_db,IDB_RADIO01,TRUE) !LATLON
 CALL EnableControl(iwnd_db,IDB_RADIO02,TRUE) !CARTESIAN
 CALL EnableControl(iwnd_db,IDB_RADIO03,FALSE) !UTM
ELSE
 CALL EnableControl(iwnd_db,IDB_RADIO01,TRUE) !LATLON
 CALL EnableControl(iwnd_db,IDB_RADIO02,TRUE) !CARTESIAN
 CALL EnableControl(iwnd_db,IDB_RADIO03,TRUE) !UTM
END IF

!---- Hide/Show LOCAL Coordinate REAL EDIT boxes

CALL EnableControl(iwnd_db,IDB_REAL1   ,enable) !X Origin
CALL ShowControl  (iwnd_db,IDB_REAL1   ,showflg)
CALL EnableControl(iwnd_db,IDB_REAL2   ,enable) !Y Origin
CALL ShowControl  (iwnd_db,IDB_REAL2   ,showflg)
CALL EnableControl(iwnd_db,IDB_REAL9   ,enable) !X Origin
CALL ShowControl  (iwnd_db,IDB_REAL9   ,showflg)
CALL EnableControl(iwnd_db,IDB_REAL10  ,enable) !Y Origin
CALL ShowControl  (iwnd_db,IDB_REAL10  ,showflg)
CALL ShowControl  (iwnd_db,IDB_STATIC12,showflg) !Labels
CALL ShowControl  (iwnd_db,IDB_STATIC13,showflg) !Labels
CALL ShowControl  (iwnd_db,IDB_STATIC14,showflg) !Labels

RETURN
END
!***********************************************************************
!                ResetLocal
!***********************************************************************
SUBROUTINE reset_local( iwnd_db,iopt )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd
USE pltchoice_fi

!     This routine resets axes to default values

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              iopt    !Option flag

INTEGER i,id_level,jd_level
INTEGER(POINTER_LEN) jwnd

REAL      atmp(4)
LOGICAL   lopt,LatLon

IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
    PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT   .OR. &
    PlotDef(EDIT_LEVEL)%Field%Category == HP_TABLE )RETURN

!---- Find AXES,PLOT Dialog ID

CALL FindHwndListId(IDB_AXES,jwnd,id_level)
CALL FindHwndListId(IDB_PLOT,jwnd,jd_level)

!---- Reset Axes

IF( lcheck(8,jd_level) )THEN !Vertical slices
  DO i = 1,6
    dbreal(3+i-1,id_level) = axesdef(EDIT_LEVEL)%dbreal(10+i)
  END DO
ELSE
  atmp(1) = dbreal(2,id_level)
  atmp(2) = dbreal(1,id_level)
  atmp(3) = dbreal(9,id_level)
  atmp(4) = dbreal(10,id_level)
!        lopt = iopt .eq. 1
  lopt = iopt /= I_LATLON
  LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
  CALL axes_transform(axesdef(EDIT_LEVEL)%dbreal(5),atmp, &
                dbreal(3,id_level),LatLon, &
                lopt,.TRUE.,.TRUE.)
END IF
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

!---- Display values

CALL SetEditRs( iwnd_db,dbreal(3,id_level),3,6 )

!---- Set labels

IF( iopt == I_CARTESIAN )THEN
  string1 = 'X (km)'
  string2 = 'Y (km)'
ELSE IF( iopt == I_UTM )THEN
  string1 = 'E''ing'
  string2 = 'N''ing'
ELSE
  string1 = 'Lon.'
  string2 = 'Lat.'
END IF
CALL SetControlText(iwnd_db,IDB_STATIC22,string1)
CALL SetControlText(iwnd_db,IDB_STATIC23,string2)

CALL show_local( iwnd_db,project(BASE_LEVEL)%MapCoord,project(BASE_LEVEL)%Ref,iopt )

RETURN
END
!***********************************************************************
!                ShowContours
!***********************************************************************
SUBROUTINE show_contours( iwnd_db,iopt )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pltchoice_fi
USE SwinAPI
USE GUIparam_fd

!     This routine shows/hides the contour items

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              iopt    !Contour type

INTEGER nenable,uenable,senable,nshow,ushow,sshow,i
INTEGER xenable,xshow,tsenable,tsshow

!---- Set Enable/Disable Flags based on contour type

SELECT CASE( iopt )
  CASE( 2 ) !Nonuniform
    nenable = TRUE
    xenable = TRUE
    uenable = FALSE
    senable = FALSE
    tsenable = FALSE
    nshow   = SW_SHOWNORMAL
    xshow   = SW_SHOWNORMAL
    ushow   = SW_HIDE
    sshow   = SW_HIDE
    tsshow   = SW_HIDE
  CASE DEFAULT !Uniform
    uenable = TRUE
    xenable = TRUE
    nenable = FALSE
    senable = FALSE
    tsenable = FALSE
    xshow   = SW_SHOWNORMAL
    ushow   = SW_SHOWNORMAL
    nshow   = SW_HIDE
    sshow   = SW_HIDE
    tsshow   = SW_HIDE
END SELECT

!---- Show/Hide Uniform contour controls

CALL EnableControl(iwnd_db,IDB_INT1    ,uenable) !Approx. number of contours
CALL ShowControl  (iwnd_db,IDB_INT1    ,ushow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC25,ushow  ) !Label

CALL EnableControl(iwnd_db,IDB_REAL1   ,uenable) !Minimum contours
CALL ShowControl  (iwnd_db,IDB_REAL1   ,ushow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC22,ushow  ) !Label

CALL EnableControl(iwnd_db,IDB_REAL2   ,uenable) !Maximum contours
CALL ShowControl  (iwnd_db,IDB_REAL2   ,ushow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC23,ushow  ) !Label

CALL EnableControl(iwnd_db,IDB_REAL3   ,uenable) !Contour Increment
CALL ShowControl  (iwnd_db,IDB_REAL3   ,ushow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC24,ushow  ) !Label

CALL EnableControl(iwnd_db,IDB_RADIO11 ,uenable) !Linear Spacing
CALL ShowControl  (iwnd_db,IDB_RADIO11 ,ushow  ) !

CALL EnableControl(iwnd_db,IDB_RADIO12 ,uenable) !Log Spacing
CALL ShowControl  (iwnd_db,IDB_RADIO12 ,ushow  ) !

!---- Show/Hide Nonuniform contour controls

CALL update_contour_buttons(iwnd_db,nenable)

CALL ShowControl  (iwnd_db,IDB_STATIC06,nshow  ) !Label

CALL EnableControl(iwnd_db,IDB_LIST1   ,nenable) !LIST Box
CALL ShowControl  (iwnd_db,IDB_LIST1   ,nshow  ) !
DO i = 4,10
  CALL ShowControl(iwnd_db,BUTTON_BASE+i ,nshow  ) !
END DO

!---- Show/Hide SCIP contour controls

CALL EnableControl(iwnd_db,IDB_RADIO21 ,senable) !Numeric labels
CALL ShowControl  (iwnd_db,IDB_RADIO21 ,sshow  ) !

CALL EnableControl(iwnd_db,IDB_RADIO22 ,tsenable) !Text labels
CALL ShowControl  (iwnd_db,IDB_RADIO22 ,tsshow  ) !

!---- Show/Hide Scale/Units controls

CALL EnableControl(iwnd_db,IDB_REAL4,xenable) !Scale input
CALL ShowControl  (iwnd_db,IDB_REAL4,xshow  )
CALL EnableControl(iwnd_db,IDB_EDIT1,xenable) !Units input
CALL ShowControl  (iwnd_db,IDB_EDIT1,xshow  )

CALL ShowControl  (iwnd_db,IDB_STATIC31,xshow) !Scale Box
CALL ShowControl  (iwnd_db,IDB_STATIC32,xshow) !Scale label
CALL ShowControl  (iwnd_db,IDB_STATIC33,xshow) !Units label

RETURN
END
!***********************************************************************
!                ShowMaps
!***********************************************************************
SUBROUTINE show_maps( iwnd_db,iopt,jopt )

USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE SwinAPI

!     This routine shows/hides the map items

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle
INTEGER              iopt    !MAP type
INTEGER              jopt    !Pop type

INTEGER lenable,henable,lshow,hshow,isChecked,irv

!---- Set Enable/Disable Flags based on Map type

SELECT CASE( iopt )
  CASE( 3 ) !None
    lenable = FALSE
    henable = FALSE
    lshow   = SW_HIDE
    hshow   = SW_HIDE
  CASE( 2 ) !LoRes
    lenable = TRUE
    henable = FALSE
    lshow   = SW_SHOWNORMAL
    hshow   = SW_HIDE
  CASE DEFAULT !HiRes
    lenable = TRUE
    henable = TRUE
    lshow   = SW_SHOWNORMAL
    hshow   = SW_SHOWNORMAL
END SELECT

!---- Show/Hide LoRes CHECK Boxes : Populated Places : Symbols, Text

CALL EnableControl(iwnd_db,IDB_CHECK1  ,lenable) !PP Symbols
CALL ShowControl  (iwnd_db,IDB_CHECK1  ,lshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK2  ,lenable) !PP Text
CALL ShowControl  (iwnd_db,IDB_CHECK2  ,lshow  ) !

!---- Show/Hide HiRes CHECK Boxes : Rest of them

CALL EnableControl(iwnd_db,IDB_CHECK3  ,henable) !PP Areas
CALL ShowControl  (iwnd_db,IDB_CHECK3  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK4  ,henable) !Roads
CALL ShowControl  (iwnd_db,IDB_CHECK4  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK5  ,henable) !Railroads
CALL ShowControl  (iwnd_db,IDB_CHECK5  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK6  ,henable) !Nuclear Facilities
CALL ShowControl  (iwnd_db,IDB_CHECK6  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK7  ,henable) !Airports
CALL ShowControl  (iwnd_db,IDB_CHECK7  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK8  ,henable) !Nuc Fac Text
CALL ShowControl  (iwnd_db,IDB_CHECK8  ,hshow  ) !

CALL EnableControl(iwnd_db,IDB_CHECK9  ,henable) !Airport Text
CALL ShowControl  (iwnd_db,IDB_CHECK9  ,hshow  ) !

!---- Show/Hide PP Text RADIO Buttons

CALL ShowControl  (iwnd_db,IDB_RADIO11 ,hshow  ) !Full List
CALL ShowControl  (iwnd_db,IDB_RADIO12 ,hshow  ) !Partial List
IF( henable )THEN
  isChecked = IsDlgButtonChecked(iwnd_db,IDB_CHECK2) !Is PP Text Checked
ELSE
  isChecked = FALSE
END IF
CALL EnableControl(iwnd_db,IDB_RADIO11,isChecked) !Full List
CALL EnableControl(iwnd_db,IDB_RADIO12,isChecked) !Partial List

!---- Population density

IF( .NOT.PopData )THEN
  IF( jopt == 1 )THEN
    jopt = 2
    irv = CheckRadioButton(iwnd_db,IDB_RADIO21,IDB_RADIO22,IDB_RADIO22)
  END IF
  CALL EnableControl(iwnd_db,IDB_RADIO21,FALSE)
END IF

isChecked = IsDlgButtonChecked(iwnd_db,IDB_CHECK10) !Is Population checked

SELECT CASE( jopt )
  CASE( 1 ) !LoRes
    henable = FALSE
  CASE DEFAULT !HiRes
    henable = isChecked
END SELECT

IF( henable == FALSE )THEN
  hshow = SW_HIDE
ELSE
  hshow = SW_SHOWNORMAL
END IF

CALL ShowControl  (iwnd_db,IDB_REAL1    ,hshow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC32 ,hshow  ) !
CALL ShowControl  (iwnd_db,IDB_STATIC33 ,hshow  ) !
CALL EnableControl(iwnd_db,IDB_REAL1    ,henable)

RETURN
END
!***********************************************************************
!                ShowCheckMarks
!***********************************************************************
SUBROUTINE ShowCheckMarks( iwnd_db )
USE resource_fd
USE defineok_fd
USE reldef_fd
USE tooluser_fd
USE pcscipuf_fi
USE create_fi
USE GUItool_fi
USE GUImatl_fi
USE files_fi
USE mettype_fd
USE SwinAPI

!     This routine shows/hides the MEAN/PROB items

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog handle

INTEGER adflagr

IF( materials(EDIT_LEVEL_1)%nmatl > 0 )THEN
  adflagr = TRUE
ELSE
  adflagr = FALSE
END IF
CALL EnableControl(iwnd_db,IDB_BUTTON5 ,adflagr) !Release

!---- Set Checkmarks

IF( BTEST(DefinedOK,DF_RELEASE) )THEN
  CALL set_checkmark(iwnd_db,DF_RELEASE)
ELSE
  CALL hide_checkmark(iwnd_db,DF_RELEASE)
END IF

IF( BTEST(DefinedOK,DF_MATERIAL) )THEN
  CALL set_checkmark(iwnd_db,DF_MATERIAL)
ELSE
  CALL hide_checkmark(iwnd_db,DF_MATERIAL)
END IF

IF( BTEST(DefinedOK,DF_TIME) )THEN
  CALL set_checkmark(iwnd_db,DF_TIME)
ELSE
  CALL hide_checkmark(iwnd_db,DF_TIME)
END IF

IF( BTEST(DefinedOK,DF_DOMAIN) )THEN
  CALL set_checkmark(iwnd_db,DF_DOMAIN)
ELSE
  CALL hide_checkmark(iwnd_db,DF_DOMAIN)
END IF

IF( BTEST(DefinedOK,DF_MET) )THEN
  CALL set_checkmark(iwnd_db,DF_MET)
ELSE
  CALL hide_checkmark(iwnd_db,DF_MET)
END IF

RETURN
END
