!*******************************************************************************
!*******************************************************************************
!             LoadPlotCombo
!*******************************************************************************
!*******************************************************************************
SUBROUTINE LoadPlotCombo( curWindow, curLevel, comboID )

USE SwinAPI
USE pltchoice_fi
USE dialog_fi
USE pcscipuf_fi
USE plotdlg_fi
USE param_fd

IMPLICIT NONE

INTEGER curLevel, comboID
INTEGER(POINTER_LEN)curWindow
INTEGER first, indx, irv, current
INTEGER i, j
INTEGER NearestPlotTimeGUI, nPltType
LOGICAL exitTime, exitKind, InRange
REAL    CurrentTime

!==============================================================================
! Initialize exit flags
!==============================================================================

exitTime = comboID > CLASS_COMBO_ID .AND. comboID <= TIME_COMBO_ID
exitKind = comboID == KIND_COMBO_ID     !.OR. comboID == CHOICE_COMBO_ID

!==============================================================================
! CLASS combo
!==============================================================================

IF( comboID < CLASS_COMBO_ID )THEN
  CALL EnableControl( curWindow,CLASS_COMBO,TRUE )
  CALL   ShowControl( curWindow,CLASS_COMBO,SW_SHOWNORMAL )
  CALL     ClearList( curWindow,CLASS_COMBO )
  DO i = 1,nPltClass
	  CALL     AddList( curWindow,CLASS_COMBO,-1,ClassStr(i)%string,irv )
    CALL SetListData( curWindow,CLASS_COMBO,i-1,i,irv )
  END DO
  CALL SetListSelString( curWindow,CLASS_COMBO,ClassStr(PlotDef(EDIT_LEVEL)%Field%Class)%string,irv )
ELSE IF( comboID == CLASS_COMBO_ID )THEN
  CALL GetListSel( curWindow,CLASS_COMBO,1,indx,irv )
  CALL GetListData( curWindow,CLASS_COMBO,indx,current,irv )
  PlotDef(EDIT_LEVEL)%Field%Class = current
END IF

!==============================================================================
! CHOICE combo
!==============================================================================

IF( comboID < CHOICE_COMBO_ID )THEN
  CALL EnableControl( curWindow,CHOICE_COMBO,TRUE )
  CALL   ShowControl( curWindow,CHOICE_COMBO,SW_SHOWNORMAL )
  CALL     ClearList( curWindow,CHOICE_COMBO )
  indx = 0
  DO j = 1,nPltChoice
    IF( BTEST(ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,j)%available,HPB_AVAILABLE) )THEN
      CALL     AddList( curWindow,CHOICE_COMBO,-1,ChoiceStr(j)%string,irv )
      CALL SetListData( curWindow,CHOICE_COMBO,indx,j,irv )
      IF( indx == 0 )first = j
	    indx = indx + 1
    END IF
  END DO
  IF( BTEST(ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%available,HPB_AVAILABLE) )THEN
    CALL SetListSelString( curWindow,CHOICE_COMBO,ChoiceStr(PlotDef(EDIT_LEVEL)%Field%Choice)%string,irv )
  ELSE
    CALL SetListSelString( curWindow,CHOICE_COMBO,ChoiceStr(first)%string,irv )
	  PlotDef(EDIT_LEVEL)%Field%Choice = first
  END IF
ELSE IF( comboID == CHOICE_COMBO_ID )THEN
  CALL  GetListSel( curWindow,CHOICE_COMBO,1,indx,irv )
  CALL GetListData( curWindow,CHOICE_COMBO,indx,current,irv )
  PlotDef(EDIT_LEVEL)%Field%Choice = current
END IF

!==============================================================================
! KIND combo
!==============================================================================

IF( comboID < KIND_COMBO_ID )THEN
  CALL EnableControl( curWindow,KIND_COMBO,TRUE )
  CALL   ShowControl( curWindow,KIND_COMBO,SW_SHOWNORMAL )
  CALL     ClearList( curWindow,KIND_COMBO )
  IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%kind == SCIPtrue )THEN
    indx = 0
    DO i = 1,ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%nkind
	    j = ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%ikind + i - 1
      CALL     AddList( curWindow,KIND_COMBO,-1,KindStr(j)%string,irv )
      CALL SetListData( curWindow,KIND_COMBO,indx,j,irv )
	    indx = indx + 1
    END DO
	  i = ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%ikind
	  j = i + ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%nkind - 1
    IF( InRange(i,j,PlotDef(EDIT_LEVEL)%Field%Kind) )THEN
      CALL SetListSelString( curWindow,KIND_COMBO,KindStr(PlotDef(EDIT_LEVEL)%Field%Kind)%string,irv )
    ELSE
	    first = ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%ikind
      CALL SetListSelString( curWindow,KIND_COMBO,KindStr(first)%string,irv )
	    PlotDef(EDIT_LEVEL)%Field%Kind = first
	  END IF
  ELSE
    CALL EnableControl( curWindow,KIND_COMBO,FALSE )
    CALL   ShowControl( curWindow,KIND_COMBO,SW_HIDE )
  END IF
ELSE IF( comboID == KIND_COMBO_ID )THEN
  CALL  GetListSel( curWindow,KIND_COMBO,1,indx,irv )
  CALL GetListData( curWindow,KIND_COMBO,indx,current,irv )
  PlotDef(EDIT_LEVEL)%Field%Kind = current
END IF

!==============================================================================
! if just CHOICE or KIND combo selection still must reset TYPE because of Hazard Area
!     so just skip Time Combo
!==============================================================================

IF(.NOT. exitKind)THEN

!==============================================================================
! TIME combo
!==============================================================================

  IF( comboID < TIME_COMBO_ID )THEN
    IF( PlotDef(EDIT_LEVEL)%Field%TimeID == NOT_SET_I )THEN
      currentTime = NOT_SET_R
    ELSE IF( .NOT.InRange(1,nTimePlot,PlotDef(EDIT_LEVEL)%Field%TimeID) )THEN
      currentTime = DEF_VAL_R
    ELSE
      currentTime = timePlot(PlotDef(EDIT_LEVEL)%Field%TimeID)%time%runTime
    END IF
    CALL EnableControl( curWindow,TIME_COMBO,TRUE )
    CALL   ShowControl( curWindow,TIME_COMBO,SW_SHOWNORMAL )
    CALL     ClearList( curWindow,TIME_COMBO )
    i = PlotDef(EDIT_LEVEL)%Field%Class
    j = PlotDef(EDIT_LEVEL)%Field%Choice
    CALL LoadPlotTimes( ClassChoiceArray(i,j)%itime,ClassChoiceArray(i,j)%usertime )
    IF( nTimePlot > 0 )THEN
      CALL ShowControl( curWindow,TIME_COMBO_LABEL,SW_SHOWNORMAL )
  	  indx = 0
  	  DO i = 1,nTimePlot
        CALL     AddList( curWindow,TIME_COMBO,-1,timePlot(i)%string,irv )
        CALL SetListData( curWindow,TIME_COMBO,indx,i,irv )
  	    indx = indx + 1
      END DO
  	  PlotDef(EDIT_LEVEL)%Field%timeID = NearestPlotTimeGUI(timePlot,nTimePlot,currentTime)
      CALL SetListSelString( curWindow,TIME_COMBO,timePlot(PlotDef(EDIT_LEVEL)%Field%timeID)%string,irv )
      IF( PlotDef(EDIT_LEVEL)%Field%TimeID == nTimePlot )THEN
        IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%usertime == SCIPtrue )THEN
          CALL EnableControl( curWindow,USER_TIME_EDIT ,TRUE )
          CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_SHOWNORMAL )
          CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_SHOWNORMAL )
  	    ELSE
          CALL EnableControl( curWindow,USER_TIME_EDIT ,FALSE )
          CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_HIDE )
          CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_HIDE )
  	    END IF
      ELSE
        CALL EnableControl( curWindow,USER_TIME_EDIT ,FALSE )
        CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_HIDE )
        CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_HIDE )
      END IF
    ELSE
      CALL EnableControl( curWindow,TIME_COMBO,FALSE )
      CALL   ShowControl( curWindow,TIME_COMBO,SW_HIDE )
      CALL   ShowControl( curWindow,TIME_COMBO_LABEL,SW_HIDE )
      CALL EnableControl( curWindow,USER_TIME_EDIT ,FALSE )
      CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_HIDE )
      CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_HIDE )
    END IF
  ELSE IF( comboID == TIME_COMBO_ID )THEN
    CALL GetListSel( curWindow,TIME_COMBO,1,indx,irv )
    CALL GetListData( curWindow,TIME_COMBO,indx,current,irv )
    PlotDef(EDIT_LEVEL)%Field%TimeID = current
    IF( PlotDef(EDIT_LEVEL)%Field%TimeID == nTimePlot )THEN
      IF( ClassChoiceArray(PlotDef(EDIT_LEVEL)%Field%Class,PlotDef(EDIT_LEVEL)%Field%Choice)%usertime == SCIPtrue )THEN
        CALL EnableControl( curWindow,USER_TIME_EDIT ,TRUE )
        CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_SHOWNORMAL )
        CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_SHOWNORMAL )
  	  ELSE
        CALL EnableControl( curWindow,USER_TIME_EDIT ,FALSE )
        CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_HIDE )
        CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_HIDE )
  	  END IF
    ELSE
      CALL EnableControl( curWindow,USER_TIME_EDIT ,FALSE )
      CALL   ShowControl( curWindow,USER_TIME_EDIT ,SW_HIDE )
      CALL   ShowControl( curWindow,USER_TIME_LABEL,SW_HIDE )
    END IF
  END IF

END IF

!==============================================================================
! Skip to TYPE and EXCEED combo unless full load
!==============================================================================

IF( exitTime )GOTO 9998

!==============================================================================
! CATEGORY combo
!==============================================================================

IF( comboID < CATEGORY_COMBO_ID )THEN
  CALL EnableControl( curWindow,CATEGORY_COMBO,TRUE )
  CALL   ShowControl( curWindow,CATEGORY_COMBO,SW_SHOWNORMAL )
  CALL     ClearList( curWindow,CATEGORY_COMBO )
  indx = 0
  DO i = 1,HP_NUMCAT
    IF( CatClassArray(i,PlotDef(EDIT_LEVEL)%Field%Class)%available == SCIPtrue )THEN
      CALL     AddList( curWindow,CATEGORY_COMBO,-1,CATEGORY_STRING(i),irv )
      CALL SetListData( curWindow,CATEGORY_COMBO,indx,i,irv )
      IF( indx == 0 )first = i
	    indx = indx + 1
    END IF
  END DO
  IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%available == SCIPtrue )THEN
    CALL SetListSelString( curWindow,CATEGORY_COMBO,CATEGORY_STRING(PlotDef(EDIT_LEVEL)%Field%Category),irv )
  ELSE
    CALL SetListSelString( curWindow,CATEGORY_COMBO,CATEGORY_STRING(first),irv )
	  PlotDef(EDIT_LEVEL)%Field%Category = first
  END IF
ELSE IF( comboID == CATEGORY_COMBO_ID )THEN
  CALL  GetListSel( curWindow,CATEGORY_COMBO,1,indx,irv )
  CALL GetListData( curWindow,CATEGORY_COMBO,indx,current,irv )
  PlotDef(EDIT_LEVEL)%Field%Category = current
END IF
SELECT CASE( PlotDef(EDIT_LEVEL)%Field%Category )
  CASE( HP_TABLE )
    CALL EnableControl( curWindow,CONTOURBUTTON,FALSE )
    CALL EnableControl( curWindow,MAPSBUTTON,   FALSE )
    CALL EnableControl( curWindow,OPTIONBUTTON, FALSE )
    CALL EnableControl( curWindow,AXESBUTTON,   FALSE )
  CASE DEFAULT
    CALL EnableControl( curWindow,CONTOURBUTTON,TRUE )
    CALL EnableControl( curWindow,MAPSBUTTON,   TRUE )
    CALL EnableControl( curWindow,OPTIONBUTTON, TRUE )
    CALL EnableControl( curWindow,AXESBUTTON,   TRUE )
END SELECT

!==============================================================================
! TYPE combo
!==============================================================================

9998 CONTINUE

IF( comboID < TYPE_COMBO_ID )THEN
  CALL EnableControl( curWindow,TYPE_COMBO,TRUE )
  CALL   ShowControl( curWindow,TYPE_COMBO,SW_SHOWNORMAL )
  CALL     ClearList( curWindow,TYPE_COMBO )
  IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%type == SCIPtrue )THEN
    nPltType = HP_NUMTYP
    indx     = 0
    DO i = 1,nPltType
      IF( i <= HP_NUMTYP )THEN
        CALL AddList( curWindow,TYPE_COMBO,-1,TYPE_STRING(i),irv )
      ELSE
        CALL AddList( curWindow,TYPE_COMBO,-1,'Hazard Area',irv )
      END IF
      CALL SetListData( curWindow,TYPE_COMBO,indx,i,irv )
	    indx = indx + 1
    END DO
    IF( InRange(1,nPltType,PlotDef(EDIT_LEVEL)%Type) )THEN
      IF( PlotDef(EDIT_LEVEL)%Type <= HP_NUMTYP )THEN
        CALL SetListSelString( curWindow,TYPE_COMBO,TYPE_STRING(PlotDef(EDIT_LEVEL)%Type),irv )
      ELSE
        CALL SetListSelString( curWindow,TYPE_COMBO,'Hazard Area',irv )
      END IF
    ELSE
	    first = 1
      CALL SetListSelString( curWindow,TYPE_COMBO,TYPE_STRING(first),irv )
	    PlotDef(EDIT_LEVEL)%Type = first
	  END IF
    SELECT CASE( PlotDef(EDIT_LEVEL)%Type )
      CASE( HP_NUMTYP+1 )
        CALL EnableControl( curWindow,CONTOURBUTTON,FALSE )
      CASE DEFAULT
        CALL EnableControl( curWindow,CONTOURBUTTON,TRUE )
    END SELECT
  ELSE
    PlotDef(EDIT_LEVEL)%Type = HP_MEAN
    CALL EnableControl( curWindow,TYPE_COMBO,FALSE )
    CALL   ShowControl( curWindow,TYPE_COMBO,SW_HIDE )
  END IF
ELSE IF( comboID == TYPE_COMBO_ID )THEN
  CALL  GetListSel( curWindow,TYPE_COMBO,1,indx,irv )
  CALL GetListData( curWindow,TYPE_COMBO,indx,current,irv )
  PlotDef(EDIT_LEVEL)%Type = current
  SELECT CASE( PlotDef(EDIT_LEVEL)%Type )
    CASE( HP_NUMTYP+1 )
      CALL EnableControl( curWindow,CONTOURBUTTON,FALSE )
    CASE DEFAULT
      CALL EnableControl( curWindow,CONTOURBUTTON,TRUE )
  END SELECT
END IF

!==============================================================================
! Reset contours
!==============================================================================

! 9998 CONTINUE

PlotDef(EDIT_LEVEL)%ContourIndex = USER_CONTOUR
!==============================================================================
! Show/Hide SLICE controls
!==============================================================================

CALL   ShowControl( curWindow,HSLICE_LABEL ,SW_HIDE )
CALL   ShowControl( curWindow,HSLICE_EDIT  ,SW_HIDE )
CALL EnableControl( curWindow,HSLICE_EDIT  ,FALSE )
CALL   ShowControl( curWindow,VSLICE_LABEL ,SW_HIDE )
CALL   ShowControl( curWindow,VSLICE_BUTTON,SW_HIDE )
CALL EnableControl( curWindow,VSLICE_BUTTON,FALSE )

SELECT CASE( PlotDef(EDIT_LEVEL)%Field%Category )
  CASE( HP_HSLICE )
    CALL   ShowControl( curWindow,HSLICE_LABEL,SW_SHOWNORMAL )
    CALL   ShowControl( curWindow,HSLICE_EDIT ,SW_SHOWNORMAL )
    CALL EnableControl( curWindow,HSLICE_EDIT ,TRUE )
  CASE( HP_VSLICE,HP_HINT )
    CALL    ShowControl( curWindow,VSLICE_LABEL ,SW_SHOWNORMAL )
    CALL    ShowControl( curWindow,VSLICE_BUTTON,SW_SHOWNORMAL )
    CALL  EnableControl( curWindow,VSLICE_BUTTON,TRUE )
    CALL SetControlText( curWindow,VSLICE_LABEL,'Slice' )
    CALL MoveControl( curWindow,VSLICE_LABEL,VSLICE_BUTTON,HSLICE_LABEL,HSLICE_EDIT )
  CASE DEFAULT
END SELECT

!==============================================================================
! Show/Hide EXCEED/PROBABILITY/RISK controls
!==============================================================================

CALL EnableControl( curWindow,IDB_COMBO7,FALSE )
CALL   ShowControl( curWindow,IDB_COMBO7,SW_HIDE )
CALL EnableControl( curWindow,PROB_EDIT ,FALSE )
CALL   ShowControl( curWindow,PROB_EDIT ,SW_HIDE )
CALL EnableControl( curWindow,EXCD_EDIT ,FALSE )
CALL   ShowControl( curWindow,EXCD_EDIT ,SW_HIDE )
CALL EnableControl( curWindow,RISK_EDIT ,FALSE )
CALL   ShowControl( curWindow,RISK_EDIT ,SW_HIDE )
CALL   ShowControl( curWindow,TYPE_LABEL,SW_HIDE )

IF( CatClassArray(PlotDef(EDIT_LEVEL)%Field%Category,PlotDef(EDIT_LEVEL)%Field%Class)%type == SCIPtrue )THEN

  SELECT CASE( PlotDef(EDIT_LEVEL)%Type )
    CASE( HP_EXCEED )
      CALL SetControlText( curWindow,TYPE_LABEL,'Probability' )
      CALL  EnableControl( curWindow,PROB_EDIT ,TRUE )
      CALL    ShowControl( curWindow,PROB_EDIT ,SW_SHOWNORMAL )
      CALL    ShowControl( curWindow,TYPE_LABEL,SW_SHOWNORMAL )
    CASE( HP_PROB )
      CALL SetControlText( curWindow,TYPE_LABEL,'Exceed' )
      CALL  EnableControl( curWindow,EXCD_EDIT ,TRUE)
      CALL    ShowControl( curWindow,EXCD_EDIT ,SW_SHOWNORMAL )
      CALL    ShowControl( curWindow,TYPE_LABEL,SW_SHOWNORMAL )
    CASE DEFAULT
  END SELECT
END IF

!==============================================================================
! EXIT
!==============================================================================

9999 CONTINUE

RETURN
END
!*******************************************************************************
!*******************************************************************************
!             LoadPlotTimes
!*******************************************************************************
!*******************************************************************************
SUBROUTINE LoadPlotTimes( iTime,iUser )

USE pltchoice_fi
USE errorParam_fd
USE default_fd
USE dialog_fi
use pcscipuf_fi, ONLY: NULL_POINTER

IMPLICIT NONE

INTEGER iTime, iUser
INTEGER i, ios, jos
LOGICAL lUser
REAL    TimeMax

CHARACTER(24) string1,string2

INTEGER, EXTERNAL :: FXGuiSetMaxTime

nTimePlot = 0

lUser = iUser == SCIPtrue

IF( ALLOCATED(timePlot) )THEN
  DEALLOCATE( timePlot,STAT=jos )
ELSE
  jos = 0
END IF

InputTimeMax = DEF_VAL_R
SELECT CASE( iTime )
  CASE( HP_SRFTIME )
    nTimePlot = nTimeSrf
  CASE( HP_PUFFTIME )
    nTimePlot = nTimePuff
  CASE( HP_METTIME )
    nTimePlot = nTimeMet
  CASE DEFAULT
    nTimePlot = 0
END SELECT

IF( nTimePlot > 0 .AND. lUser )nTimeplot = nTimePlot + 1

IF( nTimePlot > 0 )THEN

  ALLOCATE( timePlot(nTimePlot) ,STAT=ios )
  IF( ios /= 0 .OR. jos /= 0 )THEN
    nTimePlot = 0
    WRITE(string1,*)ios
    WRITE(string2,*)jos
    CALl SetError( UK_ERROR,'Error allocating memory for plot times', &
                   'ALLOCATE status = '//TRIM(ADJUSTL(string1))//':'//TRIM(ADJUSTL(string2)), &
                   ' ','AllocatePlotTime' )
    CALL ShowErrorMessage( NULL_POINTER )
  ELSE
    SELECT CASE( iTime )
      CASE( HP_SRFTIME )
        DO i = 1,nTimeSrf
          timePlot(i) = timeSrf(i)
        END DO
        TimeMax = timePlot(nTimeSrf)%time%runTime
      CASE( HP_PUFFTIME )
        DO i = 1,nTimePuff
          timePlot(i) = timePuff(i)
        END DO
        TimeMax = timePlot(nTimePuff)%time%runTime
      CASE( HP_METTIME )
        DO i = 1,nTimeMet
          timePlot(i) = timeMet(i)
        END DO
        TimeMax = timePlot(nTimeMet)%time%runTime
      CASE DEFAULT
        TimeMax = 0.0
    END SELECT
	  IF( lUser )THEN
	    IF( nTimePlot > 1 )THEN
	      timePlot(nTimePlot) = timePlot(nTimePlot-1)
	    END IF
	    timePlot(nTimePlot)%time%runTime = DEF_VAL_R
	    timePlot(nTimePlot)%string = 'User Input'
	  END IF
  END IF
ELSE
  TimeMax = 0.0
END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!             NearestPlotTime
!*******************************************************************************
!*******************************************************************************
INTEGER FUNCTION NearestPlotTimeGUI( tList,nList,time )

USE plotlist_fd
USE default_fd

IMPLICIT NONE

TYPE(SCIPTimeT) tList(*)
INTEGER        nList
REAL           time

REAL    dt, dtx
INTEGER i

NearestPlotTimeGUI = NOT_SET_I

IF( time == DEF_VAL_R )THEN

  NearestPlotTimeGUI = nList

ELSE IF( time == NOT_SET_R )THEN

  NearestPlotTimeGUI = nList

ELSE

  IF( time <= tList(1)%time%runTime )THEN
    NearestPlotTimeGUI = 1
  ELSE IF( time >= tList(nList)%time%runTime )THEN
    NearestPlotTimeGUI = nList
  ELSE
    dtx = DEF_VAL_R
    DO i = 1,nList
      dt = ABS(time - tList(i)%time%runTime)
      IF( dt < dtx )THEN
        dtx = dt
        NearestPlotTimeGUI = i
      END IF
    END DO
  END IF

END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!             NearestPlotExceed
!*******************************************************************************
!*******************************************************************************
INTEGER FUNCTION NearestPlotExceed( eList,nList,exceed )

USE default_fd
USE pltchoice_fd

IMPLICIT NONE

TYPE(SCIPContourElementT), DIMENSION(*) :: eList
INTEGER        nList
REAL           exceed

REAL    de, dex
INTEGER i

NearestPlotExceed = NOT_SET_I

IF( exceed == DEF_VAL_R )THEN

  NearestPlotExceed = 1

ELSE IF( exceed == NOT_SET_R )THEN

  NearestPlotExceed = 1

ELSE

  IF( exceed <= eList(1)%Contour )THEN
    NearestPlotExceed = 1
  ELSE IF( exceed >= eList(nList)%Contour )THEN
    NearestPlotExceed = nList
  ELSE
    dex = DEF_VAL_R
    NearestPlotExceed = 1
    DO i = 1,nList
      IF( LEN_TRIM(eList(i)%LAbel) > 0 )THEN
        de = ABS(exceed - eList(i)%Contour)
        IF( de < dex )THEN
          dex = de
          NearestPlotExceed = i
        END IF
	  END IF
    END DO
  END IF

END IF

RETURN
END
!*******************************************************************************
!*******************************************************************************
!             MoveControl
!*******************************************************************************
!*******************************************************************************
SUBROUTINE MoveControl( curWindow,labelID,controlID,newLableID,newControlID )

USE SwinAPI

IMPLICIT NONE

INTEGER labelID, controlID, newLableID, newControlID
INTEGER(POINTER_LEN)curWindow

INTEGER irv
INTEGER(POINTER_LEN) ictrl

TYPE ( T_POINT ) Pnt
TYPE ( T_RECT  ) BoxN, BoxO

IF( labelID /= 0 )THEN
  ictrl = GetDlgItem( curWindow,newLableID )
  irv   = GetWindowRect( ictrl,BoxN )
  ictrl = GetDlgItem( curWindow,labelID )
  irv   = GetWindowRect( ictrl,BoxO )
  Pnt.x = BoxO.left
  Pnt.y = BoxN.top
  irv = ScreenToClient( curWindow,Pnt )
  irv = SetWindowPos( ictrl,                       & !Position Dialog Window
                      0,                           & !  Z-order : Always ignored
                      Pnt.x,Pnt.y,                 & !  Position
                      0,0,                         & !  Size : Ignored unless sizing flag set
                      IOR(SWP_NOZORDER,SWP_NOSIZE) ) !  Change flag
END IF

IF( controlID /= 0 )THEN
  ictrl = GetDlgItem( curWindow,newControlID )
  irv   = GetWindowRect( ictrl,BoxN )
  ictrl = GetDlgItem( curWindow,controlID )
  irv   = GetWindowRect( ictrl,BoxO )
  Pnt.x = BoxO.left
  Pnt.y = BoxN.top
  irv = ScreenToClient( curWindow,Pnt )
  irv = SetWindowPos( ictrl,                       & !Position Dialog Window
                      0,                           & !  Z-order : Always ignored
                      Pnt.x,Pnt.y,                 & !  Position
                      0,0,                         & !  Size : Ignored unless sizing flag set
                      IOR(SWP_NOZORDER,SWP_NOSIZE) ) !  Change flag
END IF

RETURN
END
