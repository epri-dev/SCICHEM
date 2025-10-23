RECURSIVE SUBROUTINE process_combo( iwnd_db,MyCmd )

USE resource_fd
USE SwinAPI
USE SCIAPIversion_fd
USE files_fi
USE pcscipuf_fi

IMPLICIT NONE

INTEGER(POINTER_LEN),     INTENT( IN ) :: iwnd_db !Dialog Box handle
TYPE( CMD ),              INTENT( IN ) :: MyCmd   !Command Structure
!
!     This routine processes COMBOBOXes from Dialog Boxes
!
INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER irv,indx,idb_old
LOGICAL lok
CHARACTER(PATH_MAXLENGTH) db_old

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: AddNull, StripNull

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Set New Value (Text and Index number) from Current Selection

CALL GetListSel( iwnd_db,id_cntrl,1,indx,irv )
IF( irv > 0 )THEN
  CALL GetListItem( iwnd_db,id_cntrl,indx,string1,irv )
  IF( irv > 0 )THEN
    idb_old = idbcmbo(id_button,id_level)
    db_old  = dbcmbo(id_button,id_level)
    idbcmbo(id_button,id_level) = indx + 1
    dbcmbo(id_button,id_level)  = AddNull( TRIM(string1(1:irv)) )
  END IF
END IF

IF( irv <= 0 )RETURN

!---- Select further action based on dialog number

SELECT CASE( id_dialog )
  CASE( IDB_PLOT ) !PLOT
    CALL plot_combo( iwnd_db,id_button,id_level )

  CASE( IDB_AUDIT ) !AUDIT
    IF( id_button == 1 )THEN !Classification
      IF( idbcmbo(id_button,id_level) == 4 )THEN
        CALL EnableControl( iwnd_db,IDB_EDIT2,TRUE )
        CALL ShowControl(   iwnd_db,IDB_EDIT2,SW_SHOWNORMAL )
      ELSE
        CALL EnableControl( iwnd_db,IDB_EDIT2,FALSE )
        CALL ShowControl(   iwnd_db,IDB_EDIT2,SW_HIDE )
      END IF
    END IF

  CASE( IDB_PRJDEF ) !PROJECT
    SELECT CASE( id_button )
      CASE( 1 ) !Classification
        IF( idbcmbo(id_button,id_level) == 4 )THEN
          CALL EnableControl( iwnd_db,IDB_EDIT2,TRUE )
          CALL ShowControl(   iwnd_db,IDB_EDIT2,SW_SHOWNORMAL )
        ELSE
          CALL EnableControl( iwnd_db,IDB_EDIT2,FALSE )
          CALL ShowControl(   iwnd_db,IDB_EDIT2,SW_HIDE )
        END IF
      CASE( 2 ) !Dynamics
        ichoice(4,id_level) = idbcmbo(2,id_level)
        CALL owner_redraw( iwnd_db,id_dialog,3,id_level )
      CASE( 6 ) !StaticPuffs
      CASE( 7 ) !Hazard
      CASE( 8 ) !Mode
      CASE( 9 ) !SourceNests
    END SELECT

  CASE( IDB_SETUP ) !New Setup
    SELECT CASE( id_button )
      CASE( 2 ) !Dynamics
        ichoice(4,id_level) = idbcmbo(2,id_level)
        CALL owner_redraw( iwnd_db,id_dialog,3,id_level )
      CASE( 4 ) !Ref Pt. Deg/DMS
        CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                                   idbcmbo(4,id_level),lcheck(1,id_level) )
      CASE( 6 ) !StaticPuffs
      CASE( 7 ) !Hazard
      CASE( 8 ) !Mode
      CASE( 9 ) !SourceNests
    END SELECT

  CASE( IDB_DOMAIN ) !DOMAIN
    SELECT CASE( id_button )
      CASE( 3 ) !LLA Deg/DMS
        CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                                   idbcmbo(4,id_level),lcheck(1,id_level) )
      CASE( 4 ) !Ref Pt. Deg/DMS
        CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                                   idbcmbo(4,id_level),lcheck(1,id_level) )
    END SELECT

  CASE( IDB_RELDEF ) !RELDEF
    CALL release_combo( iwnd_db,id_button,id_level )

  CASE( IDB_TIME ) !TIME
    IF( id_button == 1 )CALL UpdateEndTime( iwnd_db )

  CASE( IDB_SCIPUF ) !RUN
    IF( id_button == 1 )CALL UpdateEndTime( iwnd_db )

  CASE( IDB_METDEF ) !METDEF
    CALL meteorology_combo( iwnd_db,id_button,id_level,lok )
    IF( .NOT.lok )THEN
      idbcmbo(id_button,id_level) = idb_old
      dbcmbo(id_button,id_level)  = db_old
      IF( idb_old > 0 )THEN
        string1 = StripNull( db_old )
        CALL SetListSelString( iwnd_db,COMBO_BASE+id_button,string1,irv )
      ELSE
        CALL SetListSel( iwnd_db,COMBO_BASE+id_button,-1,irv )
      END IF
    END IF

  CASE ( IDB_MATDEF ) !MATDEF
    CALL material_combo( iwnd_db,id_button,id_level )

  CASE DEFAULT

END SELECT

RETURN
END
!***********************************************************************
!               PlotCombo
!***********************************************************************
SUBROUTINE plot_combo( iwnd_db,id_button,id_level )

USE resource_fd
USE SwinAPI
USE SCIAPIversion_fd
USE files_fi
USE param_fd
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi

!     This routine processes COMBOBOXes from the Plot Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN), INTENT( IN ) :: iwnd_db !Dialog Box handle
INTEGER,              INTENT( IN ) :: id_button !Button ID number
INTEGER,              INTENT( IN ) :: id_level !Dialog level (for data storage)

INTEGER ii, i
LOGICAL LatLon, Cartesian
REAL    rtmp(4)

INTEGER LastCat

!---- Changing Plot variables/times so set READ file and create SLICE flags

LastCat = PlotDef(EDIT_LEVEL)%Field%Category

CALL LoadPlotCombo( iwnd_db,id_level,id_button )

!---- Select by Button number

SELECT CASE( id_button )
  CASE( CLASS_COMBO_ID,CATEGORY_COMBO_ID ) !Changing PLOT Type
    lcheck(10,id_level) = .TRUE. !lread
    IF( LastCat /= PlotDef(EDIT_LEVEL)%Field%Category )THEN
      CALL EnableControlL( iwnd_db,IDB_BUTTON2,.FALSE. ) !Enable/Disable COPY/SAVE button
      CALL EnableControlL( iwnd_db,IDB_BUTTON3,.FALSE. ) !Enable/Disable COPY/SAVE button
      CALL EnableControlL( iwnd_db,IDB_BUTTON4,.FALSE. ) !Enable/Disable PRINT button
      IF( PlotDef(EDIT_LEVEL)%Field%Category == HP_VSLICE .OR. &
          PlotDef(EDIT_LEVEL)%Field%Category == HP_HINT )THEN
        DO i = 1,6
          axesdef(BASE_LEVEL)%dbreal(16+i) = axesdef(BASE_LEVEL)%dbreal(10+i)
        END DO
      ELSE
        CALL set_llc_reference(project( BASE_LEVEL)%MapCoord,axesdef(BASE_LEVEL)%MapCoord, &
                                        0,rtmp(2),rtmp(1),rtmp(3),rtmp(4),ii )
        LatLon = project(BASE_LEVEL)%MapCoord == I_LATLON
        Cartesian = axesdef(BASE_LEVEL)%MapCoord /= I_LATLON
        CALL axes_transform( axesdef(BASE_LEVEL)%dbreal(5),rtmp, &
                             axesdef(BASE_LEVEL)%dbreal(17),LatLon, &
                             Cartesian,.TRUE.,.TRUE. )
      END IF
    END IF

  CASE( TIME_COMBO_ID ) !Changing TIME
    lcheck(10,id_level) = .TRUE. !lread

  CASE( CHOICE_COMBO_ID ) !Changing MATERIAL
    lcheck(10,id_level) = .TRUE. !lread

  CASE( KIND_COMBO_ID ) !Changing SUBGROUP
    lcheck(10,id_level) = .TRUE. !lread

  CASE( TYPE_COMBO_ID ) !Changing Variable type
    lcheck(10,id_level) = .TRUE.

  CASE DEFAULT

END SELECT

RETURN
END
