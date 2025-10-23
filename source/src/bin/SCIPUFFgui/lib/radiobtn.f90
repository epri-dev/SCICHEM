!***********************************************************************
!                ProcessRadio
!***********************************************************************
RECURSIVE SUBROUTINE process_radio( iwnd_db,MyCmd )

USE resource_fd
USE pcscipuf_fi
USE winAPI_fd

!     This routine processes RADIOBUTTONs from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN),     INTENT( IN ) :: iwnd_db !Dialog Box handle
TYPE( CMD ),              INTENT( IN ) :: MyCmd   !Command Structure

INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER igroup,old_choice,jd_level
INTEGER zone,irv,indx
INTEGER(POINTER_LEN)jwnd_db
REAL    lat0,lon0,x0,y0
LOGICAL lok

CHARACTER(128), EXTERNAL :: AddNull

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Set New selection

igroup = id_button/10
old_choice = ichoice(igroup+1,id_level)
ichoice(igroup+1,id_level) = id_button - 10*igroup

!---- Select additional actions based on dialog and group number

SELECT CASE( id_dialog )

  CASE( IDB_CONTOUR ) !CONTOUR
    IF( igroup == 0 )THEN ! Contour Type
      CALL show_contours( iwnd_db,ichoice(igroup+1,id_level) ) ! Show appropriate controls
    END IF

  CASE( IDB_MAPS ) !MAPS

    SELECT CASE( igroup )

      CASE( 0 ) ! Map Type
        CALL show_maps( iwnd_db,ichoice(1,id_level),ichoice(3,id_level) ) ! Show appropriate controls

      CASE( 2 ) ! Population Type
        CALL show_maps( iwnd_db,ichoice(1,id_level),ichoice(3,id_level) ) ! Show appropriate controls
        CALL FindHwndListId( IDB_PLOT,jwnd_db,jd_level                  ) ! Parent = PLOT
        lcheck(10,jd_level) = .TRUE.

      CASE DEFAULT

    END SELECT

  CASE( IDB_AXES ) !AXES

    IF( igroup == 0 )THEN ! Coordinate Type
      CALL set_llc_reference(project( BASE_LEVEL)%MapCoord,ichoice(1,id_level), &
                                      1,lon0,lat0,x0,y0,zone )
      dbreal(1,id_level)  = lon0 !Local Coord. X origin
      dbreal(2,id_level)  = lat0 !Local Coord. Y Origin
      dbreal(9,id_level)  = x0   !Local Coord. X origin
      dbreal(10,id_level) = y0   !Local Coord. Y Origin
      CALL SetEditRs( iwnd_db,dbreal(1,id_level),1,2 )
      CALL SetEditRs( iwnd_db,dbreal(9,id_level),9,2 )
      CALL reset_local( iwnd_db,ichoice(igroup+1,id_level) ) !   Reset Axes
    END IF

  CASE( IDB_RELDEF ) !RELDEF
    IF( igroup == 0 )THEN !  Release Spec
      CALL release_spec_radio( iwnd_db,id_level,TRUE )
    ELSE IF( igroup == 1 )THEN
      IF( ichoice(2,id_level) == 1 )THEN
        CALL EnableControl( iwnd_db,IDB_EDIT4,FALSE )
      ELSE
        CALL EnableControl( iwnd_db,IDB_EDIT4,TRUE )
      END IF
    END IF

  CASE( IDB_SCIPUF ) !RUN
    IF( igroup == 0 )THEN !  Plot definition
      IF( ichoice(1,id_level) == 1 )THEN
        CALL EnableControl( iwnd_db,IDB_BUTTON4,FALSE )
      ELSE
        CALL EnableControl( iwnd_db,IDB_BUTTON4,TRUE )
      END IF
    END IF

  CASE( IDB_METDEF ) !METDEF
    CALL meteorology_radio( iwnd_db,igroup,id_level )

  CASE( IDB_TIME ) !TIME

  CASE( IDB_DOMAIN ) !DOMAIN
    IF( igroup == 0 )THEN !  Release Spec
      IF( .NOT.project_setup )THEN
        CALL CheckLocalCoordinates( iwnd_db,id_level,.TRUE.,lok )
      END IF
      CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                                 idbcmbo(4,id_level),lcheck(1,id_level) )
    END IF

  CASE( IDB_PRJDEF ) !Setup
    IF( igroup == 3 )THEN
      CALL owner_redraw( iwnd_db,id_dialog,igroup,id_level )
      indx = ichoice(4,id_level) - 1
      CALL SetListSel( iwnd_db,IDB_COMBO2,indx,irv )
      CALL GetListSel( iwnd_db,IDB_COMBO2,1,indx,irv )
      IF( irv > 0 )THEN
        CALL GetListItem( iwnd_db,IDB_COMBO2,indx,string1,irv )
        IF( irv > 0 )THEN
          idbcmbo(2,id_level) = indx + 1
          dbcmbo(2,id_level)  = AddNull( TRIM(string1(1:irv)) )
        END IF
      END IF
    END IF

  CASE( IDB_SETUP ) !Setup
    SELECT CASE( igroup )

      CASE( 0 ) !  Release Spec
        IF( .NOT. project_setup )THEN
          CALL CheckLocalCoordinates( iwnd_db,id_level,.TRUE.,lok )
        END IF
        CALL ShowLocalCoordinates( iwnd_db,ichoice(1,id_level),idbcmbo(3,id_level), &
                                   idbcmbo(4,id_level),lcheck(1,id_level) )

      CASE( 3 )
        CALL owner_redraw( iwnd_db,id_dialog,igroup,id_level )
        indx = ichoice(4,id_level) - 1
        CALL SetListSel( iwnd_db,IDB_COMBO2,indx,irv )
        CALL GetListSel( iwnd_db,IDB_COMBO2,1,indx,irv )
        IF( irv > 0 )THEN
          CALL GetListItem( iwnd_db,IDB_COMBO2,indx,string1,irv )
          IF( irv > 0 )THEN
            idbcmbo(2,id_level) = indx + 1
            dbcmbo(2,id_level)  = AddNull( TRIM(string1(1:irv)) )
          END IF
        END IF

      CASE DEFAULT

    END SELECT

  CASE( IDB_EDTPRJ ) !EDTPRJ
    CALL ShowCheckMarks( iwnd_db )

  CASE DEFAULT

END SELECT

RETURN
END
