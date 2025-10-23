RECURSIVE SUBROUTINE process_check( iwnd_db,MyCmd )

USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi

!     This routine processes CHECKBOXes from Dialog Boxes

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
TYPE( CMD )          MyCmd !Command Structure

INTEGER id_cntrl,id_button,id_type,id_dialog,id_level,ibutton
INTEGER idlm
INTEGER(POINTER_LEN) jwnd
LOGICAL lok

CHARACTER(PATH_MAXLENGTH) filename

CHARACTER(PATH_MAXLENGTH), EXTERNAL :: StripNull

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Toggle Check Box parameter

lcheck(id_button,id_level) = .NOT.lcheck(id_button,id_level)

!---- Check for additional actions

SELECT CASE( id_dialog )
  CASE( IDB_LABELS ) !TITLES
    IF( id_button == 3 )THEN
      CALL FindHwndListId( IDB_PLOT,jwnd,idlm )
      IF( PlotDef(EDIT_LEVEL)%Field%Category /= HP_TABLE )THEN
        IF( TRIM(StripNull(dbtext(3,idlm))) =='default' )THEN
          IF( lcheck(id_button,id_level) )THEN
            CALL EnableControl(iwnd_db,IDB_CHECK4,TRUE)
            CALL EnableControl(iwnd_db,IDB_CHECK5,TRUE)
          ELSE
            CALL EnableControl(iwnd_db,IDB_CHECK4,FALSE)
            CALL EnableControl(iwnd_db,IDB_CHECK5,FALSE)
          END IF
        END IF
      END IF
    END IF
  CASE (IDB_PLTOPT) !Plot Options
    IF( id_button == 13 .AND. lcheck(id_button,id_level) )THEN
      filename = TRIM(dbtext(1,id_level))
      CALL GetFile(id_dialog,id_button,id_level,lok,iwnd_db,filename)
      IF( lok )THEN
        dbtext(1,id_level) = TRIM(filename)
      ELSE
        lcheck(id_button,id_level) = .FALSE.
        CALL SetChecks(iwnd_db,lcheck(id_button,id_level),id_button,1)
      END IF
    ELSE IF( id_button == 3 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl( iwnd_db,IDB_CHECK10,TRUE )
      ELSE
        CALL EnableControl( iwnd_db,IDB_CHECK10,FALSE )
      END IF
    END IF
  CASE (IDB_MAPS) !MAPS
    CALL map_check(iwnd_db,id_button,id_level)
  CASE (IDB_SCIPUF) !RUN
    IF( id_button == 1 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl(iwnd_db,IDB_INT7,TRUE)
        CALL EnableControl(iwnd_db,IDB_RADIO01,TRUE)
        CALL EnableControl(iwnd_db,IDB_RADIO02,TRUE)
        IF( ichoice(1,id_level) == 2)THEN
          CALL EnableControl(iwnd_db,IDB_BUTTON4,TRUE)
        ELSE
          CALL EnableControl(iwnd_db,IDB_BUTTON4,FALSE)
        END IF
      ELSE
        CALL EnableControl(iwnd_db,IDB_BUTTON4,FALSE)
        CALL EnableControl(iwnd_db,IDB_INT7,FALSE)
        CALL EnableControl(iwnd_db,IDB_RADIO01,FALSE)
        CALL EnableControl(iwnd_db,IDB_RADIO02,FALSE)
      END IF
    END IF
  CASE (IDB_OPTIONS) !OPTIONS
    IF( id_button == 2 )THEN
      IF( lcheck(2,id_level) )THEN
        IF( dbtext(2,id_level) ==' ' )THEN
          string1 ='Please specify a sampler input file'
        ELSE
          string1 ='Sampler Input file = '//TRIM(dbtext(2,id_level))
        END IF
        CALL EnableControl(iwnd_db,IDB_REAL10,TRUE)
        CALL ShowControl(iwnd_db,IDB_REAL10,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC18,SW_SHOWNORMAL)
      ELSE
        string1 =' '
        CALL EnableControl(iwnd_db,IDB_REAL10,FALSE)
        CALL ShowControl(iwnd_db,IDB_REAL10,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC18,SW_HIDE)
      END IF
      CALL SetControlText(iwnd_db,IDB_STATIC17,string1)
      CALL EnableButtons(iwnd_db,lcheck(2,id_level),2,1)
    END IF
  CASE (IDB_MATDEF) !MATDEF
    IF( id_button == 5 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl(iwnd_db,IDB_BUTTON17,TRUE)
        CALL ShowControl(iwnd_db,IDB_BUTTON17,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC63,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC64,SW_SHOWNORMAL)
      ELSE
        CALL EnableControl(iwnd_db,IDB_BUTTON17,FALSE)
        CALL ShowControl(iwnd_db,IDB_BUTTON17,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC63,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC64,SW_HIDE)
      END IF
    END IF
  CASE (IDB_DOMAIN,IDB_SETUP)
    CALL ShowLocalCoordinates(iwnd_db,ichoice(1,id_level), &
 idbcmbo(3,id_level),idbcmbo(4,id_level),lcheck(id_button,id_level))
  CASE (IDB_METDEF) !METDEF
    IF( id_button == 4 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl(iwnd_db,IDB_BUTTON18,TRUE)
        CALL ShowControl(iwnd_db,IDB_BUTTON18,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC88,SW_SHOWNORMAL)
      ELSE
        CALL EnableControl(iwnd_db,IDB_BUTTON18,FALSE)
        CALL ShowControl(iwnd_db,IDB_BUTTON18,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC88,SW_HIDE)
      END IF
    ELSE IF( id_button == 5 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl(iwnd_db,IDB_BUTTON19,TRUE)
        CALL ShowControl(iwnd_db,IDB_BUTTON19,SW_SHOWNORMAL)
        CALL ShowControl(iwnd_db,IDB_STATIC89,SW_SHOWNORMAL)
      ELSE
        CALL EnableControl(iwnd_db,IDB_BUTTON19,FALSE)
        CALL ShowControl(iwnd_db,IDB_BUTTON19,SW_HIDE)
        CALL ShowControl(iwnd_db,IDB_STATIC89,SW_HIDE)
      END IF
    END IF
  CASE (IDB_RELDEF) !RELDEF
    ibutton = BUTTON_BASE + id_button + 4
    IF( lcheck(id_button,id_level) )THEN
      CALL EnableControl(iwnd_db,ibutton,TRUE)
      CALL ShowControl(iwnd_db,ibutton,SW_SHOWNORMAL)
      IF( id_button == 1 )CALL SetControlText(iwnd_db,IDB_STATIC40,'Mass/location :')
    ELSE
      CALL EnableControl(iwnd_db,ibutton,FALSE)
      CALL ShowControl(iwnd_db,ibutton,SW_HIDE)
      IF( id_button == 1 )CALL SetControlText(iwnd_db,IDB_STATIC40,'Mass :')
    END IF
  CASE (IDB_PRTFIL) !PRINT
    IF( id_button == 5 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL EnableControl(iwnd_db,IDB_LIST1,TRUE)
        CALL ShowControl(iwnd_db,IDB_LIST1,SW_SHOWNORMAL)
      ELSE
        CALL EnableControl(iwnd_db,IDB_LIST1,FALSE)
        CALL ShowControl(iwnd_db,IDB_LIST1,SW_HIDE)
      END IF
    END IF
  CASE (IDB_TERPARM) !TERRAIN
    IF( id_button == 1 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL show_metsave(iwnd_db,id_level,TRUE)
      ELSE
        CALL hide_metsave(iwnd_db,id_level)
      END IF
    ELSE IF( id_button == 2 )THEN
      IF( lcheck(id_button,id_level) )THEN
        CALL show_terrain(iwnd_db,TRUE, &
                      lcheck(7,id_level),lcheck(8,id_level), &
                      lcheck(5,id_level),lcheck(6,id_level))
      ELSE
        CALL hide_terrain(iwnd_db)
      END IF
    ELSE IF( id_button == 5 .OR. id_button == 6 )THEN
      CALL show_terrain(iwnd_db,TRUE, &
                      lcheck(7,id_level),lcheck(8,id_level), &
                      lcheck(5,id_level),lcheck(6,id_level))
    END IF
 CASE DEFAULT
END SELECT

RETURN
END
!***********************************************************************
!               MapCheck
!***********************************************************************
SUBROUTINE map_check(iwnd_db,id_button,id_level)

USE resource_fd
USE basic_fd
USE pcscipuf_fi

!     This routine processes CHECKBOXES from the MAPS Dialog Box

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
INTEGER              id_button !Button ID number
INTEGER              id_level !Dialog level (for data storage)

!---- Select by Button number

SELECT CASE (id_button)
  CASE (2) !Populated Places Text
    IF( lcheck(id_button,id_level) )THEN ! If ON
      CALL EnableControl(iwnd_db,IDB_RADIO11,TRUE) !   Enable Full List
      CALL EnableControl(iwnd_db,IDB_RADIO12,TRUE) !   Enable Partial List
    ELSE ! If OFF
      CALL EnableControl(iwnd_db,IDB_RADIO11,FALSE) !   Disable Full List
      CALL EnableControl(iwnd_db,IDB_RADIO12,FALSE) !   Disable Partial List
    END IF
  CASE (10) !Populated Places Text
    IF( lcheck(id_button,id_level) )THEN ! If ON
      CALL EnableControl(iwnd_db,IDB_RADIO22,TRUE) !   Enable uniform
      CALL show_maps(iwnd_db,ichoice(1,id_level),ichoice(3,id_level)) !   Show appropriate controls
      IF( lcheck(11,id_level) )THEN ! If ON
        lcheck(11,id_level) = .FALSE.
        CALL SetChecks(iwnd_db,lcheck(11,id_level),11,1)
      END IF
    ELSE ! If OFF
      CALL EnableControl(iwnd_db,IDB_RADIO22,FALSE) !   Disable uniform
      CALL show_maps(iwnd_db,ichoice(1,id_level),ichoice(3,id_level)) !   Show appropriate controls
    END IF
  CASE (11) !Populated Places Text
    IF( lcheck(id_button,id_level) )THEN ! If ON
      IF( lcheck(10,id_level) )THEN ! If ON
        lcheck(10,id_level) = .FALSE.
        CALL SetChecks(iwnd_db,lcheck(10,id_level),10,1)
        CALL EnableControl(iwnd_db,IDB_RADIO22,FALSE) !   Disable uniform
        CALL show_maps(iwnd_db,ichoice(1,id_level),ichoice(3,id_level)) !   Show appropriate controls
      END IF
    END IF
  CASE DEFAULT
END SELECT

RETURN
END
