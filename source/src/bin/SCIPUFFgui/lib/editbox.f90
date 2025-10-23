!***********************************************************************
RECURSIVE SUBROUTINE process_edit(iwnd_db,MyCmd)
USE resource_fd
USE SwinAPI
USE pcscipuf_fi
USE plotdlg_fi
USE pltchoice_fi

IMPLICIT NONE

INTEGER(POINTER_LEN) iwnd_db !Dialog Box handle
TYPE( CMD )          MyCmd !Command Structure
!
!     This routine processes text EDITBOXes from Dialog Boxes
!

CHARACTER(PATH_MAXLENGTH) ctemp
INTEGER id_cntrl,id_button,id_type,id_dialog,id_level
INTEGER ncht,idlm
INTEGER(POINTER_LEN) jwnd

!---- Extract Command parameters

id_cntrl  = MyCmd%cntrl
id_button = MyCmd%button
id_type   = MyCmd%type
id_dialog = MyCmd%id
id_level  = MyCmd%level

!---- Set New Value (Return value is number of characters returned)

ncht = GetDlgItemText(iwnd_db,id_cntrl,ctemp,128)

dbtext(id_button,id_level) = ctemp(1:ncht)

!---- Check for additional actions

SELECT CASE (id_dialog)
  CASE (IDB_LABELS) !TITLES
    IF( id_button == 3 )THEN
      CALL FindHwndListId(IDB_PLOT,jwnd,idlm)
      IF( PlotDef(EDIT_LEVEL)%Field%Category /= HP_TABLE )THEN
        IF( dbtext(id_button,id_level)(1:ncht) =='default' )THEN
          CALL EnableControl(iwnd_db,IDB_CHECK4,TRUE)
          CALL EnableControl(iwnd_db,IDB_CHECK5,TRUE)
        ELSE
          CALL EnableControl(iwnd_db,IDB_CHECK4,FALSE)
          CALL EnableControl(iwnd_db,IDB_CHECK5,FALSE)
        END IF
      END IF
    END IF
!        CASE (IDB_MATDEF)                                              ! MATDEF
!          call update_material_buttons(iwnd_db)                                 !  Reset Buttons
  CASE DEFAULT
END SELECT !

RETURN
END
